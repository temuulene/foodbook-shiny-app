# Foodbook Shiny App

An interactive Shiny application to compare observed case exposures against Foodbook reference percentages from the Canadian Foodbook survey. The app integrates OMD's Foodbook microdata to compute weighted references, supports combined PT references and optional age/month restrictions, and provides multiple input workflows including manual entry, CSV upload, and automated CEDARS Excel processing.

## Features

### Core Analysis
- **Combined PT reference** using Foodbook microdata (weighted), aligned with OMD's Stata approach
- **Optional filters**: Age Group (0-9, 10-19, 20-64, 65+) and Month (seasonal analysis)
- **416 food exposures** from Foodbook 1 and 2 surveys
- **Significance classification**: Alert (≤0.05), Borderline (≤0.10), Not Significant, Insufficient Data, No Reference Value
- **Intelligent defaults**: "Canada" and "All" auto-deselect when specific values are chosen

### Data Input Options
- **Manual entry**: Enter case counts (Yes/Probably/No/Don't Know) for selected exposures
- **Simple CSV upload**: Upload aggregated exposure counts (no CEDARS access required)
- **Advanced CEDARS upload**: Automated analysis from CEDARS Excel export (auto-detects sheets)

### Custom Exposures
- **Type any exposure name** not in Foodbook database
- **Provide custom reference percentage** (0-100%) or use default
- Custom exposures marked in outputs for transparency

### Results & Outputs
- **Complete results tables** with Yes/Probably/No/DK counts alongside statistical tests
- **Publication-ready visualizations** with downloadable PNG (300 DPI)
- **Descriptive export filenames** including PT, age, month, date (e.g., `foodbook_Ontario_age20to64_Jun-Jul-Aug_2025-10-30.png`)
- **URL bookmarking** for sharing complete analyses with colleagues
- **Multiple export formats**: Copy, CSV, Excel with one click

### User Experience
- **Accessibility-minded** theme using Bootstrap 5 (bslib) and `thematic`
- **Loading indicators** and progress feedback for long operations
- **Input validation** with helpful error messages
- **Tooltips** for technical terms and guidance

## Quick Start

1. Install R (version 4.5.x recommended; same major/minor as in `manifest.json` if deploying to Connect)

2. Install required packages:

```r
install.packages(c(
  "shiny", "bslib", "thematic", "dplyr", "purrr", "tidyr",
  "stringr", "data.table", "DT", "ggplot2", "shinyjs",
  "shinycssloaders", "haven", "readxl", "rlang", "tibble"
))
```

3. Run the app from the project root:

```r
shiny::runApp(".")
```

4. (Optional) For testing, install test dependencies:

```r
install.packages(c("testthat", "writexl"))
```

## Data Sources

### Foodbook Microdata (Primary)
Located in `upgrade-context/`:
- `foodbook.dta` - Foodbook 1 survey microdata
- `foodbook2v2.dta` - Foodbook 2 survey microdata
- `foodbook data.do` - Variable rename mappings (Stata)
- `foodbook variable labeling.do` - Exposure code→label mappings (416 exposures)

**Features:**
- Weighted population-representative estimates
- Stratification by PT, age group, and month
- Combined PT analysis with proper weighting

### Simple CSV Upload (External Users)
Upload a CSV file with aggregated exposure counts. Required columns:
```csv
Exposure,Yes,Probably,No,DK
Cherry tomatoes,25,5,10,2
Romaine lettuce,15,3,20,4
```

- Column names are case-insensitive
- Special characters in column names are ignored
- Perfect for external partners without CEDARS access

### Advanced CEDARS Upload (Internal Users)
Upload a CEDARS Excel export with **any disease name**. The app auto-detects sheets based on required columns:

**Exposure data sheet** (required columns):
- NationalID
- ExposureCode
- HasExposureOccurred

**Linelist sheet** (required columns):
- NationalID (minimum)
- CaseStatus (optional, used to filter to confirmed cases)

**Example sheet names that work:**
- "Salmonella Case", "Campylobacter Case", "Listeria Case", "E. coli Case"
- Any custom sheet name as long as required columns are present

### Legacy CSV (Fallback)
`data/foodbook_data.csv` provides pre-computed references when microdata is unavailable:
- 356 exposures × 14 PTs
- No age/month filtering capability
- Simple averaging for multiple PTs (no weights)

## Regenerating legacy CSV (optional)
If you need to refresh `data/foodbook_data.csv` from the toolkit workbook:

```r
# Dependencies for data prep
install.packages(c("readxl", "here", "skimr", "dplyr", "tidyr"))

# Run the tidy pipeline
source("src/data-clean-proportions.R")
```

This reads the Excel sheet (Table 6), pivots to long format, cleans values, prints a summary, and writes `data/foodbook_data.csv`.

## How References Are Computed

### With Microdata (Recommended)
- Weighted proportions using survey weights
- Formula: `weighted_yes / (weighted_yes + weighted_no) × 100`
- Exposure coding: 1 = yes, 2 = no, others = missing
- Combined PT analysis: Weighted pooling maintains population representativeness
- Optional filters: Age Group (0-9, 10-19, 20-64, 65+) and Month (1-12)
- Rounded to 1 decimal place for display

### With CSV Fallback
- Pre-computed proportions from `foodbook_data.csv`
- Simple averaging across multiple PTs (no weights available)
- No age/month filtering capability

### Custom Exposures
- User-provided reference percentage (0-100%)
- Defaults to 60% if not specified
- Clearly marked as "(custom)" in all outputs

## Classification System

Results are classified based on statistical significance and direction:

| Classification | Criteria | Color | Meaning |
|---------------|----------|-------|---------|
| **Alert** | p ≤ 0.05 AND observed > reference | Red | Statistically significant elevated exposure |
| **Borderline** | p ≤ 0.10 AND observed > reference | Yellow | Marginally significant elevated exposure |
| **Not Significant** | p > 0.10 OR observed ≤ reference | Blue | No significant elevation detected |
| **Insufficient Data** | Missing p-value (zero cases) | Gray | Cannot perform statistical test |
| **No Reference Value** | Reference data unavailable | Gray | Exposure not in Foodbook or filters too restrictive |

**Statistical test:** Upper-tail binomial test
**Null hypothesis:** Case proportion = population proportion
**Alternative:** Case proportion > population proportion

## UX Behavior

### Smart Defaults
- "Canada" and "All" auto-deselect when specific values are chosen (avoids ambiguity)
- Prevents selecting both general and specific filters simultaneously

### Input Validation
- Non-negative integers only (0 to 10,000)
- Real-time validation with red highlights for invalid entries
- CSV uploads validated for required columns with helpful error messages

### Results Display
- Conditional formatting by classification (color-coded rows)
- Sortable and searchable tables
- Export buttons: Copy, CSV, Excel
- Plot download: High-resolution PNG (300 DPI)

### URL Bookmarking
- Complete analysis state saved in URL
- Share analyses with colleagues via link
- All filters, exposures, and case counts restore correctly

## Deploying to Posit Connect
This repo includes a `manifest.json` for Git-backed deployment.

- Push changes to the default branch (e.g., `main`).
- In Posit Connect, create (or reconfigure) a Git-backed content item pointing at this repo and branch.
- Connect will use `manifest.json` to resolve R version and packages.

If you prefer push-based deploys from R instead of Git-backed:

```r
install.packages("rsconnect")
rsconnect::addConnectServer("https://<your-connect-host>", "prod")
rsconnect::connectUser(server = "prod")  # authenticate in browser
rsconnect::deployApp(appName = "foodbook-shiny-app", server = "prod", account = "<your-username>")
```

## Repository Layout

```
foodbook-shiny-app/
├── app.R                          # Main Shiny application (~1,500 lines)
├── src/
│   ├── foodbook_backend.R         # Backend functions (microdata, Stata parsing, calculations)
│   └── data-clean-proportions.R   # Optional: regenerate legacy CSV
├── data/
│   ├── foodbook_data.csv          # Legacy pre-computed references (fallback)
│   └── Toolkit-*.xlsx             # Source workbook for CSV regeneration
├── upgrade-context/               # OMD Foodbook assets (sensitive)
│   ├── foodbook.dta               # Foodbook 1 microdata
│   ├── foodbook2v2.dta            # Foodbook 2 microdata
│   ├── foodbook data.do           # Variable renames (Stata)
│   └── foodbook variable labeling.do  # Exposure labels (416 exposures)
├── tests/
│   ├── testthat.R                 # Test runner
│   ├── README.md                  # Testing documentation
│   └── testthat/
│       ├── test-backend-parsing.R      # Stata parsing tests (28 tests)
│       ├── test-backend-calculations.R # Statistical tests (20 tests)
│       └── test-new-features.R         # New features tests (43 tests)
├── manifest.json                  # Posit Connect dependency manifest (R 4.5.1)
├── README.md                      # This file - User documentation
├── CLAUDE.md                      # Developer/agent guidance
├── AGENTS.md                      # Quick reference for AI agents
└── foodbook-shiny-app.Rproj      # RStudio project file
```

## Recent Enhancements (October 2025)

Based on PHAC Outbreak Management Division demo meeting feedback:

1. **Response Count Columns** - Results tables now show Yes, Probably, No, DK counts alongside percentages
2. **"No Reference Value" Classification** - Missing references explicitly flagged instead of defaulting to 60%
3. **CEDARS Auto-Detection** - Upload Excel with any disease name (Salmonella, Listeria, E. coli, etc.)
4. **Descriptive Export Filenames** - All exports include PT, age, month, exposure count, and date
5. **Custom Exposures** - Type any exposure name and provide custom reference percentage
6. **Simple CSV Upload** - External users can upload aggregated counts without CEDARS access
7. **URL Bookmarking** - Share complete analyses via URL with all inputs preserved

**Testing:** All features covered by comprehensive test suite (~108 tests total).

## Testing

Run all tests:
```r
testthat::test_dir("tests/testthat")
```

Run specific test suites:
```r
testthat::test_file("tests/testthat/test-backend-parsing.R")
testthat::test_file("tests/testthat/test-backend-calculations.R")
testthat::test_file("tests/testthat/test-new-features.R")  # October 2025 features
```

**Test Coverage:** ~108 tests across 3 files
- Backend parsing: 28 tests
- Backend calculations: 20 tests
- New features: 43 tests (auto-detection, custom exposures, CSV upload, bookmarking, etc.)

See [tests/README.md](tests/README.md) for detailed testing documentation.

## Notes
- Keep `manifest.json` up to date when changing packages. Regenerate with:

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

- For detailed implementation notes and developer guidance, see [CLAUDE.md](CLAUDE.md)
- For AI agent quick reference, see [AGENTS.md](AGENTS.md)

## Support
Issues and enhancements are welcome via GitHub issues.

