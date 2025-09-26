# Foodbook Shiny App

An interactive Shiny application to compare observed case exposures against Foodbook reference proportions across Canadian provinces and territories. It highlights exposures with elevated risk (Alert/Borderline), provides sortable/downloadable tables, and produces clear comparative visuals.

## Features
- Interactive multi-select of exposures and regions with bookmarkable URLs
- Significance classification using p-value thresholds (Alert, Borderline, Not Significant)
- Publication-ready comparative plot of observed vs. reference percentages
- Downloadable results table (copy/CSV/Excel) with conditional formatting
- Accessibility-minded theme using Bootstrap 5 (bslib) and `thematic`

## Quick Start
1. Install R (same major/minor version as in `manifest.json` if deploying to Connect).
2. Install required packages:

```r
install.packages(c(
  "shiny", "bslib", "thematic", "dplyr", "purrr", "tidyr",
  "stringr", "data.table", "DT", "ggplot2", "shinyjs"
))
```

3. Run the app from the project root:

```r
shiny::runApp(".")
```

## Data
- Primary runtime data: `data/foodbook_data.csv`
- Source workbook (for regeneration): `data/Toolkit-binomial-probability-calculation-tool-2.0.xlsx`

Columns expected in `foodbook_data.csv`:
- `Exposure` (chr)
- `Province.Territory` (chr) – two-letter codes or full names; the app normalizes these
- `Proportion` (numeric, percent 0–100)

## Regenerating Data (optional)
A helper script is provided to rebuild `data/foodbook_data.csv` from the source workbook.

```r
# Dependencies for data prep
install.packages(c("readxl", "here", "skimr", "dplyr", "tidyr"))

# Run the tidy pipeline
source("src/data-clean-proportions.R")
```

This reads the Excel sheet (Table 6), pivots to long format, cleans values, prints a summary, and writes `data/foodbook_data.csv`.

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
- `app.R` – main Shiny application
- `data/` – CSV used by the app and optional source workbook
- `src/data-clean-proportions.R` – script to rebuild the CSV
- `manifest.json` – dependency manifest for Posit Connect
- `foodbook-shiny-app.Rproj` – optional RStudio project file

## Notes
- Keep `manifest.json` up to date when changing packages. Regenerate with:

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

- Large documentation PDFs and ad hoc test scripts were removed for a leaner repo; they aren’t required to run or deploy the app.

## Support
Issues and enhancements are welcome via GitHub issues.
