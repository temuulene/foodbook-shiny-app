# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Quick Start

**Run the app:**
```r
shiny::runApp(".")
```

**Run all tests:**
```r
testthat::test_dir("tests/testthat")
# Or:
source("tests/testthat.R")
```

**Run specific test file:**
```r
testthat::test_file("tests/testthat/test-backend-parsing.R")
testthat::test_file("tests/testthat/test-backend-calculations.R")
```

**Update manifest after changing dependencies:**
```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

**Regenerate legacy CSV (optional):**
```r
source("src/data-clean-proportions.R")
```

## Architecture Overview

This is a **Shiny application** for analyzing food exposure data against Foodbook survey references. The app compares observed case exposures with reference percentages using statistical significance testing.

### Core Design

**Single-file Shiny app** (`app.R`) that:
- Sources backend helpers from `src/foodbook_backend.R`
- Uses **Shiny modules** for exposure inputs (one module per exposure)
- Supports **custom exposures** with user-defined reference percentages
- Implements **dual-mode reference calculation**:
  - **Microdata mode** (when available): Computes weighted proportions from `.dta` files in `upgrade-context/`
  - **CSV fallback mode**: Uses pre-computed values from `data/foodbook_data.csv`
- Provides **multiple data input methods**:
  - Manual entry (one exposure at a time)
  - CSV upload (aggregated counts)
  - CEDARS Excel upload (automated workflow)

**Backend abstraction** (`src/foodbook_backend.R`):
- Detects microdata availability via `fb_is_available()`
- Parses Stata `.do` files to extract variable renames and exposure labels
- Loads Foodbook microdata (`.dta` files) using `haven`
- Normalizes weight columns and computes weighted proportions
- Provides unified API regardless of data source

**Helper functions** (`app.R`):
- `classify_exposure()` - Determines significance classification (Alert/Borderline/Not Significant/Insufficient Data/No Reference Value)
- `make_safe_id()` - Sanitizes strings for HTML element IDs
- `find_sheet_by_columns()` - Auto-detects Excel sheets by required columns (CEDARS flexibility)

### Data Flow

1. **Initialization** (startup):
   - `fb_init()` scans for microdata files and `.do` files
   - Parses renames and labels from Stata scripts
   - Loads and prepares microdata if present
   - Falls back gracefully if microdata unavailable

2. **Reference Calculation**:
   - **With microdata**: `fb_reference_percents()` filters by PT/age/month, computes weighted %
   - **Without microdata**: `fb_reference_percents_csv()` reads from CSV
   - Combined PT references weighted across provinces when multiple selected

3. **User Workflow**:
   - **Analysis tab**:
     - Select reference population (PT/Age/Month filters)
     - Choose exposures from Foodbook database OR type custom exposure names
     - Manual entry of counts (Yes/Probably/No/DK) OR upload CSV file
     - View results with Yes/Prob/No/DK columns, statistical tests, and visualizations
     - Export with descriptive filenames (includes PT, age, month, date)
   - **Advanced tab** (only when microdata available):
     - Upload CEDARS Excel export (auto-detects sheets by columns, works with any disease)
     - Exposure answers and case linelist automatically joined
     - Same filtering and analysis as Analysis tab
     - Flags exposures with "No Reference Value" if not in Foodbook database
   - **Data Info tab**: View reference population statistics, top exposures, coverage by PT/month
   - **About tab**: Methodology, interpretation guide, good practices

### Key Architecture Patterns

**Modular design**: Each exposure gets its own Shiny module (`exposure_module_ui` + `exposure_module_server`) to encapsulate state and avoid naming collisions. Modules dynamically detect custom exposures and show appropriate UI (custom reference input vs. calculated reference).

**Reactive computation**: Results tables react to PT/Age/Month filter changes, recomputing references in real-time. Custom reference percentages override calculated values when provided.

**Graceful degradation**: App works without microdata by falling back to CSV. Advanced tab conditionally appears only when `fb_is_available() == TRUE`. Missing references explicitly flagged as "No Reference Value" instead of silent defaults.

**URL bookmarking**: Custom bookmark handlers save and restore all analysis state (filters, exposures, case counts) for reproducibility and sharing.

**Bootstrap 5 theming**: Uses `bslib` with custom variables and `thematic` for plot consistency.

## File Organization

```
app.R                   # Main Shiny UI/server, modules, theming (~1,500 lines)
src/
  foodbook_backend.R    # Backend: microdata loading, Stata parsing, weighted calculations
  data-clean-proportions.R  # Optional: regenerate CSV from Excel toolkit
data/
  foodbook_data.csv     # Legacy pre-computed references (fallback)
  Toolkit-*.xlsx        # Source workbook for CSV regeneration
upgrade-context/        # OMD Foodbook assets (sensitive):
  foodbook.dta          # Foodbook 1 microdata
  foodbook2v2.dta       # Foodbook 2 microdata
  foodbook data.do      # Variable renames
  foodbook variable labeling.do  # Exposure code → label mapping (416 exposures)
tests/
  testthat/
    test-backend-parsing.R      # Tests for Stata parsing, renames, labels
    test-backend-calculations.R # Tests for weighted %, PT mapping, filtering
manifest.json           # Posit Connect dependency manifest (R 4.5.1)
CLAUDE.md               # This file - developer/agent guidance
AGENTS.md               # Quick reference for AI agents
README.md               # User documentation
```

## Backend API

The backend (`src/foodbook_backend.R`) exposes these helpers:

- `fb_init()` - Initialize backend (call once at startup)
- `fb_is_available()` - Returns TRUE if microdata loaded successfully
- `fb_exposure_choices()` - Returns named list of exposure codes → labels
- `fb_reference_percents(pt_names, age_group, month, exposures)` - Compute weighted % from microdata
- `fb_reference_percents_csv(pt_names, exposures)` - Fallback using CSV
- `fb_pt_names()` - Available province/territory names
- `fb_age_groups()` - Available age groups (0-9, 10-19, 20-64, 65+)
- `fb_months()` - Available months

**Key implementation details:**
- Stata `.do` files parsed via regex to extract `rename` directives and `label =` assignments
- Weight normalization: searches for `EXPWEIGHT_CMA2`, `proj_weight_non_traveller`, or `weight` columns
- Combined PT reference: weighted mean across selected provinces using `sum(weight[exposed]) / sum(weight)`
- Coding: 1 = yes, 2 = no; others treated as missing

## Testing

Uses **testthat** framework with two main test files:

**test-backend-parsing.R**: Tests Stata parsing and data transformations
- `fb_parse_renames()` - Parsing rename directives
- `fb_parse_label_map()` - Extracting exposure labels
- `fb_apply_renames()` - Applying renames to data frames
- `fb_normalise_weight()` - Weight column detection

**test-backend-calculations.R**: Tests statistical computations
- `fb_weighted_percent()` - Weighted proportion calculations
- `fb_pt_map()` - PT code mapping
- `fb_filter_micro()` - Filtering by PT/age/month
- Reference calculation functions

Tests use mock data to avoid dependencies on actual microdata files.

## Data & Privacy

**Sensitive data**: Files in `upgrade-context/` (`.dta` files, `.do` files) contain OMD microdata and should be treated as sensitive. The app tolerates their absence and falls back to CSV.

**Never commit** microdata externally unless policy explicitly allows.

## Deployment

**Target platform**: Posit Connect using R 4.5.1

**Git-backed deployment**:
1. Push changes to `main` branch
2. Configure Posit Connect to point at this repo
3. Connect auto-resolves R version and packages from `manifest.json`

**Push-based deployment** (alternative):
```r
library(rsconnect)
rsconnect::addConnectServer("https://<connect-host>", "prod")
rsconnect::connectUser(server = "prod")  # authenticate in browser
rsconnect::deployApp(appName = "foodbook-shiny-app", server = "prod", account = "<username>")
```

## R Coding Conventions (from AGENTS.md)

This project follows tidyverse style guide principles:

**Naming**:
- `snake_case` for variables and functions
- Verbs for function names (`classify_exposure`, `make_safe_id`)
- Avoid R reserved names (`sd`, `data`)

**Style**:
- Use `<-` for assignment (except R6 methods which use `=`)
- Use `::` for package functions (`dplyr::filter`, `haven::read_dta`)
- Prefer `glue::glue()` for string interpolation over `paste0()`
- Keep lines ≤ 80 characters

**Performance**:
- Prefer vectorized operations over loops
- Use `data.table` for large data (already loaded)
- Preallocate type and length when creating empty objects

**Shiny-specific**:
- Use modules (`moduleServer`, `NS()`) for encapsulation
- Keep UI declarative, separate from data logic
- Avoid `renderUI`/`uiOutput` unless necessary (adds reactivity complexity)
- Use `www/` for static assets

## Recent Enhancements (October 2025)

Based on PHAC-OMD demo meeting feedback, the following features were added:

1. **Response count columns**: Results tables now show Yes/Prob/No/DK counts alongside percentages
2. **"No Reference Value" classification**: Missing references explicitly flagged instead of defaulting to 60%
3. **CEDARS auto-detection**: Upload Excel with any disease name - app auto-detects sheets by required columns
4. **Descriptive export filenames**: Exports include PT, age, month, exposure count, and date
5. **Custom exposures**: Type any exposure name, provide custom reference percentage
6. **Simple CSV upload**: External users can upload aggregated counts (no CEDARS access needed)
7. **URL bookmarking**: Share complete analyses via URL (all inputs preserved)

## Common Gotchas

1. **Missing data sources**: If both `data/foodbook_data.csv` and microdata are missing, reference displays will show "No Reference Value". Ensure at least one source exists.

2. **PT codes vs names**: Backend maps numeric PT codes (from microdata) to full province names. Be consistent when adding filters.

3. **Weight normalization**: When loading new microdata columns, ensure weight field is mapped to `weight`. Backend auto-detects common weight column names.

4. **UX auto-deselection**: "Canada" and "All" selections auto-deselect when specific values are chosen. This is intentional to avoid ambiguity.

5. **Windows paths**: UNC paths work fine. Avoid hard-coded absolute paths; work relative to project root.

6. **Custom exposures**: When user types exposure name not in database, module automatically shows custom reference input. Custom exposures marked with "(custom)" suffix in outputs.

7. **CEDARS sheet names**: No longer hardcoded - app scans all sheets for required columns. Works with "Salmonella Case", "Listeria Case", "E. coli Case", etc.

8. **CSV upload format**: Simple CSV upload requires exact columns: `Exposure, Yes, Probably, No, DK`. Column names are case-insensitive and special chars ignored.

9. **URL bookmarks**: Module inputs are automatically bookmarked. Test by creating bookmark, pasting URL in new tab, and verifying all inputs restore correctly.

10. **Export filenames**: DT table exports use custom filenames (not default "DataTables Table"). If changing filter logic, update filename generation in both Analysis and Advanced renderDT blocks.

## Dependencies

**Core runtime** (required):
- `shiny`, `bslib`, `thematic` - App framework and theming
- `dplyr`, `purrr`, `tidyr`, `stringr` - Data manipulation
- `data.table` - Fast data operations
- `DT` - Interactive tables
- `ggplot2` - Plotting
- `shinyjs` - JS utilities
- `shinycssloaders` - Loading indicators
- `rlang` - Metaprogramming

**Backend** (optional for Advanced mode):
- `haven` - Read Stata `.dta` files
- `readxl` - Parse CEDARS Excel uploads

**Testing**:
- `testthat` - Unit test framework

**See `manifest.json` for pinned versions used in production.**
