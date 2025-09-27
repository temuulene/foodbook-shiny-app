# Foodbook Shiny App

An interactive Shiny application to compare observed case exposures against Foodbook reference percentages from the Foodbook survey. The app integrates OMD’s Foodbook microdata to compute weighted references, supports combined PT references and optional age/month restrictions, and provides an Advanced workflow for CEDARS Excel uploads.

## Features
- Combined PT reference using Foodbook microdata (weighted), aligned with OMD’s Stata approach
- Optional reference filters: Age Group (0-9, 10-19, 20-64, 65+) and Month
- Intelligent defaults: "Canada" and "All" auto-deselect once other values are chosen
- Advanced tab: upload CEDARS Excel (exposure answers + linelist) for automated analysis
- Significance classification (Alert ≤ 0.05, Borderline ≤ 0.10)
- Publication-ready plot and downloadable results (copy/CSV/Excel)
- Accessibility-minded theme using Bootstrap 5 (bslib) and `thematic`

## Quick Start
1. Install R (same major/minor version as in `manifest.json` if deploying to Connect).
2. Install required packages:

```r
install.packages(c(
  "shiny", "bslib", "thematic", "dplyr", "purrr", "tidyr",
  "stringr", "data.table", "DT", "ggplot2", "shinyjs",
  "haven", "readxl", "rlang"
))
```

3. Run the app from the project root:

```r
shiny::runApp(".")
```

## Data
- Foodbook references: OMD microdata and .do files under `upgrade-context/`
  - `foodbook.dta`, `foodbook2v2.dta`
  - `foodbook data.do` (variable renames)
  - `foodbook variable labeling.do` (exposure labels)
- Advanced (optional): Upload a CEDARS Excel with sheets:
  - `case exposure answer` (exposure answers)
  - `Salmonella Case` (linelist; used to filter confirmed cases when present)

Legacy CSV support (kept for compatibility):
- `data/foodbook_data.csv` can still be used for static displays, but references in the Analysis and Advanced tabs are computed from the microdata above.

## Regenerating legacy CSV (optional)
If you need to refresh `data/foodbook_data.csv` from the toolkit workbook:

```r
# Dependencies for data prep
install.packages(c("readxl", "here", "skimr", "dplyr", "tidyr"))

# Run the tidy pipeline
source("src/data-clean-proportions.R")
```

This reads the Excel sheet (Table 6), pivots to long format, cleans values, prints a summary, and writes `data/foodbook_data.csv`.

## How references are computed
- Weighted proportions from Foodbook microdata (1 = yes, 2 = no; others treated as missing)
- Combined across selected PT(s) when multiple are chosen
- Optional filters for Age Group (0-9, 10-19, 20-64, 65+) and Month
- Reference % values are rounded to 1 decimal in the UI

## UX behaviour
- Defaults ("Canada" and "All") are removed from the selection when additional values are chosen to avoid ambiguity.
- Results tables are downloadable and include conditional formatting by classification.

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
- `app.R` - main Shiny application
- `data/` - legacy CSV used by the app and optional source workbook
- `upgrade-context/` - OMD Foodbook assets (microdata + Stata .do files)
- `src/data-clean-proportions.R` - script to rebuild the CSV
- `manifest.json` - dependency manifest for Posit Connect
- `foodbook-shiny-app.Rproj` - optional RStudio project file

## Notes
- Keep `manifest.json` up to date when changing packages. Regenerate with:

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

## Support
Issues and enhancements are welcome via GitHub issues.

