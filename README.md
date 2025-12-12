# Foodbook Shiny App

**Disponible en francais** | **Available in English**

Interactive Shiny apps to compare observed case exposures against Foodbook reference percentages from the Canadian Foodbook survey.

## Apps

-   `app-public`: Manual entry or CSV upload for partners outside PHAC.
-   `app-internal`: CEDARS Excel upload workflow for internal teams.

Run from the repo root:
```r
shiny::runApp("app-public")
shiny::runApp("app-internal")
```

## Data sources

-   **Preferred (internal)**: Place Foodbook microdata and Stata label files in `upgrade-context/`:
    -   `foodbook.dta` (FB1), `foodbook2v2.dta` (FB2), `foodbook data.do`, `foodbook variable labeling.do`
-   **Open Canada PUMF (fallback)**: Copy published CSVs into:
    -   `data/open-canada/foodbook-1/foodbook-pumf-fmgd-part-partie-1-en.csv` (and part 2/3, use `-fr` variants for French)
    -   `data/open-canada/foodbook-2/foodbook-2.0-public-use-microdata-file-2023.csv` (or the French file name)
-   **Legacy CSV (last resort)**: `data/foodbook_data.csv` ships in the repo and is used when no microdata are present.

The backend automatically prefers internal microdata, then Open Canada CSVs, then the legacy CSV.

## Testing

```r
testthat::test_dir("tests/testthat")
```

## Documentation

-   [AGENT.md](AGENT.md): Developer guide, architecture, and contribution rules.
-   [DEPLOYMENT.md](DEPLOYMENT.md): Posit Connect deployment instructions.

## Support

For issues or enhancements, please use the GitHub issue tracker.
