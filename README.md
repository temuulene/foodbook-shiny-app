# Foodbook Shiny App

**Disponible en francais** | **Available in English**

Interactive Shiny app to compare observed case exposures against Foodbook reference percentages from publicly available Foodbook 1 and Foodbook 2 data.

## Run the app

From the repo root:
```r
shiny::runApp("app-public")
```

## Data

Uses publicly available Foodbook 1 and Foodbook 2 data, with the backend preferring richer microdata when present and falling back to bundled reference data as needed.

## Testing

```r
testthat::test_dir("tests/testthat")
```

## Documentation

-   [AGENTS.md](AGENTS.md): Developer guide, architecture, and contribution rules.
-   [DEPLOYMENT.md](DEPLOYMENT.md): Posit Connect deployment instructions.

## Support

For issues or enhancements, please use the GitHub issue tracker.
