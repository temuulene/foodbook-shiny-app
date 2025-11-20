# Foodbook Shiny App

**Disponible en français** | **Available in English**

An interactive Shiny application to compare observed case exposures against Foodbook reference percentages from the Canadian Foodbook survey.

## Quick Start

**1. Run the Public App (Manual/CSV upload):**
```r
shiny::runApp("app-public")
```

**2. Run the Internal App (CEDARS upload):**
```r
shiny::runApp("app-internal")
```

## Key Features

-   **Bilingual (EN/FR)**: Full interface translation.
-   **Data Sources**: Uses Open Canada Foodbook data (FB1 & FB2).
-   **Analysis**: Weighted reference proportions, significance testing, and visualizations.
-   **Input Methods**: Manual entry, CSV upload, or CEDARS Excel export (Internal).

## Documentation

-   **[AGENT.md](AGENT.md)**: Developer guide, architecture, and contribution rules.
-   **[DEPLOYMENT.md](DEPLOYMENT.md)**: Detailed deployment instructions for Posit Connect.

## Support

For issues or enhancements, please use the GitHub issue tracker.
