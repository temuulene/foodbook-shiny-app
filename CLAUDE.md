# Foodbook Shiny App

Food Exposure Analysis Tool for comparing case exposure data against population reference values from the Foodbook Report (PHAC).

## Project Structure

```
app-public/          # Public app for PT partners (manual entry + CSV upload)
  app.R              # Main Shiny app (UI + server)
  www/               # Static assets (styles.css, app.js)
app-internal/        # Internal app for PHAC CEDARS workflow (Excel upload)
  app.R
  www/
src/
  foodbook_backend.R # Bootstrap: sources constants.R, then all backend/* modules
  constants.R        # Central constants (thresholds, limits, classification levels)
  common_ui.R        # Shared UI: fb_theme(), fb_commons_head()
  common_server.R    # Shared server init: fb_init_common()
  app_public_helpers.R # Public-app-specific helpers (sanitization, classification, collection)
  i18n_helper.R      # Internationalization setup
  backend/
    fb_exposures.R   # Exposure label resolution, reference percentages, CSV caching
    fb_data_loading.R # CEDARS Excel parsing, data loading
    fb_geography.R   # PT normalization, geographic filters
    fb_utils.R       # classify_exposure(), make_safe_id(), shared utilities
  modules/
    exposure_module.R     # Dynamic exposure input UI + server (Yes/Prob/No/DK counts)
    mod_ref_settings.R    # Reference population filter controls (PT, age, month)
    mod_results_table.R   # DataTable output with classification row styling
    mod_visualization.R   # ggplot2 lollipop chart (observed vs reference)
    mod_about.R           # About/methodology panel
    mod_data_info.R       # Data info panel (coverage tables, snapshot)
    language_selector_module.R # EN/FR language switcher
translations/
  translation.json   # All UI strings in EN/FR (shiny.i18n format)
tests/testthat/      # testthat tests
renv/                # renv package management
```

## Key Architecture Patterns

### Two-App Deployment
- **app-public**: PT partners enter case counts manually or via Excel upload.
- **app-internal**: PHAC epidemiologists upload CEDARS Excel exports.
- Both apps share `src/` backend code, `src/common_ui.R` theme, and `translations/`.
- Both apps use the **same authoritative Foodbook microdata** from `upgrade-context/` (Stata .dta files). Open Canada CSV loaders exist in `fb_data_loading.R` but are NOT called by `fb_init()`.

### Data Sources
- **Foodbook 1.0 (2014-2015)**: `upgrade-context/foodbook.dta` — telephone survey, ~10,000 respondents. Exposures marked with `*` are from this survey only.
- **Foodbook 2.0 (2023-2024)**: `upgrade-context/foodbook2v2.dta` — online+telephone survey, ~21,000 respondents.
- Combined dataset: ~32,686 respondents after merging (FB2 takes precedence for shared variables).
- Labels: `upgrade-context/foodbook variable labeling.do` + `data/exposures_bilingual.csv`
- Toolkit proportions: `data/exposure_proportions_by_pt.csv` (pre-computed fallback if microdata unavailable)

### Dynamic Exposure Modules (Public App)
The public app dynamically creates/destroys exposure input modules based on user selection via `renderUI`. A critical pattern preserves user-entered values across re-renders:

1. `exposure_value_store` (plain R environment) persists input values outside the reactive graph
2. Before each `renderUI` re-render, current values are saved via `isolate(input[[...]])`
3. Saved values are passed as `initial_values` to `exposure_module_ui()` so inputs render with correct values

This is necessary because `renderUI` destroys and recreates all child inputs, which would otherwise reset them to 0.

### Internationalization
- All UI strings go through `tr$t("key")` (shiny.i18n Translator)
- JavaScript message handlers update static UI elements (navbar, tabs, cards) on language change
- Every new user-facing string MUST be added to `translations/translation.json`

### Reference Data Pipeline
1. User selects filters (PT, age group, month) via `mod_ref_settings`
2. Filters are normalized by `fb_normalize_filters()` (converts display names to backend codes)
3. `fb_reference_percents()` computes weighted reference % from cached microdata
4. CSV data is cached in `fb_env$foodbook_csv_data` using `data.table::fread()` for performance
5. Results classified by `classify_exposure()` using constants from `src/constants.R`

### Global State
`fb_env` (an R environment in `fb_exposures.R`) caches:
- `label_map`: exposure code-to-label mappings
- `foodbook_csv_data`: parsed CSV reference data (avoids repeated disk reads)
- Microdata loaded at app startup

## Constants (`src/constants.R`)
All magic numbers are centralized here. Key values:
- `FB_P_VALUE_ALERT = 0.05`, `FB_P_VALUE_BORDERLINE = 0.10` (classification thresholds)
- `FB_MAX_UPLOAD_BYTES = 10MB` (file upload limit)
- `FB_MIN_SAMPLE_SIZE = 5L` (minimum for statistical testing)
- `FB_DEBOUNCE_MS = 400L` (reactive debounce delay)
- `FB_DEFAULT_CUSTOM_REF = 60` (default custom reference %)

## Security
- File uploads validated server-side (type + size)
- Error messages sanitized (raw errors logged, generic message shown to user)
- Numeric inputs clamped via `fb_public_sanitize_count()`
- Unknown exposure labels HTML-escaped via `htmltools::htmlEscape()`
- See `tests/testthat/test-security-validation.R` for security test coverage

## Development

### Running
```r
# From project root:
shiny::runApp("app-public")
shiny::runApp("app-internal")
```

### Testing
```r
testthat::test_dir("tests/testthat")
```

### Adding New Translations
Add entries to `translations/translation.json` in `{"en": "...", "fr": "..."}` format inside the `translation` array. Missing translations produce console warnings at runtime.

### Adding New Exposures/Constants
Edit `src/constants.R` for thresholds/limits. Exposure data comes from the Foodbook toolkit data files loaded at startup.
