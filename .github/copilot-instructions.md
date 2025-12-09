# Copilot Instructions for Foodbook Shiny App

## Project Overview

**Foodbook** is a bilingual (EN/FR) Shiny application for analyzing food exposure case data against Canadian Foodbook reference percentages. It consists of two separate apps sharing backend logic:
- **Public App** (`app-public/`): Manual entry, CSV upload for external partners
- **Internal App** (`app-internal/`): CEDARS Excel upload for PHAC staff

Data flows from initialization → reference calculation → user input → statistical comparison (binomial tests).

## Architecture & Data Priority

**Key Components:**
- `src/foodbook_backend.R` - Shared backend (data loading, weighted calculations, binomial tests)
- `src/modules/` - Reusable Shiny modules (`exposure_module.R`, `language_selector_module.R`)
- `translations/translation.json` - Bilingual UI strings (shiny.i18n)
- `data/open-canada/` - Public Foodbook 1 & 2 microdata (3 CSV parts each)

**Data Loading Priority:**
1. Foodbook 2 microdata (if available)
2. Foodbook 1 microdata (3 CSV parts that require joining)
3. Legacy CSV fallback (`data/foodbook_data.csv`)

Microdata enables weighted % calculations filtered by province/territory, age group, and month. CSV fallback uses pre-computed averages.

## Critical Patterns & Conventions

### Bilingual Pattern (EN/FR)
Use `shiny.i18n::Translator` with `translation.json`. Always respect the current language context:
- **Dynamic UI**: Use `renderUI()` + `translate_or_fallback()` helper for runtime translation
- **NOT** JavaScript text replacement
- Test both "en" and "fr" language states

**Infrastructure Details:**
- `init_translator(session, lang)` - Initialize translator in app server (stores in `session$userData$translator`)
- `set_language(lang)` / `get_language()` - Switch language globally
- `t_(key, lang)` - Translate single key with optional language override
- `translate_or_fallback(key, lang, fallback_en, fallback_fr)` - Safe fallback if key missing from JSON

**Example from `exposure_module.R`:**
```r
yes_label <- translate_or_fallback("Yes", lang, "Yes", "Oui")
no_label <- translate_or_fallback("No", lang, "No", "Non")
```

**Example app initialization:**
```r
# In app server function
translator <- init_translator(session, lang = "en", translation_path = "../translations/translation.json")

# Update UI when language changes
observeEvent(language_reactive(), {
  set_language(language_reactive(), session)
  # Re-render components that depend on language
  session$sendCustomMessage("language_changed", language_reactive())
})
```

**Database Labels (Open Canada):**
- Foodbook 1 & 2 use bilingual Stata `.do` files for exposure labels
- Use `fb_parse_label_map_bilingual(en_path, fr_path)` to extract both EN/FR labels simultaneously
- Labels cached in `fb_env$exposure_labels` after first parse
- Always normalize French column names to English internally with `fb_normalize_fb1_colnames()` / `fb_normalize_fb2_colnames()`

### Module Pattern
All reusable UI uses Shiny modules with `moduleServer()` and `NS()`. Modules maintain state via `reactiveValues()`, never globals.

**Module Structure:**
```r
# UI function creates namespace and HTML
exposure_module_ui <- function(id, exposure_name, ref_value, is_custom = FALSE, lang = "en") {
  ns <- NS(id)
  div(
    h4(exposure_name),
    fluidRow(
      column(2, numericInput(ns("yes"), "Yes", 0, min = 0)),
      column(2, numericInput(ns("no"), "No", 0, min = 0))
    )
  )
}

# Server function uses moduleServer and returns reactive values
exposure_module_server <- function(id, translator, lang_reactive) {
  moduleServer(id, function(input, output, session) {
    # Create local reactive values for module state
    state <- reactiveValues(
      observed = 0,
      p_value = NA,
      classification = "Not Significant"
    )
    
    observeEvent({input$yes; input$no; input$prob; input$dk}, {
      state$observed <- (input$yes + input$prob) / (input$yes + input$prob + input$no + input$dk)
    })
    
    return(reactive(state))  # Return as reactive for parent access
  })
}

# Call modules from parent server with NS()
callModule(exposure_module_server, "exposure1", translator, language_reactive)
```

**Key Rules:**
- Always use `NS(id)` prefix for input/output IDs to avoid conflicts
- Return reactive values/lists (not regular variables) for parent access
- Handle missing column names with `sanitize_id()` before using in selectors

### Backend Path Resolution
`fb_get_base_path()` handles running from root or subdirectories. Always use relative paths; never hardcode absolute paths. The function tries current dir first, then parent dir.

## Reactive Patterns in Shiny

**State Management:**
- Store mutable state in `reactiveValues()` within module scope, NOT in `session$userData`
- Use `reactive()` for derived computations that depend on other reactives
- Avoid triggering `observe()` with side effects; use `observeEvent()` instead

**Example from exposure module:**
```r
exposure_module_server <- function(id, translator, lang_reactive) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(observed = 0, p_value = NA)
    
    # Good: observeEvent with isolated side effects
    observeEvent(input$yes, {
      state$observed <- calculate_proportion()  # Trigger only on input$yes change
    })
    
    # Bad: observe will trigger on every reactive dependency
    # observe({ state$observed <- calculate_proportion() })
    
    return(reactive(state))  # Return for parent access
  })
}
```

**Language-Aware Reactives:**
- Language change is `language_reactive()` observable
- UI elements depending on language should use `renderUI()` with language as dependency
- Example:
```r
output$exposure_ui <- renderUI({
  lang <- language_reactive()  # Force dependency
  exposure_module_ui("exp1", "Eggs", 25, lang = lang)
})
```

**Avoiding Reactive Pitfalls:**
- Don't call reactive functions outside `reactive()`, `observe()`, or `output$*`
- Don't modify external variables from within reactives (use return values)
- Don't nest `reactiveValues()` inside reactives (create once at module scope)

## Weighted Calculations & Data Filtering

**Core Functions:**
- `fb_weighted_percent(code, d)` - Calculate weighted exposure % from microdata
  - Filters rows where code value is 1 (yes) or 2 (no)
  - Returns NA if all values missing or invalid
  - Handles weights via `d[["weight"]]` column
  
- `fb_reference_percents(codes, pt_names, age_groups, months)` - Calculate reference percentages
  - `codes`: Named list mapping CEDARS code to Foodbook column (e.g., `list(P01001 = "fb_P01001")`)
  - `pt_names`: Character vector of province/territory names to filter
  - `age_groups`: Character vector like `"5-10 years", "11-18 years"`
  - `months`: Character vector like `"January", "February"`
  - Returns list of reference percentages, one per code
  - Falls back to CSV if microdata unavailable

**Data Filtering Logic:**
- PT filtering uses numeric codes (1-13) internally - mapped via `fb_pt_map()`
- Age group codes: Check Foodbook data dictionary for exact values
- Month filtering uses month name strings (case-sensitive)
- Missing/NA values excluded from denominator (not counted as "no")

**Example from test suite:**
```r
# Microdata filtering
test_that("fb_reference_percents filters by PT correctly", {
  codes <- list(P01001 = "fb_P01001")
  result <- fb_reference_percents(codes, pt_names = "Ontario")
  expect_true(is.numeric(result[[1]]))
})

# CSV fallback (when microdata unavailable)
ref_csv <- fb_reference_percents_csv(codes, pt_names = "Ontario")
```

### Data Classification
The function `classify_exposure(p_value, observed_prop, ref_prop)` returns one of five categories:
- "Alert" (p ≤ 0.05 and observed > reference)
- "Borderline" (0.05 < p ≤ 0.10 and observed > reference)
- "Not Significant"
- "Insufficient Data" (p-value NA)
- "No Reference Value" (reference NA)

Used for color-coding and alerts in UI.

### Exposure Naming & HTML IDs
Exposure names may contain special characters. Always sanitize with `make_safe_id()` before using as HTML id attributes. This prevents selector conflicts.

## Development Workflow

**Run Apps:**
```r
shiny::runApp("app-public")   # Public app
shiny::runApp("app-internal")  # Internal app
```

**Run Tests:**
```r
testthat::test_dir("tests/testthat")  # All tests
testthat::test_file("tests/testthat/test-backend-calculations.R")  # Specific
```

**Terminal R Commands (Windows Corporate):**
First set PATH to R binary:
```powershell
$env:PATH = "C:\Program Files\R\R-4.4.1\bin\x64;$env:PATH"
```
Then use `Rscript` commands normally.

**Update Manifests:**
After adding dependencies, regenerate manifest files:
```r
rsconnect::writeManifest(appDir = "app-public", appPrimaryDoc = "app.R")
rsconnect::writeManifest(appDir = "app-internal", appPrimaryDoc = "app.R")
```

## Safe Changes for Agents

✅ **Low-Risk:**
- UI tweaks in app files (text, layout, styling)
- Module improvements (respecting module signatures)
- Backend performance gains (no signature changes)
- Test additions/fixes
- Translation additions

⚠️ **Medium-Risk (Test First):**
- Backend function signature changes (impacts both apps)
- Data loading logic
- Weighted calculation modifications

❌ **High-Risk (Consult First):**
- Removing data sources
- Breaking module interfaces
- Language switching logic overhauls
- Deployment configuration changes

## Testing Requirements

**Test Framework:** `testthat`  
**Key Test Files:**
- `test-backend-parsing.R` - Stata file parsing, label extraction, rename directives
- `test-backend-calculations.R` - Weighted percentages, filtering, statistical tests
- `test-new-features.R` - Recent feature tests

**Running Tests:**
```powershell
# Set PATH first (Windows corporate)
$env:PATH = "C:\Program Files\R\R-4.4.1\bin\x64;$env:PATH"

# Run all tests
Rscript -e "testthat::test_dir('tests/testthat')"

# Run specific file
Rscript -e "testthat::test_file('tests/testthat/test-backend-calculations.R')"

# Run specific test
Rscript -e "testthat::test_that('description', { expect_equal(...) })"
```

**Test Patterns:**
Always test before modifying calculation functions:
```r
test_that("fb_weighted_percent handles missing data", {
  df <- data.frame(exposure_a = c(1, 2, NA), weight = c(1, 1, 1))
  result <- fb_weighted_percent("exposure_a", df)
  expect_equal(result, 50)  # Only first two rows count
})

test_that("fb_reference_percents filters by PT correctly", {
  codes <- list(P01001 = "fb_P01001")
  result <- fb_reference_percents(codes, pt_names = "Ontario")
  expect_true(is.numeric(result[[1]]))
})
```

**Critical Coverage Areas:**
- Handling missing/NA values in microdata
- PT filtering (numeric codes 1-13)
- Age group and month filtering edge cases
- Bilingual label extraction (EN vs FR paths)
- CSV fallback when microdata unavailable

## Git Conventions

- **Commits**: Use conventional commits (feat:, fix:, docs:, test:)
- **NO AI MENTIONS**: Never reference "Claude", "AI", "LLM" in messages
- **Message Format**: "feat: add custom exposure support" (focus on WHAT and WHY)
- **Sensitive Data**: `upgrade-context/` is .gitignored (legacy microdata)

## Key Files by Task

| Task | File |
|------|------|
| Add UI element | `app-public/app.R` or `app-internal/app.R` |
| New calculation | `src/foodbook_backend.R` |
| Reusable component | `src/modules/exposure_module.R` |
| Bilingual text | `translations/translation.json` |
| Test logic | `tests/testthat/test-*.R` |
| Deployment | `DEPLOYMENT.md` |

## Common Gotchas

1. **Missing Data Files**: Apps fail silently if open-canada CSVs missing. Check `fb_is_available()` output.
2. **PT Code Mapping**: Backend uses numeric codes (1-13) internally. Map carefully with `fb_pt_map()` and `fb_pt_names()`.
3. **Language Context**: Ensure current language matches when calling translation functions. Use `translator$get_translation_language()`.
4. **Reactive Timing**: In Shiny apps, avoid side effects in render functions. Use `observeEvent()` for state changes.
5. **Path Resolution**: Always use relative paths from app directory. Test from both root and subdirectory contexts.

## Performance & Optimization

**Data Loading (Critical at App Startup):**
- Foodbook 2: Single CSV (~600MB), loaded with `data.table::fread()` for speed
- Foodbook 1: Three CSV parts (~100MB each), joined on `uniqueid`
- **Optimization**: Data loaded once at `fb_init()`, cached in `fb_env` as global environment (not session)
- **Load time target**: < 30 seconds for full app startup on typical hardware

**Weighted Calculations:**
- `fb_weighted_percent()` filters microdata by code, applies weights, counts valid responses (1/2 only)
- **Edge cases**: NA values excluded, invalid codes (0, 3+) treated as missing
- **Optimization**: Use vectorized `dplyr` operations; avoid row-by-row loops

**Example performance-sensitive code:**
```r
# GOOD: Vectorized filtering and weighted calculation
fb_weighted_percent <- function(code, d) {
  col_values <- d[[code]]
  col_weights <- d[["weight"]]
  valid <- col_values %in% c(1, 2)  # Vectorized
  if (!any(valid)) return(NA_real_)
  yes_weight <- sum(col_weights[col_values == 1], na.rm = TRUE)
  total_weight <- sum(col_weights[valid], na.rm = TRUE)
  (yes_weight / total_weight) * 100
}

# BAD: Row-by-row processing (slow for large datasets)
for (i in seq_len(nrow(d))) {
  if (d$exposure[i] == code) { ... }  # Avoid this pattern
}
```

**Language Switching:**
- Language change triggers UI re-render via `session$sendCustomMessage()` but does NOT reload data
- Exposure labels cached per language in `fb_env$exposure_labels[[lang]]`
- No network requests during language switch

## Dependencies to Know

**Core**: shiny, bslib, thematic, shiny.i18n, dplyr, purrr, tidyr, stringr, data.table, DT, ggplot2, shinyjs, shinycssloaders  
**Backend**: haven (legacy .dta files), readxl (Excel upload)  
**Testing**: testthat

See `app-public/manifest.json` and `app-internal/manifest.json` for complete pinned versions.

## Helpful Resources

- **Architecture Details**: [AGENT.md](../AGENT.md)
- **Deployment Guide**: [DEPLOYMENT.md](../DEPLOYMENT.md)
- **Style Guide**: [tidyverse style guide](https://style.tidyverse.org/)
- **Shiny Modules**: [Shiny module docs](https://shiny.rstudio.com/articles/modules.html)
- **i18n Library**: [shiny.i18n on CRAN](https://cran.r-project.org/web/packages/shiny.i18n/index.html)
