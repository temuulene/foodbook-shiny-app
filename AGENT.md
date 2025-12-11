# AGENT.md - Developer & Agent Guide

This file is the authoritative guide for developers and AI agents working on the Foodbook Shiny App. It combines project-specific instructions with general R best practices.

## 1. Quick Start

**Run the Apps:**
- **Internal App (CEDARS analysis):** `shiny::runApp("app-internal")`
- **Public App (Manual/CSV upload):** `shiny::runApp("app-public")`

**Run Tests:**
- All tests: `testthat::test_dir("tests/testthat")`
- Specific file: `testthat::test_file("tests/testthat/test-backend-parsing.R")`

**Regenerate Legacy CSV (Optional):**
- `source("src/data-clean-proportions.R")`

## 2. Architecture Overview

This project provides **two separate Shiny applications** sharing a common backend:
1.  **Public App** (`app-public/app.R`): For external partners (Manual entry, CSV upload).
2.  **Internal App** (`app-internal/app.R`): For PHAC internal use (CEDARS Excel upload).

**Core Components:**
-   **Shared Backend** (`src/foodbook_backend.R`): Handles data loading, bilingual labels, and weighted calculations.
-   **Shiny Modules** (`src/modules/`): Reusable UI components (e.g., `exposure_module.R`).
-   **Bilingual Support**: Fully bilingual (EN/FR) using `shiny.i18n` and `translations/translation.json`.
-   **Data Priority**: Foodbook 2 (Open Canada) → Foodbook 1 (Open Canada) → Legacy CSV.

**Data Flow:**
1.  **Initialization**: `fb_init()` detects microdata/CSV availability.
2.  **Reference Calculation**:
    -   *Microdata*: Computes weighted % filtered by PT/Age/Month.
    -   *CSV Fallback*: Uses pre-computed averages (no age/month filtering).
3.  **User Workflow**: Users select filters, input exposure data (Manual/CSV/CEDARS), and view statistical comparisons.

## 3. File Organization

```
app-public/             # Public app entry point & manifest
app-internal/           # Internal app entry point & manifest
src/
  foodbook_backend.R    # Core backend logic (data loading, calcs)
  i18n_helper.R         # Internationalization helpers
  modules/              # Reusable Shiny modules
  data-clean-proportions.R # Script to regenerate legacy CSV
translations/           # Bilingual UI strings (translation.json)
data/
  open-canada/          # Public Foodbook data (FB1 & FB2)
  foodbook_data.csv     # Legacy fallback data
upgrade-context/        # Internal microdata (SENSITIVE - DO NOT COMMIT)
tests/                  # Unit tests (testthat)
DEPLOYMENT.md           # Deployment guide
AGENT.md                # This file
README.md               # User documentation
```

## 4. Backend API (`src/foodbook_backend.R`)

-   **Init**: `fb_init(lang)`, `fb_is_available()`
-   **Data**: `fb_exposure_choices(lang)`, `fb_pt_names(lang)`, `fb_month_names(lang)`
-   **Calc**: `fb_reference_percents(codes, pt_names, age_groups, months)`

## 5. Development Guidelines

### Safe Changes for Agents
-   **UI/Copy**: Tweaks in `app-public/app.R` or `app-internal/app.R`.
-   **Formatting**: Adjusting tables/plots (respecting `bslib` theme).
-   **Backend**: Performance improvements in `src/` without changing signatures.
-   **Features**: Add behind conditionals preserving defaults.

### Common Gotchas
1.  **Missing Data**: Ensure at least `foodbook_data.csv` or microdata exists.
2.  **PT Codes**: Backend uses numeric codes internally; map carefully to names.
3.  **Language Switching**: Use `renderUI` for dynamic translations, not JS text replacement.
4.  **Paths**: Use relative paths; avoid hardcoded absolute paths.

### Git Commit Guidelines
-   **NO AI ATTRIBUTION**: Do not mention "Claude", "AI", "LLM" in commits.
-   **Professional Messages**: Focus on WHAT and WHY.
-   **Format**: Use conventional commits (feat:, fix:, docs:) where appropriate.

## 6. R Coding Conventions

**General Style:**
-   **Tidyverse**: Follow the [tidyverse style guide](https://style.tidyverse.org/).
-   **Naming**: `snake_case` for variables/functions. Verbs for functions.
-   **Assignment**: `<-` for assignment.
-   **Strings**: `glue::glue()` over `paste0()`.
-   **Line Length**: Keep under 80 chars.

**Shiny Specifics:**
-   **Modules**: Use `moduleServer` and `NS()` for all reusable UI.
-   **Reactivity**: Keep `app.R` declarative. Move logic to `src/`.
-   **State**: Use `session$userData` or `reactiveValues`, never global variables.

**Performance:**
-   **Vectorization**: Prefer `dplyr`/`data.table` over loops.
-   **Data**: Use `data.table` for large datasets if needed.

## 7. Testing

-   **Framework**: `testthat`
-   **Locations**: `tests/testthat/`
-   **Key Tests**:
    -   `test-backend-parsing.R`: Stata parsing, renames.
    -   `test-backend-calculations.R`: Weighted proportions, filtering.

## 8. Deployment

-   **Platform**: Posit Connect (R 4.5.1).
-   **Method**: Git-backed (recommended) or Push-based (`rsconnect`).
-   **Manifests**: Update `app-public/manifest.json` and `app-internal/manifest.json` when dependencies change.
    ```r
    rsconnect::writeManifest(appDir = "app-public", appPrimaryDoc = "app.R")
    ```

## 8.1 Running R from Terminal (Corporate Workstation)

R is not in the system PATH on this workstation. Use the full path to Rscript:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\x64\Rscript.exe" -e "your_command_here"
```

Or set PATH first, then use Rscript commands normally:
```powershell
$env:PATH = "C:\Program Files\R\R-4.5.1\bin\x64;$env:PATH"
Rscript -e "rsconnect::writeManifest()"
```

**AI Agents**: Always run the PATH command above before attempting any R commands in terminal.

## 9. Dependencies

**Core**: `shiny`, `bslib`, `thematic`, `shiny.i18n`, `dplyr`, `purrr`, `tidyr`, `stringr`, `data.table`, `DT`, `ggplot2`, `shinyjs`, `shinycssloaders`, `rlang`.
**Backend**: `haven`, `readxl`.
**Testing**: `testthat`.

---

## Appendix: Detailed R Best Practices

### Project Structure
-   **Directories**: `R/` (scripts), `data/` (raw/processed), `tests/`.
-   **Config**: Use `.Rproj` and `renv` for environment management.

### Naming & Style
-   **Variables**: `snake_case` (e.g., `total_sales`).
-   **Classes**: `UpperCamelCase` (e.g., `LinearModel`).
-   **Constants**: `SCREAMING_SNAKE_CASE` (e.g., `MAX_ITERATIONS`).
-   **Comments**: Explain *why*, not *what*.

### Performance
-   **Profile**: Use `profvis` to find bottlenecks.
-   **Import**: `data.table::fread` or `readr::read_csv` for speed.
-   **Memory**: Use `duckdb` for out-of-memory data.

### Collaboration
-   **Documentation**: `roxygen2` style docstrings for functions.
-   **Version Control**: Clear commit messages, feature branches.
