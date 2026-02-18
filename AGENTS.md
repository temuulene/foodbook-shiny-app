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

This project provides **two separate Shiny applications** with a shared codebase:
1.  **Public App** (`app-public/app.R`): For external partners (Manual entry, CSV upload).
2.  **Internal App** (`app-internal/app.R`): For PHAC internal use (CEDARS Excel upload).

**Shared Backend & UI Patterns:**
-   **Full UI Parity**: Both apps follow the same modern design system (`bslib` Bootstrap 5). They share identical features:
    -   Language selector in the top-right navbar.
    -   Dark mode toggle.
    -   Sidebar inputs grouped into `accordion` panels.
-   **Shared Backend** (`src/foodbook_backend.R`): Handles data loading, bilingual labels, and weighted calculations.
-   **Shiny Modules** (`src/modules/`): Reusable UI components. `mod_data_info.R` and `mod_results_table.R` are shared across both apps.
-   **Bilingual Support**: Fully bilingual (EN/FR) using `shiny.i18n` and `translations/translation.json`.
-   **Data Priority**: Foodbook 2 (Open Canada) → Foodbook 1 (Open Canada) → Legacy CSV.

**Data Flow:**
1.  **Initialization**: `fb_init()` detects microdata/CSV availability.
2.  **Reference Calculation**:
    -   *Microdata*: Computes weighted % filtered by PT/Age/Month.
    -   *CSV Fallback*: Uses pre-computed averages (no age/month filtering).
3.  **User Workflow**: Users select filters, input exposure data (Manual/CSV/CEDARS), and view statistical comparisons in the Results & Visualization tabs.

## 3. File Organization

```
app-public/             # Public app entry point & assets
app-internal/           # Internal app entry point & assets
src/
  foodbook_backend.R    # Core backend logic (data loading, calcs)
  i18n_helper.R         # Internationalization helpers
  modules/              # Reusable Shiny modules
    exposure_module.R   # Main inputs module
    mod_data_info.R     # Shared Reference Data module
    mod_results_table.R # Shared DT Results module
    mod_visualization.R # Shared ggplot2 module
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

### Development Best Practices
-   **UI Components**: Prefer `bslib` components where possible:
    -   Wrap plots and tables in `card()`.
    -   Use `layout_columns()` and `layout_column_wrap()` for responsive grids.
    -   Use `accordion()` for grouping sidebar controls.
-   **Plotting**: Always call `thematic_shiny()` in the server (usually in `app.R`) to make ggplot2 plots match the Bootstrap theme. Avoid `font = "auto"` if it causes warnings on corporate workstations.
-   **DT Formatting**: For classification coloring in results tables, do **not** use `formatStyle()` on the target column (the backend often uses hidden keys). Instead, use the reinforced `rowCallback` in `mod_results_table.R` which targets `<td>` elements with JS `setProperty('...', '...', 'important')`.

### Common Gotchas
1.  **Missing Data**: Ensure at least `foodbook_data.csv` or microdata exists.
2.  **PT Codes**: Backend uses numeric codes internally; map carefully to names.
3.  **Language Switching**: Use `renderUI` for dynamic translations. Ensure the language selector is in the navbar (`nav_spacer() + nav_item()`).
4.  **Paths**: Use relative paths; avoid hardcoded absolute paths.
5.  **Sensitive Columns**: Hide internal 'Code' columns in public modules (default in `mod_data_info.R` and `mod_results_table.R`).

### Git Commit Guidelines
-   **Professional Messages**: Focus on WHAT and WHY.
-   **Format**: Use conventional commits (feat:, fix:, docs:) where appropriate.

## 6. R Coding Conventions

**General Style:**
-   **Tidyverse**: Follow the [tidyverse style guide](https://style.tidyverse.org/).
-   **Naming**: `snake_case` for variables/functions. Verbs for functions.
-   **Assignment**: `<-` for assignment.
-   **Strings**: `glue::glue()` over `paste0()`.
-   **Dplyr**: Prefer `dplyr::case_match()` over the deprecated `dplyr::recode()`.
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
