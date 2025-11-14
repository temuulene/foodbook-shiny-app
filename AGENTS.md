Foodbook Shiny App - AGENTS.md

Purpose
- Give agents a fast, practical map of this repo: how to run, where code lives, how data flows, and guardrails to respect when making changes.

At-a-Glance
- Stack: R 4.5.x, Shiny + bslib + thematic + shiny.i18n (bilingual), tidyverse (dplyr/tidyr/purrr/stringr), data.table, DT, ggplot2, shinyjs; backend helpers use readxl + haven.
- App entrypoints: `app-public/app.R` (external analysis workflows) and `app-internal/app.R` (CEDARS upload workflow). Both source helpers under `src/` and are fully bilingual (EN/FR).
- Data: Open Canada Foodbook CSVs under `data/open-canada/`; legacy CSV at `data/foodbook_data.csv`; optional OMD Foodbook microdata + Stata .do files under `upgrade-context/` for "Advanced" features.
- Translations: 223 bilingual strings in `translations/translation.json`; i18n helpers in `src/i18n_helper.R`; language selector module in `src/modules/language_selector_module.R`.
- Deployment: Posit Connect via `app-public/manifest.json` and `app-internal/manifest.json` (Git-backed or push-based with `rsconnect`).

Run Locally
- Install R 4.5.x (Connect manifests target `platform: 4.5.1`).
- Install packages shown in README or let the appropriate manifest resolve dependencies.
- From repo root:
  - Public app: `shiny::runApp("app-public")`
  - Internal app: `shiny::runApp("app-internal")`

Repository Layout
- `app-public/` - Public UI/server directory (see `app.R`). Sources helpers under `src/`.
- `app-internal/` - Internal UI/server directory (see `app.R`). Sources helpers under `src/`.
- `src/foodbook_backend.R` - Backend for Foodbook microdata:
  - Detects microdata availability, parses Stata `.do` files for renames/labels,
  - Loads `.dta` files with haven, normalizes weights, exposes bilingual helpers:
    - `fb_is_available()`, `fb_exposure_choices(lang)`, `fb_reference_percents()`
    - `fb_pt_names(lang)`, `fb_age_groups()`, `fb_months()`, `fb_month_names(lang)`
- `src/i18n_helper.R` - Internationalization helpers: `init_translator()`, `get_translator()`, `set_language()`, `t_()`.
- `src/modules/` - Reusable Shiny modules:
  - `exposure_module.R` - Exposure input/display module with bilingual support.
  - `language_selector_module.R` - Language switcher dropdown.
- `src/data-clean-proportions.R` - Optional script to regenerate `data/foodbook_data.csv` from the toolkit Excel (Table 6).
- `translations/translation.json` - 223 bilingual strings (EN/FR) for all UI elements.
- `data/` - Open Canada Foodbook CSVs (`foodbook-1/`, `foodbook-2/`), legacy CSV, and optional source workbook.
- `upgrade-context/` - OMD Foodbook assets (sensitive): `foodbook.dta`, `foodbook2v2.dta`, and `.do` files for renames/labels.
- `archive/app.R.legacy` - Original combined app (no longer maintained).
- `app-public/manifest.json` / `app-internal/manifest.json` - Posit Connect dependency manifests (pin R version and packages per app).
- `README.md` - Usage, deploy notes, and regeneration steps for legacy CSV.
- `CLAUDE.md` - Developer/agent guidance with common gotchas and patterns.
- `DEPLOYMENT.md` - Deployment guide for both apps.

How the App Works
- References:
  - If microdata present (see `upgrade-context/`), references are computed as weighted proportions from `.dta` using the backend helpers. Advanced tab is enabled.
  - Otherwise, the app falls back to `data/foodbook_data.csv` (Canada or simple mean across selected PTs). Advanced tab shows instructions.
- UI/UX:
  - Bootstrap 5 via `bslib` with custom variables/rules; `thematic` enables plot theming.
  - "Canada" and "All" selections auto-deselect when specific values are chosen.
  - Results are downloadable via `DT` and classified by significance thresholds.

Dependencies
- App (core): `shiny`, `bslib`, `thematic`, `shiny.i18n`, `dplyr`, `purrr`, `tidyr`, `stringr`, `data.table`, `DT`, `ggplot2`, `shinyjs`, `shinycssloaders`, `rlang`.
- Backend (advanced): `readxl`, `haven`.
- Data prep (optional): `here`, `skimr`, `readxl`, `dplyr`, `tidyr`.
- Testing: `testthat`, `writexl`.
- Connect: See app manifests (target R `4.5.1`). Keep them updated when changing deps.

Development Tips
- Start from the relevant app file (`app-public/app.R` or `app-internal/app.R`). Each sources backend helpers via `source("src/foodbook_backend.R")`.
- Advanced tab toggles with `fb_is_available()`. To enable locally, place the microdata + `.do` files in `upgrade-context/`.
- Legacy CSV regeneration: run `source("src/data-clean-proportions.R")` after ensuring the toolkit Excel exists under `data/` (sheet "Table 6"). This overwrites `data/foodbook_data.csv`.
- Update manifests after dependency changes:
  - Public: `rsconnect::writeManifest(appDir = "app-public", appPrimaryDoc = "app.R", appFiles = "manifest.json")`
  - Internal: `rsconnect::writeManifest(appDir = "app-internal", appPrimaryDoc = "app.R", appFiles = "manifest.json")`

Coding Conventions (R)
- Follow tidyverse style (snake_case for functions/objects, clear verbs for functions, pure helpers over side-effects).
- Keep Shiny UI/server logic readable: small helpers, avoid deep nesting; prefer modules when adding sizeable UI blocks.
- Place non-UI logic in `src/` helpers; keep `app.R` focused on orchestration and presentation.
- Prefer vectorized dplyr/tidyr operations over manual loops; avoid expensive work in reactive contexts when possible.

Data & Privacy Guardrails
- Treat `upgrade-context/` contents as sensitive/large. Do not commit or redistribute microdata externally unless policy allows.
- The app tolerates absence of microdata (falls back to CSV) - do not hard-require those files in code paths intended for general use.

Safe Changes for Agents
- UI copy/labels/theme tweaks in `app-public/app.R` or `app-internal/app.R`.
- Add/adjust table/plot formatting; keep accessibility and contrast in mind (Bootstrap 5 theme already tuned).
- Performance improvements in backend functions under `src/` without changing public helper signatures.
- Small features behind clear conditionals that preserve current defaults and fallbacks.

Gotchas
- If `data/foodbook_data.csv` is missing and microdata aren't present, some reference displays will be `NA` - ensure at least one reference source exists.
- PT codes vs names: backend maps OMD PT codes to names; be consistent when adding filters.
- Ensure `weight` normalization when loading new microdata columns; backend already maps common weight fields to `weight`.
- UNC/Windows paths are fine; avoid hard-coding absolute paths - work relative to project root.
- **Language switching**: When adding translatable UI elements, use `renderUI()` that creates fresh translator instances with `current_lang()` instead of JavaScript DOM manipulation. Avoids encoding issues, race conditions, and broken bindings. See `app-internal/app.R:540-590` for pattern. Never use JavaScript text matching with accented characters.

Deploying to Posit Connect
- Git-backed: point Connect at this repo/branch; configure each app with its manifest (`app-public/manifest.json`, `app-internal/manifest.json`).
- Push-based: `rsconnect` (see README). Keep manifests current for reproducibility.

Quick Commands
- Run public app: `shiny::runApp("app-public")`
- Run internal app: `shiny::runApp("app-internal")`
- Update manifests: see commands above
- Regenerate CSV: `source("src/data-clean-proportions.R")`

Contact & Issues
- See `README.md` for usage notes. File issues/enhancements via your usual tracker.

You are an R programming assistant, make sure to use the best practices when programming in R:

## Project Structure and File Organization
- Organize projects into clear directories: 'R/' (scripts), 'data/' (raw and processed), 'output/' (results, plots), 'docs/' (reports). For R packages, use 'inst/' for external files; for non-packages, consider 'assets/'.
- Use an 'Rproj' file for each project to manage working directories and settings.
- Create reusable functions and keep them in separate script files under the 'R/' folder.
- Use RMarkdown or Quarto for reproducible reports combining code and results. Prefer Quarto if available and installed.
- Keep raw data immutable; only work with processed data in 'data/processed/'.
- Use 'renv' for dependency management and reproducibility. All the dependencies must be installed, synchronized, and locked.
- Version control all projects with Git and use clear commit messages.
- Give a snake_case consistent naming for the file names. The file names should not be too long.
- Avoid using unnecessary dependencies. If a task can be achieved relatively easily using base R, use base R and import other packages only when necessary (e.g., measurably faster, more robust, or fewer lines of code).

## Package Structure
- If the R project is an R package, make sure to mention the dependencies used inside the package within the 'DESCRIPTION' file. All dependencies must have their version number mentioned (e.g: R6 (>= 2.6.1))
- If the R project is an R package, make sure a 'LICENSE' file is available. 
- If the R project is an R package, make sure a 'NEWS.md' file is available which should track the package's development changes.
- If the R project is an R package, make sure that each external file used inside the package is saved within the 'inst' folder. Reading the file should be done using the 'system.file' function. 
- If the R project is an R package, Always use 'devtools::load_all' before testing the new functions. 
- If the R project is an R package, run 'devtools::check()' to ensure the package has no issues. Notes are okay; avoid warnings and errors.
- If the R project is an R package, document functions using roxygen2. Use 'devtools::document()' to generate the required documentation (.Rd files) and 'NAMESPACE' file.

## Naming Conventions
- snake_case: variables and functions (e.g., \`total_sales\`, \`clean_data()\`). 
- UpperCamelCase: for R6, S3, S4, S7 class names (e.g., \`LinearModel\`).
- SCREAMING_SNAKE_CASE: constants and global options (e.g., \`MAX_ITERATIONS\`).
- Avoid ambiguous names (e.g., use \`customer_id\` instead of \`id\`).
- Use verbs for function names (e.g., \`plot_data\`, \`calculate_mean\`).
- Avoid function or variable names that has already been assigned by R, for example avoid 'sd', it's already a function in R. Another example would be 'data'.
- When working with R6 classes, always prepend a '.' to private methods and fields. An example of a method would be '.get_data()' which will be used as 'private$.get_data()'. 
    
## Coding Style
- Follow the [tidyverse style guide](https://style.tidyverse.org/).
- Use spaces around operators (\`a + b\`, not \`a+b\`).
- Keep line length <= 80 characters for readability.
- Use consistent indentation (2 spaces preferred).
- Use '#' for inline comments and section headers. Comment only when necessary (e.g., complex code needing explanation). The code should be self-explanatory.
- Write modular, reusable functions instead of long scripts.
- Prefer vectorized operations over loops for performance.
- Always handle missing values explicitly (\`na.rm = TRUE\`, \`is.na()\`).
- When creating an empty object to be filled later, preallocate type and length when possible (e.g., 'x <- character(length = 100)' instead of 'x <- c()').
- Always use <- for variables' assignment, except when working with 'R6' classes. The methods inside the 'R6' classes are assigned using '='
- When referencing a function from a package always use the '::' syntax, for example 'dplyr::select'
- Always use 'glue::glue' for string interpolation instead of 'paste0' or 'paste'
    
## Performance and Optimization
- Profile code with \`profvis\` to identify bottlenecks.
- Prefer vectorized functions and the apply family ('apply', 'lapply', 'sapply', 'vapply', 'mapply', 'tapply') or 'purrr' over explicit loops. When using loops, preallocate type and memory beforehand.
- Use data.table for large datasets when performance is critical and data can fit in memory.
- When reading a CSV, prefer 'data.table::fread' or 'readr::read_csv' depending on the codebase. If the codebase is tidyverse-oriented, prefer 'readr'; otherwise use 'data.table'.

- Use duckdb when data is out of memory.
- Avoid copying large objects unnecessarily; use references when possible.
    
## Testing and Validation
- Write unit tests with \`testthat\`.
- Use reproducible random seeds (\`set.seed()\`) for consistent results.
- Test functions with edge cases (empty inputs, missing values, outliers).
- Use R CMD check or \`devtools::check()\` for package development.
    
## Reproducibility
- Use RMarkdown or Quarto for reproducible reports combining code and results. Prefer 'Quarto' if already available and installed.
- Capture session info with \`sessionInfo()\` or \`sessioninfo::session_info()\`.
- Pin package versions with \`renv\`.
- Store scripts, data, and results in version control.
- Document all analysis steps in README or report files.
    
## Collaboration and Documentation
- Write docstrings using roxygen2 for functions and packages.
- Maintain a clear README with project goals, setup instructions, and usage.
- Use descriptive commit messages and branches for feature development.
- Share results via HTML/PDF reports or dashboards (Shiny, flexdashboard).
- Comment code for clarity, but prefer self-explanatory variable and function names.
- Use NEWS.md to follow the project development life cycle. 
    
## Shiny - App Structure & Modules
- Use Shiny modules (\`moduleServer\`, \`NS()\`) for encapsulation, reusability, and testability.
- Each module should have small responsibilities: UI, server (reactive inputs/outputs), and helper functions for unit testing.
- Keep UI code declarative and separate from data-processing logic.
- Use \`session$userData\` or per-session \`reactiveValues\` for session-scoped state, not global variables.
- Use \`www/\` for static assets (JS/CSS/images), served automatically by Shiny.
- Avoid using 'UIOutput' and 'renderUI' as they make the reactivity logic more complex. Use them only if it is necessary.
    
## Advanced Practices
- Use S3/S4/S7 or R6 classes for complex objects. Choose depending on the context but have a slight preference for R6.
- Write custom packages for reusable code across projects.
- Automate workflows with \`targets\` for reproducible pipelines.
- Containerize environments with Docker for deployment.
- Use CI/CD (GitHub Actions, GitLab CI) to test and deploy R projects.
  
## Dependencies
Have a preference for the following packages when relying on dependencies:
- purrr for 'list' objects manipulation and functional programming
- shiny for web application development
- 'data.table' or 'dplyr' for in-memory data manipulation
- 'data.table' or 'dplyr' for efficient data import (CSV/TSV, etc.). 
- 'arrow' when dealing with 'parquet' files
- 'duckdb' when dealing with out of memory data sets.
- 'ggplot2' for plotting. 
- 'checkmate' for inputs assertion.
- 'cli' for displaying users' messages.
- 'glue' for string interpolation.
- 'mirai' for parallel computing.
- 'plotly' for interactive plotting.
- 'renv' for dependency management.
- 'jsonlite' for working with 'json'. If the json object is large, use 'yyjsonr'.
- 'Rcpp' when integrating C++ code in the R project.
