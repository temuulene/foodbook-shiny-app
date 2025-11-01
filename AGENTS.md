Foodbook Shiny App — AGENTS.md

Purpose
- Give agents a fast, practical map of this repo: how to run, where code lives, how data flows, and guardrails to respect when making changes.

At‑a‑Glance
- Stack: R 4.5.x, Shiny + bslib + thematic, tidyverse (dplyr/tidyr/purrr/stringr), data.table, DT, ggplot2, shinyjs; backend helpers use readxl + haven.
- Entrypoint: `app.R` (single‑file Shiny app sourcing helpers from `src/`).
- Data: legacy CSV at `data/foodbook_data.csv`; optional OMD Foodbook microdata + Stata .do files under `upgrade-context/` for “Advanced” features.
- Deployment: Posit Connect via `manifest.json` (Git‑backed or push‑based with `rsconnect`).

Run Locally
- Install R 4.5.x (Connect manifest targets `platform: 4.5.1`).
- Install packages shown in README or let Connect resolve from `manifest.json`.
- From repo root, run: `shiny::runApp(".")`.

Repository Layout
- `app.R` — Main UI/server, modules, theming. Sources `src/foodbook_backend.R`.
- `src/foodbook_backend.R` — Backend for Foodbook microdata:
  - Detects microdata availability, parses Stata `.do` files for renames/labels,
  - Loads `.dta` files with haven, normalizes weights, exposes helpers:
    - `fb_is_available()`, `fb_exposure_choices()`, `fb_reference_percents()`
    - `fb_pt_names()`, `fb_age_groups()`, `fb_months()`
- `src/data-clean-proportions.R` — Optional script to regenerate `data/foodbook_data.csv` from the toolkit Excel (Table 6).
- `data/` — Legacy CSV and optional source workbook: `Toolkit-binomial-probability-calculation-tool-2.0.xlsx`.
- `upgrade-context/` — OMD Foodbook assets (sensitive): `foodbook.dta`, `foodbook2v2.dta`, and `.do` files for renames/labels.
- `manifest.json` — Posit Connect dependency manifest (pins R version and packages).
- `README.md` — Usage, deploy notes, and regeneration steps for legacy CSV.

How the App Works
- References:
  - If microdata present (see `upgrade-context/`), references are computed as weighted proportions from `.dta` using the backend helpers. Advanced tab is enabled.
  - Otherwise, the app falls back to `data/foodbook_data.csv` (Canada or simple mean across selected PTs). Advanced tab shows instructions.
- UI/UX:
  - Bootstrap 5 via `bslib` with custom variables/rules; `thematic` enables plot theming.
  - “Canada” and “All” selections auto‑deselect when specific values are chosen.
  - Results are downloadable via `DT` and classified by significance thresholds.

Dependencies
- App (core): `shiny`, `bslib`, `thematic`, `dplyr`, `purrr`, `tidyr`, `stringr`, `data.table`, `DT`, `ggplot2`, `shinyjs`, `rlang`.
- Backend (advanced): `readxl`, `haven`.
- Data prep (optional): `here`, `skimr`, `readxl`, `dplyr`, `tidyr`.
- Connect: See `manifest.json` (targets R `4.5.1`). Keep it updated when changing deps.

Development Tips
- Start in `app.R`. The app sources backend helpers: `source("src/foodbook_backend.R")`.
- Advanced tab toggles with `fb_is_available()`. To enable locally, place the microdata + `.do` files in `upgrade-context/`.
- Legacy CSV regeneration: run `source("src/data-clean-proportions.R")` after ensuring the toolkit Excel exists under `data/` (sheet "Table 6"). This overwrites `data/foodbook_data.csv`.
- Update manifest after dependency changes: `rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")`.

Coding Conventions (R)
- Follow tidyverse style (snake_case for functions/objects, clear verbs for functions, pure helpers over side‑effects).
- Keep Shiny UI/server logic readable: small helpers, avoid deep nesting; prefer modules when adding sizeable UI blocks.
- Place non‑UI logic in `src/` helpers; keep `app.R` focused on orchestration and presentation.
- Prefer vectorized dplyr/tidyr operations over manual loops; avoid expensive work in reactive contexts when possible.

Data & Privacy Guardrails
- Treat `upgrade-context/` contents as sensitive/large. Do not commit or redistribute microdata externally unless policy allows.
- The app tolerates absence of microdata (falls back to CSV) — do not hard‑require those files in code paths intended for general use.

Safe Changes for Agents
- UI copy/labels/theme tweaks in `app.R`.
- Add/adjust table/plot formatting; keep accessibility and contrast in mind (Bootstrap 5 theme already tuned).
- Performance improvements in backend functions under `src/` without changing public helper signatures.
- Small features behind clear conditionals that preserve current defaults and fallbacks.

Gotchas
- If `data/foodbook_data.csv` is missing and microdata aren’t present, some reference displays will be `NA` — ensure at least one reference source exists.
- PT codes vs names: backend maps OMD PT codes to names; be consistent when adding filters.
- Ensure `weight` normalization when loading new microdata columns; backend already maps common weight fields to `weight`.
- UNC/Windows paths are fine; avoid hard‑coding absolute paths — work relative to project root.

Deploying to Posit Connect
- Git‑backed: point Connect at this repo/branch; it uses `manifest.json` to resolve R/packages.
- Push‑based: `rsconnect` (see README). Keep `manifest.json` current for reproducibility.

Quick Commands
- Run app: `shiny::runApp(".")`
- Update manifest: `rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")`
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
- Use '#' for inline comments and section headers. Comment only when necessary (e.g., complex code needing explanation). The code should be self‑explanatory.
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
- When reading a CSV, prefer 'data.table::fread' or 'readr::read_csv' depending on the codebase. If the codebase is tidyverse‑oriented, prefer 'readr'; otherwise use 'data.table'.

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
    
## Shiny — App Structure & Modules
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