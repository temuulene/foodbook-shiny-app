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

