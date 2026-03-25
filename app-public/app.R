# Foodbook Public Analysis Tool
# For PT users and external partners - Analysis workflow
# Uses Open Canada public data exclusively

# --- 1. Load Libraries ---
library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(data.table)
library(DT)
library(ggplot2)
library(shinyjs)
library(shinycssloaders)
library(shiny.i18n)
library(readxl)
library(rlang)

# Load backend and modules
source("../src/foodbook_backend.R")
source("../src/i18n_helper.R")
source("../src/common_ui.R")
source("../src/common_server.R")
source("../src/app_public_helpers.R")
source("../src/modules/language_selector_module.R")
source("../src/modules/exposure_module.R")
source("../src/modules/mod_ref_settings.R")
source("../src/modules/mod_results_table.R")
source("../src/modules/mod_visualization.R")
source("../src/modules/mod_about.R")
source("../src/modules/mod_data_info.R")

# Suppress warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
options(bslib.precompiled = TRUE)

# --- 2. User Interface (UI) ---
ui <- function(request) {
  # Initialize translator for UI translation
  # Note: This is separate from the reactive translator in server
  translator <- Translator$new(
    translation_json_path = "../translations/translation.json"
  )
  translator$set_translation_language("en")

  page_navbar(
    title = "Food Exposure Analysis Tool", # Will be updated via JS
    lang = "en",
    theme = fb_theme(),
    header = fb_commons_head(),

    # Analysis Tab
    nav_panel(
      title = span(id = "nav-analysis-label", translator$t("Analysis")),
      icon = icon("calculator"),
      layout_sidebar(
        sidebar = sidebar(
          uiOutput("sidebar_analysis_title"),
          accordion(
            open = c("ref_settings_panel"),
            accordion_panel(
              title = span(
                id = "acc-ref-settings-label",
                translator$t("Reference Settings")
              ),
              value = "ref_settings_panel",
              icon = icon("sliders"),
              mod_ref_settings_ui("ref_settings"),
              uiOutput("category_filter_ui"),
              uiOutput("overanalysis_warning_ui")
            ),
            accordion_panel(
              title = span(
                id = "acc-upload-label",
                translator$t("Upload Exposure Counts (Optional)")
              ),
              value = "upload_panel",
              icon = icon("upload"),
              uiOutput("xlsx_file_input_ui"),
              uiOutput("xlsx_help_text"),
              uiOutput("xlsx_clear_button"),
              uiOutput("xlsx_template_ui")
            ),
            accordion_panel(
              title = span(id = "acc-actions-label", translator$t("Actions")),
              value = "actions_panel",
              icon = icon("gear"),
              actionButton(
                "reset",
                translator$t("Reset Inputs"),
                class = "btn-warning",
                width = "100%"
              ),
              bookmarkButton(
                label = translator$t("Bookmark Analysis"),
                class = "btn-secondary",
                width = "100%"
              )
            )
          )
        ),
        layout_columns(
          col_widths = breakpoints(lg = c(5, 7), md = c(12, 12)),
          # LEFT: Compact exposure inputs
          card(
            card_header(uiOutput("card_exposure_input_label")),
            card_body(
              fill = TRUE,
              class = "p-2",
              uiOutput("help_enter_counts_ui"),
              uiOutput("exposure_select_ui"),
              div(
                class = "exposure-scroll-container",
                uiOutput("exposure_modules_ui")
              ),
              uiOutput("footnote_fb1_ui")
            )
          ),
          # RIGHT: Results + Visualization (always visible)
          navset_card_underline(
            full_screen = TRUE,
            nav_panel(
              title = uiOutput("nav_results_nested_label"),
              class = "results-panel",
              mod_results_table_ui("results_table")
            ),
            nav_panel(
              title = uiOutput("nav_visualization_label"),
              class = "visual-panel",
              mod_visualization_ui("visualization")
            )
          )
        )
      )
    ),

    # Reference Data Tab
    nav_panel(
      title = span(id = "nav-ref-data-label", translator$t("Reference Data")),
      icon = icon("table"),
      card(
        card_header(span(
          id = "card-ref-by-pt-label",
          uiOutput("ref_by_pt_title", inline = TRUE)
        )),
        card_body(
          withSpinner(
            DTOutput("ref_table_by_pt"),
            type = 4
          )
        )
      )
    ),

    # Data Info Tab
    nav_panel(
      title = span(id = "nav-data-info-label", translator$t("Data Info")),
      icon = icon("database"),
      mod_data_info_ui("data_info")
    ),

    # About Tab
    nav_panel(
      title = span(id = "nav-about-label", translator$t("About")),
      icon = icon("info-circle"),
      card(
        class = "well-panel-about",
        card_header(h3(span(
          id = "card-about-label",
          translator$t("About This Tool")
        ))),
        card_body(mod_about_ui("about"))
      )
    ),

    # Right-side navbar items: spacer pushes items to the right
    nav_spacer(),
    nav_item(
      language_selector_ui("lang_selector", style = "dropdown")
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  )
}

# --- 3. Server Logic ---
server <- function(input, output, session) {
  # Initialize shared logic
  common <- fb_init_common(session, "../translations/translation.json")
  translator <- common$translator
  current_lang <- common$current_lang
  get_tr <- common$get_tr # Reactive translator getter

  # Exposure label helpers extracted to src/app_public_helpers.R:
  # fb_build_exposure_label_map() and fb_fb_resolve_exposure_label()

  # Store uploaded CSV data
  csv_data <- reactiveVal(NULL)
  # Flag for CSV population
  csv_needs_population <- reactiveVal(FALSE)

  # Load toolkit data on startup
  fb_load_toolkit_data()

  # --- Modules ---

  # Reference Settings Module (sidebar)
  # Public app uses "Canada" as default and available PTs are from toolkit/backend
  ref_settings <- mod_ref_settings_server(
    "ref_settings",
    get_tr = get_tr,
    available_pts_reactive = reactive(fb_public_available_pts())
  )

  # Helper to get selected values from module
  selected_province <- ref_settings$province
  selected_age <- ref_settings$age_group
  selected_month <- ref_settings$month

  # About Module
  mod_about_server("about", get_tr = get_tr)

  # --- Local UI Logic ---

  # Render Category Filter
  output$category_filter_ui <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    cats <- c(tr$t("All Categories"), fb_exposure_categories(lang))
    selectInput("category_filter", tr$t("Filter Category"), choices = cats)
  })

  # Render sidebar title
  output$sidebar_analysis_title <- renderUI({
    tags$div(class = "title", get_tr()$t("Analysis Parameters"))
  })

  output$card_exposure_input_label <- renderUI({
    span(id = "card-exposure-input-label", get_tr()$t("Exposure Data Input"))
  })

  output$nav_results_nested_label <- renderUI({
    span(id = "nav-results-nested-label", get_tr()$t("Results"))
  })

  output$nav_visualization_label <- renderUI({
    span(id = "nav-viz-label", get_tr()$t("Visualization"))
  })

  output$help_enter_counts_ui <- renderUI({
    helpText(span(
      id = "help-enter-counts",
      get_tr()$t(
        "Enter case counts for each exposure in each selected location."
      )
    ))
  })

  output$footnote_fb1_ui <- renderUI({
    helpText(
      span(
        id = "footnote-fb1-label",
        get_tr()$t("* Exposures from Foodbook 1.0")
      ),
      class = "text-body-secondary",
      style = "font-size: 0.8rem; margin-top: 0.5rem;"
    )
  })

  output$footnote_fb1_only_ui <- renderUI({
    helpText(
      span(
        id = "footnote-fb1-only-label",
        get_tr()$t("* Exposures from Foodbook 1 only")
      ),
      class = "text-body-secondary",
      style = "font-size: 0.8rem; margin-top: 0.5rem;"
    )
  })

  # Render over-analysis warning
  output$overanalysis_warning_ui <- renderUI({
    tr <- get_tr()
    div(
      class = "alert alert-warning",
      style = "font-size: 0.85rem; padding: 0.75rem; margin-top: 0.5rem;",
      icon("exclamation-triangle"),
      " ",
      tags$strong(tr$t("Data Quality Warning")),
      tags$br(),
      tr$t(
        "Please be careful not to overanalyse the data. Limiting the data to a small subset of respondents (for example, respondents ages 0-9 from PEI in March) can result in small sample sizes and make the data less reliable. This is especially important for exposures that are rare within the population."
      )
    )
  })

  # Render XLSX inputs (same as before)
  output$xlsx_file_input_ui <- renderUI({
    tr <- get_tr()
    fileInput(
      "simple_xlsx_upload",
      label = tr$t("Upload Excel File"),
      accept = c(".xlsx"),
      buttonLabel = tr$t("Browse"),
      placeholder = tr$t("No file selected")
    )
  })

  output$xlsx_help_text <- renderUI({
    tr <- get_tr()
    tagList(helpText(HTML(paste0(
      "<strong>",
      tr$t("Note"),
      ":</strong> ",
      tr$t(
        "Exposure names will be matched against Foodbook database in English or French (case-insensitive). Unmatched exposures will use custom references."
      )
    ))))
  })

  output$xlsx_clear_button <- renderUI({
    actionButton(
      "xlsx_clear",
      label = get_tr()$t("Remove File"),
      icon = icon("trash"),
      class = "btn btn-outline-secondary w-100 mt-2"
    )
  })

  output$xlsx_template_ui <- renderUI({
    downloadLink(
      "download_template",
      get_tr()$t("Download Template"),
      class = "btn btn-outline-primary btn-sm mt-2",
      style = "display: block; text-align: center;"
    )
  })

  # Download handler for template file
  output$download_template <- downloadHandler(
    filename = function() {
      "exposure_template.xlsx"
    },
    content = function(file) {
      file.copy("www/exposure_template.xlsx", file)
    }
  )

  # --- Exposure Selection Logic ---

  # Dynamic Exposure Selection
  output$exposure_select_ui <- renderUI({
    lang <- current_lang()
    tr <- get_tr()

    cat_filter <- input$category_filter
    real_cat <- if (
      !is.null(cat_filter) && cat_filter != tr$t("All Categories")
    ) {
      cat_filter
    } else {
      NULL
    }

    all_exposures <- tryCatch(
      fb_toolkit_exposure_choices(lang, category = real_cat),
      error = function(e) {
        warning("Unable to load exposure choices: ", e$message)
        list()
      }
    )

    current_selection <- isolate(input$exposure_select)

    selectizeInput(
      "exposure_select",
      tr$t("Select Exposures:"),
      choices = all_exposures,
      selected = current_selection,
      multiple = TRUE,
      width = "100%",
      options = list(
        placeholder = tr$t("Start typing..."),
        plugins = list("remove_button"),
        create = TRUE
      )
    )
  })

  # --- Dynamic Exposure Modules ---

  # Track active module IDs
  exposure_module_ids <- reactiveVal(character(0))

  # Observer to handle exposure selection addition/removal
  observeEvent(
    input$exposure_select,
    {
      current_selection <- input$exposure_select %||% character()

      # Normalize IDs
      # Warning: Input can be names or codes.
      # We need safe IDs for modules.
      needed_ids <- unique(purrr::map_chr(current_selection, make_safe_id))

      # Update stored IDs
      exposure_module_ids(needed_ids)
    },
    ignoreNULL = FALSE
  )

  # Render exposure modules (Dynamic UI)
  output$exposure_modules_ui <- renderUI({
    selected <- input$exposure_select
    if (length(selected) == 0) {
      return(NULL)
    }

    # Save current module values before re-rendering (isolate to avoid deps)
    for (id in isolate(exposure_module_ids())) {
      mod_id <- paste0("exp_", id)
      y <- isolate(input[[paste0(mod_id, "-yes")]])
      if (!is.null(y)) {
        exposure_value_store[[id]] <- list(
          yes = y,
          prob = isolate(input[[paste0(mod_id, "-prob")]]),
          no = isolate(input[[paste0(mod_id, "-no")]]),
          dk = isolate(input[[paste0(mod_id, "-dk")]]),
          custom_ref = isolate(input[[paste0(mod_id, "-custom_ref")]])
        )
      }
    }

    lang <- current_lang()
    tr <- get_tr()
    label_map <- fb_build_exposure_label_map(lang)

    # Calculate reference values based on current filters
    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    # Normalize filter selections to backend format
    filters <- fb_normalize_filters(provs, ages, months)

    # Get refs
    refs <- fb_reference_percents(
      selected,
      pt_names = filters$pt,
      months = filters$month,
      age_groups = filters$age
    )

    # Build UI list
    ui_list <- lapply(selected, function(exposure) {
      safe_id <- make_safe_id(exposure)
      ref_val <- refs[[exposure]]
      is_custom <- is.na(ref_val)

      label <- fb_resolve_exposure_label(exposure, lang, label_map)

      # Restore saved values for this exposure (if any)
      saved <- if (exists(safe_id, envir = exposure_value_store)) {
        get(safe_id, envir = exposure_value_store)
      } else {
        NULL
      }

      exposure_module_ui(
        id = paste0("exp_", safe_id),
        exposure_name = label,
        ref_value = if (is.na(ref_val)) 60 else round(ref_val, 1),
        is_custom = is_custom,
        lang = lang,
        initial_values = saved
      )
    })

    do.call(tagList, ui_list)
  })

  module_registry <- reactiveValues()

  # Non-reactive store for preserving exposure input values across UI re-renders
  exposure_value_store <- new.env(parent = emptyenv())

  # Server logic for dynamic modules
  observeEvent(
    exposure_module_ids(),
    {
      ids <- exposure_module_ids()
      if (!length(ids)) {
        return()
      }
      existing_ids <- names(reactiveValuesToList(module_registry))
      new_ids <- setdiff(ids, existing_ids)
      for (id in new_ids) {
        module_registry[[id]] <- exposure_module_server(paste0("exp_", id))
      }
    },
    ignoreInit = TRUE
  )

  # Populate via CSV -- wait for module IDs to update before populating values
  observeEvent(exposure_module_ids(), {
    req(csv_needs_population())
    df <- csv_data()
    req(df)

    for (i in seq_len(nrow(df))) {
      exposure_code <- df$matched_exposure[i]
      safe_id <- make_safe_id(exposure_code)
      mod_id <- paste0("exp_", safe_id)

      y_val <- as.numeric(df$yes[i])
      p_val <- as.numeric(df$probably[i])
      n_val <- as.numeric(df$no[i])
      dk_val <- as.numeric(df$dk[i])

      exposure_module_update(
        session,
        mod_id,
        yes = y_val,
        prob = p_val,
        no = n_val,
        dk = dk_val
      )
    }

    csv_needs_population(FALSE)
  })

  # CSV Upload Handling (from original)
  observeEvent(input$simple_xlsx_upload, {
    req(input$simple_xlsx_upload)
    # ... (Same CSV reading/matching logic as original, but using csv_data reactive) ...
    # See below for abbreviated logic since we already have shared logic? No, this is public-specific.

    file_info <- input$simple_xlsx_upload
    lang <- current_lang()
    tr <- get_tr()

    # Server-side file type validation
    req(tools::file_ext(file_info$name) == "xlsx")

    # Server-side file size validation (10 MB limit)
    file_size <- file.info(file_info$datapath)$size
    validate(need(
      file_size <= FB_MAX_UPLOAD_BYTES,
      tr$t("File too large. Maximum size is 10 MB.")
    ))

    tryCatch(
      {
        df <- readxl::read_excel(file_info$datapath)
        names(df) <- gsub("[^a-z0-9]+", "", tolower(names(df)))
        validate(need(
          all(c("exposure", "yes", "probably", "no", "dk") %in% names(df)),
          "Invalid columns"
        ))

        # Matching logic
        foodbook_choices_en <- fb_exposure_choices(
          "en",
          apply_public_exclusions = TRUE
        )
        foodbook_choices_fr <- fb_exposure_choices(
          "fr",
          apply_public_exclusions = TRUE
        )
        fb_lookup_en <- rlang::set_names(
          foodbook_choices_en,
          tolower(names(foodbook_choices_en))
        )
        fb_lookup_fr <- rlang::set_names(
          foodbook_choices_fr,
          tolower(names(foodbook_choices_fr))
        )

        matched_exposures <- character(nrow(df))
        match_count <- 0
        custom_count <- 0

        for (i in seq_len(nrow(df))) {
          csv_name <- as.character(df$exposure[i])
          if (is.na(csv_name) || !nzchar(csv_name)) {
            matched_exposures[i] <- csv_name
            custom_count <- custom_count + 1
            next
          }
          csv_name_lower <- tolower(csv_name)
          if (csv_name_lower %in% names(fb_lookup_en)) {
            matched_exposures[i] <- fb_lookup_en[[csv_name_lower]]
            match_count <- match_count + 1
          } else if (csv_name_lower %in% names(fb_lookup_fr)) {
            matched_exposures[i] <- fb_lookup_fr[[csv_name_lower]]
            match_count <- match_count + 1
          } else {
            matched_exposures[i] <- csv_name
            custom_count <- custom_count + 1
          }
        }
        df$matched_exposure <- matched_exposures
        csv_data(df)

        # For custom exposures, we need to add them to the choices
        # Get current choices and add any custom (unmatched) exposures
        current_choices <- fb_toolkit_exposure_choices(lang)
        custom_exposures <- fb_public_merge_custom_choices(
          matched_exposures,
          current_choices
        )
        if (length(custom_exposures) > 0) {
          # Add custom exposures as choices (name = value for custom)
          custom_choices <- rlang::set_names(custom_exposures, custom_exposures)
          all_choices <- c(current_choices, custom_choices)
        } else {
          all_choices <- current_choices
        }

        updateSelectizeInput(
          session,
          "exposure_select",
          choices = all_choices,
          selected = matched_exposures
        )

        msg <- paste0(
          tr$t("Success"),
          ": ",
          nrow(df),
          " ",
          tr$t("exposures loaded")
        )
        if (custom_count > 0) {
          msg <- paste0(
            msg,
            " (",
            custom_count,
            " ",
            tr$t("custom/unmatched"),
            ")"
          )
        }
        showNotification(msg, type = "message")

        csv_needs_population(TRUE) # Trigger update
      },
      error = function(e) {
        message("[Upload Error] ", e$message)
        showNotification(
          tr$t(
            "An error occurred while processing your file. Please check the format and try again."
          ),
          type = "error"
        )
      }
    )
  })

  observeEvent(input$xlsx_clear, {
    csv_data(NULL)
    updateSelectizeInput(session, "exposure_select", selected = character(0))
    shinyjs::js$resetFileInput(id = "simple_xlsx_upload")
  })

  # --- Analysis Logic ---

  # Gather Data from Modules for Results (debounced to avoid re-render on each exposure add)
  reactive_results_raw <- reactive({
    ids <- exposure_module_ids()
    if (length(ids) == 0) {
      return(NULL)
    }

    tr <- get_tr()
    lang <- current_lang()
    label_map <- fb_build_exposure_label_map(lang)

    exposure_codes <- input$exposure_select %||% character()
    if (length(exposure_codes) == 0) {
      return(NULL)
    }

    # Convert to DF
    df <- fb_public_collect_exposure_inputs(exposure_codes, input)
    if (nrow(df) == 0) {
      return(NULL)
    }

    df$ExposureLabel <- purrr::map_chr(
      df$Exposure,
      fb_resolve_exposure_label,
      lang = lang,
      label_map = label_map
    )

    # Backend Parameters
    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    # Normalize filters
    filters <- fb_normalize_filters(provs, ages, months)
    backend_pt <- filters$pt
    ages <- filters$age
    months <- filters$month

    # Build scope label for display
    if (is.null(backend_pt)) {
      scope_label <- tr$t("Canada")
    } else {
      pt_names_loc <- fb_pt_names(lang)
      display <- pt_names_loc[provs]
      display[is.na(display)] <- provs[is.na(display)]
      scope_label <- paste(display, collapse = ", ")
    }

    # Calculate Refs
    refs <- fb_reference_percents(
      df$Exposure,
      pt_names = backend_pt,
      months = months,
      age_groups = ages
    )

    # Build analysis input: resolve reference % (system or custom fallback)
    sys_refs <- as.numeric(refs[df$Exposure])
    df$ref_pct <- dplyr::if_else(is.na(sys_refs), df$custom, sys_refs)
    df$scope_label <- scope_label

    fb_classify_results(df, lang = lang)
  })
  reactive_results <- reactive_results_raw |> debounce(millis = FB_DEBOUNCE_MS)

  # Pass results to modules
  mod_results_table_server("results_table", reactive_results, get_tr)
  mod_visualization_server("visualization", reactive_results, get_tr)

  reference_table_data <- reactive({
    lang <- current_lang()
    choices <- fb_toolkit_exposure_choices(lang)
    if (!length(choices)) {
      return(NULL)
    }

    codes <- unname(unlist(choices, use.names = FALSE))
    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    filters <- fb_normalize_filters(provs, ages, months)
    backend_pt <- filters$pt
    ages <- filters$age
    months <- filters$month

    refs <- fb_reference_percents(
      codes,
      pt_names = backend_pt,
      months = months,
      age_groups = ages
    )
    tbl <- fb_public_reference_table_from_choices(choices, refs)
    if (!nrow(tbl)) {
      return(NULL)
    }
    tbl
  })

  # --- Reference Data Tab: Per-PT Table ---
  output$ref_by_pt_title <- renderUI({
    get_tr()$t("Reference Values by Province/Territory")
  })

  output$ref_table_by_pt <- renderDT({
    tr <- get_tr()
    lang <- current_lang()
    tbl <- fb_public_reference_table_by_pt(lang)
    if (is.null(tbl) || !nrow(tbl)) {
      return(NULL)
    }

    datatable(
      tbl,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2),
        language = list(
          search = tr$t("Search:"),
          lengthMenu = paste0(tr$t("Show"), " _MENU_ ", tr$t("entries")),
          info = paste0(
            tr$t("Showing"),
            " _START_ ",
            tr$t("to"),
            " _END_ ",
            tr$t("of"),
            " _TOTAL_ ",
            tr$t("entries")
          ),
          zeroRecords = tr$t("No data available"),
          paginate = list(
            previous = tr$t("Previous"),
            `next` = tr$t("Next")
          )
        )
      ),
      extensions = "FixedColumns",
      rownames = FALSE
    ) |>
      formatStyle(
        columns = "Exposure",
        fontWeight = "bold"
      )
  })

  # --- Data Info Tab (Module) ---
  mod_data_info_server(
    "data_info",
    get_tr = get_tr,
    current_lang = current_lang,
    selected_province = selected_province,
    selected_age = selected_age,
    selected_month = selected_month,
    reference_table_data = reference_table_data
  )

  # Reset Button
  observeEvent(input$reset, {
    updateSelectizeInput(session, "exposure_select", selected = character(0))
    csv_data(NULL)
    updateSelectInput(
      session,
      "category_filter",
      selected = get_tr()$t("All Categories")
    )
    shinyjs::js$resetFileInput(id = "simple_xlsx_upload")
    # Reset filters
    # Can't easily reset module inputs from here without a reset method.
    # But filters default to All/Canada, so user can manually reset.
    # Or reload app.
  })
}

# --- 4. Run the Application ---
thematic::thematic_shiny()
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))
shinyApp(ui = ui, server = server, enableBookmarking = "url")
