# Foodbook Internal Analysis Tool
# For PHAC internal use - CEDARS outbreak data analysis
# Supports Open Canada data + legacy microdata (if available)

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

# Load backend and modules (using relative paths from app-internal directory)
source("../src/foodbook_backend.R")
source("../src/i18n_helper.R")
source("../src/modules/language_selector_module.R")

# Suppress warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
options(bslib.precompiled = TRUE)

# --- 2. Initialize Backend and Translator ---
translator <- Translator$new(translation_json_path = "../translations/translation.json")
translator$set_translation_language("en")

# Initialize backend (will use Open Canada + legacy if available)
fb_init(lang = "en")
backend_ok <- tryCatch(fb_is_available(), error = function(e) FALSE)

# --- 3. Helper Functions ---
# Shared helpers live in src/foodbook_backend.R

# Helper to find Excel sheet by required columns
find_sheet_by_columns <- function(excel_path, required_cols) {
  all_sheets <- tryCatch(readxl::excel_sheets(excel_path), error = function(e) character(0))

  for (sheet_name in all_sheets) {
    sheet_data <- tryCatch(
      readxl::read_excel(excel_path, sheet = sheet_name, n_max = 1),
      error = function(e) NULL
    )

    if (!is.null(sheet_data)) {
      normalized_cols <- gsub("[^a-z0-9]+", "", tolower(names(sheet_data)))
      if (all(required_cols %in% normalized_cols)) {
        return(list(sheet = sheet_name, found = TRUE, available_sheets = all_sheets))
      }
    }
  }
  return(list(found = FALSE, available_sheets = all_sheets))
}

# --- 4. User Interface (UI) ---
ui <- function(request) {
  page_navbar(
    title = translator$t("Food Exposure Analysis Tool"),
    lang = "en",
    theme = bs_theme(
      version = 5,
      bg = "#f7f9fc",
      fg = "#0f172a",
      primary = "#0e4a7b",
      secondary = "#4b5563",
      success = "#176d4e",
      info = "#0e6a88",
      warning = "#a45100",
      danger = "#b21f2d",
      base_font = font_google("Inter", wght = "300;400;600;700"),
      heading_font = font_google("DM Sans", wght = "400;600;700")
    ) |>
      bs_add_variables(
        "body-color" = "#0f172a",
        "card-cap-bg" = "#f7faff",
        "card-border-color" = "#dde6f5",
        "border-radius" = "0.75rem",
        "min-contrast-ratio" = 4.5,
        "color-contrast-dark" = "#000000",
        "color-contrast-light" = "#ffffff",
        "link-color" = "#0e4a7b",
        "link-hover-color" = "#0a3a61"
      ) |>
      # CSS styles moved to www/styles.css for maintainability
      bs_add_rules(""),
    header = tagList(
      useShinyjs(),
      extendShinyjs(
        text = "
          shinyjs.resetFileInput = function(params) {
            var id = params.id;
            var $fileInput = $('input[type=\"file\"][id=' + id + ']');
            if (!$fileInput.length) {
              $fileInput = $('#' + id).find('input[type=\"file\"]');
            }
            if ($fileInput.length) {
              $fileInput.val('');
              $fileInput.trigger('change');
            }
            var $wrapper = $fileInput.length ? $fileInput.closest('.shiny-file-input') : $('#' + id);
            if ($wrapper.length) {
              var $textInput = $wrapper.find('input[type=\"text\"]');
              if ($textInput.length) {
                $textInput.val('');
              }
              $wrapper.find('.progress-bar').css('width', '0%');
            }
          };
        ",
        functions = c("resetFileInput")
      ),
      # Load external CSS and JS files (extracted for maintainability)
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "app.js")
      ),
      # Hidden language selector (will be moved to navbar by JavaScript)
      tags$div(
        id = "lang_selector_container",
        style = "display: none;",
        language_selector_ui("lang_selector", style = "dropdown")
      )
    ),

    # CEDARS Upload Tab
    nav_panel(
      title = span(id = "nav-cedars-label", translator$t("CEDARS Analysis")),
      icon = icon("upload"),

      layout_sidebar(
        fillable = FALSE,

        sidebar = sidebar(
          width = 350,

          uiOutput("sidebar_upload_title"),

          div(
            id = "cedars_upload_container",
            uiOutput("cedars_file_input_ui"),

            helpText(span(
              id = "help-auto-detect",
              translator$t(
                "The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist)."
              )
            )),
            actionButton(
              "cedars_clear",
              label = translator$t("Remove File"),
              icon = icon("trash"),
              class = "btn btn-outline-secondary w-100 mb-3"
            )
          ),

          hr(),

          uiOutput("sidebar_parameters_title"),

          tooltip(
            selectInput("adv_province",
                       translator$t("Reference PT(s)"),
                       choices = stats::setNames(
                         c("Canada", fb_pt_names("en")),
                         c(translator$t("Canada"), fb_pt_names("en"))
                       ),
                       selected = "Canada",
                       multiple = TRUE),
            translator$t("Select one or more provinces/territories for the reference population. Use Ctrl+Click to select multiple.")
          ),

          tooltip(
            selectInput("adv_age_group",
                       translator$t("Restrict by Age Group"),
                       choices = stats::setNames(
                         c("All Ages", fb_age_groups()),
                         c(translator$t("All Ages"), fb_age_groups())
                       ),
                       selected = "All Ages",
                       multiple = TRUE),
            translator$t("Optionally filter the reference population by age group.")
          ),

          tooltip(
            selectInput("adv_month",
                       translator$t("Restrict by Month"),
                       choices = stats::setNames(
                         c("All Months", as.character(1:12)),
                         c(translator$t("All Months"), fb_month_names())
                       ),
                       selected = "All Months",
                       multiple = TRUE),
            translator$t("Optionally filter the reference population by survey month.")
          )
        ),

        # Main panel
        card(
          card_header(uiOutput("results_card_header", inline = TRUE)),
          card_body(
            withSpinner(
              uiOutput("adv_results_table_container", width = "100%"),
              type = 4,
              color = "#0f4c81"
            )
          )
        )
      )
    ),

    # Data Info Tab
    nav_panel(
      title = span(id = "nav-data-info-label", translator$t("Data Info")),
      icon = icon("database"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(uiOutput("ref_settings_header", inline = TRUE)),
          card_body(uiOutput("ref_summary_ui"))
        ),
        card(
          card_header(uiOutput("ref_snapshot_header", inline = TRUE)),
          card_body(withSpinner(DTOutput("ref_top_exposures"), type = 4, color = "#0f4c81"))
        ),
        card(
          card_header(uiOutput("ref_pt_header", inline = TRUE)),
          card_body(withSpinner(plotOutput("ref_pt_plot", height = "350px"), type = 4, color = "#0f4c81"))
        ),
        card(
          card_header(uiOutput("ref_month_header", inline = TRUE)),
          card_body(withSpinner(plotOutput("ref_month_plot", height = "350px"), type = 4, color = "#0f4c81"))
        )
      )
    ),

    # About Tab
    nav_panel(
      title = span(id = "nav-about-label", translator$t("About")),
      icon = icon("info-circle"),

      card(
        class = "well-panel-about",
        card_header(translator$t("About")),
        card_body(
          uiOutput("about_content")
        )
      )
    )
  )
}

# --- 5. Server Logic ---
server <- function(input, output, session) {
  translator <- init_translator(session, translation_path = "../translations/translation.json")

  build_province_display_map <- function(lang) {
    stats::setNames(fb_pt_names(lang), fb_pt_names("en"))
  }

  build_province_choices <- function(lang, available = fb_pt_names("en")) {
    available <- unique(available)
    available <- available[!is.na(available)]
    display_map <- build_province_display_map(lang)
    labels <- display_map[available]
    labels[is.na(labels)] <- available[is.na(labels)]
    stats::setNames(c("Canada", available),
                    c(translator$t("Canada"), labels))
  }

  # Language selector
  lang_state <- language_selector_server("lang_selector",
                                         session_parent = session,
                                         style = "dropdown")

  current_lang <- lang_state$language
  get_tr <- reactive({
    lang <- current_lang()
    set_language(lang, session)
    get_translator(session)
  })

  adv_cases <- reactiveVal(NULL)

  # Render file input with current language
  output$cedars_file_input_ui <- renderUI({
    lang <- current_lang()
    tr <- get_tr()

    tooltip(
      fileInput("cedars_file",
               tr$t("Choose Excel file..."),
               accept = c(".xlsx"),
               buttonLabel = tr$t("Browse"),
               placeholder = tr$t("No file selected")),
      tr$t("Upload your CEDARS outbreak data export. The tool will automatically extract case exposure information.")
    )
  })

  # Render sidebar titles
  output$sidebar_upload_title <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    h4(tr$t("Upload CEDARS Exposure Data"))
  })

  output$sidebar_parameters_title <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    h5(tr$t("Analysis Parameters"))
  })

  # Render results card header
  output$results_card_header <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    tr$t("Results")
  })

  # Update UI when language changes
  observeEvent(current_lang(), {
    lang <- current_lang()
    set_language(lang, session)
    translator <- get_tr()

    # Update language labels only, don't re-initialize entire backend
    fb_update_language(lang = lang)

    # Preserve current selections by converting to appropriate format
    current_prov <- input$adv_province
    if (is.null(current_prov) || !length(current_prov)) current_prov <- "Canada"

    current_age <- input$adv_age_group
    if (is.null(current_age) || !length(current_age)) current_age <- "All Ages"

    current_month <- input$adv_month
    if (is.null(current_month) || !length(current_month)) current_month <- "All Months"

    available_pts <- if (!is.null(adv_cases())) {
      pts <- case_pts()
      if (is.null(pts) || !length(pts)) fb_pt_names("en") else pts
    } else {
      fb_pt_names("en")
    }

    valid_selected <- current_prov[current_prov == "Canada" | current_prov %in% available_pts]
    if (!length(valid_selected)) {
      if ("Canada" %in% current_prov) {
        valid_selected <- "Canada"
      } else if (length(available_pts)) {
        valid_selected <- available_pts
      } else {
        valid_selected <- "Canada"
      }
    }

    updateSelectInput(session, "adv_province",
                     label = translator$t("Reference PT(s)"),
                     choices = build_province_choices(lang, available_pts),
                     selected = unique(valid_selected))

    updateSelectInput(session, "adv_age_group",
                     label = translator$t("Restrict by Age Group"),
                     choices = stats::setNames(
                       c("All Ages", fb_age_groups()),
                       c(translator$t("All Ages"), fb_age_groups())
                     ),
                     selected = current_age)

    month_choices <- stats::setNames(
      c("All Months", as.character(1:12)),
      c(translator$t("All Months"), fb_month_names(lang))
    )
    updateSelectInput(session, "adv_month",
                     label = translator$t("Restrict by Month"),
                     choices = month_choices,
                     selected = current_month)

    updateActionButton(session, "cedars_clear",
                       label = translator$t("Remove File"))

    # Update button labels via JavaScript
    session$sendCustomMessage("update-button-labels", list(
      download = translator$t("Download Plot")
    ))

    # Update tab names via JavaScript
    session$sendCustomMessage("update-tab-names", list(
      cedars = translator$t("CEDARS Analysis"),
      data_info = translator$t("Data Info"),
      about = translator$t("About"),
      results = translator$t("Results"),
      visualization = translator$t("Visualization")
    ))

    # Update misc labels via JavaScript
    session$sendCustomMessage("update-misc-labels", list(
      auto_detect_help = translator$t("The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist).")
    ))

    session$sendCustomMessage("language_changed", lang)
  }, ignoreInit = TRUE)

  # Auto-deselect "Canada" when specific PTs selected
  observeEvent(input$adv_province, {
    if ("Canada" %in% input$adv_province && length(input$adv_province) > 1) {
      updateSelectInput(session, "adv_province",
                       selected = setdiff(input$adv_province, "Canada"))
    }
  })

  # Auto-deselect "All Ages" when specific ages selected
  observeEvent(input$adv_age_group, {
    if ("All Ages" %in% input$adv_age_group && length(input$adv_age_group) > 1) {
      updateSelectInput(session, "adv_age_group",
                       selected = setdiff(input$adv_age_group, "All Ages"))
    }
  })

  # Auto-deselect "All Months" when specific months selected
  observeEvent(input$adv_month, {
    if ("All Months" %in% input$adv_month && length(input$adv_month) > 1) {
      updateSelectInput(session, "adv_month",
                       selected = setdiff(input$adv_month, "All Months"))
    }
  })

  observeEvent(input$cedars_file, {
    req(input$cedars_file)
    lang <- current_lang()
    tr <- get_tr()

    tryCatch({
      df <- withProgress(message = tr$t("Processing..."), value = 0, {
        path <- input$cedars_file$datapath
        filename <- input$cedars_file$name

        # Validate file type
        validate(need(
          grepl("\\.xlsx?$", tolower(filename)),
          tr$t("Invalid file format")
        ))

        # Auto-detect exposure data sheet
        incProgress(0.2, detail = "Finding exposure data sheet")
        exp_sheet_result <- find_sheet_by_columns(path, c("nationalid", "exposurecode", "hasexposureoccurred"))

        validate(need(
          exp_sheet_result$found,
          "Could not find sheet with required exposure columns (NationalID, ExposureCode, HasExposureOccurred)"
        ))

        # Read exposure data
        incProgress(0.1, detail = paste0("Reading: ", exp_sheet_result$sheet))
        df_exp <- tryCatch(
          readxl::read_excel(path, sheet = exp_sheet_result$sheet),
          error = function(e) NULL
        )

        validate(need(!is.null(df_exp) && nrow(df_exp) > 0, "Sheet is empty"))

        # Normalize column names
        names(df_exp) <- gsub("[^a-z0-9]+", "", tolower(names(df_exp)))

        df_exp <- df_exp %>%
          transmute(
            natid = as.character(.data$nationalid),
            exposure = as.character(.data$exposurecode),
            val = tolower(as.character(.data$hasexposureoccurred))
          )

        # Auto-detect linelist sheet (must NOT be the exposure sheet)
        incProgress(0.3, detail = "Finding linelist sheet")

        # Try multiple strategies to find the linelist sheet
        all_sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
        line_sheet_name <- NULL

        for (sheet in all_sheets) {
          if (sheet == exp_sheet_result$sheet) next  # Skip the exposure sheet

          sheet_data <- tryCatch(
            readxl::read_excel(path, sheet = sheet, n_max = 1),
            error = function(e) NULL
          )

          if (!is.null(sheet_data)) {
            cols <- gsub("[^a-z0-9]+", "", tolower(names(sheet_data)))
            if (("nationalid" %in% cols || "natid" %in% cols) && !("exposurecode" %in% cols)) {
              line_sheet_name <- sheet
              break
            }
          }
        }

        validate(need(
          !is.null(line_sheet_name),
          paste0("Could not find linelist sheet. Looking for sheet with NationalID/natid but without ExposureCode. Available sheets: ", paste(all_sheets, collapse = ", "))
        ))

        line_sheet_result <- list(sheet = line_sheet_name, found = TRUE)

        # Read linelist
        incProgress(0.1, detail = paste0("Reading: ", line_sheet_result$sheet))
        df_line <- tryCatch(
          readxl::read_excel(path, sheet = line_sheet_result$sheet),
          error = function(e) NULL
        )

        validate(need(!is.null(df_line) && nrow(df_line) > 0, "Linelist sheet is empty"))

        names(df_line) <- gsub("[^a-z0-9]+", "", tolower(names(df_line)))

        # Filter confirmed cases if CaseStatus column exists
        incProgress(0.2, detail = "Filtering confirmed cases")
        if ("casestatus" %in% names(df_line)) {
          df_line <- df_line %>% filter(tolower(as.character(.data$casestatus)) == "confirmed")
          validate(need(nrow(df_line) > 0, "No confirmed cases found"))
        }

        # Handle NationalID variations
        if (!"natid" %in% names(df_line)) {
          if ("nationalid" %in% names(df_line)) {
            df_line$natid <- as.character(df_line$nationalid)
          } else {
            validate(need(FALSE, "Missing NationalID column in linelist"))
          }
        }

        has_pt_col <- "provinceterritory" %in% names(df_line)

        df_line <- df_line %>%
          transmute(
            natid = as.character(.data$natid),
            provinceterritory = if (has_pt_col) .data$provinceterritory else NA_character_
          ) %>%
          distinct(natid, .keep_all = TRUE)

        # Merge (many exposures to one case)
        incProgress(0.2, detail = "Merging data")
        df <- df_exp %>% inner_join(df_line, by = "natid", relationship = "many-to-one")

        validate(need(nrow(df) > 0, "No matching cases found between sheets"))

        # Use enc2utf8 to ensure proper UTF-8 encoding in notification
        showNotification(
          enc2utf8(paste0(tr$t("Success"), ": ", length(unique(df$natid)), " ", tr$t("cases"))),
          type = "message"
        )

        df
      })

      adv_cases(df)
    }, error = function(e) {
      adv_cases(NULL)
      shinyjs::js$resetFileInput(id = "cedars_file")
      if (!inherits(e, "shiny.silent.error")) {
        showNotification(enc2utf8(paste(tr$t("Error"), ": ", e$message)), type = "error")
      }
    })
  }, ignoreNULL = TRUE)

  # Clear uploaded data
  observeEvent(input$cedars_clear, {
    adv_cases(NULL)
    shinyjs::js$resetFileInput(id = "cedars_file")

    lang <- current_lang()
    tr <- get_tr()

    updateSelectInput(session, "adv_province",
                     choices = build_province_choices(lang),
                     selected = "Canada")

    updateSelectInput(session, "adv_age_group",
                     choices = stats::setNames(
                       c("All Ages", fb_age_groups()),
                       c(tr$t("All Ages"), fb_age_groups())
                     ),
                     selected = "All Ages")

    updateSelectInput(session, "adv_month",
                     choices = stats::setNames(
                       c("All Months", as.character(1:12)),
                       c(tr$t("All Months"), fb_month_names(lang))
                     ),
                     selected = "All Months")

    # Use enc2utf8 to ensure proper UTF-8 encoding in notification
    showNotification(enc2utf8(tr$t("Upload cleared")), type = "message")
  })

  # Get unique PTs from uploaded cases
  case_pts <- reactive({
    d <- adv_cases()
    if (is.null(d) || !"provinceterritory" %in% names(d)) {
      return(NULL)
    }

    unique_pts <- unique(d$provinceterritory[!is.na(d$provinceterritory)])
    if (length(unique_pts) == 0) return(NULL)

    unique_pts
  })

  # Update PT filter choices when cases are uploaded
  observeEvent(adv_cases(), {
    lang <- current_lang()

    current_cases <- adv_cases()
    if (!is.null(current_cases) && "provinceterritory" %in% names(current_cases)) {
      pts <- case_pts()
      if (!is.null(pts) && length(pts) > 0) {
        updateSelectInput(session, "adv_province",
                         choices = build_province_choices(lang, pts),
                         selected = pts)
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # Calculate results
  adv_results <- reactive({
    d <- adv_cases()
    if (is.null(d)) return(NULL)
    lang <- current_lang()
    tr <- get_tr()

    withProgress(message = tr$t("Processing..."), value = 0, {
      # Get PT filter
      pts_selected <- input$adv_province
      if (is.null(pts_selected)) {
        pts_selected <- character(0)
      }

      pts_for_backend <- pts_selected

      # Filter cases by PT if specific PT(s) selected
      incProgress(0.1, detail = "Filtering cases by PT")
      d_filtered <- d
      if ("provinceterritory" %in% names(d) && !is.null(pts_selected)) {
        if (!("Canada" %in% pts_selected)) {
          d_filtered <- d %>% filter(is.na(provinceterritory) | provinceterritory %in% pts_for_backend)
          validate(need(nrow(d_filtered) > 0, tr$t("No cases found for selected province(s)")))
        }
      }

      # Get unique PTs from filtered cases for Reference Scope
      if ("provinceterritory" %in% names(d_filtered)) {
        unique_case_pts <- unique(d_filtered$provinceterritory[!is.na(d_filtered$provinceterritory)])
        if (length(unique_case_pts) == 0) {
          reference_scope <- "Canada"
          pts_for_reference <- "Canada"
        } else {
          reference_scope_labels <- build_province_display_map(lang)[unique_case_pts]
          reference_scope_labels[is.na(reference_scope_labels)] <- unique_case_pts[is.na(reference_scope_labels)]
          reference_scope <- paste(reference_scope_labels, collapse = ", ")
          pts_for_reference <- unique_case_pts
        }
      } else {
        reference_scope <- "Canada"
        pts_for_reference <- "Canada"
      }
      if (identical(reference_scope, "Canada")) {
        reference_scope <- tr$t("Canada")
      }

      # Summarise counts by exposure code (count unique cases, not rows)
      incProgress(0.3, detail = "Summarizing exposure counts")
      exposure_counts <- d_filtered %>%
        mutate(val = dplyr::recode(val, y = "Y", n = "N", p = "P", dk = "DK")) %>%
        filter(val %in% c("Y", "N", "P", "DK")) %>%
        distinct(natid, exposure, val) %>%  # Ensure one response per case per exposure
        count(exposure, val) %>%
        tidyr::pivot_wider(names_from = val, values_from = n, values_fill = 0)

      codes <- exposure_counts$exposure

      ages <- if (is.null(input$adv_age_group) || (length(input$adv_age_group) == 1 && "All Ages" %in% input$adv_age_group)) NULL else input$adv_age_group
      months <- if (is.null(input$adv_month) || (length(input$adv_month) == 1 && "All Months" %in% input$adv_month)) NULL else as.integer(input$adv_month)

      incProgress(0.4, detail = "Computing reference percentages")
      ref_perc <- fb_reference_percents(codes, pt_names = pts_for_reference, months = months, age_groups = ages)

      # Map codes to labels for display - use fb_exposure_choices_all() to include legacy CEDARS codes
      code_to_label <- names(fb_exposure_choices_all(lang))
      names(code_to_label) <- as.vector(fb_exposure_choices_all(lang))

      incProgress(0.3, detail = "Computing statistical tests")
      results <- exposure_counts %>%
        rowwise() %>%
        mutate(
          Exposure = sub(" \\([^)]+\\)$", "", code_to_label[exposure] %||% exposure),
          province_ref = as.numeric(ref_perc[match(exposure, names(ref_perc))]),
          y_plus_p = (Y %||% 0) + (P %||% 0),
          total = y_plus_p + (N %||% 0),
          observed_prop = if (total > 0) y_plus_p / total else NA_real_,
          p_value = if (total > 0) pbinom(y_plus_p - 1, total, province_ref / 100, lower.tail = FALSE) else NA_real_,
          Classification = classify_exposure(p_value, observed_prop, province_ref)
        ) %>%
        ungroup() %>%
        mutate(`Reference %` = round(province_ref, 1)) %>%
        transmute(
          `Reference Scope` = reference_scope,
          Exposure,
          `Total Valid` = total,
          Yes = Y %||% 0,
          Probably = P %||% 0,
          No = N %||% 0,
          DK = DK %||% 0,
          `Observed %` = observed_prop,
          `Reference %`,
          `P-Value` = p_value,
          Classification
        )

      # Translate classifications
      results <- results %>%
        mutate(Classification = purrr::map_chr(Classification, ~classification_label_i18n(., lang)))

      results
    })
  })

  # Render results table
  # Render results table container (with empty state support)
  output$adv_results_table_container <- renderUI({
    lang <- current_lang()
    tr <- get_tr()

    res <- adv_results()

    # Show empty state if no CEDARS file uploaded
    if (is.null(res)) {
      return(div(
        class = "empty-state",
        style = "text-align: center; padding: 4rem 2rem; color: #6c757d;",
        div(
          icon("file-excel", style = "font-size: 4rem; opacity: 0.3; margin-bottom: 1rem;")
        ),
        h4(tr$t("No data available"), style = "margin-bottom: 0.5rem;"),
        p(tr$t("Upload a CEDARS file to begin your analysis"))
      ))
    }

    # Otherwise, render the table
    tagList(
      DTOutput("adv_results_table", width = "100%"),
      helpText(tr$t("*Data available from Foodbook 1.0 only"), style = "font-size: 0.8rem; margin-top: 0.5rem; color: #6c757d;")
    )
  })

  output$adv_results_table <- renderDT(server = FALSE, {
    res <- adv_results()
    lang <- current_lang()
    tr <- get_tr()

    if (is.null(res)) {
      return(NULL)
    }

    res <- res %>%
      # Filter out rows with missing exposure names
      filter(!is.na(Exposure), Exposure != "") %>%
      # Create sort key for p-values: real numbers first (ascending), then "-" at end
      mutate(
        p_value_sort = if_else(is.na(`P-Value`), 999, `P-Value`),
        `Observed %` = round(`Observed %` * 100, 1),
        `Reference %` = if_else(is.na(`Reference %`), "-", as.character(`Reference %`)),
        `P-Value` = if_else(is.na(`P-Value`), "-", as.character(round(`P-Value`, 4)))
      ) %>%
      # Sort by p-value: lowest first, then "-" values at end
      arrange(p_value_sort) %>%
      select(-p_value_sort)

    # Build filename
    pts <- input$adv_province %||% "Canada"
    pt_str <- if (length(pts) == 1) gsub(" ", "", pts[1]) else paste0(length(pts), "PTs")
    n_exp <- nrow(res)
    filename <- paste0("cedars_results_", pt_str, "_", n_exp, "exp_", Sys.Date())

    datatable(
      res,
      options = list(
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = filename),
          'copy', 'print'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      colnames = c(
        tr$t("Reference Scope"),
        tr$t("Exposure"),
        tr$t("Total Valid"),
        tr$t("Yes"),
        tr$t("Probably"),
        tr$t("No"),
        tr$t("DK"),
        tr$t("Observed %"),
        tr$t("Reference %"),
        tr$t("P-Value"),
        tr$t("Classification")
      )
    ) %>%
      formatStyle(
        "Classification",
        backgroundColor = styleEqual(
          c(tr$t("Alert"), tr$t("Borderline"),
            tr$t("Not Significant"), tr$t("Insufficient Data"),
            tr$t("No Reference Value")),
          c("#fde4e6", "#fff4d6", "#edf2ff", "#f1f5f9", "#e2e8f0")
        ),
        color = styleEqual(
          c(tr$t("Alert"), tr$t("Borderline"),
            tr$t("Not Significant"), tr$t("Insufficient Data"),
            tr$t("No Reference Value")),
          c("#b82c3a", "#b35c00", "#1f2933", "#475569", "#64748b")
        ),
        fontWeight = "600"
      )
  })

  # Reference filter helpers (for Data Info tab)
  ref_filters <- reactive({
    all_ages_labels <- unique(c("All Ages", t_("All Ages", lang = "en"), t_("All Ages", lang = "fr")))
    all_months_labels <- unique(c("All Months", t_("All Months", lang = "en"), t_("All Months", lang = "fr")))
    list(
      pts = input$adv_province,
      ages = if (is.null(input$adv_age_group) || (length(input$adv_age_group) == 1 && any(all_ages_labels %in% input$adv_age_group))) NULL else input$adv_age_group,
      months = if (is.null(input$adv_month) || (length(input$adv_month) == 1 && any(all_months_labels %in% input$adv_month))) NULL else as.integer(input$adv_month)
    )
  })

  fb_filtered <- reactive({
    req(backend_ok)
    f <- ref_filters()
    fb_filter_micro(pt_names = f$pts, months = f$months, age_groups = f$ages)
  })

  # Data Info tab card headers (reactive for translation)
  output$ref_settings_header <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    tr$t("Reference Settings")
  })

  output$ref_snapshot_header <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    tr$t("Population Exposure Snapshot (Reference)")
  })

  output$ref_pt_header <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    tr$t("Microdata Coverage by PT (after filters)")
  })

  output$ref_month_header <- renderUI({
    lang <- current_lang()
    tr <- get_tr()
    tr$t("Microdata Coverage by Month (after filters)")
  })

  # Data Info tab outputs
  output$ref_summary_ui <- renderUI({
    lang <- current_lang()
    tr <- get_tr()

    f <- ref_filters()
    pts <- f$pts %||% tr$t("Canada")
    ages <- f$ages %||% tr$t("All Ages")

    # Get month names in current language
    if (is.null(f$months)) {
      months <- tr$t("All Months")
    } else {
      month_names <- fb_month_names(lang)
      months <- paste(month_names[as.integer(f$months)], collapse = ", ")
    }

    tagList(
      p(tr$t("This app computes reference exposure percentages from Foodbook microdata, weighted and combined across your selected filters.")),
      tags$ul(
        tags$li(tags$b(tr$t("Reference PT(s): ")), paste(pts, collapse = ", ")),
        tags$li(tags$b(tr$t("Age group(s): ")), paste(ages, collapse = ", ")),
        tags$li(tags$b(tr$t("Month(s): ")), months)
      ),
      p(tr$t("Tip: Defaults like \"Canada\" and \"All\" auto-deselect once you add another selection."))
    )
  })

  output$ref_top_exposures <- renderDT({
    req(backend_ok)
    lang <- current_lang()
    f <- ref_filters()

    codes <- as.vector(fb_exposure_choices(lang))
    refs <- fb_reference_percents(codes, pt_names = f$pts, months = f$months, age_groups = f$ages)
    lbls <- names(fb_exposure_choices(lang))
    names(lbls) <- as.vector(fb_exposure_choices(lang))

    tibble::tibble(
      Exposure = lbls[names(refs)],
      `Reference %` = round(as.numeric(refs), 1)
    ) %>%
      arrange(desc(`Reference %`)) %>%
      head(30) %>%
      datatable(options = list(pageLength = 10, order = list(list(1, 'desc'))), rownames = FALSE)
  })

  output$ref_pt_plot <- renderPlot({
    d <- fb_filtered()
    req(nrow(d) > 0)
    lang <- current_lang()
    tr <- get_tr()

    pt_map <- fb_pt_names(lang)
    # invert mapping names->codes to codes->names
    codes <- unname(fb_pt_map())
    names(codes) <- names(fb_pt_map())
    inv <- stats::setNames(names(codes), codes)

    tibble::tibble(PT = d$PT) %>%
      mutate(PT = inv[as.character(PT)] %||% PT) %>%
      count(PT) %>%
      ggplot(aes(x = reorder(PT, n), y = n)) +
      geom_col(fill = "#0f4c81", alpha = 0.85) +
      coord_flip() +
      labs(title = tr$t("Coverage by PT (after filters)"), x = NULL, y = tr$t("Records")) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13)
      )
  })

  output$ref_month_plot <- renderPlot({
    d <- fb_filtered()
    req(nrow(d) > 0)
    lang <- current_lang()
    tr <- get_tr()

    month_names_display <- if (lang == "fr") fb_month_names("fr") else month.name

    tibble::tibble(Month = as.integer(d$Month)) %>%
      filter(!is.na(Month), Month >= 1, Month <= 12) %>%
      mutate(MonthName = factor(month_names_display[Month], levels = month_names_display)) %>%
      count(MonthName) %>%
      ggplot(aes(x = MonthName, y = n)) +
      geom_col(fill = "#1b7b57", alpha = 0.85) +
      labs(title = tr$t("Coverage by Month (after filters)"), x = NULL, y = tr$t("Records")) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  # About page content (reactive for translation)
  output$about_content <- renderUI({
    lang <- current_lang()
    tr <- get_tr()

    tagList(
      # Purpose/Methodology
      h4(tr$t("Methodology")),
      p(tr$t("This tool compares observed case exposures against Foodbook reference percentages using statistical significance testing.")),

      hr(),

      # CEDARS Upload Workflow
      h4(tr$t("CEDARS Upload Workflow")),
      p(tr$t("This internal tool is designed for PHAC epidemiologists to analyze outbreak data exported from CEDARS.")),
      tags$ol(
        tags$li(tr$t("Export case exposure data from CEDARS to Excel (.xlsx)")),
        tags$li(tr$t("Upload the file using the file input")),
        tags$li(tr$t("The app auto-detects the required sheets (case exposure answer, case linelist)")),
        tags$li(tr$t("Select reference population filters (PT, age, month)")),
        tags$li(tr$t("View results with statistical testing and classifications"))
      ),

      hr(),

      # Data Sources
      h4(tr$t("Data Sources")),
      p(tr$t("Foodbook is a population-based survey conducted in all Canadian provinces and territories. It provides essential data on food, animal and water exposure used to understand, respond to, control and prevent enteric illness in Canada.")),
      tags$ul(
        tags$li(
          strong(tr$t("Foodbook 2.0 (2023-2024)")), ": ",
          tr$t("Online and telephone survey with ~21,000 respondents across Canada")
        ),
        tags$li(
          strong(tr$t("Foodbook 1.0 (2014-2015)")), ": ",
          tr$t("Telephone survey with ~10,000 respondents (exposures marked with * are from this survey only)")
        )
      ),

      hr(),

      # Statistical Methodology
      h4(tr$t("Statistical Methodology")),
      p(tr$t("The tool uses a one-sided binomial test to compare observed case exposure rates against population reference values:")),
      tags$ul(
        tags$li(tr$t("Null hypothesis: Case exposure rate ≤ Population reference rate")),
        tags$li(tr$t("Alternative hypothesis: Case exposure rate > Population reference rate"))
      ),
      p(tr$t("Reference percentages are calculated using survey weights to ensure population representativeness.")),

      hr(),

      # Interpretation Guide
      h4(tr$t("Interpretation Guide")),
      tags$ul(
        tags$li(strong(tr$t("Alert")), ": ", tr$t("Observed exposure is significantly higher than reference (p < 0.05)")),
        tags$li(strong(tr$t("Borderline")), ": ", tr$t("Suggestive evidence (0.05 ≤ p < 0.10)")),
        tags$li(strong(tr$t("Not Significant")), ": ", tr$t("No significant difference from reference (p ≥ 0.10)")),
        tags$li(strong(tr$t("Insufficient Data")), ": ", tr$t("Too few cases to calculate statistics (< 5 total responses)")),
        tags$li(strong(tr$t("No Reference Value")), ": ", tr$t("Exposure not found in Foodbook database"))
      ),

      hr(),

      # Limitations
      h4(tr$t("Limitations")),
      tags$ul(
        tags$li(tr$t("Survey data may not reflect current food consumption patterns (data collected in 2014-2015 and 2023-2024)")),
        tags$li(tr$t("Self-reported exposure data is subject to recall bias")),
        tags$li(tr$t("Some exposures may have seasonal variations not captured when using annual data")),
        tags$li(tr$t("Small sample sizes in specific PT/age/month combinations may yield unstable estimates")),
        tags$li(tr$t("Exposures from Foodbook 1.0 (*) use different survey weights than Foodbook 2.0"))
      ),

      hr(),

      # FAQ
      h4(tr$t("Frequently Asked Questions")),
      tags$div(
        class = "faq-section",
        tags$p(strong(tr$t("Why is my exposure showing 'No Reference Value'?"))),
        tags$p(tr$t("This means the exposure was not asked in either Foodbook survey, or the variable name doesn't match. Try searching for a similar exposure name."), style = "margin-bottom: 1rem;"),

        tags$p(strong(tr$t("What does the * mean next to some exposures?"))),
        tags$p(tr$t("Exposures marked with * are only available from Foodbook 1.0 (2014-2015). They are included for completeness but may not reflect current consumption patterns."), style = "margin-bottom: 1rem;"),

        tags$p(strong(tr$t("Why do reference values change when I select different PTs?"))),
        tags$p(tr$t("Food consumption varies by region. The reference is recalculated using only respondents from the selected province(s)/territory(ies)."), style = "margin-bottom: 1rem;"),

        tags$p(strong(tr$t("How should I interpret 'Borderline' results?"))),
        tags$p(tr$t("Borderline results (p-value between 0.05 and 0.10) suggest a possible association that warrants further investigation but doesn't meet conventional significance thresholds."))
      ),

      hr(),

      # Links
      h4(tr$t("Useful Links")),
      tags$ul(
        tags$li(tags$a(
          href = "https://health-infobase.canada.ca/foodbook/about.html",
          target = "_blank",
          "About Foodbook (Health Infobase)"
        )),
        tags$li(tags$a(
          href = "https://www.canada.ca/en/public-health/services/publications/food-nutrition/foodbook-report-2.html",
          target = "_blank",
          "Foodbook 2.0 Report"
        )),
        tags$li(tags$a(
          href = "https://open.canada.ca/data/en/dataset/1efcd118-a3df-4cd0-86ae-e4233386b0c6",
          target = "_blank",
          "Foodbook 2.0 Microdata (Open Canada)"
        )),
        tags$li(tags$a(
          href = "https://www.canada.ca/en/public-health/services/publications/food-nutrition/foodbook-report.html",
          target = "_blank",
          "Foodbook 1.0 Report"
        )),
        tags$li(tags$a(
          href = "https://open.canada.ca/data/en/dataset/ddf6c129-2698-422a-abb5-f7465ed549ee",
          target = "_blank",
          "Foodbook 1.0 Microdata (Open Canada)"
        ))
      ),

      hr(),

      # Contact
      h4(tr$t("Contact")),
      p(tr$t("For questions or support, please contact:")),
      p(tags$code(tr$t("[Contact email placeholder]")))
    )
  })
}

# --- 6. Run Application ---
shinyApp(ui, server, enableBookmarking = "url")





