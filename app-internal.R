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

# Load backend and modules
source("src/foodbook_backend.R")
source("src/i18n_helper.R")
source("src/modules/language_selector_module.R")

# Suppress warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
options(bslib.precompiled = TRUE)

# --- 2. Initialize Backend and Translator ---
translator <- Translator$new(translation_json_path = "translations/translation.json")
translator$set_translation_language("en")

# Initialize backend (will use Open Canada + legacy if available)
fb_init(lang = "en")
backend_ok <- tryCatch(fb_is_available(), error = function(e) FALSE)

# --- 3. Helper Functions ---
classify_exposure <- function(p_value, observed_prop, ref_prop) {
  if (is.na(ref_prop)) return("No Reference Value")
  ref_prop_decimal <- ref_prop / 100
  if (is.na(p_value)) return("Insufficient Data")
  if (observed_prop > ref_prop_decimal) {
    case_when(p_value <= 0.05 ~ "Alert",
              p_value <= 0.10 ~ "Borderline",
              TRUE ~ "Not Significant")
  } else {
    "Not Significant"
  }
}

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
      bs_add_rules("
        body {
          background: linear-gradient(135deg, #eef2f8 0%, #fdfdfd 60%);
        }
        .navbar {
          border-bottom: 1px solid #d7e3f7;
          background-color: rgba(255, 255, 255, 0.92);
          backdrop-filter: blur(6px);
        }
        .navbar-brand {
          color: #0f4c81 !important;
          font-weight: 700;
          letter-spacing: 0.02em;
        }
        .nav-link {
          color: #4b5563 !important;
          border-radius: 999px;
          padding: 0.6rem 1.1rem;
          margin: 0 0.3rem;
        }
        .nav-link:hover {
          color: #0f4c81 !important;
          background-color: rgba(15, 76, 129, 0.08);
        }
        .nav-link.active {
          color: #0f4c81 !important;
          background-color: rgba(15, 76, 129, 0.14) !important;
          box-shadow: inset 0 -3px 0 #0f4c81;
        }
        .bslib-sidebar-layout .sidebar {
          background: #ffffff;
          border-right: 1px solid #dde6f5;
          box-shadow: 4px 0 24px rgba(15, 76, 129, 0.08);
        }
        .bslib-sidebar-layout .sidebar .title {
          color: #0f4c81;
          font-weight: 600;
        }
        .sidebar hr {
          border-color: #d0ddf0;
        }
        .btn-primary {
          background: linear-gradient(135deg, #1160aa 0%, #0b4a86 100%);
          border: none;
          box-shadow: 0 12px 24px rgba(15, 76, 129, 0.18);
        }
        .btn-primary:hover {
          background: linear-gradient(135deg, #0b4a86 0%, #073866 100%);
        }
        .btn-warning {
          background: linear-gradient(135deg, #f7b733 0%, #f59e0b 100%);
          border: none;
          color: #1f2933;
          box-shadow: 0 12px 24px rgba(245, 158, 11, 0.22);
        }
        .btn-warning:hover {
          background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%);
        }
        .btn-secondary {
          background: linear-gradient(135deg, #4b5563 0%, #364152 100%);
          border: none;
          color: #f9fafb;
        }
        .card {
          border: 1px solid #dde6f5;
          box-shadow: 0 12px 30px rgba(15, 76, 129, 0.12);
        }
        .card-header {
          background: linear-gradient(135deg, #f7faff 0%, #ecf2ff 100%);
          border-bottom: 1px solid #d0ddf0;
          font-weight: 600;
          color: #0f4c81;
        }
        .nav-tabs .nav-link {
          color: #4b5563 !important;
        }
        .nav-tabs .nav-link.active {
          color: #0f4c81 !important;
          background-color: #e8f1ff !important;
          border-color: #e8f1ff #e8f1ff #ffffff;
        }
        .selectize-input {
          border: 2px solid #d0ddf0;
          border-radius: 0.65rem;
          min-height: 44px;
          font-weight: 500;
          transition: all 0.2s ease;
        }
        .selectize-input.focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.18);
        }
        .selectize-dropdown-content .option.active {
          background: #e8f1ff;
          color: #0f4c81;
        }
        .sidebar .form-group > label {
          font-weight: 600;
          color: #334155;
        }
        .sidebar .selectize-input,
        .sidebar select.form-control {
          border-radius: 0.65rem;
          border: 1px solid #d0ddf0;
          min-height: 44px;
        }
        .sidebar .selectize-input.focus,
        .sidebar select.form-control:focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.18);
          outline: none;
        }
        .dataTables_wrapper .dt-buttons .btn {
          background: #0f4c81;
          color: #ffffff;
          border: none;
          border-radius: 0.5rem;
          box-shadow: 0 8px 18px rgba(15, 76, 129, 0.18);
        }
        .dataTables_wrapper .dt-buttons .btn:hover {
          background: #0b3a67;
        }
        .dataTables_wrapper .dataTables_filter input {
          border-radius: 0.5rem;
          border: 1px solid #d0ddf0;
          padding: 0.45rem 0.75rem;
        }
        .well-panel-about h4 {
          color: #0f4c81;
          font-weight: 600;
        }
        .well-panel-about li {
          margin-bottom: 0.55rem;
          line-height: 1.5;
        }
      "),
    header = tagList(
      useShinyjs(),
      tags$head(
        tags$script(HTML("
          // Inject language selector into navbar on page load
          $(document).ready(function() {
            setTimeout(function() {
              var langContainer = $('#lang_selector_container');
              console.log('Found lang container:', langContainer.length);

              if (langContainer.length > 0) {
                // Find the form-group div inside the container
                var formGroup = langContainer.find('.form-group').first();
                console.log('Found form group:', formGroup.length);

                if (formGroup.length > 0) {
                  // Create wrapper and append to navbar
                  var wrapper = $('<div class=\"language-selector-wrapper\"></div>');
                  $('nav.navbar').first().css('position', 'relative').append(wrapper);

                  // Move the entire form-group to the wrapper and make visible
                  formGroup.appendTo(wrapper);
                  formGroup.css('display', 'block');
                  wrapper.css('display', 'block');

                  console.log('Language selector moved to navbar');
                }
              }
            }, 500);
          });

          // Custom message handler for updating button labels
          Shiny.addCustomMessageHandler('update-button-labels', function(labels) {
            $('#download_plot').text(labels.download);
          });

          // Custom message handler for updating tab names
          Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
            // Update main nav tabs
            $('a.nav-link').each(function() {
              var $icon = $(this).find('i');
              var iconHtml = $icon.length ? $icon.prop('outerHTML') + ' ' : '';
              var text = $(this).text().trim();

              if (text === 'CEDARS Analysis' || text === 'Analyse SCEDAC') {
                $(this).html(iconHtml + labels.cedars);
              } else if (text === 'Data Info' || text === 'Info sur les données') {
                $(this).html(iconHtml + labels.data_info);
              } else if (text === 'About' || text === 'À propos') {
                $(this).html(iconHtml + labels.about);
              }
            });

            // Update inner nav tabs (Results/Visualization)
            $('.nav-tabs .nav-link').each(function() {
              var text = $(this).text().trim();
              if (text === 'Results' || text === 'Résultats') {
                $(this).text(labels.results);
              } else if (text === 'Visualization' || text === 'Visualisation') {
                $(this).text(labels.visualization);
              }
            });
          });

          // Custom message handler for updating sidebar title
          Shiny.addCustomMessageHandler('update-sidebar-title', function(title) {
            $('.bslib-sidebar-layout .sidebar h4, .bslib-sidebar-layout .sidebar h5').each(function() {
              var text = $(this).text();
              if (text.includes('Upload') || text.includes('Télécharger')) {
                $(this).text(title.upload);
              } else if (text.includes('Analysis Parameters') || text.includes('Paramètres')) {
                $(this).text(title.parameters);
              }
            });
          });

          // Custom message handler for updating misc labels (file inputs, help text, etc)
          Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
            // Update file input labels
            $('.form-label').each(function() {
              var text = $(this).text().trim();
              if (text.includes('Choose Excel file') || text.includes('Choisir un fichier Excel')) {
                $(this).text(labels.choose_excel);
              }
            });

            // Update help text
            $('span.help-block, span.form-text, .shiny-input-container .help-block').each(function() {
              var text = $(this).text().trim();
              if (text.includes('auto-detect sheets') || text.includes('détectera automatiquement')) {
                $(this).text(labels.auto_detect_help);
              }
            });

            // Update Browse button
            $('.btn-file, .form-control[type=\\'file\\'] + .btn').each(function() {
              if ($(this).text().includes('Browse') || $(this).text().includes('Parcourir')) {
                $(this).text(labels.browse);
              }
            });
          });
        ")),
        tags$style(HTML("
        /* Ensure navbar is above all content */
        nav.navbar {
          position: relative;
          z-index: 9000 !important;
        }

        /* Main page content should be below navbar */
        .tab-content {
          position: relative;
          z-index: 1;
        }

        .language-selector-wrapper {
          position: absolute;
          top: 10px;
          right: 20px;
          z-index: 10000;
        }
        .language-selector-wrapper .form-group {
          margin: 0;
          position: relative;
          z-index: 10000;
        }
        .language-selector-wrapper label {
          display: none;
        }
        .language-selector-wrapper select {
          padding: 0.4rem 0.8rem;
          border-radius: 0.5rem;
          border: 1px solid #d0ddf0;
          background: white;
          color: #4b5563;
          font-weight: 500;
          font-size: 0.9rem;
          cursor: pointer;
          box-shadow: 0 2px 8px rgba(15, 76, 129, 0.15);
          transition: all 0.2s;
          position: relative;
          z-index: 10000;
        }
        .language-selector-wrapper select:hover {
          background: #f1f5ff;
          border-color: #0f4c81;
          color: #0f4c81;
        }
        .language-selector-wrapper select:focus {
          outline: none;
          border-color: #0f4c81;
          box-shadow: 0 0 0 3px rgba(15, 76, 129, 0.1);
        }
        "))
      ),
      # Hidden language selector (will be moved to navbar by JavaScript)
      tags$div(id = "lang_selector_container", style = "display: none;",
          language_selector_ui("lang_selector", style = "dropdown"))
    ),

    # CEDARS Upload Tab
    nav_panel(
      title = translator$t("CEDARS Analysis"),
      icon = icon("upload"),

      layout_sidebar(
        fillable = FALSE,

        sidebar = sidebar(
          width = 350,

          h4(translator$t("Upload CEDARS Exposure Data")),

          tooltip(
            fileInput("cedars_file",
                     translator$t("Choose Excel file..."),
                     accept = c(".xlsx")),
            translator$t("Upload your CEDARS outbreak data export. The tool will automatically extract case exposure information.")
          ),

          helpText(translator$t("The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist).")),

          hr(),

          h5(translator$t("Analysis Parameters")),

          tooltip(
            selectInput("adv_province",
                       translator$t("Reference PT(s)"),
                       choices = c(translator$t("Canada"), fb_pt_names()),
                       selected = translator$t("Canada"),
                       multiple = TRUE),
            translator$t("Select one or more provinces/territories for the reference population. Use Ctrl+Click to select multiple.")
          ),

          tooltip(
            selectInput("adv_age_group",
                       translator$t("Restrict by Age Group"),
                       choices = c(translator$t("All Ages"), fb_age_groups()),
                       selected = translator$t("All Ages"),
                       multiple = TRUE),
            translator$t("Optionally filter the reference population by age group.")
          ),

          tooltip(
            selectInput("adv_month",
                       translator$t("Restrict by Month"),
                       choices = c(translator$t("All Months"),
                                 stats::setNames(1:12, fb_month_names())),
                       selected = translator$t("All Months"),
                       multiple = TRUE),
            translator$t("Optionally filter the reference population by survey month.")
          )
        ),

        # Main panel
        card(
          card_header(translator$t("Results")),
          card_body(
            withSpinner(
              DTOutput("adv_results_table", width = "100%"),
              type = 4,
              color = "#0f4c81"
            )
          )
        )
      )
    ),

    # Data Info Tab
    nav_panel(
      title = translator$t("Data Info"),
      icon = icon("database"),

      card(
        card_header(translator$t("Data Source")),
        card_body(
          div(
            h5(translator$t("Reference Population Statistics")),
            p(strong(translator$t("Data Source:")),
              if (backend_ok) fb_env$data_source else "No microdata"),
            p(strong(translator$t("Total Respondents:")),
              if (backend_ok && !is.null(fb_env$micro)) nrow(fb_env$micro) else "N/A"),
            p(strong(translator$t("Available Exposures:")),
              if (backend_ok) length(fb_exposure_choices()) else "N/A"),
            hr(),
            p(translator$t("This tool uses public Foodbook data from Open Canada (Foodbook 1 and Foodbook 2).")),
            p("For internal use, legacy microdata from upgrade-context/ is also supported if available.")
          )
        )
      )
    ),

    # About Tab
    nav_panel(
      title = translator$t("About"),
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

  # Language selector
  lang_state <- language_selector_server("lang_selector",
                                         session_parent = session,
                                         translator = translator)

  current_lang <- lang_state$language

  # Update UI when language changes
  observeEvent(current_lang(), {
    lang <- current_lang()
    translator$set_translation_language(lang)

    # Re-initialize backend
    fb_env$initialised <- NULL
    fb_init(lang = lang)

    # Preserve current selections by converting to appropriate format
    current_prov <- input$adv_province
    current_age <- input$adv_age_group
    current_month <- input$adv_month

    # If default values selected, update them to new language
    if (!is.null(current_prov) && ("Canada" %in% current_prov)) {
      current_prov[current_prov == "Canada"] <- translator$t("Canada")
    }
    if (!is.null(current_age) && ("All Ages" %in% current_age || "Tous les âges" %in% current_age)) {
      current_age[current_age == "All Ages" | current_age == "Tous les âges"] <- translator$t("All Ages")
    }
    if (!is.null(current_month) && ("All Months" %in% current_month || "Tous les mois" %in% current_month)) {
      current_month[current_month == "All Months" | current_month == "Tous les mois"] <- translator$t("All Months")
    }

    # Update all select inputs with new labels
    updateSelectInput(session, "adv_province",
                     label = translator$t("Reference PT(s)"),
                     choices = c(translator$t("Canada"), fb_pt_names(lang)),
                     selected = current_prov)

    updateSelectInput(session, "adv_age_group",
                     label = translator$t("Restrict by Age Group"),
                     choices = c(translator$t("All Ages"), fb_age_groups()),
                     selected = current_age)

    # Update month selector with translated month names from backend
    month_choices <- c(translator$t("All Months"), stats::setNames(1:12, fb_month_names(lang)))
    updateSelectInput(session, "adv_month",
                     label = translator$t("Restrict by Month"),
                     choices = month_choices,
                     selected = current_month)

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

    # Update sidebar titles via JavaScript
    session$sendCustomMessage("update-sidebar-title", list(
      upload = translator$t("Upload CEDARS Exposure Data"),
      parameters = translator$t("Analysis Parameters")
    ))

    # Update misc labels via JavaScript
    session$sendCustomMessage("update-misc-labels", list(
      choose_excel = translator$t("Choose Excel file..."),
      auto_detect_help = translator$t("The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist)."),
      browse = translator$t("Browse")
    ))

    session$sendCustomMessage("language_changed", lang)
  }, ignoreInit = TRUE)

  # Auto-deselect "Canada" when specific PTs selected
  observeEvent(input$adv_province, {
    if (translator$t("Canada") %in% input$adv_province && length(input$adv_province) > 1) {
      updateSelectInput(session, "adv_province",
                       selected = setdiff(input$adv_province, translator$t("Canada")))
    }
  })

  # Auto-deselect "All Ages" when specific ages selected
  observeEvent(input$adv_age_group, {
    if (translator$t("All Ages") %in% input$adv_age_group && length(input$adv_age_group) > 1) {
      updateSelectInput(session, "adv_age_group",
                       selected = setdiff(input$adv_age_group, translator$t("All Ages")))
    }
  })

  # Auto-deselect "All Months" when specific months selected
  observeEvent(input$adv_month, {
    if (translator$t("All Months") %in% input$adv_month && length(input$adv_month) > 1) {
      updateSelectInput(session, "adv_month",
                       selected = setdiff(input$adv_month, translator$t("All Months")))
    }
  })

  # Process CEDARS upload
  adv_cases <- reactive({
    req(input$cedars_file)

    withProgress(message = translator$t("Processing..."), value = 0, {
      path <- input$cedars_file$datapath
      filename <- input$cedars_file$name

      # Validate file type
      validate(need(
        grepl("\\.xlsx?$", tolower(filename)),
        translator$t("Invalid file format")
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

      # Auto-detect linelist sheet
      incProgress(0.3, detail = "Finding linelist sheet")
      line_sheet_result <- find_sheet_by_columns(path, c("nationalid"))

      validate(need(line_sheet_result$found, "Could not find linelist sheet"))

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

      df_line <- df_line %>%
        transmute(
          natid = as.character(.data$natid),
          provinceterritory = if("provinceterritory" %in% names(.)) .data$provinceterritory else NA_character_
        ) %>%
        distinct(natid, .keep_all = TRUE)  # Ensure one row per case

      # Merge (many exposures to one case)
      incProgress(0.2, detail = "Merging data")
      df <- df_exp %>% inner_join(df_line, by = "natid", relationship = "many-to-one")

      validate(need(nrow(df) > 0, "No matching cases found between sheets"))

      showNotification(
        paste0(translator$t("Success"), ": ", length(unique(df$natid)), " ", translator$t("cases")),
        type = "message"
      )

      df
    })
  })

  # Get unique PTs from uploaded cases
  case_pts <- reactive({
    d <- adv_cases()

    if (!"provinceterritory" %in% names(d)) {
      return(NULL)
    }

    unique_pts <- unique(d$provinceterritory[!is.na(d$provinceterritory)])
    if (length(unique_pts) == 0) return(NULL)

    # Return PT names in current language
    lang <- current_lang()
    fb_pt_names(lang)[fb_pt_names("en") %in% unique_pts]
  })

  # Update PT filter choices when cases are uploaded
  observeEvent(adv_cases(), {
    req(adv_cases())
    lang <- current_lang()

    if ("provinceterritory" %in% names(adv_cases())) {
      pts <- case_pts()
      if (!is.null(pts) && length(pts) > 0) {
        # Set choices to PTs from uploaded cases
        updateSelectInput(session, "adv_province",
                         choices = c(translator$t("Canada"), pts),
                         selected = pts)  # Default to all PTs from upload
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # Calculate results
  adv_results <- reactive({
    d <- adv_cases()
    lang <- current_lang()

    withProgress(message = translator$t("Processing..."), value = 0, {
      # Get PT filter
      pts_selected <- input$adv_province

      # Convert French PT names to English for backend
      pts_for_backend <- pts_selected
      if (lang == "fr" && !is.null(pts_selected)) {
        if (!(translator$t("Canada") %in% pts_selected || "Canada" %in% pts_selected)) {
          en_pt_names <- fb_pt_names("en")
          fr_pt_names <- fb_pt_names("fr")
          pt_map <- stats::setNames(en_pt_names, fr_pt_names)
          pts_for_backend <- sapply(pts_selected, function(pt) if (pt %in% names(pt_map)) pt_map[pt] else pt)
        }
      }

      # Filter cases by PT if specific PT(s) selected
      incProgress(0.1, detail = "Filtering cases by PT")
      d_filtered <- d
      if ("provinceterritory" %in% names(d) && !is.null(pts_selected)) {
        if (!(translator$t("Canada") %in% pts_selected || "Canada" %in% pts_selected)) {
          d_filtered <- d %>% filter(is.na(provinceterritory) | provinceterritory %in% pts_for_backend)
          validate(need(nrow(d_filtered) > 0, translator$t("No cases found for selected province(s)")))
        }
      }

      # Get unique PTs from filtered cases for Reference Scope
      if ("provinceterritory" %in% names(d_filtered)) {
        unique_case_pts <- unique(d_filtered$provinceterritory[!is.na(d_filtered$provinceterritory)])

        # Debug output
        message("DEBUG: unique_case_pts = ", paste(unique_case_pts, collapse = ", "))
        message("DEBUG: length(unique_case_pts) = ", length(unique_case_pts))

        if (length(unique_case_pts) == 0) {
          reference_scope <- "Canada"
          pts_for_reference <- "Canada"
        } else {
          reference_scope <- paste(unique_case_pts, collapse = ", ")
          pts_for_reference <- unique_case_pts
        }
      } else {
        message("DEBUG: provinceterritory NOT in names(d_filtered)")
        reference_scope <- "Canada"
        pts_for_reference <- "Canada"
      }

      message("DEBUG: Final reference_scope = ", reference_scope)

      # Summarise counts by exposure code (count unique cases, not rows)
      incProgress(0.3, detail = "Summarizing exposure counts")
      exposure_counts <- d_filtered %>%
        mutate(val = dplyr::recode(val, y = "Y", n = "N", p = "P", dk = "DK")) %>%
        filter(val %in% c("Y", "N", "P", "DK")) %>%
        distinct(natid, exposure, val) %>%  # Ensure one response per case per exposure
        count(exposure, val) %>%
        tidyr::pivot_wider(names_from = val, values_from = n, values_fill = 0)

      codes <- exposure_counts$exposure

      ages <- if (is.null(input$adv_age_group) || (length(input$adv_age_group) == 1 && (translator$t("All Ages") %in% input$adv_age_group || "All Ages" %in% input$adv_age_group || "Tous les âges" %in% input$adv_age_group))) NULL else input$adv_age_group
      months <- if (is.null(input$adv_month) || (length(input$adv_month) == 1 && (translator$t("All Months") %in% input$adv_month || "All Months" %in% input$adv_month || "Tous les mois" %in% input$adv_month))) NULL else as.integer(input$adv_month)

      incProgress(0.4, detail = "Computing reference percentages")
      ref_perc <- fb_reference_percents(codes, pt_names = pts_for_reference, months = months, age_groups = ages)

      # Map codes to labels for display - use fb_exposure_choices_all() to include legacy CEDARS codes
      code_to_label <- names(fb_exposure_choices_all(lang))
      names(code_to_label) <- as.vector(fb_exposure_choices_all(lang))

      incProgress(0.3, detail = "Computing statistical tests")
      results <- exposure_counts %>%
        rowwise() %>%
        mutate(
          Exposure = code_to_label[exposure] %||% exposure,
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
  output$adv_results_table <- renderDT({
    req(adv_results())
    lang <- current_lang()

    res <- adv_results() %>%
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
        translator$t("Reference Scope"),
        translator$t("Exposure"),
        translator$t("Total Valid"),
        translator$t("Yes"),
        translator$t("Probably"),
        translator$t("No"),
        translator$t("DK"),
        translator$t("Observed %"),
        translator$t("Reference %"),
        translator$t("P-Value"),
        translator$t("Classification")
      )
    ) %>%
      formatStyle(
        "Classification",
        backgroundColor = styleEqual(
          c(translator$t("Alert"), translator$t("Borderline"),
            translator$t("Not Significant"), translator$t("Insufficient Data"),
            translator$t("No Reference Value")),
          c("#fde4e6", "#fff4d6", "#edf2ff", "#f1f5f9", "#e2e8f0")
        ),
        color = styleEqual(
          c(translator$t("Alert"), translator$t("Borderline"),
            translator$t("Not Significant"), translator$t("Insufficient Data"),
            translator$t("No Reference Value")),
          c("#b82c3a", "#b35c00", "#1f2933", "#475569", "#64748b")
        ),
        fontWeight = "600"
      )
  })

  # About page content (reactive for translation)
  output$about_content <- renderUI({
    lang <- current_lang()

    tagList(
      h4(translator$t("Methodology")),
      p(translator$t("This tool compares observed case exposures against Foodbook reference percentages using statistical significance testing.")),

      hr(),

      h4(translator$t("CEDARS Upload Workflow")),
      p(translator$t("This internal tool is designed for PHAC epidemiologists to analyze outbreak data exported from CEDARS.")),

      tags$ol(
        tags$li(translator$t("Export case exposure data from CEDARS to Excel (.xlsx)")),
        tags$li(translator$t("Upload the file using the file input")),
        tags$li(translator$t("The app auto-detects the required sheets (case exposure answer, case linelist)")),
        tags$li(translator$t("Select reference population filters (PT, age, month)")),
        tags$li(translator$t("View results with statistical testing and classifications"))
      ),

      hr(),

      h4(translator$t("Interpretation Guide")),
      tags$ul(
        tags$li(strong(translator$t("Alert")), ": ", translator$t("Observed exposure is significantly higher than reference (p < 0.05)")),
        tags$li(strong(translator$t("Borderline")), ": ", translator$t("Suggestive evidence (0.05 ≤ p < 0.10)")),
        tags$li(strong(translator$t("Not Significant")), ": ", translator$t("No significant difference from reference (p ≥ 0.10)")),
        tags$li(strong(translator$t("Insufficient Data")), ": ", translator$t("Too few cases to calculate statistics (< 5 total responses)")),
        tags$li(strong(translator$t("No Reference Value")), ": ", translator$t("Exposure not found in Foodbook database"))
      )
    )
  })
}

# --- 6. Run Application ---
shinyApp(ui, server)
