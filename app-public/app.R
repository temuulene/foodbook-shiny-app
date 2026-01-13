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

# Load backend and modules (using relative paths from app-public directory)
source("../src/foodbook_backend.R")
source("../src/i18n_helper.R")
source("../src/modules/language_selector_module.R")
source("../src/modules/exposure_module.R")

# Suppress warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
options(bslib.precompiled = TRUE)

# --- 2. Initialize Backend and Translator ---
translator <- Translator$new(
  translation_json_path = "../translations/translation.json"
)
translator$set_translation_language("en")

fb_init(lang = "en")
backend_ok <- tryCatch(fb_is_available(), error = function(e) FALSE)

# --- 3. Helper Functions ---
# NOTE: classify_exposure and make_safe_id are now also defined in foodbook_backend.R
# These local definitions are kept for clarity and backwards compatibility

classify_exposure <- function(p_value, observed_prop, ref_prop) {
  if (is.na(ref_prop)) {
    return("No Reference Value")
  }
  ref_prop_decimal <- ref_prop / 100
  if (is.na(p_value)) {
    return("Insufficient Data")
  }
  if (observed_prop > ref_prop_decimal) {
    case_when(
      p_value <= 0.05 ~ "Alert",
      p_value <= 0.10 ~ "Borderline",
      TRUE ~ "Not Significant"
    )
  } else {
    "Not Significant"
  }
}

make_safe_id <- function(exposure_name) {
  gsub("[^a-zA-Z0-9]", "", exposure_name)
}

# --- 4. User Interface (UI) ---
ui <- function(request) {
  page_navbar(
    title = "Food Exposure Analysis Tool",
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
      bs_add_rules(
        "
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
          min-height: 50px;
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
      "
      ),
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
      tags$head(
        tags$script(HTML(
          "
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
            $('#reset').text(labels.reset);
            $('button[id*=bookmark]').text(labels.bookmark);
            $('#download_plot').text(labels.download);
          });

          // Custom message handler for updating tab names
          Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
            // Update main nav tabs
            $('a.nav-link').each(function() {
              var $icon = $(this).find('i');
              var iconHtml = $icon.length ? $icon.prop('outerHTML') + ' ' : '';
              var text = $(this).text().trim();

              if (text === 'Analysis' || text === 'Analyse') {
                $(this).html(iconHtml + labels.analysis);
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
            $('.bslib-sidebar-layout .sidebar .title').text(title);
          });

          // Custom message handler for updating accordion titles
          Shiny.addCustomMessageHandler('update-accordion-titles', function(labels) {
            $('.accordion-button').each(function() {
              if ($(this).text().includes('Upload') || $(this).text().includes('Télécharger')) {
                var $icon = $(this).find('i');
                var iconHtml = $icon.length ? $icon.prop('outerHTML') + ' ' : '';
                $(this).html(iconHtml + labels.upload_exposure);
              }
            });
          });

          // Custom message handler for updating card headers
          Shiny.addCustomMessageHandler('update-card-headers', function(labels) {
            $('.card-header').each(function() {
              var text = $(this).text().trim();
              if (text === 'Exposure Data Input' || text === 'Saisie des données d\\'exposition') {
                $(this).text(labels.exposure_data_input);
              } else if (text === 'Reference Settings' || text === 'Paramètres de référence') {
                $(this).text(labels.reference_settings);
              } else if (text.includes('Population Exposure Snapshot') || text.includes('Instantané d\\'exposition de la population')) {
                $(this).text(labels.population_snapshot);
              } else if (text.includes('Microdata Coverage by PT') || text.includes('Couverture des microdonnées par PT')) {
                $(this).text(labels.microdata_pt);
              } else if (text.includes('Microdata Coverage by Month') || text.includes('Couverture des microdonnées par mois')) {
                $(this).text(labels.microdata_month);
              } else if (text === 'About This Tool' || text === 'À propos de cet outil') {
                $(this).text(labels.about_tool);
              }
            });
          });

          // Custom message handler for updating misc labels (help text, file inputs, etc)
          Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
            // Update help text (but NOT file input controls - they're handled by renderUI)
            $('span.help-block, span.form-text, .shiny-input-container .help-block').each(function() {
              var text = $(this).text().trim();
              if (text.includes('Enter case counts') || text.includes('Entrez les comptes')) {
                $(this).text(labels.enter_case_counts);
              }
            });
          });
        "
        )),
        tags$style(HTML(
          "
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

        .exposure-input-group {
          border: 1px solid #dde6f5;
          border-radius: 0.9rem;
          background: #ffffff;
          padding: 1.2rem;
          margin-bottom: 1.25rem;
          box-shadow: 0 16px 40px rgba(15, 76, 129, 0.08);
        }
        .exposure-input-group:first-child {
          margin-top: 0.75rem;
        }
        .exposure-input-group .row {
          row-gap: 0.75rem;
        }
        .exposure-header {
          color: #0f4c81;
          font-weight: 600;
          letter-spacing: 0.01em;
          margin-bottom: 1rem;
        }
        .exposure-input-group .form-control {
          border-radius: 0.65rem;
          border: 1px solid #d0ddf0;
          padding: 0.6rem 0.75rem;
          font-weight: 500;
        }
        .exposure-input-group .form-control:focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.15);
        }
        .ref-value {
          color: #1b7b57;
          font-weight: 600;
          display: inline-block;
          margin-top: 0.25rem;
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
        .sidebar .action-button {
          font-weight: 600;
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
      "
        ))
      ),
      # Hidden language selector (will be moved to navbar by JavaScript)
      tags$div(
        id = "lang_selector_container",
        style = "display: none;",
        language_selector_ui("lang_selector", style = "dropdown")
      )
    ),

    # Analysis Tab
    nav_panel(
      translator$t("Analysis"),
      icon = icon("calculator"),
      layout_sidebar(
        sidebar = sidebar(
          uiOutput("sidebar_analysis_title"),
          tooltip(
            selectInput(
              "province",
              translator$t("Reference PT(s)"),
              choices = stats::setNames(
                c("Canada", fb_pt_names("en")),
                c(translator$t("Canada"), fb_pt_names("en"))
              ),
              selected = "Canada",
              multiple = TRUE
            ),
            translator$t(
              "PT = Province/Territory. Select which geographic areas to include in the reference population."
            )
          ),
          tooltip(
            selectInput(
              "age_group",
              translator$t("Restrict by Age Group"),
              choices = stats::setNames(
                c("All Ages", fb_age_groups()),
                c(translator$t("All Ages"), fb_age_groups())
              ),
              selected = "All Ages",
              multiple = TRUE
            ),
            translator$t(
              "Filter the reference population to specific age groups for age-stratified analysis."
            )
          ),
          tooltip(
            selectInput(
              "month",
              translator$t("Restrict by Month"),
              choices = stats::setNames(
                c("All Months", as.character(1:12)),
                c(translator$t("All Months"), fb_month_names())
              ),
              selected = "All Months",
              multiple = TRUE
            ),
            translator$t(
              "Filter the reference population by month(s) to account for seasonal variation."
            )
          ),
          # Over-analysis warning
          uiOutput("overanalysis_warning_ui"),
          hr(),
          accordion(
            accordion_panel(
              title = translator$t("Upload Exposure Counts (Optional)"),
              icon = icon("upload"),
              uiOutput("xlsx_file_input_ui"),
              uiOutput("xlsx_help_text"),
              uiOutput("xlsx_clear_button")
            )
          ),
          hr(),
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
        ),
        card(
          card_header(translator$t("Exposure Data Input")),
          card_body(
            uiOutput("category_filter_ui"),
            helpText(translator$t(
              "Enter case counts for each exposure in each selected location."
            )),
            uiOutput("exposure_select_ui"),
            div(
              style = "max-height: 60vh; overflow-y: auto;",
              uiOutput("exposure_modules_ui")
            ),
            helpText(
              translator$t("* Exposures from Foodbook 1.0"),
              style = "font-size: 0.8rem; margin-top: 0.5rem; color: #6c757d;"
            )
          )
        ),
        navset_card_tab(
          full_screen = TRUE,
          nav_panel(
            translator$t("Results"),
            class = "results-panel",
            withSpinner(
              uiOutput("results_table_container", width = "100%"),
              type = 4,
              color = "#0f4c81"
            )
          ),
          nav_panel(
            translator$t("Visualization"),
            class = "visual-panel",
            div(
              style = "margin-bottom: 1rem;",
              uiOutput("download_plot_button_ui")
            ),
            uiOutput("plot_container")
          )
        )
      )
    ),

    # Reference Data Tab
    nav_panel(
      translator$t("Reference Data"),
      icon = icon("table"),
      card(
        card_header(translator$t("Reference Values")),
        card_body(
          withSpinner(
            DTOutput("sys_ref_table"),
            type = 4,
            color = "#0f4c81"
          ),
          helpText(
            translator$t("* Exposures from Foodbook 1 only"),
            style = "font-size: 0.8rem; margin-top: 0.5rem; color: #6c757d;"
          )
        )
      )
    ),

    # Data Info Tab
    nav_panel(
      translator$t("Data Info"),
      icon = icon("database"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(translator$t("Reference Settings")),
          card_body(uiOutput("ref_summary_ui"))
        ),
        card(
          card_header(translator$t("Population Exposure Snapshot (Reference)")),
          card_body(withSpinner(
            DTOutput("ref_top_exposures"),
            type = 4,
            color = "#0f4c81"
          ))
        ),
        card(
          card_header(translator$t("Microdata Coverage by PT (after filters)")),
          card_body(withSpinner(
            plotOutput("ref_pt_plot", height = "350px"),
            type = 4,
            color = "#0f4c81"
          ))
        ),
        card(
          card_header(translator$t(
            "Microdata Coverage by Month (after filters)"
          )),
          card_body(withSpinner(
            plotOutput("ref_month_plot", height = "350px"),
            type = 4,
            color = "#0f4c81"
          ))
        )
      )
    ),

    # About Tab
    nav_panel(
      translator$t("About"),
      icon = icon("info-circle"),
      card(
        class = "well-panel-about",
        card_header(h3(translator$t("About This Tool"))),
        card_body(uiOutput("about_content"))
      )
    )
  )
}

# --- 6. Server Logic ---
server <- function(input, output, session) {
  translator <- init_translator(
    session,
    translation_path = "../translations/translation.json"
  )

  lang_state <- language_selector_server(
    "lang_selector",
    session_parent = session,
    style = "dropdown"
  )
  current_lang <- lang_state$language

  # Store uploaded CSV data
  csv_data <- reactiveVal(NULL)

  # Flag for CSV population (defined early to avoid reference before definition)
  csv_needs_population <- reactiveVal(FALSE)

  # Load toolkit data on startup
  fb_load_toolkit_data()

  # Render Category Filter
  output$category_filter_ui <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    
    cats <- c(tr$t("All Categories"), fb_exposure_categories(lang))
    selectInput("category_filter", tr$t("Filter Category"), choices = cats)
  })

  # Render sidebar title with current language
  output$sidebar_analysis_title <- renderUI({
    lang <- current_lang()
    # Create translator with current language to avoid race condition
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    tags$div(class = "title", tr$t("Analysis Parameters"))
  })

  # Render over-analysis warning
  output$overanalysis_warning_ui <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    div(
      class = "alert alert-warning",
      style = "font-size: 0.85rem; padding: 0.75rem; margin-top: 0.5rem;",
      icon("exclamation-triangle"),
      " ",
      tags$strong(tr$t("Data Quality Warning")),
      tags$br(),
      tr$t("Please be careful not to overanalyse the data. Limiting the data to a small subset of respondents (for example, respondents ages 0-9 from PEI in March) can result in small sample sizes and make the data less reliable. This is especially important for exposures that are rare within the population.")
    )
  })

  # Render XLSX file input
  output$xlsx_file_input_ui <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    fileInput(
      "simple_xlsx_upload",
      label = tr$t("Upload Excel File"),
      accept = c(".xlsx"),
      buttonLabel = tr$t("Browse"),
      placeholder = tr$t("No file selected")
    )
  })

  # Render XLSX help text
  output$xlsx_help_text <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    tagList(
      helpText(HTML(paste0(
        "<strong>",
        tr$t("Note"),
        ":</strong> ",
        tr$t(
          "Exposure names will be matched against Foodbook database in English or French (case-insensitive). Unmatched exposures will use custom references."
        )
      )))
    )
  })

  # Render XLSX clear button
  output$xlsx_clear_button <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    actionButton(
      "xlsx_clear",
      label = tr$t("Remove File"),
      icon = icon("trash"),
      class = "btn btn-outline-secondary w-100 mt-2"
    )
  })

  # Render exposure select to avoid re-initializing selectize plugins on language change
  output$exposure_select_ui <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)

    # Get category filter
    cat_filter <- input$category_filter
    real_cat <- if (!is.null(cat_filter) && cat_filter != tr$t("All Categories")) cat_filter else NULL

    all_exposures <- tryCatch(
      fb_toolkit_exposure_choices(lang, category = real_cat),
      error = function(e) {
        warning("Unable to load exposure choices: ", e$message)
        showNotification(
          tr$t("Unable to load exposure list. Please try again."),
          type = "error"
        )
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
      options = list(
        placeholder = tr$t("Start typing..."),
        plugins = list("remove_button"),
        create = TRUE
      )
    )
  })

  # Update UI when language changes
  observeEvent(
    current_lang(),
    {
      lang <- current_lang()
      set_language(lang, session)
      translator <- get_translator(session)
      # Only update language labels, don't re-initialize entire backend
      fb_update_language(lang = lang)

      # Preserve current selections by converting to appropriate format
      current_prov <- input$province
      current_age <- input$age_group
      current_month <- input$month

      # If default values selected, update them to new language
      if (!is.null(current_prov) && ("Canada" %in% current_prov)) {
        current_prov[current_prov == "Canada"] <- translator$t("Canada")
      }
      all_ages_en <- t_("All Ages", lang = "en")
      all_ages_fr <- t_("All Ages", lang = "fr")
      if (
        !is.null(current_age) &&
          (all_ages_en %in% current_age || all_ages_fr %in% current_age)
      ) {
        current_age[
          current_age %in% c(all_ages_en, all_ages_fr)
        ] <- translator$t("All Ages")
      }
      all_months_en <- t_("All Months", lang = "en")
      all_months_fr <- t_("All Months", lang = "fr")
      if (
        !is.null(current_month) &&
          (all_months_en %in% current_month || all_months_fr %in% current_month)
      ) {
        current_month[
          current_month %in% c(all_months_en, all_months_fr)
        ] <- translator$t("All Months")
      }

      # Update all select inputs with new labels
      updateSelectInput(
        session,
        "province",
        label = translator$t("Reference PT(s)"),
        choices = c(translator$t("Canada"), fb_pt_names(lang)),
        selected = current_prov
      )

      updateSelectInput(
        session,
        "age_group",
        label = translator$t("Restrict by Age Group"),
        choices = c(translator$t("All Ages"), fb_age_groups()),
        selected = current_age
      )

      # Update month selector with translated month names from backend
      month_choices <- c(
        translator$t("All Months"),
        stats::setNames(1:12, fb_month_names(lang))
      )
      updateSelectInput(
        session,
        "month",
        label = translator$t("Restrict by Month"),
        choices = month_choices,
        selected = current_month
      )

      # Update button labels via JavaScript
      session$sendCustomMessage(
        "update-button-labels",
        list(
          reset = translator$t("Reset Inputs"),
          bookmark = translator$t("Bookmark Analysis"),
          download = translator$t("Download Plot")
        )
      )

      # Update tab names via JavaScript
      session$sendCustomMessage(
        "update-tab-names",
        list(
          analysis = translator$t("Analysis"),
          data_info = translator$t("Data Info"),
          about = translator$t("About"),
          results = translator$t("Results"),
          visualization = translator$t("Visualization")
        )
      )

      # Update sidebar title via JavaScript
      session$sendCustomMessage(
        "update-sidebar-title",
        translator$t("Analysis Parameters")
      )

      # Update accordion titles via JavaScript
      session$sendCustomMessage(
        "update-accordion-titles",
        list(
          upload_exposure = translator$t("Upload Exposure Counts (Optional)")
        )
      )

      # Update card headers via JavaScript
      session$sendCustomMessage(
        "update-card-headers",
        list(
          exposure_data_input = translator$t("Exposure Data Input"),
          reference_settings = translator$t("Reference Settings"),
          population_snapshot = translator$t(
            "Population Exposure Snapshot (Reference)"
          ),
          microdata_pt = translator$t(
            "Microdata Coverage by PT (after filters)"
          ),
          microdata_month = translator$t(
            "Microdata Coverage by Month (after filters)"
          ),
          about_tool = translator$t("About This Tool")
        )
      )

      # Update help text via JavaScript
      session$sendCustomMessage(
        "update-misc-labels",
        list(
          enter_case_counts = translator$t(
            "Enter case counts for each exposure in each selected location."
          )
        )
      )

      # If CSV data exists, trigger re-population after language change
      if (!is.null(csv_data())) {
        message(
          "=== Language changed with CSV data present, triggering re-population ==="
        )
        csv_needs_population(TRUE)
      }
    },
    ignoreInit = TRUE
  )

  # Auto-deselect "Canada" when specific PTs are selected
  observeEvent(input$province, {
    if (
      translator$t("Canada") %in% input$province && length(input$province) > 1
    ) {
      updateSelectInput(
        session,
        "province",
        selected = setdiff(input$province, translator$t("Canada"))
      )
    }
  })

  # Auto-deselect "All Ages" when specific ages selected
  observeEvent(input$age_group, {
    if (
      translator$t("All Ages") %in%
        input$age_group &&
        length(input$age_group) > 1
    ) {
      updateSelectInput(
        session,
        "age_group",
        selected = setdiff(input$age_group, translator$t("All Ages"))
      )
    }
  })

  # Auto-deselect "All Months" when specific months selected
  observeEvent(input$month, {
    if (
      translator$t("All Months") %in% input$month && length(input$month) > 1
    ) {
      updateSelectInput(
        session,
        "month",
        selected = setdiff(input$month, translator$t("All Months"))
      )
    }
  })

  # Process XLSX upload
  observeEvent(input$simple_xlsx_upload, {
    req(input$simple_xlsx_upload)
    file_info <- input$simple_xlsx_upload
    lang <- current_lang()
    # Create fresh translator to avoid encoding issues
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)

    tryCatch(
      {
        # Read Excel file
        df <- readxl::read_excel(file_info$datapath)
        message(
          "Excel file read successfully. Columns: ",
          paste(names(df), collapse = ", ")
        )
        message("First row: ", paste(df[1, ], collapse = ", "))

        names(df) <- gsub("[^a-z0-9]+", "", tolower(names(df)))
        message(
          "After normalization. Columns: ",
          paste(names(df), collapse = ", ")
        )

        validate(need(
          all(c("exposure", "yes", "probably", "no", "dk") %in% names(df)),
          "Excel file must have columns: Exposure, Yes, Probably, No, DK"
        ))

        message("CSV validation passed. Rows: ", nrow(df))
        print(df)

        # Match CSV exposures against Foodbook database (case-insensitive)
        # Check BOTH English and French labels regardless of current language
        lang <- current_lang()
        foodbook_choices_en <- fb_exposure_choices(
          "en",
          apply_public_exclusions = TRUE
        ) # label = code format
        foodbook_choices_fr <- fb_exposure_choices(
          "fr",
          apply_public_exclusions = TRUE
        ) # label = code format

        # Create lookups: lowercase label -> code for both languages
        fb_lookup_en <- stats::setNames(
          foodbook_choices_en,
          tolower(names(foodbook_choices_en))
        )
        fb_lookup_fr <- stats::setNames(
          foodbook_choices_fr,
          tolower(names(foodbook_choices_fr))
        )

        # Match each CSV exposure
        matched_exposures <- character(nrow(df))
        match_count <- 0
        custom_count <- 0

        for (i in seq_len(nrow(df))) {
          csv_name <- df$exposure[i]
          csv_name_lower <- tolower(csv_name)

          # Try to find in English first, then French
          if (csv_name_lower %in% names(fb_lookup_en)) {
            # Found in English!
            matched_exposures[i] <- fb_lookup_en[[csv_name_lower]]
            match_count <- match_count + 1
            message(
              "  Matched '",
              csv_name,
              "' (EN) -> Foodbook code: ",
              fb_lookup_en[[csv_name_lower]]
            )
          } else if (csv_name_lower %in% names(fb_lookup_fr)) {
            # Found in French!
            matched_exposures[i] <- fb_lookup_fr[[csv_name_lower]]
            match_count <- match_count + 1
            message(
              "  Matched '",
              csv_name,
              "' (FR) -> Foodbook code: ",
              fb_lookup_fr[[csv_name_lower]]
            )
          } else {
            # Not found in either language, keep as custom
            matched_exposures[i] <- csv_name
            custom_count <- custom_count + 1
            message(
              "  No match for '",
              csv_name,
              "' - will use custom reference"
            )
          }
        }

        # Update df with matched exposure codes for later use
        df$matched_exposure <- matched_exposures

        # Store CSV data for populating modules
        csv_data(df)
        message("CSV data stored in reactive")

        # Update exposure selection with matched exposures
        # Use the matched codes, not the original CSV names
        updateSelectizeInput(
          session,
          "exposure_select",
          selected = matched_exposures
        )
        message("Exposure selection updated with matched codes")

        # Show notification with match statistics
        if (custom_count > 0) {
          msg <- paste0(
            tr$t("Success"),
            ": ",
            nrow(df),
            " ",
            tr$t("exposures loaded"),
            " (",
            match_count,
            " ",
            tr$t("matched"),
            ", ",
            custom_count,
            " ",
            tr$t("custom"),
            ")"
          )
        } else {
          msg <- paste0(
            tr$t("Success"),
            ": ",
            nrow(df),
            " ",
            tr$t("exposures loaded"),
            " (",
            tr$t("all matched"),
            ")"
          )
        }
        showNotification(enc2utf8(msg), type = "message")
      },
      error = function(e) {
        message("XLSX upload error: ", e$message)
        showNotification(
          enc2utf8(paste(tr$t("Error"), ": ", e$message)),
          type = "error"
        )
      }
    )
  })

  # Clear XLSX upload
  observeEvent(input$xlsx_clear, {
    csv_data(NULL)
    csv_needs_population(FALSE)
    shinyjs::js$resetFileInput(id = "simple_xlsx_upload")
    updateSelectizeInput(session, "exposure_select", selected = character(0))
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    showNotification(enc2utf8(tr$t("Upload cleared")), type = "message")
  })

  # Store module server instances
  exposure_module_returns <- reactiveValues()

  # Cache reference percentages to avoid duplicate backend calls
  cached_ref_percents <- reactive({
    req(input$exposure_select)
    lang <- current_lang()

    pts <- input$province
    if (translator$t("Canada") %in% pts) {
      pts <- "Canada"
    }

    # Map French PT names back to English for backend
    if (lang == "fr" && !translator$t("Canada") %in% pts) {
      en_pt_names <- fb_pt_names("en")
      fr_pt_names <- fb_pt_names("fr")
      pt_map <- stats::setNames(en_pt_names, fr_pt_names)
      pts <- sapply(pts, function(pt) {
        if (pt %in% names(pt_map)) pt_map[pt] else pt
      })
    }

    ages <- if (translator$t("All Ages") %in% input$age_group) {
      NULL
    } else {
      input$age_group
    }
    months <- if (translator$t("All Months") %in% input$month) {
      NULL
    } else {
      as.integer(input$month)
    }

    exposure_codes <- input$exposure_select
    
    # Hybrid Approach:
    # 1. Try to get weighted values from microdata (supports Age/Month filtering)
    # 2. Fill in gaps (NAs) with static Toolkit values (Table 6 match)
    
    # Step 1: Microdata
    res <- tryCatch(
      fb_reference_percents(
        exposure_codes,
        pt_names = pts,
        months = months,
        age_groups = ages
      ),
      error = function(e) {
        structure(rep(NA_real_, length(exposure_codes)), names = exposure_codes)
      }
    )
    
    # Step 2: Fallback to Toolkit (if simple filters)
    # Only fallback if NO Age/Month filters are active (Toolkit is static Total Population)
    if (is.null(ages) && is.null(months)) {
      
      # Determine PT to use for Toolkit lookup
      # Toolkit supports Single PT or Canada.
      # If multiple PTs selected, we can't use static Toolkit table easily (would need weighted average)
      # So only fallback if "Canada" or Single PT selected.
      
      pt_to_use <- NULL
      if ("Canada" %in% pts || length(pts) > 1) {
         # If Canada is in list, or multiple PTs selected -> default to Canada level fallback?
         # Actually, Toolkit behaviour: Select 1 PT.
         # App behaviour: Select multiple.
         # If user selects multiple PTs and a var is missing from microdata, we probably can't give a good ref value.
         # But if user selects "Canada" (default), we definitely can.
         if ("Canada" %in% pts) pt_to_use <- "Canada"
      } else if (length(pts) == 1) {
         pt_to_use <- pts
      }
      
      if (!is.null(pt_to_use)) {
        # Check for missing values
        for (code in exposure_codes) {
          # If missing in microdata result or NA
          val <- if (code %in% names(res)) res[[code]] else NA_real_
          
          if (is.na(val)) {
            # Try Toolkit
            tk_val <- fb_toolkit_reference_percent(code, pt_to_use)
            if (!is.na(tk_val)) {
              if (code %in% names(res)) {
                 res[[code]] <- tk_val
              } else {
                 # Should replace in vector, but res might be named vector
                 # Reconstruct res to ensure all codes present
                 # Actually fb_reference_percents returns vector with names matching input? 
                 # Usually yes. But let's be safe.
                 # If code was not returned by microdata function, it's problematic for current UI which iterates input$exposure_select
                 # But UI uses input$exposure_select to index the result?
                 # No, exposure_module uses numeric index or name?
                 # Let's assign to res by name.
                 res[code] <- tk_val
              }
            }
          }
        }
      }
    }
    
    res
  })

  # Cache reference sample size for small sample suppression
  cached_ref_sample_size <- reactive({
    pts <- input$province
    if (translator$t("Canada") %in% pts) {
      pts <- "Canada"
    }

    # Map French PT names back to English for backend
    lang <- current_lang()
    if (lang == "fr" && !translator$t("Canada") %in% pts) {
      en_pt_names <- fb_pt_names("en")
      fr_pt_names <- fb_pt_names("fr")
      pt_map <- stats::setNames(en_pt_names, fr_pt_names)
      pts <- sapply(pts, function(pt) {
        if (pt %in% names(pt_map)) pt_map[pt] else pt
      })
    }

    ages <- if (translator$t("All Ages") %in% input$age_group) {
      NULL
    } else {
      input$age_group
    }
    months <- if (translator$t("All Months") %in% input$month) {
      NULL
    } else {
      as.integer(input$month)
    }

    fb_reference_sample_size(pt_names = pts, months = months, age_groups = ages)
  })

  # Generate exposure module UIs and instantiate servers
  output$exposure_modules_ui <- renderUI({
    req(input$exposure_select)
    lang <- current_lang()

    # Use cached reference percentages
    ref_perc <- cached_ref_percents()
    exposure_codes <- input$exposure_select

    # Get exposure choices (label = code format)
    all_exposure_choices <- fb_exposure_choices(
      lang,
      apply_public_exclusions = TRUE
    )
    # Create reverse map (code = label) for lookups
    code_to_label <- stats::setNames(
      names(all_exposure_choices),
      all_exposure_choices
    )

    lapply(exposure_codes, function(exp_code) {
      safe_id <- make_safe_id(exp_code)
      ref_val <- ref_perc[exp_code]
      is_custom <- is.na(ref_val) || !(exp_code %in% all_exposure_choices)

      exposure_label <- if (is_custom) exp_code else code_to_label[exp_code]

      # Instantiate module server and store return value
      exposure_module_returns[[safe_id]] <- exposure_module_server(safe_id)

      # Return UI
      exposure_module_ui(
        safe_id,
        exposure_label,
        if (!is.na(ref_val)) round(ref_val, 1) else "N/A",
        is_custom,
        lang
      )
    })
  })

  # Populate modules with CSV data when available
  # csv_needs_population is already defined at the top of the server function

  # Set flag when CSV is uploaded
  observeEvent(
    csv_data(),
    {
      req(csv_data())
      message("=== CSV data uploaded, setting population flag ===")
      csv_needs_population(TRUE)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Debug: Watch exposure_select changes
  observeEvent(
    input$exposure_select,
    {
      message(
        "*** input$exposure_select changed to: ",
        paste(input$exposure_select %||% "NULL", collapse = ", ")
      )
    },
    ignoreNULL = FALSE
  )

  # Populate when BOTH csv data exists AND exposure selection is updated
  observe({
    req(csv_needs_population())
    req(input$exposure_select)
    req(csv_data())

    df <- csv_data()

    message("=== Both CSV data and exposure selection ready ===")
    message("CSV has ", nrow(df), " exposures")
    message(
      "Exposure selection: ",
      paste(input$exposure_select, collapse = ", ")
    )

    # Wait for modules to be fully rendered (1 second should be enough now)
    shinyjs::delay(1000, {
      message("=== Starting module population (after 1s delay) ===")

      for (i in seq_len(nrow(df))) {
        # Use matched_exposure (Foodbook code) instead of original CSV name
        exp_code <- df$matched_exposure[i]
        safe_id <- make_safe_id(exp_code)

        message(
          "[",
          i,
          "/",
          nrow(df),
          "] Processing '",
          df$exposure[i],
          "' (matched: '",
          exp_code,
          "') -> safe_id: '",
          safe_id,
          "'"
        )

        # Check if this exposure is in the selection
        selection_current <- isolate(input$exposure_select)

        if (exp_code %in% selection_current) {
          tryCatch(
            {
              exposure_module_update(
                session,
                safe_id,
                yes = as.numeric(df$yes[i]),
                prob = as.numeric(df$probably[i]),
                no = as.numeric(df$no[i]),
                dk = as.numeric(df$dk[i])
              )
              message(
                "      SUCCESS: yes=",
                df$yes[i],
                ", prob=",
                df$probably[i],
                ", no=",
                df$no[i],
                ", dk=",
                df$dk[i]
              )
            },
            error = function(e) {
              message("      ERROR: ", e$message)
            }
          )
        } else {
          message("      SKIPPED: not in selection")
        }
      }
      message("=== Module population complete ===")

      # Clear the flag so we don't re-populate
      csv_needs_population(FALSE)
    })
  })

  # Calculate results
  results_data <- reactive({
    req(input$exposure_select)
    lang <- current_lang()

    # Use cached reference percentages
    ref_perc <- cached_ref_percents()
    exposure_codes <- input$exposure_select

    # Get exposure choices (label = code format)
    all_exposure_choices <- fb_exposure_choices(
      lang,
      apply_public_exclusions = TRUE
    )
    # Create reverse map (code = label) for lookups
    code_to_label <- stats::setNames(
      names(all_exposure_choices),
      all_exposure_choices
    )

    rows <- lapply(exposure_codes, function(exp_code) {
      safe_id <- make_safe_id(exp_code)

      # Get module data from stored reactive
      module_reactive <- exposure_module_returns[[safe_id]]
      req(module_reactive)
      module_data <- module_reactive()

      y <- module_data$yes
      p <- module_data$prob
      n <- module_data$no
      dk <- module_data$dk
      custom_ref <- module_data$custom_ref

      y_plus_p <- y + p
      total <- y_plus_p + n
      observed_prop <- if (total > 0) y_plus_p / total else NA_real_

      ref_val <- if (!is.na(custom_ref)) custom_ref else ref_perc[exp_code]

      p_value <- if (total > 0 && !is.na(ref_val)) {
        pbinom(y_plus_p - 1, total, ref_val / 100, lower.tail = FALSE)
      } else {
        NA_real_
      }

      classification <- classify_exposure(p_value, observed_prop, ref_val)
      classification_i18n <- classification_label_i18n(classification, lang)

      exposure_label <- if (exp_code %in% all_exposure_choices) {
        code_to_label[exp_code]
      } else {
        exp_code
      }

      tibble(
        Exposure = exposure_label,
        `Total Valid` = total,
        Yes = y,
        Probably = p,
        No = n,
        DK = dk,
        `Observed %` = round(observed_prop * 100, 1),
        `Reference %` = round(ref_val, 1),
        `P-Value` = round(p_value, 4),
        Classification = classification_i18n
      )
    })

    result_df <- bind_rows(rows)

    # Add Reference Scope column showing which PTs were used
    ref_scope <- if (
      translator$t("Canada") %in% input$province || length(input$province) == 0
    ) {
      "Canada"
    } else {
      paste(input$province, collapse = ", ")
    }

    result_df %>%
      mutate(`Reference Scope` = ref_scope) %>%
      select(`Reference Scope`, everything())
  })

  # Render results table container (with empty state support)
  output$results_table_container <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)

    # Check if exposures are selected
    if (is.null(input$exposure_select) || length(input$exposure_select) == 0) {
      return(div(
        class = "empty-state",
        style = "text-align: center; padding: 4rem 2rem; color: #6c757d;",
        div(
          icon(
            "chart-bar",
            style = "font-size: 4rem; opacity: 0.3; margin-bottom: 1rem;"
          )
        ),
        h4(tr$t("No exposures selected"), style = "margin-bottom: 0.5rem;"),
        p(tr$t(
          "Add exposures to the exposure data input box or upload an Excel file from the sidebar to get started"
        ))
      ))
    }

    # If exposures selected but no results data, show loading or different message
    res <- results_data()
    if (is.null(res) || nrow(res) == 0) {
      return(div(
        class = "empty-state",
        style = "text-align: center; padding: 4rem 2rem; color: #6c757d;",
        div(
          icon(
            "info-circle",
            style = "font-size: 4rem; opacity: 0.3; margin-bottom: 1rem;"
          )
        ),
        h4(tr$t("No data available"))
      ))
    }

    # Check if reference sample size is very small (≤5)
    ref_sample_size <- cached_ref_sample_size()
    small_sample_warning <- NULL
    if (!is.null(ref_sample_size) && ref_sample_size <= 5) {
      small_sample_warning <- div(
        class = "alert alert-danger",
        style = "margin-bottom: 1rem;",
        icon("exclamation-circle"),
        " ",
        tr$t("* A reliable estimate cannot be displayed due to small sample size.")
      )
    }

    # Otherwise, render the table with optional warning
    tagList(
      small_sample_warning,
      DTOutput("results_table", width = "100%")
    )
  })

  # Render results table
  output$results_table <- renderDT(server = FALSE, {
    req(results_data())
    lang <- current_lang()

    res <- results_data()
    pts <- input$province %||% translator$t("Canada")
    pt_str <- if (length(pts) == 1) {
      gsub(" ", "", pts[1])
    } else {
      paste0(length(pts), "PTs")
    }
    n_exp <- nrow(res)
    filename <- paste0(
      "analysis_results_",
      pt_str,
      "_",
      n_exp,
      "exp_",
      Sys.Date()
    )

    datatable(
      res,
      options = list(
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', filename = filename),
          'copy',
          'print'
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
          c(
            translator$t("Alert"),
            translator$t("Borderline"),
            translator$t("Not Significant"),
            translator$t("Insufficient Data"),
            translator$t("No Reference Value")
          ),
          c("#E74C3C", "#F39C12", "#27AE60", "#95A5A6", "#BDC3C7")
        )
      )
  })

  # Reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "province", selected = translator$t("Canada"))
    updateSelectInput(session, "age_group", selected = translator$t("All Ages"))
    updateSelectInput(session, "month", selected = translator$t("All Months"))
    updateSelectizeInput(session, "exposure_select", selected = character(0))
  })

  # Conditionally show Download Plot button
  output$download_plot_button_ui <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)

    # Only show button if there's data to plot
    res <- results_data()
    if (!is.null(res) && nrow(res) > 0) {
      # Check if there's plottable data (non-NA observed and reference percentages)
      plot_data <- res %>%
        dplyr::filter(!is.na(`Observed %`), !is.na(`Reference %`))

      if (nrow(plot_data) > 0) {
        return(downloadButton(
          "download_plot",
          tr$t("Download Plot"),
          class = "btn-primary"
        ))
      }
    }

    # Otherwise, return nothing (hide button)
    return(NULL)
  })

  # Visualization
  output$plot_container <- renderUI({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)

    # Check if exposures are selected
    if (is.null(input$exposure_select) || length(input$exposure_select) == 0) {
      return(div(
        class = "empty-state",
        style = "text-align: center; padding: 4rem 2rem; color: #6c757d;",
        div(
          icon(
            "chart-line",
            style = "font-size: 4rem; opacity: 0.3; margin-bottom: 1rem;"
          )
        ),
        h4(tr$t("No plot to display"), style = "margin-bottom: 0.5rem;"),
        p(tr$t("Select at least one exposure to generate visualization"))
      ))
    }

    # Check if results data exists
    res <- results_data()
    if (is.null(res) || nrow(res) == 0) {
      return(div(
        class = "empty-state",
        style = "text-align: center; padding: 4rem 2rem; color: #6c757d;",
        div(
          icon(
            "info-circle",
            style = "font-size: 4rem; opacity: 0.3; margin-bottom: 1rem;"
          )
        ),
        h4(tr$t("No data available"))
      ))
    }

    # Otherwise, render the plot with dynamic height
    plot_height <- max(500, length(input$exposure_select) * 180)
    withSpinner(
      plotOutput("results_plot", height = paste0(plot_height, "px")),
      type = 4,
      color = "#0f4c81"
    )
  })

  # Reactive to store the plot object for both display and download
  plot_reactive <- reactive({
    req(results_data())
    lang <- current_lang()

    plot_data <- results_data() %>%
      filter(!is.na(`Observed %`), !is.na(`Reference %`)) %>%
      mutate(
        Reference_Comparison = ifelse(
          `Observed %` > `Reference %`,
          paste0("+", round(`Observed %` - `Reference %`, 1), "%"),
          paste0(round(`Observed %` - `Reference %`, 1), "%")
        )
      )

    if (nrow(plot_data) == 0) {
      return(NULL)
    }

    ref_scope <- unique(plot_data$`Reference Scope`)
    ref_scope_str <- paste(ref_scope, collapse = ", ")

    # Palette tuned for accessibility and contrast
    alert_palette <- c(
      "Alert" = "#d62839",
      "Borderline" = "#f4a259",
      "Not Significant" = "#4b5563",
      "Insufficient Data" = "#94a3b8",
      "No Reference Value" = "#94a3b8"
    )

    ggplot(plot_data, aes(y = reorder(Exposure, `Observed %`))) +
      geom_segment(
        aes(
          x = `Reference %`,
          xend = `Observed %`,
          yend = Exposure,
          color = Classification
        ),
        linewidth = 2,
        alpha = 0.8
      ) +
      geom_point(
        aes(x = `Reference %`, size = `Total Valid`),
        color = "#0f4c81",
        shape = 1,
        stroke = 2
      ) +
      geom_point(
        aes(x = `Observed %`, size = `Total Valid`, fill = Classification),
        color = "#1f2933",
        shape = 21,
        stroke = 1.4
      ) +
      geom_text(
        aes(
          x = pmax(`Observed %`, `Reference %`),
          label = Reference_Comparison
        ),
        hjust = -0.2,
        size = 5.2,
        fontface = "bold",
        color = "#1f2933"
      ) +
      scale_fill_manual(
        values = alert_palette,
        name = "Significance",
        na.value = "#94a3b8"
      ) +
      scale_color_manual(
        values = alert_palette,
        name = "Significance",
        na.value = "#94a3b8"
      ) +
      guides(
        size = guide_legend(override.aes = list(shape = 21, fill = "#0f4c81"))
      ) +
      scale_size(range = c(5, 12), name = "Number of Cases") +
      scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
      labs(
        title = translator$t("Food Exposure Risk Assessment"),
        subtitle = paste(
          translator$t(
            "Comparison of case exposures vs. population reference values"
          ),
          " | ",
          translator$t("Reference scope:"),
          ref_scope_str
        ),
        x = translator$t("Exposure Percentage (%)"),
        y = NULL,
        caption = translator$t(
          "Outline circles = Reference exposure | Filled circles = Case exposure"
        )
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(
          face = "bold",
          size = 14,
          color = "#0f4c81"
        ),
        legend.text = element_text(size = 13, color = "#4b5563"),
        plot.title = element_text(face = "bold", size = 22, color = "#0f4c81"),
        plot.subtitle = element_text(
          margin = margin(b = 15),
          size = 16,
          color = "#334155"
        ),
        axis.title = element_text(face = "bold", size = 16, color = "#1f2933"),
        axis.text = element_text(size = 14, color = "#4b5563"),
        strip.text = element_text(
          size = 15,
          face = "bold",
          hjust = 0,
          color = "#0f4c81"
        ),
        strip.background = element_rect(
          fill = "#f1f5ff",
          color = "#0f4c81",
          linewidth = 0.8
        ),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.background = element_rect(fill = "#f9fbff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA)
      )
  })

  output$results_plot <- renderPlot({
    plot_reactive()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("exposure_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- plot_reactive()
      if (!is.null(p)) {
        ggsave(file, p, width = 12, height = 8, dpi = 300)
      }
    }
  )

  # Reference filter helpers
  ref_filters <- reactive({
    all_ages_labels <- unique(c(
      "All Ages",
      t_("All Ages", lang = "en"),
      t_("All Ages", lang = "fr")
    ))
    all_months_labels <- unique(c(
      "All Months",
      t_("All Months", lang = "en"),
      t_("All Months", lang = "fr")
    ))
    list(
      pts = input$province,
      ages = if (
        is.null(input$age_group) ||
          (length(input$age_group) == 1 &&
            any(all_ages_labels %in% input$age_group))
      ) {
        NULL
      } else {
        input$age_group
      },
      months = if (
        is.null(input$month) ||
          (length(input$month) == 1 && any(all_months_labels %in% input$month))
      ) {
        NULL
      } else {
        as.integer(input$month)
      }
    )
  })

  fb_filtered <- reactive({
    req(backend_ok)
    f <- ref_filters()
    fb_filter_micro(pt_names = f$pts, months = f$months, age_groups = f$ages)
  })

  # Data Info tab outputs
  output$ref_summary_ui <- renderUI({
    lang <- current_lang()
    f <- ref_filters()
    pts <- f$pts %||% translator$t("Canada")
    ages <- f$ages %||% translator$t("All Ages")

    # Get month names in current language
    if (is.null(f$months)) {
      months <- translator$t("All Months")
    } else {
      month_names <- fb_month_names(lang)
      months <- paste(month_names[as.integer(f$months)], collapse = ", ")
    }

    tagList(
      p(translator$t(
        "This app computes reference exposure percentages from Foodbook microdata, weighted and combined across your selected filters."
      )),
      tags$ul(
        tags$li(
          tags$b(translator$t("Reference PT(s): ")),
          paste(pts, collapse = ", ")
        ),
        tags$li(
          tags$b(translator$t("Age group(s): ")),
          paste(ages, collapse = ", ")
        ),
        tags$li(tags$b(translator$t("Month(s): ")), months)
      ),
      p(translator$t(
        "Tip: Defaults like \"Canada\" and \"All\" auto-deselect once you add another selection."
      ))
    )
  })

  # About page content (reactive to language changes)
  output$about_content <- renderUI({
    lang <- current_lang()
    tr <- translator

    tagList(
      # Purpose
      h4(tr$t("Purpose")),
      p(tr$t(
        "Compare your case exposures to typical population exposures from Foodbook to prioritise hypotheses during outbreak investigations."
      )),

      hr(),

      # Data Sources
      h4(tr$t("Data Sources")),
      p(tr$t(
        "Foodbook is a population-based survey conducted in all Canadian provinces and territories. It provides essential data on food, animal and water exposure used to understand, respond to, control and prevent enteric illness in Canada."
      )),
      tags$ul(
        tags$li(
          strong(tr$t("Foodbook 2.0 (2023-2024)")),
          ": ",
          tr$t(
            "Online and telephone survey with ~21,000 respondents across Canada"
          )
        ),
        tags$li(
          strong(tr$t("Foodbook 1.0 (2014-2015)")),
          ": ",
          tr$t(
            "Telephone survey with ~10,000 respondents (exposures marked with * are from this survey only)"
          )
        )
      ),

      hr(),

      # How references are computed
      h4(tr$t("How references are computed")),
      tags$ul(
        tags$li(tr$t("References use Foodbook microdata with survey weights.")),
        tags$li(tr$t(
          "If multiple PTs are selected, a single combined reference is computed across them."
        )),
        tags$li(tr$t(
          "You can optionally limit the reference by Age Group and Month."
        ))
      ),

      hr(),

      # Statistical Methodology
      h4(tr$t("Statistical Methodology")),
      p(tr$t(
        "The tool uses a one-sided binomial test to compare observed case exposure rates against population reference values:"
      )),
      tags$ul(
        tags$li(tr$t(
          "Null hypothesis: Case exposure rate ≤ Population reference rate"
        )),
        tags$li(tr$t(
          "Alternative hypothesis: Case exposure rate > Population reference rate"
        ))
      ),
      p(tr$t(
        "Reference percentages are calculated using survey weights to ensure population representativeness."
      )),

      hr(),

      # Interpretation Guide
      h4(tr$t("Interpretation Guide")),
      tags$ul(
        tags$li(
          strong(tr$t("Alert")),
          ": ",
          tr$t(
            "Observed exposure is significantly higher than reference (p < 0.05)"
          )
        ),
        tags$li(
          strong(tr$t("Borderline")),
          ": ",
          tr$t("Suggestive evidence (0.05 ≤ p < 0.10)")
        ),
        tags$li(
          strong(tr$t("Not Significant")),
          ": ",
          tr$t("No significant difference from reference (p ≥ 0.10)")
        ),
        tags$li(
          strong(tr$t("Insufficient Data")),
          ": ",
          tr$t("Too few cases to calculate statistics (< 5 total responses)")
        ),
        tags$li(
          strong(tr$t("No Reference Value")),
          ": ",
          tr$t("Exposure not found in Foodbook database")
        )
      ),

      hr(),

      # Limitations
      h4(tr$t("Limitations")),
      tags$ul(
        tags$li(tr$t(
          "Survey data may not reflect current food consumption patterns (data collected in 2014-2015 and 2023-2024)"
        )),
        tags$li(tr$t("Self-reported exposure data is subject to recall bias")),
        tags$li(tr$t(
          "Some exposures may have seasonal variations not captured when using annual data"
        )),
        tags$li(tr$t(
          "Small sample sizes in specific PT/age/month combinations may yield unstable estimates"
        )),
        tags$li(tr$t(
          "Exposures from Foodbook 1.0 (*) use different survey weights than Foodbook 2.0"
        ))
      ),

      hr(),

      # FAQ
      h4(tr$t("Frequently Asked Questions")),
      tags$div(
        class = "faq-section",
        tags$p(strong(tr$t(
          "Why is my exposure showing 'No Reference Value'?"
        ))),
        tags$p(
          tr$t(
            "This means the exposure was not asked in either Foodbook survey, or the variable name doesn't match. Try searching for a similar exposure name."
          ),
          style = "margin-bottom: 1rem;"
        ),

        tags$p(strong(tr$t("What does the * mean next to some exposures?"))),
        tags$p(
          tr$t(
            "Exposures marked with * are only available from Foodbook 1.0 (2014-2015). They are included for completeness but may not reflect current consumption patterns."
          ),
          style = "margin-bottom: 1rem;"
        ),

        tags$p(strong(tr$t(
          "Why do reference values change when I select different PTs?"
        ))),
        tags$p(
          tr$t(
            "Food consumption varies by region. The reference is recalculated using only respondents from the selected province(s)/territory(ies)."
          ),
          style = "margin-bottom: 1rem;"
        ),

        tags$p(strong(tr$t("How should I interpret 'Borderline' results?"))),
        tags$p(tr$t(
          "Borderline results (p-value between 0.05 and 0.10) suggest a possible association that warrants further investigation but doesn't meet conventional significance thresholds."
        ))
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

  output$ref_top_exposures <- renderDT({
    req(backend_ok)
    lang <- current_lang()
    f <- ref_filters()

    codes <- as.vector(fb_exposure_choices(
      lang,
      apply_public_exclusions = TRUE
    ))
    refs <- fb_reference_percents(
      codes,
      pt_names = f$pts,
      months = f$months,
      age_groups = f$ages
    )
    lbls <- names(fb_exposure_choices(lang, apply_public_exclusions = TRUE))
    names(lbls) <- as.vector(fb_exposure_choices(
      lang,
      apply_public_exclusions = TRUE
    ))

    tibble::tibble(
      Exposure = lbls[names(refs)],
      `Reference %` = round(as.numeric(refs), 1)
    ) %>%
      arrange(desc(`Reference %`)) %>%
      head(30) %>%
      datatable(
        options = list(pageLength = 10, order = list(list(1, 'desc'))),
        rownames = FALSE
      )
  })

  output$ref_pt_plot <- renderPlot({
    d <- fb_filtered()
    req(nrow(d) > 0)
    lang <- current_lang()

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
      labs(
        title = translator$t("Coverage by PT (after filters)"),
        x = NULL,
        y = translator$t("Records")
      ) +
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

    month_names_display <- if (lang == "fr") {
      fb_month_names("fr")
    } else {
      month.name
    }

    tibble::tibble(Month = as.integer(d$Month)) %>%
      filter(!is.na(Month), Month >= 1, Month <= 12) %>%
      mutate(
        MonthName = factor(
          month_names_display[Month],
          levels = month_names_display
        )
      ) %>%
      count(MonthName) %>%
      ggplot(aes(x = MonthName, y = n)) +
      geom_col(fill = "#1b7b57", alpha = 0.85) +
      labs(
        title = translator$t("Coverage by Month (after filters)"),
        x = NULL,
        y = translator$t("Records")
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  output$sys_ref_table <- renderDT({
    lang <- current_lang()
    tr <- Translator$new(
      translation_json_path = "../translations/translation.json"
    )
    tr$set_translation_language(lang)
    
    if (is.null(fb_env$toolkit_proportions) || is.null(fb_env$toolkit_exposures)) {
      fb_load_toolkit_data()
    }
    
    df <- fb_env$toolkit_proportions
    exposures <- fb_env$toolkit_exposures
    
    if (is.null(df) || is.null(exposures)) return(NULL)
    
    # Merge label and category based on language
    label_col <- if (lang == "fr") "exposure_fr" else "exposure_en"
    cat_col <- if (lang == "fr") "category_fr" else "category_en"
    
    # Create display table
    # Join proportions with exposure info
    display_df <- df |>
      dplyr::left_join(exposures, by = "exposure_number", suffix = c("", "_meta")) |>
      dplyr::select(
        !!cat_col, 
        !!label_col,
        variable_name,
        Canada, BC, AB, SK, MB, ON, QC, NB, NS, PE, NL, YT, NT, NU
      )
      
    # Rename columns for display
    colnames(display_df)[1:3] <- c(
      tr$t("Category"), 
      tr$t("Exposure"), 
      "Variable ID"
    )
    
    datatable(
      display_df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = 'top'
    ) |>
      formatRound(columns = 4:17, digits = 1) 
  })
}

# --- 7. Run Application ---
shinyApp(ui, server, enableBookmarking = "url")
