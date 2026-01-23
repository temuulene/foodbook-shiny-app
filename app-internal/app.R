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
source("../src/foodbook_backend.R")
source("../src/i18n_helper.R")
source("../src/common_ui.R")
source("../src/common_server.R")
source("../src/app_public_helpers.R")
source("../src/modules/language_selector_module.R")
source("../src/modules/mod_ref_settings.R")
source("../src/modules/mod_results_table.R")
source("../src/modules/mod_visualization.R")
source("../src/modules/mod_about.R")

# Suppress warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
options(bslib.precompiled = TRUE)

# --- 2. Helper Functions ---
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

# --- 3. User Interface (UI) ---
ui <- function(request) {
  translator <- Translator$new(translation_json_path = "../translations/translation.json")
  translator$set_translation_language("en")

  page_navbar(
    title = "Food Exposure Analysis Tool",
    lang = "en",
    theme = fb_theme(),
    header = tagList(
      fb_commons_head(),
      tags$div(
        id = "lang_selector_container",
        style = "display: none;",
        language_selector_ui("lang_selector", style = "dropdown")
      )
    ),

    # CEDARS Analysis Tab
    nav_panel(
      title = span(id = "nav-cedars-label", translator$t("CEDARS Analysis")),
      icon = icon("upload"),

      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          width = 350,
          uiOutput("sidebar_upload_title"),

          uiOutput("cedars_upload_section_ui"),
          
          hr(),
          uiOutput("sidebar_parameters_title"),
          
          # Reference Settings Module
          mod_ref_settings_ui("ref_settings")
        ),

        # Main panel
        card(
          card_header(uiOutput("card_results_label")),
          card_body(
             mod_results_table_ui("results_table")
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
          card_header(span(id = "card-ref-settings-label", translator$t("Reference Settings"))),
          card_body(uiOutput("ref_summary_ui"))
        ),
        card(
          card_header(span(id = "card-pop-snapshot-label", translator$t("Population Exposure Snapshot (Reference)"))),
          card_body(withSpinner(DTOutput("ref_top_exposures"), type = 4, color = "#0f4c81"))
        ),
        card(
          card_header(span(id = "card-cov-pt-label", translator$t("Microdata Coverage by PT (after filters)"))),
          card_body(withSpinner(plotOutput("ref_pt_plot", height = "350px"), type = 4, color = "#0f4c81"))
        ),
        card(
          card_header(span(id = "card-cov-month-label", translator$t("Microdata Coverage by Month (after filters)"))),
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
        card_header(h3(span(id = "card-about-label", translator$t("About")))),
        card_body(mod_about_ui("about"))
      )
    ),
  )
}

# --- 4. Server Logic ---
server <- function(input, output, session) {
  
  # Initialize shared logic
  common <- fb_init_common(session, "../translations/translation.json")
  translator <- common$translator
  current_lang <- common$current_lang
  get_tr <- common$get_tr

  # Reactive holding uploaded CEDARS data
  adv_cases <- reactiveVal(NULL)

  # Get unique PTs from uploaded cases for module parameters
  case_pts <- reactive({
    d <- adv_cases()
    if (is.null(d) || !"provinceterritory" %in% names(d)) return(NULL)
    unique_pts <- unique(d$provinceterritory[!is.na(d$provinceterritory)])
    if (length(unique_pts) == 0) return(NULL)
    fb_normalize_pt_values(unique_pts)
  })
  
  # Available PTs wrapper
  # Returns "Canada" + any PTs found in data to Ref Settings module
  available_pts_reactive <- reactive({
    pts <- case_pts()
    if (is.null(pts) || !length(pts)) return("Canada")
    fb_available_pts_from_cases(pts)
  })

  # --- Modules ---
  
  # Reference Settings Module (sidebar)
  ref_settings <- mod_ref_settings_server("ref_settings", 
                                          lang_reactive = current_lang,
                                          available_pts_reactive = available_pts_reactive,
                                          default_select_all = TRUE)
  
  selected_province <- ref_settings$province
  selected_age <- ref_settings$age_group
  selected_month <- ref_settings$month
  
  # About Module
  mod_about_server("about", lang_reactive = current_lang)

  # --- CEDARS File Processing ---

  output$cedars_upload_section_ui <- renderUI({
    tr <- get_tr()
    div(
      id = "cedars_upload_container",
      tooltip(
        fileInput("cedars_file", tr$t("Choose Excel file..."), accept = c(".xlsx"), buttonLabel = tr$t("Browse"), placeholder = tr$t("No file selected")),
        tr$t("Upload your CEDARS outbreak data export. The tool will automatically extract case exposure information.")
      ),
      helpText(tr$t("The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist).")),
      actionButton("cedars_clear", label = tr$t("Remove File"), icon = icon("trash"), class = "btn btn-outline-secondary w-100 mb-3")
    )
  })

  output$sidebar_upload_title <- renderUI({ h4(get_tr()$t("Upload CEDARS Exposure Data")) })
  output$sidebar_parameters_title <- renderUI({ h5(get_tr()$t("Analysis Parameters")) })
  output$card_results_label <- renderUI({
    span(id = "card-results-label", get_tr()$t("Results"))
  })

  observeEvent(input$cedars_file, {
    req(input$cedars_file)
    lang <- current_lang()
    tr <- get_tr()

    tryCatch({
      df <- withProgress(message = tr$t("Processing..."), value = 0, {
        path <- input$cedars_file$datapath
        
        # 1. Sheet finding
        exp_sheet <- find_sheet_by_columns(path, c("nationalid", "exposurecode", "hasexposureoccurred"))
        validate(need(exp_sheet$found, "Missing exposure columns"))
        
        # 2. Reading exposure
        df_exp <- readxl::read_excel(path, sheet = exp_sheet$sheet)
        names(df_exp) <- gsub("[^a-z0-9]+", "", tolower(names(df_exp)))
        df_exp <- df_exp %>% transmute(natid = as.character(.data$nationalid), exposure = as.character(.data$exposurecode), val = tolower(as.character(.data$hasexposureoccurred)))
        
        # 3. Find Linelist
        all_sheets <- readxl::excel_sheets(path)
        line_sheet <- NULL
        for (s in all_sheets) {
            if (s == exp_sheet$sheet) next
            d <- tryCatch(readxl::read_excel(path, sheet=s, n_max=1), error=function(e) NULL)
            cols <- gsub("[^a-z0-9]+", "", tolower(names(d)))
            if (("nationalid" %in% cols || "natid" %in% cols) && !("exposurecode" %in% cols)) { line_sheet <- s; break }
        }
        validate(need(!is.null(line_sheet), "Missing linelist sheet"))
        
        # 4. Read Linelist
        df_line <- readxl::read_excel(path, sheet=line_sheet)
        names(df_line) <- gsub("[^a-z0-9]+", "", tolower(names(df_line)))
        
        # 5. Filter & Merge
        if ("casestatus" %in% names(df_line)) df_line <- df_line %>% filter(tolower(as.character(casestatus)) == "confirmed")
        
        if (!"natid" %in% names(df_line)) df_line$natid <- as.character(df_line$nationalid)
        
        pt_values <- fb_extract_provinceterritory(df_line)
        df_line <- df_line %>% 
          transmute(
            natid = as.character(.data$natid),
            provinceterritory = pt_values
          ) %>%
          distinct(natid, .keep_all=TRUE)
          
        df <- df_exp %>% inner_join(df_line, by="natid", relationship="many-to-one")
        validate(need(nrow(df) > 0, "No matching cases"))
        
        showNotification(paste0(tr$t("Success"), ": ", length(unique(df$natid)), " ", tr$t("cases")), type="message")
        df
      })
      adv_cases(df)
    }, error = function(e) {
      adv_cases(NULL)
        shinyjs::js$resetFileInput(id = "cedars_file")
      if (!inherits(e, "shiny.silent.error")) showNotification(paste(tr$t("Error"), ": ", e$message), type = "error")
    })
  }) # ignoreNULL default

  observeEvent(input$cedars_clear, {
      adv_cases(NULL)
      shinyjs::js$resetFileInput(id = "cedars_file")
      showNotification(get_tr()$t("Upload cleared"), type="message")
  })

  # --- Results Calculation ---
  
  adv_results <- reactive({
    d <- adv_cases()
    if (is.null(d)) return(NULL)
    tr <- get_tr()
    lang <- current_lang()
    
    # Get filters from module
    # These are REACTIVES from module, so we call them()
    pts_selected <- selected_province() %||% character(0)
    ages_selected <- selected_age()
    months_selected <- selected_month()
    
    # Filter Logic
    d_filtered <- d
    if ("provinceterritory" %in% names(d) && length(pts_selected) > 0 && !("Canada" %in% pts_selected)) {
        d_filtered <- d %>% filter(is.na(provinceterritory) | provinceterritory %in% pts_selected)
    }
    
    # Determine Reference Scope
    if ("provinceterritory" %in% names(d_filtered)) {
      unique_case_pts <- unique(d_filtered$provinceterritory[!is.na(d_filtered$provinceterritory)])
      if (length(unique_case_pts) == 0) {
        scope_label <- tr$t("Canada"); ref_pts <- "Canada"
      } else {
        # Translate for label
        abbr_map <- fb_pt_abbrev_map()
        abbr_to_en <- stats::setNames(names(abbr_map), unname(abbr_map))
        map_display <- abbr_to_en[unique_case_pts]
        map_display[is.na(map_display)] <- unique_case_pts[is.na(map_display)]
        if (lang == "fr") {
          fr_map <- fb_pt_names_bilingual()
          map_display <- unname(fr_map[map_display])
          map_display[is.na(map_display)] <- unique_case_pts[is.na(map_display)]
        }
        scope_label <- paste(map_display, collapse=", ")
        ref_pts <- unique_case_pts
      }
    } else {
       scope_label <- tr$t("Canada"); ref_pts <- "Canada"
    }

    # Summarize
    exposure_counts <- d_filtered %>%
      mutate(val = dplyr::recode(val, y = "Y", n = "N", p = "P", dk = "DK")) %>%
      filter(val %in% c("Y", "N", "P", "DK")) %>%
      distinct(natid, exposure, val) %>%
      count(exposure, val) %>%
      tidyr::pivot_wider(names_from = val, values_from = n, values_fill = 0)
      
    if (nrow(exposure_counts) == 0) return(NULL)
    
    # Get Refs
    if (!is.null(ages_selected) && "All Ages" %in% ages_selected) ages_selected <- NULL
    if (!is.null(months_selected) && "All Months" %in% months_selected) months_selected <- NULL
    else if (!is.null(months_selected)) months_selected <- as.integer(months_selected)
    
    ref_perc <- fb_reference_percents(exposure_counts$exposure, pt_names = ref_pts, months = months_selected, age_groups = ages_selected)
    
    # Classify
    code_to_label <- names(fb_exposure_choices_all(lang))
    names(code_to_label) <- as.vector(fb_exposure_choices_all(lang))
    
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
      transmute(
          `Reference Scope` = scope_label,
          Exposure,
          `Total Valid` = total,
          Yes = Y %||% 0, Probably = P %||% 0, No = N %||% 0, DK = DK %||% 0,
          `Observed %` = observed_prop,
          `Reference %` = province_ref,
          `P-Value` = p_value,
          Classification
      )
      
    results$Classification <- purrr::map_chr(results$Classification, ~classification_label_i18n(., lang))
    results
  })

  # Pass results to modules
  mod_results_table_server("results_table", adv_results, current_lang)
  
  # Data Info (Legacy/Inline) - reused logic from Public approx
  output$ref_summary_ui <- renderUI({
     tr <- get_tr()
     # Just display selected params
     provs <- selected_province() %||% tr$t("Canada")
     if (!"Canada" %in% provs) {
       pt_map <- fb_pt_names(current_lang()); disp <- pt_map[provs]; disp[is.na(disp)] <- provs[is.na(disp)]
       provs <- disp
     } else provs <- tr$t("Canada")
     
     tagList(
       div(strong(tr$t("Location:")), paste(provs, collapse=", ")),
       div(strong(tr$t("Age Groups:")), paste(selected_age() %||% tr$t("All Ages"), collapse=", ")),
       div(strong(tr$t("Months:")), paste(selected_month() %||% tr$t("All Months"), collapse=", "))
     )
  })
  
  reference_table_data <- reactive({
    lang <- current_lang()
    choices <- fb_toolkit_exposure_choices(lang)
    if (!length(choices)) return(NULL)

    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    if (is.null(provs) || (length(provs) == 1 && provs == "Canada")) {
      backend_pt <- NULL
    } else {
      backend_pt <- provs
    }

    if (!is.null(ages) && "All Ages" %in% ages) ages <- NULL
    if (!is.null(months) && "All Months" %in% months) months <- NULL
    else if (!is.null(months)) months <- as.integer(months)

    tbl <- fb_public_build_reference_table(
      choices,
      pt_names = backend_pt,
      months = months,
      age_groups = ages
    )
    if (!nrow(tbl)) return(NULL)
    tbl
  })

  output$ref_top_exposures <- renderDT({
    tr <- get_tr()
    tbl <- reference_table_data()
    if (is.null(tbl) || !nrow(tbl)) return(NULL)

    top_tbl <- fb_public_top_exposures(tbl, n = 10)
    if (!nrow(top_tbl)) return(NULL)
    top_tbl$`Reference %` <- round(top_tbl$`Reference %`, 2)

    datatable(
      top_tbl,
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        language = list(
          zeroRecords = tr$t("No data available")
        )
      ),
      rownames = FALSE
    )
  })

  output$ref_pt_plot <- renderPlot({
    lang <- current_lang()
    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    if (is.null(provs) || (length(provs) == 1 && provs == "Canada")) provs <- NULL
    if (!is.null(ages) && "All Ages" %in% ages) ages <- NULL
    if (!is.null(months) && "All Months" %in% months) months <- NULL
    else if (!is.null(months)) months <- as.integer(months)

    df <- fb_filter_micro(pt_names = provs, months = months, age_groups = ages)
    cov <- fb_public_pt_coverage(df, lang = lang)
    req(nrow(cov) > 0)

    ggplot(cov, aes(x = reorder(PT, Count), y = Count)) +
      geom_col(fill = "#0f4c81") +
      coord_flip() +
      labs(x = NULL, y = get_tr()$t("Sample Size (n)")) +
      theme_minimal(base_size = 12)
  })

  output$ref_month_plot <- renderPlot({
    lang <- current_lang()
    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    if (is.null(provs) || (length(provs) == 1 && provs == "Canada")) provs <- NULL
    if (!is.null(ages) && "All Ages" %in% ages) ages <- NULL
    if (!is.null(months) && "All Months" %in% months) months <- NULL
    else if (!is.null(months)) months <- as.integer(months)

    df <- fb_filter_micro(pt_names = provs, months = months, age_groups = ages)
    cov <- fb_public_month_coverage(df, lang = lang)
    req(nrow(cov) > 0)
    cov$Month <- factor(cov$Month, levels = cov$Month)

    ggplot(cov, aes(x = Month, y = Count)) +
      geom_col(fill = "#0f4c81") +
      labs(x = NULL, y = get_tr()$t("Sample Size (n)")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# --- 5. Run the Application ---
shinyApp(ui = ui, server = server)
