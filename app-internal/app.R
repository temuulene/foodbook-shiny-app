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
source("../src/modules/mod_data_info.R")

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
    header = fb_commons_head(),

    # CEDARS Analysis Tab
    nav_panel(
      title = span(id = "nav-cedars-label", translator$t("CEDARS Analysis")),
      icon = icon("upload"),

      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          width = 350,
          uiOutput("sidebar_upload_title"),
          accordion(
            open = c("upload_panel", "ref_settings_panel"),
            accordion_panel(
              title = span(id = "acc-upload-label", translator$t("Upload CEDARS Data")),
              value = "upload_panel",
              icon = icon("upload"),
              uiOutput("cedars_upload_section_ui")
            ),
            accordion_panel(
              title = span(id = "acc-ref-settings-label", translator$t("Analysis Parameters")),
              value = "ref_settings_panel",
              icon = icon("sliders"),
              mod_ref_settings_ui("ref_settings")
            )
          )
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
      mod_data_info_ui("data_info")
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

    # Right-side navbar items
    nav_spacer(),
    nav_item(
      language_selector_ui("lang_selector", style = "dropdown")
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )
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

  # Unique Age Groups from uploaded cases
  case_ages <- reactive({
    d <- adv_cases()
    if (is.null(d) || !"age_group_raw" %in% names(d)) return(NULL)
    unique(d$age_group_raw[!is.na(d$age_group_raw)])
  })

  # Unique Months from uploaded cases
  case_months <- reactive({
    d <- adv_cases()
    if (is.null(d) || !"episode_date" %in% names(d)) return(NULL)
    dates <- d$episode_date[!is.na(d$episode_date)]
    if (length(dates) == 0) return(NULL)
    
    # Try to parse date if character, otherwise assume Date/POSIXct
    # Extract month number (1-12)
    # Using format() handles Date and POSIXct
    # If character, need to be careful, but readxl usually gives POSIXct for dates
    tryCatch({
      m_params <- unique(as.integer(format(as.Date(dates), "%m")))
      as.character(sort(m_params))
    }, error = function(e) NULL)
  })

  # --- Modules ---
  
  # Reference Settings Module (sidebar)
  ref_settings <- mod_ref_settings_server("ref_settings", 
                                          get_tr = get_tr,
                                          available_pts_reactive = available_pts_reactive,
                                          available_ages_reactive = case_ages,
                                          available_months_reactive = case_months,
                                          default_select_all = TRUE)
  
  selected_province <- ref_settings$province
  selected_age <- ref_settings$age_group
  selected_month <- ref_settings$month
  
  # About Module
  mod_about_server("about", get_tr = get_tr)

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
        df_exp <- df_exp |> transmute(natid = as.character(.data$nationalid), exposure = as.character(.data$exposurecode), val = tolower(as.character(.data$hasexposureoccurred)))
        
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
        if ("casestatus" %in% names(df_line)) df_line <- df_line |> filter(tolower(as.character(casestatus)) == "confirmed")
        
        if (!"natid" %in% names(df_line)) df_line$natid <- as.character(df_line$nationalid)
        
        pt_values <- fb_extract_provinceterritory(df_line)
        has_agegroup <- "agegroup" %in% names(df_line)
        has_episodedate <- "episodedate" %in% names(df_line)
        df_line <- dplyr::transmute(
          df_line,
          natid = as.character(.data$natid),
          provinceterritory = pt_values,
          age_group_raw = if (has_agegroup) as.character(.data$agegroup) else NA_character_,
          episode_date = if (has_episodedate) .data$episodedate else NA
        ) |>
          distinct(natid, .keep_all = TRUE)
          
        df <- df_exp |> inner_join(df_line, by="natid", relationship="many-to-one")
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
        d_filtered <- d |> filter(is.na(provinceterritory) | provinceterritory %in% pts_selected)
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
    exposure_counts <- d_filtered |>
      mutate(val = dplyr::recode_values(val, from = c("y", "n", "p", "dk"), to = c("Y", "N", "P", "DK"), default = val)) |>
      filter(val %in% c("Y", "N", "P", "DK")) |>
      distinct(natid, exposure, val) |>
      count(exposure, val) |>
      tidyr::pivot_wider(names_from = val, values_from = n, values_fill = 0)
      
    if (nrow(exposure_counts) == 0) return(NULL)
    
    # Normalize age/month filters
    filters <- fb_normalize_filters(ref_pts, ages_selected, months_selected)
    ages_selected <- filters$age
    months_selected <- filters$month
    
    ref_perc <- fb_reference_percents(exposure_counts$exposure, pt_names = ref_pts, months = months_selected, age_groups = ages_selected)
    
    # Build analysis input
    code_to_label <- names(fb_exposure_choices_all(lang))
    names(code_to_label) <- as.vector(fb_exposure_choices_all(lang))

    analysis_df <- data.frame(
      ExposureLabel = vapply(
        exposure_counts$exposure,
        function(x) sub(" \\([^)]+\\)$", "", code_to_label[x] %||% x),
        character(1)
      ),
      Y = exposure_counts$Y %||% 0L,
      P = exposure_counts$P %||% 0L,
      N = exposure_counts$N %||% 0L,
      DK = exposure_counts$DK %||% 0L,
      ref_pct = as.numeric(ref_perc[match(exposure_counts$exposure, names(ref_perc))]),
      scope_label = scope_label,
      stringsAsFactors = FALSE
    )

    fb_classify_results(analysis_df, lang = lang)
  })

  # Pass results to modules
  mod_results_table_server("results_table", adv_results, get_tr)
  
  reference_table_data <- reactive({
    lang <- current_lang()
    choices <- fb_toolkit_exposure_choices(lang)
    if (!length(choices)) return(NULL)

    provs <- selected_province()
    ages <- selected_age()
    months <- selected_month()

    filters <- fb_normalize_filters(provs, ages, months)
    backend_pt <- filters$pt
    ages <- filters$age
    months <- filters$month

    tbl <- fb_public_build_reference_table(
      choices,
      pt_names = backend_pt,
      months = months,
      age_groups = ages
    )
    if (!nrow(tbl)) return(NULL)
    tbl
  })

  # Data Info (Module) -- defined after reference_table_data
  mod_data_info_server(
    "data_info",
    get_tr = get_tr,
    current_lang = current_lang,
    selected_province = selected_province,
    selected_age = selected_age,
    selected_month = selected_month,
    reference_table_data = reference_table_data
  )
}

# --- 5. Run the Application ---
thematic::thematic_shiny()
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))
shinyApp(ui = ui, server = server)
