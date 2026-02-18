# Shared Server Logic for Foodbook Shiny Apps
# Used by both app-public and app-internal

fb_init_common <- function(session, translation_path = "../translations/translation.json") {
  # Initialize translator
  translator <- init_translator(session, translation_path = translation_path)
  
  # Initialize backend logic
  # Note: The specific 'lang' passed to fb_init might depend on session state
  # For now, we initialize with English default, and updates happen reactively
  fb_init(lang = "en")
  
  # Language selector logic
  lang_state <- language_selector_server("lang_selector",
                                         session_parent = session,
                                         style = "dropdown")
  
  current_lang <- lang_state$language
  
  # Reactive translator getter
  get_tr <- reactive({
    lang <- current_lang()
    set_language(lang, session)
    get_translator(session)
  })

  # Common Observers for Language Changes
  observeEvent(current_lang(), {
    lang <- current_lang()
    tr <- get_tr()
    
    # Update backend language
    fb_update_language(lang = lang)
    
    # Update navbar title
    navbar_title <- tr$t("Food Exposure Analysis Tool")
    session$sendCustomMessage(
      "update-navbar-title",
      navbar_title
    )
    
    # Update button labels
    session$sendCustomMessage(
      "update-button-labels",
      list(
        reset = tr$t("Reset Inputs"),
        bookmark = tr$t("Bookmark Analysis"),
        download = tr$t("Download Plot"),
        cedars_clear = tr$t("Remove File"),
        xlsx_clear = tr$t("Remove File")
      )
    )
    
     # Update sidebar title
    session$sendCustomMessage(
      "update-sidebar-title",
      tr$t("Analysis Parameters")
    )
    
    # Update tab names 
    session$sendCustomMessage(
      "update-tab-names",
      list(
        analysis = tr$t("Analysis"),
        reference_data = tr$t("Reference Data"),
        data_info = tr$t("Data Info"),
        about = tr$t("About"),
        cedars = tr$t("CEDARS Analysis"), 
        results = tr$t("Results"),
        visualization = tr$t("Visualization")
      )
    )

    # Update accordion titles
    session$sendCustomMessage(
      "update-accordion-titles",
      list(
        reference_settings = tr$t("Reference Settings"),
        upload_exposure = tr$t("Upload Exposure Counts (Optional)"),
        actions = tr$t("Actions")
      )
    )

    # Update card headers
    session$sendCustomMessage(
      "update-card-headers",
      list(
        exposure_data_input = tr$t("Exposure Data Input"),
        reference_settings = tr$t("Reference Settings"),
        population_snapshot = tr$t("Population Exposure Snapshot (Reference)"),
        microdata_pt = tr$t("Microdata Coverage by PT (after filters)"),
        microdata_month = tr$t("Microdata Coverage by Month (after filters)"),
        about_tool = tr$t("About This Tool"),
        results = tr$t("Results"),
        reference_values = tr$t("Reference Values")
      )
    )

    # Update misc labels
    session$sendCustomMessage(
      "update-misc-labels",
      list(
        auto_detect_help = tr$t("The app will auto-detect sheets with required columns: NationalID, ExposureCode, HasExposureOccurred (exposure data) and NationalID (linelist)."),
        enter_case_counts = tr$t("Enter case counts for each exposure in each selected location."),
        fb1_asterisk = tr$t("* Exposures from Foodbook 1.0"),
        fb1_only_asterisk = tr$t("* Exposures from Foodbook 1 only")
      )
    )
  }, ignoreInit = FALSE)  # Changed to FALSE so messages are sent on initial load
  
  list(
    translator = translator,
    current_lang = current_lang,
    get_tr = get_tr
  )
}

fb_public_resolve_default_selection <- function(selected, default_value, previous = NULL) {
  if (is.null(selected) || !length(selected)) {
    return(selected)
  }

  if (default_value %in% selected && length(selected) > 1) {
    default_was_selected <- !is.null(previous) && default_value %in% previous
    if (default_was_selected) {
      return(setdiff(selected, default_value))
    }
    return(default_value)
  }

  selected
}

