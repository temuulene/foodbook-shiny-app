# Module: Reference Settings (Sidebar Parameters)
# Handles Province, Age Group, and Month selection

mod_ref_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("settings_ui"))
  )
}

mod_ref_settings_server <- function(
  id,
  lang_reactive,
  available_pts_reactive = reactive("Canada"),
  default_select_all = FALSE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper to get translator
    get_tr <- reactive({
      lang <- lang_reactive()
      # We assume global translator is set properly, but for safety inside module:
      tr <- Translator$new(translation_json_path = "../translations/translation.json")
      tr$set_translation_language(lang)
      tr
    })
    
    # Render UI
    output$settings_ui <- renderUI({
      tr <- get_tr()
      lang <- lang_reactive()
      
      tagList(
        tooltip(
          selectInput(
            ns("province"),
            tr$t("Reference PT(s)"),
            choices = NULL, # Updated by server
            selected = NULL,
            multiple = TRUE
          ),
          tr$t("PT = Province/Territory. Select which geographic areas to include in the reference population.")
        ),
        tooltip(
          selectInput(
            ns("age_group"),
            tr$t("Restrict by Age Group"),
            choices = NULL, # Updated by server
            selected = NULL,
            multiple = TRUE
          ),
          tr$t("Filter the reference population to specific age groups for age-stratified analysis.")
        ),
        tooltip(
          selectInput(
            ns("month"),
            tr$t("Restrict by Month"),
            choices = NULL, # Updated by server
            selected = NULL,
            multiple = TRUE
          ),
          tr$t("Filter the reference population by month(s) to account for seasonal variation.")
        )
      )
    })
    
    prev_pt_codes <- reactiveVal(NULL)

    # Update Inputs on Language or Available PTs Change
    observeEvent(list(lang_reactive(), available_pts_reactive()), {
      lang <- lang_reactive()
      tr <- get_tr()
      avail_pts <- available_pts_reactive()
      
      # Province Choices
      # Logic: "Canada" + localized names for available_pts
      # Note: available_pts are usually English codes (e.g. "ON", "BC") or "Canada"
      pt_codes <- setdiff(avail_pts, "Canada")

      # Build abbreviation -> localized name map (e.g. "ON" -> "Ontario")
      pt_en <- fb_pt_names("en")
      pt_fr <- fb_pt_names("fr")
      abbr_map <- fb_pt_abbrev_map()
      pt_abbr <- unname(abbr_map[pt_en])

      code_to_name <- character()
      if (length(pt_en) > 0 && length(pt_en) == length(pt_fr)) {
        labels <- if (lang == "fr") pt_fr else pt_en
        names(labels) <- pt_abbr
        code_to_name <- labels[!is.na(names(labels))]
      }
      
      # Construct choices vector
      choices_vec <- c()
      
      # Always offer Canada if it's the default scope or specifically available
      if ("Canada" %in% avail_pts || length(pt_codes) == 0) {
        choices_vec <- c(choices_vec, setNames("Canada", tr$t("Canada")))
      }
      
      if (length(pt_codes) > 0) {
        # Filter code_to_name by available pt_codes
        display_names <- code_to_name[pt_codes]
        # Handle any missing codes (fallback to code itself)
        na_idx <- is.na(display_names)
        display_names[na_idx] <- pt_codes[na_idx]

        choices_vec <- c(choices_vec, setNames(pt_codes, display_names))
      }

      # Preserve selection
      current_prov <- input$province
      if (isTRUE(default_select_all)) {
        new_codes <- sort(pt_codes)
        prev_codes <- prev_pt_codes()
        codes_changed <- is.null(prev_codes) || !identical(sort(prev_codes), new_codes)
        if (is.null(current_prov) || codes_changed) {
          if (length(new_codes)) {
            current_prov <- new_codes
          } else if ("Canada" %in% avail_pts) {
            current_prov <- "Canada"
          } else {
            current_prov <- character()
          }
        }
      } else if (is.null(current_prov)) {
        if ("Canada" %in% avail_pts) current_prov <- "Canada"
        else current_prov <- pt_codes
      }
      # Translate "Canada" in selection if needed (though backend expects "Canada")
      # Actually, input logic usually keeps "Canada" as value.
      
      updateSelectInput(
        session, 
        "province",
        label = tr$t("Reference PT(s)"),
        choices = choices_vec,
        selected = current_prov
      )
      prev_pt_codes(pt_codes)
      
      # Age Groups
      current_age <- input$age_group
      if (is.null(current_age)) current_age <- "All Ages"
      
      age_choices <- c(stats::setNames("All Ages", tr$t("All Ages")), fb_age_groups())
      updateSelectInput(
        session,
        "age_group",
        label = tr$t("Restrict by Age Group"),
        choices = age_choices,
        selected = current_age
      )
      
      # Months
      current_month <- input$month
      if (is.null(current_month)) current_month <- "All Months"
      
      month_choices <- c(stats::setNames("All Months", tr$t("All Months")), stats::setNames(as.character(1:12), fb_month_names(lang)))
      updateSelectInput(
        session,
        "month",
        label = tr$t("Restrict by Month"),
        choices = month_choices,
        selected = current_month
      )
      
    }, ignoreInit = FALSE)
    
    prev_province <- reactiveVal(NULL)
    prev_age_group <- reactiveVal(NULL)
    prev_month <- reactiveVal(NULL)

    # If default is selected alongside others, keep default and clear others.
    # If default was already selected and user adds others, drop default.
    observeEvent(input$province, {
      selected <- fb_public_resolve_default_selection(
        input$province,
        "Canada",
        prev_province()
      )
      if (!identical(selected, input$province)) {
        updateSelectInput(session, "province", selected = selected)
      }
      prev_province(selected)
    })
    
    observeEvent(input$age_group, {
      selected <- fb_public_resolve_default_selection(
        input$age_group,
        "All Ages",
        prev_age_group()
      )
      if (!identical(selected, input$age_group)) {
        updateSelectInput(session, "age_group", selected = selected)
      }
      prev_age_group(selected)
    })

    observeEvent(input$month, {
      selected <- fb_public_resolve_default_selection(
        input$month,
        "All Months",
        prev_month()
      )
      if (!identical(selected, input$month)) {
        updateSelectInput(session, "month", selected = selected)
      }
      prev_month(selected)
    })
    
    # Return Reactive Values
    list(
      province = reactive(input$province),
      age_group = reactive(input$age_group),
      month = reactive(input$month)
    )
  })
}
