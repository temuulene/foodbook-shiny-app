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
  get_tr,
  available_pts_reactive = reactive("Canada"),
  available_ages_reactive = reactive(NULL),
  available_months_reactive = reactive(NULL),
  default_select_all = FALSE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render UI
    output$settings_ui <- renderUI({
      tr <- get_tr()
      lang <- tr$get_translation_language()
      
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

    # Update Inputs on Language or Available Data Change
    observeEvent(list(get_tr(), available_pts_reactive(), available_ages_reactive(), available_months_reactive()), {
      lang <- get_tr()$get_translation_language()
      tr <- get_tr()
      avail_pts <- available_pts_reactive()
      avail_ages <- available_ages_reactive()
      avail_months <- available_months_reactive()
      
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

      # Preserve selection (Province)
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
      
      # Dynamic Age Choices
      # If avail_ages is present (from upload), filter standard choices to just those present + "All Ages"
      # Otherwise show all standard choices
      std_age_choices <- fb_age_groups()
      
      if (!is.null(avail_ages) && length(avail_ages) > 0) {
        # Only keep ages that exist in uploaded data
        # Check against names or values? fb_age_groups() return named vector: c("0-4 Years" = "0-4 Years", ...)
        # Actually in app code we extract raw age strings. Assuming they match standard keys.
        # Let's interact: filter std_age_choices where value is in avail_ages
        valid_ages <- std_age_choices[std_age_choices %in% avail_ages]
        # Also include any custom ages found in file but not in standard list?
        # Ideally yes, but fb_toolkit requires standard ages for ref data.
        # If the file has non-standard ages, they won't match ref table anyway.
        # So strict filtering is safer for now.
        if (length(valid_ages) > 0) {
           age_choices_list <- c(stats::setNames("All Ages", tr$t("All Ages")), valid_ages)
        } else {
           # Fallback if no matches (weird data format?) -> show all
           age_choices_list <- c(stats::setNames("All Ages", tr$t("All Ages")), std_age_choices)
        }
      } else {
        age_choices_list <- c(stats::setNames("All Ages", tr$t("All Ages")), std_age_choices)
      }

      updateSelectInput(
        session,
        "age_group",
        label = tr$t("Restrict by Age Group"),
        choices = age_choices_list,
        selected = current_age
      )
      
      # Months
      current_month <- input$month
      if (is.null(current_month)) current_month <- "All Months"
      
      # Dynamic Month Choices
      # avail_months is char vector "1".."12"
      std_month_choices <- stats::setNames(as.character(1:12), fb_month_names(lang))
      
      if (!is.null(avail_months) && length(avail_months) > 0) {
         # Filter standard list
         # std_month_choices values are "1", "2"...
         valid_months <- std_month_choices[std_month_choices %in% avail_months]
         if (length(valid_months) > 0) {
             month_choices_list <- c(stats::setNames("All Months", tr$t("All Months")), valid_months)
         } else {
             month_choices_list <- c(stats::setNames("All Months", tr$t("All Months")), std_month_choices)
         }
      } else {
         month_choices_list <- c(stats::setNames("All Months", tr$t("All Months")), std_month_choices)
      }

      updateSelectInput(
        session,
        "month",
        label = tr$t("Restrict by Month"),
        choices = month_choices_list,
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
