# Module: Data Info
# Renders the Data Info tab: summary, top exposures, PT coverage, month coverage

mod_data_info_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(span(id = ns("card-ref-settings-label"), uiOutput(ns("ref_settings_title"), inline = TRUE))),
      card_body(uiOutput(ns("ref_summary_ui")))
    ),
    card(
      card_header(span(id = ns("card-pop-snapshot-label"), uiOutput(ns("pop_snapshot_title"), inline = TRUE))),
      card_body(withSpinner(DTOutput(ns("ref_top_exposures")), type = 4))
    ),
    card(
      full_screen = TRUE,
      card_header(span(id = ns("card-cov-pt-label"), uiOutput(ns("cov_pt_title"), inline = TRUE))),
      card_body(withSpinner(plotOutput(ns("ref_pt_plot"), height = "350px"), type = 4))
    ),
    card(
      full_screen = TRUE,
      card_header(span(id = ns("card-cov-month-label"), uiOutput(ns("cov_month_title"), inline = TRUE))),
      card_body(withSpinner(plotOutput(ns("ref_month_plot"), height = "350px"), type = 4))
    )
  )
}

mod_data_info_server <- function(
  id,
  get_tr,
  current_lang,
  selected_province,
  selected_age,
  selected_month,
  reference_table_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Card header titles (reactive for language switching)
    output$ref_settings_title <- renderUI({ get_tr()$t("Reference Settings") })
    output$pop_snapshot_title <- renderUI({ get_tr()$t("Population Exposure Snapshot (Reference)") })
    output$cov_pt_title <- renderUI({ get_tr()$t("Microdata Coverage by PT (after filters)") })
    output$cov_month_title <- renderUI({ get_tr()$t("Microdata Coverage by Month (after filters)") })

    # Filter summary
    output$ref_summary_ui <- renderUI({
      tr <- get_tr()
      provs <- selected_province() %||% tr$t("Canada")
      ages  <- selected_age() %||% tr$t("All Ages")
      months <- selected_month() %||% tr$t("All Months")

      if (!"Canada" %in% provs) {
        pt_map <- fb_pt_names(current_lang())
        disp <- pt_map[provs]
        disp[is.na(disp)] <- provs[is.na(disp)]
        provs <- disp
      } else {
        provs <- tr$t("Canada")
      }

      tagList(
        div(strong(tr$t("Location:")), paste(provs, collapse = ", ")),
        div(strong(tr$t("Age Groups:")), paste(ages, collapse = ", ")),
        div(strong(tr$t("Months:")), paste(months, collapse = ", "))
      )
    })

    # Top exposures snapshot table
    output$ref_top_exposures <- renderDT({
      tr <- get_tr()
      tbl <- reference_table_data()
      if (is.null(tbl) || !nrow(tbl)) return(NULL)

      top_tbl <- fb_public_top_exposures(tbl, n = 10)
      if (!nrow(top_tbl)) return(NULL)
      top_tbl$`Reference %` <- round(top_tbl$`Reference %`, 2)

      # Find Code column index (0-based) to hide it from display
      code_col_idx <- which(names(top_tbl) == "Code") - 1

      datatable(
        top_tbl,
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = if (length(code_col_idx) > 0) {
            list(list(visible = FALSE, targets = code_col_idx))
          } else {
            list()
          },
          language = list(
            zeroRecords = tr$t("No data available")
          )
        ),
        rownames = FALSE
      )
    })

    # PT coverage bar chart
    output$ref_pt_plot <- renderPlot({
      req(current_lang(), selected_province(), selected_age(), selected_month())
      lang <- current_lang()
      provs <- selected_province()
      ages  <- selected_age()
      months <- selected_month()

      filters <- fb_normalize_filters(provs, ages, months)
      provs <- filters$pt; ages <- filters$age; months <- filters$month

      df  <- fb_filter_micro(pt_names = provs, months = months, age_groups = ages)
      cov <- fb_public_pt_coverage(df, lang = lang)
      req(nrow(cov) > 0)

      ggplot(cov, aes(x = reorder(PT, Count), y = Count)) +
        geom_col(fill = "#0f4c81") +
        coord_flip() +
        labs(x = NULL, y = get_tr()$t("Sample Size (n)")) +
        theme_minimal(base_size = 12)
    })

    # Month coverage bar chart
    output$ref_month_plot <- renderPlot({
      req(current_lang(), selected_province(), selected_age(), selected_month())
      lang <- current_lang()
      provs <- selected_province()
      ages  <- selected_age()
      months <- selected_month()

      filters <- fb_normalize_filters(provs, ages, months)
      provs <- filters$pt; ages <- filters$age; months <- filters$month

      df  <- fb_filter_micro(pt_names = provs, months = months, age_groups = ages)
      cov <- fb_public_month_coverage(df, lang = lang)
      req(nrow(cov) > 0)
      cov$Month <- factor(cov$Month, levels = cov$Month)

      ggplot(cov, aes(x = Month, y = Count)) +
        geom_col(fill = "#0f4c81") +
        labs(x = NULL, y = get_tr()$t("Sample Size (n)")) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}
