# Module: Data Info
# Renders the Data Info tab: summary, top exposures, PT coverage, month coverage

mod_data_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Row 1: Summary (left, compact) + Snapshot table (right, wider)
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header(
          class = "bg-light",
          span(
            id = ns("card-ref-settings-label"),
            uiOutput(ns("ref_settings_title"), inline = TRUE)
          )
        ),
        card_body(
          padding = 0,
          div(
            style = "padding: 0.4rem 0.8rem;",
            uiOutput(ns("ref_summary_ui"))
          )
        )
      ),
      card(
        card_header(
          class = "bg-light",
          span(
            id = ns("card-pop-snapshot-label"),
            uiOutput(ns("pop_snapshot_title"), inline = TRUE)
          )
        ),
        card_body(
          padding = 0,
          DTOutput(ns("ref_top_exposures"))
        )
      )
    ),
    # Row 2: Both coverage plots — full width, generous height
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light",
          span(
            id = ns("card-cov-pt-label"),
            uiOutput(ns("cov_pt_title"), inline = TRUE)
          )
        ),
        card_body(
          padding = 0,
          plotOutput(ns("ref_pt_plot"), height = "420px")
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light",
          span(
            id = ns("card-cov-month-label"),
            uiOutput(ns("cov_month_title"), inline = TRUE)
          )
        ),
        card_body(
          padding = 0,
          plotOutput(ns("ref_month_plot"), height = "420px")
        )
      )
    )
  )
}

# Shared publication-quality theme for Data Info plots
# No title/subtitle — the card header provides that context
.di_plot_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.margin = margin(8, 12, 4, 4),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#ecf0f1", linewidth = 0.4),
      axis.text.y = element_text(size = base_size, colour = "#2c3e50"),
      axis.text.x = element_text(size = base_size - 1, colour = "#495057"),
      axis.title = element_text(size = base_size - 1, colour = "#6c757d"),
      legend.position = "none"
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

    # ── Colour palette ────────────────────────────────────────────────
    # Navy-to-teal gradient for ranked bars
    .bar_palette <- c(
      "#0b3d6e",
      "#0f4c81",
      "#1a6faa",
      "#2590c8",
      "#36a2d4",
      "#4fb3db",
      "#6cc4e0",
      "#8dd3e5",
      "#ade1ed",
      "#cdeef5"
    )

    # Card header titles (reactive for language switching)
    output$ref_settings_title <- renderUI({
      get_tr()$t("Reference Settings")
    })
    output$pop_snapshot_title <- renderUI({
      get_tr()$t("Population Exposure Snapshot (Reference)")
    })
    output$cov_pt_title <- renderUI({
      get_tr()$t("Microdata Coverage by PT (after filters)")
    })
    output$cov_month_title <- renderUI({
      get_tr()$t("Microdata Coverage by Month (after filters)")
    })

    # ── Compact filter summary + data source badge ────────────────────
    output$ref_summary_ui <- renderUI({
      tr <- get_tr()
      lang <- current_lang()
      provs <- selected_province() %||% tr$t("Canada")
      ages <- selected_age() %||% tr$t("All Ages")
      months <- selected_month() %||% tr$t("All Months")

      if (!"Canada" %in% provs) {
        pt_map <- fb_pt_names(lang)
        disp <- pt_map[provs]
        disp[is.na(disp)] <- provs[is.na(disp)]
        provs <- disp
      } else {
        provs <- tr$t("Canada")
      }

      # Build data source badge from microdata
      source_badges <- NULL
      micro <- fb_env[["micro"]]
      if (!is.null(micro) && "fb_source" %in% names(micro)) {
        src_tbl <- table(micro[["fb_source"]])
        badge_items <- lapply(names(src_tbl), function(src) {
          n <- as.integer(src_tbl[[src]])
          span(
            class = "badge bg-info text-dark me-1",
            style = "font-size: 0.85em;",
            paste0(
              src,
              ": ",
              format(n, big.mark = ","),
              " ",
              tr$t("respondents")
            )
          )
        })
        source_badges <- div(
          style = "display: inline-flex; gap: 0.25rem; margin-left: 1rem;",
          badge_items
        )
      } else if (!is.null(micro)) {
        n <- nrow(micro)
        source_badges <- div(
          style = "display: inline-flex; margin-left: 1rem;",
          span(
            class = "badge bg-info text-dark",
            style = "font-size: 0.85em;",
            paste0(
              "Foodbook: ",
              format(n, big.mark = ","),
              " ",
              tr$t("respondents")
            )
          )
        )
      }

      # Single-line compact layout
      div(
        style = "display: flex; flex-wrap: wrap; align-items: center; gap: 0.75rem;",
        span(strong(tr$t("Location:")), paste(provs, collapse = ", ")),
        span(style = "color: #adb5bd;", "\u2022"),
        span(strong(tr$t("Age Groups:")), paste(ages, collapse = ", ")),
        span(style = "color: #adb5bd;", "\u2022"),
        span(strong(tr$t("Months:")), paste(months, collapse = ", ")),
        source_badges
      )
    })

    # ── Top exposures snapshot table ──────────────────────────────────
    output$ref_top_exposures <- renderDT({
      tr <- get_tr()
      tbl <- reference_table_data()
      if (is.null(tbl) || !nrow(tbl)) {
        return(NULL)
      }

      top_tbl <- fb_public_top_exposures(tbl, n = 10)
      if (!nrow(top_tbl)) {
        return(NULL)
      }
      top_tbl$`Reference %` <- round(top_tbl$`Reference %`, 2)

      # Find Code column index (0-based) to hide it from display
      code_col_idx <- which(names(top_tbl) == "Code") - 1
      ref_col_idx <- which(names(top_tbl) == "Reference %") - 1

      datatable(
        top_tbl,
        options = list(
          dom = "t",
          pageLength = 10,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE,
          paging = FALSE,
          columnDefs = c(
            if (length(code_col_idx) > 0) {
              list(list(visible = FALSE, targets = code_col_idx))
            } else {
              list()
            },
            list(list(className = "dt-right", targets = ref_col_idx))
          ),
          language = list(
            zeroRecords = tr$t("No data available")
          )
        ),
        rownames = FALSE
      ) |>
        formatStyle(
          "Reference %",
          background = styleColorBar(
            range(top_tbl$`Reference %`, na.rm = TRUE),
            "#d6eaf8"
          ),
          backgroundSize = "98% 78%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    # ── PT coverage bar chart (horizontal, descending) ─────────────
    pt_coverage_data <- reactive({
      req(current_lang(), selected_province(), selected_age(), selected_month())
      lang <- current_lang()
      provs <- selected_province()
      ages <- selected_age()
      months <- selected_month()

      filters <- fb_normalize_filters(provs, ages, months)
      provs <- filters$pt
      ages <- filters$age
      months <- filters$month

      df <- fb_filter_micro(
        pt_names = provs,
        months = months,
        age_groups = ages
      )
      fb_public_pt_coverage(df, lang = lang)
    })

    output$ref_pt_plot <- renderPlot(
      {
        cov <- pt_coverage_data()
        req(nrow(cov) > 0)
        tr <- get_tr()

        # Rank-based colour mapping (most → darkest)
        # Name fills by PT name (row-aligned) BEFORE factoring to avoid mismatch
        n <- nrow(cov)
        pal <- colorRampPalette(.bar_palette)(max(n, 2))
        fill_vals <- rlang::set_names(
          pal[rank(-cov$Count, ties.method = "first")],
          cov$PT
        )

        # Descending order: largest count at the TOP of the chart
        cov$PT <- factor(cov$PT, levels = cov$PT[order(cov$Count)])

        ggplot(cov, aes(x = Count, y = PT, fill = PT)) +
          geom_col(width = 0.7, show.legend = FALSE) +
          geom_text(
            aes(label = scales::comma(Count)),
            hjust = -0.15,
            size = 3.5,
            fontface = "bold",
            colour = "#2c3e50"
          ) +
          scale_fill_manual(values = fill_vals) +
          scale_x_continuous(
            expand = expansion(mult = c(0, 0.15)),
            labels = scales::comma
          ) +
          labs(x = tr$t("Sample Size (n)"), y = NULL) +
          .di_plot_theme(base_size = 13)
      },
      res = 96
    )

    # ── Month coverage bar chart (vertical, chronological) ────────────
    output$ref_month_plot <- renderPlot(
      {
        req(
          current_lang(),
          selected_province(),
          selected_age(),
          selected_month()
        )
        lang <- current_lang()
        provs <- selected_province()
        ages <- selected_age()
        months <- selected_month()
        tr <- get_tr()

        filters <- fb_normalize_filters(provs, ages, months)
        provs <- filters$pt
        ages <- filters$age
        months <- filters$month

        df <- fb_filter_micro(
          pt_names = provs,
          months = months,
          age_groups = ages
        )
        cov <- fb_public_month_coverage(df, lang = lang)
        req(nrow(cov) > 0)
        cov$Month <- factor(cov$Month, levels = cov$Month)

        # Gentle seasonal gradient (teal → warm)
        n <- nrow(cov)
        month_pal <- colorRampPalette(c(
          "#1a6faa",
          "#2590c8",
          "#36a2d4",
          "#e67e22"
        ))(n)

        ggplot(cov, aes(x = Month, y = Count, fill = Month)) +
          geom_col(width = 0.7, show.legend = FALSE) +
          geom_text(
            aes(label = scales::comma(Count)),
            vjust = -0.5,
            size = 4,
            fontface = "bold",
            colour = "#2c3e50"
          ) +
          scale_fill_manual(
            values = rlang::set_names(month_pal, levels(cov$Month))
          ) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.15)),
            labels = scales::comma
          ) +
          labs(x = NULL, y = tr$t("Sample Size (n)")) +
          .di_plot_theme(base_size = 13) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(
              colour = "#ecf0f1",
              linewidth = 0.4
            ),
            axis.text.x = element_text(
              size = 11,
              colour = "#2c3e50",
              angle = 35,
              hjust = 1,
              vjust = 1
            )
          )
      },
      res = 96
    )
  })
}
