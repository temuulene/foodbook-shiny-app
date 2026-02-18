# Module: Visualization
# Renders enhanced visualizations with diverging lollipop chart,
# classification coloring, value box summaries, and interactive features

mod_visualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Summary value boxes strip
    uiOutput(ns("summary_boxes")),
    # Download controls
    div(
      style = "margin-bottom: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap;",
      uiOutput(ns("download_btn_ui"))
    ),
    # Main plot container
    uiOutput(ns("plot_container"))
  )
}

mod_visualization_server <- function(id, results_data_reactive, get_tr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Classification colors (semantic, accessible)
    class_colors <- c(
      "Alert" = "#c0392b",     # strong red
      "Alerte" = "#c0392b",
      "Borderline" = "#e67e22", # warm orange
      "Limite" = "#e67e22",
      "Normal" = "#6c757d",     # neutral gray
      "Normale" = "#6c757d"
    )

    # --- Summary Value Boxes ---
    output$summary_boxes <- renderUI({
      res <- results_data_reactive()
      tr <- get_tr()
      if (is.null(res) || nrow(res) == 0) return(NULL)

      n_total <- nrow(res)
      n_alert <- sum(res$Classification %in% c("Alert", "Alerte"), na.rm = TRUE)
      n_border <- sum(res$Classification %in% c("Borderline", "Limite"), na.rm = TRUE)
      n_normal <- n_total - n_alert - n_border

      layout_column_wrap(
        width = 1 / 3,
        fill = FALSE,
        class = "summary-value-box",
        value_box(
          title = tr$t("Alert"),
          value = n_alert,
          showcase = icon("circle-exclamation"),
          theme = "danger"
        ),
        value_box(
          title = tr$t("Borderline"),
          value = n_border,
          showcase = icon("triangle-exclamation"),
          theme = "warning"
        ),
        value_box(
          title = tr$t("Normal"),
          value = n_normal,
          showcase = icon("circle-check"),
          theme = "secondary"
        )
      )
    })

    # --- Download Buttons ---
    output$download_btn_ui <- renderUI({
      tr <- get_tr()
      tagList(
        downloadButton(ns("download_plot_png"), paste(tr$t("Download Plot"), "(PNG)"), class = "btn-secondary btn-sm"),
        downloadButton(ns("download_plot_svg"), paste(tr$t("Download Plot"), "(SVG)"), class = "btn-secondary btn-sm")
      )
    })

    # --- Core Plot Logic ---
    generated_plot <- reactive({
      res <- results_data_reactive()
      if (is.null(res) || nrow(res) == 0) return(NULL)

      tr <- get_tr()

      # Filter for significant + borderline, or fallback to top observed
      top_exposures <- res %>%
        filter(
          Classification %in% c("Alert", "Borderline", "Alerte", "Limite") |
            (!is.na(`P-Value`) & `P-Value` <= 0.10)
        ) %>%
        arrange(`P-Value`) %>%
        head(20)

      if (nrow(top_exposures) == 0) {
        top_exposures <- res %>%
          arrange(desc(`Observed %`)) %>%
          head(10)
      }

      # Normalize both to 0-100 scale
      plot_data <- top_exposures
      plot_data$obs_pct <- plot_data$`Observed %` * 100
      plot_data$ref_pct <- as.numeric(plot_data$`Reference %`)
      plot_data$diff <- plot_data$obs_pct - plot_data$ref_pct

      # Map classification to colour
      plot_data$class_color <- class_colors[plot_data$Classification]
      plot_data$class_color[is.na(plot_data$class_color)] <- "#6c757d"

      # Determine if we have valid reference values for diverging chart
      has_refs <- any(!is.na(plot_data$ref_pct))

      if (has_refs) {
        # --- Diverging Lollipop Chart ---
        # Show observed vs reference as connected dots
        plot_data <- plot_data %>%
          filter(!is.na(ref_pct)) %>%
          mutate(Exposure = factor(Exposure, levels = Exposure[order(diff)]))

        p <- ggplot(plot_data, aes(y = Exposure)) +
          # Connecting segment
          geom_segment(
            aes(x = ref_pct, xend = obs_pct, yend = Exposure),
            color = "#bdc3c7",
            linewidth = 0.8,
            lineend = "round"
          ) +
          # Reference ◆ diamond
          geom_point(
            aes(x = ref_pct, shape = tr$t("Reference")),
            color = "#2c3e50",
            size = 3,
            fill = "#2c3e50"
          ) +
          # Observed ● circle, colored by classification
          geom_point(
            aes(x = obs_pct, color = Classification, shape = tr$t("Observed")),
            size = 4
          ) +
          scale_color_manual(
            values = class_colors,
            name = tr$t("Classification"),
            breaks = unique(plot_data$Classification)
          ) +
          scale_shape_manual(
            values = stats::setNames(c(16, 18), c(tr$t("Observed"), tr$t("Reference"))),
            name = ""
          ) +
          labs(
            x = tr$t("Percentage (%)"),
            y = NULL,
            title = tr$t("Observed vs Reference Exposure Rates"),
            subtitle = tr$t("Segments connect reference (diamond) to observed (circle) values")
          ) +
          theme(
            legend.position = "bottom",
            legend.box = "vertical",
            plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(color = "#6c757d", size = 11),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 11)
          )
      } else {
        # --- Fallback: Simple bar chart colored by classification ---
        plot_data <- plot_data %>%
          mutate(Exposure = factor(Exposure, levels = Exposure[order(obs_pct)]))

        p <- ggplot(plot_data, aes(x = Exposure, y = obs_pct, fill = Classification)) +
          geom_col(width = 0.7) +
          coord_flip() +
          scale_fill_manual(
            values = class_colors,
            name = tr$t("Classification"),
            breaks = unique(plot_data$Classification)
          ) +
          labs(
            x = NULL,
            y = tr$t("Percentage (%)"),
            title = tr$t("Top Exposures by Observed Rate")
          ) +
          theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 15),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 11)
          )
      }

      p
    })

    # --- Plot Rendering ---
    output$plot_container <- renderUI({
      p <- generated_plot()
      if (is.null(p)) {
        tr <- get_tr()
        return(
          div(
            class = "viz-empty-state",
            div(class = "empty-icon", icon("chart-bar")),
            h5(tr$t("No data to visualize")),
            p(tr$t("Select exposures and enter counts in the Analysis tab to see results here."))
          )
        )
      }

      # Determine dynamic height based on number of exposures
      n <- tryCatch({
        res <- results_data_reactive()
        if (is.null(res)) 10 else min(nrow(res), 20)
      }, error = function(e) 10)
      plot_height <- paste0(max(400, n * 35 + 120), "px")

      plotOutput(ns("plot"), height = plot_height)
    })

    output$plot <- renderPlot({
      req(generated_plot())
      generated_plot()
    })

    # --- Download Handlers ---
    output$download_plot_png <- downloadHandler(
      filename = function() { paste0("exposure_plot_", Sys.Date(), ".png") },
      content = function(file) {
        ggsave(file, plot = generated_plot(), device = "png", width = 12, height = 8, dpi = 300)
      }
    )

    output$download_plot_svg <- downloadHandler(
      filename = function() { paste0("exposure_plot_", Sys.Date(), ".svg") },
      content = function(file) {
        ggsave(file, plot = generated_plot(), device = "svg", width = 12, height = 8)
      }
    )
  })
}
