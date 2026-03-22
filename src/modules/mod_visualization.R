# Module: Visualization
# Renders enhanced visualizations with diverging lollipop chart,
# classification coloring, and interactive features

mod_visualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main plot container (fills available space)
    uiOutput(ns("plot_container")),
    # Download controls below plot
    div(
      style = "margin-top: 0.5rem; display: flex; gap: 0.5rem; flex-wrap: wrap; justify-content: flex-end;",
      uiOutput(ns("download_btn_ui"))
    )
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

    # --- Download Buttons ---
    output$download_btn_ui <- renderUI({
      tr <- get_tr()
      tagList(
        downloadButton(ns("download_plot_png"), paste(tr$t("Download Plot"), "(PNG)"), class = "btn-outline-secondary btn-sm"),
        downloadButton(ns("download_plot_svg"), paste(tr$t("Download Plot"), "(SVG)"), class = "btn-outline-secondary btn-sm")
      )
    })

    # Track number of plotted exposures for dynamic height
    n_plotted <- reactiveVal(0)

    # --- Core Plot Logic ---
    generated_plot <- reactive({
      res <- results_data_reactive()
      if (is.null(res) || nrow(res) == 0) {
        n_plotted(0)
        return(NULL)
      }

      tr <- get_tr()

      # Filter for significant + borderline, or fallback to top observed
      top_exposures <- res |>
        filter(
          Classification %in% c("Alert", "Borderline", "Alerte", "Limite") |
            (!is.na(`P-Value`) & `P-Value` <= 0.10)
        ) |>
        arrange(`P-Value`) |>
        head(20)

      if (nrow(top_exposures) == 0) {
        top_exposures <- res |>
          arrange(desc(`Observed %`)) |>
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

      # Truncate long exposure names for y-axis readability
      plot_data$exposure_label <- dplyr::if_else(
        nchar(plot_data$Exposure) > 30,
        paste0(substr(plot_data$Exposure, 1, 28), "\u2026"),
        plot_data$Exposure
      )

      if (has_refs) {
        # --- Diverging Lollipop Chart ---
        plot_data <- plot_data |>
          filter(!is.na(ref_pct)) |>
          mutate(exposure_label = factor(exposure_label, levels = exposure_label[order(diff)]))

        n_plotted(nrow(plot_data))

        # Format value labels
        plot_data$obs_label <- paste0(round(plot_data$obs_pct, 1), "%")
        plot_data$ref_label <- paste0(round(plot_data$ref_pct, 1), "%")

        p <- ggplot(plot_data, aes(y = exposure_label)) +
          # Connecting segment
          geom_segment(
            aes(x = ref_pct, xend = obs_pct, yend = exposure_label),
            color = "#bdc3c7",
            linewidth = 1.5,
            lineend = "round"
          ) +
          # Reference diamond
          geom_point(
            aes(x = ref_pct, shape = tr$t("Reference")),
            color = "#2c3e50",
            size = 5,
            fill = "#2c3e50"
          ) +
          # Observed circle, colored by classification
          geom_point(
            aes(x = obs_pct, color = Classification, shape = tr$t("Observed")),
            size = 6
          ) +
          # Value labels on observed points
          geom_text(
            aes(x = obs_pct, label = obs_label),
            size = 3.5, fontface = "bold",
            vjust = -1.2,
            color = "#2c3e50"
          ) +
          scale_color_manual(
            values = class_colors,
            name = tr$t("Classification"),
            breaks = unique(plot_data$Classification)
          ) +
          scale_shape_manual(
            values = rlang::set_names(c(16, 18), c(tr$t("Observed"), tr$t("Reference"))),
            name = ""
          ) +
          scale_x_continuous(
            expand = expansion(mult = c(0.02, 0.08)),
            labels = function(x) paste0(x, "%")
          ) +
          labs(
            x = NULL,
            y = NULL,
            title = tr$t("Observed vs Reference Exposure Rates"),
            subtitle = tr$t("Segments connect reference (diamond) to observed (circle) values")
          ) +
          theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.key.size = unit(1.2, "lines"),
            legend.spacing.x = unit(0.5, "cm"),
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(color = "#6c757d", size = 12),
            plot.margin = margin(10, 15, 10, 5),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#ecf0f1", linewidth = 0.5),
            axis.text.y = element_text(size = 13, color = "#2c3e50"),
            axis.text.x = element_text(size = 11)
          )
      } else {
        # --- Fallback: Simple bar chart colored by classification ---
        plot_data <- plot_data |>
          mutate(exposure_label = factor(exposure_label, levels = exposure_label[order(obs_pct)]))

        n_plotted(nrow(plot_data))

        p <- ggplot(plot_data, aes(x = exposure_label, y = obs_pct, fill = Classification)) +
          geom_col(width = 0.7) +
          geom_text(
            aes(label = paste0(round(obs_pct, 1), "%")),
            hjust = -0.15, size = 4, fontface = "bold", color = "#2c3e50"
          ) +
          coord_flip() +
          scale_fill_manual(
            values = class_colors,
            name = tr$t("Classification"),
            breaks = unique(plot_data$Classification)
          ) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.15)),
            labels = function(x) paste0(x, "%")
          ) +
          labs(
            x = NULL,
            y = NULL,
            title = tr$t("Top Exposures by Observed Rate")
          ) +
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(face = "bold", size = 16),
            plot.margin = margin(10, 15, 10, 5),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#ecf0f1", linewidth = 0.5),
            axis.text.y = element_text(size = 13, color = "#2c3e50"),
            axis.text.x = element_text(size = 11)
          )
      }

      p
    })

    # --- Plot Rendering ---
    output$plot_container <- renderUI({
      p <- generated_plot()
      tr <- get_tr()
      if (is.null(p)) {
        return(
          div(
            class = "viz-empty-state",
            div(class = "empty-icon", icon("chart-bar")),
            h5(tr$t("No data to visualize")),
            p(tr$t("Select exposures and enter counts in the Analysis tab to see results here."))
          )
        )
      }

      # Dynamic height based on actual plotted exposures (not total results)
      n <- max(n_plotted(), 1)
      plot_height <- paste0(max(350, n * 60 + 100), "px")

      div(
        style = "width: 100%;",
        role = "img",
        `aria-label` = tr$t("Exposure analysis chart"),
        plotOutput(ns("plot"), height = plot_height, width = "100%")
      )
    })

    output$plot <- renderPlot({
      p <- generated_plot()
      req(p)
      p
    }, res = 96)

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
