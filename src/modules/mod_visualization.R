# Module: Visualization
# Renders the ggplot visualization

mod_visualization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 1rem;",
      uiOutput(ns("download_btn_ui"))
    ),
    uiOutput(ns("plot_container"))
  )
}

mod_visualization_server <- function(id, results_data_reactive, get_tr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Download Button UI
    output$download_btn_ui <- renderUI({
      tr <- get_tr()
      downloadButton(ns("download_plot"), tr$t("Download Plot"), class = "btn-secondary")
    })
    
    # Plot Logic
    generated_plot <- reactive({
      res <- results_data_reactive()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      
      tr <- get_tr()
      lang <- tr$get_translation_language()
      
      # Filter for significant results (Alert/Borderline logic)
      # Note: Classification is already translated in the results data
      # We need robust check. 
      # Better approach: check P-Value < 0.10 if available in data, or use string match
      
      # Assuming 'res' has 'Classification' and real 'P-Value' (or derived)
      # Actually mod_results_table formatted it. 
      # WAIT: The apps passed the *raw* calculated data to render* functions usually.
      # To keep modules clean, let's assume raw data input?
      # The previous module (results_table) accepted the calculated dataframe. 
      # We should do the same here.
      
      top_exposures <- res %>%
        filter(
          Classification %in% c("Alert", "Borderline", "Alerte", "Limite") |
          (!is.na(`P-Value`) & `P-Value` <= 0.10)
        ) %>%
        arrange(`P-Value`) %>%
        head(20)
      
      if (nrow(top_exposures) == 0) {
        # Fallback to top observed if no significant findings
        top_exposures <- res %>%
          arrange(desc(`Observed %`)) %>%
          head(10)
      }
      
      # Create Plot (reusing existing ggplot logic)
      ggplot(top_exposures, aes(x = reorder(Exposure, `Observed %`), y = `Observed %` * 100)) +
        geom_bar(stat = "identity", fill = "#0e4a7b") +
        geom_point(aes(y = `Reference %`, color = "Reference"), size = 3) + 
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          x = NULL,
          y = tr$t("Percentage (%)"),
          title = tr$t("Top Exposures and Reference Values"),
          color = tr$t("Legend")
        ) +
        scale_color_manual(values = c("Reference" = "#b21f2d"))
    })
    
    output$plot_container <- renderUI({
      p <- generated_plot()
      if (is.null(p)) {
         tr <- get_tr()
         return(div(class="alert alert-info", tr$t("No data to visualize")))
      }
      plotOutput(ns("plot"), height = "600px")
    })
    
    output$plot <- renderPlot({
      generated_plot()
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("exposure_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(file, plot = generated_plot(), device = "png", width = 10, height = 8)
      }
    )
  })
}
