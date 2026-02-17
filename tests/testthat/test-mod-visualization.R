context("Module: Visualization")
library(shiny)
library(dplyr)
library(ggplot2)

source(file.path("..", "..", "src", "modules", "mod_visualization.R"))




# Mock Translator
mock_tr <- function(lang = "en") {
  list(
    t = function(x, ...) x,
    get_translation_language = function() lang
  )
}

test_that("mod_visualization_server generates plot for significant results", {
  test_data <- data.frame(
    Exposure = c("Exp A", "Exp B", "Exp C"),
    `Observed %` = c(0.5, 0.4, 0.3),
    `Reference %` = c(40, 30, 20),
    `Classification` = c("Alert", "Borderline", "Not Significant"),
    `P-Value` = c(0.01, 0.08, 0.5),
    check.names = FALSE
  )
  
  shiny::testServer(mod_visualization_server, args = list(
    results_data_reactive = reactive(test_data),
    get_tr = reactive(mock_tr())
  ), {
    # Check that generated_plot() returns a ggplot object
    p <- generated_plot()
    expect_s3_class(p, "ggplot")
    
    # Check that data is filtered to significant results (A and B)
    # Extract data from plot
    plot_data <- p$data
    expect_equal(nrow(plot_data), 2)
    expect_setequal(plot_data$Exposure, c("Exp A", "Exp B"))
  })
})

test_that("mod_visualization_server falls back to top observed when no significant results", {
  test_data <- data.frame(
    Exposure = paste0("Exp ", 1:15),
    `Observed %` = seq(0.15, 0.01, by = -0.01),
    `Reference %` = 10,
    `Classification` = "Not Significant",
    `P-Value` = 0.5,
    check.names = FALSE
  )
  
  shiny::testServer(mod_visualization_server, args = list(
    results_data_reactive = reactive(test_data),
    get_tr = reactive(mock_tr())
  ), {
    p <- generated_plot()
    
    # Logic: Fallback to top 10 by Observed %
    plot_data <- p$data
    expect_equal(nrow(plot_data), 10)
    expect_equal(max(plot_data$`Observed %`), 0.15)
  })
})

test_that("mod_visualization_server handles empty data gracefully", {
  shiny::testServer(mod_visualization_server, args = list(
    results_data_reactive = reactive(data.frame()),
    get_tr = reactive(mock_tr())
  ), {
    expect_null(generated_plot())
  })
})
