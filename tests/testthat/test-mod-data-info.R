context("Module: Data Info")

library(shiny)
library(dplyr)
library(ggplot2)
library(DT) # For renderDT

# Load backend and module
source(file.path("..", "..", "src", "foodbook_backend.R"))
source(file.path("..", "..", "src", "modules", "mod_data_info.R"))

# Initialize backend with real data for coverage plots
# This assumes the test is run from tests/testthat/
# fb_init() uses fb_get_base_path to find data
if (!exists("fb_micro_1", envir = fb_env)) {
  fb_init()
}

# Mock Translator
mock_tr <- function(lang = "en") {
  list(
    t = function(x, ...) x,
    get_translation_language = function() lang
  )
}

test_that("mod_data_info_server generates correct summary text", {
  # Mock inputs
  selected_province <- reactive("Canada")
  selected_age <- reactive("All Ages")
  selected_month <- reactive("All Months")
  current_lang <- reactive("en")
  reference_table_data <- reactive(NULL) # Not needed for summary
  
  shiny::testServer(mod_data_info_server, args = list(
    get_tr = reactive(mock_tr()),
    current_lang = current_lang,
    selected_province = selected_province,
    selected_age = selected_age,
    selected_month = selected_month,
    reference_table_data = reference_table_data
  ), {
    # Check summary UI output
    # output$ref_summary_ui is a renderUI
    # We can inspect the output
    out <- output$ref_summary_ui
    expect_true(grepl("Location", out$html))
    expect_true(grepl("Canada", out$html))
  })
})

test_that("mod_data_info_server generates coverage plots with real data", {
  # Mock inputs for filtering
  selected_province <- reactive("Canada")
  selected_age <- reactive("All Ages") 
  selected_month <- reactive("All Months")
  current_lang <- reactive("en")
  reference_table_data <- reactive(NULL)
  
  shiny::testServer(mod_data_info_server, args = list(
    get_tr = reactive(mock_tr()),
    current_lang = current_lang,
    selected_province = selected_province,
    selected_age = selected_age,
    selected_month = selected_month,
    reference_table_data = reference_table_data
  ), {
    # Check plot outputs
    # ref_pt_plot and ref_month_plot should be ggplot objects
    # Note: testServer captures the return value of renderPlot
    
    # Trigger the plot generation
    pt_plot <- output$ref_pt_plot
    expect_true(!is.null(pt_plot))
    # In testServer, renderPlot logic is executed. 
    # But output$ref_pt_plot might be the plot object itself if not wrapped? 
    # Shiny output slots usually contain the result of the render function.
    # For renderPlot, it returns an object that Shiny uses to draw.
    # Let's inspect generated plots by calling the internal logic if accessible?
    # No, testServer exposes output$... which runs the render function.
    
    # We can't easily check class(pt_plot) because renderPlot wraps it.
    # But if it ran without error, that's a good sign.
    
    month_plot <- output$ref_month_plot
    expect_true(!is.null(month_plot))
  })
})
