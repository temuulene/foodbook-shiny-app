# Module: About
# Renders the About Tab content

mod_about_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("about_content"))
}

mod_about_server <- function(id, get_tr) {
  moduleServer(id, function(input, output, session) {
    
    output$about_content <- renderUI({
      tr <- get_tr()
      
      tagList(
        p(tr$t("The Food Exposure Analysis Tool facilitates the comparison of case exposure data against population reference values from the Foodbook Report.")),
        h5(tr$t("Data Sources")),
        tags$ul(
          tags$li(
            strong("Foodbook 1.0 (2014-2015):"), " ",
            tr$t("Population reference values for Canada and Provinces/Territories.")
          ),
          tags$li(
            strong("Foodbook 2.0 (2025):"), " ",
            tr$t("Updated data where available.")
          )
        ),
        h5(tr$t("Interpretation Guide")),
        p(tr$t("Results are classified based on statistical comparison (Binomial Exact Test):")),
        tags$ul(
          tags$li(
            span(class = "text-danger", icon("exclamation-circle"), strong(tr$t("Alert"))), ": ",
            tr$t("p-value ≤ 0.05. Observed proportion is significantly higher than reference.")
          ),
          tags$li(
            span(class = "text-warning", icon("exclamation-triangle"), strong(tr$t("Borderline"))), ": ",
            tr$t("p-value ≤ 0.10. Observed proportion is marginally higher than reference.")
          )
        ),
        hr(),
        p(
          style = "font-size: 0.9rem; color: #666;",
          tr$t("Developed by Public Health Agency of Canada.")
        )
      )
    })
  })
}
