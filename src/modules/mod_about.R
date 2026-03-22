# Module: About
# Renders the About Tab content with comprehensive documentation

mod_about_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("about_content"))
}

mod_about_server <- function(id, get_tr) {
  moduleServer(id, function(input, output, session) {

    output$about_content <- renderUI({
      tr <- get_tr()

      tagList(
        # --- Purpose ---
        h4(tr$t("Purpose")),
        p(tr$t("The Food Exposure Analysis Tool facilitates the comparison of case exposure data against population reference values from the Foodbook Report.")),
        p(tr$t("Compare your case exposures to typical population exposures from Foodbook to prioritise hypotheses during outbreak investigations.")),

        hr(),

        # --- What is Foodbook? ---
        h4(tr$t("Data Sources")),
        p(tr$t("Foodbook is a population-based survey conducted in all Canadian provinces and territories. It provides essential data on food, animal and water exposure used to understand, respond to, control and prevent enteric illness in Canada.")),

        tags$ul(
          tags$li(
            strong(tr$t("Foodbook 2.0 (2023-2024):")), " ",
            tr$t("Online and telephone survey with ~21,000 respondents across Canada")
          ),
          tags$li(
            strong(tr$t("Foodbook 1.0 (2014-2015):")), " ",
            tr$t("Telephone survey with ~10,000 respondents (exposures marked with * are from this survey only)")
          )
        ),
        p(tr$t("Reference percentages are calculated using survey weights to ensure population representativeness.")),

        hr(),

        # --- How references are computed ---
        h4(tr$t("How references are computed")),
        tags$ul(
          tags$li(tr$t("References use Foodbook microdata with survey weights.")),
          tags$li(tr$t("If multiple PTs are selected, a single combined reference is computed across them.")),
          tags$li(tr$t("You can optionally limit the reference by Age Group and Month."))
        ),

        hr(),

        # --- Statistical Methodology ---
        h4(tr$t("Statistical Methodology")),
        p(tr$t("The tool uses a one-sided binomial test to compare observed case exposure rates against population reference values:")),
        tags$ul(
          tags$li(tr$t("Null hypothesis: Case exposure rate \u2264 Population reference rate")),
          tags$li(tr$t("Alternative hypothesis: Case exposure rate > Population reference rate"))
        ),

        hr(),

        # --- Interpretation Guide ---
        h4(tr$t("Interpretation Guide")),
        p(tr$t("Results are classified based on statistical comparison (Binomial Exact Test):")),
        tags$ul(
          tags$li(
            span(class = "text-danger", icon("exclamation-circle"), strong(tr$t("Alert"))), ": ",
            tr$t("p-value \u2264 0.05. Observed proportion is significantly higher than reference.")
          ),
          tags$li(
            span(class = "text-warning", icon("exclamation-triangle"), strong(tr$t("Borderline"))), ": ",
            tr$t("p-value \u2264 0.10. Observed proportion is marginally higher than reference.")
          ),
          tags$li(
            strong(tr$t("Not Significant")), ": ",
            tr$t("No significant difference from reference (p \u2265 0.10)")
          ),
          tags$li(
            strong(tr$t("Insufficient Data")), ": ",
            tr$t("Too few cases to calculate statistics (< 5 total responses)")
          ),
          tags$li(
            strong(tr$t("No Reference Value")), ": ",
            tr$t("Exposure not found in Foodbook database")
          )
        ),

        hr(),

        # --- Good Practices ---
        h4(tr$t("Good Practices")),
        tags$ul(
          tags$li(tr$t("Select reference population filters that match your case demographics (PT, age, season).")),
          tags$li(tr$t("Focus on Alert and Borderline classifications for hypothesis generation.")),
          tags$li(tr$t("Consider multiple testing correction when examining many exposures.")),
          tags$li(tr$t("Custom exposures require you to provide the expected reference percentage.")),
          tags$li(tr$t("Please be careful not to overanalyse the data. Limiting the data to a small subset of respondents (for example, respondents ages 0-9 from PEI in March) can result in small sample sizes and make the data less reliable. This is especially important for exposures that are rare within the population."))
        ),

        hr(),

        # --- Limitations ---
        h4(tr$t("Limitations")),
        tags$ul(
          tags$li(tr$t("Survey data may not reflect current food consumption patterns (data collected in 2014-2015 and 2023-2024)")),
          tags$li(tr$t("Self-reported exposure data is subject to recall bias")),
          tags$li(tr$t("Some exposures may have seasonal variations not captured when using annual data")),
          tags$li(tr$t("Small sample sizes in specific PT/age/month combinations may yield unstable estimates")),
          tags$li(tr$t("Exposures from Foodbook 1.0 (*) use different survey weights than Foodbook 2.0"))
        ),

        hr(),

        # --- FAQ ---
        h4(tr$t("Frequently Asked Questions")),

        tags$dl(
          tags$dt(tr$t("Why is my exposure showing 'No Reference Value'?")),
          tags$dd(tr$t("This means the exposure was not asked in either Foodbook survey, or the variable name doesn't match. Try searching for a similar exposure name.")),

          tags$dt(tr$t("What does the * mean next to some exposures?")),
          tags$dd(tr$t("Exposures marked with * are only available from Foodbook 1.0 (2014-2015). They are included for completeness but may not reflect current consumption patterns.")),

          tags$dt(tr$t("Why do reference values change when I select different PTs?")),
          tags$dd(tr$t("Food consumption varies by region. The reference is recalculated using only respondents from the selected province(s)/territory(ies).")),

          tags$dt(tr$t("How should I interpret 'Borderline' results?")),
          tags$dd(tr$t("Borderline results (p-value between 0.05 and 0.10) suggest a possible association that warrants further investigation but doesn't meet conventional significance thresholds."))
        ),

        hr(),

        # --- Useful Links ---
        h4(tr$t("Useful Links")),
        tags$ul(
          tags$li(
            tags$a(
              href = "https://open.canada.ca/data/en/dataset/foodbook-2-0-public-use-microdata-file",
              target = "_blank",
              tr$t("Foodbook 2.0 Data (Open Canada)")
            )
          ),
          tags$li(
            tags$a(
              href = "https://open.canada.ca/data/en/dataset/foodbook-open-data",
              target = "_blank",
              tr$t("Foodbook 1.0 Data (Open Canada)")
            )
          ),
          tags$li(
            tags$a(
              href = "https://www.canada.ca/en/public-health.html",
              target = "_blank",
              tr$t("Public Health Agency of Canada")
            )
          )
        ),

        hr(),

        # --- Contact ---
        p(
          class = "text-body-secondary", style = "font-size: 0.9rem;",
          tr$t("For questions or support, please contact:"), " ",
          tr$t("[Contact email placeholder]")
        ),
        p(
          class = "text-body-secondary", style = "font-size: 0.85rem;",
          tr$t("Developed by Public Health Agency of Canada.")
        )
      )
    })
  })
}
