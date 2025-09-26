library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyjs)
library(tidyr)
library(ggplot2)

# Load Foodbook 2.0 data
foodbook_data <- read.csv(here::here("data", "foodbook_data.csv"))

# UI Component
ui <- dashboardPage(
  dashboardHeader(title = "Foodborne Illness Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            width = 12,
            title = "Input Parameters",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput(
              "location",
              "Select Location:",
              choices = c("Canada", unique(foodbook_data$Province.Territory)),
              multiple = TRUE,
              selected = "Canada"
            ),
            selectInput(
              "food_category",
              "Filter by Food Category:",
              choices = c("All", unique(foodbook_data$Category)),
              selected = "All"
            ),
            numericInput("n_cases", "Number of Cases:", value = 0, min = 0),
            actionButton("reset", "Reset Inputs", class = "btn-warning")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Food Exposure Data",
            status = "primary",
            solidHeader = TRUE,
            selectizeInput(
              "food_search",
              "Search and Select Food Items:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Type to search foods...",
                plugins = list("remove_button"),
                maxItems = 10
              )
            ),
            div(
              style = "max-height: 600px; overflow-y: auto;",
              uiOutput("selected_food_inputs")
            )
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "Results Table",
              div(style = "overflow-x: scroll;", DTOutput("results_table"))
            ),
            tabPanel(
              "Visualization",
              div(style = "height:600px;", plotOutput("exposure_plot", height = "100%"))
            )
          )
        )
      ),
      tabItem(
        tabName = "about",
        box(width = 12, includeMarkdown("about.md"))
      )
    )
  )
)

# Server Component
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, reset_trigger = 0)
  
  selected_data <- reactive({
    req(input$location)
    if ("Canada" %in% input$location) {
      foodbook_data
    } else {
      foodbook_data %>%
        filter(Province.Territory %in% input$location) %>%
        group_by(Exposure) %>%
        summarize(Proportion = mean(Proportion), .groups = "drop")
    }
  })
  
  filtered_foods <- reactive({
    data <- selected_data()
    if (input$food_category != "All") {
      data <- data %>% filter(Category == input$food_category)
    }
    data
  })
  
  observe({
    updateSelectizeInput(session, "food_search", choices = unique(filtered_foods()$Exposure))
  })
  
  output$selected_food_inputs <- renderUI({
    req(input$food_search)
    lapply(input$food_search, function(item) {
      div(
        class = "food-item",
        style = "margin-bottom: 10px; padding: 5px; border-bottom: 1px solid #eee;",
        fluidRow(
          column(4, strong(item)),
          column(8,
                 fluidRow(
                   column(4, numericInput(paste0("yes_", item), "Yes", value = 0, min = 0)),
                   column(4, numericInput(paste0("prob_", item), "Probably", value = 0, min = 0)),
                   column(4, numericInput(paste0("no_", item), "No", value = 0, min = 0))
                 )
          )
        )
      )
    })
  })
  
  results <- reactive({
    req(input$food_search)
    validate(need(length(input$food_search) > 0, "Please select at least one food item"))
    data <- filtered_foods()
    
    results_df <- data %>%
      filter(Exposure %in% input$food_search) %>%
      rowwise() %>%
      mutate(
        Yes = input[[paste0("yes_", Exposure)]] %||% 0,
        Probably = input[[paste0("prob_", Exposure)]] %||% 0,
        No = input[[paste0("no_", Exposure)]] %||% 0,
        Y.plus.P = Yes + Probably,
        Total = Y.plus.P + No,
        Percentage = if(Total > 0) (Y.plus.P / Total) * 100 else 0,
        Canada_Prop = foodbook_data$Proportion[foodbook_data$Exposure == Exposure & 
                                                 foodbook_data$Province.Territory == "Canada"],
        # Updated P.Value calculation to match Excel BINOMDIST
        P.Value = case_when(
          Total == 0 ~ NA_real_,
          Y.plus.P <= Total & Y.plus.P >= 0 ~ {
            # Using dbinom to match Excel's BINOMDIST with cumulative=0
            dbinom(Y.plus.P, Total, Canada_Prop/100)
          },
          TRUE ~ NA_real_
        ),
        Exposure.of.Interest = case_when(
          is.na(P.Value) ~ "Insufficient Data",
          Total < 5 ~ "Sample Size Too Small",
          # Updated logic to match Excel formula
          P.Value <= 0.05 & (Y.plus.P / Total) > (Canada_Prop / 100) ~ "Alert",
          !is.na(Canada_Prop) & P.Value <= 0.05 & (Y.plus.P / Total) < (Canada_Prop / 100) ~ "Protective",
          is.na(Canada_Prop) & (Y.plus.P / Total) >= 0.60 ~ "Alert",
          P.Value > 0.05 & P.Value <= 0.10 ~ "Borderline",
          TRUE ~ "Not Significant"
        ),
        CI_Lower = if(Total > 0) binom.test(Y.plus.P, Total)$conf.int[1] * 100 else NA_real_,
        CI_Upper = if(Total > 0) binom.test(Y.plus.P, Total)$conf.int[2] * 100 else NA_real_
      ) %>%
      ungroup()
    
    return(results_df)
  })
  
  output$results_table <- renderDT({
    req(results())
    results_df <- results()
    
    # Create base display dataframe
    display_df <- results_df %>%
      select(Exposure, Yes, Probably, No, Percentage, Canada_Prop, P.Value, Exposure.of.Interest)
    
    # Add location-specific columns
    if ("Canada" %in% input$location) {
      display_df <- display_df %>%
        mutate(
          `Canadian Proportion` = round(Canada_Prop, 2),
          `P-Value` = round(P.Value, 4),
          `Exposure of Interest` = Exposure.of.Interest
        ) %>%
        select(-Canada_Prop, -P.Value, -Exposure.of.Interest)  # Remove original columns
    } else if (length(input$location) > 0) {
      for (province in input$location) {
        display_df <- display_df %>%
          mutate(
            !!sym(paste0(province, " Proportion")) := round(get(paste0(province, "_Prop")), 2),
            !!sym(paste0(province, " P-Value")) := round(get(paste0(province, "_P.Value")), 4),
            !!sym(paste0(province, " Exposure")) := get(paste0(province, "_Exposure.of.Interest"))
          )
      }
    }
    
    datatable(
      display_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        names(display_df)[grepl("Exposure", names(display_df))],
        backgroundColor = styleEqual(
          c("Alert", "Protective", "Borderline", "Not Significant", 
            "Insufficient Data", "Sample Size Too Small"),
          c('pink', 'lightgreen', 'yellow', 'white', 'gray', 'lightgray')
        )
      )
  })
  
  output$exposure_plot <- renderPlot({
    req(results())
    validate(need(nrow(results()) > 0, "No data available for plotting"))
    
    ggplot(results(), aes(x = reorder(Exposure, Percentage))) +
      geom_bar(aes(y = Percentage), stat = "identity", fill = "skyblue", alpha = 0.6) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0.2
      ) +
      geom_point(aes(y = Canada_Prop), color = "red", size = 3) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Food Exposure Comparison",
        x = "Food Item",
        y = "Percentage (%)"
      ) +
      theme(
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  observeEvent(input$reset, {
    rv$reset_trigger <- rv$reset_trigger + 1
    updateNumericInput(session, "n_cases", value = 0)
    updateSelectizeInput(session, "food_search", selected = character(0))
  })
}

shinyApp(ui = ui, server = server)