# --- 1. Load Libraries ---
library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(data.table)
library(DT)
library(ggplot2)
library(shinyjs)

# --- 2. Data Preparation (Unchanged) ---
foodbook_data <- fread("data/foodbook_data.csv") %>%
  mutate(
    Province.Territory = case_when(
      Province.Territory == "AB" ~ "Alberta",
      Province.Territory == "BC" ~ "British Columbia",
      Province.Territory == "MB" ~ "Manitoba",
      Province.Territory == "NB" ~ "New Brunswick",
      Province.Territory == "NL" ~ "Newfoundland and Labrador",
      Province.Territory == "NS" ~ "Nova Scotia",
      Province.Territory == "NT" ~ "Northwest Territories",
      Province.Territory == "NU" ~ "Nunavut",
      Province.Territory == "ON" ~ "Ontario",
      Province.Territory == "PE" ~ "Prince Edward Island",
      Province.Territory == "QC" ~ "Quebec",
      Province.Territory == "SK" ~ "Saskatchewan",
      Province.Territory == "YT" ~ "Yukon",
      TRUE ~ Province.Territory
    ),
    Foodbook_Version = ifelse(grepl("\\*$", Exposure), "Foodbook1", "Foodbook2"),
    Proportion = round(Proportion, 2)
  ) %>%
  setDT()

foodbook_data <- foodbook_data %>%
  group_by(Province.Territory, Exposure) %>%
  filter(n() == 1 | Foodbook_Version == "Foodbook2") %>%
  ungroup()

# --- 3. Helper Functions ---
classify_exposure <- function(p_value, observed_prop, ref_prop) {
  ref_prop <- ifelse(is.na(ref_prop), 60, ref_prop)
  ref_prop_decimal <- ref_prop / 100
  if (is.na(p_value)) return("Insufficient Data")
  if (observed_prop > ref_prop_decimal) {
    case_when(p_value <= 0.05 ~ "Alert",
              p_value <= 0.10 ~ "Borderline",
              TRUE ~ "Not Significant")
  } else {
    "Not Significant"
  }
}

# MODIFICATION: The function arguments now exactly match the column names
# in the data frame passed to pmap_chr, resolving the error.
make_safe_id <- function(Exposure, Province.Territory) {
  paste(
    gsub("[^a-zA-Z0-9]", "", Exposure),
    gsub("[^a-zA-Z0-9]", "", Province.Territory),
    sep = "_"
  )
}

# --- 4. Shiny Module Definition (Unchanged) ---
exposure_module_ui <- function(id, exposure_name, ref_value) {
  ns <- NS(id)
  div(
    class = "exposure-input-group",
    h4(exposure_name, class = "exposure-header"),
    fluidRow(
      column(2, numericInput(ns("yes"), "Yes", 0, min = 0)),
      column(2, numericInput(ns("prob"), "Probably", 0, min = 0)),
      column(2, numericInput(ns("no"), "No", 0, min = 0)),
      column(2, numericInput(ns("dk"), "DK", 0, min = 0)),
      column(4,
             p("Reference Value:", class = "ref-value"),
             span(style = "font-size: 1.2em;", paste0(ref_value, "%")))
    )
  )
}

exposure_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      tibble(
        yes = input$yes %||% 0,
        prob = input$prob %||% 0,
        no = input$no %||% 0,
        dk = input$dk %||% 0
      )
    })
  })
}

# --- 5. User Interface (UI) ---
ui <- function(request) {
  page_navbar(
    title = "Food Exposure Analysis Tool",
    theme = bs_theme(
      version = 5,
      bg = "#f5f5f5", fg = "#212529", primary = "#005ea2",
      "card-bg" = "#ffffff", "card-border-color" = "#dee2e6",
      base_font = font_google("Inter", wght = "300;400;700")
    ),
    header = tagList(
      useShinyjs(),
      tags$head(tags$style(HTML("
        .exposure-input-group { border-top: 1px solid #dee2e6; padding-top: 15px; margin-top: 15px; }
        .well-panel-about li { margin-bottom: 8px; line-height: 1.4; }
        .well-panel-about h4 { color: #005ea2; margin-top: 20px; margin-bottom: 10px; }
      ")))
    ),
    nav_panel("Analysis",
              icon = icon("calculator"),
              layout_sidebar(
                sidebar = sidebar(
                  title = "Analysis Parameters",
                  selectInput("province", "Select Location(s):",
                              choices = c("Canada", unique(foodbook_data$Province.Territory[foodbook_data$Province.Territory != "Canada"])),
                              selected = "Canada",
                              multiple = TRUE),
                  selectInput("food_category", "Filter Category:",
                              choices = c("All", unique(foodbook_data$Foodbook_Version)),
                              selected = "All"),
                  hr(),
                  actionButton("reset", "Reset Inputs", class = "btn-warning", width = "100%"),
                  bookmarkButton(label = "Bookmark Analysis", class = "btn-secondary", width = "100%")
                ),
                card(
                  card_header("Exposure Data Input"),
                  card_body(
                    helpText("Enter case counts for each exposure in each selected location."),
                    selectizeInput("exposure_select", "Select Exposures:",
                                   choices = NULL, multiple = TRUE,
                                   options = list(placeholder = "Start typing...",
                                                  plugins = list("remove_button"), create = TRUE)),
                    div(style = "max-height: 60vh; overflow-y: auto;",
                        uiOutput("exposure_modules_ui"))
                  )
                ),
                navset_card_tab(
                  full_screen = TRUE,
                  nav_panel("Results", DTOutput("results_table")),
                  nav_panel("Visualization", uiOutput("plot_container"))
                )
              )
    ),
    
    # MODIFICATION: Restored the correct UI content for the Data Info tab.
    nav_panel("Data Info",
              icon = icon("database"),
              card(
                card_header("Foodbook Data Composition"),
                card_body(
                  plotOutput("data_coverage_plot"),
                  hr(),
                  DTOutput("data_summary_table")
                )
              )
    ),
    
    # MODIFICATION: Restored the correct UI content for the About tab.
    nav_panel("About",
              icon = icon("info-circle"),
              card(
                class = "well-panel-about",
                card_header(h3("About This Tool")),
                card_body(
                  h4("What This Tool Does"),
                  p("This tool helps identify foods that might be linked to foodborne illness outbreaks by comparing:"),
                  tags$ul(
                    tags$li("What people report eating in your investigation"),
                    tags$li("What Canadians typically eat (from Foodbook data)")
                  ),
                  h4("Key Metrics"),
                  tags$ul(
                    tags$li(strong("Exposed:"), "People who ate the food (Yes + Probably)"),
                    tags$li(strong("Observed %:"), "Exposed people in your data"),
                    tags$li(strong("Reference %:"), "Typical exposure in population"),
                    tags$li(strong("P-Value:"), "Chance of seeing these results if normal")
                  ),
                  h4("Using Results"),
                  p("Start with Alert items first and compare with lab results and other evidence. Small samples (<5 responses) may be unreliable."),
                  h4("Need Help?"),
                  p("Contact your regional foodborne illness team:", br(), "📞 555-123-4567", br(), "✉️ food.safety@yourorg.ca")
                )
              )
    )
  )
}

# --- 6. Server Logic ---
server <- function(input, output, session) {
  thematic_on(
    bg = "white", fg = "#212529", accent = "#005ea2",
    font = font_spec(font_google("Inter"), scale = 1.2)
  )
  
  exposure_module_reactives <- reactiveVal(list())
  
  combinations_reactive <- reactive({
    req(input$exposure_select, input$province)
    tidyr::crossing(
      Exposure = input$exposure_select,
      Province.Territory = input$province
    ) %>%
      mutate(module_id = make_safe_id(Exposure, Province.Territory))
  })
  
  observe({
    updateSelectizeInput(session, "exposure_select",
                         choices = unique(foodbook_data$Exposure), server = TRUE)
  })
  
  output$exposure_modules_ui <- renderUI({
    combinations <- combinations_reactive()
    req(nrow(combinations) > 0)
    
    module_outputs <- purrr::pmap(combinations, function(Exposure, Province.Territory, module_id) {
      ref_value <- foodbook_data %>%
        filter(Exposure == !!Exposure, Province.Territory == !!Province.Territory) %>%
        pull(Proportion) %>%
        first() %>%
        replace_na(60)
      
      list(
        ui = exposure_module_ui(
          id = module_id,
          exposure_name = paste0(Exposure, " (", Province.Territory, ")"),
          ref_value = ref_value
        ),
        server = exposure_module_server(module_id)
      )
    })
    
    servers <- purrr::map(module_outputs, "server")
    names(servers) <- combinations$module_id
    exposure_module_reactives(servers)
    
    purrr::map(module_outputs, "ui")
  })
  
  results <- reactive({
    req(length(exposure_module_reactives()) > 0)
    combinations <- combinations_reactive()
    input_values <- purrr::map_dfr(exposure_module_reactives(), ~ .x(), .id = "module_id")
    req(nrow(input_values) > 0)
    
    input_values %>%
      left_join(combinations, by = "module_id") %>%
      left_join(
        foodbook_data %>% select(Exposure, Province.Territory, Proportion),
        by = c("Exposure", "Province.Territory")
      ) %>%
      rename(province_ref = Proportion) %>%
      mutate(
        province_ref = coalesce(province_ref, 60),
        y_plus_p = yes + prob,
        total = y_plus_p + no,
        observed_prop = if_else(total > 0, y_plus_p / total, NA_real_)
      ) %>%
      rowwise() %>%
      mutate(
        p_value_province = if (total > 0) pbinom(y_plus_p - 1, total, province_ref / 100, lower.tail = FALSE) else NA_real_,
        class_province = classify_exposure(p_value_province, observed_prop, province_ref)
      ) %>%
      ungroup() %>%
      select(
        `Province/Territory` = Province.Territory,
        Exposure,
        `Total Valid` = total,
        `Observed %` = observed_prop,
        `Reference %` = province_ref,
        `P-Value` = p_value_province,
        Classification = class_province
      )
  })
  
  output$results_table <- renderDT({
    req(nrow(results()) > 0)
    res <- results() %>%
      mutate(`Observed %` = round(`Observed %` * 100, 1), `P-Value` = round(`P-Value`, 4))
    datatable(res, extensions = 'Buttons', rownames = FALSE,
              options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
                             scrollX = TRUE, autoWidth = FALSE)) %>%
      formatStyle(columns = 'Classification',
                  backgroundColor = styleEqual(c("Alert", "Borderline", "Not Significant", "Insufficient Data"),
                                               c("#F8D7DA", "#FFF3CD", "#FFFFFF", "#E9ECEF")))
  })
  
  output$plot_container <- renderUI({
    req(nrow(results()) > 0)
    plot_height <- max(500, length(input$exposure_select) * length(input$province) * 60)
    plotOutput("exposure_plot", height = paste0(plot_height, "px"))
  })
  
  output$exposure_plot <- renderPlot({
    req(nrow(results()) > 0)
    plot_data <- results() %>%
      mutate(`Observed %` = `Observed %` * 100,
             Reference_Comparison = ifelse(`Observed %` > `Reference %`,
                                           paste0("+", round(`Observed %` - `Reference %`, 1), "%"),
                                           paste0(round(`Observed %` - `Reference %`, 1), "%")))
    alert_palette <- c("Alert" = "#e74c3c", "Borderline" = "#f1c40f", "Not Significant" = "#6c757d", "Insufficient Data" = "#adb5bd")
    
    ggplot(plot_data, aes(y = reorder(`Province/Territory`, `Observed %`))) +
      geom_segment(aes(x = `Reference %`, xend = `Observed %`, yend = `Province/Territory`, color = Classification),
                   linewidth = 1.5, alpha = 0.7) +
      geom_point(aes(x = `Reference %`, size = `Total Valid`), color = "#212529", shape = 1, stroke = 1.5) +
      geom_point(aes(x = `Observed %`, size = `Total Valid`, fill = Classification), color = "#212529", shape = 21, stroke = 1) +
      geom_text(aes(x = pmax(`Observed %`, `Reference %`), label = Reference_Comparison),
                hjust = -0.2, size = 3.5, fontface = "bold") +
      scale_fill_manual(values = alert_palette, name = "Significance") +
      scale_color_manual(values = alert_palette, name = "Significance") +
      scale_size(range = c(4, 9), name = "Number of Cases") +
      scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
      labs(title = "Food Exposure Risk Assessment by Location",
           subtitle = "Comparison of case exposures vs. population reference values",
           x = "Exposure Percentage (%)", y = NULL,
           caption = "Outline circles = Reference exposure | Filled circles = Case exposure") +
      facet_wrap(~ str_wrap(Exposure, 40), ncol = 1, scales = "free_y") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(margin = margin(b = 15)),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 11),
            strip.text = element_text(size = 12, face = "bold", hjust = 0),
            strip.background = element_rect(fill = "#f5f5f5", color = NA),
            panel.grid.minor.x = element_blank(),
            panel.spacing.y = unit(1.5, "lines"))
  })
  
  # --- MODIFICATION: Restored server logic for the Data Info tab ---
  output$data_coverage_plot <- renderPlot({
    foodbook_data %>%
      count(Province.Territory, Foodbook_Version) %>%
      ggplot(aes(x = Province.Territory, y = n, fill = Foodbook_Version)) +
      geom_col(position = "dodge") +
      labs(title = "Data Coverage by Region and Foodbook Version", x = NULL, y = "Number of Exposures") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$data_summary_table <- renderDT({
    foodbook_data %>%
      group_by(Exposure, Foodbook_Version) %>%
      summarise(
        Provinces = n_distinct(Province.Territory),
        Min_Proportion = min(Proportion, na.rm = TRUE),
        Max_Proportion = max(Proportion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      datatable(options = list(pageLength = 10)) %>%
      formatRound(columns = c("Min_Proportion", "Max_Proportion"), digits = 1)
  })
  
  # Reset handler
  observeEvent(input$reset, {
    updateSelectizeInput(session, "exposure_select", selected = character(0))
    updateSelectInput(session, "province", selected = "Canada")
  })
}

# --- 7. Run Application ---
shinyApp(ui, server, enableBookmarking = "url")