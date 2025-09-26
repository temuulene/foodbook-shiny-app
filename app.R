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

# Suppress SASS contrast warnings
options(sass.cache = FALSE)
Sys.setenv("SASS_SILENCE_DEPRECATION" = "1")
# Ensure Bootstrap themes are precompiled (prevents devmode from falling back
# to legacy behavior where colors may appear inconsistent)
options(bslib.precompiled = TRUE)

# --- 2. Data Preparation ---
foodbook_data <- read.csv("data/foodbook_data.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Province.Territory = case_when(
      Province.Territory == "BC" ~ "British Columbia",
      Province.Territory == "AB" ~ "Alberta",
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

make_safe_id <- function(Exposure, Province.Territory) {
  paste(
    gsub("[^a-zA-Z0-9]", "", Exposure),
    gsub("[^a-zA-Z0-9]", "", Province.Territory),
    sep = "_"
  )
}

# --- 4. Shiny Module Definition ---
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
      bg = "#f7f9fc",         # slightly brighter background
      fg = "#0f172a",          # near-black body text for strong contrast
      primary = "#0e4a7b",     # deep blue
      secondary = "#4b5563",   # slate gray
      success = "#176d4e",     # dark green
      info = "#0e6a88",        # teal-blue
      warning = "#a45100",     # dark amber (contrast-friendly)
      danger = "#b21f2d",      # rich red
      base_font = font_google("Inter", wght = "300;400;600;700"),
      heading_font = font_google("DM Sans", wght = "400;600;700")
    ) |>
      bs_add_variables(
        "body-color" = "#0f172a",
        "card-cap-bg" = "#f7faff",
        "card-border-color" = "#dde6f5",
        "border-radius" = "0.75rem",
        # Strengthen Bootstrap's contrast algorithm inputs
        "min-contrast-ratio" = 4.5,
        "color-contrast-dark" = "#000000",
        "color-contrast-light" = "#ffffff",
        # Ensure link colors align with the palette and pass contrast
        "link-color" = "#0e4a7b",
        "link-hover-color" = "#0a3a61"
      ) |>
      bs_add_rules("
        body {
          background: linear-gradient(135deg, #eef2f8 0%, #fdfdfd 60%);
        }
        .navbar {
          border-bottom: 1px solid #d7e3f7;
          background-color: rgba(255, 255, 255, 0.92);
          backdrop-filter: blur(6px);
        }
        .navbar-brand {
          color: #0f4c81 !important;
          font-weight: 700;
          letter-spacing: 0.02em;
        }
        .nav-link {
          color: #4b5563 !important;
          border-radius: 999px;
          padding: 0.6rem 1.1rem;
          margin: 0 0.3rem;
        }
        .nav-link:hover {
          color: #0f4c81 !important;
          background-color: rgba(15, 76, 129, 0.08);
        }
        .nav-link.active {
          color: #0f4c81 !important;
          background-color: rgba(15, 76, 129, 0.14) !important;
          box-shadow: inset 0 -3px 0 #0f4c81;
        }
        .bslib-sidebar-layout .sidebar {
          background: #ffffff;
          border-right: 1px solid #dde6f5;
          box-shadow: 4px 0 24px rgba(15, 76, 129, 0.08);
        }
        .bslib-sidebar-layout .sidebar .title {
          color: #0f4c81;
          font-weight: 600;
        }
        .sidebar hr {
          border-color: #d0ddf0;
        }
        .btn-primary {
          background: linear-gradient(135deg, #1160aa 0%, #0b4a86 100%);
          border: none;
          box-shadow: 0 12px 24px rgba(15, 76, 129, 0.18);
        }
        .btn-primary:hover {
          background: linear-gradient(135deg, #0b4a86 0%, #073866 100%);
        }
        .btn-warning {
          background: linear-gradient(135deg, #f7b733 0%, #f59e0b 100%);
          border: none;
          color: #1f2933;
          box-shadow: 0 12px 24px rgba(245, 158, 11, 0.22);
        }
        .btn-warning:hover {
          background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%);
        }
        .btn-secondary {
          background: linear-gradient(135deg, #4b5563 0%, #364152 100%);
          border: none;
          color: #f9fafb;
        }
        .card {
          border: 1px solid #dde6f5;
          box-shadow: 0 12px 30px rgba(15, 76, 129, 0.12);
        }
        .card-header {
          background: linear-gradient(135deg, #f7faff 0%, #ecf2ff 100%);
          border-bottom: 1px solid #d0ddf0;
          font-weight: 600;
          color: #0f4c81;
        }
        .nav-tabs .nav-link {
          color: #4b5563 !important;
        }
        .nav-tabs .nav-link.active {
          color: #0f4c81 !important;
          background-color: #e8f1ff !important;
          border-color: #e8f1ff #e8f1ff #ffffff;
        }
        .selectize-input {
          border: 2px solid #d0ddf0;
          border-radius: 0.65rem;
          min-height: 44px;
          font-weight: 500;
          transition: all 0.2s ease;
        }
        .selectize-input.focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.18);
        }
        .selectize-dropdown-content .option.active {
          background: #e8f1ff;
          color: #0f4c81;
        }
      "),
    header = tagList(
      useShinyjs(),
            tags$head(tags$style(HTML("
        .exposure-input-group {
          border: 1px solid #dde6f5;
          border-radius: 0.9rem;
          background: #ffffff;
          padding: 1.2rem;
          margin-bottom: 1.25rem;
          box-shadow: 0 16px 40px rgba(15, 76, 129, 0.08);
        }
        .exposure-input-group:first-child {
          margin-top: 0.75rem;
        }
        .exposure-input-group .row {
          row-gap: 0.75rem;
        }
        .exposure-header {
          color: #0f4c81;
          font-weight: 600;
          letter-spacing: 0.01em;
          margin-bottom: 1rem;
        }
        .exposure-input-group .form-control {
          border-radius: 0.65rem;
          border: 1px solid #d0ddf0;
          padding: 0.6rem 0.75rem;
          font-weight: 500;
        }
        .exposure-input-group .form-control:focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.15);
        }
        .ref-value {
          color: #1b7b57;
          font-weight: 600;
          display: inline-block;
          margin-top: 0.25rem;
        }
        .sidebar .form-group > label {
          font-weight: 600;
          color: #334155;
        }
        .sidebar .selectize-input,
        .sidebar select.form-control {
          border-radius: 0.65rem;
          border: 1px solid #d0ddf0;
          min-height: 44px;
        }
        .sidebar .selectize-input.focus,
        .sidebar select.form-control:focus {
          border-color: #0f4c81;
          box-shadow: 0 0 0 4px rgba(15, 76, 129, 0.18);
          outline: none;
        }
        .sidebar .action-button {
          font-weight: 600;
        }
        .dataTables_wrapper .dt-buttons .btn {
          background: #0f4c81;
          color: #ffffff;
          border: none;
          border-radius: 0.5rem;
          box-shadow: 0 8px 18px rgba(15, 76, 129, 0.18);
        }
        .dataTables_wrapper .dt-buttons .btn:hover {
          background: #0b3a67;
        }
        .dataTables_wrapper .dataTables_filter input {
          border-radius: 0.5rem;
          border: 1px solid #d0ddf0;
          padding: 0.45rem 0.75rem;
        }
        .well-panel-about h4 {
          color: #0f4c81;
          font-weight: 600;
        }
        .well-panel-about li {
          margin-bottom: 0.55rem;
          line-height: 1.5;
        }
        .contact-list {
          margin-top: 0.75rem;
        }
        .contact-line {
          display: flex;
          align-items: center;
          gap: 0.5rem;
          font-weight: 600;
          color: #1f2933;
          margin-bottom: 0.5rem;
        }
        .contact-line i {
          color: #0f4c81;
        }
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
                  nav_panel("Results", class = "results-panel", DTOutput("results_table", width = "100%")),
                  nav_panel("Visualization", class = "visual-panel", uiOutput("plot_container"))
                )
              )
    ),
    
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
                  p("Contact your regional foodborne illness team:"),
                  tags$div(
                    class = "contact-list",
                    tags$p(class = "contact-line", icon("phone"), span("555-123-4567")),
                    tags$p(class = "contact-line", icon("envelope"), span("food.safety@yourorg.ca"))
                  )
                )
              )
    )
  )
}

# --- 6. Server Logic ---
server <- function(input, output, session) {
  thematic_on(
    bg = "#f3f6fb",
    fg = "#1f2933",
    accent = "#0f4c81",
    font = font_spec(font_google("Inter"), scale = 1.1)
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
    datatable(res, extensions = "Buttons", rownames = FALSE,
              class = "stripe hover row-border order-column",
              options = list(pageLength = 15, dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE, autoWidth = FALSE)) %>%
      formatStyle(
        columns = "Classification",
        backgroundColor = styleEqual(
          c("Alert", "Borderline", "Not Significant", "Insufficient Data"),
          c("#fde4e6", "#fff4d6", "#edf2ff", "#f1f5f9")
        ),
        color = styleEqual(
          c("Alert", "Borderline", "Not Significant", "Insufficient Data"),
          c("#b82c3a", "#b35c00", "#1f2933", "#475569")
        ),
        fontWeight = "600"
      )
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
    
    # Palette tuned for accessibility and contrast
    alert_palette <- c(
      "Alert" = "#d62839",
      "Borderline" = "#f4a259",
      "Not Significant" = "#4b5563",
      "Insufficient Data" = "#94a3b8"
    )
    
    ggplot(plot_data, aes(y = reorder(`Province/Territory`, `Observed %`))) +
      geom_segment(aes(x = `Reference %`, xend = `Observed %`, yend = `Province/Territory`, color = Classification),
                   linewidth = 2, alpha = 0.8) +
      geom_point(aes(x = `Reference %`, size = `Total Valid`), color = "#0f4c81", shape = 1, stroke = 2) +
      geom_point(aes(x = `Observed %`, size = `Total Valid`, fill = Classification), color = "#1f2933", shape = 21, stroke = 1.4) +
      geom_text(aes(x = pmax(`Observed %`, `Reference %`), label = Reference_Comparison),
                hjust = -0.2, size = 4, fontface = "bold", color = "#1f2933") +
      scale_fill_manual(values = alert_palette, name = "Significance", na.value = "#94a3b8") +
      scale_color_manual(values = alert_palette, name = "Significance", na.value = "#94a3b8") +
      guides(size = guide_legend(override.aes = list(shape = 21, fill = "#0f4c81"))) +
      scale_size(range = c(4, 11), name = "Number of Cases") +
      scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
      labs(title = "Food Exposure Risk Assessment by Location",
           subtitle = "Comparison of case exposures vs. population reference values",
           x = "Exposure Percentage (%)", y = NULL,
           caption = "Outline circles = Reference exposure | Filled circles = Case exposure") +
      facet_wrap(~ str_wrap(Exposure, 40), ncol = 1, scales = "free_y") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold", color = "#0f4c81"),
        legend.text = element_text(color = "#4b5563"),
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        plot.subtitle = element_text(margin = margin(b = 15), size = 14, color = "#334155"),
        axis.title = element_text(face = "bold", size = 13, color = "#1f2933"),
        axis.text = element_text(size = 12, color = "#4b5563"),
        strip.text = element_text(size = 13, face = "bold", hjust = 0, color = "#0f4c81"),
        strip.background = element_rect(fill = "#f1f5ff", color = "#0f4c81", linewidth = 0.8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = "#e2e8f0"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.background = element_rect(fill = "#f9fbff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA)
      )
  })
  
  output$data_coverage_plot <- renderPlot({
    foodbook_data %>%
      count(Province.Territory, Foodbook_Version) %>%
      ggplot(aes(x = Province.Territory, y = n, fill = Foodbook_Version)) +
      geom_col(position = "dodge", color = "#f9fbff", linewidth = 0.6, alpha = 0.95) +
      scale_fill_manual(values = c("Foodbook1" = "#0f4c81", "Foodbook2" = "#1b7b57")) +
      labs(
        title = "Data Coverage by Region and Foodbook Version", 
        x = NULL, 
        y = "Number of Exposures",
        fill = "Version"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = "#4b5563"),
        plot.title = element_text(face = "bold", size = 16, color = "#0f4c81"),
        axis.title.y = element_text(face = "bold", size = 12, color = "#1f2933"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
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
