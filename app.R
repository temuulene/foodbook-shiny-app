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
source("src/foodbook_backend.R")

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

# Initialise Foodbook backend (labels + microdata)
fb_init()

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
                  selectInput("province", "Reference PT(s):",
                              choices = c("Canada", fb_pt_names()),
                              selected = "Canada",
                              multiple = TRUE),
                  selectInput("age_group", "Restrict by Age Group (optional):",
                              choices = c("All", fb_age_groups()),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("month", "Restrict by Month (optional):",
                              choices = c("All", fb_months()),
                              selected = "All",
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

    nav_panel("Advanced",
              icon = icon("upload"),
              layout_sidebar(
                sidebar = sidebar(
                  title = "Upload CEDARS Exposure Data (.xlsx)",
                  fileInput("cedars_file", "Upload Excel", accept = c(".xlsx")),
                  helpText("Expected sheets: 'case exposure answer' and 'Salmonella Case'."),
                  hr(),
                  selectInput("adv_province", "Reference PT(s):",
                              choices = c("Canada", fb_pt_names()),
                              selected = "Canada", multiple = TRUE),
                  selectInput("adv_age_group", "Restrict by Age Group (optional):",
                              choices = c("All", fb_age_groups()), selected = "All", multiple = TRUE),
                  selectInput("adv_month", "Restrict by Month (optional):",
                              choices = c("All", fb_months()), selected = "All", multiple = TRUE)
                ),
                card(
                  card_header("Results"),
                  card_body(DTOutput("adv_results_table", width = "100%"))
                )
              )
    ),
    
    nav_panel("Data Info",
              icon = icon("database"),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Reference Settings"),
                  card_body(uiOutput("ref_summary_ui"))
                ),
                card(
                  card_header("Population Exposure Snapshot (Reference)"),
                  card_body(DTOutput("ref_top_exposures"))
                ),
                card(
                  card_header("Microdata Coverage by PT (after filters)"),
                  card_body(plotOutput("ref_pt_plot", height = "350px"))
                ),
                card(
                  card_header("Microdata Coverage by Month (after filters)"),
                  card_body(plotOutput("ref_month_plot", height = "350px"))
                )
              )
    ),
    
    nav_panel("About",
              icon = icon("info-circle"),
              card(
                class = "well-panel-about",
                card_header(h3("About This Tool")),
                card_body(
                  h4("Purpose"),
                  p("Compare your case exposures to typical population exposures from Foodbook to prioritise hypotheses during outbreak investigations."),
                  h4("How references are computed"),
                  tags$ul(
                    tags$li("References use Foodbook microdata with survey weights (as in OMD’s Stata workflow)."),
                    tags$li("If multiple PTs are selected, a single combined reference is computed across them."),
                    tags$li("You can optionally limit the reference by Age Group (0-9, 10-19, 20-64, 65+) and Month."),
                    tags$li("Defaults like \"Canada\" and \"All\" auto-deselect once you add other selections.")
                  ),
                  h4("Analysis outputs"),
                  tags$ul(
                    tags$li(strong("Observed %"), ": (Yes + Probably) / (Yes + Probably + No) in your cases."),
                    tags$li(strong("Reference %"), ": Weighted population exposure % from Foodbook for your selected filters (rounded to 1 decimal)."),
                    tags$li(strong("P-Value"), ": Binomial test of observed vs reference % (upper tail)."),
                    tags$li(strong("Classification"), ": Alert (≤0.05), Borderline (≤0.10), Not Significant, or Insufficient Data.")
                  ),
                  h4("Advanced (CEDARS upload)"),
                  tags$ul(
                    tags$li("Upload the CEDARS Excel export. Expected sheets: ‘case exposure answer’ and ‘Salmonella Case’."),
                    tags$li("Columns are auto-detected even if the wording changes (we normalise names)."),
                    tags$li("We analyse confirmed cases if available in the linelist sheet.")
                  ),
                  h4("Good practice"),
                  tags$ul(
                    tags$li("Interpret alongside lab, trace-back, and epi linkage evidence."),
                    tags$li("Small totals can be unstable; look for patterns across multiple signals."),
                    tags$li("Foodbook is de-identified open data; no direct identifiers are used.")
                  ),
                  h4("Roadmap"),
                  tags$ul(
                    tags$li("Optional Bayesian results to complement binomial tests (per OMD collaboration).")
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
    font = font_spec(font_google("Inter"), scale = 1.25)
  )

  
  exposure_module_reactives <- reactiveVal(list())
  
  combinations_reactive <- reactive({
    req(input$exposure_select)
    tibble::tibble(
      ExposureCode = input$exposure_select,
      module_id = paste0("exp_", gsub("[^a-zA-Z0-9]", "", input$exposure_select))
    )
  })
  
  observe({
    ch <- fb_exposure_choices()
    updateSelectizeInput(session, "exposure_select",
                         choices = as.list(ch), server = TRUE)
  })
  
  output$exposure_modules_ui <- renderUI({
    combinations <- combinations_reactive()
    req(nrow(combinations) > 0)
    
    module_outputs <- purrr::pmap(combinations, function(ExposureCode, module_id) {
      # Determine label for display
      label <- names(fb_exposure_choices())[match(ExposureCode, fb_exposure_choices())]
      # Resolve filters
      pts <- input$province
      ages <- if (is.null(input$age_group) || (length(input$age_group) == 1 && input$age_group[1] == "All")) NULL else input$age_group
      months <- if (is.null(input$month) || (length(input$month) == 1 && input$month[1] == "All")) NULL else as.integer(input$month)
      ref_value <- fb_reference_percents(ExposureCode, pt_names = pts, months = months, age_groups = ages)[[1]]
      ref_value <- ifelse(is.na(ref_value), 60, round(ref_value, 1))

      list(
        ui = exposure_module_ui(
          id = module_id,
          exposure_name = label %||% ExposureCode,
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

    # Build reference percents for selected exposures with filters
    pts <- input$province
    ages <- if (is.null(input$age_group) || (length(input$age_group) == 1 && input$age_group[1] == "All")) NULL else input$age_group
    months <- if (is.null(input$month) || (length(input$month) == 1 && input$month[1] == "All")) NULL else as.integer(input$month)
    ref_perc <- fb_reference_percents(combinations$ExposureCode, pt_names = pts, months = months, age_groups = ages)

    # Map codes to labels for display
    code_to_label <- names(fb_exposure_choices())
    names(code_to_label) <- as.vector(fb_exposure_choices())

    input_values %>%
      left_join(combinations, by = "module_id") %>%
      mutate(
        Exposure = code_to_label[ExposureCode] %||% ExposureCode,
        province_ref = as.numeric(ref_perc[match(ExposureCode, names(ref_perc))])
      ) %>%
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
      mutate(`Reference Scope` = paste(if (is.null(pts)) "Canada" else paste(pts, collapse = ", "))) %>%
      select(
        `Reference Scope`,
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
      mutate(`Observed %` = round(`Observed %` * 100, 1), `Reference %` = round(`Reference %`, 1), `P-Value` = round(`P-Value`, 4))
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
    plot_height <- max(500, length(input$exposure_select) * 180)
    plotOutput("exposure_plot", height = paste0(plot_height, "px"))
  })
  
  output$exposure_plot <- renderPlot({
    req(nrow(results()) > 0)
    plot_data <- results() %>%
      mutate(`Observed %` = `Observed %` * 100,
             Reference_Comparison = ifelse(`Observed %` > `Reference %`,
                                           paste0("+", round(`Observed %` - `Reference %`, 1), "%"),
                                           paste0(round(`Observed %` - `Reference %`, 1), "%")))
    ref_scope <- paste(unique(plot_data$`Reference Scope`), collapse = ", ")
    
    # Palette tuned for accessibility and contrast
    alert_palette <- c(
      "Alert" = "#d62839",
      "Borderline" = "#f4a259",
      "Not Significant" = "#4b5563",
      "Insufficient Data" = "#94a3b8"
    )
    
    ggplot(plot_data, aes(y = reorder(Exposure, `Observed %`))) +
      geom_segment(aes(x = `Reference %`, xend = `Observed %`, yend = Exposure, color = Classification),
                   linewidth = 2, alpha = 0.8) +
      geom_point(aes(x = `Reference %`, size = `Total Valid`), color = "#0f4c81", shape = 1, stroke = 2) +
      geom_point(aes(x = `Observed %`, size = `Total Valid`, fill = Classification), color = "#1f2933", shape = 21, stroke = 1.4) +
      geom_text(aes(x = pmax(`Observed %`, `Reference %`), label = Reference_Comparison),
                hjust = -0.2, size = 5.2, fontface = "bold", color = "#1f2933") +
      scale_fill_manual(values = alert_palette, name = "Significance", na.value = "#94a3b8") +
      scale_color_manual(values = alert_palette, name = "Significance", na.value = "#94a3b8") +
      guides(size = guide_legend(override.aes = list(shape = 21, fill = "#0f4c81"))) +
      scale_size(range = c(5, 12), name = "Number of Cases") +
      scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
      labs(title = "Food Exposure Risk Assessment",
           subtitle = paste("Comparison of case exposures vs. population reference values | Reference scope:", ref_scope),
           x = "Exposure Percentage (%)", y = NULL,
           caption = "Outline circles = Reference exposure | Filled circles = Case exposure") +
      # Single panel since reference is combined across PTs
      NULL +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 14, color = "#0f4c81"),
        legend.text = element_text(size = 13, color = "#4b5563"),
        plot.title = element_text(face = "bold", size = 22, color = "#0f4c81"),
        plot.subtitle = element_text(margin = margin(b = 15), size = 16, color = "#334155"),
        axis.title = element_text(face = "bold", size = 16, color = "#1f2933"),
        axis.text = element_text(size = 14, color = "#4b5563"),
        strip.text = element_text(size = 15, face = "bold", hjust = 0, color = "#0f4c81"),
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
    updateSelectInput(session, "age_group", selected = "All")
    updateSelectInput(session, "month", selected = "All")
  })

  # Auto-remove defaults for clarity
  observeEvent(input$province, ignoreInit = TRUE, {
    sel <- input$province
    if (length(sel) > 1 && "Canada" %in% sel) {
      updateSelectInput(session, "province", selected = setdiff(sel, "Canada"))
    }
  })
  observeEvent(input$age_group, ignoreInit = TRUE, {
    sel <- input$age_group
    if (length(sel) > 1 && "All" %in% sel) {
      updateSelectInput(session, "age_group", selected = setdiff(sel, "All"))
    }
  })
  observeEvent(input$month, ignoreInit = TRUE, {
    sel <- input$month
    if (length(sel) > 1 && "All" %in% sel) {
      updateSelectInput(session, "month", selected = setdiff(sel, "All"))
    }
  })

  observeEvent(input$adv_province, ignoreInit = TRUE, {
    sel <- input$adv_province
    if (length(sel) > 1 && "Canada" %in% sel) {
      updateSelectInput(session, "adv_province", selected = setdiff(sel, "Canada"))
    }
  })
  observeEvent(input$adv_age_group, ignoreInit = TRUE, {
    sel <- input$adv_age_group
    if (length(sel) > 1 && "All" %in% sel) {
      updateSelectInput(session, "adv_age_group", selected = setdiff(sel, "All"))
    }
  })
  observeEvent(input$adv_month, ignoreInit = TRUE, {
    sel <- input$adv_month
    if (length(sel) > 1 && "All" %in% sel) {
      updateSelectInput(session, "adv_month", selected = setdiff(sel, "All"))
    }
  })

  # Reference filter helpers
  ref_filters <- reactive({
    list(
      pts = input$province,
      ages = if (is.null(input$age_group) || (length(input$age_group) == 1 && input$age_group[1] == "All")) NULL else input$age_group,
      months = if (is.null(input$month) || (length(input$month) == 1 && input$month[1] == "All")) NULL else as.integer(input$month)
    )
  })

  fb_filtered <- reactive({
    f <- ref_filters()
    fb_filter_micro(pt_names = f$pts, months = f$months, age_groups = f$ages)
  })

  output$ref_summary_ui <- renderUI({
    f <- ref_filters()
    pts <- f$pts %||% "Canada"
    ages <- f$ages %||% "All"
    months <- if (is.null(f$months)) "All" else month.name[as.integer(f$months)]
    tagList(
      p("This app computes reference exposure percentages from Foodbook microdata, weighted and combined across your selected filters."),
      tags$ul(
        tags$li(tags$b("Reference PT(s): "), paste(pts, collapse = ", ")),
        tags$li(tags$b("Age group(s): "), paste(ages, collapse = ", ")),
        tags$li(tags$b("Month(s): "), paste(months, collapse = ", "))
      ),
      p("Tip: Defaults like \"Canada\" and \"All\" auto-deselect once you add another selection.")
    )
  })

  output$ref_top_exposures <- renderDT({
    f <- ref_filters()
    codes <- as.vector(fb_exposure_choices())
    refs <- fb_reference_percents(codes, pt_names = f$pts, months = f$months, age_groups = f$ages)
    lbls <- names(fb_exposure_choices())
    names(lbls) <- as.vector(fb_exposure_choices())
    tibble::tibble(
      Exposure = lbls[names(refs)],
      `Reference %` = round(as.numeric(refs), 1)
    ) %>%
      arrange(desc(`Reference %`)) %>%
      head(30) %>%
      datatable(options = list(pageLength = 10, order = list(list(1, 'desc'))), rownames = FALSE)
  })

  output$ref_pt_plot <- renderPlot({
    d <- fb_filtered()
    req(nrow(d) > 0)
    pt_map <- fb_pt_names()
    # invert mapping names->codes to codes->names
    codes <- unname(fb_pt_map())
    names(codes) <- names(fb_pt_map())
    inv <- stats::setNames(names(codes), codes)
    tibble::tibble(PT = d$PT) %>%
      mutate(PT = inv[as.character(PT)] %||% PT) %>%
      count(PT) %>%
      ggplot(aes(x = reorder(PT, n), y = n)) +
      geom_col(fill = "#0f4c81", alpha = 0.85) +
      coord_flip() +
      labs(title = "Coverage by PT (after filters)", x = NULL, y = "Records") +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13)
      )
  })

  output$ref_month_plot <- renderPlot({
    d <- fb_filtered()
    req(nrow(d) > 0)
    tibble::tibble(Month = as.integer(d$Month)) %>%
      filter(!is.na(Month), Month >= 1, Month <= 12) %>%
      mutate(MonthName = factor(month.name[Month], levels = month.name)) %>%
      count(MonthName) %>%
      ggplot(aes(x = MonthName, y = n)) +
      geom_col(fill = "#1b7b57", alpha = 0.85) +
      labs(title = "Coverage by Month (after filters)", x = NULL, y = "Records") +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#0f4c81"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  # Advanced: process uploaded CEDARS Excel and compute results
  adv_cases <- reactive({
    req(input$cedars_file)
    path <- input$cedars_file$datapath
    # Read long-form exposure answers
    df_exp <- tryCatch(readxl::read_excel(path, sheet = "case exposure answer"), error = function(e) NULL)
    validate(need(!is.null(df_exp), "Could not read 'case exposure answer' sheet."))
    # Normalise column names
    names(df_exp) <- gsub("[^a-z0-9]+", "", tolower(names(df_exp)))
    # Expect columns: NationalID, Exposurecode, Hasexposureoccurred
    need_cols <- c("nationalid", "exposurecode", "hasexposureoccurred")
    validate(need(all(need_cols %in% names(df_exp)), paste0("Missing columns in exposure sheet: ", paste(setdiff(need_cols, names(df_exp)), collapse = ", "))))
    df_exp <- df_exp %>%
      transmute(natid = as.character(.data$nationalid),
                exposure = as.character(.data$exposurecode),
                val = tolower(as.character(.data$hasexposureoccurred)))

    # Keep confirmed cases by merging linelist
    df_line <- tryCatch(readxl::read_excel(path, sheet = "Salmonella Case"), error = function(e) NULL)
    validate(need(!is.null(df_line), "Could not read 'Salmonella Case' sheet."))
    names(df_line) <- gsub("[^a-z0-9]+", "", tolower(names(df_line)))
    # Expect columns: natid, casestatus, provinceterritory, sexcase, agecase, earliestdate
    # Use available subset; filter to Confirmed if casestatus exists
    if ("casestatus" %in% names(df_line)) {
      df_line <- df_line %>% filter(tolower(as.character(.data$casestatus)) == "confirmed")
    }
    if (!"natid" %in% names(df_line)) {
      # Some exports use NationalID
      if ("nationalid" %in% names(df_line)) df_line$natid <- as.character(df_line$nationalid)
    }
    validate(need("natid" %in% names(df_line), "Missing 'natid' in linelist."))
    df_line <- df_line %>% transmute(natid = as.character(.data$natid), provinceterritory = .data$provinceterritory %||% NA)

    df <- df_exp %>% inner_join(df_line, by = "natid")
    df
  })

  adv_results <- reactive({
    d <- adv_cases()
    # Summarise counts by exposure code
    exposure_counts <- d %>%
      mutate(val = recode(val, y = "Y", n = "N", p = "P", dk = "DK")) %>%
      filter(val %in% c("Y", "N", "P", "DK")) %>%
      count(exposure, val) %>%
      tidyr::pivot_wider(names_from = val, values_from = n, values_fill = 0)

    codes <- exposure_counts$exposure
    pts <- input$adv_province
    ages <- if (is.null(input$adv_age_group) || (length(input$adv_age_group) == 1 && input$adv_age_group[1] == "All")) NULL else input$adv_age_group
    months <- if (is.null(input$adv_month) || (length(input$adv_month) == 1 && input$adv_month[1] == "All")) NULL else as.integer(input$adv_month)

    ref_perc <- fb_reference_percents(codes, pt_names = pts, months = months, age_groups = ages)

    code_to_label <- names(fb_exposure_choices())
    names(code_to_label) <- as.vector(fb_exposure_choices())

    exposure_counts %>%
      rowwise() %>%
      mutate(
        Exposure = code_to_label[exposure] %||% exposure,
        province_ref = as.numeric(ref_perc[match(exposure, names(ref_perc))]),
        y_plus_p = (`Y` %||% 0) + (`P` %||% 0),
        total = y_plus_p + (`N` %||% 0),
        observed_prop = if (total > 0) y_plus_p / total else NA_real_,
        p_value = if (total > 0) pbinom(y_plus_p - 1, total, province_ref / 100, lower.tail = FALSE) else NA_real_,
        Classification = classify_exposure(p_value, observed_prop, province_ref)
      ) %>%
      ungroup() %>%
      mutate(`Reference %` = round(province_ref, 1)) %>%
      transmute(
        Exposure,
        `Total Valid` = total,
        `Observed %` = observed_prop,
        `Reference %` = `Reference %`,
        `P-Value` = p_value,
        Classification
      )
  })

  output$adv_results_table <- renderDT({
    req(adv_results())
    res <- adv_results() %>%
      mutate(`Observed %` = round(`Observed %` * 100, 1), `Reference %` = round(`Reference %`, 1), `P-Value` = round(`P-Value`, 4))
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
}

# --- 7. Run Application ---
shinyApp(ui, server, enableBookmarking = "url")
