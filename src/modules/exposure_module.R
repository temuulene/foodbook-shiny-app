# Exposure Input Module
# Provides UI and server logic for entering case exposure counts

translate_or_fallback <- function(key, lang, fallback_en, fallback_fr = fallback_en) {
  fallback <- if (identical(lang, "fr")) fallback_fr else fallback_en

  if (exists("t_", mode = "function")) {
    translated <- tryCatch(
      t_(key, lang = lang),
      error = function(e) NULL
    )
    if (!is.null(translated) && nzchar(translated)) {
      return(translated)
    }
  }

  fallback
}

exposure_module_ui <- function(id, exposure_name, ref_value, is_custom = FALSE, lang = "en") {
  ns <- NS(id)

  yes_label <- translate_or_fallback("Yes", lang, "Yes", "Oui")
  prob_label <- translate_or_fallback("Probably", lang, "Probably", "Probablement")
  no_label <- translate_or_fallback("No", lang, "No", "Non")
  dk_label <- translate_or_fallback("DK", lang, "DK", "NSP")
  custom_label <- translate_or_fallback(
    "Custom Reference % (optional)",
    lang,
    "Custom Reference % (optional)",
    "% de r\u00e9f\u00e9rence personnalis\u00e9 (optionnel)"
  )
  ref_label <- translate_or_fallback(
    "Reference Value",
    lang,
    "Reference Value",
    "Valeur de r\u00e9f\u00e9rence"
  )
  custom_suffix_label <- translate_or_fallback(
    "(custom)",
    lang,
    "(custom)",
    "(personnalis\u00e9)"
  )
  header_text <- if (is_custom) {
    paste(exposure_name, custom_suffix_label)
  } else {
    exposure_name
  }

  div(
    class = "exposure-input-group",
    h4(header_text, class = "exposure-header"),
    layout_columns(
      col_widths = c(2, 2, 2, 2, 4),
      numericInput(ns("yes"), yes_label, 0, min = 0, max = 10000, step = 1),
      numericInput(ns("prob"), prob_label, 0, min = 0, max = 10000, step = 1),
      numericInput(ns("no"), no_label, 0, min = 0, max = 10000, step = 1),
      numericInput(ns("dk"), dk_label, 0, min = 0, max = 10000, step = 1),
      if (is_custom) {
        numericInput(ns("custom_ref"), custom_label, value = 60, min = 0, max = 100, step = 0.1)
      } else {
        div(
          p(paste0(ref_label, ":"), class = "ref-value"),
          span(style = "font-size: 1.2em;", paste0(ref_value, "%"))
        )
      }
    )
  )
}

exposure_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      tibble::tibble(
        yes = pmax(0, floor(input$yes %||% 0)),
        prob = pmax(0, floor(input$prob %||% 0)),
        no = pmax(0, floor(input$no %||% 0)),
        dk = pmax(0, floor(input$dk %||% 0)),
        custom_ref = if (!is.null(input$custom_ref)) input$custom_ref else NA_real_
      )
    })
  })
}

# Update exposure module inputs from the PARENT session.
# MODULE ISOLATION NOTE (#3/#12): NS(id) is intentionally used here instead of
# session$ns because this function is called from app.R (parent context), not
# from within moduleServer(). Using session$ns would double-nest the namespace.
exposure_module_update <- function(session, id, yes = NULL, prob = NULL, no = NULL, dk = NULL, custom_ref = NULL) {
  ns <- NS(id)
  if (!is.null(yes)) updateNumericInput(session, ns("yes"), value = yes)
  if (!is.null(prob)) updateNumericInput(session, ns("prob"), value = prob)
  if (!is.null(no)) updateNumericInput(session, ns("no"), value = no)
  if (!is.null(dk)) updateNumericInput(session, ns("dk"), value = dk)
  if (!is.null(custom_ref)) updateNumericInput(session, ns("custom_ref"), value = custom_ref)
}
