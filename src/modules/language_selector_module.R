# Language Selector Module
# Provides UI and server logic for switching between English and French

language_selector_ui <- function(id, style = "radio") {
  ns <- NS(id)

  choices <- c("English" = "en", "Français" = "fr")

  if (style == "radio") {
    div(
      style = "display: inline-block; margin-left: 20px;",
      radioButtons(
        inputId = ns("language"),
        label = NULL,
        choices = choices,
        selected = "en",
        inline = TRUE
      )
    )
  } else if (style == "dropdown") {
    selectInput(
      inputId = ns("language"),
      label = "Language / Langue",
      choices = choices,
      selected = "en"
    )
  } else {
    div(
      style = "display: inline-block; margin-left: 20px;",
      radioGroupButtons(
        inputId = ns("language"),
        label = NULL,
        choices = choices,
        selected = "en",
        justified = FALSE,
        size = "sm"
      )
    )
  }
}

language_selector_server <- function(id, session_parent = NULL, style = "radio") {
  moduleServer(id, function(input, output, session) {
    observeEvent(session$clientData$url_search, once = TRUE, {
      if (!is.null(session_parent)) {
        query <- parseQueryString(session_parent$clientData$url_search)
        url_lang <- query$lang

        if (!is.null(url_lang) && url_lang %in% c("en", "fr")) {
          if (style == "dropdown") {
            updateSelectInput(session, "language", selected = url_lang)
          } else {
            updateRadioButtons(session, "language", selected = url_lang)
          }
        }
      }
    })

    observeEvent(input$language, {
      session$sendCustomMessage("language_changed", input$language)
    }, ignoreInit = FALSE)

    list(
      language = reactive(input$language),
      is_french = reactive(input$language == "fr"),
      is_english = reactive(input$language == "en")
    )
  })
}

get_current_language <- function(lang_reactive) {
  if (is.reactive(lang_reactive)) {
    return(lang_reactive())
  }
  "en"
}
