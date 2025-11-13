# Internationalization (i18n) Helper Functions
# Provides easy access to translations throughout the app

suppressPackageStartupMessages({
  library(shiny.i18n)
})

.fb_i18n_env <- new.env(parent = emptyenv())
.fb_i18n_env$translation_path <- "translations/translation.json"

fb_create_translator <- function(lang = "en") {
  translator <- Translator$new(
    translation_json_path = .fb_i18n_env$translation_path
  )
  translator$set_translation_language(lang)
  translator
}

init_translator <- function(session, lang = "en") {
  stopifnot(!is.null(session))
  translator <- fb_create_translator(lang)
  session$userData$translator <- translator
  translator
}

get_translator <- function(session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    translator <- session$userData$translator
    if (!is.null(translator)) {
      return(translator)
    }
  }

  if (!exists("fallback_translator", envir = .fb_i18n_env, inherits = FALSE)) {
    .fb_i18n_env$fallback_translator <- fb_create_translator()
  }

  .fb_i18n_env$fallback_translator
}

t_ <- function(key, lang = NULL) {
  translator <- get_translator()
  if (!is.null(lang)) {
    current_lang <- translator$get_translation_language()
    translator$set_translation_language(lang)
    result <- translator$t(key)
    translator$set_translation_language(current_lang)
    return(result)
  }
  translator$t(key)
}

set_language <- function(lang, session = shiny::getDefaultReactiveDomain()) {
  translator <- get_translator(session)
  translator$set_translation_language(lang)
  if (!is.null(session)) {
    session$userData$translator <- translator
  } else {
    .fb_i18n_env$fallback_translator <- translator
  }
  invisible(TRUE)
}

get_language <- function(session = shiny::getDefaultReactiveDomain()) {
  translator <- get_translator(session)
  translator$get_translation_language()
}

pt_names_i18n <- function(lang = NULL) {
  if (is.null(lang)) lang <- get_language()
  fb_pt_names(lang = lang)
}

month_names_i18n <- function(lang = NULL) {
  if (is.null(lang)) lang <- get_language()
  fb_month_names(lang = lang)
}

classification_label_i18n <- function(classification, lang = NULL) {
  if (is.null(lang)) lang <- get_language()

  classification_map <- c(
    "Alert" = "Alert",
    "Borderline" = "Borderline",
    "Not Significant" = "Not Significant",
    "Insufficient Data" = "Insufficient Data",
    "No Reference Value" = "No Reference Value"
  )

  key <- classification_map[classification]
  if (is.na(key)) return(classification)

  t_(key, lang = lang)
}
