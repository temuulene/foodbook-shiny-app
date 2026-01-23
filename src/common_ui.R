# Shared UI components for Foodbook Shiny Apps
# Used by both app-public and app-internal

library(shiny)
library(bslib)
library(shinyjs)

# Common Theme
fb_theme <- function() {
  bs_theme(
    version = 5,
    bg = "#f7f9fc",
    fg = "#0f172a",
    primary = "#0e4a7b",
    secondary = "#4b5563",
    success = "#176d4e",
    info = "#0e6a88",
    warning = "#a45100",
    danger = "#b21f2d",
    base_font = font_google("Inter", wght = "300;400;600;700"),
    heading_font = font_google("DM Sans", wght = "400;600;700")
  ) |>
    bs_add_variables(
      "body-color" = "#0f172a",
      "card-cap-bg" = "#f7faff",
      "card-border-color" = "#dde6f5",
      "border-radius" = "0.75rem",
      "min-contrast-ratio" = 4.5,
      "color-contrast-dark" = "#000000",
      "color-contrast-light" = "#ffffff",
      "link-color" = "#0e4a7b",
      "link-hover-color" = "#0a3a61"
    ) |>
    # CSS styles moved to www/styles.css for maintainability
    bs_add_rules("")
}

# Common HEAD elements
fb_commons_head <- function() {
  tagList(
    useShinyjs(),
    extendShinyjs(
      text = "
        shinyjs.resetFileInput = function(params) {
          var id = params.id;
          var $fileInput = $('input[type=\"file\"][id=' + id + ']');
          if (!$fileInput.length) {
            $fileInput = $('#' + id).find('input[type=\"file\"]');
          }
          if ($fileInput.length) {
            $fileInput.val('');
            $fileInput.trigger('change');
          }
          var $wrapper = $fileInput.length ? $fileInput.closest('.shiny-file-input') : $('#' + id);
          if ($wrapper.length) {
            var $textInput = $wrapper.find('input[type=\"text\"]');
            if ($textInput.length) {
              $textInput.val('');
            }
            $wrapper.find('.progress-bar').css('width', '0%');
          }
        };
      ",
      functions = c("resetFileInput")
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "app.js")
    )
  )
}
