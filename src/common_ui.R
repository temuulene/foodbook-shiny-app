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
    primary = "#EB2D37", # FIP Red
    secondary = "#4b5563",
    success = "#176d4e",
    info = "#0e6a88",
    warning = "#a45100",
    danger = "#B91C25", # A darker shade of FIP red for distinction
    base_font = font_google("Inter", wght = "300;400;600;700"),
    heading_font = font_google("DM Sans", wght = "400;600;700"),
    "card-border-radius" = "0.85rem",
    "card-cap-padding-y" = "0.85rem",
    "card-cap-padding-x" = "1.1rem",
    "card-spacer-y" = "1rem",
    "card-spacer-x" = "1.1rem",
    "nav-link-font-weight" = "500",
    "navbar-padding-y" = "0.65rem"
  ) |>
    bs_add_variables(
      "border-radius" = "0.75rem",
      "min-contrast-ratio" = 2,
      "link-color" = "#EB2D37",
      "link-hover-color" = "#B91C25"
    )
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
