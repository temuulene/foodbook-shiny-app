# =============================================================================
# Toolkit Integration Functions (Added Jan 2026)
# =============================================================================

#' Load systematized Excel toolkit data (bilingual list + proportions)
fb_load_toolkit_data <- function() {
  # Load Bilingual Exposure List
  bilingual_path <- fb_get_base_path("data/exposures_bilingual.csv")
  if (file.exists(bilingual_path)) {
    fb_env$toolkit_exposures <- utils::read.csv(bilingual_path, encoding = "UTF-8", stringsAsFactors = FALSE)
  }

  # Load Proportions
  props_path <- fb_get_base_path("data/exposure_proportions_by_pt.csv")
  if (file.exists(props_path)) {
    fb_env$toolkit_proportions <- utils::read.csv(props_path, encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  
  invisible(list(exposures = fb_env$toolkit_exposures, proportions = fb_env$toolkit_proportions))
}

#' Get list of categories in specified language
fb_exposure_categories <- function(lang = "en") {
  if (is.null(fb_env$toolkit_exposures)) {
    fb_load_toolkit_data()
  }
  
  if (is.null(fb_env$toolkit_exposures)) return(character(0))
  
  col <- if (lang == "fr") "category_fr" else "category_en"
  # Clean up categories (remove empty or NA)
  cats <- unique(fb_env$toolkit_exposures[[col]])
  cats <- cats[!is.na(cats) & cats != ""]
  
  # Convert to Title Case if English
  if (lang != "fr") {
    cats <- tools::toTitleCase(tolower(cats))
  }
  sort(cats)
}

#' Get exposures filtered by category for Toolkit mode
fb_toolkit_exposure_choices <- function(lang = "en", category = NULL) {
  if (is.null(fb_env$toolkit_exposures)) {
    fb_load_toolkit_data()
  }
  
  if (is.null(fb_env$toolkit_exposures)) return(list())
  
  df <- fb_env$toolkit_exposures
  
  # Filter by category if specified
  if (!is.null(category) && category != "" && category != "All" && category != "Toutes" && category != "Tous") {
    cat_col <- if (lang == "fr") "category_fr" else "category_en"
    norm_category <- tolower(category)
    norm_df <- tolower(df[[cat_col]])
    df <- df[norm_df == norm_category, ]
  }
  
  # Prepare choices list: Label -> Variable Name (or Number if var name missing)
  label_col <- if (lang == "fr") "exposure_fr" else "exposure_en"
  
  values <- ifelse(df$variable_name != "", df$variable_name, as.character(df$number))
  names(values) <- df[[label_col]]
  
  # Sort alphabetically by label
  values <- values[order(names(values))]
  
  as.list(values)
}

#' Get reference percent from Toolkit (Table 6)
#' @param exposure_id Variable name or exposure number
#' @param pt_name Full PT name (e.g. "British Columbia") or "Canada" or abbreviation
fb_toolkit_reference_percent <- function(exposure_id, pt_name = "Canada") {
  if (is.null(fb_env$toolkit_proportions)) {
    fb_load_toolkit_data()
  }
  
  if (is.null(fb_env$toolkit_proportions)) return(NA_real_)
  
  # Map PT name to Abbreviation used in CSV
  pt_abbr <- "Canada"
  if (pt_name != "Canada") {
    # Try normalizing to code first
    code <- fb_normalize_pt_names(pt_name)
    if (length(code) > 0) {
      abbr_map <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU")
      # Note: codes are 1-13
      if (code >= 1 && code <= 13) {
        pt_abbr <- abbr_map[code]
      }
    } else {
      # Maybe it's already an abbr?
      if (pt_name %in% c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU")) {
        pt_abbr <- pt_name
      }
    }
  }
  
  # Find row by variable_name or exposure_number
  row_idx <- which(fb_env$toolkit_proportions$variable_name == exposure_id)
  
  if (length(row_idx) == 0) {
     # Try matching as number
     row_idx <- which(fb_env$toolkit_proportions$exposure_number == exposure_id)
  }
  
  if (length(row_idx) == 0) return(NA_real_)
  
  val <- fb_env$toolkit_proportions[row_idx[1], pt_abbr]
  if (is.null(val)) return(NA_real_)
  as.numeric(val)
}
