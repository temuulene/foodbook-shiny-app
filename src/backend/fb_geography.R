# PT code mapping used by OMD (Foodbook)
fb_pt_map <- function() {
  c(
    "British Columbia" = 1L,
    "Alberta" = 2L,
    "Saskatchewan" = 3L,
    "Manitoba" = 4L,
    "Ontario" = 5L,
    "Quebec" = 6L,
    "New Brunswick" = 7L,
    "Nova Scotia" = 8L,
    "Prince Edward Island" = 9L,
    "Newfoundland and Labrador" = 10L,
    "Yukon" = 11L,
    "Northwest Territories" = 12L,
    "Nunavut" = 13L
  )
}

#' Normalize PT names to numeric codes
#' Accepts English names, French names, and abbreviations
#' @param pt_names Character vector of PT names (any format)
#' @return Integer vector of PT codes (1-13)
fb_normalize_pt_names <- function(pt_names) {
  if (is.null(pt_names) || length(pt_names) == 0) {
    return(integer())
  }

  # Mapping from all accepted formats to numeric codes
  all_pt_map <- c(
    # English names
    "British Columbia" = 1L,
    "Alberta" = 2L,
    "Saskatchewan" = 3L,
    "Manitoba" = 4L,
    "Ontario" = 5L,
    "Quebec" = 6L,
    "New Brunswick" = 7L,
    "Nova Scotia" = 8L,
    "Prince Edward Island" = 9L,
    "Newfoundland and Labrador" = 10L,
    "Yukon" = 11L,
    "Northwest Territories" = 12L,
    "Nunavut" = 13L,
    # French names
    "Colombie-Britannique" = 1L,
    "Québec" = 6L,
    "Nouveau-Brunswick" = 7L,
    "Nouvelle-Écosse" = 8L,
    "Nouvelle-Ecosse" = 8L,
    "Île-du-Prince-Édouard" = 9L,
    "Ile-du-Prince-Edouard" = 9L,
    "Terre-Neuve-et-Labrador" = 10L,
    "Territoires du Nord-Ouest" = 12L,
    # Abbreviations
    "BC" = 1L,
    "AB" = 2L,
    "SK" = 3L,
    "MB" = 4L,
    "ON" = 5L,
    "QC" = 6L,
    "NB" = 7L,
    "NS" = 8L,
    "PE" = 9L,
    "PEI" = 9L,
    "NL" = 10L,
    "YT" = 11L,
    "NT" = 12L,
    "NU" = 13L
  )

  codes <- unname(all_pt_map[pt_names])
  codes[!is.na(codes)]
}

# Normalize mixed PT inputs to abbreviations (e.g., "ON")
fb_normalize_pt_values <- function(pt_values) {
  if (is.null(pt_values) || length(pt_values) == 0) {
    return(character())
  }

  abbrs <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU")
  abbrs_upper <- abbrs

  normalize_one <- function(value) {
    if (is.null(value) || is.na(value)) {
      return(NA_character_)
    }

    # Numeric or numeric-like values (1-13)
    if (is.numeric(value)) {
      code <- as.integer(value)
      if (!is.na(code) && code >= 1 && code <= 13) {
        return(abbrs[code])
      }
    }

    val_chr <- trimws(as.character(value))
    num_chr <- safe_as_numeric(val_chr)
    if (!is.na(num_chr) && num_chr >= 1 && num_chr <= 13 && grepl("^\\d+$", val_chr)) {
      return(abbrs[num_chr])
    }

    val_upper <- toupper(val_chr)
    if (val_upper %in% abbrs_upper) {
      return(val_upper)
    }

    code <- fb_normalize_pt_names(val_chr)
    if (length(code) > 0) {
      return(abbrs[code[1]])
    }

    val_chr
  }

  purrr::map_chr(pt_values, normalize_one)
}

# Extract Province/Territory values from a data frame and optionally normalize
fb_extract_provinceterritory <- function(df, normalize = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    return(character())
  }

  values <- if ("provinceterritory" %in% names(df)) {
    as.character(df$provinceterritory)
  } else {
    rep(NA_character_, nrow(df))
  }

  if (normalize) {
    values <- fb_normalize_pt_values(values)
  }

  values
}

# Build available PT list from cases; include Canada only when all PTs present
fb_available_pts_from_cases <- function(pt_values) {
  pt_codes <- fb_normalize_pt_values(pt_values)
  pt_codes <- unique(pt_codes[!is.na(pt_codes) & nzchar(pt_codes)])
  if (!length(pt_codes)) {
    return(character())
  }

  all_pts <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU")
  if (setequal(pt_codes, all_pts)) {
    return(c("Canada", pt_codes))
  }

  pt_codes
}

# Public: compute reference percentages for a vector of exposure codes
fb_pt_abbrev_map <- function() {
  c(
    "British Columbia" = "BC",
    "Alberta" = "AB",
    "Saskatchewan" = "SK",
    "Manitoba" = "MB",
    "Ontario" = "ON",
    "Quebec" = "QC",
    "New Brunswick" = "NB",
    "Nova Scotia" = "NS",
    "Prince Edward Island" = "PE",
    "Newfoundland and Labrador" = "NL",
    "Yukon" = "YT",
    "Northwest Territories" = "NT",
    "Nunavut" = "NU",
    "Canada" = "Canada"
  )
}
