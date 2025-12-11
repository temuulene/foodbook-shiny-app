# Backend helpers to use OMD Foodbook microdata and labels
# =============================================================================
# This backend is designed to work with PHAC OMD's authoritative data files:
#   - upgrade-context/foodbook.dta (Foodbook 1)
#   - upgrade-context/foodbook2v2.dta (Foodbook 2)
#   - upgrade-context/foodbook data.do (rename mappings)
#   - upgrade-context/foodbook variable labeling.do (exposure labels)
#
# When legacy .dta files are not available, falls back to Open Canada CSVs.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(data.table)
  # haven is optional - only needed for legacy .dta microdata files
  if (requireNamespace("haven", quietly = TRUE)) {
    library(haven)
  }
})

# =============================================================================
# Shared Utility Functions (used by both app-public and app-internal)
# =============================================================================

#' Classify exposure based on p-value and observed vs reference proportions
#' @param p_value P-value from binomial test
#' @param observed_prop Observed proportion (0-1 scale)
#' @param ref_prop Reference percentage (0-100 scale)
#' @return Character classification: "Alert", "Borderline", "Not Significant",
#'         "Insufficient Data", or "No Reference Value"
classify_exposure <- function(p_value, observed_prop, ref_prop) {
  if (is.na(ref_prop)) {
    return("No Reference Value")
  }
  ref_prop_decimal <- ref_prop / 100
  if (is.na(p_value)) {
    return("Insufficient Data")
  }
  if (observed_prop > ref_prop_decimal) {
    case_when(
      p_value <= 0.05 ~ "Alert",
      p_value <= 0.10 ~ "Borderline",
      TRUE ~ "Not Significant"
    )
  } else {
    "Not Significant"
  }
}

#' Create safe HTML ID from exposure name
#' @param exposure_name Character string to sanitize
#' @return Character string with only alphanumeric characters
make_safe_id <- function(exposure_name) {
  gsub("[^a-zA-Z0-9]", "", exposure_name)
}

fb_env <- new.env(parent = emptyenv())

# Helper to detect correct base directory (handles both root and subdirectory apps)
fb_get_base_path <- function(rel_path) {
  # Try current directory first
  if (file.exists(rel_path) || dir.exists(rel_path)) {
    return(rel_path)
  }
  # Try one level up (for apps in subdirectories)
  parent_path <- file.path("..", rel_path)
  if (file.exists(parent_path) || dir.exists(parent_path)) {
    return(parent_path)
  }
  # Return original path (will fail downstream with clear error)
  return(rel_path)
}

# =============================================================================
# New Open Canada Data Loading Functions (FB1 + FB2)
# =============================================================================

# Load Foodbook 1 microdata from Open Canada (3 CSV files that need joining)
fb_load_fb1_csv <- function(lang = "en") {
  base_dir <- fb_get_base_path("data/open-canada/foodbook-1")

  # File patterns based on language
  if (lang == "fr") {
    part1_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-1-fr.csv")
    part2_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-2-fr.csv")
    part3_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-3-fr.csv")
  } else {
    part1_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-1-en.csv")
    part2_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-2-en.csv")
    part3_file <- file.path(base_dir, "foodbook-pumf-fmgd-part-partie-3-en.csv")
  }

  # Check if files exist
  if (
    !file.exists(part1_file) ||
      !file.exists(part2_file) ||
      !file.exists(part3_file)
  ) {
    return(NULL)
  }

  # Load all three parts using data.table for speed
  # Use fill=TRUE to handle rows with varying field counts
  part1 <- tryCatch(
    data.table::fread(
      part1_file,
      data.table = FALSE,
      fill = TRUE,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )
  part2 <- tryCatch(
    data.table::fread(
      part2_file,
      data.table = FALSE,
      fill = TRUE,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )
  part3 <- tryCatch(
    data.table::fread(
      part3_file,
      data.table = FALSE,
      fill = TRUE,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(part1) || is.null(part2) || is.null(part3)) {
    return(NULL)
  }

  # Normalize column names (French → English for consistency)
  if (lang == "fr") {
    part1 <- fb_normalize_fb1_colnames(part1, "fr")
    part2 <- fb_normalize_fb1_colnames(part2, "fr")
    part3 <- fb_normalize_fb1_colnames(part3, "fr")
  }

  # Join all three parts on uniqueid
  # Part 1 and Part 2 share uniqueid, Part 3 may have QINTRO3 for PT
  df <- part1 |>
    dplyr::left_join(part2, by = "uniqueid", suffix = c("", "_p2")) |>
    dplyr::left_join(part3, by = "uniqueid", suffix = c("", "_p3"))

  # Normalize weight column
  df <- fb_normalise_weight(df)

  # Add dataset source identifier
  df$fb_source <- "FB1"

  df
}

# Normalize FB1 French column names to English
fb_normalize_fb1_colnames <- function(df, lang) {
  if (lang != "fr") {
    return(df)
  }

  # French → English mapping for key columns
  name_map <- c(
    "ID_unique" = "uniqueid",
    "IDunique" = "uniqueid",
    "mois_dv" = "month_dv",
    "groupe_age_dv" = "age_grp_dv",
    "EXPWEIGHT_CMA2_dv" = "EXPWEIGHT_CMA2_dv"
  )

  # Apply mapping where columns exist
  for (fr_name in names(name_map)) {
    if (fr_name %in% names(df)) {
      names(df)[names(df) == fr_name] <- name_map[fr_name]
    }
  }

  df
}

# Load Foodbook 2 microdata from Open Canada (single CSV file)
fb_load_fb2_csv <- function(lang = "en") {
  base_dir <- fb_get_base_path("data/open-canada/foodbook-2")

  # File patterns based on language
  if (lang == "fr") {
    fb2_file <- file.path(
      base_dir,
      "atlas-alimentaire-2.0-fichier-de-microdonnees-a-grande-diffusion-2023.csv"
    )
  } else {
    fb2_file <- file.path(
      base_dir,
      "foodbook-2.0-public-use-microdata-file-2023.csv"
    )
  }

  # Check if file exists
  if (!file.exists(fb2_file)) {
    return(NULL)
  }

  # Load using data.table for speed
  df <- tryCatch(
    data.table::fread(
      fb2_file,
      data.table = FALSE,
      fill = TRUE,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(df)) {
    return(NULL)
  }

  # Normalize column names (French → English for consistency)
  if (lang == "fr") {
    df <- fb_normalize_fb2_colnames(df)
  }

  # Normalize weight column
  df <- fb_normalise_weight(df)

  # Add dataset source identifier
  df$fb_source <- "FB2"

  df
}

# Normalize FB2 French column names to English
fb_normalize_fb2_colnames <- function(df) {
  # French → English mapping for key columns
  name_map <- c(
    "ID_unique" = "Unique_id",
    "mois_dv" = "month_dv",
    "Genre_dv" = "Gender_dv",
    "groupe_age_dv" = "Age_grp_dv",
    "Proj_weight_non_traveller_dv" = "Proj_weight_non_traveller_dv",
    "PT" = "PT"
  )

  # Apply mapping where columns exist
  for (fr_name in names(name_map)) {
    if (fr_name %in% names(df)) {
      names(df)[names(df) == fr_name] <- name_map[fr_name]
    }
  }

  df
}

# Parse bilingual Stata label files and return EN + FR labels
fb_parse_label_map_bilingual <- function(en_path, fr_path = NULL) {
  # Parse English labels
  en_labels <- fb_parse_label_map(en_path)

  # If no French path provided, return English only
  if (is.null(fr_path) || !file.exists(fr_path)) {
    en_labels <- en_labels |>
      dplyr::mutate(
        label_en = label,
        label_fr = as.character(label) # Explicit character conversion for type safety
      )
    return(en_labels)
  }

  # Parse French labels
  fr_labels <- fb_parse_label_map(fr_path)

  # Join on code
  result <- en_labels |>
    dplyr::left_join(
      fr_labels |> dplyr::rename(label_fr = label),
      by = "code",
      suffix = c("_en", "")
    ) |>
    dplyr::rename(label_en = label) |>
    dplyr::mutate(
      # Use coalesce for type-safe NA handling (ifelse can return logical)
      label_fr = dplyr::coalesce(label_fr, label_en)
    )

  result
}

# Parse Stata rename directives from a .do file
# Returns tibble with old (original column) and new (renamed column) names
# Based on the authoritative upgrade-context/foodbook data.do
fb_parse_renames <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) {
    character()
  })
  if (!length(lines)) {
    return(tibble::tibble(old = character(), new = character()))
  }

  # Match rename statements, handling optional whitespace and comments
  # Pattern: rename OLD NEW (ignore commented lines starting with *)
  m <- stringr::str_match(lines, "^\\s*rename\\s+([^\\s]+)\\s+([^\\s/*]+)")
  m <- m[!is.na(m[, 1]), , drop = FALSE]

  # Also skip lines that are fully commented (starting with *)
  commented <- grepl(
    "^\\s*\\*",
    lines[!is.na(stringr::str_match(lines, "^\\s*rename")[, 1])]
  )
  if (nrow(m) > 0 && length(commented) == nrow(m)) {
    m <- m[!commented, , drop = FALSE]
  }

  tibble::tibble(old = m[, 2], new = m[, 3])
}

# Parse FB1-specific renames (before the "drop Q*" line in foodbook data.do)
# These are the Foodbook 1 variables kept before appending FB2
fb_parse_fb1_renames <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) {
    character()
  })
  if (!length(lines)) {
    return(tibble::tibble(old = character(), new = character()))
  }

  # Find the "drop Q*" line - everything before it is FB1-specific
  drop_line <- which(grepl("^\\s*drop\\s+Q\\*", lines))
  if (length(drop_line) == 0) {
    # No drop line found, return all renames
    return(fb_parse_renames(path))
  }

  # Get only lines before the drop Q*
  fb1_lines <- lines[1:(drop_line[1] - 1)]

  m <- stringr::str_match(fb1_lines, "^\\s*rename\\s+([^\\s]+)\\s+([^\\s/*]+)")
  m <- m[!is.na(m[, 1]), , drop = FALSE]

  tibble::tibble(old = m[, 2], new = m[, 3])
}

# Parse FB2-specific renames (after the "append using" line in foodbook data.do)
fb_parse_fb2_renames <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) {
    character()
  })
  if (!length(lines)) {
    return(tibble::tibble(old = character(), new = character()))
  }

  # Find the "append using" line - everything after it is FB2-specific
  append_line <- which(grepl("^\\s*append\\s+using", lines))
  if (length(append_line) == 0) {
    return(tibble::tibble(old = character(), new = character()))
  }

  # Get only lines after the append
  fb2_lines <- lines[(append_line[1] + 1):length(lines)]

  m <- stringr::str_match(fb2_lines, "^\\s*rename\\s+([^\\s]+)\\s+([^\\s/*]+)")
  m <- m[!is.na(m[, 1]), , drop = FALSE]

  # Skip commented lines
  commented <- grepl(
    "^\\s*\\*",
    fb2_lines[!is.na(stringr::str_match(fb2_lines, "^\\s*rename")[, 1])]
  )
  if (nrow(m) > 0 && length(commented) == nrow(m)) {
    m <- m[!commented, , drop = FALSE]
  }

  tibble::tibble(old = m[, 2], new = m[, 3])
}

# Get mapping: CEDARS exposure code (P-codes) -> Foodbook column name
# This allows us to calculate references for CEDARS P-codes like P01001
# Note: This should ONLY be used for actual CEDARS P-codes, not renamed Foodbook columns
fb_cedars_to_foodbook_map <- function() {
  fb_init()

  # Check if CEDARS map is loaded
  if (is.null(fb_env$cedars_to_fb_map)) {
    # The CEDARS P-codes need to be manually mapped to Foodbook columns
    # This mapping is based on CEDARS exposure code -> Foodbook renamed column
    # For now, return empty - CEDARS integration would require a separate mapping file
    fb_env$cedars_to_fb_map <- stats::setNames(character(), character())
  }

  fb_env$cedars_to_fb_map
}

# Parse exposure code -> human label mapping from the variable labeling .do file
# Based on the authoritative upgrade-context/foodbook variable labeling.do
fb_parse_label_map <- function(path) {
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      character()
    }
  )
  if (!length(lines)) {
    return(tibble::tibble(code = character(), label = character()))
  }

  # Try authoritative OMD format first: gen/replace label = "Label" if exposure == "code"
  # This is the format used in upgrade-context/foodbook variable labeling.do
  m_omd <- stringr::str_match(
    lines,
    '^\\s*(?:gen|replace)\\s+label\\s*=\\s*"([^"]+)"\\s+if\\s+exposure\\s*==\\s*"([^"]+)"'
  )
  m_omd <- m_omd[!is.na(m_omd[, 1]), , drop = FALSE]

  if (nrow(m_omd) > 0) {
    # OMD format found - this is the authoritative source
    out <- tibble::tibble(
      label = stringr::str_squish(m_omd[, 2]),
      code = m_omd[, 3]
    ) |>
      dplyr::filter(label != "") |>
      dplyr::distinct(code, .keep_all = TRUE)
    return(out)
  }

  # Try Open Canada format: label var CODE "Label"
  m_open <- stringr::str_match(
    lines,
    '^\\s*label\\s+var\\s+([^\\s]+)\\s+"([^"]+)"'
  )
  m_open <- m_open[!is.na(m_open[, 1]), , drop = FALSE]

  if (nrow(m_open) > 0) {
    # Open Canada format found
    # Filter to keep only exposure variables (Q-prefixed and special exposure codes)
    out <- tibble::tibble(
      code = m_open[, 2],
      label = stringr::str_squish(m_open[, 3])
    ) |>
      dplyr::filter(
        label != "",
        !grepl("^\\*", code), # Skip commented lines
        # Only keep exposure variables: Q-prefixed, DQ-prefixed, or specific exposure patterns
        grepl("^(D?Q)[0-9]", code) | code %in% c("organic_dv", "freshherbs_dv")
      ) |>
      dplyr::distinct(code, .keep_all = TRUE)
    return(out)
  }

  tibble::tibble(code = character(), label = character())
}

# Apply a rename mapping to a data.frame (only where cols exist)
fb_apply_renames <- function(df, renames) {
  if (!nrow(renames)) {
    return(df)
  }
  present <- renames$old %in% names(df)
  if (!any(present)) {
    return(df)
  }
  map <- renames$new[present]
  names(map) <- renames$old[present]
  dplyr::rename(df, !!!rlang::set_names(names(map), map))
}

# Discover weight column and normalise to `weight`
# Based on authoritative logic from upgrade-context/foodbook data.do:
#   gen weight = EXPWEIGHT_CMA2
#   replace weight = proj_weight_non_traveller if weight == .
fb_normalise_weight <- function(df) {
  w <- NULL

  # Priority 1: FB1 weight (EXPWEIGHT_CMA2)
  if ("EXPWEIGHT_CMA2" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$EXPWEIGHT_CMA2))
  }

  # Priority 2: FB2 weight (proj_weight_non_traveller) - fills in where FB1 weight is NA
  if ("proj_weight_non_traveller" %in% names(df)) {
    fb2_w <- suppressWarnings(as.numeric(df$proj_weight_non_traveller))
    if (is.null(w)) {
      w <- fb2_w
    } else {
      # Replace NA values with FB2 weight (matches Stata: replace weight = X if weight == .)
      w <- ifelse(is.na(w), fb2_w, w)
    }
  }

  # Fallback: Try Open Canada variant column names
  if (is.null(w) && "EXPWEIGHT_CMA2_dv" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$EXPWEIGHT_CMA2_dv))
  }
  if (is.null(w) && "Proj_weight_non_traveller_dv" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$Proj_weight_non_traveller_dv))
  }

  # Fallback: Generic weight column
  if (is.null(w) && "weight" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$weight))
  }

  # Default to 1 if no weight found
  if (is.null(w)) {
    df$weight <- 1
  } else {
    df$weight <- w
  }
  df
}

# Load, rename, and combine Foodbook microdata
# Based on authoritative workflow from upgrade-context/foodbook data.do:
#   1. Load foodbook.dta (FB1)
#   2. Apply FB1-specific renames (before "drop Q*")
#   3. Append foodbook2v2.dta (FB2)
#   4. Apply FB2-specific renames (after "append using")
#   5. Merge weights
fb_load_microdata <- function(
  do_renames_path = NULL,
  dta_paths = NULL
) {
  # Set defaults with path detection
  if (is.null(do_renames_path)) {
    do_renames_path <- fb_get_base_path("upgrade-context/foodbook data.do")
  }
  if (is.null(dta_paths)) {
    dta_paths <- c(
      fb_get_base_path("upgrade-context/foodbook.dta"),
      fb_get_base_path("upgrade-context/foodbook2v2.dta")
    )
  }

  # Check if haven is available
  if (!requireNamespace("haven", quietly = TRUE)) {
    message("Package 'haven' not available - cannot load .dta files")
    return(NULL)
  }

  # Parse renames from authoritative file
  fb1_renames <- fb_parse_fb1_renames(do_renames_path)
  fb2_renames <- fb_parse_fb2_renames(do_renames_path)

  dfs <- list()

  # Load FB1 (foodbook.dta)
  if (length(dta_paths) >= 1 && file.exists(dta_paths[1])) {
    fb1 <- tryCatch(haven::read_dta(dta_paths[1]), error = function(e) NULL)
    if (!is.null(fb1)) {
      fb1 <- as.data.frame(fb1)
      # Apply FB1-specific renames
      fb1 <- fb_apply_renames(fb1, fb1_renames)
      fb1$fb_source <- "FB1"
      dfs[[length(dfs) + 1]] <- fb1
    }
  }

  # Load FB2 (foodbook2v2.dta)
  if (length(dta_paths) >= 2 && file.exists(dta_paths[2])) {
    fb2 <- tryCatch(haven::read_dta(dta_paths[2]), error = function(e) NULL)
    if (!is.null(fb2)) {
      fb2 <- as.data.frame(fb2)
      # Apply FB2-specific renames
      fb2 <- fb_apply_renames(fb2, fb2_renames)
      fb2$fb_source <- "FB2"
      dfs[[length(dfs) + 1]] <- fb2
    }
  }

  if (!length(dfs)) {
    return(NULL)
  }

  # Combine datasets (append FB2 to FB1, as per authoritative workflow)
  combined <- suppressWarnings(dplyr::bind_rows(dfs))

  # Apply weight normalization (merges FB1 and FB2 weights)
  combined <- fb_normalise_weight(combined)

  combined
}

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
    "Île-du-Prince-Édouard" = 9L,
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

# Initialise and cache everything we need
# Priority order (based on authoritative PHAC OMD workflow):
#   1. Legacy .dta files from upgrade-context/ (most accurate, internal use)
#   2. Open Canada FB2 CSV (public, newest)
#   3. Open Canada FB1 CSV (public, fallback)
fb_init <- function(lang = "en") {
  if (!is.null(fb_env$initialised) && isTRUE(fb_env$initialised)) {
    return(invisible(TRUE))
  }

  # =============================================================================
  # PRIORITY 1: Try legacy OMD microdata from upgrade-context/ (authoritative)
  # This is the data PHAC OMD uses for official analyses
  # =============================================================================
  legacy_data <- NULL
  legacy_dta_path <- fb_get_base_path("upgrade-context/foodbook.dta")
  legacy_dta2_path <- fb_get_base_path("upgrade-context/foodbook2v2.dta")

  if (file.exists(legacy_dta_path) || file.exists(legacy_dta2_path)) {
    message("Found legacy microdata in upgrade-context/, loading...")
    legacy_data <- fb_load_microdata()
    if (!is.null(legacy_data)) {
      message(
        "Loaded authoritative microdata from upgrade-context/ (",
        nrow(legacy_data),
        " respondents)"
      )
    }
  }

  # =============================================================================
  # PRIORITY 2: Try Open Canada FB2 (public, newest, 21K respondents)
  # =============================================================================
  fb2_data <- NULL
  fb1_data <- NULL

  if (is.null(legacy_data)) {
    message("Legacy microdata not found, trying Open Canada data...")
    fb2_data <- fb_load_fb2_csv(lang = lang)

    if (!is.null(fb2_data)) {
      message(
        "Loaded Foodbook 2 microdata from Open Canada (",
        nrow(fb2_data),
        " respondents)"
      )
    } else {
      message("Foodbook 2 data not found, trying Foodbook 1...")
    }

    # =============================================================================
    # PRIORITY 3: Try Open Canada FB1 (fallback for FB1-only exposures)
    # =============================================================================
    fb1_data <- fb_load_fb1_csv(lang = lang)

    if (!is.null(fb1_data)) {
      message(
        "Loaded Foodbook 1 microdata from Open Canada (",
        nrow(fb1_data),
        " respondents)"
      )
    }
  }

  # =============================================================================
  # Set primary microdata source
  # =============================================================================
  if (!is.null(legacy_data)) {
    fb_env$micro <- legacy_data
    fb_env$data_source <- "Legacy"
    fb_env$micro_fb1 <- NULL # Legacy data already combines FB1+FB2
  } else if (!is.null(fb2_data)) {
    fb_env$micro <- fb2_data
    fb_env$data_source <- "FB2"
  } else if (!is.null(fb1_data)) {
    fb_env$micro <- fb1_data
    fb_env$data_source <- "FB1"
  } else {
    fb_env$micro <- NULL
    fb_env$data_source <- NULL
  }

  # Store supplementary FB1 data if FB2 is primary (Open Canada mode only)
  if (!is.null(fb2_data) && !is.null(fb1_data)) {
    # Normalize FB1 column names for filtering compatibility
    if ("QINTRO3" %in% names(fb1_data) && !"PT" %in% names(fb1_data)) {
      fb1_data$PT <- fb1_data$QINTRO3
    }
    if ("month_dv" %in% names(fb1_data) && !"Month" %in% names(fb1_data)) {
      fb1_data$Month <- fb1_data$month_dv
    }
    # Convert age_grp_dv (1-4) to AgeBand ("0-9", "10-19", "20-64", "65+")
    if ("age_grp_dv" %in% names(fb1_data) && !"AgeBand" %in% names(fb1_data)) {
      age_map <- c(`1` = "0-9", `2` = "10-19", `3` = "20-64", `4` = "65+")
      fb1_data$AgeBand <- unname(age_map[as.character(fb1_data$age_grp_dv)])
    }

    fb_env$micro_fb1 <- fb1_data
  } else if (is.null(fb_env$micro_fb1)) {
    fb_env$micro_fb1 <- NULL
  }

  # =============================================================================
  # Load label maps - PRIORITIZE authoritative legacy file
  # =============================================================================
  legacy_label <- fb_get_base_path(
    "upgrade-context/foodbook variable labeling.do"
  )
  fb2_label_en <- fb_get_base_path(
    "data/open-canada/foodbook-2/foodbook-2.0-stata-label-code.txt"
  )
  fb2_label_fr <- fb_get_base_path(
    "data/open-canada/foodbook-2/latlas-alimentaire-2.0-stata-code-des-etiquettes.txt"
  )
  fb1_label_en <- fb_get_base_path(
    "data/open-canada/foodbook-1/foodbook-stata-label-code-des-etiquettes-en.do"
  )
  fb1_label_fr <- fb_get_base_path(
    "data/open-canada/foodbook-1/foodbook-stata-label-code-des-etiquettes-fr.do"
  )

  # Load legacy labels FIRST (authoritative source from PHAC OMD)
  # This file uses the renamed column names (celery, carrot, etc.)
  if (file.exists(legacy_label)) {
    fb_env$label_map_legacy <- fb_parse_label_map(legacy_label) |>
      dplyr::mutate(
        label_en = as.character(label),
        label_fr = as.character(label) # English only in legacy file
      )
    message(
      "Loaded authoritative labels from upgrade-context/foodbook variable labeling.do"
    )
  } else {
    fb_env$label_map_legacy <- tibble::tibble(
      code = character(),
      label_en = character(),
      label_fr = character()
    )
  }

  # Load FB2 labels (bilingual) - fallback/supplement
  if (file.exists(fb2_label_en)) {
    fb_env$label_map_fb2 <- fb_parse_label_map_bilingual(
      fb2_label_en,
      fb2_label_fr
    )
  } else {
    fb_env$label_map_fb2 <- tibble::tibble(
      code = character(),
      label_en = character(),
      label_fr = character()
    )
  }

  # Load FB1 labels (bilingual) - fallback/supplement
  if (file.exists(fb1_label_en)) {
    fb_env$label_map_fb1 <- fb_parse_label_map_bilingual(
      fb1_label_en,
      fb1_label_fr
    )
  } else {
    fb_env$label_map_fb1 <- tibble::tibble(
      code = character(),
      label_en = character(),
      label_fr = character()
    )
  }

  # =============================================================================
  # Create unified label map: Legacy FIRST (authoritative), then FB2, then FB1
  # =============================================================================

  # Handle collisions: If FB1 has a code that is also in FB2 but with a different label,
  # rename the FB1 code to code_FB1 to preserve it.
  if (
    !is.null(fb_env$label_map_fb2) &&
      nrow(fb_env$label_map_fb2) > 0 &&
      !is.null(fb_env$label_map_fb1) &&
      nrow(fb_env$label_map_fb1) > 0
  ) {
    fb2_codes <- fb_env$label_map_fb2$code

    # Identify collisions
    collisions <- fb_env$label_map_fb1 |>
      dplyr::filter(code %in% fb2_codes)

    if (nrow(collisions) > 0) {
      # For each collision, check if labels are significantly different
      # (Simple check: if not identical)
      # Actually, we should just rename all collisions to be safe and let user decide,
      # or only if they are different.
      # Let's rename all collisions to _FB1 to allow access to the old variable.

      colliding_codes <- collisions$code

      # Rename in label map (keep labels unchanged, * will indicate FB1)
      fb_env$label_map_fb1 <- fb_env$label_map_fb1 |>
        dplyr::mutate(
          code = ifelse(code %in% colliding_codes, paste0(code, "_FB1"), code)
        )

      # Rename in microdata
      if (!is.null(fb_env$micro_fb1)) {
        # Check which colliding codes are actually in the microdata
        cols_to_rename <- intersect(names(fb_env$micro_fb1), colliding_codes)
        if (length(cols_to_rename) > 0) {
          # dplyr::rename takes new_name = old_name
          new_names <- paste0(cols_to_rename, "_FB1")
          rename_vec <- stats::setNames(cols_to_rename, new_names)
          fb_env$micro_fb1 <- dplyr::rename(fb_env$micro_fb1, !!!rename_vec)
        }
      }
    }
  }

  # Construct missing FB1 derived variables (DQ2_dv, DQ6_dv)
  if (!is.null(fb_env$micro_fb1)) {
    # Helper for FB1 construction
    get_val_fb1 <- function(col) {
      # Handle potential _FB1 suffix if renamed
      if (!col %in% names(fb_env$micro_fb1)) {
        col_fb1 <- paste0(col, "_FB1")
        if (col_fb1 %in% names(fb_env$micro_fb1)) col <- col_fb1
      }
      if (col %in% names(fb_env$micro_fb1)) {
        v <- suppressWarnings(as.numeric(fb_env$micro_fb1[[col]]))
        return(ifelse(is.na(v), 2, v)) # Treat NA as No
      }
      return(rep(2, nrow(fb_env$micro_fb1))) # Default to No if missing
    }

    # DQ2_dv: Any carrots (Q20 + Q21)
    if (!"DQ2_dv" %in% names(fb_env$micro_fb1)) {
      v20 <- get_val_fb1("Q20")
      v21 <- get_val_fb1("Q21")
      fb_env$micro_fb1$DQ2_dv <- ifelse(v20 == 1 | v21 == 1, 1, 2)
    }

    # DQ6_dv: Any berries (Q_58 + Q_59 + Q_60 + Q_61 + QN1_A)
    if (!"DQ6_dv" %in% names(fb_env$micro_fb1)) {
      v58 <- get_val_fb1("Q_58")
      v59 <- get_val_fb1("Q_59")
      v60 <- get_val_fb1("Q_60")
      v61 <- get_val_fb1("Q_61")
      vn1a <- get_val_fb1("QN1_A")
      fb_env$micro_fb1$DQ6_dv <- ifelse(
        v58 == 1 | v59 == 1 | v60 == 1 | v61 == 1 | vn1a == 1,
        1,
        2
      )
    }
  }

  fb_env$label_map <- dplyr::bind_rows(
    fb_env$label_map_legacy, # PRIORITY 1: Authoritative OMD labels
    fb_env$label_map_fb2, # PRIORITY 2: Open Canada FB2 labels
    fb_env$label_map_fb1 # PRIORITY 3: Open Canada FB1 labels
  ) |>
    dplyr::distinct(code, .keep_all = TRUE) # First occurrence wins (legacy takes priority)

  # =============================================================================
  # In LEGACY mode, remove duplicate labels from Open Canada maps
  # Legacy labels use renamed codes (e.g., "anycheese"), Open Canada uses Q-codes
  # Both refer to the same exposure, so we keep only the legacy version
  # =============================================================================
  if (fb_env$data_source == "Legacy" && nrow(fb_env$label_map_legacy) > 0) {
    legacy_labels <- unique(trimws(fb_env$label_map_legacy$label_en))
    # Remove Open Canada entries that have the same label as a legacy entry
    fb_env$label_map <- fb_env$label_map |>
      dplyr::filter(
        # Keep if: label is from legacy (code in legacy codes) OR label is NOT a duplicate
        code %in%
          fb_env$label_map_legacy$code |
          !trimws(label_en) %in% legacy_labels
      )
  }

  # =============================================================================
  # Apply label overrides for clarity
  # =============================================================================
  # "Carrots*" -> "Carrots (not mini)*" to distinguish from mini carrots
  carrot_idx <- which(fb_env$label_map$code == "carrot")
  if (length(carrot_idx) > 0) {
    fb_env$label_map$label_en[carrot_idx] <- "Carrots (not mini)*"
    fb_env$label_map$label_fr[carrot_idx] <- "Carottes (pas mini)*"
  }

  # For backward compatibility with existing code that expects single "label" column
  if (lang == "fr") {
    fb_env$label_map$label <- fb_env$label_map$label_fr
  } else {
    fb_env$label_map$label <- fb_env$label_map$label_en
  }

  # =============================================================================
  # Determine exposure columns present in microdata
  # =============================================================================
  if (!is.null(fb_env$micro)) {
    # Get codes from primary microdata (FB2)
    primary_codes <- fb_env$label_map$code[
      fb_env$label_map$code %in% names(fb_env$micro)
    ]

    # Get codes from supplementary microdata (FB1) if available
    supp_codes <- character()
    if (!is.null(fb_env$micro_fb1)) {
      supp_codes <- fb_env$label_map$code[
        fb_env$label_map$code %in% names(fb_env$micro_fb1)
      ]
    }

    # Combine unique codes
    fb_env$exposure_codes <- unique(c(primary_codes, supp_codes))

    # Filter label map to only exposures available in ANY microdata
    fb_env$label_map <- fb_env$label_map |>
      dplyr::filter(code %in% fb_env$exposure_codes)
  } else {
    fb_env$exposure_codes <- character()
  }

  # =============================================================================
  # Normalize column names for consistency
  # =============================================================================
  if (!is.null(fb_env$micro)) {
    # Normalize PT column (FB1 uses QINTRO3, FB2 uses PT)
    if ("QINTRO3" %in% names(fb_env$micro) && !"PT" %in% names(fb_env$micro)) {
      fb_env$micro$PT <- fb_env$micro$QINTRO3
    }

    # Normalize Month column (various names)
    if (
      "month_dv" %in% names(fb_env$micro) && !"Month" %in% names(fb_env$micro)
    ) {
      fb_env$micro$Month <- fb_env$micro$month_dv
    }

    # Normalize Age_group column (FB1 uses age_grp_dv, FB2 uses Age_grp_dv)
    if (
      "age_grp_dv" %in%
        names(fb_env$micro) &&
        !"Age_group" %in% names(fb_env$micro)
    ) {
      fb_env$micro$Age_group <- fb_env$micro$age_grp_dv
    }
    if (
      "Age_grp_dv" %in%
        names(fb_env$micro) &&
        !"Age_group" %in% names(fb_env$micro)
    ) {
      fb_env$micro$Age_group <- fb_env$micro$Age_grp_dv
    }

    # Keep only the useful columns for filtering
    keep_cols <- unique(c(
      "PT",
      "Month",
      "Age_group",
      "Gender",
      "age",
      "sex",
      "weight",
      "fb_source",
      fb_env$exposure_codes
    ))

    # Ensure berries_dv is kept if it exists in FB2
    if ("berries_dv" %in% names(fb_env$micro)) {
      keep_cols <- c(keep_cols, "berries_dv")
      if (!"berries_dv" %in% fb_env$exposure_codes) {
        fb_env$exposure_codes <- c(fb_env$exposure_codes, "berries_dv")
      }

      # Add to label map if missing
      if (!"berries_dv" %in% fb_env$label_map$code) {
        new_row <- tibble::tibble(
          code = "berries_dv",
          label_en = "Any berries",
          label_fr = "Toutes les baies", # Approximate translation
          label = if (lang == "fr") "Toutes les baies" else "Any berries"
        )
        fb_env$label_map <- dplyr::bind_rows(fb_env$label_map, new_row)
      }
    }

    fb_env$micro <- fb_env$micro[,
      intersect(keep_cols, names(fb_env$micro)),
      drop = FALSE
    ]
  }

  # =============================================================================
  # Construct AgeBand for filtering
  # =============================================================================
  if (!is.null(fb_env$micro) && "Age_group" %in% names(fb_env$micro)) {
    ag <- suppressWarnings(as.integer(fb_env$micro$Age_group))
    # Open Canada age groups: 1-10 map to our 4 bands
    # 1-2 (0-9), 3-4 (10-19), 5-8 (20-64), 9-10 (65+)
    age_map <- c(
      `1` = "0-9",
      `2` = "0-9",
      `3` = "10-19",
      `4` = "10-19",
      `5` = "20-64",
      `6` = "20-64",
      `7` = "20-64",
      `8` = "20-64",
      `9` = "65+",
      `10` = "65+"
    )
    fb_env$micro$AgeBand <- unname(age_map[as.character(ag)])
  } else if (!is.null(fb_env$micro) && "age" %in% names(fb_env$micro)) {
    a <- suppressWarnings(as.numeric(fb_env$micro$age))
    fb_env$micro$AgeBand <- cut(
      a,
      breaks = c(-Inf, 9, 19, 64, Inf),
      labels = c("0-9", "10-19", "20-64", "65+"),
      right = TRUE
    ) |>
      as.character()
  }

  fb_env$pt_map <- fb_pt_map()
  fb_env$initialised <- TRUE
  invisible(TRUE)
}

# Update language without re-initializing entire backend
# More efficient than calling fb_init() when only language changed
fb_update_language <- function(lang = "en") {
  # Ensure backend is initialized
  if (!isTRUE(fb_env$initialised)) {
    fb_init(lang = lang)
    return(invisible(TRUE))
  }

  # Update the active label column based on language
  if (!is.null(fb_env$label_map) && nrow(fb_env$label_map) > 0) {
    if (lang == "fr" && "label_fr" %in% names(fb_env$label_map)) {
      fb_env$label_map$label <- fb_env$label_map$label_fr
    } else {
      fb_env$label_map$label <- fb_env$label_map$label_en
    }
  }

  invisible(TRUE)
}

# Normalize English labels for display (e.g., remove accents)
fb_normalize_en_label <- function(labels) {
  # Handle double-encoded UTF-8 mojibake (e.g., "PÃ¢tÃ©" = UTF-8 decoded as Latin-1)
  labels <- gsub("PÃ¢tÃ©/meat spread", "Pate/meat spread", labels, fixed = TRUE)
  labels <- gsub("PÃ¢tÃ©", "Pate", labels, fixed = TRUE)
  # Also handle correctly-encoded accented forms
  labels <- gsub("Pâté/meat spread", "Pate/meat spread", labels, fixed = TRUE)
  labels <- gsub("Pâté", "Pate", labels, fixed = TRUE)
  labels
}

# Normalize labels for matching against exclusion lists
fb_normalize_label_for_match <- function(labels) {
  labels |>
    (function(x) {
      x <- gsub("\\*$", "", x, perl = TRUE)
      x <- trimws(x)
      # Handle both mojibake and correct encodings
      x <- gsub("PÃ¢tÃ©", "Pate", x, fixed = TRUE)
      x <- gsub("Pâté", "Pate", x, fixed = TRUE)
      x
    })()
}

# Public app exclusion list (label-based)
fb_public_exposure_exclusions <- function() {
  if (!is.null(fb_env$public_exclusions)) {
    return(fb_env$public_exclusions)
  }

  path <- fb_get_base_path(file.path(
    "config",
    "public_exposure_exclusions.csv"
  ))
  if (!file.exists(path)) {
    fb_env$public_exclusions <- character()
    return(fb_env$public_exclusions)
  }

  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) {
    NULL
  })
  if (is.null(df) || !"Variable" %in% names(df)) {
    fb_env$public_exclusions <- character()
    return(fb_env$public_exclusions)
  }

  fb_env$public_exclusions <- df$Variable[
    !is.na(df$Variable) & nzchar(df$Variable)
  ] |>
    trimws()
  fb_env$public_exclusions
}

# Public exclusion codes (use codes to avoid ambiguous label matching)
fb_public_exposure_exclusion_codes <- function() {
  # Explicitly drop FB1 pate: Q89_D and any renamed collision variant
  c("Q89_D", "Q89_D_FB1")
}

fb_exposure_choices <- function(lang = "en", apply_public_exclusions = FALSE) {
  fb_init(lang = lang)
  # If microdata + labels available, return label->code; else fall back to CSV Exposure labels
  if (!is.null(fb_env$micro) && nrow(fb_env$label_map)) {
    lm <- fb_env$label_map

    # Select label column based on language
    if (lang == "fr" && "label_fr" %in% names(lm)) {
      label_col <- lm$label_fr
    } else if ("label_en" %in% names(lm)) {
      label_col <- lm$label_en
    } else {
      label_col <- lm$label
    }

    # Filter out unwanted variables (hunting/game questions and non-food FB1 vars)
    # Exclude:
    # - Hunting/game: Q60-Q66
    # - FB1 non-food: BQ*, AQ*, QINTRO*, uniqueid, weight
    # - FB1 food safety: Q141-Q146
    # - FB1 general Q140_FS (but keep Q140_FSA etc. as they are food)

    # - FB1 non-food: BQ*, AQ*, QINTRO*, uniqueid, weight
    # - FB1 food safety: Q141-Q146
    # - FB1 general Q140_FS (but keep Q140_FSA etc. as they are food)
    # - Q21_FB1 (Mini carrots) is redundant with FB2 Q10 (Mini/baby carrots)

    unwanted_pattern <- "^(Q6[0-6]|BQ|AQ|QINTRO|uniqueid|weight|Q14[1-6]|Q140_FS$|Q21_FB1$)"
    keep_mask <- !grepl(unwanted_pattern, lm$code)

    lm <- lm[keep_mask, ]
    label_col <- label_col[keep_mask]

    # Deduplicate by label: If an FB1 exposure has the same label
    # as an FB2 exposure, remove the FB1 version (prefer FB2).

    if (!is.null(fb_env$micro) && !is.null(fb_env$micro_fb1)) {
      fb2_codes <- names(fb_env$micro)

      # Get labels for FB2 vars (before adding any asterisks)
      fb2_mask <- lm$code %in% fb2_codes
      fb2_labels <- unique(label_col[fb2_mask])

      # Identify FB1 vars that are NOT in FB2 (candidates for *)
      fb1_only_mask <- !lm$code %in% fb2_codes

      # Among these FB1-only vars, check if their label exists in FB2_labels
      # If so, it's a "cross-version duplicate" (same name, different code) -> Hide it
      duplicate_label_mask <- fb1_only_mask & (label_col %in% fb2_labels)

      # Remove these duplicates from the list
      if (any(duplicate_label_mask)) {
        lm <- lm[!duplicate_label_mask, ]
        label_col <- label_col[!duplicate_label_mask]
        # Re-calculate mask after dropping rows
        fb1_only_mask <- !lm$code %in% fb2_codes
      }

      # Now add * to the remaining FB1-only vars
      if (any(fb1_only_mask)) {
        label_col[fb1_only_mask] <- paste0(label_col[fb1_only_mask], "*")
      }
    }

    # Normalize English labels for display
    if (lang != "fr") {
      label_col <- fb_normalize_en_label(label_col)
    }

    # Apply public app exclusions (label based)
    if (isTRUE(apply_public_exclusions)) {
      # First drop by code (safer for dup labels across FB1/FB2)
      code_exclusions <- fb_public_exposure_exclusion_codes()
      if (length(code_exclusions)) {
        keep_mask <- !lm$code %in% code_exclusions
        lm <- lm[keep_mask, ]
        label_col <- label_col[keep_mask]
      }

      # Then drop by label (for legacy exclusions). Allow starred entries except pate/meat spread* which is handled by code.
      exclusions <- fb_public_exposure_exclusions()
      exclusions <- setdiff(exclusions, "Pate/meat spread*")
      if (length(exclusions)) {
        norm_labels <- fb_normalize_label_for_match(label_col)
        norm_exclusions <- fb_normalize_label_for_match(exclusions)
        keep_mask <- !norm_labels %in% norm_exclusions
        lm <- lm[keep_mask, ]
        label_col <- label_col[keep_mask]
      }
    }

    # Final deduplication: if same label appears for multiple codes, keep the first one.
    # Since FB2 codes are listed before FB1 codes in the label_map, this prefers FB2.
    dup_label_mask <- duplicated(label_col)
    if (any(dup_label_mask)) {
      lm <- lm[!dup_label_mask, ]
      label_col <- label_col[!dup_label_mask]
    }

    return(stats::setNames(lm$code, label_col))
  }
  # Fallback: read from legacy CSV
  if (file.exists("data/foodbook_data.csv")) {
    df <- tryCatch(
      read.csv("data/foodbook_data.csv", stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (
      !is.null(df) &&
        all(c("Exposure", "Province.Territory", "Proportion") %in% names(df))
    ) {
      exps <- sort(unique(fb_normalize_en_label(df$Exposure)))
      return(stats::setNames(exps, exps))
    }
  }
  character()
}

# Get bilingual exposure labels as data frame
fb_exposure_labels_bilingual <- function() {
  fb_init()
  if (nrow(fb_env$label_map)) {
    return(fb_env$label_map[, c("code", "label_en", "label_fr")])
  }
  tibble::tibble(
    code = character(),
    label_en = character(),
    label_fr = character()
  )
}

# Get exposure label by code and language
fb_exposure_label <- function(code, lang = "en") {
  fb_init(lang = lang)
  if (nrow(fb_env$label_map) == 0) {
    return(code)
  }

  row <- fb_env$label_map[fb_env$label_map$code == code, ]
  if (nrow(row) == 0) {
    return(code)
  }

  if (lang == "fr" && "label_fr" %in% names(row)) {
    return(row$label_fr[1])
  } else if ("label_en" %in% names(row)) {
    return(fb_normalize_en_label(row$label_en[1]))
  } else {
    return(fb_normalize_en_label(row$label[1]))
  }
}

# Get ALL exposure labels including legacy CEDARS codes (not filtered by microdata columns)
# This is needed for CEDARS uploads which use legacy exposure codes
fb_exposure_choices_all <- function(lang = "en") {
  fb_init(lang = lang)

  # Get the full combined label map (FB2 + FB1 + legacy) without filtering by microdata
  lm <- dplyr::bind_rows(
    if (!is.null(fb_env$label_map_fb2)) {
      fb_env$label_map_fb2
    } else {
      tibble::tibble()
    },
    if (!is.null(fb_env$label_map_fb1)) {
      fb_env$label_map_fb1
    } else {
      tibble::tibble()
    },
    if (!is.null(fb_env$label_map_legacy)) {
      fb_env$label_map_legacy
    } else {
      tibble::tibble()
    }
  ) |>
    dplyr::distinct(code, .keep_all = TRUE)

  if (nrow(lm) == 0) {
    return(stats::setNames(character(), character()))
  }

  # Select label column based on language
  if (lang == "fr" && "label_fr" %in% names(lm)) {
    label_col <- lm$label_fr
  } else if ("label_en" %in% names(lm)) {
    label_col <- lm$label_en
  } else if ("label" %in% names(lm)) {
    label_col <- lm$label
  } else {
    return(stats::setNames(character(), character()))
  }

  # Handle duplicate labels
  duplicated_labels <- duplicated(label_col)
  label_col[duplicated_labels] <- paste0(
    label_col[duplicated_labels],
    " (",
    lm$code[duplicated_labels],
    ")"
  )

  # Filter out unwanted variables (hunting/game questions)
  exclude_codes <- c("Q60", "Q61", "Q62", "Q63", "Q64", "Q65", "Q66")
  keep_mask <- !lm$code %in% exclude_codes

  return(stats::setNames(lm$code[keep_mask], label_col[keep_mask]))
}

fb_age_groups <- function() {
  fb_init()
  # If microdata present, offer the fixed mapping; else return empty so UI shows "All" only
  if (!is.null(fb_env$micro)) c("0-9", "10-19", "20-64", "65+") else character()
}

fb_months <- function() {
  fb_init()
  if (is.null(fb_env$micro) || !"Month" %in% names(fb_env$micro)) {
    return(integer())
  }
  m <- sort(unique(na.omit(as.integer(fb_env$micro$Month))))
  m <- m[m >= 1 & m <= 12]
  stats::setNames(as.character(m), month.name[m])
}

fb_pt_names <- function(lang = "en") {
  if (lang == "fr") {
    return(c(
      "Colombie-Britannique",
      "Alberta",
      "Saskatchewan",
      "Manitoba",
      "Ontario",
      "Québec",
      "Nouveau-Brunswick",
      "Nouvelle-Écosse",
      "Île-du-Prince-Édouard",
      "Terre-Neuve-et-Labrador",
      "Yukon",
      "Territoires du Nord-Ouest",
      "Nunavut"
    ))
  } else {
    return(names(fb_pt_map()))
  }
}

# Get bilingual PT names as named vector (EN name = FR name)
fb_pt_names_bilingual <- function() {
  en_names <- fb_pt_names("en")
  fr_names <- fb_pt_names("fr")
  stats::setNames(fr_names, en_names)
}

# Get month names by language
fb_month_names <- function(lang = "en") {
  if (lang == "fr") {
    return(c(
      "Janvier",
      "Février",
      "Mars",
      "Avril",
      "Mai",
      "Juin",
      "Juillet",
      "Août",
      "Septembre",
      "Octobre",
      "Novembre",
      "Décembre"
    ))
  } else {
    return(month.name)
  }
}

# Internal: Filter a single dataset
fb_filter_dataset <- function(
  d,
  pt_names = NULL,
  months = NULL,
  age_groups = NULL
) {
  if (is.null(d)) {
    return(NULL)
  }

  # PT filtering: accept English, French, or abbreviations
  if (
    !is.null(pt_names) &&
      length(pt_names) &&
      !("Canada" %in% pt_names) &&
      "PT" %in% names(d)
  ) {
    codes <- fb_normalize_pt_names(pt_names)
    if (length(codes) > 0) {
      d <- d |> dplyr::filter(PT %in% codes)
    }
  }

  if (!is.null(months) && length(months) && "Month" %in% names(d)) {
    d <- d |> dplyr::filter(Month %in% months)
  }
  if (!is.null(age_groups) && length(age_groups) && "AgeBand" %in% names(d)) {
    d <- d |> dplyr::filter(AgeBand %in% age_groups)
  }
  d
}

# Internal: filter microdata given reference selections
fb_filter_micro <- function(pt_names = NULL, months = NULL, age_groups = NULL) {
  fb_init()

  # Filter primary dataset (FB2)
  d_main <- fb_filter_dataset(fb_env$micro, pt_names, months, age_groups)

  # Filter supplementary dataset (FB1)
  d_supp <- fb_filter_dataset(fb_env$micro_fb1, pt_names, months, age_groups)

  # Combine if both exist
  if (!is.null(d_main) && !is.null(d_supp)) {
    # Bind rows (keeping all columns, filling missing with NA)
    d <- dplyr::bind_rows(d_main, d_supp)
  } else if (!is.null(d_main)) {
    d <- d_main
  } else if (!is.null(d_supp)) {
    d <- d_supp
  } else {
    d <- data.frame()
  }

  d
}

# Compute a weighted percentage (0-100) for a single exposure code
fb_weighted_percent <- function(code, d) {
  if (!code %in% names(d)) {
    return(NA_real_)
  }
  x <- suppressWarnings(as.numeric(d[[code]]))
  w <- suppressWarnings(as.numeric(d$weight))
  ok <- !is.na(x) & (x %in% c(0, 1, 2))
  # Many FB dv vars are 1 = yes, 2 = no; treat 0 as missing/not asked
  yy <- ok & (x == 1)
  denom <- sum(w[ok], na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  100 * sum(w[yy], na.rm = TRUE) / denom
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

fb_reference_percents_csv <- function(codes, pt_names = NULL) {
  # NOTE: This fallback uses pre-computed CSV data and calculates simple (unweighted)
  # average when multiple PTs are selected. Results may differ from microdata calculations.
  df <- tryCatch(
    read.csv("data/foodbook_data.csv", stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (
    is.null(df) ||
      !all(c("Exposure", "Province.Territory", "Proportion") %in% names(df))
  ) {
    return(stats::setNames(rep(NA_real_, length(codes)), codes))
  }
  # If Canada is selected or no PTs, use Canada rows
  if (is.null(pt_names) || length(pt_names) == 0 || any(pt_names == "Canada")) {
    res <- vapply(
      codes,
      function(x) {
        v <- df$Proportion[df$Exposure == x & df$Province.Territory == "Canada"]
        v <- suppressWarnings(as.numeric(v[1]))
        ifelse(length(v) == 0, NA_real_, v)
      },
      numeric(1)
    )
    return(res)
  }
  # Otherwise, average across selected PTs (simple mean; survey weights unavailable in CSV)
  ab <- fb_pt_abbrev_map()
  sel_ab <- unname(ab[pt_names])
  res <- vapply(
    codes,
    function(x) {
      v <- df$Proportion[df$Exposure == x & df$Province.Territory %in% sel_ab]
      v <- suppressWarnings(as.numeric(v))
      if (!length(v)) {
        return(NA_real_)
      }
      mean(v, na.rm = TRUE)
    },
    numeric(1)
  )
  res
}

fb_reference_percents <- function(
  codes,
  pt_names = NULL,
  months = NULL,
  age_groups = NULL
) {
  if (!is.null(fb_env$micro)) {
    cedars_map <- fb_cedars_to_foodbook_map()

    fb_codes <- vapply(
      codes,
      function(code) {
        if (code %in% names(cedars_map)) {
          cedars_map[[code]]
        } else {
          code
        }
      },
      character(1),
      USE.NAMES = FALSE
    )

    # Filter datasets separately
    d_main <- fb_filter_dataset(fb_env$micro, pt_names, months, age_groups)
    d_supp <- fb_filter_dataset(fb_env$micro_fb1, pt_names, months, age_groups)

    # Check if we're using legacy combined data (FB1+FB2 already merged with renames)
    is_legacy_mode <- fb_env$data_source == "Legacy"

    # Helper to check if code is FB1-only (not in FB2 microdata)
    # Only relevant when NOT in legacy mode
    fb2_cols <- if (!is.null(fb_env$micro)) names(fb_env$micro) else character()
    fb1_codes <- if (!is.null(fb_env$label_map_fb1) && !is_legacy_mode) {
      fb_env$label_map_fb1$code
    } else {
      character()
    }
    is_fb1_only <- function(fb_col) {
      # In legacy mode, all data is in one dataset - no FB1-only logic needed
      if (is_legacy_mode) {
        return(FALSE)
      }

      # Check if column exists only in FB1 (not in FB2)
      in_fb2 <- fb_col %in% fb2_cols || paste0(fb_col, "_dv") %in% fb2_cols
      in_fb1 <- !is.null(d_supp) &&
        (fb_col %in%
          names(d_supp) ||
          paste0(fb_col, "_dv") %in% names(d_supp) ||
          paste0(fb_col, "_FB1") %in% names(d_supp))
      return(!in_fb2 && in_fb1)
    }

    # Calculate reference percentages for each code
    results <- vapply(
      fb_codes,
      function(fb_col) {
        # For FB1 exposures (Open Canada mode only), prefer CSV values (official published values)
        # Skip this logic in legacy mode - calculate directly from microdata
        if (!is_legacy_mode && (is_fb1_only(fb_col) || fb_col %in% fb1_codes)) {
          # Map code -> English label (CSV uses English exposure names)
          label_row <- fb_env$label_map[
            fb_env$label_map$code == fb_col,
            ,
            drop = FALSE
          ]
          label_en <- if (nrow(label_row) && "label_en" %in% names(label_row)) {
            label_row$label_en[[1]]
          } else {
            fb_col
          }

          # Append * for FB1-only exposures to align with CSV naming
          if (is_fb1_only(fb_col)) {
            label_en <- paste0(label_en, "*")
          }

          # Try CSV lookup with star, then without as a fallback
          csv_result <- fb_reference_percents_csv(label_en, pt_names)
          val <- csv_result[[1]]
          if (is.na(val) && is_fb1_only(fb_col)) {
            csv_result <- fb_reference_percents_csv(
              label_row$label_en[[1]],
              pt_names
            )
            val <- csv_result[[1]]
          }
          if (!is.na(val)) {
            return(val)
          }
        }

        # 1. Try Primary Dataset (FB2 or Legacy combined)
        if (!is.null(d_main)) {
          if (fb_col %in% names(d_main)) {
            return(fb_weighted_percent(fb_col, d_main))
          }
          # Try with _dv suffix
          fb_col_dv <- paste0(fb_col, "_dv")
          if (fb_col_dv %in% names(d_main)) {
            return(fb_weighted_percent(fb_col_dv, d_main))
          }
        }

        # 2. Fallback to Supplementary Dataset (FB1)
        if (!is.null(d_supp)) {
          # Try direct match
          if (fb_col %in% names(d_supp)) {
            return(fb_weighted_percent(fb_col, d_supp))
          }
          # Try with _dv suffix
          fb_col_dv <- paste0(fb_col, "_dv")
          if (fb_col_dv %in% names(d_supp)) {
            return(fb_weighted_percent(fb_col_dv, d_supp))
          }
          # Try with _FB1 suffix (for collision-renamed FB1 columns)
          fb_col_fb1 <- paste0(fb_col, "_FB1")
          if (fb_col_fb1 %in% names(d_supp)) {
            return(fb_weighted_percent(fb_col_fb1, d_supp))
          }
        }

        NA_real_
      },
      numeric(1)
    )

    names(results) <- codes
    return(results)
  }
  fb_reference_percents_csv(codes, pt_names)
}

fb_is_available <- function() {
  fb_init()
  !is.null(fb_env$micro)
}
