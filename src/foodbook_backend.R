# Backend helpers to use OMD Foodbook microdata and labels
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(haven)
  library(data.table)
})

fb_env <- new.env(parent = emptyenv())

# =============================================================================
# New Open Canada Data Loading Functions (FB1 + FB2)
# =============================================================================

# Load Foodbook 1 microdata from Open Canada (3 CSV files that need joining)
fb_load_fb1_csv <- function(lang = "en") {
  base_dir <- "data/open-canada/foodbook-1"

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
  if (!file.exists(part1_file) || !file.exists(part2_file) || !file.exists(part3_file)) {
    return(NULL)
  }

  # Load all three parts using data.table for speed
  # Use fill=TRUE to handle rows with varying field counts
  part1 <- tryCatch(
    data.table::fread(part1_file, data.table = FALSE, fill = TRUE, showProgress = FALSE),
    error = function(e) NULL
  )
  part2 <- tryCatch(
    data.table::fread(part2_file, data.table = FALSE, fill = TRUE, showProgress = FALSE),
    error = function(e) NULL
  )
  part3 <- tryCatch(
    data.table::fread(part3_file, data.table = FALSE, fill = TRUE, showProgress = FALSE),
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
  if (lang != "fr") return(df)

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
  base_dir <- "data/open-canada/foodbook-2"

  # File patterns based on language
  if (lang == "fr") {
    fb2_file <- file.path(base_dir, "atlas-alimentaire-2.0-fichier-de-microdonnees-a-grande-diffusion-2023.csv")
  } else {
    fb2_file <- file.path(base_dir, "foodbook-2.0-public-use-microdata-file-2023.csv")
  }

  # Check if file exists
  if (!file.exists(fb2_file)) {
    return(NULL)
  }

  # Load using data.table for speed
  df <- tryCatch(
    data.table::fread(fb2_file, data.table = FALSE, fill = TRUE, showProgress = FALSE),
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
        label_fr = as.character(label)  # Explicit character conversion for type safety
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
# Returns tibble with old (Foodbook column) and new (CEDARS code) names
fb_parse_renames <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(tibble::tibble(old = character(), new = character()))
  m <- stringr::str_match(lines, "^\\s*rename\\s+([^\\s]+)\\s+([^\\s]+)")
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  tibble::tibble(old = m[, 2], new = m[, 3])
}

# Get reverse mapping: CEDARS code -> Foodbook column name
# This allows us to calculate references for CEDARS codes
fb_cedars_to_foodbook_map <- function() {
  fb_init()

  # Check if renames are loaded
  if (is.null(fb_env$cedars_to_fb_map)) {
    # Try to load from foodbook data.do
    rename_path <- "upgrade-context/foodbook data.do"
    if (file.exists(rename_path)) {
      renames <- fb_parse_renames(rename_path)
      # Create reverse map: new (CEDARS code) -> old (Foodbook column)
      fb_env$cedars_to_fb_map <- stats::setNames(renames$old, renames$new)
    } else {
      fb_env$cedars_to_fb_map <- stats::setNames(character(), character())
    }
  }

  fb_env$cedars_to_fb_map
}

# Parse exposure code -> human label mapping from the variable labeling .do file
fb_parse_label_map <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(tibble::tibble(code = character(), label = character()))

  # Try Open Canada format first: label var CODE "Label"
  m_open <- stringr::str_match(lines, '^\\s*label\\s+var\\s+([^\\s]+)\\s+"([^"]+)"')
  m_open <- m_open[!is.na(m_open[, 1]), , drop = FALSE]

  if (nrow(m_open) > 0) {
    # Open Canada format found
    # Filter to keep only exposure variables (Q-prefixed and special exposure codes)
    out <- tibble::tibble(code = m_open[, 2], label = stringr::str_squish(m_open[, 3])) |>
      dplyr::filter(
        label != "",
        !grepl("^\\*", code),  # Skip commented lines
        # Only keep exposure variables: Q-prefixed or specific exposure patterns
        grepl("^Q[0-9]", code) | code %in% c("organic_dv", "freshherbs_dv")
      ) |>
      dplyr::distinct(code, .keep_all = TRUE)
    return(out)
  }

  # Fall back to legacy format: gen label = "..." if exposure == "code"
  m <- stringr::str_match(lines, '^\\s*(?:gen|replace)\\s+label\\s*=\\s*"([^"]+)"\\s+if\\s+exposure\\s*==\\s*"([^"]+)"')
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  # Some labels contain stray control chars; trim and normalise spaces
  out <- tibble::tibble(label = stringr::str_squish(m[, 2]), code = m[, 3]) |>
    dplyr::filter(label != "") |>
    dplyr::distinct(code, .keep_all = TRUE)
  out
}

# Apply a rename mapping to a data.frame (only where cols exist)
fb_apply_renames <- function(df, renames) {
  if (!nrow(renames)) return(df)
  present <- renames$old %in% names(df)
  if (!any(present)) return(df)
  map <- renames$new[present]
  names(map) <- renames$old[present]
  dplyr::rename(df, !!!rlang::set_names(names(map), map))
}

# Discover weight column and normalise to `weight`
fb_normalise_weight <- function(df) {
  w <- NULL
  # Try Open Canada FB2 weight column first
  if ("Proj_weight_non_traveller_dv" %in% names(df)) w <- df$Proj_weight_non_traveller_dv
  # Try Open Canada FB1 weight column
  if (is.null(w) && "EXPWEIGHT_CMA2_dv" %in% names(df)) w <- df$EXPWEIGHT_CMA2_dv
  # Try legacy OMD weight columns
  if (is.null(w) && "EXPWEIGHT_CMA2" %in% names(df)) w <- df$EXPWEIGHT_CMA2
  if (is.null(w) && "proj_weight_non_traveller" %in% names(df)) w <- df$proj_weight_non_traveller
  # Generic weight column
  if (is.null(w) && "weight" %in% names(df)) w <- df$weight
  # Default to 1 if no weight found
  if (is.null(w)) {
    df$weight <- 1
  } else {
    df$weight <- suppressWarnings(as.numeric(w))
  }
  df
}

# Load, rename, and combine Foodbook microdata
fb_load_microdata <- function(
  do_renames_path = "upgrade-context/foodbook data.do",
  dta_paths = c("upgrade-context/foodbook.dta", "upgrade-context/foodbook2v2.dta")
) {
  ren <- fb_parse_renames(do_renames_path)
  dfs <- list()
  for (p in dta_paths) {
    if (!file.exists(p)) next
    df <- tryCatch(haven::read_dta(p), error = function(e) NULL)
    if (is.null(df)) next
    df <- as.data.frame(df)
    df <- fb_apply_renames(df, ren)
    df <- fb_normalise_weight(df)
    dfs[[length(dfs) + 1]] <- df
  }
  if (!length(dfs)) return(NULL)
  suppressWarnings(dplyr::bind_rows(dfs))
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

# Initialise and cache everything we need
fb_init <- function(lang = "en") {
  if (!is.null(fb_env$initialised) && isTRUE(fb_env$initialised)) return(invisible(TRUE))

  # =============================================================================
  # PRIORITY 1: Try Open Canada FB2 (newest, largest, 21K respondents)
  # =============================================================================
  fb2_data <- fb_load_fb2_csv(lang = lang)
  fb1_data <- NULL

  if (!is.null(fb2_data)) {
    message("Loaded Foodbook 2 microdata from Open Canada (", nrow(fb2_data), " respondents)")
  } else {
    message("Foodbook 2 data not found, trying Foodbook 1...")
  }

  # =============================================================================
  # PRIORITY 2: Try Open Canada FB1 (fallback for FB1-only exposures)
  # =============================================================================
  fb1_data <- fb_load_fb1_csv(lang = lang)

  if (!is.null(fb1_data)) {
    message("Loaded Foodbook 1 microdata from Open Canada (", nrow(fb1_data), " respondents)")
  }

  # =============================================================================
  # PRIORITY 3: Try legacy OMD microdata from upgrade-context/ (for internal use)
  # =============================================================================
  legacy_data <- NULL
  if (is.null(fb2_data) && is.null(fb1_data)) {
    message("Open Canada data not found, trying legacy microdata...")
    legacy_data <- fb_load_microdata()
    if (!is.null(legacy_data)) {
      message("Loaded legacy microdata from upgrade-context/ (", nrow(legacy_data), " respondents)")
      legacy_data$fb_source <- "Legacy"
    }
  }

  # =============================================================================
  # Combine microdata: FB2 primary, FB1 supplementary (for unique exposures)
  # =============================================================================
  if (!is.null(fb2_data)) {
    fb_env$micro <- fb2_data
    fb_env$data_source <- "FB2"
  } else if (!is.null(fb1_data)) {
    fb_env$micro <- fb1_data
    fb_env$data_source <- "FB1"
  } else if (!is.null(legacy_data)) {
    fb_env$micro <- legacy_data
    fb_env$data_source <- "Legacy"
  } else {
    fb_env$micro <- NULL
    fb_env$data_source <- NULL
  }

  # Store supplementary FB1 data if FB2 is primary
  if (!is.null(fb2_data) && !is.null(fb1_data)) {
    fb_env$micro_fb1 <- fb1_data
  } else {
    fb_env$micro_fb1 <- NULL
  }

  # =============================================================================
  # Load bilingual label maps from Open Canada Stata label files
  # =============================================================================
  fb2_label_en <- "data/open-canada/foodbook-2/foodbook-2.0-stata-label-code.txt"
  fb2_label_fr <- "data/open-canada/foodbook-2/latlas-alimentaire-2.0-stata-code-des-etiquettes.txt"
  fb1_label_en <- "data/open-canada/foodbook-1/foodbook-stata-label-code-des-etiquettes-en.do"
  fb1_label_fr <- "data/open-canada/foodbook-1/foodbook-stata-label-code-des-etiquettes-fr.do"
  legacy_label <- "upgrade-context/foodbook variable labeling.do"

  # Load FB2 labels (bilingual)
  if (file.exists(fb2_label_en)) {
    fb_env$label_map_fb2 <- fb_parse_label_map_bilingual(fb2_label_en, fb2_label_fr)
  } else {
    fb_env$label_map_fb2 <- tibble::tibble(code = character(), label_en = character(), label_fr = character())
  }

  # Load FB1 labels (bilingual)
  if (file.exists(fb1_label_en)) {
    fb_env$label_map_fb1 <- fb_parse_label_map_bilingual(fb1_label_en, fb1_label_fr)
  } else {
    fb_env$label_map_fb1 <- tibble::tibble(code = character(), label_en = character(), label_fr = character())
  }

  # Load legacy labels (English only)
  if (file.exists(legacy_label)) {
    fb_env$label_map_legacy <- fb_parse_label_map(legacy_label) |>
      dplyr::mutate(
        label_en = as.character(label),
        label_fr = as.character(label)  # Fallback to English, explicit character type
      )
  } else {
    fb_env$label_map_legacy <- tibble::tibble(code = character(), label_en = character(), label_fr = character())
  }

  # =============================================================================
  # Create unified label map: FB2 priority, FB1 supplementary, legacy fallback
  # =============================================================================
  fb_env$label_map <- dplyr::bind_rows(
    fb_env$label_map_fb2,
    fb_env$label_map_fb1,
    fb_env$label_map_legacy
  ) |>
    dplyr::distinct(code, .keep_all = TRUE)  # Remove duplicates, keeping first (FB2 priority)

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
    fb_env$exposure_codes <- fb_env$label_map$code[fb_env$label_map$code %in% names(fb_env$micro)]
    # Filter label map to only exposures available in microdata
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
    if ("month_dv" %in% names(fb_env$micro) && !"Month" %in% names(fb_env$micro)) {
      fb_env$micro$Month <- fb_env$micro$month_dv
    }

    # Normalize Age_group column (FB1 uses age_grp_dv, FB2 uses Age_grp_dv)
    if ("age_grp_dv" %in% names(fb_env$micro) && !"Age_group" %in% names(fb_env$micro)) {
      fb_env$micro$Age_group <- fb_env$micro$age_grp_dv
    }
    if ("Age_grp_dv" %in% names(fb_env$micro) && !"Age_group" %in% names(fb_env$micro)) {
      fb_env$micro$Age_group <- fb_env$micro$Age_grp_dv
    }

    # Keep only the useful columns for filtering
    keep_cols <- unique(c(
      "PT", "Month", "Age_group", "Gender", "age", "sex",
      "weight", "fb_source", fb_env$exposure_codes
    ))
    fb_env$micro <- fb_env$micro[, intersect(keep_cols, names(fb_env$micro)), drop = FALSE]
  }

  # =============================================================================
  # Construct AgeBand for filtering
  # =============================================================================
  if (!is.null(fb_env$micro) && "Age_group" %in% names(fb_env$micro)) {
    ag <- suppressWarnings(as.integer(fb_env$micro$Age_group))
    # Open Canada age groups: 1-10 map to our 4 bands
    # 1-2 (0-9), 3-4 (10-19), 5-8 (20-64), 9-10 (65+)
    age_map <- c(
      `1` = "0-9", `2` = "0-9",
      `3` = "10-19", `4` = "10-19",
      `5` = "20-64", `6` = "20-64", `7` = "20-64", `8` = "20-64",
      `9` = "65+", `10` = "65+"
    )
    fb_env$micro$AgeBand <- unname(age_map[as.character(ag)])
  } else if (!is.null(fb_env$micro) && "age" %in% names(fb_env$micro)) {
    a <- suppressWarnings(as.numeric(fb_env$micro$age))
    fb_env$micro$AgeBand <- cut(
      a,
      breaks = c(-Inf, 9, 19, 64, Inf),
      labels = c("0-9", "10-19", "20-64", "65+"),
      right = TRUE
    ) |> as.character()
  }

  fb_env$pt_map <- fb_pt_map()
  fb_env$initialised <- TRUE
  invisible(TRUE)
}

fb_exposure_choices <- function(lang = "en") {
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

    # Handle duplicate labels
    duplicated_labels <- duplicated(label_col)
    label_col[duplicated_labels] <- paste0(label_col[duplicated_labels], " (", lm$code[duplicated_labels], ")")

    return(stats::setNames(lm$code, label_col))
  }
  # Fallback: read from legacy CSV
  if (file.exists("data/foodbook_data.csv")) {
    df <- tryCatch(read.csv("data/foodbook_data.csv", stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(df) && all(c("Exposure", "Province.Territory", "Proportion") %in% names(df))) {
      exps <- sort(unique(df$Exposure))
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
  tibble::tibble(code = character(), label_en = character(), label_fr = character())
}

# Get exposure label by code and language
fb_exposure_label <- function(code, lang = "en") {
  fb_init(lang = lang)
  if (nrow(fb_env$label_map) == 0) return(code)

  row <- fb_env$label_map[fb_env$label_map$code == code, ]
  if (nrow(row) == 0) return(code)

  if (lang == "fr" && "label_fr" %in% names(row)) {
    return(row$label_fr[1])
  } else if ("label_en" %in% names(row)) {
    return(row$label_en[1])
  } else {
    return(row$label[1])
  }
}

# Get ALL exposure labels including legacy CEDARS codes (not filtered by microdata columns)
# This is needed for CEDARS uploads which use legacy exposure codes
fb_exposure_choices_all <- function(lang = "en") {
  fb_init(lang = lang)

  # Get the full combined label map (FB2 + FB1 + legacy) without filtering by microdata
  lm <- dplyr::bind_rows(
    if (!is.null(fb_env$label_map_fb2)) fb_env$label_map_fb2 else tibble::tibble(),
    if (!is.null(fb_env$label_map_fb1)) fb_env$label_map_fb1 else tibble::tibble(),
    if (!is.null(fb_env$label_map_legacy)) fb_env$label_map_legacy else tibble::tibble()
  ) |>
    dplyr::distinct(code, .keep_all = TRUE)

  if (nrow(lm) == 0) return(stats::setNames(character(), character()))

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
  label_col[duplicated_labels] <- paste0(label_col[duplicated_labels], " (", lm$code[duplicated_labels], ")")

  return(stats::setNames(lm$code, label_col))
}

fb_age_groups <- function() {
  fb_init()
  # If microdata present, offer the fixed mapping; else return empty so UI shows "All" only
  if (!is.null(fb_env$micro)) c("0-9", "10-19", "20-64", "65+") else character()
}

fb_months <- function() {
  fb_init()
  if (is.null(fb_env$micro) || !"Month" %in% names(fb_env$micro)) return(integer())
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
    return(c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
             "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
  } else {
    return(month.name)
  }
}

# Internal: filter microdata given reference selections
fb_filter_micro <- function(pt_names = NULL, months = NULL, age_groups = NULL) {
  fb_init()
  d <- fb_env$micro
  if (is.null(d)) return(data.frame())
  if (!is.null(pt_names) && length(pt_names) && !("Canada" %in% pt_names) && "PT" %in% names(d)) {
    codes <- unname(fb_env$pt_map[pt_names])
    d <- d |> dplyr::filter(PT %in% codes)
  }
  if (!is.null(months) && length(months) && "Month" %in% names(d)) {
    d <- d |> dplyr::filter(Month %in% months)
  }
  if (!is.null(age_groups) && length(age_groups) && "AgeBand" %in% names(d)) {
    d <- d |> dplyr::filter(AgeBand %in% age_groups)
  }
  d
}

# Compute a weighted percentage (0-100) for a single exposure code
fb_weighted_percent <- function(code, d) {
  if (!code %in% names(d)) return(NA_real_)
  x <- suppressWarnings(as.numeric(d[[code]]))
  w <- suppressWarnings(as.numeric(d$weight))
  ok <- !is.na(x) & (x %in% c(0, 1, 2))
  # Many FB dv vars are 1 = yes, 2 = no; treat 0 as missing/not asked
  yy <- ok & (x == 1)
  denom <- sum(w[ok], na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
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
  df <- tryCatch(read.csv("data/foodbook_data.csv", stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !all(c("Exposure", "Province.Territory", "Proportion") %in% names(df))) {
    return(stats::setNames(rep(NA_real_, length(codes)), codes))
  }
  # If Canada is selected or no PTs, use Canada rows
  if (is.null(pt_names) || length(pt_names) == 0 || any(pt_names == "Canada")) {
    res <- vapply(codes, function(x) {
      v <- df$Proportion[df$Exposure == x & df$Province.Territory == "Canada"]
      v <- suppressWarnings(as.numeric(v[1]))
      ifelse(length(v) == 0, NA_real_, v)
    }, numeric(1))
    return(res)
  }
  # Otherwise, average across selected PTs (simple mean; weights unavailable in CSV)
  ab <- fb_pt_abbrev_map()
  sel_ab <- unname(ab[pt_names])
  res <- vapply(codes, function(x) {
    v <- df$Proportion[df$Exposure == x & df$Province.Territory %in% sel_ab]
    v <- suppressWarnings(as.numeric(v))
    if (!length(v)) return(NA_real_)
    mean(v, na.rm = TRUE)
  }, numeric(1))
  res
}

fb_reference_percents <- function(codes, pt_names = NULL, months = NULL, age_groups = NULL) {
  if (!is.null(fb_env$micro)) {
    # Get CEDARS->Foodbook mapping
    cedars_map <- fb_cedars_to_foodbook_map()

    # Map CEDARS codes to Foodbook columns where possible
    fb_codes <- sapply(codes, function(code) {
      if (code %in% names(cedars_map)) {
        cedars_map[code]  # Map to Foodbook column
      } else {
        code  # Keep as-is (might already be a Foodbook code)
      }
    })

    # Calculate references using Foodbook column names
    # Try FB2 first (primary), then FB1 if column doesn't exist
    results <- vapply(seq_along(fb_codes), function(i) {
      fb_col <- fb_codes[i]

      # Try FB2 first (fb_env$micro)
      d_fb2 <- fb_filter_micro(pt_names, months, age_groups)
      if (fb_col %in% names(d_fb2)) {
        return(fb_weighted_percent(fb_col, d_fb2))
      }

      # If not in FB2, try FB1 (fb_env$micro_fb1)
      if (!is.null(fb_env$micro_fb1)) {
        # Use custom filter for FB1 since it has different structure
        d_fb1 <- fb_env$micro_fb1

        # Filter by PT
        if (!is.null(pt_names) && "PT" %in% names(d_fb1)) {
          pt_codes <- fb_pt_map()[pt_names]
          d_fb1 <- d_fb1[d_fb1$PT %in% pt_codes, ]
        }

        # Filter by month
        if (!is.null(months) && "month_dv" %in% names(d_fb1)) {
          d_fb1 <- d_fb1[d_fb1$month_dv %in% months, ]
        }

        # Filter by age group
        if (!is.null(age_groups) && "age_grp_dv" %in% names(d_fb1)) {
          age_map <- c("0-9" = 1L, "10-19" = 2L, "20-64" = 3L, "65+" = 4L)
          age_codes <- age_map[age_groups]
          d_fb1 <- d_fb1[d_fb1$age_grp_dv %in% age_codes, ]
        }

        # Try exact column name first
        if (fb_col %in% names(d_fb1)) {
          return(fb_weighted_percent(fb_col, d_fb1))
        }

        # Try with _dv suffix (common in Open Canada data)
        fb_col_dv <- paste0(fb_col, "_dv")
        if (fb_col_dv %in% names(d_fb1)) {
          return(fb_weighted_percent(fb_col_dv, d_fb1))
        }
      }

      # Column not found in either dataset
      return(NA_real_)
    }, numeric(1))

    # Return with original CEDARS code names
    names(results) <- codes
    return(results)
  }
  fb_reference_percents_csv(codes, pt_names)
}

fb_is_available <- function() {
  fb_init()
  !is.null(fb_env$micro)
}
