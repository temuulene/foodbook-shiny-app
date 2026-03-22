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

#' Get mapping: CEDARS exposure code (P-codes) -> Foodbook column name
#' Used internally by fb_reference_percents() (line ~1509) for CEDARS code resolution.
#' NOT dead code — called when CEDARS data is uploaded.
#' @return Named character vector (currently empty; pending CEDARS mapping file)
fb_cedars_to_foodbook_map <- function() {
  fb_init()

  # Check if CEDARS map is loaded
  if (is.null(fb_env$cedars_to_fb_map)) {
    # The CEDARS P-codes need to be manually mapped to Foodbook columns
    # This mapping is based on CEDARS exposure code -> Foodbook renamed column
    # For now, return empty - CEDARS integration would require a separate mapping file
    fb_env$cedars_to_fb_map <- rlang::set_names(character(), character())
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
  
  # Only rename if 'old' exists in df
  present <- renames$old %in% names(df)
  if (!any(present)) {
    return(df)
  }
  
  # Filter to relevant renames
  relevant_renames <- renames[present, ]
  
  # Avoid collisions: Do not rename if 'new' name ALREADY exists in df
  # (This prevents errors like "Names must be unique" if target col exists)
  collision <- relevant_renames$new %in% names(df)
  if (any(collision)) {
    # If the target already exists, we assume the existing column is the correct one 
    # and skip renaming the legacy/source column to avoid overwriting/duplication errors.
    relevant_renames <- relevant_renames[!collision, ]
  }
  
  if (nrow(relevant_renames) == 0) {
    return(df)
  }

  map <- relevant_renames$new
  names(map) <- relevant_renames$old
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
    w <- safe_as_numeric(df$EXPWEIGHT_CMA2, "EXPWEIGHT_CMA2")
  }

  # Priority 2: FB2 weight (proj_weight_non_traveller) - fills in where FB1 weight is NA
  if ("proj_weight_non_traveller" %in% names(df)) {
    fb2_w <- safe_as_numeric(df$proj_weight_non_traveller, "proj_weight_non_traveller")
    if (is.null(w)) {
      w <- fb2_w
    } else {
      # Replace NA values with FB2 weight (matches Stata: replace weight = X if weight == .)
      w <- ifelse(is.na(w), fb2_w, w)
    }
  }

  # Fallback: Try Open Canada variant column names
  if (is.null(w) && "EXPWEIGHT_CMA2_dv" %in% names(df)) {
    w <- safe_as_numeric(df$EXPWEIGHT_CMA2_dv, "EXPWEIGHT_CMA2_dv")
  }
  if (is.null(w) && "Proj_weight_non_traveller_dv" %in% names(df)) {
    w <- safe_as_numeric(df$Proj_weight_non_traveller_dv, "Proj_weight_non_traveller_dv")
  }

  # Fallback: Generic weight column
  if (is.null(w) && "weight" %in% names(df)) {
    w <- safe_as_numeric(df$weight, "weight")
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

  # [LOOSE MERGE] Load FB1 extra variable map to recover legacy vars
  # This map handles variables that were dropped by the Stata script (drop Q*)
  fb1_map_path <- fb_get_base_path("data/fb1_variable_map.csv")
  if (file.exists(fb1_map_path)) {
    fb1_map <- utils::read.csv(fb1_map_path, stringsAsFactors = FALSE)
    # Convert to rename format: old=fb1_var, new=toolkit_var
    # Use fb1_var as 'old' and toolkit_var as 'new'
    if (all(c("fb1_var", "toolkit_var") %in% names(fb1_map))) {
      extra_renames <- tibble::tibble(
        old = fb1_map$fb1_var,
        new = fb1_map$toolkit_var
      )
      # Append to authoritative renames (toolkit map takes precedence)
      fb1_renames <- dplyr::bind_rows(extra_renames, fb1_renames) |>
        dplyr::distinct(old, .keep_all = TRUE) 
    }
  }

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
      
      # [LOOSE MERGE] Load FB2 extra variable map to recover vars not renamed in .do
      fb2_map_path <- fb_get_base_path("data/fb2_variable_map.csv")
      if (file.exists(fb2_map_path)) {
        fb2_map <- utils::read.csv(fb2_map_path, stringsAsFactors = FALSE)
        if (all(c("fb_var", "toolkit_var") %in% names(fb2_map))) {
          extra_fb2_renames <- tibble::tibble(
            old = fb2_map$fb_var,
            new = fb2_map$toolkit_var
          )
          # Append to authoritative renames, prioritize toolkit maps
          fb2_renames <- dplyr::bind_rows(extra_fb2_renames, fb2_renames) |>
            dplyr::distinct(old, .keep_all = TRUE)
        }
      }

      # Apply FB2-specific renames
      fb2 <- fb_apply_renames(fb2, fb2_renames)
      fb2$fb_source <- "FB2"
      dfs[[length(dfs) + 1]] <- fb2
    }
  }

  if (!length(dfs)) {
    return(NULL)
  }

  # Apply precedence: If a variable exists in both FB1 and FB2, 
  # null out the FB1 version so FB2 values take precedence in the combined dataset.
  if (length(dfs) >= 2) {
    fb1 <- dfs[[1]]
    fb2 <- dfs[[2]]
    
    # Identify shared columns that are actual data (not indexing/structural)
    structural <- c(
      "uniqueid", "PT", "Month", "Age_group", "weight", "Weight", 
      "fb_source", "AgeBand", "month_dv", "age_grp_dv", "QINTRO3"
    )
    shared <- intersect(names(fb1), names(fb2))
    targets <- setdiff(shared, structural)
    
    if (length(targets) > 0) {
      # For shared variables, set FB1 to NA so that weighted means 
      # correctly prioritize the most recent (FB2) data.
      fb1[targets] <- lapply(fb1[targets], function(x) {
        if (is.numeric(x)) NA_real_ else NA_character_
      })
      dfs[[1]] <- fb1
    }
  }

  # Data Quality: Sanitize structural columns to avoid attribute mismatch warnings
  # (FB1 and FB2 may have different haven_labelled attributes which upset bind_rows)
  dfs <- lapply(dfs, function(d) {
    if ("Month" %in% names(d)) d$Month <- as.integer(d$Month)
    if ("PT" %in% names(d)) d$PT <- as.integer(d$PT)
    if ("Age_group" %in% names(d)) d$Age_group <- as.integer(d$Age_group)
    d
  })

  # Combine datasets (append FB2 to FB1, as per authoritative workflow)
  combined <- tryCatch(
    dplyr::bind_rows(dfs),
    warning = function(w) {
      message("[Data Quality] Column type mismatch during dataset merge: ", conditionMessage(w))
      suppressWarnings(dplyr::bind_rows(dfs))
    }
  )

  # Apply weight normalization (merges FB1 and FB2 weights)
  combined <- fb_normalise_weight(combined)

  combined
}
