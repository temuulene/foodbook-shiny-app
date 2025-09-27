# Backend helpers to use OMD Foodbook microdata and labels
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(haven)
})

fb_env <- new.env(parent = emptyenv())

# Parse Stata rename directives from a .do file
fb_parse_renames <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(tibble::tibble(old = character(), new = character()))
  m <- stringr::str_match(lines, "^\\s*rename\\s+([^\\s]+)\\s+([^\\s]+)")
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  tibble::tibble(old = m[, 2], new = m[, 3])
}

# Parse exposure code -> human label mapping from the variable labeling .do file
fb_parse_label_map <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(tibble::tibble(code = character(), label = character()))
  # Matches both: gen label = "..." if exposure == "code" OR replace label = "..." if exposure == "code"
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
  if ("EXPWEIGHT_CMA2" %in% names(df)) w <- df$EXPWEIGHT_CMA2
  if (is.null(w) && "proj_weight_non_traveller" %in% names(df)) w <- df$proj_weight_non_traveller
  if (is.null(w) && "weight" %in% names(df)) w <- df$weight
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
fb_init <- function() {
  if (!is.null(fb_env$initialised) && isTRUE(fb_env$initialised)) return(invisible(TRUE))
  fb_env$label_map <- fb_parse_label_map("upgrade-context/foodbook variable labeling.do")
  fb_env$micro <- fb_load_microdata()
  if (is.null(fb_env$micro)) stop("Foodbook microdata not found or failed to load.")
  # Determine exposure columns present in microdata
  fb_env$exposure_codes <- fb_env$label_map$code[fb_env$label_map$code %in% names(fb_env$micro)]
  fb_env$label_map <- fb_env$label_map |>
    dplyr::filter(code %in% fb_env$exposure_codes)
  # Keep only the useful columns for filtering
  keep_cols <- unique(c(
    "PT", "Month", "Age_group", "Gender", "age", "sex",
    "weight", fb_env$exposure_codes
  ))
  fb_env$micro <- fb_env$micro[, intersect(keep_cols, names(fb_env$micro)), drop = FALSE]
  # Construct AgeBand using specified mapping when Age_group is present; else derive from numeric age
  if ("Age_group" %in% names(fb_env$micro)) {
    ag <- suppressWarnings(as.integer(fb_env$micro$Age_group))
    age_map <- c(`1` = "0-9", `2` = "10-19", `3` = "20-64", `4` = "65+")
    fb_env$micro$AgeBand <- unname(age_map[as.character(ag)])
  } else if ("age" %in% names(fb_env$micro)) {
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

fb_exposure_choices <- function() {
  fb_init()
  # Return a named vector: label -> code (used by selectize, labels shown)
  # Ensure uniqueness of labels
  lm <- fb_env$label_map
  duplicated_labels <- duplicated(lm$label)
  lm$label[duplicated_labels] <- paste0(lm$label[duplicated_labels], " (", lm$code[duplicated_labels], ")")
  stats::setNames(lm$code, lm$label)
}

fb_age_groups <- function() {
  fb_init()
  # Fixed mapping per request
  c("0-9", "10-19", "20-64", "65+")
}

fb_months <- function() {
  fb_init()
  if (!"Month" %in% names(fb_env$micro)) return(integer())
  m <- sort(unique(na.omit(as.integer(fb_env$micro$Month))))
  m <- m[m >= 1 & m <= 12]
  stats::setNames(as.character(m), month.name[m])
}

fb_pt_names <- function() {
  names(fb_pt_map())
}

# Internal: filter microdata given reference selections
fb_filter_micro <- function(pt_names = NULL, months = NULL, age_groups = NULL) {
  fb_init()
  d <- fb_env$micro
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
fb_reference_percents <- function(codes, pt_names = NULL, months = NULL, age_groups = NULL) {
  d <- fb_filter_micro(pt_names, months, age_groups)
  vapply(codes, fb_weighted_percent, numeric(1), d = d)
}
