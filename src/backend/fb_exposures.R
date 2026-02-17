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
  
  # If microdata + labels available, return label->code
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

    # Normalize English labels for display
    if (lang != "fr") {
      label_col <- fb_normalize_en_label(label_col)
    }

    # Apply public app exclusions (label based)
    if (isTRUE(apply_public_exclusions)) {
      # First drop by code
      code_exclusions <- fb_public_exposure_exclusion_codes()
      if (length(code_exclusions)) {
        keep_mask <- !lm$code %in% code_exclusions
        lm <- lm[keep_mask, ]
        label_col <- label_col[keep_mask]
      }

      # Then drop by label
      exclusions <- fb_public_exposure_exclusions()
      if (length(exclusions)) {
        norm_labels <- fb_normalize_label_for_match(label_col)
        norm_exclusions <- fb_normalize_label_for_match(exclusions)
        keep_mask <- !norm_labels %in% norm_exclusions
        lm <- lm[keep_mask, ]
        label_col <- label_col[keep_mask]
      }
    }

    # Final deduplication: if same label appears for multiple codes, keep the first one
    dup_label_mask <- duplicated(label_col)
    if (any(dup_label_mask)) {
      lm <- lm[!dup_label_mask, ]
      label_col <- label_col[!dup_label_mask]
    }

    return(stats::setNames(lm$code, label_col))
  }
  
  # No fallback - require authoritative data
  stop("Authoritative microdata required. Please ensure upgrade-context/ data is available.")
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

#' Get ALL exposure labels including legacy CEDARS codes (not filtered by microdata)
#' Used by app-internal/app.R for CEDARS label mapping in uploads.
#' NOT dead code — required for CEDARS upload workflow.
#' @param lang Language code ("en" or "fr")
#' @return Named character vector of exposure code -> label
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
    },
    if (!is.null(fb_env$label_map)) {
      fb_env$label_map
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
# Returns a named list with 'percent' and 'sample_size'
fb_weighted_percent <- function(code, d, return_sample_size = FALSE) {
  if (!code %in% names(d)) {
    if (return_sample_size) {
      return(list(percent = NA_real_, sample_size = 0L))
    }
    return(NA_real_)
  }
  x <- safe_as_numeric(d[[code]], code)
  w <- safe_as_numeric(d$weight, "weight")
  ok <- !is.na(x) & (x %in% c(0, 1, 2))
  # Many FB dv vars are 1 = yes, 2 = no; treat 0 as missing/not asked
  yy <- ok & (x == 1)
  denom <- sum(w[ok], na.rm = TRUE)
  sample_n <- sum(ok)
  if (!is.finite(denom) || denom <= 0) {
    if (return_sample_size) {
      return(list(percent = NA_real_, sample_size = sample_n))
    }
    return(NA_real_)
  }
  pct <- 100 * sum(w[yy], na.rm = TRUE) / denom
  if (return_sample_size) {
    return(list(percent = pct, sample_size = sample_n))
  }
  pct
}

# Get reference sample size for a given filter configuration
fb_reference_sample_size <- function(pt_names = NULL, months = NULL, age_groups = NULL) {
  fb_init()
  d <- fb_filter_micro(pt_names, months, age_groups)
  if (is.null(d)) return(0L)
  nrow(d)
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
        v <- safe_as_numeric(v[1])
        ifelse(length(v) == 0, NA_real_, v)
      },
      numeric(1)
    )
    return(res)
  }
  # Otherwise, average across selected PTs (simple mean; survey weights unavailable in CSV)
  pt_codes <- fb_normalize_pt_names(pt_names)
  pt_abbr <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "YT", "NT", "NU")
  sel_ab <- unique(pt_abbr[pt_codes])
  res <- vapply(
    codes,
    function(x) {
      v <- df$Proportion[df$Exposure == x & df$Province.Territory %in% sel_ab]
      v <- safe_as_numeric(v)
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
  pt_to_use <- NULL
  if (is.null(pt_names) || length(pt_names) == 0) {
    pt_to_use <- "Canada"
  } else if (length(pt_names) == 1) {
    pt_to_use <- pt_names
  }

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
        # High Priority: For Total Population (no Age/Month filters), 
        # use official published values from Toolkit if available.
        if (is.null(months) && is.null(age_groups) && !is.null(pt_to_use)) {
          tk_val <- fb_toolkit_reference_percent(fb_col, pt_to_use)
          if (!is.na(tk_val)) {
            return(tk_val)
          }
        }

        # Legacy fallback for FB1 only variables (primarily Open Canada mode)
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

    # 3. Fallback to Toolkit Proportions (systematized CSV)
        # This handles FB2 variables that might be missing from microdata filters or just missing in general
        # but present in the official toolkit data.
        if (!is.null(pt_to_use)) {
          val <- fb_toolkit_reference_percent(fb_col, pt_to_use)
          if (!is.na(val)) return(val)
        }

        NA_real_
      },
      numeric(1)
    )

    names(results) <- codes
    return(results)
  }
  
  # If microdata is NULL, try Toolkit data first, then legacy CSV
  # Try Toolkit first as it's more comprehensive and uses codes
  res_toolkit <- vapply(codes, function(x) {
    if (is.null(pt_to_use)) {
      return(NA_real_)
    }
    val <- fb_toolkit_reference_percent(x, pt_to_use)
    if (is.na(val)) {
       # Fallback to legacy CSV logic
       # This requires mapping code to label if possible, but fb_reference_percents_csv takes "label"
       # We only have code here. The legacy function might need checking.
       return(NA_real_) 
    }
    val
  }, numeric(1))
  
  if (all(is.na(res_toolkit))) {
    return(fb_reference_percents_csv(codes, pt_names))
  }
  
  # Fill NAs in toolkit results with legacy results
  na_idx <- is.na(res_toolkit)
  if (any(na_idx)) {
    legacy_res <- fb_reference_percents_csv(codes[na_idx], pt_names)
    res_toolkit[na_idx] <- legacy_res
  }
  res_toolkit
}

fb_is_available <- function() {
  fb_init()
  !is.null(fb_env$micro)
}
