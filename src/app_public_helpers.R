# Helpers for the public Shiny app

#' Build a label map from toolkit exposure choices (value -> name)
#' @param lang Language code
#' @return Named character vector: exposure_code -> label
fb_build_exposure_label_map <- function(lang) {
  choices <- fb_toolkit_exposure_choices(lang)
  values <- unlist(choices, use.names = FALSE)
  if (!length(values)) return(character())
  rlang::set_names(names(choices), values)
}

#' Resolve a human-readable label for an exposure code
#' @param code Exposure code
#' @param lang Language code
#' @param label_map Named character vector from fb_build_exposure_label_map()
#' @return Label string (escaped if user-created)
fb_resolve_exposure_label <- function(code, lang, label_map) {
  label <- label_map[code]
  if (length(label) == 0 || is.na(label) || !nzchar(label)) {
    label <- fb_exposure_label(code, lang)
    # fb_exposure_label returns code as-is when not found — escape it
    if (identical(label, code)) {
      return(htmltools::htmlEscape(code))
    }
  }
  if (length(label) == 0 || is.na(label) || !nzchar(label)) {
    return(htmltools::htmlEscape(code))
  }
  unname(label)
}

#' Compute observed proportions, p-values, and classifications for exposure data
#'
#' Shared analysis logic used by both public and internal apps.
#' Fully vectorized — no rowwise() needed.
#'
#' @param df Data frame with columns: ExposureLabel, Y, P, N, DK, ref_pct, scope_label
#' @param lang Language code for classification labels ("en" or "fr")
#' @return Data frame with columns: Reference Scope, Exposure, Total Valid,
#'         Yes, Probably, No, DK, Observed %, Reference %, P-Value, Classification
fb_classify_results <- function(df, lang = "en") {
  y_plus_p <- df$Y + df$P
  total <- y_plus_p + df$N
  ref_pct <- df$ref_pct

  observed_prop <- dplyr::if_else(total > 0, y_plus_p / total, NA_real_)

  can_test <- total >= FB_MIN_SAMPLE_SIZE &
    !is.na(ref_pct) & ref_pct > 0 & ref_pct <= 100
  p_value <- rep(NA_real_, nrow(df))
  if (any(can_test)) {
    p_value[can_test] <- stats::pbinom(
      y_plus_p[can_test] - 1,
      total[can_test],
      ref_pct[can_test] / 100,
      lower.tail = FALSE
    )
  }

  classification <- classify_exposure(p_value, observed_prop, ref_pct)
  classification <- purrr::map_chr(
    classification,
    classification_label_i18n,
    lang = lang
  )

  tibble::tibble(
    `Reference Scope` = df$scope_label,
    Exposure = df$ExposureLabel,
    `Total Valid` = total,
    Yes = df$Y,
    Probably = df$P,
    No = df$N,
    DK = df$DK,
    `Observed %` = observed_prop,
    `Reference %` = ref_pct,
    `P-Value` = p_value,
    Classification = unname(classification)
  )
}

fb_public_sanitize_count <- function(value, max_val = 10000L) {
  if (is.null(value) || is.na(value)) return(0L)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) return(0L)
  as.integer(max(0L, min(max_val, floor(num))))
}

fb_public_collect_exposure_inputs <- function(exposure_codes, input_values) {
  if (length(exposure_codes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(exposure_codes, function(code) {
    safe_id <- make_safe_id(code)
    ns_prefix <- paste0("exp_", safe_id, "-")

    y_val <- fb_public_sanitize_count(
      input_values[[paste0(ns_prefix, "yes")]]
    )
    p_val <- fb_public_sanitize_count(
      input_values[[paste0(ns_prefix, "prob")]]
    )
    n_val <- fb_public_sanitize_count(
      input_values[[paste0(ns_prefix, "no")]]
    )
    dk_val <- fb_public_sanitize_count(
      input_values[[paste0(ns_prefix, "dk")]]
    )

    custom_ref <- input_values[[paste0(ns_prefix, "custom_ref")]]
    if (is.null(custom_ref) || is.na(custom_ref)) {
      custom_ref <- NA_real_
    } else {
      custom_ref <- as.numeric(custom_ref)
      if (!is.na(custom_ref)) {
        custom_ref <- max(0, min(100, custom_ref))
      }
    }

    tibble::tibble(
      Exposure = code,
      Y = y_val,
      P = p_val,
      N = n_val,
      DK = dk_val,
      custom = custom_ref
    )
  })
}

fb_public_merge_custom_choices <- function(matched_exposures, current_choices) {
  if (length(matched_exposures) == 0) {
    return(character())
  }
  known_codes <- unname(unlist(current_choices, use.names = FALSE))
  custom <- matched_exposures[!matched_exposures %in% known_codes]
  unique(custom[!is.na(custom) & nzchar(custom)])
}

fb_public_reference_table_from_choices <- function(choices, refs) {
  if (length(choices) == 0) {
    return(tibble::tibble())
  }

  codes <- unname(unlist(choices, use.names = FALSE))
  labels <- names(choices)
  ref_vals <- as.numeric(refs[codes])

  tibble::tibble(
    Exposure = labels,
    Code = codes,
    `Reference %` = ref_vals
  )
}

fb_public_build_reference_table <- function(
  choices,
  pt_names = NULL,
  months = NULL,
  age_groups = NULL,
  reference_fun = fb_reference_percents
) {
  if (length(choices) == 0) {
    return(tibble::tibble())
  }

  codes <- unname(unlist(choices, use.names = FALSE))
  refs <- reference_fun(
    codes, pt_names = pt_names,
    months = months, age_groups = age_groups
  )
  fb_public_reference_table_from_choices(choices, refs)
}

fb_public_top_exposures <- function(ref_table, n = 10) {
  if (is.null(ref_table) || !nrow(ref_table)) {
    return(ref_table)
  }

  tbl <- ref_table
  tbl <- tbl[!is.na(tbl$`Reference %`), , drop = FALSE]
  if (!nrow(tbl)) {
    return(tbl)
  }

  tbl <- tbl[order(tbl$`Reference %`, decreasing = TRUE), , drop = FALSE]
  utils::head(tbl, n)
}

fb_public_pt_coverage <- function(df, lang = "en") {
  if (is.null(df) || !nrow(df) || !"PT" %in% names(df)) {
    return(tibble::tibble())
  }

  pt_vals <- df$PT
  pt_vals <- pt_vals[!is.na(pt_vals)]
  if (!length(pt_vals)) {
    return(tibble::tibble())
  }

  counts <- sort(table(pt_vals), decreasing = TRUE)
  pt_labels <- names(counts)

  if (is.numeric(pt_vals)) {
    code_to_name_num <- rlang::set_names(
      names(fb_pt_map()), as.character(fb_pt_map())
    )
    labels_en <- code_to_name_num[pt_labels]
  } else {
    abbr_map <- fb_pt_abbrev_map()
    code_to_name <- rlang::set_names(
      names(abbr_map), unname(abbr_map)
    )
    labels_en <- code_to_name[pt_labels]
  }
  labels_en[is.na(labels_en)] <- pt_labels[is.na(labels_en)]

  pt_display <- if (lang == "fr") {
    fr_map <- fb_pt_names_bilingual()
    labels_fr <- fr_map[labels_en]
    labels_fr[is.na(labels_fr)] <- labels_en[is.na(labels_fr)]
    labels_fr
  } else {
    labels_en
  }

  tibble::tibble(
    PT = pt_display,
    Count = as.integer(counts)
  )
}

fb_public_month_coverage <- function(df, lang = "en") {
  if (is.null(df) || !nrow(df) || !"Month" %in% names(df)) {
    return(tibble::tibble())
  }

  month_vals <- df$Month
  month_vals <- month_vals[!is.na(month_vals)]
  if (!length(month_vals)) {
    return(tibble::tibble())
  }

  month_vals <- as.integer(month_vals)
  month_vals <- month_vals[month_vals >= 1 & month_vals <= 12]
  if (!length(month_vals)) {
    return(tibble::tibble())
  }

  counts <- table(factor(month_vals, levels = 1:12))
  counts <- counts[counts > 0]
  month_names <- fb_month_names(lang)

  tibble::tibble(
    Month = month_names[as.integer(names(counts))],
    Count = as.integer(counts)
  )
}

fb_public_available_pts <- function() {
  if (is.null(fb_env$toolkit_proportions)) {
    fb_load_toolkit_data()
  }
  if (is.null(fb_env$toolkit_proportions)) {
    return("Canada")
  }

  df <- fb_env$toolkit_proportions
  valid_pts <- unique(c("Canada", unname(fb_pt_abbrev_map())))
  pt_cols <- intersect(names(df), valid_pts)
  pt_cols <- unique(c("Canada", setdiff(pt_cols, "Canada")))
  pt_cols
}

#' Normalize filter selections into backend-ready values
#' @param provs Province/territory selection (character); "Canada" or NULL -> NULL pt
#' @param ages Age group selection (character); "All Ages" or NULL -> NULL age
#' @param months Month selection (character or integer); "All Months" or NULL -> NULL month
#' @return List with elements `pt`, `age`, and `month` (each NULL or specific value)
fb_normalize_filters <- function(provs, ages, months) {
  if (is.null(provs) || (length(provs) == 1 && provs == "Canada")) provs <- NULL
  if (!is.null(ages) && "All Ages" %in% ages) ages <- NULL
  if (!is.null(months) && "All Months" %in% months) months <- NULL
  else if (!is.null(months)) months <- as.integer(months)
  list(pt = provs, age = ages, month = months)
}
