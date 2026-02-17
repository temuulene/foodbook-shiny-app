# Initialise and cache everything we need
# ONLY uses authoritative PHAC OMD data from upgrade-context/
# Open Canada data has been archived and removed from workflow
fb_init <- function(lang = "en") {
  if (!is.null(fb_env$initialised) && isTRUE(fb_env$initialised)) {
    return(invisible(TRUE))
  }

  # =============================================================================
  # Load authoritative PHAC OMD microdata from upgrade-context/
  # =============================================================================
  legacy_dta_path <- fb_get_base_path("upgrade-context/foodbook.dta")
  legacy_dta2_path <- fb_get_base_path("upgrade-context/foodbook2v2.dta")

  if (!file.exists(legacy_dta_path) && !file.exists(legacy_dta2_path)) {
    stop(
      "Authoritative microdata not found in upgrade-context/. ",
      "Please ensure foodbook.dta and/or foodbook2v2.dta are present."
    )
  }

  message("Loading authoritative microdata from upgrade-context/...")
  fb_env$micro <- fb_load_microdata()
  
  if (is.null(fb_env$micro)) {
    stop("Failed to load microdata from upgrade-context/")
  }
  
  message(
    "Loaded authoritative microdata from upgrade-context/ (",
    nrow(fb_env$micro),
    " respondents)"
  )
  
  fb_env$data_source <- "Legacy"
  fb_env$micro_fb1 <- NULL  # Not used in legacy mode

  # =============================================================================
  # Load authoritative labels from upgrade-context/
  # =============================================================================
  legacy_label <- fb_get_base_path("upgrade-context/foodbook variable labeling.do")

  if (!file.exists(legacy_label)) {
    stop(
      "Authoritative label file not found: ",
      "upgrade-context/foodbook variable labeling.do"
    )
  }

  fb_env$label_map <- fb_parse_label_map(legacy_label) |>
    dplyr::mutate(
      label_en = as.character(label),
      label_fr = as.character(label)  # English only in legacy file
    )
  message("Loaded authoritative labels from upgrade-context/foodbook variable labeling.do")

  # [LOOSE MERGE] Load extra labels from exposures_bilingual.csv
  # This ensures the restored FB1 variables have labels suitable for the UI
  extra_labels_path <- fb_get_base_path("data/exposures_bilingual.csv")
  if (file.exists(extra_labels_path)) {
    extra_labels <- utils::read.csv(extra_labels_path, stringsAsFactors = FALSE)
    # Expected cols: variable_name, exposure_en, exposure_fr
    if (all(c("variable_name", "exposure_en", "exposure_fr") %in% names(extra_labels))) {
      # Prepare as same structure as label_map
      extra_map <- tibble::tibble(
        code = extra_labels$variable_name,
        label = extra_labels$exposure_en,
        label_en = extra_labels$exposure_en,
        label_fr = extra_labels$exposure_fr
      )
      
      # Merge: start with toolkit map (more user friendly), then add others from OMD
      fb_env$label_map <- dplyr::bind_rows(
        extra_map,
        fb_env$label_map |> dplyr::filter(!code %in% extra_map$code)
      )
    }
  }

  # =============================================================================
  # Apply label overrides for clarity (preserve asterisk if present)
  # =============================================================================
  # "Carrots*" -> "Carrots (not mini)*" to distinguish from mini carrots
  carrot_idx <- which(fb_env$label_map$code == "carrot")
  if (length(carrot_idx) > 0) {
    fb_env$label_map$label[carrot_idx] <- "Carrots (not mini)*"
    fb_env$label_map$label_en[carrot_idx] <- "Carrots (not mini)*"
    fb_env$label_map$label_fr[carrot_idx] <- "Carottes (pas mini)*"
  }

  # =============================================================================
  # Label renames from Megan (Jan 7, 2026) for consistency with Toolkit
  # =============================================================================
  # Helper function to apply label rename by matching old label pattern
  apply_label_rename <- function(old_pattern, new_en, new_fr) {
    idx <- which(grepl(old_pattern, fb_env$label_map$label_en, ignore.case = TRUE))
    if (length(idx) > 0) {
      fb_env$label_map$label[idx] <- new_en
      fb_env$label_map$label_en[idx] <- new_en
      fb_env$label_map$label_fr[idx] <- new_fr
    }
  }

  # "Any nuts- on their own..." -> "Any nuts"
  apply_label_rename(
    "Any nuts.*on their own",
    "Any nuts",
    "Noix de tout genre"
  )

  # "Chips*" -> "Chips or pretzels*"
  apply_label_rename(
    "^Chips\\*?$",
    "Chips or pretzels*",
    "Croustilles ou bretzels*"
  )

  # "Municipal water" -> "Consumed municipal water"
  apply_label_rename(
    "^Municipal water$",
    "Consumed municipal water",
    "Eau municipale consommée"
  )

  # "Bottled water" -> "Consumed store-bought bottled water"
  apply_label_rename(
    "^Bottled water$",
    "Consumed store-bought bottled water",
    "Eau embouteillée achetée en magasin consommée"
  )

  # "Granola" -> "Granola bars, power bars, or other protein bars*"
  apply_label_rename(
    "^Granola$",
    "Granola bars, power bars, or other protein bars*",
    "Barres granola, barres énergétiques ou autres barres protéinées*"
  )

  # For backward compatibility with existing code that expects single "label" column
  if (lang == "fr") {
    fb_env$label_map$label <- fb_env$label_map$label_fr
  } else {
    fb_env$label_map$label <- fb_env$label_map$label_en
  }

  # =============================================================================
  # Determine exposure columns (skip aggressive filtering to preserve labels)
  # =============================================================================
  # Harmonize common code renames in label_map to match toolkit names
  # This avoids duplicate labels causing one to be dropped
  fb_env$label_map$code[fb_env$label_map$code == "water_municipal"] <- "cmunicipal"
  fb_env$label_map$code[fb_env$label_map$code == "water_bottled"] <- "cbottled"
  fb_env$label_map$code[fb_env$label_map$code == "water_well"] <- "cwell"
  
  # Ensure exposure_codes is still updated for other logic
  fb_env$exposure_codes <- unique(fb_env$label_map$code)

  # =============================================================================
  # Normalize column names for consistency
  # =============================================================================
  # Normalize PT column
  if ("QINTRO3" %in% names(fb_env$micro) && !"PT" %in% names(fb_env$micro)) {
    fb_env$micro$PT <- fb_env$micro$QINTRO3
  }

  # Normalize Month column
  if ("month_dv" %in% names(fb_env$micro) && !"Month" %in% names(fb_env$micro)) {
    fb_env$micro$Month <- fb_env$micro$month_dv
  }

  # Normalize Age_group column
  if ("age_grp_dv" %in% names(fb_env$micro) && !"Age_group" %in% names(fb_env$micro)) {
    fb_env$micro$Age_group <- fb_env$micro$age_grp_dv
  }
  if ("Age_grp_dv" %in% names(fb_env$micro) && !"Age_group" %in% names(fb_env$micro)) {
    fb_env$micro$Age_group <- fb_env$micro$Age_grp_dv
  }

  # =============================================================================
  # Construct AgeBand for filtering
  # =============================================================================
  if ("Age_group" %in% names(fb_env$micro)) {
    ag <- safe_as_numeric(fb_env$micro$Age_group, "Age_group")
    
    # [FB1 FIX] Coalesce with age_grp4_dv if present (legacy FB1 variable)
    # Both variables use 1-4 coding mapping to same bands
    if ("age_grp4_dv" %in% names(fb_env$micro)) {
      ag_fb1 <- safe_as_numeric(fb_env$micro$age_grp4_dv, "age_grp4_dv")
      ag <- ifelse(is.na(ag) & !is.na(ag_fb1), ag_fb1, ag)
    }

    # Age groups in authoritative microdata:
    # - Values 1, 2, 3, 4 map directly to 4 bands (confirmed from data)
    # - Value 999 = missing/refused
    # - NA = not applicable
    age_map <- c(
      `1` = "0-9",
      `2` = "10-19",
      `3` = "20-64",
      `4` = "65+"
    )
    fb_env$micro$AgeBand <- unname(age_map[as.character(ag)])
  } else if ("age" %in% names(fb_env$micro)) {
    a <- safe_as_numeric(fb_env$micro$age, "age")
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
