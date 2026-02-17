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
#' Fully vectorized — accepts scalar or vector inputs.
#' @param p_value P-value from binomial test (numeric)
#' @param observed_prop Observed proportion (0-1 scale)
#' @param ref_prop Reference percentage (0-100 scale)
#' @return Character vector of classifications: "Alert", "Borderline",
#'         "Not Significant", "Insufficient Data", or "No Reference Value"
classify_exposure <- function(p_value, observed_prop, ref_prop) {
  ref_prop_decimal <- ref_prop / 100
  dplyr::case_when(
    is.na(ref_prop)                                     ~ "No Reference Value",
    is.na(p_value)                                      ~ "Insufficient Data",
    observed_prop > ref_prop_decimal & p_value <= 0.05  ~ "Alert",
    observed_prop > ref_prop_decimal & p_value <= 0.10  ~ "Borderline",
    TRUE                                                ~ "Not Significant"
  )
}

#' Create safe HTML ID from exposure name
#' @param exposure_name Character string to sanitize
#' @return Character string with only alphanumeric characters
make_safe_id <- function(exposure_name) {
  gsub("[^a-zA-Z0-9]", "", exposure_name)
}

#' Safely convert to numeric with explicit validation
#' Replaces suppressWarnings(as.numeric(...)) throughout the codebase.
#' Non-convertible values become NA; optionally logs data quality warnings.
#' @param x Vector to convert
#' @param context Optional context label for log messages (NULL = silent)
#' @return Numeric vector (non-convertible values become NA)
safe_as_numeric <- function(x, context = NULL) {
  result <- tryCatch(
    as.numeric(x),
    warning = function(w) {
      if (!is.null(context)) {
        message("[Data Quality] Non-numeric values in ", context, ": ", conditionMessage(w))
      }
      suppressWarnings(as.numeric(x))
    }
  )
  result
}

# =============================================================================
# Global Backend State
# =============================================================================
# ARCHITECTURE NOTE (#9): fb_env is a process-global environment used to cache
# microdata, label maps, and toolkit data. In a single-process multi-session
# deployment (e.g., Shiny Server open-source), all sessions share this state.
#
# Immutable data (microdata, exposure codes, toolkit CSVs) is safe to share.
# The only mutable field is label_map$label, which fb_update_language() swaps
# between label_en/label_fr. This is safe because all functions that read labels
# explicitly select the correct column via the `lang` parameter rather than
# relying on the cached `label` column.
#
# If true per-session isolation is needed in the future, wrap fb_env in a
# session-scoped reactiveValues object inside fb_init_common().
# =============================================================================
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
  # Try two levels up (for tests nested in tests/testthat)
  grandparent_path <- file.path("../..", rel_path)
  if (file.exists(grandparent_path) || dir.exists(grandparent_path)) {
    return(grandparent_path)
  }
  # Return original path (will fail downstream with clear error)
  return(rel_path)
}
