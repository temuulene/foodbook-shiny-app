# =============================================================================
# Foodbook Backend Hub
# =============================================================================
# This file serves as the entry point for the backend.
# It sources specialized sub-modules in the correct dependency order.
#
# Original file split on 2026-02-16 to improve maintainability.
# =============================================================================

# Define directory containin backend modules
# We use a robust method to find the 'backend' folder relative to this script
get_backend_dir <- function() {
  # Check if being sourced (standard behavior)
  if (!is.null(sys.calls())) {
    for (i in seq_len(sys.nframe())) {
      # Look for source call frame
      if (exists("ofile", envir = sys.frame(i), inherits = FALSE)) {
        return(file.path(dirname(sys.frame(i)$ofile), "backend"))
      }
    }
  }
  # Fallback: assume running from project root (standard test/dev env)
  return("src/backend")
}

backend_dir <- get_backend_dir()

# Source sub-modules in dependency order
# -------------------------------------

# 1. Utilities (Global state `fb_env`, shared helpers)
source(file.path(backend_dir, "fb_utils.R"), local = FALSE)

# 2. Data Loading (CSV/DTA parsing, renames)
# Depends on: fb_utils
source(file.path(backend_dir, "fb_data_loading.R"), local = FALSE)

# 3. Geography (PT mapping)
# Depends on: fb_utils
source(file.path(backend_dir, "fb_geography.R"), local = FALSE)

# 4. Initialization (Backend init, language updates)
# Depends on: fb_utils, fb_data_loading, fb_geography
source(file.path(backend_dir, "fb_init.R"), local = FALSE)

# 5. Toolkit (Toolkit data loading and lookups)
# Depends on: fb_utils, fb_geography
source(file.path(backend_dir, "fb_toolkit.R"), local = FALSE)

# 6. Exposures (Core logic, choices, filtering, calculations)
# Depends on: fb_utils, fb_init, fb_toolkit
source(file.path(backend_dir, "fb_exposures.R"), local = FALSE)
