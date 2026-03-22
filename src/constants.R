# Shared constants for Foodbook Shiny Apps
# Used by both app-public and app-internal

# Input validation limits
FB_MAX_COUNT <- 10000L
FB_MAX_UPLOAD_BYTES <- 10L * 1024L * 1024L # 10 MB

# Statistical thresholds
FB_MIN_SAMPLE_SIZE <- 5L
FB_P_VALUE_ALERT <- 0.05
FB_P_VALUE_BORDERLINE <- 0.10

# UI behavior
FB_DEBOUNCE_MS <- 400L
FB_DEFAULT_CUSTOM_REF <- 60

# Classification levels (English canonical names)
FB_CLASSIFICATION_LEVELS <- c(
  "Alert",
  "Borderline",
  "Not Significant",
  "Insufficient Data",
  "No Reference Value"
)
