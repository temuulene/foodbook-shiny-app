
# Generate Comprehensive FB2 Variable Mapping
# Creates a complete fb2_variable_map.csv by matching FB2 labels to Toolkit exposure labels

library(haven)
library(dplyr)
library(stringr)

# Load exposures_bilingual.csv (source of truth for UI labels)
exposures <- read.csv("data/exposures_bilingual.csv", stringsAsFactors = FALSE)
message(sprintf("Loaded %d exposures from exposures_bilingual.csv", nrow(exposures)))
message("Columns: ", paste(names(exposures), collapse=", "))

# Load FB2 microdata directly
fb2 <- read_dta("upgrade-context/foodbook2v2.dta")
fb2 <- as.data.frame(fb2)

# Get variable labels from FB2
fb2_labels <- purrr::map_chr(fb2, ~ attr(.x, "label", exact = TRUE) %||% "")
fb2_label_df <- tibble::tibble(
  fb_var = names(fb2),
  fb_label = fb2_labels
) |>
  filter(fb_label != "")

message(sprintf("FB2 has %d labeled variables", nrow(fb2_label_df)))

# Normalize labels for matching
normalize_label <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9 ]", "")
  x <- str_squish(x)
  x
}

fb2_label_df$fb_label_norm <- normalize_label(fb2_label_df$fb_label)

# Handle NA in exposures exposure_en
exposures$exposure_en[is.na(exposures$exposure_en)] <- ""
exposures$exposure_en_norm <- normalize_label(exposures$exposure_en)

# Match by normalized label
matched <- list()

for (i in seq_len(nrow(exposures))) {
  exp_var <- exposures$variable_name[i]
  exp_label <- exposures$exposure_en[i]
  exp_label_norm <- exposures$exposure_en_norm[i]
  
  # Skip empty labels
  if (is.na(exp_label) || exp_label == "" || exp_label_norm == "") next
  
  # Skip if already a column name in FB2
  if (exp_var %in% names(fb2)) next
  
  # Try exact normalized match
  fb2_match <- fb2_label_df |>
    filter(fb_label_norm == exp_label_norm)
  
  if (nrow(fb2_match) == 1) {
    matched[[length(matched) + 1]] <- tibble::tibble(
      fb_var = fb2_match$fb_var[1],
      toolkit_var = exp_var,
      fb_label = fb2_match$fb_label[1],
      exp_label = exp_label,
      match_type = "exact"
    )
  } else if (nrow(fb2_match) == 0) {
    # Try fuzzy match (contains) - only if label is long enough
    if (nchar(exp_label_norm) >= 10) {
      fb2_match <- fb2_label_df |>
        filter(str_detect(fb_label_norm, fixed(exp_label_norm)) |
               str_detect(exp_label_norm, fixed(fb_label_norm)))
      
      if (nrow(fb2_match) == 1) {
        matched[[length(matched) + 1]] <- tibble::tibble(
          fb_var = fb2_match$fb_var[1],
          toolkit_var = exp_var,
          fb_label = fb2_match$fb_label[1],
          exp_label = exp_label,
          match_type = "fuzzy"
        )
      }
    }
  }
}

# Combine results
if (length(matched) > 0) {
  result <- dplyr::bind_rows(matched)
  
  # Remove duplicates (same fb_var mapped multiple times)
  result <- result |>
    group_by(fb_var) |>
    slice(1) |>
    ungroup()
  
  message(sprintf("\n=== Found %d new mappings ===\n", nrow(result)))
  
  # Print all mappings
  print(result |> select(fb_var, toolkit_var, fb_label), n = 200)
  
  # Save the complete map (combining with existing)
  existing <- read.csv("data/fb2_variable_map.csv", stringsAsFactors = FALSE)
  
  # Add new mappings
  new_map <- tibble::tibble(
    fb_var = result$fb_var,
    toolkit_var = result$toolkit_var
  )
  
  # Combine (existing takes priority)
  combined <- dplyr::bind_rows(existing, new_map) |>
    distinct(toolkit_var, .keep_all = TRUE)
  
  write.csv(combined, "data/fb2_variable_map_generated.csv", row.names = FALSE)
  message(sprintf("\nSaved %d mappings to data/fb2_variable_map_generated.csv", nrow(combined)))
} else {
  message("No matches found")
}
