# Comprehensive Analysis: Excel Toolkit vs OMD Stata Microdata
# ==============================================================================
# This script compares:
# 1. Exposure variable lists
# 2. Reference percentage values
# 3. Calculation methodologies
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
})

source("src/foodbook_backend.R")

# ==============================================================================
# PART 1: Load All Data Sources
# ==============================================================================

cat("Loading data sources...\n")

# 1.1 Excel Toolkit Data (Direct from Source)
# We read 'Table 6' which contains the reference data used by the tool
toolkit_path <- "data/Toolkit-binomial-probability-calculation-tool-2.0.xlsx"
if (requireNamespace("readxl", quietly = TRUE)) {
  # Read Table 6 - assuming Row 2 has headers (Provinces) and data starts Row 3
  # Based on HLOOKUP($P$1,'Table 6'!$B$2:$P$383,A6,FALSE)
  # Row 2 is likely the header row for HLOOKUP
  
  # Let's inspect headers first
  headers <- readxl::read_excel(toolkit_path, sheet = "Table 6", range = "B2:P2", col_names = FALSE)
  
  # Read full table
  # Range A2:P383 (Col A is ID, B-P are data)
  toolkit_data <- readxl::read_excel(toolkit_path, sheet = "Table 6", range = "A2:P383", col_names = TRUE)
  
  # Clean up column names
  # The first column is ID ('#'), second is 'Vegetables' etc.
  # We need to ensure we map columns correctly
  # Rename columns based on headers found in row 2
  colnames(toolkit_data)[2:ncol(toolkit_data)] <- as.character(headers[1,])
  colnames(toolkit_data)[1] <- "ID"
  
  # Determine Canada column
  # In headers, "Canada" should be present
  canada_col <- grep("Canada", colnames(toolkit_data), ignore.case = TRUE, value = TRUE)
  
  # Construct a clean toolkit exposures dataframe matching the format needed
  # We need to map row numbers or IDs to Variable Names used in the app
  # The App's 'exposures_bilingual.csv' was extracted from this file, so we can link by index or label
  
  # Load the mapping we extracted previously (to get Variable Names)
  fb_load_toolkit_data()
  map_df <- fb_env$toolkit_exposures
  
  # Match toolkit_data to map_df by Label (Column B in Table 6 seems to be English Label)
  # toolkit_data Col 2 is the Label
  names(toolkit_data)[2] <- "exposure_en"
  
  # Join
  toolkit_ref_clean <- toolkit_data %>%
    left_join(map_df %>% select(exposure_en, variable_name), by = "exposure_en") %>%
    filter(!is.na(variable_name))
    
  # Expose toolkit_exposures globally for later use
  toolkit_exposures <<- map_df
  toolkit_ref_values <<- toolkit_ref_clean
    
} else {
  stop("readxl package required")
}

cat("Toolkit Table 6 loaded:", nrow(toolkit_data), "rows\n")
cat("Matched with Variable Names:", nrow(toolkit_ref_clean), "rows\n")

# 1.2 OMD Stata Microdata
fb_init()
omd_microdata <- fb_env$micro
omd_labels <- fb_env$label_map

cat("OMD Microdata Respondents:", nrow(omd_microdata), "\n")
cat("OMD Labeled Exposures:", nrow(omd_labels), "\n")

# 1.3 Load individual Stata files for detailed inspection
fb1_raw <- read_dta("upgrade-context/foodbook.dta")
fb2_raw <- read_dta("upgrade-context/foodbook2v2.dta")

cat("FB1 Raw Variables:", ncol(fb1_raw), "\n")
cat("FB2 Raw Variables:", ncol(fb2_raw), "\n")

# ==============================================================================
# PART 2: Exposure List Comparison
# ==============================================================================

cat("\n=== EXPOSURE LIST ANALYSIS ===\n")

# 2.1 Identify which Toolkit variables exist in which dataset
toolkit_vars <- toolkit_exposures$variable_name[toolkit_exposures$variable_name != ""]

# Check in merged OMD data
in_merged <- toolkit_vars %in% names(omd_microdata)

# Check in FB1 raw (using variable labels to map)
fb1_labels <- sapply(fb1_raw, function(x) attr(x, "label"))
fb1_cols <- names(fb1_raw)

# Check in FB2 raw
fb2_labels <- sapply(fb2_raw, function(x) attr(x, "label"))
fb2_cols <- names(fb2_raw)

exposure_summary <- data.frame(
  toolkit_var = toolkit_vars,
  in_merged_omd = in_merged,
  stringsAsFactors = FALSE
)

# Try to find matches in FB1 and FB2 by label matching
for (i in seq_len(nrow(exposure_summary))) {
  tk_var <- exposure_summary$toolkit_var[i]
  tk_label <- toolkit_exposures$exposure_en[toolkit_exposures$variable_name == tk_var][1]
  
  # Clean label for matching - handle encoding issues
  tk_label_clean <- tryCatch({
    tk_label_clean <- iconv(tk_label, to = "UTF-8", sub = "byte")
    trimws(gsub("\\*", "", tk_label_clean))
  }, error = function(e) {
    trimws(gsub("\\*", "", as.character(tk_label)))
  })
  
  # Check FB1 - handle encoding
  fb1_labels_clean <- sapply(fb1_labels, function(x) {
    tryCatch({
      iconv(as.character(x), to = "UTF-8", sub = "byte")
    }, error = function(e) as.character(x))
  })
  
  fb1_match <- which(tolower(trimws(fb1_labels_clean)) == tolower(tk_label_clean))
  exposure_summary$in_fb1_raw[i] <- length(fb1_match) > 0
  if (length(fb1_match) > 0) {
    exposure_summary$fb1_varname[i] <- fb1_cols[fb1_match[1]]
  }
  
  # Check FB2 - handle encoding
  fb2_labels_clean <- sapply(fb2_labels, function(x) {
    tryCatch({
      iconv(as.character(x), to = "UTF-8", sub = "byte")
    }, error = function(e) as.character(x))
  })
  
  fb2_match <- which(tolower(trimws(fb2_labels_clean)) == tolower(tk_label_clean))
  exposure_summary$in_fb2_raw[i] <- length(fb2_match) > 0
  if (length(fb2_match) > 0) {
    exposure_summary$fb2_varname[i] <- fb2_cols[fb2_match[1]]
  }
}

# Categorize exposures
exposure_summary <- exposure_summary %>%
  mutate(
    source_category = case_when(
      in_merged_omd ~ "In Merged OMD (FB1+FB2)",
      in_fb2_raw & !in_merged_omd ~ "FB2 Only (Dropped in Merge)",
      in_fb1_raw & !in_fb2_raw ~ "FB1 Only (Legacy)",
      in_fb1_raw & in_fb2_raw ~ "Both FB1 & FB2 (Collision)",
      TRUE ~ "Missing from All OMD Data"
    )
  )

cat("\nExposure Distribution by Source:\n")
print(table(exposure_summary$source_category))

# ==============================================================================
# PART 3: Reference Percentage Comparison
# ==============================================================================

cat("\n=== REFERENCE PERCENTAGE ANALYSIS ===\n")

# 3.1 Compare Canada-level reference values
canada_comparison <- data.frame(
  variable_name = character(),
  toolkit_canada_pct = numeric(),
  omd_weighted_pct = numeric(),
  difference = numeric(),
  pct_difference = numeric(),
  stringsAsFactors = FALSE
)

# Get ALL exposures that exist in both sources (no sampling)
sample_vars <- exposure_summary$toolkit_var[exposure_summary$in_merged_omd]
cat("Comparing", length(sample_vars), "exposures that exist in both Toolkit and OMD...\n")

for (var in sample_vars) {
  # Toolkit value
  tk_val <- toolkit_ref_values$Canada[toolkit_ref_values$variable_name == var]
  if (length(tk_val) == 0) next
  
  # OMD weighted value
  omd_val <- fb_reference_percents(var, pt_names = "Canada")[1]
  
  if (!is.na(tk_val) && !is.na(omd_val)) {
    canada_comparison <- rbind(canada_comparison, data.frame(
      variable_name = var,
      toolkit_canada_pct = as.numeric(tk_val),
      omd_weighted_pct = omd_val,
      difference = omd_val - as.numeric(tk_val),
      pct_difference = ((omd_val - as.numeric(tk_val)) / as.numeric(tk_val)) * 100,
      stringsAsFactors = FALSE
    ))
  }
}

cat("\nCanada Reference Value Comparison (Sample of", nrow(canada_comparison), "exposures):\n")
cat("Average Absolute Difference:", mean(abs(canada_comparison$difference), na.rm = TRUE), "%\n")
cat("Average % Difference:", mean(abs(canada_comparison$pct_difference), na.rm = TRUE), "%\n")
cat("Max Difference:", max(abs(canada_comparison$difference), na.rm = TRUE), "%\n")

# Identify largest discrepancies
top_discrepancies <- canada_comparison %>%
  arrange(desc(abs(difference))) %>%
  head(10)

cat("\nTop 10 Largest Discrepancies:\n")
print(top_discrepancies[, c("variable_name", "toolkit_canada_pct", "omd_weighted_pct", "difference")])

# ==============================================================================
# PART 4: Methodology Analysis
# ==============================================================================

cat("\n=== METHODOLOGY COMPARISON ===\n")

# 4.1 Weighting
cat("\nWeighting Approach:\n")
cat("- Toolkit: Static proportions (pre-calculated, unknown weighting)\n")
cat("- OMD: Survey weights applied dynamically\n")
cat("  - FB1 weight: EXPWEIGHT_CMA2\n")
cat("  - FB2 weight: proj_weight_non_traveller\n")
cat("  - Merged: Uses FB1 weight where available, else FB2 weight\n")

# 4.2 Sample comparison
cat("\nSample Size Information:\n")
cat("- FB1 Respondents:", nrow(fb1_raw), "\n")
cat("- FB2 Respondents:", nrow(fb2_raw), "\n")
cat("- Merged OMD:", nrow(omd_microdata), "\n")

# 4.3 Check demographic variables
cat("\nDemographic Variables:\n")
cat("FB1:")
cat("\n  - Province: QINTRO3\n")
cat("  - Age: age_grp4_dv (1=0-9, 2=10-19, 3=20-64, 4=65+)\n")
cat("  - Month: month_dv\n")
cat("  - Weight: EXPWEIGHT_CMA2\n")

cat("\nFB2:")
cat("\n  - Province: PT\n")
cat("  - Age: Age_grp_dv\n")
cat("  - Month: month_dv\n")
cat("  - Weight: proj_weight_non_traveller\n")

# ==============================================================================
# PART 5: Export Results
# ==============================================================================

cat("\n=== EXPORTING RESULTS ===\n")

write.csv(exposure_summary, "toolkit_vs_omd_exposure_summary.csv", row.names = FALSE)
write.csv(canada_comparison, "toolkit_vs_omd_canada_comparison.csv", row.names = FALSE)

cat("Exported:\n")
cat("  - toolkit_vs_omd_exposure_summary.csv\n")
cat("  - toolkit_vs_omd_canada_comparison.csv\n")
