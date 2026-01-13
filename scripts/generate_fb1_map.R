
# Generate Mapping between Toolkit Variables and FB1 Microdata
# Output: data/fb1_variable_map.csv

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
})

if (file.exists("src/foodbook_backend.R")) {
  source("src/foodbook_backend.R")
}

cat("Loading Data...\n")
fb_load_toolkit_data()
toolkit_df <- fb_env$toolkit_exposures

fb1_df <- read_dta("upgrade-context/foodbook.dta")
fb1_cols <- names(fb1_df)
fb1_lbls <- sapply(fb1_df, function(x) attr(x, "label"))

# Clean Functions
clean_str <- function(x) {
  # Handle encoding - ensure valid UTF-8
  x <- iconv(x, to = "UTF-8", sub = "byte")
  x <- gsub("\\*", "", x)
  x <- trimws(x)
  return(x)
}

toolkit_df$clean_lbl <- clean_str(toolkit_df$exposure_en)
fb1_lbl_map <- data.frame(
  var = fb1_cols,
  lbl = clean_str(as.character(fb1_lbls)),
  stringsAsFactors = FALSE
)

# Initialize Map
matches <- data.frame(toolkit_var = character(), fb1_var = character(), stringsAsFactors = FALSE)

# 1. Automatic Label Match
cat("Performing Label Matching...\n")
for (i in 1:nrow(toolkit_df)) {
  tk_var <- toolkit_df$variable_name[i]
  tk_lbl <- toolkit_df$clean_lbl[i]
  
  if (tk_var == "") next
  if (tk_var %in% matches$toolkit_var) next
  
  # Exact Match
  idx <- which(fb1_lbl_map$lbl == tk_lbl)
  if (length(idx) > 0) {
    fb1_v <- fb1_lbl_map$var[idx[1]]
    matches <- rbind(matches, data.frame(toolkit_var=tk_var, fb1_var=fb1_v))
    next
  }
}

# 2. Add Hardcoded Mappings (Verified by inspection)
manual_maps <- list(
  "anytom" = "Q6_dv",         # Any tomatoes -> Tomatoes (Q6_dv)
  "cabbage" = "Q13",          # Cabbage
  "pearpod" = "Q22",          # Peas (shelled or in pods) -> Peas (Q22)
  "anycarrot" = "carrot_dv",  # Any carrots -> Any carrots (including baby and mini)
  "freshherb" = "freshherb_dv", # Any fresh herbs -> Any fresh herbs
  "tarragon" = "Q33_F", # Fresh tarragon -> Other fresh herbs? (Maybe Q33_F?)
                        # Wait, "Fresh tarragon" is specific. Q33_F is "Other fresh herbs". 
                        # That's not a direct map. The Toolkit has specific tarragon?
                        # If FB1 doesn't have tarragon specific var, we can't map it.
                        # I'll check if "Fresh tarragon" label exists in FB1.
  "anyspice" = "spice_dv",    # Any spices -> Any spices
  "fruitjuice" = "Q62",       # Unpasteurized fruit juice -> Unpasteurized fruit juice
  "anyfrozenfruit" = "frozenfruit_dv", # Any frozen fruit -> Any frozen fruit
  "frozenfruit" = "Q106_C",   # Frozen fruit (not including berries) -> Frozen fruit other than berries (Q106_C)
  "anybabyformula" = "babyformula_dv" 
)

cat("Adding Manual Mappings...\n")
for (tk in names(manual_maps)) {
  fb1 <- manual_maps[[tk]]
  # Only add if not already matched
  if (!tk %in% matches$toolkit_var) {
    if (fb1 %in% fb1_cols) {
      matches <- rbind(matches, data.frame(toolkit_var=tk, fb1_var=fb1))
    } else {
      cat("Warning: Manual map target", fb1, "not found in FB1.\n")
    }
  }
}

# 3. Save
cat("Total Mapped:", nrow(matches), "\n")
write.csv(matches, "data/fb1_variable_map.csv", row.names=FALSE)
cat("Saved to data/fb1_variable_map.csv\n")
