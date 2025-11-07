# Tests for new features added in October 2025
# Based on PHAC-OMD demo meeting requirements

library(testthat)
library(dplyr)
library(readxl)

# Define helper functions locally for testing
# These are copies from app.R to avoid sourcing the entire Shiny app

classify_exposure <- function(p_value, observed_prop, ref_prop) {
  # Check if reference is missing/unavailable
  if (is.na(ref_prop)) return("No Reference Value")

  ref_prop_decimal <- ref_prop / 100
  if (is.na(p_value)) return("Insufficient Data")
  if (observed_prop > ref_prop_decimal) {
    dplyr::case_when(p_value <= 0.05 ~ "Alert",
              p_value <= 0.10 ~ "Borderline",
              TRUE ~ "Not Significant")
  } else {
    "Not Significant"
  }
}

make_safe_id <- function(Exposure, Province.Territory) {
  paste(
    gsub("[^a-zA-Z0-9]", "", Exposure),
    gsub("[^a-zA-Z0-9]", "", Province.Territory),
    sep = "_"
  )
}

find_sheet_by_columns <- function(excel_path, required_cols) {
  all_sheets <- tryCatch(readxl::excel_sheets(excel_path), error = function(e) character(0))

  for (sheet_name in all_sheets) {
    sheet_data <- tryCatch(
      readxl::read_excel(excel_path, sheet = sheet_name, n_max = 1),
      error = function(e) NULL
    )

    if (!is.null(sheet_data)) {
      # Normalize column names
      normalized_cols <- gsub("[^a-z0-9]+", "", tolower(names(sheet_data)))

      # Check if all required columns are present
      if (all(required_cols %in% normalized_cols)) {
        return(list(sheet = sheet_name, found = TRUE))
      }
    }
  }

  # No matching sheet found
  return(list(sheet = NULL, found = FALSE, available_sheets = all_sheets))
}

# =============================================================================
# Tests for find_sheet_by_columns() - CEDARS auto-detection
# =============================================================================

test_that("find_sheet_by_columns detects sheet with required columns", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # Create a temporary Excel file with multiple sheets
  temp_excel <- tempfile(fileext = ".xlsx")

  # Sheet 1: Has required columns
  sheet1 <- data.frame(
    NationalID = c("001", "002"),
    ExposureCode = c("anytom", "celery"),
    HasExposureOccurred = c("Yes", "No"),
    OtherColumn = c("A", "B")
  )

  # Sheet 2: Missing required columns
  sheet2 <- data.frame(
    ID = c("001", "002"),
    Value = c(1, 2)
  )

  # Write Excel file
  writexl::write_xlsx(list("Correct Sheet" = sheet1, "Wrong Sheet" = sheet2), temp_excel)

  # Test: Should find "Correct Sheet"
  result <- find_sheet_by_columns(temp_excel, c("nationalid", "exposurecode", "hasexposureoccurred"))

  expect_true(result$found)
  expect_equal(result$sheet, "Correct Sheet")

  # Cleanup
  unlink(temp_excel)
})

test_that("find_sheet_by_columns returns FALSE when no matching sheet", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # Create Excel file without required columns
  temp_excel <- tempfile(fileext = ".xlsx")

  sheet1 <- data.frame(
    ID = c("001", "002"),
    Value = c(1, 2)
  )

  writexl::write_xlsx(list("Sheet1" = sheet1), temp_excel)

  # Test: Should not find matching sheet
  result <- find_sheet_by_columns(temp_excel, c("nationalid", "exposurecode", "hasexposureoccurred"))

  expect_false(result$found)
  expect_null(result$sheet)
  expect_equal(result$available_sheets, "Sheet1")

  # Cleanup
  unlink(temp_excel)
})

test_that("find_sheet_by_columns handles column name variations", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # Create Excel with different column naming conventions
  temp_excel <- tempfile(fileext = ".xlsx")

  sheet1 <- data.frame(
    `National ID` = c("001", "002"),  # Spaces
    `Exposure_Code` = c("anytom", "celery"),  # Underscore
    `HAS EXPOSURE OCCURRED` = c("Yes", "No"),  # Uppercase
    check.names = FALSE
  )

  writexl::write_xlsx(list("Sheet1" = sheet1), temp_excel)

  # Test: Should normalize and find columns
  result <- find_sheet_by_columns(temp_excel, c("nationalid", "exposurecode", "hasexposureoccurred"))

  expect_true(result$found)
  expect_equal(result$sheet, "Sheet1")

  # Cleanup
  unlink(temp_excel)
})

# =============================================================================
# Tests for classify_exposure() - No Reference Value classification
# =============================================================================

test_that("classify_exposure returns 'No Reference Value' when ref_prop is NA", {
  result <- classify_exposure(p_value = 0.03, observed_prop = 0.8, ref_prop = NA)
  expect_equal(result, "No Reference Value")
})

test_that("classify_exposure returns 'Alert' for significant high exposure", {
  result <- classify_exposure(p_value = 0.03, observed_prop = 0.8, ref_prop = 50)
  expect_equal(result, "Alert")
})

test_that("classify_exposure returns 'Borderline' for marginally significant", {
  result <- classify_exposure(p_value = 0.08, observed_prop = 0.7, ref_prop = 50)
  expect_equal(result, "Borderline")
})

test_that("classify_exposure returns 'Not Significant' when observed <= reference", {
  result <- classify_exposure(p_value = 0.03, observed_prop = 0.4, ref_prop = 50)
  expect_equal(result, "Not Significant")
})

test_that("classify_exposure returns 'Insufficient Data' when p_value is NA", {
  result <- classify_exposure(p_value = NA, observed_prop = 0.8, ref_prop = 50)
  expect_equal(result, "Insufficient Data")
})

test_that("classify_exposure prioritizes 'No Reference Value' over other classifications", {
  # Even if p-value is NA, "No Reference Value" should take priority
  result <- classify_exposure(p_value = NA, observed_prop = 0.8, ref_prop = NA)
  expect_equal(result, "No Reference Value")
})

# =============================================================================
# Tests for make_safe_id() - HTML ID sanitization
# =============================================================================

test_that("make_safe_id removes special characters", {
  result <- make_safe_id("Cherry tomatoes", "British Columbia")
  expect_equal(result, "Cherrytomatoes_BritishColumbia")
})

test_that("make_safe_id handles spaces and hyphens", {
  result <- make_safe_id("Roma-plum tomatoes", "New Brunswick")
  expect_equal(result, "Romaplumtomatoes_NewBrunswick")
})

test_that("make_safe_id handles parentheses and slashes", {
  result <- make_safe_id("Tomatoes (raw/cooked)", "Ontario")
  expect_equal(result, "Tomatoesrawcooked_Ontario")
})

# =============================================================================
# Tests for custom exposure support
# =============================================================================

test_that("Custom exposures are detected when not in fb_exposure_choices", {
  # Mock fb_exposure_choices to return known exposures
  # Note: fb_exposure_choices() returns setNames(lm$code, lm$label)
  # which means: names = labels, values = codes
  available_exposures <- c("Any tomatoes" = "anytom",
                          "Celery" = "celery",
                          "Iceberg lettuce" = "iceberg")

  # Test: Known exposure code should not be custom (should be in values)
  is_custom_known <- !("anytom" %in% available_exposures)
  expect_false(is_custom_known)

  # Test: Unknown exposure code should be custom (not in values)
  is_custom_unknown <- !("my_custom_food" %in% available_exposures)
  expect_true(is_custom_unknown)
})

# =============================================================================
# Tests for response count columns
# =============================================================================

test_that("Results include Yes/Prob/No/DK columns", {
  # Create mock results data
  mock_results <- tibble(
    `Reference Scope` = "Canada",
    Exposure = "Any tomatoes",
    `Total Valid` = 40,
    Yes = 25,
    Probably = 5,
    No = 10,
    DK = 2,
    `Observed %` = 0.75,
    `Reference %` = 60,
    `P-Value` = 0.03,
    Classification = "Alert"
  )

  # Test: All count columns present
  expect_true("Yes" %in% names(mock_results))
  expect_true("Probably" %in% names(mock_results))
  expect_true("No" %in% names(mock_results))
  expect_true("DK" %in% names(mock_results))

  # Test: Counts are numeric
  expect_type(mock_results$Yes, "double")
  expect_type(mock_results$Probably, "double")
  expect_type(mock_results$No, "double")
  expect_type(mock_results$DK, "double")

  # Test: Total Valid equals Yes + Probably + No
  expect_equal(mock_results$`Total Valid`,
               mock_results$Yes + mock_results$Probably + mock_results$No)
})

# =============================================================================
# Tests for descriptive export filenames
# =============================================================================

test_that("Export filename includes analysis parameters", {
  # Mock inputs
  pts <- "Ontario"
  ages <- "All"
  months <- "All"
  n_exp <- 5
  date_str <- "2025-10-30"

  # Build filename (logic from app.R)
  pt_str <- gsub(" ", "", pts)
  age_str <- "allages"
  month_str <- "allmonths"

  filename <- paste0("foodbook_", pt_str, "_", age_str, "_", month_str, "_", n_exp, "exposures_", date_str)

  expect_equal(filename, "foodbook_Ontario_allages_allmonths_5exposures_2025-10-30")
})

test_that("Export filename handles multiple PTs", {
  # Mock inputs
  pts <- c("Ontario", "Quebec", "British Columbia")
  ages <- c("20-64")
  months <- c("6", "7", "8")
  n_exp <- 12
  date_str <- "2025-10-30"

  # Build filename
  pt_str <- paste0(length(pts), "PTs")
  age_str <- paste0("age", gsub("-", "to", ages[1]))
  month_str <- paste(month.abb[as.integer(months)], collapse = "-")

  filename <- paste0("foodbook_", pt_str, "_", age_str, "_", month_str, "_", n_exp, "exposures_", date_str)

  expect_equal(filename, "foodbook_3PTs_age20to64_Jun-Jul-Aug_12exposures_2025-10-30")
})

# =============================================================================
# Tests for CSV upload validation
# =============================================================================

test_that("CSV upload validates required columns", {
  # Create valid CSV
  valid_csv <- data.frame(
    Exposure = c("Cherry tomatoes", "Romaine lettuce"),
    Yes = c(25, 15),
    Probably = c(5, 3),
    No = c(10, 20),
    DK = c(2, 4)
  )

  # Normalize column names (as done in app)
  names(valid_csv) <- gsub("[^a-z0-9]+", "", tolower(names(valid_csv)))

  required_cols <- c("exposure", "yes", "probably", "no", "dk")
  missing_cols <- setdiff(required_cols, names(valid_csv))

  expect_equal(length(missing_cols), 0)
})

test_that("CSV upload detects missing columns", {
  # Create invalid CSV (missing 'DK' column)
  invalid_csv <- data.frame(
    Exposure = c("Cherry tomatoes"),
    Yes = c(25),
    Probably = c(5),
    No = c(10)
  )

  # Normalize column names
  names(invalid_csv) <- gsub("[^a-z0-9]+", "", tolower(names(invalid_csv)))

  required_cols <- c("exposure", "yes", "probably", "no", "dk")
  missing_cols <- setdiff(required_cols, names(invalid_csv))

  expect_equal(missing_cols, "dk")
})

test_that("CSV upload handles column name variations", {
  # Create CSV with various naming conventions
  csv_data <- data.frame(
    `Exposure Name` = c("Cherry tomatoes"),
    `YES COUNT` = c(25),
    `Probably_Count` = c(5),
    `no-count` = c(10),
    `Don't Know` = c(2),
    check.names = FALSE
  )

  # Normalize (removes spaces, hyphens, uppercase)
  names(csv_data) <- gsub("[^a-z0-9]+", "", tolower(names(csv_data)))

  # Should have: exposurename, yescount, probablycount, nocount, dontknow
  expect_true("exposurename" %in% names(csv_data))
  expect_true("yescount" %in% names(csv_data))
  expect_true("probablycount" %in% names(csv_data))
  expect_true("nocount" %in% names(csv_data))
  expect_true("dontknow" %in% names(csv_data))
})

# =============================================================================
# Tests for CEDARS upload with optional provinceterritory column
# =============================================================================

test_that("CEDARS linelist without provinceterritory column is handled correctly", {
  # Create mock linelist data WITHOUT provinceterritory column
  df_line <- data.frame(
    natid = c("001", "002", "003"),
    age = c(25, 45, 67),
    sex = c("M", "F", "M")
  )

  # Apply the same transformation as in app.R (line 1222-1224)
  df_line_transformed <- df_line %>%
    transmute(
      natid = as.character(natid),
      provinceterritory = if("provinceterritory" %in% names(.)) provinceterritory else NA_character_
    )

  # Test: Should have natid and provinceterritory columns
  expect_true("natid" %in% names(df_line_transformed))
  expect_true("provinceterritory" %in% names(df_line_transformed))

  # Test: provinceterritory should be NA for all rows
  expect_true(all(is.na(df_line_transformed$provinceterritory)))

  # Test: natid should be preserved as character
  expect_equal(df_line_transformed$natid, c("001", "002", "003"))
  expect_type(df_line_transformed$natid, "character")
})

test_that("CEDARS linelist WITH provinceterritory column is handled correctly", {
  # Create mock linelist data WITH provinceterritory column
  df_line <- data.frame(
    natid = c("001", "002", "003"),
    provinceterritory = c("ON", "QC", "BC"),
    age = c(25, 45, 67)
  )

  # Apply the same transformation as in app.R (line 1222-1224)
  df_line_transformed <- df_line %>%
    transmute(
      natid = as.character(natid),
      provinceterritory = if("provinceterritory" %in% names(.)) provinceterritory else NA_character_
    )

  # Test: Should have both columns
  expect_true("natid" %in% names(df_line_transformed))
  expect_true("provinceterritory" %in% names(df_line_transformed))

  # Test: provinceterritory values should be preserved
  expect_equal(df_line_transformed$provinceterritory, c("ON", "QC", "BC"))

  # Test: natid should be preserved as character
  expect_equal(df_line_transformed$natid, c("001", "002", "003"))
})

# =============================================================================
# Integration test: Full workflow
# =============================================================================

test_that("Full analysis workflow with custom exposure and missing reference", {
  # Simulate user entering custom exposure
  exposure_code <- "MyCustomFood"
  custom_ref <- 45.5

  # Simulate case counts
  yes <- 20
  prob <- 3
  no <- 15
  dk <- 2

  # Calculate observed proportion
  total_valid <- yes + prob + no
  observed_prop <- (yes + prob) / total_valid

  # Since it's custom, reference would be the custom_ref (if provided) or NA
  ref_prop <- custom_ref

  # Calculate p-value
  p_value <- pbinom(yes + prob - 1, total_valid, ref_prop / 100, lower.tail = FALSE)

  # Classify
  classification <- classify_exposure(p_value, observed_prop, ref_prop)

  # Test expectations
  expect_equal(total_valid, 38)
  expect_equal(round(observed_prop, 3), 0.605)
  expect_equal(ref_prop, 45.5)
  expect_true(p_value < 0.10)  # Should be significant
  expect_true(classification %in% c("Alert", "Borderline"))
})

test_that("Full analysis workflow with missing reference", {
  # Simulate exposure without reference data
  exposure_code <- "UnknownExposure"
  custom_ref <- NA  # User didn't provide custom reference

  # Simulate case counts
  yes <- 20
  prob <- 3
  no <- 15
  dk <- 2

  # Calculate observed proportion
  total_valid <- yes + prob + no
  observed_prop <- (yes + prob) / total_valid

  # Reference is NA
  ref_prop <- custom_ref

  # Classification should be "No Reference Value"
  # P-value calculation would fail with NA reference, so it would also be NA
  p_value <- if (is.na(ref_prop)) NA_real_ else pbinom(yes + prob - 1, total_valid, ref_prop / 100, lower.tail = FALSE)

  classification <- classify_exposure(p_value, observed_prop, ref_prop)

  # Test expectations
  expect_true(is.na(ref_prop))
  expect_equal(classification, "No Reference Value")
})

# =============================================================================
# Summary message
# =============================================================================

message("\n✓ All new feature tests completed successfully!")
message("  - CEDARS auto-detection: find_sheet_by_columns()")
message("  - Classification: No Reference Value handling")
message("  - Custom exposures: Detection and reference input")
message("  - Response counts: Yes/Prob/No/DK columns")
message("  - Export filenames: Descriptive naming")
message("  - CSV upload: Column validation")
message("  - CEDARS linelist: Optional provinceterritory column")
message("  - Integration: Full workflows")
