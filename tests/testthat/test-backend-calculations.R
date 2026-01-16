# Unit tests for Foodbook backend calculation functions

test_that("fb_weighted_percent calculates correct weighted percentages", {
  # Create mock data: exposure coded as 1=yes, 2=no
  df <- data.frame(
    exposure_a = c(1, 1, 2, 2, 1, NA),
    weight = c(1, 2, 1, 1, 1, 1)
  )

  result <- fb_weighted_percent("exposure_a", df)

  # Yes responses (value=1): indices 1,2,5 with weights 1,2,1 = sum 4
  # All valid responses (1 or 2): indices 1-5 with weights 1,2,1,1,1 = sum 6
  # Percentage = (4/6)*100 = 66.67%
  expect_equal(round(result, 2), 66.67)
})

test_that("fb_weighted_percent handles edge cases", {
  # Missing exposure code
  df <- data.frame(exposure_a = c(1, 2), weight = c(1, 1))
  result1 <- fb_weighted_percent("nonexistent", df)
  expect_true(is.na(result1))

  # All NA values
  df_na <- data.frame(exposure_a = c(NA, NA, NA), weight = c(1, 1, 1))
  result2 <- fb_weighted_percent("exposure_a", df_na)
  expect_true(is.na(result2))

  # Zero weight total
  df_zero <- data.frame(exposure_a = c(1, 2), weight = c(0, 0))
  result3 <- fb_weighted_percent("exposure_a", df_zero)
  expect_true(is.na(result3))

  # Values outside 0/1/2 (should be treated as missing)
  df_invalid <- data.frame(exposure_a = c(1, 3, 4, 2), weight = c(1, 1, 1, 1))
  result4 <- fb_weighted_percent("exposure_a", df_invalid)
  # Only first (1) and last (2) are valid: yes=1, total=2, so 50%
  expect_equal(result4, 50)
})

test_that("fb_pt_map returns correct province/territory codes", {
  pt_map <- fb_pt_map()

  expect_type(pt_map, "integer")
  expect_equal(length(pt_map), 13)  # 13 provinces/territories
  expect_equal(pt_map["British Columbia"], c("British Columbia" = 1L))
  expect_equal(pt_map["Ontario"], c("Ontario" = 5L))
  expect_equal(pt_map["Nunavut"], c("Nunavut" = 13L))
  expect_true(all(names(pt_map) != ""))
})

test_that("fb_pt_abbrev_map returns correct abbreviations", {
  abbrev_map <- fb_pt_abbrev_map()

  expect_type(abbrev_map, "character")
  expect_equal(length(abbrev_map), 14)  # 13 PTs + Canada
  expect_equal(abbrev_map["British Columbia"], c("British Columbia" = "BC"))
  expect_equal(abbrev_map["Ontario"], c("Ontario" = "ON"))
  expect_equal(abbrev_map["Canada"], c("Canada" = "Canada"))
})

test_that("fb_normalize_pt_names handles French variants", {
  expect_equal(fb_normalize_pt_names(c("Qu\u00e9bec")), 6L)
  expect_equal(fb_normalize_pt_names(c("Nouvelle-\u00c9cosse")), 8L)
  expect_equal(fb_normalize_pt_names(c("Nouvelle-Ecosse")), 8L)
  expect_equal(
    fb_normalize_pt_names(c("\u00cele-du-Prince-\u00c9douard")),
    9L
  )
  expect_equal(
    fb_normalize_pt_names(c("Ile-du-Prince-Edouard")),
    9L
  )
})

test_that("fb_reference_percents skips toolkit fallback for multi-PT", {
  old_micro <- fb_env$micro
  old_micro_fb1 <- fb_env$micro_fb1
  old_data_source <- fb_env$data_source
  old_toolkit_proportions <- fb_env$toolkit_proportions

  fb_env$micro <- data.frame(weight = c(1, 1))
  fb_env$micro_fb1 <- NULL
  fb_env$data_source <- "Legacy"
  fb_env$toolkit_proportions <- data.frame(
    variable_name = "fake_code",
    exposure_number = "1",
    Canada = 12.3,
    ON = 9.9,
    QC = 10.1,
    stringsAsFactors = FALSE
  )

  res <- fb_reference_percents(
    codes = c("fake_code"),
    pt_names = c("Ontario", "Quebec")
  )
  expect_true(is.na(res[["fake_code"]]))

  fb_env$micro <- old_micro
  fb_env$micro_fb1 <- old_micro_fb1
  fb_env$data_source <- old_data_source
  fb_env$toolkit_proportions <- old_toolkit_proportions
})

test_that("fb_filter_micro filters by PT correctly", {
  # Create mock microdata
  mock_micro <- data.frame(
    PT = c(1, 1, 5, 5, 6, 6),  # BC, BC, ON, ON, QC, QC
    exposure_a = c(1, 2, 1, 2, 1, 2),
    weight = rep(1, 6)
  )

  # Temporarily set fb_env for testing and mark as initialized
  old_micro <- fb_env$micro
  old_pt_map <- fb_env$pt_map
  old_init <- fb_env$initialised

  fb_env$micro <- mock_micro
  fb_env$pt_map <- fb_pt_map()
  fb_env$initialised <- TRUE  # Prevent fb_init() from trying to load files

  # Filter for BC only
  result_bc <- fb_filter_micro(pt_names = "British Columbia")
  expect_equal(nrow(result_bc), 2)
  expect_true(all(result_bc$PT == 1))

  # Filter for ON and QC
  result_multi <- fb_filter_micro(pt_names = c("Ontario", "Quebec"))
  expect_equal(nrow(result_multi), 4)
  expect_true(all(result_multi$PT %in% c(5, 6)))

  # "Canada" should return all
  result_canada <- fb_filter_micro(pt_names = "Canada")
  expect_equal(nrow(result_canada), 6)

  # NULL or empty should return all
  result_null <- fb_filter_micro(pt_names = NULL)
  expect_equal(nrow(result_null), 6)

  # Restore original environment
  fb_env$micro <- old_micro
  fb_env$pt_map <- old_pt_map
  fb_env$initialised <- old_init
})

test_that("fb_filter_micro filters by age group correctly", {
  mock_micro <- data.frame(
    PT = rep(1, 6),
    AgeBand = c("0-9", "10-19", "20-64", "65+", "20-64", NA),
    exposure_a = rep(1, 6),
    weight = rep(1, 6)
  )

  old_micro <- fb_env$micro
  old_init <- fb_env$initialised
  fb_env$micro <- mock_micro
  fb_env$initialised <- TRUE

  # Filter for children only
  result_children <- fb_filter_micro(age_groups = "0-9")
  expect_equal(nrow(result_children), 1)
  expect_equal(result_children$AgeBand[1], "0-9")

  # Filter for multiple age groups
  result_multi <- fb_filter_micro(age_groups = c("20-64", "65+"))
  expect_equal(nrow(result_multi), 3)  # Includes NA
  expect_true(all(result_multi$AgeBand %in% c("20-64", "65+", NA)))

  fb_env$micro <- old_micro
  fb_env$initialised <- old_init
})

test_that("fb_filter_micro filters by month correctly", {
  mock_micro <- data.frame(
    PT = rep(1, 6),
    Month = c(1, 2, 6, 6, 12, NA),
    exposure_a = rep(1, 6),
    weight = rep(1, 6)
  )

  old_micro <- fb_env$micro
  old_init <- fb_env$initialised
  fb_env$micro <- mock_micro
  fb_env$initialised <- TRUE

  # Filter for summer months
  result_summer <- fb_filter_micro(months = c(6, 7, 8))
  expect_equal(nrow(result_summer), 2)
  expect_true(all(result_summer$Month %in% c(6)))

  # Filter for winter months
  result_winter <- fb_filter_micro(months = c(12, 1, 2))
  expect_equal(nrow(result_winter), 3)

  fb_env$micro <- old_micro
  fb_env$initialised <- old_init
})

test_that("fb_reference_percents_csv reads and processes CSV correctly", {
  # Create temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    Exposure = c("Raw vegetables", "Raw vegetables", "Fresh fruit", "Fresh fruit"),
    Province.Territory = c("Canada", "BC", "Canada", "ON"),
    Proportion = c(45.5, 50.0, 30.2, 28.5)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Temporarily change working directory or update function to accept path
  old_wd <- getwd()
  temp_dir <- tempdir()
  dir.create(file.path(temp_dir, "data"), showWarnings = FALSE)
  file.copy(temp_csv, file.path(temp_dir, "data", "foodbook_data.csv"), overwrite = TRUE)
  setwd(temp_dir)

  # Test Canada reference
  result_canada <- fb_reference_percents_csv(
    codes = c("Raw vegetables", "Fresh fruit"),
    pt_names = "Canada"
  )
  expect_equal(result_canada["Raw vegetables"], c("Raw vegetables" = 45.5))
  expect_equal(result_canada["Fresh fruit"], c("Fresh fruit" = 30.2))

  # Test PT-specific averaging
  result_pts <- fb_reference_percents_csv(
    codes = c("Raw vegetables"),
    pt_names = c("British Columbia", "Ontario")
  )
  # Should average BC (50.0) and ON (no data, so just BC)
  expect_true(!is.na(result_pts["Raw vegetables"]))

  # Test missing exposure
  result_missing <- fb_reference_percents_csv(
    codes = c("Nonexistent food"),
    pt_names = "Canada"
  )
  expect_true(is.na(result_missing["Nonexistent food"]))

  setwd(old_wd)
  unlink(temp_csv)
  unlink(file.path(temp_dir, "data", "foodbook_data.csv"))
})

test_that("fb_age_groups and fb_months return expected values", {
  # When microdata available
  old_micro <- fb_env$micro
  old_init <- fb_env$initialised
  fb_env$micro <- data.frame(Month = c(1, 6, 12), exposure_a = c(1, 2, 1), weight = c(1, 1, 1))
  fb_env$initialised <- TRUE

  age_groups <- fb_age_groups()
  expect_equal(age_groups, c("0-9", "10-19", "20-64", "65+"))

  months <- fb_months()
  expect_type(months, "character")
  expect_true(all(names(months) %in% month.name))

  # When microdata not available
  fb_env$micro <- NULL
  age_groups_empty <- fb_age_groups()
  expect_equal(length(age_groups_empty), 0)

  months_empty <- fb_months()
  expect_equal(length(months_empty), 0)

  fb_env$micro <- old_micro
  fb_env$initialised <- old_init
})
