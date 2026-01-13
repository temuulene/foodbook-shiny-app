
test_that("FB1 Legacy Data Loading", {
  # 1. Test Load Function
  df <- fb_load_fb1_legacy()
  expect_true(!is.null(df))
  expect_true("PT" %in% names(df))
  expect_true("Month" %in% names(df))
  expect_true("AgeBand" %in% names(df))
  expect_true("weight" %in% names(df))
  
  # Check if Q6_dv (Tomatoes) exists
  expect_true("Q6_dv" %in% names(df))
})

test_that("FB1 Integration in Backend", {
  # 2. Test Init
  fb_init()
  expect_true(!is.null(fb_env$micro_fb1))
  expect_true(!is.null(fb_env$fb1_map))
  
  # 3. Test Exposure Mapping
  # "anytom" maps to "Q6_dv"
  # Should return a numeric percentage, not NA (unless insufficient data)
  # anytom is FB1 only, so it should query micro_fb1 via map
  
  # Mock environment if needed? No, use real data if available
  # If data not available in test env, skip
  if (is.null(fb_env$micro_fb1)) skip("FB1 data not loaded")
  
  res <- fb_reference_percents(c("anytom"), pt_names = "Canada")
  expect_true(!is.na(res["anytom"]))
  expect_true(res["anytom"] > 0)
  
  # Test with Filter
  # Filter by Age "0-9" (should work)
  res_age <- fb_reference_percents(c("anytom"), pt_names = "Canada", age_groups = "0-9")
  expect_true(!is.na(res_age["anytom"]))
  
  # Filter by Month "1" (January) - API expects codes/integers not names
  res_month <- fb_reference_percents(c("anytom"), pt_names = "Canada", months = "1")
  expect_true(!is.na(res_month["anytom"]))
})
