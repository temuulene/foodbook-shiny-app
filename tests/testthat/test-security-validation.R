# Tests for security validation and input sanitization

# --- fb_public_sanitize_count ---

test_that("fb_public_sanitize_count handles NULL and NA", {
  expect_equal(fb_public_sanitize_count(NULL), 0L)
  expect_equal(fb_public_sanitize_count(NA), 0L)
  expect_equal(fb_public_sanitize_count(NA_real_), 0L)
})

test_that("fb_public_sanitize_count clamps to valid range", {
  expect_equal(fb_public_sanitize_count(-5), 0L)
  expect_equal(fb_public_sanitize_count(0), 0L)
  expect_equal(fb_public_sanitize_count(100), 100L)
  expect_equal(fb_public_sanitize_count(10000), 10000L)
  expect_equal(fb_public_sanitize_count(99999), 10000L)
  expect_equal(fb_public_sanitize_count(1e9), 10000L)
})

test_that("fb_public_sanitize_count floors decimal values", {
  expect_equal(fb_public_sanitize_count(5.7), 5L)
  expect_equal(fb_public_sanitize_count(5.2), 5L)
})

test_that("fb_public_sanitize_count respects custom max_val", {
  expect_equal(fb_public_sanitize_count(200, max_val = 100L), 100L)
  expect_equal(fb_public_sanitize_count(50, max_val = 100L), 50L)
})

test_that("fb_public_sanitize_count handles non-numeric input", {
  expect_equal(fb_public_sanitize_count("abc"), 0L)
  expect_equal(fb_public_sanitize_count("5"), 5L)
})

# --- classify_exposure edge cases ---

test_that("classify_exposure handles boundary ref_prop values", {
  # ref_prop = NA -> No Reference Value
  expect_equal(
    classify_exposure(0.03, 0.8, NA_real_),
    "No Reference Value"
  )
  # p_value = NA -> Insufficient Data
  expect_equal(
    classify_exposure(NA_real_, 0.8, 50),
    "Insufficient Data"
  )
  # Both NA
  expect_equal(
    classify_exposure(NA_real_, NA_real_, NA_real_),
    "No Reference Value"
  )
})

test_that("classify_exposure is vectorized", {
  result <- classify_exposure(
    c(0.03, 0.08, 0.5, NA_real_, 0.03),
    c(0.8, 0.8, 0.3, 0.5, 0.8),
    c(50, 50, 50, 50, NA_real_)
  )
  expect_equal(result, c(
    "Alert", "Borderline", "Not Significant",
    "Insufficient Data", "No Reference Value"
  ))
})

test_that("classify_exposure uses constant thresholds", {
  # Exactly at 0.05 -> Alert

  expect_equal(
    classify_exposure(0.05, 0.8, 50),
    "Alert"
  )
  # Exactly at 0.10 -> Borderline
  expect_equal(
    classify_exposure(0.10, 0.8, 50),
    "Borderline"
  )
  # Just above 0.10 -> Not Significant
  expect_equal(
    classify_exposure(0.11, 0.8, 50),
    "Not Significant"
  )
})

# --- fb_normalize_filters ---

test_that("fb_normalize_filters handles Canada and defaults", {
  f <- fb_normalize_filters("Canada", "All Ages", "All Months")
  expect_null(f$pt)
  expect_null(f$age)
  expect_null(f$month)
})

test_that("fb_normalize_filters converts string months to integer", {
  f <- fb_normalize_filters("Ontario", "0-9", c("1", "6"))
  expect_equal(f$pt, "Ontario")
  expect_equal(f$age, "0-9")
  expect_equal(f$month, c(1L, 6L))
})

test_that("fb_normalize_filters handles NULL inputs", {
  f <- fb_normalize_filters(NULL, NULL, NULL)
  expect_null(f$pt)
  expect_null(f$age)
  expect_null(f$month)
})

# --- fb_public_collect_exposure_inputs ---

test_that("fb_public_collect_exposure_inputs returns tibble for empty input", {
  result <- fb_public_collect_exposure_inputs(character(), list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# --- fb_resolve_exposure_label HTML escaping ---

test_that("fb_resolve_exposure_label escapes unknown codes", {
  label_map <- character()
  result <- fb_resolve_exposure_label(
    "<script>alert('xss')</script>",
    "en",
    label_map
  )
  expect_false(grepl("<script>", result, fixed = TRUE))
})

# --- Constants are loaded ---

test_that("constants are defined", {
  expect_true(exists("FB_MAX_COUNT"))
  expect_true(exists("FB_MIN_SAMPLE_SIZE"))
  expect_true(exists("FB_P_VALUE_ALERT"))
  expect_true(exists("FB_P_VALUE_BORDERLINE"))
  expect_true(exists("FB_DEBOUNCE_MS"))
  expect_true(exists("FB_MAX_UPLOAD_BYTES"))
  expect_true(exists("FB_CLASSIFICATION_LEVELS"))

  expect_equal(FB_MAX_COUNT, 10000L)
  expect_equal(FB_MIN_SAMPLE_SIZE, 5L)
  expect_equal(FB_P_VALUE_ALERT, 0.05)
  expect_equal(FB_P_VALUE_BORDERLINE, 0.10)
  expect_equal(FB_MAX_UPLOAD_BYTES, 10L * 1024L * 1024L)
})
