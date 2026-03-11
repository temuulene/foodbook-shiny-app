# Tests for mod_ref_settings module
# Tests the default selection toggle logic (fb_public_resolve_default_selection)
# and helper functions used by the reference settings module

library(testthat)

# =============================================================================
# fb_public_resolve_default_selection()
# =============================================================================

test_that("resolve_default_selection: selecting Canada removes other PTs", {
  # User had "ON" selected, then adds "Canada"
  result <- fb_public_resolve_default_selection(
    selected = c("ON", "Canada"),
    default_value = "Canada",
    previous = "ON"
  )
  expect_equal(result, "Canada")
})

test_that("resolve_default_selection: selecting a PT removes Canada", {
  # User had "Canada" selected, then adds "ON"
  result <- fb_public_resolve_default_selection(
    selected = c("Canada", "ON"),
    default_value = "Canada",
    previous = "Canada"
  )
  expect_equal(result, "ON")
})

test_that("resolve_default_selection: single non-default stays unchanged", {
  result <- fb_public_resolve_default_selection(
    selected = "ON",
    default_value = "Canada",
    previous = "ON"
  )
  expect_equal(result, "ON")
})

test_that("resolve_default_selection: Canada alone stays unchanged", {
  result <- fb_public_resolve_default_selection(
    selected = "Canada",
    default_value = "Canada",
    previous = "Canada"
  )
  expect_equal(result, "Canada")
})

test_that("resolve_default_selection: NULL returns NULL", {
  result <- fb_public_resolve_default_selection(
    selected = NULL,
    default_value = "Canada",
    previous = "Canada"
  )
  expect_null(result)
})

test_that("resolve_default_selection: works for All Ages default", {
  result <- fb_public_resolve_default_selection(
    selected = c("All Ages", "20-64"),
    default_value = "All Ages",
    previous = "All Ages"
  )
  expect_equal(result, "20-64")
})

test_that("resolve_default_selection: adding All Ages removes specific ages", {
  result <- fb_public_resolve_default_selection(
    selected = c("20-64", "All Ages"),
    default_value = "All Ages",
    previous = "20-64"
  )
  expect_equal(result, "All Ages")
})

test_that("resolve_default_selection: works for All Months default", {
  result <- fb_public_resolve_default_selection(
    selected = c("All Months", "6"),
    default_value = "All Months",
    previous = "All Months"
  )
  expect_equal(result, "6")
})

test_that("resolve_default_selection: multiple PTs without default stay unchanged", {
  result <- fb_public_resolve_default_selection(
    selected = c("ON", "BC", "QC"),
    default_value = "Canada",
    previous = c("ON", "BC")
  )
  expect_equal(result, c("ON", "BC", "QC"))
})

# =============================================================================
# fb_normalize_filters()
# =============================================================================

test_that("normalize_filters: Canada becomes NULL pt", {
  result <- fb_normalize_filters("Canada", "All Ages", "All Months")
  expect_null(result$pt)
  expect_null(result$age)
  expect_null(result$month)
})

test_that("normalize_filters: specific PT is preserved", {
  result <- fb_normalize_filters("ON", "20-64", "6")
  expect_equal(result$pt, "ON")
  expect_equal(result$age, "20-64")
  expect_equal(result$month, 6L)
})

test_that("normalize_filters: NULL province becomes NULL", {
  result <- fb_normalize_filters(NULL, NULL, NULL)
  expect_null(result$pt)
  expect_null(result$age)
  expect_null(result$month)
})
