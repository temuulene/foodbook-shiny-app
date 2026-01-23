test_that("fb_public_collect_exposure_inputs sanitizes values and keeps custom", {
  if (!exists("fb_public_collect_exposure_inputs")) {
    fail("fb_public_collect_exposure_inputs not implemented")
    return()
  }

  input_values <- list(
    "exp_A1-yes" = 2.9,
    "exp_A1-prob" = -1,
    "exp_A1-no" = 3.2,
    "exp_A1-dk" = NA_real_
  )

  res <- fb_public_collect_exposure_inputs(c("A-1"), input_values)

  expect_true("custom" %in% names(res))
  expect_equal(res$Y, 2)
  expect_equal(res$P, 0)
  expect_equal(res$N, 3)
  expect_equal(res$DK, 0)
  expect_true(is.na(res$custom))
})

test_that("fb_public_merge_custom_choices returns only unmatched exposures", {
  if (!exists("fb_public_merge_custom_choices")) {
    fail("fb_public_merge_custom_choices not implemented")
    return()
  }

  current_choices <- list("Known A" = "A", "Known B" = "B")
  matched_exposures <- c("A", "Custom", "B", "Custom")

  res <- fb_public_merge_custom_choices(matched_exposures, current_choices)

  expect_equal(res, "Custom")
})

test_that("fb_public_reference_table_from_choices aligns labels to refs", {
  if (!exists("fb_public_reference_table_from_choices")) {
    fail("fb_public_reference_table_from_choices not implemented")
    return()
  }

  choices <- list("Label A" = "A", "Label B" = "B")
  refs <- c(A = 10, B = 20)

  res <- fb_public_reference_table_from_choices(choices, refs)

  expect_equal(res$Exposure, c("Label A", "Label B"))
  expect_equal(res$Code, c("A", "B"))
  expect_equal(res$`Reference %`, c(10, 20))
})

test_that("fb_public_top_exposures selects highest reference values", {
  if (!exists("fb_public_top_exposures")) {
    fail("fb_public_top_exposures not implemented")
    return()
  }

  tbl <- data.frame(
    Exposure = c("A", "B", "C"),
    `Reference %` = c(10, 30, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  res <- fb_public_top_exposures(tbl, n = 2)

  expect_equal(res$Exposure, c("B", "C"))
  expect_equal(res$`Reference %`, c(30, 20))
})

test_that("fb_public_pt_coverage counts PT occurrences", {
  if (!exists("fb_public_pt_coverage")) {
    fail("fb_public_pt_coverage not implemented")
    return()
  }

  df <- data.frame(
    PT = c("ON", "QC", "ON", NA_character_),
    stringsAsFactors = FALSE
  )

  res <- fb_public_pt_coverage(df, lang = "en")

  expect_equal(res$Count, c(2, 1))
})

test_that("fb_public_month_coverage orders months by calendar sequence", {
  if (!exists("fb_public_month_coverage")) {
    fail("fb_public_month_coverage not implemented")
    return()
  }

  df <- data.frame(
    Month = c(2, 1, 2, NA_integer_),
    stringsAsFactors = FALSE
  )

  res <- fb_public_month_coverage(df, lang = "en")

  expect_equal(res$Month, c("January", "February"))
  expect_equal(res$Count, c(1, 2))
})

test_that("fb_public_available_pts returns toolkit PT codes", {
  if (!exists("fb_public_available_pts")) {
    fail("fb_public_available_pts not implemented")
    return()
  }

  pts <- fb_public_available_pts()

  expect_true("Canada" %in% pts)
  expect_true("ON" %in% pts)
})

test_that("fb_public_build_reference_table uses reference function output", {
  if (!exists("fb_public_build_reference_table")) {
    fail("fb_public_build_reference_table not implemented")
    return()
  }

  choices <- list("Label A" = "A", "Label B" = "B")
  fake_ref <- function(codes, pt_names = NULL, months = NULL, age_groups = NULL) {
    stats::setNames(c(10, 20), codes)
  }

  tbl <- fb_public_build_reference_table(
    choices,
    pt_names = NULL,
    months = NULL,
    age_groups = NULL,
    reference_fun = fake_ref
  )

  expect_equal(tbl$Exposure, c("Label A", "Label B"))
  expect_equal(tbl$`Reference %`, c(10, 20))
})

test_that("fb_public_resolve_default_selection resolves default transitions", {
  if (!exists("fb_public_resolve_default_selection")) {
    fail("fb_public_resolve_default_selection not implemented")
    return()
  }

  expect_equal(
    fb_public_resolve_default_selection(c("Canada", "ON"), "Canada", "Canada"),
    "ON"
  )
  expect_equal(
    fb_public_resolve_default_selection(c("Canada", "ON"), "Canada", "ON"),
    "Canada"
  )
  expect_equal(
    fb_public_resolve_default_selection(c("ON", "QC"), "Canada", "ON"),
    c("ON", "QC")
  )
  expect_equal(
    fb_public_resolve_default_selection("Canada", "Canada", "Canada"),
    "Canada"
  )
  expect_equal(
    fb_public_resolve_default_selection(character(), "Canada", character()),
    character()
  )
})
