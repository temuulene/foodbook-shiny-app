test_that("Legacy microdata loading", {
  fb_env$initialised <- FALSE
  fb_env$micro <- NULL
  fb_env$data_source <- NULL
  fb_env$micro_fb1 <- NULL
  fb_init()

  expect_true(!is.null(fb_env$micro))
  expect_true("PT" %in% names(fb_env$micro))
  expect_true("Month" %in% names(fb_env$micro))
  expect_true("AgeBand" %in% names(fb_env$micro))
  expect_true("weight" %in% names(fb_env$micro))
  expect_equal(fb_env$data_source, "Legacy")
  expect_true(is.null(fb_env$micro_fb1))

  anytom_col <- if ("anytom" %in% names(fb_env$micro)) {
    "anytom"
  } else if ("anytom_dv" %in% names(fb_env$micro)) {
    "anytom_dv"
  } else {
    NA_character_
  }

  expect_true(!is.na(anytom_col))
})

test_that("Legacy reference percents return values for known exposure", {
  fb_env$initialised <- FALSE
  fb_env$micro <- NULL
  fb_env$data_source <- NULL
  fb_env$micro_fb1 <- NULL
  fb_init()

  res <- fb_reference_percents(c("anytom"), pt_names = "Canada")
  expect_true(!is.na(res[["anytom"]]))
  expect_true(res[["anytom"]] >= 0 && res[["anytom"]] <= 100)

  res_age <- fb_reference_percents(
    c("anytom"),
    pt_names = "Canada",
    age_groups = "0-9"
  )
  expect_true(!is.na(res_age[["anytom"]]))

  res_month <- fb_reference_percents(
    c("anytom"),
    pt_names = "Canada",
    months = 1
  )
  expect_true(!is.na(res_month[["anytom"]]))
})
