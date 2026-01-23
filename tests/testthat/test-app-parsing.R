
# Test script to verify app files parse correctly
test_that("App files parse successfully", {
  # Paths relative to tests/testthat/
  expect_silent(parse(file = "../../app-public/app.R"))
  expect_silent(parse(file = "../../app-internal/app.R"))
  expect_silent(parse(file = "../../src/modules/mod_ref_settings.R"))
  expect_silent(parse(file = "../../src/modules/mod_results_table.R"))
  expect_silent(parse(file = "../../src/modules/mod_visualization.R"))
  expect_silent(parse(file = "../../src/common_ui.R"))
  expect_silent(parse(file = "../../src/common_server.R"))
})
