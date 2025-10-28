# Unit tests for Foodbook backend parsing functions

test_that("fb_parse_renames correctly parses Stata rename directives", {
  # Create temporary .do file with rename directives
  temp_file <- tempfile(fileext = ".do")
  writeLines(c(
    "* Header comment",
    "rename v1 RecordID",
    "rename v2 Province",
    "  rename  v3  Gender  ",  # Test whitespace handling
    "// Comment line",
    "gen newvar = 1",  # Should be ignored
    "rename oldname newname"
  ), temp_file)

  result <- fb_parse_renames(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_true(all(c("old", "new") %in% names(result)))
  expect_equal(result$old, c("v1", "v2", "v3", "oldname"))
  expect_equal(result$new, c("RecordID", "Province", "Gender", "newname"))

  unlink(temp_file)
})

test_that("fb_parse_renames handles empty or invalid files gracefully", {
  # Non-existent file
  result1 <- fb_parse_renames("nonexistent.do")
  expect_s3_class(result1, "tbl_df")
  expect_equal(nrow(result1), 0)

  # Empty file
  temp_empty <- tempfile(fileext = ".do")
  writeLines(character(0), temp_empty)
  result2 <- fb_parse_renames(temp_empty)
  expect_equal(nrow(result2), 0)
  unlink(temp_empty)

  # File with no rename directives
  temp_norename <- tempfile(fileext = ".do")
  writeLines(c("gen x = 1", "drop y"), temp_norename)
  result3 <- fb_parse_renames(temp_norename)
  expect_equal(nrow(result3), 0)
  unlink(temp_norename)
})

test_that("fb_parse_label_map correctly extracts exposure labels", {
  temp_file <- tempfile(fileext = ".do")
  writeLines(c(
    "gen label = \"Raw vegetables\" if exposure == \"P01005b\"",
    "replace label = \"Fresh fruit\" if exposure == \"P01001\"",
    "  gen   label = \"Leafy greens\"   if exposure == \"P01002\"  ",  # Whitespace
    "replace label = \"  Raw  meat  \" if exposure == \"P02001\"",  # Extra spaces
    "gen other = \"Something\" if exposure == \"X001\"",  # Wrong variable, should be ignored
    "gen label = \"\" if exposure == \"EMPTY\"",  # Empty label, should be filtered
    "replace label = \"Duplicate\" if exposure == \"DUP1\"",
    "replace label = \"Duplicate2\" if exposure == \"DUP1\""  # Duplicate code, keep first
  ), temp_file)

  result <- fb_parse_label_map(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("code", "label") %in% names(result)))
  expect_gte(nrow(result), 4)
  expect_true("P01005b" %in% result$code)
  expect_equal(result$label[result$code == "P02001"], "Raw meat")  # Spaces normalized
  expect_false("EMPTY" %in% result$code)  # Empty labels filtered out
  expect_equal(sum(result$code == "DUP1"), 1)  # Only one duplicate retained

  unlink(temp_file)
})

test_that("fb_apply_renames correctly renames data frame columns", {
  df <- data.frame(v1 = 1:3, v2 = 4:6, v3 = 7:9, other = 10:12)
  renames <- tibble::tibble(
    old = c("v1", "v2", "v4"),  # v4 doesn't exist
    new = c("ID", "Province", "Missing")
  )

  result <- fb_apply_renames(df, renames)

  expect_equal(names(result), c("ID", "Province", "v3", "other"))
  expect_equal(result$ID, 1:3)
  expect_equal(result$Province, 4:6)
})

test_that("fb_apply_renames handles edge cases", {
  df <- data.frame(x = 1:3, y = 4:6)

  # Empty renames
  empty_renames <- tibble::tibble(old = character(), new = character())
  result1 <- fb_apply_renames(df, empty_renames)
  expect_equal(names(result1), c("x", "y"))

  # No matching columns
  no_match <- tibble::tibble(old = c("a", "b"), new = c("c", "d"))
  result2 <- fb_apply_renames(df, no_match)
  expect_equal(names(result2), c("x", "y"))
})

test_that("fb_normalise_weight identifies and normalizes weight columns", {
  # Test with EXPWEIGHT_CMA2 (Foodbook 1)
  df1 <- data.frame(EXPWEIGHT_CMA2 = c(1.5, 2.0, 0.5), x = 1:3)
  result1 <- fb_normalise_weight(df1)
  expect_true("weight" %in% names(result1))
  expect_equal(result1$weight, c(1.5, 2.0, 0.5))

  # Test with proj_weight_non_traveller (Foodbook 2)
  df2 <- data.frame(proj_weight_non_traveller = c(10, 20, 30), x = 1:3)
  result2 <- fb_normalise_weight(df2)
  expect_equal(result2$weight, c(10, 20, 30))

  # Test with existing 'weight' column
  df3 <- data.frame(weight = c(5, 10, 15), x = 1:3)
  result3 <- fb_normalise_weight(df3)
  expect_equal(result3$weight, c(5, 10, 15))

  # Test with no weight column (should default to 1)
  df4 <- data.frame(x = 1:3, y = 4:6)
  result4 <- fb_normalise_weight(df4)
  expect_equal(result4$weight, c(1, 1, 1))

  # Test with character weights (should convert to numeric)
  df5 <- data.frame(weight = c("1.5", "2.5", "3.5"), x = 1:3)
  result5 <- fb_normalise_weight(df5)
  expect_equal(result5$weight, c(1.5, 2.5, 3.5))
})
