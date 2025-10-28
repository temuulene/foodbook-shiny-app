# Foodbook Shiny App - Test Suite

This directory contains unit tests for the Foodbook backend functions using the `testthat` framework.

## Running the Tests

### Prerequisites

Ensure all required packages are installed:

```r
install.packages(c("testthat", "dplyr", "stringr", "readxl", "haven", "tibble"))
```

### Run All Tests

From the project root directory:

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Or use the test runner
source("tests/testthat.R")
```

### Run Specific Test Files

```r
# Test parsing functions only
testthat::test_file("tests/testthat/test-backend-parsing.R")

# Test calculation functions only
testthat::test_file("tests/testthat/test-backend-calculations.R")
```

### Run from Command Line

```bash
Rscript tests/testthat.R
```

## Test Coverage

### Backend Parsing Functions (`test-backend-parsing.R`)
- `fb_parse_renames()` - Stata rename directive parsing
- `fb_parse_label_map()` - Exposure label extraction
- `fb_apply_renames()` - Data frame column renaming
- `fb_normalise_weight()` - Weight column identification and normalization

### Backend Calculation Functions (`test-backend-calculations.R`)
- `fb_weighted_percent()` - Weighted percentage calculations
- `fb_pt_map()` - Province/territory code mapping
- `fb_pt_abbrev_map()` - PT abbreviation mapping
- `fb_filter_micro()` - Microdata filtering by PT/age/month
- `fb_reference_percents_csv()` - CSV-based reference calculations
- `fb_age_groups()` and `fb_months()` - Available filter options

## Test Structure

Each test file follows this pattern:

```r
test_that("function_name does expected behavior", {
  # Arrange: Set up test data
  # Act: Call the function
  # Assert: Verify results
  expect_equal(...)
  expect_true(...)
  expect_s3_class(...)
})
```

## Adding New Tests

When adding new backend functions:

1. Create test cases in the appropriate test file
2. Test both expected behavior and edge cases
3. Include tests for error handling
4. Document any special test data requirements

## Continuous Integration

These tests can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run tests
  run: Rscript tests/testthat.R
```

## Notes

- Tests use temporary files that are automatically cleaned up
- Mock data is created within tests to avoid dependencies on real data files
- The backend functions handle graceful fallbacks, which are also tested
