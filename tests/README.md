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

# Test new features (October 2025)
testthat::test_file("tests/testthat/test-new-features.R")
```

### Run from Command Line

```bash
Rscript tests/testthat.R
```

## Test Coverage

### Backend Parsing Functions (`test-backend-parsing.R`) - 28 tests
- `fb_parse_renames()` - Stata rename directive parsing
- `fb_parse_label_map()` - Exposure label extraction
- `fb_apply_renames()` - Data frame column renaming
- `fb_normalise_weight()` - Weight column identification and normalization

### Backend Calculation Functions (`test-backend-calculations.R`) - 20 tests
- `fb_weighted_percent()` - Weighted percentage calculations
- `fb_pt_map()` - Province/territory code mapping
- `fb_pt_abbrev_map()` - PT abbreviation mapping
- `fb_filter_micro()` - Microdata filtering by PT/age/month
- `fb_reference_percents_csv()` - CSV-based reference calculations
- `fb_age_groups()` and `fb_months()` - Available filter options

### New Features (October 2025) (`test-new-features.R`) - 20+ tests
- `find_sheet_by_columns()` - CEDARS auto-detection by required columns
- `classify_exposure()` - Classification including "No Reference Value"
- `make_safe_id()` - HTML ID sanitization
- Custom exposure detection and handling
- Response count columns (Yes/Prob/No/DK) in results
- Descriptive export filename generation
- CSV upload validation and column normalization
- Integration tests: Full analysis workflows

**Total Test Count:** ~70 tests across 3 files

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
- New features tests (Oct 2025) require `writexl` package for Excel file creation:
  ```r
  install.packages("writexl")
  ```
- Tests marked with `skip_if_not_installed()` will be skipped if optional packages are missing
- Some tests create temporary Excel files to test CEDARS auto-detection functionality

## Recent Updates (October 2025)

Added comprehensive test suite for new features based on PHAC-OMD demo meeting requirements:

1. **CEDARS Auto-Detection:** Tests for flexible sheet name detection based on required columns
2. **No Reference Value:** Tests for new classification when reference data unavailable
3. **Custom Exposures:** Tests for user-defined exposures with custom reference percentages
4. **Response Counts:** Tests for Yes/Prob/No/DK columns in results tables
5. **Export Filenames:** Tests for descriptive filename generation with analysis parameters
6. **CSV Upload:** Tests for simple CSV validation and column normalization
7. **Integration Tests:** Full workflow tests simulating real user interactions

These tests ensure reliability of all new functionality and prevent regressions in future updates.
