# Main test runner for Foodbook Shiny App
# Run with: Rscript tests/testthat.R or testthat::test_dir("tests/testthat")

library(testthat)

# Source the backend functions
source("src/foodbook_backend.R")

# Run all tests
test_dir("tests/testthat", reporter = "progress")
