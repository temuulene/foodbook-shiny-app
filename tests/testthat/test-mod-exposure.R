context("Module: Exposure Input")
source(file.path("..", "..", "src", "modules", "exposure_module.R"))


test_that("exposure_module_server returns correct reactive data structure", {
  shiny::testServer(exposure_module_server, {
    # Set inputs
    session$setInputs(yes = 10, prob = 5, no = 100, dk = 2)
    
    # Verify reactive output
    res <- session$getReturned()()
    expect_equal(res$yes, 10)
    expect_equal(res$prob, 5)
    expect_equal(res$no, 100)
    expect_equal(res$dk, 2)
    expect_true(is.na(res$custom_ref))
  })
})

test_that("exposure_module_server clamps negative values to 0", {
  shiny::testServer(exposure_module_server, {
    session$setInputs(yes = -5, prob = -1, no = 10, dk = 0)
    
    res <- session$getReturned()()
    expect_equal(res$yes, 0)
    expect_equal(res$prob, 0)
    expect_equal(res$no, 10)
    expect_equal(res$dk, 0)
  })
})

test_that("exposure_module_server handles custom reference percentage", {
  shiny::testServer(exposure_module_server, {
    session$setInputs(yes = 10, custom_ref = 25.5)
    
    res <- session$getReturned()()
    expect_equal(res$yes, 10)
    expect_equal(res$custom_ref, 25.5)
  })
})

test_that("exposure_module_server handles NULL/empty inputs gracefully", {
  shiny::testServer(exposure_module_server, {
    # Inputs start as NULL implicitly
    res <- session$getReturned()()
    expect_equal(res$yes, 0)
    expect_equal(res$prob, 0)
    expect_equal(res$no, 0)
    expect_equal(res$dk, 0)
  })
})
