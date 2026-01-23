test_that("common server exposes default selection resolver", {
  env <- new.env(parent = baseenv())
  sys.source(file.path("..", "..", "src", "common_server.R"), envir = env)

  expect_true(
    exists("fb_public_resolve_default_selection", envir = env),
    info = "fb_public_resolve_default_selection should be available in common_server.R"
  )
})
