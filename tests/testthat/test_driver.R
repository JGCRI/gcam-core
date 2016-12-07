# Test the driver

context("driver")

# Not much to do here yet

test_that("errors with bad input", {
  expect_error(driver(1))
  expect_error(driver("1"))
})

test_that("runs", {
  # x <- capture_output({
  #   out <- driver()
  # })
  # expect_true(is.list(out))
})
