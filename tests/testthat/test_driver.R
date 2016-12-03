# Test the driver

context("driver")

# Not much to do here yet

test_that("errors with bad input", {
  expect_error(driver(1))
  expect_error(driver("1"))
})
