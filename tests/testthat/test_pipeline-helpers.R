# Test global constants for proper capitalization convention
# E.g. "XXX" and "driver.XXX" are OK, but "xxx" is not

context("pipeline-helpers")


test_that("approx_fun works", {
  expect_error(approx_fun("a", 1))
  expect_error(approx_fun(1, "a"))

  year <- 1:5
  value <- c(2, 4, NA, 8, 10)
  z <- approx_fun(year, value)
  expect_true(all(is.numeric(z)))
  expect_equal(z[3], 6)

  expect_error(approx_fun(year, value[-1]))
  expect_error(approx_fun(year, value, rule = 3))
})

test_that("PH_year_value_historical works", {
  tibble(year = 1900:2100, value = "1") %>%
    PH_year_value_historical ->
    x

  expect_true(all(x$year %in% HISTORICAL_YEARS))
  expect_is(x$value, "numeric")
})
