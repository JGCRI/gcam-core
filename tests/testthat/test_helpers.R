# Test the pipeline helper utilities

context("helpers")

test_that("left_join_error_no_match works", {
  d1 <- tibble(X = LETTERS[1:3], Y = 1:3)
  d2 <- tibble(X = d1$X, Z = 4:6)

  # Bad data
  expect_error(left_join_error_no_match(1))

  # Good data
  expect_message(left_join_error_no_match(d1, d2))
  expect_silent(left_join_error_no_match(d1, d2, by = "X"))
  result <- left_join_error_no_match(d1, d2, by = "X")
  expect_is(result, "tbl")
  expect_equal(nrow(result), nrow(d1))

  # Incomplete merge data
  d2$Z[2] <- NA
  expect_error(left_join_error_no_match(d1, d2))
  d2 <- tibble(X = d1$X[1:2], Z = 4:5)
  expect_error(left_join_error_no_match(d1, d2))
})


test_that("repeat_add_columns works", {
  x <- tibble::tibble(x = 1:3)
  y <- tibble::tibble(y = c(4, 5), z = c(6,7))

  expect_silent(repeat_add_columns(x, y))

  z <- repeat_add_columns(x, y)
  expect_is(z, "tbl")
  expect_equal(nrow(z), nrow(x) * nrow(y))
  expect_equal(ncol(z), ncol(x) + ncol(y))
})

test_that("gdp_deflator works", {
  # Errors for nonsense years
  expect_error(gdp_deflator(-33, 1975))
  expect_error(gdp_deflator(1975, -33))

  # Computes something reasonable
  expect_is(gdp_deflator(2008, 1950), "numeric")

  # Correctly returns old data system values
  if(OLD_DATA_SYSTEM_BEHAVIOR) {
    expect_equal(gdp_deflator(2010, 1990), 1.510)
  }
})
