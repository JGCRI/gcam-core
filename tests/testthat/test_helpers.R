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
