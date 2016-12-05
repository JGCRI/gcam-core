# Test the file utilities

context("fileutil")


test_that("error with bad input", {
  expect_error(load_csv_files(TRUE))
  expect_error(load_csv_files(1))
  expect_error(find_csv_file(TRUE))
  expect_error(find_csv_file(1))
  expect_error(find_csv_file(c("h","i")))
})

test_that("handle empty input", {
  x <- load_csv_files(character(0))
  expect_is(x, "list")
  expect_length(x, 0)
})
