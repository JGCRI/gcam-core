# Test the data utilities

context("datautil")


test_that("error with bad input", {
  expect_error(find_chunks(pattern = 1))
  expect_error(chunk_inputs(chunks = 1))
  expect_error(chunk_outputs(chunks = 1))
})

test_that("handle empty input", {
  d <- chunk_inputs(character(0))
  expect_true(tibble::is.tibble(d))
  expect_equal(nrow(d), 0)

  d <- chunk_outputs(character(0))
  expect_true(tibble::is.tibble(d))
  expect_equal(nrow(d), 0)
})

test_that("commenting system works", {
  cmnt <- "test"
  d <- tibble::tibble()
  c1 <- get_comments(d)
  d <- add_comments(d, cmnt)
  expect_gt(length(get_comments(d)), length(c1))
  expect_equal(get_comments(d), cmnt)
})

test_that("flag system works", {
  flag <- "test"
  d <- tibble::tibble()
  c1 <- get_flags(d)
  d <- add_flags(d, flag)
  expect_gt(length(get_flags(d)), length(c1))
  expect_equal(get_flags(d), flag)
})

test_that("return_data works", {
  d <- return_data(cars, iris)
  expect_is(d, "list")
  expect_identical(names(d), c("cars", "iris"))
})

test_that("add/get data work", {
  all_data <- empty_data()
  expect_true(is_data_list(all_data))
  expect_equal(length(all_data), 0)

  d1 <- tibble(x=1:3)

  expect_error(add_data(list(d1, cars, iris))) # no names
  all_data <- add_data(return_data(d1, cars, iris), all_data)

  expect_identical(get_data(all_data, "d1"), d1)
  expect_identical(get_data(all_data, "cars"), cars)
  expect_identical(get_data(all_data, "iris"), iris)
  expect_error(get_data(all_data, "not_there"))
})
