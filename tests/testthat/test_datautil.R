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
  d2 <- NA   # optional input, not found

  expect_error(add_data(list(d1, cars, iris))) # no names
  all_data <- add_data(return_data(d1, cars, iris, d2), all_data)

  expect_identical(get_data(all_data, "d1"), d1)
  expect_identical(get_data(all_data, "cars"), cars)
  expect_identical(get_data(all_data, "iris"), iris)
  expect_true(is.null(get_data(all_data, "d2")))
  expect_error(get_data(all_data, "not_there"))
})
