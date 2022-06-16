# Test the data utilities

context("datautil")


test_that("error with bad input", {
  expect_error(find_chunks(pattern = 1))
  expect_error(chunk_inputs(chunks = 1))
  expect_error(chunk_outputs(chunks = 1))
})

test_that("handle empty input", {
  d <- chunk_inputs(character(0))
  expect_true(tibble::is_tibble(d))
  expect_equal(nrow(d), 0)

  d <- chunk_outputs(character(0))
  expect_true(tibble::is_tibble(d))
  expect_equal(nrow(d), 0)
})

test_that("return_data works", {
  d <- return_data(cars, iris)
  expect_is(d, "list")
  expect_identical(names(d), c("cars", "iris"))

  # test it does not allow grouped data
  cars_grouped <- group_by(cars, speed)
  expect_error(return_data(cars, iris, cars_grouped),
               regexp = "is being returned grouped. This is not allowed; please ungroup")

  # ensure no failure with non-groupable types
  list_data <- list(a = "1")
  expect_silent(d <- return_data(cars, iris, list_data))
})

test_that("add/get/remove data work", {
  all_data <- empty_data()
  expect_true(is_data_list(all_data))
  expect_equal(length(all_data), 0)

  d1 <- tibble(x = 1:3)
  d2 <- missing_data()   # optional input, not found

  expect_error(add_data(list(d1, cars, iris))) # no names
  all_data <- add_data(return_data(d1, cars, iris, d2), all_data)

  expect_identical(get_data(all_data, "d1"), d1)
  expect_identical(get_data(all_data, "cars"), cars)
  expect_identical(get_data(all_data, "iris"), iris)
  expect_true(is.null(get_data(all_data, "d2")))
  expect_error(get_data(all_data, "not_there"))

  expect_error(remove_data("not_there", all_data))
  all_data <- remove_data("cars", all_data)
  expect_error(remove_data("cars", all_data))
})

test_that("same_attributes_as works", {
  tibble(a = 1) %>%
    add_title("t") %>% add_units("u") %>% add_comments("c") %>% add_precursors("p") %>%
    add_legacy_name("l") %>% add_reference("r") %>% add_flags("f") -> x
  tibble(b = 1) %>%
    same_attributes_as(x) ->
    y

  expect_identical(get_title(x), get_title(y))
  expect_identical(get_units(x), get_units(y))
  expect_identical(get_comments(x), get_comments(y))
  expect_identical(get_precursors(x), get_precursors(y))
})

test_that("prebuilt_data works", {
  pb <- list(x = 1, y = 2)
  expect_null(extract_prebuilt_data("not_prebuild_data"))

  obj <- extract_prebuilt_data(names(pb)[1], pb = pb)
  expect_equivalent(obj, pb[[1]])
  expect_is(get_comments(obj), "character")  # should have a comment attached
})

test_that("verify_identical_prebuilt works", {
  pb <- list(x = 1, y = 2)
  x <- 1
  expect_silent(verify_identical_prebuilt(x, pb = pb))
  expect_warning(verify_identical_prebuilt(zzz = 1, pb = pb))
})
