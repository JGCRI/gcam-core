# Test the dstrace utility

context("dstrace")

test_that("handles bad input", {

  skip("skip")

  expect_error(dstrace(1, empty_data()))
  expect_error(dstrace("1", 1))
  expect_error(dstrace("1", empty_data(), recurse = 1))

  # Make a fake data structure
  tibble(x = 1) %>%
    add_title("o1")  ->
    o1

  tibble(x = 1) %>%
    add_title("o2") %>%
    add_flags(FLAG_INPUT_DATA) ->
    o2

  tibble(x = 1) %>%
    add_title("o3") %>%
    add_precursors("o1", "o2") ->
    o3

  all_data <- add_data(list("o1" = o1, "o2" = o2, "o3" = o3), empty_data())

  # Errors if object not present
  expect_error(dstrace("o4", all_data))
  # Identifies objects with no precursors
  expect_output(dstrace("o1", all_data), regexp = "No precursors")
  # Identifies objects read from file
  expect_output(dstrace("o2", all_data), regexp = "read from file")
  # Traces precursors
  expect_output(dstrace("o3", all_data), regexp = "Precursor: o1")
  expect_output(dstrace("o3", all_data), regexp = "Precursor: o2")

})
