# Test the dstrace utility

context("dstrace")

test_that("works", {

  # handles bad input
  expect_error(dstrace(1))
  expect_error(dstrace("1", gcam_data_map = 1))
  expect_error(dstrace("1", recurse = 1))

  # Make a fake data map structure in which i1 input -> chunk 1 o1 -> chunk 2 o2
  gdm <- tibble(name = c("INPUT", "chunk1", "chunk2"),
                output = c("i1", "o1", "o2"),
                precursors = c("", "i1", "o1"),
                title = c("i1", "o1", "o2"),
                units = "",
                comments = "",
                flags = c(gcamdata:::FLAG_INPUT_DATA, "", gcamdata:::FLAG_XML))

  # Errors if object not present
  expect_error(dstrace("o4", gcam_data_map = gdm))

  # UPSTREAM
  # Identifies objects with no precursors
  expect_output(dstrace("i1", gcam_data_map = gdm), regexp = "No precursors")
  # Identifies objects read from file
  expect_output(dstrace("i1", gcam_data_map = gdm), regexp = "read from file")
  # Identifies immediate precursor
  expect_output(dstrace("o1", gcam_data_map = gdm, recurse = FALSE), regexp = "Precursor: i1")
  # Identifies all precursors
  expect_output(dstrace("o2", gcam_data_map = gdm), regexp = "Precursor: o1")
  expect_output(dstrace("o2", gcam_data_map = gdm), regexp = "Precursor: i1")

  # DOWNSTREAM
  # Identifies objects with no dependents
  expect_output(dstrace("o2", gcam_data_map = gdm, downstream = TRUE), regexp = "No dependents")
  # Identifies objects XML data
  expect_output(dstrace("o2", gcam_data_map = gdm, downstream = TRUE), regexp = "XML data")
  # Identifies immediate dependent
  expect_output(dstrace("i1", gcam_data_map = gdm, downstream = TRUE, recurse = FALSE), regexp = "Dependent: o1")
  # Identifies all precursors
  expect_output(dstrace("i1", gcam_data_map = gdm, downstream = TRUE), regexp = "Dependent: o1")
  expect_output(dstrace("i1", gcam_data_map = gdm, downstream = TRUE), regexp = "Dependent: o2")
})
