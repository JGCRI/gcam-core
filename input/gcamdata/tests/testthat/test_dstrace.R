# Test the dstrace utility

context("dstrace")

test_that("dstrace works", {

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

  # UPSTREAM ONLY
  # Identifies objects with no precursors
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "up"), regexp = "No precursors")
  # Identifies objects read from file
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "up"), regexp = "read from file")
  # Identifies immediate precursor
  expect_output(dstrace("o1", gcam_data_map = gdm, direction = "up", recurse = FALSE), regexp = "Precursor: i1")
  # Identifies all precursors
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "up"), regexp = "Precursor: o1")
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "up"), regexp = "Precursor: i1")

  # DOWNSTREAM ONLY
  # Identifies objects with no dependents
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "down"), regexp = "No dependents")
  # Identifies objects XML data
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "down"), regexp = "XML data")
  # Identifies immediate dependent
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "down", recurse = FALSE), regexp = "Dependent: o1")
  # Identifies all dependents
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "down"), regexp = "Dependent: o1")
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "down"), regexp = "Dependent: o2")

  # BOTH WAYS
  # Identifies objects with no precursors
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "both"), regexp = "No precursors")
  # Identifies objects read from file
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "both"), regexp = "read from file")
  # Identifies immediate precursor
  expect_output(dstrace("o1", gcam_data_map = gdm, direction = "both", recurse = FALSE), regexp = "Precursor: i1")
  # Identifies all precursors
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "both"), regexp = "Precursor: o1")
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "both"), regexp = "Precursor: i1")
  # Identifies objects with no dependents
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "both"), regexp = "No dependents")
  # Identifies objects XML data
  expect_output(dstrace("o2", gcam_data_map = gdm, direction = "both"), regexp = "XML data")
  # Identifies immediate dependent
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "both", recurse = FALSE), regexp = "Dependent: o1")
  # Identifies all dependents
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "both"), regexp = "Dependent: o1")
  expect_output(dstrace("i1", gcam_data_map = gdm, direction = "both"), regexp = "Dependent: o2")

  # Test ds_plot too, a bit
  # This tracelist corresponds to the dstrace output for gdm above
  tracelist <- tibble(object_name = c("i1", "o1", "o2"),
                      tracenum = 1:3,
                      relationship = c("Self", "Dependent", "Dependent"),
                      relatives = c("o1", "o2", ""))
  mat <- dstrace_plot("i1", tracelist, upstream = TRUE, downstream = TRUE)
  expect_identical(dim(mat), c(nrow(tracelist), nrow(tracelist)))

  # Catches f-ed up relationship
  tracelist$relationship[2] <- "Cousin marriage"
  expect_error(dstrace_plot("i1", tracelist, upstream = TRUE, downstream = TRUE))
})

test_that("info works", {
  # handles bad input
  expect_error(info(1))
  expect_error(info("1", gcam_data_map = 1))
  expect_error(info("1", previous_tracelist = 1))
  expect_error(info("L100.FAO_ag_Exp_t", upstream = 1))
  expect_error(info("L100.FAO_ag_Exp_t", downstream = 1))

  # opens help page
  expect_is(info("module_aglu_an_input_xml"), "help_files_with_topic")

  # Most of this functionality is tested through the dstrace tests above
})

