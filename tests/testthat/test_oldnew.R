# Test that chunk outputs match original data system

context("oldnew")

library(readr)

test_that("matches old data system output", {
  # If we are running the code coverage tests then let's skip this since
  # it will take a long to time run and the purpose of this test is to
  # make sure the chunk outputs match the old data system and not to test
  # the functionality of any chunks
  if(isTRUE(as.logical(Sys.getenv("gcamdata.is_coverage_test")))) {
    skip("Skip old new when only interested in code coverage")
  }

  # If we're on Travis, need to run the driver to ensure chunk outputs saved
  # Don't do this locally, to speed things up

  # Look for output data in OUTPUTS_DIR under top level
  # (as this code will be run in tests/testthat)
  outputs_dir <- normalizePath(file.path("../..", OUTPUTS_DIR))
  xml_dir <- normalizePath(file.path("../..", XML_DIR))

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    gcam_data_map <- driver(write_outputs = TRUE, outdir = outputs_dir, xmldir = xml_dir, return_data_map_only = TRUE)
    # The following two tests are only run on Travis because they will fail
    # during the R CMD CHECK process locally (as the R build process removes outputs/)
    expect_equivalent(file.access(outputs_dir, mode = 4), 0,  # outputs_dir exists and is readable
                      info = paste("Directory", outputs_dir, "unreadable or does not exist from", getwd()))
    expect_true(file.info(outputs_dir)$isdir)

    # Now, and also run only on Travis, we compare the data map returned above with the pre-packaged version
    # They should match! See https://github.com/JGCRI/gcamdata/pull/751#issuecomment-331578990
    expect_equivalent(gcam_data_map, gcamdata:::GCAM_DATA_MAP,
                     info = "GCAM_DATA_MAP is out of date; rerun data-raw/generate-package-data.R")
  }

  # For each file in OUTPUTS_DIR, look for corresponding file in our
  # comparison data. Load them, reshape new data if necessary, compare.
  for(newf in list.files(outputs_dir, full.names = TRUE)) {
    # In this rewrite, we're not putting X's in front of years,
    # nor are we going to spend time unnecessarily reshaping datasets
    # (i.e. wide to long and back). But we still need to be able to
    # verify old versus new datasets! Chunks tag the data if it's
    # reshaped, and save_chunkdata puts flag(s) at top of the file.
    new_firstline <- readLines(newf, n = 1)

    if(grepl(FLAG_NO_TEST, new_firstline)) {
      next
    }

    flag_long_year_form <- grepl(FLAG_LONG_YEAR_FORM, new_firstline)
    flag_no_xyear_form <- grepl(FLAG_NO_XYEAR, new_firstline)
    flag_sum_test <- grepl(FLAG_SUM_TEST, new_firstline)
    flag_year_col_xyears <- grepl(FLAG_YEAR_COL_XYEARS, new_firstline)

    newdata <- read_csv(newf, comment = COMMENT_CHAR)

    # If there's a 'year' columns with xyears, add an X
    if(flag_year_col_xyears) {
      expect_true("year" %in% names(newdata),
                  info = paste("FLAG_YEAR_COL_XYEARS specified in", basename(newf),
                               "but not 'year' column present"))
      newdata$year <- paste0("X", newdata$year)
    }

    # If there's a logit column, delete that sucker immediately
    newdata[[LOGIT_COLUMN_NAME]] <- NULL

    # Reshape new data if necessary--see comment above
    if(flag_long_year_form) {
      expect_true(all(c("year", "value") %in% names(newdata)),
                  info = paste("FLAG_LONG_YEAR_FORM specified in", basename(newf),
                               "but no 'year' and 'value' columns present"))
      newdata <- try(spread(newdata, year, value))
      if(isTRUE(class(newdata) == "try-error")) {
        stop("Error reshaping ", basename(newf), "; are there `year`` and `value` columns?")
        next
      }
    }

    # Change year column names to "xyear" (X1970, etc) names if necessary
    if(flag_no_xyear_form) {
      yearcols <- grep(YEAR_PATTERN, names(newdata))
      expect_true(length(yearcols) > 0,
                  info = paste("FLAG_NO_XYEAR specified in", basename(newf),
                               "but no year-type columns seem to be present"))
      names(newdata)[yearcols] <- paste0("X", names(newdata)[yearcols])
    }

    oldf <- list.files("comparison_data", pattern = basename(newf), recursive = TRUE, full.names = TRUE)
    expect_true(length(oldf) == 1, info = paste("Either zero, or multiple, comparison datasets found for", basename(newf)))

    if(length(oldf) == 1) {
      # If the old file has an "INPUT_TABLE" header, need to skip that
      old_firstline <- read_lines(oldf, n_max = 1)
      oldskip <- ifelse(old_firstline == "INPUT_TABLE", 4, 0)
      olddata <- read_csv(oldf, comment = COMMENT_CHAR, skip = oldskip)

      # Finally, test (NB rounding numeric columns to a sensible number of
      # digits; otherwise spurious mismatches occur)
      # Also first converts integer columns to numeric (otherwise test will
      # fail when comparing <int> and <dbl> columns)
      DIGITS <- 3
      round_df <- function(x, digits = DIGITS) {
        integer_columns <- sapply(x, class) == "integer"
        x[integer_columns] <- lapply(x[integer_columns], as.numeric)

        numeric_columns <- sapply(x, class) == "numeric"
        x[numeric_columns] <- round(x[numeric_columns], digits)
        x
      }

      expect_identical(dim(olddata), dim(newdata), info = paste("Dimensions are not the same for", basename(newf)))

      # Some datasets throw errors when tested via `expect_equivalent` because of
      # rounding issues, even when we verify that they're identical to three s.d.
      # I think this is because of differences between readr::write_csv and write.csv
      # To work around this, we allow chunks to tag datasets with FLAG_SUM_TEST,
      # which is less strict, just comparing the sum of all numeric data
      if(flag_sum_test) {
        numeric_columns_old <- sapply(olddata, is.numeric)
        numeric_columns_new <- sapply(newdata, is.numeric)
        expect_equivalent(sum(olddata[numeric_columns_old]), sum(newdata[numeric_columns_new]),
                          info = paste(basename(newf), "doesn't match (sum test)"))
      } else {
        expect_equivalent(round_df(olddata), round_df(newdata), info = paste(basename(newf), "doesn't match"))
      }
    }
  }
})


test_that('New XML outputs match old XML outputs', {
  ## The XML comparison data is huge, so we don't want to try to include it in
  ## the package.  Instead, we look for an option that indicates where the data
  ## can be found.  If the option isn't set, then we skip this test.
  xml_cmp_dir <- getOption('gcamdata.xml_cmpdir')
  if(is.null(xml_cmp_dir)) {
    skip("XML comparison data not provided. Set option 'gcamdata.xml_cmpdir' to run this test.")
  }
  else {
    xml_cmp_dir <- normalizePath(xml_cmp_dir)
  }
  expect_true(file.exists(xml_cmp_dir))

  xml_dir <- normalizePath(file.path("../..", XML_DIR))
  expect_true(file.exists(xml_dir))

  for(newxml in list.files(xml_dir, full.names = TRUE)) {
    oldxml <- list.files(xml_cmp_dir, pattern = paste0('^',basename(newxml),'$'), recursive = TRUE,
                         full.names = TRUE)
    if(length(oldxml) > 0) {
      expect_equal(length(oldxml), 1,
                   info = paste('Testing file', newxml, ': Found', length(oldxml),
                                'comparison files.  There can be only one.'))
      ## If we come back with multiple matching files, we'll try to run the test anyhow, selecting
      ## the first one as the true comparison.
      expect_true(cmp_xml_files(oldxml[1], newxml),
                  info = paste('Sorry to be the one to tell you, but new XML file',
                               newxml, "is not equivalent to its old version."))
    }
    else {
      ## If no comparison file found, issue a message, but don't fail the test.
      message('No comparison file found for ', newxml, '. Skipping.')
    }
  }
})
