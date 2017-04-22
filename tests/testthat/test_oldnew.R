# Test that chunk outputs match original data system

context("oldnew")

library(readr)

test_that("matches old data system output", {

  # If we're on Travis, need to run the driver to ensure chunk outputs saved
  # Don't do this locally, to speed things up

  # Look for output data in OUTPUTS_DIR under top level
  # (as this code will be run in tests/testthat)
  outputs_dir <- normalizePath(file.path("../..", OUTPUTS_DIR))

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    driver(write_outputs = TRUE, outdir = outputs_dir)
  }
  expect_equivalent(file.access(outputs_dir, mode = 4), 0,  # outputs_dir exists and is readable
                    info = paste("Directory", outputs_dir, "unreadable or does not exist."))
  expect_true(file.info(outputs_dir)$isdir)

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

    newskip <- 0
    if(flag_long_year_form | flag_no_xyear_form | flag_sum_test) {
      newskip <- 1
    }

    newdata <- read_csv(newf, comment = COMMENT_CHAR, skip = newskip)

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
    if(flag_no_xyear_form) {
      yearcols <- grep("^[0-9]{4}$", names(newdata))
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
        x[, integer_columns] <- sapply(x[, integer_columns], as.numeric)

        numeric_columns <- sapply(x, class) == "numeric"
        x[numeric_columns] <- round(x[numeric_columns], digits)
        x
      }

      # Some datasets throw errors when tested via `expect_equivalent` because of
      # rounding issues, even when we verify that they're identical to three s.d.
      # I think this is because of differences between readr::write_csv and write.csv
      # To work around this, we allow chunks to tag datasets with FLAG_SUM_TEST,
      # which is less strict, just comparing the sum of all numeric data
      if(flag_sum_test) {
        numeric_columns_old <- sapply(olddata, class) == "numeric"
        numeric_columns_new <- sapply(newdata, class) == "numeric"
        expect_equivalent(sum(olddata[numeric_columns_old]), sum(newdata[numeric_columns_new]),
                          info = paste(basename(newf), "doesn't match (sum test)"))
      } else {
        expect_equivalent(round_df(olddata), round_df(newdata), info = paste(basename(newf), "doesn't match"))
      }
    }
  }
})
