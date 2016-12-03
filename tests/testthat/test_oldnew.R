# Test that chunk outputs match original data system

context("oldnew")

library(readr)

test_that("matches old data system output", {

  # For each file in OUTPUTS_DIR, look for corresponding file in our
  # comparison data. Load them, reshape new data if necessary, compare.
  outputs_dir <- file.path("../..", OUTPUTS_DIR)
  for(newf in list.files(outputs_dir, full.names = TRUE)) {

    # In this rewrite, we're not putting X's in front of years,
    # nor are we going to spend time unnecessarily reshaping datasets
    # (i.e. wide to long and back). But we still need to be able to
    # verify old versus new datasets! Chunks tag the data if it's
    # reshaped, and save_chunkdata puts a special marker at top of the file.
    new_firstline <- readLines(newf, n = 1)
    newskip <- 0
    if(new_firstline %in% c(LONG_NO_X_FORM)) {
      newskip <- 1
    }

    newdata <- read_csv(newf, comment = "#", skip = newskip)

    # Reshape new data if necessary--see comment above
    if(new_firstline == LONG_NO_X_FORM) {
      newdata %>%
        mutate(year = paste0("X", newdata$year)) %>%
        spread(year, value) ->
        newdata
    }

    oldf <- file.path("comparison_data", basename(newf))
    expect_true(file.exists(oldf), label = paste0(oldf, "doesn't exist"))

    # If the old file has an "INPUT_TABLE" header, need to skip that
    old_firstline <- readLines(oldf, n = 1)
    oldskip <- ifelse(old_firstline == "INPUT_TABLE", 4, 0)
    olddata <- read_csv(oldf, comment = "#", skip = oldskip)

    # Finally, test (NB rounding numeric columns to a sensible number of
    # digits; otherwise spurious mismatches occur)
    DIGITS <- 3
    round_df <- function(x, digits = DIGITS) {
      numeric_columns <- sapply(x, class) == "numeric"
      x[numeric_columns] <- round(x[numeric_columns], digits)
      x
    }

    expect_equivalent(round_df(olddata),
                      round_df(newdata),
                      label = paste(basename(newf), "doesn't match"))
  }
})
