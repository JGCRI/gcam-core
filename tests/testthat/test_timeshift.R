# Test the chunks - do they handle different conditions?

# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

context("timeshift")


if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

  test_that("chunks handle timeshift", {

    UNDER_TIMESHIFT <<- TRUE

    # Move the historical/future division back by five years
    hyr <- HISTORICAL_YEARS
    HISTORICAL_YEARS <<- 1971:2005       # normally 1971:2010
    fyr <- FUTURE_YEARS
    FUTURE_YEARS <<- seq(2010, 2100, 5)  # normally seq(2015, 2100, 5)
    byr <- BASE_YEARS
    BASE_YEARS <<- c(1975, 1990, 2005)   # normally (1975, 1990, 2005, 2010)
    myr <- MODEL_YEARS
    MODEL_YEARS <<- c(BASE_YEARS, FUTURE_YEARS)

    with_mock(
      run_chunk = function(chunk, all_data) {
        # Wrap this call in `try` because otherwise the testing infrastructure tries to print the contents
        # of the potentially very large all_data when a chunk fails. This is bad.
        try({
          x <- do.call(chunk, list(driver.MAKE, all_data))
        })
        expect_true(exists("x"), info = paste("Timeshift error invoking", chunk))
        x
      },

      driver(quiet = TRUE, write_outputs = FALSE)
    )

    # Reset to what it was before
    UNDER_TIMESHIFT <<- FALSE
    HISTORICAL_YEARS <<- hyr
    FUTURE_YEARS <<- fyr
    BASE_YEARS <<- byr
    MODEL_YEARS <<- myr
  })

}
