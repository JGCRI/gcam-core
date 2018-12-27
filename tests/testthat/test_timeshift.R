# Test the chunks - do they handle different conditions?

# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

context("timeshift")


if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

  test_that("chunks handle timeshift", {

    # If we are running the code coverage tests then we skip this test since
    # it will take a long to time run and while it may exercise some code
    # we would like to ensure works properly the cost just isn't worth it
    # at the moment.
    if(isTRUE(as.logical(Sys.getenv("gcamdata.is_coverage_test")))) {
      skip("Skip timeshift when only interested in code coverage")
    }

    UNDER_TIMESHIFT <<- TRUE

    # Re-set the model base years to 1975, 1990. Run 2005 and 2010 as future years
    # Note that this does not affect the HISTORICAL_YEARS or FUTURE_YEARS for data processing
    byr <- MODEL_BASE_YEARS
    MODEL_BASE_YEARS <<- c(1975, 1990)   # normally (1975, 1990, 2005, 2010)
    fyr <- MODEL_FUTURE_YEARS
    MODEL_FUTURE_YEARS <<- c(2005, 2010, seq(2015, 2100, 5))   # normally (1975, 1990, 2005, 2010)
    myr <- MODEL_YEARS
    MODEL_YEARS <<- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)

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
    MODEL_BASE_YEARS <<- byr
    MODEL_FUTURE_YEARS <<- fyr
    MODEL_YEARS <<- myr
  })

}
