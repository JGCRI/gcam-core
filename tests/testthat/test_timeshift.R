# Test the chunks - do they handle different conditions?

# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

context("timeshift")


if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

  test_that("chunks handle timeshift", {

    hy <- HISTORICAL_YEARS
    HISTORICAL_YEARS <<- HISTORICAL_YEARS + 1
    ahy <- AGLU_HISTORICAL_YEARS
    AGLU_HISTORICAL_YEARS <<- AGLU_HISTORICAL_YEARS + 1
    fhy <- FAO_HISTORICAL_YEARS
    FAO_HISTORICAL_YEARS <<- FAO_HISTORICAL_YEARS + 1

    # FUTURE_YEARS needs to be adjusted by 5, because in `gcam-usa_LA100.Socioeconomics_makedata`
    # the future data is 2010, 2015, etc., and a one-year shift means no future years at all.
    fy <- FUTURE_YEARS
    FUTURE_YEARS <- FUTURE_YEARS + 5

    with_mock(
      run_chunk = function(chunk, all_data) {
        x <- do.call(chunk, list(driver.MAKE, all_data))
        expect_is(x, "list", info = paste("Timeshift error invoking", chunk))
        x
      },
      try(driver(quiet = TRUE, write_outputs = FALSE))
    )

    # Reset to what it was before
    HISTORICAL_YEARS <<- hy
    AGLU_HISTORICAL_YEARS <<- ahy
    FAO_HISTORICAL_YEARS <<- fhy
    FUTURE_YEARS <<- fy

  })

}
