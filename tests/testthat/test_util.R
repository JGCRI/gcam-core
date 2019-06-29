context("util")

test_that("screening for use of forbidden functions works", {
  testgood_L20 <- function(d1, d2) {
    # This function should pass because we only mention
    # match and ifelse in comments and strings
    foo <- left_join_error_no_match(d1, d2)
    message('The "match" above passes because it is part of left_join_error_no_match')
    foo <- gather(d1, year, value, matches(YEAR_PATTERN))
    message('The "match" above passes because it is dplyr::matches')

    # We reference model time constants but that's OK as we're a level2 function
    x <- MODEL_BASE_YEARS
    y <- MODEL_FUTURE_YEARS
  }

  testbad_L10 <- function(x, d) {
    # This function makes no sense, but it does use all of the currently forbidden functions,
    # including match
    y <- ifelse(x < 0,0, x)
    z <- ifelse(x > 10, 10, x)
    d %>%
      mutate(x = 1) %>%
      mutate(y = 2)
    dd <- melt(d)
    df <- cast(dd)
    dfm <- merge(d, dd)
    dx <- cbind(d, x)
    wtf <- rbind(d, df)
    gtfo <- match(x, y)
    mby <- MODEL_BASE_YEARS
    mfy <- MODEL_FUTURE_YEARS
  }

  expect_equal(screen_forbidden(testgood_L20), character())
  tb <- screen_forbidden(testbad_L10)
  expect_equivalent(tb[,1], c("consecutive mutate calls", "(?<!error_no_)match(?!es)",
                         "ifelse", "ifelse", "melt", "cast", "rbind",
                         "cbind", "merge", "MODEL_BASE_YEARS", "MODEL_FUTURE_YEARS"))
})


# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

  test_that("inputs_of and outputs_of work", {

    expect_null(inputs_of(NULL))
    expect_null(inputs_of(""))
    expect_null(outputs_of(NULL))
    expect_null(outputs_of(""))

    chunknames <- c("test1", "test2")
    mockr::with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(chunks, ....) filter(tibble(name = chunknames,
                                                          input = c("i1", "i2"),
                                                          from_file = TRUE), name == chunks),
      chunk_outputs = function(chunks, ...) filter(tibble(name = chunknames,
                                                          output = c("o1", "o2")), name == chunks),
      expect_identical(inputs_of("test1"), "i1"),
      expect_identical(outputs_of("test2"), "o2")
    )
  })

}
