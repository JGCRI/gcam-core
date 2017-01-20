# Test the driver

# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

context("driver")

if(require(mockr, warn.conflicts = FALSE)) {

  test_that("catches bad input", {
    expect_error(driver(1, 1))
    expect_error(driver("1", 1))
  })

  test_that("catches non-unique outputs", {
    # Create a couple (fake) chunks that produce the same thing
    chunknames <- c("test1", "test2")
    mockr::with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = TRUE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o1")),
      expect_error(driver(), regexp = "Outputs appear multiple times")
    )
  })

  test_that("catches unmarked file inputs", {
    # Create a (fake) chunk that hasn't marked its file inputs correctly
    chunknames <- c("test1", "test2")
    with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = c(TRUE, FALSE)),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o2")),
      expect_error(driver(), regexp = "not marked as from file")
    )
  })

  test_that("catches lying chunks", {
    # Create a (fake) chunk that declares and produces different outputs
    chunknames <- c("test1")
    with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = "i1",
                                          from_file = TRUE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = "o1"),
      load_csv_files = function(...) { i1 <- tibble(); return_data(i1) },
      run_chunk = function(...) { o2 <- tibble(); return_data(o2) },
      expect_error(driver(), regexp = "is not returning what it promised")
    )
  })

  test_that("catches stuck", {
    # Create a couple (fake) chunks that depend on each other
    chunknames <- c("test1", "test2")
    with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = FALSE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("i1", "i2")),
      expect_error(driver(), regexp = "we are stuck")
    )
  })

  test_that("run_chunk runs chunk", {
    with_mock(
      module_sample_sample = function(...) TRUE,
      expect_true(run_chunk("module_sample_sample", 1))
    )
  })

}
