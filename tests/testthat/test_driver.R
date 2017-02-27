# Test the driver

# This code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

context("driver")

if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

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
      expect_error(driver(quiet = TRUE), regexp = "Outputs appear multiple times")
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
      expect_error(driver(quiet = TRUE), regexp = "not marked as from file")
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
      run_chunk = function(...) {
        tibble() %>% add_title("o2") %>% add_units("units") %>%
          add_comments("comments") %>% add_precursors("i1") -> o2
        return_data(o2)
      },
      expect_error(driver(quiet = TRUE), regexp = "is not returning what it promised")
    )
  })

  test_that("check_chunk_outputs works", {
    chunk <- "test1"
    po <- "o1"  # promised outputs

    # Nothing missing
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_comments("comments") %>% add_precursors("i1") -> o1
    expect_silent(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # Missing title
    tibble() %>% add_units("units") %>%
      add_comments("comments") %>% add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # Missing units
    tibble() %>% add_title("o1") %>%
      add_comments("comments") %>% add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # Missing comments
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # No precursors at all, even with inputs
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_comments("comments") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # Recursive precursor
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_comments("comments") %>% add_precursors("o1") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po))

    # Precursor can be another chunk product
    po <- c("o1", "o2")  # promised outputs
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_comments("comments") %>% add_precursors("i1") -> o1
    tibble() %>% add_title("o2") %>% add_units("units") %>%
      add_comments("comments") %>% add_precursors("o1") -> o2
    expect_silent(check_chunk_outputs("c1", return_data(o1, o2), "i1", po))
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
      expect_error(driver(quiet = TRUE), regexp = "we are stuck")
    )
  })

  test_that("run_chunk runs chunk", {
    with_mock(
      module_sample_sample = function(...) TRUE,
      expect_true(run_chunk("module_sample_sample", 1))
    )
  })

}
