# Test the driver

context("driver")

test_that("catches bad input", {
  expect_error(driver(1, 1))
  expect_error(driver("1", 1))
  expect_error(driver(return_inputs_of = 1))
  expect_error(driver(return_outputs_of = 1))
  expect_error(driver(return_data_names = 1))
  expect_error(driver(return_data_map_only = 1))
  expect_error(driver(write_outputs = 1))
  expect_error(driver(quiet = 1))
})

# The following code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

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
                                          from_file = TRUE,
                                          optional = FALSE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o2"),
                                           to_xml = FALSE),
      load_csv_files = function(...) { i1 <- tibble(); return_data(i1) },
      run_chunk = function(...) {
        tibble() %>% add_title("o2") %>% add_units("units") %>%
          add_comments("comments") %>% add_legacy_name("legacy") %>%
          add_precursors("i1") -> o2
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
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("i1") -> o1
    expect_silent(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # Missing title
    tibble() %>% add_units("units") %>%
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # Missing legacy name
    tibble() %>% add_units("units") %>%
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # Missing units
    tibble() %>% add_title("o1") %>%
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # Missing comments
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_legacy_name("legacy") %>% add_precursors("i1") -> o1
    expect_warning(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # XML flag but not marked in input
    tibble() %>% add_title("o1") %>% add_flags(FLAG_XML) %>%
      add_precursors("i1") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # XML marked in input but no flag
    tibble() %>% add_title("o1") %>% add_precursors("i1") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po, TRUE))

    # No precursors at all, even with inputs
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_legacy_name("legacy") %>% add_comments("comments") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # An input that doesn't appear in any precursors
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_legacy_name("legacy") %>% add_comments("comments") %>% add_precursors("i2") -> o1
    expect_message(check_chunk_outputs("c1", return_data(o1), c("i1", "i2"), po, FALSE))

    # Recursive precursor
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_legacy_name("legacy") %>% add_comments("comments") %>% add_precursors("o1") -> o1
    expect_error(check_chunk_outputs("c1", return_data(o1), "i1", po, FALSE))

    # Precursor can be another chunk product
    po <- c("o1", "o2")  # promised outputs
    tibble() %>% add_title("o1") %>% add_units("units") %>%
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("i1") -> o1
    tibble() %>% add_title("o2") %>% add_units("units") %>%
      add_comments("comments") %>% add_legacy_name("legacy") %>%
      add_precursors("o1") -> o2
    expect_silent(check_chunk_outputs("c1", return_data(o1, o2), "i1", po, c(FALSE, FALSE)))
  })

  test_that("catches stuck", {
    # Create a couple (fake) chunks that depend on each other
    chunknames <- c("test1", "test2")
    with_mock(
      find_chunks = function(...) tibble(name = chunknames),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = FALSE,
                                          optional = FALSE),
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

  test_that("warn_data_injects works", {
    # No chunks using temp-data-inject data
    with_mock(
      find_chunks = function(...) tibble(name = c("A", "B", "C")),
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("Ai", "Ao", "Bo")),
      chunk_outputs = function(...) tibble(name = c("A", "B", "C"),
                                           output = c("Ao", "Bo", "Co")),
      expect_silent(warn_data_injects()),
      expect_equal(warn_data_injects(), 0)
    )

    # Chunks using temp-data-inject data because 'A' not enabled yet
    with_mock(
      find_chunks = function(include_disabled)
        if(include_disabled) {
          tibble(name = c("A", "B", "C"))
        } else {
          tibble(name = c("B", "C"))
        } ,
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("Ai", paste0(TEMP_DATA_INJECT, "Ao"), "Bo")),
      chunk_outputs = function(...) tibble(name = c("B", "C"),
                                           output = c("Bo", "Co")),
      expect_silent(warn_data_injects()),
      expect_equal(warn_data_injects(), 0)
    )

    # Chunk using temp-data-inject data but real data is available
    with_mock(
      find_chunks = function(...) tibble(name = c("A", "B", "C")),
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("Ai", paste0(TEMP_DATA_INJECT, "Ao"), "Bo")),
      chunk_outputs = function(...) tibble(name = c("A", "B", "C"),
                                           output = c("Ao", "Bo", "Co")),
      expect_message(warn_data_injects()),
      expect_equal(warn_data_injects(), 1)
    )
  })

  test_that("warn_datachunk_bypass works", {
    # No chunks bypassing data chunk
    with_mock(
      find_chunks = function(...) tibble(name = c("A", "B", "C")),
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("Ai", "Ao", "Bo"),
                                          from_file = c(TRUE, FALSE, FALSE)),
      chunk_outputs = function(...) tibble(name = c("A", "B", "C"),
                                           output = c("Ao", "Bo", "Co")),
      expect_silent(warn_datachunk_bypass()),
      expect_equal(warn_datachunk_bypass(), 0)
    )

    # Chunk bypassing a data chunk
    with_mock(
      find_chunks = function(...) tibble(name = c("dcA", "B", "C")),
      chunk_inputs = function(...) tibble(name = c("B", "C"),
                                          input = c("inst/extdata/Ao", "Bo"),
                                          from_file = c(TRUE, FALSE)),
      chunk_outputs = function(...) tibble(name = c("dcA", "B", "C"),
                                           output = c("Ao", "Bo", "Co")),
      expect_message(warn_datachunk_bypass()),
      expect_equal(warn_datachunk_bypass(), 1)
    )
  })

  test_that("warn_mismarked_fileinputs works", {
    # No chunks mismarking inputs
    with_mock(
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("inst/extdata/Ai", "Ao", "Bo"),
                                          from_file = c(TRUE, FALSE, FALSE)),
      expect_silent(warn_mismarked_fileinputs()),
      expect_equal(warn_mismarked_fileinputs(), 0)
    )

    # Chunk mismarking an input as from_file, when it's not
    with_mock(
      chunk_inputs = function(...) tibble(name = c("A", "B", "C"),
                                          input = c("inst/extdata/Ai", "Ao", "Bo"),
                                          from_file = c(TRUE, FALSE, TRUE)),
      expect_message(warn_mismarked_fileinputs()),
      expect_equal(warn_mismarked_fileinputs(), 1)
    )
  })

  test_that("tibbelize_outputs works", {
    # Catches bad input
    expect_error(tibbelize_outputs(1, "1"))
    expect_error(tibbelize_outputs(empty_data(), 1))

    chunkname <- "chunk"
    title <- "t"
    precs <- "p"
    unts <- "u"
    com1 <- "c1"
    com2 <- "c2"
    f1 <- FLAG_INPUT_DATA
    f2 <- FLAG_NO_TEST
    f3 <- FLAG_LONG_YEAR_FORM

    tibble(a = 1) %>%
      add_title(title) %>% add_precursors(precs) %>% add_units(unts) %>%
      add_comments(com1) %>% add_comments(com2) %>%
      add_flags(f1, f2, f3) -> x

    tb <- tibbelize_outputs(add_data(list(output = x), empty_data()), chunkname)

    expect_identical(tb$name, chunkname)
    expect_identical(tb$output, "output")
    expect_identical(tb$precursors, precs)
    expect_identical(tb$units, unts)
    expect_identical(tb$comments, paste(com1, com2, sep = driver.SEPARATOR))
    expect_identical(tb$flags, paste(f1, f2, f3, sep = driver.SEPARATOR))
  })
}
