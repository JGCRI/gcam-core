# Test the driver

context("driver")

# find_chunks.old <- find_chunks
# chunk_inputs.old <- chunk_inputs

# Not much to do here yet

test_that("errors with bad input", {
  expect_error(driver(1))
  expect_error(driver("1"))
})

test_that("catches non-unique outputs", {
  chunknames <- c("test1", "test2")
  with_mock(
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
  chunknames <- c("test1", "test2")
  with_mock(
    find_chunks = function(...) tibble(name = chunknames),
    chunk_inputs = function(...) tibble(name = chunknames,
                                        input = c("i1", "i2"),
                                        from_file = c(TRUE, FALSE)),
    chunk_outputs = function(...) tibble(name = chunknames,
                                         output = c("o1", "o2")),
    expect_error(driver(), regexp = "Unfound inputs not marked as from file")
  )
})

test_that("catches lying chunks", {
  chunknames <- c("test1")
  with_mock(
    find_chunks = function(...) tibble(name = chunknames),
    chunk_inputs = function(...) tibble(name = chunknames,
                                        input = "i1",
                                        from_file = TRUE),
    chunk_outputs = function(...) tibble(name = chunknames,
                                         output = "o1"),
    run_chunk = function(...) { o2 <- tibble(); return_data(o2) },
    load_csv_files = function(...) { i1 <- tibble(); return_data(i1) },
    expect_error(driver(), regexp = "is not returning what it promised")
  )
})

test_that("catches stuck", {
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


test_that("runs", {
  # x <- capture_output({
  #   out <- driver()
  # })
  # expect_true(is.list(out))
})
