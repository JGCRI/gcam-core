# Test the chunk grapher

context("chunk_readylist")

# The following code is written using the `mockr` package, currently only
# available via GitHub. Apparently `testthat::with_mock` is going
# to be deprecated soon.

if(require(mockr, quietly = TRUE, warn.conflicts = FALSE)) {

  test_that("runs-no dependencies", {
    # Create a couple (fake) chunks that don't depend on each other
    chunknames <- c("test1", "test2")
    mockr::with_mock(
      find_chunks = function(...) tibble(name = chunknames,
                                         module = c("m1", "m2"),
                                         chunk = chunknames,
                                         disabled = c(TRUE, FALSE)),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = TRUE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o2"),
                                           to_xml = FALSE),
      # output should be a tibble data frame
      expect_is(chunk_readylist(), "tbl_df"),
      # only 1 row - enabled chunks don't appear
      expect_equal(nrow(chunk_readylist()), 1),
      # no downstream dependencies
      expect_equal(count_downstream_dependencies(chunknames[1], find_chunks(),
                                      chunk_inputs(), chunk_outputs())[[1]], 0),
      expect_equal(count_downstream_dependencies(chunknames[2], find_chunks(),
                                      chunk_inputs(), chunk_outputs())[[1]], 0)
    )
  })

  test_that("runs-dependencies", {
    # Create a couple (fake) chunks that depend on each other
    chunknames <- c("test1", "test2")
    mockr::with_mock(
      find_chunks = function(...) tibble(name = chunknames,
                                         module = c("m1", "m2"),
                                         chunk = chunknames,
                                         disabled = c(TRUE, TRUE)),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "o1"),
                                          from_file = c(TRUE, FALSE)),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o2"),
                                           to_xml = FALSE),
      # output should be a tibble data frame
      expect_is(chunk_readylist(), "tbl_df"),
      # only 1 row - both chunks are disabled
      expect_equal(nrow(chunk_readylist()), 2),
      # each chunk has one input
      expect_equal(chunk_readylist()$n_inputs, c(1, 1)),
      # first chunk's inputs are available, second's are not
      expect_equal(chunk_readylist()$n_avail, c(1, 0)),
      # downstream dependency count
      expect_equal(chunk_readylist()$n_deps_total, c(1, 0)),
      expect_equal(count_downstream_dependencies(chunknames[1], find_chunks(),
                                                 chunk_inputs(), chunk_outputs())[[1]], 1),
      expect_equal(count_downstream_dependencies(chunknames[2], find_chunks(),
                                                 chunk_inputs(), chunk_outputs())[[1]], 0)
    )
  })
}
