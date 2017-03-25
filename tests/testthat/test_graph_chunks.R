# Test the chunk grapher

context("graph_chunks")

test_that("catches bad input", {
  expect_error(graph_chunks(1))
  expect_error(graph_chunks("x", plot_gcam = 1))
  expect_error(graph_chunks("x", include_disabled = 1))
  expect_error(graph_chunks("x", quiet = 1))
})

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
                                         disabled = FALSE),
      chunk_inputs = function(...) tibble(name = chunknames,
                                          input = c("i1", "i2"),
                                          from_file = TRUE),
      chunk_outputs = function(...) tibble(name = chunknames,
                                           output = c("o1", "o2"),
                                           to_xml = FALSE),
      # output should be a numeric matrix
      expect_is(graph_chunks(), "matrix"),
      expect_equal(dim(graph_chunks()), c(2, 2)),
      expect_equal(colnames(graph_chunks()), chunknames),
      # no dependencies
      expect_true(all(graph_chunks() == 0)),
      # filter works
      expect_equal(dim(graph_chunks(module_filter = "m1")), c(1, 1)),
      # filter for nonexistent module
      expect_warning(graph_chunks(module_filter = "xxxxx"))
    )
  })

    test_that("runs-dependencies", {
      # Create a couple (fake) chunks that depend on each other
      # One writes xml and is disabled
      chunknames <- c("test1", "test2")
      mockr::with_mock(
        find_chunks = function(...) tibble(name = chunknames,
                                           module = c("m1", "m2"),
                                           chunk = chunknames,
                                           disabled = c(FALSE, TRUE)),
        chunk_inputs = function(...) tibble(name = chunknames,
                                            input = c("i1", "o1"),
                                            from_file = c(TRUE, FALSE)),
        chunk_outputs = function(...) tibble(name = chunknames,
                                             output = c("o1", "o2"),
                                             to_xml = c(FALSE, TRUE)),
        # output should be a numeric matrix
        expect_is(graph_chunks(), "matrix"),
        expect_equal(dim(graph_chunks(include_disabled = FALSE)), c(1, 1)),
        expect_equal(dim(graph_chunks(include_disabled = TRUE)), c(2, 2)),
        # adds a node for gcam
        expect_equal(dim(graph_chunks(include_disabled = TRUE,
                                      plot_gcam = TRUE)), c(3, 3)),
        # dependencies
        expect_equal(sum(graph_chunks(include_disabled = TRUE) > 0), 1)
      )
    })


}
