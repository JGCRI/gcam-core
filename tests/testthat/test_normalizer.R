context("normalizer")

test_that("input file normalizer works", {

  # Copy our test files to a temp directory
  td <- file.path(tempdir(), "testing")
  if(!dir.exists(td)) {
    dir.create(td)
  }
  oldfiles <- list.files("line_ending_files/", full.names = TRUE)
  for(f in oldfiles) {
    file.copy(f, file.path(td, basename(f)))
  }

  # Run the normalizer
  expect_message(normalize_files(td))

  # Sanity check
  newfiles <- list.files(td, full.names = TRUE)
  expect_equal(length(oldfiles), length(newfiles))

  # The two compressed files are tiny and should have been uncompressed
  expect_true(all(grepl("\\.csv$", newfiles)))

  # We could check that the no-final-line file has one added, that files aren't
  # changed, etc., but that seems like overkill. We'll know (from trying to run
  # driver) whether there's a problem or not.

  # Clean up
  file.remove(newfiles)
})
