# General tests, common to all chunks

context("chunks")

test_that("errors on unknown command", {
  chunklist <- find_chunks()

  for(ch in chunklist$name) {
    cl <- call(ch, "THISMAKESNOSENSE")
    expect_error(eval(cl), label = ch)
  }
})

test_that("handles DECLARE_INPUTS", {
  chunklist <- find_chunks()

  for(ch in chunklist$name) {
    cl <- call(ch, driver.DECLARE_INPUTS)
    inputs <- eval(cl)
    expect_true(is.null(inputs) |  # might be no inputs
                  is.character(inputs) & is.vector(inputs), label = ch)
  }
})

test_that("handles DECLARE_OUTPUTS", {
  chunklist <- find_chunks()

  for(ch in chunklist$name) {
    cl <- call(ch, driver.DECLARE_OUTPUTS)
    inputs <- eval(cl)
    expect_true(is.character(inputs) & is.vector(inputs), label = ch)
  }
})

test_that("errors if required data not available", {
  chunkdeps <- chunk_inputs()

  for(ch in unique(chunkdeps$name)) {
    cl <- call(ch, driver.MAKE, NULL)
    expect_error(eval(cl), label = ch)
  }
})

test_that("DECLARE_OUTPUTS and MAKE names match if no dependencies", {
  chunklist <- find_chunks()
  chunkdeps <- chunk_inputs()
  nodeps <- setdiff(chunklist$name, chunkdeps$name)

  for(ch in nodeps) {
    cl <- call(ch, driver.DECLARE_OUTPUTS)
    chnames <- eval(cl)
    cl <- call(ch, driver.MAKE, NULL)
    chdata <- eval(cl)

    expect_equal(sort(chnames), sort(names(chdata)), label = ch)
  }
})
