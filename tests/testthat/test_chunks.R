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
    cl <- call(ch, driver.MAKE, empty_data())
    expect_error(eval(cl), label = ch)
  }
})

test_that("doesn't use forbidden calls", {
  chunklist <- find_chunks()
  forbiddens <- c("[^error_no_]match", "ifelse", "melt", "cast", "rbind", "cbind")

  for(ch in unique(chunklist$name)) {
    code <- capture.output(getFromNamespace(ch, ns = "gcamdata"))
    code <- gsub("#.*$", "", code)  # remove comments
    code <- gsub('".*"', "", code)   # remove quotes
    for(f in forbiddens) {
      expect_equal(grep(f, code), integer(),   # should be no matches
                      info = paste(ch, "uses", f))
    }
  }
})
