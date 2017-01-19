# Test global constants for proper capitalization convention
# E.g. "XXX" and "driver.XXX" are OK, but "xxx" is not

context("globals")


test_that("constants are capitalized correctly", {
  ns <- getNamespace("gcamdata")
  objnames <- ls(ns, all.names = FALSE)

  for(oname in objnames) {
    obj <- get(oname, envir = ns)
    if(!is.function(obj)) {

      # Globals may have an initial qualifier that's lowercase, but after
      # that needs to be all uppercase
      ss <- unlist(strsplit(oname, split = "\\."))
      if(length(ss) > 1) {
        ss <- ss[-1]
      }
      expect_identical(ss, toupper(ss), label = paste(oname, "incorrectly capitalized"))
    }
  }
})
