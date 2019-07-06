# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# onload.R

.onLoad <- function(libname, pkgname) {

  # Older versions of readr produce incorrect results in gcamdata (#1086)
  # This is bad.
  readr_version <- utils::packageVersion("readr")
  if (!(readr_version >= "1.3")) {
    stop("Need readr >= v1.3, but installed version is ", readr_version)
  }

  op <- options()
  op.gcamdata <- list(
    gcamdata.use_java = TRUE
  )
  if(Sys.getenv("GCAMDATA_USE_JAVA") != "") {
    op.gcamdata[["gcamdata.use_java"]] <- isTRUE(as.logical(Sys.getenv("GCAMDATA_USE_JAVA")))
  }
  toset <- !(names(op.gcamdata) %in% names(op))
  if(any(toset)) {
    options(op.gcamdata[toset])
  }
  invisible()
}
