# onload.R

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.gcamdata <- list(
    gcamdata.use_java = FALSE
  )
  toset <- !(names(op.gcamdata) %in% names(op))
  if (any(toset)) {
    options(op.gcamdata[toset])
  }
  invisible()
}
