# utils.R


#' load_csv
#'
#' Load an internal, i.e. included with the package, csv (or csv.gz) data file
#' @param filename Name of file to load
#' @param ... Any other parameter to pass to \code{read_csv}
#' @return data frame from file
#' @export
load_csv <- function(filename, ...) {
  fqfn <- system.file("extdata", filename, package = "gcamdata")
  suppressMessages(readr::read_csv(fqfn, comment = "#", ...))
}
