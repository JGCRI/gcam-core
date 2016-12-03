# utils.R


#' load_csv
#'
#' Load an internal, i.e. included with the package, csv (or csv.gz) data file
#' @param filename Name of file to load
#' @param ... Any other parameter to pass to \code{read_csv}
#' @return data frame from file
load_csv <- function(filename, ...) {
  fqfn <- system.file("extdata", filename, package = "gcamdata")
  suppressMessages(readr::read_csv(fqfn, comment = "#", ...))
}


#' find_chunks
#'
#' Get a list of chunks in this package.
#' These are functions with a name of "module_{modulename}_{chunkname}"
#' @param pattern Regular expression pattern to search for
#' @return A data frame with fields 'name', 'module', and 'chunk'.
#' @importFrom magrittr "%>%"
find_chunks <- function(pattern = "^module_[a-zA-Z]*_.*$") {
  assertthat::assert_that(is.character(pattern))

  ls(name = parent.env(environment()), pattern = pattern) %>%
    tibble::tibble(name = .) %>%
    tidyr::separate(name, into = c("x", "module", "chunk"), remove = FALSE,
                    sep = "_", extra = "merge") %>%
    dplyr::select(-x)
}


#' chunk_dependencies
#'
#' @param chunklist A character vector of chunks names
#' @return A tibble with columns 'name' (chunk name) and 'input' (name of data)
chunk_dependencies <- function(chunks = find_chunks()$name) {
  # Get list of data required by each chunk
  chunkinputs <- list()
  for(ch in chunks) {
    cl <- call(ch, driver.DECLARE_INPUTS)
    reqdata <- eval(cl)
    if(!is.null(reqdata)) {
      chunkinputs[[ch]] <- tibble(name = ch, input = reqdata)
    }
  }
  dplyr::bind_rows(chunkinputs)
}
