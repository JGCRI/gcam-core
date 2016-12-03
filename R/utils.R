# utils.R


#' load_csv
#'
#' Load an internal, i.e. included with the package, csv (or csv.gz) data file
#' @param filename Name of file to load
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @return data frame from file
load_csv <- function(filename, ...) {
  assertthat::assert_that(is.character(filename))
  fqfn <- system.file("extdata", filename, package = "gcamdata")
  suppressMessages(readr::read_csv(fqfn, comment = "#", ...))
}


#' save_chunkdata
#'
#' @param chunkdata Named list of data frames to write
#' Write data produced by chunks to csv files.
#' @param chunkdata List of tibbles to write
save_chunkdata <- function(chunkdata) {
  assertthat::assert_that(is.list(chunkdata))
  assertthat::assert_that(!is.null(names(chunkdata)))

  dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)
  for(cn in names(chunkdata)) {
    fqfn <- file.path(OUTPUTS_DIR, paste0(cn, ".csv"))
    suppressWarnings(file.remove(fqfn))

    cd <- chunkdata[[cn]]
    if(!is.null(comment(cd))) {
      cat(paste("#", comment(cd)), file = fqfn, sep = "\n")
    }
    readr::write_csv(cd, fqfn, append = TRUE, col_names = TRUE)
  }
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
#' @param chunks A character vector of chunks names
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
