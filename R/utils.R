# utils.R


#' load_csv
#'
#' Load an internal, i.e. included with the package, csv (or csv.gz) data file
#' @param filename Name of file to load
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frame read in is marked as an input, not one that has
#' been computed, via \code{\link{add_dscomments}}.
#' @return data frame from file
load_csv <- function(filename, ...) {
  assertthat::assert_that(is.character(filename))
  fqfn <- system.file("extdata", filename, package = "gcamdata")
  df <- suppressMessages(readr::read_csv(fqfn, comment = "#", ...))
  add_dscomments(df, INPUT_DATA_MARKER)
}


#' save_chunkdata
#'
#' @param chunkdata Named list of tibbles (data frames) to write
#' @param write_inputs Write data that were read as inputs, not computed?
#' Write data produced by chunks to csv files.
save_chunkdata <- function(chunkdata, write_inputs = FALSE) {
  assertthat::assert_that(is.list(chunkdata))
  assertthat::assert_that(!is.null(names(chunkdata)))

  dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)
  for(cn in names(chunkdata)) {
    fqfn <- file.path(OUTPUTS_DIR, paste0(cn, ".csv"))
    suppressWarnings(file.remove(fqfn))

    cd <- chunkdata[[cn]]
    cmnts <- get_dscomments(cd)

    # If data is in a different from for original data system, indicate
    # that by writing to first line of file
    if(LONG_NO_X_FORM %in% cmnts) {
      cat(LONG_NO_X_FORM, file = fqfn, sep = "\n")
    }

    # If these data have been tagged as input data, don't write
    if(INPUT_DATA_MARKER %in% cmnts & !write_inputs) {
      next
    }

    if(!is.null(cmnts)) {
      cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
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


#' add_dscomments
#'
#' @param x An object
#' @param comments A character vector of comments
#' @return \code{x} with comments appended to any existing comments
add_dscomments <- function(x, comments) {
  assertthat::assert_that(is.character(comments))
  comment(x) <- c(comment(x), comments)
  x
}

#' get_dscomments
#'
#' @param x An object
#' @return comments attached to \code{x}
get_dscomments <- function(x) {
  comment(x)
}
