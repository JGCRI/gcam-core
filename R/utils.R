# utils.R


#' load_csv_files
#'
#' Load one or more internal, i.e. included with the package, csv (or csv.gz) data files.
#' @param filenames Character vector of filenames to load
#' @param quiet Logical - suppress messages?
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frames read in are marked as inputs, not ones that have
#' been computed, via \code{\link{add_dscomments}}.
#' @return A list of data frames (tibbles).
#' @importFrom magrittr "%>%"
load_csv_files <- function(filenames, quiet = FALSE, ...) {
  assertthat::assert_that(is.character(filenames))
  assertthat::assert_that(is.logical(quiet))
  filedata <- list()
  for(f in filenames) {
    if(!quiet) cat("Loading", f, "...\n")
    fqfn <- find_csv_file(f, quiet = quiet)
    suppressMessages(readr::read_csv(fqfn, comment = "#", ...)) %>%
      add_dscomments(INPUT_DATA_MARKER) ->
      filedata[[f]]
  }
  filedata
}


#' find_csv_file
#'
#' Find an internal, i.e. included with the package, data file.
#' @param filename Filename (extension optional) to find
#' @param quiet Logical - suppress messages?
#' @return Full name of file.
find_csv_file <- function(filename, quiet = FALSE) {
  assertthat::assert_that(is.character(filename))
  assert_that(assert_that(length(filename) == 1))
  assertthat::assert_that(is.logical(quiet))

  extensions <- c("", ".csv", ".csv.gz", ".csv.zip")
  for(ex in extensions) {
    fqfn <- system.file("extdata", paste0(filename, ex), package = "gcamdata")
    if(fqfn != "") {
      if(!quiet) cat("Found", fqfn, "\n")
      return(fqfn)  # found it
    }
  }
  stop("Couldn't find required data ", filename)
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
#' These are functions with a name of "module_{modulename}_{chunkname}".
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


#' chunk_inputs
#'
#' @param chunks A character vector of chunks names
#' @return A tibble with columns 'name' (chunk name) and 'input' (name of data)
#' @export
chunk_inputs <- function(chunks = find_chunks()$name) {
  assertthat::assert_that(is.character(chunks))

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


#' chunk_outputs
#'
#' List all chunk outputs.
#'
#' @param chunks A character vector of chunks names
#' @return A tibble with columns 'name' (chunk name) and 'output' (name of data)
#' @export
chunk_outputs <- function(chunks = find_chunks()$name) {
  assertthat::assert_that(is.character(chunks))

  chunkinputs <- list()
  for(ch in chunks) {
    cl <- call(ch, driver.DECLARE_OUTPUTS)
    reqdata <- eval(cl)
    if(!is.null(reqdata)) {
      chunkinputs[[ch]] <- tibble(name = ch, output = reqdata)
    }
  }
  dplyr::bind_rows(chunkinputs)
}


#' add_dscomments
#'
#' @param x An object
#' @param comments A character vector of comments
#' @return \code{x} with comments appended to any existing comments.
add_dscomments <- function(x, comments) {
  assertthat::assert_that(is.character(comments))
  comment(x) <- c(comment(x), comments)
  x
}


#' get_dscomments
#'
#' @param x An object
#' @return Comments attached to \code{x}.
get_dscomments <- function(x) {
  comment(x)
}


#' getdata
#'
#' This function returns a tibble (currently) in \code{all_data},
#' abstracting away the mechanism for accessing it from the chunks.
#'
#' @param all_data Data structure
#' @param name Name of data to return
#' @return Data object (currently, a tibble or data frame).
get_data <- function(all_data, name) {
  all_data[[name]]
}


#' return_data
#'
#' Construct a data structure of objects (\code{...}) and return it.
#' Abstracts this away from chunk function code.
#' @param ... Objects to handle
#' @return Object ready for insertion into the data system data structure.
return_data <- function(...) {
  dots <- list(...)
  names(dots) <- as.list(substitute(list(...)))[-1L]
  dots
}


#' empty_data
#'
#' @return An empty data store
empty_data <- function() { list() }


#' add_data
#'
#' Add \code{data_list} to an existing data store.
#'
#' @param data_list List of data frames or other objects
#' @param all_data An existing (possibly empty) data store
#'
#' @return An empty data store
add_data <- function(data_list, all_data) {
  assertthat::assert_that(!is.null(names(data_list)))

  for(d in names(data_list)) {
    all_data[[d]] <- data_list[[d]]
  }
  all_data
}
