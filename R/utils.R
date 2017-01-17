# utils.R


#' load_csv_file
#'
#' Load one or more internal, i.e. included with the package, csv (or csv.gz) data files.
#' @param filename Filename to load
#' @param quiet Logical - suppress messages?
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frames read in are marked as inputs, not ones that have
#' been computed, via \code{\link{add_dscomments}}.
#' @return A list of data frames (tibbles).
#' @importFrom magrittr "%>%"
load_csv_file <- function(filename, quiet = FALSE, ...) {
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(length(filename) == 1)
  assertthat::assert_that(is.logical(quiet))

  if(!quiet) cat("Loading", filename, "...\n")
  fqfn <- find_csv_file(filename, quiet = quiet)
  suppressMessages(readr::read_csv(fqfn, comment = COMMENT_CHAR, ...)) %>%
    add_dsflags(FLAG_INPUT_DATA)
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
#' @param outputs_dir Directory to save data into
#' Write data produced by chunks to csv files.
save_chunkdata <- function(chunkdata, write_inputs = FALSE, outputs_dir = OUTPUTS_DIR) {
  assertthat::assert_that(is.list(chunkdata))
  assertthat::assert_that(!is.null(names(chunkdata)))
  assertthat::assert_that(is.logical(write_inputs))
  assertthat::assert_that(is.character(outputs_dir))

  dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)
  for(cn in names(chunkdata)) {
    fqfn <- file.path(outputs_dir, paste0(cn, ".csv"))
    suppressWarnings(file.remove(fqfn))

    cd <- chunkdata[[cn]]
    cmnts <- get_dscomments(cd)
    flags <- get_dsflags(cd)

    # If these data have been tagged as input data, don't write
    if(FLAG_NO_OUTPUT %in% flags |
       FLAG_INPUT_DATA %in% flags & !write_inputs) {
      next
    }

    # If data is in a different from for original data system, indicate
    # that by writing to first line of file
    if(!is.null(flags)) {
      cat(paste(flags, collapse = " "), file = fqfn, sep = "\n")
    }

    if(!is.null(cmnts)) {
      cat(paste(COMMENT_CHAR, cmnts), file = fqfn, sep = "\n", append = TRUE)
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
find_chunks <- function(pattern = "^module_[a-zA-Z-]*_.*$") {
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

    # Chunks mark their file inputs specially, using vector names
    if(is.null(names(reqdata))) {
      fileinputs <- FALSE
    } else {
      fileinputs <- names(reqdata) == "FILE"
    }
    if(!is.null(reqdata)) {
      chunkinputs[[ch]] <- tibble(name = ch, input = reqdata, from_file = fileinputs)
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


#' approx_fun
#'
#' \code{\link{approx}} for use in a dplyr pipeline.
#'
#' @param year Numeric year, in a melted tibble or data frame
#' @param value Numeric value to interpolate
#' @param rule Rule to use; see \code{\link{approx}} and details
#' @details This was \code{gcam_interp} in the original data system.
#' @return Interpolated values.
#' @export
#' @examples
#' df <- data.frame(year = 1:5, value = c(1, 2, NA, 4, 5))
#' approx_fun(df$year, df$value, rule = 2)
approx_fun <- function(year, value, rule = 1) {
  if(rule == 1 | rule == 2 ) {
    stats::approx(as.vector(year), value, rule = rule, xout = year)$y
  } else {
    stop("Not implemented yet!")
  }
}
