# utils.R


#' load_csv_files
#'
#' Load one or more internal, i.e. included with the package, csv (or csv.gz) data files.
#' @param filenames Character vector of filenames to load
#' @param quiet Logical - suppress messages?
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frames read in are marked as inputs, not ones that have
#' been computed, via \code{\link{add_comments}}.
#' @return A list of data frames (tibbles).
#' @importFrom magrittr "%>%"
load_csv_files <- function(filenames, quiet = FALSE, ...) {
  assertthat::assert_that(is.character(filenames))
  assertthat::assert_that(is.logical(quiet))

  filedata <- list()
  for(f in filenames) {
    if(!quiet) cat("Loading", f, "...\n")
    fqfn <- find_csv_file(f, quiet = quiet)
    suppressMessages(readr::read_csv(fqfn, comment = COMMENT_CHAR, ...)) %>%
      parse_csv_header(fqfn) %>%
      add_comments(paste("Read from", gsub("^.*extdata", "extdata", fqfn))) %>%
      add_flags(FLAG_INPUT_DATA) ->
      filedata[[f]]

    # Title might have been filled in, or not
    if(is.null(get_title(filedata[[f]]))) {
      filedata[[f]] <- add_title(filedata[[f]], f)
    }

  }
  filedata
}


#' extract_header_info
#'
#' Extract information from CSV headers.
#'
#' @param header_lines Character vector holding raw header lines
#' @param label Label to search for - character
#' @param filename Filename (for error reporting purposes)
#' @param required Is this label required? (logical)
#' @param multiline Can this label hold multi-line information? (logical)
#' @details CSV files can have headers, commented lines of the form "# Title: xxxx",
#' "# Source: xxxx", etc. Extract this information if present. This function is called
#' by \code{\link{parse_csv_header}}.
#' @return Extracted label information, as a character vector
extract_header_info <- function(header_lines, label, filename, required = FALSE, multiline = FALSE) {
  assert_that(is.character(header_lines))
  assert_that(is.character(label))
  assert_that(is.character(filename))
  assert_that(is.logical(required))
  assert_that(is.logical(multiline))

  label_regex <- paste0("^", COMMENT_CHAR, "\\s*", trimws(label))
  label_line <- grep(label_regex, header_lines)
  if(length(label_line) > 1) {
    stop("Header label ", label, " appears on >1 line in ", basename(filename))
  } else if(length(label_line) == 1) {
    if(multiline) {
      # Multiline comments may end with the last comment line before data...
      comment_end1 <- max(grep(paste0("^", COMMENT_CHAR), header_lines))
      # ...or at the next label (xxx:)...
      all_label_lines <- grep(paste0("^", COMMENT_CHAR, "\\s*[a-zA-Z]*:"), header_lines)
      if(any(all_label_lines > label_line)) {
        comment_end2 <- min(all_label_lines[all_label_lines > label_line])
      } else {
        comment_end2 <- NA  # no next label
      }
      # ... whichever comes first
      comment_end <- min(comment_end1, comment_end2 - 1, na.rm = TRUE)
    } else {
      comment_end <- label_line
    }
    # Pull out information and return
    header_lines[label_line:comment_end] %>%
      gsub(label_regex, "", .) %>%
      gsub(paste0("^", COMMENT_CHAR), "", .) %>%
      trimws
  } else {
    if(required) {
      stop("Required metadata label '", label, "not found in ", basename(filename))
    }
    NULL   # label not present
  }
}


#' parse_csv_header
#'
#' Parse a CSV file's header, if present.
#'
#' @param obj The object to attach attributes to
#' @param filename Fully-qualified filename
#' @param n Number of lines to read from the beginning of the file
#' @param enforce_requirements Enforce mandatory fields?
#' @details Headers are given at the top of files and consist of labels ("Title:", "Units:", etc)
#' prefixed by comment characters (#). The parser looks for these, and calls \code{\link{add_title}} and
#' similar functions to return an empty data frame with appropriate attribute set.
#' @return An empty \code{\link{tibble}} with appropriate attributes filled in.
#' @export
parse_csv_header <- function(obj, filename, n = 20, enforce_requirements = FALSE) {
  assert_that(tibble::is_tibble(obj))
  assert_that(is.character(filename))
  assert_that(is.numeric(n))
  assert_that(is.logical(enforce_requirements))
  assert_that(file.exists(filename))

  # File may be compressed; handle this via a connection
  if(grepl("\\.gz", filename)) {
    con <- gzfile(filename)
  } else if(grepl("\\.zip", filename)) {
    con <- unz(filename, filename = basename(gsub("\\.zip$", "", filename)))
  } else {
    con <- file(filename)
  }

  x <- readLines(con, n = n)
  close(con)

  # Excel tries to be 'helpful' and, when working with CSV files, quotes lines with
  # commas in them...which you CAN'T SEE when re-opening in Excel. Trap this problem.
  if(any(grepl(paste0('^"', COMMENT_CHAR), x))) {
    stop('A quoted comment (# prefixed by a double quote, probably due to Excel) detected in ', basename(filename))
  }

  obj %>%
    add_title(extract_header_info(x, "Title:", filename, required = enforce_requirements)) %>%
    add_units(extract_header_info(x, "Units:", filename, required = enforce_requirements)) %>%
    add_comments(extract_header_info(x, "Comments:", filename, multiline = TRUE)) %>%
    add_comments(extract_header_info(x, "Description:", filename, multiline = TRUE)) %>%
    add_comments(extract_header_info(x, "References?:", filename))
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
  assertthat::assert_that(is_data_list(chunkdata))
  assertthat::assert_that(!is.null(names(chunkdata)))
  assertthat::assert_that(is.logical(write_inputs))
  assertthat::assert_that(is.character(outputs_dir))

  dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)
  for(cn in names(chunkdata)) {
    fqfn <- file.path(outputs_dir, paste0(cn, ".csv"))
    suppressWarnings(file.remove(fqfn))

    cd <- chunkdata[[cn]]
    cmnts <- get_comments(cd)
    flags <- get_flags(cd)

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
#' @param include_disabled Return names of disabled chunks?
#' @return A data frame with fields 'name', 'module', and 'chunk'.
#' @details If a chunk name ends with \code{_DISABLED}, by default its name
#' will not be returned.
#' @importFrom magrittr "%>%"
#' @export
find_chunks <- function(pattern = "^module_[a-zA-Z\\.]*_.*$", include_disabled = FALSE) {
  assertthat::assert_that(is.character(pattern))

  ls(name = parent.env(environment()), pattern = pattern) %>%
    tibble::tibble(name = .,
                   disabled = grepl("_DISABLED$", name)) %>%
    filter(include_disabled | !disabled) %>%
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

  chunkoutputs <- list()
  for(ch in chunks) {
    cl <- call(ch, driver.DECLARE_OUTPUTS)
    reqdata <- eval(cl)

    # Chunks mark any XML file outputs using vector names
    if(is.null(names(reqdata))) {
      fileoutputs <- FALSE
    } else {
      fileoutputs <- names(reqdata) == "XML"
    }
    if(!is.null(reqdata)) {
      chunkoutputs[[ch]] <- tibble(name = ch, output = reqdata, to_xml = fileoutputs)
    }
  }
  dplyr::bind_rows(chunkoutputs)
}

