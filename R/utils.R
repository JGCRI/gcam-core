# utils.R

#' load_csv_files
#'
#' Load one or more internal, i.e. included with the package, csv (or csv.gz) data files.
#' @param filenames Character vector of filenames to load
#' @param optionals Logical vector, specifying whether corresponding file is optional
#' @param quiet Logical - suppress messages?
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frames read in are marked as inputs, not ones that have
#' been computed, via \code{\link{add_comments}}. Optional files that are not found
#' as returned as NA in the list.
#' @return A list of data frames (tibbles).
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
load_csv_files <- function(filenames, optionals, quiet = FALSE, ...) {
  assert_that(is.character(filenames))
  assert_that(is.logical(optionals))
  assert_that(is.logical(quiet))
  assert_that(length(filenames) == length(optionals))

  # Remove duplicates
  optionals <- optionals[!duplicated(filenames)]
  filenames <- filenames[!duplicated(filenames)]

  filedata <- list()
  for(fnum in seq_along(filenames)) {
    f <- filenames[fnum]
    if(!quiet) cat("Loading", f, "...\n")
    fqfn <- find_csv_file(f, optionals[fnum], quiet = quiet)

    if(is.null(fqfn)) {
      assert_that(optionals[fnum]) # if we get back a NULL, file has to be optional
      filedata[[f]] <- missing_data()
      if(!quiet) message("Note: optional input ", f, "not found")
      next
    }
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
#' "# Source: xxxx", etc. Extract this information if present. Note that empty headers
#' are not allowed. This function is called by \code{\link{parse_csv_header}}.
#' @return Extracted label information, as a character vector
extract_header_info <- function(header_lines, label, filename, required = FALSE, multiline = FALSE) {

  . <- NULL                             # silence notes on package check.

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
      trimws ->
      info

    if(nchar(paste(info, collapse = "")) == 0) {
      stop("Empty metadata label '", label, "' found in ", basename(filename))
    }
    return(info)
  } else {
    if(required) {
      stop("Required metadata label '", label, "' not found in ", basename(filename))
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
parse_csv_header <- function(obj, filename, n = 20, enforce_requirements = TRUE) {
  assert_that(tibble::is_tibble(obj))
  assert_that(is.character(filename))
  assert_that(is.numeric(n))
  assert_that(is.logical(enforce_requirements))
  assert_that(file.exists(filename))

  # TEMPORARY: don't enforce metadata, for test, for data injection (from old d.s.)
  if(isTRUE(grepl(TEMP_DATA_INJECT, filename, fixed = TRUE))) {
    enforce_requirements <- FALSE
    obj <- add_flags(obj, FLAG_NO_TEST)
  }

  # File may be compressed; handle this via a connection
  if(grepl("\\.gz$", filename)) {
    con <- gzfile(filename)
  } else if(grepl("\\.zip$", filename)) {
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

  # The 'File:' field has to match the actual filename
  filecheck <- extract_header_info(x, "File:", filename, required = enforce_requirements)
  # Remove trailing commas - stupid Excel
  filecheck <- gsub(",*$", "", filecheck)
  # Remove any compression extension
  filename_clean <- gsub("\\.(zip|gz)$", "", filename) %>% basename
  if(enforce_requirements & !identical(filecheck, filename_clean)) {
    stop("'File:' given in header (", filecheck, ") doesn't match filename in ", filename)
  }

  obj %>%
    add_title(extract_header_info(x, "Title:", filename, required = enforce_requirements)) %>%
    add_units(extract_header_info(x, "Units?:", filename, required = enforce_requirements)) %>%
    add_comments(extract_header_info(x, "(Comments|Description):", filename, multiline = TRUE)) %>%
    add_reference(extract_header_info(x, "(References?|Sources?):", filename, multiline = TRUE))
}


#' find_csv_file
#'
#' Find an internal, i.e. included with the package, data file.
#' @param filename Filename (extension optional) to find
#' @param optional Logical: file optional to find?
#' @param quiet Logical - suppress messages?
#' @return Full name of file, or NULL if file not found but is optional.
#' @details Throws an error if file not found (and file is not optional).
#' @importFrom assertthat assert_that
find_csv_file <- function(filename, optional, quiet = FALSE) {
  assert_that(is.character(filename))
  assert_that(assert_that(length(filename) == 1))
  assert_that(is.logical(optional))
  assert_that(is.logical(quiet))

  extensions <- c("", ".csv", ".csv.gz", ".csv.zip")
  for(ex in extensions) {
    fqfn <- system.file("extdata", paste0(filename, ex), package = "gcamdata")
    if(fqfn != "") {
      if(!quiet) cat("Found", fqfn, "\n")
      return(fqfn)  # found it
    }
  }
  if(optional) {
    return(NULL)
  } else {
    stop("Couldn't find required data ", filename)
  }
}


#' Write data produced by chunks to csv files.
#'
#' Write the data produced by the chunks to their output files.  This is mostly
#' a wrapper around \code{write_csv} that figures out file names, processes
#' table flags, writes metadata comments, and so forth.
#'
#' One thing to be aware of is that there is a wart in \code{readr} v 1.1 and
#' later that causes floating point data to be written as integers if they
#' happen to have integer values.  This can cause problems if there are so many
#' apparently-integer values before the first obviously-float value that
#' \code{read_csv} concludes that the column should have integer type.  If this
#' seems to be happening to your table, add the PROTECT_FLOAT flag to it, and
#' any floating point data in your table will be protected before it is
#' written.  Use this option sparingly, as the data written that way tends to be
#' a lot bigger, owing to the large number of digits we have to write.
#'
#' @param chunkdata Named list of tibbles (data frames) to write
#' @param write_inputs Write data that were read as inputs, not computed? Logical
#' @param create_dirs Create directory if necessary, and delete contents? Logical
#' @param outputs_dir Directory to save data into
#' @param xml_dir Directory to save XML results into
#' @importFrom assertthat assert_that
save_chunkdata <- function(chunkdata, write_inputs = FALSE, create_dirs = FALSE,
                           outputs_dir = OUTPUTS_DIR, xml_dir = XML_DIR) {
  assert_that(is_data_list(chunkdata))
  assert_that(is.logical(write_inputs))
  assert_that(is.logical(create_dirs))
  assert_that(is.character(outputs_dir))

  # Create directory if necessary, and remove any previous outputs
  if(create_dirs) {
    dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
    unlink(file.path(outputs_dir, "*.csv"))
    dir.create(xml_dir, showWarnings = FALSE, recursive = TRUE)
    unlink(file.path(xml_dir, "*.xml"))
  }

  for(cn in names(chunkdata)) {
    cd <- get_data(chunkdata, cn)
    if(is.null(cd)) next   # optional file that wasn't found

    if(FLAG_XML %in% get_flags(cd)) {
      # TODO: worry about absolute paths?
      cd$xml_file <- file.path(xml_dir, cd$xml_file)
      run_xml_conversion(cd)
    } else {
      fqfn <- file.path(outputs_dir, paste0(cn, ".csv"))
      suppressWarnings(file.remove(fqfn))

      cmnts <- get_comments(cd)
      flags <- get_flags(cd)

      # If these data have been tagged as input data, don't write
      if(FLAG_NO_OUTPUT %in% flags ||
         FLAG_INPUT_DATA %in% flags && !write_inputs) {
        next
      }

      # If data is in a different from for original data system, indicate
      # that by writing to first line of file
      if(!is.null(flags)) {
        cat(paste(COMMENT_CHAR, paste(flags, collapse = " ")), file = fqfn, sep = "\n")
      }

      if(FLAG_PROTECT_FLOAT %in% flags) {
        cd <- protect_float(cd)
      }

      if(!is.null(cmnts)) {
        cat(paste(COMMENT_CHAR, cmnts), file = fqfn, sep = "\n", append = TRUE)
      }

      readr::write_csv(cd, fqfn, append = TRUE, col_names = TRUE)
    }
  }
}


#' Protect floating point values in a data frame.
#'
#' All of the currently extant functions for writing tables render floating
#' point values using C's %g format.  This format is badly broken, in that a
#' value like 2.0 will be output as "2".  This can cause problems when the table
#' is read in at some later time, as "2" looks like an integer.  This function
#' converts all floating point columns in a data frame to character strings that
#' are properly formatted as floating point literals.  Using
#' \code{\link[readr]{write_csv}} on a data frame protected in this way will
#' produce the expected output.
#'
#' @note The output produced this way will probably be a lot larger than one
#' produced by the default behavior, so this function should be used only where
#' the default behavior is causing problems.
#' @note Because the numeric columns are converted to characters, a data
#' frame converted in this way is no longer useful for computation.
#'
#' @param df Data frame to have floats protected.
#' @return Data frame with floating point columns converted to character.
protect_float <- function(df) {
  floatcols <- names(df)[sapply(df, function(col) {is.numeric(col) &&
      !is.integer(col)})]
  for(col in floatcols) {
    ## Write entries with very large or very small values in scientific
    ## notation.  Other values will be in decimal notation.

    df[[col]] <- if_else(abs(df[[col]]) < 1e-4 | abs(df[[col]]) > 1e6,
                         sprintf("%.10e", df[[col]]),
                         sprintf("%.10f", df[[col]]))
  }
  df
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

  . <- name <- disabled <- x <- NULL    # silence notes on package check.

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
#' @return A tibble with columns 'name' (chunk name), 'input' (name of data),
#' 'file_file' (whether object is read from a file), and 'optional' (whether
#' the object is optional or not).
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
      file_inputs <- FALSE
      optional_file_inputs <- FALSE
    } else {
      file_inputs <- names(reqdata) %in% c("FILE", "OPTIONAL_FILE")
      optional_file_inputs <- names(reqdata) == "OPTIONAL_FILE"
    }
    if(!is.null(reqdata)) {
      chunkinputs[[ch]] <- tibble(name = ch,
                                  input = reqdata,
                                  from_file = file_inputs,
                                  optional = optional_file_inputs)
    }
  }
  dplyr::bind_rows(chunkinputs)
}


#' inputs_of
#'
#' Convenience function for getting the inputs of one or more chunks
#'
#' @param chunks Names of chunks, character
#' @return Character vector of inputs.
#' @export
inputs_of <- function(chunks) {
  if(is.null(chunks) || chunks == "") return(NULL)
  chunk_inputs(chunks)$input
}

#' chunk_outputs
#'
#' List all chunk outputs.
#'
#' @param chunks A character vector of chunks names
#' @return A tibble with columns 'name' (chunk name), 'output' (name of data),
#' and 'to_xml' (whether or not this is an XML structure).
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

#' outputs_of
#'
#' Convenience function for getting the outputs of one or more chunks
#'
#' @param chunks Names of chunks, character
#' @return Character vector of inputs.
#' @export
outputs_of <- function(chunks) {
  if(is.null(chunks) || chunks == "") return(NULL)
  chunk_outputs(chunks)$output
}

#' screen_forbidden
#'
#' Screen a function for use of functions forbidden by data system style guide.
#'
#' Certain functions are forbidden by the dsr style guide from being used in
#' code chunks.  This function tests a function for calls to forbidden functions
#' and flags the offending lines.
#'
#' @param fn The function to be tested.  This is the actual function object, not
#' the name of the function.
#' @return Nx2 Character matrix of flagged lines and the test that tripped them
#' (empty vector, if none)
#' @author RL 19 Apr 2017
#' @importFrom utils capture.output
screen_forbidden <- function(fn) {
  forbidden <- c("(?<!error_no_)match(?!es)", "ifelse",
                 "melt", "cast",
                 "rbind", "cbind", "merge",
                 "read\\.csv", "write\\.csv",
                 "summarise_each", "mutate_each")

  code <- capture.output(fn)
  code <- gsub("#.*$", "", code)      # remove comments
  code <- gsub('"[^"]*"', "", code)   # remove double quoted material
  code <- gsub("'[^']*'", "", code)   # remove single quoted material
  rslt <- character()
  for(f in unique(forbidden)) {
    bad <- grep(f, code, perl = TRUE)
    if(length(bad) > 0) {
      rslt <- rbind(rslt,
                    cbind(f, code[bad]))
    }
  }
  rslt
}


#' normalize_files
#'
#' Normalize line endings for all package input data and compress files.
#'
#' @param root Folder root to scan, character
#' @return Nothing - run for side effects only.
#' @note Set \code{root} to "./extdata" in the git directory, not the package root, to make changes that 'stick'.
#' @export
#' @details Some GCAM input datafiles have bad line endings, and/or
#' don't have a final newline. This utility script converts all files to have Unix line endings (\code{\\n}) and a final newline.
#' @author BBL
normalize_files <- function(root = system.file("extdata", package = "gcamdata")) {
  assert_that(is.character(root))
  message("Root: ", root)

  # Get a list of all input files: CSV files that may or may not be already compressed
  files <- list.files(root, pattern = "\\.csv(\\.gz|\\.zip)?$", full.names = TRUE, recursive = TRUE)

  for(f in seq_along(files)) {
    shortfn <- gsub(root, "", files[f])
    size <- round(file.size(files[f]) / 1024 / 1024, 3)  # MB
    message(f, "/", length(files), ": ", shortfn, ", ", size, " Mb ", appendLF = FALSE)

    # Open the appropriate-type connection, depending on compression
    if(grepl("\\.gz$", files[f])) {
      con <- gzfile(files[f])
      message("(gz)")
    } else if(grepl("\\.zip$", files[f])) {
      con <- unz(files[f], filename = basename(gsub("\\.zip$", "", files[f])))
      message("(zip)")
    } else {
      con <- file(files[f])
      message()
    }

    # Read file and then write it back out
    message("\tReading...", appendLF = FALSE)
    open(con)
    txt <- readLines(con, warn = FALSE)
    close(con)
    uc_size <- format(utils::object.size(txt), units = "Mb")
    message("OK. ", uc_size, " uncompressed")

    message("\tWriting...", appendLF = FALSE)
    ofile <- gsub("(\\.zip|\\.gz)$", "", files[f])
    writeLines(txt, ofile)
    message("OK")

    if(ofile != files[f]) {
      message("\tRemoving original file...", appendLF = FALSE)
      file.remove(files[f])
      message("OK")
    }
  }
}
