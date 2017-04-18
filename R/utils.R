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
      filedata[[f]] <- NA
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

  # Create directory if necessary, and remove any previous outputs
  dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
  unlink(file.path(outputs_dir, "*.csv"))

  for(cn in names(chunkdata)) {
    cd <- chunkdata[[cn]]
    if(!isTRUE(identical(NA, cd))) {   # NA means an optional file that wasn't found

      fqfn <- file.path(outputs_dir, paste0(cn, ".csv"))
      suppressWarnings(file.remove(fqfn))

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

#' create_xml
#'
#' The basis to define how to convert data to an XML file.  This method
#' simple requires the name to save the XML file as and optionally the
#' model interface "header" file that defines the transformation lookup
#' to go from tabular data to hierarchical.  The result of this should be
#' used in a dplyr pipeline with one or more calls to \code{\link{add_xml_data}}
#' to add the data to convert and finally ending with \code{\link{run_xml_conversion}}
#' to run the conversion.
#'
#' @param xml_file The name to save the XML file to
#' @param mi_header The model interface "header".  This will default to the one
#' included in this package
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @export
create_xml <- function(xml_file, mi_header = NULL) {
  if(is.null(mi_header)) {
    mi_header <- system.file("extdata/mi_headers", "ModelInterface_headers.txt",
                             package = "gcamdata")
  }

  list(xml_file = xml_file, mi_header = mi_header, data_tables = list())
}

#' add_xml_data
#'
#' Add a table to include for conversion to XML.  We need the tibble to convert
#' and a header tag which can be looked up in the header file to convert the
#' tibble.  This method is meant to be included in a pipeline between calls of
#' \code{\link{create_xml}} and \code{\link{run_xml_conversion}}.
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The header tag to can be looked up in the header file to
#' convert \code{data}
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author PP March 2017
#' @export
add_xml_data <- function(dot, data, header) {
  curr_table <- list(data = data, header = header)
  dot$data_tables[[length(dot$data_tables)+1]] <- curr_table

  dot
}

# Note: we have put the definition of run_xml_conversion inside of the "closure"
# make_run_xml_conversion so that we may stash the XML_WARNING_GIVEN flag in an
# environment that can only be reached by run_xml_conversion.
make_run_xml_conversion <- function() {
  XML_WARNING_GIVEN <- FALSE
  function(dot) {
    use_java <- getOption("gcamdata.use_java")
    if(!isTRUE(use_java) && !XML_WARNING_GIVEN) {
      warning("Skipping conversion as global option gcamdata.use_java is not TRUE")
      # set the flag to avoid repeating the warning.
      XML_WARNING_GIVEN <<- TRUE
    } else if(isTRUE(use_java)) {
      java_cp <- system.file("extdata/ModelInterface", "CSVToXML.jar",
                             package = "gcamdata")
      cmd <- c(
        "java",
        "-cp", java_cp,
        "-Xmx1g", # TODO: memory limits?
        "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain",
        "-", # Read from STDIN
        dot$mi_header,
        dot$xml_file
      )
      conv_pipe <- pipe(paste(cmd, collapse=" "), open = "w")
      on.exit(close(conv_pipe))

      for(i in seq_along(dot$data_tables)) {
        table <- dot$data_tables[[i]]
        cat("INPUT_TABLE", file = conv_pipe, sep = "\n")
        cat("Variable ID", file = conv_pipe, sep = "\n")
        cat(table$header, file = conv_pipe, sep = "\n")
        cat("", file = conv_pipe, sep = "\n")
        utils::write.table(table$data, file = conv_pipe, sep=",", row.names = FALSE, col.names = TRUE, quote = FALSE)
        cat("", file = conv_pipe, sep = "\n")
      }
    }
  }
}

#' run_xml_conversion
#'
#' Run the CSV to XML conversion using the model interface tool.  This method
#' should be the final call in a pipeline started with \code{\link{create_xml}}
#' and one or more calls to \code{\link{add_xml_data}}.
#'
#' Not that this method relies on Java to run the conversion.  To avoid errors for
#' users who do not have Java installed it will check the global option
#' \code{gcamdata.use_java} before attempting to run the conversion.  If the flag
#' is set to \code{FALSE} a warning will be issued and the conversion skipped.
#' To enable the use of Java a user can set \code{options(gcamdata.use_java=TRUE)}
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @author PP March 2017
#' @export
run_xml_conversion <- make_run_xml_conversion()
