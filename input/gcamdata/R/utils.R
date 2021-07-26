# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# utils.R


#' find_header
#'
#' Read a file line-by-line to find how far its header extends, and return it.
#'
#' @param fqfn Fully qualified filename, character
#' @return The header as a character vector.
#' @note Headers are defined as consecutive lines beginning with "#" at the top of a file.
#' @author Alexey Shiklomanov
find_header <- function(fqfn) {
  con <- file(fqfn, "r")
  on.exit(close(con))

  is_comment <- TRUE
  header <- character()

  while(is_comment) {
    line <- readLines(con, n = 1)
    is_comment <- grepl("^#", line)
    if (is_comment) {
      header <- c(header, line)
    }
  }
  header
}

#' load_csv_files
#'
#' Load one or more internal, i.e. included with the package, csv (or csv.gz) data files.
#'
#' @param filenames Character vector of filenames to load
#' @param optionals Logical vector, specifying whether corresponding file is optional
#' @param quiet Logical - suppress messages?
#' @param dummy Not used, here as a hack for drake
#' @param ... Any other parameter to pass to \code{readr::read_csv}
#' @details The data frames read in are marked as inputs, not ones that have
#' been computed, via \code{\link{add_comments}}. Optional files that are not found
#' as returned as NA in the list.
#' @return A list of data frames (tibbles).
#' @importFrom magrittr "%>%"
#' @importFrom methods is
#' @importFrom assertthat assert_that
load_csv_files <- function(filenames, optionals, quiet = FALSE, dummy = NULL, ...) {
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

    # Read the file header and extract the column type info from it
    assert_that(file.exists(fqfn))
    header <- find_header(fqfn)
    col_types <- extract_header_info(header, label = "Column types:", fqfn, required = TRUE)

    # Attempt the file read
    # Note `options(warn = 2)` forces all warnings to errors...
    op <- options(warn = 2)
    fd <- try(readr::read_csv(fqfn, comment = COMMENT_CHAR, col_types = col_types, ...))
    options(op)
    # ...ensuring we trap any anomaly here
    if(is(fd, "try-error")) {
      stop("Error or warning while reading ", basename(fqfn))
    }

    # Parse the file's header and add metadata
    fd %>%
      parse_csv_header(fqfn, header) %>%
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

    if(any(grepl(",,+$", info))) {
      warning("Multiple commas at end of header line in ", filename)
    }
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
#' @param header A vector of strings comprising the file header
#' @param enforce_requirements Enforce mandatory fields?
#' @details Headers are given at the top of files and consist of labels ("Title:", "Units:", etc)
#' prefixed by comment characters (#). The parser looks for these, and calls \code{\link{add_title}} and
#' similar functions to return an empty data frame with appropriate attribute set.
#' @return An empty \code{\link{tibble}} with appropriate attributes filled in.
#' @export
parse_csv_header <- function(obj, filename, header, enforce_requirements = TRUE) {
  assert_that(tibble::is_tibble(obj))
  assert_that(is.character(filename))
  assert_that(is.character(header))
  assert_that(is.logical(enforce_requirements))

  # Excel tries to be 'helpful' and, when working with CSV files, quotes lines with
  # commas in them...which you CAN'T SEE when re-opening in Excel. Trap this problem.
  if(any(grepl(paste0('^"', COMMENT_CHAR), header))) {
    stop('A quoted comment (# prefixed by a double quote, probably due to Excel) detected in ', basename(filename))
  }

  # The 'File:' field has to match the actual filename
  filecheck <- extract_header_info(header, "File:", filename, required = enforce_requirements)
  # Remove trailing commas - stupid Excel
  filecheck <- gsub(",*$", "", filecheck)
  if(enforce_requirements & !identical(filecheck, basename(filename))) {
    stop("'File:' given in header (", filecheck, ") doesn't match filename in ", filename)
  }

  obj %>%
    add_title(extract_header_info(header, "Title:", filename, required = enforce_requirements)) %>%
    add_units(extract_header_info(header, "Units?:", filename, required = enforce_requirements)) %>%
    add_comments(extract_header_info(header, "(Comments|Description):", filename, multiline = TRUE)) %>%
    add_reference(extract_header_info(header, "(References?|Sources?):", filename, multiline = TRUE))
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

  extensions <- c(".csv", ".csv.gz", "")
  for(ex in extensions) {
    fqfn <- system.file("extdata", paste0(filename, ex), package = "gcamdata")
    if(fqfn != "") {
      # check if a relative path to the file is shorter than the absolute in
      # hopes of being able to stay within windows file path length limits
      fn_rel <- get_relative_to_workdir(fqfn)
      if(nchar(fn_rel) < nchar(fqfn)) {
        fqfn <- fn_rel
      }
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

#' Normalize file path to the working directory
#'
#' On Windows we may run into the MAX_PATH file path limit when referencing
#' files using the absolute path.  This method will normalize the file path
#' to the working directory which _may_ be shorter.
#'
#' @param fqfn The full path of the file to normalize.
#' @return A relative path to \code{fqfn} from the current working directory
#' given by \code{getwd()}.
get_relative_to_workdir <- function(fqfn) {
  # split the paths using the path separator '/' which R uses consistently
  # across platforms
  fqfn_split <- strsplit(fqfn, '/')[[1]]
  wd_split <- c(strsplit(getwd(), '/')[[1]])

  # figure out where the file paths diverge
  wd_match <- wd_split == fqfn_split[seq_along(wd_split)]
  diverge_index <- which(!wd_match, arr.ind = TRUE)
  if(length(diverge_index) == 0) {
    # case where the entire working dir path matches
    diverge_index <- length(wd_split) + 1
  } else {
    # only part of the path matches
    # we need to make sure we get first / earliest divergence
    diverge_index <- diverge_index[1]
  }

  # to make the path relative we need to:
  # 1) add as many .. as the number of dirs that diverged in the working directory
  # 2) remove all dirs from fqfn prior to diverge_index
  # 3) paste it all together with the path separator '/'
  fqfn_rel <- paste0(c(rep('..', times = length(wd_split) - diverge_index + 1),
                       fqfn_split[diverge_index:length(fqfn_split)]), collapse = '/')

  fqfn_rel
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
#' @param write_outputs Write all chunk outputs to disk?
#' @param write_xml Write XML Batch chunk outputs to disk?
#' @param outputs_dir Directory to save data into
#' @param xml_dir Directory to save XML results into
#' @importFrom assertthat assert_that
save_chunkdata <- function(chunkdata, write_inputs = FALSE, create_dirs = FALSE,
                           write_outputs = TRUE, write_xml = write_outputs,
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
      if(write_xml) {
        # TODO: worry about absolute paths?
        cd$xml_file <- file.path(xml_dir, cd$xml_file)
        run_xml_conversion(cd)
      }
    } else if(write_outputs) {
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

  . <- name <- disabled <- x <- NULL    # silence notes on package check.

  assertthat::assert_that(is.character(pattern))

  ls(name = parent.env(environment()), pattern = pattern) %>%
    tibble::tibble(name = .,
                   disabled = grepl("_DISABLED$", name) | grepl(paste0("^module_.*", DISABLED_MODULES), name)) %>%
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
  chunk_names <- character()
  inputs <- character()
  from_files <- logical()
  optionals <- logical()
  for(ch in chunks) {
    cl <- call(ch, driver.DECLARE_INPUTS)
    reqdata <- eval(cl)

    # Chunks mark their file inputs specially, using vector names
    if(is.null(names(reqdata))) {
      file_inputs <- rep(FALSE, times = length(reqdata))
      optional_file_inputs <- rep(FALSE, times = length(reqdata))
    } else {
      file_inputs <- names(reqdata) %in% c("FILE", "OPTIONAL_FILE")
      optional_file_inputs <- names(reqdata) == "OPTIONAL_FILE"
    }
    if(!is.null(reqdata)) {
      chunk_names <- c(chunk_names, rep(ch, times = length(reqdata)))
      inputs <- c(inputs, as.vector(unlist(reqdata)))
      from_files <- c(from_files, file_inputs)
      optionals <- c(optionals, optional_file_inputs)
    }
  }
  tibble(name = chunk_names, input = inputs, from_file = from_files, optional = optionals)
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

  chunk_names <- character()
  outputs <- character()
  to_xmls <- logical()
  for(ch in chunks) {
    cl <- call(ch, driver.DECLARE_OUTPUTS)
    reqdata <- eval(cl)

    # Chunks mark any XML file outputs using vector names
    if(is.null(names(reqdata))) {
      fileoutputs <- rep(FALSE, times = length(reqdata))
    } else {
      fileoutputs <- names(reqdata) == "XML"
    }
    if(!is.null(reqdata)) {
      chunk_names <- c(chunk_names, rep(ch, times = length(reqdata)))
      outputs <- c(outputs, as.vector(unlist(reqdata)))
      to_xmls <- c(to_xmls, fileoutputs)
    }
  }
  tibble(name = chunk_names, output = outputs, to_xml = to_xmls)
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
#' @param fn The function to be tested. This is the actual function object, not
#' the name of the function.
#' @return Nx2 Character matrix of flagged lines and the test that tripped them
#' (empty vector, if none)
#' @author RL 19 Apr 2017
#' @importFrom utils capture.output
screen_forbidden <- function(fn) {
  forbidden <- c("(?<!error_no_)match(?!es)", "ifelse",
                 "melt", "cast",
                 "rbind(?!list)", "cbind", "merge",
                 "read\\.csv", "write\\.csv",
                 "summarise_each", "mutate_each")

  # If this is a level 1 chunk we add a couple constants
  level1 <- regexpr("_L1[0-9]+", deparse(substitute(fn))) > 0
  if(level1) {
    forbidden <- c(forbidden, "MODEL_BASE_YEARS", "MODEL_FUTURE_YEARS")
  }

  code <- capture.output(fn)
  code <- gsub("#.*$", "", code)      # remove comments
  code <- gsub('"[^"]*"', "", code)   # remove double quoted material
  code <- gsub("'[^']*'", "", code)   # remove single quoted material

  # For some reason the R package check process seems to concatenate certain lines;
  # in particular a mutate() followed by a replace_na() ends up on a single line, which
  # can cause false positives below if it's then followed by another mutate(). This
  # does not occur during 'normal' testthat testing.
  # Anyway, ensure all %>% operations are on separate lines
  code <- unlist(vapply(code, strsplit, split = "%>%", fixed = TRUE, FUN.VALUE = list(1)))

  # Special multiline case: consecutive mutate calls
  rslt <- character()
  mutates <- grep("^\\s*mutate\\(", code)
  diff1s <- base::diff(mutates) == 1
  if(any(diff1s)) {
    rslt <- cbind("consecutive mutate calls", code[mutates[which(diff1s)]])
  }

  # General screen-forbidden search, single lines only
  for(f in unique(forbidden)) {
    bad <- grep(f, code, perl = TRUE)
    if(length(bad) > 0) {
      rslt <- rbind(rslt,
                    cbind(f, code[bad]))
    }
  }
  rslt
}

# some utils specific for dealing with drake

#' load_from_cache
#'
#' Load the given chunk inputs / outputs from the drake cache.  The
#' names given can come from \code{inputs_of} or \code{outputs_of} for
#' instance.
#'
#' @param return_data_names The names of the data to return.
#' @param ... Parameters to pass directly to drake::readd to allow
#' users to specify a non-default cache for instance.
#' @return The data loaded from cache returned as a list in the
#' same format as all_data.
#' @export
load_from_cache <- function(return_data_names, ...) {
  # gracefully handle an empty request
  if(length(return_data_names) == 0) {
    return(c())
  }

  # convert to drake target names
  sanitized_names <- make.names(return_data_names)

  # we need to filter the ... arguments to only those that drake::readd takes
  all_ellipsis_args <- list(...)
  readd_args <- all_ellipsis_args[names(all_ellipsis_args) %in% names(formals(drake::readd))]
  # by default readd is interpreting the target as symbol so we must force
  # character_only = TRUE
  readd_args[["character_only"]] <- TRUE

  # readd can only one target at a time so we need to wrap it in an apply
  ret_data <- sapply(sanitized_names, function(target) {
    readd_args[["target"]] <- target
    do.call(drake::readd, readd_args)
  })
  # reset names to the gcamdata names as users would otherwise expect
  names(ret_data) <- return_data_names

  invisible(ret_data)
}

#' create_datamap_from_cache
#'
#' Re-creates the GCAM data map by pulling the data out of cache and processing
#' its metadata.  We need to know the plan to be able to identify which data to
#' to pull and the chunk names that generated them.
#'
#' @param gcamdata_plan The drake plan that generated the outputs
#' @param ... Parameters to pass directly to drake::readd to allow
#' users to specify a non-default cache for instance.
#' @return The full GCAM data map
#' @importFrom magrittr "%>%"
#' @importFrom dplyr pull mutate if_else select group_by bind_rows
create_datamap_from_cache <- function(gcamdata_plan, ...) {
  target <- command <- chunk <- data <- name <- output <- NULL    # silence notes on package check.

  # a helper function to use in an lapply to combine the steps:
  # 1) decay a tibble of targets to a character vector
  # 2) load data from cache
  # 3) run the data through tibbelize_outputs
  load_and_tibbilize_helper <- function(target_df, chunk, ...) {
    tibbelize_outputs(load_from_cache(pull(target_df, target), ...), chunk)
  }

  gcamdata_plan %>%
    # only need targets that load inputs or are outputs of a chunk
    # we can find those by knowing:
    # INPUT have commands that start with load_csv_files
    # chunks outputs have commands that look like chunk['chunk_output_name']
    mutate(chunk = if_else(grepl("load_csv_files", command), "INPUT", sub('\\[.*', '', command))) %>%
    filter(chunk == "INPUT" | grepl("]$", command)) %>%
    select(-command) %>%
    # we would like to "nest" targets by chunk so we can call tibbelize_outputs
    # for each chunk and a vector of "targets" which are related
    tidyr::nest(target) %>%
    # also group by to ensure we call load_and_tibbilize_helper once per chunk
    group_by(chunk) %>%
    mutate(data = lapply(data, load_and_tibbilize_helper, chunk, ...)) %>%
    # pull all of the metadata into a single tibble
    pull(data) %>%
    bind_rows() %>%
    # convert drake names back to data system names
    mutate(output = if_else(name == "INPUT", gsub('\\.', '/', output), output))
}
