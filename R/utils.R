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
      add_title(f) %>%
      add_comments(paste("Read from", gsub("^.*extdata", "extdata", fqfn))) %>%
      add_flags(FLAG_INPUT_DATA) ->
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
#' @param xml_file The name to save the XML file to.
#' @param mi_header The model interface "header".  This will default to the one
#' included in this package.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @export
create_xml <- function(xml_file, mi_header=NULL) {
  if(is.null(mi_header)) {
    mi_header <- system.file("extdata/mi_headers", "ModelInterface_headers.txt",
                             package="gcamdata")
  }

  list(xml_file=xml_file, mi_header=mi_header, data_tables=list())
}

#' add_xml_data
#'
#' Add a table to include for conversion to XML.  We need the tibble to convert
#' and a header tag which can be looked up in the header file to convert the
#' tibble.  This method is meant to be included in a pipeline between calls of
#' \code{\link{create_xml}} and \code{\link{run_xml_conversion}}.
#'
#' @param dot The current state of the pipeline started from \code{create_xml}.
#' @param data The tibble of data to add to the conversion.
#' @param header The header tag to can be looked up in the header file to
#' convert \code{data}.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @export
add_xml_data <- function(dot, data, header) {
  curr_table <- list(data=data, header=header)
  dot$data_tables[[length(dot$data_tables)+1]] <- curr_table

  dot
}

#' run_xml_conversion
#'
#' Run the CSV to XML conversion using the model interface tool.  This method
#' is should be the final call in a pipeline started with \code{\link{create_xml}}
#' and one or more calls to \code{\link{add_xml_data}}.
#'
#' @param dot The current state of the pipeline started from \code{create_xml}.
#' @export
run_xml_conversion <- function(dot) {
  java_cp <- system.file("extdata/ModelInterface", "CSVToXML.jar",
                         package="gcamdata")
  cmd <- c(
    "java",
    "-cp", java_cp,
    "-Xmx2g", # TODO: memory limits?
    "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain",
    "-", # Read from STDIN
    dot$mi_header,
    dot$xml_file
  )
  conv_pipe <- pipe(paste(cmd, collapse=" "), open="w")

  tryCatch({
    for(i in seq_along(dot$data_tables)) {
      table <- dot$data_tables[[i]]
      cat("INPUT_TABLE", file=conv_pipe, sep="\n")
      cat("Variable ID", file=conv_pipe, sep="\n")
      cat(table$header, file=conv_pipe, sep="\n")
      cat("", file=conv_pipe, sep="\n")
      write.table( table$data, file=conv_pipe, sep=",", row.names=F, col.names=T, quote=F )
      cat("", file=conv_pipe, sep="\n")
    }
  }, finally={
    close(conv_pipe)
  })
}
