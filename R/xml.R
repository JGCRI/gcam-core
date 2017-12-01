# xml.R

#' The basis to start a pipeline to convert data to an XML file.
#'
#' This method simply requires the name to save the XML file as and optionally the
#' model interface "header" file that defines the transformation lookup
#' to go from tabular data to hierarchical.  The result of this function should be
#' used in a dplyr pipeline with one or more calls to \code{\link{add_xml_data}}
#' to add the data to convert and finally ending with \code{\link{run_xml_conversion}}
#' to run the conversion.
#' Please see the wiki for more details \url{https://github.com/JGCRI/gcamdata/wiki/XML-Conversion}
#'
#' @param xml_file The file name to save the XML file to
#' @param mi_header The model interface "header" file name.  This will default to
#' the one included in this package
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @export
create_xml <- function(xml_file, mi_header = NULL) {
  if(is.null(mi_header)) {
    mi_header <- system.file("extdata/mi_headers", "ModelInterface_headers.txt",
                             package = "gcamdata")
  }

  list(xml_file = xml_file,
       mi_header = mi_header,
       data_tables = list()) %>%
    add_flags(FLAG_XML)
}

#' Add a table to an XML pipeline to include for conversion to XML.
#'
#' We need the tibble to convert and a header tag which can be looked up in
#' the header file to convert the tibble.  This method is meant to be included
#' in a pipeline between calls of \code{\link{create_xml}} and \code{\link{run_xml_conversion}}.
#' Please see the wiki for more details \url{https://github.com/JGCRI/gcamdata/wiki/XML-Conversion}
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The header tag to can be looked up in the header file to
#' convert \code{data}
#' @param column_order_lookup A tag that can be used to look up \code{LEVEL2_DATA_NAMES}
#' to reorder the columns of data before XML conversion to ensure they correspond
#' with the ModelInterface header.  Note by default the \code{header} is used and if
#' given \code{NULL} no column reordering will be done.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author PP March 2017
#' @export
add_xml_data <- function(dot, data, header, column_order_lookup = header) {
  # Users can skip column reordering by passing NULL as the argument value.
  if(!is.null(column_order_lookup)) {
    data <- data[, LEVEL2_DATA_NAMES[[column_order_lookup]] ]
  }

  curr_table <- list(data = data, header = header)
  dot$data_tables[[length(dot$data_tables)+1]] <- curr_table

  dot
}

# Note: we have put the definition of run_xml_conversion inside of the "closure"
# make_run_xml_conversion so that we may stash the XML_WARNING_GIVEN flag in an
# environment that can only be reached by run_xml_conversion.  The XML_WARNING_GIVEN
# flag allows us to issue the warning that we are skipping the XML conversion since
# java is disabled only one time instead of spewing it a hundred times.
make_run_xml_conversion <- function() {
  XML_WARNING_GIVEN <- FALSE
  function(dot) {
    use_java <- getOption("gcamdata.use_java")
    if(!isTRUE(use_java) && !XML_WARNING_GIVEN) {
      message("Skipping XML conversion as global option gcamdata.use_java is not TRUE")
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

    dot
  }
}

#' Run the CSV to XML conversion using the model interface tool.
#'
#' This method should be the final call in a pipeline started with \code{\link{create_xml}}
#' and one or more calls to \code{\link{add_xml_data}}.
#' Please see the wiki for more details \url{https://github.com/JGCRI/gcamdata/wiki/XML-Conversion}
#'
#' Note that this method relies on Java to run the conversion.  To avoid errors for
#' users who do not have Java installed it will check the global option
#' \code{gcamdata.use_java} before attempting to run the conversion.  If the flag
#' is set to \code{FALSE} a warning will be issued and the conversion skipped.
#' To enable the use of Java a user can set \code{options(gcamdata.use_java=TRUE)}
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @return The argument passed in unmodified in case a user wanted run the
#' conversion again at a later time.
#' @author PP March 2017
#' @export
run_xml_conversion <- make_run_xml_conversion()

#' Add a table which has definitions with logit.type to be converted to XML
#'
#' Such tables need to get partitioned into a series of tables due to limitations
#' in the way that the Model Interfaces processes information.  In particular all
#' XML tag names have to be specified in the header and can not be set dynamically
#' as is the intention with the logit.type.
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The header tag to can be looked up in the header file to
#' convert \code{data}
#' @param base_logit_header The base header tag to use for the generated logit type
#' tables such that \code{paste(base_logit_header, logit.type, sep="_")} corresponds
#' to the appropriate model interface header.  Note by default this value is the
#' same as \code{header} as often this is the case but certainly not always.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @note For documentation of the options for logit choice functions in GCAM, see
#' \url{http://jgcri.github.io/gcam-doc/choice.html}.
#' @author Pralit Patel
#' @export
add_logit_tables_xml <- function(dot, data, header, base_logit_header=header) {
  # pre: the data must contain a column called "logit.type"
  assert_that("logit.type" %in% names(data))

  # We need to split data into at least three tables based upon the logit.type
  # column:
  # 1) The logit group from XML_NODE_EQUIV so that we only need one table to
  #    read logit parameters into each of the possible gcam.LOGIT_TYPES.
  # 2) A series of one or more tables for each gcam.LOGIT_TYPES that doesn't
  #    actually read in any data but sets the proper XML name.
  # 3) Finally data with the logit.type column dropped.

  logit.type <- . <- NULL  # silence package check notes

  dot <- add_node_equiv_xml(dot, "discrete-choice")

  # The default logit type is used if the value of a logit.type is NA
  default_logit_type <- gcam.LOGIT_TYPES[1]

  # Set the logit type to the default if currently unspecified
  data$logit.type[is.na(data$logit.type)] <- default_logit_type

  # Loop through each of the logit types and create a table for it
  # using the appropriate header name.
  for(curr_logit_type in gcam.LOGIT_TYPES) {
    curr_header <- paste(base_logit_header, curr_logit_type, sep="_")
    data %>%
      filter(logit.type == curr_logit_type) %>%
      # Note we rely on add_xml_data to select the appropriate columns for us
      # which it does by using LEVEL2_DATA_NAMES and curr_header
      add_xml_data(dot, ., curr_header) ->
      dot
  }

  # Note we rely on add_xml_data to select the appropriate columns for us
  # which it does by using LEVEL2_DATA_NAMES and header
  dot %>% add_xml_data(data, header)
}

#' Add a table to an XML pipeline that instructs the ModelInterface to rename
#' LandNodeX to LandNode.
#'
#' Such a table is necessary to help work around limitations in the XML processing
#' that node names of the same name can not be nested with in each other:
#' LandNode/LandNode thus instead we say LandNode1/LandNode2 and rename as the last step.
#' Therefore in most cases a user should add this table near the end of the XML pipeline.
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author Pralit Patel
#' @export
add_rename_landnode_xml <- function(dot) {
  land_name_table <- tibble(from=paste0("LandNode", seq(1,5)),to="LandNode")

  add_xml_data(dot, land_name_table, "NodeRename", NULL)
}

#' add_node_equiv_xml
#'
#' Add a table to an XML pipeline that instructs the ModelInterface to treat
#' tags in the same class as the same when enabled, thus not requiring multiple
#' headers for instance to read in a share-weight into a technology
#' intermittent-technology or tranTechnology.  A user taking advantage of this
#' feature would then read this table early in the XML pipeline.
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param equiv_class A name of the equivalence class to add.  This could be any
#' key in the list \code{XML_NODE_EQUIV}.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author Pralit Patel
#' @export
add_node_equiv_xml <- function(dot, equiv_class) {
  tag <- NULL   # silence package check notes
  equiv_list <- XML_NODE_EQUIV[[equiv_class]]
  if(is.null(equiv_list)) {
    stop("Could not find ", equiv_class, " in XML_NODE_EQUIV")
  }

  tibble(group.name = equiv_class, col = paste0("tag", seq(1, length(equiv_list))), tag = equiv_list) %>%
    spread(col, tag) ->
    equiv_table

  add_xml_data(dot, equiv_table, "EQUIV_TABLE", NULL)
}

#' Run the XML diff utility on two XML files
#'
#' Comparing two XML files turns out to be more easily done in python, so this
#' function runs the python comparison code.  The standard mode checks the
#' return code and returns TRUE if the files are equivalent, or FALSE if they
#' are not.  If the files are not equivalent, the differences found will *not*
#' be written to the console; you have to use raw mode if you want to get that
#' information.
#'
#' The 'raw' mode returns the stdout generated by the comparison code as a
#' string. If the files are equivalent this return value will be empty (i.e., a
#' zero-length character vector, not an empty string).  If not, then it will
#' contain the diagnostic output. In either case the string returned will have a
#' 'status' attribute giving the return code.
#' @param fleft The first file to compare
#' @param fright The second file to compare
#' @param raw Flag: if true run in raw mode
#' @export
cmp_xml_files <- function(fleft, fright, raw = FALSE)
{
  cmd <- system2('which', 'python', stdout=TRUE)
  py <- system.file('exec/xml_verify.py', package = 'gcamdata')
  ## normalizePath prints a warning when one of the files doesn't exist, but we'll
  ## catch that as an error below.
  suppressWarnings({args <- normalizePath(c(py, fleft, fright))})
  if(raw) {
    rslt <- system2(cmd, args, stdout = TRUE, stderr=FALSE) # stderr output is
                                        # not needed.
    if(is.null(attr(rslt, 'status'))) {
      ## For some reason, system2 doesn't set the status attribute isn't set when the call
      ## is successful
      attr(rslt, 'status') <- 0
    }
    return(rslt)
  }
  else {
    for(file in args)
      if(!file.exists(file))
        stop("Can't find file: ", file)
    rslt <- system2(cmd, args, stdout = FALSE, stderr = FALSE)
    if(rslt == 0) {
      return(TRUE)
    }
    else if(rslt == 3) {
      return(FALSE)
    }
    else {
      ## stderr from the python code will be printed to console.
      stop("Comparison command failed")
    }
  }
}


#' Compare all XML files in a directory to their counterparts in the output
#'
#' The 'old' directory should contain reference versions of the files.  The
#' 'new' directory should be the directory where the output xml files are
#' stored.  The old directory is searched recursively; the new directory is not
#' (because the new system currently dumps all of its outputs into a single
#' output directory).
#'
#' @param olddir Directory containing the old (reference) output xml files
#' @param newdir Directory containing the new output xml files
#' @return Number of discrepancies found.
#' @export
run_xml_tests <- function(olddir, newdir = XML_DIR)
{
    oldfiles <- list.files(olddir, '\\.xml$', recursive=TRUE, full.names=TRUE)
    newfiles <- file.path(newdir, sapply(oldfiles, basename))

    if (length(oldfiles) == 0) {
        warning('No XML files found in ', olddir)
        return(0)
    }

    isgood <- Map(cmp_xml_files, oldfiles, newfiles) %>% simplify2array
    if (!all(isgood)) {
        badfiles <- paste(sapply(newfiles[!isgood], basename), collapse = ' ')
        warning('The following files had discrepancies: ', badfiles)
    }

    sum(!isgood)
}

#' A list of XML tag equivalence classes so that the ModelInterface when converting
#' data to XML can treat tags in the same class as the same when enabled, thus not
#' requiring multiple headers for instance to read in a share-weight into a technology
#' intermittent-technology or tranTechnology.
#' @author Pralit Patel
XML_NODE_EQUIV <- list(
  "sector" = c("supplysector", "AgSupplySector", "pass-through-sector"),
  "subsector" = c("subsector", "AgSupplySubsector", "tranSubsector"),
  "technology" = c("technology", "stub-technology", "intermittent-technology",
                   "tranTechnology", "AgProductionTechnology", "pass-through-technology"),
  "discrete-choice" = c("dummy-logit-tag", "relative-cost-logit",
                        "absolute-cost-logit"),
  "LandLeaf" = c("LandLeaf", "UnmanagedLandLeaf"),
  "carbon-calc" = c("land-carbon-densities", "no-emiss-carbon-calc")
)
