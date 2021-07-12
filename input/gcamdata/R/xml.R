# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
    add_flags(FLAG_XML) %>%
    invisible()
}

#' set_xml_file_helper
#'
#' @param xml The xml pipeline object
#' @param fq_name The full path to the XML file
#' @return The updated XML object.
set_xml_file_helper <- function(xml, fq_name) {
  xml$xml_file <- fq_name

  invisible(xml)
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

  invisible(dot)
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
      # Note ideally we would use the `pipe` method to run the CSVToXML conversion
      # as this would allow us to avoid writing large CSV files to disk only to
      # convert to XML. However it appears on Windows there is no way to "close"
      # the pipe for STDIN without terminating the entire process.
      # Instead we will fall back to `system2` and since (from the documentation):
      # input    if a character vector is supplied, this is copied one string per
      #          line to a temporary file, and the standard input of command is
      #          redirected to the file.
      # We will just write to a temporary file ourselves and avoid incurring _that_
      # performance penalty as well.
      tmpfn <- tempfile()
      tmp_conn <- file(tmpfn, open = "w")
      for(i in seq_along(dot$data_tables)) {
        table <- dot$data_tables[[i]]
        cat("INPUT_TABLE", file = tmp_conn, sep = "\n")
        cat("Variable ID", file = tmp_conn, sep = "\n")
        cat(table$header, file = tmp_conn, sep = "\n")
        cat("", file = tmp_conn, sep = "\n")
        utils::write.table(table$data, file = tmp_conn, sep = ", ", row.names = FALSE, col.names = TRUE, quote = FALSE)
        cat("", file = tmp_conn, sep = "\n")
      }
      close(tmp_conn)
      args <- c(
        "-cp", shQuote(java_cp),
        "-Xmx2g", # TODO: memory limits?
        "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain",
        tmpfn, # Read from the temporary file
        shQuote(dot$mi_header),
        shQuote(dot$xml_file)
      )
      warning_msgs <- system2("java", args, stdout = TRUE, stderr = TRUE)
      unlink(tmpfn)

      # Note warnings and errors will have been combined together which ideally
      # would be separate so we can forward them to the appropriate message stream
      # in R but for simplicity we will put them all on warning.
      if(!is.null(warning_msgs) && length(warning_msgs) > 0) {
        warning(warning_msgs)
      }
    }

    invisible(dot)
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
#' tables such that \code{paste(base_logit_header, logit.type, sep = "_")} corresponds
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
  assert_that(LOGIT_TYPE_COLNAME %in% names(data))

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
    curr_header <- paste(base_logit_header, curr_logit_type, sep = "_")
    data %>%
      filter(logit.type == curr_logit_type) %>%
      select(-logit.type) %>%
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
  land_name_table <- tibble(from = paste0("LandNode", seq(1,5)),to = "LandNode")

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

#' Add a table to convert to XML but generate additional levels of XML nesting
#'
#' We have the ability to create as many levels of "subsector" nesting as we
#' want but in doing so we want to avoid having to make an explict copy of
#' the headers / LEVEL2_DATA_NAMES. So we provide this method which will instruct
#' the ModelIntercae to generate the addtional levels automatically.  It assumes
#' the data in the base header is provided as is and the additional columns needed
#' for nesting will be moved to the end (see detail for the \code{column_name} and
#' \code{column_order_lookup} params).
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The base header tag to can be looked up in the header file to
#' convert \code{data} besides the additional levels of nesting.
#' @param old_tag The XML tag which is being expanded to add more levels
#' @param new_tag The XML tag which will serve as the additional levels
#' @param num_levels The number of additional levels to generate
#' @param rename_final Whether to rename the final \code{old_tag} to
#' \code{new_tag} in the original header
#' @param column_name The base name of the column in \code{data} which will get
#' expanded by appending \code{paste0(column_name, (seq_len(num_levels) - 1))}
#' @param column_order_lookup A tag that can be used to look up \code{LEVEL2_DATA_NAMES}
#' to reorder the columns of data before XML conversion to ensure they correspond
#' with the ModelInterface header.  Note by default the \code{header} is used then we
#' append the additional columns generated by \code{column_name} + \code{num_levels}. If
#' given \code{NULL} no column reordering will be done.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author Pralit Patel
#' @export
add_xml_data_generate_levels <- function(dot, data, header, old_tag, new_tag, num_levels, rename_final, column_name = old_tag, column_order_lookup = header) {
  # generate the new column names which we will move to the end of data (assuming
  # the user explicitly asked for no column re-ordering)
  new_cols <- paste0(column_name, (seq_len(num_levels) - 1))
  # Users can skip column reordering by passing NULL as the argument value.
  if(!is.null(column_order_lookup)) {
    data <- data[, c(LEVEL2_DATA_NAMES[[column_order_lookup]], new_cols) ]
  }

  # create a node equiv class for the new_tag for the various number of levels
  # to ensure if we have tables that have not renamed back to new_tag they will
  # still get merged into the same tag
  tibble(group.name = new_tag, col = c("tag", paste0("tag", seq(1, num_levels))), tag = c(new_tag, paste0(new_tag, seq(0,num_levels-1)))) %>%
    spread(col, tag) ->
    equiv_table

  #rename_final <- tag <- NULL  # silence package check notes
  tag <- NULL # silence package check notes

  dot <- add_xml_data(dot, equiv_table, "EQUIV_TABLE", NULL)

  # The "command" to add levels is just added to the base level (seperated by ',')
  # and itself is delimited by '/' as 'old_tag/new_tag/num_levels/rename_final'
  add_levels_command <- paste(old_tag, new_tag, num_levels, rename_final, sep="/")
  dot <- add_xml_data(dot, data, paste(header, add_levels_command, sep=","), NULL)

  # due to limitations in the way the CSV to XML processing works the depth is
  # included in the node names
  # thus we have to include a "NodeRename" table fix it after the fact
  new_tag_rename <- tibble(from = paste0(new_tag, seq(0,num_levels+1)),to = new_tag)

  add_xml_data(dot, new_tag_rename, "NodeRename", NULL)
}

#' Add a table to convert to XML but generate additional levels of XML nesting
#' providing a special case for logit tables.
#'
#' This is basically a combination of \code{add_xml_data_generate_levels} and
#' \code{add_logit_tables_xml} check the documentation for each to understand
#' the individual behaviors.
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The base header tag to can be looked up in the header file to
#' convert \code{data} besides the additional levels of nesting.
#' @param old_tag The XML tag which is being expanded to add more levels
#' @param new_tag The XML tag which will serve as the additional levels
#' @param num_levels The number of additional levels to generate
#' @param rename_final Whether to rename the final \code{old_tag} to
#' \code{new_tag} in the original header
#' @param column_name The base name of the column in \code{data} which will get
#' expanded by appending \code{paste0(column_name, (seq_len(num_levels) - 1))}
#' @param column_order_lookup A tag that can be used to look up \code{LEVEL2_DATA_NAMES}
#' to reorder the columns of data before XML conversion to ensure they correspond
#' with the ModelInterface header.  Note by default the \code{header} is used then we
#' append the additional columns generated by \code{column_name} + \code{num_levels}. If
#' given \code{NULL} no column reordering will be done.
#' @param base_logit_header The base header tag to use for the generated logit type
#' tables such that \code{paste(base_logit_header, logit.type, sep = "_")} corresponds
#' to the appropriate model interface header.  Note by default this value is the
#' same as \code{header} as often this is the case but certainly not always.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author Pralit Patel
#' @export
add_logit_tables_xml_generate_levels <- function(dot, data, header, old_tag, new_tag, num_levels,
                                                 rename_final, column_name = old_tag, column_order_lookup = header,
                                                 base_logit_header = header) {
  # pre: the data must contain a column called "logit.type"
  assert_that(LOGIT_TYPE_COLNAME %in% names(data))

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
    curr_header <- paste(base_logit_header, curr_logit_type, sep = "_")
    data %>%
      filter(logit.type == curr_logit_type) %>%
      select(-logit.type) %>%
      # Note we rely on add_xml_data to select the appropriate columns for us
      # which it does by using LEVEL2_DATA_NAMES and curr_header
      add_xml_data_generate_levels(dot, ., curr_header, old_tag, new_tag, num_levels,
                                   rename_final, column_name, curr_header) ->
      dot
  }

  # Note we rely on add_xml_data to select the appropriate columns for us
  # which it does by using LEVEL2_DATA_NAMES and header
  dot %>% add_xml_data_generate_levels(data, header, old_tag, new_tag, num_levels,
                                       rename_final, column_name, column_order_lookup)
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
  "subsector" = c("subsector", "AgSupplySubsector", "tranSubsector",
                  "nesting-subsector"),
  "technology" = c("technology", "stub-technology", "intermittent-technology",
                   "tranTechnology", "AgProductionTechnology", "pass-through-technology",
                   "resource.reserve.technology"),
  "discrete-choice" = c("dummy-logit-tag", "relative-cost-logit",
                        "absolute-cost-logit"),
  "LandLeaf" = c("LandLeaf", "UnmanagedLandLeaf"),
  "carbon-calc" = c("land-carbon-densities", "no-emiss-carbon-calc"),
  "resource" = c("resource", "renewresource", "unlimited-resource"),
  "subresource" = c("subresource", "sub-renewable-resource", "smooth-renewable-subresource",
                    "reserve-subresource")
)
