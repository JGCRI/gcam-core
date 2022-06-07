# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.


#' A tracing utility for the GCAM Data System.
#'
#' @param object_name Name of object to trace (can be either a data object or a code chunk)
#' @param direction Trace direction ("upstream", the default; "downstream"; or "both"), character
#' @param graph Plot a directed graph of relationships? Logical
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset
#' @param previous_tracelist Information about previous objects printed
#' @param recurse Recurse to print information about precursor objects? Logical
#' @param print Print trace to console? Logical
#' @param ... Extra arguments passed on to \code{\link{dstrace_plot}}
#' @return A tibble with the trace information (object name and trace number), invisibly
#' @details What other data products feed into some particular data system object?
#' Conversely, to what other products does some object feed? These are the kinds
#' of questions that \code{dstrace} can help answer.
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
#' @examples
#' dstrace("L100.FAO_ag_Exp_t")
#' dstrace("L100.FAO_ag_Exp_t", direction = "downstream")
#' dstrace("L100.FAO_ag_Exp_t", direction = "both", graph = TRUE)
dstrace <- function(object_name, direction = "upstream", graph = FALSE,
                    gcam_data_map = NULL,
                    previous_tracelist = NULL, recurse = TRUE, print = TRUE, ...) {

  if(is.null(gcam_data_map)) {
    gcam_data_map <- GCAM_DATA_MAP
  }
  assert_that(is.character(object_name))
  assert_that(is.character(direction))
  assert_that(length(direction) == 1)
  assert_that(is.logical(graph))
  assert_that(is_tibble(gcam_data_map))
  assert_that(is.logical(recurse))

  if(!object_name %in% gcam_data_map$output) {
    stop("Unknown object name '", object_name, "'")
  }

  # Initialization
  upstream <- grepl("^(up|both)", direction)
  downstream <- grepl("^(down|both)", direction)
  output <- precursors <- . <- NULL  # silence package check notes

  # If this is the first time (no recursion yet), create a new tracelist
  if(is.null(previous_tracelist)) {
    # 'tracenum' is the number that gets printed next to all entries
    # Allows easy and consistent referencing in what could be a long list
    tracenum <- 1
    previous_tracelist <- tibble(object_name = object_name,
                                 tracenum = tracenum,
                                 relationship = "Self",
                                 relatives = NA_character_)
  } else {
    # Not our first rodeo; look up tracenum
    tracenum <- previous_tracelist$tracenum[which(previous_tracelist$object_name == object_name)]
  }

  # Print object information and get linked objects and new tracelist
  objectdata <- info(object_name, gcam_data_map = gcam_data_map, previous_tracelist = previous_tracelist,
                     upstream = upstream, downstream = downstream, print = print)
  linked_objects <- objectdata$linked_objects
  lo_up <- linked_objects[names(linked_objects) == data.PRECURSOR]
  lo_down <- linked_objects[names(linked_objects) == data.DEPENDENT]
  relationship <- names(linked_objects)
  new_tracelist <- objectdata$new_tracelist

  # Insert names of linked (i.e. either immediate precursors, or immediate dependents)
  # objects into tracelist
  obj_num <- which(previous_tracelist$tracenum == tracenum)
  previous_tracelist$relatives[obj_num] <- paste(linked_objects, collapse = data.SEPARATOR)

  # Recurse as necessary
  if(recurse) {
    previous_tracelist <- bind_rows(previous_tracelist, new_tracelist)
    for(i in seq_len(nrow(new_tracelist))) {
      obj <- new_tracelist$object_name[i]
      if(upstream && obj %in% lo_up) {
        previous_tracelist <- dstrace(obj,
                                      direction = "upstream",
                                      graph = FALSE,
                                      gcam_data_map = gcam_data_map,
                                      previous_tracelist = previous_tracelist,
                                      print = print)
      }
      if(downstream && obj %in% lo_down) {
        previous_tracelist <- dstrace(obj,
                                      direction = "downstream",
                                      graph = FALSE,
                                      gcam_data_map = gcam_data_map,
                                      previous_tracelist = previous_tracelist,
                                      print = print)
      }
    } # for
  } # if

  if(graph) {
    dstrace_plot(object_name, previous_tracelist, upstream, downstream)
  }

  invisible(previous_tracelist)
}

#' A simplified dstrace which simply finds the chunks which are precursors
#' recursively.
#'
#' The algorithm is performing a depth first search where chunks are "nodes"
#' and data objects are "verticies" connecting them.
#' @param chunk_names A list of chunks to find the precursors for.
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset.
#' @param prev_trace_list The list of chunks already found.
#' @return The unique list of chunk names which are precursors for \code{chunk_names}.
#' @export
#' @importFrom magrittr %$%
dstrace_chunks <- function(chunk_names, gcam_data_map, prev_trace_list = c()) {
  trace_list = prev_trace_list

  name.x <- name.y <- NULL  # silence package check note

  for(chunk_name in chunk_names) {
    if(!(chunk_name %in% trace_list)) {
      trace_list <- c(trace_list, chunk_name)
      precursor_chunks <- gcam_data_map %>% filter(name.y == chunk_name) %$% name.x
      trace_list <- dstrace_chunks(precursor_chunks,
                                   gcam_data_map,
                                   trace_list)
    }
  }

  trace_list
}


#' Plot a trace
#'
#' @param object_name Name of original object being traced
#' @param tracelist Record of the trace, a tibble
#' @param upstream Looking upstream? Logical
#' @param downstream Looking downstream? Logical
#' @param ... Extra arguments passed on to \code{\link{plot}}
#' @return Adjacency matrix, invisible
dstrace_plot <- function(object_name, tracelist, upstream, downstream, ...) {

  # Make an adjacency matrix, laboriously
  mat <- matrix(0, nrow = nrow(tracelist), ncol = nrow(tracelist))
  colnames(mat) <- paste0(tracelist$tracenum, ". ", tracelist$object_name)
  for(i in seq_len(nrow(tracelist))) {
    for(j in strsplit(tracelist$relatives[i], data.SEPARATOR, fixed = TRUE)[[1]]) {
      obj_num <- which(tracelist$object_name == j)
      rel <- tracelist$relationship[obj_num]
      if(rel == data.DEPENDENT) {
        mat[i, obj_num] <- 1
      } else if(rel == data.PRECURSOR) {
        mat[obj_num, i] <- 1
      } else {
        stop("??? ", rel)
      }
    }
  }

  # Graph it
  g <- igraph::graph.adjacency(mat)
  coords <- igraph::layout_with_drl(g)

  direction <- if_else(!xor(downstream, upstream), "", if_else(downstream, "Downstream", "Upstream"))

  vc <- rainbow(2)[1 + as.numeric(tracelist$object_name == object_name)]
  lc <- c("darkgrey", "black")[1 + as.numeric(tracelist$object_name == object_name)]

  plot(g, vertex.color = vc,
       vertex.label.dist = 3,
       vertex.label.cex = 1.0,
       vertex.label.color = lc,
       vertex.size = 10,
       edge.arrow.size = 0.5,
       layout = coords, ...)
  title(paste(direction, object_name))

  invisible(mat)
}



#' info
#'
#' Print and return information about an object.
#'
#' @param object_name Name of object to get information about (can be either a data object or a code chunk)
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset
#' @param previous_tracelist Information about previous objects printed (if called from \code{\link{dstrace}})
#' @param upstream Print and return upstream precursors? Logical
#' @param downstream Print and return downstream dependents? Logical
#' @param print Print to console? Logical
#' @return If called directly, returns an entry from \code{GCAM_DATA_MAP}; if called from \code{\link{dstrace}}, a two-
#' element list with linked object and tracelist information. If called and \code{object_name} is the name of a
#' code chunk (as opposed to that of a data object), the relevant help page will be pulled up.
#' @export
#' @examples
#' info("L100.FAO_ag_Exp_t")
#' info("module_aglu_L222.land_input_2")
info <- function(object_name, gcam_data_map = NULL, previous_tracelist = NULL, upstream = TRUE, downstream = TRUE, print = TRUE) {

  if(is.null(gcam_data_map)) {
    gcam_data_map <- GCAM_DATA_MAP
  }
  assert_that(is.character(object_name))
  assert_that(is_tibble(gcam_data_map))
  assert_that(is.null(previous_tracelist) || is_tibble(previous_tracelist))
  assert_that(is.logical(upstream))
  assert_that(is.logical(downstream))

  output <- precursors <- . <- NULL  # silence package check notes

  # If it's a chunk, just pull up relevant help page
  if(object_name %in% find_chunks()$name) {
    return(utils::help(object_name))
  } else if(!object_name %in% gcam_data_map$output) {
    stop("Unknown object name '", object_name, "'")
  }

  # Pull detailed object information from the internal data structure
  obj_info <- filter(gcam_data_map, output == object_name)
  new_tracelist <- NULL

  IN_DSTRACE <- !is.null(previous_tracelist)
  if(IN_DSTRACE) {  # some special initialization and printing to do if we're in the middle of a trace
    tracenum <- previous_tracelist$tracenum[which(previous_tracelist$object_name == object_name)]
    if(print) cat(tracenum, "- ")
    tn <- max(previous_tracelist$tracenum) + 1
    new_tracelist <- tibble()
  }

  # Print basic information about the current object
  if(print) cat(object_name, "- ")
  isfile <- grepl(FLAG_INPUT_DATA, obj_info$flags)
  isxml <- grepl(FLAG_XML, obj_info$flags)
  if(isfile) {
    if(print) cat("read from file\n")
  } else {
    if(print) cat("produced by", obj_info$name, "\n")
  }
  if(isxml) {
    if(print) cat("\tXML data structures to be parsed by GCAM\n")
  } else {
    if(print)  cat("\t", obj_info$title, " (", obj_info$units, ")\n", sep = "")
    if(print) writeLines(paste0("\t", strwrap(obj_info$comments)))
  }

  # Figure out relationships to other objects
  lo_down <- lo_up <- NULL
  if(downstream) {
    # Get the dependents list
    lo_down <- gcam_data_map %>% filter(grepl(object_name, precursors)) %>% pull(output)
    names(lo_down) <- rep("Dependent", length(lo_down))
    if(is.null(lo_down) | length(lo_down) == 0) {
      if(print) cat("\tNo dependents\n")
    }
  }
  if(upstream) {
    # Get the precursor list
    lo_up <- unlist(strsplit(obj_info$precursors, split = data.SEPARATOR, fixed = TRUE))
    names(lo_up) <- rep("Precursor", length(lo_up))
    if(is.null(lo_up) | length(lo_up) == 0) {
      if(print) cat("\tNo precursors\n")
    }
  }
  linked_objects <- c(lo_down, lo_up)

  # Print precursors/dependents, checking against previous_tracelist if IN_DSTRACE
  for(i in seq_along(linked_objects)) {
    obj <- linked_objects[i]
    if(print) cat("\t", names(linked_objects)[i], ": ", obj, sep = "")

    if(IN_DSTRACE) {
      if(print) cat(" (#")
      if(obj %in% previous_tracelist$object_name) {
        # We've already printed info for this precursor/descendent
        if(print) cat(previous_tracelist$tracenum[which(previous_tracelist$object_name == obj)], "above)")
      } else {
        # Have not (yet) printed info. Add to new_tracelist
        new_tracelist %>%
          bind_rows(tibble(object_name = obj, tracenum = tn, relationship = names(linked_objects)[i])) ->
          new_tracelist
        if(print) cat(tn, "below)")
        tn <- tn + 1
      }
    }
    if(print) cat("\n")
  }

  # Return tracing information (if called from dstrace) or just basic from GCAM_DATA_MAP
  if(IN_DSTRACE) {
    invisible(list(linked_objects = linked_objects, new_tracelist = new_tracelist))  # dstrace needs this info
  } else {
    invisible(obj_info)
  }
}
