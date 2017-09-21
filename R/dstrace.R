
#' A tracing utility for the GCAM Data System.
#'
#' @param object_name Name of object to trace (can be either a data object or a code chunk)
#' @param direction Trace direction (upstream, the default; downstream; or both), character
#' @param graph Plot a directed graph of relationships? Logical
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset
#' @param previous_tracelist Information about previous objects printed
#' @param recurse Recurse to print information about precursor objects? Logical
#' @param ... Extra arguments passed on to \code{\link{dstrace_plot}}
#' @return A tibble with the trace information (object name and trace number)
#' @details What other data products feed into some particular data system object?
#' Conversely, to what other products does some object feed? These are the kinds
#' of questions that \code{dstrace} can help answer.
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
#' @examples
#' dstrace("L100.FAO_ag_Exp_t")
#' dstrace("L100.FAO_ag_Exp_t", downstream = TRUE)
dstrace <- function(object_name, direction = "upstream", graph = FALSE,
                    gcam_data_map = GCAM_DATA_MAP,
                    previous_tracelist = NULL, recurse = TRUE, ...) {

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

  # Pull detailed object information from the internal data structure
  obj_info <- filter(gcam_data_map, output == object_name)

  # Print basic information about the current object
  cat(tracenum, "-", object_name, "- ")
  isfile <- grepl(FLAG_INPUT_DATA, obj_info$flags)
  isxml <- grepl(FLAG_XML, obj_info$flags)
  if(isfile) {
    cat("read from file\n")
  } else {
    cat("produced by", obj_info$name, "\n")
  }
  if(isxml) {
    cat("\tXML data structures to be parsed by GCAM\n")
  } else {
    cat("\t", obj_info$title, " (", obj_info$units, ")\n", sep = "")
    writeLines(paste0("\t", strwrap(obj_info$comments)))
  }

  # Figure out relationships to other objects
  obj_num <- which(previous_tracelist$tracenum == tracenum)
  relationship <- NA_character_
  tn <- max(previous_tracelist$tracenum) + 1
  lo_down <- lo_up <- rel_down <- rel_up <- NULL
  if(downstream) {
    # Get the dependents list
    lo_down <- gcam_data_map %>% filter(grepl(object_name, precursors)) %>% .[["output"]]
    rel_down <- rep("Dependent", length(lo_down))
    if(is.null(lo_down) | length(lo_down) == 0) {
      cat("\tNo dependents\n")
    }
  }
  if(upstream) {
    # Get the precursor list
    lo_up <- unlist(strsplit(obj_info$precursors, split = driver.SEPARATOR, fixed = TRUE))
    rel_up <- rep("Precursor", length(lo_up))
    if(is.null(lo_up) | length(lo_up) == 0) {
      cat("\tNo precursors\n")
    }
  }

  # Insert names of linked (i.e. either immediate precursors, or immediate dependents)
  # objects into tracelist
  linked_objects <- c(lo_down, lo_up)
  previous_tracelist$relatives[obj_num] <- paste(c(lo_down, lo_up), collapse = driver.SEPARATOR)
  relationship <- c(rel_down, rel_up)

  # Print precursors/dependents, checking against previous_tracelist
  new_tracelist <- tibble()
  for(i in seq_along(linked_objects)) {
    obj <- linked_objects[i]
    cat("\t", relationship[i], ": ", obj, " (#", sep = "")
    if(obj %in% previous_tracelist$object_name) {
      # We've already printed info for this precursor/descendent
      cat(previous_tracelist$tracenum[which(previous_tracelist$object_name == obj)], "above")
    } else {
      # Have not (yet) printed info. Add to new_tracelist
      new_tracelist %>%
        bind_rows(tibble(object_name = obj, tracenum = tn, relationship = relationship[i])) ->
        new_tracelist
      cat(tn, "below")
      tn <- tn + 1
    }
    cat(")\n")
  }

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
                                      previous_tracelist = previous_tracelist)
      }
      if(downstream && obj %in% lo_down) {
        previous_tracelist <- dstrace(obj,
                                      direction = "downstream",
                                      graph = FALSE,
                                      gcam_data_map = gcam_data_map,
                                      previous_tracelist = previous_tracelist)
      }
    } # for
  } # if

  if(graph) {
    dstrace_plot(object_name, previous_tracelist, upstream, downstream)
  }

  invisible(previous_tracelist)
}


#' Plot a trace
#'
#' @param object_name Name of original object being traced
#' @param tracelist Record of the trace, a tibble
#' @param upstream Upstream? Logical
#' @param downstream Downstream? Logical
#' @param ... Extra arguments passed on to \code{\link{plot}}
#' @return Adjacency matrix, invisible
dstrace_plot <- function(object_name, tracelist, upstream, downstream, ...) {

  # Make an adjacency matrix, laboriously
  mat <- matrix(0, nrow = nrow(tracelist), ncol = nrow(tracelist))
  colnames(mat) <- paste0(tracelist$tracenum, ". ", tracelist$object_name)
  for(i in seq_len(nrow(tracelist))) {
    for(j in strsplit(tracelist$relatives[i], driver.SEPARATOR, fixed = TRUE)[[1]]) {
      obj_num <- which(tracelist$object_name == j)
      rel <- tracelist$relationship[obj_num]
      if(rel == "Dependent") {
        mat[i, obj_num] <- 1
      } else if(rel == "Precursor") {
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
