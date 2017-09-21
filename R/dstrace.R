
#' A tracing utility for the GCAM Data System.
#'
#' @param object_name Name of object to trace (can be either a data object or a code chunk)
#' @param downstream Trace upstream instead of down? Logical
#' @param graph Plot a directed graph of relationships? Logical
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset
#' @param previous_tracelist Information about previous objects printed
#' @param recurse Recurse to print information about precursor objects? Logical
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
dstrace <- function(object_name, downstream = FALSE, graph = FALSE,
                    gcam_data_map = GCAM_DATA_MAP,
                    previous_tracelist = NULL, recurse = TRUE) {

  assert_that(is.character(object_name))
  assert_that(is.logical(downstream))
  assert_that(is.logical(graph))
  assert_that(is_tibble(gcam_data_map))
  assert_that(is.logical(recurse))

  # 'tracenum' is the number that gets printed next to all entries
  # Allows easy and consistent referencing in what could be a long list
  if(is.null(previous_tracelist)) {
    tracenum <- 1
    previous_tracelist <- tibble(object_name = object_name,
                                 tracenum = tracenum,
                                 related = NA_character_)
  } else {
    tracenum <- previous_tracelist$tracenum[which(previous_tracelist$object_name == object_name)]
  }

  if(!object_name %in% gcam_data_map$output) {
    stop("Unknown object name '", object_name, "'")
  }

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
  relationship <- NA_character_
  tn <- max(previous_tracelist$tracenum) + 1
  linked_objects <- NULL
  if(downstream) {
    # Get the dependents list
    relationship <- "Dependent"
    gcam_data_map %>% filter(grepl(object_name, precursors)) %>% .[["output"]] -> linked_objects
    tn <- max(previous_tracelist$tracenum) + 1
  } else {
    # Get the precursor list
    relationship <- "Precursor"
    linked_objects <- unlist(strsplit(obj_info$precursors, split = driver.SEPARATOR, fixed = TRUE))
  }

  if(is.null(linked_objects) | length(linked_objects) == 0) {
    cat("\tNo ", tolower(relationship), "s\n", sep = "")
  } else {
    # insert linked objects into tracelist
    previous_tracelist$related[which(previous_tracelist$tracenum == tracenum)] <- paste(linked_objects, collapse = driver.SEPARATOR)
    # print precursors/dependents, checking against previous_tracelist ("see #x above")
    new_tracelist <- tibble()
    for(pc in linked_objects) {
      cat("\t", relationship, ": ", pc, " (#", sep = "")
      if(pc %in% previous_tracelist$object_name) {
        # We've already printed info for this precursor/descendent
        cat(previous_tracelist$tracenum[which(previous_tracelist$object_name == pc)], "above")
      } else {
        # Have not (yet) printed info. Add to new_tracelist
        new_tracelist %>%
          bind_rows(tibble(object_name = pc, tracenum = tn)) ->
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
        previous_tracelist <- dstrace(new_tracelist$object_name[i],
                                      downstream = downstream,
                                      graph = FALSE,
                                      gcam_data_map = gcam_data_map,
                                      previous_tracelist = previous_tracelist)
      } # for
    } # if
  } # if

  if(graph) {
    dstrace_plot(object_name, previous_tracelist, downstream = downstream)
  }

  invisible(previous_tracelist)
}


#' Plot a trace
#'
#' @param object_name Name of original object being traced
#' @param tracelist Record of the trace, a tibble
#' @param downstream Downstream? Logical
#' @return Adjacency matrix, invisible
dstrace_plot <- function(object_name, tracelist, downstream) {

  # Make an adjacency matrix, laboriously
  mat <- matrix(0, nrow = nrow(tracelist), ncol = nrow(tracelist))
  colnames(mat) <- paste0(tracelist$tracenum, ". ", tracelist$object_name)
  for(i in seq_len(nrow(tracelist))) {
    if(!is.na(tracelist$related[i])) {
      for(j in strsplit(tracelist$related[i], driver.SEPARATOR, fixed = TRUE)[[1]]) {
        #        cat(tracelist$object_name[i], "->", j, "\n")
        if(downstream) {
          mat[i, which(tracelist$object_name == j)] <- 1
        } else {
          mat[which(tracelist$object_name == j), i] <- 1

        }
      }
    }
  }

  # Graph it
  g <- igraph::graph.adjacency(mat)
  coords <- igraph::layout_nicely(g)

  direction <- if_else(downstream, "Downstream", "Upstream")
  vc <- rainbow(2)[1 + as.numeric(tracelist$object_name == object_name)]
  lc <- c("darkgrey", "black")[1 + as.numeric(tracelist$object_name == object_name)]

  plot(g,
       vertex.color = vc,
       vertex.label.dist = 3,
       vertex.label.cex = 1.0,
       vertex.label.color = lc,
       vertex.size = 10,
       edge.arrow.size = 0.5,
       layout = coords)
  title(paste(direction, object_name))

  invisible(g)
}
