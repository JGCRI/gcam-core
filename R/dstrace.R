
#' dstrace
#'
#' @param object_name Name of object to trace (can be either a data object or a code chunk)
#' @param gcam_data_map A tibble of metadata information; normally a built-in package dataset
#' @param previous_tracelist Information about previous objects printed
#' @param recurse Recurse to print information about precursor objects?
#' @return A tibble with the trace information (object name and trace number)
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
#' @examples
#' dstrace("L100.FAO_ag_Exp_t")
dstrace <- function(object_name, gcam_data_map = GCAM_DATA_MAP, previous_tracelist = NULL, recurse = TRUE) {

  assert_that(is.character(object_name))
  assert_that(is_tibble(gcam_data_map))
  assert_that(is.logical(recurse))

  # 'tracenum' is the number that gets printed next to all entries
  # Allows easy and consistent referencing in what could be a long list
  if(is.null(previous_tracelist)) {
    tracenum <- 1
    previous_tracelist <- tibble(object_name = object_name, tracenum = tracenum)
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
  if(isfile) {
    cat("read from file\n")
  } else {
    cat("produced by", obj_info$name, "\n")
  }
  cat("\t", obj_info$title, " (", obj_info$units, ")\n", sep = "")
  writeLines(paste0("\t", strwrap(obj_info$comments)))

  # Print the precursor list, keeping track of which precursors
  # need to get THEIR information printed
  pcs <- unlist(strsplit(obj_info$precursors, split = driver.SEPARATOR, fixed = TRUE))
  tn <- max(previous_tracelist$tracenum) + 1

  if(is.null(pcs) | length(pcs) == 0) {
    cat("\tNo precursors\n")
  } else {
    # print precursors, checking against previous_tracelist ("see #x above")
    # for any precursors not found and prepare ("see #x below") to recurse
    new_tracelist <- tibble()
    for(pc in pcs) {
      cat("\t", "Precursor: ", pc, " (#", sep = "")
      if(pc %in% previous_tracelist$object_name) {
        # We've already printed info for this precursor
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
        previous_tracelist <- dstrace(new_tracelist$object_name[i], gcam_data_map = gcam_data_map, previous_tracelist)
      } # for
    } #if
  } # if

  invisible(previous_tracelist)
}
