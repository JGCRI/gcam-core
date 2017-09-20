
#' dstrace
#'
#' @param object_name Name of object to trace (can be either a data object or a code chunk)
#' @param previous_tracelist Information about previous objects printed
#' @param recurse Recurse to print information about precursor objects?
#' @return Boolean (object_name found, or not)
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
dstrace <- function(object_name, all_data, previous_tracelist = NULL, recurse = TRUE) {

  assert_that(is.character(object_name))
  assert_that(is.logical(recurse))

  # 'tracenum' is the number that gets printed next to all entries
  # Allows easy and consistent referencing in what could be a long list
  if(is.null(previous_tracelist)) {
    tracenum <- 1
    previous_tracelist <- tibble(object_name = object_name, tracenum = tracenum)
  } else {
    tracenum <- previous_tracelist$tracenum[which(previous_tracelist$object_name == object_name)]
  }

  stopifnot(object_name %in% GCAM_DATA_MAP$output)

  co <- chunk_outputs()
  chunk_index <- which(co$output == object_name)
  obj <- get_data(all_data, object_name)

  # Print basic information about the current object
  cat(tracenum, "-", object_name, "- ")
  isfile <- FLAG_INPUT_DATA %in% get_flags(obj)
  if(isfile) {
    cat("read from file\n")
  } else {
    cat("produced by", co$name[chunk_index], "\n")
  }
  cat("\t", get_title(obj), " (", get_units(obj), ")\n", sep = "")
  cat(paste0("\t", get_comments(obj)), sep = "\n")

  # Print the precursor list, keeping track of which precursors
  # need to get THEIR information printed
  pcs <- filter(GCAM_DATA_MAP, output == object_name)$precursor
  tn <- max(previous_tracelist$tracenum) + 1

  if(is.null(pcs)) {
    cat("\tNo precursors\n")
  } else {
    # print precursors, checking against previous_tracelist ("see #x above")
    # for any precursors not found and prepare ("see #x below") to recurse
    new_tracelist <- NULL
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
        previous_tracelist <- dstrace(new_tracelist$object_name[i], all_data, previous_tracelist)
      } # for
    } #if
  } # if

  invisible(previous_tracelist)
}
