# driver.R


#' driver
#'
#' Run the entire data system
#'
#' @param write_outputs Write all chunk outputs to disk?
#' @return a list of all built data
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
driver <- function(write_outputs = TRUE) {

  chunklist <- find_chunks()
  cat("Found", nrow(chunklist), "chunks\n")

  chunkinputs <- chunk_dependencies(chunklist$name)
  cat("Found", nrow(chunkinputs), "chunk data requirements\n")

  # all_data holds all the data produced by chunks
  all_data <- list()
  chunks_to_run <- chunklist$name
  while(length(chunks_to_run)) {
    nchunks <- length(chunks_to_run)

    # Loop through all chunks and see who can run (i.e. all dependencies are available)
    for(chunk in chunks_to_run) {
      print(chunk)

      if(!all(dplyr::filter(chunkinputs, name == chunk)$input %in% names(all_data))) {
        print("- data not available yet")
        next  # chunk's inputs are not available yet
      }

      # Order chunk to build its data
      print("- make")
      cl <- call(chunk, driver.MAKE, all_data)
      chunk_data <- eval(cl)
      assert_that(is.list(chunk_data))

      # Add this chunk's data to the global data store
      # This will overwrite any previous data returned
      for(cd in names(chunk_data)) {
        all_data[[cd]] <- chunk_data[[cd]]
      }

      # Remove the current chunk from the to-run list
      chunks_to_run <- chunks_to_run[chunks_to_run != chunk]
    } # for

    # We have to be able to run >=1 chunk every loop iteration
    if(length(chunks_to_run) == nchunks) {
      stop("No chunks were run--we are stuck")
    }
  } # while

  cat(length(all_data), "data frames generated\n")

  if(write_outputs) {
    cat("Writing chunk data...\n")
    save_chunkdata(all_data)
  }

  cat("All done.\n")
  invisible(all_data)
}
