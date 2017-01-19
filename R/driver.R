# driver.R


#' driver
#'
#' Run the entire data system.
#'
#' @param write_outputs Write all chunk outputs to disk?
#' @param all_data Data to be pre-loaded into data system
#' @return A list of all built data.
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
driver <- function(write_outputs = TRUE, all_data = empty_data()) {
  assert_that(is.logical(write_outputs))

  chunklist <- find_chunks()
  cat("Found", nrow(chunklist), "chunks\n")
  #cat(chunklist$chunk, "\n")

  chunkinputs <- chunk_inputs(chunklist$name)
  cat("Found", nrow(chunkinputs), "chunk data requirements\n")
  chunkoutputs <- chunk_outputs(chunklist$name)
  cat("Found", nrow(chunkoutputs), "chunk data products\n")

  # Outputs should all be unique
  dupes <- duplicated(chunkoutputs$output)
  if(any(dupes)) {
    stop("Outputs appear multiple times: ", chunkoutputs$output[dupes])
  }

  # If there are any unaccounted for input requirements,
  # try to load them from csv files
  unfound_inputs <- setdiff(chunkinputs$input, chunkoutputs$output)
  if(length(unfound_inputs)) {

    # These should all be marked as 'from_file'
    ff <- chunkinputs$from_file[chunkinputs$input %in% unfound_inputs]
    if(any(!ff)) {
      stop("Unfound inputs not marked as from file: ", unfound_inputs[!ff])
    }

    cat(length(unfound_inputs), "chunk data input(s) not accounted for\n")
    load_csv_files(unfound_inputs, quiet = TRUE) %>%
      add_data(all_data) ->
      all_data
  }

  chunks_to_run <- chunklist$name
  while(length(chunks_to_run)) {
    nchunks <- length(chunks_to_run)

    # Loop through all chunks and see who can run (i.e. all dependencies are available)
    for(chunk in chunks_to_run) {
      print(chunk)

      input_names <- dplyr::filter(chunkinputs, name == chunk)$input
      if(!all(input_names %in% names(all_data))) {
        print("- data not available yet")
        next  # chunk's inputs are not all available
      }

      # Order chunk to build its data
      time1 <- Sys.time()
      cl <- call(chunk, driver.MAKE, all_data[input_namesd])
      chunk_data <- eval(cl)
      tdiff <- as.numeric(difftime(Sys.time(), time1, units = "secs"))

      print(paste("- make", format(round(tdiff, 2), nsmall = 2)))
      assert_that(is.list(chunk_data))
      assert_that(tibble::is.tibble(chunk_data[[1]]))

      # Chunk should return EXACTLY what it promised
      promised <- subset(chunkoutputs, name == chunk)$output
      if(!identical(sort(names(chunk_data)), sort(promised))) {
        stop("Chunk ", chunk, "is not returning what it promised!")
      }

      # Add this chunk's data to the global data store
      # This will overwrite any previous data returned
      all_data <- add_data(chunk_data, all_data)

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
