
#' driver
#'
#' Run the entire data system
#'
#' @return a list of all built data
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
driver <- function() {

  # Get a list of chunks in this package
  # These are functions with a name of "module_{modulename}_{chunkname}"
  ls(name = parent.env(environment()), pattern = "^module_[a-zA-Z]*_.*$") %>%
    tibble(name = .) %>%
    tidyr::separate(name, into = c("x", "module", "chunk"), remove = FALSE,
                    sep = "_", extra = "merge") %>%
    dplyr::select(-x) ->
    chunklist

  cat("Found", nrow(chunklist), "chunks\n")

  # Get list of data required by each chunk
  chunkinputs <- list()
  for(i in seq_len(nrow(chunklist))) {
    cl <- call(chunklist$name[i], driver.DECLARE_INPUTS)
    reqdata <- eval(cl)
    if(!is.null(reqdata)) {
      chunkinputs[[i]] <- tibble(name = chunklist$name[i], input = reqdata)
    }
  }
  chunkinputs <- dplyr::bind_rows(chunkinputs)

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

    # We have to be able to run at >=1 chunk every loop iteration
    if(length(chunks_to_run) == nchunks) {
      stop("No chunks were run--we are stuck")
    }
  } # while

  cat(length(all_data), "data frames generated\n")
  cat("All done.\n")
  invisible(all_data)
}
