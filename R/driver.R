
#' driver
#'
#' Run the entire data system
#'
#' @return a list of all built data
#' @export
#' @importFrom magrittr "%>%"
driver <- function() {

  # Get a list of chunks in this package
  # These are functions with a name of "module_{modulename}_{chunkname}"
  ls(name = parent.env(environment()), pattern = "^module_[a-zA-Z]*_.*$") %>%
    tibble(name = .) %>%
    tidyr::separate(name, into = c("x", "module", "chunk"), remove = FALSE, sep = "_") %>%
    dplyr::select(-x) %>%
    dplyr::mutate(done = FALSE) ->
    chunklist

  cat("Found", nrow(chunklist), "chunks\n")
  print(chunklist)

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

  # Get all the data produced by each module
  all_data <- list()
  while(!all(chunklist$done)) {
    have_all_data <- TRUE  # let's hope for the best

    # Loop through all modules and make everyone build their data
    for(i in seq_len(nrow(chunklist))) {
      chunk <- chunklist$name[i]
      print(chunk)

      if(chunklist$done[i]) {
        print("- already done, skip")
        next  # chunk has already run
      }

      if(!all(dplyr::filter(chunkinputs, name == chunk)$input %in% names(all_data))) {
        print("- not available, skip")
        next  # chunk's inputs are not available yet
      }

      # Order chunk to build its data
      print("- make")
      cl <- call(chunk, driver.MAKE, all_data)
      chunk_data <- eval(cl)

      # Add this module's data to the global data store
      # This will overwrite any previous data returned
      for(cd in names(chunk_data)) {
        all_data[[cd]] <- chunk_data[[cd]]
      }

      chunklist$done[i] <- TRUE
    } # for
  } # while

  all_data
}
