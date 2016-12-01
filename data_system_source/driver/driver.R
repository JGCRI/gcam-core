# Driver

#' Run the entire data system
#'
#' @export
#' @importFrom tibble tibble
driver <- function() {

  # Get a list of chunks in this package
  # These are functions with a name of "module_{modulename}_{chunkname}"
  ls(name = parent.env(environment()), pattern = "^module_[a-zA-Z]*_.*$") %>%
    tibble(name = .) %>%
    separate(name, into = c("x", "module", "chunk"), remove = FALSE, sep = "_") %>%
    dplyr::select(-x) ->
    chunklist

  cat("Found", nrow(chunklist), "chunks\n")

  # Get list of data required by each chunk
  chunkinputs <- list()
  for(i in seq_len(nrow(chunklist))) {
    cl <- call(chunklist$name, driver.DECLARE_INPUTS, all_data)
    reqdata <- eval(cl)
    if(!is.null(reqdata)) {
      chunkinputs[[i]] <- tibble(chunk = chunklist$name[i], input = reqdata)
    }
  }
  chunkinputs <- dplyr::bind_rows(chunkinputs)

  return(chunkinputs)

  # Get all the data produced by each module
  all_data <- list()
  have_all_data <- FALSE
  while(!have_all_data) {
    have_all_data <- TRUE  # let's hope for the best

    # Loop through all modules and make everyone build their data
    for(chunk in chunklist$name) {
      print(m)

      # Check to see whether all of chunk's data requirements are available

      # Order chunk to build its data
      cl <- call(chunk, driver.MAKE, all_data)
      chunk_data <- eval(cl)

      # Add this module's data to the global data store
      # This will overwrite any previous data returned by `m`
      for(cd in names(chunk_data)) {
        all_data[[cd]] <- chunk_data[[cd]]
      }

      # Check that `m` didn't return any NULL data
      data_ok <- !unlist(lapply(m_data, is.null))
      if(!all(data_ok)) {
        print(paste("- NULL", names(m_data)[!data_ok]))
        have_all_data <- FALSE
      }

      # Check that all data declared by `m` are actually present,
      # and all data returned were declared
      m_declared <- eval(call(m, driver.DECLARE))
      names_ok <- identical(sort(m_declared), sort(names(m_data)))
      if(!names_ok) {
        print(paste("- Missing", setdiff(m_declared, names(m_data))))
        print(paste("- Extra", setdiff(names(m_data), m_declared)))
        have_all_data <- FALSE
      }

      if(have_all_data) {
        print("- OK")
      }
    } # for
  } # while !have_all_data
}
