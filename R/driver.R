# Driver

#' Run the entire data system
#'
#' @export
driver <- function() {

  # Get a list of modules in this package
  # These are functions with a name of "module_XXXX"
  modules <- ls(name = parent.env(environment()), pattern = "^module_[a-zA-Z]*$")
  all_data <- list()

  # Get all the data produced by each module
  have_all_data <- FALSE
  while(!have_all_data) {
    have_all_data <- TRUE  # let's hope for the best

    # Loop through all modules and make everyone build their data
    for(m in modules) {
      print(m)

      # Order module `m` to build its data
      cl <- call(m, driver.MAKE, all_data)
      m_data <- eval(cl)

      # Add this module's data to the global data store
      # This will overwrite any previous data returned by `m`
      for(md in names(m_data)) {
        all_data[[md]] <- m_data[[md]]
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
