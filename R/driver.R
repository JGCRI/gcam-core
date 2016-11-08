# Driver

#' Run the entire data system
#'
#' @export
driver <- function() {

  # Get a list of modules in this package
  # These are functions with a name of "module_XXXX"
  modules <- ls(name = parent.env(environment()), pattern = "^module_[a-zA-Z]*$")

  # Get all the data produced by each module
  have_all_data <- FALSE
  while(!have_all_data) {
    have_all_data <- TRUE
    for(m in modules) {
      print(m)
      cl <- call(m, driver.MAKE)
      m_data <- eval(cl)

      # Check that there's no NULL data
      data_ok <- !unlist(lapply(x, is.null))
      if(!all(data_ok)) {
        print(paste("- NULL", names(m_data)[!data_ok]))
        have_all_data <- FALSE
      }

      # Check that all data declared are present
      m_declared <- eval(call(m, driver.DECLARE))
      names_ok <- identical(sort(m_declared), sort(names(m_data)))
      if(!names_ok) {
        print(paste("- Missing", setdiff(m_declared, names(m_data))))
        have_all_data <- FALSE
      }

      if(have_all_data) {
        print("- OK")
      }
    }
  }
}
