
#' module_atest_xxx
#'
#' A test module that can't make data until modeltime data is available
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}
#' @author BBL
#' @export
module_atest_xxx <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return("L200.ModelTime")
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("atestoutput")
  } else if(command == driver.MAKE) {
    atest_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' atest_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all available test data
#' @importFrom magrittr "%>%"
atest_makedata <- function(all_data) {

  get_data(all_data, "L200.ModelTime")

  tibble::tibble(x = 1) %>%
    add_dsflags(FLAG_INPUT_DATA) ->
    atestoutput

  return_data(atestoutput)
}
