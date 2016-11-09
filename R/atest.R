
#' module_atest
#'
#' A test module that can't make data until modeltime data is available
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}
#' @author BBL
#' @export
#'
#' @examples
#' module_test("MAKE")
module_atest <- function(command, ...) {
  if(command == driver.DECLARE) {
    atest_declaredata()
  } else if(command == driver.MAKE) {
    atest_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' atest_declaredata
#'
#' Declare what data are produced by \code{atest}
#'
#' @return Names of all test data
#'
atest_declaredata <- function() {
  c("atestoutput")
}


#' atest_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all available test data
#'
atest_makedata <- function(all_data) {

  modeltime_data_we_need <- all_data[["L200.ModelTime"]]

  if(is.null(modeltime_data_we_need)) {
    atestoutput <- NULL
  } else {
    atestoutput <- 1
  }

  list("atestoutput" = atestoutput)
}
