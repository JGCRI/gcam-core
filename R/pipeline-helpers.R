# pipeline-helpers.R

# Pipeline shortcuts we use a lot

# Make sure year and value are numeric, and within historical years
PH_year_value_historical <- function(d) {
  d %>%
    mutate(year = as.numeric(year),
           value = as.numeric(value)) %>%
    filter(year %in% HISTORICAL_YEARS)
}


#' left_join_error_no_match
#'
#' A restrictive version of \code{\link{left_join}}.
#'
#' @param d Data frame (typically from pipeline)
#' @param ... Rest of call to \code{\link{left_join}}
#' @return Joined data.
#' @details Restrictive version of dplyr::left_join meant for replacing `match` calls.
# Ensures that number of rows of data doesn't change, and everything has matched data.
#' @export
left_join_error_no_match <- function(d, ...) {
  dnames <- names(d)
  drows <- nrow(d)
  d <- left_join(d, ...)
  if(nrow(d) != drows) {
    stop("left_join_no_match: number of rows in data changed")
  }
  if(any(is.na(d[setdiff(names(d), dnames)]))) {
    stop("left_join_no_match: NA values in new data columns")
  }
  d
}


#' approx_fun
#'
#' \code{\link{approx}} (interpolation) for use in a dplyr pipeline.
#'
#' @param year Numeric year, in a melted tibble or data frame
#' @param value Numeric value to interpolate
#' @param rule Rule to use; see \code{\link{approx}} and details
#' @details This was \code{gcam_interp} in the original data system.
#' @return Interpolated values.
#' @export
#' @examples
#' df <- data.frame(year = 1:5, value = c(1, 2, NA, 4, 5))
#' approx_fun(df$year, df$value, rule = 2)
approx_fun <- function(year, value, rule = 1) {
  assertthat::assert_that(is.numeric(year))
  assertthat::assert_that(is.numeric(value))

  if(rule == 1 | rule == 2 ) {
    stats::approx(as.vector(year), value, rule = rule, xout = year)$y
  } else {
    stop("Not implemented yet!")
  }
}
