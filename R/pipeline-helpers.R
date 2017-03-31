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
  assertthat::assert_that(tibble::is.tibble(d))
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
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' df <- data.frame(year = 1:5, value = c(1, 2, NA, 4, 5))
#' approx_fun(df$year, df$value, rule = 2)
approx_fun <- function(year, value, rule = 1) {
  assert_that(is.numeric(year))
  assert_that(is.numeric(value))

  if(rule == 1 | rule == 2 ) {
    stats::approx(as.vector(year), value, rule = rule, xout = year)$y
  } else {
    stop("Not implemented yet!")
  }
}



#' repeat_add_columns
#'
#' Repeat a data frame for each entry in a second, binding the columns together.
#'
#' @param x Data frame (tibble) to repeat
#' @param y A copy of \code{x} is created for each row of this tibble
#' @return A repeated \code{x} with columns from \code{y} added.
#' @details This corresponds to \code{repeat_and_add_vector} in the old data system.
#' @importFrom assertthat assert_that
#' @author BBL
#' @export
#' @examples
#' x <- tibble::tibble(x = 1:3)
#' y <- tibble::tibble(y = c(4, 5), z = c(6, 7))
#' repeat_add_columns(x, y)
repeat_add_columns <- function(x, y) {
  assert_that(tibble::is_tibble(x))
  assert_that(tibble::is_tibble(y))

  x %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD)
}
