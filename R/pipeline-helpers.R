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

#' Change a particular ISO code in a table to another code.
#'
#' A few countries have changed their ISO codes over time.  Usually this is because countries
#' split or merged; occasionally it's because they just didn't like the old one.  This function
#' allows us to insert an ISO code change into a pipeline.
#'
#' If you're dealing with a split or a merger, then making this change will generally create
#' duplicate rows, so you should follow it up with an appropriate aggregation.
#'
#' @param d The data to be transformed
#' @param from The old ISO code
#' @param to The replacement ISO code
#' @param col The name of the columns with the ISO codes (default = 'iso')
#' @return Tibble with the old iso code replaced by the new one.
#' @export
change_iso_code <- function(d, from, to, col='iso') {
  assertthat::assert_that(tibble::is_tibble(d))
  iso <- d[[col]]
  d[[col]] <- if_else(iso == from, to, iso)
  d
}

#' Standardize the ISO column by converting to lower case and renaming to 'iso'
#'
#' Most of the processing functions expect ISO codes to be in lower case and in
#' a column called ISO.
#' @param d The data to be transformed.
#' @param col The column currently containing the iso codes (default = 'iso')
#' @return Tibble with the iso codes converted to lower case. If the column with
#'   the codes was not called 'iso', it will be renamed to 'iso'.
#' @export
standardize_iso <- function(d, col='iso') {
  assertthat::assert_that(tibble::is_tibble(d))
  d[['iso']] <- tolower(d[[col]])
  if(col != 'iso') {
    ## This is surprisingly hard to do using dplyr
    d[[col]] <- NULL
  }
  d
}

#' Protect column names that are bare integers from being interpreted as integers
#'
#' \code{protect_integer_cols} will prepend an 'X' to column names that are bare
#' integers to protect them from being misinterpreted during sensitive
#' operations (see below).  \code{unprotect_integer_cols} will reverse the
#' effect, reverting the column names to their original form.
#'
#' Some of our data frames have column names that are years, such as "2005".  In
#' some cases a column name of this form can be misinterpreted as a column
#' index, the (likely nonexistent) 2005th column in this example.  The *_if
#' functions in dplyr are examples of such circumstances.  Protecting the column
#' names with a leading character allows these functions to perform normally.
#'
#' Much of the data system is expecting years in column names to be bare
#' integers; therefore, you should unprotect the column names as soon as the
#' sensitive operations are complete.
#' @param d The data to have integer column names protected or unprotected
#' @return Tibble with the integer column names protected
#' @export
#' @examples
#' library(magrittr)
#' df <- dplyr::tibble(iso=c('bad','dum'), `2005`=c(123.45, NA), `2050`=c(867, 5309))
#' protect_integer_cols(df) %>%
#'     dplyr::select_if(function(col){!any(is.na(col))}) %>%
#'     unprotect_integer_cols
protect_integer_cols <- function(d) {
  assertthat::assert_that(tibble::is_tibble(d))
  names(d) <- sub('^([0-9]+)$', 'X\\1', names(d))
  d
}

#' @rdname protect_integer_cols
#' @export
unprotect_integer_cols <- function(d) {
  assertthat::assert_that(tibble::is_tibble(d))
  names(d) <- sub('^X([0-9]+)$', '\\1', names(d))
  d
}
