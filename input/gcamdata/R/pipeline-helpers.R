# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# pipeline-helpers.R

# Pipeline shortcuts we use a lot

# Make sure year and value are numeric, and within historical years
PH_year_value_historical <- function(d) {
  year <- value <- NULL                 # silence notes in package check
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
#' @param ignore_columns Optional column name(s) to ignore, character vector
#' @return Joined data.
#' @details Restrictive version of dplyr::left_join meant for replacing `match` calls.
# Ensures that number of rows of data doesn't change, and everything has matched data.
#' @importFrom dplyr left_join
#' @export
left_join_error_no_match <- function(d, ..., ignore_columns = NULL) {
  assertthat::assert_that(tibble::is_tibble(d))
  dnames <- names(d)
  drows <- nrow(d)
  d <- left_join(d, ...)
  if(nrow(d) != drows) {
    stop("left_join_no_match: number of rows in data changed")
  }
  names_to_check <- dplyr::setdiff(names(d), dnames) %>% dplyr::setdiff(ignore_columns)
  if(any(is.na(d[names_to_check]))) {
    stop("left_join_no_match: NA values in new data columns")
  }
  d
}


#' Compute a left join, taking only the first match.
#'
#' In an ordinary \code{\link{left_join}}, if a row in the left operand has
#' multiple matches in the right operand, you get a copy of the row for each
#' match in the right operand.  Sometimes you want just one arbitrary member of
#' the matching set.  This could be because the right operand is a one-to-many
#' mapping, and you don't care which one you get (but you want only one), or it
#' could be that you're trying to reproduce the behavior of legacy code that
#' uses \code{\link{match}}, which has this behavior.  This function performs
#' such a join.
#'
#' This function performs a left join, except that if the right operand has
#' multiple matches for a row in the left operand, \emph{only} the first match
#' is kept.  \strong{Use this function with caution.}  The results will depend
#' on the order of the rows in the right operand, meaning that seemingly
#' innocuous changes can produce changes in output.  Consider yourself warned.
#'
#' @param x Left table to join
#' @param y Right table to join
#' @param by Vector of id columns.  Unlike in other join variants, these must be
#' supplied explicitly.
#' @return Joined table.  In case of multiple matches, only the first will be
#' included.
#' @importFrom dplyr ungroup left_join
left_join_keep_first_only <- function(x, y, by) {
  ## Our strategy is to use summarize/first on y for each non-match category,
  ## then join that to x.
  . <- NULL                           # silence notes on package check
  ll <- by
  names(ll) <- NULL
  y %>%
    dplyr::group_by_at(tidyselect::all_of(ll)) %>%
    dplyr::summarize_at(dplyr::vars(-tidyselect::any_of(ll)), first) %>%
    ungroup() %>%
    left_join(x, ., by=by)
}


#' Fast left join for large tables
#'
#' The dplyr join functions are a little on the slow side for very large
#' tables.  This version converts its inputs
#' \code{\link[data.table]{data.table}} structures, and uses that package's
#' faster indexing capabilities to do a faster join.
#'
#' Because there is some overhead associated with setting up and indexing the
#' data.table structures, this function is only useful when the right-side table
#' is big enough that the savings in the join to make up for the overhead.
#' Therefore, this function should only be used for joins that are demonstrably
#' causing bottlenecks due to the size of the tables involved.  This version
#' should \emph{never} be the first choice in development.  As a rule of thumb,
#' any join that is taking more than 500ms using the dplyr join functions is a
#' candidate for this function.
#'
#' When using this function, be aware that data.table has some slightly
#' different conventions for handling duplicated columns that are not being
#' joined on.  Suppose we have tables \code{A} and \code{B}, both of which have
#' a column \code{value} that is not being joined on.  Then,
#' \code{AB <- dplyr::left_join(A, B)} will have columns \code{AB$value.x} with
#' the values from table \code{A} and \code{AB$value.y} with the values from
#' table \code{B}.  In \code{AB <- gcamdata::fast_left_join(A, B)}, the
#' corresponding columns will be \code{AB$i.value} for the values from table
#' \code{A}, and \code{AB$value} \emph{(sic)} for the values from table
#' \code{B}.  This function makes no attempt to correct the column names in the
#' result to conform to the dplyr convention, and is therefore not exactly a
#' drop-in replacement for \code{left_join}.  However, it is usually easy enough
#' to make corrections on the returned value.
#'
#' Since this function is intended only for specialized use, we don't provide
#' any of the other join variants like first-only or error-no-match.  The cases
#' where that extra functionality is needed \emph{and} the tables involved are
#' too large for the slower version of join are uncommon enough that they can be
#' handled on a case by case basis.  (That's documentation-speak for "You're on
#' your own.")
#'
#' @param left The left-side table to join.  Any class inheriting from
#' \code{data.frame} is acceptable.
#' @param right The right-side table to join.  Any class inheriting from
#' \code{data.frame} is acceptable.
#' @param by Character vector of column names to join by.
#' @return The left join of \code{left} and \code{right}.  It will be returned
#' as a \code{tbl_df}, irrespective of the type of the inputs.
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
#' @importFrom tibble as_tibble
fast_left_join <- function(left, right, by) {
  assert_that(is.data.frame(left))
  assert_that(is.data.frame(right))

  ## To key or not to key?  A key is required for the right table, but it is
  ## optional for the left, *provided* that the join columns are in order and
  ## come before the non-join columns.  Keying takes time, but it makes the
  ## join eventually go a little faster.  In the one example we have, it
  ## keying the left table doesn't seem to pay for itself in the join, but
  ## it's possible that depends on the specifics of the input.  For now we
  ## *won't* key, instead opting to reorder the columns of the left table.
  dtl <- data.table(left[ , union(by, names(left))])
  dtr <- data.table(right, key=by)

  as_tibble(dtr[dtl, allow.cartesian=TRUE])
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

  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year, ties = mean)$y,
             error = function(e) NA)

  } else {
    stop("Use fill_exp_decay_extrapolate!")
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
#' @importFrom dplyr full_join mutate
#' @author BBL
#' @export
#' @examples
#' x <- tibble::tibble(x = 1:3)
#' y <- tibble::tibble(y = c(4, 5), z = c(6, 7))
#' repeat_add_columns(x, y)
repeat_add_columns <- function(x, y) {
  UNIQUE_JOIN_FIELD <- NULL           # silence package checks.
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
change_iso_code <- function(d, from, to, col = "iso") {
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
#' @param delete_original Delete original column? Logical
#' @return Tibble with the iso codes converted to lower case. If the column with
#'   the codes was not called 'iso', it will be renamed to 'iso'.
#' @export
standardize_iso <- function(d, col = "iso", delete_original = TRUE) {
  assertthat::assert_that(tibble::is_tibble(d))
  d[["iso"]] <- tolower(d[[col]])
  if(delete_original && col != "iso") {
    # This is surprisingly hard to do using dplyr
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
#'     dplyr::select_if(function(col) {!any(is.na(col))}) %>%
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


#' missing_data
#'
#' @return A tibble used to signal missing (not created) data
missing_data <- function() {
  tibble(x = NA_real_) %>%
    add_title("Data not created") %>%
    add_units("Data not created") %>%
    add_comments("Data not created") %>%
    add_flags(FLAG_NO_TEST, FLAG_NO_OUTPUT)
}

#' Calculate a gross domestic product (GDP) implicit price deflator between two years.
#'
#' The GDP deflator is a measure of price inflation with respect to a
#' specific base year; it allows us to back out the effects of inflation when we
#' compare prices over time.  This function calculates a deflator given a base
#' year (the year to convert from) and a conversion year (the year to convert
#' to).  To use the deflator, multiply prices in base-year dollars by the deflator; the
#' result will be prices in the converted dollar year.
#'
#' @param year Year to convert TO.
#' @param base_year Year to convert FROM.
#' @return GDP Deflator.  Multiply to convert FROM \code{base_year} dollars TO
#' \code{year} dollars.
#' @source U.S. Bureau of Economic Analysis, Gross domestic product (implicit
#' price deflator) [A191RD3A086NBEA], retrieved from FRED, Federal Reserve Bank
#' of St. Louis; https://fred.stlouisfed.org/series/A191RD3A086NBEA, April 12,
#' 2017
#' @author BBL
#' @export
#' @examples
#' gdp_bil_1990USD <- c(4770, 4779, 4937)
#' gdp_bil_2010USD <- gdp_bil_1990USD * gdp_deflator(2010, base_year = 1990)
gdp_deflator <- function(year, base_year) {
  # This time series is the BEA "A191RD3A086NBEA" product
  # Downloaded April 13, 2017 from https://fred.stlouisfed.org/series/A191RD3A086NBEA
  gdp_years <- 1929:2019
  gdp <- c(9.896, 9.535, 8.555, 7.553, 7.345, 7.749, 7.908, 8.001, 8.347,
           8.109, 8.033, 8.131, 8.68, 9.369, 9.795, 10.027, 10.288, 11.618,
           12.887, 13.605, 13.581, 13.745, 14.716, 14.972, 15.157, 15.298,
           15.559, 16.091, 16.625, 17.001, 17.237, 17.476, 17.669, 17.886,
           18.088, 18.366, 18.702, 19.227, 19.786, 20.627, 21.642, 22.784,
           23.941, 24.978, 26.337, 28.703, 31.361, 33.083, 35.135, 37.602,
           40.706, 44.377, 48.52, 51.53, 53.565, 55.466, 57.24, 58.395,
           59.885, 61.982, 64.392, 66.773, 68.996, 70.569, 72.248, 73.785,
           75.324, 76.699, 78.012, 78.859, 80.065, 81.887, 83.754, 85.039,
           86.735, 89.12, 91.988, 94.814, 97.337, 99.246, 100, 101.221,
           103.311, 105.214, 106.913, 108.828, 109.998, 111.445, 113.545,
           116.311, 118.339)
  names(gdp) <- gdp_years

  assert_that(all(year %in% gdp_years))
  assert_that(all(base_year %in% gdp_years))

  as.vector(unlist(gdp[as.character(year)] / gdp[as.character(base_year)]))
}


#' Helper function: call \code{tidyr::gather} for year-like columns and convert them to integers
#'
#' @param d Data frame to operate on (a tibble)
#' @param value_col Name of the resulting (gathered) value column, string or unquoted column name
#' @param year_pattern Year pattern to match against
#' @param na.rm Remove NAs flag passed on to tidyr::gather
#' @return The gathered (reshaped) data frame.
#' @importFrom dplyr matches mutate
#' @importFrom tidyr gather
#' @export
gather_years <- function(d, value_col = "value", year_pattern = YEAR_PATTERN, na.rm = FALSE) {
  assert_that(is_tibble(d))
  assert_that(is.character(value_col))
  assert_that(is.character(year_pattern))

  . <- year <- value <- NULL  # silence package check notes

  d %>%
    gather(year, value, matches(year_pattern), na.rm = na.rm) %>%
    mutate(year = as.integer(year)) %>%
    stats::setNames(sub("value", value_col, names(.)))
}



#kbn adding notin for later calculations
#' Helper function: select elements not in user defined criteria
#' @usage a %notin% b
#' @export
`%notin%` <- Negate(`%in%`)
