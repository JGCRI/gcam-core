# utils-data.R
#
# Utility functions dealing with data storage, comments, flags
# Only functions in this file 'know' the internal structure
# Everyone accesses the data store through add_data, get_data, etc.


ATTR_TITLE <- "title"
ATTR_UNITS <- "units"
ATTR_COMMENTS <- "comments"
ATTR_PRECURSORS <- "precursors"
ATTR_LEGACY_NAME <- "legacy_name"
ATTR_REFERENCE <- "reference"

#' add_title
#'
#' Add character units to a data system object. Units are written out
#' with the data when the file is saved.
#'
#' @param x An object
#' @param title Title of object (character)
#' @return \code{x} with units appended to any existing comments.
add_title <- function(x, title) {
  assertthat::assert_that(is.character(title) | is.null(title))

  if(!is.null(attr(x, ATTR_TITLE))) {
    stop("Not allowed to overwrite current title '", attr(x, ATTR_TITLE), "'")
  }
  attr(x, ATTR_TITLE) <- title
  x
}

get_title <- function(x) { attr(x, ATTR_TITLE) }

#' add_comments
#'
#' Add character comments to a data system object. Comments are written out
#' with the data when the file is saved.
#'
#' @param x An object
#' @param comments A character vector of comments
#' @return \code{x} with comments appended to any existing comments.
add_comments <- function(x, comments) {
  assertthat::assert_that(is.character(comments) | is.null(comments))
  comments <- gsub("[\r\n]", " ", comments)  # remove any line breaks (h/t CH)
  attr(x, ATTR_COMMENTS) <- c(attr(x, ATTR_COMMENTS), comments)
  x
}

#' add_legacy_name
#'
#' Add the legacy (old data system) name to a data system object.
#'
#' @param x An object
#' @param legacy_name Legacy name (character)
#' @return \code{x} with legacy name set.
add_legacy_name <- function(x, legacy_name) {
  assertthat::assert_that(is.character(legacy_name) | is.null(legacy_name))

  if(!is.null(attr(x, ATTR_LEGACY_NAME))) {
    stop("Not allowed to overwrite current title '", attr(x, ATTR_LEGACY_NAME), "'")
  }
  attr(x, ATTR_LEGACY_NAME) <- legacy_name
  x
}

get_legacy_name <- function(x) { attr(x, ATTR_LEGACY_NAME) }

#' get_comments
#'
#' @param x An object
#' @return Comments attached to \code{x}.
get_comments <- function(x) {
  attr(x, ATTR_COMMENTS)
}

#' add_units
#'
#' Add character units to a data system object. Units are written out
#' with the data when the file is saved.
#'
#' @param x An object
#' @param units Units (character)
#' @return \code{x} with units appended to any existing comments.
add_units <- function(x, units) {
  assertthat::assert_that(is.character(units) | is.null(units))
  attr(x, ATTR_UNITS) <- c(attr(x, ATTR_UNITS), units)
  x
}

get_units <- function(x) { attr(x, ATTR_UNITS) }

#' add_precursors
#'
#' Add names of precursors (objects that contribute to the computation)
#' to a data system object. This allows for granular tracking
#' of dependencies across the data system.
#'
#' @param x An object
#' @param ... Names of precursor objects (character)
#' @return \code{x} with units appended to any existing comments.
add_precursors <- function(x, ...) {
  pc <- as.character(list(...))
  attr(x, ATTR_PRECURSORS) <- c(attr(x, ATTR_PRECURSORS), pc)
  x
}

#' same_precursors_as
#'
#' Copy precursors from one object to another.
#'
#' @param x An object (that should get same precursors as \code{y})
#' @param y Another object (source of precursors)
#' @return \code{x} with precursors set to those of \code{y}.
same_precursors_as <- function(x, y) {
  attr(x, ATTR_PRECURSORS) <- c(attr(x, ATTR_PRECURSORS), attr(y, ATTR_PRECURSORS))
  x
}

get_precursors <- function(x) { attr(x, ATTR_PRECURSORS) }


#' same_attributes_as
#'
#' Copy attributes from one data system object to another.
#'
#' @param x An object (that should get same data system attributes as \code{y})
#' @param y Another object (source of attributes)
#' @return \code{x} with attributes set to those of \code{y}.
same_attributes_as <- function(x, y) {
  attr(x, ATTR_TITLE) <- attr(y, ATTR_TITLE)
  attr(x, ATTR_UNITS) <- attr(y, ATTR_UNITS)
  attr(x, ATTR_COMMENTS) <- attr(y, ATTR_COMMENTS)
  attr(x, ATTR_PRECURSORS) <- attr(y, ATTR_PRECURSORS)
  attr(x, ATTR_LEGACY_NAME) <- attr(y, ATTR_LEGACY_NAME)
  attr(x, ATTR_REFERENCE) <- attr(y, ATTR_REFERENCE)
  x
}


#' add_flags
#'
#' Add character flags to a data system object. Flags are used internally, and in some
#' cases (for testing data) are written out with the data when the file is saved.
#'
#' @param x An object
#' @param ... One or more flags (that can be coerced to character)
#' @return \code{x} with flags appended to any existing flags
add_flags <- function(x, ...) {
  attr(x, "flags") <- c(attr(x, "flags"), ...)
  x
}


#' get_flags
#'
#' @param x An object
#' @return Flags attached to \code{x}.
get_flags <- function(x) {
  attr(x, "flags")
}

#' add_reference
#'
#' Add character units to a data system object. Units are written out
#' with the data when the file is saved.
#'
#' @param x An object
#' @param reference Reference for object (character)
#' @return \code{x} with units appended to any existing references
add_reference <- function(x, reference) {
  assertthat::assert_that(is.character(reference) | is.null(reference))

  attr(x, ATTR_REFERENCE) <- c(attr(x, ATTR_REFERENCE), reference)
  x
}

get_reference <- function(x) { attr(x, ATTR_REFERENCE) }


#' getdata
#'
#' This function returns a tibble (currently) in \code{all_data},
#' abstracting away the mechanism for accessing it from the chunks.
#' Throws an error if data is not present.
#'
#' @param all_data Data structure
#' @param name Name of data to return
#' @return Data object (currently, a tibble or data frame).
get_data <- function(all_data, name) {
  assertthat::assert_that(is_data_list(all_data))
  if(is.null(all_data[[name]])) {
    stop("Data system: couldn't find ", name)
  }
  all_data[[name]]
}


#' return_data
#'
#' Construct a data structure of objects (\code{...}) and return it.
#' Abstracts this away from chunk function code.
#'
#' @param ... Objects to handle
#' @return Object ready for insertion into the data system data structure.
return_data <- function(...) {
  dots <- list(...)
  names(dots) <- as.list(substitute(list(...)))[-1L]
  dots
}


#' empty_data
#'
#' @return An empty data store.
empty_data <- function() { list() }


#' add_data
#'
#' Add \code{data_list} to an existing data store.
#'
#' @param data_list List of data frames or other objects
#' @param all_data An existing (possibly empty) data store
#' @importFrom assertthat assert_that
#' @return The modified data store.
add_data <- function(data_list, all_data) {
  assert_that(is_data_list(data_list))
  assert_that(!is.null(names(data_list)))
  assert_that(is_data_list(all_data))

  for(d in names(data_list)) {
    all_data[[d]] <- data_list[[d]]
  }
  all_data
}


#' is_data_list
#'
#' Check whether an object is a valid list (collection) of data objects.
#'
#' @param data_list List of data frames or other objects
#' @return TRUE or FALSE.
#' @details Currently a data_list is just a list.
is_data_list <- function(data_list) {
  is.list(data_list)
}
