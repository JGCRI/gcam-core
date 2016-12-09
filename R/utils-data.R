# utils-data.R
#
# Utility functions dealing with data storage, comments, flags
# Only functions in this file 'know' the internal structure
# Everyone accesses the data store through add_data, get_data, etc.


#' add_dscomments
#'
#' Add character comments to a data system object. Comments are written out
#' with the data when the file is saved.
#'
#' @param x An object
#' @param comments A character vector of comments
#' @return \code{x} with comments appended to any existing comments.
add_dscomments <- function(x, comments) {
  assertthat::assert_that(is.character(comments))
  comment(x) <- c(comment(x), comments)
  x
}


#' get_dscomments
#'
#' @param x An object
#' @return Comments attached to \code{x}.
get_dscomments <- function(x) {
  comment(x)
}


#' add_dsflag
#'
#' Add character flags to a data system object. Flags are used internally, and in some
#' cases (for testing data) are written out with the data when the file is saved.
#'
#' @param x An object
#' @param flags A character vector of flags
#' @return \code{x} with flags appended to any existing flags
add_dsflags <- function(x, flags) {
  assertthat::assert_that(is.character(flags))
  attr(x, "flags") <- c(attr(x, "flags"), flags)
  x
}


#' get_dsflags
#'
#' @param x An object
#' @return Flags attached to \code{x}.
get_dsflags <- function(x) {
  attr(x, "flags")
}

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
  assertthat::assert_that(!is.null(all_data[[name]]))
  all_data[[name]]
}


#' return_data
#'
#' Construct a data structure of objects (\code{...}) and return it.
#' Abstracts this away from chunk function code.
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
#'
#' @return The modified data store.
add_data <- function(data_list, all_data) {
  assertthat::assert_that(!is.null(names(data_list)))

  for(d in names(data_list)) {
    all_data[[d]] <- data_list[[d]]
  }
  all_data
}
