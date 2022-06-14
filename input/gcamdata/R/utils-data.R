# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @param overwrite Allow overwrite of title? Logical
#' @return \code{x} with units appended to any existing comments.
add_title <- function(x, title, overwrite = FALSE) {
  assertthat::assert_that(is.character(title) | is.null(title))

  if(!overwrite && !is.null(attr(x, ATTR_TITLE))) {
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
#' @param overwrite Allow overwrite of legacy_name? Logical
#' @return \code{x} with legacy name set.
add_legacy_name <- function(x, legacy_name, overwrite = FALSE) {
  assertthat::assert_that(is.character(legacy_name) | is.null(legacy_name))

  if(!overwrite && !is.null(attr(x, ATTR_LEGACY_NAME))) {
    stop("Not allowed to overwrite current legacy name '", attr(x, ATTR_LEGACY_NAME), "'")
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
#' @param copy_strict_flags A flag if \code{FALSE} indicates to not copy the "strict"
#' flags which may not be manipulated later.  Namely this avoids copying the title
#' and legacy name.  Note the default is copy everything, i.e. \code{TRUE}.
#' @return \code{x} with attributes set to those of \code{y}.
same_attributes_as <- function(x, y, copy_strict_flags = TRUE) {
  assertthat::assert_that(is.logical(copy_strict_flags))
  if(copy_strict_flags) {
    attr(x, ATTR_TITLE) <- attr(y, ATTR_TITLE)
  }
  attr(x, ATTR_UNITS) <- attr(y, ATTR_UNITS)
  attr(x, ATTR_COMMENTS) <- attr(y, ATTR_COMMENTS)
  attr(x, ATTR_PRECURSORS) <- attr(y, ATTR_PRECURSORS)
  if(copy_strict_flags) {
    attr(x, ATTR_LEGACY_NAME) <- attr(y, ATTR_LEGACY_NAME)
  }
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
#' @param strip_attributes Boolean indicating that `gcamdata` attributes should be
#'                         removed when this data chunk is loaded.
#' @return Data object (currently, a tibble or data frame). If the object was marked
#' \code{NA} in the data store, indicating an optional input that was not found,
#' a \code{NULL} is returned.
get_data <- function(all_data, name, strip_attributes = FALSE) {
  assertthat::assert_that(is_data_list(all_data))

  names(all_data) <- gsub(data.USER_MOD_POSTFIX, '', names(all_data))
  if(is.null(all_data[[name]])) {
    stop("Data system: couldn't find ", name)
  }

  # If a chunk's output is missing, it returns a tibble with all NA values
  # In this case we don't want to copy it to main data list, so that subsequent
  # chunks an easily check for its status via is.null()
  if(nrow(all_data[[name]]) > 0 && all(is.na(all_data[[name]]))) {
    return(NULL)
  }

  # If strip_attributes == TRUE, remove all attributes.
  # As of dplyr 1.0.0, these can no longer be easily overwritten, so we remove them
  if(strip_attributes) {
    attr(all_data[[name]], ATTR_TITLE) <- NULL
    attr(all_data[[name]], ATTR_UNITS) <- NULL
    attr(all_data[[name]], ATTR_COMMENTS) <- NULL
    attr(all_data[[name]], ATTR_PRECURSORS) <- NULL
    attr(all_data[[name]], ATTR_LEGACY_NAME) <- NULL
    attr(all_data[[name]], ATTR_REFERENCE) <- NULL
    all_data[[name]]
  } else {
    all_data[[name]]
  }
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
  raw_names <- as.list(substitute(list(...)))[-1L]
  # if a user explicitly named a return data then keep their name
  # otherwise use the name of the raw variable to set the name
  if(is.null(names(dots))) {
    # if none of the arguments were explicitly named then `names(dots)`
    # returns NULL and we should set all the names to the raw names
    names(dots) <- raw_names
  } else {
    # at least some variables are explicitly named
    # those that are not will have an empty name so replace those with
    # the raw name
    names(dots)[names(dots) == ""] <- raw_names[names(dots) == ""]
  }
  # disallow any data which is "grouped" as it may lead to unexpected
  # behavior, especially for unsuspecting chunks which may use it down
  # the line not expecting any groupings.
  lapply(names(dots), function(dname) {
    # note we may return data which are not tibbles however for any
    # data which group_by had been called on is_tibble will return
    # true including for instance data.tables
    # any other data could not possibly be grouped so we can skip the
    # check for them
    if(is_tibble(dots[[dname]])) {
      assert_that(length(dplyr::groups(dots[[dname]])) == 0, msg =
                    paste0(dname, " is being returned grouped. This is not allowed; please ungroup()"))
    }
  })
  dots
}

#' return_modified
#'
#' Construct a data structure of objects (\code{...}) and return it.
#' This version should only be used in user modification chunks where
#' it is used in place of \link{return_data}.
#'
#' @param ... Objects to handle
#' @return Object ready for insertion into the data system data structure.
#' @export
return_modified <- function(...) {
  data_list <- return_data(...)
  lapply(names(data_list), function(dname) {
    attr(data_list[[dname]], ATTR_PRECURSORS) <<- c(dname)
  })
  names(data_list) <- paste0(names(data_list), data.USER_MOD_POSTFIX)

  data_list
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


#' remove_data
#'
#' Remove \code{data_list} from an existing data store.
#'
#' @param data_list A character vector of object names
#' @param all_data An existing (possibly empty) data store
#' @importFrom assertthat assert_that
#' @return The modified data store.
remove_data <- function(data_list, all_data) {
  assert_that(is.character(data_list) | is.null(data_list))
  assert_that(is_data_list(all_data))

  for(d in data_list) {
    assert_that(!is.null(all_data[[d]]))
    all_data[[d]] <- NULL
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


#' extract_prebuilt_data
#'
#' Extract a prebuilt data object from the PREBUILT_DATA store.
#'
#' @param object_name The name of the desired object, character
#' @param pb \code{PREBUILT_DATA} object; overridden only for testing
#' @return The data object (a tibble).
extract_prebuilt_data <- function(object_name, pb = NULL) {
  if(is.null(pb)) {
    pb <- PREBUILT_DATA
  }
  if(object_name %in% names(pb)) {
    pb[[object_name]] %>%
      add_comments("** PRE-BUILT; RAW IEA DATA NOT AVAILABLE **")
  } else {
    NULL
  }
}


#' verify_identical_prebuilt
#'
#' Check whether objects are identical to their prebuilt versions.
#'
#' @param ... The objects
#' @param pb \code{PREBUILT_DATA} object; overridden only for testing
#' @note Called primarily for its side effects: a warning is issued for each non-identical object.
#' @return A logical indicating whether a mismatch occurred.
verify_identical_prebuilt <- function(..., pb = NULL) {
  if(is.null(pb)) {
    pb <- PREBUILT_DATA
  }
  dots <- list(...)
  names(dots) <- as.list(substitute(list(...)))[-1L]
  mismatch <- FALSE
  for(i in seq_along(dots)) {
    if(!isTRUE(all.equal(dots[[i]], pb[[names(dots)[i]]]))) {
      warning(names(dots)[i], " is not the same as its prebuilt version")
      mismatch <- TRUE
    }
  }
  if(mismatch) {
    warning("Re-run generate_package_data.R to rebuild package data")
  }
  invisible(mismatch)
}
