# module-helpers.R
# Module specific helper functions

#' get_water_inputs_for_mapping
#'
#' Get the appropriate minicam.energy.input name to use in the GCAM supplysector.
#'
#' @param x A tibble containing a \code{water.sector} and \code{water_type}
#' (and \code{GLU} if the water sector is irrigation) to determine the new input names
#' @param water_mapping A tibble with a mapping between \code{water.sector} and  \code{supplysector}
#' @details Get the appropriate minicam.energy.input name to use in the GCAM supplysector
#' by looking up using a mapping to the water.sector and water_type. The minicam.energy.input
#' name to use will have to be some water mapping sector for water_types that are "mapped".
#' @return A vector of names of form supplysector_watertype or supplysector_GLU_watertype.
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom assertthat assert_that
get_water_inputs_for_mapping <- function(x, water_mapping) {

  # Sanity checks
  assert_that(tibble::is_tibble(x))
  assert_that(tibble::is_tibble(water_mapping))

  # If there's no irrigation sector w/ mapped water type, create an empty GLU column (if one
  # doesn't already exist) so the if_else below executes OK
  if(!any(x$water.sector == "Irrigation" & x$water_type %in% MAPPED_WATER_TYPES)) {
    x$GLU <- NA  # will not be used
  }

  x %>%
    # Add in the base mapped sector name and short water names
    left_join_error_no_match(select(water_mapping, water.sector, supplysector), by = "water.sector") %>%
    mutate(water_type_short = MAPPED_WATER_TYPES_SHORT[water_type],
           # non-mapped water_types keep their names unchanged
           new_name = if_else(water_type %in% MAPPED_WATER_TYPES, NA_character_, water_type),
           # non-irrigation mapped types
           new_name = if_else(water.sector != IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, water_type_short, sep = "_"), new_name),
           # irrigation mapped types - needs the GLU column
           new_name = if_else(water.sector == IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, GLU, water_type_short, sep = "_"), new_name)) ->
    x
  x$new_name
}
