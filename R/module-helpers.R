# module-helpers.R
# Module specific helper functions

#' get_water_inputs_for_mapping
#'
#' Get the appropriate minicam.energy.input name to use in the GCAM supplysector.
#'
#' @param water_sector A character vector containing water sector information
#' @param water_type A character vector containing water type information
#' @param water_mapping A tibble with a mapping between \code{water.sector} and  \code{supplysector}
#' @param GLU An optional character vector containing GLU information (only used for irrigation with mapped water types)
#' @details Get the appropriate minicam.energy.input name to use in the GCAM supplysector
#' by looking up using a mapping to the water.sector and water_type. The minicam.energy.input
#' name to use will have to be some water mapping sector for water_types that are "mapped".
#' @return A vector of names of form supplysector_watertype or supplysector_GLU_watertype.
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom assertthat assert_that
get_water_inputs_for_mapping <- function(water_sector, water_type, water_mapping, GLU = NA_character_) {

  # Sanity checks
  assert_that(is.character(water_sector))
  assert_that(is.character(water_type))
  assert_that(length(water_sector) == length(water_type))
  assert_that(tibble::is_tibble(water_mapping))
  assert_that(all(c("water.sector", "supplysector") %in% names(water_mapping)))
  assert_that(is.character(GLU))

  # If there's an irrigation sector w/ mapped water type, need a GLU
  if(any(water_sector == IRRIGATION & water_type %in% MAPPED_WATER_TYPES)) {
    assert_that(all(!is.na(GLU)))
    assert_that(length(GLU) == length(water_sector))
  }

  tibble(water_sector, water_type, GLU) %>%
    # Add in the base mapped sector name and short water names
    left_join_error_no_match(select(water_mapping, water.sector, supplysector), by = c("water_sector" = "water.sector")) %>%
    mutate(wt_short = MAPPED_WATER_TYPES_SHORT[water_type],
           # non-mapped water_types keep their names unchanged
           new_name = if_else(water_type %in% MAPPED_WATER_TYPES, NA_character_, water_type),
           # non-irrigation mapped types
           new_name = if_else(water_sector != IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, wt_short, sep = "_"), new_name),
           # irrigation mapped types - needs the GLU column
           new_name = if_else(water_sector == IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, GLU, wt_short, sep = "_"), new_name)) %>%
  .$new_name
}
