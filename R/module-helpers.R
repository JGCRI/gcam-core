# module-helpers.R
# Module specific helper functions

#' set_water_input_name
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
#' @author BBL April 2017
set_water_input_name <- function(water_sector, water_type, water_mapping, GLU = NA_character_) {

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


#' rename_SO2
#'
#' Rename SO2 to regional SO2. (TODO)
#'
#' @param x Data object (typically from pipeline)
#' @param so2_map TODO
#' @param is_awb TODO
#' @return Data object with \code{Non.CO2} changed to SO2 name for SO2 data.
#' @author BBL May 2017
rename_SO2 <- function(x, so2_map, is_awb = FALSE) {

  extension <- if_else(is_awb, "_AWB", "")
  data_so2 <- filter(x, Non.CO2 == paste0("SO2", extension))
  data_notso2 <- filter(x, Non.CO2 != paste0("SO2", extension))

  so2_map %>%
    mutate(SO2_name = paste0(SO2_name, extension)) %>%
    # pull so2_map information into SO2 data
    select(region, SO2_name) %>%
    left_join_error_no_match(data_so2, ., by = "region") %>%
    rename(Non.CO2 = SO2_name) %>%
    bind_rows(data_notso2)
}
