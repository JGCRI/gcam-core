# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L100.water_supply_runoff
#'
#' Prepare GCAM basin runoff and accessible water fractions using Xanthos output.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.runoff_max_bm3}, \code{L100.runoff_accessible}. The corresponding file in the
#' original data system was \code{L100.water_supply_runoff.R} (Water level1).
#' @details Reads Xanthos outputs and converts to maximum and accessible runoff water for all GCAM model years.
#' @importFrom tibble tibble
#' @importFrom dplyr arrange filter group_by mutate rename summarise ungroup
#' @importFrom tidyr complete gather nesting
#' @author ST September 2018
module_water_L100.water_supply_runoff <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/xanthos_basin_runoff",
             FILE = "water/xanthos_accessible_water"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.runoff_max_bm3",
             "L100.runoff_accessible"))
  } else if(command == driver.MAKE) {

    year <- runoff <- name <- accessible_water <-
      access_fraction <- runoff_max <- id <- GCAM_basin_ID <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    # historical runoff and accessible water by basin for 1961 - 2016
    xanthos_runoff <- get_data(all_data, "water/xanthos_basin_runoff")
    xanthos_access <- get_data(all_data, "water/xanthos_accessible_water")

    # convert data to long form
    xanthos_runoff %>%
      gather(year, runoff, -name, -id) ->
      runoff_historical

    xanthos_access %>%
      gather(year, accessible_water, -name, -id) ->
      accessible_historical

    # compute the accessible fraction as the average ...
    # ... of accessible / runoff for each basin
    left_join_error_no_match(runoff_historical,
              accessible_historical,
              by = c("id", "name", "year")) %>%
      mutate(access_fraction = accessible_water / runoff) %>%
      group_by(id) %>% summarise(access_fraction = mean(access_fraction)) %>%
      ungroup() %>%
      rename(GCAM_basin_ID = id) ->
      L100.runoff_accessible

    # compute basin runoff as mean of
    # (this will have to be updated for climate change runs from Xanthos)
    runoff_historical %>%
      group_by(id) %>%
      summarise(runoff_max = mean(runoff)) %>%
      ungroup() %>%
      rename(GCAM_basin_ID = id) %>%
      mutate(year = 2000) %>% #temp year written over by following line
      complete(year = MODEL_YEARS, nesting(GCAM_basin_ID, runoff_max)) %>%
      filter(year %in% MODEL_YEARS) %>%
      arrange(GCAM_basin_ID) ->
      L100.runoff_max_bm3


    # Prepare outputs

    L100.runoff_max_bm3 %>%
      add_title("Maximum runoff by basin (all model years)") %>%
      add_units("km^3/yr") %>%
      add_comments("Computed directly from Xanthos outputs") %>%
      add_legacy_name("L100.runoff_max_bm3") %>%
      add_precursors("water/xanthos_basin_runoff") ->
      L100.runoff_max_bm3

    L100.runoff_accessible %>%
      add_title("Proportion of runoff available for access by basin") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L100.runoff_max_bm3") %>%
      add_precursors("water/xanthos_basin_runoff",
                     "water/xanthos_accessible_water") ->
      L100.runoff_accessible

    return_data(L100.runoff_max_bm3,
                L100.runoff_accessible)

  } else {
    stop("Unknown command")
  }
}
