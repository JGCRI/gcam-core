# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L111.ag_resbio_R_C
#'
#' Calculate the production-weighted parameters of residue biomass by GCAM region and commodity.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.ag_resbio_R_C}. The corresponding file in the
#' original data system was \code{LB111.ag_resbio_R_C.R} (aglu level1).
#' @details This chunk calculates the production-weighted average
#' residue biomass parameters by GCAM region and commodity.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by mutate select summarize_if
#' @importFrom tidyr gather spread
#' @author RC March 2017 XZ 2022
module_aglu_L111.ag_resbio_R_C <- function(command, ...) {

  MODULE_INPUTS <-
    c("L100.FAO_ag_Prod_t",
      FILE = "aglu/Various_ag_resbio_data")

  MODULE_OUTPUTS <-
    c("L111.ag_resbio_R_C")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    iso <- item <- year <- value <- resbio_params <- GCAM_region_ID <-
        GCAM_commodity <- NULL          # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Future development: L111.ag_resbio_R_C should be differentiated by GCAM_subsector

    # Compute weighted averages of each parameter (HarvestIndex, ErosionControl, and
    # ResidueEnergyContent) for each crop type in each GCAM region
    L100.FAO_ag_Prod_t %>%
      select(iso, GCAM_region_ID, item, item_code, GCAM_commodity, year, value) %>%
      filter(year %in% max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      rename(prod = value) %>%
      left_join(Various_ag_resbio_data %>% select(-item), by = "item_code") %>%
      # Drop rows with NA values (dropping commodities that are not in the resbio dataset)
      na.omit() %>%
      # also drop rows where production weights are zero, as these would return missing values later on
      filter(prod != 0) %>%
      # Multiply by production to get weights, change to long-format for easier calculation
      gather(resbio_params, value, -iso, -GCAM_region_ID, -GCAM_commodity, -item, -item_code, -prod) %>%
      mutate(value = value * prod) %>%
      # For GCAM regions and commodities, collapse, and divide by production to get residue biomass values
      select(-item_code) %>%
      group_by(GCAM_region_ID, GCAM_commodity, resbio_params) %>%
      summarize_if(is.numeric, sum) %>%
      # Dividing by production to get weighted average residue biomass parameters by region and crop
      ungroup() %>%
      mutate(value = value / prod) %>%
      select(-prod) %>%
      spread(resbio_params, value) %>%

      # Produce outputs
      add_title("Weighted average residue biomass parameters by GCAM region / commodity") %>%
      add_units("Varied") %>%
      add_comments("Calculate the HarvestIndex, ErosCtrl, ResEnergy, Root_Shoot, and WaterContent of residue biomass") %>%
      add_comments("These parameters are weighted by production when calculating the average by GCAM region and commodity") %>%
      add_legacy_name("L111.ag_resbio_R_C") %>%
      add_precursors("L100.FAO_ag_Prod_t",
                     "aglu/Various_ag_resbio_data") ->
      L111.ag_resbio_R_C

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
