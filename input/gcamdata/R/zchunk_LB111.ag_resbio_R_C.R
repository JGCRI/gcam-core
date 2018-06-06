#' module_aglu_LB111.ag_resbio_R_C
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
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC March 2017
module_aglu_LB111.ag_resbio_R_C <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L100.FAO_ag_Prod_t",
             FILE = "aglu/Various_ag_resbio_data"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.ag_resbio_R_C"))
  } else if(command == driver.MAKE) {

    iso <- item <- year <- value <- resbio_params <- GCAM_region_ID <-
        GCAM_commodity <- NULL          # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
    Various_ag_resbio_data <- get_data(all_data, "aglu/Various_ag_resbio_data")

    # Compute weighted averages of each parameter (HarvestIndex, ErosionControl, and
    # ResidueEnergyContent) for each crop type in each GCAM region
    L100.FAO_ag_Prod_t %>%
      select(iso, item, year, value) %>%
      filter(year %in% max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      rename(prod = value) %>%
      full_join(Various_ag_resbio_data, by = "item") %>%
      # Drop rows with NA values (dropping commodities that are not in the resbio dataset)
      na.omit() %>%
      # also drop rows where production weights are zero, as these would return missing values later on
      filter(prod != 0) %>%
      # Multiply by production to get weights, change to long-format for easier calculation
      gather(resbio_params, value, -iso, -item, -prod) %>%
      mutate(value = value * prod) %>%
      # Add vectors for GCAM regions and commodities, collapse, and divide by production to get residue biomass values
      left_join_error_no_match(iso_GCAM_regID[c("iso", "GCAM_region_ID")], by = "iso") %>%
      left_join_error_no_match(FAO_ag_items_PRODSTAT[c("item", "GCAM_commodity")], by = "item") %>%
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
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.FAO_ag_Prod_t",
                     "aglu/Various_ag_resbio_data") ->
      L111.ag_resbio_R_C

    return_data(L111.ag_resbio_R_C)
  } else {
    stop("Unknown command")
  }
}
