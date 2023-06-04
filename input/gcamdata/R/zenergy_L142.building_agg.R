# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L142.building_agg
#'
#' Calculate building sector energy consumption, producing the following output table: Building energy consumption by GCAM region / fuel / historical year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.in_EJ_R_bld_F_Yh}. The corresponding file in the
#' original data system was \code{LA142.building_agg.R} (energy level1).
#' @details  Building sector energy consumption was obtained from end use energy consumption data
#' @details  Fuel inputs to heat were added to building energy use in regions where heat is not modeled as a final fuel
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate pull select summarise
#' @author AS May 2017
module_energy_L142.building_agg <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/mappings/enduse_sector_aggregation",
             "L124.in_EJ_R_heat_F_Yh",
             "L131.in_EJ_R_Senduse_F_Yh",
             "L131.share_R_Senduse_heat_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.in_EJ_R_bld_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    enduse_sector_aggregation <- get_data(all_data, "energy/mappings/enduse_sector_aggregation")
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh")
    L131.share_R_Senduse_heat_Yh <- get_data(all_data, "L131.share_R_Senduse_heat_Yh")

    # ===================================================

    # Calculation of building sector energy consumption

    # Silence package check notes
    . <- has_district_heat <- fuel <- bld <- sector_agg <- GCAM_region_ID <- sector <- year <- variable <- value <-
      tradbio_region <- conc_column <- value_share <- NULL

    # End use data will be filtered for building relevant sectors
    # The first intermediate fuel will be mapped to a sector-specific intermediate fuel
    # The first intermediate sector will be mapped to an aggregated intermediate sector
    L131.in_EJ_R_Senduse_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      filter(grepl("bld", sector)) %>% # Filter for only building-related sectors
      left_join_error_no_match(enduse_sector_aggregation, by = "sector") %>% # Mapping first intermediate sector to aggregated intermediate sector
      # Cannot use left_join_error_no_match next because non-blg columns in joining dataframe will have NAs. Those will be filtered out later however.
      left_join(enduse_fuel_aggregation, by = "fuel") %>% # Mapping first intermediate fuel to sector-specific intermediate fuel
      group_by(GCAM_region_ID, sector_agg, bld, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(sector = sector_agg, fuel = bld) %>%
      mutate(sector = sub("in_", "", sector)) ->
      L142.in_EJ_R_bld_F_Yh

    # Heat: fuel inputs to heat need to be added to building energy use, but only in regions where heat is not modeled as a final fuel
    # We will use data on the share of end use heat, but we will first filter for building relevant sectors only
    L131.share_R_Senduse_heat_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      filter(grepl("bld", sector)) %>% # Filter for building-related sectors only
      mutate(sector = sub("in_", "", sector)) %>%
      select(-fuel) %>% # Entries for fuel are only heat. Doesn't serve a purpose so remove.
      rename(value_share = value) ->
      L142.share_R_bld_heat_Yh

    # Next, we want to multiply these shares by the energy inputs to heat, but only for regions where heat is not modeled as a final fuel.
    # We will create a list for these regions.
    A_regions %>%
      filter(has_district_heat == 0) %>% # Note, "0" indicates that heat is not modeled as a final fuel. "1" indicates that it is.
      pull(GCAM_region_ID) ->
      list_no_heat

    # We also want to create a list for relevant building heat sectors
    list_sector <- unique(L142.share_R_bld_heat_Yh$sector)

    # Here are the energy inputs to heat. We will first filter it for regions where heat is not modeled as a final fuel,
    # then expand the dataframe to include relevant building heat sectors,
    # and finally multiply the values by the shares from above
    L124.in_EJ_R_heat_F_Yh %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS, GCAM_region_ID %in% list_no_heat) %>% # Filter for region where heat is not modeled as a final fuel
      select(-sector) %>% # We want to populate it using our list of sectors, so we will remove this.
      repeat_add_columns(tibble::tibble(sector = list_sector)) %>%
      left_join_error_no_match(L142.share_R_bld_heat_Yh, by = c("GCAM_region_ID", "sector", "year")) %>% # Joining the share data
      mutate(value = value * value_share) %>% # Multiplying the energy input data by its share
      select(GCAM_region_ID, sector, fuel, year, value) ->
      L142.in_EJ_R_bldheat_F_Yh

    # Re-calculate building energy as original estimate plus fuel inputs to heat in regions where heat is not modeled as its own fuel
    L142.in_EJ_R_bld_F_Yh %>%
      bind_rows(L142.in_EJ_R_bldheat_F_Yh) ->
      L142.in_EJ_R_Sbld_F_Yh

    # Finally, we want to drop heat in regions where this fuel is backed out to its fuel inputs
    # We will create two lists to use for filtering out this data
    # We will concatenate the region ID and fuel input (i.e., heat and traditional biomass) so as to selectively remove those pairs from the dataset
    A_regions %>%
      filter(has_district_heat == 0) %>%
      mutate(concatenate_list_no_heat = paste(GCAM_region_ID, "heat")) %>%
      pull(concatenate_list_no_heat) ->
      concatenate_list_no_heat

    A_regions %>%
      filter(tradbio_region == 0) %>%
      mutate(concatenate_list_no_tradbio = paste(GCAM_region_ID, "traditional biomass")) %>%
      pull(concatenate_list_no_tradbio) ->
      concatenate_list_no_tradbio

    # Now those region ID/fuel input pairs will be removed
    L142.in_EJ_R_Sbld_F_Yh %>%
      mutate(conc_column = paste(GCAM_region_ID, fuel)) %>% # Create concatenate list in base dataframe to match the syntax of our lists above
      filter(!conc_column %in% concatenate_list_no_heat, !conc_column %in% concatenate_list_no_tradbio) %>% # Dropping heat in regions where this fuel is backed out to its fuel inputs
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L142.in_EJ_R_bld_F_Yh

    # ===================================================

    L142.in_EJ_R_bld_F_Yh %>%
      add_title("Building energy consumption by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Building sector energy consumption was obtained from end use energy consumption data") %>%
      add_comments("Fuel inputs to heat were added to building energy use in regions where heat is not modeled as a final fuel") %>%
      add_legacy_name("L142.in_EJ_R_bld_F_Yh") %>%
      add_precursors("energy/A_regions", "energy/mappings/enduse_fuel_aggregation", "energy/mappings/enduse_sector_aggregation",
                     "L124.in_EJ_R_heat_F_Yh", "L131.in_EJ_R_Senduse_F_Yh", "L131.share_R_Senduse_heat_Yh") ->
      L142.in_EJ_R_bld_F_Yh

    return_data(L142.in_EJ_R_bld_F_Yh)
  } else {
    stop("Unknown command")
  }
}
