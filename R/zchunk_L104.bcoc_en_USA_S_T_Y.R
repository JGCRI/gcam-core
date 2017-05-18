#' module_emissions_L104.bcoc_en_USA_S_T_Y
#'
#' Creates USA BC and OC emission factors for combustion sectors
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L104.bcoc_tgej_USA_en_T_1990}. The corresponding file in the
#' original data system was \code{L104.bcoc_en_USA_S_T_Y.R} (emissions level1).
#' @details 1990 USA BC and OC emissions are divided by year 2000 USA energy use to generate emission factors for GCAM aggregate sectors
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author SJS May 2017
module_emissions_L104.bcoc_en_USA_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "emissions/mappings/Bond_bc.oc_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L101.in_EJ_R_en_Si_F_Yh",
             FILE = "emissions/Bond_BC_1990",
             FILE = "emissions/Bond_OC_1990"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L104.bcoc_tgej_USA_en_T_1990"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    Bond_bc.oc_tech <- get_data(all_data, "emissions/mappings/Bond_bc.oc_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh")
    Bond_BC_1990 <- get_data(all_data, "emissions/Bond_BC_1990")
    Bond_OC_1990 <- get_data(all_data, "emissions/Bond_OC_1990")

    # Select for USA data
    Bond_BC_1990 %>%
      select(Technology, USA) %>%
      rename(BC_em = USA) -> Bond_BC_USA_1990

    Bond_OC_1990 %>%
      select(Technology, USA) %>%
      rename(OC_em = USA)  -> Bond_OC_USA_1990

    # Combine BC & OC data and convert to Tg
    Bond_OC_USA_1990 %>%
      left_join_error_no_match(Bond_BC_USA_1990, by = "Technology") %>%
      mutate(OC_em = OC_em * CONV_GG_TG,
             BC_em = BC_em * CONV_GG_TG) ->
      Bond_BCOC_USA_1990

    # Aggregate by GCAM sector and technology
    Bond_BCOC_USA_1990 %>%
      rename(BCOC.Technology = Technology) %>%
      # Not all items in the BCOC input dataset have GCAM equivalents, so will have some NAs.
      # This is expected. Use left_join for this reason.
      left_join(Bond_bc.oc_tech,by = "BCOC.Technology") %>%
      group_by(sector, technology) %>%
      summarize(OC_em = sum(OC_em), BC_em = sum(BC_em)) %>%
      filter(!is.na(sector)) %>%
      rename(BCOC_agg_sector = sector) %>%
      ungroup() ->
      USA_bcoc_in_tg_1990

    # Extract year 2000 Energy data
    # See note below on why year 2000 is used
    L101.in_EJ_R_en_Si_F_Yh %>%
      select(GCAM_region_ID, sector, technology, fuel, `2000`) %>%
      rename(Fuel_Use = `2000`) %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      select(-GCAM_region_ID)  ->
      GCAM_USA_energy_2000

    # Extract mapping between GCAM and Bond data
    GCAM_sector_tech %>%
      select(sector, technology, fuel, BCOC_agg_sector, BCOC_agg_fuel) %>%
      filter(!is.na(BCOC_agg_sector)) -> # Where there is no correspondence remove those rows
      BCOC_GCAM_sector_tech

    # Aggregate GCAM energy consumption to Bond BCOC categories
    GCAM_USA_energy_2000 %>%
      # Not all GCAM sectors have energy inputs, so use left_join since not all elements will have a match
      left_join(BCOC_GCAM_sector_tech, by = c("sector", "technology", "fuel")) %>%
      # Where there is no correspondence remove those rows
      filter(!is.na(BCOC_agg_sector)) %>%
      # Now aggregate to BCOC aggregate sector/tech
      group_by(BCOC_agg_sector, technology) %>%
      summarize_if(is.numeric, sum) %>%
      ungroup() ->
      GCAM_energy_2000_byBCOC_agg

    # The old data system energy aggregation has several errors:
    # 1) Air and rail energy are far too high (likely included service in addition to energy)
    # 2) Road and ship energy consumption are zero.
    # In order to replicate old behavior, these erroneous values are inserted manually here
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      GCAM_energy_2000_byBCOC_agg %>%
        mutate(Fuel_Use = if_else(BCOC_agg_sector == "trn_rail" & technology == "refined liquids", 8.74835393158511, Fuel_Use)) %>%
        mutate(Fuel_Use = if_else(BCOC_agg_sector == "trn_air" & technology == "refined liquids", 17.3290691678305, Fuel_Use)) %>%
        mutate(Fuel_Use = if_else(BCOC_agg_sector == "trn_road" & technology == "refined liquids", 0.0, Fuel_Use)) %>%
        mutate(Fuel_Use = if_else(BCOC_agg_sector == "trn_ship" & technology == "refined liquids", 0.0, Fuel_Use)) ->
        GCAM_energy_2000_byBCOC_agg
    }

    # Other issues relating to energy data used here
    # A) Electricity natural gas consumption only includes steam/CT but not CC
    # consumption in both old and new versions. This is not a substantial issue here since BC/OC
    # from natural gas are very small, but could be for other emission species.
    #
    # B) Industry natural gas includes fertilizer. Not much of an issue for BC/OC. For
    # other emissions, however, the emission factor for fertilizer use vs general
    # combustion could be very different
    #
    # C) Not clear what to do for cement emissions. For BC/OC could be given same
    # EF as other industrial combustion sectors). These appear to not be included presently.


    # Create emission factors by dividing emissions by energy consumption
    # Note, the use of year 2000 data results in inconsistent emission factors. Year 2000
    # data is used primarily because the IEA data used here lacks residential building
    # biomass information in 1990. If this code continues to be used, it should be updated
    # to use consistent emissions and energy information

    # Join using BCOC emissions first, since these are the categories we want data for.
    # If there is no energy data, then want a zero emission factor for these categories
    USA_bcoc_in_tg_1990 %>%
      # Still don't expect a complete match, so use left_join
      left_join(GCAM_energy_2000_byBCOC_agg,by = c("BCOC_agg_sector","technology")) %>%
      mutate(bc_em_factor = BC_em / Fuel_Use) %>%
      mutate(oc_em_factor = OC_em / Fuel_Use) %>%
      select(-BC_em, -OC_em, -Fuel_Use) %>%
      rename(sector = BCOC_agg_sector) %>%
      replace_na(list(bc_em_factor = 0, oc_em_factor = 0)) %>%
      mutate_all(funs(replace(., is.infinite(.), 0))) ->
      USA_BCOC_Emission_Factors

    # ===================================================

    # Produce outputs
    USA_BCOC_Emission_Factors %>%
      add_title("USA BC and OC combustion sector emission factors") %>%
      add_units("Tg/EJ") %>%
      add_comments("1990 BC and OC emissions divided by year 2000 energy consumption") %>%
      add_legacy_name("L104.bcoc_tgej_USA_en_T_1990") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/mappings/IEA_flow_sector",
                     "energy/mappings/IEA_product_fuel",
                     "emissions/mappings/Bond_bc.oc_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "L101.in_EJ_R_en_Si_F_Yh",
                     "emissions/Bond_BC_1990",
                     "emissions/Bond_OC_1990") ->
      L104.bcoc_tgej_USA_en_T_1990

    return_data(L104.bcoc_tgej_USA_en_T_1990)
  } else {
    stop("Unknown command")
  }
}
