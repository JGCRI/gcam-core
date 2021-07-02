# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA102.en_emiss_CDIAC
#'
#' Calculates regional TPES as well as regional and global emissions coefficients by fuel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.CO2_Mt_R_F_Yh}, \code{L102.Ccoef_kgCGJ_R_F_Yh}, \code{L102.Ccoef_kgCGJ_F_Yh}. The corresponding file in the
#' original data system was \code{LA102.en_emiss_CDIAC.R} (energy level1).
#' @details This chunk maps the CDIAC country emissions database (L100.CDIAC_CO2_ctry_hist) onto GCAM
#' regions (iso_GCAM_regID) and fuels (CDIAC_fuel) to determine regional emissions by fuel (L102.CO2_Mt_R_F_Yh),
#' extracts non-energy emissions and uses the remainder to calculate regional (L102.Ccoef_kgCGJ_R_F_Yh) and
#' global (L102.Ccoef_kgCGJ_F_Yh) emissions coefficients
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by mutate select summarise
#' @author CWR July 2017

module_energy_LA102.en_emiss_CDIAC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/mappings/CDIAC_fuel",
             FILE = "energy/A32.nonenergy_Cseq",
             "L100.CDIAC_CO2_ctry_hist",
             "L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.CO2_Mt_R_F_Yh",
             "L102.Ccoef_kgCGJ_R_F_Yh",
             "L102.Ccoef_kgCGJ_F_Yh"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    GCAM_region_ID <- curr_table <- fuel <- iso <- remove.fraction <-
      sector <- val_energy <- val_non_energy <- value <- value.x <- value.y <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    CDIAC_fuel <- get_data(all_data, "emissions/mappings/CDIAC_fuel")
    A32.nonenergy_Cseq <- get_data(all_data, "energy/A32.nonenergy_Cseq")
    L100.CDIAC_CO2_ctry_hist <- get_data(all_data, "L100.CDIAC_CO2_ctry_hist")
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh")


    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...

    # 2. Perform computations
    # prep region IDs for matching
    iso_GCAM_regID %>%
      select(iso, GCAM_region_ID) ->
      iso_GCAM_regID_reg

    # append GCAM region IDs to isos and map GCAM fuels to CDIAC fuels
    L100.CDIAC_CO2_ctry_hist %>%
      gather(fuel, value, -iso, -year) %>%
      left_join_error_no_match(iso_GCAM_regID_reg, by = "iso") %>%
      rename(CDIAC_fuel = fuel) %>%
      left_join_error_no_match(CDIAC_fuel, by = "CDIAC_fuel") %>%

    # Aggregate CO2 emissions by GCAM region and fuel
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value) * CONV_KT_MT) %>%
      ungroup() ->
      L102.CO2_Mt_R_F_Yh

    # Calculate regional and global CO2 emissions coefficients by fuel
    # Calculate the TPES by fuel, deducting non-energy use of fuels that does not result in CO2 emissions

    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "in_industry_feedstocks",
             fuel %in% L102.CO2_Mt_R_F_Yh$fuel) %>%
      left_join_error_no_match(A32.nonenergy_Cseq, by = c("fuel" = "subsector")) %>%
      mutate(val_non_energy = value * remove.fraction) %>%
      select(GCAM_region_ID, fuel, year, val_non_energy) ->
      L102.en_sequestered_EJ_R_Fi_Yh

    # subtracts the non-energy use of fuels (sequestered carbon) from the TPES to get only the emitting energy
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES", fuel %in% L102.CO2_Mt_R_F_Yh$fuel) %>%
      left_join_error_no_match(L102.en_sequestered_EJ_R_Fi_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(val_energy = value - val_non_energy) %>%
      select(-sector, -val_non_energy, -value) ->
      L102.en_emitted_EJ_R_Fi_Yh

    # Default emissions coefficients to replace NA values where below energy consumption is 0 (kgC/GJ)
    DEFAULT_GAS_CCOEF <- 14.2
    DEFAULT_COAL_CCOEF <- 27.3
    DEFAULT_LIQUIDS_CCOEF <- 19.6

    L102.CO2_Mt_R_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS) -> L102.CO2_Mt_R_F_Yh

    # Calculate the regional emissions coefficients by fuel, using only the energy whose carbon is assumed to be emitted
    L102.CO2_Mt_R_F_Yh %>%
      filter(fuel %in% L102.en_emitted_EJ_R_Fi_Yh$fuel) %>%
      left_join_error_no_match(L102.en_emitted_EJ_R_Fi_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value / val_energy) %>%
      select(-val_energy) %>%

    # reset to defaults wherever NAs result from 0 energy consumption
      mutate(value = if_else(fuel == "gas" & is.na(value), DEFAULT_GAS_CCOEF, value),
             value = if_else(fuel == "coal" & is.na(value), DEFAULT_COAL_CCOEF, value),
             value = if_else(fuel == "refined liquids" & is.na(value), DEFAULT_LIQUIDS_CCOEF, value)) ->
      L102.Ccoef_kgCGJ_R_F_Yh

    # aggregate regional values to global and calculate global coefficients
    L102.en_emitted_EJ_R_Fi_Yh %>%
      group_by(fuel, year) %>%
      summarise(value = sum(val_energy)) %>%
      ungroup() ->
      L102.en_emitted_EJ_Fi_Yh

    # global coefficient (value) calculated by emissions / energy (value.x / value.y)
    L102.CO2_Mt_R_F_Yh %>%
      group_by(fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(fuel %in% L102.en_emitted_EJ_Fi_Yh$fuel) %>%
      left_join_error_no_match(L102.en_emitted_EJ_Fi_Yh, by = c("fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) ->
      L102.Ccoef_kgCGJ_F_Yh

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L102.CO2_Mt_R_F_Yh %>%
      add_title("CO2 emissions by GCAM region / fuel type / historical year") %>%
      add_units("MtC") %>%
      add_comments("Aggregated from CDIAC country emissions database") %>%
      add_legacy_name("L102.CO2_Mt_R_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel") ->
      L102.CO2_Mt_R_F_Yh

    L102.Ccoef_kgCGJ_R_F_Yh %>%
      add_title("Historic regional carbon coefficients by fuel by GCAM ") %>%
      add_units("kgC/GJ") %>%
      add_comments("ratio of CDIAC carbon emissions to energy consumption") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_R_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/A32.nonenergy_Cseq")  ->
      L102.Ccoef_kgCGJ_R_F_Yh

    L102.Ccoef_kgCGJ_F_Yh %>%
      add_title("Historic global carbon coefficients by fuel") %>%
      add_units("kgC/GJ") %>%
      add_comments("aggregated regional data for CDIAC carbon emissions and energy balances to find global ratios") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/A32.nonenergy_Cseq") ->
      L102.Ccoef_kgCGJ_F_Yh

    return_data(L102.CO2_Mt_R_F_Yh, L102.Ccoef_kgCGJ_R_F_Yh, L102.Ccoef_kgCGJ_F_Yh)
  } else {
    stop("Unknown command")
  }
}
