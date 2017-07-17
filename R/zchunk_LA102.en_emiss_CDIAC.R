#' module_energy_LA102.en_emiss_CDIAC
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.CO2_Mt_R_F_Yh}, \code{L102.Ccoef_kgCGJ_R_F_Yh}, \code{L102.Ccoef_kgCGJ_F_Yh}. The corresponding file in the
#' original data system was \code{LA102.en_emiss_CDIAC.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA102.en_emiss_CDIAC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/mappings/CDIAC_fuel",
             FILE = "energy/A32.nonenergy_Cseq",
             "L100.CDIAC_CO2_ctry_hist",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.CO2_Mt_R_F_Yh",
             "L102.Ccoef_kgCGJ_R_F_Yh",
             "L102.Ccoef_kgCGJ_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    CDIAC_fuel <- get_data(all_data, "emissions/mappings/CDIAC_fuel")
    A32.nonenergy_Cseq <- get_data(all_data, "energy/A32.nonenergy_Cseq")
    L100.CDIAC_CO2_ctry_hist <- get_data(all_data, "L100.CDIAC_CO2_ctry_hist")
    get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5))) ->   # change Xyear to year
      L1011.en_bal_EJ_R_Si_Fi_Yh

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...

    # 2. Perform computations
    # prep region IDs for matching
    iso_GCAM_regID %>%
    select(iso, GCAM_region_ID) -> iso_GCAM_regID_reg

    # append GCAM region IDs to isos and map GCAM fuels to CDIAC fuels
    L100.CDIAC_CO2_ctry_hist %>%
    gather(fuel, value, -iso, -year) %>%
    left_join_error_no_match(iso_GCAM_regID_reg) %>%
    rename(CDIAC_fuel = `fuel`) %>%
    left_join_error_no_match(CDIAC_fuel) -> L102.CDIAC_CO2_ctry_hist_matched

    # Aggregate CO2 emissions by GCAM region and fuel
    L102.CDIAC_CO2_ctry_hist_matched %>%
    group_by(GCAM_region_ID, fuel, year) %>%
    summarise(value = sum(value) * CONV_KT_MT) -> L102.CO2_Mt_R_F_Yh

    # Calculate regional and global CO2 emissions coefficients by fuel
    # Calculate the TPES by fuel, deducting non-energy use of fuels that does not result in CO2 emissions

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
    filter(sector == "in_industry_feedstocks" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel) %>%
    left_join_error_no_match(A32.nonenergy_Cseq, by = c("fuel" = "subsector")) %>%
    mutate(val = value * remove.fraction) %>%
    select(GCAM_region_ID, fuel, year, val) -> L102.en_sequestered_EJ_R_Fi_Yh

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
    filter(sector == "TPES" & fuel %in% L102.CO2_Mt_R_F_Yh$fuel) %>%
    left_join_error_no_match(L102.en_sequestered_EJ_R_Fi_Yh) %>%
    mutate(val = value - val) %>%
    select(-sector, -value)  -> L102.en_emitted_EJ_R_Fi_Yh

    # Calculate the emissions coefficients by fuel, using only the energy whose carbon is assumed to be emitted
    # regional
    L102.CO2_Mt_R_F_Yh %>%
    filter(fuel %in% L102.en_emitted_EJ_R_Fi_Yh$fuel) %>%
    left_join_error_no_match(L102.en_emitted_EJ_R_Fi_Yh) %>%
    mutate(value = value / val) %>%
    select(-val) %>%

    # reset to defaults wherever NAs result from 0 energy consumption
    mutate(value = if_else(fuel == "gas" & is.na(value), energy.DEFAULT_GAS_CCOEF, value)) %>%
    mutate(value = if_else(fuel == "coal" & is.na(value), energy.DEFAULT_COAL_CCOEF, value)) %>%
    mutate(value = if_else(fuel == "refined liquids" & is.na(value), energy.DEFAULT_LIQUIDS_CCOEF, value)) -> L102.Ccoef_kgCGJ_R_F_Yh

    # aggregate regional values to global and calculate global coefficients
    L102.en_emitted_EJ_R_Fi_Yh %>%
    group_by(fuel, year) %>%
    summarise(value = sum(val)) -> L102.en_emitted_EJ_Fi_Yh

    L102.CO2_Mt_R_F_Yh %>%
    group_by(fuel, year) %>%
    summarise(value = sum(value)) %>%
    filter(fuel %in% L102.en_emitted_EJ_Fi_Yh$fuel) %>%
    left_join_error_no_match(L102.en_emitted_EJ_Fi_Yh, by = c("fuel", "year")) %>%
    mutate(value = value.x / value.y) %>%
    select(-value.x, -value.y) -> L102.Ccoef_kgCGJ_F_Yh

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L102.CO2_Mt_R_F_Yh %>%
      add_title("Historic regional carbon emissions by fuel") %>%
      add_units("MtC") %>%
      add_comments("Aggregated from CDIAC country emissions database") %>%
      add_legacy_name("L102.CO2_Mt_R_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.CO2_Mt_R_F_Yh

    L102.Ccoef_kgCGJ_R_F_Yh %>%
      add_title("Historic regional carbon coefficients by fuel") %>%
      add_units("kgC/GJ") %>%
      add_comments("ratio of CDIAC carbon emissions to energy consumption") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_R_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel", "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A32.nonenergy_Cseq") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.Ccoef_kgCGJ_R_F_Yh

    L102.Ccoef_kgCGJ_F_Yh %>%
      add_title("Historic global carbon coefficients by fuel") %>%
      add_units("kgC/GJ") %>%
      add_comments("aggregated regional data for CDIAC carbon emissions and energy balances to find global ratios") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "common/iso_GCAM_regID", "emissions/mappings/CDIAC_fuel", "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A32.nonenergy_Cseq") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.Ccoef_kgCGJ_F_Yh

    return_data(L102.CO2_Mt_R_F_Yh, L102.Ccoef_kgCGJ_R_F_Yh, L102.Ccoef_kgCGJ_F_Yh)
  } else {
    stop("Unknown command")
  }
}
