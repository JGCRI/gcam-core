#' module_energy_LA1321.cement
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_R_cement_Yh}, \code{L1321.IO_GJkg_R_cement_F_Yh}, \code{L1321.in_EJ_R_cement_F_Y}, \code{L1321.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' original data system was \code{LA1321.cement.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CWR Nov 2017
#' @export
module_energy_LA1321.cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/mappings/cement_regions",
             FILE = "energy/Worrell_1994_cement",
             FILE = "energy/IEA_cement_elec_kwht",
             FILE = "energy/IEA_cement_TPE_GJt",
             FILE = "energy/IEA_cement_fuelshares",
             "L100.CDIAC_CO2_ctry_hist",
             "L102.CO2_Mt_R_F_Yh",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L132.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y",
             "L1321.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    cement_regions <- get_data(all_data, "energy/mappings/cement_regions")
    Worrell_1994_cement <- get_data(all_data, "energy/Worrell_1994_cement")
    IEA_cement_elec_kwht <- get_data(all_data, "energy/IEA_cement_elec_kwht")
    IEA_cement_TPE_GJt <- get_data(all_data, "energy/IEA_cement_TPE_GJt")
    IEA_cement_fuelshares <- get_data(all_data, "energy/IEA_cement_fuelshares")
    L100.CDIAC_CO2_ctry_hist <- get_data(all_data, "L100.CDIAC_CO2_ctry_hist")
    L102.CO2_Mt_R_F_Yh <- get_data(all_data, "L102.CO2_Mt_R_F_Yh")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L132.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L132.in_EJ_R_indenergy_F_Yh")
browser()
    # ===================================================

    LIMESTONE_CCOEF <- A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "limestone"]

    # If the CO2 emissions inventories do not go to the latest historical time period, copy the last available year
    additional_years <- HISTORICAL_YEARS[!HISTORICAL_YEARS %in% CO2_historical_years]
    final_CO2_year <- CO2_historical_years[length(CO2_historical_years)]


    # Downscale Worrell dataset to country level using CDIAC emissions inventory
    # Part 1: Derivation of cement production and limestone consumption by region and historical year
    # Downscale Worrell's process CO2 emissions and cement production in 1994 to all countries
    cement_regions %>%
      left_join(L100.CDIAC_CO2_ctry_hist, by = "iso") %>%
      filter(year == 1994) %>%
      select(iso, Worrell_region, year, cement) %>%
      na.omit() %>%
      rename(process_emissions_ktC = cement) ->
      L1321.Cement_Worrell_ctry

    L1321.Cement_Worrell_ctry %>%
      group_by(Worrell_region) %>%
      summarise(reg_process_emissions = sum(process_emissions_ktC)) %>%
      ungroup() ->
      L1321.Cement_Worrell_reg

    # Select country-level emissions to prepare for scaling
    Worrell_1994_cement %>%
      select(Country, cement_prod_Mt, process_emissions_MtC) ->
      Worrell_1994_cement_slim

    # Compute country-level shares of regional emissions and multiply the region-level process emissions
    # and cement production by these shares to get the country-level estimates
    L1321.Cement_Worrell_ctry %>%
      left_join(L1321.Cement_Worrell_reg, by = "Worrell_region") %>%
      mutate(share = process_emissions_ktC / reg_process_emissions) %>%
      left_join_error_no_match(Worrell_1994_cement_slim, by = c("Worrell_region" = "Country")) %>%
      mutate(cement_prod_Mt = cement_prod_Mt * share, process_emissions_MtC = process_emissions_MtC * share) %>%

      # Now match and aggregate Worrell's process CO2 emissions and cement production in 1994 by GCAM region and compute the emissions ratio
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(-country_name, -region_GCAM3) %>%
      group_by(GCAM_region_ID) %>%
      summarise(cement_prod_Mt = sum(cement_prod_Mt), process_emissions_MtC = sum(process_emissions_MtC)) %>%
      mutate(prod_emiss_ratio = cement_prod_Mt / process_emissions_MtC) ->
      L1321.Cement_Worrell_R

    # Calculate cement production over time, assuming that this ratio is constant over time for each region


    #
    L102.CO2_Mt_R_F_Yh %>%
      filter(fuel == "limestone") ->
      L1321.CO2_Mt_R_F_Yh_base
    # NOTE: this is slightly less robust, it works for a single missing historical year as opposed to a set of them
    L1321.CO2_Mt_R_F_Yh_base %>%
      filter(year == final_CO2_year) %>%
      mutate(year = additional_years)  %>%
      bind_rows(L1321.CO2_Mt_R_F_Yh_base) ->
      L1321.CO2_Mt_R_F_Yh

    # Calculate cement production over time by multiplying production emissions ratio by emissions
    # assuming that this ratio is constant over time for each region
    L1321.Cement_Worrell_R %>%
      mutate(sector = "cement") %>%
      left_join(L1321.CO2_Mt_R_F_Yh, by = "GCAM_region_ID") %>%
      mutate(value = prod_emiss_ratio * value) ->
      L1321.out_Mt_R_cement_Yh

    # Use the assumed limestone fuel carbon content (same in all regions) to calculate the limestone consumption
    # and limestone IO coefficients in each region
    L1321.CO2_Mt_R_F_Yh %>%
      mutate(sector = "cement", in.value = value / LIMESTONE_CCOEF) %>%
      select(-value) ->
      L1321.in_Cement_Mt_R_limestone_Yh

    # Calculate input-output coefficients
    L1321.in_Cement_Mt_R_limestone_Yh %>%
      left_join_error_no_match(L1321.out_Mt_R_cement_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = in.value / value) ->
      L1321.IO_Cement_R_limestone_Yh

    # Derivation of energy inputs to cement production by region and historical year
    # Interpolate available data on electricity intensity to all historical years
    IEA_cement_elec_kwht %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) * CONV_KWH_GJ / CONV_T_KG) %>%
      ungroup() ->
      L1321.IEA_cement_elec_intensity

    # Interpolate available data on total primary energy intensity to all historical years by region
    IEA_cement_TPE_GJt %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) / CONV_T_KG) %>%
      ungroup() ->
      L1321.IEA_cement_TPE_intensity

    # Calculate average electric and TPE intensity for each GCAM region (use process emissions as a weighting factor)
    L100.CDIAC_CO2_ctry_hist %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(iso, year, cement) %>%
      rename(emiss_ktC = cement) %>%
      # Match in region IDs by iso code and drop extra columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(-country_name, -region_GCAM3) %>%
      # Replace process emissions with actual cement production
      left_join_error_no_match(L1321.Cement_Worrell_R)
      mutate(prod_Mt = emiss_ktC * prod_emiss_ratio ->
      L1321.Cement_ALL_ctry_Yh
# ===================================================OLD
# 2. Perform computations


     L1321.Cement_ALL_ctry_Yh$prod_Mt <- L1321.Cement_ALL_ctry_Yh$emiss_ktC * L1321.Cement_Worrell_R$prod_emiss_ratio[
      match( L1321.Cement_ALL_ctry_Yh$GCAM_region_ID, L1321.Cement_Worrell_R$GCAM_region_ID ) ] *
      conv_kt_Mt
    L1321.Cement_ALL_ctry_Yh$IEA_intensity_region <- cement_regions$IEA_intensity_region[ match( L1321.Cement_ALL_ctry_Yh$iso, cement_regions$iso ) ]
    L1321.Cement_ALL_ctry_Yh$elec_GJkg <- L1321.IEA_cement_elec_intensity$elec_GJkg[
      match( vecpaste( L1321.Cement_ALL_ctry_Yh[ c( "IEA_intensity_region", "year" ) ] ),
             vecpaste( L1321.IEA_cement_elec_intensity[ c( "Country", "year" ) ] ) ) ]
    L1321.Cement_ALL_ctry_Yh$TPE_GJkg <- L1321.IEA_cement_TPE_intensity$TPE_GJkg[
      match( vecpaste( L1321.Cement_ALL_ctry_Yh[ c( "IEA_intensity_region", "year" ) ] ),
             vecpaste( L1321.IEA_cement_TPE_intensity[ c( "Country", "year" ) ] ) ) ]

    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.out_Mt_R_cement_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.out_Mt_R_cement_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.IO_GJkg_R_cement_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.in_EJ_R_cement_F_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.in_EJ_R_cement_F_Y

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.in_EJ_R_indenergy_F_Yh

    return_data(L1321.out_Mt_R_cement_Yh, L1321.IO_GJkg_R_cement_F_Yh, L1321.in_EJ_R_cement_F_Y, L1321.in_EJ_R_indenergy_F_Yh)
  } else {
    stop("Unknown command")
  }
}
