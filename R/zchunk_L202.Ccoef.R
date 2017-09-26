#' module_energy_L202.Ccoef
#'
#' This chunk creates the file of CO2 coefficients (carbon emissions/GJ energy consumed) for primary energy sources.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.CarbonCoef}. The corresponding file in the
#' original data system was \code{L202.Ccoef.R} (energy level2).
#' @details Based on user-defined switches (in constants.R), this code produces the CO2 coefficients for primary energy sources
#' # based on 1. GCAM 3.0 assumptions 2. global average emissions coefficients based on CDIAC inventory and IEA energy balances
#' # or 3. region-specific average emissions coefficients based on CDIAC inventory and IEA energy balances.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author JDH Sept 2017

module_energy_L202.Ccoef <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "emissions/mappings/fuel_to_Ccoef",
             "L102.Ccoef_kgCGJ_F_Yh",
             "L102.Ccoef_kgCGJ_R_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.CarbonCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    fuel_to_Ccoef <- get_data(all_data, "emissions/mappings/fuel_to_Ccoef")
    L102.Ccoef_kgCGJ_F_Yh <- get_data(all_data, "L102.Ccoef_kgCGJ_F_Yh")
    L102.Ccoef_kgCGJ_R_F_Yh <- get_data(all_data, "L102.Ccoef_kgCGJ_R_F_Yh")

    # ===================================================
    # Carbon contents of fuels in all regions
    A_PrimaryFuelCCoef %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["CarbonCoef"]]), GCAM_region_names = GCAM_region_names, has_traded = TRUE) -> L202.CarbonCoef


    # For regions with no agricultural and land use sector (Taiwan),
    # need to remove the passthrough supplysectors for first-gen biofuels
    ag_en <- c("regional corn for ethanol", "regional sugar for ethanol", "regional biomassOil")
    L202.CarbonCoef %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS | !PrimaryFuelCO2Coef.name %in% ag_en) -> L202.CarbonCoef

    # if emissions.USE_GCAM3_CCOEFS = 1, using exogenous C coefs from GCAM 3.0
    if(emissions.USE_GCAM3_CCOEFS == 1) {
      L202.CarbonCoef <- L202.CarbonCoef
    }
    else if(emissions.USE_GCAM3_CCOEFS == 0) {
      L202.CarbonCoef %>%
        left_join(fuel_to_Ccoef, by = "PrimaryFuelCO2Coef.name") -> L202.CarbonCoef
      if(emissions.USE_GLOBAL_CCOEFS == 1) {
        # Using global average emissions coefficients based on CDIAC inventory and IEA energy balances
        L102.Ccoef_kgCGJ_F_Yh %>%
          filter(year %in% HISTORICAL_YEARS) %>%
          filter(year == emissions.INVENTORY_MATCH_YEAR) -> L202.Ccoef_kgCGJ_F_fby

        L202.CarbonCoef %>%
          left_join(L202.Ccoef_kgCGJ_F_fby, by = "fuel") %>%
          mutate(PrimaryFuelCO2Coef = if_else(!is.na(fuel), round(value, emissions.DIGITS_CO2COEF), PrimaryFuelCO2Coef)) %>%
          select(one_of(LEVEL2_DATA_NAMES[["CarbonCoef"]])) -> L202.CarbonCoef
      }
      else if(emissions.USE_GLOBAL_CCOEFS == 0) {
        # Using region-specific average emissions coefficients based on CDIAC inventory and IEA energy balances
        L102.Ccoef_kgCGJ_R_F_Yh %>%
          filter(year %in% HISTORICAL_YEARS) %>%
          filter(year == emissions.INVENTORY_MATCH_YEAR) %>%
          left_join(GCAM_region_names, by = "GCAM_region_ID") -> L202.Ccoef_kgCGJ_R_F_fby

        L202.CarbonCoef %>%
          left_join(L202.Ccoef_kgCGJ_R_F_fby, by = c("fuel", "region")) %>%
          mutate(PrimaryFuelCO2Coef = if_else(!is.na(fuel), round(value, emissions.DIGITS_CO2COEF), PrimaryFuelCO2Coef)) %>%
          select(one_of(LEVEL2_DATA_NAMES[["CarbonCoef"]])) -> L202.CarbonCoef
      }
      else stop("Unknown emissions.USE_GLOBAL_CCOEFS value ", emissions.USE_GLOBAL_CCOEFS)
    }
    else stop("Unknown emissions.USE_GCAM3_CCOEFS value ", emissions.USE_GCAM3_CCOEFS)



    # ===================================================

    # Produce outputs

    L202.CarbonCoef %>%
      add_title("Primary Energy CO2 Coefficient") %>%
      add_units("kgC/GJ") %>%
      add_comments("Data can be generated in 3 ways. 1. Using GCAM 3.0 assumptions 2. Using global average emissions") %>%
      add_comments("coefficients based on CDIAC inventory and IEA energy balances or 3. Using region-specific average emissions coefficients based on CDIAC inventory and IEA energy balances") %>%
      add_legacy_name("L202.CarbonCoef") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_PrimaryFuelCCoef", "emissions/mappings/fuel_to_Ccoef", "L102.Ccoef_kgCGJ_F_Yh", "L102.Ccoef_kgCGJ_R_F_Yh") ->
      L202.CarbonCoef

    return_data(L202.CarbonCoef)
  } else {
    stop("Unknown command")
  }
}
