#' module_energy_LA124.heat
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.in_EJ_R_heat_F_Yh}, \code{L124.out_EJ_R_heat_F_Yh}, \code{L124.out_EJ_R_heatfromelec_F_Yh}, \code{L124.heatoutratio_R_elec_F_tech_Yh}. The corresponding file in the
#' original data system was \code{LA124.heat.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author JDH April 2017
#' @export
module_energy_LA124.heat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/A24.globaltech_coef",
             FILE = "energy/calibrated_techs",
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L124.in_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heatfromelec_F_Yh",
             "L124.heatoutratio_R_elec_F_tech_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    A24.globaltech_coef <- get_data(all_data, "energy/A24.globaltech_coef")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh")

    # ===================================================
    # Heat regions
    heat_regionIDs <- A_regions$GCAM_region_ID[ A_regions$heat == 1]

    # Fuel inputs to district heat
    # Process fuel inputs in all regions; some will have the energy assigned to bld/ind, and others have a district heat sector
    L1011.en_bal_EJ_R_Si_Fi_Yh  %>%
      filter(sector == "in_heat") %>%
      mutate(sector = sub("in_", "", sector)) %>%
      left_join(enduse_fuel_aggregation, by="fuel") %>%
      select(-fuel, -electricity, -bld, -industry, -trn) %>%
      rename(fuel=heat) %>%
      gather(key=year, value=value, -fuel, -sector, -GCAM_region_ID) %>%
      group_by(fuel, sector, GCAM_region_ID, year) %>%
      summarise(value=sum(value)) -> L124.in_EJ_R_heat_F_Yh

    # Heat production from district heat sector
    # Create empty table of historical years to fill
    A24.globaltech_coef[,as.character(HISTORICAL_YEARS[-1])] <- NA

    # Interpolate to fill in missing globaltech_coef historical years
    A24.globaltech_coef %>%
      gather(year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      mutate(year = as.integer(year))  %>%
      arrange(year) %>%
      group_by(technology, subsector, supplysector, minicam.energy.input)  %>%
      mutate(value = approx_fun(year, value)) %>%
      left_join(distinct(calibrated_techs)) %>%
      select(-calibration, -secondary.output) -> L124.globaltech_coef

    # Heat output: fuel inputs to heat divided by exogenous input-output coefficients
    L124.in_EJ_R_heat_F_Yh %>%
      left_join(L124.globaltech_coef %>%
                  rename(temp=value) %>%
                  mutate(year=paste0("X", as.character(year))),
                      by=c("sector", "fuel", "year")) %>%
      mutate(value=value/temp)%>%
      select(-temp) %>%
      filter(GCAM_region_ID %in% heat_regionIDs)-> L124.out_EJ_R_heat_F_Yh #  Output of district heat only applies to regions where this is modeled as a separate fuel. Drop all others

    # Secondary output of heat from main activity CHP plants
    # Only do this for regions where district heat is being modeled
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_electricity_heat" , GCAM_region_ID %in% heat_regionIDs) %>%
      mutate(sector = sub("out_", "", sector)) %>%
      left_join(enduse_fuel_aggregation, by="fuel") %>%
      select(-fuel, -electricity, -bld, -industry, -trn) %>%
      rename(fuel=heat) %>%
      gather(key=year, value=value, -fuel, -sector, -GCAM_region_ID) %>%
      group_by(fuel, sector, GCAM_region_ID, year) %>%
      summarise(value=sum(value)) -> L124.out_EJ_R_heatfromelec_F_Yh

 #Only do this for regions where district heat is being modeled, and for technologies where heat output is modeled
    #Renaming value col for join in next step PUT THIS INSIDE JOIN STEP
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      filter(GCAM_region_ID %in% heat_regionIDs) %>%
      filter(technology %in% calibrated_techs$technology[calibrated_techs$secondary.output == "heat"]) %>%
      gather(key=year, value=value, -fuel, -sector, -GCAM_region_ID, -technology) %>%
      left_join(L124.out_EJ_R_heatfromelec_F_Yh %>%
                  rename(value_elec=value) %>%
                  rename(temp=sector), by=c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value=value_elec/value) %>%
      select(-value_elec, -temp)-> L124.heatoutratio_R_elec_F_tech_Yh

    #Reset missing and infinite values (applicable for CC in the base years) to 0
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      mutate(value=if_else(is.na(value)|is.infinite(value), 0, value))-> L124.heatoutratio_R_elec_F_tech_Yh

    #Drop all rows where all years are 0
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      group_by(GCAM_region_ID, technology, sector, fuel) %>%
      summarise(sum=sum(value)) %>%
      filter(sum==0) -> all_years_0

    L124.heatoutratio_R_elec_F_tech_Yh %>%
      left_join(all_years_0) %>%
      mutate(sum=if_else(is.na(sum), 2, sum)) %>% #2 was a random, nonzero choice
      filter(sum!=0)  %>%
      select(-sum) -> L124.heatoutratio_R_elec_F_tech_Yh

    #L124.heatoutratio_R_elec_F_tech_Yh %>%
     # spread(year, value) -> temp

    # This is complicated. Some historical regions / years have 0 district heat output (all fuels). This is fine unless there is heat output from
    # IEA's MAINCHP flow, which we model in the electric sector with a secondary output of heat to the district heat sector. In GCAM this will
    # cause solution failure as the district heat supply will be 0. In order to avoid this outcome we need a nominal amount of heat produced
    # in the zero years. The code below sets an exogenous year as the one to use to extract fuel shares for each region. In all 0 region / years,
    # output is equal to the output in the fuel-share year times 1e-3 to avoid throwing off the energy balances.
    fuel_share_year <- "X2010"
    L124.in_EJ_R_heat_F_Yh %>%
      group_by(sector, GCAM_region_ID, year) %>%
      summarise(value=sum(value)) %>%
      mutate(value=if_else(value==0, 1e-3, value)) %>%
      mutate(value=if_else(value!=1e-3, 0, value)) -> L124.mult_R_heat_Yh

    L124.out_EJ_R_heatfromelec_F_Yh %>%
      group_by(sector, GCAM_region_ID, year) %>%
      summarise(elec_heat=sum(value)) %>%
      mutate(elec_heat=if_else(elec_heat!=0, 1, elec_heat))-> L124.mult_R_heatfromelec_Yh

    L124.mult_R_heat_Yh %>%
      left_join(L124.mult_R_heatfromelec_Yh %>%
                  rename(temp=sector),
                  by=c("year", "GCAM_region_ID")) %>%
      mutate(value=value*elec_heat) %>%
      select(-temp, -elec_heat) %>%
      mutate(value=if_else(is.na(value), 0, value))-> L124.mult_R_heat_Yh

    L124.in_EJ_R_heat_F_Yh %>%
      filter(year==fuel_share_year) %>%
      select(-year) %>%
      rename(fsy_val=value)-> fsy

    L124.in_EJ_R_heat_F_Yh %>%
      left_join(fsy, by= c("fuel", "sector", "GCAM_region_ID")) %>%
      left_join(L124.mult_R_heat_Yh %>%
                  rename(heat_val=value), by=c("sector", "GCAM_region_ID", "year")) %>%
      mutate(value=value+(fsy_val*heat_val)) %>%
      select(-fsy_val, -heat_val) -> L124.in_EJ_R_heat_F_Yh

    L124.out_EJ_R_heat_F_Yh %>%
      left_join(fsy, by= c("fuel", "sector", "GCAM_region_ID")) %>%
      left_join(L124.mult_R_heat_Yh %>%
                  rename(heat_val=value), by=c("sector", "GCAM_region_ID", "year")) %>%
      mutate(value=value+(fsy_val*heat_val)) %>%
      select(-fsy_val, -heat_val) -> L124.out_EJ_R_heat_F_Yh


    # ===================================================

    # Produce outputs
    L124.in_EJ_R_heat_F_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.in_EJ_R_heat_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/enduse_fuel_aggregation") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM) ->
      L124.in_EJ_R_heat_F_Yh
    L124.out_EJ_R_heat_F_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.out_EJ_R_heat_F_Yh") %>%
      add_precursors("energy/A24.globaltech_coef", "energy/calibrated_techs") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM) ->
      L124.out_EJ_R_heat_F_Yh
    L124.out_EJ_R_heatfromelec_F_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.out_EJ_R_heatfromelec_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/enduse_fuel_aggregation") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM) ->
      L124.out_EJ_R_heatfromelec_F_Yh
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.heatoutratio_R_elec_F_tech_Yh") %>%
      add_precursors("temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh", "energy/calibrated_techs") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM) ->
      L124.heatoutratio_R_elec_F_tech_Yh

    return_data(L124.in_EJ_R_heat_F_Yh, L124.out_EJ_R_heat_F_Yh, L124.out_EJ_R_heatfromelec_F_Yh, L124.heatoutratio_R_elec_F_tech_Yh)
  } else {
    stop("Unknown command")
  }
}
