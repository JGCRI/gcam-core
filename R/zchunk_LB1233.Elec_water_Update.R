#' module_gcamusa_LB1233.Elec_water_Update
#'
#' Compute water withdrawals/consumption coefficients by state, fuel, technology, and cooling system type.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.wdraw_coef_R_elec_F_tech_Yh_ref},\code{L1233.wcons_coef_R_elec_F_tech_Yh_ref}
#' The corresponding file in the
#' original data system was \code{LB1233.Elec_water.R} (gcam-usa level1).
#' @details Compute water withdrawals/consumption coefficients by state, fuel, technology, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Zarrar Khan September 2018
module_gcamusa_LB1233.Elec_water_Update <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/elec_tech_water_map",
             FILE = "gcam-usa/UCS_tech_names",
             FILE = "gcam-usa/UCS_water_types",
             FILE = "gcam-usa/Macknick_elec_water_m3MWh",
             FILE = "gcam-usa/UCS_Database",
             "LA1233.CoolingSystemShares_RG3_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.wdraw_coef_R_elec_F_tech_Yh_ref",
             "L1233.wcons_coef_R_elec_F_tech_Yh_ref"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    state <- fuel <- plant_type <- technology <- water_withdrawals <- subsector <-
      water_consumption <- ':=' <- year <- x <- water_type <- State <- NEMS <-
      cooling_system <- withdraw <- cons <- consSum <- withdrawSum <- value <- NULL

    # ===================================================
    # Load required inputs
    elec_tech_water_map <- get_data(all_data, "gcam-usa/elec_tech_water_map")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    UCS_tech_names <- get_data(all_data, "gcam-usa/UCS_tech_names")
    UCS_water_types <- get_data(all_data, "gcam-usa/UCS_water_types")
    Macknick_elec_water_m3MWh <- get_data(all_data, "gcam-usa/Macknick_elec_water_m3MWh")
    UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database")
    LA1233.CoolingSystemShares_RG3_ref <- get_data(all_data, "LA1233.CoolingSystemShares_RG3_ref")

    # ===================================================
    # Perform Computations

    # Add missing years.
    LA1233.CSS_allyears <- LA1233.CoolingSystemShares_RG3_ref

    newyrs <- as.character(FUTURE_YEARS[!(FUTURE_YEARS %in% names(LA1233.CoolingSystemShares_RG3_ref))])
    LA1233.CSS_allyears[,newyrs] <- NA_real_

    # Interpolate for missing years using approx_fun
    LA1233.CSS_allyears %>%
      gather(key = year, value = value, -fuel, -plant_type, -water_type, -technology, -State, -NEMS, -cooling_system) %>%
      mutate(year = as.numeric(as.character(year))) %>%
      group_by(fuel, plant_type, water_type, technology, State, NEMS, cooling_system) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(water_type != "seawater") ->   # Remove Seawater since only want cooling coefficient for fresh water
      LA1233.CoolingSystemShares_RG3_future_ref

    # Calculate Water withdrawals and Consumption
    LA1233.CoolingSystemShares_RG3_future_ref %>%
      left_join((Macknick_elec_water_m3MWh %>%      # join with elec_tech_water_map to get plant_type (May not have complete matches so no ljenm)
                        left_join_error_no_match((elec_tech_water_map %>%
                                                    dplyr::select(technology, plant_type) %>%
                                                    unique),
                                                 by = c("technology")) %>%
                        dplyr::select(plant_type, cooling_system, fuel, water_withdrawals, water_consumption)),
                        by = c("plant_type", "fuel", "cooling_system")) %>% unique %>%
      mutate(withdraw = water_withdrawals*value*CONV_M3_BM3 / CONV_MWH_EJ,
             withdraw = case_when(is.na(withdraw)~0,TRUE~withdraw),
             cons = water_consumption*value*CONV_M3_BM3 / CONV_MWH_EJ,
             cons = case_when(is.na(cons)~0,TRUE~cons)) %>%
      group_by(year, State, fuel, plant_type, technology) %>%
      summarise(withdrawSum = sum(withdraw),
                consSum = sum(cons)) ->
      L1233.All_coef_R_elec_F_tech_Yh_ref_data


    # Water withdrawals in wide format
    L1233.All_coef_R_elec_F_tech_Yh_ref_data %>%
      dplyr::select(-consSum) %>%
      ungroup %>%
      spread(key = year, value = withdrawSum) %>%
      rename(state = State)->
    L1233.wdraw_coef_R_elec_F_tech_Yh_ref

    # Water consumption in wide format
    L1233.All_coef_R_elec_F_tech_Yh_ref_data %>%
      dplyr::select(-withdrawSum) %>%
      ungroup %>%
      spread(key = year, value = consSum) %>%
      rename(state = State)->
      L1233.wcons_coef_R_elec_F_tech_Yh_ref

    # ===================================================
    # Outputs

    L1233.wdraw_coef_R_elec_F_tech_Yh_ref %>%
      add_title("Water withdrawal coef for electricity generation by state by technology") %>%
      add_units("km3 (bm3) / EJ") %>%
      add_comments("Generated using zchunk_LB1233.Elec_water_Update") %>%
      add_legacy_name("L1233.wdraw_coef_R_elec_F_tech_Yh_ref") %>%
      add_precursors("LA1233.CoolingSystemShares_RG3_ref",
                     "gcam-usa/elec_tech_water_map",
                     "gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/Macknick_elec_water_m3MWh") ->
      L1233.wdraw_coef_R_elec_F_tech_Yh_ref


    L1233.wcons_coef_R_elec_F_tech_Yh_ref %>%
      add_title("Water consumption coef for electricity generation by state by technology") %>%
      add_units("km3 (bm3) / EJ") %>%
      add_comments("Generated using zchunk_LB1233.Elec_water_Update") %>%
      add_legacy_name("L1233.wcons_coef_R_elec_F_tech_Yh_ref") %>%
      add_precursors("LA1233.CoolingSystemShares_RG3_ref",
                     "gcam-usa/elec_tech_water_map",
                     "gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/Macknick_elec_water_m3MWh") ->
      L1233.wcons_coef_R_elec_F_tech_Yh_ref

    return_data(L1233.wdraw_coef_R_elec_F_tech_Yh_ref, L1233.wcons_coef_R_elec_F_tech_Yh_ref)
  } else {
    stop("Unknown command")
  }
}
