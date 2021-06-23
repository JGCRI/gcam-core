# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA131.enduse_noEFW_USA
#'
#' Generate the following two tables:
#' \itemize{
#'  \item{Final scaled energy input by USA / end-use sector (incl CHP) / fuel / historical year (no EFW)}
#' }
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.in_EJ_USA_Senduse_F_Yh_noEFW}.
#' @details This chunk performs the same steps as module_energy_LA131.enduse, but works from the energy balances prior to the
#' energy-for-water deductions, in order to generate information for GCAM-USA which does not have energy-for-water
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by mutate select summarise
#' @importFrom tidyr replace_na
#' @author GPK November 2020
module_gcamusa_LA131.enduse_noEFW_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/enduse_sector_aggregation",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L121.in_EJ_R_unoil_F_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L126.out_EJ_R_electd_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.in_EJ_USA_Senduse_F_Yh_noEFW"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <- value.y <- value.x <-
        has_district_heat <- . <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    enduse_sector_aggregation <- get_data(all_data, "energy/mappings/enduse_sector_aggregation")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)


     get_data(all_data, "L121.in_EJ_R_unoil_F_Yh") %>%
      filter(year %in% HISTORICAL_YEARS) ->   # ensure temp data match our current history
      L121.in_EJ_R_unoil_F_Yh

    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh")
    L126.out_EJ_R_electd_F_Yh <- get_data(all_data, "L126.out_EJ_R_electd_F_Yh")

    # ===================================================
    # ELECTRICITY SCALING
    # First, subset and aggregate the "upstream" electricity demands by the energy system that are not being scaled
    L121.in_EJ_R_unoil_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             fuel == "electricity") ->
      Unoil_elect_USA

    L122.in_EJ_R_refining_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             fuel == "electricity", year %in% HISTORICAL_YEARS) %>%
      bind_rows(Unoil_elect_USA) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Unoil_Refin_elect_USA

    # Subtract this from total delivered electricity (output of t&d sector). This is the amount that is available for scaling to end uses.
    Unoil_Refin_elect_USA %>%
      left_join_error_no_match(L126.out_EJ_R_electd_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.y - value.x) %>%
      select(-sector, -value.x, -value.y) ->
      Enduse_elect_USA

    # Subset the end use sectors and aggregate by fuel
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             sector %in% enduse_sector_aggregation$sector,
             year %in% HISTORICAL_YEARS) %>%
      filter(fuel == "electricity") %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Enduse_elect_unscaled_USA

    # Calculate the scalers required to balance electricity within each region
    Enduse_elect_USA %>%
      left_join_error_no_match(Enduse_elect_unscaled_USA, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, value) ->
      Enduse_elect_scaler_USA

    # Multiply the electricity scalers by the original estimates of electricity consumption by end use sectors
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel == "electricity") %>%
      left_join_error_no_match(Enduse_elect_scaler_USA, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_elect_scaled_USA

    # Replace unscaled estimates of end use sector electricity consumption in full table
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel != "electricity") %>%
      bind_rows(Enduse_elect_scaled_USA) ->
      L131.in_EJ_USA_Senduse_F_Yh_noEFW

    # ===================================================
    L131.in_EJ_USA_Senduse_F_Yh_noEFW %>%
      add_title("Final scaled energy input by USA / end-use sector (incl CHP) / fuel / historical year (no EFW)") %>%
      add_units("EJ") %>%
      add_comments("Indicates energy consumption by end-use sector when energy-for-water is not considered") %>%
      add_precursors("energy/mappings/enduse_sector_aggregation", "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_unoil_F_Yh", "L122.in_EJ_R_refining_F_Yh", "L126.out_EJ_R_electd_F_Yh") ->
      L131.in_EJ_USA_Senduse_F_Yh_noEFW

    return_data(L131.in_EJ_USA_Senduse_F_Yh_noEFW)
  } else {
    stop("Unknown command")
  }
}
