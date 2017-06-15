#' module_energy_LA131.enduse
#'
#' This chunk generates the following two tables:
#' \itemize{
#'  \item{Final scaled energy input by GCAM region / end-use sector, incl CHP / fuel / historical year; and}
#'  \item{Share of heat consumption by end-use sector within GCAM region / historical year}
#' }
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.in_EJ_R_Senduse_F_Yh}, \code{L131.share_R_Senduse_heat_Yh}. The corresponding file in the
#' original data system was \code{LA131.enduse.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS May 2017
module_energy_LA131.enduse <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/enduse_sector_aggregation",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "temp-data-inject/L121.in_EJ_R_unoil_F_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L124.out_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heatfromelec_F_Yh",
             "L126.out_EJ_R_electd_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.in_EJ_R_Senduse_F_Yh",
             "L131.share_R_Senduse_heat_Yh"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <- value.y <- value.x <-
        has_district_heat <- . <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")

    enduse_sector_aggregation <- get_data(all_data, "energy/enduse_sector_aggregation")

    get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(year %in% HISTORICAL_YEARS) ->   # ensure temp data match our current history
      L1011.en_bal_EJ_R_Si_Fi_Yh

    get_data(all_data, "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(year %in% HISTORICAL_YEARS) ->   # ensure temp data match our current history
      L121.in_EJ_R_unoil_F_Yh

    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh")
    L124.out_EJ_R_heat_F_Yh <- get_data(all_data, "L124.out_EJ_R_heat_F_Yh")
    L124.out_EJ_R_heatfromelec_F_Yh <- get_data(all_data, "L124.out_EJ_R_heatfromelec_F_Yh")
    L126.out_EJ_R_electd_F_Yh <- get_data(all_data, "L126.out_EJ_R_electd_F_Yh")

    # ===================================================
    # ELECTRICITY SCALING
    # First, subset and aggregate the "upstream" electricity demands by the energy system that are not being scaled
    L121.in_EJ_R_unoil_F_Yh %>%
      filter(fuel == "electricity") ->
      Unoil_elect

    L122.in_EJ_R_refining_F_Yh %>%
      filter(fuel == "electricity", year %in% HISTORICAL_YEARS) %>%
      bind_rows(Unoil_elect) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Unoil_Refin_elect

    # Subtract this from total delivered electricity (output of t&d sector). This is the amount that is available for scaling to end uses.
    Unoil_Refin_elect %>%
      left_join_error_no_match(L126.out_EJ_R_electd_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.y - value.x) %>%
      select(-sector, -value.x, -value.y) ->
      Enduse_elect

    # Subset the end use sectors and aggregate by fuel
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector %in% enduse_sector_aggregation$sector, year %in% HISTORICAL_YEARS) %>%
      filter(fuel == "electricity") %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Enduse_elect_unscaled

    # Calculate the scalers required to balance electricity within each region
    Enduse_elect %>%
      left_join_error_no_match(Enduse_elect_unscaled, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, value) ->
      Enduse_elect_scaler

    # Multiply the electricity scalers by the original estimates of electricity consumption by end use sectors
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel == "electricity") %>%
      left_join_error_no_match(Enduse_elect_scaler, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_elect_scaled

    # Replace unscaled estimates of end use sector electricity consumption in full table
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel != "electricity") %>%
      bind_rows(Enduse_elect_scaled) ->
      Enduse_elect_scaled_heat_unscaled # still need to scale heat

    # HEAT SCALING
    # Total delivered heat = output of district heat sector + secondary (heat) output of electric sector
    L124.out_EJ_R_heatfromelec_F_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      Heatfromelect

    L124.out_EJ_R_heat_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      left_join_error_no_match(Heatfromelect, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x + value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat

    # Subset the end use sectors and aggregate by fuel. Only in regions where heat is modeled as a separate fuel.
    A_regions %>%
      filter(has_district_heat == 1) %>% # Filtering for regions where heat is modeled as a separate fuel
      select(GCAM_region_ID) %>%
      .$GCAM_region_ID ->
      GCAM_region_ID_heat

    Enduse_elect_scaled_heat_unscaled %>%
      filter(fuel == "heat", GCAM_region_ID %in% GCAM_region_ID_heat) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      Enduse_heat_unscaled

    # Calculate the scalers required to balance district heat production and consumption within each region
    Enduse_heat %>%
      left_join_error_no_match(Enduse_heat_unscaled, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x / value.y) %>%
      replace_na(list(value = 0)) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaler

    # Multiply the district heat scalers by the original estimates of district heat consumption by end use sectors
    Enduse_elect_scaled_heat_unscaled %>%
      filter(fuel == "heat", GCAM_region_ID %in% GCAM_region_ID_heat) %>%
      left_join_error_no_match(Enduse_heat_scaler, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaled

    # Replace unscaled estimates of end use sector heat consumption in full table
    Enduse_elect_scaled_heat_unscaled %>%
      mutate(fuel = replace(fuel, GCAM_region_ID %in% GCAM_region_ID_heat & fuel == "heat", "heat_scaled")) %>%
      filter(fuel != "heat_scaled") %>%
      bind_rows(Enduse_heat_scaled) %>%
      arrange(GCAM_region_ID, sector, fuel, year) ->
      L131.in_EJ_R_Senduse_F_Yh # Output table 1

    # Heat in some regions is not modeled separately from the fuels used to produce it
    A_regions %>%
      filter(has_district_heat == 0) %>% # Filtering for regions where heat is not modeled as a separate fuel
      select(GCAM_region_ID) %>%
      .$GCAM_region_ID ->
      GCAM_region_ID_no_heat

    # In these regions, calculate the share of regional heat demand by each sector
    L131.in_EJ_R_Senduse_F_Yh %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Enduse_total

    L131.in_EJ_R_Senduse_F_Yh %>%
      filter(fuel == "heat") %>%
      filter(GCAM_region_ID %in% GCAM_region_ID_no_heat) %>%
      left_join_error_no_match(Enduse_total, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaled_share

    # Regions may have zero heat consumption by demand sectors while nevertheless having heat production. Assign this to industry
    Enduse_heat_scaled_share %>%
      filter(sector == "in_industry_general") %>%
      replace_na(list(value = 1)) ->
      Enduse_heat_scaled_share_indust

    Enduse_heat_scaled_share %>%
      filter(sector != "in_industry_general") %>%
      replace_na(list(value = 0)) %>%
      bind_rows(Enduse_heat_scaled_share_indust) %>%
      arrange(GCAM_region_ID, sector, year) ->
      L131.share_R_Senduse_heat_Yh # Output table 2

    # ===================================================
    L131.in_EJ_R_Senduse_F_Yh %>%
      add_title("Final scaled energy input by GCAM region / end-use sector (incl CHP) / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Scalers were used to balance electricity and district heat production and consumption within each region") %>%
      add_legacy_name("L131.in_EJ_R_Senduse_F_Yh") %>%
      add_precursors("energy/enduse_sector_aggregation", "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh", "L122.in_EJ_R_refining_F_Yh", "L126.out_EJ_R_electd_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.in_EJ_R_Senduse_F_Yh

    L131.share_R_Senduse_heat_Yh %>%
      add_title("Share of heat consumption by end-use sector within GCAM region / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Share of regional heat demand by each sector was calculated for regions where heat is not modeled separately from the fuels used to produce it. Moreoever, regions having zero heat consumption by demand sectors while nevertheless also having heat production, this was assigned to industry") %>%
      add_legacy_name("L131.share_R_Senduse_heat_Yh") %>%
      add_precursors("energy/A_regions", "energy/enduse_sector_aggregation",
                     "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh",
                     "L122.in_EJ_R_refining_F_Yh",
                     "L124.out_EJ_R_heat_F_Yh", "L124.out_EJ_R_heatfromelec_F_Yh",
                     "L126.out_EJ_R_electd_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.share_R_Senduse_heat_Yh

    return_data(L131.in_EJ_R_Senduse_F_Yh, L131.share_R_Senduse_heat_Yh)
  } else {
    stop("Unknown command")
  }
}
