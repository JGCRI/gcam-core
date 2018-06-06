#' module_water_L132.water.demand.manufacturing
#'
#' Computes manufacturing energy use coefficients.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.water_coef_manufacturing_R_W_m3_GJ}. The corresponding file in the
#' original data system was \code{L132.water.demand.manufacturing.R} (water level1).
#' @details Computes manufacturing energy use coefficients for water withdrawal
#' and consumption for all regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author SWDT April 2017
module_water_L132.water.demand.manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh",
             FILE = "water/manufacturing_water_mapping",
             FILE = "water/manufacturing_water_data",
             FILE = "water/manufacturing_water_ratios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.water_coef_manufacturing_R_W_m3_GJ"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <- continent <-
        withdrawals <- consumption <- `water withdrawals` <-
        `water consumption` <- water_type <- coefficient <- region <-
         NULL                           # silence package checks.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    manufacturing_water_mapping <- get_data(all_data, "water/manufacturing_water_mapping")
    manufacturing_water_data <- get_data(all_data, "water/manufacturing_water_data")
    manufacturing_water_ratios <- get_data(all_data, "water/manufacturing_water_ratios")

    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")

    # ===================================================

    # Pull out global ratio constants for manufacturing water
    selfTotalRatio <- manufacturing_water_ratios$`self-to-total-ratio`
    consWithRatio <- manufacturing_water_ratios$`cons-to-with-ratio`

    # Get total industrial energy use for manufacturing water continent regions for 1995
    L1322.in_EJ_R_indenergy_F_Yh %>%
      bind_rows(L1322.in_EJ_R_indfeed_F_Yh) %>%
      filter(year == 1995) %>%
      select(GCAM_region_ID, value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(manufacturing_water_mapping, by = "region") %>%
      group_by(continent) %>%
      summarise(value = sum(value)) %>%

      # Join continental water withdrawals and consumtpion
      left_join_error_no_match(manufacturing_water_data, by = "continent") %>%

      # Convert withdrawals and consumption to km^3
      mutate(withdrawals = withdrawals * CONV_MILLION_M3_KM3,
             consumption = consumption * CONV_MILLION_M3_KM3) %>%

      # Compute withdrawals and consumption in km^3 per EJ
      mutate(`water withdrawals` = withdrawals / value * selfTotalRatio) %>%
      mutate(`water consumption` = `water withdrawals` * consWithRatio) %>%
      select(continent, `water withdrawals`, `water consumption`) %>%
      gather(water_type, coefficient, -continent) -> L132.manufacture_content_energy

    # Map coefficients back to GCAM regions
    L132.manufacture_content_energy %>%
      right_join(manufacturing_water_mapping, by = "continent") %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      arrange(region) %>%
      select(GCAM_region_ID, water_type, coefficient) -> L132.water_coef_manufacturing

    # ===================================================

    L132.water_coef_manufacturing %>%
      add_title("Manufacturing energy water coefficients by region and water type") %>%
      add_units("m^3 / GJ") %>%
      add_comments("Uses continental industrial energy use coefficents (1995)") %>%
      add_comments("to determine water withdrawal and consumption coefficients") %>%
      add_legacy_name("L132.water_coef_manufacturing_R_W_m3_GJ") %>%
      add_precursors("common/GCAM_region_names",
                     "L1322.in_EJ_R_indenergy_F_Yh",
                     "L1322.in_EJ_R_indfeed_F_Yh",
                     "water/manufacturing_water_mapping",
                     "water/manufacturing_water_data",
                     "water/manufacturing_water_ratios") ->
      L132.water_coef_manufacturing_R_W_m3_GJ

    return_data(L132.water_coef_manufacturing_R_W_m3_GJ)
  } else {
    stop("Unknown command")
  }
}
