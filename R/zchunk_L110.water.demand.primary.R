#' module_water_L110.water.demand.primary
#'
#' Gets water use coefficients for primary energy.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L110.water_demand_primary_R_S_W_m3_GJ}. The corresponding file in the
#' original data system was \code{L110.water.demand.primary.R} (water level1).
#' @details Gets water use coefficients for primary energy using regional
#' fractions of saline and freshwater shares.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select arrange left_join
#' @importFrom tidyr gather spread
#' @importFrom tibble as_tibble
#' @author SWDT April 2017
module_water_L110.water.demand.primary <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/A227.resource_water_coef_mapping",
             FILE = "water/resource_water_data",
             FILE = "water/resource_water_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L110.water_demand_primary_R_S_W_m3_GJ"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A227.resource_water_coef_mapping <- get_data(all_data, "water/A227.resource_water_coef_mapping")
    resource_water_data <- get_data(all_data, "water/resource_water_data")
    resource_water_share <- get_data(all_data, "water/resource_water_share")

    # ===================================================

    # Get water consumption (m^3/TJ) by fuel and supply sector
    A227.resource_water_coef_mapping %>%
      left_join_error_no_match(resource_water_data, by = c("fuel", "subsector", "technology")) %>%
      select(fuel, supplysector, water.coefficient.m3.per.TJ) %>%
      arrange(fuel) %>%
      select(-fuel) %>%
      unique() -> L110.global_water_cons_coef

    # Create tibble for all regions and supply sectors
    GCAM_region_names %>%
      repeat_add_columns(L110.global_water_cons_coef) %>%
      select(-region) %>%
      arrange(GCAM_region_ID) -> L110.water_coef_region_supplysector

    # Get water consumption and withdrawal ratios 32 GCAM regions...
    # ... all regions other than Middle East set to US values
    GCAM_region_names %>%
      mutate(region_GCAM3 = region) %>%
      left_join(resource_water_share, "region_GCAM3") %>%
      select(-region_GCAM3, -region) -> L110.resource_water_share
    names(L110.resource_water_share) <- c("GCAM_region_ID", "sal", "fresh", "cons_fr", "cons_tot")
    L110.resource_water_share$sal[is.na(L110.resource_water_share$sal)] <-
      filter(L110.resource_water_share, GCAM_region_ID == 1)$sal
    L110.resource_water_share$fresh[is.na(L110.resource_water_share$fresh)] <-
      filter(L110.resource_water_share, GCAM_region_ID == 1)$fresh
    L110.resource_water_share$cons_fr[is.na(L110.resource_water_share$cons_fr)] <-
      filter(L110.resource_water_share, GCAM_region_ID == 1)$cons_fr
    L110.resource_water_share$cons_tot[is.na(L110.resource_water_share$cons_tot)] <-
      filter(L110.resource_water_share, GCAM_region_ID == 1)$cons_tot

    # Bring consumption ratios and water usage coefficents into single table
    L110.resource_water_share %>%
      left_join(L110.water_coef_region_supplysector, by = "GCAM_region_ID") -> L110.water_ratios_coef

    # Calculate water withdrawal, seawater, and consumption, then bind to single tibble
    L110.water_ratios_coef %>%
      mutate(water_type = "water withdrawals") %>%
      mutate(coefficient_WC = fresh * water.coefficient.m3.per.TJ * 1e-3) %>%
      mutate(coefficient = coefficient_WC / cons_fr) %>%
      select(-coefficient_WC) -> L110.water_withdrawals
    L110.water_ratios_coef %>%
      mutate(water_type = "seawater") %>%
      mutate(coefficient = sal * water.coefficient.m3.per.TJ * 1e-3) -> L110.seawater
    L110.water_ratios_coef %>%
      mutate(water_type = "water consumption") %>%
      mutate(coefficient = fresh * water.coefficient.m3.per.TJ * 1e-3) %>%
      bind_rows(L110.water_withdrawals, L110.seawater) %>%
      select(GCAM_region_ID, supplysector, water_type, coefficient) ->
      L110.water_demand_primary

    L110.water_demand_primary %>%
      add_title("Primary energy water coefficients by region ID / supply sector / water type") %>%
      add_units("m^3 / GJ") %>%
      add_comments("Primary energy water use coefficients calculated using US data for fractions of saline and freshwater share") %>%
      add_legacy_name("L110.water_demand_primary_R_S_W_m3_GJ") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A227.resource_water_coef_mapping",
                     "water/resource_water_data",
                     "water/resource_water_share") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_XYEAR) -> L110.water_demand_primary_R_S_W_m3_GJ

    # Reinistate old behavior for test (i.e., Middle East same as all other regions)
    # See issue #179 on gcamdata repo
     if(OLD_DATA_SYSTEM_BEHAVIOR) {
       L110.water_demand_primary_R_S_W_m3_GJ[L110.water_demand_primary_R_S_W_m3_GJ == 21, "coefficient"] <-
         L110.water_demand_primary_R_S_W_m3_GJ[L110.water_demand_primary_R_S_W_m3_GJ == 1, "coefficient"]
       return_data(L110.water_demand_primary_R_S_W_m3_GJ)
       } else {
       return_data(L110.water_demand_primary_R_S_W_m3_GJ)
       }

    # ===================================================


  } else {
    stop("Unknown command")
  }
}
