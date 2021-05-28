# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB161.ag_R_C_Y_GLU_irr
#'
#' Calculates irrigated and rainfed agriculture production, harvested area and yields by GCAM region / commodity / GLU / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.ag_irrProd_Mt_R_C_Y_GLU}, \code{L161.ag_rfdProd_Mt_R_C_Y_GLU}, \code{L161.ag_irrHA_bm2_R_C_Y_GLU}, \code{L161.ag_rfdHA_bm2_R_C_Y_GLU}, \code{L161.ag_irrYield_kgm2_R_C_Y_GLU}, \code{L161.ag_rfdYield_kgm2_R_C_Y_GLU}, \code{L161.ag_irrHA_frac_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB161.ag_R_C_Y_GLU_irr.R} (aglu level1).
#' @details This chunk combines FAO annual data and GTAP disaggregated irrigated vs. rainfed data to compute irrigated and rainfed
#' agriculture production, harvested area and yields by GCAM region / commodity / GLU / year. The same irrigated and rainfed fraction
#' is applied to all historical years for each commodity, region and GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr if_else left_join mutate right_join select
#' @importFrom tidyr replace_na
#' @author RC August 2017
module_aglu_LB161.ag_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.ag_Prod_Mt_R_C_Y_GLU",
             "L101.ag_HA_bm2_R_C_Y_GLU",
             "L152.ag_irrProd_Mt_R_C_GLU",
             "L152.ag_irrHA_bm2_R_C_GLU",
             "L152.ag_rfdProd_Mt_R_C_GLU",
             "L152.ag_rfdHA_bm2_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L161.ag_irrHA_bm2_R_C_Y_GLU",
             "L161.ag_rfdHA_bm2_R_C_Y_GLU",
             "L161.ag_irrYield_kgm2_R_C_Y_GLU",
             "L161.ag_rfdYield_kgm2_R_C_Y_GLU",
             "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- GCAM_region_ID <- GCAM_commodity <- GLU <- value <- irrProd <- rfdProd <- irrProd_frac <-
      irrHA <- rfdHA <- irrHA_frac <- irrYield <- rfdYield <- MgdFor_adj <- GCAM_subsector <- NULL # silence package check.

    # Load required inputs
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU")
    L101.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L101.ag_HA_bm2_R_C_Y_GLU")
    L152.ag_irrProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_irrProd_Mt_R_C_GLU", strip_attributes = TRUE)
    L152.ag_irrHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_irrHA_bm2_R_C_GLU", strip_attributes = TRUE)
    L152.ag_rfdProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_rfdProd_Mt_R_C_GLU")
    L152.ag_rfdHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_rfdHA_bm2_R_C_GLU")

    # Compute irrigated and rainfed agriculture production by GCAM region / commodity / GLU / year
    # Combine FAO annual data and GTAP irrigated vs rainfed disaggregated data
    # Multiply annual production and a constant irrigated vs rainfed fraction for each GCAM region / commodity / GLU
    L152.ag_irrProd_Mt_R_C_GLU %>%
      # Combine GTAP irrigated and rainfed production
      left_join_error_no_match(L152.ag_rfdProd_Mt_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Compute fraction of production that is irrigated for each GCAM region / commodity / GLU
      mutate(irrProd_frac = irrProd / (irrProd + rfdProd)) %>%
      # Repeat the same irrigated fraction to all historical years
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # Match to FAO annual total production
      right_join(L101.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate irrigated production by multiplying total by fraction irrigated
      # For islands that are included in the FAO data but not the MIRCA inventory, irrProd_frac will be a missing value after the join. Re-set to 0 (assume all rainfed)
      mutate(irrProd_frac = if_else(is.na(irrProd_frac), 0, irrProd_frac),
             irrProd = value * irrProd_frac,
             # Calculate rainfed production by multiplying total by fraction rainfed
             rfdProd = value * (1 - irrProd_frac)) %>%
      select(-irrProd_frac, -value) ->
      L161.ag_Prod_Mt_R_C_Y_GLU

    # Compute irrigated and rainfed harvested area by GCAM commodity / region / GLU / year
    # Combine FAO annual data and GTAP irrigated vs rainfed disaggregated data
    # Multiply total annual harvested area and a constant irrigated vs rainfed fraction for each GCAM region / GLU / commodity
    L152.ag_irrHA_bm2_R_C_GLU %>%
      # Combine GTAP irrigated and rainfed harvested area
      left_join_error_no_match(L152.ag_rfdHA_bm2_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Compute fraction of harvested area that is irrigated for each GCAM region / GLU / commodity
      mutate(irrHA_frac = irrHA / (irrHA + rfdHA)) ->
      # Save the irrigated harvested area fraction table as an output
      L161.ag_irrHA_frac_R_C_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      # Repeat the same irrigated fraction to all historical years
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # Match to FAO annual total harvested area
      right_join(L101.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate irrigated production by multiplying total by fraction irrigated
      # For islands that are included in the FAO data but not the MIRCA inventory, irrHA_frac will be a missing value after the join. Re-set to 0 (assume all rainfed)
      mutate(irrHA_frac = if_else(is.na(irrHA_frac), 0, irrHA_frac),
             irrHA = value * irrHA_frac,
             # Calculate rainfed production by multiplying total by fraction rainfed
             rfdHA = value * (1 - irrHA_frac)) %>%
      select(-irrHA_frac, -value) ->
      L161.ag_HA_bm2_R_C_Y_GLU

    # Compute irrigated and rainfed yields (kg/m2) by GCAM region / commodity / GLU / year
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      # Match proudction and harvested area
      left_join_error_no_match(L161.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate irrigated yields as irrigated production divided by irrigated harvestest area
      mutate(irrYield = irrProd / irrHA,
             # Calculate rainfed yields as rainfed production divided by rainfed harvestest area
             rfdYield = rfdProd / rfdHA) %>%
      # Replace missing value with zero
      replace_na(list(irrYield = 0, rfdYield = 0)) %>%
      select(-irrProd, -rfdProd, -irrHA, -rfdHA) ->
      L161.ag_Yield_kgm2_R_C_Y_GLU

    # Produce outputs
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrProd) %>%
      add_title("Irrigated production by GCAM region / commodity / GLU / year") %>%
      add_units("Mt") %>%
      add_comments("Combine FAO annual production data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total production and a constant irrigated fraction across all histrical years") %>%
      add_legacy_name("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_irrProd_Mt_R_C_GLU") ->
      L161.ag_irrProd_Mt_R_C_Y_GLU

    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdProd) %>%
      add_title("Rainfed production by GCAM region / commodity / GLU / year") %>%
      add_units("Mt") %>%
      add_comments("Combine FAO annual production data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total production and a constant rainfed fraction across all histrical years") %>%
      add_legacy_name("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_rfdProd_Mt_R_C_GLU") ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrHA) %>%
      add_title("Irrigated harvested area by GCAM region / commodity / GLU / year") %>%
      add_units("bm2") %>%
      add_comments("Combine FAO annual harvested area data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total harvested area and a constant irrigated fraction across all histrical years") %>%
      add_legacy_name("L161.ag_irrHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L101.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_irrHA_bm2_R_C_GLU") ->
      L161.ag_irrHA_bm2_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdHA) %>%
      add_title("Rainfed harvested area by GCAM region / GLU / commodity / year") %>%
      add_units("bm2") %>%
      add_comments("Combine FAO annual harvested area data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total harvested area and a constant rainfed fraction across all histrical years") %>%
      add_legacy_name("L161.ag_rfdHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L101.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") ->
      L161.ag_rfdHA_bm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrYield) %>%
      add_title("Unadjusted irrigated agronomic yield by GCAM region / commodity / GLU / year") %>%
      add_units("kg/m2") %>%
      add_comments("Divide irrigated production by irrigated harvested area for each region, commodity, GLU and year") %>%
      add_comments("Replace missing value with zero") %>%
      add_legacy_name("L161.ag_irrYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrHA_bm2_R_C_Y_GLU") ->
      L161.ag_irrYield_kgm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdYield) %>%
      add_title("Unadjusted rainfed agronomic yield by GCAM region / commodity / GLU / year") %>%
      add_units("kg/m2") %>%
      add_comments("Divide rainfed production by rainfed harvested area for each region, commodity, GLU and year") %>%
      add_comments("Replace missing value with zero") %>%
      add_legacy_name("L161.ag_rfdYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdHA_bm2_R_C_Y_GLU")  ->
      L161.ag_rfdYield_kgm2_R_C_Y_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      add_title("Fraction of harvested area that is irigated by GCAM region / commodity / GLU") %>%
      add_units("Unitless") %>%
      add_comments("Divide irrigated harvested area by the sum of irrigated and rainfed harvest area for each region, commodity and GLU") %>%
      add_legacy_name("L161.ag_irrHA_frac_R_C_GLU") %>%
      add_precursors("L152.ag_irrHA_bm2_R_C_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") ->
      L161.ag_irrHA_frac_R_C_GLU

    return_data(L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU, L161.ag_irrHA_bm2_R_C_Y_GLU, L161.ag_rfdHA_bm2_R_C_Y_GLU, L161.ag_irrYield_kgm2_R_C_Y_GLU, L161.ag_rfdYield_kgm2_R_C_Y_GLU, L161.ag_irrHA_frac_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
