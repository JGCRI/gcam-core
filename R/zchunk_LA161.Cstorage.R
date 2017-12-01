#' module_energy_LA161.Cstorage
#'
#' Build carbon storage supply curves by region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.RsrcCurves_MtC_R}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (energy level1).
#' @details Build carbon storage supply curves by region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH November 2017
module_energy_LA161.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/Land_type_area_ha",
             FILE = "energy/A61.Cstorage_curves",
             FILE = "energy/Dooley_Cstorage_RG3_MtCO2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.RsrcCurves_MtC_R"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- available <- cost_2005USDtCO2 <- fraction <- grade <- iso <-
      region_GCAM3 <- share <- value <- variable <- year <- NULL

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Land_type_area_ha <- get_data(all_data, "aglu/LDS/Land_type_area_ha")
    A61.Cstorage_curves <- get_data(all_data, "energy/A61.Cstorage_curves")
    Dooley_Cstorage_RG3_MtCO2 <- get_data(all_data, "energy/Dooley_Cstorage_RG3_MtCO2") %>%
      gather(variable, value, -region_GCAM3)

    # ===================================================
    # Calculate the supply curves by GCAM 3 region based on the available data
    L161.Cstorage_MtCO2_RG3_grade <- Dooley_Cstorage_RG3_MtCO2 %>%
      # NOTE: Not including any offshore carbon resources in the onshore resource estimates. Offshore is considered an unlimited resource
      filter(!grepl("Offshore", variable)) %>%
      # Aggregate by region
      group_by(region_GCAM3) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # Repeat by number of grades, and multiply by the fraction of the total resource assigned to each grade
      repeat_add_columns(tibble(grade = unique(A61.Cstorage_curves$grade))) %>%
      left_join_error_no_match(A61.Cstorage_curves, by = "grade") %>%
      mutate(available = value * fraction)

    # Downscaling GCAM 3.0 carbon storage supply curves to countries on the basis of land area
    # Calculate land cover shares of GCAM regions within region_GCAM3
    L161.LC_bm2_ctry <- Land_type_area_ha %>%
      filter(year == max(year)) %>%
      group_by(iso) %>%
      # Total land per country
      summarise(value = sum(value * CONV_HA_BM2)) %>%
      ungroup() %>%
      # Add in GCAM3 region and calculate shares of countries within region
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(region_GCAM3) %>%
      mutate(share = value / sum(value)) %>%
      ungroup()

    # Repeat by number of grades, and match in the available quantities
    L161.Available_MtC_R_C <- L161.LC_bm2_ctry %>%
      repeat_add_columns(tibble(grade = unique(A61.Cstorage_curves$grade))) %>%
      left_join_error_no_match(L161.Cstorage_MtCO2_RG3_grade, by = c("region_GCAM3", "grade")) %>%
      # The carbon storage quantities from the literature are in CO2; for GCAM we convert to C.
      mutate(available = available * share / emissions.CONV_C_CO2) %>%
      # Aggregate to GCAM 4 regions
      group_by(GCAM_region_ID, grade, cost_2005USDtCO2) %>%
      summarise(available = sum(available)) %>%
      ungroup()

    # Building carbon storage supply curves
    L161.RsrcCurves_MtC_R <- L161.Available_MtC_R_C %>%
      mutate(resource = unique(A61.Cstorage_curves$resource),
             subresource = unique(A61.Cstorage_curves$subresource),
             # NOTE: currently assuming that all regions have the same price points
             # Rounding the digits here, as the fossil supply curves are normally already rounded to one digit
             extractioncost = round(cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1990), energy.DIGITS_COST)) %>%
      select(-cost_2005USDtCO2) %>%
      # Produce output
      add_title("Carbon storage resource supply curves by GCAM region") %>%
      add_units("Mt C") %>%
      add_comments("GCAM3 supply curves downscaled to country by land share") %>%
      add_comments("Country supply curves aggregated to GCAM4 region") %>%
      add_legacy_name("L161.RsrcCurves_MtC_R") %>%
      add_precursors("common/iso_GCAM_regID","aglu/LDS/Land_type_area_ha",
                     "energy/A61.Cstorage_curves", "energy/Dooley_Cstorage_RG3_MtCO2") ->
      L161.RsrcCurves_MtC_R

    return_data(L161.RsrcCurves_MtC_R)
  } else {
    stop("Unknown command")
  }
}
