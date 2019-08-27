# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA113.MSW
#'
#' Generate municipal solid waste data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.RsrcCurves_EJ_R_MSW}. The corresponding file in the
#' original data system was \code{LA113.MSW.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter full_join group_by mutate full_join summarise
#' @importFrom tidyr gather spread
#' @author BBL
module_energy_LA113.MSW <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A13.MSW_curves",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.RsrcCurves_EJ_R_MSW"))
  } else if(command == driver.MAKE) {

    year <- region_GCAM3 <- value <- sumvalue <- share <- maxSubResource <- NULL
    ## silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")
    A13.MSW_curves <- get_data(all_data, "energy/A13.MSW_curves")

    # Perform computations 2a. Municipal solid waste supply curves
    # Downscale GCAM 3.0 MSW supply curves to countries on the basis of GDP
    # Calculate GDP shares of GCAM regions within region_GCAM3
    L100.gdp_mil90usd_ctry_Yh %>%
      PH_year_value_historical %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      left_join_error_no_match(iso_GCAM_regID[c("iso", "region_GCAM3", GCAM_REGION_ID)], by = "iso") ->
      L113.GDP_ctry

    L113.GDP_ctry %>%
      group_by(region_GCAM3) %>%
      summarise(sumvalue = sum(value)) %>%
      full_join(L113.GDP_ctry, by = "region_GCAM3") %>%
      mutate(share = value / sumvalue) %>%
      left_join_error_no_match(A13.MSW_curves[c("region_GCAM3", "maxSubResource")], by = "region_GCAM3") %>%
      mutate(maxSubResource = share * maxSubResource) ->
      L113.GDP_ctry

    # Aggregate country-level MSW resources to GCAM regions
    # This method assumes that all regions have the same curve shape parameters
    if(length(unique(A13.MSW_curves$resource)) > 1 |
       length(unique(A13.MSW_curves$subresource)) > 1 |
       length(unique(A13.MSW_curves$`mid-price`)) > 1 |
       length(unique(A13.MSW_curves$`curve-exponent`)) > 1 |
       length(unique(A13.MSW_curves$gdpSupplyElast)) > 1) {
      stop("The A13.MSW_curves data have different curve shape parameters!")
    }

    L113.GDP_ctry %>%
      dplyr::group_by_(GCAM_REGION_ID) %>%
      summarise(maxSubResource = sum(maxSubResource)) ->
      L113.MSW_maxSubResource

    # printlog("Writing the supply curves to all regions")
    tibble(GCAM_region_ID = sort(unique(iso_GCAM_regID[[GCAM_REGION_ID]])),
           resource = unique(A13.MSW_curves$resource),
           subresource = unique(A13.MSW_curves$subresource),
           mid.price = unique(A13.MSW_curves$`mid-price`),
           curve.exponent = unique(A13.MSW_curves$`curve-exponent`),
           gdpSupplyElast = unique(A13.MSW_curves$gdpSupplyElast)) %>%
      left_join_error_no_match(L113.MSW_maxSubResource[c(GCAM_REGION_ID, "maxSubResource")], by = GCAM_REGION_ID) %>%

      # Documentation
      add_title("Municipal solid waste resource curves by GCAM region") %>%
      add_units("EJ") %>%
      add_comments("Downscale GCAM 3.0 MSW supply curves to countries on GDP basis; aggregate to GCAM regions") %>%
      add_legacy_name("L113.RsrcCurves_EJ_R_MSW") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "energy/A13.MSW_curves",
                     "common/iso_GCAM_regID") ->
      L113.RsrcCurves_EJ_R_MSW

    # 2b. Historical biomass prices (currently determined at global level, so no level 1
    # processing necessary)

    return_data(L113.RsrcCurves_EJ_R_MSW)
  } else {
    stop("Unknown command")
  }
}
