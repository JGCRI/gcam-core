# module-energy.R


#' module_energy_LA113.MSW
#'
#' Construct the \code{energy} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
module_energy_LA113.MSW <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.RsrcCurves_EJ_R_MSW"))
  } else if(command == driver.DECLARE_INPUTS) {
    return(c("L100.gdp_mil90usd_ctry_Yh",
             FILE = "energy/A13.MSW_curves.csv",
             FILE = "common/iso_GCAM_regID.csv"))
  } else if(command == driver.MAKE) {
    energy_LA113.MSW(...)
  } else {
    stop("Unknown command")
  }
}


#' energy_LA113.MSW
#'
#' @param all_data A named list, holding all energy system products so far
#' @return A named list with all energy data.
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom tidyr gather spread
#' @export
energy_LA113.MSW <- function(all_data) {
  #  printlog("Municipal solid waste supply curves")

  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID.csv")
  L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")
  A13.MSW_curves <- get_data(all_data, "energy/A13.MSW_curves.csv")

  # Perform computations 2a. Municipal solid waste supply curves
  # printlog("Downscaling GCAM 3.0 MSW supply curves to countries on the basis of GDP")
  # Calculate GDP shares of GCAM regions within region_GCAM3
  L100.gdp_mil90usd_ctry_Yh %>%
    PH_year_value_historical %>%
    filter(year == max(HISTORICAL_YEARS)) %>%
    left_join(iso_GCAM_regID[c("iso", "region_GCAM3", GCAM_REGION_ID)], by = "iso") ->
    L113.GDP_ctry

  L113.GDP_ctry %>%
    group_by(region_GCAM3) %>%
    summarise(sumvalue = sum(value)) %>%
    full_join(L113.GDP_ctry, by = "region_GCAM3") %>%
    mutate(share = value / sumvalue) %>%
    left_join(A13.MSW_curves[c("region_GCAM3", "maxSubResource")], by = "region_GCAM3") %>%
    mutate(maxSubResource = share * maxSubResource) ->
    L113.GDP_ctry

  # printlog("Aggregating country-level MSW resources to GCAM regions")
  # printlog("NOTE: this method assumes that all regions have the same curve shape parameters")
  if(length(unique(A13.MSW_curves$resource)) > 1 |
     length(unique(A13.MSW_curves$subresource)) > 1 |
     length(unique(A13.MSW_curves$`mid-price`)) > 1 |
     length(unique(A13.MSW_curves$`curve-exponent`)) > 1 |
     length(unique(A13.MSW_curves$gdpSupplyElast)) > 1) {
    stop("The A13.MSW_curves data have different curve shape parameters!")
  }

  L113.GDP_ctry %>%
    group_by_(GCAM_REGION_ID) %>%
    summarise(maxSubResource = sum(maxSubResource)) ->
    L113.MSW_maxSubResource

  # printlog("Writing the supply curves to all regions")
  tibble(GCAM_region_ID = sort(unique(iso_GCAM_regID[[GCAM_REGION_ID]])),
         resource = unique(A13.MSW_curves$resource),
         subresource = unique(A13.MSW_curves$subresource),
         mid.price = unique(A13.MSW_curves$`mid-price`),
         curve.exponent = unique(A13.MSW_curves$`curve-exponent`),
         gdpSupplyElast = unique(A13.MSW_curves$gdpSupplyElast)) %>%
    left_join(L113.MSW_maxSubResource[c(GCAM_REGION_ID, "maxSubResource")], by = GCAM_REGION_ID) %>%
    add_comments(c("MSW resource curves by GCAM region", "Unit = EJ")) ->
    L113.RsrcCurves_EJ_R_MSW

  # 2b. Historical biomass prices (currently determined at global level, so no level 1
  # processing necessary)

  return_data(L113.RsrcCurves_EJ_R_MSW)
}
