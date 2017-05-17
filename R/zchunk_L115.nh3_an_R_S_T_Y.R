#' module_emissions_L115.nh3_an_R_S_T_Y
#'
#' Annual animal NH3 emissions by GCAM region, sector and technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L115.nh3_tg_R_an_C_Sys_Fd_Yh}. The corresponding file in the
#' original data system was \code{L115.nh3_an_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD May 2017
module_emissions_L115.nh3_an_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L105.nh3_tgmt_USA_an_Yh",
             FILE = "emissions/EDGAR/EDGAR_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.nh3_tg_R_an_C_Sys_Fd_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L105.nh3_tgmt_USA_an_Yh <- get_data(all_data, "L105.nh3_tgmt_USA_an_Yh")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3")

    # ===================================================
    # Compute emissions using EPA emissions factors and FAO animal production

    # Rename column and add gas name
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      rename(production = value) %>%
      mutate(Non.CO2 = "NH3_AGR") ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh

    # Match emissions factors by year and compute unscaled emissions
    L115.nh3_tg_R_an_C_Sys_Fd_Yh %>%
      left_join(L105.nh3_tgmt_USA_an_Yh, by = "year") %>%
      rename(emfact = value) %>%
      mutate(epa_emissions = production * emfact) %>%
      na.omit() ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt

    # Match by sector, technology and fuel
    left_join(tidyr::unite(L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt, col = "commodity_system_feed", c(GCAM_commodity, system, feed), sep = "~"),
              tidyr::unite(GCAM_sector_tech, col = "sector_tech_fuel", c(sector, fuel, technology), sep = "~"),
              by = c("commodity_system_feed"="sector_tech_fuel")) %>%
      tidyr::separate(commodity_system_feed, c("GCAM_commodity", "system", "feed"), sep = "~") %>%
      select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, Non.CO2, emfact, epa_emissions, EDGAR_agg_sector) ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt

    # Aggregate by sector and region
    L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarize(EPA_emissions = sum(epa_emissions)) %>%
      arrange(year) ->
      L115.nh3_tg_R_an_C_Yh.mlt

    # Compute EDGAR emissions by region and sector
    L115.EDGAR <- EDGAR_NH3

    # Renaming L115.EDGAR variable names - Note: may remove if names for L115.EDGAR change
    names(L115.EDGAR)[1] <- "IPCC_Annex"
    names(L115.EDGAR)[2] <- "World_Region"

    # Add gas name and match by emissions factors
    L115.EDGAR %>%
      mutate(Non.CO2 = "NH3_AGR") %>%
      left_join(EDGAR_sector, by = "IPCC") %>%
      rename(EDGAR_agg_sector = agg_sector) ->
      L115.EDGAR

    # Convert from EDGAR iso to GCAM_region_ID
    L115.EDGAR %>%
      mutate( iso = tolower( ISO_A3 ), ISO_A3 = NULL ) %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") ->
      L115.EDGAR

    # Note : the old data system drops values from 1970, it is unclear if that is intentional or an accident.
    # Drop unnecessary columns, make long, and aggregate by region.
    L115.EDGAR %>%
      select(-c(IPCC_Annex, World_Region, iso, Name, IPCC, IPCC_description.x, IPCC_description.y, country_name, region_GCAM3)) %>%
      na.omit() %>%
      gather(year, value, -c(Non.CO2, EDGAR_agg_sector, GCAM_region_ID)) %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(value = sum(value)) ->
      L115.EDGAR

    # Scale EPA emission by tech to match EDGAR tools
    L115.emiss_scaler <- L115.nh3_tg_R_an_C_Yh.mlt

    # First match by GCAM Region ID, gas, EDGAR_agg_sector and year, then compute scalers
    tidyr::unite(L115.emiss_scaler, col = "Region_GHG_Sector_Yr", c(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year), sep = "~") %>%
      left_join(tidyr::unite(L115.EDGAR, col = "Region_GHG_Sector_Yr", c(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year), sep = "~"), by = "Region_GHG_Sector_Yr") %>%
      rename(EDGAR_emissions = value) %>%
      tidyr::separate(Region_GHG_Sector_Yr, c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year"), sep = "~") %>%
      mutate( ., scaler = EDGAR_emissions / EPA_emissions / 1000.0 ) ->
      L115.emiss_scaler

    # Now, match by GCAM Region ID, gas, EDGAR_agg_sector and year, then scale EPA emission by tech to match EDGAR tools
    tidyr::unite(L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt, col = "Region_GHG_Sector_Yr", c(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year), sep = "~") %>%
      left_join(tidyr::unite(L115.emiss_scaler, col = "Region_GHG_Sector_Yr", c(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year), sep = "~"), by = "Region_GHG_Sector_Yr") %>%
      tidyr::separate(Region_GHG_Sector_Yr, c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year"), sep = "~") %>%
      mutate(emissions = epa_emissions * scaler) %>%
      mutate_all(funs(replace(., is.na(.), 0))) ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt

    # Rename columns and reshape data
    L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt %>%
      rename(supplysector = GCAM_commodity, subsector = system, stub.technology = feed, value = emissions) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value) ->
    L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt

    # ===================================================

    # Produce outputs
    L115.nh3_tg_R_an_C_Sys_Fd_Yh.mlt %>%
      add_title(" Animal NH3 emissions by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("Annual animal NH3 emissions is computed using EPA emissions factors and FAO animal production.") %>%
      add_comments("EPA emissions are scaled by technology to match EDGAR totals.") %>%
      add_legacy_name("L115.nh3_tg_R_an_C_Sys_Fd_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector",
                     "emissions/GCAM_sector_tech", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "L105.nh3_tgmt_USA_an_Yh",
                     "emissions/EDGAR/EDGAR_NH3") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh

    return_data(L115.nh3_tg_R_an_C_Sys_Fd_Yh)
  } else {
    stop("Unknown command")
  }
}


