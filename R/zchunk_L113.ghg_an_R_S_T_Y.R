# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L113.ghg_an_R_S_T_Y
#'
#'  Calculate the animal GHG emissions (CH4 and N2O)
#'  by GCAM region / sector / technology / historical year, by scaling EPA
#'  emissions by tech to match EDGAR.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.ghg_tg_R_an_C_Sys_Fd_Yh}. The corresponding file in the
#' original data system was \code{L113.ghg_an_R_S_T_Y.R} (emissions level1).
#' @details "Calculate the animal emissions (CH4 and N2O) first compute unscaled emissions
#' by country and technology, then match emissions factors from EPA, next
#' compute unscaled emissions (production * emfactors) and aggregate by sector and region,
#' then compute EDGAR emissions by region and sector and lastly, scale the EPA emissions
#' by tech to match EDGAR. Note that file L115, handles NH3."
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise
#' @importFrom tidyr gather spread
#' @author CH July 2017
module_emissions_L113.ghg_an_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/mappings/EPA_ghg_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L103.ghg_tgmt_USA_an_Sepa_F_2005",
             FILE = "emissions/EDGAR/EDGAR_CH4",
             FILE = "emissions/EDGAR/EDGAR_N2O"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.ghg_tg_R_an_C_Sys_Fd_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    value <- GCAM_commodity <- feed <- GCAM_region_ID <- year <- production <-
      EPA_agg_sector <- EDGAR_agg_sector <- ch4_em_factor <- fuel <- Non.CO2 <-
      epa_emissions <- `IPCC-Annex` <- `World Region` <- ISO_A3 <- Name <- IPCC <-
      IPCC_description <- IPCC_description.y <- agg_sector <- EDGAR_emissions <-
      EPA_emissions <- scalar <- emissions <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    EPA_ghg_tech <- get_data(all_data, "emissions/mappings/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L103.ghg_tgmt_USA_an_Sepa_F_2005 <- get_data(all_data, "L103.ghg_tgmt_USA_an_Sepa_F_2005")
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR/EDGAR_CH4")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR/EDGAR_N2O")

    # ===================================================
    # Computing unscaled emissions by country and technology
    # using animal production from L107.an_Prod_Mt_R_C_SYS_Fd_Y
    # and EPA emissions factos.

      L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      rename(production = value) %>%
      left_join(GCAM_sector_tech, by = c("GCAM_commodity" = "sector", "system" = "fuel", "feed" = "technology")) %>%
      select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, EPA_agg_sector, EDGAR_agg_sector) %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = c("N2O_AGR", "CH4_AGR"))) %>%  # Add Gas Name and AGR for agriculture
      # match in emissions factors, using left_join and dropping fuel column
      left_join(L103.ghg_tgmt_USA_an_Sepa_F_2005, by = c("EPA_agg_sector" = "sector")) %>%
      mutate(epa_emissions = production * ch4_em_factor) %>%  # compute unscaled emissions
      select(-fuel) %>%
      na.omit() ->
      L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt

    # Aggregate by sector and region
    L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(EPA_emissions = sum(epa_emissions)) %>%
      ungroup() ->
      L113.ghg_tg_R_an_C_Yh.mlt

    # Compute EDGAR emissions by region and sector
    EDGAR_CH4$Non.CO2 <- "CH4_AGR"
    EDGAR_N2O$Non.CO2 <- "N2O_AGR"
    L113.EDGAR <- bind_rows(EDGAR_CH4, EDGAR_N2O) # combine CH4 and N2O

    L113.EDGAR %>%  # convert to long format
      gather_years %>%
      # Not all IPCC codes in L113.EDGAR (5F2 and 5d) are in EDGAR_sector, so we can't use left_join_error_no_match
      left_join(EDGAR_sector, by = "IPCC") %>%
      select(-IPCC_description.y) %>%
      rename (EDGAR_agg_sector = agg_sector) %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>% # update Romania ISO code
      left_join(iso_GCAM_regID, by = "iso") %>%
      # Drop unnecessary columns, aggregate by region, and melt
      select(year, value, Non.CO2, EDGAR_agg_sector, GCAM_region_ID) %>%
      na.omit() %>%
      filter(year %in% emissions.EDGAR_YEARS) %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector,year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(year = as.integer(year)) ->
      L113.EDGAR.mlt

    # Scale EPA emissions by tech to match EDGAR totals

    # First compute scalers
    L113.ghg_tg_R_an_C_Yh.mlt %>%
      left_join(L113.EDGAR.mlt, by = c("year", "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector")) %>%
      rename(EDGAR_emissions = value) %>%
      mutate(scalar = EDGAR_emissions / EPA_emissions / 1000.0) -> # 1000.0 to convert from Gg to Tg
      L113.emiss_scaler

    # Second scale EPA emissions
    L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
      left_join(L113.emiss_scaler, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      mutate(emissions = epa_emissions * scalar) %>%
      select(-EPA_emissions, -EDGAR_emissions) %>%
      filter(year %in% emissions.EDGAR_YEARS) %>%
      replace_na(list(emissions = 0)) %>%
      select(GCAM_region_ID, Non.CO2, supplysector = GCAM_commodity, subsector = system, stub.technology = feed,
          value = emissions, year) %>%

      add_title("Animal GHG emissions (CH4 and N2O) by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("First: compute unscaled emissions by country and technology") %>%
      add_comments("Second: match in emissions factors from EPA") %>%
      add_comments("Third: compute unscaled emissions (production * emfactors) and aggregate by sector and region") %>%
      add_comments("Fourth: compute EDGAR emissions by region and sector") %>%
      add_comments("Fifth: scale EPA emissions by tech to match EDGAR") %>%
      add_legacy_name("L113.ghg_tg_R_an_C_Sys_Fd_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "emissions/mappings/EPA_ghg_tech",
                     "emissions/mappings/GCAM_sector_tech", "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "L103.ghg_tgmt_USA_an_Sepa_F_2005", "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O") ->
      L113.ghg_tg_R_an_C_Sys_Fd_Yh

    return_data(L113.ghg_tg_R_an_C_Sys_Fd_Yh)
  } else {
    stop("Unknown command")
  }
}
