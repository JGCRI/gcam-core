# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @details This chunk uses EPA emissions and FAO data to estimate agricultural NH3 emissions which are scaled to regional values using EDGAR data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange group_by left_join mutate select summarise
#' @importFrom tidyr replace_na separate
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

    EDGAR_agg_sector <- . <- EDGAR_emissions <- GCAM_commodity <- GCAM_region_ID <- IPCC <-
      `IPCC-Annex` <- IPCC_Annex <- IPCC_description.x <- IPCC_description.y <- ISO_A3 <-
      Name <- Non.CO2 <- Region_GHG_Sector_Yr <- `World Region` <- World_Region <-
      agg_sector <- commodity_system_feed <- country_name <- emfact <- emissions <- feed <-
      fuel <- hybrid_emissions <- iso <- production <- region_GCAM3 <- scaler <- sector <-
      technology <- total_hybrid_emissions <- value <- year <- NULL  # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L105.nh3_tgmt_USA_an_Yh <- get_data(all_data, "L105.nh3_tgmt_USA_an_Yh")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3")

    # ===================================================
    # The first section estimates emissions "hybrid emissions" from EPA emission factors and FAO animal production
    # The second section computes scalers from estimated region emissions to EDGAR totals
    # The third section scales emission estimates by the calculated EGAR scaler ratio


    # Aggergate emissions by region and sector

    # Rename column and add gas name
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      rename(production = value) %>%
      mutate(Non.CO2 = "NH3_AGR") ->
      L115.nh3_tg_R_C_yr_Sys_Fd_pro_G

    # Join EPA emissions and emissions factor by years, there should be a 1:1 match.
    # Estimate "hybrid emissions" by multiplying unscaled EPA emissions by FAO animal production
    L115.nh3_tg_R_C_yr_Sys_Fd_pro_G %>%
      left_join(L105.nh3_tgmt_USA_an_Yh, by = "year") %>%
      rename(emfact = value) %>%
      mutate(hybrid_emissions = production * emfact) %>%
      na.omit() ->
      L115.nh3_tg_R_C_yr_Sys_Fd_pro_G_fu_emf_hyb

    # Match hybrid emissions with GCAM technology by region, system, and fuel. There should be a 1:1 match.
    L115.nh3_tg_R_C_yr_Sys_Fd_pro_G_fu_emf_hyb %>%
      left_join(GCAM_sector_tech, by = c("GCAM_commodity" = "sector", "system" = "fuel", "feed" = "technology")) %>%
      select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, Non.CO2, emfact, hybrid_emissions, EDGAR_agg_sector) ->
      L115.nh3_tg_R_C_Sys_Fd_yr_pro_G_emf_hyb_sec

    # Calculate total hybrid emissions for sector and region
    L115.nh3_tg_R_C_Sys_Fd_yr_pro_G_emf_hyb_sec %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(total_hybrid_emissions = sum(hybrid_emissions)) %>%
      ungroup %>%
      arrange(year) ->
      L115.nh3_tg_R_G_sec_yr_tHyb


    # Compute scalers for EDGAR totals


    # Rename EDGAR variable names
    L115.EDGAR <- rename(EDGAR_NH3, IPCC_Annex = `IPCC-Annex`, World_Region = `World Region`)

    # Add gas name and match with agg sector by IPCC, there should be gas data for every IPCC agg sector.
    L115.EDGAR %>%
      mutate(Non.CO2 = "NH3_AGR") %>%
      left_join(EDGAR_sector, by = "IPCC") %>%
      rename(EDGAR_agg_sector = agg_sector) ->
      L115.EDGAR_G_sec

    # Convert from EDGAR iso to GCAM_region_ID, there should be a 1:1 match between EDGAR regions and GCAM regions.
    L115.EDGAR_G_sec %>%
      mutate(iso = tolower(ISO_A3), ISO_A3 = NULL) %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") ->
      L115.EDGAR_GCAM

    # Drop unnecessary columns, make long, and aggregate by region and sector
    L115.EDGAR_GCAM %>%
      select(-c(IPCC_Annex, World_Region, iso, Name, IPCC, IPCC_description.x, IPCC_description.y, country_name, region_GCAM3)) %>%
      na.omit() %>%
      gather_years %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L115.EDGAR_R_G_sec_yr_v


    # Match total hybrid emissions data with EDGAR emissions data by technology
    # Create the from the scaler ratio of EDGAR total emissions to aggregated total hybrid emissions and multiplying by 1000
    L115.nh3_tg_R_G_sec_yr_tHyb %>%
      left_join_error_no_match(L115.EDGAR_R_G_sec_yr_v, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      rename(EDGAR_emissions = value) %>%
      mutate(scaler = EDGAR_emissions / total_hybrid_emissions / 1000.0) ->
      L115.emiss_scaler


    # Scale the hybird emissions to match the EDGAR totals


    # Match calculated hybrid emissions with the emission scaler ratio
    # Multiply the hybrid emissions by the scaler ratio to match the EDGAR totals
    L115.nh3_tg_R_C_Sys_Fd_yr_pro_G_emf_hyb_sec %>%
      left_join_error_no_match(L115.emiss_scaler, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      mutate(emissions = hybrid_emissions * scaler) %>%
      replace_na(list(emissions = 0)) ->
      L115.nh3_tg_R_G_sec_yr_C_Sys_Fd_pro_emf_hy_tHy_ED_sc_em

    # Rename columns and reshape data
    L115.nh3_tg_R_G_sec_yr_C_Sys_Fd_pro_emf_hy_tHy_ED_sc_em %>%
      select(GCAM_region_ID, Non.CO2, supplysector = GCAM_commodity, subsector = system, stub.technology = feed, year, value = emissions) ->
      L115.nh3_tg_R_G_Sup_sub_Tec_yr_v_final

    # ===================================================

    # Produce outputs
    L115.nh3_tg_R_G_Sup_sub_Tec_yr_v_final %>%
      add_title(" Animal NH3 emissions by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("Annual animal NH3 emissions is computed using EPA emissions factors and FAO animal production.") %>%
      add_comments("EPA emissions are scaled by technology to match EDGAR totals.") %>%
      add_legacy_name("L115.nh3_tg_R_an_C_Sys_Fd_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector",
                     "emissions/mappings/GCAM_sector_tech", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "L105.nh3_tgmt_USA_an_Yh",
                     "emissions/EDGAR/EDGAR_NH3") ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh

    return_data(L115.nh3_tg_R_an_C_Sys_Fd_Yh)
  } else {
    stop("Unknown command")
  }
}
