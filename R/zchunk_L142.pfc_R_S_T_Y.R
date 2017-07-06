#' module_emissions_L142.pfc_R_S_T_Y
#'
#' This chunk maps HFC emission shares by region, sector, technology, gas, and year for years 1971-2008.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.pfc_R_S_T_Yh}. The corresponding file in the
#' original data system was \code{L142.pfc_R_S_T_Y.R} (emissions level1).
#' @details First, a table was created that has all historical HFCs values.
#' Then, regional and sector information was added.  And finally, HFC emission shares
#' were calculated by suming emissions over region, sector, technology, and gas by year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL June 2017
module_emissions_L142.pfc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/gcam_fgas_tech",
             FILE = "emissions/other_f_gases",
             FILE = "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector_fgas",
             FILE = "emissions/A41.GWP",
             FILE = "emissions/EDGAR/EDGAR_SF6",
             FILE = "emissions/EDGAR/EDGAR_C2F6",
             FILE = "emissions/EDGAR/EDGAR_CF4"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.pfc_R_S_T_Yh"))
  } else if(command == driver.MAKE) {

    sector <- fuel <- service <- IPCC_Annex <- World_Region <- ISO_A3 <- Name <- IPCC <- IPCC_description <-
      Gas <- agg_sector <- EDGAR_agg_sector <- Sector <- supplysector <- subsector <- stub.technology <-
      EDGAR_emissions <- value.y <- value.x <- share <- emissions <-year <- iso <- awb <- Non.CO2 <-
      GCAM_region_ID <- AWB_emiss_share <- awb_emission <- value <- . <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    GCAM_tech <- get_data(all_data, "emissions/gcam_fgas_tech")
    Other_F <- get_data(all_data, "emissions/other_f_gases")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector_fgas")
    GWP <- get_data(all_data, "emissions/A41.GWP")

    get_data(all_data, "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh")  %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel, -service) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L144.in_EJ_R_bld_serv_F_Yh
    get_data(all_data, "emissions/EDGAR/EDGAR_SF6")  %>%
      gather(year, value, -IPCC_Annex, -World_Region, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 5))) ->
      EDGAR_SF6
    get_data(all_data, "emissions/EDGAR/EDGAR_C2F6") %>%
      gather(year, value, -IPCC_Annex, -World_Region, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 5))) ->
      EDGAR_C2F6
    get_data(all_data, "emissions/EDGAR/EDGAR_CF4") %>%
      gather(year, value, -IPCC_Annex, -World_Region, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 5))) ->
      EDGAR_CF4

    # This chunk maps EDGAR HFC emissions to GCAM technologies
    # First, create a table with all EDGAR HFCs, and prep "Other_F" table for use by renaming column.

    EDGAR_SF6$Non.CO2 <- "SF6"

    EDGAR_C2F6$Non.CO2 <- "C2F6"

    EDGAR_CF4 %>%
      mutate(Non.CO2 = "CF4") %>%
      bind_rows(EDGAR_SF6, EDGAR_C2F6, .) ->
      ALL_EDGAR_HFC

    Other_F %>%
      rename(Non.CO2 = Gas) ->
      Other_F

    # Then, prepare EDGAR data for use.
    # Map in region ID and sector name, and remove year 1970 to match "HISTORICAL_YEARS" constant.

    ALL_EDGAR_HFC %>%
      left_join_error_no_match(EDGAR_sector, by = "IPCC_description") %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%                                        # Switch Romania iso code to its pre-2002 value
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(GCAM_region_ID, EDGAR_agg_sector = agg_sector, Non.CO2, year, value) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L142.EDGAR_HFC

    L142.EDGAR_HFC %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
      summarise(EDGAR_emissions = sum(value)) ->
      L142.EDGAR_PFC_R_S_T_Yh.tmp1

    # Map in other f-gas sector, which varies by gas.
    # Seperate PFCs and add non-CO2 gas information, then recombine.

    L142.EDGAR_PFC_R_S_T_Yh.tmp1 %>%
      filter(EDGAR_agg_sector != "other_f_gases") ->
      L142.EDGAR_PFC_R_S_T_Yh_rest

    L142.EDGAR_PFC_R_S_T_Yh.tmp1 %>%
      filter(EDGAR_agg_sector == "other_f_gases") %>%
      ungroup() %>%
      left_join_error_no_match(Other_F, by = "Non.CO2") %>%
      select(-EDGAR_agg_sector) %>%
      rename(EDGAR_agg_sector = Sector) %>%
      bind_rows(L142.EDGAR_PFC_R_S_T_Yh_rest, .) %>%
      ungroup() ->                                              # ungroup needed for later `left_join_keep_first_only`
      L142.EDGAR_PFC_R_S_T_Yh.tmp1

    # Map PFC gases to GCAM technologies.

    GCAM_tech %>%
      repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
      repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble("Non.CO2" = emissions.PFCS)) %>%
      left_join_keep_first_only(L142.EDGAR_PFC_R_S_T_Yh.tmp1, by = c("GCAM_region_ID", "year", "EDGAR_agg_sector", "Non.CO2")) %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, EDGAR_emissions) %>%
      replace_na(list(EDGAR_emissions = 0)) ->
      L142.pfc_R_S_T_Yh.tmp1

    # Disaggregate cooling emissions to residential and commercial sectors.

    L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(service %in% c("comm cooling", "resid cooling") & fuel == "electricity") ->
      L142.R_cooling_T_Yh

    L142.R_cooling_T_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      L142.R_cooling_Yh

    # Calculate emission shares for residential and commerical sectors.
    # Use 'left_join' because the number of rows is not equal, and NA values are needed.

    L142.R_cooling_Yh %>%
      left_join(L142.R_cooling_T_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(share = value.y / value.x) ->
      L142.R_cooling_T_Yh.tmp1

    # Calculate emissions by share, then sum up emissions over region, sector, technology, and gas by year.

    L142.R_cooling_T_Yh.tmp1 %>%
      rename(supplysector = service) %>%
      right_join(L142.pfc_R_S_T_Yh.tmp1, by = c("GCAM_region_ID", "year", "supplysector")) %>%
      replace_na(list(share = 1)) %>%
      mutate(emissions = EDGAR_emissions * share) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      summarize(emissions = sum(emissions)) %>%
      replace_na(list(emissions = 0)) %>%
      rename(value = emissions) ->
      L142.pfc_R_S_T_Yh

    # Produce outputs

    L142.pfc_R_S_T_Yh %>%
      add_title("HFC emissions by region, sector, technology, gas, and historical year") %>%
      add_units("Gg") %>%
      add_comments("Created a table with all historical HFCs values.") %>%
      add_comments("Regional and sector information was added.") %>%
      add_comments("Calculated HFC emissions by share over region, sector, technology, and gas by year.") %>%
      add_legacy_name("L142.pfc_R_S_T_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/A41.GWP",
                     "emissions/EDGAR/EDGAR_SF6",
                     "emissions/EDGAR/EDGAR_C2F6",
                     "emissions/EDGAR/EDGAR_CF4") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L142.pfc_R_S_T_Yh

    return_data(L142.pfc_R_S_T_Yh)
  } else {
    stop("Unknown command")
  }
}
