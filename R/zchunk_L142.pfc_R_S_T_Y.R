#' module_emissions_L142.pfc_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.pfc_R_S_T_Yh}. The corresponding file in the
#' original data system was \code{L142.pfc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL June 2017
#' @export
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

    year <- iso <- awb <- Non.CO2 <- GCAM_region_ID <- AWB_emiss_share <-
      awb_emission <- value <- . <- NULL # silence package check.

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
    # First, create a table with all EDGAR HFCs, and prepare "Other_F" gases by renaming column.

    EDGAR_SF6 %>%
      mutate(Non.CO2 = "SF6") ->
      EDGAR_SF6

    EDGAR_C2F6 %>%
      mutate(Non.CO2 = "C2F6") ->
      EDGAR_C2F6

    EDGAR_CF4 %>%
      mutate(Non.CO2 = "CF4") %>%
      bind_rows(EDGAR_SF6,EDGAR_C2F6) ->
      ALL_EDGAR_HFC

    Other_F %>%
      rename(Non.CO2 = Gas) ->
      Other_F

    # Then, prepare EDGAR data for use.
    # Map in region ID and sector name, and remove year 1970 to match "HISTORICAL_YEARS" constant

    ALL_EDGAR_HFC %>%
      left_join_error_no_match(EDGAR_sector, by = "IPCC_description") %>%     # Add GCAM sector from the sector mapping
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou','rom') %>%                                        # Switch Romania iso code to its pre-2002 value
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%
      rename(EDGAR_agg_sector = agg_sector) %>%
      select(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year, value)  %>%
      filter(year !="1970") ->
      L142.EDGAR_PFC

    L142.EDGAR_PFC %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
      summarize(value = sum(value))  %>%
      rename(EDGAR_emissions = value) ->
      L142.EDGAR_PFC_R_S_T_Yh.melt

   # Map in other f-gas sector, which varies by gas.
   # Seperate

   L142.EDGAR_PFC_R_S_T_Yh.melt %>%
     filter(EDGAR_agg_sector != "other_f_gases") ->
     L142.EDGAR_PFC_R_S_T_Yh_rest

   L142.EDGAR_PFC_R_S_T_Yh.melt %>%
     filter(EDGAR_agg_sector == "other_f_gases")  %>%
     left_join(Other_F, by = "Non.CO2")   %>%
     ungroup() %>%
     select(-EDGAR_agg_sector)  %>%
     rename(EDGAR_agg_sector = Sector)  %>%
     bind_rows(L142.EDGAR_PFC_R_S_T_Yh_rest) ->
     L142.EDGAR_PFC_R_S_T_Yh.melt

  #Map to GCAM technologies

  GCAM_tech %>%
    repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
    repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
    repeat_add_columns(tibble("Non.CO2" = emissions.PFCs))  %>%
    ungroup() %>%
    left_join(L142.EDGAR_PFC_R_S_T_Yh.melt, by = c("GCAM_region_ID", "year", "EDGAR_agg_sector", "Non.CO2"))  %>%
    select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, EDGAR_emissions)  %>%
    mutate(EDGAR_emissions = (replace(EDGAR_emissions, is.na(EDGAR_emissions), 0))) ->
    L142.pfc_R_S_T_Yh.melt

  #Disaggregate cooling emissions to residential and commercial sectors

  L144.in_EJ_R_bld_serv_F_Yh %>%
    filter(service %in% c("comm cooling", "resid cooling") & fuel == "electricity") ->
    L142.R_cooling_T_Yh

  L142.R_cooling_T_Yh %>%
    group_by(GCAM_region_ID, year)  %>%
    summarize(value = sum(value)) ->
    L142.R_cooling_Yh

  L142.R_cooling_Yh %>%
    left_join(L142.R_cooling_T_Yh, by = c("GCAM_region_ID", "year"))  %>%
    mutate(share = value.y /value.x) ->
    L142.R_cooling_T_Yh.melt

  # Resh
  L142.R_cooling_T_Yh.melt %>%
    rename(supplysector = service)  %>%
    right_join(L142.pfc_R_S_T_Yh.melt, by = c("GCAM_region_ID", "year", "supplysector")) %>%
    mutate(share = (replace(share,is.na(share),1)))  %>%
    mutate(emissions = EDGAR_emissions * share)  %>%
    group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
    summarize(emissions = sum(emissions))  %>%
    mutate_all(funs(replace(., is.na(.), 0))) ->
    L142.pfc_R_S_T_Yh

#     # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
 #   tibble() %>%
  L142.pfc_R_S_T_Yh %>%
      add_title("HFC emissions by region, sector, technology, gas, and historical year") %>%
      add_units("Gg") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
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
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.pfc_R_S_T_Yh

    return_data(L142.pfc_R_S_T_Yh)
  } else {
    stop("Unknown command")
  }
 }
