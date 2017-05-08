#' module_emissions_L131.nonco2_proc_R_S_T_Y
#'
#' Calculate historical emissions from the processing sector by GCAM technology, computed from EDGAR emissions data and EPA emissions factors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.nonco2_tg_R_prc_S_S_Yh}. The corresponding file in the
#' original data system was \code{L131.nonco2_proc_R_S_T_Y.R} (emissions level1).
#' @details This chunk calculates the historical emissions from the processing sector by GCAM technology,
#' computed from EDGAR emissions data and EPA emissions factors. First: Compute subsector share of sectoral emissions using EPA data.
#' Second: Compute share of sectoral emissions in each subsector using EPA data. Third: Group by and then join by sector, subsector,
#' and Non.CO2. Fourth: Disaggregate EDGAR emissions to subsectors. Fifth: map in all data and compute emissions (EDGAR emissions*tech_share)
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CH May 2017
module_emissions_L131.nonco2_proc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
  #           FILE = "emissions/EDGAR/EDGAR_nation",
             FILE = "emissions/EPA_ghg_tech",
             FILE = "emissions/GCAM_sector_tech",
             "EDGAR_gases",
             FILE = "emissions/EPA_FCCC_IndProc_2005"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.nonco2_tg_R_prc_S_S_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs, set to long format and change column names where needed

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
   # EDGAR_nation <- get_data(all_data, "emissions/EDGAR/EDGAR_nation")
    EPA_ghg_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
    EPA_Ind <- get_data(all_data, "emissions/EPA_FCCC_IndProc_2005")
    EDGAR_gases <- get_data(all_data, "EDGAR_gases")

    ### Perform computations  ###

    # First: Compute subsector share of industrial sectoral emissions using EPA data for the USA

    EPA_Ind %>%
      left_join(EPA_ghg_tech, by = "Source_Category") %>%
      rename(subsector = fuel) %>%
      select(-EPA_Source_Category_Raw, -Source_Category) %>%   # Remove unnecessary columns
      unique() %>%
      gather(Non.CO2, value, -sector, -subsector) %>%  # make a new column called Non.co2
      na.omit() %>%  # delete rows with NAs
      group_by(sector, subsector, Non.CO2) %>% # group by sector, subsector and Non.CO2
      summarise(x = sum(value)) -> # sum the values
      L131.EPA_nonco2_indproc.mlt

    # Second: Compute share of sectoral emissions in each subsector using EPA data for the USA
    # Group by and then join by sector, subsector, and Non.CO2.  However, to join the columns they have to have
    # the same name. Rename column to tech_emissions. Tech_emissions is the sum and tech_share is the tech_emissions/sector_emissions

    GCAM_sector_tech %>%
      select(supplysector, subsector, stub.technology, EPA_agg_sector, EPA_agg_fuel_ghg, EDGAR_agg_sector) %>%
      filter(EDGAR_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater", "aerosols",
                                     "metals", "foams", "solvents", "semiconductors")) %>%
      repeat_add_columns(tibble(Non.CO2 = c("CH4", "N2O", "NMVOC", "NOx", "SO2", "CO", "VOC"))) %>%
      group_by(EPA_agg_sector, EPA_agg_fuel_ghg, Non.CO2) %>%
      left_join(L131.EPA_nonco2_indproc.mlt, by = c(EPA_agg_sector = "sector", EPA_agg_fuel_ghg = "subsector", "Non.CO2")) %>%
      rename(tech_emissions = x) %>%
      mutate(tech_emissions = if_else(is.na(tech_emissions), 1, tech_emissions)) -> # set missing values to 1, emissions coef will be overwritten below.
      L131.nonco2_pct_R_prc_S_S_2005

    L131.nonco2_pct_R_prc_S_S_2005 %>%
      group_by(EPA_agg_sector, Non.CO2) %>% # group by sector, and Non CO2
      summarise(tech_emissions = sum(tech_emissions)) -> # calculate tech_emissions per agg sector
      L131.nonco2_gg_R_prc_S_2005

    L131.nonco2_pct_R_prc_S_S_2005 %>%
      group_by(EPA_agg_sector, Non.CO2) %>%
      left_join(L131.nonco2_gg_R_prc_S_2005, by = c("EPA_agg_sector", "Non.CO2")) %>%
      rename(sector_emissions = tech_emissions.y) %>%
      mutate(tech_share = tech_emissions.x / sector_emissions) -> # calculate tech_share
      L131.nonco2_pct_R_prc_S_S_2005


    # Third: Disaggregate EDGAR emissions to subsectors
    # First aggregate all EDGAR data and subset for processing sectors

    EDGAR_gases %>%
      # filter out years 2009 and 2010 for NMVOCs only
      filter(Non.CO2 == "NMVOC", ! year %in% c(2009, 2010)) %>%
      bind_rows(filter(EDGAR_gases, Non.CO2 != "NMVOC")) %>%
      left_join(EDGAR_sector, by = c("IPCC_description", "IPCC")) %>%
      rename(EDGAR_agg_sector = agg_sector) %>%
      # left_join_keep_first_only(EDGAR_nation, by = "ISO_A3") %>%
      mutate( iso = tolower( ISO_A3 ), ISO_A3 = NULL ) %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      filter(EDGAR_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater", "aerosols",
                                     "metals", "foams", "solvents", "semiconductors")) %>%
      select(year,value, Non.CO2, EDGAR_agg_sector, GCAM_region_ID) %>%
      na.omit()  %>%  # delete rows with NA's
      group_by(year, Non.CO2, EDGAR_agg_sector, GCAM_region_ID) %>%
      summarise(EDGAR_emissions = sum(value)) %>%
      mutate(EDGAR_emissions = EDGAR_emissions * CONV_GG_TG) -> # convert from gg to tg
      L131.EDGAR.mlt

    # Fourth: Map in all data and compute emissions (EDGAR_emissions * tech_share).

    GCAM_sector_tech %>%
      select(supplysector, subsector, stub.technology, EDGAR_agg_sector, EPA_agg_sector, EPA_agg_fuel_ghg) %>%
      filter(EDGAR_agg_sector %in% c("industry_processes" , "chemicals", "landfills", "wastewater", "aerosols",  # Filter for the agg sectors in EDGAR for all NonCO2s.
                                     "metals", "foams", "solvents", "semiconductors")) %>%
      repeat_add_columns(tibble(Non.CO2 = c("CH4", "N2O", "NMVOC", "NOx", "SO2", "CO", "VOC"))) %>%
      group_by(supplysector, subsector, stub.technology, Non.CO2) %>%
      left_join(L131.nonco2_pct_R_prc_S_S_2005,  # Then combine with the EPA sector information
                by = c("supplysector", "subsector", "stub.technology",
                       "Non.CO2", "EPA_agg_sector", "EDGAR_agg_sector", "EPA_agg_fuel_ghg")) %>%
      repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
      left_join(L131.EDGAR.mlt, by = c("GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "year")) %>%
      na.omit() %>%  # delete rows with NA's
      mutate(input.emissions = EDGAR_emissions * tech_share) %>%  # Calculate emissions
      select(-sector_emissions, -tech_emissions.x) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      summarise(value = sum(input.emissions)) %>% # Calculate total emissions

      # in order to match old data we have to turn the data in wide format
      # which introduces NA's that are then converted to 0. Convert back to long format for new data system.
      spread(year, value) %>%
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(value = if_else(is.na(value), 0, value)) %>%

      # Produce outputs
      add_title("GHG emissions by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("Calculate historical emissions from the processing sector by GCAM, ") %>%
      add_comments("technology computed from EDGAR emissions data and EPA emissions factors.") %>%
      add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector",
                   #  "emissions/EDGAR/EDGAR_nation",
                     "emissions/EPA_ghg_tech",
                     "emissions/GCAM_sector_tech",
                     "EDGAR_gases",
                     "emissions/EPA_FCCC_IndProc_2005") %>%
      add_flags(FLAG_SUM_TEST,FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.nonco2_tg_R_prc_S_S_Yh

    return_data(L131.nonco2_tg_R_prc_S_S_Yh)
  } else {
    stop("Unknown command")
  }
}
