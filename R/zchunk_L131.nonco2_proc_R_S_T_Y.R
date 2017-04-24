#' module_emissions_L131.nonco2_proc_R_S_T_Y
#'
#' Calculate historical emissions from the processing sector by GCAM technology,computed from EDGAR emissions data and EPA emissions factors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.nonco2_tg_R_prc_S_S_Yh}. The corresponding file in the
#' original data system was \code{L131.nonco2_proc_R_S_T_Y.R} (emissions level1).
#' @details This chunck calculated the historical emissions from the processing sector by GCAM technology,
#' computed from EDGAR emissions data and EPA emissions factors. First: Compute subsector share of sectoral emissions using EPA data.
#' Second: Compute share of sectoral emissions in each subsector using EPA data. Third: Group by and then join by sector, subsector,
#' and Non.CO2. Fourth: Disaggregate EDGAR emissions to subsectors. Fifth: map in all data and compute emissions (EDGAR emissions*tech_share)
#' Note: emissions/EDGAR_nation was a file of capitlized ISO codes.  I now call 'toupper' to change the iso codes to capitals.
#' Deleting this dependcy.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CH April 2017
#' @export
module_emissions_L131.nonco2_proc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR_sector",
             FILE = "emissions/EPA_ghg_tech",
             FILE = "emissions/GCAM_sector_tech",
             FILE = "emissions/EDGAR_NH3",
             FILE = "emissions/EDGAR_CH4",
             FILE = "emissions/EDGAR_N2O",
             FILE = "emissions/EDGAR_NMVOC",
             FILE = "emissions/EDGAR_NOx",
             FILE = "emissions/EDGAR_SO2",
             FILE = "emissions/EDGAR_CO",
             FILE = "emissions/EPA_FCCC_IndProc_2005"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.nonco2_tg_R_prc_S_S_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs, set to long format and change column names where needed

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR_sector")
    EPA_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
    EPA_Ind <- get_data(all_data, "emissions/EPA_FCCC_IndProc_2005")

    # EDGAR emissions data
    get_data(all_data, "emissions/EDGAR_NH3") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='NH3') ->
      EDGAR_NH3
    get_data(all_data, "emissions/EDGAR_CH4") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='CH4')->
      EDGAR_CH4
    get_data(all_data, "emissions/EDGAR_N2O") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='N2O')->
      EDGAR_N2O
    EDGAR_VOC <- get_data(all_data, "emissions/EDGAR_NMVOC") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='NMVOC')->
      EDGAR_VOC
    get_data(all_data, "emissions/EDGAR_NOx") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='NOx')->
      EDGAR_NOx
    get_data(all_data, "emissions/EDGAR_SO2") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='SO2')->
      EDGAR_SO2
    get_data(all_data, "emissions/EDGAR_CO") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(Non.CO2='CO')->
      EDGAR_CO


    ### Perform computations  ###

    # First: Compute subsector share of sectoral emissions using EPA data

    EPA_Ind %>%
      left_join(EPA_tech, by = "Source_Category") %>%
      rename(subsector = fuel) %>%
      select(-EPA_Source_Category_Raw, -Source_Category) %>%   # Remove unnecessary columns
      gather(Non.CO2, value, -sector,-subsector) %>%  # make a new column called Non.co2
      na.omit() %>%  # delete rows with NAs
      group_by(sector,subsector,Non.CO2) %>% # group by sector, subsector and Non.CO2
      summarise(x=sum(value)) -> #sum the values
      L131.EPA_nonco2_indproc.mlt

    # Second: Compute share of sectoral emissions in each subsector using EPA data
    GCAM_sector_tech %>%
      select(supplysector, subsector, stub.technology, EPA_agg_sector, EPA_agg_fuel_ghg, EDGAR_agg_sector) %>%
      filter(EDGAR_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater","aerosols",
                                     "metals","foams","solvents", "semiconductors")) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=c("CH4","N2O","NMVOC","NOx","SO2","CO","VOC"))) ->
      L131.nonco2_pct_R_prc_S_S_2005

    # Third: Group by and then join by sector, subsector, and Non.CO2.  However, to join the columns they have to have
    # the same name. Rename column to tech_emissions. Tech_emissions is the sum and tech_share is the tech_emissions/sector_emissions
    L131.nonco2_pct_R_prc_S_S_2005 %>%
      group_by(EPA_agg_sector,EPA_agg_fuel_ghg,Non.CO2) %>%
      left_join(L131.EPA_nonco2_indproc.mlt, by=c(EPA_agg_sector="sector", EPA_agg_fuel_ghg = "subsector","Non.CO2")) %>%
      rename(tech_emissions = x) %>%
      mutate(tech_emissions=if_else(is.na(tech_emissions),1,tech_emissions)) ->
      L131.nonco2_pct_R_prc_S_S_2005

    L131.nonco2_pct_R_prc_S_S_2005 %>%
      group_by(EPA_agg_sector, Non.CO2) %>% #group by sector, and Non CO2
      summarise(tech_emissions=sum(tech_emissions)) ->
      L131.nonco2_gg_R_prc_S_2005


    L131.nonco2_pct_R_prc_S_S_2005 %>%
      group_by(EPA_agg_sector, Non.CO2) %>%
      left_join(L131.nonco2_gg_R_prc_S_2005, by =c("EPA_agg_sector", "Non.CO2")) %>%
      rename(sector_emissions = tech_emissions.y ) %>% #sector emisisons from thr join
      rename(tech_emissions = tech_emissions.x) %>%
      mutate(tech_share = tech_emissions/sector_emissions) ->
      L131.nonco2_pct_R_prc_S_S_2005

    # Fourth: Disaggregate EDGAR emissions to subsectors
    # First aggregate all EDGAR data and subset for processing sectors

    #filter out years 2009 and 2010 in EDGAR_VOC
    EDGAR_VOC %>%
      filter(year!= "2009" & year != "2010") ->
      EDGAR_VOC

    #rather than read in a new file (EDGAR_nation) let's make a new file
    iso_GCAM_regID <- mutate_all(iso_GCAM_regID, funs(toupper))


    # combine all of the nonco2s together in one file
    bind_rows(EDGAR_CH4, EDGAR_N2O, EDGAR_VOC, EDGAR_NOx, EDGAR_SO2, EDGAR_CO, EDGAR_NH3) %>%
      left_join(EDGAR_sector, by=c("IPCC_description", "IPCC")) %>%
      left_join(iso_GCAM_regID, by=c(ISO_A3 = "iso")) %>%
      filter(agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater","aerosols",
                               "metals","foams","solvents", "semiconductors")) %>%
      select(year,value, Non.CO2,agg_sector,GCAM_region_ID) %>%
      filter(complete.cases(.)) %>%  #delete rows with NAs
      group_by(GCAM_region_ID, year, Non.CO2, agg_sector) %>%
      summarise(EDGAR_emissions=sum(value)) %>%
      mutate(EDGAR_emissions=EDGAR_emissions * CONV_GG_TG) -> #convert from gg to tg
      L131.EDGAR.mlt

    L131.EDGAR.mlt$GCAM_region_ID <- as.integer(L131.EDGAR.mlt$GCAM_region_ID)
    L131.EDGAR.mlt$year <- as.integer(L131.EDGAR.mlt$year)

    # Fifth: map in all data and compute emissions (EDGAR_emissions * tech_share)
    GCAM_sector_tech %>%
      select(supplysector, subsector, stub.technology, EDGAR_agg_sector, EPA_agg_sector, EPA_agg_fuel_ghg) %>%
      filter(EDGAR_agg_sector %in% c("industry_processes" ,"chemicals", "landfills", "wastewater", "aerosols",
                                     "metals", "foams", "solvents", "semiconductors")) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=c("CH4","N2O","NMVOC","NOx","SO2","CO","VOC"))) %>%
      group_by(supplysector, subsector, stub.technology, Non.CO2) %>%
      left_join(L131.nonco2_pct_R_prc_S_S_2005, by = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      repeat_add_columns(tibble::tibble(year= c(emissions.EDGAR_YEARS))) %>%
      repeat_add_columns(tibble::tibble(GCAM_region_ID = c(GCAM_region_names$GCAM_region_ID))) %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector.x, Non.CO2, year) %>%
      left_join(L131.EDGAR.mlt, by = c("GCAM_region_ID", "EDGAR_agg_sector.x" ="agg_sector", "Non.CO2", "year")) %>%
      na.omit() %>%  #delete rows with NAs
      mutate(input.emissions = EDGAR_emissions * tech_share) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      summarise(input.emissions=sum(input.emissions)) %>%

      # Produce outputs\
       add_title("GHG emissions by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("Calculate historical emissions from the processing sector by GCAM
                   technology,computed from EDGAR emissions data and EPA emissions factors.") %>%
      add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR_sector",
                     "emissions/EPA_ghg_tech",
                     "emissions/GCAM_sector_tech",
                     "emissions/EDGAR_NH3",
                     "emissions/EDGAR_CH4",
                     "emissions/EDGAR_N2O",
                     "emissions/EDGAR_NMVOC",
                     "emissions/EDGAR_NOx",
                     "emissions/EDGAR_SO2",
                     "emissions/EDGAR_CO",
                     "emissions/EPA_FCCC_IndProc_2005") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.nonco2_tg_R_prc_S_S_Yh

    return_data(L131.nonco2_tg_R_prc_S_S_Yh)
  } else {
    stop("Unknown command")
  }
}
