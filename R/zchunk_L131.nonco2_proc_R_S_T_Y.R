#' module_emissions_L131.nonco2_proc_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.nonco2_tg_R_prc_S_S_Yh}. The corresponding file in the
#' original data system was \code{L131.nonco2_proc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L131.nonco2_proc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR_sector",
             FILE = "emissions/EPA_ghg_tech",
             #FILE = "emissions/EDGAR_nation", #####  GET RID OF? TOUPPER IN ##### MERGE WITH ISO_GCAM_regID
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

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR_sector")
    EPA_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
    #EDGAR_nation <- get_data(all_data, "emissions/EDGAR_nation")
    GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR_NH3")
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR_CH4")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR_N2O")
    EDGAR_NMVOC <- get_data(all_data, "emissions/EDGAR_NMVOC")
    EDGAR_NOx <- get_data(all_data, "emissions/EDGAR_NOx")
    EDGAR_SO2 <- get_data(all_data, "emissions/EDGAR_SO2")
    EDGAR_CO <- get_data(all_data, "emissions/EDGAR_CO")
    EPA_Ind <- get_data(all_data, "emissions/EPA_FCCC_IndProc_2005")

    #Perform computations

    # Compute subsector share of sectoral emissions using EPA data
    # Compute share of sectoral emissions in each subsector using EPA data

     EPA_Ind %>%
       left_join(EPA_tech, by = "Source_Category") %>%
       rename(subsector = fuel) %>%
       select(-EPA_Source_Category_Raw, -Source_Category) %>%   #Remove unnecessary columns
       gather(Non.CO2, value, -sector,-subsector) %>%  #make a new column of non.co2
       filter(complete.cases(.)) %>%  #delete rows with NAs
       group_by(sector,subsector,Non.CO2) %>% #group by sector, subsector and Non CO2
       summarise(x=sum(value)) -> #sum the values
       L131.EPA_nonco2_indproc.melt

 #Compute subsector share of sectoral total
     GCAM_sector_tech %>%
       select(supplysector, subsector, stub.technology, EPA_agg_sector, EPA_agg_fuel_ghg, EDGAR_agg_sector) %>%
       filter(EDGAR_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater","aerosols",
                                      "metals","foams","solvents", "semiconductors")) %>%
       repeat_add_columns(tibble::tibble(Non.CO2=c("CH4","N2O","NMVOC","NOx","SO2","CO","VOC"))) ->
       L131.nonco2_pct_R_prc_S_S_2005

   # Group by and then join by sector, subsector, and Non.CO2.  However, to join the columns they have to have
     # the same name. Rename column to tech_emissions.
     L131.nonco2_pct_R_prc_S_S_2005 %>%
       group_by(EPA_agg_sector,EPA_agg_fuel_ghg,Non.CO2) %>%
       left_join(L131.EPA_nonco2_indproc.melt, by=c(EPA_agg_sector="sector", EPA_agg_fuel_ghg = "subsector",
                                                    "Non.CO2")) %>%
       rename(tech_emissions = x) %>%
       mutate(tech_emissions=if_else(is.na(tech_emissions),1,tech_emissions)) %>%
       group_by(EPA_agg_sector,Non.CO2) %>% #group by sector, and Non CO2
       summarise(tech_emissions=sum(tech_emissions))->
       L131.nonco2_pct_R_prc_S_S_2005

 # #old code
 #
      L131.nonco2_gg_R_prc_S_2005 <- aggregate( L131.nonco2_pct_R_prc_S_S_2005$tech_emissions,
                                                by = L131.nonco2_pct_R_prc_S_S_2005[ c( "EPA_agg_sector", "Non.CO2" ) ], sum )
      L131.nonco2_pct_R_prc_S_S_2005$sector_emissions <- L131.nonco2_gg_R_prc_S_2005$x[
        match( vecpaste(L131.nonco2_pct_R_prc_S_S_2005[ c( "EPA_agg_sector", "Non.CO2" )] ),
               vecpaste( L131.nonco2_gg_R_prc_S_2005[ c( "EPA_agg_sector", "Non.CO2" )]))]
    L131.nonco2_pct_R_prc_S_S_2005$tech_share <- with( L131.nonco2_pct_R_prc_S_S_2005, tech_emissions / sector_emissions )


      # Produce outputs
  # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR_sector",
                     "emissions/EPA_ghg_tech",
                    # "emissions/EDGAR_nation",
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
