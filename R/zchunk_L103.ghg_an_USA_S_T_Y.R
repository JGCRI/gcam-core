#' module_emissions_L103.ghg_an_USA_S_T_Y
#'
#' Calculates methane emissions factors for animals by GCAM technology,
#' computed from EPA emissions data and FAO animal data for the US, 2005
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.ghg_tgmt_USA_an_Sepa_F_2005}. The corresponding file in the
#' original data system was \code{L103.ghg_an_USA_S_T_Y.R} (emissions level1).
#' @details Calculated methane emissions factors for animal production by GCAM technology (animal type) from EPA
#' emissions estimates and FAO production data for the US in 2005.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH April 2017
module_emissions_L103.ghg_an_USA_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EPA_ghg_tech",
             FILE = "emissions/GCAM_sector_tech",
             FILE = "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             FILE = "emissions/EPA_FCCC_AG_2005"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.ghg_tgmt_USA_an_Sepa_F_2005"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EPA_ghg_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    EPA_FCCC_AG_2005 <- get_data(all_data, "emissions/EPA_FCCC_AG_2005")

    # Map EPA ghg emissions to GCAM sector and fuel,
    # aggregate and convert from Gg to Tg
    # EPA data contains estimates for aggregate and disaggregate sectors. There should be NAs when
    # mapping to GCAM sector and fuel to avoid double counting those totals.

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
    # Old system yields an emissions estimate of 0 for poultry because it has NA values.
    EPA_FCCC_AG_2005 %>%
      left_join(EPA_ghg_tech, by = "Source_Category") %>%
      group_by(sector, fuel) %>%
      summarize_if(is.numeric , sum) %>%
      filter( !is.na(sector), !is.na(fuel)) %>%
      mutate_all(funs(replace(., is.na(.), 0))) %>%
      mutate_if(is.numeric, funs(. * CONV_GG_TG)) ->
        L103.ghg_tg_USA_an_Sepa_F_2005

    } else {
      # Corrected poltry emissions estimate, methane emissions for poultry are relatively small
      # compared to other animals, but non-zero
    EPA_FCCC_AG_2005 %>%
        left_join(EPA_ghg_tech, by = "Source_Category") %>%
        group_by(sector, fuel) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE) %>%
        filter(!is.na(sector), !is.na(fuel)) %>%
        mutate_all( funs( replace(., is.na(.), 0))) %>%
        mutate_if(is.numeric, funs(. * CONV_GG_TG)) ->
        L103.ghg_tg_USA_an_Sepa_F_2005
    }

    # Map FAO production to EPA sectors and aggregate
    # Select region - US and year - 2005
    # The old data system uses US methane emission factors for 2005 for all historical values in all regions.
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      left_join_error_no_match( select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel),
                 by = c("GCAM_commodity" = "sector", "system" = "fuel" , "feed" = "technology")) %>%
      ungroup %>%
      mutate(year = as.numeric(year)) %>%
      filter(year == 2005, GCAM_region_ID == 1) %>%
      group_by(EPA_agg_sector, year) %>%
      select(EPA_agg_sector, year, value) %>%
      summarise_if(is.numeric, sum) ->
      L107.an_Prod_US_Sepa_2005

    # Calculate CH4 emissions factor
    L103.ghg_tg_USA_an_Sepa_F_2005 %>%
      left_join(L107.an_Prod_US_Sepa_2005, by = c("sector" = "EPA_agg_sector")) %>%
      mutate(ch4_em_factor = CH4 / value) %>%
      select(sector, fuel, ch4_em_factor) %>%
      ungroup() ->
      L103.ghg_tgmt_USA_an_Sepa_F_2005

    # Produce outputs
    L103.ghg_tgmt_USA_an_Sepa_F_2005 %>%
      add_title("Methane emission factors for animal production estimated from EPA and FAO data for US in 2005") %>%
      add_units("Tg/Mt") %>%
      add_comments("Methane emission factors for animal production by animal type from EPA GHG inventory and FAO animal production data for the US in 2005 in Tg per Mt") %>%
      add_legacy_name("L103.ghg_tgmt_USA_an_Sepa_F_2005") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/EPA_ghg_tech",
                     "emissions/GCAM_sector_tech",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "emissions/EPA_FCCC_AG_2005") ->
      L103.ghg_tgmt_USA_an_Sepa_F_2005

    return_data(L103.ghg_tgmt_USA_an_Sepa_F_2005)
  } else {
    stop("Unknown command")
  }
}
