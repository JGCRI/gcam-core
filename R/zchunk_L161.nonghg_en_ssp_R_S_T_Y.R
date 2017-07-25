#' module_emissions_L161.nonghg_en_ssp_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.SSP15_EF}, \code{L161.SSP2_EF}, \code{L161.SSP34_EF}. The corresponding file in the
#' original data system was \code{L161.nonghg_en_ssp_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH July 2017
#' @export
module_emissions_L161.nonghg_en_ssp_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/gains_to_gcam_sector", # source
             FILE = "emissions/GAINS_activities", # source
             FILE = "emissions/GAINS_emissions", # source
             "L102.pcgdp_thous90USD_Scen_R_Y",
             FILE = "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
             "L114.bcoc_tgej_R_en_S_F_2000"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.SSP15_EF",
             "L161.SSP2_EF",
             "L161.SSP34_EF"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "emissions/A_regions")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    GAINS_sector <- get_data(all_data, "emissions/mappings/gains_to_gcam_sector")
    GAINS_activities <- get_data(all_data, "emissions/GAINS_activities")
    GAINS_emissions <- get_data(all_data, "emissions/GAINS_emissions") %>%
      # NOTE: these are three different scenarios
      # CLE = current legislation, SLE = stringent legislation, MFR = maximum feasible reductions
      gather(variable, value, CLE, MFR, SLE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X"))
    L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000")

    # ===================================================

    # Aggregate GAINS emissions data by GCAM sector
    GAINS_emissions_agg <- GAINS_emissions %>%
      # Change pollutant names
      mutate(POLL = replace(POLL, POLL == "NOX", "NOx"),
             POLL = replace(POLL, POLL == "VOC", "NMVOC")) %>%
      # Use left_join because NAs in GAINS_sector
      left_join(GAINS_sector, by = c("TIMER_SECTOR" = "IIASA_Sector")) %>%
      group_by(TIMER_REGION, agg_sector = GCAM_tag, POLL, IDYEARS, variable) %>%
      summarise(value = sum(value)) %>%
      na.omit %>%
      ungroup

    # Aggregate GAINS activity data by GCAM sector
    GAINS_activities_agg <- GAINS_activities %>%
      # Use left_join because NAs in GAINS_sector
      left_join(GAINS_sector, by = c("TIMER_SECTOR" = "IIASA_Sector")) %>%
      group_by(TIMER_REGION, agg_sector = GCAM_tag, IDYEARS) %>%
      summarise(ACT = sum(ACT)) %>%
      na.omit %>%
      ungroup

    # Compute emissions factors by dividing GAINS activity by GAINS emissions
    GAINS_emfact <- GAINS_emissions_agg %>%
      left_join_error_no_match(GAINS_activities_agg, by = c("TIMER_REGION", "agg_sector", "IDYEARS")) %>%
      mutate(emfact = value / ACT) %>%
      select(TIMER_REGION, agg_sector, POLL, IDYEARS, variable, emfact) %>%
      group_by(TIMER_REGION, agg_sector, POLL, IDYEARS) %>%
      # Replace SLE & MFR base year (2005) emissions factors with CLE emissions factors.
      # They don't all start from the same value.
      mutate(CLE_base = emfact[variable == "CLE"]) %>%
      ungroup() %>%
      mutate(emfact = replace(emfact, IDYEARS == emissions.GAINS_BASE_YEAR, CLE_base[IDYEARS == emissions.GAINS_BASE_YEAR])) %>%
      select(-CLE_base)

    # Compute emissions factor scaler.
    # These scalers are relative to the previous time period's numbers.
    GAINS_emfact_scaler <- GAINS_emfact %>%
      # Get rid of any years before base year
      filter(IDYEARS >= emissions.GAINS_BASE_YEAR) %>%
      group_by(TIMER_REGION, agg_sector, POLL, variable) %>%
      mutate(scaler = emfact / lag(emfact, n = 1L, order_by = IDYEARS)) %>%
      ungroup() %>%
      # Set base year scaler equal to 1 and don't allow any value greater than 1
      mutate(scaler = replace(scaler, IDYEARS == emissions.GAINS_BASE_YEAR, 1),
             scaler = replace(scaler, scaler > 1, 1))

# NOTE: This code converts gdp using a conv_xxxx_xxxx_USD constant
# Use the `gdp_deflator(year, base_year)` function instead
    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.SSP15_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP15_EF
      tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.SSP2_EF") %>%
      add_precursors("emissions/A_regions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP2_EF
      tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.SSP34_EF") %>%
      add_precursors("emissions/A_regions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP34_EF

    return_data(L161.SSP15_EF, L161.SSP2_EF, L161.SSP34_EF)
  } else {
    stop("Unknown command")
  }
}



