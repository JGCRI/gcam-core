# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L162.dac
#'
#' Sets up input, output, and IO coefficients for dac
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L162.out_Mt_R_dac_Yh},
#' @details This chunk defines historical demand for the climate engineering services sector, setting up structure for future competition.
#' For all historical years, is met entirely by "no DAC" technology
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr complete gather nesting
#' @author JF March 2021
module_energy_L162.dac <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L161.RsrcCurves_MtC_R"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L162.out_Mt_R_dac_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    Cstorage <- GCAM_region_ID <- sector <- year <- value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L161.RsrcCurves_MtC_R <- get_data(all_data, "L161.RsrcCurves_MtC_R")


    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------

    L161.RsrcCurves_MtC_R %>%
      group_by(GCAM_region_ID) %>%
      summarise(Cstorage = sum(available)) %>%
      ungroup() %>%
      mutate(sector = "CO2 removal",
             year = max(MODEL_BASE_YEARS),
             value = Cstorage / Cstorage[GCAM_region_ID == gcam.USA_CODE] * energy.DAC_LIMIT_USA_MTC) %>%
      select(GCAM_region_ID, sector, year, value) ->
      L162.out_Mt_R_dac_Yh

    L162.out_Mt_R_dac_Yh %>%
      add_title("Calibration of DAC in each region") %>%
      add_units("Mt C") %>%
      add_comments("Calculated from each region's total onshore CO2 storage, indexed to an exogenous DAC limit assigned to the USA") %>%
      add_legacy_name("L162.out_Mt_R_dac_Yh") %>%
      add_precursors("L161.RsrcCurves_MtC_R") ->
      L162.out_Mt_R_dac_Yh


    return_data(L162.out_Mt_R_dac_Yh)
  } else {
    stop("Unknown command")
  }
}
