# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA162.dac
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
module_energy_LA162.dac <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A62.calibration"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L162.out_Mt_R_dac_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    . <- Biomass <- Biomass_EJ <- Coal <- Coal_EJ <- Country <- GCAM_region_ID <- Gas <- Gas_EJ <-
    IEA_fuelshare_region <- IEA_intensity_region <- IOelec <- Oil <- Oil_EJ <- TPE_GJkg <-
    Worrell_region <- cement_prod_Mt <- country_name <- elec_EJ <- elec_GJkg <-
    emiss_ktC <- fuel <- heat_EJ <- heat_GJkg <- in.value <- ind.value <- iso <-
    old.year <- out.value <- process_emissions_MtC <- process_emissions_ktC <-
    prod_Mt <- prod_emiss_ratio <- reg_process_emissions <- region_GCAM3 <- sector <-
    share <- value <- cement <- year <- value.y <- value.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs

    ces_calibration <- get_data(all_data, "energy/A62.calibration")


    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------

    ces_calibration -> L162.out_Mt_R_dac_Yh


    L162.out_Mt_R_dac_Yh %>%
      add_units("Mt C") %>%
      add_comments("Outputs are calculated by simply transposing the calibration matrix, which contains arbitrarily high values for dac+noDAC") %>%
      add_legacy_name("L162.out_Mt_R_dac_Yh") %>%
      add_precursors("energy/A62.calibration") ->
      L162.out_Mt_R_dac_Yh


    return_data(L162.out_Mt_R_dac_Yh)
  } else {
    stop("Unknown command")
  }
}
