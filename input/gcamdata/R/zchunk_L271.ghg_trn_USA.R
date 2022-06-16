#' module_gcamusa_L271.ghg_trn_USA
#'
#' Non-CO2 GHG emissions parameters for transportation technologies in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.ghg_trn_tech_coeff_USA}
#' The corresponding file in the original data system was \code{L271.trn_nonCO2_USA.R} (gcam-usa level2).
#' @details Prepare level 2 transportation sector non-CO2 GHG emissions files for USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather
#' @author BY September 2019 / YO 2022

module_gcamusa_L271.ghg_trn_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.en_ghg_emissions",
             "L254.StubTranTechCalInput_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.ghg_trn_tech_coeff_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    supplysector  <- tranSubsector <- stub.technology <- year <- calibrated.value <-
      region <- Non.CO2 <- emiss.coef <- subsector <- input.emissions <- input.name <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes = TRUE)
    L254.StubTranTechCalInput_USA <- get_data(all_data, "L254.StubTranTechCalInput_USA", strip_attributes = TRUE)

    # -------------------------------------------------------------------------------------------
    # Future work: once we have state-level nonCO2 GHGs, we will be perform the scaling process by state
    # For this round we will just match the total USA emissions

    # transportation energy input
    L254.StubTranTechCalInput_total <- L254.StubTranTechCalInput_USA %>%
      group_by(supplysector, tranSubsector, stub.technology, year) %>%
      # use .groups = "drop" to mute a warning message, no change to results
      summarise(calibrated.value = sum(calibrated.value), .groups = "drop") %>%
      ungroup()

    # US total emissions
    L201.en_ghg_emissions_trn <- L201.en_ghg_emissions %>%
      filter(year %in% MODEL_BASE_YEARS & region == gcam.USA_REGION & Non.CO2 %in% emissions.GHG_NAMES) %>%
      rename(tranSubsector = subsector) %>%
      # just keep on-road techs
      semi_join(L254.StubTranTechCalInput_total, by = c("supplysector", "tranSubsector", "stub.technology", "year"))

    # develop national average emission factor and map to all states
    L201.en_ghg_emissions_trn %>%
      left_join_error_no_match(L254.StubTranTechCalInput_total, by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(emiss.coef = input.emissions / calibrated.value) %>%
      filter(emiss.coef > 0 & !is.infinite(emiss.coef) & !(is.na(emiss.coef))) %>%
      select(supplysector, tranSubsector, stub.technology, Non.CO2, emiss.coef, year, input.name) %>%
      write_to_all_states(names = c("region", names(.))) ->
      L271.ghg_trn_tech_coeff_USA

    # ===================================================

    # Produce outputs

    L271.ghg_trn_tech_coeff_USA %>%
      add_title("Non-CO2 GHG transportation emissions coefficients by state / supplysector / tranSubsector / stub.technology / year / Non.CO2") %>%
      add_units("Tg/million pass-km or Tg/million ton-km") %>%
      add_comments("national average emission factor applied to all states") %>%
      add_legacy_name("L271.ghg_trn_tech_coeff_USA") %>%
      add_precursors("L201.en_ghg_emissions",
                     "L254.StubTranTechCalInput_USA") ->
      L271.ghg_trn_tech_coeff_USA

    return_data(L271.ghg_trn_tech_coeff_USA)

  } else {
    stop("Unknown command")
  }
}
