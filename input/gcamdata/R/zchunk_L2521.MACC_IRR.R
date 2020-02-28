# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L2521.MACC_IRR
#'
#' Adds technologies and tech changes to marginal abatement cost curves, MACC, for animals and agriculture.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2521.AgMAC}, \code{L2521.MAC_Ag_TC_SSP1}, \code{L2521.MAC_An_TC_SSP1}, \code{L2521.MAC_Ag_TC_SSP2}, \code{L2521.MAC_An_TC_SSP2}, \code{L2521.MAC_Ag_TC_SSP5}, \code{L2521.MAC_An_TC_SSP5}. The corresponding file in the
#' original data system was \code{L2521.MACC_IRR.R} (emissions level2).
#' @details Adds agricultural technology to agriculture marginal abatement cost curves, MACC. Adds SSP-specific tech changes to animal and agriculture MAC curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join mutate select
#' @author RH July 2017
module_emissions_L2521.MACC_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_MACC_TechChange",
             "L252.AgMAC",
             "L252.MAC_an"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2521.AgMAC",
             "L2521.MAC_Ag_TC_SSP1",
             "L2521.MAC_An_TC_SSP1",
             "L2521.MAC_Ag_TC_SSP2",
             "L2521.MAC_An_TC_SSP2",
             "L2521.MAC_Ag_TC_SSP5",
             "L2521.MAC_An_TC_SSP5"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    AgSupplySubsector <- EPA_region <- Irr_Rfd <- scenario <- tech_change <- region <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange") %>%
      rename(tech.change = tech_change)
    L252.AgMAC <- get_data(all_data, "L252.AgMAC")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")

    # ===================================================
    # L2521.AgMAC: Ag MAC curves are simply copied by irr and rfd technologies
    L2521.AgMAC <- L252.AgMAC %>%
      repeat_add_columns(tibble(Irr_Rfd = c("IRR", "RFD"))) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = "_")) %>%
      select(-Irr_Rfd)

    # Tech Change on Ag MACCs for all available SSPs
    L2521.MAC_Ag_TC <- L2521.AgMAC %>%
      # Using left_join because the number of rows change - there is a value for each SSP
      left_join(A_MACC_TechChange, by = c("mac.control" = "MAC")) %>%
      # Split by scenario and add attributes since these are the same except for SSP name
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title(paste("Agricultural marginal abatement cost curves with technology changes for ", unique(df$scenario))) %>%
          add_legacy_name(paste0("L2521.MAC_Ag_TC_", unique(df$scenario))) %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Appends scenario-specific tech change to Ag MACC curves") %>%
          add_precursors("L252.AgMAC", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })

    # Tech Change on animal MACCs for all available SSPs
    L2521.MAC_An_TC <- L252.MAC_an %>%
      # Using left_join because the number of rows change - there is a value for each SSP
      left_join(A_MACC_TechChange, by = c("mac.control" = "MAC")) %>%
      # Split by scenario and add attributes since these are the same except for SSP name
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title(paste("Animal marginal abatement cost curves with technology changes for ", unique(df$scenario))) %>%
          add_legacy_name(paste0("L2521.MAC_An_TC_", unique(df$scenario))) %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Appends scenario-specific tech change to animal MACC curves") %>%
          add_precursors("L252.MAC_an", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })

    # ===================================================

    # Produce outputs
    L2521.AgMAC %>%
      add_title("Agricultural Marginal Abatement Cost Curves") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Adds irrigation and rainfed technology to each subsector in L252.AgMAC") %>%
      add_legacy_name("L2521.AgMAC") %>%
      add_precursors("L252.AgMAC") ->
      L2521.AgMAC
    L2521.MAC_Ag_TC[["SSP1"]] ->
      L2521.MAC_Ag_TC_SSP1
    L2521.MAC_An_TC[["SSP1"]]  ->
      L2521.MAC_An_TC_SSP1
    L2521.MAC_Ag_TC[["SSP2"]]  ->
      L2521.MAC_Ag_TC_SSP2
    L2521.MAC_An_TC[["SSP2"]] ->
      L2521.MAC_An_TC_SSP2
    L2521.MAC_Ag_TC[["SSP5"]] ->
      L2521.MAC_Ag_TC_SSP5
    L2521.MAC_An_TC[["SSP5"]]  ->
      L2521.MAC_An_TC_SSP5

    return_data(L2521.AgMAC, L2521.MAC_Ag_TC_SSP1, L2521.MAC_An_TC_SSP1, L2521.MAC_Ag_TC_SSP2, L2521.MAC_An_TC_SSP2, L2521.MAC_Ag_TC_SSP5, L2521.MAC_An_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}
