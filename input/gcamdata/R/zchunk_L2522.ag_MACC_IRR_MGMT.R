#' module_emissions_L2522.ag_MACC_IRR_MGMT
#'
#' Add new technology data from A_MACC_TechChange to the animal and agricultural marginal abatement cost "MAC" curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2522.AgMAC}, \code{L2522.MAC_Ag_TC_SSP1}, \code{L2522.MAC_An_TC_SSP1}, \code{L2522.MAC_Ag_TC_SSP2}, \code{L2522.MAC_An_TC_SSP2}, \code{L2522.MAC_Ag_TC_SSP5}, \code{L2522.MAC_An_TC_SSP5}. The corresponding file in the
#' original data system was \code{L2522.ag_MACC_IRR_MGMT.R} (emissions level2).
#' @details Add new technology data from A_MACC_TechChange to the animal and agricultural marginal abatement cost "MAC" curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD Aug 2017
module_emissions_L2522.ag_MACC_IRR_MGMT <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_MACC_TechChange",
             "L252.MAC_an",
             "L2521.AgMAC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2522.AgMAC",
             "L2522.MAC_Ag_TC_SSP1",
             "L2522.MAC_An_TC_SSP1",
             "L2522.MAC_Ag_TC_SSP2",
             "L2522.MAC_An_TC_SSP2",
             "L2522.MAC_Ag_TC_SSP5",
             "L2522.MAC_An_TC_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")
    L2521.AgMAC <- get_data(all_data, "L2521.AgMAC")

    # Silence package checks
    AgProductionTechnology <- AgSupplySector <- AgSupplySubsector <-
      MAC <- Non.CO2 <- lvl <- mac.control <- mac.reduction <- region <-
      scenario <- stub.technology <- subsector <- supplysector <- tax <-
      tech_change <- year <- market.name <- NULL

    # ===================================================

    # Add irrigation management level to the marginal abatement cost (MAC) curves for agriculture.
    L2521.AgMAC %>%
      repeat_add_columns(tibble(lvl = c("lo", "hi"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, lvl, sep = "_") ->
      L2522.AgMAC

    # Add technology change data to the agricultural and animal curve technology MAC curves by
    # SSP scenario and mac.control (supplysector) then create separate data frames for each SPP MAC curve.

    # First save a list of all the SSP-sepcific scenarios in the from the MAC curve technology change parameters
    # assumption file.
    A_MACC_TechChange %>%
      select(scenario) %>%
      distinct->
      scenario_list

    # Add SSP scenarios to the the MAC agriculture data frame.
    L2522.AgMAC %>%
      repeat_add_columns(scenario_list) ->
      L2522.AgMAC_all_SSP

    # Add SSP scenarios to the the MAC animal data frame.
    L252.MAC_an %>%
      repeat_add_columns(scenario_list) ->
      L2522.MAC_an_all_SSP

    # Add the technology change to the agricultural and animal MAC curves by matching SSP
    # scenario and mac.control (supplysector).
    #
    # Agricultural MAC curve
    L2522.AgMAC_all_SSP %>%
      left_join_error_no_match(A_MACC_TechChange %>% select(scenario, MAC, tech_change),
                               by = c("scenario", "mac.control" = "MAC")) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2, mac.control,
             tax, mac.reduction, tech.change = tech_change, scenario, market.name) ->
      L2522.AgMAC_all_SSP_tech.change

    # Animal MAC curve
    L2522.MAC_an_all_SSP %>%
      left_join_error_no_match(A_MACC_TechChange %>% select(scenario, MAC, tech_change),
                               by = c("scenario", "mac.control" = "MAC")) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control,
             tax, mac.reduction, tech.change = tech_change, scenario, market.name) ->
      L2522.MAC_an_all_SSP_tech.change

    # Separate the MAC curves by SPP scenario.

    # The filter_ssp function defined here will be used to filter
    # the agricultural and animal MAC curvers for each SSP
    filter_spp <- function(x, ssp) {
      x %>%
        filter(scenario == ssp) %>%
        select(-scenario)
    }

    # Agricultural and animal MAC curves for SSPs
    L2522.MAC_Ag_TC_SSP1 <- filter_spp(L2522.AgMAC_all_SSP_tech.change, ssp = "SSP1")
    L2522.MAC_An_TC_SSP1 <- filter_spp(L2522.MAC_an_all_SSP_tech.change, ssp = "SSP1")
    L2522.MAC_Ag_TC_SSP2 <- filter_spp(L2522.AgMAC_all_SSP_tech.change, ssp = "SSP2")
    L2522.MAC_An_TC_SSP2 <- filter_spp(L2522.MAC_an_all_SSP_tech.change, ssp = "SSP2")
    L2522.MAC_Ag_TC_SSP5 <- filter_spp(L2522.AgMAC_all_SSP_tech.change, ssp = "SSP5")
    L2522.MAC_An_TC_SSP5 <- filter_spp(L2522.MAC_an_all_SSP_tech.change, ssp = "SSP5")


    # ===================================================

    # Produce outputs
    L2522.AgMAC %>%
      add_title("Marginal Abatement Cost Curves for Agriculture with Irrigation Management Level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Add irrigation management level") %>%
      add_legacy_name("L2522.AgMAC") %>%
      add_precursors("emissions/A_MACC_TechChange", "L252.MAC_an", "L2521.AgMAC") ->
      L2522.AgMAC

    L2522.MAC_Ag_TC_SSP1 %>%
      add_title("Marginal Abatement Cost Curves for Agriculture for SSP1") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_Ag_TC_SSP1") %>%
      add_precursors("emissions/A_MACC_TechChange", "L2521.AgMAC") ->
      L2522.MAC_Ag_TC_SSP1

    L2522.MAC_An_TC_SSP1 %>%
      add_title("Marginal Abatement Cost Curves for Animals for SSP1") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_An_TC_SSP1") %>%
      add_precursors("emissions/A_MACC_TechChange", "L252.MAC_an") ->
      L2522.MAC_An_TC_SSP1

    L2522.MAC_Ag_TC_SSP2 %>%
      add_title("Marginal Abatement Cost Curves for Agriculture for SSP2") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_Ag_TC_SSP2") %>%
      add_precursors("emissions/A_MACC_TechChange", "L2521.AgMAC")  ->
      L2522.MAC_Ag_TC_SSP2

    L2522.MAC_An_TC_SSP2 %>%
      add_title("Marginal Abatement Cost Curves for Animals for SSP2") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_An_TC_SSP2") %>%
      add_precursors("emissions/A_MACC_TechChange", "L252.MAC_an") ->
      L2522.MAC_An_TC_SSP2

    L2522.MAC_Ag_TC_SSP5 %>%
      add_title("Marginal Abatement Cost Curves for Agriculture for SSP5") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_Ag_TC_SSP5") %>%
      add_precursors("emissions/A_MACC_TechChange", "L2521.AgMAC")  ->
      L2522.MAC_Ag_TC_SSP5

    L2522.MAC_An_TC_SSP5 %>%
      add_title("Marginal Abatement Cost Curves for Animals for SSP5") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction; tech.change: unitless") %>%
      add_comments("Added tech.change from A_MACC_TechChange") %>%
      add_legacy_name("L2522.MAC_An_TC_SSP5") %>%
      add_precursors("emissions/A_MACC_TechChange", "L252.MAC_an") ->
      L2522.MAC_An_TC_SSP5

    return_data(L2522.AgMAC, L2522.MAC_Ag_TC_SSP1, L2522.MAC_An_TC_SSP1, L2522.MAC_Ag_TC_SSP2, L2522.MAC_An_TC_SSP2, L2522.MAC_Ag_TC_SSP5, L2522.MAC_An_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}
