# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_trn_scenarios_xml
#'
#' Construct XML data structure for \code{transport_LowTech.xml},
#' \code{transport_AdvTech.xml},
#' \code{transport_gcamusa_LowTech.xml}, \code{transport_gcamusa_AdvTech.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{transport_LowTech.xml},
#'  \code{transport_AdvTech.xml}, \code{transport_gcamusa_LowTech.xml},
#'  \code{transport_gcamusa_AdvTech.xml}.
module_gcamusa_trn_scenarios_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/trn_tech_scenarios",
             FILE = "gcam-usa/states_subregions",
             "L254.StubTranTechCoef_USA",
             "L254.StubTranTechCoef",
             "L254.StubTranTechCost",
             "L254.StubTranTechCost_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transport_LowTech.xml",
             XML = "transport_AdvTech.xml",
             XML = "transport_gcamusa_LowTech.xml",
             XML = "transport_gcamusa_AdvTech.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trn_tech_scenarios <- get_data(all_data, "energy/trn_tech_scenarios")
    L254.StubTranTechCoef_USA <- get_data(all_data, "L254.StubTranTechCoef_USA", strip_attributes = TRUE)
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef", strip_attributes = TRUE)
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost", strip_attributes = TRUE)
    L254.StubTranTechCost_USA <- get_data(all_data, "L254.StubTranTechCost_USA", strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions",strip_attributes = TRUE)

    # ===================================================

    # Process tables at the USA region level to the states level.
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.USA_REGION) %>%
        write_to_all_states(names = c(names(data), "region"))

      # Re-set markets from USA to grid region, if the minicam.energy.input is considered a regional fuel market
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      # For fuels consumed from state markets, the market.name is the region
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                       region[minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS]))
      }

      data_new
    }

    # Define the scenario differentiation year (model time period)
    TECH_SCENARIO_DIVERGENCE_YEAR <- 2025
    TECH_SCENARIO_TERMINAL_YEAR <- 2050
    TECH_SCENARIO_YEAR <- MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= TECH_SCENARIO_DIVERGENCE_YEAR &
                                         MODEL_FUTURE_YEARS <= TECH_SCENARIO_TERMINAL_YEAR]

    # Go through the scenarios table variable-by-variable and scenario-by-scenario
    # 1. Technology share-weights
    # Note that these aren't the actual share-weight paths which use interpolation rules
    # The values here will be used as the "to-value" of the corresponding technology's interpolation rule
    # and held constant thereafter
    Trn_Shrwt_Scen <- filter(trn_tech_scenarios,
                           !is.na(technology),
                           variable == "share.weight") %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, share.weight = value) %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      repeat_add_columns(tibble(year = TECH_SCENARIO_YEAR)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], scenario))

    L254.GlobalTechShrwt_trn_LowTech <- filter(Trn_Shrwt_Scen, scenario == "LowTech")
    L254.GlobalTechShrwt_trn_AdvTech <- filter(Trn_Shrwt_Scen, scenario == "HiTech")

    # 2. Global technology share-weight interpolation rules
    GlobalTechInterpTo_Scen <- filter(trn_tech_scenarios, variable %in% c("from.year", "to.value", "to.year", "interpolation.function")) %>%
      select(scenario, sector.name = supplysector, subsector.name = subsector, technology, variable, value, rule_number) %>%
      spread(key = variable, value = value) %>%
      mutate(apply.to = "share.weight",
             from.year = as.numeric(from.year),
             to.value = as.numeric(to.value),
             to.year = as.integer(to.year)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]], scenario))

    L254.GlobalTechInterpTo_LowTech <- filter(GlobalTechInterpTo_Scen, scenario == "LowTech")
    L254.GlobalTechInterpTo_AdvTech <- filter(GlobalTechInterpTo_Scen, scenario == "HiTech")

    # 3. Energy intensity adjustment
    Coef_Scen <- filter(trn_tech_scenarios,
                          !is.na(adj_factor),
                          variable == "coefficient") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCoef_Scen <- Coef_Scen %>%
      left_join(L254.StubTranTechCoef,
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(coefficient = round(coefficient * adj_factor, digits = energy.DIGITS_COEFFICIENT)) %>%
      filter(year >= TECH_SCENARIO_DIVERGENCE_YEAR) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCoef"]], scenario))

    L254.StubTranTechCoef_Scen %>%
      mutate(coefficient = round(coefficient, digits = gcamusa.DIGITS_TRNUSA_DEFAULT)) %>%
      process_USA_to_states ->
      L254.StubTranTechCoef_USA_Scen

    L254.StubTranTechCoef_LowTech <- filter(L254.StubTranTechCoef_Scen, scenario == "LowTech")
    L254.StubTranTechCoef_AdvTech <- filter(L254.StubTranTechCoef_Scen, scenario == "HiTech")

    L254.StubTranTechCoef_USA_LowTech <- filter(L254.StubTranTechCoef_USA_Scen, scenario == "LowTech")
    L254.StubTranTechCoef_USA_AdvTech <- filter(L254.StubTranTechCoef_USA_Scen, scenario == "HiTech")

    # 4. Capital costs adjustment
    Cost_Scen <- filter(trn_tech_scenarios,
                          !is.na(adj_factor),
                          variable == "input.cost") %>%
      select(scenario, supplysector, tranSubsector = subsector, stub.technology = technology, year = value, adj_factor) %>%
      mutate(year = as.numeric(year))

    L254.StubTranTechCost_Scen <- Cost_Scen %>%
      left_join(L254.StubTranTechCost,
                by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(input.cost = round(input.cost * adj_factor, digits = energy.DIGITS_COST)) %>%
      filter(year >= TECH_SCENARIO_DIVERGENCE_YEAR) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTranTechCost"]], scenario))

    process_USA_to_states(L254.StubTranTechCost_Scen) -> L254.StubTranTechCost_USA_Scen

    L254.StubTranTechCost_LowTech <- filter(L254.StubTranTechCost_Scen, scenario == "LowTech")
    L254.StubTranTechCost_AdvTech <- filter(L254.StubTranTechCost_Scen, scenario == "HiTech")

    L254.StubTranTechCost_USA_LowTech <- filter(L254.StubTranTechCost_USA_Scen, scenario == "LowTech")
    L254.StubTranTechCost_USA_AdvTech <- filter(L254.StubTranTechCost_USA_Scen, scenario == "HiTech")

    # Produce outputs
    create_xml("transport_LowTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_LowTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_LowTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_LowTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_LowTech, "StubTranTechCoef") %>%
      add_precursors("energy/trn_tech_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_LowTech.xml

    create_xml("transport_AdvTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_AdvTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_AdvTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_AdvTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_AdvTech, "StubTranTechCoef") %>%
      add_precursors("energy/trn_tech_scenarios",
                     "L254.StubTranTechCoef",
                     "L254.StubTranTechCost") ->
      transport_AdvTech.xml

    create_xml("transport_gcamusa_LowTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_LowTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_LowTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_LowTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_LowTech, "StubTranTechCoef") %>%
      add_precursors("energy/trn_tech_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_gcamusa_LowTech.xml

    create_xml("transport_gcamusa_AdvTech.xml") %>%
      add_xml_data(L254.GlobalTechInterpTo_AdvTech, "GlobalTechInterpTo") %>%
      add_xml_data(L254.GlobalTechShrwt_trn_AdvTech, "GlobalTechShrwt") %>%
      add_xml_data(L254.StubTranTechCost_USA_AdvTech, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_AdvTech, "StubTranTechCoef") %>%
      add_precursors("energy/trn_tech_scenarios",
                     "gcam-usa/states_subregions",
                     "L254.StubTranTechCoef_USA",
                     "L254.StubTranTechCost_USA") ->
      transport_gcamusa_AdvTech.xml


    return_data(transport_LowTech.xml,
                transport_AdvTech.xml,
                transport_gcamusa_LowTech.xml,
                transport_gcamusa_AdvTech.xml)
  } else {
    stop("Unknown command")
  }
}
