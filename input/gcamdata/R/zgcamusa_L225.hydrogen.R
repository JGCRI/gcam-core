# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L225.hydrogen
#'
#' Selects the subsectors to be removed from the hydrogen sectors for GCAM USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_USA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_USA.R} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author KD September 2017
module_gcamusa_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2",
             "L201.Pop_GCAMUSA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSupplysector_h2_USA",
             "L225.Supplysector_h2_USA",
             "L225.SectorUseTrialMarket_h2_USA",
             "L225.SubsectorLogit_h2_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTech_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_USA",
             "L225.Supplysector_h2_ind_USA",
             "L225.SubsectorLogit_h2_ind_USA",
             "L225.SubsectorShrwtFllt_h2_ind_USA",
             "L225.TechCoef_h2_ind_USA",
             "L225.TechShrwt_h2_ind_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- sector.name <- subsector.name <- technology <-
      state <- grid_region <- minicam.energy.input <- market.name <- stub.technology <- year <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2", strip_attributes = TRUE)
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2", strip_attributes = TRUE)
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2", strip_attributes = TRUE)
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2", strip_attributes = TRUE)
    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2", strip_attributes = TRUE)
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA", strip_attributes = TRUE)

    # ===================================================

    # Delete the hydrogen sectors from the USA region
    L225.DeleteSupplysector_h2_USA <- L225.Supplysector_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]])

    L225.Supplysector_h2_USA <- L225.Supplysector_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME))

    L225.SectorUseTrialMarket_h2_USA <- L225.SectorUseTrialMarket_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SectorUseTrialMarket"]])

    L225.SubsectorLogit_h2_USA <- L225.SubsectorLogit_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    L225.SubsectorShrwtFllt_h2_USA <- L225.SubsectorShrwtFllt_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    L225.StubTech_h2_USA <- L225.StubTech_h2 %>%
      filter(region == gcam.USA_REGION,
             !(region == 'DC' & subsector %in% c('solar','wind'))) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTech"]]) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind')))

    # Assign the market names. Use the USA region as the default, then
    # - re-set grid-region fuel market
    # - re-set state-level fuel markets
    # - re-set upstream hydrogen commodity markets (hack - this replacement will need to be updated when inter-state hydrogen markets are represented)
    L225.StubTechMarket_h2_USA <- L225.GlobalTechCoef_h2 %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region),
                               by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% L225.Supplysector_h2_USA$supplysector,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% c("water_td_ind_C","water_td_ind_W","trn_freight_road","onshore wind resource","global solar resource"),
                                   region, market.name),
             minicam.energy.input = if_else(minicam.energy.input == 'global solar resource','PV_resource',minicam.energy.input)) %>%
      filter(!(region == 'DC' & subsector %in% c('solar','wind'))) #We should eventually do an anti-join for this but for now it's easier to just say DC

    L225.StubTechMarket_h2_USA %>%
      filter(minicam.energy.input == "PV_resource") %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L225.DeleteStubTechMinicamEnergyInput_H2_USA

    # create "H2 industrial" supplysector in USA region with subsectors/technologies for each state
    L225.Supplysector_h2_ind_USA <- L225.Supplysector_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      mutate(region = gcam.USA_REGION)

    L225.SubsectorLogit_h2_ind_USA <- L225.SubsectorLogit_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,.keep_all=TRUE) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcam.USA_REGION)

    L225.PopShrwts <- L201.Pop_GCAMUSA %>%
      group_by(year) %>%
      mutate(popShrwt = totalPop / sum(totalPop)) %>%
      ungroup()

    # These share-weights are revised each model time period, according to the population share over time.
    # Full_join is used as an expanding join is wanted here (expanding by year)
    L225.SubsectorShrwtFllt_h2_ind_USA <- L225.SubsectorShrwtFllt_h2_USA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,year.fillout,.keep_all=TRUE) %>%
      full_join(L225.PopShrwts, by = c('region')) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcam.USA_REGION,
             share.weight = if_else(as.numeric(share.weight) != 0, round(popShrwt,energy.DIGITS_SHRWT), as.numeric(share.weight)))

    # Full_join is used here in order to expand a global technology table by region (state)
    L225.TechCoef_h2_ind_USA <- L225.GlobalTechCoef_h2 %>%
      filter(sector.name == 'H2 industrial') %>%
      full_join(states_subregions %>%
                  select(state) %>%
                  mutate(sector.name = 'H2 industrial'),by = c('sector.name')) %>%
      distinct(state,year,.keep_all=TRUE) %>%
      mutate(subsector.name = paste0(state,' ',sector.name),
             technology = paste0(state,' ',sector.name),
             minicam.energy.input = 'H2 industrial',
             region = gcam.USA_REGION,
             market.name = state) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    L225.TechShrwt_h2_ind_USA <- L225.TechCoef_h2_ind_USA %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # ===================================================

    # Produce outputs
    L225.DeleteStubTechMinicamEnergyInput_H2_USA %>%
      add_title("Delete global solar resource Energy Input for PV Technologies") %>%
      add_units("NA") %>%
      add_comments("global solar resource input deleted; will be replaced by PV_resource") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L225.DeleteStubTechMinicamEnergyInput_H2_USA") %>%
      add_precursors('L225.Supplysector_h2') ->
      L225.DeleteStubTechMinicamEnergyInput_H2_USA





    L225.DeleteSupplysector_h2_USA %>%
      add_title("Remove hydrogen sectors of USA region for GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("There are no USA hydrogen sectors in GCAM-USA") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.DeleteSupplysector_h2_USA

    L225.Supplysector_h2_USA %>%
      add_title("Supplysector info for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_USA

    L225.SectorUseTrialMarket_h2_USA %>%
      add_title("Supplysector trial market assignments for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SectorUseTrialMarket_h2") ->
      L225.SectorUseTrialMarket_h2_USA

    L225.SubsectorLogit_h2_USA %>%
      add_title("Logit exponents for hydrogen subsectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_USA

    L225.SubsectorShrwtFllt_h2_USA %>%
      add_title("Subsector shareweight fillout for hydrogen subsectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2") ->
      L225.SubsectorShrwtFllt_h2_USA

    L225.StubTech_h2_USA %>%
      add_title("Stub technology pointers for hydrogen sectors in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.StubTech_h2") ->
      L225.StubTech_h2_USA

    L225.StubTechMarket_h2_USA %>%
      add_title("Stub technology market names for inputs to hydrogen technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.StubTechMarket_h2_USA

    L225.Supplysector_h2_ind_USA %>%
      add_title("Add back H2 industrial to USA region") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_ind_USA

    L225.SubsectorLogit_h2_ind_USA %>%
      add_title("State-level logit exponents for H2 industrial in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_ind_USA

    L225.SubsectorShrwtFllt_h2_ind_USA %>%
      add_title("Subsector shareweight fillout for state-level H2 industrial in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights based on relative population in each state") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2",
                     "L201.Pop_GCAMUSA") ->
      L225.SubsectorShrwtFllt_h2_ind_USA

    L225.TechCoef_h2_ind_USA %>%
      add_title("Technology market names for inputs to state-level H2 industrial technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechCoef_h2_ind_USA

    L225.TechShrwt_h2_ind_USA %>%
      add_title("Technology market names for inputs to state-level H2 industrial technologies in GCAM-USA") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechShrwt_h2_ind_USA

    return_data(L225.DeleteSupplysector_h2_USA,
                L225.Supplysector_h2_USA,
                L225.SectorUseTrialMarket_h2_USA,
                L225.SubsectorLogit_h2_USA,
                L225.SubsectorShrwtFllt_h2_USA,
                L225.StubTech_h2_USA,
                L225.StubTechMarket_h2_USA,
                L225.DeleteStubTechMinicamEnergyInput_H2_USA,
                L225.Supplysector_h2_ind_USA,
                L225.SubsectorLogit_h2_ind_USA,
                L225.SubsectorShrwtFllt_h2_ind_USA,
                L225.TechCoef_h2_ind_USA,
                L225.TechShrwt_h2_ind_USA)
  } else {
    stop("Unknown command")
  }
}
