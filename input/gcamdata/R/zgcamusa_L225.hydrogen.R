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
             FILE = "energy/H2ALite_TEAdata",
             FILE = "energy/H2ALite_wind_solar_CF",
             FILE = "energy/mappings/H2ALite_TEA_mapping",
             FILE = "energy/Melaina_h2_water",
             FILE = "gcam-usa/A225.structure",
             "L1233.globaltech_capital_ATB",
             "L1233.globaltech_OMfixed_ATB",
             "L1233.globaltech_capital_ATB_adv",
             "L1233.globaltech_capital_ATB_low",
             "L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSupplysector_h2_USA",
             "L225.Supplysector_h2_USA",
             "L225.SectorUseTrialMarket_h2_USA",
             "L225.SubsectorLogit_h2_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTech_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_USA",
             "L225.StubTechCost_h2_USA_ref",
             "L225.StubTechCost_h2_USA_adv",
             "L225.StubTechCost_h2_USA_lotech",
             "L225.StubTechCoef_h2_USA_ref",
             "L225.StubTechCoef_h2_USA_adv",
             "L225.StubTechCoef_h2_USA_lotech",
             "L225.InterestRate_PADD",
             "L225.Pop_PADD",
             "L225.GDP_PADD",
             "L225.Supplysector_h2_PADD",
             "L225.SubsectorShrwtFllt_h2_PADD",
             "L225.SubsectorShrwt_h2_PADD",
             "L225.SubsectorLogit_h2_PADD",
             "L225.TechShrwt_h2_PADD",
             "L225.TechCoef_h2_PADD"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- sector.name <- subsector.name <- technology <-
      state <- grid_region <- minicam.energy.input <- market.name <- stub.technology <- year <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    H2ALite_TEAdata <- get_data(all_data, "energy/H2ALite_TEAdata")
    H2ALite_TEA_mapping <- get_data(all_data, "energy/mappings/H2ALite_TEA_mapping")
    H2ALite_wind_solar_CF <- get_data(all_data,"energy/H2ALite_wind_solar_CF", strip_attributes = TRUE)
    Melaina_h2_water <- get_data(all_data, "energy/Melaina_h2_water")
    A225.structure <- get_data(all_data, "gcam-usa/A225.structure")
    L1233.globaltech_capital_ATB <- get_data(all_data, "L1233.globaltech_capital_ATB", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB <- get_data(all_data, "L1233.globaltech_OMfixed_ATB", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_adv <- get_data(all_data, "L1233.globaltech_capital_ATB_adv", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_low <- get_data(all_data, "L1233.globaltech_capital_ATB_low", strip_attributes = TRUE)
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2", strip_attributes = TRUE)
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2", strip_attributes = TRUE)
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2", strip_attributes = TRUE)
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2", strip_attributes = TRUE)
    L225.GlobalTechCoef_h2_ref <- get_data(all_data, "L225.GlobalTechCoef_h2_ref", strip_attributes = TRUE)

    # ===================================================

    # A vector of USA PADD region names
    states_subregions %>%
      select(PADD) %>%
      unique %>%
      arrange(PADD) %>%
      unlist ->
      PADD_regions

    # Socioeconomic information in the PADD regions (required for GCAM to run with these regions)

    # L225.InterestRate_PADD: Interest rates in the PADD regions
    tibble(region = PADD_regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L225.InterestRate_PADD

    # L225.Pop_PADD: Population
    tibble(region = PADD_regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L225.Pop_PADD

    # L225.GDP_PADD: GDP in PADD regions
    tibble(region = PADD_regions,
           GDP = 1)  %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))->
      L225.GDP_PADD

    # Supplysector information for H2 passthru T&D sectors in PADD regions
    A225.structure %>%
      select(-region,-market.name) %>%
      repeat_add_columns(tibble(PADD = PADD_regions)) %>%
      left_join(states_subregions, by = c("PADD")) %>%
      mutate(market.name = state,
             subsector = state) %>%
      select(region = PADD,
             supplysector,subsector,technology,minicam.energy.input,market.name,
             subsector.logit,subsector.logit.type,technology.logit,technology.logit.type,
             output.unit,input.unit,price.unit) -> L225.structure_PADD

    # Supplysector info
    L225.structure_PADD %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent = subsector.logit, logit.type) ->
      L225.Supplysector_h2_PADD

    # Subsector (grid region) shareweights in USA electricity
    L225.structure_PADD %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) -> L225.SubsectorShrwtFllt_h2_PADD

    L225.structure_PADD %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES$SubsectorShrwt) -> L225.SubsectorShrwt_h2_PADD


    # NOTE: There is only one tech per subsector in the PADD markets so the logit choice does not matter
    L225.structure_PADD %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(region, supplysector, subsector, logit.year.fillout,
             logit.exponent = technology.logit, logit.type) -> L225.SubsectorLogit_h2_PADD

    # Technology shareweights, USA region
    L225.structure_PADD %>%
      select(region, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) -> L225.TechShrwt_h2_PADD

    # Technology coefficients and market names
    L225.structure_PADD %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = gcamusa.DEFAULT_COEFFICIENT) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input,
             coefficient, market.name) -> L225.TechCoef_h2_PADD

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
      filter(!(region == 'DC' & subsector == "hybrid"))

    L225.SubsectorShrwtFllt_h2_USA <- L225.SubsectorShrwtFllt_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) %>%
      filter(!(region == 'DC' & subsector == "hybrid"))

    L225.StubTech_h2_USA <- L225.StubTech_h2 %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTech"]]) %>%
      filter(!(region == 'DC' & subsector == "hybrid"))

    # Process H2ALite data to generate IO coefs and non-energy costs of inputs, for hybrid technology
    # All other technologies will simply use stub-technology pointers to global tech database
    H2ALite_TEAdata %>%
      filter(Region != "US Average",
             Technology %in% unique(H2ALite_TEA_mapping$TechnologyH2A[H2ALite_TEA_mapping$subsector.name == "hybrid"])) %>%
      left_join_error_no_match(select(states_subregions, state, Region = state_name),
                               by = "Region") %>%
      select(Scenario, state, TechnologyH2A = Technology, year = Year, `Energy-free levelized cost [2022$/kg]`,
             contains('Energy use')) ->
      L225.H2ALite_Hybrid_TEAdata

    # IO coefficients are determined first as they are used to compute renewable electricity generation costs
    # 6/28/25 GPK - modify to include water inputs
    SolarH2_water_m3GJ <- Melaina_h2_water$Value[Melaina_h2_water$Technology == "Solar PV electrolysis"] * CONV_GAL_M3 / CONV_GJ_KGH2
    WindH2_water_m3GJ <- Melaina_h2_water$Value[Melaina_h2_water$Technology == "Wind electrolysis"] * CONV_GAL_M3 / CONV_GJ_KGH2

    L225.StubTechCoef_h2_USA_scen <- L225.H2ALite_Hybrid_TEAdata %>%
      mutate(PV_resource = `Energy use Electricity (Solar) [kWh/kg]`* CONV_KWH_GJ / CONV_GJ_KGH2,
             `onshore wind resource` = `Energy use Electricity (On-shore wind) [kWh/kg]`* CONV_KWH_GJ / CONV_GJ_KGH2,
             water_td_ind_W = (PV_resource * SolarH2_water_m3GJ + `onshore wind resource` * WindH2_water_m3GJ) /
               (PV_resource + `onshore wind resource`),
             water_td_ind_C = water_td_ind_W) %>%
      select(Scenario, region = state, TechnologyH2A, year, PV_resource, `onshore wind resource`, water_td_ind_W, water_td_ind_C) %>%
      tidyr::gather(key = "minicam.energy.input", value = "coefficient", -Scenario, -region, -TechnologyH2A, -year) %>%
      complete(nesting(Scenario, region, TechnologyH2A, minicam.energy.input), year = MODEL_YEARS) %>%
      group_by(Scenario, region, TechnologyH2A, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,  rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      mutate(market.name = region) %>%
      inner_join(H2ALite_TEA_mapping, by = "TechnologyH2A") %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["StubTechCoef"]]))

    # Cost calculations
    L225.StubTechCost_h2_USA_scen <- L225.H2ALite_Hybrid_TEAdata %>%
      mutate(input.cost = `Energy-free levelized cost [2022$/kg]` * gdp_deflator(1975,2022) / CONV_GJ_KGH2,
             minicam.non.energy.input = "other non-energy") %>%
      inner_join(H2ALite_TEA_mapping, by = "TechnologyH2A") %>%
      select(Scenario, region = state, supplysector = sector.name, subsector = subsector.name,
             stub.technology = technology, year, minicam.non.energy.input, input.cost) %>%
      complete(nesting(Scenario, region, supplysector, subsector, stub.technology, minicam.non.energy.input),
               year = MODEL_YEARS) %>%
      group_by(Scenario, region, supplysector, subsector, stub.technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, input.cost,  rule = 2), energy.DIGITS_COST)) %>%
      ungroup() %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["StubTechCost"]]))

    # Calculate and bind in the costs of generating renewable electricity in each state
    # Renewable electricity cost = solar LCOE * solar IOcoef + wind LCOE * wind IOcoef
    L225.CapacityFactor_USA <- H2ALite_wind_solar_CF %>%
      filter(State != "US Average") %>%
      left_join_error_no_match(select(states_subregions, state, State = state_name),
                               by = "State") %>%
      rename(wind = Wind_CF, solar = Solar_CF) %>%
      select(region = state, wind, solar) %>%
      gather(key = subsector, value = capacity.factor, -region)

    L225.RenewElec_cost_USA_scen <- L1233.globaltech_capital_ATB %>%
      mutate(Scenario = "med") %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_adv, Scenario = "low")) %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_low, Scenario = "high")) %>%
      filter(technology %in% c("wind", "PV")) %>%
      left_join(L1233.globaltech_OMfixed_ATB, by = c("supplysector", "subsector", "technology", "year")) %>%
      left_join(L225.CapacityFactor_USA, by = c("subsector")) %>%
      mutate(elec_cost_75USD_GJ = (capital.overnight * calc_fixed_charge_rate(interest.rate, payback.years) + OM.fixed) /
               (CONV_YEAR_HOURS * capacity.factor * CONV_KWH_GJ)) %>%
      select(Scenario, region, renew_tech = subsector, year, elec_cost_75USD_GJ)

    # Join in the costs to the coefficient table (which indicates electricity IO coefs), multiply,
    # and aggregate to get the total renewable-electric non-energy cost
    L225.StubTechCost_h2_renewelec_USA_scen <- L225.StubTechCoef_h2_USA_scen %>%
      mutate(renew_tech = if_else(minicam.energy.input == "PV_resource", "solar",
                                  if_else(minicam.energy.input == "onshore wind resource", "wind", "drop"))) %>%
      filter(renew_tech != "drop") %>%
      left_join(L225.RenewElec_cost_USA_scen, by = c("Scenario", "region", "renew_tech", "year")) %>%
      mutate(minicam.non.energy.input = "renewable electricity generation",
             input.cost = coefficient * elec_cost_75USD_GJ) %>%
      group_by(Scenario, region, supplysector, subsector, stub.technology, year, minicam.non.energy.input) %>%
      summarise(input.cost = sum(input.cost)) %>%
      ungroup()

    L225.StubTechCost_h2_USA_scen <- L225.StubTechCost_h2_USA_scen %>%
      bind_rows(L225.StubTechCost_h2_renewelec_USA_scen) %>%
      mutate(input.cost = round(input.cost,energy.DIGITS_COST))

    # Because Alaska and Hawaii are not in the H2ALite data, they would inherit the global tech defaults if not include specifically.
    # Because the global tech defaults (from "US Average") are from near-optimal siting in the US, they aren't appropriate here
    # IO coefs: NH has the lowest solar coefficients of any state, so copying it. HI will use FL.
    L225.StubTechCoef_h2_USA_scen <- bind_rows(
      L225.StubTechCoef_h2_USA_scen,
      mutate(filter(L225.StubTechCoef_h2_USA_scen, region == "NH"),
             region = "AK", market.name = "AK"),
      mutate(filter(L225.StubTechCoef_h2_USA_scen, region == "FL"),
             region = "HI", market.name = "HI")
    )

    # Split the IOcoef data into scenario-specific tables
    L225.StubTechCoef_h2_USA_ref <- filter(L225.StubTechCoef_h2_USA_scen, Scenario == "med") %>%
      select(-Scenario)
    L225.StubTechCoef_h2_USA_adv <- filter(L225.StubTechCoef_h2_USA_scen, Scenario == "low") %>%
      select(-Scenario)
    L225.StubTechCoef_h2_USA_lotech <- filter(L225.StubTechCoef_h2_USA_scen, Scenario == "high") %>%
      select(-Scenario)

    # AK/HI costs: assign Florida's costs to both, as FL costs are near at/near the upper limit among the states
    L225.StubTechCost_h2_USA_scen <- bind_rows(
      L225.StubTechCost_h2_USA_scen,
      mutate(filter(L225.StubTechCost_h2_USA_scen, region == "FL"), region = "AK"),
      mutate(filter(L225.StubTechCost_h2_USA_scen, region == "FL"), region = "HI")
    )

    # Split costs by scenario for data write-out
    L225.StubTechCost_h2_USA_ref <- filter(L225.StubTechCost_h2_USA_scen, Scenario == "med") %>%
      select(-Scenario)
    L225.StubTechCost_h2_USA_adv <- filter(L225.StubTechCost_h2_USA_scen, Scenario == "low") %>%
      select(-Scenario)
    L225.StubTechCost_h2_USA_lotech <- filter(L225.StubTechCost_h2_USA_scen, Scenario == "high") %>%
      select(-Scenario)

    # Assign the market names to all hydrogen technologies. Use the USA region as the default, then
    # - re-set grid-region fuel market
    # - re-set state-level fuel markets
    # - re-set upstream hydrogen commodity markets
    L225.StubTechMarket_h2_USA <- L225.GlobalTechCoef_h2_ref %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region,PADD),
                               by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% L225.Supplysector_h2_USA$supplysector,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% c("water_td_ind_C","water_td_ind_W","trn_freight_road","onshore wind resource","global solar resource"),
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.H2_TD_MARKETS,PADD,market.name),
             minicam.energy.input = if_else(minicam.energy.input == 'global solar resource','PV_resource',minicam.energy.input)) %>%
      filter(!(region == 'DC' & subsector == "hybrid")) #We should eventually do an anti-join for this but for now it's easier to just say DC

    L225.StubTechMarket_h2_USA %>%
      filter(minicam.energy.input == "PV_resource") %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L225.DeleteStubTechMinicamEnergyInput_H2_USA

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
                     "L225.GlobalTechCoef_h2_ref") ->
      L225.StubTechMarket_h2_USA

    L225.StubTechCost_h2_USA_ref %>%
      add_title("State-level green hydrogen production costs (reference scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/H2ALite_TEAdata",
                     "energy/H2ALite_wind_solar_CF",
                     "energy/mappings/H2ALite_TEA_mapping",
                     "L1233.globaltech_capital_ATB",
                     "L1233.globaltech_OMfixed_ATB") ->
      L225.StubTechCost_h2_USA_ref

    L225.StubTechCost_h2_USA_adv %>%
      add_title("State-level green hydrogen production costs (adv scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) %>%
      add_precursors("L1233.globaltech_capital_ATB_low") ->
      L225.StubTechCost_h2_USA_adv

    L225.StubTechCost_h2_USA_lotech %>%
      add_title("State-level green hydrogen production costs (lotech scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) %>%
      add_precursors("L1233.globaltech_capital_ATB_adv") ->
      L225.StubTechCost_h2_USA_lotech

    L225.StubTechCoef_h2_USA_ref %>%
      add_title("State-level green hydrogen IO coefficients (ref scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("GJelec/GJh2 inputs of wind and solar to hybrid technology") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) %>%
      add_precursors("energy/Melaina_h2_water") ->
      L225.StubTechCoef_h2_USA_ref

    L225.StubTechCoef_h2_USA_adv %>%
      add_title("State-level green hydrogen IO coefficients (adv scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("GJelec/GJh2 inputs of wind and solar to hybrid technology") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) ->
      L225.StubTechCoef_h2_USA_adv

    L225.StubTechCoef_h2_USA_lotech %>%
      add_title("State-level green hydrogen IO coefficients (lotech scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("GJelec/GJh2 inputs of wind and solar to hybrid technology") %>%
      same_precursors_as(L225.StubTechCost_h2_USA_ref) ->
      L225.StubTechCoef_h2_USA_lotech

    L225.InterestRate_PADD %>%
      add_title("Interest rates in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L225.InterestRate_PADD") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.InterestRate_PADD

    L225.Pop_PADD %>%
      add_title("Population in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L225.Pop_PADD") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.Pop_PADD

    L225.GDP_PADD %>%
      add_title("GDP in PADD regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L225.GDP_PADD

    L225.Supplysector_h2_PADD %>%
      #add_title("PADD region Hydrogen T&D Passthrough Sector Information")
      add_units("unitless") %>%
      add_comments("Supply sector information for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.Supplysector_h2_PADD") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A225.structure") ->
      L225.Supplysector_h2_PADD

    L225.SubsectorLogit_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Logits") %>%
      add_units("unitless") %>%
      add_comments("Subsector logits for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorLogit_h2_PADD

    L225.SubsectorShrwtFllt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorShrwtFllt_h2_PADD

    L225.SubsectorShrwt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for hydrogen T&D passthrough sectors in the PADD regions at points of inflexion") %>%
      same_precursors_as("L225.Supplysector_h2_PADD") ->
      L225.SubsectorShrwt_h2_PADD

    L225.TechShrwt_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Technology Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Technology share weights for hydrogen T&D passthrough sectors in the PADD regions") %>%
      add_legacy_name("L225.TechShrwt_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD_USA") ->
      L225.TechShrwt_h2_PADD

    L225.TechCoef_h2_PADD %>%
      #add_title("PADD hydrogen T&D Passthrough Technology Market Info") %>%
      add_units("unitless") %>%
      add_comments("Hydrogen T&D passthrough technology coefficients and market names in the PADD regions") %>%
      add_legacy_name("L225.TechCoef_h2_PADD") %>%
      same_precursors_as("L225.Supplysector_h2_PADD_USA") ->
      L225.TechCoef_h2_PADD

    return_data(L225.DeleteSupplysector_h2_USA,
                L225.Supplysector_h2_USA,
                L225.SectorUseTrialMarket_h2_USA,
                L225.SubsectorLogit_h2_USA,
                L225.SubsectorShrwtFllt_h2_USA,
                L225.StubTech_h2_USA,
                L225.StubTechMarket_h2_USA,
                L225.DeleteStubTechMinicamEnergyInput_H2_USA,
                L225.StubTechCost_h2_USA_ref,
                L225.StubTechCost_h2_USA_adv,
                L225.StubTechCost_h2_USA_lotech,
                L225.StubTechCoef_h2_USA_ref,
                L225.StubTechCoef_h2_USA_adv,
                L225.StubTechCoef_h2_USA_lotech,
                L225.InterestRate_PADD,
                L225.Pop_PADD,
                L225.GDP_PADD,
                L225.Supplysector_h2_PADD,
                L225.SubsectorShrwtFllt_h2_PADD,
                L225.SubsectorShrwt_h2_PADD,
                L225.SubsectorLogit_h2_PADD,
                L225.TechShrwt_h2_PADD,
                L225.TechCoef_h2_PADD)
  } else {
    stop("Unknown command")
  }
}
