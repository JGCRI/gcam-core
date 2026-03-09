# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L125.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.globaltech_coef},  \code{L125.globaltech_cost},
#' \code{L125.StubTechCost_h2_hybrid_scen}, \code{L125.StubTechCoef_h2_hybrid_scen}.
#' @details Takes inputs from H2A-Lite and generates GCAM's assumptions by technology and year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select if_else
#' @importFrom tidyr complete nesting
#' @author YZ/GPK April 2025
#'
#'
#' # Note:  NREL H2A v2018 did not include the following H2 production technologies:
#           bio + CCS, coal w/o CCS, coal + CCS (future), nuclear H2 prod,
#           solar electrolysis, and wind electrolysis. See in line comments below for further detail.
# ------------------------------------------------------------------------------
#'
module_energy_L125.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/H2ALite_TEAdata",
             FILE = "energy/H2ALite_wind_solar_CF",
             FILE = "energy/Melaina_h2_water",
             FILE = "energy/mappings/H2ALite_TEA_mapping",
             "L223.GlobalTechCapFac_elec",
             "L1233.globaltech_capital_ATB",
             "L1233.globaltech_capital_ATB_adv",
             "L1233.globaltech_capital_ATB_low",
             "L1233.globaltech_OMfixed_ATB",
             "L1233.globaltech_OMvar_ATB",
             "L223.StubTechCapFactor_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.globaltech_coef_scen",
             "L125.globaltech_cost_scen",
             "L125.nuclear_hydrogen_costs_adv",
             "L125.nuclear_hydrogen_costs_low",
             "L125.StubTechCost_h2_hybrid_scen",
             "L125.StubTechCoef_h2_hybrid_scen"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- technology <- capacity.factor <- NULL  # silence package check notes

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    H2ALite_TEAdata <- get_data(all_data, "energy/H2ALite_TEAdata")
    H2ALite_TEA_mapping <- get_data(all_data, "energy/mappings/H2ALite_TEA_mapping")
    H2ALite_wind_solar_CF <- get_data(all_data,"energy/H2ALite_wind_solar_CF", strip_attributes = TRUE)
    Melaina_h2_water <- get_data(all_data, "energy/Melaina_h2_water")

    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB <- get_data(all_data, "L1233.globaltech_capital_ATB", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_adv <- get_data(all_data, "L1233.globaltech_capital_ATB_adv", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_low <- get_data(all_data, "L1233.globaltech_capital_ATB_low", strip_attributes = TRUE)
    L1233.globaltech_OMvar_ATB <- get_data(all_data, "L1233.globaltech_OMvar_ATB", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB <- get_data(all_data, "L1233.globaltech_OMfixed_ATB", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)

    # ===================================================

    # Process data

    H2ALite_TEAdata %>%
      filter(Region=="US Average",
             Technology %in% unique(H2ALite_TEA_mapping$TechnologyH2A)) %>%
      select(Year, Scenario, Technology,`Energy-free levelized cost [2022$/kg]`, contains('Energy use')) %>%
      rename(TechnologyH2A=Technology,
             year=Year) %>%
      fill(`Energy-free levelized cost [2022$/kg]`, contains('Energy use'), .direction="down") ->
      L125.H2ALite_TEAdata

    # cost data: convert units and expand to all model years
    # for hybrid and nuclear electrolysis, these costs do not include the costs of self-generated electricity
    # this is denoted by using "other non-energy" as the name of the non-energy input cost
    # for onsite production, these costs do not include on-site compression, refrigeration, and storage, which are added
    # by A25.globaltech_cost. This is denoted by using "production" as the name of the NE cost
    L125.globaltech_cost_scen <- L125.H2ALite_TEAdata %>%
      inner_join(H2ALite_TEA_mapping, by = "TechnologyH2A") %>%
      mutate(input.cost = `Energy-free levelized cost [2022$/kg]` * cost_multiplier * gdp_deflator(1975,2022) / CONV_GJ_KGH2) %>%
      select(Scenario, sector.name, subsector.name, technology, year, input.cost) %>%
      complete(nesting(Scenario, sector.name, subsector.name, technology), year = MODEL_YEARS) %>%
      mutate(minicam.non.energy.input = if_else(subsector.name %in% c("hybrid", "nuclear"), "other non-energy", "non-energy"),
             minicam.non.energy.input = if_else(subsector.name == "onsite production", "production", minicam.non.energy.input),
             units="$1975/GJ H2") %>%
      group_by(Scenario, sector.name, subsector.name, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost,  rule = 2)) %>%
      ungroup()

    # Input-output coefficients are indicated in individually named columns (one column per fuel) in H2ALite_TEAdata
    # These are translated to GCAM's input names and coefficients prior to gathering, in order to apply fuel-specific conversion factors
    # Zero values (really NA's) are filtered out, and the data are extrapolated to all model years
    # Technology names are assigned from the mapping table, and the nuclear technology's input is switched from electricity to nuclear fuel, multiplying by 3
    # This table is finished with the exception of renewable electricity inputs, for the hybrid renewable self-generation technology
    L125.globaltech_coef_scen<- L125.H2ALite_TEAdata %>%
      mutate(`regional natural gas` = `Energy use Natural Gas (Industrial) [mmBTU HHV/kg]` * CONV_NG_HHV_LHV * CONV_BTU_KJ / CONV_GJ_KGH2,
             `regional biomass` = `Energy use Biomass [short dry ton/kg]`/ CONV_T_METRIC_SHORT * aglu.BIO_ENERGY_CONTENT_GJT / CONV_GJ_KGH2,
             elect_td_ind = `Energy use Electricity (Industrial) [kWh/kg]`* CONV_KWH_GJ / CONV_GJ_KGH2,
             `global solar resource`=`Energy use Electricity (Solar) [kWh/kg]`* CONV_KWH_GJ / CONV_GJ_KGH2,
             `onshore wind resource`=`Energy use Electricity (On-shore wind) [kWh/kg]`* CONV_KWH_GJ / CONV_GJ_KGH2) %>%
      select(Scenario, TechnologyH2A, year, matches('regional|resource|elect_td')) %>%
      tidyr::gather(key="minicam.energy.input", value="coefficient", -year, -Scenario, -TechnologyH2A) %>%
      filter(coefficient > 0) %>%
      complete(nesting(Scenario, TechnologyH2A, minicam.energy.input), year = MODEL_YEARS) %>%
      group_by(Scenario, TechnologyH2A, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      inner_join(H2ALite_TEA_mapping, by = "TechnologyH2A") %>%
      select(Scenario, sector.name, subsector.name, technology, year, minicam.energy.input, coefficient) %>%
      mutate(minicam.energy.input = if_else(subsector.name == "onsite production" & minicam.energy.input == "regional natural gas",
                                            "delivered gas", minicam.energy.input),
             minicam.energy.input = if_else(subsector.name == "onsite production" & minicam.energy.input == "elect_td_ind" & sector.name != "H2 industrial",
                                            "elect_td_trn", minicam.energy.input),
             minicam.energy.input = if_else(subsector.name == "nuclear", "nuclearFuelGenIII", minicam.energy.input),
             coefficient = if_else(subsector.name == "nuclear", coefficient * 3, coefficient))

    # Nuclear electricity generation costs are estimated from power sector assumptions, multiplied by electricity IOcoef
    # In these nuclear tables we use "adv" for high tech and "low" for low tech (staying consistent with XML file names)
    # This differs from the H2Alite scenario names where "low" means low-cost (i.e., high tech)
    L125.nuclear_elec_costs_scen <- mutate(L1233.globaltech_capital_ATB, Scenario = "ref") %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_adv, Scenario = "adv")) %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_low, Scenario = "low")) %>%
      filter(technology == "large reactor") %>%
      left_join_error_no_match(L223.GlobalTechCapFac_elec,
                               by = c(supplysector = "sector.name", subsector = "subsector.name", "technology", "year")) %>%
      left_join_error_no_match(L1233.globaltech_OMfixed_ATB,
                               by = c("supplysector", "subsector", "technology", "year")) %>%
      left_join_error_no_match(L1233.globaltech_OMvar_ATB,
                               by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(nonfuel.LCOE = (capital.overnight * fixed.charge.rate + OM.fixed ) / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ) + OM.var / 1000 * CONV_KWH_GJ) %>%
      select(Scenario, year, nonfuel.LCOE)

    # Nuclear IO coef: re-divide nuclear fuel input by 3 to get electricity:hydrogen IOcoef
    L125.nuc_IO <- filter(L125.globaltech_coef_scen, Scenario == "med" & subsector.name == "nuclear") %>%
      mutate(coefficient = coefficient / 3) %>%
      select(sector.name, subsector.name, technology, year, coefficient)

    # Multiply the non-fuel LCOE (cost of electricity) by the hydrogen IOcoef
    L125.nuclear_hydrogen_costs_scen <- left_join_error_no_match(L125.nuclear_elec_costs_scen, L125.nuc_IO,
                                                             by = "year") %>%
      mutate(minicam.non.energy.input = "nuclear electricity generation",
             input.cost = round(nonfuel.LCOE * coefficient, energy.DIGITS_COST)) %>%
      select(Scenario, sector.name, subsector.name, technology, year, minicam.non.energy.input, input.cost)

    # Split the 3 nuclear scenarios into separate tables
    # Adv and low nuclear-h2 electricity generation costs are saved, for adding to the nuclear_adv.xml and nuclear_low.xml input files
    # Nuclear ref is used as the basis of the standard hydrogen sector assumptions, in all 3 hydrogen technology scenarios
    L125.nuclear_hydrogen_costs_adv <- filter(L125.nuclear_hydrogen_costs_scen, Scenario == "adv") %>%
      select(-Scenario)
    L125.nuclear_hydrogen_costs_ref <- filter(L125.nuclear_hydrogen_costs_scen, Scenario == "ref") %>%
      select(-Scenario)
    L125.nuclear_hydrogen_costs_low <- filter(L125.nuclear_hydrogen_costs_scen, Scenario == "low") %>%
      select(-Scenario)

    L125.globaltech_cost_nuc_elec_scen <- L125.nuclear_hydrogen_costs_ref %>%
      repeat_add_columns(tibble(Scenario = unique(H2ALite_TEAdata$Scenario)))

    L125.globaltech_cost_scen <- bind_rows(L125.globaltech_cost_scen, L125.globaltech_cost_nuc_elec_scen)

    # YZ 3/30/2025 estimate energy IO coefs and non-energy cost for solar-wind-hybrid electrolysis for non-USA regions
    # Implementation of hybrid electrolyzer in the USA is in global tech database, but regions over-write these in their stub-techs
    # Implementation for non-USA regions are based on linear models to predict the levelized non-energy
    # cost and input-output coefficient based on explanatory variables of solar and wind CF in each region
    # gather data and prepare for regression based on Paul Wolfram's analysis
    L125.H2ALite_TEAdata_hybrid <- H2ALite_TEAdata %>%
      filter(Year == 2040, Technology == 'Hybrid PEM', Region != 'US Average') %>%
      select(Scenario,
             region = Region,
             cost_2022usd_per_kg = `Energy-free levelized cost [2022$/kg]`,
             IO_solar_kwh_per_kg = `Energy use Electricity (Solar) [kWh/kg]`,
             IO_wind_kwh_per_kg = `Energy use Electricity (On-shore wind) [kWh/kg]`) %>%
      left_join_error_no_match(H2ALite_wind_solar_CF %>%
                                 rename(region = State), by = 'region') %>%
      mutate(sum_CF = Wind_CF + Solar_CF, # calculate the sum of solar and wind CFs
             sum_CF_transform = 1/log10(sum_CF), # data transformation for better fit (see below)
             CF_ratio_sol_wind = Solar_CF / Wind_CF, # calculate the fraction of the solar capacity factor over the wind capacity factor
             H2_IO_ratio_sol_wind = IO_solar_kwh_per_kg / IO_wind_kwh_per_kg, # calculate the fraction of solar power used over wind power used per kg of H2 produced
             cost_2022usd_per_kg_transf = 1/log10(cost_2022usd_per_kg)) # data transformation for better fit (see below)

    Interpol_cost_scen <- L223.StubTechCapFactor_elec %>%
      filter(year == 2040, stub.technology %in% c('wind', 'PV')) %>%
      group_by(region) %>%
      summarise(sum_CF = sum(capacity.factor)) %>%
      ungroup() %>%
      mutate(sum_CF_transform = 1/log10(sum_CF)) %>%
      select(-sum_CF) %>%
      mutate(cost_2022usd_per_kg_transf = as.numeric("")) %>%
      repeat_add_columns(distinct(H2ALite_TEAdata, Scenario)) %>%
      bind_rows(L125.H2ALite_TEAdata_hybrid %>%
                  select(Scenario, region, sum_CF_transform, cost_2022usd_per_kg_transf))

    Interpol_coef_scen <- L223.StubTechCapFactor_elec %>%
      filter(year == 2040, stub.technology %in% c('wind', 'PV')) %>%
      select(region,stub.technology,capacity.factor) %>%
      spread(stub.technology,capacity.factor) %>%
      mutate(CF_ratio_sol_wind = PV / wind) %>%
      select(region, CF_ratio_sol_wind) %>%
      mutate(H2_IO_ratio_sol_wind = as.numeric("")) %>%
      repeat_add_columns(distinct(H2ALite_TEAdata, Scenario)) %>%
      bind_rows(L125.H2ALite_TEAdata_hybrid %>%
                  select(Scenario, region, CF_ratio_sol_wind, H2_IO_ratio_sol_wind))

    Interpol_cost <- list()
    Interpol_coef <- list()
    for(i in unique(H2ALite_TEAdata$Scenario)){
      model_cost_new_regions <- lm(cost_2022usd_per_kg_transf ~ sum_CF_transform, data = filter(Interpol_cost_scen, Scenario == i))
      model_coef_new_regions <- lm(H2_IO_ratio_sol_wind ~ CF_ratio_sol_wind, data = filter(Interpol_coef_scen, Scenario == i))

      Interpol_cost[[i]] <- filter(Interpol_cost_scen, Scenario == i) %>%
        mutate(predicted_cost_transform = predict(model_cost_new_regions, filter(Interpol_cost_scen, Scenario == i)),
               predicted_cost_2022usd_per_kg = 10^(1/predicted_cost_transform)) # reverse the data transformation, 1 /log10(x) that we did at the beginning

      Interpol_coef[[i]] <- filter(Interpol_coef_scen, Scenario == i) %>%
        mutate(predicted_H2_IO_ratio_sol_wind = predict(model_coef_new_regions, filter(Interpol_coef_scen, Scenario == i)))
      # YZ end of regression ----
    }

    Interpol_cost_df <- do.call(bind_rows, Interpol_cost)
    Interpol_coef_df <- do.call(bind_rows, Interpol_coef)

    # Generate table of region-specific non-energy costs of hybrid electrolysis, for the 2040 time period
    Interpol_cost_df %>%
      filter(region %in% GCAM_region_names$region) %>%
      # convert to unit of 1975$/GJ
      mutate(input.cost = predicted_cost_2022usd_per_kg * gdp_deflator(1975,2022) / CONV_GJ_KGH2,
             subsector = 'hybrid',
             minicam.non.energy.input = "other non-energy") %>%
      mutate(supplysector = H2ALite_TEA_mapping$sector.name[H2ALite_TEA_mapping$subsector.name == "hybrid"],
             stub.technology = H2ALite_TEA_mapping$technology[H2ALite_TEA_mapping$subsector.name == "hybrid"]) %>%
      select(c("Scenario", "region", "supplysector", "subsector", "stub.technology", "minicam.non.energy.input", "input.cost")) ->
      L125.StubTechCost_h2_hybrid_scen_2040

    # Expand to all years using tech-change multipliers with 2040 as the base year
    L125.hybrid_costmult_scen <- H2ALite_TEAdata %>%
      filter(Technology == 'Hybrid PEM', Region == 'US Average') %>%
      select(Scenario, year = Year, cost = `Energy-free levelized cost [2022$/kg]`) %>%
      complete(nesting(Scenario), year = MODEL_YEARS) %>%
      group_by(Scenario) %>%
      mutate(cost = approx_fun(year, cost, rule = 2),
             CostMult = cost / cost[year == 2040]) %>%
      ungroup() %>%
      select(-cost)

    L125.StubTechCost_h2_hybrid_scen <- L125.StubTechCost_h2_hybrid_scen_2040 %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(L125.hybrid_costmult_scen, by = c("Scenario", "year")) %>%
      mutate(input.cost = input.cost * CostMult) %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["StubTechCost"]]))

    # YZ 4/16/25; GPK 4/23/25
    # This data table has USA costs predicted from the regression. We don't read these to the model, as "US Average" is specified in
    # H2ALite data, but we use them to estimate the bias correction to be applied to all other regions.
    # The fundamental issue is that the "US Average" in the data is estimated from optimization,
    # so national average costs are near the lower limit of what is observed across all states,
    # whereas GCAM's regional capacity factor assumptions are intended to reflect output-weighted averages,
    # with no assumption of optimal siting.
    # So, H2ALite's "US Average" costs are lower than the state-capacity-factor-based regression
    # The method below applies the observed:predicted ratio of the US to all regions, thus assuming similar sub-regional
    # heterogeneity in site quality as is seen in the US.
    # In geographically diverse regions (e.g. Europe) this probably performs well, whereas in smaller and more
    # homogeneous regions (e.g., South Korea) the method should under-estimate the costs.
    # Ultimately the best method would be to work from source data in a wider sample of regions globally.
    L125.StubTechCost_h2_hybrid_scen_US <- filter(L125.globaltech_cost_scen, subsector.name == "hybrid") %>%
      mutate(region = "USA") %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["StubTechCost"]]))

    L125.NECost_regression_multiplier <- L125.StubTechCost_h2_hybrid_scen_US %>%
      left_join_error_no_match(L125.StubTechCost_h2_hybrid_scen,
                               by = c("Scenario", "region", "supplysector", "subsector",
                                      "stub.technology", "year", "minicam.non.energy.input"),
                               suffix = c(".observed", ".predicted")) %>%
      mutate(cost.bias.multiplier = input.cost.observed / input.cost.predicted) %>%
      select(Scenario, year, cost.bias.multiplier)

    L125.StubTechCost_h2_hybrid_scen <- L125.StubTechCost_h2_hybrid_scen %>%
      left_join_error_no_match(L125.NECost_regression_multiplier, by = c("Scenario", "year")) %>%
      mutate(input.cost = input.cost * cost.bias.multiplier) %>%
      select(-cost.bias.multiplier)

    # YZ 3/31/2025 add solar-wind hybrid electrolysis IO coefficient for non-USA regions based on the regression analysis
    # the regression predicts the ratio of solar energy use intensity over wind energy use intensity for the hybrid electrolysis
    # We use this ratio and the US average sum of solar and wind energy use intensity to calculate the regional energy use
    # intensity for solar and wind in non-USA regions
    # given EI_wind + EI_sol = x; EI_sol / EI_wind = y
    # so EI_wind = x / (1+y); EI_sol = EI_wind * y = x*y / (1+y)
    Interpol_coef_df %>%
      filter(region %in% GCAM_region_names$region) %>%
      select(Scenario, region, predicted_H2_IO_ratio_sol_wind) %>%
      mutate(subsector.name = 'hybrid') %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(
        L125.globaltech_coef_scen %>%
          filter(subsector.name == 'hybrid', minicam.energy.input %in% c('global solar resource', 'onshore wind resource')) %>%
          group_by(Scenario, sector.name, subsector.name, technology, year) %>%
          summarise(WindSolarSum = sum(coefficient)) %>%
          ungroup(),
        by = c('Scenario', 'subsector.name', 'year')) %>%
      mutate(`onshore wind resource` = WindSolarSum / (1 + predicted_H2_IO_ratio_sol_wind),
             `global solar resource` = WindSolarSum * predicted_H2_IO_ratio_sol_wind / (1 + predicted_H2_IO_ratio_sol_wind)) %>%
      select(-predicted_H2_IO_ratio_sol_wind, -WindSolarSum) %>%
      gather(minicam.energy.input, coefficient, -Scenario, -region, -sector.name, -subsector.name, -technology, -year) %>%
      # Solar input energy intensity per unit of hydrogen production is relatively small using the current CFs in gcamdata
      # For USA, the predicted solar EI is about 0.45, but the H2Alite data from misho is about  0.85 GJ input per GJ hydrogen output
      # this should be automatically updated when we update the solar CFs in gcamdata
      select(Scenario, region, supplysector = sector.name, subsector = subsector.name, stub.technology = technology, year, minicam.energy.input, coefficient) %>%
      filter(region != 'USA') -> # remove USA, will use global tech coef
      L125.StubTechCoef_h2_hybrid_scen_noUS

    L125.StubTechCoef_h2_hybrid_scen_US <- L125.globaltech_coef_scen %>%
      filter(subsector.name == 'hybrid',
             minicam.energy.input %in% c('global solar resource', 'onshore wind resource')) %>%
      mutate(region = "USA") %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology)

    L125.StubTechCoef_h2_hybrid_scen <-
      bind_rows(L125.StubTechCoef_h2_hybrid_scen_US, L125.StubTechCoef_h2_hybrid_scen_noUS)

    # 6/26/25 GPK - compile and include data on water use by hydrogen production
    L125.h2_water<- Melaina_h2_water %>%
      mutate(coefficient = Value * CONV_GAL_M3 / CONV_GJ_KGH2) %>%
      select(WaterTechnology = Technology, coefficient)

    # Global tech water coefficients
    # Hybrid: turn wind and solar into percentages, multiply by water intensities of each, and add them up.
    L125.globaltech_coef_water_hybrid <- L125.globaltech_coef_scen %>%
      filter(subsector.name == "hybrid") %>%
      group_by(Scenario, sector.name, subsector.name, technology, year) %>%
      mutate(share = coefficient / sum(coefficient)) %>%
      ungroup() %>%
      mutate(WaterTechnology = if_else(minicam.energy.input == "global solar resource", "Solar PV electrolysis", "Wind electrolysis")) %>%
      left_join_error_no_match(L125.h2_water, by = "WaterTechnology", suffix = c(".energy", ".water")) %>%
      mutate(coefficient = coefficient.water * share) %>%
      group_by(Scenario, sector.name, subsector.name, technology, year) %>%
      summarise(coefficient = sum(coefficient)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(minicam.energy.input = c("water_td_ind_W", "water_td_ind_C"))) %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["GlobalTechCoef"]]))

    # Standard global technologies
    L125.globaltech_coef_water <- L125.globaltech_coef_scen %>%
      filter(subsector.name != "hybrid") %>%
      distinct(Scenario, sector.name, subsector.name, technology, year) %>%
      left_join_error_no_match(H2ALite_TEA_mapping, by = c("sector.name", "subsector.name", "technology")) %>%
      left_join_error_no_match(L125.h2_water, by = "WaterTechnology") %>%
      repeat_add_columns(tibble(minicam.energy.input = c("water_td_ind_W", "water_td_ind_C"))) %>%
      select(c("Scenario", LEVEL2_DATA_NAMES[["GlobalTechCoef"]])) %>%
      bind_rows(L125.globaltech_coef_water_hybrid)

    L125.globaltech_coef_scen <- bind_rows(L125.globaltech_coef_scen, L125.globaltech_coef_water)

    # Hybrid stub-technologies
    L125.StubTechCoef_h2_water_hybrid_scen <- L125.StubTechCoef_h2_hybrid_scen %>%
      group_by(Scenario, region, supplysector, subsector, stub.technology, year) %>%
      mutate(share = coefficient / sum(coefficient)) %>%
      ungroup() %>%
      mutate(WaterTechnology = if_else(minicam.energy.input == "global solar resource", "Solar PV electrolysis", "Wind electrolysis")) %>%
      left_join_error_no_match(L125.h2_water, by = "WaterTechnology", suffix = c(".energy", ".water")) %>%
      mutate(coefficient = coefficient.water * share) %>%
      group_by(Scenario, region, supplysector, subsector, stub.technology, year) %>%
      summarise(coefficient = sum(coefficient)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(minicam.energy.input = c("water_td_ind_W", "water_td_ind_C")))

    L125.StubTechCoef_h2_hybrid_scen <- bind_rows(L125.StubTechCoef_h2_hybrid_scen, L125.StubTechCoef_h2_water_hybrid_scen)

    # ===================================================
    # Produce outputs

    L125.globaltech_coef_scen %>%
      add_title("Input-output coefficients of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated original data into all model years") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/H2ALite_TEA_mapping",
                     "energy/H2ALite_TEAdata", "energy/H2ALite_wind_solar_CF",
                     "energy/Melaina_h2_water", "L223.StubTechCapFactor_elec")  ->
      L125.globaltech_coef_scen

    L125.globaltech_cost_scen %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCost_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/H2ALite_TEA_mapping",
                     "energy/H2ALite_TEAdata", "energy/H2ALite_wind_solar_CF", "L223.GlobalTechCapFac_elec",
                     "L1233.globaltech_capital_ATB", "L1233.globaltech_OMvar_ATB", "L1233.globaltech_OMfixed_ATB",
                     "L223.StubTechCapFactor_elec")  ->
      L125.globaltech_cost_scen

    L125.nuclear_hydrogen_costs_adv  %>%
      add_title("Costs of nuclear electric generation costs for dedicated nuclear-h2 technology in the nuc_adv scenario") %>%
      add_units("1975$/GJ of hydrogen") %>%
      add_comments("Based on electrolysis IO coef and scenario-specific assumptions of nuclear electric generation costs") %>%
      same_precursors_as(L125.globaltech_cost_scen) %>%
      add_precursors("L1233.globaltech_capital_ATB_adv")->
      L125.nuclear_hydrogen_costs_adv

    L125.nuclear_hydrogen_costs_low  %>%
      add_title("Costs of nuclear electric generation costs for dedicated nuclear-h2 technology in the nuc_low scenario") %>%
      add_units("1975$/GJ of hydrogen") %>%
      add_comments("Based on electrolysis IO coef and scenario-specific assumptions of nuclear electric generation costs") %>%
      same_precursors_as(L125.globaltech_cost_scen) %>%
      add_precursors("L1233.globaltech_capital_ATB_low")->
      L125.nuclear_hydrogen_costs_low

    L125.StubTechCost_h2_hybrid_scen  %>%
      add_title("Hybrid (wind and solar) hydrogen electrolysis non-energy costs by region and year") %>%
      add_units("1975$ / GJh2") %>%
      add_comments("Based on total electrolysis cost by year, regional wind + solar capacity factors") %>%
      same_precursors_as(L125.globaltech_coef_scen) ->
      L125.StubTechCost_h2_hybrid_scen

    L125.StubTechCoef_h2_hybrid_scen  %>%
      add_title("Wind and solar electricity inputs to hydrogen electrolysis (hybrid technology) by region and year") %>%
      add_units("GJelec/GJh2") %>%
      add_comments("Based on total electrolysis IO coef by year, regional wind and solar capacity factors") %>%
      same_precursors_as(L125.globaltech_coef_scen) %>%
      add_precursors("L223.StubTechCapFactor_elec")->
      L125.StubTechCoef_h2_hybrid_scen

    return_data(L125.globaltech_coef_scen,
                L125.globaltech_cost_scen,
                L125.nuclear_hydrogen_costs_adv,
                L125.nuclear_hydrogen_costs_low,
                L125.StubTechCost_h2_hybrid_scen,
                L125.StubTechCoef_h2_hybrid_scen)
  } else {
    stop("Unknown command")
  }
}
