# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L225.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.Supplysector_h2}, \code{L225.SubsectorLogit_h2}, \code{L225.SubsectorShrwtFllt_h2}, \code{L225.StubTech_h2}, \code{L225.GlobalTechCoef_h2}, \code{L225.GlobalTechCost_h2}, \code{L225.GlobalTechShrwt_h2}, \code{L225.PrimaryRenewKeyword_h2}, \code{L225.GlobalTechCapture_h2}, \code{L225.StubTechCost_h2}, \code{L225.GlobalTechProfitShutdown_h2}, \code{L225.GlobalTechSCurve_h2}. The corresponding file in the
#' original data system was \code{L225.hydrogen.R} (energy level2).
#' @details Provides supply sector information, subsector information, technology information for hydrogen sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select
#' @importFrom tidyr complete nesting
#' @author LF Augest 2017
module_energy_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A25.sector",
             FILE = "energy/A25.subsector_logit",
             FILE = "energy/A25.subsector_shrwt",
             FILE = "energy/A25.globaltech_cost",
             FILE = "energy/A25.globaltech_coef",
             FILE = "energy/A25.globaltech_losses",
             FILE = "energy/A25.globaltech_retirement",
             FILE = "energy/A25.globaltech_shrwt",
             FILE = "energy/A25.globaltech_keyword",
             FILE = "energy/A25.globaltech_co2capture",
             "L1233.globaltech_capital_ATB",
             "L1233.globaltech_OMfixed_ATB",
             "L1233.globaltech_capital_ATB_adv",
             "L1233.globaltech_capital_ATB_low",
             "L223.StubTechCapFactor_elec",
             "L2231.StubTechCapFactor_onshore_wind",
             "L125.globaltech_coef_scen",
             "L125.globaltech_cost_scen",
             "L125.StubTechCost_h2_hybrid_scen",
             "L125.StubTechCoef_h2_hybrid_scen"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2_ref",
             "L225.GlobalTechCoef_h2_adv",
             "L225.GlobalTechCoef_h2_lotech",
             "L225.StubTechCoef_h2_hybrid_ref",
             "L225.StubTechCoef_h2_hybrid_adv",
             "L225.StubTechCoef_h2_hybrid_lotech",
             "L225.GlobalTechCost_h2_ref",
             "L225.GlobalTechCost_h2_adv",
             "L225.GlobalTechCost_h2_lotech",
             "L225.GlobalTechTrackCapital_h2_ref",
             "L225.GlobalTechTrackCapital_h2_adv",
             "L225.GlobalTechTrackCapital_h2_lotech",
             "L225.GlobalTechShrwt_h2",
             "L225.PrimaryRenewKeyword_h2",
             "L225.AvgFossilEffKeyword_h2",
             "L225.GlobalTechCapture_h2",
             "L225.GlobalTechInputPMult_h2",
             "L225.GlobalTechProfitShutdown_h2",
             "L225.GlobalTechSCurve_h2",
             "L225.StubTechCost_h2_hybrid_ref",
             "L225.StubTechCost_h2_hybrid_adv",
             "L225.StubTechCost_h2_hybrid_lotech",
             "L225.OutputEmissCoeff_h2"))
  } else if(command == driver.MAKE) {

    # Silencing package checks
    region <- coefficient <- cost <- price.unit.conversion <- sector.name <- subsector.name <-
      stub.technology <- capacity.factor <- `2040` <- `2020` <- `2050` <-
      intermittent.technology <- capital.overnight <- fixed.charge.rate <- OM.fixed <-
      cost_75USD_kW_yr <- kWh_elec_per_kgH2 <- output_kgh2_d <- cost_75USD_kgH2 <- NULL

    all_data <- list(...)[[1]]

    year.fillout <- technology <- year <- efficiency <- supplysector <- value <-
      subsector <- minicam.energy.input <- input.cost <- minicam.non.energy.input <-
      share.weight <- primary.renewable <- average.fossil.efficiency <-
      remove.fraction <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A25.sector <- get_data(all_data, "energy/A25.sector", strip_attributes = TRUE)
    A25.subsector_logit <- get_data(all_data, "energy/A25.subsector_logit", strip_attributes = TRUE)
    A25.subsector_shrwt <- get_data(all_data, "energy/A25.subsector_shrwt", strip_attributes = TRUE)
    A25.globaltech_coef <- get_data(all_data, "energy/A25.globaltech_coef", strip_attributes = TRUE)
    A25.globaltech_losses <- get_data(all_data, "energy/A25.globaltech_losses", strip_attributes = TRUE)
    A25.globaltech_cost <- get_data(all_data, "energy/A25.globaltech_cost", strip_attributes = TRUE)
    A25.globaltech_shrwt <- get_data(all_data, "energy/A25.globaltech_shrwt", strip_attributes = TRUE)
    A25.globaltech_keyword <- get_data(all_data, "energy/A25.globaltech_keyword", strip_attributes = TRUE)
    A25.globaltech_retirement <- get_data(all_data, "energy/A25.globaltech_retirement", strip_attributes = TRUE)
    A25.globaltech_co2capture <- get_data(all_data, "energy/A25.globaltech_co2capture", strip_attributes = TRUE)

    L1233.globaltech_capital_ATB <- get_data(all_data, "L1233.globaltech_capital_ATB", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB <- get_data(all_data, "L1233.globaltech_OMfixed_ATB", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_adv <- get_data(all_data, "L1233.globaltech_capital_ATB_adv", strip_attributes = TRUE)
    L1233.globaltech_capital_ATB_low <- get_data(all_data, "L1233.globaltech_capital_ATB_low", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)
    L2231.StubTechCapFactor_onshore_wind <- get_data(all_data, "L2231.StubTechCapFactor_onshore_wind", strip_attributes = TRUE)
    L125.globaltech_coef_scen <- get_data(all_data, "L125.globaltech_coef_scen", strip_attributes = TRUE)
    L125.globaltech_cost_scen <- get_data(all_data, "L125.globaltech_cost_scen", strip_attributes = TRUE)
    L125.StubTechCost_h2_hybrid_scen <- get_data(all_data, "L125.StubTechCost_h2_hybrid_scen", strip_attributes = TRUE)
    L125.StubTechCoef_h2_hybrid_scen <- get_data(all_data, "L125.StubTechCoef_h2_hybrid_scen", strip_attributes = TRUE)

    # ===================================================

    # 1. Build tables for CSVs
    # 1a. Supply sector information

    # L225.Supplysector_h2: Supply sector information for hydrogen sectors
    A25.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L225.Supplysector_h2

    # H2 liquid truck has a simultaneity that may benefit from using a trial market here
    L225.SectorUseTrialMarket_h2 <- filter(L225.Supplysector_h2, supplysector == "H2 liquid truck") %>%
      mutate(supplysector = "trn_freight_road") %>%
      select(region, supplysector) %>%
      mutate(use.trial.market = 1)

    # 1b. Subsector information

    # L225.SubsectorLogit_h2: Subsector logit exponents of hydrogen sectors
    A25.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      filter(!(subsector %in% c("wind", "solar"))) ->
      L225.SubsectorLogit_h2

    # L225.SubsectorShrwtFllt_h2: Subsector shareweights of hydrogen sectors
    A25.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names)%>%
      filter(!(subsector %in% c("wind", "solar"))) ->
      L225.SubsectorShrwtFllt_h2

    # 1c. Technology information

    # L225.StubTech_h2: Identification of stub technologies of hydrogen
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A25.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      filter(!(subsector %in% c("wind", "solar"))) %>%
      rename(stub.technology = technology) ->
      L225.StubTech_h2

    # L225.GlobalTechCoef_h2_[scen]: Energy inputs coefficients of global technologies for hydrogen, by scenario
    # The reference scenario assumptions will pass through to the general hydrogen.xml
    # The alternate case data tables are filtered to only
    L225.GlobalTechCoef_h2_scen <- L125.globaltech_coef_scen %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT))

    L225.GlobalTechCoef_h2_ref <- filter(L225.GlobalTechCoef_h2_scen, Scenario == "med") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])

    # function to generate alternate case data tables
    generate_alt_scenario_table <- function(full_table, alt_scenario_name, ref_scenario_table, value_var){
      # full_table: data table with the assumptions, and all scenarios
      # alt_scenario_name: name of alternative scenario in the data table
      # ref_scenario_table: data table with reference scenario assumptions
      # value_var: name of data column with numerical data
      joinvars <- names(ref_scenario_table)[names(ref_scenario_table) != value_var]
      value_var_ref <- paste0(value_var, ".ref")
      out_data <- filter(full_table, Scenario == alt_scenario_name) %>%
        left_join_error_no_match(ref_scenario_table,
                                 by = joinvars,
                                 suffix = c("", ".ref"))
      # filter to only rows where the alt scenario differs from the reference
      out_data <- out_data[out_data[[value_var]] != out_data[[value_var_ref]],]
      # return only the column names of the ref_scenario_table
      out_data <- out_data[names(ref_scenario_table)]

      return(out_data)
    }

    GlobalTechCoef_joinvars <- LEVEL2_DATA_NAMES[["GlobalTechCoef"]][LEVEL2_DATA_NAMES[["GlobalTechCoef"]] != "coefficient"]
    L225.GlobalTechCoef_h2_adv <- generate_alt_scenario_table(full_table = L225.GlobalTechCoef_h2_scen,
                                                              alt_scenario_name = "low",
                                                              ref_scenario_table = L225.GlobalTechCoef_h2_ref,
                                                              value_var = "coefficient")

    L225.GlobalTechCoef_h2_lotech <- generate_alt_scenario_table(full_table = L225.GlobalTechCoef_h2_scen,
                                                                 alt_scenario_name = "high",
                                                                 ref_scenario_table = L225.GlobalTechCoef_h2_ref,
                                                                 value_var = "coefficient")

    # L225.StubTechCoef_h2_hybrid_[scen]: region- and scenario-specific IO coefficients of hybrid (wind+solar) technology
    L225.StubTechCoef_h2_hybrid_scen <- L125.StubTechCoef_h2_hybrid_scen %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT),
             market.name = region)

    L225.StubTechCoef_h2_hybrid_ref <- filter(L225.StubTechCoef_h2_hybrid_scen, Scenario == "med") %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L225.StubTechCoef_h2_hybrid_adv <- generate_alt_scenario_table(full_table = L225.StubTechCoef_h2_hybrid_scen,
                                                                   alt_scenario_name = "low",
                                                                   ref_scenario_table = L225.StubTechCoef_h2_hybrid_ref,
                                                                   value_var = "coefficient")

    L225.StubTechCoef_h2_hybrid_lotech <- generate_alt_scenario_table(full_table = L225.StubTechCoef_h2_hybrid_scen,
                                                                      alt_scenario_name = "high",
                                                                      ref_scenario_table = L225.StubTechCoef_h2_hybrid_ref,
                                                                      value_var = "coefficient")

    # L225.GlobalTechCost_h2_[scen]: Costs of global technologies for hydrogen by scenario
    L225.GlobalTechCost_h2_scen <- L125.globaltech_cost_scen %>%
      mutate(input.cost = round(input.cost,energy.DIGITS_COST))

    L225.GlobalTechCost_h2_ref <- filter(L225.GlobalTechCost_h2_scen, Scenario == "med") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    L225.GlobalTechCost_h2_adv <- generate_alt_scenario_table(full_table = L225.GlobalTechCost_h2_scen,
                                                              alt_scenario_name = "low",
                                                              ref_scenario_table = L225.GlobalTechCost_h2_ref,
                                                              value_var = "input.cost")
    L225.GlobalTechCost_h2_lotech <- generate_alt_scenario_table(full_table = L225.GlobalTechCost_h2_scen,
                                                              alt_scenario_name = "high",
                                                              ref_scenario_table = L225.GlobalTechCost_h2_ref,
                                                              value_var = "input.cost")

    # L225.StubTechCost_h2_[scen]: Costs of global technologies for hydrogen by scenario
    # Stub-technology costs include two components:
    # (1) the "other non-energy costs" from H2ALite's "Energy-free levelized cost", and
    # (2) the costs of self-generated electricity, equal to the wind and solar IOcoefs multiplied by
    # the levelized cost of wind and solar electricity generation

    # In the code below, the scenario names in the power sector are assigned as per the names in the H2ALite data (low = advanced, low cost)
    # We are not generating all possible combinations of renewable-electric and hydrogen-electrolysis costs; the technology scenarios differ both in
    # the $/kwh of renewable electricity, and in the kwh of electricity per kg of hydrogen produced.
    # The capacity factors use left_join in order to expand by region
    # Capacity factors for wind in L223.StubTechCapFactor_elec are over-written by those in L2231.StubTechCapFactor_onshore_wind
    L225.RenewElec_CapFactor <- L2231.StubTechCapFactor_onshore_wind %>%
      filter(stub.technology == "wind") %>%
      bind_rows(filter(L223.StubTechCapFactor_elec, stub.technology == "PV"))
    L225.RenewElec_cost_scen <- L1233.globaltech_capital_ATB %>%
      mutate(Scenario = "med") %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_adv, Scenario = "low")) %>%
      bind_rows(mutate(L1233.globaltech_capital_ATB_low, Scenario = "high")) %>%
      filter(technology %in% c("wind", "PV")) %>%
      left_join(L1233.globaltech_OMfixed_ATB, by = c("supplysector", "subsector", "technology", "year")) %>%
      rename(stub.technology = technology) %>%
      left_join(L225.RenewElec_CapFactor, by = c("supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(elec_cost_75USD_GJ = (capital.overnight * calc_fixed_charge_rate(interest.rate, payback.years) + OM.fixed) /
               (CONV_YEAR_HOURS * capacity.factor * CONV_KWH_GJ)) %>%
      select(Scenario, region, renew_tech = subsector, year, elec_cost_75USD_GJ)


    # Join in the costs to the coefficient table (which indicates electricity IO coefs), multiply,
    # and aggregate to get the total renewable-electric non-energy cost
    L225.StubTechCost_h2_renewelec_scen <- L225.StubTechCoef_h2_hybrid_scen %>%
      mutate(renew_tech = if_else(minicam.energy.input == "global solar resource", "solar",
                                  if_else(minicam.energy.input == "onshore wind resource", "wind", "drop"))) %>%
      filter(renew_tech != "drop") %>%
      left_join_error_no_match(L225.RenewElec_cost_scen, by = c("Scenario", "region", "renew_tech", "year")) %>%
      mutate(minicam.non.energy.input = "renewable electricity generation",
             input.cost = coefficient * elec_cost_75USD_GJ) %>%
      group_by(Scenario, region, supplysector, subsector, stub.technology, year, minicam.non.energy.input) %>%
      summarise(input.cost = sum(input.cost)) %>%
      ungroup()

    L225.StubTechCost_h2_hybrid_scen <- L125.StubTechCost_h2_hybrid_scen %>%
      bind_rows(L225.StubTechCost_h2_renewelec_scen) %>%
      mutate(input.cost = round(input.cost,energy.DIGITS_COST))

    L225.StubTechCost_h2_hybrid_ref <- filter(L225.StubTechCost_h2_hybrid_scen, Scenario == "med") %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]])

    L225.StubTechCost_h2_hybrid_adv <- generate_alt_scenario_table(full_table = L225.StubTechCost_h2_hybrid_scen,
                                                                   alt_scenario_name = "low",
                                                                   ref_scenario_table = L225.StubTechCost_h2_hybrid_ref,
                                                                   value_var = "input.cost")

    L225.StubTechCost_h2_hybrid_lotech <- generate_alt_scenario_table(full_table = L225.StubTechCost_h2_hybrid_scen,
                                                                      alt_scenario_name = "high",
                                                                      ref_scenario_table = L225.StubTechCost_h2_hybrid_ref,
                                                                      value_var = "input.cost")

    # L225.GlobalTechTrackCapital_h2_[scen]: We want track capital investments for these technologies thus
    # we will change the object type accordingly and add the market name which will track investments
    # and the fraction of the total non-energy cost we should assume is annual investment in capital
    L225.GlobalTechTrackCapital_h2_ref <- L225.GlobalTechCost_h2_ref %>%
      filter(sector.name == "H2 central production") %>%
      mutate(capital.ratio = socioeconomics.H2_CAPITAL_RATIO,
             interest.rate = socioeconomics.DEFAULT_INTEREST_RATE,
             payback.years = socioeconomics.H2_CAP_PAYMENTS,
             invest.unit.conversion = 1,
             tracking.market =socioeconomics.EN_CAPITAL_MARKET_NAME,
             depreciation.rate = 0) %>% # note: vintaging is active so depreciation.rate is ignored
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']])

    L225.GlobalTechTrackCapital_h2_adv <- L225.GlobalTechCost_h2_adv %>%
      filter(sector.name == "H2 central production") %>%
      mutate(capital.ratio = socioeconomics.H2_CAPITAL_RATIO,
             interest.rate = socioeconomics.DEFAULT_INTEREST_RATE,
             payback.years = socioeconomics.H2_CAP_PAYMENTS,
             invest.unit.conversion = 1,
             tracking.market =socioeconomics.EN_CAPITAL_MARKET_NAME,
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']])

    L225.GlobalTechTrackCapital_h2_lotech <- L225.GlobalTechCost_h2_lotech %>%
      filter(sector.name == "H2 central production") %>%
      mutate(capital.ratio = socioeconomics.H2_CAPITAL_RATIO,
             interest.rate = socioeconomics.DEFAULT_INTEREST_RATE,
             payback.years = socioeconomics.H2_CAP_PAYMENTS,
             invest.unit.conversion = 1,
             tracking.market =socioeconomics.EN_CAPITAL_MARKET_NAME,
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']])

    # L225.GlobalTechShrwt_h2: Shareweights of global technologies for hydrogen
    # Shareweights of global technologies
    A25.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechShrwt_h2

    A25.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             units = 'GJ input / GJ H2') %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      anti_join(L225.GlobalTechCoef_h2_ref, by = c("sector.name", "subsector.name", "technology", "minicam.energy.input")) %>%
      select(-value,-price.unit.conversion) ->
      L225.GlobalTechCoef_h2_noprod #filter and convert efficiencies to coefficients for only end use and distribution pass-through sectors and technologies

    L225.GlobalTechCoef_h2_ref <- bind_rows(L225.GlobalTechCoef_h2_ref,L225.GlobalTechCoef_h2_noprod) %>%
      mutate(minicam.energy.input = if_else(grepl("elect_td_trn", minicam.energy.input), "elect_td_trn", minicam.energy.input)) %>%
      group_by(sector.name,subsector.name,technology,minicam.energy.input,year) %>%
      summarize(coefficient = sum(coefficient)) %>%
      ungroup()

    A25.globaltech_coef %>%
      filter(!is.na(price.unit.conversion)) %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(price.unit.conversion = approx_fun(year, price.unit.conversion, rule = 2)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInputPMult"]]) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) -> L225.GlobalTechInputPMult

    # Process generic cost data from assumptions tables (A25), anti-join costs already computed, and bind in a single cost table
    A25.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, value, rule = 2), digits = energy.DIGITS_COST),
             units = '$1975/GJ H2') %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      anti_join(L225.GlobalTechCost_h2_ref, by = c("sector.name", "subsector.name", "technology", "minicam.non.energy.input")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L225.GlobalTechCost_h2_noprod # costs for only T&D and end-use technologies

    L225.GlobalTechCost_h2_ref <- bind_rows(L225.GlobalTechCost_h2_ref,L225.GlobalTechCost_h2_noprod)

    # L225.PrimaryRenewKeyword_h2: Keywords of primary renewable electric generation technologies
    A25.globaltech_keyword %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L225.AllKeyword_h2

    L225.AllKeyword_h2 %>%
      filter(!is.na(primary.renewable)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "primary.renewable") ->
      L225.PrimaryRenewKeyword_h2

    # L225.AvgFossilEffKeyword_h2: Keywords of fossil/bio electric generation technologies
    L225.AllKeyword_h2 %>%
      filter(!is.na(average.fossil.efficiency)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "average.fossil.efficiency") ->
      L225.AvgFossilEffKeyword_h2

    # L225.GlobalTechCapture_h2: CO2 capture fractions from global fertilizer production technologies with CCS
    # Note: No need to consider historical periods or intermittent technologies here
    A25.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction,energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L225.GlobalTechCapture_h2


    # Set global technology retirement information for all hydrogen sector technologies
    # ------------------------------------------------------------------------------------

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L225.globaltech_retirement for each of these functions and creates a subset for each option then removes any subsets with 0 rows
    # All of these options have different headers, and all are allowed.
    # Also, technologies that have an additional shutdown rate as a function of their profitability are also defined.
    # Replace years and prepare assumptions into correct format
    A25.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L225.globaltech_retirement_base

    # Copies base year retirement information into all future years and appends back onto itself
    L225.globaltech_retirement_base %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(filter(L225.globaltech_retirement_base, year == max(MODEL_BASE_YEARS))) ->
      L225.globaltech_retirement

    # S-CURVE RETIREMENT
    # Subsets the S-Curve retirement function
    L225.globaltech_retirement %>%
      filter(!is.na(L225.globaltech_retirement$half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L225.GlobalTechSCurve_h2

    # PROFIT-BASED SHUTDOWN PARAMETERS
    # Subsets any technologies with a shutdown parameter based on profitability
    L225.globaltech_retirement %>%
      filter(!is.na(L225.globaltech_retirement$median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L225.GlobalTechProfitShutdown_h2

    # Adjustment to coefficients for losses
    L225.globaltech_losses <- gather_years(A25.globaltech_losses) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, Non.CO2), year = MODEL_YEARS) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, Non.CO2) %>%
      mutate(multiplier = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      select(-value)

    L225.GlobalTechCoef_h2_ref <- left_join_error_no_match(L225.GlobalTechCoef_h2_ref, L225.globaltech_losses,
                                        by = c(sector.name = "supplysector", subsector.name = "subsector", "technology", "minicam.energy.input", "year"),
                                        ignore_columns = c("Non.CO2", "multiplier")) %>%
      mutate(coefficient = if_else(is.na(multiplier),
                                   coefficient,
                                   round(coefficient * multiplier, energy.DIGITS_COEFFICIENT))) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])

    # Emissions coefficients
    # Emissions coefficients are read as region-specific data, so the default coefs need to be repeated by all regions
    L225.OutputEmissCoeff_h2 <- L225.globaltech_losses %>%
      mutate(emiss.coeff = round((multiplier - 1) / CONV_GJ_KGH2, emissions.DIGITS_EMISS_COEF)) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(GCAM_region_names["region"])) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]])

    # ===================================================
    # Produce outputs

    L225.Supplysector_h2 %>%
      add_title("Supply sector information for hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand sector information for all GCAM regions") %>%
      add_legacy_name("L225.Supplysector_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.sector") ->
      L225.Supplysector_h2

    L225.SectorUseTrialMarket_h2 %>%
      add_title("Supply sector trial markets") %>%
      add_units("Boolean") %>%
      add_comments("Read in to help solver deal with simultaneities") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.sector") ->
      L225.SectorUseTrialMarket_h2

    L225.SubsectorLogit_h2 %>%
      add_title("Subsector logit exponents of hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand subsector logit exponents for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorLogit_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.subsector_logit") ->
      L225.SubsectorLogit_h2

    L225.SubsectorShrwtFllt_h2 %>%
      add_title("Subsector shareweights of hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand Subsector shareweights for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorShrwtFllt_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.subsector_shrwt") ->
      L225.SubsectorShrwtFllt_h2

    L225.StubTech_h2 %>%
      add_title("Identification of stub technologies of hydrogen") %>%
      add_units("NA") %>%
      add_comments("Expand stub technologies information for all GCAM regions") %>%
      add_comments("assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)") %>%
      add_legacy_name("L225.StubTech_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.globaltech_shrwt") ->
      L225.StubTech_h2

    L225.GlobalTechCoef_h2_ref %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen (ref scenario)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef_scen", "L125.StubTechCoef_h2_hybrid_scen",
                     'energy/A25.globaltech_coef', "energy/A25.globaltech_losses") ->
      L225.GlobalTechCoef_h2_ref

    L225.GlobalTechCoef_h2_adv %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen (adv scenario)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.GlobalTechCoef_h2_ref) ->
      L225.GlobalTechCoef_h2_adv

    L225.GlobalTechCoef_h2_lotech %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen (lotech scenario)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.GlobalTechCoef_h2_ref) ->
      L225.GlobalTechCoef_h2_lotech

    L225.StubTechCoef_h2_hybrid_ref %>%
      add_title("Energy inputs and efficiencies of hybrid technologies for hydrogen in USA (ref scenario)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.StubTechCoef_h2_hybrid_scen") ->
      L225.StubTechCoef_h2_hybrid_ref

    L225.StubTechCoef_h2_hybrid_adv %>%
      add_title("Energy inputs and efficiencies of lotech hybrid technologies for hydrogen in USA") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.StubTechCoef_h2_hybrid_ref) ->
      L225.StubTechCoef_h2_hybrid_adv

    L225.StubTechCoef_h2_hybrid_lotech %>%
      add_title("Energy inputs and efficiencies of lotech hybrid technologies for hydrogen in USA") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.StubTechCoef_h2_hybrid_ref) ->
      L225.StubTechCoef_h2_hybrid_lotech

    L225.GlobalTechCost_h2_ref %>%
      add_title("Costs of global technologies for hydrogen (ref scenario)") %>%
      add_units("$1975 / GJ H2") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_cost_scen", "L125.StubTechCost_h2_hybrid_scen", 'energy/A25.globaltech_cost') ->
      L225.GlobalTechCost_h2_ref

    L225.GlobalTechCost_h2_adv %>%
      add_title("Costs of global technologies for hydrogen (adv scenario)") %>%
      add_units("$1975 / GJ H2") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.GlobalTechCost_h2_ref) ->
      L225.GlobalTechCost_h2_adv

    L225.GlobalTechCost_h2_lotech %>%
      add_title("Costs of global technologies for hydrogen (lotech scenario)") %>%
      add_units("$1975 / GJ H2") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      same_precursors_as(L225.GlobalTechCost_h2_ref) ->
      L225.GlobalTechCost_h2_lotech

    L225.GlobalTechTrackCapital_h2_ref %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      add_comments("Note for capital tracking we are interested only in the H2 production techs") %>%
      same_precursors_as(L225.GlobalTechCost_h2_ref) ->
      L225.GlobalTechTrackCapital_h2_ref

    L225.GlobalTechTrackCapital_h2_adv %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      add_comments("Note for capital tracking we are interested only in the H2 production techs") %>%
      same_precursors_as(L225.GlobalTechCost_h2_ref) ->
      L225.GlobalTechTrackCapital_h2_adv

    L225.GlobalTechTrackCapital_h2_lotech %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      add_comments("Note for capital tracking we are interested only in the H2 production techs") %>%
      same_precursors_as(L225.GlobalTechCost_h2_ref) ->
      L225.GlobalTechTrackCapital_h2_lotech

    L225.GlobalTechShrwt_h2 %>%
      add_title("Shareweights of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechShrwt_h2") %>%
      add_precursors("energy/A25.globaltech_shrwt") ->
      L225.GlobalTechShrwt_h2

    L225.PrimaryRenewKeyword_h2 %>%
      add_title("Keywords of primary renewable electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of primary renewable electric generation technologies for all model years") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L225.PrimaryRenewKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.PrimaryRenewKeyword_h2

    L225.AvgFossilEffKeyword_h2 %>%
      add_title("Keywords of fossil/bio electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of fossil/bio electric generation technologies for all model years") %>%
      add_legacy_name("L225.AvgFossilEffKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.AvgFossilEffKeyword_h2

    L225.GlobalTechCapture_h2 %>%
      add_title("CO2 capture fractions from global fertilizer production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCapture_h2") %>%
      add_precursors("energy/A25.globaltech_co2capture")->
      L225.GlobalTechCapture_h2

    L225.GlobalTechInputPMult %>%
      add_title("Price conversion from transportation technologies") %>%
      add_comments("converts from $1990/tkm to $1975$/EJ") %>%
      add_units("Unitless") ->
      L225.GlobalTechInputPMult_h2

    L225.GlobalTechSCurve_h2 %>%
      add_title("Global tech lifetime for techs with s-curve retirement function") %>%
      add_units("Lifetime in years, half-life in years") %>%
      add_comments("Filters for any technology that uses an S-curve retirement function") %>%
      add_legacy_name("L225.GlobalTechSCurve_h2") %>%
      add_precursors("energy/A25.globaltech_retirement") ->
      L225.GlobalTechSCurve_h2

    L225.GlobalTechProfitShutdown_h2 %>%
      add_title("Global tech profit shutdown decider and parameters") %>%
      add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
      add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
      add_legacy_name("L225.GlobalTechProfitShutdown_h2") %>%
      add_precursors("energy/A25.globaltech_retirement") ->
      L225.GlobalTechProfitShutdown_h2

    L225.StubTechCost_h2_hybrid_ref %>%
      add_title("Region-specific hybrid hydrogen production costs (ref scenario)") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.StubTechCost_h2_hybrid_scen", "L1233.globaltech_capital_ATB", "L1233.globaltech_OMfixed_ATB",
                     "L1233.globaltech_capital_ATB_adv", "L1233.globaltech_capital_ATB_low", "L223.StubTechCapFactor_elec", "L2231.StubTechCapFactor_onshore_wind") ->
      L225.StubTechCost_h2_hybrid_ref

    L225.StubTechCost_h2_hybrid_adv %>%
      add_title("Regional adv hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_hybrid_ref) ->
      L225.StubTechCost_h2_hybrid_adv

    L225.StubTechCost_h2_hybrid_lotech %>%
      add_title("Regional lotech hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      same_precursors_as(L225.StubTechCost_h2_hybrid_ref) ->
      L225.StubTechCost_h2_hybrid_lotech

    L225.OutputEmissCoeff_h2 %>%
      add_title("Hydrogen gas emissions coefficients") %>%
      add_units("kg of H2 per GJ of hydrogen output") %>%
      add_comments("calculated from the assumed losses") %>%
      same_precursors_as(L225.GlobalTechCoef_h2_ref) %>%
      add_precursors("energy/A25.globaltech_losses") ->
      L225.OutputEmissCoeff_h2

    return_data(L225.Supplysector_h2, L225.SectorUseTrialMarket_h2, L225.SubsectorLogit_h2, L225.StubTech_h2,
                L225.GlobalTechCoef_h2_ref, L225.GlobalTechCoef_h2_adv, L225.GlobalTechCoef_h2_lotech,
                L225.StubTechCoef_h2_hybrid_ref, L225.StubTechCoef_h2_hybrid_adv, L225.StubTechCoef_h2_hybrid_lotech,
                L225.GlobalTechCost_h2_ref, L225.GlobalTechCost_h2_adv, L225.GlobalTechCost_h2_lotech,
                L225.GlobalTechTrackCapital_h2_ref, L225.GlobalTechTrackCapital_h2_adv, L225.GlobalTechTrackCapital_h2_lotech,
                L225.GlobalTechShrwt_h2, L225.PrimaryRenewKeyword_h2, L225.AvgFossilEffKeyword_h2,
                L225.GlobalTechCapture_h2, L225.SubsectorShrwtFllt_h2, L225.GlobalTechInputPMult_h2,
                L225.GlobalTechSCurve_h2, L225.GlobalTechProfitShutdown_h2,
                L225.StubTechCost_h2_hybrid_ref, L225.StubTechCost_h2_hybrid_adv, L225.StubTechCost_h2_hybrid_lotech,
                L225.OutputEmissCoeff_h2)
  } else {
    stop("Unknown command")
  }
}
