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
             FILE = "energy/A25.globaltech_retirement",
             FILE = "energy/A25.globaltech_shrwt",
             FILE = "energy/A25.globaltech_keyword",
             FILE = "energy/A25.globaltech_co2capture",
             "L125.globaltech_coef",
             "L125.globaltech_cost",
             "L125.Electrolyzer_IdleRatio_Params",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechCapital_elec",
             "L223.GlobalIntTechOMfixed_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2",
             "L225.GlobalTechCost_h2",
             "L225.GlobalTechShrwt_h2",
             "L225.PrimaryRenewKeyword_h2",
             "L225.AvgFossilEffKeyword_h2",
             "L225.GlobalTechCapture_h2",
             "L225.GlobalTechInputPMult_h2",
             "L225.GlobalTechProfitShutdown_h2",
             "L225.GlobalTechSCurve_h2",
             "L225.StubTechCost_h2"))
  } else if(command == driver.MAKE) {

    # Silencing package checks
    region <- coefficient <- cost <- price.unit.conversion <- sector.name <- subsector.name <-
      stub.technology <- capacity.factor <- IdleRatio <- `2040` <- `2015` <- `2050` <-
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
    A25.globaltech_cost <- get_data(all_data, "energy/A25.globaltech_cost", strip_attributes = TRUE)
    A25.globaltech_shrwt <- get_data(all_data, "energy/A25.globaltech_shrwt", strip_attributes = TRUE)
    A25.globaltech_keyword <- get_data(all_data, "energy/A25.globaltech_keyword", strip_attributes = TRUE)
    A25.globaltech_retirement <- get_data(all_data, "energy/A25.globaltech_retirement", strip_attributes = TRUE)
    A25.globaltech_co2capture <- get_data(all_data, "energy/A25.globaltech_co2capture", strip_attributes = TRUE)

    L125.globaltech_coef <- get_data(all_data, "L125.globaltech_coef", strip_attributes = TRUE)
    L125.globaltech_cost <- get_data(all_data, "L125.globaltech_cost", strip_attributes = TRUE)
    L125.Electrolyzer_IdleRatio_Params <- get_data(all_data, "L125.Electrolyzer_IdleRatio_Params", strip_attributes = TRUE)

    L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec", strip_attributes = TRUE)
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)

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
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L225.SubsectorLogit_h2

    # L225.SubsectorShrwtFllt_h2: Subsector shareweights of hydrogen sectors
    A25.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L225.SubsectorShrwtFllt_h2

    # 1c. Technology information

    # L225.StubTech_h2: Identification of stub technologies of hydrogen
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A25.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L225.StubTech_h2

    # L225.GlobalTechCoef_h2: Energy inputs coefficients of global technologies for hydrogen
    L125.globaltech_coef %>%
           # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
           rename(sector.name = supplysector,
                  subsector.name = subsector,
                  coefficient = value) %>%
      mutate(coefficient = round(coefficient,energy.DIGITS_COEFFICIENT))-> L225.GlobalTechCoef_h2

    # L225.GlobalTechCost_h2: Costs of global technologies for hydrogen
    # Costs of global technologies
    L125.globaltech_cost %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector,
             input.cost = cost) %>%
      mutate(input.cost = round(input.cost,energy.DIGITS_COST))-> L225.GlobalTechCost_h2

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
      anti_join(L125.globaltech_coef, by = c("sector.name", "subsector.name", "technology", "minicam.energy.input")) %>%
      select(-value,-efficiency,-price.unit.conversion) ->
      L225.GlobalTechCoef_h2_noprod #filter and convert efficiencies to coefficients for only end use and distribution pass-through sectors and technologies

    L225.GlobalTechCoef_h2 <- bind_rows(L225.GlobalTechCoef_h2,L225.GlobalTechCoef_h2_noprod) %>%
      mutate(minicam.energy.input = if_else(minicam.energy.input == 'elect_td_trn (compression and refrigeration)','elect_td_trn',minicam.energy.input)) %>%
      group_by(sector.name,subsector.name,technology,minicam.energy.input,units,year) %>%
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
      anti_join(L125.globaltech_cost, by = c("sector.name", "subsector.name", "technology", "minicam.non.energy.input")) %>%
      select(-value) ->
      L225.GlobalTechCost_h2_noprod #get costs for only end use, distribution pass-through sectors, (global) renewable portion of LCOH (e.g., solar panels), and technologies from A25

    L225.GlobalTechCost_h2 <- bind_rows(L225.GlobalTechCost_h2,L225.GlobalTechCost_h2_noprod)

    # Estimate the region-specific costs of direct wind and solar electrolysis, based on the capacity factors of the
    # electric generation technologies and relationship between capacity factors and NE costs of electrolysis
    L125.Electrolyzer_IdleRatio_Params_2015 <- filter(L125.Electrolyzer_IdleRatio_Params, year == 2015)
    L125.Electrolyzer_IdleRatio_Params_2040 <- filter(L125.Electrolyzer_IdleRatio_Params, year == 2040)
    # Set the fraction of 2050 to 2040 costs. The specific number is from Pat's workbook
    Electrolyzer_2050_2040_cost_ratio <- 0.77

    # The following block uses the wind+solar capacity factors in each region to estimate the levelized cost of the
    # hydrogen electrolyzers. The available cost estimate years are 2015 and 2040. Cost reductions are extrapolated to
    # 2050 using the same improvement factor in all regions. The costs assigned to years between 2015 and 2050 are estimated
    # with linear interpolation, and outside this window we use fixed extrapolation.
    # Electrolyzer non-energy costs replace rather than add to the global default
    # values in L225.GlobalTechCost_h2. This is handled in the left_join.
    L223.StubTechCapFactor_elec %>%
      filter(year == max(MODEL_BASE_YEARS),
             stub.technology %in% c("wind", "PV")) %>%
      mutate(IdleRatio = pmax(1, 1 / (capacity.factor / energy.ELECTROLYZER_RENEWABLE_CAPACITY_RATIO)),
             `2015` = L125.Electrolyzer_IdleRatio_Params_2015$intercept +
               IdleRatio * L125.Electrolyzer_IdleRatio_Params_2015$slope,
             `2040` = L125.Electrolyzer_IdleRatio_Params_2040$intercept +
               IdleRatio * L125.Electrolyzer_IdleRatio_Params_2040$slope,
             `2050` = `2040` * Electrolyzer_2050_2040_cost_ratio) %>%
      select(region, subsector, `2015`, `2040`, `2050`) %>%
      gather_years() %>%
      complete(nesting(region, subsector), year = MODEL_YEARS) %>%
      group_by(region, subsector) %>%
      mutate(minicam.non.energy.input = "electrolyzer",
             input.cost = approx_fun(year, value, rule = 2),
             input.cost = input.cost * gdp_deflator(1975, 2016) / CONV_GJ_KGH2) %>%
      ungroup() %>%
      select(-value) %>%
      left_join(select(L225.GlobalTechCost_h2, -input.cost),
                by = c("subsector" = "subsector.name", "year", "minicam.non.energy.input")) %>%
      rename(supplysector = sector.name, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L225.StubTechCost_h2_electrolyzer

    # Estimate the wind turbine and solar panel related aspects of the costs of direct renewable hydrogen electrolysis
    # These are estimated from the capital costs of wind and solar technologies (generic, global), the region-specific
    # capacity factors, efficiencies of hydrogen production, and size of the representative hydrogen plant
    L225.RenewElec_cost <- L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology %in% c("wind", "PV")) %>%
      left_join(L223.GlobalIntTechOMfixed_elec, by = c("sector.name", "subsector.name", "intermittent.technology", "year")) %>%
      mutate(cost_75USD_kW_yr = capital.overnight * fixed.charge.rate + OM.fixed) %>%
      select(subsector.name, intermittent.technology, year, cost_75USD_kW_yr)

    L225.RenewElec_eff <- filter(L225.GlobalTechCoef_h2,
                                 subsector.name %in% c("solar", "wind") & !grepl("water", minicam.energy.input)) %>%
      mutate(kWh_elec_per_kgH2 = coefficient * CONV_GJ_KGH2 / CONV_KWH_GJ) %>%
      select(subsector.name, year, kWh_elec_per_kgH2)

    L225.RenewElec_cost %>%
      left_join_error_no_match(L225.RenewElec_eff, by = c("subsector.name", "year")) %>%
      left_join(L223.StubTechCapFactor_elec,
                by = c("subsector.name" = "subsector", "intermittent.technology" = "stub.technology", "year")) %>%
      mutate(minicam.non.energy.input = if_else(subsector.name == "solar", "solar panels", "wind turbines"),
             output_kgh2_d = if_else(subsector.name == "solar", energy.SOLAR_ELECTROLYSIS_KGH2_D, energy.WIND_ELECTROLYSIS_KGH2_D),
             cost_75USD_kgH2 = cost_75USD_kW_yr * kWh_elec_per_kgH2 * output_kgh2_d / CONV_DAY_HOURS /
               (output_kgh2_d * capacity.factor / CONV_DAYS_YEAR),
             input.cost = cost_75USD_kgH2 / CONV_GJ_KGH2) %>%
      select(region, subsector.name, year, minicam.non.energy.input, input.cost) %>%
      left_join(select(L225.GlobalTechCost_h2, -input.cost), by = c("subsector.name", "year", "minicam.non.energy.input")) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L225.StubTechCost_h2_renewables

    # Combine the electrolyzer and renewable power generation technologies' levelized non-energy costs into a single table
    L225.StubTechCost_h2 <- bind_rows(L225.StubTechCost_h2_electrolyzer, L225.StubTechCost_h2_renewables) %>%
      mutate(input.cost = round(input.cost, digits = energy.DIGITS_COST))

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

    L225.GlobalTechCoef_h2 %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_coef",'energy/A25.globaltech_coef') -> L225.GlobalTechCoef_h2

    L225.GlobalTechCost_h2 %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("$1975 / GJ H2") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_precursors("L125.globaltech_cost",'energy/A25.globaltech_cost')  -> L225.GlobalTechCost_h2

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

    L225.StubTechCost_h2 %>%
      add_title("Regional hydrogen production costs") %>%
      add_units("$1975/GJ") %>%
      add_comments("LCOH for the electrolyzer and renewables providing electricity.") %>%
      add_precursors("L125.Electrolyzer_IdleRatio_Params",
                     "L223.StubTechCapFactor_elec",
                     "L223.GlobalIntTechCapital_elec",
                     "L223.GlobalIntTechOMfixed_elec") ->
      L225.StubTechCost_h2

    return_data(L225.Supplysector_h2, L225.SectorUseTrialMarket_h2, L225.SubsectorLogit_h2, L225.StubTech_h2,
                L225.GlobalTechCoef_h2, L225.GlobalTechCost_h2, L225.GlobalTechShrwt_h2,
                L225.PrimaryRenewKeyword_h2, L225.AvgFossilEffKeyword_h2,
                L225.GlobalTechCapture_h2, L225.SubsectorShrwtFllt_h2,
                L225.GlobalTechInputPMult_h2,
                L225.GlobalTechSCurve_h2, L225.GlobalTechProfitShutdown_h2, L225.StubTechCost_h2)
  } else {
    stop("Unknown command")
  }
}
