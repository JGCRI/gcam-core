# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2328.food_processing
#'
#' Compute a variety of sector, share weight, and technology information for food processing-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2328.SectorLogitTables[[ curr_table ]]$data}, \code{L2328.Supplysector_food}, \code{L2328.FinalEnergyKeyword_food},
#' \code{L2328.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2328.SubsectorLogit_food}, \code{L2328.SubsectorShrwtFllt_food},
#' \code{L2328.SubsectorInterp_food}, \code{L2328.StubTech_food}, \code{L2328.GlobalTechShrwt_food}, \code{L2328.GlobalTechCoef_food},
#' \code{L2328.GlobalTechCost_food}, \code{L2328.StubTechCost_food}, \code{L2328.StubTechProd_food}, \code{L2328.StubTechCalInput_food_heat},
#' \code{L2328.StubTechCoef_food}, \code{L2328.StubCalorieContent}, \code{L2328.StubCaloriePriceConv},
#' \code{L2328.GlobalTechSecOut_food}, \code{L2328.GlobalTechTrackCapital_food},
#' \code{object}.
#' @details The chunk provides supplysector/subsector information, supplysector/subsector interpolation information,
#' global technology share weight, global technology efficiency, global technology coefficients, global technology cost,
#' stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc. for the food processing sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author SAS Dec 2022
module_energy_L2328.food_processing <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/calibrated_techs",
      FILE = "energy/A_regions",
      FILE = "energy/A328.sector",
      FILE = "energy/A23.chp_elecratio",
      FILE = "energy/A328.subsector_interp",
      FILE = "energy/A328.subsector_logit",
      FILE = "energy/A328.subsector_shrwt",
      FILE = "energy/A328.globaltech_coef",
      FILE = "energy/A328.globaltech_cost",
      FILE = "energy/A328.regionaltech_cost",
      FILE = "energy/A328.globaltech_shrwt",
      FILE = "energy/A328.globaltech_retirement",
      FILE = "energy/A328.demand",
      "L1328.in_EJ_R_food_F_Yh",
      "L1328.out_Pcal_R_food_Yh",
      "L1328.IO_EJPcal_R_food_F_Yh")

  MODULE_OUTPUTS <-
    c("L2328.Supplysector_food",
      "L2328.FinalEnergyKeyword_food",
      "L2328.SubsectorLogit_food",
      "L2328.SubsectorShrwtFllt_food",
      "L2328.SubsectorInterp_food",
      "L2328.StubTech_food",
      "L2328.GlobalTechShrwt_food",
      "L2328.GlobalTechCoef_food",
      "L2328.GlobalTechCost_food",
      "L2328.StubTechCost_food",
      "L2328.GlobalTechTrackCapital_food",
      "L2328.GlobalTechShutdown_food",
      "L2328.GlobalTechSCurve_food",
      "L2328.GlobalTechLifetime_food",
      "L2328.GlobalTechProfitShutdown_food",
      "L2328.StubTechProd_food",
      "L2328.StubTechCalInput_food_heat",
      "L2328.StubTechCoef_food",
      "L2328.StubCalorieContent",
      "L2328.StubCaloriePriceConv",
      "L2328.GlobalTechSecOut_food")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <- output.ratio <-
      value.x <- value.y <- parameter <- secondary.output <- elec_ratio <- year.x <- year.y <- output.ratio.x <-
      output.ratio.y <- sector.name <- subsector.name <- stub.technology <- market.name <- non_heat_input <- NULL

    # ===================================================
    # 1. Perform computations
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(sector %in% c("food processing") & subsector == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2328.rm_heat_techs_R # intermediate tibble


    # 1a. Supplysector information
    # L2328.Supplysector_food: Supply sector information for food processing sector
    A328.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2328.Supplysector_food

    # L2328.FinalEnergyKeyword_food: Supply sector keywords for food processing sector
    A328.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2328.FinalEnergyKeyword_food


    # 1b. Subsector information
    # L2328.SubsectorLogit_food: Subsector logit exponents of food processing sector
    A328.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L2328.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2328.SubsectorLogit_food

    # and L2328.SubsectorShrwtFllt_food: Subsector shareweights of food processing sector
    A328.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L2328.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2328.SubsectorShrwtFllt_food

    # L2328.SubsectorInterp_food: Subsector shareweight interpolation of food processing sector
    A328.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L2328.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2328.SubsectorInterp_food


    # 1c. Technology information
    # L2328.StubTech_food: Identification of stub technologies of food processing
    # Note: assuming that technology list in the share weight table includes the full set (any others would default to a 0 shareweight)
    A328.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2328.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2328.StubTech_food

    # L2328.GlobalTechShrwt_food: Share weights of global food processing technologies
    A328.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L2328.GlobalTechShrwt_food

    # L2328.GlobalTechCoef_food: Energy inputs and coefficients of global food processing energy use technologies
    A328.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2328.globaltech_coef.long # intermediate tibble

    L2328.globaltech_coef.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2328.GlobalTechCoef_food

    # Secondary outputs of cogen technologies: these are input as a ratio
    # L2328.GlobalTechSecOut_food: Secondary output ratios of food processing cogeneration technologies
    A328.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_EFFICIENCY)) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      filter(!is.na(secondary.output)) %>%
      left_join_error_no_match(A23.chp_elecratio, by = c("subsector" = "fuel")) %>%
      mutate(output.ratio = elec_ratio * coefficient,
             output.ratio = round(output.ratio, energy.DIGITS_EFFICIENCY)) %>%
      # NOTE: holding the output ratio constant over time in future periods
      left_join_error_no_match(select(filter(., year == max(MODEL_BASE_YEARS)), -coefficient, -elec_ratio),
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input", "secondary.output")) %>%
      mutate(output.ratio = if_else(year.x %in% MODEL_BASE_YEARS, output.ratio.x, output.ratio.y)) %>%
      ungroup %>%
      rename(year = year.x,
             sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSecOut"]]) ->
      L2328.GlobalTechSecOut_food

    # Retirement information
    A328.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2328.globaltech_retirement_base

    # Copies first future year retirement information into all future years and appends back onto base year
    L2328.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2328.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2328.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2328.globaltech_retirement_future) ->
      L2328.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2328.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2328.globaltech_retirement$shutdown.rate))) {
      L2328.globaltech_retirement %>%
        filter(!is.na(L2328.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2328.GlobalTechShutdown_food
    }

    if(any(!is.na(L2328.globaltech_retirement$half.life))) {
      L2328.globaltech_retirement %>%
        filter(!is.na(L2328.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2328.GlobalTechSCurve_food
    }

    # L2328.GlobalTechLifetime_food: Global tech lifetime
    if(any(is.na(L2328.globaltech_retirement$shutdown.rate) & is.na(L2328.globaltech_retirement$half.life))) {
      L2328.globaltech_retirement %>%
        filter(is.na(L2328.globaltech_retirement$shutdown.rate) & is.na(L2328.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2328.GlobalTechLifetime_food
    }

    # L2328.GlobalTechProfitShutdown_food: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2328.globaltech_retirement$median.shutdown.point))) {
      L2328.globaltech_retirement %>%
        filter(!is.na(L2328.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2328.GlobalTechProfitShutdown_food
    }

    # L2328.GlobalTechCost_food: Non-energy costs of global food processing manufacturing technologies
    A328.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2328.GlobalTechCost_food

    # L2328.StubTechCost_food: Non-energy costs of regional food processing technologies for the overall food processing sector
    # regions without these costs specified regionally will use the global default values
    A328.regionaltech_cost %>%
      gather_years() %>%
      rename(stub.technology = technology, input.cost = value) %>%
      complete(nesting(supplysector, subsector, region, stub.technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(region, supplysector, subsector, stub.technology, minicam.non.energy.input, year) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 1),
           input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
    L2328.StubTechCost_food

    # generate warning if any region was not explicitly specified in the regional costs input file
    # the global default cost value will be used for the new region in that case
    if(!all(unique(A_regions$region) %in% unique(A328.regionaltech_cost$region))) {
      warning("New region added that is not specified in A328.regionaltech_cost.csv, default cost (global tech value) will be used for that region")
    }

    # L2328.GlobalTechTrackCapital_food: tracking capital investments
    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.FOOD_PROCESSING_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.FOOD_PROCESSING_CAP_PAYMENTS -1)
    L2328.GlobalTechCost_food %>%
      # we need to track investments in "energy" only, food output is not energy
      filter(sector.name == "process heat food processing") %>%
      mutate(capital.coef = socioeconomics.FOOD_PROCESSING_CAPITAL_RATIO / FCR,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # vintaging is active so no need for depreciation
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L2328.GlobalTechTrackCapital_food


    # L2328.StubTechCoef_food: region-specific coefficients of food processing technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1328.IO_EJPcal_R_food_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS[HISTORICAL_YEARS %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)]) %>%
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT)) %>%
      rename(supplysector = sector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("supplysector", "fuel")) %>%
      mutate(stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) %>%
      # Fill out the values in the final base year to all future years
      # Note: Fixed future coefficient for now
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = if_else(year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)], coefficient)) %>%
      ungroup() ->
      L2328.StubTechCoef_food


    # L2328.StubTechCalInput_food_heat: calibrated food production inputs to process heat (including cogen)
    L2328.GlobalTechCoef_food %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2328.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from regions without heat modeled
      filter(!(supplysector %in% unique(L2328.StubTechCoef_food[["supplysector"]]))) -> # filter for process heat only
      L2328.food_heat_tmp # tibble of all region/year/technology combinations for process heat

    L1328.in_EJ_R_food_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # remove electricity that is used directly rather than used for process heat
      # replace na later
      left_join(L1328.IO_EJPcal_R_food_F_Yh %>%
                  filter(fuel == "electricity") %>%
                  select(GCAM_region_ID, fuel, year, sector, non_heat_input = input),
                by = c("GCAM_region_ID", "fuel", "year", "sector")) %>%
      mutate(non_heat_input = replace_na(non_heat_input, 0),
             value = value - non_heat_input,
             supplysector = "process heat food processing",
             # to use just the standard type of each technology, label the technology as the fuel
             # this is necessary to make sure gas gets allocated to standard gas tech (rather than gas with solar), and electricity to standard electricity tech (rather than heat pump or electricity with solar)
             technology = fuel) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("sector", "supplysector", "fuel", "technology")) %>%
      mutate(stub.technology = technology, technology = NULL, GCAM_region_ID = NULL, fuel = NULL, sector = NULL, non_heat_input = NULL) ->
      L2328.StubTechCalInput_food_heat_tmp

    L2328.food_heat_tmp %>%
      # NA expected and replace with zero later
      left_join(L2328.StubTechCalInput_food_heat_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(value = replace_na(value, 0),
             calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name changeto accommodate function set_subsector_shrwt
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2328.StubTechCalInput_food_heat


    # L2328.StubTechProd_food: calibrated food production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_output # temporary tibble

    L1328.out_Pcal_R_food_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(sector = "food processing",
             calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_output, by = "sector") %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2328.StubTechProd_food


    # L2328.StubCalorieContent: demand for food processing set by food demand
    A328.demand %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name", "subsector0", "coefficient"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) %>%
      # Add all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(efficiency = 1 / coefficient) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechCalorieContent"]], "subsector0")) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L2328.StubCalorieContent

    # L2328.StubCaloriePriceConv: price conversion for food processing input to food demand
    A328.demand %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name", "subsector0", "price.unit.conversion"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) %>%
      # Add all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["StubTechCaloriePriceConv"]], "subsector0")) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L2328.StubCaloriePriceConv


    # =======================================================
    # Produce outputs
    L2328.Supplysector_food %>%
      add_title("Supply sector information for food processing sector") %>%
      add_units("NA") %>%
      add_comments("For food processing sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A328.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2328.Supplysector_food") %>%
      add_precursors("energy/A328.sector", "common/GCAM_region_names") ->
      L2328.Supplysector_food

    L2328.FinalEnergyKeyword_food %>%
      add_title("Supply sector keywords for food processing sector") %>%
      add_units("NA") %>%
      add_comments("For food processing sector, the supply sector final energy keywords from A328.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2328.FinalEnergyKeyword_food") %>%
      add_precursors("energy/A328.sector", "common/GCAM_region_names") ->
      L2328.FinalEnergyKeyword_food

    L2328.SubsectorLogit_food %>%
      add_title("Subsector logit exponents of food processing sector") %>%
      add_units("Unitless") %>%
      add_comments("For food processing sector, the subsector logit exponents from A328.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2328.SubsectorLogit_food") %>%
      add_precursors("energy/A328.subsector_logit", "energy/A_regions","common/GCAM_region_names") ->
      L2328.SubsectorLogit_food

    L2328.SubsectorShrwtFllt_food %>%
      add_title("Subsector shareweights of food processing sector") %>%
      add_units("unitless") %>%
      add_comments("For food processing sector, the subsector shareweights from A328.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2328.SubsectorShrwtFllt_food") %>%
      add_precursors("energy/A328.subsector_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2328.SubsectorShrwtFllt_food

    L2328.SubsectorInterp_food %>%
      add_title("Subsector shareweight interpolation of food processing sector") %>%
      add_units("NA") %>%
      add_comments("For food processing sector, the subsector shareweight interpolation function information from A328.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2328.SubsectorInterp_food") %>%
      add_precursors("energy/A328.subsector_interp", "energy/A_regions","common/GCAM_region_names") ->
      L2328.SubsectorInterp_food

    L2328.StubTech_food %>%
      add_title("Identification of stub technologies of food processing") %>%
      add_units("NA") %>%
      add_comments("For food proessing sector, the stub technologies from A328.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2328.StubTech_food") %>%
      add_precursors("energy/A328.globaltech_shrwt","energy/A_regions", "common/GCAM_region_names") ->
      L2328.StubTech_food

    L2328.GlobalTechShrwt_food %>%
      add_title("Shareweights of global food processing technologies") %>%
      add_units("Unitless") %>%
      add_comments("For food processing sector, the share weights from A328.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2328.GlobalTechShrwt_food") %>%
      add_precursors("energy/A328.globaltech_shrwt") ->
      L2328.GlobalTechShrwt_food

    L2328.GlobalTechCoef_food %>%
      add_title("Energy inputs and coefficients of food processing technologies") %>%
      add_units("Unitless") %>%
      add_comments("For food processing sector, the energy use coefficients from A328.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2328.GlobalTechCoef_food") %>%
      add_precursors("energy/A328.globaltech_coef") ->
      L2328.GlobalTechCoef_food

    L2328.GlobalTechCost_food %>%
      add_title("Non-energy costs of global food processing technologies") %>%
      add_units("1975$/Mcal for supplysector food processing; 1975$/GJ for supplysector process heat food processing") %>%
      add_comments("For food processing sector, the non-energy costs of global food processing technologies are calculated") %>%
      add_legacy_name("L2328.GlobalTechCost_food") %>%
      add_precursors("energy/A328.globaltech_cost") ->
      L2328.GlobalTechCost_food

    L2328.StubTechCost_food %>%
      add_title("Non-energy costs of regional food processing technologies") %>%
      add_units("1975$/Mcal for supplysector food processing") %>%
      add_comments("The non-energy costs of regional food processing overall sector are included") %>%
      add_legacy_name("L2328.StubTechCost_food") %>%
      add_precursors("energy/A328.regionaltech_cost") ->
      L2328.StubTechCost_food

    L2328.GlobalTechTrackCapital_food %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      same_precursors_as(L2328.GlobalTechCost_food) ->
      L2328.GlobalTechTrackCapital_food

    if(exists("L2328.GlobalTechShutdown_food")) {
      L2328.GlobalTechShutdown_food %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2328.GlobalTechShutdown_food") %>%
        add_precursors("energy/A328.globaltech_retirement") ->
        L2328.GlobalTechShutdown_food
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2328.GlobalTechShutdown_food") ->
        L2328.GlobalTechShutdown_food
    }

    if(exists("L2328.GlobalTechSCurve_food")) {
      L2328.GlobalTechSCurve_food %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2328.GlobalTechSCurve_food") %>%
        add_precursors("energy/A328.globaltech_retirement") ->
        L2328.GlobalTechSCurve_food
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2328.GlobalTechSCurve_food") ->
        L2328.GlobalTechSCurve_food
    }

    if(exists("L2328.GlobalTechLifetime_food")) {
      L2328.GlobalTechLifetime_food %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2328.GlobalTechLifetime_food") %>%
        add_precursors("energy/A328.globaltech_retirement") ->
        L2328.GlobalTechLifetime_food
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2328.GlobalTechLifetime_food") ->
        L2328.GlobalTechLifetime_food
    }

    if(exists("L2328.GlobalTechProfitShutdown_food")) {
      L2328.GlobalTechProfitShutdown_food %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2328.GlobalTechProfitShutdown_food") %>%
        add_precursors("energy/A328.globaltech_retirement") ->
        L2328.GlobalTechProfitShutdown_food
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2328.GlobalTechProfitShutdown_food") ->
        L2328.GlobalTechProfitShutdown_food
    }

    L2328.StubTechProd_food %>%
      add_title("calibrated food production") %>%
      add_units("Pcal") %>%
      add_comments("Values are calculated using L1328.out_Pcal_R_food_Yh, then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2328.StubTechProd_food") %>%
      add_precursors("energy/calibrated_techs",  "common/GCAM_region_names", "L1328.out_Pcal_R_food_Yh") ->
      L2328.StubTechProd_food

    L2328.StubTechCalInput_food_heat %>%
      add_title("calibrated food processing process heat production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1328.in_EJ_R_food_F_Yh then added GCAM region information and supplysector, subsector, technology, and input information") %>%
      add_legacy_name("L2328.StubTechCalInput_food") %>%
      add_precursors("energy/calibrated_techs", "L1328.in_EJ_R_food_F_Yh", "common/GCAM_region_names",
                     "L1328.IO_EJPcal_R_food_F_Yh", "energy/A_regions", "energy/A328.globaltech_coef") ->
      L2328.StubTechCalInput_food_heat

    L2328.StubTechCoef_food %>%
      add_title("region-specific coefficients of food processing technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients calculated based on energy from regional energy (IEA) and production (FAO) data") %>%
      add_legacy_name("L2328.StubTechCoef_food") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "L1328.IO_EJPcal_R_food_F_Yh") ->
      L2328.StubTechCoef_food

    L2328.StubCalorieContent %>%
      add_title("Calories of food processing per food crop demand") %>%
      add_units("unitless") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_precursors("common/GCAM_region_names", "energy/A328.demand") ->
      L2328.StubCalorieContent

    L2328.StubCaloriePriceConv %>%
      add_title("Price unit conversion for calories of food processing per food crop demand") %>%
      add_units("unitless") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_precursors("common/GCAM_region_names", "energy/A328.demand") ->
      L2328.StubCaloriePriceConv

    L2328.GlobalTechSecOut_food %>%
      add_title("Secondary output ratios of food processing cogeneration technologies") %>%
      add_units("Unitless") %>%
      add_comments("Secondary output ratios are calculated as electricity ratio (Assumed CHP electricity output per unit fuel input) over efficiency") %>%
      add_legacy_name("L2328.GlobalTechSecOut_food") %>%
      add_precursors("energy/A23.chp_elecratio", "energy/A328.globaltech_coef") ->
      L2328.GlobalTechSecOut_food

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
