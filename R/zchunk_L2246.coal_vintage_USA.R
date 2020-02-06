# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2246.coal_vintage_USA
#'
#' Generates GCAM-USA model inputs to include detailed coal vintages.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2246.StubTechProd_coal_vintage_USA}, \code{L2246.StubTechEff_coal_vintage_USA},
#' \code{L2246.StubTechLifetime_coal_vintage_USA}, \code{L2246.StubTechSCurve_coal_vintage_USA},
#' \code{L2246.StubTechProfitShutdown_coal_vintage_USA}, \code{L2246.StubTechMarket_coal_vintage_USA},
#' \code{L2246.GlobalTechShrwt_coal_vintage_USA}, \code{L2246.GlobalTechEff_coal_vintage_USA},
#' \code{L2246.GlobalTechCapFac_coal_vintage_USA}, \code{L2246.GlobalTechCapital_coal_vintage_USA},
#' \code{L2246.GlobalTechOMfixed_coal_vintage_USA}, \code{L2246.GlobalTechOMvar_coal_vintage_USA}.
#' The corresponding file in the original data system was \code{L2246.coal_vintage.R} (gcam-usa level2).
#' @details This chunk creates input files to include detailed coal vintages. Calibrated outputs in 2010 are allocated based on
#' generation share by vintage groups in 2015 using EIA unit-level data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr complete nesting replace_na
#' @author RC Sep 2018
module_gcamusa_L2246.coal_vintage_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/prime_mover_map",
             FILE = "gcam-usa/EIA_860_generators_existing_2015",
             FILE = "gcam-usa/EIA_923_generator_gen_fuel_2015",
             FILE = "gcam-usa/EIA_860_generators_retired_2016",
             "L2240.StubTechProd_elec_coalret_USA",
             "L2240.StubTechEff_elec_coalret_USA",
             "L2240.GlobalTechEff_elec_coalret_USA",
             "L2240.GlobalTechCapFac_elec_coalret_USA",
             "L2240.GlobalTechCapital_elec_coalret_USA",
             "L2240.GlobalTechOMfixed_elec_coalret_USA",
             "L2240.GlobalTechOMvar_elec_coalret_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2246.StubTechProd_coal_vintage_USA",
             "L2246.StubTechEff_coal_vintage_USA",
             "L2246.StubTechLifetime_coal_vintage_USA",
             "L2246.StubTechSCurve_coal_vintage_USA",
             "L2246.StubTechProfitShutdown_coal_vintage_USA",
             "L2246.StubTechMarket_coal_vintage_USA",
             "L2246.GlobalTechShrwt_coal_vintage_USA",
             "L2246.GlobalTechEff_coal_vintage_USA",
             "L2246.GlobalTechCapFac_coal_vintage_USA",
             "L2246.GlobalTechCapital_coal_vintage_USA",
             "L2246.GlobalTechOMfixed_coal_vintage_USA",
             "L2246.GlobalTechOMvar_coal_vintage_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- supplysector <- subsector <- stub.technology <- technology <- stub.technology.new  <-
      unit.capacity.MW <- Retirement.Year <- Nameplate.Capacity..MW. <- Summer.Capacity..MW. <- State <-
      Plant.Code <- Generator.ID <- Operating.Year <- Planned.Retirement.Year <- Net.Generation.Year.To.Date <-
      Status <- gen_share <- Prime.Mover <- Reported.Prime.Mover <- Energy.Source.1 <- Reported.Fuel.Type.Code <-
      vintage.bin <- capacity <- generation <- gcam_fuel <- cap.lifetime <- lifetime <- avg.lifetime <-
      sector.name <- subsector.name <- median.shutdown.point <- profit.shutdown.steepness <-
      share.weight.year <- subs.share.weight <- share.weight <- tech.share.weight <- share.vintage <-
      calOutputValue <- capacity.factor <- input.capital <- capital.overnight <- fixed.charge.rate <-
      input.OM.fixed <- OM.fixed <- input.OM.var <- OM.var <- minicam.energy.input <- efficiency <-
      Non.CO2 <- input.emissions <- input.emissions.new <- gen_frac_2010_2005 <- gen_frac_curr_prev <- NULL

    # Load required inputs
    prime_mover_map <- get_data(all_data, "gcam-usa/prime_mover_map")
    eia_860_data_2015 <- get_data(all_data, "gcam-usa/EIA_860_generators_existing_2015")
    eia_923_data_2015 <- get_data(all_data, "gcam-usa/EIA_923_generator_gen_fuel_2015")
    eia_860_retired_2016 <- get_data(all_data, "gcam-usa/EIA_860_generators_retired_2016")
    L2240.StubTechProd_elec_coalret_USA <- get_data(all_data, "L2240.StubTechProd_elec_coalret_USA")
    L2240.StubTechEff_elec_coalret_USA <- get_data(all_data, "L2240.StubTechEff_elec_coalret_USA")
    L2240.GlobalTechEff_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechEff_elec_coalret_USA")
    L2240.GlobalTechCapFac_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechCapFac_elec_coalret_USA")
    L2240.GlobalTechCapital_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechCapital_elec_coalret_USA")
    L2240.GlobalTechOMfixed_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechOMfixed_elec_coalret_USA")
    L2240.GlobalTechOMvar_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechOMvar_elec_coalret_USA")

    # ===================================================
    # Perform computations

    # Create tables of coal units operating in 2015 that include vintage, capacity, generation and planned retirement year.
    eia_860_data_2015 %>%
      # Use nameplate capacity for now. Not sure if summer capacity makes more sense
      mutate(unit.capacity.MW = replace(Nameplate.Capacity..MW., is.na(Nameplate.Capacity..MW.),
                                        Summer.Capacity..MW.[is.na(Nameplate.Capacity..MW.)])) %>%
      left_join(prime_mover_map,
                by = c("Prime.Mover" = "Reported.Prime.Mover", "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
      filter(gcam_fuel == "coal") %>%
      select(State, Plant.Code, Generator.ID, Operating.Year, Planned.Retirement.Year, capacity = unit.capacity.MW) %>%
      # Obtain unit/generator-level generation in 2015 from generator-level generation data in Form 923.
      left_join(eia_923_data_2015 %>% rename(generation = Net.Generation.Year.To.Date),
                by = c("State", "Generator.ID" = "Generator.Id", "Plant.Code" = "Plant.Id")) %>%
      replace_na(list(generation = 0)) ->
      L2246.coal_units_gen_2015
    ##### The above processing results in U.S. coal generation in 2015 to equal 1339015117 MWh which is 1.0% less than
    ##### coal generation reported in EIA's Electricity data browser (1352 TWh).


    # Create a table of units retired until 2016.
    eia_860_retired_2016 %>%
      filter(Status == "RE") %>%
      # Use nameplate capacity for now. Not sure if summer capacity makes more sense
      mutate(unit.capacity.MW = replace(Nameplate.Capacity..MW., is.na(Nameplate.Capacity..MW.),
                                        Summer.Capacity..MW.[is.na(Nameplate.Capacity..MW.)])) %>%
      left_join(prime_mover_map,
                by = c("Prime.Mover" = "Reported.Prime.Mover", "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
      filter(gcam_fuel == "coal") %>%
      select(State, Plant.Code, Generator.ID, Operating.Year, Retirement.Year, capacity = unit.capacity.MW) %>%
      # Adding vintage bins for now for the sake of ease of independent processing
      mutate(vintage.bin = cut(Operating.Year, breaks = gcamusa.COAL_VINTAGE_BREAKS, labels = gcamusa.COAL_VINTAGE_LABELS)) ->
      L2246.coal_units_retired

    # Calculate the weighted-average lifetime of retired units that were built before 1970 by vintage bin
    L2246.coal_units_retired %>%
      filter(Operating.Year <= 1970) %>%
      mutate(lifetime = Retirement.Year - Operating.Year,
             cap.lifetime = capacity * lifetime) %>%
      group_by(vintage.bin) %>%
      summarise(avg.lifetime = sum(cap.lifetime) / sum(capacity)) %>%
      ungroup ->
      L2246.coal_units_retired_avg_lifetime


    # Combine unit-level generation and capacity data with retirement data.
    # This will generate a table of coal units operating in 2015.
    # The Retirement.year variable reflects actual retirements through 2016 or planned retirements.
    # And estimate retirement years data when missing:
    ## For units built before 1970, we assume that the lifetime for units without retirement information equals
    ## the national average lifetime of units retired in that vintage bin through 2016.
    ## For units built after 1970, we assume an average lifetime.

    L2246.coal_units_gen_2015 %>%
      left_join(L2246.coal_units_retired %>% select(State, Plant.Code, Generator.ID, Retirement.Year),
                by = c("State", "Plant.Code", "Generator.ID")) %>%
      # Note that NAs in Retirement year column indicate that the units were not retired through 2016. Repalce NAs with 9999 for now
      mutate(Retirement.Year = replace(Retirement.Year, is.na(Retirement.Year), Planned.Retirement.Year[is.na(Retirement.Year)]),
             # Categorize coal units by vintage bins. We stick with 5-year bins for now.
             vintage.bin = cut(Operating.Year, breaks = gcamusa.COAL_VINTAGE_BREAKS, labels = gcamusa.COAL_VINTAGE_LABELS)) %>%
      # Use left_join due to missing values in avg.lifetime, replace with the assumed value next
      left_join(L2246.coal_units_retired_avg_lifetime, by = "vintage.bin") %>%
      # For units built after 1970, we assume an average lifetime
      replace_na(list(avg.lifetime = gcamusa.AVG_COAL_PLANT_LIFETIME)) %>%
      # Estimate retirement years for those without retirement data
      mutate(Retirement.Year = replace(Retirement.Year, is.na(Retirement.Year),
                                       round(Operating.Year[is.na(Retirement.Year)] + avg.lifetime[is.na(Retirement.Year)], 0))) ->
      L2246.coal_units_gen_2015

    # Create a table of generation and capacity-weighted lifetime from 2010 and by state and vintage based 2015 units
    L2246.coal_units_gen_2015 %>%
      # Calculate lifetime from 2010 which is model bas year
      mutate(cap.lifetime = capacity * (Retirement.Year - 2010)) %>%
      group_by(State, vintage.bin) %>%
      summarise(lifetime = round(sum(cap.lifetime) / sum(capacity), 0), generation = sum(generation)) %>%
      mutate(generation = generation * CONV_MWH_GJ * CONV_GJ_EJ,
             # For 2015 units, if lifetimes are less than 5, we filter those out because we're already trying to "soft calibrate" 2015 using
             # the fast-retire split. For 2020 retirements, we'll chose only those vintages with lifetimes >= 5 and < 10
             lifetime = replace(lifetime, lifetime >= 5 & lifetime < 10, 10)) %>%
      ungroup() %>%
      filter(lifetime >= 5) %>%
      # Keep share of each vintage bin of the total generation in 2010 in each state ready to be applied to
      # calibrated value in 2010
      group_by(State) %>%
      mutate(share.vintage = generation / sum(generation)) %>%
      ungroup() ->
      L2246.coal_vintage_gen_2015

    # Apply vintage share by state to calibrated values for slow retire component and create table to be read in
    L2246.coal_vintage_gen_2015 %>%
      rename(region = State) %>%
      left_join(L2240.StubTechProd_elec_coalret_USA %>%
                  filter(subsector == "coal", year == max(MODEL_BASE_YEARS), grepl("slow_retire", stub.technology)), by = "region") %>%
      filter(!is.na(calOutputValue), calOutputValue != 0) %>%
      mutate(calOutputValue = calOutputValue * share.vintage,
             # Create new technologies. Naming the variable as stub.technology.new so that we can use stub.technology as reference later
             stub.technology.new = paste(stub.technology, vintage.bin, sep = " "),
             year = max(MODEL_BASE_YEARS), share.weight.year = max(MODEL_BASE_YEARS),
             subs.share.weight = 1, tech.share.weight = 1) %>%
      # Select variables. For now, include lifetime and vintage.bin as well. We'll remove it later
      select(LEVEL2_DATA_NAMES[["StubTechProd"]], stub.technology.new, lifetime, vintage.bin) ->
      L2246.StubTechProd_coal_vintage_USA_2010

    # Create a table to read lifetimes for vintage bin techs by state, for vintages with =< 20 year lifetime remaining
    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      mutate(stub.technology = stub.technology.new) %>%
      filter(lifetime <= gcamusa.COAL_REMAINING_LIFETIME) %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]]) ->
      L2246.StubTechLifetime_coal_vintage_USA

    # Create a table to read in S-curve parameters for vintage bin techs by state, for vintages with > 20 year lifetime remaining
    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      mutate(stub.technology = stub.technology.new) %>%
      filter(lifetime > gcamusa.COAL_REMAINING_LIFETIME) %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]]) %>%
      mutate(steepness = gcamusa.COAL_RETIRE_STEEPNESS,
             half.life = round(lifetime * (gcamusa.AVG_COAL_PLANT_HALFLIFE / gcamusa.AVG_COAL_PLANT_LIFETIME), 0)) ->
      L2246.StubTechSCurve_coal_vintage_USA

    # Read in profit shutdown decider for vintage technologies
    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechLifetime"]]) %>%
      mutate(median.shutdown.point = gcamusa.MEDIAN_SHUTDOWN_POINT, profit.shutdown.steepness = gcamusa.PROFIT_SHUTDOWN_STEEPNESS) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProfitShutdown"]]) ->
      L2246.StubTechProfitShutdown_coal_vintage_USA

    # Create a table to read in energy inputs and efficiencies for the new technologies in calibration years. Assuming
    # state average efficiency for all vintages since historically, there is not much correlation between
    # efficiency of a generator and its vintage.
    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], stub.technology.new) %>%
      complete(nesting(region, supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2240.StubTechEff_elec_coalret_USA,
                               by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2246.StubTechEff_coal_vintage_USA

    # Read in energy inputs for future periods
    L2246.StubTechEff_coal_vintage_USA %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-efficiency, -year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L2246.StubTechMarket_coal_vintage_USA


    # Create tables to read in energy and non-energy inputs for future years in global technology database

    # Create a basic strucure with common variables
    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      select(supplysector, subsector, stub.technology, stub.technology.new, year) %>%
      unique() %>%
      complete(nesting(supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_YEARS) ->
      L2246.GlobalTech

    # Energy inputs: Efficiency
    L2246.GlobalTech %>%
      left_join_error_no_match(L2240.GlobalTechEff_elec_coalret_USA,
                               by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2246.GlobalTechEff_coal_vintage_USA

    # Non-energy inputs: capacity factor, capital costs, fixed and variable OM costs
    # Capacity factor:
    L2246.GlobalTech %>%
      left_join_error_no_match(L2240.GlobalTechCapFac_elec_coalret_USA,
                               by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapFac"]]) ->
      L2246.GlobalTechCapFac_coal_vintage_USA

    # Capita costs:
    L2246.GlobalTech %>%
      left_join_error_no_match(L2240.GlobalTechCapital_elec_coalret_USA,
                               by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2246.GlobalTechCapital_coal_vintage_USA

    # Fixed OM costs:
    L2246.GlobalTech %>%
      left_join_error_no_match(L2240.GlobalTechOMfixed_elec_coalret_USA,
                               by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2246.GlobalTechOMfixed_coal_vintage_USA

    # Variable OM costs
    L2246.GlobalTech %>%
      left_join_error_no_match(L2240.GlobalTechOMvar_elec_coalret_USA,
                               by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2246.GlobalTechOMvar_coal_vintage_USA

    # Create table to read in shareweights in future years in global technology database
    L2246.GlobalTech %>%
      mutate(share.weight = 0) %>%
      rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2246.GlobalTechShrwt_coal_vintage_USA

    # Clean up StubTechProd table

    L2246.StubTechProd_coal_vintage_USA_2010 %>%
      mutate(stub.technology = stub.technology.new) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      complete(nesting(region, supplysector, subsector, stub.technology), year = MODEL_BASE_YEARS) %>%
      mutate(share.weight.year = year) %>%
      # Read in zero caloutputvalue for other base years
      replace_na(list(calOutputValue = 0, subs.share.weight = 1, tech.share.weight = 0)) ->
      L2246.StubTechProd_coal_vintage_USA

    # Read in zero calOutputValue for 2010 for existing coal conv pul technology
    L2240.StubTechProd_elec_coalret_USA %>%
                  filter(subsector == "coal", year == max(MODEL_BASE_YEARS), calOutputValue != 0,
                         grepl("slow_retire", stub.technology), region %in% unique(L2246.coal_vintage_gen_2015$State)) %>%
                  mutate(calOutputValue = 0, tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
      bind_rows(L2246.StubTechProd_coal_vintage_USA) %>%
      arrange(region, year) ->
      L2246.StubTechProd_coal_vintage_USA


    # ===================================================
    # Produce outputs

    L2246.StubTechProd_coal_vintage_USA %>%
      add_title("Calibration outputs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("EJ") %>%
      add_comments("Generation shares by vintage are calculated based on EIA unit-level 2015 data") %>%
      add_comments("Generation shares by vintage are then applied to slow_retire stub-technology 2010 generation in each state") %>%
      add_comments("Generation in other base years are set to zero to each vintage stub-technology") %>%
      add_legacy_name("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("gcam-usa/prime_mover_map",
                     "gcam-usa/EIA_860_generators_existing_2015",
                     "gcam-usa/EIA_923_generator_gen_fuel_2015",
                     "L2240.StubTechProd_elec_coalret_USA") ->
      L2246.StubTechProd_coal_vintage_USA

    L2246.StubTechEff_coal_vintage_USA %>%
      add_title("Efficiencies of slow_retire conventional coal electricity plants by detailed vintage and state in calibration years") %>%
      add_units("Unitless") %>%
      add_comments("Apply the same efficiencies to all vintage groups") %>%
      add_legacy_name("L2246.StubTechEff_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.StubTechEff_elec_coalret_USA") ->
      L2246.StubTechEff_coal_vintage_USA

    L2246.StubTechLifetime_coal_vintage_USA %>%
      add_title("Lifetime of slow_retire conventional coal electricity plants from 2010 by detailed vintage and state") %>%
      add_units("Years") %>%
      add_comments("Average lifetime for each vintage group is weighted by capacity, based on EIA unit-level 2015 data") %>%
      add_comments("Only for vintage groups with 20 or fewer years of lifetime remaining") %>%
      add_legacy_name("L2246.StubTechLifetime_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("gcam-usa/EIA_860_generators_retired_2016") ->
      L2246.StubTechLifetime_coal_vintage_USA

    L2246.StubTechSCurve_coal_vintage_USA %>%
      add_title("Lifetime and retirement parameters for slow_retire conventional coal electricity plants from 2010 by detailed vintage and state") %>%
      add_units("Years") %>%
      add_comments("Average lifetime for each vintage group is weighted by capacity, based on EIA unit-level 2015 data") %>%
      add_comments("Only for vintage groups with greater than 20 years of lifetime remaining") %>%
      same_precursors_as("L2246.StubTechLifetime_coal_vintage_USA") ->
      L2246.StubTechSCurve_coal_vintage_USA

    L2246.StubTechProfitShutdown_coal_vintage_USA %>%
      add_title("Profit shutdown decider for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.StubTechProfitShutdown_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechLifetime_coal_vintage_USA") ->
      L2246.StubTechProfitShutdown_coal_vintage_USA

    L2246.StubTechMarket_coal_vintage_USA %>%
      add_title("Energy inputs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.StubTechMarket_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechEff_coal_vintage_USA") ->
      L2246.StubTechMarket_coal_vintage_USA

    L2246.GlobalTechShrwt_coal_vintage_USA %>%
      add_title("Shareweights for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Set zero shareweights for all vintage stub-technologies") %>%
      add_legacy_name("L2246.GlobalTechShrwt_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") ->
      L2246.GlobalTechShrwt_coal_vintage_USA

    L2246.GlobalTechEff_coal_vintage_USA %>%
      add_title("Efficiencies for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.GlobalTechEff_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.GlobalTechEff_elec_coalret_USA") ->
      L2246.GlobalTechEff_coal_vintage_USA

    L2246.GlobalTechCapFac_coal_vintage_USA %>%
      add_title("Capacity factors for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.GlobalTechCapFac_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.GlobalTechCapFac_elec_coalret_USA") ->
      L2246.GlobalTechCapFac_coal_vintage_USA

    L2246.GlobalTechCapital_coal_vintage_USA %>%
      add_title("Capital costs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.GlobalTechCapital_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.GlobalTechCapital_elec_coalret_USA") ->
      L2246.GlobalTechCapital_coal_vintage_USA

    L2246.GlobalTechOMfixed_coal_vintage_USA %>%
      add_title("Fixed OM costs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.GlobalTechOMfixed_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.GlobalTechOMfixed_elec_coalret_USA") ->
      L2246.GlobalTechOMfixed_coal_vintage_USA

    L2246.GlobalTechOMvar_coal_vintage_USA %>%
      add_title("Variable OM costs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("1975$/MWh") %>%
      add_comments("Same to all vintage groups") %>%
      add_legacy_name("L2246.GlobalTechOMvar_coal_vintage_USA") %>%
      same_precursors_as("L2246.StubTechProd_coal_vintage_USA") %>%
      add_precursors("L2240.GlobalTechOMvar_elec_coalret_USA") ->
      L2246.GlobalTechOMvar_coal_vintage_USA

    return_data(L2246.StubTechProd_coal_vintage_USA,
                L2246.StubTechEff_coal_vintage_USA,
                L2246.StubTechLifetime_coal_vintage_USA,
                L2246.StubTechSCurve_coal_vintage_USA,
                L2246.StubTechProfitShutdown_coal_vintage_USA,
                L2246.StubTechMarket_coal_vintage_USA,
                L2246.GlobalTechShrwt_coal_vintage_USA,
                L2246.GlobalTechEff_coal_vintage_USA,
                L2246.GlobalTechCapFac_coal_vintage_USA,
                L2246.GlobalTechCapital_coal_vintage_USA,
                L2246.GlobalTechOMfixed_coal_vintage_USA,
                L2246.GlobalTechOMvar_coal_vintage_USA)

  } else {
    stop("Unknown command")
  }
}
