# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2239.CSP_reeds_USA
#'
#' Create updated solar CSP resource supply curves consistent with ReEDS.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2239.DeleteUnlimitRsrc_reeds_USA}, \code{L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA},
#' \code{L2239.RenewRsrc_CSP_reeds_USA}, \code{L2239.GrdRenewRsrcCurves_CSP_reeds_USA},
#' \code{L2239.GrdRenewRsrcMax_CSP_reeds_USA}, \code{L2239.StubTechEffFlag_CSP_reeds_USA},
#' \code{L2239.StubTechCapFactor_CSP_reeds_USA}, \code{L2239.RenewRsrcTechChange_CSP_reeds_USA},
#' \code{L2239.StubTechCost_CSP_reeds_USA}, \code{L2239.ResTechShrwt_CSP_reeds_USA}.
#' The corresponding file in the original data system was \code{L2239.CSP_reeds_USA.R} (gcam-usa level2).
#' @details Create state-level solar CSP resource supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB September 2018, AJS June 2019
module_gcamusa_L2239.CSP_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/reeds_regions_states',
             FILE = 'gcam-usa/states_subregions',
             FILE = 'gcam-usa/reeds_CSP_curve_capacity',
             FILE = 'gcam-usa/reeds_CSP_curve_CF',
             FILE = 'gcam-usa/reeds_CSP_curve_grid_cost',
             FILE = 'gcam-usa/A23.elecS_tech_mapping_cool',
             FILE = 'gcam-usa/non_reeds_CSP_grid_cost',
             FILE = 'gcam-usa/NREL_us_re_technical_potential',
             FILE = 'gcam-usa/NREL_us_re_capacity_factors',
             FILE = "gcam-usa/A10.renewable_resource_delete",
             FILE = 'energy/A10.rsrc_info',
             'L2234.StubTechCapFactor_elecS_solar_USA',
             'L2234.StubTechMarket_elecS_USA',
             'L2234.GlobalIntTechCapital_elecS_USA',
             'L223.GlobalIntTechCapital_elec',
             'L223.GlobalIntTechOMfixed_elec'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2239.DeleteUnlimitRsrc_reeds_USA",
             "L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA",
             "L2239.RenewRsrc_CSP_reeds_USA",
             "L2239.GrdRenewRsrcCurves_CSP_reeds_USA",
             "L2239.GrdRenewRsrcMax_CSP_reeds_USA",
             "L2239.StubTechEffFlag_CSP_reeds_USA",
             "L2239.StubTechCapFactor_CSP_reeds_USA",
             "L2239.RenewRsrcTechChange_CSP_reeds_USA",
             "L2239.StubTechCost_CSP_reeds_USA",
             "L2239.ResTechShrwt_CSP_reeds_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    reeds_regions_states <- get_data(all_data, 'gcam-usa/reeds_regions_states')
    states_subregions <- get_data(all_data, 'gcam-usa/states_subregions', strip_attributes = TRUE)
    reeds_CSP_curve_capacity <- get_data(all_data, 'gcam-usa/reeds_CSP_curve_capacity')
    reeds_CSP_curve_CF <- get_data(all_data, 'gcam-usa/reeds_CSP_curve_CF')
    NREL_us_re_technical_potential <- get_data(all_data, 'gcam-usa/NREL_us_re_technical_potential')
    NREL_us_re_capacity_factors <- get_data(all_data, 'gcam-usa/NREL_us_re_capacity_factors')
    non_reeds_CSP_grid_cost <- get_data(all_data, 'gcam-usa/non_reeds_CSP_grid_cost')
    reeds_CSP_curve_grid_cost <- get_data(all_data, 'gcam-usa/reeds_CSP_curve_grid_cost')
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete")
    A10.rsrc_info <- get_data(all_data, 'energy/A10.rsrc_info')
    L2234.StubTechCapFactor_elecS_solar_USA <- get_data(all_data, 'L2234.StubTechCapFactor_elecS_solar_USA', strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, 'L2234.StubTechMarket_elecS_USA', strip_attributes = TRUE)
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, 'L2234.GlobalIntTechCapital_elecS_USA', strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, 'L223.GlobalIntTechCapital_elec')
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, 'L223.GlobalIntTechOMfixed_elec')

    # Silence package checks
    region <- state <- states_list <- sector.name <- subsector.name <- intermittent.technology <-
      supplysector <- subsector <- stub.technology <- year <- input.capital <- capital.overnight <-
      fixed.charge.rate <- input.OM.fixed <- OM.fixed <- CF <- cspsc1 <- cspsc2 <- cspsc3 <-
      cspsc4 <- cspsc5 <- Region <- CSP.class <- resource.potential.MW <- State <- resource.potential.EJ <-
      fcr <- price <- Pmin <- Pvar <- CFmax <- available <- grade <- extractioncost <- maxSubResource <-
      k1 <- capital.tech.change.5yr <- k2 <- tech.change.5yr <- tech.change <- bin <- cost <- grid.cost <-
      renewresource <- sub.renewable.resource <- year.fillout <- minicam.energy.input <- efficiency <-
      market.name <- flag <- capacity.factor <- input.cost <- capital.tech.change.period <-
      tech.change.period <- time.change <- subresource <- CSP_GWh <- CSP <- resource <- value <-
      non_reeds_state <- technology <- subsector_1 <- to.technology <- subsector.name0 <- NULL

    # ===================================================
    # Data Processing

    # First, process the states not included in the REEDS data, so they can be easily merged into the ReEDS data
    # and associated processing pipeline

    # L2239.non_reeds_states: Create a list of states not in the ReEDS data
    reeds_regions_states %>%
      distinct(State) -> reeds_states

    states_subregions %>%
      select(State=state) %>%
      anti_join(reeds_states, by = "State") %>%
      pull(State) -> L2239.non_reeds_states

    # L2239.non_reeds_states_CSP_technical_potential: CSP technical potential for non-ReEDS states
    NREL_us_re_technical_potential %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, CSP_GWh) %>%
      filter(State %in% L2239.non_reeds_states,
             CSP_GWh > 0) -> L2239.non_reeds_states_CSP_technical_potential

    # L2239.non_reeds_states_CSP_capacity_factor: CSP capacity factor for non-ReEDS states
    NREL_us_re_capacity_factors %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, CF = CSP) %>%
      filter(State %in% L2239.non_reeds_states,
             CF > 0) -> L2239.non_reeds_states_CSP_capacity_factor

    # L2239.CSP_potential_EJ_non_reeds_states: NREL data set does not include resource class, which is needed for data processing below
    # Create a Dummy Class for Resource Potential - assume a Class 4 resource, which is the lowest starting class for most other states
    # Also convert GWh to EJ values
    L2239.non_reeds_states_CSP_technical_potential %>%
      select(State, CSP_GWh) %>%
      mutate(CSP.class = "cspclass4",
             resource.potential.EJ = CSP_GWh * CONV_GWH_EJ) %>%
      select(-CSP_GWh) -> L2239.CSP_potential_EJ_non_reeds_states

    # L2239.CSP_CF_non_reeds_states: Process data for capacity factor.
    # The class data will be joined from the potential table in order to create a capacity factor table by state and class
    L2239.non_reeds_states_CSP_capacity_factor %>%
      left_join_error_no_match(L2239.CSP_potential_EJ_non_reeds_states, by = "State") %>%
      select(State, class = CSP.class, CF) -> L2239.CSP_CF_non_reeds_states


    # Second, process ReEDS data and combine with data from other states

    # L2239.CSP_CF: Capacity factors for CSP systems by class
    # Calculating average capacity factor by CSP class. Note that capacity factor data by region is not available.
    # Hence we assume same representative capacity factors by class across states.
    # Also note that since capacity factors are given by timeslice, we need to ignore 0 capacity factors.
    reeds_CSP_curve_CF %>%
      group_by(class) %>%
      summarise(CF = mean(CF)) %>%
      ungroup() %>%
      # map to ReEDS states so that non-ReEDS states can be added later
      repeat_add_columns(reeds_states) %>%
      semi_join(states_subregions, by = c("State" = "state")) -> L2239.CSP_CF

    # Merge capacity factor data from non-ReEDS states
    L2239.CSP_CF %>%
      bind_rows(L2239.CSP_CF_non_reeds_states) -> L2239.CSP_CF

    # L2239.CSP_potential_EJ: Resource potential in EJ by state and class
    # Calculate the resource potential in EJ in each ReEDS region and class my multiplying the
    # potential in MW with the average representative capacity factor for each class obtained above.
    # This is then aggregated up to the state-level.
    reeds_CSP_curve_capacity %>%
      replace_na(list(cspsc1 = 0, cspsc2 = 0, cspsc3 = 0, cspsc4 = 0, cspsc5 = 0)) %>%
      mutate(resource.potential.MW = cspsc1 + cspsc2 + cspsc3 + cspsc4 + cspsc5) %>%
      select(Region, CSP.class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_regions_states, by = "Region") %>%
      left_join_error_no_match(L2239.CSP_CF, by = c("State", "CSP.class" = "class")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CONV_MWH_EJ) %>%
      group_by(State, CSP.class) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
      ungroup() -> L2239.CSP_potential_EJ

    # Merge data with ReEDS potential data
    L2239.CSP_potential_EJ %>%
      bind_rows(L2239.CSP_potential_EJ_non_reeds_states) -> L2239.CSP_potential_EJ

    # L2239.CSP_matrix: Creating a matrix of costs (1975$/GJ) and resource potential (EJ) by state and class
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "CSP_peak",
             year == max(MODEL_BASE_YEARS)) %>%
      select(capital.overnight) -> L2239.CSP_capital
    L2239.CSP_capital <- as.numeric(L2239.CSP_capital)

    L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology == "CSP",
             year == max(MODEL_BASE_YEARS)) %>%
      select(fixed.charge.rate) -> L2239.fcr
    L2239.fcr <- as.numeric(L2239.fcr)

    L223.GlobalIntTechOMfixed_elec %>%
      filter(intermittent.technology == "CSP",
             year == max(MODEL_BASE_YEARS)) %>%
      select(OM.fixed) -> L2239.CSP_OMfixed
    L2239.CSP_OMfixed <- as.numeric(L2239.CSP_OMfixed)

    L2239.CSP_potential_EJ %>%
      left_join_error_no_match(L2239.CSP_CF, by = c("State", "CSP.class" ="class")) %>%
      mutate(capital.overnight = L2239.CSP_capital,
             fcr = L2239.fcr,
             OM.fixed = L2239.CSP_OMfixed,
             price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ + OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) ->
      L2239.CSP_matrix

    # From the matrix of costs and supplies obtained above, we create a graded resource curve for Pvar versus supply.
    # Unlike the graded resource curves for depletable sources,each grade for the renewresource represents
    # the fraction of maximum resource (cumulative) and price. Hence, we first create a matrix of cumulative resource
    # and price (Pvar).
    L2239.CSP_matrix %>%
      group_by(State) %>%
      arrange(State, price) %>%
      mutate(Pmin = min(price),
             Pvar = price - Pmin,
             CFmax = max(CF),
             available = round(resource.potential.EJ, energy.DIGITS_MAX_SUB_RESOURCE),
             extractioncost = round(Pvar, energy.DIGITS_COST),
             CFmax = round(CFmax, energy.DIGITS_CAPACITY_FACTOR),
             grade = paste ("grade", row_number(), sep = ' '),
             available = cumsum(available)) %>%
      ungroup() %>%
      select(State, grade, available, extractioncost, CFmax) -> L2239.CSP_curve

    # Calculating maxSubResource for the graded renewable resource supply curves
    L2239.CSP_curve %>%
      group_by(State) %>%
      arrange(State, extractioncost) %>%
      mutate(maxSubResource = max(available)) %>%
      ungroup() %>%
      distinct(State, maxSubResource) -> L2239.maxSubResource_CSP

    # The points on the graded curves need to be read in as fractions of the maxSubResource
    L2239.CSP_curve %>%
      left_join_error_no_match(L2239.maxSubResource_CSP, by ="State") %>%
      mutate(available = available / maxSubResource,
             # Adjusting the curves so that we have a supply of 0 at a Pvar of 0.
             # The available resource potential is accounted for in the subsequent grade
             # because of the cumulative calculation above.
             available= if_else(grade == "grade 1", 0, available)) -> L2239.CSP_curve

    # L2239.single_grade_states: Determine the list of single grade states
    L2239.CSP_curve %>%
      filter(grade == "grade 2") %>%
      select(State) -> L2239.multi_grade_states

    L2239.CSP_curve %>%
      distinct(State) %>%
      anti_join(L2239.multi_grade_states, by = "State") %>%
      pull(State) -> L2239.single_grade_states

    # L2239.CSP_curve_single_grade: Extract the relevant states from the graded curves
    L2239.CSP_curve %>%
      filter(State %in% L2239.single_grade_states) -> L2239.CSP_curve_single_grade

    # We get the unlimited global solar resource price which will be applied to the one grade states by creating a dummy second grade which will utilize the maxsubresource for the given state
    A10.rsrc_info %>%
      gather_years() %>%
      filter(resource == "global solar resource",
             year == max(year)) %>%
      pull(value) -> A10_solar_cost

    # Make a second grade for all the single grade states by using full maxsubresource percebtage as available value and global solar resource price as extraction cost
    L2239.CSP_curve_single_grade %>%
      mutate(grade= "grade 2",
             available = maxSubResource / maxSubResource,
             extractioncost = A10_solar_cost) -> L2239.CSP_curve_single_grade

    # Add the second grade to the graded curve
    L2239.CSP_curve %>%
      bind_rows(L2239.CSP_curve_single_grade) -> L2239.CSP_curve

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
    # a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed), where
    # k1 = FCR / (CONV_YEAR_HOURS * kWh_GJ) and k2 = 1 / (CONV_YEAR_HOURS * kWh_GJ)
    # Thus, we calculate model input parameter techChange (which is the reduction per year) as 1 - a'^(1/5).
    # This approach ignores changes in fixed OM costs over time.
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "CSP_peak") %>%
      select(year, capital.overnight) %>%
      mutate(capital.tech.change.period = lag(capital.overnight, 1) / capital.overnight,
             time.change = year - lag(year),
             fixed.charge.rate = L2239.fcr,
             OM.fixed = L2239.CSP_OMfixed,
             k1 = fixed.charge.rate / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM.fixed) / (k1 * capital.overnight + k2 * OM.fixed),
             tech.change = round(abs(1 - (tech.change.period) ^ (1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
      select(year, tech.change) %>%
      filter(!is.na(tech.change),
             year > max(MODEL_BASE_YEARS)) -> L2239.CSP_curve_tech_change

    # Capacity factors at the technology level need to be updated for all states that have the resource available.
    # Hence, creating a list of all states.
    states_list_CSP <- unique(L2239.CSP_curve$State)

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state.
    # Our starting data comprises of grid connection costs in $/MW by ReEDS region and CSP class.
    # This data also categorizes the connection cost into five bins in each region and class.
    # We first take the minimum cost (or mean, whichever is less) for a region and class.
    # Using this data, we then obtain grid connection cost in $/GJ for each region and class as
    # FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS * CF * MWh_GJ).
    # Costs are then obtained for a state by taking the minimum or the mean, whichever is less.
    # This method gives us costs that lines up with ReEDS national level average estimates.
    # We also filter out states without CSP resources
    # In the future, we might think about a separate state-level curve for grid connection costs.
    reeds_CSP_curve_grid_cost %>%
      gather(bin, cost, -Region, -CSP.class) %>%
      replace_na(list(cost = 0)) %>%
      group_by(Region, CSP.class) %>%
      summarise(cost = min(min(cost[cost>0]),mean(cost))) %>%
      ungroup() %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 select(Region, State),
                               by = "Region") %>%
      left_join_error_no_match(L2239.CSP_CF, by = c("State", "CSP.class" ="class")) %>%
      mutate(fcr = L2239.fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ),
             grid.cost = grid.cost* gdp_deflator(1975, 2004)) %>%
      group_by(State) %>%
      summarise(grid.cost = min(min(grid.cost), mean(grid.cost))) %>%
      ungroup() %>%
      mutate(grid.cost = round(grid.cost, energy.DIGITS_COST)) %>%
      filter(State %in% states_list_CSP) -> L2239.grid_cost

    # L2239.grid_cost_non_reeds_states: Calculate grid costs for non-ReEDS states
    # grid costs based on mapping to states with similar geography or grid costs
    # these assumptions can be changed in gcam-usa/non_reeds_PV_grid_cost
    non_reeds_CSP_grid_cost %>%
      left_join_error_no_match(L2239.grid_cost, by = c("comparison_state" = "State")) %>%
      select(State = non_reeds_state, grid.cost) -> L2239.grid_cost_non_reeds_states

    # Bind tables
    L2239.grid_cost %>%
      bind_rows(L2239.grid_cost_non_reeds_states) -> L2239.grid_cost


    # Preparing tables for output

    # Table to delete global solar resource
    # All states now have a PV_resource, and
    # all states which have CSP technologies have a CSP_resource
    states_subregions %>%
      distinct(region = state) %>%
      mutate(unlimited.resource = "global solar resource") %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it is never assigned a "global solar resource".
      # Use anti_join to remove DC from this table.
      anti_join(A10.renewable_resource_delete, by = c("region", "unlimited.resource" = "resource_elec_subsector")) ->
      L2239.DeleteUnlimitRsrc_reeds_USA

    # Table to read in renewresource, output.unit, price.unit and market
    L2239.CSP_curve %>%
      distinct(region = State) %>%
      mutate(renewresource = "CSP_resource",
             output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) -> L2239.RenewRsrc_CSP_reeds_USA

    # Table to create the graded resource curves
    L2239.CSP_curve %>%
      mutate(renewresource = "CSP_resource",
             sub.renewable.resource = "CSP_resource",
             available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region = State, renewresource, sub.renewable.resource, grade, available, extractioncost) ->
      L2239.GrdRenewRsrcCurves_CSP_reeds_USA

    # Table to read in maximum resource
    L2239.maxSubResource_CSP %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             renewresource = "CSP_resource",
             sub.renewable.resource = "CSP_resource",
             year.fillout = min(MODEL_YEARS)) %>%
      select(region = State, renewresource, sub.renewable.resource, year.fillout, maxSubResource ) ->
      L2239.GrdRenewRsrcMax_CSP_reeds_USA

    # Table to delete global solar resource minicam-energy-input
    L2234.StubTechMarket_elecS_USA %>%
      filter(region %in% states_list_CSP,
             grepl("CSP", stub.technology)) %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA

    # Table to read in energy inputs at the technology level
    L2234.StubTechMarket_elecS_USA %>%
      filter(region %in% states_list_CSP,
             grepl("CSP", stub.technology)) %>%
      mutate(minicam.energy.input = "CSP_resource",
             market.name = region,
             efficiency = 1,
             # Hard code in type "Resource" for intermittent technology resource input only
             flag = "Resource") %>%
      select(region, supplysector, subsector, stub.technology, year,
             minicam.energy.input, efficiency, market.name, flag) -> L2239.StubTechEffFlag_CSP_reeds_USA

    # Table to read in region-specific CFmax (that will be used to calculate Pmin within the model)
    L2234.StubTechCapFactor_elecS_solar_USA %>%
      filter(region %in% states_list_CSP,
             grepl("CSP", stub.technology),
             !grepl("storage", stub.technology)) %>%
      left_join_error_no_match(L2239.CSP_curve %>%
                                 distinct(State, CFmax),
                               by = c("region" = "State")) %>%
      mutate(capacity.factor = round(CFmax, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year, capacity.factor) ->
      L2239.StubTechCapFactor_CSP_nostorage_reeds_USA

    # We read in higher CFmax for CSP technologies with dedicated thermal storage.
    # This is in contrast to our approach for wind and PV because CSP w/ thermal storage is argued to
    # achieve better capacity factors compared to CSP systems without storage in the literature.
    # Howeevr, wind w/ battery storage and PV w/ battery storage technologies do not really have solid
    # arguments for higher capacity factors compared to their intermittent counterparts in the literature.
    # We give a capacity factor credit of 0.2 to CSP with thermal storage technologies.
    # This is rather arbitrary and needs looking into.
    # See also Muratori et al. 2017: Cost of power or power of cost: A US modeling perspective,
    # Renewable and Sustainable Energy Reviews, 77, pp.861-874.
    L2234.StubTechCapFactor_elecS_solar_USA %>%
      filter(region %in% states_list_CSP,
             grepl("CSP", stub.technology),
             grepl("storage", stub.technology)) %>%
      left_join_error_no_match(L2239.CSP_curve %>%
                                 distinct(State, CFmax),
                               by = c("region" = "State")) %>%
      mutate(capacity.factor = round(CFmax + 0.2, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year, capacity.factor) ->
      L2239.StubTechCapFactor_CSP_storage_reeds_USA

    L2239.StubTechCapFactor_CSP_nostorage_reeds_USA %>%
      bind_rows(L2239.StubTechCapFactor_CSP_storage_reeds_USA) -> L2239.StubTechCapFactor_CSP_reeds_USA

    # Copying tech change to all states and filtering out only the relevant states
    L2239.RenewRsrcTechChange_CSP_reeds_USA <- write_to_all_states(L2239.CSP_curve_tech_change, c("region", "year","tech.change"))
    L2239.RenewRsrcTechChange_CSP_reeds_USA %>%
      filter(region %in% states_list_CSP) %>%
      mutate(renewresource = "CSP_resource",
             sub.renewable.resource = "CSP_resource") %>%
      select(region, renewresource, sub.renewable.resource,
             year.fillout = year, techChange = tech.change) -> L2239.RenewRsrcTechChange_CSP_reeds_USA

    # Reading the grid connection cost as a state-level non-energy cost adder
    L2234.StubTechCapFactor_elecS_solar_USA %>%
      filter(region %in% states_list_CSP,
             grepl("CSP", stub.technology)) %>%
      select(region, supplysector, subsector, stub.technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L2239.grid_cost, by = c("region" = "State")) %>%
      rename(input.cost = grid.cost) %>%
      filter(!is.na(input.cost)) -> L2239.StubTechCost_CSP_reeds_USA

    # Establishing Shareweights
    L2239.GrdRenewRsrcMax_CSP_reeds_USA %>%
      select(region, resource = renewresource, subresource = sub.renewable.resource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L2239.ResTechShrwt_CSP_reeds_USA

    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs
    add_cooling_techs <- function(data){
      data_new <- data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology)%>%
        arrange(region,year)
      return(data_new)
    }
    L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA <- add_cooling_techs(L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA)
    L2239.StubTechEffFlag_CSP_reeds_USA <- add_cooling_techs(L2239.StubTechEffFlag_CSP_reeds_USA)
    L2239.StubTechCapFactor_CSP_reeds_USA <- add_cooling_techs(L2239.StubTechCapFactor_CSP_reeds_USA)
    L2239.StubTechCost_CSP_reeds_USA <- add_cooling_techs(L2239.StubTechCost_CSP_reeds_USA)


    # ===================================================
    # Produce outputs

    L2239.DeleteUnlimitRsrc_reeds_USA %>%
      add_title("Delete global solar resource for States with PV & CSP Resource Supply Curves") %>%
      add_units("NA") %>%
      add_comments("Only applies to those states in ReEDS PV & CSP data sets") %>%
      add_legacy_name("L2239.DeleteUnlimitRsrc_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_CSP_curve_capacity',
                     'gcam-usa/reeds_CSP_curve_CF',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     "gcam-usa/A10.renewable_resource_delete",
                     'energy/A10.rsrc_info',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2239.DeleteUnlimitRsrc_reeds_USA

    L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA %>%
      add_title("Delete global solar resource Energy Input for CSP Technologies") %>%
      add_units("NA") %>%
      add_comments("global solar resource input deleted; will be replaced by CSP_resource") %>%
      add_comments("Only applies to those states in ReEDS CSP data set") %>%
      add_legacy_name("L2239.DeleteStubTechMinicamEnergyInput_CSP_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_CSP_curve_capacity',
                     'gcam-usa/reeds_CSP_curve_CF',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'energy/A10.rsrc_info',
                     'L2234.StubTechMarket_elecS_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA

    L2239.RenewRsrc_CSP_reeds_USA %>%
      add_title("Market Information for CSP Resources") %>%
      add_units("NA") %>%
      add_comments("Only applies to states in ReEDS CSP data set") %>%
      add_legacy_name("L2239.RenewRsrc_CSP_USA_reeds") %>%
      same_precursors_as("L2239.DeleteUnlimitRsrc_reeds_USA") ->
      L2239.RenewRsrc_CSP_reeds_USA

    L2239.GrdRenewRsrcCurves_CSP_reeds_USA %>%
      add_title("Graded Supply Curves of CSP Resources at the State-Level") %>%
      add_units("available: fraction of maxSubResource; extractioncost: $1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2239.GrdRenewRsrcCurves_CSP_USA_reeds") %>%
      same_precursors_as("L2239.DeleteUnlimitRsrc_reeds_USA") ->
      L2239.GrdRenewRsrcCurves_CSP_reeds_USA

    L2239.GrdRenewRsrcMax_CSP_reeds_USA %>%
      add_title("Maximum Subresource Availability for CSP Resources at the State-Level") %>%
      add_units("EJ") %>%
      add_comments("Each grade represents the (cumulative) fraction of maxSubResource available at a given price") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2239.GrdRenewRsrcMax_CSP_USA_reeds") %>%
      same_precursors_as("L2239.DeleteUnlimitRsrc_reeds_USA") ->
      L2239.GrdRenewRsrcMax_CSP_reeds_USA

    L2239.StubTechEffFlag_CSP_reeds_USA %>%
      add_title("Market Information for CSP Technologies") %>%
      add_units("unitless") %>%
      add_comments("Only applies to 9 states in ReEDS CSP data set") %>%
      add_legacy_name("L2239.StubTechEffFlag_CSP_USA_reeds") %>%
      same_precursors_as("L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA") ->
      L2239.StubTechEffFlag_CSP_reeds_USA

    L2239.StubTechCapFactor_CSP_reeds_USA %>%
      add_title("State-specific Capacity Factors for CSP Technologies") %>%
      add_units("unitless") %>%
      add_comments("Capacity factors updated for all 18 states in the ReEDS CSP data set") %>%
      add_comments("This includes 9 regions with only 1 CSP grade which are not assigned CSP resource supply curves") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2239.StubTechCapFactor_CSP_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_CSP_curve_capacity',
                     'gcam-usa/reeds_CSP_curve_CF',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'L2234.StubTechCapFactor_elecS_solar_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2239.StubTechCapFactor_CSP_reeds_USA

    L2239.RenewRsrcTechChange_CSP_reeds_USA %>%
      add_title("Technological Change Parameter for CSP Resources") %>%
      add_units("unitless") %>%
      add_comments("Technological change in the supply curve is related to assumed improvements in capital cost") %>%
      add_legacy_name("L2239.RenewRsrcTechChange_CSP_USA_reeds") %>%
      same_precursors_as("L2239.DeleteUnlimitRsrc_reeds_USA") ->
      L2239.RenewRsrcTechChange_CSP_reeds_USA

    L2239.StubTechCost_CSP_reeds_USA %>%
      add_title("State-specific Grid Connection Cost Adders for CSP Technologies") %>%
      add_units("$1975/GJ") %>%
      add_comments("Grid connection cost adders are read in for all 18 states in the ReEDS CSP data set") %>%
      add_comments("This includes 9 regions with only 1 CSP grade which are not assigned CSP resource supply curves") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2239.StubTechCost_CSP_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_CSP_curve_capacity',
                     'gcam-usa/reeds_CSP_curve_CF',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'gcam-usa/reeds_CSP_curve_grid_cost',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'gcam-usa/non_reeds_CSP_grid_cost',
                     'L2234.StubTechCapFactor_elecS_solar_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2239.StubTechCost_CSP_reeds_USA

    L2239.ResTechShrwt_CSP_reeds_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_attributes_as(L2239.GrdRenewRsrcMax_CSP_reeds_USA) ->
      L2239.ResTechShrwt_CSP_reeds_USA

    return_data(L2239.DeleteUnlimitRsrc_reeds_USA,
                L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA,
                L2239.RenewRsrc_CSP_reeds_USA,
                L2239.GrdRenewRsrcCurves_CSP_reeds_USA,
                L2239.GrdRenewRsrcMax_CSP_reeds_USA,
                L2239.StubTechEffFlag_CSP_reeds_USA,
                L2239.StubTechCapFactor_CSP_reeds_USA,
                L2239.RenewRsrcTechChange_CSP_reeds_USA,
                L2239.StubTechCost_CSP_reeds_USA,
                L2239.ResTechShrwt_CSP_reeds_USA)

  } else {
    stop("Unknown command")
  }
}
