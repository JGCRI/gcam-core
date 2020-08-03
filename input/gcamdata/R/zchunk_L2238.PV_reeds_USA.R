# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2238.PV_reeds_USA
#'
#' Create updated solar PV resource supply curves consistent with ReEDS.
#' Also add non-ReEDS states (AK,DC,HI) based on NREL technical potential data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA}, \code{L2238.RenewRsrc_PV_reeds_USA},
#' \code{L2238.GrdRenewRsrcCurves_PV_reeds_USA}, \code{L2238.GrdRenewRsrcMax_PV_reeds_USA},
#' \code{L2238.StubTechEffFlag_PV_reeds_USA}, \code{L2238.StubTechCapFactor_PV_reeds_USA},
#' \code{L2238.RenewRsrcTechChange_PV_reeds_USA}, and \code{L2238.StubTechCost_PV_reeds_USA},
#' \code{L2238.ResTechShrwt_PV_reeds_USA}.
#' The corresponding file in the original data system was \code{L2238.PV_reeds_USA.R} (gcam-usa level2).
#' @details Create state-level solar PV resource supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB September 2018; AJS June 2019
module_gcamusa_L2238.PV_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'energy/A10.rsrc_info',
             FILE = 'gcam-usa/states_subregions',
             FILE = 'gcam-usa/reeds_regions_states',
             FILE = 'gcam-usa/reeds_PV_curve_capacity',
             FILE = 'gcam-usa/reeds_PV_curve_CF_avg',
             FILE = 'gcam-usa/reeds_PV_curve_grid_cost',
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = 'gcam-usa/non_reeds_PV_grid_cost',
             FILE = 'gcam-usa/NREL_us_re_technical_potential',
             FILE = 'gcam-usa/NREL_us_re_capacity_factors',
             FILE = "gcam-usa/A10.renewable_resource_delete",
             'L2234.StubTechCapFactor_elecS_solar_USA',
             'L2234.StubTechMarket_elecS_USA',
             'L2234.GlobalIntTechCapital_elecS_USA',
             'L223.GlobalIntTechCapital_elec',
             'L223.GlobalIntTechOMfixed_elec'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA",
             "L2238.RenewRsrc_PV_reeds_USA",
             "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
             "L2238.GrdRenewRsrcMax_PV_reeds_USA",
             "L2238.StubTechEffFlag_PV_reeds_USA",
             "L2238.StubTechCapFactor_PV_reeds_USA",
             "L2238.RenewRsrcTechChange_PV_reeds_USA",
             "L2238.StubTechCost_PV_reeds_USA",
             "L2238.ResTechShrwt_PV_reeds_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A10.rsrc_info <- get_data(all_data, 'energy/A10.rsrc_info')
    states_subregions <- get_data(all_data, 'gcam-usa/states_subregions')
    reeds_regions_states <- get_data(all_data, 'gcam-usa/reeds_regions_states')
    reeds_PV_curve_capacity <- get_data(all_data, 'gcam-usa/reeds_PV_curve_capacity')
    reeds_PV_curve_CF_avg <- get_data(all_data, 'gcam-usa/reeds_PV_curve_CF_avg')
    reeds_PV_curve_grid_cost <- get_data(all_data, 'gcam-usa/reeds_PV_curve_grid_cost')
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    non_reeds_PV_grid_cost <- get_data(all_data, 'gcam-usa/non_reeds_PV_grid_cost')
    NREL_us_re_technical_potential <- get_data(all_data, 'gcam-usa/NREL_us_re_technical_potential')
    NREL_us_re_capacity_factors <- get_data(all_data, 'gcam-usa/NREL_us_re_capacity_factors')
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete")
    L2234.StubTechCapFactor_elecS_solar_USA <- get_data(all_data, 'L2234.StubTechCapFactor_elecS_solar_USA', strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, 'L2234.StubTechMarket_elecS_USA', strip_attributes = TRUE)
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, 'L2234.GlobalIntTechCapital_elecS_USA', strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, 'L223.GlobalIntTechCapital_elec')
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, 'L223.GlobalIntTechOMfixed_elec')

    # Silence package checks
    region <- state <- states_list <- sector.name <- subsector.name <- intermittent.technology <-
      supplysector <- subsector <- stub.technology <- year <- input.capital <- capital.overnight <-
      fixed.charge.rate <- input.OM.fixed <- OM.fixed <- BA <- State <- PV.class <- CF <- upvsc1 <-
      upvsc2 <- upvsc3 <- upvsc4 <- upvsc5 <- resource.potential.MW <- resource.potential.EJ <-
      fcr <- price <- Pmin <- Pvar <- CFmax <- available <- grade <- extractioncost <-
      maxSubResource <- k1 <- capital.tech.change.5yr <- k2 <- tech.change.5yr <- tech.change <-
      bin <- cost <- grid.cost <- renewresource <- sub.renewable.resource <- year.fillout <-
      minicam.energy.input <- efficiency <- market.name <- flag <- capacity.factor <-
      input.cost <- capital.tech.change.period <- tech.change.period <- time.change <-
      subresource <- Urban_Utility_scale_PV_GWh <- Rural_Utility_scale_PV_GWh <- Urban_Utility_scale_PV <-
      Rural_Utility_scale_PV <- Total_Utility_scale_PV_GWh <- resource <- value <- non_reeds_state <-
      technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # First, process the states not included in the REEDS data, so they can be easily merged into the ReEDS data
    # and associated processing pipeline

    # L2238.non_reeds_states: Create a list of states not in the ReEDS data
    reeds_regions_states %>%
      distinct(State) -> reeds_states

    states_subregions %>%
      select(State=state) %>%
      anti_join(reeds_states, by = "State") %>%
      pull(State) -> L2238.non_reeds_states

    # L2238.non_reeds_states_PV_technical_potential : Total technical potential (urban + rural)
    # for non-ReEDS states from the NREL RE Technical Potential Database
    NREL_us_re_technical_potential %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, Urban_Utility_scale_PV_GWh, Rural_Utility_scale_PV_GWh) %>%
      filter(State %in% L2238.non_reeds_states) %>%
      mutate(Total_Utility_scale_PV_GWh = Urban_Utility_scale_PV_GWh + Rural_Utility_scale_PV_GWh) ->
      L2238.non_reeds_states_PV_technical_potential

    # L2238.non_reeds_states_PV_capacity_factor - combined (weighted for urban / rural potential contribution)
    # capacity factor for non-ReEDS states from the NREL RE Capacity Factor Database
    NREL_us_re_capacity_factors %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, Urban_Utility_scale_PV, Rural_Utility_scale_PV) %>%
      filter(State %in% L2238.non_reeds_states) %>%
      left_join_error_no_match(L2238.non_reeds_states_PV_technical_potential, by = "State") %>%
      mutate(CF = Urban_Utility_scale_PV * (Urban_Utility_scale_PV_GWh / Total_Utility_scale_PV_GWh) +
               Rural_Utility_scale_PV * (Rural_Utility_scale_PV_GWh / Total_Utility_scale_PV_GWh)) %>%
      select(State, CF) -> L2238.non_reeds_states_PV_capacity_factor

    # L2238.PV_potential_EJ_non_reeds_states: NREL data set does not include resource class, which is needed for data processing below
    # Create a Dummy Class for Resource Potential - assume a Class 4 resource, which is the lowest starting class for most other states
    # Also convert GWh to EJ values
    L2238.non_reeds_states_PV_technical_potential %>%
      select(State, Total_Utility_scale_PV_GWh) %>%
      mutate(PV.class = "class4",
             resource.potential.EJ = Total_Utility_scale_PV_GWh * CONV_GWH_EJ) %>%
      select(-Total_Utility_scale_PV_GWh) -> L2238.PV_potential_EJ_non_reeds_states

    # L2238.PV_CF_non_reeds_states: Process data for capacity factor.
    # The class data will be joined from the potential table in order to create a capacity factor table by state and class
    L2238.non_reeds_states_PV_capacity_factor %>%
      left_join_error_no_match(L2238.PV_potential_EJ_non_reeds_states, by = "State") %>%
      select(State, PV.class, CF) -> L2238.PV_CF_non_reeds_states


    # Second, process ReEDS data and combine with data from other states

    # L2238.PV_CF: Capacity factor by state and PV class
    reeds_PV_curve_CF_avg %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, PV.class, CF ) %>%
      group_by(State, PV.class) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() -> L2238.PV_CF

    # Merge capacity factor data from non-ReEDS states
    L2238.PV_CF %>%
      bind_rows(L2238.PV_CF_non_reeds_states) -> L2238.PV_CF


    # L2238.PV_potential_EJ: Resource potential in EJ by state and class
    # We first calculate the resource potential in EJ in each ReEDS region and class using the
    # potential in MW with the average capacity factor for each region and class.
    # We then aggregate this to the state-level.
    reeds_PV_curve_capacity %>%
      mutate(resource.potential.MW = upvsc1 + upvsc2 + upvsc3 + upvsc4 + upvsc5) %>%
      select(BA, PV.class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_PV_curve_CF_avg, by = c("BA", "PV.class")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CF * CONV_MWH_EJ) %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, PV.class, resource.potential.EJ) %>%
      group_by(State, PV.class) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() -> L2238.PV_potential_EJ

    # Merge potential data from non-ReEDS states
    L2238.PV_potential_EJ %>%
      bind_rows(L2238.PV_potential_EJ_non_reeds_states) -> L2238.PV_potential_EJ

    # L2238.PV_matrix: Create a matrix of costs (1975$/GJ) and resource potential (EJ) by state and class
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "PV_peak",
             year == max(MODEL_BASE_YEARS)) %>%
      select(capital.overnight) -> L2238.PV_capital
    L2238.PV_capital <- as.numeric(L2238.PV_capital)

    L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology == "PV",
             year == max(MODEL_BASE_YEARS)) %>%
      select(fixed.charge.rate) -> L2238.fcr
    L2238.fcr <- as.numeric(L2238.fcr)

    L223.GlobalIntTechOMfixed_elec %>%
      filter(intermittent.technology == "PV",
             year == max(MODEL_BASE_YEARS)) %>%
      select(OM.fixed) -> L2238.PV_OMfixed
    L2238.PV_OMfixed <- as.numeric(L2238.PV_OMfixed)

    L2238.PV_potential_EJ %>%
      left_join_error_no_match(L2238.PV_CF, by = c("State","PV.class")) %>%
      mutate(capital.overnight = L2238.PV_capital,
             fcr = L2238.fcr,
             OM.fixed = L2238.PV_OMfixed,
             price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ +
               OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) -> L2238.PV_matrix

    # We noticed that there are some classes with same capacity factor data. For example, in VT, class 3 and 4 have same
    # capacity factors. This is becuase the hourly capacity factor data from ReEDS are also the same for these classes. This
    # could be an error in the data. We get around this simply by making sure that the duplicate points in terms of capacity
    # factors are removed and the resource potentials in those classes are accounted for.
    L2238.PV_matrix %>%
      group_by(State, CF, price) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
      ungroup() %>%
      group_by(State) %>%
      arrange(State, dplyr::desc(CF)) %>%
      mutate(CFmax = max(CF)) %>%
      ungroup() -> L2238.PV_matrix

    # From the matrix of costs and supplies obtained above, we create a graded resource curve for Pvar versus supply.
    # Unlike the graded resource curves for depletable sources, each grade for the renewresource represents
    # the fraction of maximum resource (cumulative) and price. Hence, we first create a matrix of cumulative resource
    # and price (Pvar).
    L2238.PV_matrix %>%
      group_by(State) %>%
      arrange(State, price) %>%
      mutate(Pmin = min(price),
             Pvar = price - Pmin,
             available = round(resource.potential.EJ, energy.DIGITS_MAX_SUB_RESOURCE),
             extractioncost = round(Pvar, energy.DIGITS_COST),
             CFmax = round(CFmax, energy.DIGITS_CAPACITY_FACTOR),
             grade = paste ("grade", row_number(), sep = ' '),
             available = cumsum(available)) %>%
      ungroup() %>%
      select(State, grade, available, extractioncost, CFmax) -> L2238.PV_curve

    # Calculating maxSubResource for the graded renewable resource supply curve
    L2238.PV_curve %>%
      group_by(State) %>%
      arrange(State, extractioncost) %>%
      mutate(maxSubResource = max(available)) %>%
      ungroup() %>%
      distinct(State, maxSubResource) -> L2238.maxSubResource_PV

    # The points on the graded curves need to be read in as fractions of the maxSubResource
    L2238.PV_curve %>%
      left_join_error_no_match(L2238.maxSubResource_PV , by ="State") %>%
      mutate(available = available / maxSubResource) %>%
      # Adjusting the curves so that we have a supply of 0 at a Pvar of 0. The available resource potential is accounted
      # for in the subsequent grade because of the cumulative calculation above.
      mutate(available = if_else(grade == "grade 1", 0, available)) %>%
      # Removing duplicate grades within states.  This only impacts WA grade 5,
      # which has just 0.00012 EJ of resource and thus the same available fraction (1) as grade 4.
      distinct(State, available, .keep_all = TRUE) -> L2238.PV_curve

    # L2238.single_grade_states: Create a list of states with only a single PV resource
    L2238.PV_curve %>%
      filter(grade == "grade 2") %>%
      select(State) -> L2238.multi_grade_states

    L2238.PV_curve %>%
      distinct(State) %>%
      anti_join(L2238.multi_grade_states, by = "State") %>%
      pull(State) -> L2238.single_grade_states

    # L2238.PV_curve_single_grade: Extract the relevant states from the graded curves
    L2238.PV_curve %>%
      filter(State %in% L2238.single_grade_states) -> L2238.PV_curve_single_grade

    # We get the unlimited global solar resource price which will be applied to the one grade states
    # by creating a dummy second grade which will utilize the maxsubresource for the given state
    A10.rsrc_info %>%
      gather_years() %>%
      filter(resource == "global solar resource",
             year == max(year)) %>%
      pull(value) -> A10_solar_cost

    # Make a second grade for all the single grade states by using full the maxsubresource percentage
    # as available value and global solar resource price as extraction cost
    L2238.PV_curve_single_grade %>%
      mutate(grade = "grade 2",
             available = maxSubResource / maxSubResource,
             extractioncost = A10_solar_cost) -> L2238.PV_curve_single_grade

    # Add the second grade to back into the main PV resource curve table
    L2238.PV_curve %>%
      bind_rows(L2238.PV_curve_single_grade) -> L2238.PV_curve

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
    # a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed), where
    # k1 = FCR / (CONV_YEAR_HOURS * kWh_GJ) and k2 = 1 / (CONV_YEAR_HOURS * kWh_GJ)
    # Thus, we calculate model input parameter techChange (which is the reduction per year) as 1 - a'^ (1/5)
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "PV_peak") %>%
      select(year, capital.overnight) %>%
      mutate(capital.tech.change.period = lag(capital.overnight, 1) / capital.overnight,
             time.change = year - lag(year),
             fixed.charge.rate = L2238.fcr,
             OM.fixed = L2238.PV_OMfixed,
             k1 = fixed.charge.rate / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM.fixed) /
               (k1 * capital.overnight + k2 * OM.fixed),
             tech.change = round(abs(1 - (tech.change.period) ^ (1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
      select(year, tech.change) %>%
      filter(!is.na(tech.change),
             year > max(MODEL_BASE_YEARS)) -> L2238.PV_curve_tech_change

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state.
    # Our starting data comprises of grid connection costs in $/MW by ReEDS region and PV class.
    # This data also categorizes the connection cost into five bins in each region and class.
    # We first calculate the minimum cost for a region and class.
    # Using this data, we then obtain grid connection cost in $/GJ for each region and class as
    # FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS * CF * MWh_GJ).
    # Costs are then obtained for a state by taking the minimum.
    # In the future, we might think about a separate state-level curve for grid connection costs.
    reeds_PV_curve_grid_cost %>%
      gather(bin, cost, -BA, -PV.class) %>%
      group_by(BA, PV.class) %>%
      summarise(cost = min(cost[cost>0])) %>%
      ungroup() %>%
      left_join_error_no_match(reeds_PV_curve_CF_avg, by = c("BA", "PV.class")) %>%
      mutate(fcr = L2238.fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ),
             grid.cost = grid.cost * gdp_deflator(1975, 2005))  %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, BA, PV.class, grid.cost) %>%
      group_by(State) %>%
      summarise(grid.cost = min(grid.cost)) %>%
      ungroup() %>%
      mutate(grid.cost = round(grid.cost, energy.DIGITS_COST)) -> L2238.grid_cost

    # L2238.grid_cost_non_reeds_states: Calculate grid costs for non-ReEDS states
    # grid costs based on mapping to states with similar geography or grid costs
    # these assumptions can be changed in gcam-usa/non_reeds_PV_grid_cost
    non_reeds_PV_grid_cost %>%
      left_join_error_no_match(L2238.grid_cost, by = c("comparison_state" = "State")) %>%
      select(State = non_reeds_state, grid.cost) -> L2238.grid_cost_non_reeds_states

    # Bind tables
    L2238.grid_cost %>%
      bind_rows(L2238.grid_cost_non_reeds_states) -> L2238.grid_cost


    # Format tables for output

    # Table to read in renewresource, output.unit, price.unit and market
    L2238.PV_curve %>%
      distinct(State) %>%
      rename(region = State) %>%
      mutate(renewresource = "PV_resource",
             output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2238.RenewRsrc_PV_reeds_USA

    # Table to create the graded resource curves
    L2238.PV_curve %>%
      mutate(renewresource = "PV_resource",
             sub.renewable.resource = "PV_resource",
             available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region = State, renewresource, sub.renewable.resource, grade, available, extractioncost) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2238.GrdRenewRsrcCurves_PV_reeds_USA

    # Table to read in maximum resource
    L2238.maxSubResource_PV %>%
      mutate(renewresource = "PV_resource",
             sub.renewable.resource = "PV_resource",
             year.fillout = min(MODEL_YEARS),
             maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region = State, renewresource, sub.renewable.resource, year.fillout, maxSubResource) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2238.GrdRenewRsrcMax_PV_reeds_USA

    # Table to delete global solar resource minicam-energy-input
    L2234.StubTechMarket_elecS_USA %>%
      filter(grepl("PV", stub.technology)) %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA

    # Table to read in energy inputs at the technology level
    L2234.StubTechMarket_elecS_USA %>%
      filter(grepl("PV", stub.technology)) %>%
      mutate(minicam.energy.input = "PV_resource",
             market.name = region,
             efficiency = 1,
             # Hard code in type "Resource" for intermittent technology resource input only
             flag = "Resource") %>%
      select(region, supplysector, subsector, stub.technology, year,
             minicam.energy.input, efficiency, market.name, flag) -> L2238.StubTechEffFlag_PV_reeds_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      filter(grepl("PV", stub.technology)) %>%
      left_join_error_no_match(L2238.PV_curve %>%
                                 distinct(State, CFmax),
                               by = c("region" = "State")) %>%
      mutate(capacity.factor = round(CFmax, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year,
             capacity.factor) -> L2238.StubTechCapFactor_PV_reeds_USA

    # Copying tech change to all states and filtering out only the contiguous states
    L2238.RenewRsrcTechChange_PV_reeds_USA <- write_to_all_states(L2238.PV_curve_tech_change, c("region", "year","tech.change"))
    L2238.RenewRsrcTechChange_PV_reeds_USA %>%
      mutate(renewresource = "PV_resource",
             sub.renewable.resource = "PV_resource") %>%
      select(region,renewresource, sub.renewable.resource, year.fillout = year,
             techChange = tech.change) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2238.RenewRsrcTechChange_PV_reeds_USA

    # Reading the grid connection cost as a state-level non-energy cost adder
    L2234.StubTechCapFactor_elecS_solar_USA %>%
      filter(grepl("PV", stub.technology )) %>%
      select(region, supplysector, subsector, stub.technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L2238.grid_cost, by = c("region" = "State")) %>%
      rename(input.cost = grid.cost) %>%
      filter(!is.na(input.cost)) -> L2238.StubTechCost_PV_reeds_USA

	# Establishing shareweights
    L2238.GrdRenewRsrcMax_PV_reeds_USA %>%
      select(region, resource = renewresource, subresource = sub.renewable.resource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "resource" = "resource_elec_subsector")) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L2238.ResTechShrwt_PV_reeds_USA


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
    L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA <- add_cooling_techs(L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA)
    L2238.StubTechEffFlag_PV_reeds_USA <- add_cooling_techs(L2238.StubTechEffFlag_PV_reeds_USA)
    L2238.StubTechCapFactor_PV_reeds_USA <- add_cooling_techs(L2238.StubTechCapFactor_PV_reeds_USA)
    L2238.StubTechCost_PV_reeds_USA <- add_cooling_techs(L2238.StubTechCost_PV_reeds_USA)


    # ===================================================
    # Produce outputs

    L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA %>%
      add_title("Delete global solar resource Energy Input for PV Technologies") %>%
      add_units("NA") %>%
      add_comments("global solar resource input deleted; will be replaced by PV_resource") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L2238.DeleteStubTechMinicamEnergyInput_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'energy/A10.rsrc_info',
                     'L2234.StubTechMarket_elecS_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA

    L2238.RenewRsrc_PV_reeds_USA %>%
      add_title("Market Information for Solar PV Resources") %>%
      add_units("NA") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L2238.RenewRsrc_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'gcam-usa/A10.renewable_resource_delete',
                     'energy/A10.rsrc_info',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2238.RenewRsrc_PV_reeds_USA

    L2238.GrdRenewRsrcCurves_PV_reeds_USA %>%
      add_title("Graded Supply Curves of Solar PV Resources at the State-Level") %>%
      add_units("available: fraction of maxSubResource; extractioncost: $1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.GrdRenewRsrcCurves_PV_USA_reeds") %>%
      same_precursors_as("L2238.RenewRsrc_PV_reeds_USA") ->
      L2238.GrdRenewRsrcCurves_PV_reeds_USA

    L2238.GrdRenewRsrcMax_PV_reeds_USA %>%
      add_title("Maximum Subresource Availability for Solar PV Resources at the State-Level") %>%
      add_units("EJ") %>%
      add_comments("Each grade represents the (cumulative) fraction of maxSubResource available at a given price") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.GrdRenewRsrcMax_PV_USA_reeds") %>%
      same_precursors_as("L2238.RenewRsrc_PV_reeds_USA") ->
      L2238.GrdRenewRsrcMax_PV_reeds_USA

    L2238.StubTechEffFlag_PV_reeds_USA %>%
      add_title("Market Information for Solar PV Technologies") %>%
      add_units("unitless") %>%
      add_comments("Only applies to 45 states in ReEDS PV data set") %>%
      add_legacy_name("L2238.StubTechEffFlag_PV_USA_reeds") %>%
      same_precursors_as("L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA") ->
      L2238.StubTechEffFlag_PV_reeds_USA

    L2238.StubTechCapFactor_PV_reeds_USA %>%
      add_title("State-specific Capacity Factors for Solar PV Technologies") %>%
      add_units("unitless") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.StubTechCapFactor_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'L2234.StubTechCapFactor_elecS_solar_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2238.StubTechCapFactor_PV_reeds_USA

    L2238.RenewRsrcTechChange_PV_reeds_USA %>%
      add_title("Technological Change Parameter for Solar PV Resources") %>%
      add_units("unitless") %>%
      add_comments("Technological change in the supply curve is related to assumed improvements in capital cost") %>%
      add_legacy_name("L2238.RenewRsrcTechChange_PV_USA_reeds") %>%
      same_precursors_as("L2238.RenewRsrc_PV_reeds_USA") ->
      L2238.RenewRsrcTechChange_PV_reeds_USA

    L2238.StubTechCost_PV_reeds_USA %>%
      add_title("State-specific Grid Connection Cost Adders for Solar PV Technologies") %>%
      add_units("$1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.StubTechCost_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'gcam-usa/reeds_PV_curve_grid_cost',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'gcam-usa/non_reeds_PV_grid_cost',
                     'L2234.StubTechCapFactor_elecS_solar_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2238.StubTechCost_PV_reeds_USA

    L2238.ResTechShrwt_PV_reeds_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_attributes_as(L2238.GrdRenewRsrcMax_PV_reeds_USA) ->
      L2238.ResTechShrwt_PV_reeds_USA

    return_data(L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA,
                L2238.RenewRsrc_PV_reeds_USA,
                L2238.GrdRenewRsrcCurves_PV_reeds_USA,
                L2238.GrdRenewRsrcMax_PV_reeds_USA,
                L2238.StubTechEffFlag_PV_reeds_USA,
                L2238.StubTechCapFactor_PV_reeds_USA,
                L2238.RenewRsrcTechChange_PV_reeds_USA,
                L2238.StubTechCost_PV_reeds_USA,
                L2238.ResTechShrwt_PV_reeds_USA)

  } else {
    stop("Unknown command")
  }
}
