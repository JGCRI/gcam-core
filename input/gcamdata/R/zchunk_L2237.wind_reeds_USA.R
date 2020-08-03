# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2237.wind_reeds_USA
#'
#' Create updated wind resource supply curves consistent with ReEDS.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2237.SmthRenewRsrcCurves_wind_reeds_USA}, \code{L2237.StubTechCapFactor_wind_reeds_USA},
#' \code{L2237.SmthRenewRsrcTechChange_wind_reeds_USA}, \code{L2237.StubTechCost_wind_reeds_USA},
#' \code{L2237.ResTechShrwt_wind_reeds_USA}.
#' The corresponding file in the original data system was \code{L2237.wind_reeds_USA.R} (gcam-usa level2).
#' @details Create state-level wind resource supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select row_number semi_join summarise_if group_by bind_rows
#' @importFrom tidyr gather
#' @author MTB September 2018
module_gcamusa_L2237.wind_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/reeds_regions_states',
             FILE = 'gcam-usa/reeds_wind_curve_capacity',
             FILE = 'gcam-usa/reeds_wind_curve_CF_avg',
             FILE = 'gcam-usa/reeds_wind_curve_grid_cost',
             FILE = 'gcam-usa/A23.elecS_tech_mapping_cool',
             FILE = "gcam-usa/A10.renewable_resource_delete",
             'L2234.StubTechCapFactor_elecS_wind_USA',
             'L2234.GlobalIntTechCapital_elecS_USA',
             'L223.GlobalIntTechCapital_elec',
             'L223.GlobalIntTechOMfixed_elec'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2237.SmthRenewRsrcCurves_wind_reeds_USA',
             'L2237.StubTechCapFactor_wind_reeds_USA',
             'L2237.SmthRenewRsrcTechChange_wind_reeds_USA',
             'L2237.StubTechCost_wind_reeds_USA',
             'L2237.ResTechShrwt_wind_reeds_USA'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    reeds_regions_states <- get_data(all_data, 'gcam-usa/reeds_regions_states')
    reeds_wind_curve_capacity <- get_data(all_data, 'gcam-usa/reeds_wind_curve_capacity')
    reeds_wind_curve_CF_avg <- get_data(all_data, 'gcam-usa/reeds_wind_curve_CF_avg')
    reeds_wind_curve_grid_cost <- get_data(all_data, 'gcam-usa/reeds_wind_curve_grid_cost')
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete")
    L2234.StubTechCapFactor_elecS_wind_USA <- get_data(all_data, 'L2234.StubTechCapFactor_elecS_wind_USA', strip_attributes = TRUE)
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, 'L2234.GlobalIntTechCapital_elecS_USA', strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, 'L223.GlobalIntTechCapital_elec')
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, 'L223.GlobalIntTechOMfixed_elec')

    # Silence package checks
    region <- state <- states_list <- sector.name <- subsector.name <- intermittent.technology <-
      supplysector <- subsector <- stub.technology <- year <- input.capital <- capital.overnight <-
      fixed.charge.rate <- input.OM.fixed <- OM.fixed <- State <- TRG <- CF <- Wind.Class <- wsc1 <-
      wsc2 <- wsc3 <- wsc4 <- wsc5 <- Wind.Resource.Region <- resource.potential.MW <- resource.potential.EJ <-
      fcr <- price <- supply <- CFmax <- base.price <- maxSubResource <- percent.supply <- Pvar <- P2 <- P1 <- Q2 <-
      Q1 <- mid.price <- optimize <- curve.exponent <- k1 <- capital.tech.change.5yr <- k2 <- tech.change.5yr <-
      tech.change <- Wind.Type <- bin <- cost <- grid.cost <- Region <- renewresource <-
      smooth.renewable.subresource <- year.fillout <- capacity.factor <- input.cost <-
      capital.tech.change.period <- tech.change.period <- time.change <-
      subresource <- technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # L2237.wind_CF: Capacity factor by state and wind class
    reeds_wind_curve_CF_avg %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind.Resource.Region" = "Region")) %>%
      select(State, Wind.Class = TRG, CF ) %>%
      group_by(State, Wind.Class) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() -> L2237.wind_CF

    # L2237.wind_potential_EJ: Resource potential in EJ by state and class
    # We first calculate the resource potential in EJ in each ReEDS region and class using the
    # potential in MW with the average capacity factor for each region and class.
    # We then aggregate this to the state-level.
    reeds_wind_curve_capacity %>%
      mutate(resource.potential.MW = wsc1 + wsc2 + wsc3 + wsc4 + wsc5) %>%
      select(Wind.Resource.Region, Wind.Class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_wind_curve_CF_avg, by = c("Wind.Resource.Region" , "Wind.Class" = "TRG")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CF * CONV_MWH_EJ) %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind.Resource.Region" = "Region")) %>%
      select(State, Wind.Class, resource.potential.EJ) %>%
      group_by(State, Wind.Class) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() -> L2237.wind_potential_EJ

    # L2237.wind_matrix: Creating a matrix of costs (1975$/GJ) and cumulative resource potential (EJ) by state and class
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "wind_base",
             year == max(MODEL_BASE_YEARS)) %>%
      select(capital.overnight) -> L2237.wind_capital
    L2237.wind_capital <- as.numeric(L2237.wind_capital)

    L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology == "wind",
             year == max(MODEL_BASE_YEARS)) %>%
      select(fixed.charge.rate) -> L2237.fcr
    L2237.fcr <- as.numeric(L2237.fcr)

    L223.GlobalIntTechOMfixed_elec %>%
      filter(intermittent.technology == "wind",
             year == max(MODEL_BASE_YEARS)) %>%
      select(OM.fixed) -> L2237.wind_OMfixed
    L2237.wind_OMfixed <- as.numeric(L2237.wind_OMfixed)

    L2237.wind_potential_EJ %>%
      left_join_error_no_match(L2237.wind_CF, by = c("State", "Wind.Class")) %>%
      group_by(State) %>%
      arrange(State, dplyr::desc(CF)) %>%
      mutate(CFmax = max(CF),
             supply = cumsum(resource.potential.EJ)) %>%
      ungroup() %>%
      mutate(capital.overnight = L2237.wind_capital,
             fcr = L2237.fcr,
             OM.fixed = L2237.wind_OMfixed,
             price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ + OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      select(State, price, supply, CFmax) -> L2237.wind_matrix

    # L2237.wind_curve: estimating parameters of the smooth curve
    # Calculate maxSubResource, base.price, and Pvar.
    # base.price represents the minimum cost of generating electricity from the resource.
    # base.price comprises of the cost of generating power at the most optimal location.
    # Pvar represents costs that are expected to increase from base.price as deployment increases.
    # This models the increase in costs as more optimal locations are used first.

    L2237.wind_matrix %>%
      group_by(State) %>%
      arrange(State, price) %>%
      mutate(base.price = min(price),
             Pvar = price - base.price,
             maxSubResource = round(max(supply), energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      ungroup() -> L2237.wind_curve

    # Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
    # Using these points, the mid-price can be estimated as:
    # mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
    # This assumes that the curve is largely linear between the two points above.

    # Calculating P1 and Q1
    L2237.wind_curve %>%
      mutate(percent.supply = supply / maxSubResource) %>%
      group_by(State) %>%
      arrange(State, dplyr::desc(price)) %>%
      # filter for the supply point with just less than 50% of the maxSubResource, in order to calculate the mid-price
      filter(percent.supply <= 0.5) %>%
      # NOTE: separate filter calls are needed here; combining filters in same call requires both
      # conditions to be met simultaneously (rather than sequentially), which returns an empty data set
      filter(row_number() == 1) %>%
      select(State, P1 = Pvar, Q1 = supply, maxSubResource) %>%
      ungroup() -> L2237.mid.price_1

    # Calculating P2 and Q2
    L2237.wind_curve %>%
      mutate(percent.supply = supply / maxSubResource) %>%
      group_by(State) %>%
      # filter for the supply point with just over 50% of the maxSubResource, in order to calculate the mid-price
      filter(percent.supply >= 0.5) %>%
      # NOTE: separate filter calls are needed here; combining filters in same call requires both
      # conditions to be met simultaneously (rather than sequentially), which returns an empty data set
      filter(row_number() == 1) %>%
      select(State, P2 = Pvar, Q2 = supply) %>%
      ungroup() -> L2237.mid.price_2

    # Calculating mid.price
    L2237.mid.price_1 %>%
      left_join_error_no_match(L2237.mid.price_2, by = "State") %>%
      mutate(mid.price = round(((P2 - P1) * maxSubResource + 2 * Q2 * P1 - 2 * Q1 * P2) / (2 * (Q2 - Q1)), energy.DIGITS_MID_PRICE)) %>%
      select(State, mid.price) -> L2237.mid.price

    L2237.wind_curve %>%
      left_join_error_no_match(L2237.mid.price, by = c("State")) -> L2237.wind_curve

    # Defining variables to be used later. Note that the ReEDS data includes information for the 48 contiguous states only.
    # For now, since this script creates add-on files, we'll assume that the existing curves in the remaining states are good enough.
    states_list <- unique(L2237.wind_curve$State)
    L2237.curve.exponent <- tibble()

    for (L2237.state in states_list) {

      L2237.wind_curve %>%
        filter(State == L2237.state) -> L2237.wind_curve_state

      L2237.wind_curve_state %>%
        select(price, supply) -> L2237.supply_points_state

      L2237.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval = c(1.0, 15.0),
                                            L2237.wind_curve_state$mid.price,
                                            L2237.wind_curve_state$base.price,
                                            L2237.wind_curve_state$maxSubResource,
                                            L2237.supply_points_state)

      L2237.wind_curve_state$curve.exponent <-  round(L2237.error_min_curve.exp$minimum, energy.DIGITS_CURVE_EXPONENT)
      L2237.wind_curve_state %>%
        select(State, curve.exponent) %>%
        filter(row_number() == 1) -> L2237.curve.exponent_state

      L2237.curve.exponent %>%
        bind_rows(L2237.curve.exponent_state) -> L2237.curve.exponent

    }

    L2237.wind_curve %>%
      left_join_error_no_match(L2237.curve.exponent, by = "State") -> L2237.wind_curve

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
    # a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR / (CONV_YEAR_HOURS * kWh_GJ) and k2 = 1 / (CONV_YEAR_HOURS * kWh_GJ)
    # Thus, we calculate model input parameter techChange (which is the reduction per year) as 1 - a'^(1/5)

    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(intermittent.technology == "wind_base") %>%
      select(year, capital.overnight) %>%
      mutate(capital.tech.change.period = lag(capital.overnight, 1) / capital.overnight,
             time.change = year - lag(year),
             fixed.charge.rate = L2237.fcr,
             OM.fixed = L2237.wind_OMfixed,
             k1 = fixed.charge.rate / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM.fixed) / (k1 * capital.overnight + k2 * OM.fixed),
             tech.change = round(abs(1 - (tech.change.period) ^ ( 1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
      select(year, tech.change) %>%
      filter(!is.na(tech.change),
             year > max(MODEL_BASE_YEARS)) -> L2237.wind_curve_tech_change

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state.
    # Our starting data consists of grid connection costs in $/MW by ReEDS region and wind class.
    # This data also categorizes the connection cost into five bins in each region and class.
    # Using this data, we obtain a grid connection cost in $/GJ for each region/ class/ bin data
    # point as FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS * CF * MWh_GJ).
    # Costs are then obtained for a state by averaging.
    # In the future, we might think about a separate state-level curve for grid connection costs.
    reeds_wind_curve_grid_cost %>%
      select(-Wind.Type) %>%
      gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
      filter(cost != 0) %>%
      left_join_error_no_match(reeds_wind_curve_CF_avg, by = c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
      mutate(fcr = L2237.fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ),
             grid.cost = grid.cost * gdp_deflator(1975, 2013)) %>%
      left_join_error_no_match(reeds_regions_states %>%
                  select(Region, State),
                by = c("Wind.Resource.Region" = "Region")) %>%
      group_by(State) %>%
      summarise(grid.cost = round(min(grid.cost), energy.DIGITS_COST)) %>%
      ungroup() -> L2237.grid.cost

    # Formatting tables for output
    L2237.wind_curve %>%
      distinct(State, maxSubResource, mid.price, curve.exponent) %>%
      mutate(renewresource = "onshore wind resource",
             smooth.renewable.subresource = "onshore wind resource",
             year.fillout = min(MODEL_YEARS)) %>%
      select(region = State, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource, mid.price, curve.exponent) %>%
      # Wind power is assumed to be infeasible in DC. Thus, it should not be assigned "onshore wind resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2237.SmthRenewRsrcCurves_wind_reeds_USA

    L2234.StubTechCapFactor_elecS_wind_USA %>%
      # using semi_join to filter out states not included in the ReEDS data set,
      # for which wind resource curves are not being updated
      semi_join(L2237.wind_curve, by = c("region" = "State")) %>%
      left_join_error_no_match(L2237.wind_curve %>%
                  distinct(State, CFmax),
                by = c("region" = "State")) %>%
      filter(!is.na(CFmax)) %>%
      mutate(capacity.factor = round(CFmax, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year,
             capacity.factor) -> L2237.StubTechCapFactor_wind_reeds_USA

    # Copying tech change to all states and filtering out only the contiguous states
    L2237.SmthRenewRsrcTechChange_wind_reeds_USA <- write_to_all_states(L2237.wind_curve_tech_change, c("region", "year", "tech.change"))
    L2237.SmthRenewRsrcTechChange_wind_reeds_USA %>%
      filter(region %in% states_list) %>%
      mutate(renewresource = "onshore wind resource",
             smooth.renewable.subresource = "onshore wind resource") %>%
      select(region, renewresource, smooth.renewable.subresource, year.fillout = year,
             techChange = tech.change) %>%
      # Wind power is assumed to be infeasible in DC. Thus, it should not be assigned "onshore wind resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "renewresource" = "resource_elec_subsector")) ->
      L2237.SmthRenewRsrcTechChange_wind_reeds_USA

    # Reading the grid connection cost as a state-level non-energy cost adder
    L2234.StubTechCapFactor_elecS_wind_USA %>%
      select(region, supplysector, subsector, stub.technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      # using semi_join to filter out states not included in the ReEDS data set,
      # for which wind resource curves are not being updated
      semi_join(L2237.grid.cost, by = c("region" = "State")) %>%
      left_join_error_no_match(L2237.grid.cost, by = c("region" = "State")) %>%
      rename(input.cost = grid.cost) %>%
      filter(!is.na(input.cost)) -> L2237.StubTechCost_wind_reeds_USA

    L2237.SmthRenewRsrcCurves_wind_reeds_USA %>%
      select(region, resource = renewresource, subresource = smooth.renewable.subresource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L2237.ResTechShrwt_wind_reeds_USA

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
        mutate(technology = if_else(subsector=="wind_base",subsector,technology)) %>%
        arrange(region,year)
      return(data_new)
    }

      L2237.StubTechCapFactor_wind_reeds_USA <- add_cooling_techs(L2237.StubTechCapFactor_wind_reeds_USA)
      L2237.StubTechCost_wind_reeds_USA <- add_cooling_techs(L2237.StubTechCost_wind_reeds_USA)



    # ===================================================
    # Produce outputs

    L2237.SmthRenewRsrcCurves_wind_reeds_USA %>%
      add_title("Wind Resource Supply Curve Parameters") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2237.SmthRenewRsrcCurves_wind_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/reeds_wind_curve_capacity',
                     'gcam-usa/reeds_wind_curve_CF_avg',
                     'gcam-usa/A10.renewable_resource_delete',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2237.SmthRenewRsrcCurves_wind_reeds_USA

    L2237.StubTechCapFactor_wind_reeds_USA %>%
      add_title("State-specific Capacity Factors for Wind Power Technologies") %>%
      add_units("unitless") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2237.StubTechCapFactor_wind_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/reeds_wind_curve_capacity',
                     'gcam-usa/reeds_wind_curve_CF_avg',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'L2234.StubTechCapFactor_elecS_wind_USA',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2237.StubTechCapFactor_wind_reeds_USA

    L2237.SmthRenewRsrcTechChange_wind_reeds_USA %>%
      add_title("Technological Change Parameter for Wind Resource") %>%
      add_units("unitless") %>%
      add_comments("Technological change in the supply curve is related to assumed improvements in capital cost") %>%
      add_legacy_name("L2237.SmthRenewRsrcTechChange_wind_USA_reeds") %>%
      add_precursors('gcam-usa/A10.renewable_resource_delete',
                     'L2234.GlobalIntTechCapital_elecS_USA',
                     'L223.GlobalIntTechCapital_elec',
                     'L223.GlobalIntTechOMfixed_elec') ->
      L2237.SmthRenewRsrcTechChange_wind_reeds_USA

    L2237.StubTechCost_wind_reeds_USA %>%
      add_title("State-specific Grid Connection Cost Adders for Wind Power Technologies") %>%
      add_units("$1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2237.StubTechCost_wind_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/reeds_wind_curve_CF_avg',
                     'gcam-usa/reeds_wind_curve_grid_cost',
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     'L2234.StubTechCapFactor_elecS_wind_USA',
                     'L223.GlobalIntTechCapital_elec') ->
      L2237.StubTechCost_wind_reeds_USA

    L2237.ResTechShrwt_wind_reeds_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_attributes_as(L2237.SmthRenewRsrcCurves_wind_reeds_USA) ->
      L2237.ResTechShrwt_wind_reeds_USA


    return_data(L2237.SmthRenewRsrcCurves_wind_reeds_USA,
                L2237.StubTechCapFactor_wind_reeds_USA,
                L2237.SmthRenewRsrcTechChange_wind_reeds_USA,
                L2237.StubTechCost_wind_reeds_USA,
                L2237.ResTechShrwt_wind_reeds_USA)

  } else {
    stop("Unknown command")
  }
}
