# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA120.offshore_wind_reeds_USA
#'
#' Takes in data on country-level offshore wind energy potential and global offshore wind capital cost assumptions and generates tables containing region-level offshore wind data.
#'
#' @param command API command to execute
#' @return Depends on \code{command}: either a vector of required inputs,
#' @param ... other optional parameters, depending on command
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.RsrcCurves_EJ_R_offshore_wind_USA}, \code{L120.GridCost_offshore_wind_USA}, \code{L120.RegCapFactor_offshore_wind_USA}.
#' The corresponding file in the
#' original data system was \code{LA120.offshore_wind_reeds_USA.R} (gcam_usa level1).
#' @details Takes in data on US-level offshore wind energy potential and global offshore wind capital cost assumptions and generates tables containing state-level offshore wind data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by summarise distinct arrange bind_rows
#' @importFrom tidyr gather
#' @importFrom stats optimize
#' @author MB GI AJS March 2019
module_gcamusa_LA120.offshore_wind_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/reeds_regions_states",
             FILE = "gcam-usa/A20.offshore_wind_class_depth",
             FILE = "energy/A20.offshore_wind_depth_cap_cost",
             FILE = "gcam-usa/reeds_offshore_wind_curve_capacity",
             FILE = "gcam-usa/reeds_offshore_wind_curve_grid_cost",
             FILE = "gcam-usa/reeds_offshore_wind_curve_CF_avg",
             FILE = "gcam-usa/offshore_wind_potential_missing",
             "L113.globaltech_capital_ATB",
             "L113.globaltech_OMfixed_ATB"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.RsrcCurves_EJ_R_offshore_wind_USA",
             "L120.GridCost_offshore_wind_USA",
             "L120.RegCapFactor_offshore_wind_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    State <- resource.potential.MW <- depth_class <- distance_to_shore <-
      resource.potential.EJ <- fixed.charge.rate <- technology <- socioeconomics.FINAL_HIST_YEAR <- fcr <- OM.fixed <- price <-
      CFmax <- supply <- curve.exponent <- mid.price <- base.price <- maxSubResource <- supply_points <- Pvar <- P1 <- Q1 <- sector.name <-
      supplysector <- subsector.name <- subsector <- intermittent.technology <- year <- input.capital <- capital.overnight <-
      capacity.factor <- capital.overnight.lag <- capital.tech.change.5yr <- kl  <- tech.change.5yr <- tech.change <- bin <- cost <-
      Wind_Resource_Region <- Wind_Class <- grid.cost <- TRG <- wsc1 <- wsc2 <- wsc3 <- wsc4 <- wsc5 <- renewresource <-
      smooth.renewable.subresource <- Wind_Type <- CF <- value <- percent.supply <- P2 <- Q2 <- Region <- rsrc_frac <- state <- NULL  # silence package check notes

    # Load required inputs
    reeds_regions_states <- get_data(all_data, "gcam-usa/reeds_regions_states")
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    A20.offshore_wind_class_depth <- get_data(all_data, "gcam-usa/A20.offshore_wind_class_depth")
    A20.offshore_wind_depth_cap_cost <- get_data(all_data, "energy/A20.offshore_wind_depth_cap_cost")
    reeds_offshore_wind_curve_capacity  <- get_data(all_data, "gcam-usa/reeds_offshore_wind_curve_capacity")
    reeds_offshore_wind_curve_grid_cost <- get_data(all_data, "gcam-usa/reeds_offshore_wind_curve_grid_cost")
    reeds_offshore_wind_curve_CF_avg <- get_data(all_data, "gcam-usa/reeds_offshore_wind_curve_CF_avg")
    offshore_wind_potential_missing  <- get_data(all_data, "gcam-usa/offshore_wind_potential_missing")

    # -----------------------------------------------------------------------------
    # Perform computations
    # Assigning average capacity factor by ReEDS region and class to a variable which will then be used to
    # come up with an average capacity factor by state and class.
    reeds_offshore_wind_curve_CF_avg %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind_Resource_Region" = "Region")) %>%
      select(State, Wind_Class = TRG, CF) %>%
      group_by(State, Wind_Class) %>%
      summarise(CF = mean(CF)) %>%
      ungroup() -> L120.offshore_wind_CF

    # We first calculate the resource potential in EJ in each ReEDS region and class using the
    # potential in MW with the average capacity factor for each region and class.
    # We then aggregate this to the state-level.
    reeds_offshore_wind_curve_capacity %>%
      mutate (resource.potential.MW = wsc1 + wsc2 + wsc3 + wsc4 + wsc5) %>%
      select(Wind_Resource_Region, Wind_Class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_offshore_wind_curve_CF_avg, by = c("Wind_Resource_Region", "Wind_Class" = "TRG")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CF * CONV_MWH_EJ) %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind_Resource_Region" = "Region")) %>%
      select(State, Wind_Class, resource.potential.EJ) %>%
      group_by(State, Wind_Class) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
      ungroup() -> L120.offshore_wind_potential_EJ

    A20.offshore_wind_class_depth %>%
      left_join_error_no_match(A20.offshore_wind_depth_cap_cost, by = c("depth_class")) %>%
      select(Wind_Class, capital.overnight) -> L2231.offshore_wind_capital

    L113.globaltech_capital_ATB %>%
      filter(technology == "wind_offshore") %>%
      select(fixed.charge.rate) -> L120.offshore_wind_fcr
    L120.offshore_wind_fcr <- as.numeric(L120.offshore_wind_fcr)

    L113.globaltech_OMfixed_ATB %>%
      gather_years() %>%
      filter(technology == "wind_offshore",
             year == max(HISTORICAL_YEARS)) %>%
      distinct(value) -> L120.offshore_wind_OMfixed
    L120.offshore_wind_OMfixed <- as.numeric(L120.offshore_wind_OMfixed)

    # NOTE that the process for calculating supply/ price is different for offshore wind (vs. onshore wind).  For offshore wind, we
    # (1) calculate the price associated with each wind class, then (2) arrange the dataset by region/ price and calculate
    # cumulative resource supply. (Conversely, for onshore wind, we (1) arrange the data by capacity factor and calculate supply,
    # then (2) calculate the corresponding prices.)  The reason for this difference is that, for offshore wind, price varies with
    # both capacity factor AND ocean depth.  (For onshore wind, CF is the only major determinant of price.)  Thus, arranging by CF
    # and calculating cumulative supply would result in an illogical offshore wind supply curve where price fluctuates up and down
    # as supply increases.
    L120.offshore_wind_potential_EJ %>%
      left_join_error_no_match(L120.offshore_wind_CF, by = c("State", "Wind_Class")) %>%
      left_join_error_no_match(L2231.offshore_wind_capital, by = "Wind_Class") %>%
      mutate(fcr = L120.offshore_wind_fcr,
             OM.fixed = L120.offshore_wind_OMfixed) %>%
      mutate(price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ + OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      group_by(State) %>%
      mutate(CFmax = max(CF)) %>%
      arrange(State, price) %>%
      mutate(supply = cumsum(resource.potential.EJ)) %>%
      ungroup() %>%
      select(State, price, supply, CFmax) -> L120.offshore_wind_matrix

    # Assigning resource to states missing from the ReEDS dataset (currently only Alaska).
    # Alaska is assigned 5% of each supply point (this assumption is defined in offshore_wind_potential_missing).
    # Note that this approach assumes that a missing state's resource is a representative sample of the total USA resource.
    L120.CFmax.average <- L120.offshore_wind_matrix %>%
      summarise(CFmax = mean(CFmax))
    L120.CFmax.average <- as.numeric(L120.CFmax.average)

    L120.offshore_wind_potential_EJ %>%
      left_join_error_no_match(L120.offshore_wind_CF, by = c("State", "Wind_Class")) %>%
      left_join_error_no_match(L2231.offshore_wind_capital, by = c("Wind_Class")) %>%
      mutate(fcr = L120.offshore_wind_fcr,
             OM.fixed = L120.offshore_wind_OMfixed) %>%
      mutate(price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ + OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      select(-State) %>%
      arrange(price) %>%
      repeat_add_columns(offshore_wind_potential_missing) %>%
      mutate(resource.potential.EJ = resource.potential.EJ * rsrc_frac,
             supply = cumsum(resource.potential.EJ),
             CFmax = L120.CFmax.average) %>%
      select(State = state, price, supply, CFmax) -> L120.offshore_wind_matrix_missing

    L120.offshore_wind_matrix %>%
      bind_rows(L120.offshore_wind_matrix_missing) -> L120.offshore_wind_matrix

    # Calculate maxSubResource, base.price, and Pvar.
    # base.price represents the minimum cost of generating electricity from the resource.
    # base.price comprises of the cost of generating power at the most optimal location.
    # Pvar represents costs that are expected to increase from base.price as deployment increases.
    # This models the increase in costs as more optimal locations are used first.

    L120.offshore_wind_matrix %>%
      group_by(State) %>%
      arrange(State, price) %>%
      mutate(base.price = min(price),
             Pvar = price - base.price,
             maxSubResource = round(max(supply), energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      ungroup() -> L120.offshore_wind_curve

    # Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
    # Using these points, the mid-price can be estimated as:
    # mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
    # This assumes that the curve is largely linear between the two points above.

    # Calculating P1 and Q1
    L120.offshore_wind_curve %>%
      mutate(percent.supply = supply / maxSubResource) %>%
      group_by(State) %>%
      filter(percent.supply <= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for highest price point below 50% of total resource
      filter(Pvar == max(Pvar)) %>%
      ungroup() %>%
      select(State, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1

    # Calculating P2 and Q2
    L120.offshore_wind_curve %>%
      mutate(percent.supply = supply/maxSubResource) %>%
      group_by(State) %>%
      filter(percent.supply >= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for lowest price point above 50% of total resource
      filter(Pvar == min(Pvar)) %>%
      ungroup() %>%
      select(State, P2 = Pvar, Q2 = supply) -> L120.mid.price_2

    # Calculating mid.price
    L120.mid.price_1 %>%
      left_join_error_no_match(L120.mid.price_2, by = "State") %>%
      mutate(mid.price = round(((P2 - P1) * maxSubResource + (2 * Q2 * P1) - (2 * Q1 * P2)) / (2 * (Q2 - Q1)),
                               energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(State, mid.price) -> L120.mid.price

    # NOTE:  Five states (AL, DE, IN, MS, NH) are dropped from L120.mid.price_1 because their lowest supply point contains
    # greater than 50% of the state's supply.  (AL, MS, & NH have only one supply point.)
    L120.offshore_wind_matrix %>%
      anti_join(L120.mid.price, by = c("State")) -> L120.dropped.states

    # To prevent states with offshore wind resource from being dropped:
    # 1) add P,Q point of 99% of base.price, 1% of supply at base.price and recalculate L120.offshore_wind_curve parameters
    L120.dropped.states %>%
      bind_rows(L120.dropped.states %>%
                  group_by(State) %>%
                  # select the lowest price point for each state
                  filter(price == min(price)) %>%
                  ungroup() %>%
                  # add a data point with price = 99% of base.price & supply = 1% of supply at base.price
                  # so that we can calculate a mid.price for states where the first point on the
                  # resource curve contains > 50% of the total resource
                  mutate(price = price * 0.99,
                         supply = supply * 0.01)) %>%
      group_by(State) %>%
      arrange(State, price) %>%
      mutate(base.price = min(price),
             Pvar = price - base.price,
             maxSubResource = round(max(supply), energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      ungroup() -> L120.offshore_wind_curve_adj

    # 2) calculate mid.price as above
    L120.offshore_wind_curve_adj %>%
      mutate(percent.supply = supply/maxSubResource) %>%
      group_by(State) %>%
      filter(percent.supply <= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for highest price point below 50% of total resource
      filter(Pvar == max(Pvar)) %>%
      ungroup() %>%
      select(State, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1_adj

    L120.offshore_wind_curve_adj %>%
      mutate(percent.supply = supply/maxSubResource) %>%
      group_by(State) %>%
      filter(percent.supply >= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for lowest price point above 50% of total resource
      filter(Pvar == min(Pvar)) %>%
      ungroup() %>%
      select(State, P2 = Pvar, Q2 = supply) -> L120.mid.price_2_adj

    L120.mid.price_1_adj %>%
      left_join_error_no_match(L120.mid.price_2_adj, by = "State") %>%
      mutate(mid.price = round(((P2 - P1) * maxSubResource + (2 * Q2 * P1) - (2 * Q1 * P2)) / (2 * (Q2 - Q1)),
                               energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(State, mid.price) -> L120.mid.price_adj

    # Remove dropped states from L120.offshore_wind_curve, and bind in revised data points
    L120.offshore_wind_curve %>%
      anti_join(L120.offshore_wind_curve_adj, by = c("State")) %>%
      bind_rows(L120.offshore_wind_curve_adj) -> L120.offshore_wind_curve

    # Bind in mid.price data for dropped states
    L120.mid.price %>%
      bind_rows(L120.mid.price_adj) -> L120.mid.price

    # Add mid.price to offshore_wind_curve
    L120.offshore_wind_curve %>%
      left_join_error_no_match(L120.mid.price, by = c("State")) %>%
      arrange(State) -> L120.offshore_wind_curve

    # Dropping regions with maxSubResource < .001 (i.e. NH, 0.00048 EJ), to avoid potential solution errors
    L120.offshore_wind_curve %>%
      filter(maxSubResource > energy.WIND_MIN_POTENTIAL) -> L120.offshore_wind_curve

    # Defining variables to be used later. Note that the ReEDS data includes information for the 48 contiguous states only.
    # For now, since this script creates add-on files, we'll assume that the existing curves in the remaining states are good enough.
    states_list <- unique(L120.offshore_wind_curve$State)
    L120.curve.exponent <- tibble()

    for (L120.state in states_list) {

      L120.offshore_wind_curve %>%
        filter(State == L120.state) -> L120.offshore_wind_curve_state

      L120.offshore_wind_curve_state %>%
        select(price, supply) -> L120.supply_points_state

      L120.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                           L120.offshore_wind_curve_state$mid.price,
                                           L120.offshore_wind_curve_state$base.price,
                                           L120.offshore_wind_curve_state$maxSubResource,
                                           L120.offshore_wind_curve_state )

      L120.offshore_wind_curve_state$curve.exponent <-  round(L120.error_min_curve.exp$minimum,energy.DIGITS_MAX_SUB_RESOURCE)
      L120.offshore_wind_curve_state %>%
        distinct(State, curve.exponent) -> L120.curve.exponent_state

      L120.curve.exponent %>%
        bind_rows(L120.curve.exponent_state) -> L120.curve.exponent

    }

    L120.offshore_wind_curve %>%
      left_join_error_no_match(L120.curve.exponent, by = "State") -> L120.offshore_wind_curve

    # Prepare cost curve for output
    L120.offshore_wind_curve %>%
      distinct(State, maxSubResource, mid.price, curve.exponent) %>%
      mutate(renewresource = "offshore wind resource",
              smooth.renewable.subresource = "offshore wind resource") %>%
      select(region = State, renewresource, smooth.renewable.subresource,
             maxSubResource, mid.price, curve.exponent) -> L120.RsrcCurves_EJ_R_offshore_wind_USA


    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state. Our starting data consists
    # of grid connection costs in $/MW by ReEDS region and wind class. This data also categorizes the connection cost into five
    # bins in each region and class. Using this data, we obtain a grid connection cost in $/GJ for each region/ class/ bin data
    # point as FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS*CF*MWH_GJ). Costs are then obtained for a state by averaging.
    # In the future, we might think about a separate state-level curve for grid connection costs.

    reeds_offshore_wind_curve_grid_cost %>%
      select(-Wind_Type) %>%
      gather(bin, cost, -Wind_Resource_Region, -Wind_Class) %>%
      filter(cost != 0) %>%
      left_join_error_no_match(reeds_offshore_wind_curve_CF_avg, by =c("Wind_Resource_Region", "Wind_Class" = "TRG")) %>%
      mutate(fcr = L120.offshore_wind_fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ) * gdp_deflator(1975,2013)) %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 select(Region, State),
                               by = c("Wind_Resource_Region" = "Region")) %>%
      group_by(State) %>%
      summarise(grid.cost = round(mean(grid.cost), energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(State %in% states_list) -> L120.GridCost_offshore_wind_USA

    # Assinging Alaska USA-maximum grid connection cost
    L120.GridCost_offshore_wind_USA %>%
      bind_rows(L120.GridCost_offshore_wind_USA %>%
                  summarise(grid.cost = max(grid.cost)) %>%
                  mutate(State = "AK")) -> L120.GridCost_offshore_wind_USA

    L120.offshore_wind_curve %>%
      distinct(State, CFmax) %>%
      filter(!is.na(CFmax)) -> L120.RegCapFactor_offshore_wind_USA

    # -----------------------------------------------------------------------------
    # Produce outputs

    L120.RsrcCurves_EJ_R_offshore_wind_USA %>%
      add_title("Offshore wind resource curve USA") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("Offshore wind resource curve by states") %>%
      add_precursors("gcam-usa/reeds_regions_states",
                     "L113.globaltech_capital_ATB",
                     "L113.globaltech_OMfixed_ATB",
                     "gcam-usa/A20.offshore_wind_class_depth",
                     "energy/A20.offshore_wind_depth_cap_cost",
                     "gcam-usa/reeds_offshore_wind_curve_capacity",
                     "gcam-usa/reeds_offshore_wind_curve_grid_cost",
                     "gcam-usa/reeds_offshore_wind_curve_CF_avg",
                     "gcam-usa/offshore_wind_potential_missing") ->
      L120.RsrcCurves_EJ_R_offshore_wind_USA

    L120.GridCost_offshore_wind_USA %>%
      add_title("Grid connectivity cost adder for offshore wind in USA") %>%
      add_units("$1975/GJ") %>%
      add_comments("Adder by States") %>%
      add_precursors( "gcam-usa/reeds_offshore_wind_curve_grid_cost",
                      "gcam-usa/reeds_regions_states",
                      "gcam-usa/reeds_offshore_wind_curve_CF_avg") ->
      L120.GridCost_offshore_wind_USA

    L120.RegCapFactor_offshore_wind_USA %>%
      add_title("Region-specific capacity factors for offshore wind") %>%
      add_units("Unitless") %>%
      add_comments("Region-specific maximum capacity factor") %>%
      same_precursors_as("L120.RsrcCurves_EJ_R_offshore_wind_USA") ->
      L120.RegCapFactor_offshore_wind_USA

    return_data(L120.RsrcCurves_EJ_R_offshore_wind_USA, L120.GridCost_offshore_wind_USA, L120.RegCapFactor_offshore_wind_USA)
  } else {
    stop("Unknown command")
  }
}

