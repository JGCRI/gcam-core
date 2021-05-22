# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2231.wind_update
#'
#' Updates region-specific onshore wind supply curves using improved global wind resource estimate from Eurek et al. (2016).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.SmthRenewRsrcCurves_onshore_wind}, \code{L2231.StubTechCapFactor_onshore_wind}, \code{L2231.SmthRenewRsrcTechChange_onshore_wind},
#' \code{L2231.StubTechCost_onshore_wind}. The corresponding file in the
#' original data system was \code{L2231.wind_update.R} (energy level2).
#' @details Updates region-specific onshore wind supply curves using improved global wind resource estimate from Eurek et al. (2016).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by summarise distinct arrange bind_rows rename
#' @importFrom tidyr gather
#' @importFrom stats optimize
#' @author MB GI AJS March 2019
module_energy_L2231.wind_update <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/fuel_energy_input",
             FILE = "energy/A20.wind_class_CFs",
             FILE = "energy/NREL_onshore_energy",
             FILE = "energy/onshore_wind_grid_cost",
             FILE = "energy/NREL_wind_energy_distance_range",
             "L113.globaltech_capital_ATB",
             "L113.globaltech_OMfixed_ATB",
             "L223.StubTechCapFactor_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.SmthRenewRsrcCurves_onshore_wind",
             "L2231.StubTechCapFactor_onshore_wind",
             "L2231.SmthRenewRsrcTechChange_onshore_wind",
             "L2231.StubTechCost_onshore_wind"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    country_name <- GCAM_region_ID <- IAM.Country.Name <- wind_class <- resource.potential.EJ <- region <-
      distance <- X_final_model_base_year <- fixed.charge.rate <- capital.overnight <- fcr <- OM.fixed <- CFmax <- price <-
      supply <- base.price <- Pvar <- maxSubResource <- percent.supply <- P1 <- Q1 <- mid.price <- P2 <- Q2 <- curve.exponent <-
      sector.name<- subsector.name <- intermittent.technology <- input.capital <- capacity.factor <-
      capital.overnight.lag <- capital.tech.change.5yr <- k1 <- tech.change.5yr <-
      tech.change <- year <- bin <- cost <- Wind_Resource_Region <- Wind_Class <- grid.cost <-
      renewresource <- smooth.renewable.subresource <- capacity.factor.capital <- capacity.factor.OM <-
      supplysector <- subsector <- stub.technology <- input.OM.fixed <- year.fillout <-
      techChange <- minicam.energy.input <- input.cost <- total <- IAM_country <- technology <- value <-
      CF <- k2 <- rep_dist_for_bin <- share <- cost_per_kW_km <- capital.tech.change.period <- tech.change.period <-
      time.change <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/mappings/fuel_energy_input")
    A20.wind_class_CFs <- get_data(all_data, "energy/A20.wind_class_CFs")
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    NREL_onshore_energy <- get_data(all_data, "energy/NREL_onshore_energy")
    onshore_wind_grid_cost <- get_data(all_data, "energy/onshore_wind_grid_cost")
    NREL_wind_energy_distance_range <- get_data(all_data, "energy/NREL_wind_energy_distance_range")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = TRUE)

    # ===================================================
    # Perform Computations

    # First, map NREL data on resource potential by country to GCAM 32 regions, convert PWh to EJ
    # Second, aggregate by GCAM region/ wind class

    NREL_onshore_energy %>%
      select(-total) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(country_name, GCAM_region_ID),
                               by = c("IAM_country" = "country_name")) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      select(-IAM_country, -GCAM_region_ID) %>%
      gather(wind_class, resource.potential.EJ, -region, -distance) %>%
      mutate(resource.potential.EJ = resource.potential.EJ * 1000 * CONV_TWH_EJ ) %>%
      group_by(region, wind_class) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
      ungroup() %>%
      filter(resource.potential.EJ != 0) -> L2231.onshore_wind_potential_EJ

    L113.globaltech_capital_ATB %>%
      gather_years() %>%
      filter(technology == "wind",
             year == max(MODEL_BASE_YEARS)) %>%
      distinct(value)-> L2231.onshore_wind_capital
    L2231.onshore_wind_capital <- as.numeric(L2231.onshore_wind_capital)

    L113.globaltech_capital_ATB %>%
      filter(technology == "wind") %>%
      select(fixed.charge.rate) -> L2231.onshore_wind_fcr
    L2231.onshore_wind_fcr <- as.numeric(L2231.onshore_wind_fcr)

    L113.globaltech_OMfixed_ATB %>%
      gather_years() %>%
      filter(technology == "wind",
             year == max(MODEL_BASE_YEARS)) %>%
      distinct(value)-> L2231.onshore_wind_OMfixed
    L2231.onshore_wind_OMfixed <- as.numeric(L2231.onshore_wind_OMfixed)

    L2231.onshore_wind_potential_EJ %>%
      left_join_error_no_match(A20.wind_class_CFs, by = c("wind_class")) %>%
      mutate(capital.overnight = L2231.onshore_wind_capital,
             fcr = L2231.onshore_wind_fcr,
             OM.fixed = L2231.onshore_wind_OMfixed) %>%
      mutate(price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ + OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      group_by(region) %>%
      mutate(CFmax = max(CF)) %>%
      arrange(region, price) %>%
      mutate(supply = cumsum(resource.potential.EJ)) %>%
      ungroup() %>%
      select(region, price, supply, CFmax) -> L2231.onshore_wind_matrix

    # Calculate maxSubResource, base.price, and Pvar.
    # base.price represents the minimum cost of generating electricity from the resource.
    # base.price comprises of the cost of generating power at the most optimal location.
    # Pvar represents costs that are expected to increase from base.price as deployment increases.
    # This models the increase in costs as more optimal locations are used first.

    L2231.onshore_wind_matrix %>%
      group_by(region) %>%
      arrange(region, price) %>%
      mutate(base.price = min(price),
             Pvar = price - base.price,
             maxSubResource = round(max(supply), energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      ungroup() -> L2231.onshore_wind_curve

    # Approximate mid-price using first supply points that are less than (p1, Q1) and greater than (p2,Q2) 50% of maxSubResource.
    # Using these points, the mid-price can be estimated as:
    # mid.price = ((P2-P1)*maxSubResource + 2*Q2*P1 - 2*Q1*P2)/(2*(Q2-Q1))
    # This assumes that the curve is largely linear between the two points above.

    # Calculating P1 and Q1
    L2231.onshore_wind_curve %>%
      mutate(percent.supply = supply / maxSubResource) %>%
      group_by(region) %>%
      filter(percent.supply <= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for highest price point below 50% of total resource
      filter(Pvar == max(Pvar)) %>%
      ungroup() %>%
      select(region, P1 = Pvar, Q1 = supply, maxSubResource) -> L2231.mid.price_1

    # Calculating P2 and Q2
    L2231.onshore_wind_curve %>%
      mutate(percent.supply = supply / maxSubResource) %>%
      group_by(region) %>%
      filter(percent.supply >= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for lowest price point above 50% of total resource
      filter(Pvar == min(Pvar)) %>%
      ungroup() %>%
      select(region, P2 = Pvar, Q2 = supply) -> L2231.mid.price_2

    # Calculating mid.price
    L2231.mid.price_1 %>%
      left_join_error_no_match(L2231.mid.price_2, by = "region") %>%
      mutate(mid.price = round(((P2 - P1) * maxSubResource + (2 * Q2 * P1) - (2 * Q1 * P2)) / (2 * (Q2 - Q1)),
                               energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, mid.price) -> L2231.mid.price

    L2231.onshore_wind_curve %>%
      left_join_error_no_match(L2231.mid.price, by = c("region")) -> L2231.onshore_wind_curve

    # Defining variables to be used later.
    region_list <- unique(L2231.onshore_wind_curve$region)
    L2231.curve.exponent <- tibble()

    for (L2231.region in region_list) {

      L2231.onshore_wind_curve %>%
        filter(region == L2231.region) -> L2231.onshore_wind_curve_region

      L2231.onshore_wind_curve_region %>%
        select(price, supply) -> L2231.supply_points_region

      L2231.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval = c(1.0,15.0),
                                            L2231.onshore_wind_curve_region$mid.price,
                                            L2231.onshore_wind_curve_region$base.price,
                                            L2231.onshore_wind_curve_region$maxSubResource,
                                            L2231.onshore_wind_curve_region )

      L2231.onshore_wind_curve_region$curve.exponent <-  round(L2231.error_min_curve.exp$minimum,energy.DIGITS_MAX_SUB_RESOURCE)
      L2231.onshore_wind_curve_region %>%
        distinct(region, curve.exponent) -> L2231.curve.exponent_region

      L2231.curve.exponent %>%
        bind_rows(L2231.curve.exponent_region) -> L2231.curve.exponent

    }

    L2231.onshore_wind_curve %>%
      left_join_error_no_match(L2231.curve.exponent, by = "region") -> L2231.onshore_wind_curve

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
    # a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(8760*kWh_GJ) and k2 = 1/(8760*kWh_GJ)
    # Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

    # First, calculate capital cost over time for "wind_offshore" technology
    L113.globaltech_capital_ATB %>%
      filter(technology == "wind") %>%
      fill_exp_decay_extrapolate(c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(capital.overnight = value, intermittent.technology = technology) -> L2231.onshore_wind_cap_cost

    # Second, calculate technological change
    L2231.onshore_wind_cap_cost %>%
      filter(intermittent.technology == "wind") %>%
      select(year, capital.overnight) %>%
      mutate(capital.tech.change.period = lag(capital.overnight, 1) / capital.overnight,
             time.change = year - lag(year),
             fixed.charge.rate = L2231.onshore_wind_fcr,
             OM.fixed = L2231.onshore_wind_OMfixed,
             k1 = fixed.charge.rate / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM.fixed) /
               (k1 * capital.overnight + k2 * OM.fixed),
             tech.change = round(abs(1 - (tech.change.period) ^ ( 1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
      select(year, tech.change) %>%
      filter(!is.na(tech.change),
             year > max(MODEL_BASE_YEARS)) -> L2231.TechChange_onshore_wind

    # Calculate capacity factor for all GCAM regions
    L223.StubTechCapFactor_elec %>%
      filter(subsector == "wind",
             !grepl("_offshore", stub.technology)) %>%
      left_join_error_no_match(L2231.onshore_wind_curve %>%
                                 distinct(region, CFmax),
                               by = c("region")) %>%
      mutate(capacity.factor= round(CFmax, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year, capacity.factor) -> L2231.StubTechCapFactor_onshore_wind

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ). This is calculated using three things:
    # 1. the average onshore wind $/kW-km data
    # 2. representative distances from bins used by NREL to assess wind potential, and
    # 3. Share of wind potential at various distance bins.
    # The shares and the representative distances for the bins are multiplied to determine the effective average distance
    # to grid for a given region, which is then multiplied to the $/kW-km cost to obtain the $/kW grid connection data.
    # GCAM region capacity factors calculated above to convert data into $/kWh which is then converted to $/GJ, and the
    # final number is obtained for each region by using the apporpriate FCR and deflator. The representative distance for
    # each bin are respectively 25 miles (40 km), 75 miles (120 km) and 100 miles (160 km) - for bins of 0-50 miles,
    # 50-100 miles, and 100+ miles. The literature generally assumes a 25-75 mile grid distance for onshore wind projects,
    # so midpoints of the first two bins are utilized, while there are only a few projects developed beyond a 100 miles
    # from the grid, so the minimum point for that bin is used to avoid skewing grid connection costs towards higher costs
    # based on resources which are less likely to be developed.

    # First, get share of potential by each distance bin for each GCAM region
    NREL_onshore_energy %>%
      select(IAM_country, distance, total) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(country_name, GCAM_region_ID),
                               by = c("IAM_country" = "country_name")) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      select(-IAM_country, -GCAM_region_ID) %>%
      group_by(region, distance) %>%
      summarise(total = sum(total)) %>%
      ungroup() %>%
      group_by(region) %>%
      mutate(share = total / sum(total)) %>%
      ungroup() -> L2231.onshore_wind_potential_share

    # Then, generate bins for each cost point using representative distances
    NREL_wind_energy_distance_range %>%
      filter(technology == "onshore_wind") %>%
      mutate(max = max * CONV_MILE_KM) -> NREL_wind_energy_distance_range

    # Add representative distances and cost to the potential share tables and calculate
    # average cost per kW by region.
    L2231.onshore_wind_potential_share %>%
      left_join_error_no_match(onshore_wind_grid_cost %>%
                                 select(-technology),
                               by = c("distance" = "distance_bin")) %>%
      select(-distance, -total) %>%
      mutate(cost = share * cost_per_kW_km * rep_dist_for_bin) %>%
      group_by(region) %>%
      summarise(cost = sum(cost)) %>%
      ungroup() -> L2231.onshore_wind_cost_per_kW

    # Now to convert 2010 $/kW cost to 1975 $/GJ cost
    L2231.onshore_wind_cost_per_kW %>%
      left_join_error_no_match(L2231.StubTechCapFactor_onshore_wind %>%
                                 select(region, capacity.factor) %>%
                                 unique(),
                               by = c("region")) %>%
      mutate(fcr = L2231.onshore_wind_fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * capacity.factor * CONV_KWH_GJ) * gdp_deflator(1975, 2010)) -> L2231.grid.cost

    # Set grid connection cost for all regions
    GCAM_region_names %>%
      select(region) %>%
      left_join_error_no_match(L2231.grid.cost %>%
                                 select(region, grid.cost),
                               by = "region") -> L2231.GridCost_onshore_wind

    # Preparing final tables
    region_order <- unique(GCAM_region_names$region)

    L2231.onshore_wind_curve %>%
      distinct(region, maxSubResource, mid.price, curve.exponent) %>%
      mutate(renewresource = "onshore wind resource",
             smooth.renewable.subresource = "onshore wind resource",
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource, smooth.renewable.subresource, year.fillout, maxSubResource, mid.price, curve.exponent) %>%
      mutate(region =  factor(region, levels  = region_order)) %>%
      arrange(region) -> L2231.SmthRenewRsrcCurves_onshore_wind

    # Copying tech change to all regions
    L2231.SmthRenewRsrcTechChange_onshore_wind <- write_to_all_regions(L2231.TechChange_onshore_wind, c("region", "year", "tech.change"), GCAM_region_names)
    L2231.SmthRenewRsrcTechChange_onshore_wind %>%
      mutate(renewresource = "onshore wind resource",
             smooth.renewable.subresource = "onshore wind resource") %>%
      select(region,renewresource, smooth.renewable.subresource,
             year.fillout = year, techChange = tech.change) -> L2231.SmthRenewRsrcTechChange_onshore_wind

    # Reading the grid connection cost as a state-level non-energy cost adder
    L223.StubTechCapFactor_elec %>%
      filter(subsector == "wind",
             !grepl("_offshore", stub.technology)) %>%
      select(region, supplysector, subsector, stub.technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L2231.GridCost_onshore_wind, by = c("region")) %>%
      mutate(grid.cost = round(grid.cost, energy.DIGITS_COST)) %>%
      rename(input.cost = grid.cost) -> L2231.StubTechCost_onshore_wind

    # ===================================================
    # Produce outputs

    L2231.SmthRenewRsrcCurves_onshore_wind %>%
      add_title("Smooth Renewable Resource Curve Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Resource curve and prices for onshore wind by region") %>%
      add_legacy_name("L2231.SmthRenewRsrcCurves_onshore_wind") %>%
      add_precursors( "common/iso_GCAM_regID", "common/GCAM_region_names", "energy/mappings/fuel_energy_input",  "energy/A20.wind_class_CFs", "L113.globaltech_capital_ATB", "L113.globaltech_OMfixed_ATB", "energy/NREL_onshore_energy", "L223.StubTechCapFactor_elec") ->
      L2231.SmthRenewRsrcCurves_onshore_wind

    L2231.StubTechCapFactor_onshore_wind %>%
      add_title("Capacity factor Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Capital and O&M Capacity factor for onshore wind by region") %>%
      add_legacy_name("L2231.StubTechCapFactor_onshore_wind") %>%
      same_precursors_as("L2231.SmthRenewRsrcCurves_onshore_wind") ->
      L2231.StubTechCapFactor_onshore_wind

    L2231.SmthRenewRsrcTechChange_onshore_wind %>%
      add_title("Smooth Renewable Resource Technological Change Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Technological change associated with onshore wind by region and period") %>%
      add_legacy_name("L2231.SmthRenewRsrcTechChange_onshore_wind") %>%
      same_precursors_as("L2231.SmthRenewRsrcCurves_onshore_wind") ->
      L2231.SmthRenewRsrcTechChange_onshore_wind

    L2231.StubTechCost_onshore_wind %>%
      add_title("Cost of onshore wind") %>%
      add_units("Unitless") %>%
      add_comments("Regional price adjustment of input cost by region") %>%
      add_legacy_name("L2231.StubTechCost_onshore_wind") %>%
      same_precursors_as("L2231.SmthRenewRsrcCurves_onshore_wind") %>%
      add_precursors("energy/onshore_wind_grid_cost", "energy/NREL_wind_energy_distance_range") ->
      L2231.StubTechCost_onshore_wind

    return_data(L2231.SmthRenewRsrcCurves_onshore_wind, L2231.StubTechCapFactor_onshore_wind, L2231.SmthRenewRsrcTechChange_onshore_wind, L2231.StubTechCost_onshore_wind)
  } else {
    stop("Unknown command")
  }
}
