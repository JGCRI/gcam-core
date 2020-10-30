# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA120.offshore_wind
#'
#' Takes in data on country-level offshore wind energy potential and global offshore wind capital
#' cost assumptions and generates tables containing region-level offshore wind data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.RsrcCurves_EJ_R_offshore_wind}, \code{L120.TechChange_offshore_wind},
#' \code{L120.GridCost_offshore_wind}, \code{L120.RegCapFactor_offshore_wind}. The corresponding file in the
#' original data system was \code{LA120.offshore_wind.R} (energy level1).
#' @details Takes in data on country-level offshore wind energy potential and global offshore wind
#' capital cost assumptions and generates tables containing region-level offshore wind data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by summarise distinct arrange bind_rows rename
#' @importFrom tidyr gather
#' @importFrom stats optimize
#' @author MB GI AJS March 2019
module_energy_LA120.offshore_wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/A20.wind_class_CFs",
             FILE = "energy/A20.offshore_wind_depth_cap_cost",
             FILE = "energy/NREL_offshore_energy",
             FILE = "energy/NREL_wind_energy_distance_range",
             FILE = "energy/offshore_wind_grid_cost",
             FILE = "energy/offshore_wind_potential_scaler",
             "L113.globaltech_capital_ATB",
             "L113.globaltech_OMfixed_ATB"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.RsrcCurves_EJ_R_offshore_wind",
             "L120.TechChange_offshore_wind",
             "L120.GridCost_offshore_wind",
             "L120.RegCapFactor_offshore_wind"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    country_name <- GCAM_region_ID <- wind_class <- resource.potential.PWh <- IAM_country <- depth_class <- distance_to_shore <-
      resource.potential.EJ <- fixed.charge.rate <- technology <- X_final_historical_year <- fcr <- OM.fixed <- price <- CFmax <-
      supply <- curve.exponent <- mid.price <- base.price <- maxSubResource <- supply_points <- Pvar <- P1 <- Q1 <- sector.name <-
      supplysector <- subsector.name <- subsector <- intermittent.technology <- year <- input.capital <- capital.overnight <-
      capacity.factor <- capital.overnight.lag <- capital.tech.change.5yr <- kl  <- tech.change.5yr <- tech.change <- bin <-
      cost <- Wind_Resource_Region <- Wind_Class <- grid.cost <- total <- value <- CF <- percent.supply <- P2 <- Q2 <- resource <-
      subresource <- k1 <- k2 <- potential <- region <- rep_dist_for_bin <- share <- cost_per_kW_km <- capital.tech.change.period <-
      tech.change.period <- time.change <- reason <- scaler <- NULL    # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    A20.wind_class_CFs <- get_data(all_data, "energy/A20.wind_class_CFs")
    A20.offshore_wind_depth_cap_cost <- get_data(all_data, "energy/A20.offshore_wind_depth_cap_cost")
    NREL_offshore_energy  <- get_data(all_data, "energy/NREL_offshore_energy")
    NREL_wind_energy_distance_range <- get_data(all_data, "energy/NREL_wind_energy_distance_range")
    offshore_wind_grid_cost <- get_data(all_data, "energy/offshore_wind_grid_cost")
    offshore_wind_potential_scaler <- get_data(all_data, "energy/offshore_wind_potential_scaler")

    # -----------------------------------------------------------------------------
    # Perform computations

    # Map NREL data on resource potential by country to GCAM 32 regions, convert PWh to EJ,
    # aggregate by GCAM region/ wind class/ depth class
    NREL_offshore_energy %>%
      select(-total) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(country_name, GCAM_region_ID) %>% distinct(),
                               by = c("IAM_country" = "country_name")) %>%
      gather(wind_class, resource.potential.PWh, -IAM_country, -GCAM_region_ID, -depth_class, -distance_to_shore) %>%
      mutate(resource.potential.EJ = resource.potential.PWh * 1000 * CONV_TWH_EJ ) %>%
      group_by(GCAM_region_ID, wind_class, depth_class) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ)) %>%
      ungroup() %>%
      filter(resource.potential.EJ != 0) -> L120.offshore_wind_potential_EJ

    L120.offshore_wind_capital <- A20.offshore_wind_depth_cap_cost

    L113.globaltech_capital_ATB %>%
      filter(technology == "wind_offshore") %>%
      select(fixed.charge.rate) -> L120.offshore_wind_fcr
    L120.offshore_wind_fcr <- as.numeric(L120.offshore_wind_fcr)

    L113.globaltech_OMfixed_ATB %>%
      gather_years() %>%
      filter(technology == "wind_offshore",
             year == max(HISTORICAL_YEARS)) %>%
      distinct(value)-> L120.offshore_wind_OMfixed
    L120.offshore_wind_OMfixed <- as.numeric(L120.offshore_wind_OMfixed)

    # NOTE that the process for calculating supply/ price is different for offshore wind (vs. onshore wind).  For offshore wind, we
    # (1) calculate the price associated with each wind class, then (2) arrange the dataset by region/ price and calculate
    # cumulative resource supply. (Conversely, for onshore wind, we (1) arrange the data by capacity factor and calculate supply,
    # then (2) calculate the corresponding prices.)  The reason for this difference is that, for offshore wind, price varies with
    # both capacity factor AND ocean depth.  (For onshore wind, CF is the only major determinant of price.)  Thus, arranging by CF
    # and calculating cumulative supply would result in an illogical offshore wind supply curve where price fluctuates up and down
    # as supply increases.
    L120.offshore_wind_potential_EJ %>%
      left_join_error_no_match(A20.wind_class_CFs, by = c("wind_class")) %>%
      left_join_error_no_match(L120.offshore_wind_capital, by = c("depth_class")) %>%
      mutate(fcr = L120.offshore_wind_fcr,
             OM.fixed = L120.offshore_wind_OMfixed) %>%
      mutate(price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ +
               OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      group_by(GCAM_region_ID) %>%
      mutate(CFmax = max(CF)) %>%
      arrange(GCAM_region_ID, price) %>%
      mutate(supply = cumsum(resource.potential.EJ)) %>%
      ungroup() %>%
      select(GCAM_region_ID, price, supply, CFmax) -> L120.offshore_wind_matrix

    # Assigning additional resource to the USA because the global dataset does not include any resource for Alaska,
    # even though initial estimates suggest that its resource potential could be very large.
    # Each supply point is simply increased by 5% (specified in offshore_wind_potential_scaler); thus, Alaska's resource is assumed to be a representative
    # sample of the total USA resource.
    L120.offshore_wind_matrix %>%
      left_join_error_no_match(offshore_wind_potential_scaler %>%
                                 select(-reason),
                               by = "GCAM_region_ID") %>%
      mutate(supply = supply * scaler) -> L120.offshore_wind_matrix

    # Calculate maxSubResource, base.price, and Pvar.
    # base.price represents the minimum cost of generating electricity from the resource.
    # base.price comprises of the cost of generating power at the most optimal location.
    # Pvar represents costs that are expected to increase from base.price as deployment increases.
    # This models the increase in costs as more optimal locations are used first.

    L120.offshore_wind_matrix %>%
      group_by(GCAM_region_ID) %>%
      arrange(GCAM_region_ID, price) %>%
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
      mutate(percent.supply = supply/maxSubResource) %>%
      group_by(GCAM_region_ID) %>%
      filter(percent.supply <= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for highest price point below 50% of total resource
      filter(Pvar == max(Pvar)) %>%
      ungroup() %>%
      select(GCAM_region_ID, P1 = Pvar, Q1 = supply, maxSubResource) -> L120.mid.price_1

    # Calculating P2 and Q2
    L120.offshore_wind_curve %>%
      mutate(percent.supply = supply/maxSubResource) %>%
      group_by(GCAM_region_ID) %>%
      filter(percent.supply >= energy.WIND_CURVE_MIDPOINT) %>%
      # filter for lowest price point above 50% of total resource
      filter(Pvar == min(Pvar)) %>%
      ungroup() %>%
      select(GCAM_region_ID, P2 = Pvar, Q2 = supply) -> L120.mid.price_2

    # Calculating mid.price
    L120.mid.price_1 %>%
      left_join_error_no_match(L120.mid.price_2, by = "GCAM_region_ID") %>%
      mutate(mid.price = round(((P2 - P1) * maxSubResource + 2 * Q2 * P1 - 2 * Q1 * P2) / (2 * (Q2 - Q1)),
                               energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(GCAM_region_ID, mid.price) -> L120.mid.price

    L120.offshore_wind_curve %>%
      left_join_error_no_match(L120.mid.price, by = c("GCAM_region_ID")) -> L120.offshore_wind_curve

    # Defining variables to be used later.
    region_list <- unique(L120.offshore_wind_curve$GCAM_region_ID)
    L120.curve.exponent <- tibble()

    for (L120.region in region_list) {

      L120.offshore_wind_curve %>%
        filter(GCAM_region_ID == L120.region) -> L120.offshore_wind_curve_region

      L120.offshore_wind_curve_region %>%
        select(price, supply) -> L120.supply_points_region

      L120.error_min_curve.exp <- optimize(f = smooth_res_curve_approx_error, interval=c(1.0,15.0),
                                           L120.offshore_wind_curve_region$mid.price,
                                           L120.offshore_wind_curve_region$base.price,
                                           L120.offshore_wind_curve_region$maxSubResource,
                                           L120.offshore_wind_curve_region )

      L120.offshore_wind_curve_region$curve.exponent <-  round(L120.error_min_curve.exp$minimum,energy.DIGITS_MAX_SUB_RESOURCE)
      L120.offshore_wind_curve_region %>%
        distinct(GCAM_region_ID, curve.exponent) -> L120.curve.exponent_region

      L120.curve.exponent %>%
        bind_rows(L120.curve.exponent_region) -> L120.curve.exponent

    }

    L120.offshore_wind_curve %>%
      left_join_error_no_match(L120.curve.exponent, by = "GCAM_region_ID") -> L120.offshore_wind_curve

    # Prepare supply curve for output
    L120.offshore_wind_curve %>%
      mutate(resource = "offshore wind resource",
             subresource = "offshore wind resource") %>%
      distinct(GCAM_region_ID, resource, subresource, maxSubResource, mid.price, curve.exponent) -> L120.RsrcCurves_EJ_R_offshore_wind

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, then every price point of the curve will scale by a factor a' given as follows:
    # a' = (k1.a.CC + k2. OM-fixed) / (k1.CC + k2. OM-fixed) where k1 = FCR/(CONV_YEAR_HOURS*kWh_GJ) and k2 = 1/(CONV_YEAR_HOURS*kWh_GJ)
    # Thus, we calculate model input parameter techChange (which is the reduction per year) as 1-a'^(1/5)

    # First, calculate capital cost over time for "wind_offshore" technology
    L113.globaltech_capital_ATB %>%
      # filter for wind and offshore wind technologies; wind is needed because it serves
      # as a "shadow technology" for offshore wind in fill_exp_decay_extrapolate function
      filter(technology %in% c("wind", "wind_offshore")) %>%
      fill_exp_decay_extrapolate(c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      filter(technology == "wind_offshore") %>%
      rename(capital.overnight=value, intermittent.technology=technology) -> L120.offshore_wind_cap_cost

    # Second, calculate technological change
    L120.offshore_wind_cap_cost %>%
      filter(intermittent.technology == "wind_offshore") %>%
      select(year, capital.overnight) %>%
      mutate(capital.tech.change.period = lag(capital.overnight, 1) / capital.overnight,
             time.change = year - lag(year),
             fixed.charge.rate = L120.offshore_wind_fcr,
             OM.fixed = L120.offshore_wind_OMfixed,
             k1 = fixed.charge.rate / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
             tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM.fixed) /
               (k1 * capital.overnight + k2 * OM.fixed),
             tech.change = round(abs(1 - (tech.change.period) ^ ( 1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
      select(year, tech.change) %>%
      filter(!is.na(tech.change),
             year > max(MODEL_BASE_YEARS)) -> L120.TechChange_offshore_wind

    # Creating region-specific capacity factors to be used for levelizing grid connection costs. This is calculated by
    # getting the maximum possible capacity factor for a region. Since offshore wind resource utilization is pretty low, it is
    # not expected that regions would be able to exhaust the potential offered by the highest capacity class available.
    NREL_offshore_energy %>%
      select(-depth_class,-distance_to_shore,-total) %>%
      inner_join(iso_GCAM_regID %>%
                                 select(country_name, GCAM_region_ID),
                               by = c("IAM_country" = "country_name")) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      select(-IAM_country, -GCAM_region_ID) %>%
      gather(class,potential,-region) %>%
      group_by(region, class) %>%
      summarise(potential = sum(potential)) %>%
      ungroup() %>%
      left_join_error_no_match(A20.wind_class_CFs,
                               by = c("class" = "wind_class")) %>%
      filter(potential != 0) %>%
      select(-class, -potential) %>%
      group_by(region) %>%
      mutate(CF = max(CF)) %>%
      ungroup() %>%
      unique() -> L120.offshore_wind_CF

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ). This is calculated using three things:
    # 1. the offshore wind $/kW-km cost based on distance cut-offs.
    # 2. Average distance from shore of existing and upcoming project for each bin used by NREL to assess wind potential - which are  basically midpoints, and
    # 3. Share of wind potential at various distance bins.
    # The shares and the representative distances for the bins are multiplied to determine the effective average distance to grid for a given region,
    # which is then multiplied to the $/kW-km cost to obtain the $/kW grid connection data. GCAM region capacity factors calculated above to convert data into $/kWh
    # which is then converted to $/GJ, and the final number is obtained for each region by using the apporpriate FCR and deflator.

    # First, get share of potential by each distance bin for each GCAM region
    NREL_offshore_energy %>%
      select(IAM_country, distance_to_shore, total) %>%
      inner_join(iso_GCAM_regID %>%
                                 select(country_name, GCAM_region_ID),
                               by = c("IAM_country" = "country_name")) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      select(-IAM_country, -GCAM_region_ID) %>%
      group_by(region, distance_to_shore) %>%
      summarise(total = sum(total)) %>%
      ungroup() %>%
      group_by(region) %>%
      mutate(share = total / sum(total)) %>%
      ungroup() -> L120.offshore_wind_potential_share

    # Then, generate bins for each cost point using representative distances from the shore
    NREL_wind_energy_distance_range %>%
      filter(technology == "offshore_wind") %>%
      mutate(max = max * CONV_NMILE_KM) -> NREL_wind_energy_distance_range

    # Add representative distances and cost to the potential share tables and calculate
    # average cost per kW by region.
    L120.offshore_wind_potential_share %>%
      left_join_error_no_match(offshore_wind_grid_cost %>%
                                 select(-technology),
                               by = c("distance_to_shore" = "distance_bin")) %>%
      mutate(cost = share * cost_per_kW_km * rep_dist_for_bin) %>%
      group_by(region) %>%
      summarise(cost = sum(cost)) %>%
      ungroup() -> L120.offshore_wind_cost_per_kW

    # Now to convert 2013 $/kW cost to 1975 $/GJ cost
    L120.offshore_wind_cost_per_kW %>%
      left_join_error_no_match(L120.offshore_wind_CF %>%
                                 select(region, CF) %>%
                                 unique(),
                               by = "region") %>%
      mutate(fcr = L120.offshore_wind_fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF* CONV_KWH_GJ) * gdp_deflator(1975, 2013)) -> L120.grid.cost

    # Set grid connection cost for all regions
    GCAM_region_names %>%
      select(region) %>%
      left_join_error_no_match(L120.grid.cost %>%
                                 select(region, grid.cost),
                               by = "region") -> L120.GridCost_offshore_wind

    L120.offshore_wind_curve %>%
      distinct(GCAM_region_ID, CFmax) %>%
      filter(!is.na(CFmax)) -> L120.RegCapFactor_offshore_wind

    # -----------------------------------------------------------------------------
    # Produce outputs

    L120.RsrcCurves_EJ_R_offshore_wind %>%
      add_title("Offshore wind resource curve") %>%
      add_units("EJ") %>%
      add_comments("Offshore wind resource curve by region") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "energy/NREL_offshore_energy",
                     "energy/A20.wind_class_CFs", "L113.globaltech_capital_ATB",
                     "L113.globaltech_OMfixed_ATB", "energy/A20.offshore_wind_depth_cap_cost",
                     "energy/offshore_wind_potential_scaler") ->
      L120.RsrcCurves_EJ_R_offshore_wind

    L120.TechChange_offshore_wind %>%
      add_title("Technological Change Offshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Technological Change associated with offshore wind by year") %>%
      add_precursors("L113.globaltech_capital_ATB", "L113.globaltech_OMfixed_ATB") ->
      L120.TechChange_offshore_wind

    L120.GridCost_offshore_wind %>%
      add_title("Grid connectivity cost adder for offshore wind") %>%
      add_units("$1975/GJ") %>%
      add_comments("Adder by GCAM Region") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "energy/NREL_offshore_energy",
                     "energy/A20.wind_class_CFs", "L113.globaltech_capital_ATB",
                     "energy/offshore_wind_grid_cost", "energy/NREL_wind_energy_distance_range") ->
      L120.GridCost_offshore_wind

    L120.RegCapFactor_offshore_wind %>%
      add_title("Region-specific capacity factors for offshore wind") %>%
      add_units("Unitless") %>%
      add_comments("Region-specific maximum capacity factor") %>%
      same_precursors_as(L120.RsrcCurves_EJ_R_offshore_wind) ->
      L120.RegCapFactor_offshore_wind

    return_data(L120.RsrcCurves_EJ_R_offshore_wind,
                L120.TechChange_offshore_wind,
                L120.GridCost_offshore_wind,
                L120.RegCapFactor_offshore_wind)
  } else {
    stop("Unknown command")
  }
}

