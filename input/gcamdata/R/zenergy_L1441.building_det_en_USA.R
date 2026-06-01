# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1441.building_det_en_USA
#'
#' Calculates USA detailed buildings energy data based on Scout
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1441.end_use_eff_USA}, \code{L1441.in_EJ_R_bld_serv_F_tech_Yh_USA}, \code{L1441.NEcost_75USDGJ_USA},
#'  \code{L1441.internal_gains_USA}, \code{L1441.base_service_EJ_serv_fuel_tech_USA}, \code{L1441.prices_bld_USA}.
#'  Following the structure of \code{zenergy_L144.building_det_en.R} and expand detailed technologies in the USA region.
#' @details Calculates building energy consumption, non-energy costs, energy output by service, internal gains, and end-use technology
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise
#' @importFrom tidyr complete replace_na
#' @author YZ Sep 2025
module_energy_L1441.building_det_en_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Scout_bld_calibration",
             FILE = "gcam-usa/calibrated_techs_bld_usa",
             FILE = "gcam-usa/A44.globaltech_eff",
             FILE = "gcam-usa/A44.globaltech_eff_avg",
             FILE = "gcam-usa/A44.globaltech_shares",
             FILE = "gcam-usa/A44.CalPrice_service_gcamusa",
             FILE = "gcam-usa/A44.globaltech_cost",
             FILE = "gcam-usa/A44.globaltech_intgains",
             "L144.in_EJ_R_bld_serv_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1441.in_EJ_R_bld_serv_F_tech_Yh_USA",
             "L1441.base_service_EJ_serv_fuel_tech_USA",
             "L1441.end_use_eff_USA",
             "L1441.prices_bld_USA",
             "L1441.NEcost_75USDGJ_USA",
             "L1441.internal_gains_USA"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Scout_bld_calibration <- get_data(all_data, "gcam-usa/Scout_bld_calibration", strip_attributes = TRUE)
    calibrated_techs_bld_usa <- get_data(all_data, "gcam-usa/calibrated_techs_bld_usa", strip_attributes = TRUE)
    A44.globaltech_eff <- get_data(all_data, "gcam-usa/A44.globaltech_eff", strip_attributes = TRUE) %>% gather_years()
    A44.globaltech_eff_avg <- get_data(all_data, "gcam-usa/A44.globaltech_eff_avg", strip_attributes = TRUE)
    A44.globaltech_shares <- get_data(all_data, "gcam-usa/A44.globaltech_shares", strip_attributes = TRUE)
    A44.CalPrice_service_gcamusa <- get_data(all_data, "gcam-usa/A44.CalPrice_service_gcamusa", strip_attributes = TRUE) %>% gather_years()
    A44.globaltech_cost <- get_data(all_data, "gcam-usa/A44.globaltech_cost", strip_attributes = TRUE)
    A44.globaltech_intgains <- get_data(all_data, "gcam-usa/A44.globaltech_intgains", strip_attributes = TRUE)
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")

    # ===================================================

    . <- GCAM_region_ID <- NEcostPerService <- efficiency <- fuel <- input.ratio <- scaler <- sector <- service <-
      share_TFEbysector <- subsector <- supplysector <- technology <- year <- value <- NULL

    # Create list spanning historical and future years
    HIST_FUT_YEARS <- c(HISTORICAL_YEARS, FUTURE_YEARS)

    ## Part 1 - energy consumption: use Scout data for detailed services & technologies in the USA region ----
    # YZ 2025/8/27 energy consumption
    # Scout output data processed to compute shares of service/technology within state/sector/fuel
    # resid cooling/gas is included in scout but not in detailed US buildings
    L1441.Scout_bld_calibration <- Scout_bld_calibration %>%
      filter(!(sector == "resid" & fuel == "gas" & service == "cooling")) %>%
      mutate(service = paste(sector, service)) %>%
      # aggregate to regional level
      group_by(sector, fuel, service, technology, year) %>%
      summarise(energy = sum(energy)) %>%
      ungroup() %>%
      # re-calculate shares based on regional values
      group_by(sector, fuel, year) %>%
      mutate(share = energy / sum(energy),
             region = gcam.USA_REGION,
             GCAM_region_ID = gcam.USA_CODE) %>%
      ungroup()

    # further disagg "other" services in Scout to resid televisions
    # and comm non-building
    L1441.Scout_bld_downscale_other <- L1441.Scout_bld_calibration %>%
      filter(fuel == "electricity",
             service %in% c("comm other", "resid other")) %>%
      mutate(share = if_else(service == 'comm other',
                             share * (1-energy.USA_COMM_OTHERELEC_NONBLD_FRAC),
                             share * (1-energy.USA_RESID_OTHERELEC_TV_FRAC))) %>%
      bind_rows(L1441.Scout_bld_calibration %>%
                  filter(fuel == "electricity",
                         service %in% c("comm other", "resid other")) %>%
                  mutate(service = if_else(service == 'comm other',
                                                 'comm non-building', 'resid televisions'),
                         share = if_else(service == 'comm non-building',
                                         share * energy.USA_COMM_OTHERELEC_NONBLD_FRAC,
                                         share * energy.USA_RESID_OTHERELEC_TV_FRAC)))

    # Remove the initially reported "other" data, bind the downscaled other
    L1441.Scout_bld_calibration <- anti_join(L1441.Scout_bld_calibration, L1441.Scout_bld_downscale_other,
                                            by = c("sector", "service", "fuel", "technology", "year")) %>%
      bind_rows(L1441.Scout_bld_downscale_other) %>%
      # in Scout there's only resid heating using biomass (share = 1), while in GCAM-core resid, traditional biomass
      # are split into 'resid heating' and 'resid others'. We will filter out the biomass in Scout for simplicity.
      # Note that there's no 'coal' in Scout data, so all other fuels remaining in Scout resid sector are modern fuels
      filter(fuel != 'biomass') %>%
      mutate(sector = paste0('bld_', sector))

    L1441.in_EJ_R_bld_serv_F_tech_Yh_scout <- L144.in_EJ_R_bld_serv_F_Yh %>%
      semi_join(L1441.Scout_bld_calibration,
                by = c('GCAM_region_ID','sector','fuel')) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%
      left_join(L1441.Scout_bld_calibration %>%
                  select(-year),
                by = c('GCAM_region_ID','sector','fuel'), relationship = "many-to-many") %>%
      mutate(value = total * share) %>%
      select(GCAM_region_ID, sector, fuel, service, technology, year, value)

    ## Part 2 - energy consumption: map Scout technologies to GCAM-USA technologies ----
    # YZ 2025/9/2
    # original L549 from zgcamusa_L244
    # Shares allocated to partitioned technologies need to be computed first using efficiencies
    L1441.globaltech_eff_prt <- A44.globaltech_eff %>%
      semi_join(A44.globaltech_eff_avg, by = c("supplysector", "subsector")) %>%
      filter(year == gcamusa.EFFICIENCY_PARTITION_YEAR) %>%
      select(supplysector, subsector, technology, efficiency = value)

    # Calculate technology shares using efficiency values
    L1441.globaltech_shares <- A44.globaltech_eff_avg %>%
      # Adding specific technology efficiency to stock average efficiency
      left_join_error_no_match(L1441.globaltech_eff_prt, by = c("supplysector", "subsector", "technology1" = "technology")) %>%
      rename(efficiency_tech1 = efficiency) %>%
      left_join_error_no_match(L1441.globaltech_eff_prt, by = c("supplysector", "subsector", "technology2" = "technology")) %>%
      rename(efficiency_tech2 = efficiency) %>%
      # Calculate technology shares using stock average efficiency and individual technology efficiencies
      # Equation can be derived by solving following system of equations:
      # stockavg = efficiency_tech1 * share_tech1 + efficiency_tech2 * share_tech2
      # share_tech1 + share_tech2 = 1
      mutate(share_tech1 = (stockavg - efficiency_tech2) / (efficiency_tech1 - efficiency_tech2),
             share_tech2 = 1 - share_tech1) %>%
      # Keep only same names as A44.globaltech_shares and bind with A44.globaltech_shares
      select(names(A44.globaltech_shares)) %>%
      bind_rows(A44.globaltech_shares) %>%
      na.omit() %>%
      # Clunky, but we want only one technology and share value, currently have technology1, technology2, share1, share2
      gather(share_type, share, share_tech1, share_tech2)%>%
      gather(tech_type, technology, technology1, technology2) %>%
      # Filter for same technology and share number, then remove tech_type and share_type columns
      filter(substr(tech_type, nchar(tech_type), nchar(tech_type)) == substr(share_type, nchar(share_type), nchar(share_type))) %>%
      select(-tech_type, -share_type)

    # Replace the calibration data with scout data
    # First, set the category names for matching
    # Also re-set the default technology name of lighting to incandescent
    L1441.in_EJ_R_bld_serv_F_tech_Yh_mapped <- L1441.in_EJ_R_bld_serv_F_tech_Yh_scout %>%
      rename(supplysector = service) %>%
      left_join(calibrated_techs_bld_usa %>%
                  select(sector, supplysector, fuel, subsector, minicam.energy.input) %>%
                  mutate(sector = paste0('bld_', sector)) %>%
                  distinct(), by = c("sector", "supplysector", "fuel")) %>%
      select(GCAM_region_ID, supplysector, subsector, technology, minicam.energy.input, year, calibrated.value = value) %>%
      mutate(technology = if_else(technology == "lighting", "incandescent", technology))

    # The Scout data is already disaggregated to "efficiency-partitioned" technologies that don't have the string "hi-eff"
    # These include heat pumps vs electric resistance for heating and hot water
    # 8/24/23 GPK - original Scout data submissions included partitioning of incandescent, fluorescent, and solid state lighting
    # As the current one (v3) does not, these techs need to be exogenously partitioned
    # First, re-set the technology name from "lighting" to "incandescent" which is the technology1 assignment in A44.globaltech_eff_avg
    # This data table L244.EffPrtTechsForScout is a workaround to add a "resid lighting / incandescent" row.
    L1441.EffPrtTechsForScout <- A44.globaltech_eff_avg %>%
      filter(grepl("hi-eff", technology2) | grepl("lighting", supplysector)) %>%
      bind_rows(filter(A44.globaltech_shares, supplysector == "resid lighting" & technology1 =="incandescent")) %>%
      select(supplysector, subsector, technology = technology1) %>%
      distinct()

    L1441.in_EJ_state_bld_F_U_techEffPrt_Yh <- L1441.in_EJ_R_bld_serv_F_tech_Yh_mapped %>%
      semi_join(L1441.EffPrtTechsForScout,
                by = c("supplysector", "subsector", "technology")) %>%
      inner_join(L1441.globaltech_shares, by = c("supplysector", "subsector"),
                 suffix = c(".scout", ".gcam"), relationship = "many-to-many") %>%
      mutate(calibrated.value = calibrated.value * share) %>%
      select(GCAM_region_ID, supplysector, subsector, technology = technology.gcam, minicam.energy.input, year, calibrated.value)

    # Calibration values from scout include the technologies whose calibration values aren't partitioned by efficiency,
    # and those whose values were in the prior block. anti_join to make sure none are duplicated
    L1441.in_EJ_R_bld_serv_F_tech_Yh_USA <- anti_join(L1441.in_EJ_R_bld_serv_F_tech_Yh_mapped, L1441.EffPrtTechsForScout,
                                                 by = c("supplysector", "subsector", "technology")) %>%
      bind_rows(L1441.in_EJ_state_bld_F_U_techEffPrt_Yh) %>%
      arrange(GCAM_region_ID, supplysector, subsector, technology)

    ## Part 3 - calculate service output based on technology efficiency ----
    # YZ 2025/9/2
    # original L507 from zgcamusa_L244
    L1441.end_use_eff_USA <- A44.globaltech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, HIST_FUT_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
    L1441.base_service_EJ_serv_fuel_tech_USA <- L1441.in_EJ_R_bld_serv_F_tech_Yh_USA %>%
      # Add in efficiency by technology
      left_join_error_no_match(L1441.end_use_eff_USA,
                               by = c("supplysector", "subsector",
                                      "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT))

    ## Part 4 - weighted national prices ----
    # YZ 2025/9/2
    # use gcam-usa state-level prices, weighted by Scout 'energy' column to calculate national prices
    L1441.prices_bld_gcamusa<- A44.CalPrice_service_gcamusa
    if(max(A44.CalPrice_service_gcamusa$year) < MODEL_FINAL_BASE_YEAR) {
      warning(paste0("Historical data in gcam-usa/A44.CalPrice_service_gcamusa only goes up to ",
                     max(A44.CalPrice_service_gcamusa$year), " extending to ", MODEL_FINAL_BASE_YEAR,
                     " consider updating data."))
      A44.CalPrice_service_gcamusa %>%
        tidyr::expand(tidyr::nesting(region, sector), year = MODEL_BASE_YEARS) %>%
        left_join(A44.CalPrice_service_gcamusa, by=c("region", "sector", "year")) %>%
        group_by(region, sector) %>%
        mutate(value = approx_fun(year, value, rule=2)) %>%
        ungroup() ->
        L1441.prices_bld_gcamusa
    }

    L1441.Scout_state_energy_by_serv <- Scout_bld_calibration %>%
      mutate(service = paste(sector, service)) %>%
      group_by(state, service, year) %>%
      summarise(energy = sum(energy)) %>%
      ungroup() %>%
      rename(state_name = state) %>%
      left_join_error_no_match(select(states_subregions, state, state_name),
                               by = "state_name") %>%
      select(-state_name)

    L1441.Scout_state_energy_by_serv_adj <- L1441.Scout_state_energy_by_serv %>%
      # use national partition value to split resid and comm other service
      mutate(energy = if_else(service == 'comm other', energy * (1-energy.USA_COMM_OTHERELEC_NONBLD_FRAC),
                              if_else(service == 'resid other', energy * (1-energy.USA_RESID_OTHERELEC_TV_FRAC),
                                      energy))) %>%
      bind_rows(L1441.Scout_state_energy_by_serv %>%
                  filter(service %in% c('comm other','resid other')) %>%
                  mutate(service = if_else(service == 'comm other',
                                           'comm non-building',
                                           'resid televisions'),
                         energy = if_else(service == 'comm non-building',
                                          energy * energy.USA_COMM_OTHERELEC_NONBLD_FRAC,
                                          energy * energy.USA_RESID_OTHERELEC_TV_FRAC)))

    L1441.prices_bld_USA<- L1441.prices_bld_gcamusa %>%
      # YZ 2025/9/2 currently Scout doesn't provide data for AK and HI
      # we filter the states that has a match in scout (drop AK and HI for now)
      filter(region %in% unique(L1441.Scout_state_energy_by_serv$state) ) %>%
      left_join_error_no_match(L1441.Scout_state_energy_by_serv_adj %>%
                                 select(-year),
                               by = c('region' = 'state','sector'='service')) %>%
      group_by(sector, year) %>%
      summarise(value = sum( energy*value/sum(energy) ) ) %>%
      ungroup() %>%
      complete(nesting(sector), year = c(year, HIST_FUT_YEARS)) %>%
      group_by(sector) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()


    ## Part 5 - non-energy cost ----
    # YZ 2025/9/2 modified from original L681 in zgcamusa_L244
    # Non-fuel costs of global building technologies
    # gcam-usa/A44.globaltech_cost already in the unit of 1975$/GJ
    # note that it doesn't include cost for comm non-building
    L1441.NEcost_75USDGJ_USA <- A44.globaltech_cost %>%
      gather_years(value_col = "NEcostPerService") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, HIST_FUT_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(NEcostPerService = approx_fun(year, NEcostPerService)) %>%
      ungroup()

    ## Part 6 - internal gains ----
    # YZ 2025/9/2 modified from original L696 from zgcamusa_L244
    L1441.internal_gains_USA <- A44.globaltech_intgains %>%
      repeat_add_columns(tibble(year = HIST_FUT_YEARS)) %>%
      left_join_error_no_match(L1441.end_use_eff_USA,
                               by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(value = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(supplysector, subsector, technology, year, value)

    ## additional adjustment ----
    # YZ 2025/9/22 adjust zero price to be positive if the energy use is positive
    L1441.prices_bld_USA_adj <- L1441.in_EJ_R_bld_serv_F_tech_Yh_USA %>%
      left_join_error_no_match(L1441.prices_bld_USA, by = c('supplysector'='sector','year')) %>%
      filter(calibrated.value!=0,value==0) %>%
      select(-value) %>%
      # replace zero prices for non-zero energy use sector with the minimum positive price in historical years in that sector
      left_join_error_no_match(L1441.prices_bld_USA %>%
                                 filter(value !=0) %>%
                                 group_by(sector) %>%
                                 summarise(value = min(value)) %>%
                                 ungroup(), by = c('supplysector'='sector')) %>%
      rename(sector = supplysector) %>%
      select(names(L1441.prices_bld_USA)) %>%
      distinct()

    L1441.prices_bld_USA <- L1441.prices_bld_USA %>%
      anti_join(L1441.prices_bld_USA_adj, by = c('sector', 'year') ) %>%
      bind_rows(L1441.prices_bld_USA_adj)


    # ===================================================


    L1441.in_EJ_R_bld_serv_F_tech_Yh_USA %>%
      add_title("Detailed Disaggregation of Building energy consumption in USA by sector / fuel / service / technology / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Energy consumption by detailed service and technology is calculated by allocating energy consumption across them using calculated shares from Scout data") %>%
      add_legacy_name("L1441.in_EJ_R_bld_serv_F_tech_Yh_USA") %>%
      add_precursors("gcam-usa/Scout_bld_calibration", "L144.in_EJ_R_bld_serv_F_Yh", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares") ->
      L1441.in_EJ_R_bld_serv_F_tech_Yh_USA

    L1441.base_service_EJ_serv_fuel_tech_USA %>%
      add_title("Building service energy output in USA by sector / service / fuel / technology / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("Product of energy consumption and efficiency") %>%
      add_legacy_name("L1441.base_service_EJ_serv_fuel_tech_USA") %>%
      add_precursors("gcam-usa/Scout_bld_calibration", "L144.in_EJ_R_bld_serv_F_Yh", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares", "gcam-usa/A44.globaltech_eff") ->
      L1441.base_service_EJ_serv_fuel_tech_USA

    L1441.end_use_eff_USA %>%
      add_title("Building end-use technology efficiency in USA by supplysector / subsector / technology / year") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Detailed USA building service technology efficiencies") %>%
      add_legacy_name("L1441.end_use_eff_USA") %>%
      add_precursors("gcam-usa/A44.globaltech_eff") ->
      L1441.end_use_eff_USA

    L1441.prices_bld_USA %>%
      add_title("service prices in USA by historical year") %>%
      add_units("$1975/unit") %>%
      add_comments("Weighted by state energy consumption in Scout") %>%
      add_legacy_name("L1441.prices_bld_USA") %>%
      add_precursors("gcam-usa/Scout_bld_calibration", "gcam-usa/states_subregions",
                     "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L1441.prices_bld_USA

    L1441.NEcost_75USDGJ_USA %>%
      add_title("Building Non energy cost in USA by supplysector / subsector / technology") %>%
      add_units("1975$/GJ-service") %>%
      add_comments("Detailed USA building service technology non-energy costs") %>%
      add_legacy_name("L1441.NEcost_75USDGJ_USA") %>%
      add_precursors("gcam-usa/A44.globaltech_cost") ->
      L1441.NEcost_75USDGJ_USA

    L1441.internal_gains_USA %>%
      add_title("Building Internal Gains in USA by supplysector / subsector / technology / year") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Divide by efficiency of each technology to get internal gain energy released") %>%
      add_legacy_name("L1441.internal_gains_USA") %>%
      add_precursors("gcam-usa/A44.globaltech_intgains","gcam-usa/A44.globaltech_eff") ->
      L1441.internal_gains_USA

    return_data(L1441.in_EJ_R_bld_serv_F_tech_Yh_USA, L1441.base_service_EJ_serv_fuel_tech_USA, L1441.end_use_eff_USA,
                L1441.prices_bld_USA, L1441.NEcost_75USDGJ_USA, L1441.internal_gains_USA)
  } else {
    stop("Unknown command")
  }
}
