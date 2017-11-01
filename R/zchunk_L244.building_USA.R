#' module_gcam.usa_L244.building_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.DeleteConsumer_USAbld}, \code{L244.DeleteSupplysector_USAbld}, \code{L244.SubregionalShares_gcamusa},
#' \code{L244.PriceExp_IntGains_gcamusa}, \code{L244.Floorspace_gcamusa}, \code{L244.DemandFunction_serv_gcamusa}, \code{L244.DemandFunction_flsp_gcamusa},
#' \code{L244.Satiation_flsp_gcamusa}, \code{L244.SatiationAdder_gcamusa}, \code{L244.ThermalBaseService_gcamusa}, \code{L244.GenericBaseService_gcamusa},
#' \code{L244.ThermalServiceSatiation_gcamusa}, \code{L244.GenericServiceSatiation_gcamusa}, \code{L244.Intgains_scalar_gcamusa},
#' \code{L244.ShellConductance_bld_gcamusa}, \code{L244.Supplysector_bld_gcamusa}, \code{L244.FinalEnergyKeyword_bld_gcamusa}, \code{L244.SubsectorShrwt_bld_gcamusa},
#' \code{L244.SubsectorShrwtFllt_bld_gcamusa}, \code{L244.SubsectorInterp_bld_gcamusa}, \code{L244.SubsectorInterpTo_bld_gcamusa},
#' \code{L244.SubsectorLogit_bld_gcamusa}, \code{L244.StubTech_bld_gcamusa}, \code{L244.StubTechCalInput_bld_gcamusa}, \code{L244.StubTechMarket_bld},
#' \code{L244.GlobalTechIntGainOutputRatio}, \code{L244.GlobalTechInterpTo_bld}, \code{L244.GlobalTechEff_bld},
#' \code{L244.GlobalTechShrwt_bld_gcamusa}, \code{L244.GlobalTechCost_bld_gcamusa}, \code{L244.GlobalTechSCurve_bld}, \code{L244.HDDCDD_A2_GFDL}.
#' The corresponding file in the original data system was \code{L244.building_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export

module_gcam.usa_L244.building_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.sector",
             FILE = "gcam-usa/calibrated_techs_bld_usa",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A44.bld_shell_conductance",
             FILE = "gcam-usa/A44.demandFn_flsp",
             FILE = "gcam-usa/A44.demandFn_serv",
             FILE = "gcam-usa/A44.gcam_consumer",
             FILE = "gcam-usa/A44.satiation_flsp",
             FILE = "gcam-usa/A44.sector",
             FILE = "gcam-usa/A44.subsector_interp",
             FILE = "gcam-usa/A44.subsector_logit",
             FILE = "gcam-usa/A44.subsector_shrwt",
             FILE = "gcam-usa/A44.globaltech_cost", # Units
             FILE = "gcam-usa/A44.globaltech_eff", # Units
             FILE = "gcam-usa/A44.globaltech_eff_avg", # Units
             FILE = "gcam-usa/A44.globaltech_shares",
             FILE = "gcam-usa/A44.globaltech_intgains",
             FILE = "gcam-usa/A44.globaltech_retirement",
             FILE = "gcam-usa/A44.globaltech_shrwt",
             FILE = "gcam-usa/A44.globaltech_interp",
             FILE = "gcam-usa/A44.demand_satiation_mult",
             "L144.flsp_bm2_state_res",
             "L144.flsp_bm2_state_comm",
             "L144.in_EJ_state_comm_F_U_Y",
             "L144.in_EJ_state_res_F_U_Y",
             FILE = "temp-data-inject/L143.HDDCDD_scen_state",
             "L100.Pop_thous_state",
             "L100.pcGDP_thous90usd_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.DeleteConsumer_USAbld", #
             "L244.DeleteSupplysector_USAbld", #
             "L244.SubregionalShares_gcamusa", #
             "L244.PriceExp_IntGains_gcamusa", #
             "L244.Floorspace_gcamusa", #
             "L244.DemandFunction_serv_gcamusa", #
             "L244.DemandFunction_flsp_gcamusa", #
             "L244.Satiation_flsp_gcamusa", #
             "L244.SatiationAdder_gcamusa", #
             "L244.ThermalBaseService_gcamusa",
             "L244.GenericBaseService_gcamusa",
             "L244.ThermalServiceSatiation_gcamusa",
             "L244.GenericServiceSatiation_gcamusa",
             "L244.Intgains_scalar_gcamusa",
             "L244.ShellConductance_bld_gcamusa", #
             "L244.Supplysector_bld_gcamusa", #
             "L244.FinalEnergyKeyword_bld_gcamusa", #
             "L244.SubsectorShrwt_bld_gcamusa", #
             "L244.SubsectorShrwtFllt_bld_gcamusa", #
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa", #
             "L244.StubTech_bld_gcamusa", #
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld", # units
             "L244.GlobalTechShrwt_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechSCurve_bld",
             "L244.HDDCDD_A2_GFDL")) #
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer <- get_data(all_data, "energy/A44.gcam_consumer")
    A44.sector <- get_data(all_data, "energy/A44.sector")
    calibrated_techs_bld_usa <- get_data(all_data, "gcam-usa/calibrated_techs_bld_usa")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A44.bld_shell_conductance <- get_data(all_data, "gcam-usa/A44.bld_shell_conductance")
    A44.demandFn_flsp <- get_data(all_data, "gcam-usa/A44.demandFn_flsp")
    A44.demandFn_serv <- get_data(all_data, "gcam-usa/A44.demandFn_serv")
    A44.gcam_consumer <- get_data(all_data, "gcam-usa/A44.gcam_consumer")
    A44.satiation_flsp <- get_data(all_data, "gcam-usa/A44.satiation_flsp")
    A44.sector <- get_data(all_data, "gcam-usa/A44.sector")
    A44.subsector_interp <- get_data(all_data, "gcam-usa/A44.subsector_interp")
    A44.subsector_logit <- get_data(all_data, "gcam-usa/A44.subsector_logit")
    A44.subsector_shrwt <- get_data(all_data, "gcam-usa/A44.subsector_shrwt")
    A44.globaltech_cost <- get_data(all_data, "gcam-usa/A44.globaltech_cost")
    A44.globaltech_eff <- get_data(all_data, "gcam-usa/A44.globaltech_eff") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year))
    A44.globaltech_eff_avg <- get_data(all_data, "gcam-usa/A44.globaltech_eff_avg")
    A44.globaltech_shares <- get_data(all_data, "gcam-usa/A44.globaltech_shares")
    A44.globaltech_intgains <- get_data(all_data, "gcam-usa/A44.globaltech_intgains")
    A44.globaltech_retirement <- get_data(all_data, "gcam-usa/A44.globaltech_retirement")
    A44.globaltech_shrwt <- get_data(all_data, "gcam-usa/A44.globaltech_shrwt")
    A44.globaltech_interp <- get_data(all_data, "gcam-usa/A44.globaltech_interp")
    A44.demand_satiation_mult <- get_data(all_data, "gcam-usa/A44.demand_satiation_mult")
    L144.flsp_bm2_state_res <- get_data(all_data, "L144.flsp_bm2_state_res")
    L144.flsp_bm2_state_comm <- get_data(all_data, "L144.flsp_bm2_state_comm")
    L144.in_EJ_state_comm_F_U_Y <- get_data(all_data, "L144.in_EJ_state_comm_F_U_Y")
    L144.in_EJ_state_res_F_U_Y <- get_data(all_data, "L144.in_EJ_state_res_F_U_Y")
    L143.HDDCDD_scen_state <- get_data(all_data, "temp-data-inject/L143.HDDCDD_scen_state") %>%
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L100.pcGDP_thous90usd_state <- get_data(all_data, "L100.pcGDP_thous90usd_state")
    # ===================================================
    # Need to delete the buildings sector in the USA region (gcam.consumers and supplysectors)
    L244.DeleteConsumer_USAbld <- tibble(region = "USA", gcam.consumer = A44.gcam_consumer$gcam.consumer)
    L244.DeleteSupplysector_USAbld <- tibble(region = "USA", supplysector = A44.sector$supplysector)

    # L244.SubregionalShares_gcamusa: subregional population and income shares (not currently used)
    L244.SubregionalShares_gcamusa <- write_to_all_states(A44.gcam_consumer, c("region", "gcam.consumer")) %>%
      mutate(pop.year.fillout = min(BASE_YEARS),
             inc.year.fillout = min(BASE_YEARS),
             subregional.population.share = 1,
             subregional.income.share = 1)

    # L244.PriceExp_IntGains_gcamusa: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains_gcamusa <- write_to_all_states(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]])

    # L244.Floorspace_gcamusa: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on

    # Residential floorspace
    L244.Floorspace_resid <- L144.flsp_bm2_state_res %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

    #Commercial floorspace
    L244.Floorspace_comm <- L144.flsp_bm2_state_comm %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

    L244.Floorspace_full <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm)

    # Final output only has base years
    L244.Floorspace_gcamusa <- filter(L244.Floorspace_full, year %in% BASE_YEARS)

    # L244.DemandFunction_serv_gcamusa and L244.DemandFunction_flsp_gcamusa: demand function types
    L244.DemandFunction_serv_gcamusa <- write_to_all_states(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]])
    L244.DemandFunction_flsp_gcamusa <- write_to_all_states(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]])

    # L244.Satiation_flsp_gcamusa: Satiation levels assumed for floorspace
    L244.Satiation_flsp_gcamusa <- A44.satiation_flsp %>%
      gather(gcam.consumer, value, resid, comm) %>%
      rename(region = state) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(L244.Floorspace_gcamusa %>%
                                 filter(year == max(BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(L100.Pop_thous_state %>% rename(pop = value), by = c("region" = "state", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = maximum of exogenous assumption and the observed value in the final calibration year
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # L244.SatiationAdder_gcamusa: Satiation adders in floorspace demand function - Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)
    L244.SatiationAdder_gcamusa <- L244.Satiation_flsp_gcamusa %>%
      # Add per capita GDP
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>%
                                 filter(year == energy.SATIATION_YEAR), by = c("region" = "state")) %>%
      rename(pcGDP = value) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_full, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      # Add population
      left_join_error_no_match(L100.Pop_thous_state, by = c("region" = "state", "year")) %>%
      rename(pop = value) %>%
      # Calculate per capita floorspace
      mutate(pcFlsp_mm2 = base.building.size / pop,
             # Calculate the satiation adders
             satiation.adder = round(satiation.level - (
               exp(log(2) * pcGDP / energy.GDP_MID_SATIATION) * (satiation.level - pcFlsp_mm2)),
               energy.DIGITS_SATIATION_ADDER),
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year )
      satiation.adder = if_else(satiation.adder > pcFlsp_mm2, pcFlsp_mm2 * 0.999, satiation.adder)) %>%
        select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- setdiff(unique(A44.sector$supplysector), generic_services)

    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.HDDCDD_scen_state <- L143.HDDCDD_scen_state %>%
      rename(region = state,
             degree.days = value)

    # Let's make a climate normal (historical average) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    L244.HDDCDD_normal_state <- L244.HDDCDD_scen_state %>%
      filter(year %in% seq(1981, 2000)) %>%
      group_by(region, variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup()

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    L244.HDDCDD_A2_GFDL <- tidyr::crossing(region = gcamusa.STATES, thermal.building.service.input = thermal_services) %>%
      # Add in gcam.consumer
      left_join_error_no_match(calibrated_techs_bld_usa %>%
                                 select(service, gcam.consumer = sector) %>%
                                 distinct(), by = c("thermal.building.service.input" = "service")) %>%
      # Add in nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], thermal.building.service.input) %>%
      # Add in model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add HDD/CDD so that we can join with L244.HDDCDD_scen_state, remove at end
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add in degree days
      left_join_error_no_match(L244.HDDCDD_scen_state, by = c("region", "variable", "year")) %>%
      mutate(degree.days = round(degree.days, energy.DIGITS_HDDCDD)) %>%
      # Don't need to keep Scen and GCM identifiers because only one is used
      select(-Scen, -GCM, -variable)

    # L244.ShellConductance_bld_gcamusa: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld_gcamusa <- A44.bld_shell_conductance %>%
      # Convert to long form
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year),
             value = round(value, energy.DIGITS_EFFICIENCY)) %>%
      # Repeat for all states
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet

    # L244.Supplysector_bld: Supplysector info for buildings
    L244.Supplysector_bld_gcamusa <- write_to_all_states(A44.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    L244.FinalEnergyKeyword_bld_gcamusa <- write_to_all_states(A44.sector, LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]])

    # L244.SubsectorLogit_bld: Subsector logit exponents of building sector
    L244.SubsectorLogit_bld_gcamusa <- write_to_all_states(A44.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"))

    # L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(A44.subsector_shrwt$year))){
      L244.SubsectorShrwt_bld_gcamusa <- write_to_all_states(A44.subsector_shrwt %>%
                                                       filter(!is.na(year)), LEVEL2_DATA_NAMES[["SubsectorShrwt"]])
    }
    if(any(!is.na(A44.subsector_shrwt$year.fillout))){
      L244.SubsectorShrwtFllt_bld_gcamusa <- write_to_all_states(A44.subsector_shrwt %>%
                                                           filter(!is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])
    }

    # L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector
    if(any(is.na(A44.subsector_interp$to.value))){
      L244.SubsectorInterp_bld_gcamusa <- write_to_all_states(A44.subsector_interp %>%
                                                        filter(is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterp"]])
    }
    if(any(!is.na(A44.subsector_interp$to.value))){
      L244.SubsectorInterpTo_bld_gcamusa <- write_to_all_states(A44.subsector_interp %>%
                                                          filter(!is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])
    }

    # L244.StubTech_bld_gcamusa: Identification of stub technologies for buildings
    L244.StubTech_bld_gcamusa <- A44.globaltech_eff %>%
      select(supplysector, subsector, technology) %>%
      distinct() %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      rename(stub.technology = technology)

    # L244.GlobalTechEff_bld: Assumed efficiencies (all years) of buildings technologies
    L244.end_use_eff <- A44.globaltech_eff %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

     L244.GlobalTechEff_bld <- L244.end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # L244.StubTechMarket_bld: Specify market names for fuel inputs to all technologies in each state
    L244.StubTechMarket_bld <- L244.end_use_eff %>%
      mutate(market.name = "USA") %>%
      rename(stub.technology = technology) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      # Electricity is consumed from state markets, so change market.name to states for electricity
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS, region, market.name))

    # If true, then we change market.name for selected fuels to state markets, rather than USA
    if(gcamusa.USE_REGIONAL_FUEL_MARKETS){
      L244.StubTechMarket_bld <- L244.StubTechMarket_bld %>%
        left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                     grid_region, market.name)) %>%
        select(LEVEL2_DATA_NAMES[["StubTechMarket"]])
    }

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    # Combine residential and commercial energy data
    L244.in_EJ_R_bld_serv_F_Yh <- bind_rows(L144.in_EJ_state_res_F_U_Y, L144.in_EJ_state_comm_F_U_Y) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(supplysector = service) %>%
      # Add subsector and energy.input
      left_join_error_no_match(calibrated_techs_bld_usa %>%
                                 select(sector, supplysector, fuel, subsector, minicam.energy.input) %>%
                                 distinct(), by = c("sector", "supplysector", "fuel"))

    # Shares allocated to partitioned technologies need to be computed first")
    L244.globaltech_eff_prt <- A44.globaltech_eff %>%
      semi_join(A44.globaltech_eff_avg, by = c("supplysector", "subsector")) %>%
      filter(year == gcamusa.EFFICIENCY_PARTITION_YEAR) %>%
      rename(efficiency = value)

    A44.globaltech_eff_avg$efficiency_tech1 <- L244.globaltech_eff_prt$efficiency[
      match( vecpaste( A44.globaltech_eff_avg[ c( supp, subs, "technology1" ) ] ),
             vecpaste( L244.globaltech_eff_prt[ c( s_s_t ) ] ) ) ]
    A44.globaltech_eff_avg$efficiency_tech2 <- L244.globaltech_eff_prt$efficiency[
      match( vecpaste( A44.globaltech_eff_avg[ c( supp, subs, "technology2" ) ] ),
             vecpaste( L244.globaltech_eff_prt[ c( s_s_t ) ] ) ) ]
    A44.globaltech_eff_avg$share_tech1 <- with( A44.globaltech_eff_avg, ( stockavg - efficiency_tech2 ) / ( efficiency_tech1 - efficiency_tech2 ) )
    A44.globaltech_eff_avg$share_tech2 <- 1 - A44.globaltech_eff_avg$share_tech1

    #These shares can now be binded into the table of technology shares, where shares are not computed from stock avg efficiencies
    L244.globaltech_shares <- rbind( A44.globaltech_eff_avg[ names( A44.globaltech_shares ) ], A44.globaltech_shares )

    #For calibration table, start with global tech efficiency table, repeat by states, and match in tech shares.
    L244.StubTechCalInput_bld <- repeat_and_add_vector( L244.GlobalTechEff_bld[ L244.GlobalTechEff_bld$year %in% model_base_years, names_GlobalTechInput ], "region", states_subregions$state )
    L244.StubTechCalInput_bld[ c( supp, subs, "stub.technology" ) ] <- L244.StubTechCalInput_bld[ c( "sector.name", "subsector.name", "technology" ) ]
    L244.StubTechCalInput_bld$share <- 1
    L244.StubTechCalInput_bld$share[
      vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ) ] <-
      L244.globaltech_shares$share_tech1[
        match( vecpaste( L244.StubTechCalInput_bld[ vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ), s_s_t ] ),
               vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ) ) ]
    L244.StubTechCalInput_bld$share[
      vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ) ] <-
      L244.globaltech_shares$share_tech2[
        match( vecpaste( L244.StubTechCalInput_bld[ vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ), s_s_t ] ),
               vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ) ) ]

    #At this point, the energy by state/service/fuel is ready to be matched in, multiplied by the shares allocated to each technology
    L244.StubTechCalInput_bld$calibrated.value <- round(
      L244.StubTechCalInput_bld$share *
        L244.in_EJ_R_bld_serv_F_Yh$calibrated.value[
          match( vecpaste( L244.StubTechCalInput_bld[ c( "region", supp, subs, input, Y ) ] ),
                 vecpaste( L244.in_EJ_R_bld_serv_F_Yh[ c( "state", supp, subs, input, Y ) ] ) ) ],
      digits_calOutput )

    L244.StubTechCalInput_bld$share.weight.year <- L244.StubTechCalInput_bld$year
    L244.StubTechCalInput_bld <- set_subsector_shrwt( L244.StubTechCalInput_bld, value.name = "calibrated.value" )
    L244.StubTechCalInput_bld$tech.share.weight <- ifelse( L244.StubTechCalInput_bld$calibrated.value > 0, 1, 0 )
    L244.StubTechCalInput_bld <- L244.StubTechCalInput_bld[ names_StubTechCalInput ]


    # ===================================================
    # Produce outputs
    L244.DeleteConsumer_USAbld %>%
      add_title("Deletes building sector in USA region to rewrite with GCAM-USA data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.gcam_consumer") %>%
      add_legacy_name("L244.DeleteConsumer_USAbld") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.DeleteConsumer_USAbld

    L244.DeleteSupplysector_USAbld %>%
      add_title("Deletes building sector in USA region to rewrite with GCAM-USA data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.sector") %>%
      add_legacy_name("L244.DeleteSupplysector_USAbld") %>%
      add_precursors("energy/A44.sector") ->
      L244.DeleteSupplysector_USAbld

    L244.SubregionalShares_gcamusa %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.SubregionalShares_gcamusa

    L244.PriceExp_IntGains_gcamusa %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all states") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.PriceExp_IntGains_gcamusa

    L244.Floorspace_gcamusa %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_state_res and L144.flsp_bm2_state_comm") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "energy/A44.gcam_consumer") ->
      L244.Floorspace_gcamusa

    L244.DemandFunction_serv_gcamusa %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_serv written to all states") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("gcam-usa/A44.demandFn_serv") ->
      L244.DemandFunction_serv_gcamusa

    L244.DemandFunction_flsp_gcamusa %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_flsp written to all states") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("gcam-usa/A44.demandFn_flsp") ->
      L244.DemandFunction_flsp_gcamusa

    L244.Satiation_flsp_gcamusa %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from A44.satiation_flsp or L244.Floorspace_gcamusa/L100.Pop_thous_state") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm") ->
      L244.Satiation_flsp_gcamusa

    L244.SatiationAdder_gcamusa %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation leve, per capita floorspace, and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "L100.pcGDP_thous90usd_state") ->
      L244.SatiationAdder_gcamusa

    L244.HDDCDD_A2_GFDL %>%
      add_title("Heating and Cooling Degree Days by State for GFDL A2") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_state assigned to GCAM subsectors") %>%
      add_legacy_name("L244.HDDCDD_A2_GFDL") %>%
      add_precursors("temp-data-inject/L143.HDDCDD_scen_state", "gcam-usa/A44.sector",
                     "gcam-usa/calibrated_techs_bld_usa", "gcam-usa/A44.gcam_consumer") ->
      L244.HDDCDD_A2_GFDL

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.ThermalBaseService_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GenericBaseService_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.ThermalServiceSatiation_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GenericServiceSatiation_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.Intgains_scalar_gcamusa

    L244.ShellConductance_bld_gcamusa %>%
      add_title("Shell conductance (inverse of shell efficiency) by state") %>%
      add_units("Unitless") %>%
      add_comments("values from A44.bld_shell_conductance") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("gcam-usa/A44.bld_shell_conductance", "gcam-usa/A44.gcam_consumer") ->
      L244.ShellConductance_bld_gcamusa

    L244.Supplysector_bld_gcamusa %>%
      add_title("Supplysector info for buildings") %>%
      add_units("Unitless") %>%
      add_comments("A44.sector written to all states") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("gcam-usa/A44.sector") ->
      L244.Supplysector_bld_gcamusa

    L244.FinalEnergyKeyword_bld_gcamusa %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("A44.sector written to all states") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-usa/A44.sector") ->
      L244.FinalEnergyKeyword_bld_gcamusa

    if(exists("L244.SubsectorShrwt_bld")){
      L244.SubsectorShrwt_bld_gcamusa %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("gcam-usa/A44.subsector_shrwt") ->
        L244.SubsectorShrwt_bld_gcamusa
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.SubsectorShrwt_bld_gcamusa
    }

    if(exists("L244.SubsectorShrwtFllt_bld_gcamusa")){
      L244.SubsectorShrwtFllt_bld_gcamusa %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-usa/A44.subsector_shrwt") ->
        L244.SubsectorShrwtFllt_bld_gcamusa
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.SubsectorShrwtFllt_bld_gcamusa
    }


    if(exists("L244.SubsectorInterp_bld_gcamusa")){
      L244.SubsectorInterp_bld_gcamusa %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("gcam-usa/A44.subsector_interp") ->
        L244.SubsectorInterp_bld_gcamusa
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.SubsectorInterp_bld_gcamusa
    }

    if(exists("L244.SubsectorInterpTo_bld_gcamusa")){
      L244.SubsectorInterpTo_bld_gcamusa %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-usa/A44.subsector_interp") ->
        L244.SubsectorInterpTo_bld_gcamusa
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") ->
        L244.SubsectorInterpTo_bld_gcamusa
    }

    L244.SubsectorLogit_bld_gcamusa %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("A44.subsector_logit written to all states") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("gcam-usa/A44.subsector_logit") ->
      L244.SubsectorLogit_bld_gcamusa

    L244.StubTech_bld_gcamusa %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("A44.globaltech_eff written to all states") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_eff") ->
      L244.StubTech_bld_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.StubTechCalInput_bld_gcamusa

    L244.StubTechMarket_bld %>%
      add_title("market names for fuel inputs to all technologies in each state") %>%
      add_units("NA") %>%
      add_comments("Categories from A44.globaltech_eff written to all states") %>%
      add_comments("Market set to states for electricity") %>%
      add_legacy_name("L244.StubTechMarket_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.StubTechMarket_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechIntGainOutputRatio") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechIntGainOutputRatio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechInterpTo_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechInterpTo_bld

    L244.GlobalTechEff_bld %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Look up") %>%
      add_comments("Values from A44.globaltech_eff") %>%
      add_legacy_name("L244.GlobalTechEff_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_eff") ->
      L244.GlobalTechEff_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechShrwt_bld_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechCost_bld_gcamusa

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechSCurve_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechSCurve_bld

    return_data(L244.DeleteConsumer_USAbld, L244.DeleteSupplysector_USAbld, L244.SubregionalShares_gcamusa,
                L244.PriceExp_IntGains_gcamusa, L244.Floorspace_gcamusa, L244.DemandFunction_serv_gcamusa, L244.DemandFunction_flsp_gcamusa,
                L244.Satiation_flsp_gcamusa, L244.SatiationAdder_gcamusa, L244.ThermalBaseService_gcamusa, L244.GenericBaseService_gcamusa,
                L244.ThermalServiceSatiation_gcamusa, L244.GenericServiceSatiation_gcamusa, L244.Intgains_scalar_gcamusa, L244.ShellConductance_bld_gcamusa,
                L244.Supplysector_bld_gcamusa, L244.FinalEnergyKeyword_bld_gcamusa, L244.SubsectorShrwt_bld_gcamusa, L244.SubsectorShrwtFllt_bld_gcamusa,
                L244.SubsectorInterp_bld_gcamusa, L244.SubsectorInterpTo_bld_gcamusa, L244.SubsectorLogit_bld_gcamusa, L244.StubTech_bld_gcamusa,
                L244.StubTechCalInput_bld_gcamusa, L244.StubTechMarket_bld, L244.GlobalTechIntGainOutputRatio,
                L244.GlobalTechInterpTo_bld, L244.GlobalTechEff_bld, L244.GlobalTechShrwt_bld_gcamusa, L244.GlobalTechCost_bld_gcamusa,
                L244.GlobalTechSCurve_bld, L244.HDDCDD_A2_GFDL)
  } else {
    stop("Unknown command")
  }
}
