#' module_gcam.usa_L244.building_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.DeleteConsumer_USAbld}, \code{L244.DeleteSupplysector_USAbld}, \code{L244.SubregionalShares}, \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv}, \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService}, \code{object}, \code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld}, \code{L244.SectorLogitTables_bld[[ curr_table ]]$data}, \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld}, \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogitTables_bld[[ curr_table ]]$data}, \code{L244.SubsectorLogit_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechCalInput_bld}, \code{L244.StubTechMarket_bld}, \code{L244.GlobalTechIntGainOutputRatio}, \code{L244.GlobalTechInterpTo_bld}, \code{L244.GlobalTechEff_bld}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld}, \code{L244.GlobalTechSCurve_bld}. The corresponding file in the
#' original data system was \code{L244.building_USA.R} (gcam-usa level2).
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
             "L244.SubregionalShares", #
             "L244.PriceExp_IntGains", #
             "L244.Floorspace", #
             "L244.DemandFunction_serv", #
             "L244.DemandFunction_flsp", #
             "L244.Satiation_flsp", #
             "L244.SatiationAdder", #
             "L244.ThermalBaseService",
             "L244.GenericBaseService",
             "L244.ThermalServiceSatiation",
             "L244.GenericServiceSatiation",
             "L244.Intgains_scalar",
             "L244.ShellConductance_bld",
             "L244.Supplysector_bld", # curr_table
             "L244.FinalEnergyKeyword_bld",
             "L244.SubsectorShrwt_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorInterpTo_bld",
             "L244.SubsectorLogit_bld", # curr_table
             "L244.StubTech_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.GlobalTechSCurve_bld"))
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
    A44.globaltech_eff <- get_data(all_data, "gcam-usa/A44.globaltech_eff")
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
    L143.HDDCDD_scen_state <- get_data(all_data, "temp-data-inject/L143.HDDCDD_scen_state")
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L100.pcGDP_thous90usd_state <- get_data(all_data, "L100.pcGDP_thous90usd_state")
    # ===================================================
    # Need to delete the buildings sector in the USA region (gcam.consumers and supplysectors)
    L244.DeleteConsumer_USAbld <- tibble(region = "USA", gcam.consumer = A44.gcam_consumer$gcam.consumer)
    L244.DeleteSupplysector_USAbld <- tibble(region = "USA", supplysector = A44.sector$supplysector)

    # L244.SubregionalShares: subregional population and income shares (not currently used)
    L244.SubregionalShares <- write_to_all_states(A44.gcam_consumer, c("region", "gcam.consumer")) %>%
      mutate(pop.year.fillout = min(BASE_YEARS),
             inc.year.fillout = min(BASE_YEARS),
             subregional.population.share = 1,
             subregional.income.share = 1)

    # L244.PriceExp_IntGains: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains <- write_to_all_states(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]])

    # L244.Floorspace: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on

    # Residential floorspace
    L244.Floorspace_resid <- L144.flsp_bm2_state_res %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, gcamusa.DIGITS_FLOORSPACE)) %>%
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
    L244.Floorspace <- filter(L244.Floorspace_full, year %in% BASE_YEARS)

    # L244.DemandFunction_serv and L244.DemandFunction_flsp: demand function types
    L244.DemandFunction_serv <- write_to_all_states(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]])
    L244.DemandFunction_flsp <- write_to_all_states(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]])

    # L244.Satiation_flsp: Satiation levels assumed for floorspace
    L244.Satiation_flsp <- A44.satiation_flsp %>%
      gather(gcam.consumer, value, resid, comm) %>%
      rename(region = state) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(L244.Floorspace %>%
                                 filter(year == max(BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(L100.Pop_thous_state %>% rename(pop = value), by = c("region" = "state", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = maximum of exogenous assumption and the observed value in the final calibration year
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # L244.SatiationAdder: Satiation adders in floorspace demand function - Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)
    L244.SatiationAdder <- L244.Satiation_flsp %>%
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
    L244.all_scen_gcm <- L143.HDDCDD_scen_state %>%
      select


    L244.all_scen_gcm <- unique( L143.HDDCDD_scen_state [c ("Scen", "GCM") ] )
    L244.all_scen_gcm$scenID <- 1:nrow( L244.all_scen_gcm )
    L244.HDDCDD_scen_state <- L143.HDDCDD_scen_state
    L244.HDDCDD_scen_state$scenID <- L244.all_scen_gcm$scenID[
      match( vecpaste( L244.HDDCDD_scen_state[ c( "Scen", "GCM" ) ] ),
             vecpaste( L244.all_scen_gcm[ c( "Scen", "GCM" ) ] ) ) ]

    L244.HDDCDD_scen_state.melt <- melt( L244.HDDCDD_scen_state, id.vars = c( "state","scenID", "Scen", "GCM", "variable" ), variable.name = "Xyear")
    L244.HDDCDD_scen_state.melt$year <- sub( "X", "", L244.HDDCDD_scen_state.melt$Xyear )
    L244.HDDCDD_scen_state.melt$region <- L244.HDDCDD_scen_state.melt$state

    #Let's make a climate normal for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    # Probably want this to be up to 2000, since in SRES scenarios 2001 is a future year
    L244.HDDCDD_normal_state <- subset( L244.HDDCDD_scen_state.melt, scenID == 1 & year %in% climate_normal_years )
    L244.HDDCDD_normal_state <- aggregate( L244.HDDCDD_normal_state[ "value" ],
                                           by = as.list( L244.HDDCDD_normal_state[ c( "region", "variable" ) ] ), mean )

    #Subset the heating and cooling services, separately
    heating_services <- thermal_services[ grepl( "heating", thermal_services ) ]
    cooling_services <- thermal_services[ grepl( "cooling", thermal_services ) ]
    L244.HDDCDD <- data.frame(
      region = rep( states_subregions$state, times = length( thermal_services ) ),
      thermal.building.service.input = sort( rep( thermal_services, times = nrow( states_subregions ) ) ) )
    L244.HDDCDD$gcam.consumer <- calibrated_techs_bld_usa$sector[
      match( L244.HDDCDD$thermal.building.service.input, calibrated_techs_bld_usa$service ) ]
    L244.HDDCDD[ bld_nodes_noregion ] <- A44.gcam_consumer[
      match( L244.HDDCDD$gcam.consumer, A44.gcam_consumer$gcam.consumer ),
      bld_nodes_noregion ]
    L244.HDDCDD <- L244.HDDCDD[ c( names_BldNodes, "thermal.building.service.input" ) ]
    L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, Y, model_years )
    L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, "scenID", unique( L244.HDDCDD_scen_state$scenID ) )
    L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% heating_services ] <- "HDD"
    L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% cooling_services ] <- "CDD"
    L244.HDDCDD$degree.days <- round( L244.HDDCDD_scen_state.melt$value[
      match( vecpaste( L244.HDDCDD[ c( "scenID", "region", "variable", "year" ) ] ),
             vecpaste( L244.HDDCDD_scen_state.melt[ c( "scenID", "region", "variable", "year" ) ] ) ) ],
      digits_hddcdd )
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

    L244.SubregionalShares %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.SubregionalShares

    L244.PriceExp_IntGains %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all states") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.PriceExp_IntGains

    L244.Floorspace %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_state_res and L144.flsp_bm2_state_comm") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "energy/A44.gcam_consumer") ->
      L244.Floorspace

    L244.DemandFunction_serv %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_serv written to all states") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("gcam-usa/A44.demandFn_serv") ->
      L244.DemandFunction_serv

    L244.DemandFunction_flsp %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_flsp written to all states") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("gcam-usa/A44.demandFn_flsp") ->
      L244.DemandFunction_flsp

    L244.Satiation_flsp %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from A44.satiation_flsp or L244.Floorspace/L100.Pop_thous_state") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm") ->
      L244.Satiation_flsp

    L244.SatiationAdder %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation leve, per capita floorspace, and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "L100.pcGDP_thous90usd_state") ->
      L244.SatiationAdder

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.ThermalBaseService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GenericBaseService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.ThermalServiceSatiation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GenericServiceSatiation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.Intgains_scalar

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.ShellConductance_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.Supplysector_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.FinalEnergyKeyword_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorShrwt_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.SubsectorShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.SubsectorShrwtFllt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorInterp_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.SubsectorInterp_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorInterpTo_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.SubsectorInterpTo_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.SubsectorLogit_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.StubTech_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.StubTechCalInput_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
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

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechEff_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechEff_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechCost_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechSCurve_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.GlobalTechSCurve_bld

    return_data(L244.DeleteConsumer_USAbld, L244.DeleteSupplysector_USAbld, L244.SubregionalShares,
                L244.PriceExp_IntGains, L244.Floorspace, L244.DemandFunction_serv, L244.DemandFunction_flsp,
                L244.Satiation_flsp, L244.SatiationAdder, L244.ThermalBaseService, L244.GenericBaseService,
                L244.ThermalServiceSatiation, L244.GenericServiceSatiation, L244.Intgains_scalar, L244.ShellConductance_bld,
                L244.Supplysector_bld, L244.FinalEnergyKeyword_bld, L244.SubsectorShrwt_bld, L244.SubsectorShrwtFllt_bld,
                L244.SubsectorInterp_bld, L244.SubsectorInterpTo_bld, L244.SubsectorLogit_bld, L244.StubTech_bld,
                L244.StubTechCalInput_bld, L244.StubTechMarket_bld, L244.GlobalTechIntGainOutputRatio,
                L244.GlobalTechInterpTo_bld, L244.GlobalTechEff_bld, L244.GlobalTechShrwt_bld, L244.GlobalTechCost_bld,
                L244.GlobalTechSCurve_bld)
  } else {
    stop("Unknown command")
  }
}
