# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L244.building
#'
#' Creates GCAM-USA building output files for writing to xml.
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
#' \code{L244.GlobalTechShrwt_bld_gcamusa}, \code{L244.GlobalTechCost_bld_gcamusa}, \code{L244.GlobalTechSCurve_bld}, \code{L244.HDDCDD_A2_GFDL_USA},
#' \code{L244.HDDCDD_AEO_2015_USA}, \code{L244.HDDCDD_constdds_USA}, \code{L244.GompFnParam_gcamusa}, \code{L244.Satiation_impedance_gcamusa},
#' \code{L244.GenericServiceImpedance_gcamusa},\code{L244.GenericServiceCoef_gcamusa},\code{L244.GenericServiceAdder_gcamusa},
#' \code{L244.ThermalServiceImpedance_gcamusa},\code{L244.ThermalServiceCoef_gcamusa},\code{L244.ThermalServiceAdder_gcamusa},
#' \code{L210.DeleteRsrcTradBio_gcamusa}, \code{L244.GenericServicePrice_gcamusa}, \code{L244.ThermalServicePrice_gcamusa},
#' \code{L244.GenericBaseDens_gcamusa}, \code{L244.ThermalBaseDens_gcamusa},
#' The corresponding file in the original data system was \code{L244.building_USA.R} (gcam-usa level2).
#' @details Creates GCAM-USA building output files for writing to xml.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting replace_na
#' @author RLH November 2017

module_gcamusa_L244.building <- function(command, ...) {
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
             FILE = "gcam-usa/A44.globaltech_cost",
             FILE = "gcam-usa/A44.globaltech_eff",
             FILE = "gcam-usa/A44.globaltech_eff_avg",
             FILE = "gcam-usa/A44.globaltech_shares",
             FILE = "gcam-usa/A44.globaltech_intgains",
             FILE = "gcam-usa/A44.globaltech_retirement",
             FILE = "gcam-usa/A44.globaltech_shrwt",
             FILE = "gcam-usa/A44.globaltech_interp",
             FILE = "gcam-usa/A44.demand_satiation_mult",
             FILE = "gcam-usa/A44.hab_land_flsp_usa",
             FILE = "socioeconomics/income_shares",
             FILE = "gcam-usa/A44.CalPrice_service_gcamusa",
             "L244.Supplysector_bld",
             "L144.flsp_param",
             "L144.flsp_bm2_state_res",
             "L144.flsp_bm2_state_comm",
             "L144.in_EJ_state_comm_F_U_Y",
             "L144.in_EJ_state_res_F_U_Y",
             "L143.HDDCDD_scen_state",
             "L100.Pop_thous_state",
             "L100.pcGDP_thous90usd_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.DeleteConsumer_USAbld",
             "L244.DeleteSupplysector_USAbld",
             "L244.SubregionalShares_gcamusa",
             "L244.PriceExp_IntGains_gcamusa",
             "L244.Floorspace_gcamusa",
             "L244.DemandFunction_serv_gcamusa",
             "L244.DemandFunction_flsp_gcamusa",
             "L244.Satiation_flsp_gcamusa",
             "L244.SatiationAdder_gcamusa",
             "L244.ThermalBaseService_gcamusa",
             "L244.GenericBaseService_gcamusa",
             "L244.ThermalServiceSatiation_gcamusa",
             "L244.GenericServiceSatiation_gcamusa",
             "L244.Intgains_scalar_gcamusa",
             "L244.ShellConductance_bld_gcamusa",
             "L244.Supplysector_bld_gcamusa",
             "L244.FinalEnergyKeyword_bld_gcamusa",
             "L244.SubsectorShrwt_bld_gcamusa",
             "L244.SubsectorShrwtFllt_bld_gcamusa",
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.StubTech_bld_gcamusa",
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechSCurve_bld",
             "L244.HDDCDD_A2_GFDL_USA",
             "L244.HDDCDD_constdds_USA",
             "L244.HDDCDD_AEO_2015_USA",
             "L244.GompFnParam_gcamusa",
             "L244.Satiation_impedance_gcamusa",
             "L244.GenericServiceImpedance_gcamusa",
             "L244.GenericServiceCoef_gcamusa",
             "L244.GenericServiceAdder_gcamusa",
             "L244.ThermalServiceImpedance_gcamusa",
             "L244.ThermalServiceCoef_gcamusa",
             "L244.ThermalServiceAdder_gcamusa",
             "L210.DeleteRsrcTradBio_gcamusa",
             "L244.GenericServicePrice_gcamusa",
             "L244.ThermalServicePrice_gcamusa",
             "L244.GenericBaseDens_gcamusa",
             "L244.ThermalBaseDens_gcamusa"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    GCM <- Scen <- base.building.size <- base.service <- calibrated.value <- comm <-
      degree.days <- efficiency <- efficiency_tech1 <- efficiency_tech2 <- fuel <-
      gcam.consumer <- grid_region <- half_life_new <- half_life_stock <- input.cost <-
      input.ratio <- internal.gains.market.name <- internal.gains.output.ratio <-
      internal.gains.scalar <- market.name <- minicam.energy.input <- multiplier <-
      object <- pcFlsp_mm2 <- pcGDP <- pcflsp_mm2cap <- pop <- region <- resid <-
      satiation.adder <- satiation.level <- sector <- sector.name <- service <- share <-
      share.weight <- share_tech1 <- share_tech2 <- share_type <- state <- steepness_new <-
      steepness_stock <- stockavg <- subsector <- subsector.name <- supplysector <-
      tech_type <- technology <- technology1 <- technology2 <-
      thermal.building.service.input <- to.value <- value <- year <- year.fillout <- . <-
      pop_year <- Sector <- pop_share <- growth <- flsp_growth <- area_gcam <- misc_land_usda <-
      area_thouskm2 <- flsp <- pop_thous <- flsp_pc <- tot.dens <- unadjust.satiation <-
      land.density.param <- b.param <- income.param <- gdp_pc <- flsp_est <- base_flsp <-
      bias.adjust.param <- state_name <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer_en <- get_data(all_data, "energy/A44.gcam_consumer", strip_attributes = TRUE)
    A44.sector_en <- get_data(all_data, "energy/A44.sector", strip_attributes = TRUE)
    calibrated_techs_bld_usa <- get_data(all_data, "gcam-usa/calibrated_techs_bld_usa", strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    A44.bld_shell_conductance <- get_data(all_data, "gcam-usa/A44.bld_shell_conductance", strip_attributes = TRUE)
    A44.demandFn_flsp <- get_data(all_data, "gcam-usa/A44.demandFn_flsp", strip_attributes = TRUE)
    A44.demandFn_serv <- get_data(all_data, "gcam-usa/A44.demandFn_serv", strip_attributes = TRUE)
    A44.gcam_consumer <- get_data(all_data, "gcam-usa/A44.gcam_consumer", strip_attributes = TRUE)
    A44.satiation_flsp <- get_data(all_data, "gcam-usa/A44.satiation_flsp", strip_attributes = TRUE)
    A44.sector <- get_data(all_data, "gcam-usa/A44.sector", strip_attributes = TRUE)
    A44.subsector_interp <- get_data(all_data, "gcam-usa/A44.subsector_interp", strip_attributes = TRUE)
    A44.subsector_logit <- get_data(all_data, "gcam-usa/A44.subsector_logit", strip_attributes = TRUE)
    A44.subsector_shrwt <- get_data(all_data, "gcam-usa/A44.subsector_shrwt", strip_attributes = TRUE)
    A44.globaltech_cost <- get_data(all_data, "gcam-usa/A44.globaltech_cost", strip_attributes = TRUE)
    A44.globaltech_eff <- get_data(all_data, "gcam-usa/A44.globaltech_eff", strip_attributes = TRUE) %>%
      gather_years()
    A44.globaltech_eff_avg <- get_data(all_data, "gcam-usa/A44.globaltech_eff_avg", strip_attributes = TRUE)
    A44.globaltech_shares <- get_data(all_data, "gcam-usa/A44.globaltech_shares", strip_attributes = TRUE)
    A44.globaltech_intgains <- get_data(all_data, "gcam-usa/A44.globaltech_intgains", strip_attributes = TRUE)
    A44.globaltech_retirement <- get_data(all_data, "gcam-usa/A44.globaltech_retirement", strip_attributes = TRUE)
    A44.globaltech_shrwt <- get_data(all_data, "gcam-usa/A44.globaltech_shrwt", strip_attributes = TRUE)
    A44.globaltech_interp <- get_data(all_data, "gcam-usa/A44.globaltech_interp", strip_attributes = TRUE)
    A44.demand_satiation_mult <- get_data(all_data, "gcam-usa/A44.demand_satiation_mult", strip_attributes = TRUE)
    L144.flsp_bm2_state_res <- get_data(all_data, "L144.flsp_bm2_state_res", strip_attributes = TRUE)
    L144.flsp_bm2_state_comm <- get_data(all_data, "L144.flsp_bm2_state_comm", strip_attributes = TRUE)
    L144.in_EJ_state_comm_F_U_Y <- get_data(all_data, "L144.in_EJ_state_comm_F_U_Y", strip_attributes = TRUE)
    L144.in_EJ_state_res_F_U_Y <- get_data(all_data, "L144.in_EJ_state_res_F_U_Y", strip_attributes = TRUE)
    L143.HDDCDD_scen_state <- get_data(all_data, "L143.HDDCDD_scen_state", strip_attributes = TRUE)
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state", strip_attributes = TRUE)
    L100.pcGDP_thous90usd_state <- get_data(all_data, "L100.pcGDP_thous90usd_state", strip_attributes = TRUE)
    L144.hab_land_flsp_usa<- get_data(all_data, "gcam-usa/A44.hab_land_flsp_usa", strip_attributes = TRUE)
    L144.flsp_param <- get_data(all_data, "L144.flsp_param", strip_attributes = TRUE)
    L144.prices_bld_gcamusa <- get_data(all_data, "gcam-usa/A44.CalPrice_service_gcamusa", strip_attributes = TRUE) %>%
      gather_years()
    income_shares<-get_data(all_data, "socioeconomics/income_shares")
    L244.Supplysector_bld<-get_data(all_data, "L244.Supplysector_bld") %>% filter(region == gcam.USA_REGION)

    # Add a deflator for harmonizing GDPpc with prices
    def9075<-gdp_deflator(1990, 1975)

    # ===================================================
    # Data Processing

    # Note: Building energy demands and floorspace are calculated endogenously - these are undergoing review
    # per-capita demand = (satiation.level - satiation.adder) * (1 - exp( -log2 / satiation.impedance * Demand.Driver)) + satiation.adder)
    #
    # floorspace = (satiation.level - satiation.adder) *
    # [1 - exp{(-ln(2) * per-capita-GDP/satiation.impedance) * (energy_cost/base_energy_cost)^price_effect_exponent}] + satiation.adder
    #
    # satiation.level: maximum per-capita demand that can be achieved
    # satiation.adder: value that allow the starting position of any region to be set along the demand function
    # satiation.impedance: shape parameter


    # Need to delete the buildings sector in the USA region (gcam.consumers and supplysectors)

    # For gcam-consumers: the core version  uses multiple consumers, so this needs to be considered:
    L244.DeleteConsumer_USAbld <- tibble(region = gcam.USA_REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer) %>%
      filter(gcam.consumer == "resid") %>%
      repeat_add_columns(tibble(group=unique(income_shares$category))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(tibble(region = gcam.USA_REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer)
                %>% filter(gcam.consumer == "comm"))

    # Multiple consumers need to be also added to supplysectors to delete:
    L244.DeleteSupplysector_USAbld <- tibble(region = gcam.USA_REGION, supplysector = unique(L244.Supplysector_bld$supplysector))



    # L244.SubregionalShares_gcamusa: subregional population and income shares (not currently used)
    L244.SubregionalShares_gcamusa <- write_to_all_states(A44.gcam_consumer, c("region", "gcam.consumer")) %>%
      mutate(pop.year.fillout = min(MODEL_BASE_YEARS),
             inc.year.fillout = min(MODEL_BASE_YEARS),
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

    # Commercial floorspace
    L244.Floorspace_comm <- L144.flsp_bm2_state_comm %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

    L244.Floorspace_full <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm)

    # Final output only has base years
    L244.Floorspace_gcamusa <- filter(L244.Floorspace_full, year %in% MODEL_BASE_YEARS)

    # L244.DemandFunction_serv_gcamusa and L244.DemandFunction_flsp_gcamusa: demand function types
    L244.DemandFunction_serv_gcamusa <- write_to_all_states(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]])
    L244.DemandFunction_flsp_gcamusa <- write_to_all_states(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]])

    # L244.Satiation_flsp_gcamusa: Satiation levels assumed for floorspace
    L244.Satiation_flsp_gcamusa <- A44.satiation_flsp %>%
      gather(gcam.consumer, value, resid, comm) %>%
      rename(region = state) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(L244.Floorspace_gcamusa %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(L100.Pop_thous_state %>% rename(pop = value), by = c("region" = "state", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = must be greater than the observed value in the final calibration year, so if observed value is
             # greater than calculated, multiply observed by 1.001
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # L244.SatiationImpedance_gcamusa: Calibrate satiation impedance per state.
    # sat_impedance = (-ln(2)/ln((satiation level - pcFlsp2015)/(satiation level - satiation adder))) * pcGDP2015

    L144.Satiation_impedance_gcamusa_pre<-L244.Satiation_flsp_gcamusa %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      # Add base floorspace
      left_join_error_no_match(L244.Floorspace_full,by=c("region","gcam.consumer","nodeInput","building.node.input","year")) %>%
      rename(flsp_bm2 = base.building.size) %>%
      # Add population to calculate floorspace per capita
      left_join_error_no_match(L100.Pop_thous_state %>% rename (region = state),
                               by=c("region","year")) %>%
      rename(pop_thous = value) %>%
      mutate(flsp_pc = (flsp_bm2*1E9) / (pop_thous*1E3)) %>%
      # Add GDPpc
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>% rename(region = state),
                               by=c("region","year")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Change units satiation level
      mutate(satiation.level = satiation.level * 1E6) %>%
      # Calculate satiation impedance
      mutate(`satiation-impedance` = (-log(2)/log((satiation.level - flsp_pc) / (satiation.level))) * pcGDP_thous90USD,
             `satiation-impedance`= round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE)) %>%
      select(region,nodeInput,building.node.input,`satiation-impedance`)

    L244.Satiation_impedance_gcamusa<-L144.Satiation_impedance_gcamusa_pre %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationImpedance"]])


    # L244.SatiationAdder_gcamusa: Satiation adders in floorspace demand function
    # Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)

    L244.SatiationAdder_gcamusa <- L244.Satiation_flsp_gcamusa %>%
      mutate(satiation.level = satiation.level * 1E6) %>%
      left_join_error_no_match(L244.Satiation_impedance_gcamusa,by = c("region", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L244.Floorspace_full,by=c("region","year","gcam.consumer", "nodeInput", "building.node.input")) %>%
      rename(observed_flsp_bm2 = base.building.size) %>%
      left_join_error_no_match(L100.Pop_thous_state %>% rename(region = state),
                               by = c("year", "region")) %>%
      rename(pop_thous = value) %>%
      mutate(observed_pcflsp = observed_flsp_bm2*1E9 / (pop_thous*1E3)) %>%
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>% rename(region = state),
                               by = c("year","region")) %>%
      rename(pcGDP_thous90USD = value) %>%
      mutate(est_pcflsp = satiation.level * (1-exp(-log(2)*pcGDP_thous90USD/`satiation-impedance`)),
             est_flsp_bm2 = (est_pcflsp*pop_thous*1E3) / 1E9) %>%
      group_by(region,nodeInput,building.node.input,year) %>%
      summarise(pop_thous = sum(pop_thous),
                est_flsp_bm2 = sum(est_flsp_bm2),
                observed_flsp_bm2 = sum(observed_flsp_bm2)) %>%
      ungroup() %>%
      mutate(est_flsp_bm2 = round(est_flsp_bm2,3),
             observed_flsp_bm2 = round(observed_flsp_bm2,3)) %>%
      mutate(satiation.adder = ((observed_flsp_bm2-est_flsp_bm2)*1E9) / (pop_thous*1E3)) %>%
      select(region,nodeInput,building.node.input,year,satiation.adder) %>%
      mutate(satiation.adder = round(satiation.adder,energy.DIGITS_SATIATION_ADDER),
             gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    #------------------------------------------------------
    # Updated floorspace Gompertz function
    # - Calculate the bias correction parameter (k)
    # - Write parameters for the updated floorspace function

    L144.flsp_param_usa<-L144.flsp_param %>%
      filter(region=="USA") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region=gcamusa.STATES))

    # First calculate the habitable land
    L144.hab_land_flsp_usa_fin<-L144.hab_land_flsp_usa %>%
      rename(state_name=state)%>%
      left_join(states_subregions %>% select(state,state_name),by="state_name") %>%
      filter(state %in% gcamusa.STATES) %>%
      mutate(area_thouskm2=(area_gcam-misc_land_usda)/1E3) %>%
      select(region=state,area_thouskm2)

    # Write the function parameters
    L244.GompFnParam_gcamusa<-L144.flsp_param_usa %>%
      left_join_error_no_match(L144.hab_land_flsp_usa_fin,by="region") %>%
      mutate(year=MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>% rename(region=state), by=c("region","year")) %>%
      rename(gdp_pc=value) %>%
      left_join_error_no_match(L100.Pop_thous_state %>% rename(region=state), by=c("region","year")) %>%
      rename(pop_thous=value) %>%
      left_join_error_no_match(L144.flsp_bm2_state_res %>% rename(region=state), by=c("region","year")) %>%
      rename(flsp=value) %>%
      mutate(flsp_pc=(flsp*1E9)/(pop_thous*1E3),
             base_flsp=flsp_pc,
             tot.dens=round(pop_thous/area_thouskm2,0),
             #correct 0 population density to avoid NaN
             tot.dens=if_else(tot.dens==0,1,tot.dens),
             flsp_est=(`unadjust.satiation` +(-`land.density.param`*log(tot.dens)))*exp(-`b.param`
                                                                                        *exp(-`income.param`*log(gdp_pc))),
             `bias.adjust.param`=flsp_pc-flsp_est,
             base_flsp=round(base_flsp,energy.DIGITS_FLOORSPACE),
             bias.adjust.param=round(bias.adjust.param,energy.DIGITS_FLOORSPACE),
             gcam.consumer="resid",
             nodeInput="resid",
             building.node.input="resid_building") %>%
      rename(pop.dens=tot.dens,
             habitable.land=area_thouskm2,
             base.pcFlsp=base_flsp) %>%
      select(LEVEL2_DATA_NAMES[["GompFnParam"]])


    #================================================================

    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.HDDCDD_scen_state <- L143.HDDCDD_scen_state %>%
      rename(region = state,
             degree.days = value)

    # Let's make a climate normal (historical average) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    L244.HDDCDD_normal_state <- L244.HDDCDD_scen_state %>%
      filter(year %in% seq(1981, 2000),
             # The AEO_2015 scenario changes this "normal climate" for each region,
             # which is not desirable since it does not incldue historical data
             # and is not the standard reference assumption.  Thus, we remove it
             # from this calculation.
             Scen != "AEO_2015") %>%
      group_by(region, variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup()

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    L244.HDDCDD_temp <- tidyr::crossing(region = gcamusa.STATES, thermal.building.service.input = thermal_services) %>%
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
      # L244.HDDCDD_scen_state has multiple scenarios, rows in this tbl_df are intended to be duplicated for each scenario
      # left_join_error_no_match throws an error when rows are duplicated (as intended), so left_join is used
      left_join(L244.HDDCDD_scen_state, by = c("region", "variable", "year")) %>%
      mutate(degree.days = round(degree.days, energy.DIGITS_HDDCDD))

    L244.HDDCDD_constdds_USA <- L244.HDDCDD_temp %>%
      filter(Scen == "constdds") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_A2_GFDL_USA <- L244.HDDCDD_temp %>%
      filter(Scen == "A2") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_AEO_2015_USA <- L244.HDDCDD_temp %>%
      filter(Scen == "AEO_2015") %>%
      select(-Scen, -GCM, -variable)

    # L244.ShellConductance_bld_gcamusa: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld_gcamusa <- A44.bld_shell_conductance %>%
      # Convert to long form
      gather_years() %>%
      # Interpolate to model years
      complete(gcam.consumer, year = c(year, MODEL_YEARS)) %>%
      group_by(gcam.consumer) %>%
      mutate(value = round(approx_fun(year, value), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Repeat for all states
      write_to_all_states(names = c(names(.), "region")) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet

    # L244.Supplysector_bld: Supplysector info for buildings
    L244.Supplysector_bld_gcamusa <- write_to_all_states(A44.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME))

    # L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    L244.FinalEnergyKeyword_bld_gcamusa <- write_to_all_states(A44.sector, LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]])

    # L244.SubsectorLogit_bld: Subsector logit exponents of building sector
    L244.SubsectorLogit_bld_gcamusa <- write_to_all_states(A44.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME))

    # L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(A44.subsector_shrwt$year))) {
      L244.SubsectorShrwt_bld_gcamusa <- write_to_all_states(A44.subsector_shrwt %>%
                                                               filter(!is.na(year)), LEVEL2_DATA_NAMES[["SubsectorShrwt"]])
    }
    if(any(!is.na(A44.subsector_shrwt$year.fillout))) {
      L244.SubsectorShrwtFllt_bld_gcamusa <- write_to_all_states(A44.subsector_shrwt %>%
                                                                   filter(!is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])
    }

    # L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector
    if(any(is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterp_bld_gcamusa <- write_to_all_states(A44.subsector_interp %>%
                                                                filter(is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterp"]])
    }
    if(any(!is.na(A44.subsector_interp$to.value))) {
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
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

    # Note - this code assumes that base-year efficiences are identical (should fix to copy over to make sure)
    L244.GlobalTechEff_bld <- L244.end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # L244.StubTechMarket_bld: Specify market names for fuel inputs to all technologies in each state
    L244.StubTechMarket_bld <- L244.end_use_eff %>%
      mutate(market.name = gcam.USA_REGION) %>%
      rename(stub.technology = technology) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      # For fuels with state markets, set the market.name to the region
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS, region, market.name)) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]])

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    # Combine residential and commercial energy data
    L244.in_EJ_R_bld_serv_F_Yh <- bind_rows(L144.in_EJ_state_res_F_U_Y, L144.in_EJ_state_comm_F_U_Y) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(supplysector = service) %>%
      # Add subsector and energy.input
      left_join_error_no_match(calibrated_techs_bld_usa %>%
                                 select(sector, supplysector, fuel, subsector, minicam.energy.input) %>%
                                 distinct(), by = c("sector", "supplysector", "fuel")) %>%
      select(region = state, supplysector, subsector, minicam.energy.input, year, calibrated.value)

    # Shares allocated to partitioned technologies need to be computed first using efficiencies
    L244.globaltech_eff_prt <- A44.globaltech_eff %>%
      semi_join(A44.globaltech_eff_avg, by = c("supplysector", "subsector")) %>%
      filter(year == gcamusa.EFFICIENCY_PARTITION_YEAR) %>%
      select(supplysector, subsector, technology, efficiency = value)

    # Calculate technology shares using efficiency values
    L244.globaltech_shares <- A44.globaltech_eff_avg %>%
      # Adding specific technology efficiency to stock average efficiency
      left_join_error_no_match(L244.globaltech_eff_prt, by = c("supplysector", "subsector", "technology1" = "technology")) %>%
      rename(efficiency_tech1 = efficiency) %>%
      left_join_error_no_match(L244.globaltech_eff_prt, by = c("supplysector", "subsector", "technology2" = "technology")) %>%
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
      # Clunky, but we want only one technology and share value, currently have technology1, technology2, share1, share2
      gather(share_type, share, share_tech1, share_tech2)%>%
      gather(tech_type, technology, technology1, technology2) %>%
      # Filter for same technology and share number, then remove tech_type and share_type columns
      filter(substr(tech_type, nchar(tech_type), nchar(tech_type)) == substr(share_type, nchar(share_type), nchar(share_type))) %>%
      select(-tech_type, -share_type)

    # For calibration table, start with global tech efficiency table, repeat by states, and match in tech shares.
    L244.StubTechCalInput_bld_gcamusa <- L244.GlobalTechEff_bld %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_states(names = c(names(.), "region")) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      # Using left_join because we don't have shares for all technologies, NAs will be set to 1
      left_join(L244.globaltech_shares, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      replace_na(list(share = 1)) %>%
      # Add energy by state/service/fuel
      left_join_error_no_match(L244.in_EJ_R_bld_serv_F_Yh, by = c("region", "supplysector", "subsector", "minicam.energy.input", "year")) %>%
      # calibrated.value = energy * share
      mutate(calibrated.value = round(share * calibrated.value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             calOutputValue = calibrated.value) %>%
      # Set subsector and technology shareweights
      set_subsector_shrwt() %>%
      mutate(tech.share.weight =  if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # L244.GlobalTechShrwt_bld_gcamusa: Default shareweights for global building technologies
    L244.GlobalTechShrwt_bld_gcamusa <- A44.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # L244.GlobalTechInterpTo_bld: Technology shareweight interpolation (selected techs only)
    L244.GlobalTechInterpTo_bld <- A44.globaltech_interp %>%
      set_years() %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])

    # L244.GlobalTechCost_bld: Non-fuel costs of global building technologies
    L244.GlobalTechCost_bld_gcamusa <- A44.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # L244.GlobalTechSCurve_bld: Retirement rates for building technologies
    L244.GlobalTechSCurve_bld <- L244.GlobalTechCost_bld_gcamusa %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS),
             sector.name %in% A44.globaltech_retirement$supplysector) %>%
      # Add lifetimes and steepness
      left_join_error_no_match(A44.globaltech_retirement, by = c("sector.name" = "supplysector")) %>%
      # Set steepness/halflife values to stock for base years, new for future years
      mutate(steepness = if_else(year == max(MODEL_BASE_YEARS), steepness_stock, steepness_new),
             half.life = if_else(year == max(MODEL_BASE_YEARS), half_life_stock, half_life_new)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSCurve"]])

    # L244.GlobalTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services
    calibrated_techs_bld_usa_consumer <- calibrated_techs_bld_usa %>%
      select(gcam.consumer = sector, supplysector) %>%
      distinct()

    L244.GlobalTechIntGainOutputRatio <- A44.globaltech_intgains %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))%>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_usa_consumer, by = "supplysector") %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      # Add efficiency
      left_join_error_no_match(L244.GlobalTechEff_bld,
                               by = c("sector.name", "subsector.name", "technology", "year")) %>%
      mutate(internal.gains.output.ratio = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
    L244.base_service <- L244.StubTechCalInput_bld_gcamusa %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.GlobalTechEff_bld,
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # Aggregate base service by service (supplysector)
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_usa_consumer, by = "supplysector") %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer")

    # Separate thermal and generic services into separate tables with different ID strings
    L244.GenericBaseService_gcamusa <- L244.base_service %>%
      filter(supplysector %in% generic_services) %>%
      rename(building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    L244.ThermalBaseService_gcamusa <- L244.base_service %>%
      filter(supplysector %in% thermal_services) %>%
      rename(thermal.building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # L244.GenericServiceSatiation_gcamusa: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    L244.GenericServiceSatiation_gcamusa <- L244.GenericBaseService_gcamusa %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_gcamusa, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    L244.ThermalServiceSatiation_gcamusa <- L244.ThermalBaseService_gcamusa %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_gcamusa, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)
    DDnorm <- c(gcamusa.BASE_HDD_USA, gcamusa.BASE_CDD_USA)
    US.base.scalar <- tibble(variable, scalar, DDnorm)
    threshold_HDD <- 500

    L244.Intgains_scalar_gcamusa <- L244.ThermalServiceSatiation_gcamusa %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add DDnorm & scalar
      left_join_error_no_match(US.base.scalar, by = "variable") %>%
      # Add degree days
      left_join_error_no_match(L244.HDDCDD_normal_state, by = c("region", "variable")) %>%
      mutate(internal.gains.scalar = round(scalar * degree.days / DDnorm, energy.DIGITS_HDDCDD),
             # Prevent very warm places from having negative heating demands, using exogenous threshold
             internal.gains.scalar = if_else(variable == "HDD" & degree.days < threshold_HDD, 0, internal.gains.scalar)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    #------------------------------------------------------
    # In order to make the function flexible to the implementation of multiple consumers, the satiation impedance (mu) and the calibration coefficent (k)
    # were calibrated in the DS in the global version.
    # Considering that GCAM-USA uses the same building_service function,this needs to be extended to this module.
    # Therefore, here we create L244.ThermalServiceImpedance_gcamusa, L244.GenericServiceImpedance_gcamusa,
    # L244.GenericServiceCoef_gcamusa, and L244.ThermalServiceCoef_gcamusa

    # 1-Generic services
    L244.GenericServiceImpedance_allvars_gcamusa<-L244.GenericServiceSatiation_gcamusa %>%
      left_join_error_no_match(L244.base_service %>%  filter(year==MODEL_FINAL_BASE_YEAR, supplysector %in% generic_services)
                               %>% rename(building.service.input = supplysector),
                               by=c("region","gcam.consumer","nodeInput","building.node.input","building.service.input")) %>%
      rename(base_service_EJ = base.service) %>%
      left_join_error_no_match(L244.Floorspace_full, by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ / base.building.size) %>%
      select(-base_service_EJ,-base.building.size) %>%
      # Add pcGDP
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>% rename(region = state),
                               by=c("year","region")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4
      left_join_error_no_match(L144.prices_bld_gcamusa  %>%
                                 rename(building.service.input = sector) %>%
                                 filter(building.service.input %in% generic_services, year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","building.service.input")) %>%
      rename(price = value) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price)) / log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      # Check with an adder to be 0!!!!
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(thermal_load = 1,
             afford=(pcGDP_thous90USD*1000/def9075) / price,
             serv_density = satiation.level * (1-exp((-log(2)/`satiation-impedance`) * afford)),
             coef = observed_base_serv_perflsp / serv_density*thermal_load,
             est_base_serv_perflsp = coef * thermal_load * serv_density,
             bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))

    L244.GenericServiceImpedance_gcamusa<-L244.GenericServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE))

    L244.GenericServiceCoef_gcamusa<-L244.GenericServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))

    L244.GenericServiceAdder_gcamusa<-L244.GenericServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]]) %>%
      mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))


    # 2-Thermal services
    # First calculate internal gains
    L244.internal_gains_gcamusa<-L244.StubTechCalInput_bld_gcamusa %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.GlobalTechEff_bld %>%
                                 rename(stub.technology = technology,
                                        supplysector = sector.name,
                                        subsector = subsector.name),
                               by = c("supplysector", "subsector" ,
                                      "stub.technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # use left join because not all services produce internal gains
      left_join(L244.GlobalTechIntGainOutputRatio,
                by = c("supplysector" = "sector.name", "subsector" = "subsector.name" ,
                       "stub.technology"="technology", "year")) %>%
      filter(complete.cases(.)) %>%
      mutate(int_gains = base.service*internal.gains.output.ratio,
             gcam.consumer = if_else(grepl("comm",supplysector),"comm","resid")) %>%
      group_by(region,gcam.consumer,year) %>%
      summarise(int_gains = sum(int_gains)) %>%
      ungroup() %>%
      rename(intGains_EJ= int_gains)


    L244.ThermalServiceImpedance_allvars_gcamusa<-L244.ThermalServiceSatiation_gcamusa %>%
      left_join_error_no_match(L244.base_service %>%  filter(year==MODEL_FINAL_BASE_YEAR, supplysector %in% thermal_services)
                               %>% rename(thermal.building.service.input = supplysector),
                               by=c("region","gcam.consumer","nodeInput","building.node.input","thermal.building.service.input")) %>%
      rename(base_service_EJ = base.service) %>%
      left_join_error_no_match(L244.Floorspace_full, by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ / base.building.size) %>%
      select(-base_service_EJ) %>%
      # Add pcGDP
      left_join_error_no_match(L100.pcGDP_thous90usd_state %>% rename(region = state),
                               by=c("year","region")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4
      left_join_error_no_match(L144.prices_bld_gcamusa  %>%
                                 rename(thermal.building.service.input = sector) %>%
                                 filter(thermal.building.service.input %in% thermal_services, year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","thermal.building.service.input")) %>%
      rename(price = value) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price)) / log((satiation.level)/(satiation.level-base_serv_flsp)),
             dd=if_else(grepl("cooling",thermal.building.service.input),"CDD","HDD")) %>%
      left_join_error_no_match(L244.HDDCDD_scen_state %>% filter(year == MODEL_FINAL_BASE_YEAR,
                                                               GCM == "hist") %>%
                                 rename(dd = variable) %>%
                                 select(-GCM,-Scen),
                               by=c("region","year","dd")) %>%
      left_join_error_no_match(L244.ShellConductance_bld_gcamusa %>% select(-shell.year) %>% filter(year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      left_join_error_no_match(L244.internal_gains_gcamusa %>% filter(year == MODEL_FINAL_BASE_YEAR), by=c("region","year","gcam.consumer")) %>%
      left_join_error_no_match(L244.Intgains_scalar_gcamusa,by = c("region","gcam.consumer","nodeInput",
                                                           "building.node.input","thermal.building.service.input")) %>%
      mutate(intGains_EJ_serv = intGains_EJ / base.building.size,
             thermal_load = degree.days * shell.conductance * floor.to.surface.ratio + internal.gains.scalar*intGains_EJ_serv) %>%
      select(-base.building.size) %>%
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price,
             serv_density=satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford)),
             coef = observed_base_serv_perflsp / (serv_density*thermal_load),
             est_base_serv_perflsp = coef * thermal_load * serv_density,
             bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))

    L244.ThermalServiceImpedance_gcamusa<-L244.ThermalServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE))

    L244.ThermalServiceCoef_gcamusa<-L244.ThermalServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))

    L244.ThermalServiceAdder_gcamusa<-L244.ThermalServiceImpedance_allvars_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]]) %>%
      mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))

    # Finally, need to delete the TradBio resource for the USA region to keep consistency between global GCAM and GCAM-USA:
    L210.DeleteRsrcTradBio_gcamusa<- tibble(region = gcam.USA_REGION, resource = "traditional biomass")

    #------------------------------------------------------
    # Write the service prices in final calibration year
    # These will be used in the cpp files to compute the adjustment parameter that will account for the difference between read and calculated service prices

    L244.GenericServicePrice_gcamusa<- L144.prices_bld_gcamusa %>%
      filter(sector %in% generic_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(building.service.input = sector,
             price = value) %>%
      filter(grepl("resid",building.service.input)) %>%
      mutate(gcam.consumer = "resid") %>%
      left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      bind_rows(L144.prices_bld_gcamusa %>%
                  filter(sector %in% generic_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(building.service.input = sector,
                         price = value) %>%
                  filter(grepl("comm",building.service.input)) %>%
                  mutate(gcam.consumer = "comm") %>%
                  left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["GenericServicePrice"]])


    L244.ThermalServicePrice_gcamusa<- L144.prices_bld_gcamusa %>%
      filter(sector %in% thermal_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(thermal.building.service.input = sector,
             price = value) %>%
      filter(grepl("resid",thermal.building.service.input)) %>%
      mutate(gcam.consumer = "resid") %>%
      left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      bind_rows(L144.prices_bld_gcamusa %>%
                  filter(sector %in% thermal_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(thermal.building.service.input = sector,
                         price = value) %>%
                  filter(grepl("comm",thermal.building.service.input)) %>%
                  mutate(gcam.consumer = "comm") %>%
                  left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
    select(LEVEL2_DATA_NAMES[["ThermalServicePrice"]])

    #------------------------------------------------------
    # Finally, calculate the base year service density
    # This density will be used in case it gets negative when adding the bias adder coefficient
    L244.GenericBaseDens_gcamusa<-L244.GenericBaseService_gcamusa %>%
      left_join_error_no_match(L244.Floorspace_gcamusa, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseDens"]])

    L244.ThermalBaseDens_gcamusa<-L244.ThermalBaseService_gcamusa %>%
      left_join_error_no_match(L244.Floorspace_gcamusa, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseDens"]])



    # ===================================================
    # Produce outputs
    L244.DeleteConsumer_USAbld %>%
      add_title("Deletes building sector in USA region to rewrite with GCAM-USA data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.gcam_consumer") %>%
      add_legacy_name("L244.DeleteConsumer_USAbld") %>%
      add_precursors("energy/A44.gcam_consumer","L244.Supplysector_bld","socioeconomics/income_shares") ->
      L244.DeleteConsumer_USAbld

    L244.DeleteSupplysector_USAbld %>%
      add_title("Deletes building sector in USA region to rewrite with GCAM-USA data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.sector") %>%
      add_legacy_name("L244.DeleteSupplysector_USAbld") %>%
      add_precursors("energy/A44.sector","L244.Supplysector_bld","socioeconomics/income_shares") ->
      L244.DeleteSupplysector_USAbld

    L244.SubregionalShares_gcamusa %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("gcam-usa/A44.gcam_consumer") ->
      L244.SubregionalShares_gcamusa

    L244.PriceExp_IntGains_gcamusa %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all states") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("gcam-usa/A44.gcam_consumer") ->
      L244.PriceExp_IntGains_gcamusa

    L244.Floorspace_gcamusa %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_state_res and L144.flsp_bm2_state_comm") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.gcam_consumer") ->
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


    L244.Satiation_impedance_gcamusa %>%
      add_title("Satiation impedance for floorspace") %>%
      add_units("Unitless") %>%
      add_comments("Calibrated in the DS for flexibility with multiple consumer groups") %>%
      add_legacy_name("L244.Satiation_impedance_gcamusa") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm") ->
      L244.Satiation_impedance_gcamusa

    L244.SatiationAdder_gcamusa %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation level; per capita floorspace; and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-usa/A44.satiation_flsp", "gcam-usa/A44.gcam_consumer", "L100.Pop_thous_state",
                     "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "L100.pcGDP_thous90usd_state") ->
      L244.SatiationAdder_gcamusa

    L244.GompFnParam_gcamusa %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Computed offline based on data from RECS and IEA") %>%
      add_legacy_name("L244.GompFnParam_gcamusa") %>%
      add_precursors("L144.flsp_param", "L100.Pop_thous_state","L100.pcGDP_thous90usd_state",
                     "gcam-usa/A44.hab_land_flsp_usa","L144.flsp_bm2_state_res" ) ->
      L244.GompFnParam_gcamusa


    L244.HDDCDD_A2_GFDL_USA %>%
      add_title("Heating and Cooling Degree Days by State for GFDL A2") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_state assigned to GCAM subsectors") %>%
      add_legacy_name("L244.HDDCDD_A2_GFDL") %>%
      add_precursors("L143.HDDCDD_scen_state", "gcam-usa/A44.sector",
                     "gcam-usa/calibrated_techs_bld_usa", "gcam-usa/A44.gcam_consumer") ->
      L244.HDDCDD_A2_GFDL_USA

    L244.ThermalBaseService_gcamusa %>%
      add_title("Base year output of thermal buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer") ->
      L244.ThermalBaseService_gcamusa

    L244.GenericBaseService_gcamusa %>%
      add_title("Base year output of generic buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer") ->
      L244.GenericBaseService_gcamusa

    L244.GenericServiceSatiation_gcamusa %>%
      add_title("Satiation levels assumed for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm",
                     "gcam-usa/A44.demand_satiation_mult") ->
      L244.GenericServiceSatiation_gcamusa

    L244.GenericServiceImpedance_gcamusa %>%
      add_title("Satiation impedance for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Satiation impedance estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceImpedance_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.GenericServiceImpedance_gcamusa

    L244.GenericServiceCoef_gcamusa %>%
      add_title("Calibration coefficient for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceCoef_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.GenericServiceCoef_gcamusa

    L244.GenericServiceAdder_gcamusa %>%
      add_title("Bias Adder for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceAdder_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.GenericServiceAdder_gcamusa

    L244.ThermalServiceSatiation_gcamusa %>%
      add_title("Satiation levels assumed for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm",
                     "gcam-usa/A44.demand_satiation_mult") ->
      L244.ThermalServiceSatiation_gcamusa

    L244.ThermalServiceImpedance_gcamusa %>%
      add_title("Satiation impedance for thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Satiation impedance estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceImpedance_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.ThermalServiceImpedance_gcamusa

    L244.ThermalServiceCoef_gcamusa %>%
      add_title("Calibration coefficient for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceCoef_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.ThermalServiceCoef_gcamusa

    L244.ThermalServiceAdder_gcamusa %>%
      add_title("Bias Adder for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceAdder_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm", "gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.ThermalServiceAdder_gcamusa

    L244.Intgains_scalar_gcamusa %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.scalar = exogenous scalar * degree.days / exogenous degree day norm") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares",
                     "gcam-usa/A44.gcam_consumer", "L144.flsp_bm2_state_res", "L144.flsp_bm2_state_comm",
                     "gcam-usa/A44.demand_satiation_mult", "L143.HDDCDD_scen_state") ->
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

    if(exists("L244.SubsectorShrwt_bld")) {
      L244.SubsectorShrwt_bld_gcamusa %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("gcam-usa/A44.subsector_shrwt") ->
        L244.SubsectorShrwt_bld_gcamusa
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.SubsectorShrwt_bld_gcamusa
    }

    if(exists("L244.SubsectorShrwtFllt_bld_gcamusa")) {
      L244.SubsectorShrwtFllt_bld_gcamusa %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-usa/A44.subsector_shrwt") ->
        L244.SubsectorShrwtFllt_bld_gcamusa
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.SubsectorShrwtFllt_bld_gcamusa
    }


    if(exists("L244.SubsectorInterp_bld_gcamusa")) {
      L244.SubsectorInterp_bld_gcamusa %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("gcam-usa/A44.subsector_interp") ->
        L244.SubsectorInterp_bld_gcamusa
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.SubsectorInterp_bld_gcamusa
    }

    if(exists("L244.SubsectorInterpTo_bld_gcamusa")) {
      L244.SubsectorInterpTo_bld_gcamusa %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-usa/A44.subsector_interp") ->
        L244.SubsectorInterpTo_bld_gcamusa
    } else {
      missing_data() %>%
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

    L244.StubTechCalInput_bld_gcamusa %>%
      add_title("Calibrated energy consumption and share weights by buildings technologies") %>%
      add_units("calibrated.value: EJ/yr; shareweights: Unitless") %>%
      add_comments("Energy consumption multiplied by shares to get calibrated energy") %>%
      add_comments("Shares calculated using efficiency averages") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y", "L144.in_EJ_state_comm_F_U_Y", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.globaltech_eff", "gcam-usa/A44.globaltech_eff_avg", "gcam-usa/A44.globaltech_shares") ->
      L244.StubTechCalInput_bld_gcamusa

    L244.StubTechMarket_bld %>%
      add_title("market names for fuel inputs to all technologies in each state") %>%
      add_units("NA") %>%
      add_comments("Categories from A44.globaltech_eff written to all states") %>%
      add_comments("Market set to states for electricity") %>%
      add_legacy_name("L244.StubTechMarket_bld") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L244.StubTechMarket_bld

    L244.GlobalTechIntGainOutputRatio %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.output.ratio = input.ratio from A44.globaltech_intgains divided by efficiency from L244.GlobalTechEff_bld") %>%
      add_legacy_name("L244.GlobalTechIntGainOutputRatio") %>%
      add_precursors("gcam-usa/A44.globaltech_intgains", "gcam-usa/calibrated_techs_bld_usa",
                     "gcam-usa/A44.gcam_consumer", "gcam-usa/A44.globaltech_eff") ->
      L244.GlobalTechIntGainOutputRatio

    L244.GlobalTechInterpTo_bld %>%
      add_title("Technology shareweight interpolation") %>%
      add_units("NA") %>%
      add_comments("Directly from A44.globaltech_interp") %>%
      add_legacy_name("L244.GlobalTechInterpTo_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_interp") ->
      L244.GlobalTechInterpTo_bld

    L244.GlobalTechEff_bld %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values from A44.globaltech_eff") %>%
      add_legacy_name("L244.GlobalTechEff_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_eff") ->
      L244.GlobalTechEff_bld

    L244.GlobalTechShrwt_bld_gcamusa %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated to model years from A44.globaltech_shrwt") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_shrwt") ->
      L244.GlobalTechShrwt_bld_gcamusa

    L244.GlobalTechCost_bld_gcamusa %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("Values from A44.globaltech_cost") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_cost") ->
      L244.GlobalTechCost_bld_gcamusa

    L244.GlobalTechSCurve_bld %>%
      add_title("Retirement rates for building technologies") %>%
      add_units("lifetime/half.life = years") %>%
      add_comments("Lifetime, steepness, and half.life from A44.globaltech_retirement") %>%
      add_legacy_name("L244.GlobalTechSCurve_bld") %>%
      add_precursors("gcam-usa/A44.globaltech_cost", "gcam-usa/A44.globaltech_retirement") ->
      L244.GlobalTechSCurve_bld

    L244.HDDCDD_AEO_2015_USA %>%
      add_title("Heating and Cooling Degree Days by State consistent with AEO 2015") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_state assigned to GCAM residential / commercial building consumers") %>%
      add_legacy_name("L244.HDDCDD_QER_QER") %>%
      same_precursors_as("L244.HDDCDD_A2_GFDL_USA") ->
      L244.HDDCDD_AEO_2015_USA

    L244.HDDCDD_constdds_USA %>%
      add_title("Heating and Cooling Degree Days by State - constant at historical levels") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_state assigned to GCAM subsectors") %>%
      same_precursors_as("L244.HDDCDD_A2_GFDL_USA") ->
      L244.HDDCDD_constdds_USA

    L210.DeleteRsrcTradBio_gcamusa %>%
      add_title("Delete TradBio sector in USA") %>%
      add_units("unitless") %>%
      add_comments("Keep consistency between CAm and GCAM-USA") %>%
      add_legacy_name("L210.DeleteRsrcTradBio_gcamusa") %>%
      add_precursors("gcam-usa/A44.sector") ->
      L210.DeleteRsrcTradBio_gcamusa

    L244.GenericServicePrice_gcamusa %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for generic services") %>%
      add_legacy_name("L244.GenericServicePrice_gcamusa") %>%
      add_precursors("gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.GenericServicePrice_gcamusa

    L244.ThermalServicePrice_gcamusa %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for thermal services") %>%
      add_legacy_name("L244.ThermalServicePrice_gcamusa") %>%
      add_precursors("gcam-usa/A44.CalPrice_service_gcamusa") ->
      L244.ThermalServicePrice_gcamusa

    L244.GenericBaseDens_gcamusa %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for generic services") %>%
      add_legacy_name("L244.GenericBaseDens_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y","L144.flsp_bm2_state_res") ->
      L244.GenericBaseDens_gcamusa

    L244.ThermalBaseDens_gcamusa %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for thermal services") %>%
      add_legacy_name("L244.ThermalBaseDens_gcamusa") %>%
      add_precursors("L144.in_EJ_state_res_F_U_Y","L144.flsp_bm2_state_res") ->
      L244.ThermalBaseDens_gcamusa


    return_data(L244.DeleteConsumer_USAbld,
                L244.DeleteSupplysector_USAbld,
                L244.SubregionalShares_gcamusa,
                L244.PriceExp_IntGains_gcamusa,
                L244.Floorspace_gcamusa,
                L244.DemandFunction_serv_gcamusa,
                L244.DemandFunction_flsp_gcamusa,
                L244.Satiation_flsp_gcamusa,
                L244.SatiationAdder_gcamusa,
                L244.ThermalBaseService_gcamusa,
                L244.GenericBaseService_gcamusa,
                L244.ThermalServiceSatiation_gcamusa,
                L244.GenericServiceSatiation_gcamusa,
                L244.Intgains_scalar_gcamusa,
                L244.ShellConductance_bld_gcamusa,
                L244.Supplysector_bld_gcamusa,
                L244.FinalEnergyKeyword_bld_gcamusa,
                L244.SubsectorShrwt_bld_gcamusa,
                L244.SubsectorShrwtFllt_bld_gcamusa,
                L244.SubsectorInterp_bld_gcamusa,
                L244.SubsectorInterpTo_bld_gcamusa,
                L244.SubsectorLogit_bld_gcamusa,
                L244.StubTech_bld_gcamusa,
                L244.StubTechCalInput_bld_gcamusa,
                L244.StubTechMarket_bld,
                L244.GlobalTechIntGainOutputRatio,
                L244.GlobalTechInterpTo_bld,
                L244.GlobalTechEff_bld,
                L244.GlobalTechShrwt_bld_gcamusa,
                L244.GlobalTechCost_bld_gcamusa,
                L244.GlobalTechSCurve_bld,
                L244.HDDCDD_A2_GFDL_USA,
                L244.HDDCDD_AEO_2015_USA,
                L244.HDDCDD_constdds_USA,
                L244.GompFnParam_gcamusa,
                L244.Satiation_impedance_gcamusa,
                L244.GenericServiceImpedance_gcamusa,
                L244.GenericServiceCoef_gcamusa,
                L244.GenericServiceAdder_gcamusa,
                L244.ThermalServiceImpedance_gcamusa,
                L244.ThermalServiceCoef_gcamusa,
                L244.ThermalServiceAdder_gcamusa,
                L210.DeleteRsrcTradBio_gcamusa,
                L244.GenericServicePrice_gcamusa,
                L244.ThermalServicePrice_gcamusa,
                L244.GenericBaseDens_gcamusa,
                L244.ThermalBaseDens_gcamusa)
  } else {
    stop("Unknown command")
  }
}
