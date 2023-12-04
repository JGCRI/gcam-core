# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L244.building_det
#'
#' Creates level2 data for the building sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.SubregionalShares}, \code{L244.SubregionalShares_SSP1}, \code{L244.SubregionalShares_SSP2},\code{L244.SubregionalShares_SSP3},
#' \code{L244.SubregionalShares_SSP4},\code{L244.SubregionalShares_SSP5},
#' \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv},
#' \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService},
#'\code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld},
#' \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld},
#' \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogit_bld}, \code{L244.FuelPrefElast_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechEff_bld},
#' \code{L244.StubTechCalInput_bld}, \code{L244.StubTechIntGainOutputRatio}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld},
#' \code{L244.DeleteGenericService}, \code{L244.Satiation_flsp_SSP1}, \code{L244.SatiationAdder_SSP1}, \code{L244.GenericServiceSatiation_SSP1},
#' \code{L244.Satiation_flsp_SSP2}, \code{L244.SatiationAdder_SSP2}, \code{L244.GenericServiceSatiation_SSP2}, \code{L244.Satiation_flsp_SSP3},
#' \code{L244.SatiationAdder_SSP3}, \code{L244.GenericServiceSatiation_SSP3}, \code{L244.Satiation_flsp_SSP4},
#' \code{L244.SatiationAdder_SSP4}, \code{L244.GenericServiceSatiation_SSP4},  \code{L244.Satiation_flsp_SSP5},
#' \code{L244.SatiationAdder_SSP5}, \code{L244.GenericServiceSatiation_SSP5}, \code{L244.DeleteThermalService},
#' \code{L244.HDDCDD_A2_CCSM3x}, \code{L244.HDDCDD_A2_HadCM3}, \code{L244.HDDCDD_B1_CCSM3x}, \code{L244.HDDCDD_B1_HadCM3},
#' \code{L244.HDDCDD_constdd_no_GCM} and \code{L244.GompFnParam}, \code{L244.Satiation_impedance},\code{L244.Satiation_impedance_SSP1},\code{L244.Satiation_impedance_SSP2},\code{L244.Satiation_impedance_SSP3}
#' \code{L244.Satiation_impedance_SSP4}, \code{L244.Satiation_impedance_SSP5}, \code{L244.GenericServiceImpedance}, \code{L244.GenericServiceImpedance_SSP1},
#' \code{L244.GenericServiceImpedance_SSP2}, \code{L244.GenericServiceImpedance_SSP3},\code{L244.GenericServiceImpedance_SSP4} , \code{L244.GenericServiceImpedance_SSP5}
#' \code{L244.GenericServiceAdder},\code{L244.GenericServiceAdder_SSP1}, \code{L244.GenericServiceAdder_SSP2}, \code{L244.GenericServiceAdder_SSP3}, \code{L244.GenericServiceAdder_SSP4}, \code{L244.GenericServiceAdder_SSP5}
#' \code{L244.ThermalServiceImpedance}, \code{L244.ThermalServiceAdder}
#' \code{L244.GenericServiceCoef},\code{L244.ThermalServiceCoef}, \code{L244.GenericServiceCoef_SSP1}, \code{L244.GenericServiceCoef_SSP2},
#' \code{L244.GenericServiceCoef_SSP3}, \code{L244.GenericServiceCoef_SSP4}, \code{L244.GenericServiceCoef_SSP5},
#'  \code{L244.ThermalCoalCoef}, \code{L244.GenericCoalCoef},\code{L244.ThermalTradBioCoef}, \code{L244.GenericTradBioCoef},
#' \code{L244.GenericShares}, \code{L244.ThermalShares},\code{L244.GenericServicePrice}, \code{L244.ThermalServicePrice},
#' \code{L244.GenericBaseDens}, \code{L244.ThermalBaseDens},
#' The corresponding file in the original data system was \code{L244.building_det.R} (energy level2).
#' @details Creates level2 data for the building sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting unite
#' @author RLH September 2017

module_energy_L244.building_det <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = "energy/A_regions",
             FILE = "energy/A44.sector",
             FILE = "energy/A44.subsector_interp",
             FILE = "energy/A44.subsector_logit",
             FILE = "energy/A44.subsector_shrwt",
             FILE = "energy/A44.fuelprefElasticity",
             FILE = "energy/A44.globaltech_shrwt",
             FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.demandFn_serv",
             FILE = "energy/A44.demandFn_flsp",
             FILE = "energy/A44.internal_gains",
             FILE = "energy/A44.satiation_flsp",
             FILE = "energy/A44.satiation_flsp_SSPs",
             FILE = "energy/A44.demand_satiation_mult",
             FILE = "energy/A44.demand_satiation_mult_SSPs",
             "L144.flsp_bm2_R_res_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             "L144.base_service_EJ_serv",
             "L144.base_service_EJ_serv_fuel",
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L144.end_use_eff",
             "L144.shell_eff_R_Y",
             "L144.NEcost_75USDGJ",
             "L144.internal_gains",
             "L143.HDDCDD_scen_R_Y",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             FILE = "socioeconomics/income_shares",
             "L144.flsp_param",
             "L144.hab_land_flsp_fin",
             "L144.prices_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.SubregionalShares",
             "L244.SubregionalShares_SSP1",
             "L244.SubregionalShares_SSP2",
             "L244.SubregionalShares_SSP3",
             "L244.SubregionalShares_SSP4",
             "L244.SubregionalShares_SSP5",
             "L244.PriceExp_IntGains",
             "L244.Floorspace",
             "L244.DemandFunction_serv",
             "L244.DemandFunction_flsp",
             "L244.Satiation_flsp",
             "L244.SatiationAdder",
             "L244.ThermalBaseService",
             "L244.GenericBaseService",
             "L244.ThermalServiceSatiation",
             "L244.GenericServiceSatiation",
             "L244.Intgains_scalar",
             "L244.ShellConductance_bld",
             "L244.Supplysector_bld",
             "L244.FinalEnergyKeyword_bld",
             "L244.SubsectorShrwt_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorInterpTo_bld",
             "L244.SubsectorLogit_bld",
             "L244.FuelPrefElast_bld",
             "L244.StubTech_bld",
             "L244.StubTechEff_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechIntGainOutputRatio",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.DeleteGenericService",
             "L244.Satiation_flsp_SSP1",
             "L244.SatiationAdder_SSP1",
             "L244.GenericServiceSatiation_SSP1",
             "L244.Satiation_flsp_SSP2",
             "L244.SatiationAdder_SSP2",
             "L244.GenericServiceSatiation_SSP2",
             "L244.Satiation_flsp_SSP3",
             "L244.SatiationAdder_SSP3",
             "L244.GenericServiceSatiation_SSP3",
             "L244.Satiation_flsp_SSP4",
             "L244.SatiationAdder_SSP4",
             "L244.GenericServiceSatiation_SSP4",
             "L244.Satiation_flsp_SSP5",
             "L244.SatiationAdder_SSP5",
             "L244.GenericServiceSatiation_SSP5",
             "L244.DeleteThermalService",
             "L244.HDDCDD_A2_CCSM3x",
             "L244.HDDCDD_A2_HadCM3",
             "L244.HDDCDD_B1_CCSM3x",
             "L244.HDDCDD_B1_HadCM3",
             "L244.HDDCDD_constdd_no_GCM",
             "L244.GompFnParam",
             "L244.Satiation_impedance",
             "L244.Satiation_impedance_SSP1",
             "L244.Satiation_impedance_SSP2",
             "L244.Satiation_impedance_SSP3",
             "L244.Satiation_impedance_SSP4",
             "L244.Satiation_impedance_SSP5",
             "L244.GenericServiceImpedance",
             "L244.GenericServiceImpedance_SSP1",
             "L244.GenericServiceImpedance_SSP2",
             "L244.GenericServiceImpedance_SSP3",
             "L244.GenericServiceImpedance_SSP4",
             "L244.GenericServiceImpedance_SSP5",
             "L244.GenericServiceAdder",
             "L244.GenericServiceAdder_SSP1",
             "L244.GenericServiceAdder_SSP2",
             "L244.GenericServiceAdder_SSP3",
             "L244.GenericServiceAdder_SSP4",
             "L244.GenericServiceAdder_SSP5",
             "L244.ThermalServiceImpedance",
             "L244.ThermalServiceAdder",
             "L244.ThermalServiceCoef",
             "L244.GenericServiceCoef",
             "L244.GenericServiceCoef_SSP1",
             "L244.GenericServiceCoef_SSP2",
             "L244.GenericServiceCoef_SSP3",
             "L244.GenericServiceCoef_SSP4",
             "L244.GenericServiceCoef_SSP5",
             "L244.GenericCoalCoef",
             "L244.ThermalCoalCoef",
             "L244.GenericTradBioCoef",
             "L244.ThermalTradBioCoef",
             "L244.GenericShares",
             "L244.ThermalShares",
             "L244.GenericServicePrice",
             "L244.ThermalServicePrice",
             "L244.GenericBaseDens",
             "L244.ThermalBaseDens"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    building.service.input <- calibrated.value <-  comm <- degree.days <- floorspace_bm2 <- gcam.consumer <-
      internal.gains.market.name <- internal.gains.output.ratio <- multiplier <- nodeInput <- pcFlsp_mm2 <-
      pcFlsp_mm2_fby <- pcGDP_thous90USD <- pop_thous <- region <- region.class <- resid <- satiation.adder <-
      satiation.level <- scalar_mult <- sector <- service <- service.per.flsp <- share.weight <- shell.conductance <-
      subs.share.weight <- subsector <- supplysector <- technology <- thermal.building.service.input <- to.value <-
      value <- year <- year.fillout <- GCM <- NEcostPerService <- SRES <- SSP <- TRN_SSP <- base.building.size <-
      base.service <- building.node.input <- . <- GCAM_region_ID <- L244.Satiation_flsp_SSP1 <-
      L244.SatiationAdder_SSP1 <- L244.GenericServiceSatiation_SSP1 <- L244.Satiation_flsp_SSP2 <-
      L244.SatiationAdder_SSP2 <- L244.GenericServiceSatiation_SSP2 <- L244.Satiation_flsp_SSP3 <-
      L244.SatiationAdder_SSP3 <- L244.GenericServiceSatiation_SSP3 <-L244.Satiation_flsp_SSP4 <-
      L244.SatiationAdder_SSP4 <- L244.GenericServiceSatiation_SSP4 <- L244.Satiation_flsp_SSP5 <-
      L244.SatiationAdder_SSP5 <- L244.GenericServiceSatiation_SSP5 <-  L244.GenericServiceImpedance_SSP1 <-
      L244.GenericServiceImpedance_SSP2 <-L244.GenericServiceImpedance_SSP3 <- L244.GenericServiceImpedance_SSP4 <-
      L244.GenericServiceImpedance_SSP5 <-L244.GenericServiceAdder_SSP1 <- L244.GenericServiceAdder_SSP2 <-
      L244.GenericServiceAdder_SSP3 <- L244.GenericServiceAdder_SSP4 <- L244.GenericServiceAdder_SSP5 <-
      L244.GenericServiceCoef_SSP1 <- L244.GenericServiceCoef_SSP2 <-L244.GenericServiceCoef_SSP3 <-
      L244.GenericServiceCoef_SSP4 <-L244.GenericServiceCoef_SSP5 <-
      scenario <- L244.HDDCDD_A2_CCSM3x <-
      L244.HDDCDD_A2_HadCM3 <- L244.HDDCDD_B1_CCSM3x <- L244.HDDCDD_B1_HadCM3 <- L244.HDDCDD_constdd_no_GCM <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
    A_regions <- get_data(all_data, "energy/A_regions")
    A44.sector <- get_data(all_data, "energy/A44.sector", strip_attributes = TRUE)
    A44.subsector_interp <- get_data(all_data, "energy/A44.subsector_interp", strip_attributes = TRUE)
    A44.subsector_logit <- get_data(all_data, "energy/A44.subsector_logit", strip_attributes = TRUE)
    A44.subsector_shrwt <- get_data(all_data, "energy/A44.subsector_shrwt", strip_attributes = TRUE)
    A44.fuelprefElasticity <- get_data(all_data, "energy/A44.fuelprefElasticity", strip_attributes = TRUE)
    A44.globaltech_shrwt <- get_data(all_data, "energy/A44.globaltech_shrwt") %>%
      gather_years
    A44.gcam_consumer <- get_data(all_data, "energy/A44.gcam_consumer", strip_attributes = TRUE)
    A44.demandFn_serv <- get_data(all_data, "energy/A44.demandFn_serv", strip_attributes = TRUE)
    A44.demandFn_flsp <- get_data(all_data, "energy/A44.demandFn_flsp", strip_attributes = TRUE)
    A44.internal_gains <- get_data(all_data, "energy/A44.internal_gains")
    A44.satiation_flsp <- get_data(all_data, "energy/A44.satiation_flsp")
    A44.satiation_flsp_SSPs <- get_data(all_data, "energy/A44.satiation_flsp_SSPs")
    A44.demand_satiation_mult <- get_data(all_data, "energy/A44.demand_satiation_mult")
    A44.demand_satiation_mult_SSPs <- get_data(all_data, "energy/A44.demand_satiation_mult_SSPs")
    L144.flsp_bm2_R_res_Yh <- get_data(all_data, "L144.flsp_bm2_R_res_Yh", strip_attributes = TRUE)
    L144.flsp_bm2_R_comm_Yh <- get_data(all_data, "L144.flsp_bm2_R_comm_Yh", strip_attributes = TRUE)
    L144.base_service_EJ_serv <- get_data(all_data, "L144.base_service_EJ_serv", strip_attributes = TRUE)
    L144.base_service_EJ_serv_fuel <- get_data(all_data, "L144.base_service_EJ_serv_fuel", strip_attributes = TRUE)
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    L144.end_use_eff <- get_data(all_data, "L144.end_use_eff", strip_attributes = TRUE)
    L144.shell_eff_R_Y <- get_data(all_data, "L144.shell_eff_R_Y", strip_attributes = TRUE)
    L144.NEcost_75USDGJ <- get_data(all_data, "L144.NEcost_75USDGJ", strip_attributes = TRUE)
    L144.internal_gains <- get_data(all_data, "L144.internal_gains", strip_attributes = TRUE)
    L143.HDDCDD_scen_R_Y <- get_data(all_data, "L143.HDDCDD_scen_R_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y") # year comes in as double
    L144.flsp_param <- get_data(all_data, "L144.flsp_param", strip_attributes = TRUE)
    L144.prices_bld<-get_data(all_data, "L144.prices_bld", strip_attributes = TRUE)
    income_shares<-get_data(all_data, "socioeconomics/income_shares")
    n_groups<-nrow(unique(get_data(all_data, "socioeconomics/income_shares") %>%
                            select(category)))

    # Add a deflator for harmonizing GDPpc with prices
    def9075<-2.212

    # ===================================================
    # First, clean and prepare data on current and future income distribution projections
    L144.income_shares<-income_shares %>%
      filter(model %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_MODEL)) %>%
      select(-gini,-gdp_pcap_decile,-model) %>%
      rename(group = category,
             scen = sce,
             share = shares) %>%
    group_by(GCAM_region_ID,year,scen) %>%
      mutate(share_agg = sum(share)) %>%
      ungroup()

    # Check income shares are correct for all regions
    if((sum(L144.income_shares$share_agg) / nrow(L144.income_shares))-1 > 0.01){
      print("WARNING:income shares not correctly asigned")
    }

    L144.income_shares<-L144.income_shares %>%
      select(-share_agg)


    # Adjust gcam.consumer file to add the multiple consumers combining the raw file with multiple consumer information
    A44.gcam_consumer<-A44.gcam_consumer %>%
      filter(gcam.consumer == "resid") %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(A44.gcam_consumer %>% filter(gcam.consumer == "comm"))


    # Create the final dataset with subregional population and income shares
    L244.SubregionalShares <- write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                                   GCAM_region_names = GCAM_region_names) %>%
      # filter residential sector to implement multiple consumers
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
      mutate(inc.year.fillout = pop.year.fillout) %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID), by=c("region")) %>%
      left_join_error_no_match(L144.income_shares %>%
                                 filter(scen %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 rename(pop.year.fillout=year)
                                 ,by=c("GCAM_region_ID","group","pop.year.fillout")) %>%
      mutate(subregional.population.share = 1/n_groups) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      rename(subregional.income.share = share) %>%
      select(-scen,-GCAM_region_ID) %>%
      # bind commercial subregional population and income shares (currently not used, set to 1)
      bind_rows(write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                     GCAM_region_names = GCAM_region_names) %>%
                  filter(gcam.consumer == "comm") %>%
                  repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
                  mutate(inc.year.fillout = pop.year.fillout) %>%
                  mutate(subregional.population.share = 1,
                         subregional.income.share = 1))

   # Create a similar dataframe with all historical years
   # Used to create the historical subregional GDPpc dataframe
    L244.SubregionalShares_allhist<-write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                                         GCAM_region_names = GCAM_region_names) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      repeat_add_columns(tibble(pop.year.fillout=(HISTORICAL_YEARS))) %>%
      mutate(inc.year.fillout=pop.year.fillout) %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID), by=c("region")) %>%
      left_join_error_no_match(L144.income_shares %>%
                                 filter(scen %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 rename(pop.year.fillout=year)
                               ,by=c("GCAM_region_ID","group","pop.year.fillout")) %>%
      mutate(subregional.population.share=1/n_groups) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      rename(subregional.income.share=share) %>%
      select(-scen,-GCAM_region_ID) %>%
      bind_rows(write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                     GCAM_region_names = GCAM_region_names) %>%
                  filter(gcam.consumer=="comm") %>%
                  repeat_add_columns(tibble(pop.year.fillout=HISTORICAL_YEARS)) %>%
                  mutate(inc.year.fillout=pop.year.fillout) %>%
                  mutate(subregional.population.share = 1,
                         subregional.income.share = 1))

    # Generate subregional shares for diferent SSP scenarios
    L244.SubregionalShares_SSP <- write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                                   GCAM_region_names = GCAM_region_names) %>%
      # filter residential sector to implement multiple consumers
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
      mutate(inc.year.fillout = pop.year.fillout) %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID), by=c("region")) %>%
      repeat_add_columns(tibble(scen = unique(L144.income_shares$scen))) %>%
      filter(grepl("SSP",scen)) %>%
      filter(pop.year.fillout > MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L144.income_shares %>%
                                 rename(pop.year.fillout = year)
                                    ,by=c("GCAM_region_ID","group","pop.year.fillout","scen")) %>%
      mutate(subregional.population.share = 1/n_groups) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      rename(subregional.income.share = share,
             SSP = scen) %>%
      select(-GCAM_region_ID) %>%
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Share") %>%
          add_comments("GDP share for each income group within each region for all SSP scenarios") %>%
          add_precursors("common/GCAM_region_names","energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y",
                         "L101.Pop_thous_R_Yh","socioeconomics/income_shares")
      })

    # Assign each tibble in list
    for(i in names(L244.SubregionalShares_SSP)) {
      assign(paste0("L244.SubregionalShares_", i), L244.SubregionalShares_SSP[[i]] %>%
               add_title(paste0("GDP shares: ", i)) %>%
               add_legacy_name(paste0("L244.SubregionalShares_", i)))
    }


    # ===================================================
    # Using subregional population and income shares, create datasets with subregional population and per capita income for all GCAM regions:
    # Subregional population per region, year and consumer group
    L101.Pop_thous_R_Yh_gr <- L101.Pop_thous_R_Yh %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pop_thous = value) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_",unique(L144.income_shares$group)))) %>%
      mutate(pop_thous = pop_thous*(1/n_groups)) %>%
      bind_rows(L101.Pop_thous_R_Yh %>%
                  left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="GCAM_region_ID") %>%
                  mutate(gcam.consumer = "comm") %>%
                  rename(pop_thous = value) %>%
                  filter(year %in% HISTORICAL_YEARS))

    # Subregional per capita income per region, year and consumer group
    L102.pcgdp_thous90USD_Scen_R_Y_gr <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pcGDP_thous90USD = value) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("GCAM_region_ID","year")) %>%
      rename(pop_thous = value) %>%
      mutate(gdp = pcGDP_thous90USD * 1E3 * pop_thous * 1E3) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_",unique(L144.income_shares$group)))) %>%
      mutate(pop_thous = pop_thous * (1/n_groups)) %>%
      left_join_error_no_match(L244.SubregionalShares_allhist %>%
                                 select(-subregional.population.share,-pop.year.fillout) %>%
                                 rename(year = inc.year.fillout)
                               , by=c("region","gcam.consumer","year")) %>%
      mutate(gdp_gr = gdp *subregional.income.share,
             gdp_pc=(gdp_gr/1E3) / (pop_thous*1E3)) %>%
      select(-pcGDP_thous90USD) %>%
      rename(pcGDP_thous90USD = gdp_pc) %>%
      select(scenario,GCAM_region_ID,region,gcam.consumer,year,pcGDP_thous90USD) %>%
      # add commercial
      bind_rows(L102.pcgdp_thous90USD_Scen_R_Y %>%
                  left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="GCAM_region_ID") %>%
                  mutate(gcam.consumer = "comm") %>%
                  rename(pcGDP_thous90USD = value) %>%
                  filter(year %in% HISTORICAL_YEARS))

    # ===================================================
    # Demand function: Need some dataframes to specify floorspace and building energy demand for different gcam.consumers (resid/comm)
    # Demand for floorspace:L244.DemandFunction_flsp
    L244.DemandFunction_flsp <- write_to_all_regions(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]],
                                                     GCAM_region_names = GCAM_region_names) %>%
      filter(gcam.consumer == "resid") %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(write_to_all_regions(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]],
                                     GCAM_region_names = GCAM_region_names) %>% filter(gcam.consumer == "comm"))

    # Demand for building energy:L244.DemandFunction_serv
    L244.DemandFunction_serv <- write_to_all_regions(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]],
                                                     GCAM_region_names = GCAM_region_names) %>%
      filter(gcam.consumer == "resid") %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(write_to_all_regions(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]],
                                     GCAM_region_names = GCAM_region_names) %>% filter(gcam.consumer == "comm"))


    # Also need a price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains <- write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]],
                                                   GCAM_region_names = GCAM_region_names)

    # ===================================================
    # ===================================================
    # FLOORSPACE

    # 1- Residential floorspace

    # Filter residential gcam.consumer table
    A44.gcam_consumer_resid <- A44.gcam_consumer %>%
      filter(grepl("resid", gcam.consumer))


    # Using the parameters estimated in module LA144.building_det_flsp, calculate the "estimated" residential floorspace
    # These estimations will be used for calibration and for the calculation of the regional bias adder (bias-adjust-parameter) in those regions with observed historical data
    L244.Floorspace_resid_est<-L144.flsp_param %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_",unique(L144.income_shares$group)))) %>%
      left_join_error_no_match(GCAM_region_names, by="region") %>%
      repeat_add_columns(tibble(year=HISTORICAL_YEARS)) %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO), by=c("GCAM_region_ID","year","gcam.consumer","region")) %>%
      rename(gdp_pc = pcGDP_thous90USD) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by=c("GCAM_region_ID","year","gcam.consumer","region")) %>%
      mutate(flsp_pc_est = (`unadjust.satiation` + (-`land.density.param`*log(tot_dens))) * exp(-`b.param`
                                                                                        * exp(-`income.param` * log(gdp_pc)))) %>%
      mutate(flsp_est = flsp_pc_est * 1E-9 * pop_thous * 1E3)

    # Calculate the regional bias adder as the difference between observed (L144.flsp_bm2_R_res_Yh) and estimated (L244.Floorspace_resid_est) data
    # Allocate the adder equally across multiple consumers
    L244.Floorspace_resid_adder<-L244.Floorspace_resid_est %>%
      group_by(region,year) %>%
      summarise(flsp_est = sum(flsp_est)) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by="region") %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh, by = c("year","GCAM_region_ID")) %>%
      mutate(bias.adder = (value-flsp_est)/n_groups) %>%
      select(GCAM_region_ID,year,region,bias.adder) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_",unique(L144.income_shares$group))))

    # Combine observed data with the bias adder to obtain historical residential floorspace (BM2)
    L244.Floorspace_resid<-L244.Floorspace_resid_est %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, flsp_est) %>%
      left_join_error_no_match(L244.Floorspace_resid_adder, by = c("region", "gcam.consumer","year")) %>%
      mutate(base.building.size = flsp_est + bias.adder) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)


    # Get base per capita floorspace, which is used in future residential floorspace estimation (adjustment to avoid "destruction of buildings")
    # This is added to the final table that gathers all the parameters for the Gompertz function (L244.GompFnParam)
    L244.Base_pcFlsp<-L244.Floorspace_resid %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by = c("region", "gcam.consumer", "year")) %>%
      mutate(base.pcFlsp = (base.building.size * 1E9) / (pop_thous * 1E3)) %>%
      select(-base.building.size,-pop_thous)

    # Also need the regional bias adder per capita in the final calibration year to keep it to future periods
    # This is added to the final table that gathers all the parameters for the Gompertz function (L244.GompFnParam)
    L244.Flsp_BiasAdder<-L244.Floorspace_resid_est %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      group_by(GCAM_region_ID,region,year) %>%
      summarise(flsp_est=sum(flsp_est)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(bm2_adder = round(value- flsp_est,9)) %>%
      select(-value,-flsp_est) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("GCAM_region_ID", "year")) %>%
      rename(pop_thous = value) %>%
      mutate(bias.adjust.param = (bm2_adder*1E9)/(pop_thous*1E3))

    # The following table puts together all the parameters that will be used in the estimation of future residential floorspace
    L244.GompFnParam<-L144.flsp_param %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_",unique(L144.income_shares$group)))) %>%
      left_join_error_no_match(GCAM_region_names, by="region") %>%
      mutate(year=MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Base_pcFlsp, by = c("region", "gcam.consumer", "GCAM_region_ID", "year")) %>%
      left_join_error_no_match(L244.Flsp_BiasAdder, by = c("region", "GCAM_region_ID", "year")) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      select(LEVEL2_DATA_NAMES[["GompFnParam"]])

    #----------------------------------------------------------------------
    # 2- Commercial floorspace

    # Filter commercial gcam.consumer table
    A44.gcam_consumer_comm <- A44.gcam_consumer %>%
      filter(grepl("comm", A44.gcam_consumer$gcam.consumer))

    # Format L144.flsp_bm2_R_comm_Yh (commercial floorspace)
    L244.Floorspace_comm <- L144.flsp_bm2_R_comm_Yh %>%
      mutate(base.building.size = round(value, energy.DIGITS_FLOORSPACE)) %>%
      select(-value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(gcam.consumer = A44.gcam_consumer_comm$gcam.consumer,
             nodeInput = A44.gcam_consumer_comm$nodeInput,
             building.node.input = A44.gcam_consumer_comm$building.node.input) %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)

    #-------------
    # NOTE: Combine residential and commercial floorspace to produce a dataset with "total" floorspace (L244.Floorspace)
    L244.Floorspace <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(base.building.size = round(base.building.size,energy.DIGITS_FLOORSPACE))

    #-------------

    # Commercial floorspace uses the satiation demand function, so the following code estimates the satiation level, impedance, and adder, required for the satiation function.
    # Different satiation levels assumed for different regions, classified in "region classes"
    L244.Satiation_flsp_class <- A44.satiation_flsp %>%
      gather(sector, value, resid, comm) %>%
      # Converting from square meters per capita to million square meters per capita
      mutate(satiation.level = value * CONV_THOUS_BIL) %>%
      select(-value)

    # Based on these classes, write the satiation level for all GCAM regions
    L244.Satiation_flsp <- write_to_all_regions(A44.gcam_consumer_comm, c("region", "gcam.consumer", "nodeInput", "building.node.input"), # replace with LEVEL2_DATA_NAMES[["BldNodes]]
                                                GCAM_region_names = GCAM_region_names) %>%
      # Match in the region class, and use this to then match in the satiation floorspace
      left_join_error_no_match(A_regions %>% select(region, region.class),
                               by = "region") %>%
      left_join_error_no_match(L244.Satiation_flsp_class, by = c("region.class", "gcam.consumer" = "sector")) %>%
      select(LEVEL2_DATA_NAMES[["Satiation_flsp"]])


    # Extend the analysis to SSP assumptions
    L244.Satiation_flsp_class_SSPs <- A44.satiation_flsp_SSPs %>%
      gather(sector, value, resid, comm) %>%
      mutate(satiation.level = value * CONV_THOUS_BIL)


    L244.Satiation_flsp_SSPs <- write_to_all_regions(A44.gcam_consumer_comm, c("region", "gcam.consumer", "nodeInput", "building.node.input"), # replace with LEVEL2_DATA_NAMES[["BldNodes]]
                                                     GCAM_region_names = GCAM_region_names) %>%
      repeat_add_columns(tibble(SSP = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
      # Match in the region class, and use this to then match in the satiation floorspace
      left_join_error_no_match(A_regions %>% select(region, region.class), by = "region") %>%
      left_join_error_no_match(L244.Satiation_flsp_class_SSPs, by = c("SSP", "region.class", "gcam.consumer" = "sector")) %>%
      # Calculate pcFlsp and make sure it is smaller than the satiation level
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>%
                                 # This used to be filtered to energy.SATIATION_YEAR, changed to pass timeshift test
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "SSP" = "scenario","gcam.consumer")) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by = c("GCAM_region_ID", "year", "region","gcam.consumer")) %>%
      mutate(pcFlsp_mm2 = base.building.size / pop_thous,
             satiation.level = if_else(pcFlsp_mm2 > satiation.level, 1.001 * pcFlsp_mm2, satiation.level))

    L244.Satiation_flsp_SSPs.split <- L244.Satiation_flsp_SSPs%>%
      select(region, gcam.consumer, nodeInput, building.node.input, satiation.level, SSP) %>%
      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Million squared meters per capita") %>%
          add_comments("Values from A44.satiation_flsp_SSPs added to A44.gcam_consumer written to all regions") %>%
          add_comments("Then, per-capita floorspace calculated to ensure satiation levels above per-capita floorspace") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.Satiation_flsp_SSPs.split)) {
      assign(paste0("L244.Satiation_flsp_", i), L244.Satiation_flsp_SSPs.split[[i]] %>%
               add_title(paste0("Floorspace demand satiation: ", i)) %>%
               add_legacy_name(paste0("L244.Satiation_flsp_", i)))
    }



    #------------------------------------------------------
    # Calibrate satiation impedance (in the DS) per GCAM region.
    # sat_impedance = (-ln(2)/ln((satiation level - pcFlsp2015)/(satiation level - satiation adder))) * pcGDP2015

    L144.Satiation_impedance_pre<-L244.Satiation_flsp %>%
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID,region),by="region") %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      # Add base floorspace
      left_join_error_no_match(L244.Floorspace,by=c("region","gcam.consumer","nodeInput","building.node.input","year")) %>%
      rename(flsp_bm2 = base.building.size) %>%
      group_by(GCAM_region_ID,region,year,nodeInput,building.node.input) %>%
      summarise(satiation.level = mean(satiation.level),
                flsp_bm2 = sum(flsp_bm2)) %>%
      ungroup() %>%
      # Add population to calculate floorspace per capita
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("GCAM_region_ID","year")) %>%
      rename(pop_thous = value) %>%
      mutate(flsp_pc = (flsp_bm2*1E9) / (pop_thous*1E3)) %>%
      # Add GDPpc
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                               by=c("GCAM_region_ID","year")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Change units satiation level
      mutate(satiation.level = satiation.level * 1E6) %>%
      # Calculate satiation impedance
      mutate(`satiation-impedance` = (-log(2)/log((satiation.level - flsp_pc) / (satiation.level))) * pcGDP_thous90USD) %>%
      mutate(`satiation-impedance`= round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE)) %>%
      select(region,nodeInput,building.node.input,`satiation-impedance`)


    L244.Satiation_impedance<-L144.Satiation_impedance_pre %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationImpedance"]])


    # Satiation impedance for SSPs
    L144.Satiation_impedance_SSPs_pre<-L244.Satiation_flsp_SSPs %>%
      mutate(gdp = pcGDP_thous90USD * 1E3 * pop_thous * 1E3,
             pop = pop_thous * 1E3) %>%
      group_by(GCAM_region_ID,region,year,nodeInput,building.node.input,SSP) %>%
      summarise(value = mean(value),
                gdp = sum(gdp),
                pop = sum(pop),
                base.building.size = sum(base.building.size)) %>%
      ungroup() %>%
      mutate(pcGDP_thous90USD = (gdp/pop) / 1E3,
             flsp_pc = base.building.size * 1E9 / pop) %>%
      # There are some cases where assumed satiation level is below the 2015 pcflsp value.
      # This generates problems for calibration of the satiation impedance, so it needs to be corrected.
      filter(flsp_pc <= value) %>%
      mutate(`satiation-impedance`= (-log(2)/log((value - flsp_pc)/ (value))) * pcGDP_thous90USD) %>%
      bind_rows(L244.Satiation_flsp_SSPs %>%
                  mutate(gdp = pcGDP_thous90USD * 1E3 * pop_thous * 1E3,
                         pop = pop_thous * 1E3) %>%
                  group_by(GCAM_region_ID,region,year,nodeInput,building.node.input,SSP) %>%
                  summarise(value = mean(value),
                            gdp = sum(gdp),
                            pop = sum(pop),
                            base.building.size = sum(base.building.size)) %>%
                  ungroup() %>%
                  mutate(pcGDP_thous90USD = (gdp/pop) / 1E3,
                         flsp_pc = base.building.size * 1E9 / pop) %>%
                  # There are some cases where assumed satiation level is below the 2015 pcflsp value.
                  # This generates problems for calibration of the satiation impedance, so it needs to be corrected.
                  filter(flsp_pc > value) %>%
                  mutate(`satiation-impedance`= NaN)) %>%
      mutate(`satiation-impedance`=round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE)) %>%
      select(region,nodeInput,building.node.input,`satiation-impedance`,SSP) %>%
      # substitute NaN: use approx_fun per ssp
      mutate(year = as.numeric(gsub("SSP","",SSP))) %>%
      select(-SSP) %>%
      group_by(region,nodeInput) %>%
      mutate(`satiation-impedance` = if_else(is.nan(`satiation-impedance`), approx_fun(year, `satiation-impedance`, rule = 2), `satiation-impedance`)) %>%
      ungroup() %>%
      rename(SSP = year) %>%
      mutate(SSP = paste0("SSP",SSP))

    L244.Satiation_impedance_SSPs<-L144.Satiation_impedance_SSPs_pre %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationImpedance"]], SSP)

    L244.Satiation_impedance_SSPs.split<-L244.Satiation_impedance_SSPs %>%
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("Satiation impedance computed using satiation level, per-capita GDP and per-capita floorsapce") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.Satiation_impedance_SSPs.split)) {
      assign(paste0("L244.Satiation_impedance_", i), L244.Satiation_impedance_SSPs.split[[i]] %>%
               add_title(paste0("Floorspace satiation impedance: ", i)) %>%
               add_legacy_name(paste0("L244.Satiation_impedance_", i)))
    }

    #------------------------------------------------------
    # Calibrate satiation adder (in the DS) per GCAM region
    # NOTE: The updated satiation adder it only acts as a "bias-correction" parameter to allow the model calibrate 2015,
    # by capturing the difference between the observed and the estimated values
    # At this point, with a single representative consumer in the commercial sector, the adder is going to be zero,
    # but this structure allows a future implementation of multiple consumers in the commercial sector

    # First, check that at region level observed and estimated floorspace match in 2015: (satiation adder should be equal to zero)
    L244.SatiationAdder_checkReg<- L244.Satiation_flsp %>%
      mutate(satiation.level = satiation.level * 1E6) %>%
      left_join_error_no_match(L244.Satiation_impedance,by = c("region", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      # aggregate to region level
      group_by(region,nodeInput,building.node.input) %>%
      summarise(satiation.level = mean(satiation.level),
                `satiation-impedance`= mean(`satiation-impedance`)) %>%
      ungroup() %>%
      mutate(year = 2015) %>%
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID,region),by = "region") %>%
      left_join_error_no_match(bind_rows(L144.flsp_bm2_R_res_Yh %>% mutate(nodeInput = "resid"),
                                         L144.flsp_bm2_R_comm_Yh %>% mutate(nodeInput = "comm")),
                               by=c("GCAM_region_ID","year","nodeInput")) %>%
      rename(observed_flsp_bm2 = value) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID")) %>%
      rename(pop_thous = value) %>%
      mutate(observed_pcflsp = observed_flsp_bm2*1E9 / (pop_thous*1E3)) %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),by = c("year", "GCAM_region_ID")) %>%
      rename(pcGDP_thous90USD = value) %>%
      mutate(est_pcflsp = satiation.level * (1-exp(-log(2)*pcGDP_thous90USD/`satiation-impedance`))) %>%
      mutate(satiation.adder = round(observed_pcflsp-est_pcflsp,2))

    # Print an error if any adder is not equal to zero
    if(any(L244.SatiationAdder_checkReg$satiation.adder>0.05)==T){
      print("Error: Observed and estimated values do not match")
    }

    # Then, calculate the satiation adder
    L244.SatiationAdder<- L244.Satiation_flsp %>%
      mutate(satiation.level = satiation.level * 1E6) %>%
      left_join_error_no_match(L244.Satiation_impedance,by = c("region", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID,region),by = "region") %>%
      left_join_error_no_match(L244.Floorspace,by=c("region","year","gcam.consumer", "nodeInput", "building.node.input")) %>%
      rename(observed_flsp_bm2 = base.building.size) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by = c("year", "GCAM_region_ID","gcam.consumer","region")) %>%
      rename(pop_thous = value) %>%
      mutate(observed_pcflsp = observed_flsp_bm2*1E9 / (pop_thous*1E3)) %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),by = c("year", "GCAM_region_ID","region","gcam.consumer")) %>%
      rename(pcGDP_thous90USD = value) %>%
      mutate(est_pcflsp = satiation.level * (1-exp(-log(2)*pcGDP_thous90USD/`satiation-impedance`)),
             est_flsp_bm2 = (est_pcflsp*pop_thous*1E3) / 1E9) %>%
      group_by(region,nodeInput,building.node.input,year) %>%
      summarise(pop_thous = sum(pop_thous),
                est_flsp_bm2 = sum(est_flsp_bm2),
                observed_flsp_bm2 = sum(observed_flsp_bm2)) %>%
      ungroup() %>%
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID,region),by="region") %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID")) %>%
      mutate(est_flsp_bm2 = round(est_flsp_bm2,3),
             observed_flsp_bm2 = round(observed_flsp_bm2,3)) %>%
      mutate(satiation.adder = ((observed_flsp_bm2-est_flsp_bm2)*1E9) / (pop_thous*1E3)) %>%
      select(region,nodeInput,building.node.input,year,satiation.adder) %>%
      mutate(satiation.adder = round(satiation.adder,energy.DIGITS_SATIATION_ADDER)) %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # Repeat the process for SSPs
    L244.SatiationAdder_SSPs <- L244.Satiation_flsp_SSPs %>%
      select(-satiation.level) %>%
      rename(satiation.level = value) %>%
      left_join_error_no_match(L244.Satiation_impedance_SSPs,by = c("region", "gcam.consumer", "nodeInput", "building.node.input","SSP")) %>%
      mutate(year = 2015) %>%
      rename(observed_flsp_bm2 = base.building.size) %>%
      mutate(observed_pcflsp = observed_flsp_bm2*1E9 / (pop_thous*1E3)) %>%
      mutate(est_pcflsp = satiation.level * (1-exp(-log(2)*pcGDP_thous90USD/`satiation-impedance`)),
             est_flsp_bm2 = (est_pcflsp*pop_thous*1E3) / 1E9) %>%
      group_by(region,nodeInput,building.node.input,year,SSP) %>%
      summarise(pop_thous = sum(pop_thous),
                est_flsp_bm2 = sum(est_flsp_bm2),
                observed_flsp_bm2 = sum(observed_flsp_bm2)) %>%
      ungroup() %>%
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID,region),by="region") %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID")) %>%
      mutate(est_flsp_bm2=round(est_flsp_bm2,3),
             observed_flsp_bm2=round(observed_flsp_bm2,3)) %>%
      mutate(satiation.adder = ((observed_flsp_bm2-est_flsp_bm2)*1E9) / (pop_thous*1E3)) %>%
      select(region,nodeInput,building.node.input,year,satiation.adder,SSP) %>%
      mutate(gcam.consumer = nodeInput) %>%
      mutate(satiation.adder = round(satiation.adder,energy.DIGITS_SATIATION_ADDER)) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]], SSP) %>%
      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("Satiation adder compute using satiation level, per-capita GDP and per-capita floorsapce") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.SatiationAdder_SSPs)) {
      assign(paste0("L244.SatiationAdder_", i), L244.SatiationAdder_SSPs[[i]] %>%
               add_title(paste0("Satiation adders in floorspace demand function: ", i)) %>%
               add_legacy_name(paste0("L244.SatiationAdder_", i)))
    }


    # ===================================================
    # ===================================================
    # BUILDING ENERGY

    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.internal_gains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    # Supplysectors (e.g. heating) need to be differentiated for each consumer group.
    # This makes that the fuel-technology mix for each service can vary across groups.
    # We create the "add.cg" ("add consumer groups") function to make this process automatic for the different files

    cons.groups<-unique(A44.gcam_consumer_resid$gcam.consumer)
    n.cons.groups<-as.numeric(length(unique(A44.gcam_consumer_resid$gcam.consumer)))

    add.cg<-function(df){
      df.res<-df %>% filter(grepl("resid",supplysector))
      df.comm<-df %>% filter(grepl("comm",supplysector))

      df<- df.res %>%
        repeat_add_columns(tibble::tibble(cons.groups)) %>%
        separate(cons.groups,c("sector","cons.groups"),sep="_") %>%
        unite(supplysector,c(supplysector,cons.groups), sep="_") %>%
        select(-sector) %>%
        bind_rows(df.comm)
      return(df)
    }

    # Transform input data files (A44.*) in order to adjust to consumer groups
    A44.fuelprefElasticity<-add.cg(A44.fuelprefElasticity)

    A44.globaltech_shrwt<-add.cg(A44.globaltech_shrwt)
    A44.internal_gains<-add.cg(A44.internal_gains)
    A44.sector<-add.cg(A44.sector)
    A44.subsector_interp<-add.cg(A44.subsector_interp)
    A44.subsector_logit<-add.cg(A44.subsector_logit)
    A44.subsector_shrwt<-add.cg(A44.subsector_shrwt)

    # Adjust calibrated techs in a different file
    calibrated_techs_bld_det_adj<-calibrated_techs_bld_det %>%
      filter(grepl("resid",supplysector)) %>%
      repeat_add_columns(tibble::tibble(cons.groups)) %>%
      separate(cons.groups,c("adj","cons.groups"),sep = "_") %>%
      unite(supplysector,c(supplysector,cons.groups), sep = "_", remove = F) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",cons.groups)) %>%
      select(-adj,-cons.groups) %>%
      bind_rows(calibrated_techs_bld_det %>% filter(grepl("comm",supplysector)))

    #------------------------------------------------------
    # BASE SERVICE: L244.GenericBaseService and L244.ThermalBaseService - Base year output of buildings services (per unit floorspace)
    # First, generate L244.base_service, and filter only the model base years and change names as indicated in calibrated_techs_bld_det
    L244.base_service <- L144.base_service_EJ_serv %>%
      rename(base.service = value) %>%
      mutate(base.service = round(base.service, energy.DIGITS_CALOUTPUT)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_keep_first_only(calibrated_techs_bld_det, by = c("sector", "service")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input = supplysector, year, base.service)

    # Separate thermal and generic services into separate tibbles
    L244.GenericBaseService <- L244.base_service %>%
      filter(building.service.input %in% generic_services) %>%
      complete(nesting(region,year), building.service.input = c(building.service.input, generic_services)) %>%
      mutate(gcam.consumer = if_else(grepl("resid",building.service.input),"resid","comm"),
             nodeInput = if_else(grepl("resid",building.service.input),"resid","comm"),
             building.node.input = if_else(grepl("resid",building.service.input),"resid_building","comm_building")) %>%
      replace_na(list(base.service=0)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    L244.ThermalBaseService <- L244.base_service %>%
      filter(building.service.input %in% thermal_services) %>%
      rename(thermal.building.service.input = building.service.input) %>%
      complete(nesting(region,year), thermal.building.service.input = c(thermal.building.service.input, thermal_services)) %>%
      mutate(gcam.consumer = if_else(grepl("resid",thermal.building.service.input),"resid","comm"),
             nodeInput = if_else(grepl("resid",thermal.building.service.input),"resid","comm"),
             building.node.input = if_else(grepl("resid",thermal.building.service.input),"resid_building","comm_building")) %>%
      replace_na(list(base.service=0)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    #------------------------------------------------------
    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.all_sres_gcm <- unique(L143.HDDCDD_scen_R_Y[c("SRES", "GCM")]) # These HDD/CDD scenarios are pretty old, should be updated eventually

    L244.HDDCDD_scen_R_Y <- L143.HDDCDD_scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    # Let's make a climate normal (average climate conditions) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    L244.HDDCDD_normal_R_Y <- L244.HDDCDD_scen_R_Y %>%
      filter(year %in% energy.CLIMATE_NORMAL_YEARS,
             SRES == L244.all_sres_gcm$SRES[1],
             GCM == L244.all_sres_gcm$GCM[1]) %>%
      group_by(region, variable) %>%
      summarise(degree.days = mean(value)) %>%
      ungroup()

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    # Add HDDCDD for L244.ThermalBaseService categories and model years
    L244.HDDCDD_pre <- L244.ThermalBaseService %>%
      select(-year, -base.service) %>%
      distinct() %>%
      # Add model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add SRES and GCM variables
      repeat_add_columns(L244.all_sres_gcm) %>%
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add HDD and CDD
      left_join_error_no_match(L244.HDDCDD_scen_R_Y, by = c("region", "SRES", "GCM", "variable", "year")) %>%
      mutate(degree.days = round(value, energy.DIGITS_HDDCDD)) %>%
      select(-variable, -value, -GCAM_region_ID) %>%
      # Join SRES and GCM so that we can split by unique ESM scenario
      unite(scenario, SRES, GCM) %>%
      # add consumer groups:
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(L244.ThermalBaseService %>%
                  select(-year, -base.service) %>%
                  distinct() %>%
                  # Add model years
                  repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
                  # Add SRES and GCM variables
                  repeat_add_columns(L244.all_sres_gcm) %>%
                  mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
                  # Add HDD and CDD
                  left_join_error_no_match(L244.HDDCDD_scen_R_Y, by = c("region", "SRES", "GCM", "variable", "year")) %>%
                  mutate(degree.days = round(value, energy.DIGITS_HDDCDD)) %>%
                  select(-variable, -value, -GCAM_region_ID) %>%
                  # Join SRES and GCM so that we can split by unique ESM scenario
                  unite(scenario, SRES, GCM) %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.HDDCDD_comm<-L244.HDDCDD_pre %>%
      filter(grepl("comm",gcam.consumer))

    L244.HDDCDD_res<-L244.HDDCDD_pre %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer, c("adj","group"), sep="_", remove = F) %>%
      unite(thermal.building.service.input, c("thermal.building.service.input","group"), sep = "_") %>%
      select(-adj)


    L244.HDDCDD<-bind_rows(L244.HDDCDD_res,L244.HDDCDD_comm) %>%
      split(.$scenario) %>%
      lapply(function(df) {
        select(df, -scenario) %>%
          add_units("Fahrenheit Degree Days") %>%
          add_comments("Degree days are from L143.HDDCDD_scen_R_Y") %>%
          add_precursors("L143.HDDCDD_scen_R_Y", "common/GCAM_region_names",
                         "energy/A44.internal_gains", "energy/A44.sector", "L144.base_service_EJ_serv",
                         "energy/calibrated_techs_bld_det")
      })

    # Assign each tibble in list
    for(i in names(L244.HDDCDD)) {
      assign(paste0("L244.HDDCDD_", i), L244.HDDCDD[[i]] %>%
               add_title(paste0("Heating and cooling degree days: ", i)) %>%
               add_legacy_name(paste0("L244.HDDCDD_", i)))
    }

    #------------------------------------------------------
    # L244.GenericServiceSatiation: Satiation levels assumed for non-thermal building services
    # First, calculate the service output per unit floorspace in the USA region
    L244.ServiceSatiation_USA_pre <- L144.base_service_EJ_serv %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      # Add tradBio which is not consumed in USA
      bind_rows(L144.base_service_EJ_serv %>%
                  filter(grepl("TradBio",service)) %>%
                  select(-GCAM_region_ID,-value) %>%
                  distinct() %>%
                  mutate(GCAM_region_ID = gcam.USA_CODE,
                         value = 0)) %>%
      # Using left_join_keep_first_only b/c there are repeats and we only need to keep generic columns
      left_join_keep_first_only(calibrated_techs_bld_det, by = c("sector", "service")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, sector, service, gcam.consumer, nodeInput, building.node.input, building.service.input = supplysector, year, value) %>%
      left_join_error_no_match(A44.demand_satiation_mult %>%
                                 select(supplysector,agg.service) %>%
                                 rename(service=supplysector), by = "service") %>%
      group_by(region, sector, agg.service, year) %>%
      mutate(agg.value = sum(value)) %>%
      ungroup()

    # Floorspace should be matched in for a specified year, from the full floorspace table (i.e. not one that is subsetted to model base years)
    L144.flsp_bm2_R_res_Yh <- L144.flsp_bm2_R_res_Yh %>%
      mutate(gcam.consumer = "resid") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L144.flsp_bm2_R_comm_Yh <- L144.flsp_bm2_R_comm_Yh %>%
      mutate(gcam.consumer = "comm") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L244.flsp_bm2_R <- bind_rows(L144.flsp_bm2_R_res_Yh, L144.flsp_bm2_R_comm_Yh) %>%
      # Again, used to be energy.SATIATION_YEAR, changed to pass timeshift test
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year)

    L244.ServiceSatiation_USA <- L244.ServiceSatiation_USA_pre %>%
      left_join_error_no_match(L244.flsp_bm2_R %>%
                                 rename(floorspace_bm2 = value), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector","agg.service")) %>%
      group_by(region, sector, agg.service) %>%
      mutate(satiation.level = round(agg.value[year == max(HISTORICAL_YEARS)] * multiplier / floorspace_bm2, energy.DIGITS_CALOUTPUT)) %>%
      ungroup() %>%
      select(-multiplier)

    # Generic services: read these values to all regions because they're all the same
    L244.GenericServiceSatiation <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% generic_services) %>%
      select(-region, -GCAM_region_ID, -year, -value, -agg.value) %>%
      distinct() %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], GCAM_region_names = GCAM_region_names)

    # Need to match in the floorspace into the base service table, divide to calculate the service demand
    # per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
    L244.BS <- L244.GenericBaseService %>%
      left_join_error_no_match(L244.Floorspace %>%
                                 group_by(region, nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               , by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      mutate(service.per.flsp = base.service / base.building.size) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input, service.per.flsp)

    L244.GenericServiceSatiation<- L244.GenericServiceSatiation %>%
      # not all combinations so use left_join
      left_join(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
      replace_na(list(service.per.flsp=0)) %>%
      filter(grepl("modern",building.service.input) | grepl("comm",building.service.input)) %>%
      bind_rows(L244.GenericServiceSatiation %>%
                  # not all combinations so use left_join
                  left_join(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
                  replace_na(list(service.per.flsp=0)) %>%
                  filter(grepl("coal",building.service.input) | grepl("TradBio",building.service.input))%>%
                  filter(service.per.flsp != 0)) %>%
      mutate(satiation.level = pmax(satiation.level, service.per.flsp * 1.0001)) %>%
      # extend the satiation to non existing sectors:
      group_by(building.service.input) %>%
      mutate(satiation.level.adj = mean(satiation.level,na.rm=T)) %>%
      mutate(satiation.level= if_else(is.na(satiation.level),satiation.level.adj,satiation.level)) %>%
      select(-service.per.flsp,-satiation.level.adj)

    # L244.GenericServiceSatiation_SSPs: Satiation levels assumed for non-thermal building services in the SSPs
    # First, calculate the service output per unit floorspace in the USA region
    L244.ServiceSatiation_USA_SSPs <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% generic_services) %>%
      repeat_add_columns(tibble(SSP = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
      left_join_error_no_match(A44.demand_satiation_mult_SSPs, by = c("SSP", "building.service.input" = "supplysector","agg.service")) %>%
      group_by(region, sector, agg.service) %>%
      mutate(satiation.level = round(agg.value[year == max(HISTORICAL_YEARS)] * multiplier / floorspace_bm2, energy.DIGITS_CALOUTPUT)) %>%
      ungroup()

    # Generic services: read these values to all regions because they're all the same
    L244.GenericServiceSatiation_SSPs <- L244.ServiceSatiation_USA_SSPs %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], SSP, -region) %>%
      distinct() %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], "SSP"), GCAM_region_names = GCAM_region_names) %>%
      arrange(SSP,building.service.input) %>%
      left_join(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
      replace_na(list(service.per.flsp=0)) %>%
      filter(grepl("modern",building.service.input) | grepl("comm",building.service.input)) %>%
      bind_rows(L244.ServiceSatiation_USA_SSPs %>%
                  filter(year == max(MODEL_BASE_YEARS)) %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], SSP, -region) %>%
                  distinct() %>%
                  write_to_all_regions(c(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], "SSP"), GCAM_region_names = GCAM_region_names) %>%
                  # not all combinations so use left_join
                  left_join(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
                  replace_na(list(service.per.flsp=0)) %>%
                  filter(grepl("coal",building.service.input) | grepl("TradBio",building.service.input)) %>%
                  filter(service.per.flsp != 0)) %>%
      mutate(satiation.level = pmax(satiation.level, service.per.flsp * 1.0001)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], SSP)


    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    # Write USA thermal satiation levels to all regions
    L244.ThermalServiceSatiation <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% thermal_services) %>%
      select(-region, -GCAM_region_ID, -value, -year,-agg.value) %>%
      distinct() %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], GCAM_region_names = GCAM_region_names) %>%
      rename(thermal.building.service.input = building.service.input) %>%
      # Thermal service satiation is modified in each region according to the HDD/CDD ratio to the USA in a given year
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      left_join_error_no_match(L244.HDDCDD_normal_R_Y, by = c("region", "variable")) %>%
      group_by(gcam.consumer, variable) %>%
      mutate(satiation.level = round(satiation.level * degree.days / degree.days[region == gcam.USA_REGION], digits = energy.DIGITS_CALOUTPUT)) %>%
      ungroup()

    # The service satiation in the final cal year can not be lower than the observed demand, so need to use pmax to set a floor on the quantity
    # First need to calculate the maximum quantities of demand over the historical time period, expressed per unit floorspace
    L244.tmp_pre <- L244.ThermalBaseService %>%
      left_join_error_no_match(L244.Floorspace %>%
                                 group_by(region, nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               , by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      mutate(service.per.flsp = base.service / base.building.size)

    # Adjustment for USA: need to keep coal heating, which disappears from 1990.
    # Take the maximum service per flsp in USA (1975)
    L244.tmp_pre_usa<-L244.tmp_pre %>% filter(region == "USA",thermal.building.service.input == "resid heating coal")
    usa.serv.perFlsp.coal<-max(L244.tmp_pre_usa$service.per.flsp)

    L244.tmp<-L244.tmp_pre %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-base.service, - base.building.size, -year) %>%
      mutate(service.per.flsp = if_else(region == "USA" & thermal.building.service.input == "resid heating coal",usa.serv.perFlsp.coal,service.per.flsp))

    # Then, match in this quantity into the thermal service satiation and take the max
    L244.ThermalServiceSatiation <- L244.ThermalServiceSatiation %>%
      left_join(L244.tmp, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "thermal.building.service.input")) %>%
      mutate(satiation.level = round(pmax(satiation.level, service.per.flsp * 1.0001),
                                     digits = energy.DIGITS_CALOUTPUT)) %>%
      filter(grepl("modern",thermal.building.service.input) | grepl("comm",thermal.building.service.input)) %>%
      bind_rows(L244.ThermalServiceSatiation %>%
                  left_join(L244.tmp, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "thermal.building.service.input")) %>%
                  mutate(satiation.level = round(pmax(satiation.level, service.per.flsp * 1.0001),
                                                 digits = energy.DIGITS_CALOUTPUT)) %>%
                  filter(grepl("coal",thermal.building.service.input) | grepl("TradBio",thermal.building.service.input)) %>%
                  filter(service.per.flsp != 0)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    #------------------------------------------------------
    # L244.ShellConductance_bld: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld <- L144.shell_eff_R_Y %>%
      rename(shell.conductance = value) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(shell.conductance = round(shell.conductance, digits = energy.DIGITS_EFFICIENCY)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 #mutate(internal.gains.market.name=paste0(nodeInput,"-internal-gains-trial-market")) %>%
                                 distinct() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               , by = c("supplysector" = "gcam.consumer")) %>%
      mutate(gcam.consumer = supplysector,
             shell.year = year,
             floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # L244.Supplysector_bld: Supplysector info for buildings
    L244.Supplysector_bld <- write_to_all_regions(A44.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                                                  GCAM_region_names = GCAM_region_names)

    # L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    L244.FinalEnergyKeyword_bld <- write_to_all_regions(A44.sector, c(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]]),
                                                        GCAM_region_names = GCAM_region_names) %>%
      na.omit()

    # Subsector information
    ## Not all subsectors exist in all regions; tradbio and heat are only modeled in selected regions
    ## The level1 end-use tech efficiency file has all of the combinations that exist
    L244.Tech_bld <- add.cg(L144.end_use_eff) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, technology) %>%
      distinct()

    # L244.SubsectorLogit_bld: Subsector logit exponents of building sector
    L244.SubsectorLogit_bld <- write_to_all_regions(A44.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                                                    GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))


    # L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(A44.subsector_shrwt$year))) {
      L244.SubsectorShrwt_bld <- A44.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.subsector_shrwt$year.fillout))) {
      L244.SubsectorShrwtFllt_bld <- A44.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }

    # L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector
    if(any(is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterp_bld <- A44.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }
    if(any(!is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterpTo_bld <- A44.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))
    }

    # Adjust interpolation rule to promote electricity penetration in developing economies
    # There are some regions in which electric heating needs to be promoted due to very low values in base years:
    elec_adj<-L144.base_service_EJ_serv_fuel %>%
      filter(year == 2015, fuel == "electricity", grepl("resid heating", service)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh %>% rename(flsp_bm2 = value),
                                by = c("GCAM_region_ID", "year", "region")) %>%
      mutate(value_gj_m2 = value / flsp_bm2) %>%
      arrange(value_gj_m2) %>%
      filter(value_gj_m2 <= 1e-03) %>%
      # take out Indonesia because it does not include heating demand
      filter(region != "Indonesia")

    elec_heat_regions<-unique(elec_adj$region)


    L244.SubsectorInterp_bld<-L244.SubsectorInterp_bld %>%
      mutate(to.value = 1) %>%
      mutate(interpolation.function = if_else(grepl("resid heating modern", supplysector) &
                                              region %in% elec_heat_regions &
                                              subsector %in% c("electricity"),
                                              "linear", interpolation.function),
             to.year = if_else(grepl("resid heating modern", supplysector) &
                                                region %in% elec_heat_regions &
                                                subsector %in% c("electricity"),
                                                2050, to.year)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])


    # L244.FuelPrefElast_bld: Fuel preference elasticities for buildings
    L244.FuelPrefElast_bld <- A44.fuelprefElasticity %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FuelPrefElast"]], GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))

    # L244.StubTech_bld: Identification of stub technologies for buildings
    L244.StubTech_bld <- L244.Tech_bld %>%
      rename(stub.technology = technology)

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    L244.StubTechCalInput_bld_pre <- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(calibrated.value = value) %>%
      mutate(calibrated.value = round(calibrated.value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_bld_det, by = c("sector", "service", "fuel")) %>%
      mutate(share.weight.year = year,
             stub.technology = technology) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(subs.share.weight = sum(calibrated.value)) %>%
      ungroup() %>%
      # If aggregated calibrated value > 0, set subsector shareweight to 1, else set to 0
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0),
             # If calibrated value for specific technology > 0 , set tech shareweight to 1, else set to 0
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # L244.StubTechEff_bld: Assumed efficiencies (all years) of buildings technologies
    L244.StubTechEff_bld_pre <- L144.end_use_eff %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value) %>%
      # Add region and input
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_bld_det, by = c("supplysector", "subsector", "technology")) %>%
      mutate(stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]])

    # L244.GlobalTechShrwt_bld: Default shareweights for global building technologies
    L244.GlobalTechShrwt_bld <- A44.globaltech_shrwt %>%
      # Repeat for all model years
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      # Interpolate
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # L244.GlobalTechCost_bld: Non-fuel costs of global building technologies
    L244.GlobalTechCost_bld <- add.cg(L144.NEcost_75USDGJ) %>%
      mutate(input.cost = round(NEcostPerService, energy.DIGITS_COST)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # L244.StubTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services
    L244.StubTechIntGainOutputRatio_pre <- L144.internal_gains %>%
      filter(year %in% MODEL_YEARS) %>%
      # Round and rename value
      mutate(value = round(value, energy.DIGITS_EFFICIENCY)) %>%
      rename(internal.gains.output.ratio = value) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add building.node.input
      left_join_error_no_match(calibrated_techs_bld_det %>%
                                 select(supplysector, building.node.input) %>%
                                 distinct(), by = "supplysector") %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 #mutate(internal.gains.market.name=paste0(nodeInput,"-internal-gains-trial-market")) %>%
                                 distinct() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               , by = "building.node.input") %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    InternalGainsScalar_USA <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)
    US.base.scalar <- tibble(variable, InternalGainsScalar_USA)

    L244.Intgains_scalar <- L244.ThermalServiceSatiation %>%
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      left_join_error_no_match(US.base.scalar, by = "variable") %>%
      left_join_error_no_match(L244.HDDCDD_normal_R_Y, by = c("region", "variable")) %>%
      mutate(agg.thermal.building.service.input = if_else(variable == "CDD","cooling","heating")) %>%
      left_join_error_no_match(L244.ThermalServiceSatiation %>%
                                 mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
                                 left_join_error_no_match(US.base.scalar, by = "variable") %>%
                                 left_join_error_no_match(L244.HDDCDD_normal_R_Y, by = c("region", "variable")) %>%
                                 mutate(agg.thermal.building.service.input = if_else(variable == "CDD","cooling","heating")) %>%
                                 filter(region == gcam.USA_REGION) %>%
                                 rename(degree.days.usa = degree.days) %>%
                                 select(-region,-satiation.level,-thermal.building.service.input) %>%
                                 distinct()
                               , by = c("gcam.consumer", "nodeInput", "building.node.input", "variable",
                                        "InternalGainsScalar_USA", "agg.thermal.building.service.input")) %>%
      select(-agg.thermal.building.service.input) %>%
      mutate(scalar_mult = degree.days / degree.days.usa) %>%
      mutate(internal.gains.scalar = round(InternalGainsScalar_USA * scalar_mult, energy.DIGITS_HDDCDD)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    # Need to remove any services (supplysectors and building-service-inputs) and intgains trial markets for services that don't exist in any years
    # L244.DeleteThermalService and L244.DeleteGenericService: Removing non-existent services, likely related to 0 HDD or CDD

    L244.DeleteThermalService_pre <- L244.ThermalBaseService %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, thermal.building.service.input) %>%
      summarise(base.service = max(base.service)) %>%
      ungroup()

    L244.DeleteThermalService<-calibrated_techs_bld_det %>%
      select(service) %>%
      distinct() %>%
      filter(service %in% thermal_services) %>%
      repeat_add_columns(tibble(region = unique(GCAM_region_names$region))) %>%
      select(region,service) %>%
      left_join(L244.DeleteThermalService_pre %>% select(region,service=thermal.building.service.input,base.service), by = c("region", "service")) %>%
      #replace_na(list(base.service = 0)) %>%
      filter(complete.cases(.)) %>%
      # adjust Eastern Africa to not delete modern services
      mutate(base.service = if_else(grepl("Africa",region) & service == "resid heating modern",1e-9,base.service)) %>%
      filter(base.service == 0) %>%
      select(-base.service) %>%
      rename(supplysector = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = if_else(grepl("resid",supplysector),"resid","comm"),
             nodeInput = gcam.consumer,
             building.node.input = paste0(nodeInput,"_building")) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      separate(gcam.consumer, c("adj","group"),sep="_",remove = F) %>%
      # adjust commercial
      mutate(gcam.consumer = if_else(grepl("comm",gcam.consumer),"comm",gcam.consumer)) %>%
      mutate(thermal.building.service.input = supplysector,
             supplysector =if_else(grepl("resid",gcam.consumer),paste0(supplysector,"_",group),supplysector),
             thermal.building.service.input = if_else(grepl("resid",gcam.consumer) ,paste0(thermal.building.service.input,"_",group),thermal.building.service.input)) %>%
      select(LEVEL2_DATA_NAMES[["DeleteThermalService"]])

    # Generic services
    L244.DeleteGenericService_pre<-L244.GenericBaseService %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, building.service.input) %>%
      summarise(base.service = max(base.service)) %>%
      ungroup()

    L244.DeleteGenericService<-calibrated_techs_bld_det %>%
      select(service) %>%
      distinct() %>%
      filter(service %in% generic_services) %>%
      repeat_add_columns(tibble(region = unique(GCAM_region_names$region))) %>%
      select(region,service) %>%
      left_join(L244.DeleteGenericService_pre %>% select(region,service=building.service.input,base.service), by = c("region", "service")) %>%
      #replace_na(list(base.service = 0)) %>%
      filter(complete.cases(.)) %>%
      filter(base.service == 0) %>%
      select(-base.service) %>%
      rename(supplysector = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = if_else(grepl("resid",supplysector),"resid","comm"),
             nodeInput = gcam.consumer,
             building.node.input = paste0(nodeInput,"_building")) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      separate(gcam.consumer, c("adj","group"),sep="_",remove = F) %>%
      # adjust commercial
      mutate(gcam.consumer = if_else(grepl("comm",gcam.consumer),"comm",gcam.consumer)) %>%
      mutate(building.service.input = supplysector,
             supplysector =if_else(grepl("resid",gcam.consumer),paste0(supplysector,"_",group),supplysector),
             building.service.input = if_else(grepl("resid",gcam.consumer) ,paste0(building.service.input,"_",group),building.service.input)) %>%
      select(LEVEL2_DATA_NAMES[["DeleteGenericService"]])

    #------------------------------------------------------
    # The demand for traditional fuels (coal and TradBio) is an inverse function (service demand as inverse of service affordability)
    # The parameters to define the functional form are estimated in the following lines:

    # First, estimate the parameters for the function to estimate coal demand:
    serv_coal<-L144.base_service_EJ_serv_fuel %>%
      rename(en_EJ = value) %>%
      filter(grepl("resid",service),
             fuel == "coal") %>%
      gcamdata::left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                                         , by = c("GCAM_region_ID", "year")) %>%
      rename(pcgdp_thous = value) %>%
      select(GCAM_region_ID,fuel,service,year,pcgdp_thous,en_EJ) %>%
      arrange(en_EJ) %>%
      left_join(L144.prices_bld %>% rename(service = market) %>% select(-region), by = c("GCAM_region_ID", "year","service")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford=(pcgdp_thous*1000/def9075) / price)

    formula.coal<- "en_EJ~A/(afford+k)"
    start.value.coal<-c(A = 1,k = 0.5)

    fit_coal<-nls(formula.coal, serv_coal, start.value.coal)
    A_coal<-coef(fit_coal)[1]
    k_coal<-coef(fit_coal)[2]

    # Same for traditional biomass:
    serv_TradBio<-L144.base_service_EJ_serv_fuel %>%
      rename(en_EJ = value) %>%
      filter(grepl("resid",service),
             fuel == "traditional biomass") %>%
      gcamdata::left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                                         , by = c("GCAM_region_ID", "year")) %>%
      rename(pcgdp_thous = value) %>%
      select(GCAM_region_ID,fuel,service,year,pcgdp_thous,en_EJ) %>%
      arrange(en_EJ) %>%
      left_join(L144.prices_bld %>% rename(service = market) %>% select(-region), by = c("GCAM_region_ID", "year","service")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford=(pcgdp_thous*1000/def9075) / price)


    formula.tradBio<- "en_EJ~x/(afford+y)"
    start.value.tradBio<-c(x = 10,y = 10)

    fit_tradBio<-nls(formula.tradBio, serv_TradBio, start.value.tradBio)
    x_TradBio<-coef(fit_tradBio)[1]
    y_TradBio<-coef(fit_tradBio)[2]

    #------------------------------------------------------
    # In order to make the function flexible to the implementation of multiple consumers, the satiation impedance (mu) and the calibration coefficent (k)
    # are calibrated in the DS per region.
    # Here we create L244.ThermalServiceImpedance and L244.GenericServiceImpedance, L244.GenericServiceCoef, and L244.ThermalServiceCoef (+ SSP-specific assumptions)

    # 1-L244.GenericServiceImpedance
    L244.GenericServiceImpedance_allvars<-L244.GenericServiceSatiation %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by = "region") %>%
      # Only modern services use impedance
      #filter(!grepl("coal",building.service.input),
      #       !grepl("TradBio",building.service.input)) %>%
      # use left_join due to TradBio
      left_join_error_no_match(L144.base_service_EJ_serv %>%  filter(year==MODEL_FINAL_BASE_YEAR, service %in% generic_services)
                               %>% rename(building.service.input = service),
                               by=c("GCAM_region_ID","building.service.input")) %>%
      rename(base_service_EJ = value) %>%
      left_join_error_no_match(L244.Floorspace %>%
                                 group_by(region, nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = if_else(grepl("resid",nodeInput),"resid","comm"))
                               ,by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ / base.building.size) %>%
      select(-base_service_EJ,-base.building.size) %>%
      # Add pcGDP
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario==socioeconomics.BASE_GDP_SCENARIO),
                               by=c("year","GCAM_region_ID")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4 (L144.prices_bld)
      left_join_error_no_match(L144.prices_bld  %>%
                                 rename(building.service.input = market) %>%
                                 filter(building.service.input %in% generic_services, year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","building.service.input","GCAM_region_ID")) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price)) / log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      # Check with an adder to be 0!!!!
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(thermal_load = 1) %>%
      mutate(afford=(pcGDP_thous90USD*1000/def9075) / price) %>%
      mutate(serv_density=satiation.level * (1-exp((-log(2)/`satiation-impedance`) * afford))) %>%
      mutate(serv_density = if_else(grepl("coal",building.service.input),observed_base_serv_perflsp,serv_density),
             serv_density = if_else(grepl("TradBio",building.service.input),observed_base_serv_perflsp,serv_density)) %>%
      mutate(serv_density2 = serv_density) %>%
      mutate(coef = observed_base_serv_perflsp / serv_density*thermal_load) %>%
      mutate(est_base_serv_perflsp = coef * thermal_load * serv_density) %>%
      mutate(bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))


    L244.GenericServiceImpedance<-L244.GenericServiceImpedance_allvars %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE))



    L244.GenericServiceCoef<-L244.GenericServiceImpedance_allvars %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))


    # 1.5-L244.GenericServiceImpedance_SSPs
    L244.GenericServiceImpedance_allvars_SSPs<-L244.GenericServiceSatiation_SSPs %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], SSP) %>%
      distinct() %>%
      # Only modern services use impedance
      # filter(!grepl("coal",building.service.input),
      #       !grepl("TradBio",building.service.input)) %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="region") %>%
      left_join(L144.base_service_EJ_serv %>%  filter(year==MODEL_FINAL_BASE_YEAR, service %in% generic_services)
                               %>% rename(building.service.input=service),
                               by=c("GCAM_region_ID","building.service.input")) %>%
      filter(complete.cases(.)) %>%
      rename(base_service_EJ=value) %>%
      left_join_error_no_match(L244.Floorspace %>%
                                 group_by(region, nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm"))
                               ,by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ/base.building.size) %>%
      select(-base_service_EJ,-base.building.size) %>%
      # Add pcGDP
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% rename(SSP=scenario),
                               by=c("year","GCAM_region_ID","SSP")) %>%
      rename(pcGDP_thous90USD=value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4 (L144.prices_bld)
      left_join(L144.prices_bld  %>%
                                 rename(building.service.input=market) %>%
                                 filter(building.service.input %in% generic_services, year==MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","building.service.input","GCAM_region_ID")) %>%
      replace_na(list(price=1)) %>%
      mutate(satiation.level=pmax(satiation.level,base_serv_flsp*1.001)) %>%
      filter(complete.cases(.)) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price))/log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      # Check with an adder to be 0!!!!
      rename(observed_base_serv_perflsp=base_serv_flsp) %>%
      mutate(thermal_load=1) %>%
      mutate(afford=(pcGDP_thous90USD*1000/def9075)/price) %>%
      mutate(serv_density=satiation.level*(1-exp((-log(2)/`satiation-impedance`)*afford))) %>%
      mutate(serv_density = if_else(grepl("coal",building.service.input),observed_base_serv_perflsp,serv_density),
             serv_density = if_else(grepl("TradBio",building.service.input),observed_base_serv_perflsp,serv_density)) %>%
      mutate(coef=observed_base_serv_perflsp/(serv_density*thermal_load)) %>%
      mutate(est_base_serv_perflsp=coef*thermal_load*serv_density) %>%
      # Adjust coal and TradBio services (no coef):
      mutate(est_base_serv_perflsp = if_else(grepl("coal",building.service.input),serv_density,est_base_serv_perflsp),
             est_base_serv_perflsp = if_else(grepl("TradBio",building.service.input),serv_density,est_base_serv_perflsp)) %>%
      mutate(bias.adder=round(est_base_serv_perflsp-observed_base_serv_perflsp,5))

    L244.GenericServiceImpedance_SSPs<-L244.GenericServiceImpedance_allvars_SSPs %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]],SSP)  %>%
      # only commercial and residential-modern services use coef
      filter(grepl("resid",gcam.consumer)) %>%
      # only commercial and residential-modern services use satiation impedance
      filter(!grepl("coal",building.service.input),
             !grepl("TradBio",building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars_SSPs %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]],SSP)  %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE)) %>%
      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("Satiation impedance compute using satiation level, per-capita GDP and per-capita floorsapce") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.GenericServiceImpedance_SSPs)) {
      assign(paste0("L244.GenericServiceImpedance_", i), L244.GenericServiceImpedance_SSPs[[i]] %>%
               add_title(paste0("Satiation impedance: ", i)) %>%
               add_legacy_name(paste0("L244.GenericServiceImpedance_SSPs_", i)))
    }


    L244.GenericServiceCoef_SSPs<-L244.GenericServiceImpedance_allvars_SSPs %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]],SSP)  %>%
      # only commercial and residential-modern services use coef
      filter(!grepl("coal",building.service.input),
             !grepl("TradBio",building.service.input)) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars_SSPs %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]],SSP)  %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT)) %>%
      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("Coef computed using satiation level, per-capita GDP and per-capita floorsapce") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.GenericServiceCoef_SSPs)) {
      assign(paste0("L244.GenericServiceCoef_", i), L244.GenericServiceCoef_SSPs[[i]] %>%
               add_title(paste0("Coef ", i)) %>%
               add_legacy_name(paste0("L244.GenericServiceCoef_SSPs_", i)))
    }

    # 2-L244.ThermalServiceImpedance

    # First calculate internal gains
    L244.internal_gains<-L244.StubTechCalInput_bld_pre %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.StubTechEff_bld_pre,
                               by = c("region","supplysector", "subsector" ,
                                      "stub.technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # use left join because not all services produce internal gains
      left_join(L244.StubTechIntGainOutputRatio_pre,
                by = c("region","supplysector", "subsector" ,
                       "stub.technology"="technology", "year")) %>%
      filter(complete.cases(.)) %>%
      mutate(int_gains = base.service*internal.gains.output.ratio) %>%
      mutate(gcam.consumer = ifelse(grepl("comm",supplysector),"comm","resid")) %>%
      group_by(region,gcam.consumer,year) %>%
      summarise(int_gains = sum(int_gains)) %>%
      ungroup() %>%
      rename(intGains_EJ= int_gains)


    L244.ThermalServiceImpedance_allvars<-L244.ThermalServiceSatiation %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="region") %>%
      left_join_error_no_match(L144.base_service_EJ_serv %>%  filter(year==MODEL_FINAL_BASE_YEAR, service %in% thermal_services)
                               %>% rename(thermal.building.service.input=service),
                               by=c("GCAM_region_ID","thermal.building.service.input")) %>%
      rename(base_service_EJ = value) %>%
      left_join_error_no_match(L244.Floorspace %>%
                                 group_by(region, nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = if_else(grepl("resid",nodeInput),"resid","comm"))
                               ,by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp = base_service_EJ / base.building.size) %>%
      # Bring the variables to calculate s0 (thermal load)
      mutate(dd=if_else(grepl("cooling",thermal.building.service.input),"CDD","HDD")) %>%
      left_join_error_no_match(L244.HDDCDD_scen_R_Y %>% filter(year == MODEL_FINAL_BASE_YEAR,
                                                               GCM == "no_GCM") %>%
                                 rename(dd = variable) %>%
                                 select(-GCM,-SRES),
                               by=c("GCAM_region_ID","region","year","dd")) %>%
      rename(degree.days = value) %>%
      left_join_error_no_match(L244.ShellConductance_bld %>% select(-shell.year) %>% filter(year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      left_join_error_no_match(L244.internal_gains %>% filter(year == MODEL_FINAL_BASE_YEAR), by=c("region","year","gcam.consumer")) %>%
      left_join_error_no_match(L244.Intgains_scalar,by = c("region","gcam.consumer","nodeInput",
                                                         "building.node.input","thermal.building.service.input")) %>%
      mutate(intGains_EJ_serv = intGains_EJ / base.building.size) %>%
      mutate(thermal_load = degree.days * shell.conductance * floor.to.surface.ratio + internal.gains.scalar*intGains_EJ_serv) %>%
      select(-base.building.size) %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                               by = c("year","GCAM_region_ID")) %>%
      rename(pcGDP_thous90USD = value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4 ("L144.prices_bld")
      # Use left_join bc there is no heating in Indonesia
      left_join(L144.prices_bld  %>%
                  rename(thermal.building.service.input = market) %>%
                  filter(thermal.building.service.input %in% thermal_services, year == MODEL_FINAL_BASE_YEAR),
                by=c("region","year","thermal.building.service.input","GCAM_region_ID")) %>%
      filter(complete.cases(.)) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price))/log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      # Check with an adder to be 0!!!!
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      mutate(serv_density=satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) %>%
      mutate(serv_density = if_else(grepl("coal",thermal.building.service.input),observed_base_serv_perflsp,serv_density),
             serv_density = if_else(grepl("TradBio",thermal.building.service.input),observed_base_serv_perflsp,serv_density)) %>%
      mutate(coef = observed_base_serv_perflsp / (serv_density*thermal_load)) %>%
      mutate(est_base_serv_perflsp = coef * thermal_load * serv_density) %>%
      mutate(bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))

    L244.ThermalServiceImpedance<-L244.ThermalServiceImpedance_allvars %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceImpedance_allvars %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`,energy.DIGITS_SATIATION_IMPEDANCE))

    L244.ThermalServiceCoef<-L244.ThermalServiceImpedance_allvars %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceImpedance_allvars %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      # adjust coal and tradbio
      mutate(coef = if_else(grepl("coal",thermal.building.service.input),1,coef),
             coef = if_else(grepl("TradBio",thermal.building.service.input),1,coef)) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))


    #------------------------------------------------------
    # The region-level service is allocated to different consumers using "shares"
    # First, shares are estimated using the corresponding demand functions (either for modern or traditional fuels)
    # Shares are used to allocate observed regional eergy/service data across subregional consumers
    # L244.GenericBaseService and L244.ThermalBaseService are adjusted to have service data at consumer level within each region

    # L244.GenericBaseService adjusted
    L244.GenericBaseService_pre<-L244.GenericBaseService %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO),
                               by = c("year","GCAM_region_ID")) %>%
      rename(pcGDP_thous90USD = value) %>%
      left_join(L144.prices_bld  %>%
                  rename(building.service.input=market)
                ,by = c("region","year","building.service.input","GCAM_region_ID")) %>%
      replace_na(list(price = 0)) %>%
      left_join(L244.GenericServiceImpedance_allvars %>%
                  select(region,gcam.consumer,nodeInput,building.service.input,satiation.level,`satiation-impedance`)
                ,by=c("region","gcam.consumer","nodeInput","building.service.input")) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]],satiation.level,`satiation-impedance`,pcGDP_thous90USD,price)

    L244.GenericShares_pre<-L244.GenericBaseService_pre %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericBaseService_pre %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by=c("year","region","gcam.consumer")) %>%
      rename(pop_thous_gr = pop_thous) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("year","GCAM_region_ID")) %>%
      rename(pop_thous = value) %>%
      left_join_error_no_match(L244.SubregionalShares %>%
                                 select(-inc.year.fillout) %>%
                                 rename(year = pop.year.fillout)
                               , by=c("region","gcam.consumer","year")) %>%
      mutate(pcGDP_thous90USD_gr = (pcGDP_thous90USD * 1E3 * pop_thous * 1E3 * subregional.income.share) / (pop_thous_gr * 1E3 * 1E3)) %>%
      mutate(afford = (pcGDP_thous90USD_gr*1000/def9075) / price) %>%
      mutate(afford = if_else(is.infinite(afford),0,afford)) %>%
      left_join_error_no_match(bind_rows(L244.Floorspace_resid,L244.Floorspace_comm),
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(serv=(satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) * base.building.size) %>%
      # Adjust coal and TradBio
      mutate(serv = if_else(grepl("coal",building.service.input),coef(fit_coal)[1] / (afford +coef(fit_coal)[2]),serv),
             serv = if_else(grepl("TradBio",building.service.input),coef(fit_tradBio)[1] / (afford + coef(fit_tradBio)[2]),serv)) %>%
      mutate(serv = if_else(afford == 0, 0, serv))


    # Calculate subtotals (for shares)
    L244.GenericShares_pre_subt<-L244.GenericShares_pre %>%
      group_by(region,year,building.service.input) %>%
      summarise(serv = sum(serv)) %>%
      ungroup() %>%
      rename(serv_aggReg = serv)

    # Merge the subtotals to the estimated values to calculate %shares for each consumer group, in each region and period
    L244.GenericShares<- L244.GenericShares_pre %>%
      left_join_error_no_match(L244.GenericShares_pre_subt
                               , by=c("region","building.service.input","year")) %>%
      mutate(gen_share = serv / serv_aggReg) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,gen_share) %>%
      replace_na(list(gen_share = 0)) %>%
      # Check
      group_by(region,nodeInput,building.node.input,building.service.input,year) %>%
      mutate(agg_gen_share = sum(gen_share)) %>%
      ungroup()


    L244.GenericBaseService<-L244.GenericBaseService_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year, base.service) %>%
      filter(grepl("resid",nodeInput)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericBaseService_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,base.service) %>%
                  filter(grepl("comm",nodeInput))) %>%
      left_join_error_no_match(L244.GenericShares %>% select(-agg_gen_share)
                               , by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input","year")) %>%
      mutate(base.service = base.service * gen_share) %>%
      select(-gen_share) %>%
      mutate(base.service = round(base.service, energy.DIGITS_SERVICE))


    # L244.ThermalBaseService adjusted
    L244.ThermalBaseService_pre<-L244.ThermalBaseService %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario==socioeconomics.BASE_GDP_SCENARIO),
                               by=c("year","GCAM_region_ID")) %>%
      rename(pcGDP_thous90USD=value) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4 ("L144.prices_bld")
      left_join(L144.prices_bld  %>%
                  rename(thermal.building.service.input = market) %>%
                  filter(thermal.building.service.input %in% thermal_services),
                by=c("region","year","thermal.building.service.input","GCAM_region_ID")) %>%
      left_join(L244.ThermalServiceImpedance_allvars %>%
                                 select(region,gcam.consumer,nodeInput,thermal.building.service.input,thermal_load,satiation.level,`satiation-impedance`) %>%
                                 repeat_add_columns(tibble(year=MODEL_BASE_YEARS))
                               ,by=c("region","gcam.consumer","nodeInput","thermal.building.service.input","year")) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]],satiation.level,`satiation-impedance`,thermal_load,pcGDP_thous90USD,price)


    L244.ThermalShares_pre<-L244.ThermalBaseService_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,base.service,satiation.level,thermal_load,pcGDP_thous90USD,price,`satiation-impedance`) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalBaseService_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,satiation.level,thermal_load,pcGDP_thous90USD,price,`satiation-impedance`) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh_gr, by=c("year","region","gcam.consumer")) %>%
      rename(pop_thous_gr = pop_thous) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("year","GCAM_region_ID")) %>%
      rename(pop_thous = value) %>%
      left_join_error_no_match(L244.SubregionalShares %>%
                                 select(-inc.year.fillout) %>%
                                 rename(year = pop.year.fillout)
                               , by=c("region","gcam.consumer","year")) %>%
      mutate(pcGDP_thous90USD_gr=(pcGDP_thous90USD * 1E3 * pop_thous * 1E3 * subregional.income.share) / (pop_thous_gr * 1E3 * 1E3)) %>%
      mutate(afford = (pcGDP_thous90USD_gr * 1000/def9075) / price) %>%
      mutate(afford = if_else(is.infinite(afford),0,afford)) %>%
      left_join_error_no_match(bind_rows(L244.Floorspace_resid,L244.Floorspace_comm),
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(serv=(satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) * base.building.size) %>%
      # Adjust coal and TradBio
      mutate(serv = if_else(grepl("coal",thermal.building.service.input),coef(fit_coal)[1]/(afford +coef(fit_coal)[2]),serv),
             serv = if_else(grepl("TradBio",thermal.building.service.input),coef(fit_tradBio)[1]/(afford + coef(fit_tradBio)[2]),serv)) %>%
      mutate(serv = if_else(afford == 0, 0, serv))

    # Calculate subtotals (for shares)
    L244.ThermalShares_pre_subt<-L244.ThermalShares_pre %>%
      group_by(region,year,thermal.building.service.input) %>%
      summarise(serv = sum(serv)) %>%
      ungroup() %>%
      rename(serv_aggReg = serv)

    # Merge the subtotals to the estimated values to calculate %shares for each consumer group, in each region and period
    L244.ThermalShares<- L244.ThermalShares_pre %>%
      left_join(L244.ThermalShares_pre_subt
                               , by=c("region","thermal.building.service.input","year")) %>%
      mutate(thermal_share = serv / serv_aggReg) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,thermal_share) %>%
      replace_na(list(thermal_share = 0)) %>%
      # Check
      group_by(region,nodeInput,building.node.input,thermal.building.service.input,year) %>%
      mutate(agg_thermal_share = sum(thermal_share)) %>%
      ungroup()


    L244.ThermalBaseService<-L244.ThermalBaseService_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year, base.service) %>%
      filter(grepl("resid",nodeInput)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalBaseService_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year ,base.service) %>%
                  filter(grepl("comm",nodeInput))) %>%
      left_join_error_no_match(L244.ThermalShares %>%
                  select(-agg_thermal_share)
                  , by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input","year")) %>%
      mutate(base.service = base.service * thermal_share) %>%
      select(-thermal_share) %>%
      mutate(base.service = round(base.service, energy.DIGITS_SERVICE))

    #------------------------------------------------------
    # NOTE: Using the adjusted consumer-level data, parameters for traditional fuel function can be re-fitted to improve the fit.
    # They can also be estimated by service to capture additional differences/dynamics

    # 1- Re-fit parameters: Coal

    # Generic services
    serv_coal_refit_oth<-L244.GenericBaseService %>%
      filter(grepl("coal",building.service.input)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                               , by = c("GCAM_region_ID", "year","gcam.consumer","region")) %>%
      left_join(L144.prices_bld %>% rename(building.service.input = market) %>% select(-region), by = c("GCAM_region_ID", "year","building.service.input")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      filter(complete.cases(.)) %>%
      filter(base.service != 0)

    formula.coal_refit_oth<- "base.service~A/(afford+k)"
    start.value.coal_refit_oth<-c(A = 1,k = 0.5)

    fit_coal_refit_oth<-nls(formula.coal_refit_oth, serv_coal_refit_oth, start.value.coal_refit_oth)
    A_coal_refit_oth<-coef(fit_coal_refit_oth)[1]
    k_coal_refit_oth<-coef(fit_coal_refit_oth)[2]

    # Thermal services
    serv_coal_refit_thermal<-L244.ThermalBaseService %>%
      filter(grepl("coal",thermal.building.service.input)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                               , by = c("GCAM_region_ID", "year","gcam.consumer","region")) %>%
      left_join(L144.prices_bld %>% rename(thermal.building.service.input = market) %>% select(-region), by = c("GCAM_region_ID", "year","thermal.building.service.input")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      filter(complete.cases(.)) %>%
      filter(base.service != 0)

    formula.coal_refit_thermal<- "base.service~A/(afford+k)"
    start.value.coal_refit_thermal<-c(A = 1,k = 0.5)

    fit_coal_refit_thermal<-nls(formula.coal_refit_thermal, serv_coal_refit_thermal, start.value.coal_refit_thermal)
    A_coal_refit_thermal<-coef(fit_coal_refit_thermal)[1]
    k_coal_refit_thermal<-coef(fit_coal_refit_thermal)[2]

    L244.coal.coef<-tibble(gcam.consumer = "resid", nodeInput = "resid", building.node.input = "resid_building") %>%
      repeat_add_columns(tibble(service = c("resid others coal","resid heating coal"))) %>%
      mutate(A_coal = if_else(service == "resid others coal",A_coal_refit_oth,A_coal_refit_thermal),
             k_coal = if_else(service == "resid others coal",k_coal_refit_oth,k_coal_refit_thermal)) %>%
      repeat_add_columns(tibble(GCAM_region_ID = unique(serv_coal$GCAM_region_ID))) %>%
      left_join_error_no_match(GCAM_region_names, by="GCAM_region_ID")

    # Some regions have coal services at early historical years, but have phased it out in recent historical years (2005-2015)
    # In these regions we assume there should not be any coal demand in future model periods
    # We correct this dynamic by zeroing the coefficient in the demand function:
    check_coal<-L244.base_service %>%
      filter(year == MODEL_FINAL_BASE_YEAR, grepl("coal",building.service.input)) %>%
      filter(base.service == 0) %>%
      mutate(is.no.coal = 1) %>%
      select(-base.service)

    check_coal_oth<-check_coal %>% filter(grepl("others",building.service.input))
    check_coal_thermal<-check_coal %>% filter(grepl("heating",building.service.input))

    reg_no_recentCoal_oth<-unique(check_coal_oth$region)
    reg_no_recentCoal_thermal<-unique(check_coal_thermal$region)


    L244.coal.coef<-L244.coal.coef %>%
      mutate(A_coal = if_else(grepl("others",service) & region %in% reg_no_recentCoal_oth,0,A_coal),
             A_coal = if_else(grepl("heating",service) & region %in% reg_no_recentCoal_thermal,0,A_coal))


    L244.GenericCoalCoef<-L244.coal.coef %>%
      filter(service %in% generic_services) %>%
      rename(building.service.input = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.GenericBaseService, by = c("gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "region","year")) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      rename(base.coal = base.service) %>%
      mutate(A_coal = round(A_coal, energy.DIGITS_COEFFICIENT),
             k_coal = round(k_coal, energy.DIGITS_COEFFICIENT),
             base.coal = round(base.coal, energy.DIGITS_SERVICE)) %>%
      select(LEVEL2_DATA_NAMES[["GenericCoalCoef"]])


    L244.ThermalCoalCoef<-L244.coal.coef %>%
      filter(service %in% thermal_services) %>%
      rename(thermal.building.service.input = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalBaseService, by = c("gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "region","year")) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      rename(base.coal = base.service) %>%
      mutate(A_coal = round(A_coal, energy.DIGITS_COEFFICIENT),
             k_coal = round(k_coal, energy.DIGITS_COEFFICIENT),
             base.coal = round(base.coal, energy.DIGITS_SERVICE)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalCoalCoef"]])

    # 2- Re-fit parameters: Traditional Biomass

    # Generic services
    serv_TradBio_refit_oth<-L244.GenericBaseService %>%
      filter(grepl("TradBio",building.service.input)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                               , by = c("GCAM_region_ID", "year","gcam.consumer","region")) %>%
      left_join(L144.prices_bld %>% rename(building.service.input = market) %>% select(-region), by = c("GCAM_region_ID", "year","building.service.input")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      filter(complete.cases(.)) %>%
      filter(base.service != 0)

    formula.TradBio_refit_oth<- "base.service~x/(afford+y)"
    start.value.TradBio_refit_oth<-c(x = 10,y = 10)

    fit_TradBio_refit_oth<-nls(formula.TradBio_refit_oth, serv_TradBio_refit_oth, start.value.TradBio_refit_oth)
    x_TradBio_refit_oth<-coef(fit_TradBio_refit_oth)[1]
    y_TradBio_refit_oth<-coef(fit_TradBio_refit_oth)[2]

    # Thermal services
    serv_TradBio_refit_thermal<-L244.ThermalBaseService %>%
      filter(grepl("TradBio",thermal.building.service.input)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y_gr %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                               , by = c("GCAM_region_ID", "year","gcam.consumer","region")) %>%
      left_join(L144.prices_bld %>% rename(thermal.building.service.input = market) %>% select(-region), by = c("GCAM_region_ID", "year","thermal.building.service.input")) %>%
      replace_na(list(price = 1)) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      filter(complete.cases(.)) %>%
      filter(base.service != 0)

    formula.TradBio_refit_thermal<- "base.service~x/(afford+y)"
    start.value.TradBio_refit_thermal<-c(x = 10,y = 10)

    fit_TradBio_refit_thermal<-nls(formula.TradBio_refit_thermal, serv_TradBio_refit_thermal, start.value.TradBio_refit_thermal)
    x_TradBio_refit_thermal<-coef(fit_TradBio_refit_thermal)[1]
    y_TradBio_refit_thermal<-coef(fit_TradBio_refit_thermal)[2]


    L244.tradBio.coef<-tibble(gcam.consumer = "resid", nodeInput = "resid", building.node.input = "resid_building") %>%
      repeat_add_columns(tibble(service = c("resid others TradBio","resid heating TradBio"))) %>%
      mutate(x_TradBio = if_else(service == "resid others TradBio",x_TradBio_refit_oth,x_TradBio_refit_thermal),
             y_TradBio = if_else(service == "resid others TradBio",y_TradBio_refit_oth,y_TradBio_refit_thermal)) %>%
      repeat_add_columns(tibble(GCAM_region_ID = unique(serv_TradBio$GCAM_region_ID))) %>%
      left_join_error_no_match(GCAM_region_names, by="GCAM_region_ID")


    L244.GenericTradBioCoef<-L244.tradBio.coef %>%
      filter(service %in% generic_services) %>%
      rename(building.service.input = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
      mutate(year =MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.GenericBaseService, by = c("gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "region","year")) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      rename(base.TradBio = base.service) %>%
      mutate(x_TradBio = if_else(base.TradBio==0,0,x_TradBio)) %>%
      mutate(x_TradBio = round(x_TradBio, energy.DIGITS_COEFFICIENT),
             y_TradBio = round(y_TradBio, energy.DIGITS_COEFFICIENT),
             base.TradBio = round(base.TradBio, energy.DIGITS_SERVICE)) %>%
      select(LEVEL2_DATA_NAMES[["GenericTradBioCoef"]])


    L244.ThermalTradBioCoef<-L244.tradBio.coef %>%
      filter(service %in% thermal_services) %>%
      rename(thermal.building.service.input = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
      mutate(year =MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalBaseService, by = c("gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "region","year")) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      rename(base.TradBio = base.service) %>%
      mutate(x_TradBio = if_else(base.TradBio==0,0,x_TradBio)) %>%
      mutate(x_TradBio = round(x_TradBio, energy.DIGITS_COEFFICIENT),
             y_TradBio = round(y_TradBio, energy.DIGITS_COEFFICIENT),
             base.TradBio = round(base.TradBio, energy.DIGITS_SERVICE)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalTradBioCoef"]])

    #------------------------------------------------------
    # Bias adder: The difference between the observed and estimated data in final calibration year to capture the "unobservable" effects not captured by the variables in the model equations
    # Calculated using the shares
    # First estimate consumer-specific bias adder for different consumers within each region
    # To ensure that all the consumers are in the same path, make those consumer-specific adders transition to a common adder in 2030
    # This adder is calculated using regional data, and equally spliting the regional adder across consumer-groups.
    # This transition in three periods avoids drastic jumps from final calibration year to first model period.

    # 1- Generic services

    # Coal and TradBio
    L244.GenericServiceAdder_aggObs<-L144.base_service_EJ_serv %>%
      filter(service %in% generic_services) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region,year,service) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(building.service.input = service,
             obs = value) %>%
      filter(year == MODEL_FINAL_BASE_YEAR)

    L244.GenericServiceAdder_aggObs_gr<-L244.GenericBaseService %>%
      rename(obs = value) %>%
      filter(year == MODEL_FINAL_BASE_YEAR)

    trad_fuels_oth<-c("resid others coal","resid others TradBio")
    modern_fuels_oth<-c("resid others modern")
    Adder.Conv.Year<-2030
    ADJ_MODEL_YEARS<-c(MODEL_BASE_YEARS,MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= Adder.Conv.Year])

    L244.GenericServiceAdder_coal_tradbio_pre<-L244.GenericShares_pre %>%
      filter(grepl("resid",building.service.input)) %>%
      filter(building.service.input %in% trad_fuels_oth) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,afford) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep="_",remove = F) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group),
             building.service.input = paste0(building.service.input,"_",group)) %>%
      left_join(L244.GenericCoalCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input")) %>%
      left_join(L244.GenericTradBioCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input")) %>%
      select(-group,-base.TradBio,-base.coal) %>%
      rename(A_coal_new = A_coal,
             k_coal_new = k_coal,
             x_TradBio_new = x_TradBio,
             y_TradBio_new = y_TradBio) %>%
      mutate(serv = if_else(grepl("coal",building.service.input),A_coal_new / (afford+k_coal_new),x_TradBio_new / (afford+y_TradBio_new))) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,serv) %>%
      mutate(est = serv) %>%
      separate(building.service.input,c("building.service.input","group"),sep = "_") %>%
      select(region,year,gcam.consumer,building.service.input,est)

    L244.GenericServiceAdder_coal_tradbio_pre_agg<-L244.GenericServiceAdder_coal_tradbio_pre %>%
      group_by(region,year,building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join(L244.GenericServiceAdder_aggObs, by = c("region", "year", "building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(bias.adder.equal = (obs - est_agg)/n_groups) %>%
      mutate(bias.adder = round(bias.adder.equal,9)) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      ungroup() %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder)

    L244.GenericServiceAdder_coal_tradbio<-L244.GenericServiceAdder_coal_tradbio_pre %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.GenericServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "building.service.input")) %>%
      mutate(bias.adder.share = base.service - est,
             bias.adder.share = if_else(base.service==0,0,bias.adder.share)) %>%
      left_join_error_no_match(L244.GenericServiceAdder_coal_tradbio_pre_agg, by = c("region", "gcam.consumer", "building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(base.service==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])


    # Modern
    L244.GenericServiceAdder_modern_pre<-L244.GenericShares_pre %>%
      filter(building.service.input %in% modern_fuels_oth) %>%
      mutate(est = serv) %>%
      select(region,year,gcam.consumer,building.service.input,est)

    L244.GenericServiceAdder_modern_pre_agg<-L244.GenericServiceAdder_modern_pre %>%
      group_by(region,year,building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year== MODEL_FINAL_BASE_YEAR) %>%
      left_join(L244.GenericServiceAdder_aggObs, by = c("region", "year", "building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(adder_bm2 = obs - est_agg) %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh %>% select(-gcam.consumer), by = c("region", "year")) %>%
      mutate(bias.adder = adder_bm2/value) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder)

    L244.GenericServiceAdder_modern<-L244.GenericServiceAdder_modern_pre %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.GenericServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "building.service.input")) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "year", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder.share = (base.service - est)/base.building.size,
             bias.adder.share = if_else(base.service==0,0,bias.adder.share)) %>%
      left_join_error_no_match(L244.GenericServiceAdder_modern_pre_agg, by = c("region", "gcam.consumer", "building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(base.service==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])


   L244.GenericServiceAdder<-bind_rows(L244.GenericServiceAdder_coal_tradbio,L244.GenericServiceAdder_modern) %>%
    # add commercial
    bind_rows(L244.GenericShares_pre %>%
                filter(year== MODEL_FINAL_BASE_YEAR) %>%
                filter(grepl("comm",gcam.consumer)) %>%
                mutate(bias.adder = 0) %>%
                select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])) %>%
     mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))


    # 1.5- Generic services per SSP
    L244.GenericServiceAdder_SSPs<-L244.GenericServiceAdder %>%
      repeat_add_columns(tibble(SSP=unique(L244.GenericServiceImpedance_allvars_SSPs$SSP))) %>%
      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("Bias-correction parameter for non-thermal services") %>%
          add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
                         "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
      })

    # Assign each tibble in list
    for(i in names(L244.GenericServiceAdder_SSPs)) {
      assign(paste0("L244.GenericServiceAdder_", i), L244.GenericServiceAdder_SSPs[[i]] %>%
               add_title(paste0("Satiation Adder: ", i)) %>%
               add_legacy_name(paste0("L244.GenericServiceAdder_SSPs_", i)))
    }


    # 2- Thermal services

    trad_fuels_thermal<-c("resid cooling coal","resid heating coal","resid cooling TradBio","resid heating TradBio")
    modern_fuels_thermal<-c("resid cooling modern","resid heating modern")

    # Coal and TradBio
    L244.ThermalServiceAdder_aggObs<-L144.base_service_EJ_serv %>%
      filter(service %in% thermal_services) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region,year,service) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(thermal.building.service.input = service,
             obs = value) %>%
      filter(year== MODEL_FINAL_BASE_YEAR)

    L244.ThermalServiceAdder_aggObs_gr<-L244.ThermalBaseService %>%
      rename(obs = value) %>%
      filter(year== MODEL_FINAL_BASE_YEAR)


    L244.ThermalServiceAdder_coal_tradbio_pre<-L244.ThermalShares_pre %>%
      filter(grepl("resid",thermal.building.service.input)) %>%
      filter(thermal.building.service.input %in% trad_fuels_thermal) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,afford) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep="_",remove = F) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",group),
             thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      left_join(L244.ThermalCoalCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input")) %>%
      left_join(L244.ThermalTradBioCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input")) %>%
      select(-group,-base.TradBio,-base.coal) %>%
      rename(A_coal_new = A_coal,
             k_coal_new = k_coal,
             x_TradBio_new = x_TradBio,
             y_TradBio_new = y_TradBio) %>%
      mutate(serv = if_else(grepl("coal",thermal.building.service.input),A_coal_new/(afford+k_coal_new),x_TradBio_new/(afford+y_TradBio_new))) %>%
      replace_na(list(serv = 0)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,serv) %>%
      mutate(est = serv) %>%
      separate(thermal.building.service.input,c("thermal.building.service.input","group"),sep = "_") %>%
      select(region,year,gcam.consumer,thermal.building.service.input,est)

    L244.ThermalServiceAdder_coal_tradbio_pre_agg<-L244.ThermalServiceAdder_coal_tradbio_pre %>%
      group_by(region,year,thermal.building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_aggObs, by = c("region", "year", "thermal.building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(bias.adder.equal = (obs - est_agg)/n_groups) %>%
      mutate(bias.adder = round(bias.adder.equal,9)) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      ungroup() %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder)

    L244.ThermalServiceAdder_coal_tradbio<-L244.ThermalServiceAdder_coal_tradbio_pre %>%
      filter(year== MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "thermal.building.service.input")) %>%
      mutate(bias.adder.share = base.service - est,
             bias.adder.share = if_else(base.service==0,0,bias.adder.share)) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_coal_tradbio_pre_agg, by = c("region", "gcam.consumer", "thermal.building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(base.service==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])


    # Modern
    L244.ThermalServiceAdder_modern_pre<-L244.ThermalShares_pre %>%
      filter(thermal.building.service.input %in% modern_fuels_thermal) %>%
      mutate(est = serv) %>%
      select(region,year,gcam.consumer,thermal.building.service.input,est)

    L244.ThermalServiceAdder_modern_pre_agg<-L244.ThermalServiceAdder_modern_pre %>%
      group_by(region,year,thermal.building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join(L244.ThermalServiceAdder_aggObs, by = c("region", "year", "thermal.building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(adder_bm2 = obs - est_agg) %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh %>% select(-gcam.consumer), by = c("region", "year")) %>%
      mutate(bias.adder = adder_bm2/value) %>%
      mutate(nodeInput = "resid",
             building.node.input = "resid_building") %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
        select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder)

    L244.ThermalServiceAdder_modern<-L244.ThermalServiceAdder_modern_pre %>%
      filter(year== MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "thermal.building.service.input")) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "year", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder.share = (base.service - est)/base.building.size,
             bias.adder.share = if_else(base.service==0,0,bias.adder.share)) %>%
      filter(complete.cases(.)) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_modern_pre_agg, by = c("region", "gcam.consumer", "thermal.building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(base.service==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])


    L244.ThermalServiceAdder<-bind_rows(L244.ThermalServiceAdder_coal_tradbio,L244.ThermalServiceAdder_modern) %>%
      # add commercial
      bind_rows(L244.ThermalShares_pre %>%
                  filter(year== MODEL_FINAL_BASE_YEAR) %>%
                  filter(grepl("comm",gcam.consumer)) %>%
                  mutate(bias.adder = 0) %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])) %>%
      mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))

    #------------------------------------------------------
    # Once the calibration process is finished at region level, several files need to be adjusted to multiple consumer groups:

    # Satiation level
    L244.GenericServiceSatiation<-L244.GenericServiceSatiation %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceSatiation %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # Need to match in the floorspace into the base service table, divide to calculate the service demand
    # per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
    L244.GenericServiceSatiation_SSPs <- L244.GenericServiceSatiation_SSPs %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceSatiation_SSPs %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]],SSP) %>%

      # Split by SSP, creating a list with a tibble for each SSP, then add attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("EJ/billion m2 floorspace") %>%
          add_comments("For USA, calculate satiation level as base year service / base year floorspace times multiplier") %>%
          add_comments("USA values written to all regions, then we make sure that no satiation level is below base year service per floorspace") %>%
          add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                         "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult_SSPs")
      })

    # Assign each tibble in list
    for(i in names(L244.GenericServiceSatiation_SSPs)) {
      assign(paste0("L244.GenericServiceSatiation_", i), L244.GenericServiceSatiation_SSPs[[i]] %>%
               add_title(paste0("Satiation levels for non-thermal building services: ", i)) %>%
               add_legacy_name(paste0("L244.GenericServiceSatiation_", i)))
    }


    L244.ThermalServiceSatiation<-L244.ThermalServiceSatiation %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceSatiation %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])


    # Internal gains scalar
    L244.Intgains_scalar<-L244.Intgains_scalar %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble::tibble(cons.groups)) %>%
      separate(cons.groups,c("sector","cons.groups"),sep="_",remove = F) %>%
      select(-sector) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",cons.groups),
             thermal.building.service.input = paste0(thermal.building.service.input,"_",cons.groups)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]]) %>%
      bind_rows(L244.Intgains_scalar %>%
                  filter(grepl("comm",gcam.consumer)))


    # Shell Efficiency
    # Adjust future years based on the rule used for expanding USA-based efficiency to the RoW
    # Efficiency in region r = Efficiency in USA * (HDD in region r / HDD in USA)^(-0.05)*(GDP per capita in region r / GDP per capita in USA)^elasticity
    # elasticity is assumed to linearly increase from -0.1 to -0.07 over the century
    # Given that DD are similar across consumer groups within each region, the rule is adapted to be exclusively GDP-driven:
    # Efficiency for consumer i = Regional average efficiency  * (GDP per capita for consumer i / Avergae regional GDP per capita)^elasticity

    # First, create the tibble with price elasticities
    L244.PrElast.shell<-tibble(shell.year=c(min(MODEL_FUTURE_YEARS),max(MODEL_FUTURE_YEARS)),prelast=c(-0.07,-0.1)) %>%
      complete(nesting(shell.year=MODEL_FUTURE_YEARS)) %>%
      mutate(prelast = if_else(is.na(prelast), approx_fun(shell.year, prelast, rule = 1), prelast)) %>%
      repeat_add_columns(tibble(region=unique(GCAM_region_names$region)))


    L244.ShellConductance_bld_noadj<-L244.ShellConductance_bld %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ShellConductance_bld %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      filter(gcam.consumer == "comm") %>%
      bind_rows(L244.ShellConductance_bld %>%
                  filter(grepl("resid",gcam.consumer)) %>%
                  repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
                  unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
                  bind_rows(L244.ShellConductance_bld %>%
                              filter(grepl("comm",gcam.consumer))) %>%
                  filter(grepl("resid",gcam.consumer) & year %in% MODEL_BASE_YEARS))

    L244.ShellConductance_bld<-L244.ShellConductance_bld %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ShellConductance_bld %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      anti_join(L244.ShellConductance_bld_noadj, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year", "shell.conductance", "shell.year", "floor.to.surface.ratio")) %>%
      left_join_error_no_match(A_regions %>% select(region,GCAM_region_ID),by="region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario == socioeconomics.BASE_GDP_SCENARIO)
                               , by = c("GCAM_region_ID","year")) %>%
      rename(pcGDP_thous90USD = value) %>%
      left_join_error_no_match(L244.SubregionalShares %>%
                                 select(-inc.year.fillout) %>%
                                 rename(year = pop.year.fillout)
                               , by = c("region", "gcam.consumer","year")) %>%
      # we use population in 2015 as we don't have projections
      left_join_error_no_match(L101.Pop_thous_R_Yh %>% filter(year==MODEL_FINAL_BASE_YEAR), by = c("GCAM_region_ID")) %>%
      rename(pop_thous=value) %>%
      mutate(gdp=pcGDP_thous90USD*1E3*pop_thous*1E3) %>%
      mutate(pcGDP_thous90USD_gr=(gdp*subregional.income.share)/(pop_thous*1E3*subregional.population.share*1E3)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,shell.conductance,shell.year,
             floor.to.surface.ratio,pcGDP_thous90USD,pcGDP_thous90USD_gr) %>%
      left_join_error_no_match(L244.PrElast.shell,by=c("region","shell.year")) %>%
      mutate(adj_shell=(pcGDP_thous90USD_gr/pcGDP_thous90USD)^prelast,
             shell.conductance=shell.conductance*adj_shell,
             year=shell.year) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]]) %>%
      bind_rows(L244.ShellConductance_bld_noadj) %>%
      arrange(region,year,gcam.consumer) %>%
      # To avoid big jumps, we linearly interpolate the shell efficiency from final base year to 2100
      filter(year %in% c(MODEL_BASE_YEARS,2100)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input), year = c(year, MODEL_YEARS)) %>%
      mutate(shell.year = year) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input) %>%
      mutate(shell.conductance = approx_fun(year, shell.conductance, rule = 1),
             floor.to.surface.ratio = approx_fun(year, floor.to.surface.ratio, rule = 2)) %>%
      ungroup() %>%
      mutate(shell.conductance = round(shell.conductance,energy.DIGITS_SHELL))

    # Supplysectors are also disaggregated at consumer levels (to allow consumer-specific fuel-technology mixes)
    # First, adjust efficiencies and InterganGain I/O ratios
    L244.StubTechEff_bld<-add.cg(L244.StubTechEff_bld_pre)
    L244.StubTechIntGainOutputRatio<-add.cg(L244.StubTechIntGainOutputRatio_pre)

    # Finally need to calibrate the different technologies at consumer-group level
    shares_resid<-bind_rows(L244.GenericShares %>% rename(share = gen_share,
                                                    agg.share = agg_gen_share,
                                                    adj_sector = building.service.input),
                      L244.ThermalShares %>% rename(adj_sector = thermal.building.service.input,
                                                    share = thermal_share,
                                                    agg.share = agg_thermal_share)) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("tmp","group"),sep = "_",remove = F) %>%
      select(region,year,group,adj_sector,share)

    L244.StubTechCalInput_bld_comm<- L244.StubTechCalInput_bld_pre %>%
      filter(grepl("comm",supplysector))

    L244.StubTechCalInput_bld_resid<-add.cg(L244.StubTechCalInput_bld_pre) %>%
      filter(grepl("resid",supplysector)) %>%
      separate(supplysector,c("adj_sector","group"),sep = "_",remove = F) %>%
      # use left_join due to lack of heating in Indonesia
      left_join(shares_resid, by=c("region","year","group","adj_sector")) %>%
      mutate(share = if_else(is.na(share),0,share)) %>%
      mutate(calibrated.value = calibrated.value * share) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    L244.StubTechCalInput_bld<-bind_rows(L244.StubTechCalInput_bld_resid,L244.StubTechCalInput_bld_comm)

    # Add consumer group to building.service.input and to thermal.building.service.input
    L244.ThermalBaseService<-L244.ThermalBaseService %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]]) %>%
      bind_rows(L244.ThermalBaseService %>%
      filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceCoef<-L244.ThermalServiceCoef %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
      bind_rows(L244.ThermalServiceCoef %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceSatiation<-L244.ThermalServiceSatiation %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]]) %>%
      bind_rows(L244.ThermalServiceSatiation %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceImpedance<-L244.ThermalServiceImpedance %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
      bind_rows(L244.ThermalServiceImpedance %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceAdder<-L244.ThermalServiceAdder %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]]) %>%
      bind_rows(L244.ThermalServiceAdder %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericBaseService<-L244.GenericBaseService %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]]) %>%
      bind_rows(L244.GenericBaseService %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceCoef<-L244.GenericServiceCoef %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
      bind_rows(L244.GenericServiceCoef %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceSatiation<-L244.GenericServiceSatiation %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]]) %>%
      bind_rows(L244.GenericServiceSatiation %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceImpedance<-L244.GenericServiceImpedance %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
      bind_rows(L244.GenericServiceImpedance %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceAdder<-L244.GenericServiceAdder %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]]) %>%
      bind_rows(L244.GenericServiceAdder %>%
                  filter(grepl("comm",gcam.consumer)))

    #------------------------------------------------------
    # Write the service prices in final calibration year
    # These will be used in the cpp files to compute the adjustment parameter that will account for the difference between read and calculated service prices

    L244.GenericServicePrice<- L144.prices_bld %>%
      filter(market %in% generic_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(building.service.input = market) %>%
      filter(grepl("resid",building.service.input)) %>%
      repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid$gcam.consumer))) %>%
      left_join_error_no_match(A44.gcam_consumer_resid %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(-adj,-group) %>%
      bind_rows(L144.prices_bld %>%
                  filter(market %in% generic_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(building.service.input = market) %>%
                  filter(grepl("comm",building.service.input)) %>%
                  repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_comm$gcam.consumer))) %>%
                  left_join_error_no_match(A44.gcam_consumer_comm %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["GenericServicePrice"]])


    L244.ThermalServicePrice<- L144.prices_bld %>%
      filter(market %in% thermal_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(thermal.building.service.input = market) %>%
      filter(grepl("resid",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid$gcam.consumer))) %>%
      left_join_error_no_match(A44.gcam_consumer_resid %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      separate(gcam.consumer,c("adj","group"), sep = "_",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(-adj,-group) %>%
      bind_rows(L144.prices_bld %>%
                  filter(market %in% thermal_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(thermal.building.service.input = market) %>%
                  filter(grepl("comm",thermal.building.service.input)) %>%
                  repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_comm$gcam.consumer))) %>%
                  left_join_error_no_match(A44.gcam_consumer_comm %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServicePrice"]])

    #------------------------------------------------------
    # Finally, calculate the base year service density
    # This density will be used in case it gets negative when adding the bias adder coefficient
    L244.GenericBaseDens<-L244.GenericBaseService %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseDens"]])

    L244.ThermalBaseDens<-L244.ThermalBaseService %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseDens"]])


    #===================================================
    # Produce outputs
    L244.SubregionalShares %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all regions") %>%
      add_comments("subregional.population.share and subregional.income.share set to 1") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("common/GCAM_region_names", "energy/A44.gcam_consumer") ->
      L244.SubregionalShares

    L244.PriceExp_IntGains %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all regions") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("common/GCAM_region_names", "energy/A44.gcam_consumer")->
      L244.PriceExp_IntGains

    L244.Floorspace %>%
      add_title("Base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Values taken from L144.flsp_bm2_R_res_Yh and L144.flsp_bm2_R_comm_Yh") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("common/GCAM_region_names", "energy/A44.gcam_consumer",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh") ->
      L244.Floorspace

    L244.DemandFunction_serv %>%
      add_title("Demand function for building service") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_serv written to all regions") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("common/GCAM_region_names", "energy/A44.demandFn_serv")->
      L244.DemandFunction_serv

    L244.DemandFunction_flsp %>%
      add_title("Demand function for building floorspace") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_flsp written to all regions") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("common/GCAM_region_names", "energy/A44.demandFn_flsp") ->
      L244.DemandFunction_flsp

    L244.Satiation_flsp %>%
      add_title("Floorspace demand satiation") %>%
      add_units("Million squared meters per capita") %>%
      add_comments("Values from A44.satiation_flsp added to A44.gcam_consumer written to all regions") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("energy/A44.satiation_flsp", "energy/A44.gcam_consumer", "common/GCAM_region_names", "energy/A_regions") ->
      L244.Satiation_flsp

    L244.SatiationAdder %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("Unitless") %>%
      add_comments("Satiation adder compute using satiation level, per-capita GDP and per-capita floorsapce") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("energy/A44.satiation_flsp", "energy/A44.gcam_consumer", "common/GCAM_region_names", "energy/A_regions",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh") ->
      L244.SatiationAdder

    L244.Satiation_impedance %>%
      add_title("Floorspace satiation impedance") %>%
      add_units("Unitless") %>%
      add_comments("Calibrated in the DS for flexibility with multiple consumer groups") %>%
      add_legacy_name("L244.Satiation_impedance") %>%
      add_precursors("energy/A44.satiation_flsp", "energy/A44.gcam_consumer", "common/GCAM_region_names", "energy/A_regions",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh") ->
      L244.Satiation_impedance

    L244.GompFnParam %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Computed offline based on data from RECS and IEA") %>%
      add_legacy_name("L244.GompFnParam") %>%
      add_precursors("common/GCAM_region_names",
                     "L144.flsp_param",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
                     "L144.flsp_bm2_R_res_Yh","L144.hab_land_flsp_fin") ->
      L244.GompFnParam

    L244.ThermalBaseService %>%
      add_title("Historical building heating and cooling energy output") %>%
      add_units("EJ/yr") %>%
      add_comments("L144.base_service_EJ_serv rounded and renamed") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("energy/A44.internal_gains", "energy/A44.sector", "L144.base_service_EJ_serv",
                     "energy/calibrated_techs_bld_det", "common/GCAM_region_names") ->
      L244.ThermalBaseService

    L244.GenericBaseService %>%
      add_title("Historical building `other` energy output") %>%
      add_units("EJ/yr") %>%
      add_comments("L144.base_service_EJ_serv rounded and renamed") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      same_precursors_as(L244.ThermalBaseService) ->
      L244.GenericBaseService

    L244.ThermalServiceSatiation %>%
      add_title("Satiation levels for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("For USA, calculate satiation level as base year service / base year floorspace times multiplier") %>%
      add_comments("USA values written to all regions, which are multiplied by ratio of degree days in each region to degree days in USA") %>%
      add_comments("then we make sure that no satiation level is below base year service per floorspace") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L143.HDDCDD_scen_R_Y", "energy/A44.internal_gains", "energy/A44.sector", "energy/A44.gcam_consumer") ->
      L244.ThermalServiceSatiation

    L244.GenericServiceSatiation %>%
      add_title("Satiation levels for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("For USA, calculate satiation level as base year service / base year floorspace times multiplier") %>%
      add_comments("USA values written to all regions, then we make sure that no satiation level is below base year service per floorspace") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult") ->
      L244.GenericServiceSatiation

    L244.GenericServiceImpedance %>%
      add_title("Satiation impedance for non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated based on max (Model_Base_Years) at region level") %>%
      add_legacy_name("L244.GenericServiceImpedance") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld","socioeconomics/income_shares") ->
      L244.GenericServiceImpedance

    L244.ThermalServiceImpedance %>%
      add_title("Satiation impedance for non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated based on max (Model_Base_Years) at region level") %>%
      add_legacy_name("L244.ThermalServiceImpedance") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld","socioeconomics/income_shares",
                     "L143.HDDCDD_scen_R_Y","L144.shell_eff_R_Y", "L144.internal_gains") ->
      L244.ThermalServiceImpedance

    L244.GenericServiceAdder %>%
      add_title("Bias-correction for non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated based on max (Model_Base_Years) at region level") %>%
      add_legacy_name("L244.GenericServiceImpedance") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld","socioeconomics/income_shares") ->
      L244.GenericServiceAdder

    L244.ThermalServiceAdder %>%
      add_title("Bias-correction thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated based on max (Model_Base_Years) at region level") %>%
      add_legacy_name("L244.ThermalServiceAdder") %>%
      add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
                     "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld","socioeconomics/income_shares",
                     "L143.HDDCDD_scen_R_Y","L144.shell_eff_R_Y", "L144.internal_gains") ->
      L244.ThermalServiceAdder

    L244.GenericServiceCoef %>%
      add_title("Coef paramter for thermal building services") %>%
      add_comments("It is zero if no multiple consumers are implemented") %>%
      add_units("Unitless") %>%
      add_legacy_name("L244.GenericServiceCoef") %>%
      add_precursors("L144.base_service_EJ_serv", "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld",
                     "L144.shell_eff_R_Y","L144.internal_gains", "L143.HDDCDD_scen_R_Y") ->
      L244.GenericServiceCoef

    L244.ThermalServiceCoef %>%
      add_title("Coef paramter for thermal building services") %>%
      add_comments("It is zero if no multiple consumers are implemented") %>%
      add_units("Unitless") %>%
      add_legacy_name("L244.ThermalServiceCoef") %>%
      add_precursors("L144.base_service_EJ_serv", "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh","L144.prices_bld",
                     "L144.shell_eff_R_Y","L144.internal_gains", "L143.HDDCDD_scen_R_Y") ->
      L244.ThermalServiceCoef

    L244.Intgains_scalar %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("USA base scalar assumption multiplied by ratio of degree days to USA degree days") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      same_precursors_as(L244.ThermalServiceSatiation) ->
      L244.Intgains_scalar

    L244.ShellConductance_bld %>%
      add_title("Shell conductance (inverse of shell efficiency)") %>%
      add_units("Unitless") %>%
      add_comments("Shell conductance from L144.shell_eff_R_Y") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("L144.shell_eff_R_Y", "common/GCAM_region_names", "energy/A44.gcam_consumer") ->
      L244.ShellConductance_bld

    L244.Supplysector_bld %>%
      add_title("Supplysector info for buildings") %>%
      add_units("NA") %>%
      add_comments("A44.sector written to all regions") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("energy/A44.sector", "common/GCAM_region_names") ->
      L244.Supplysector_bld

    L244.FinalEnergyKeyword_bld %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("A44.sector written to all regions") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      same_precursors_as(L244.Supplysector_bld) ->
      L244.FinalEnergyKeyword_bld


    if(exists("L244.SubsectorShrwt_bld")) {
      L244.SubsectorShrwt_bld %>%
        add_title("Subsector shareweights for building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("energy/A44.subsector_shrwt", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorShrwt_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.SubsectorShrwt_bld
    }

    if(exists("L244.SubsectorShrwtFllt_bld")) {
      L244.SubsectorShrwtFllt_bld %>%
        add_title("Subsector shareweights for building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("energy/A44.subsector_shrwt", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorShrwtFllt_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.SubsectorShrwtFllt_bld
    }

    if(exists("L244.SubsectorInterp_bld")) {
      L244.SubsectorInterp_bld %>%
        add_title("Subsector shareweight interpolation for building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("energy/A44.subsector_interp", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorInterp_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.SubsectorInterp_bld
    }

    if(exists("L244.SubsectorInterpTo_bld")) {
      L244.SubsectorInterpTo_bld %>%
        add_title("Subsector shareweight interpolation for building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("energy/A44.subsector_interp", "common/GCAM_region_names", "L144.end_use_eff")  ->
        L244.SubsectorInterpTo_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") ->
        L244.SubsectorInterpTo_bld
    }

    L244.SubsectorLogit_bld %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("A44.subsector_logit written to all regions") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("energy/A44.subsector_logit", "common/GCAM_region_names", "L144.end_use_eff") ->
      L244.SubsectorLogit_bld

    L244.FuelPrefElast_bld %>%
      add_title("Fuel preference elasticities for buildings") %>%
      add_units("Unitless") %>%
      add_comments("A44.fuelprefElasticity written to all regions") %>%
      add_legacy_name("L244.FuelPrefElast_bld") %>%
      add_precursors("energy/A44.fuelprefElasticity", "common/GCAM_region_names", "L144.end_use_eff") ->
      L244.FuelPrefElast_bld

    L244.StubTech_bld %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("Technologies from L144.end_use_eff") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("L144.end_use_eff") ->
      L244.StubTech_bld

    L244.StubTechEff_bld %>%
      add_title("Assumed efficiencies of buildings technologies") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Efficiencies taken from L144.end_use_eff") %>%
      add_legacy_name("L244.StubTechEff_bld") %>%
      add_precursors("L144.end_use_eff", "common/GCAM_region_names", "energy/calibrated_techs_bld_det") ->
      L244.StubTechEff_bld

    L244.StubTechCalInput_bld %>%
      add_title("Calibrated energy consumption by buildings technologies") %>%
      add_units("calibrated.value: EJ; shareweights: Unitless") %>%
      add_comments("Calibrated values directly from L144.in_EJ_R_bld_serv_F_Yh") %>%
      add_comments("Shareweights are 1 if subsector/technology total is non-zero, 0 otherwise") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.in_EJ_R_bld_serv_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs_bld_det") ->
      L244.StubTechCalInput_bld

    L244.StubTechIntGainOutputRatio %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless output ratio") %>%
      add_comments("Values from L144.internal_gains") %>%
      add_legacy_name("L244.StubTechIntGainOutputRatio") %>%
      add_precursors("L144.internal_gains", "common/GCAM_region_names",
                     "energy/calibrated_techs_bld_det", "energy/A44.gcam_consumer") ->
      L244.StubTechIntGainOutputRatio

    L244.GlobalTechShrwt_bld %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated from A44.globaltech_shrwt") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("energy/A44.globaltech_shrwt") ->
      L244.GlobalTechShrwt_bld

    L244.GlobalTechCost_bld %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ-service") %>%
      add_comments("Costs from L144.NEcost_75USDGJ expanded to model years") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("L144.NEcost_75USDGJ") ->
      L244.GlobalTechCost_bld

    L244.GenericCoalCoef %>%
      add_title("Coefficients for the estimation of coal: generic services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.GenericCoalCoef") %>%
      add_precursors("common/GCAM_region_names","L144.base_service_EJ_serv_fuel") ->
      L244.GenericCoalCoef

    L244.ThermalCoalCoef %>%
      add_title("Coefficients for the estimation of coal: thermal services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.ThermalCoalCoef") %>%
      add_precursors("common/GCAM_region_names","L144.base_service_EJ_serv_fuel") ->
      L244.ThermalCoalCoef

    L244.GenericTradBioCoef %>%
      add_title("Coefficients for the estimation of TradBio: generic services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.GenericTradBioCoef") %>%
      add_precursors("common/GCAM_region_names","L144.base_service_EJ_serv_fuel") ->
      L244.GenericTradBioCoef

    L244.ThermalTradBioCoef %>%
      add_title("Coefficients for the estimation of TradBio: thermal services") %>%
      add_units("Unitless") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.ThermalTradBioCoef") %>%
      add_precursors("common/GCAM_region_names","L144.base_service_EJ_serv_fuel") ->
      L244.ThermalTradBioCoef

    L244.ThermalShares %>%
      add_title("Shares for allocate thermal services across income groups") %>%
      add_units("%") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.ThermalShares") %>%
      add_precursors("common/GCAM_region_names","L144.in_EJ_R_bld_serv_F_Yh") ->
      L244.ThermalShares

    L244.GenericShares %>%
      add_title("Shares for allocate generic services across income groups") %>%
      add_units("%") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.GenericShares") %>%
      add_precursors("common/GCAM_region_names","L144.in_EJ_R_bld_serv_F_Yh") ->
      L244.GenericShares

    if(exists("L244.DeleteGenericService")) {
      L244.DeleteGenericService %>%
        add_title("Removing non-existent services") %>%
        add_units("NA") %>%
        add_comments("Categories from L244.GenericBaseService with no base.service") %>%
        add_legacy_name("L244.DeleteGenericService") %>%
        same_precursors_as(L244.GenericBaseService) ->
        L244.DeleteGenericService
    } else {
      missing_data() %>%
        add_legacy_name("L244.DeleteGenericService") ->
        L244.DeleteGenericService
    }


    L244.DeleteThermalService %>%
      add_title("Removing non-existent thermal services") %>%
      add_units("NA") %>%
      add_comments("Categories from L244.ThermalBaseService with no base.service") %>%
      add_legacy_name("L244.DeleteThermalService") %>%
      same_precursors_as(L244.ThermalBaseService) ->
      L244.DeleteThermalService


    L244.GenericServicePrice %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for generic services") %>%
      add_legacy_name("L244.GenericServicePrice") %>%
      add_precursors("common/GCAM_region_names","L144.prices_bld") ->
      L244.GenericServicePrice

    L244.ThermalServicePrice %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for thermal services") %>%
      add_legacy_name("L244.ThermalServicePrice") %>%
      add_precursors("common/GCAM_region_names","L144.prices_bld") ->
      L244.ThermalServicePrice

    L244.GenericBaseDens %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for generic services") %>%
      add_legacy_name("L244.GenericBaseDens") %>%
      add_precursors("common/GCAM_region_names","L144.in_EJ_R_bld_serv_F_Yh","L144.flsp_bm2_R_res_Yh") ->
      L244.GenericBaseDens

    L244.ThermalBaseDens %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for thermal services") %>%
      add_legacy_name("L244.ThermalBaseDens") %>%
      add_precursors("common/GCAM_region_names","L144.in_EJ_R_bld_serv_F_Yh","L144.flsp_bm2_R_res_Yh") ->
      L244.ThermalBaseDens


    return_data(L244.SubregionalShares, L244.SubregionalShares_SSP1,L244.SubregionalShares_SSP2,L244.SubregionalShares_SSP3,
                L244.SubregionalShares_SSP4,L244.SubregionalShares_SSP5,
                L244.PriceExp_IntGains, L244.Floorspace, L244.DemandFunction_serv, L244.DemandFunction_flsp,
                L244.Satiation_flsp, L244.SatiationAdder, L244.ThermalBaseService, L244.GenericBaseService, L244.ThermalServiceSatiation,
                L244.GenericServiceSatiation, L244.Intgains_scalar, L244.ShellConductance_bld,
                L244.Supplysector_bld, L244.FinalEnergyKeyword_bld, L244.SubsectorShrwt_bld, L244.SubsectorShrwtFllt_bld, L244.SubsectorInterp_bld,
                L244.SubsectorInterpTo_bld, L244.FuelPrefElast_bld,
                L244.StubTech_bld, L244.StubTechEff_bld, L244.StubTechCalInput_bld, L244.GlobalTechShrwt_bld,
                L244.GlobalTechCost_bld, L244.DeleteGenericService, L244.Satiation_flsp_SSP1, L244.SatiationAdder_SSP1,
                L244.GenericServiceSatiation_SSP1, L244.Satiation_flsp_SSP2,
                L244.SatiationAdder_SSP2, L244.GenericServiceSatiation_SSP2, L244.Satiation_flsp_SSP3, L244.SatiationAdder_SSP3,
                L244.GenericServiceSatiation_SSP3, L244.Satiation_flsp_SSP4,
                L244.SatiationAdder_SSP4, L244.GenericServiceSatiation_SSP4,
                L244.Satiation_flsp_SSP5, L244.SatiationAdder_SSP5, L244.GenericServiceSatiation_SSP5,
                L244.DeleteThermalService, L244.SubsectorLogit_bld, L244.StubTechIntGainOutputRatio,
                L244.HDDCDD_A2_CCSM3x, L244.HDDCDD_A2_HadCM3, L244.HDDCDD_B1_CCSM3x, L244.HDDCDD_B1_HadCM3, L244.HDDCDD_constdd_no_GCM,
                L244.GompFnParam,L244.Satiation_impedance,L244.Satiation_impedance_SSP1,L244.Satiation_impedance_SSP2,
                L244.Satiation_impedance_SSP3,L244.Satiation_impedance_SSP4,L244.Satiation_impedance_SSP5,
                L244.GenericServiceImpedance,L244.GenericServiceAdder,
                L244.GenericServiceImpedance_SSP1,L244.GenericServiceImpedance_SSP2,
                L244.GenericServiceImpedance_SSP3,L244.GenericServiceImpedance_SSP4,L244.GenericServiceImpedance_SSP5,
                L244.GenericServiceAdder_SSP1,L244.GenericServiceAdder_SSP2,L244.GenericServiceAdder_SSP3,
                L244.GenericServiceAdder_SSP4,L244.GenericServiceAdder_SSP5,
                L244.ThermalServiceImpedance,L244.ThermalServiceAdder,
                L244.GenericServiceCoef,L244.GenericServiceCoef_SSP1,L244.GenericServiceCoef_SSP2,L244.GenericServiceCoef_SSP3,
                L244.GenericServiceCoef_SSP4,L244.GenericServiceCoef_SSP5,L244.ThermalServiceCoef,
                L244.GenericCoalCoef,L244.ThermalCoalCoef,L244.GenericTradBioCoef,L244.ThermalTradBioCoef,
                L244.GenericShares,L244.ThermalShares,L244.GenericServicePrice,L244.ThermalServicePrice,L244.GenericBaseDens,L244.ThermalBaseDens)

  } else {
    stop("Unknown command")
  }
}
