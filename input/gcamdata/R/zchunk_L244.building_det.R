# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L244.building_det
#'
#' Creates level2 data for the building sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.SubregionalShares}, \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv},
#' \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService},
#'\code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld},
#' \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld},
#' \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogit_bld}, \code{L244.FuelPrefElast_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechEff_bld},
#' \code{L244.StubTechCalInput_bld}, \code{L244.StubTechIntGainOutputRatio}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld},
#' \code{L244.DeleteGenericService}, \code{L244.Satiation_flsp_SSP1}, \code{L244.SatiationAdder_SSP1}, \code{L244.GenericServiceSatiation_SSP1},
#' \code{L244.Satiation_flsp_SSP2}, \code{L244.SatiationAdder_SSP2}, \code{L244.GenericServiceSatiation_SSP2}, \code{L244.Satiation_flsp_SSP3},
#' \code{L244.SatiationAdder_SSP3}, \code{L244.GenericServiceSatiation_SSP3}, \code{L244.FuelPrefElast_bld_SSP3}, \code{L244.Satiation_flsp_SSP4},
#' \code{L244.SatiationAdder_SSP4}, \code{L244.GenericServiceSatiation_SSP4}, \code{L244.FuelPrefElast_bld_SSP4}, \code{L244.Satiation_flsp_SSP5},
#' \code{L244.SatiationAdder_SSP5}, \code{L244.GenericServiceSatiation_SSP5}, \code{L244.FuelPrefElast_bld_SSP15}, \code{L244.DeleteThermalService},
#' \code{L244.HDDCDD_A2_CCSM3x}, \code{L244.HDDCDD_A2_HadCM3}, \code{L244.HDDCDD_B1_CCSM3x}, \code{L244.HDDCDD_B1_HadCM3},
#' \code{L244.HDDCDD_constdd_no_GCM} and \code{L244.GompFnParam}.
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
             FILE = "energy/A44.fuelprefElasticity_SSP3",
             FILE = "energy/A44.fuelprefElasticity_SSP4",
             FILE = "energy/A44.fuelprefElasticity_SSP15",
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
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L144.end_use_eff",
             "L144.shell_eff_R_Y",
             "L144.NEcost_75USDGJ",
             "L144.internal_gains",
             "L143.HDDCDD_scen_R_Y",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L144.flsp_param",
             "L144.hab_land_flsp_fin"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.SubregionalShares",
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
             "L244.FuelPrefElast_bld_SSP3",
             "L244.Satiation_flsp_SSP4",
             "L244.SatiationAdder_SSP4",
             "L244.GenericServiceSatiation_SSP4",
             "L244.FuelPrefElast_bld_SSP4",
             "L244.Satiation_flsp_SSP5",
             "L244.SatiationAdder_SSP5",
             "L244.GenericServiceSatiation_SSP5",
             "L244.FuelPrefElast_bld_SSP15",
             "L244.DeleteThermalService",
             "L244.HDDCDD_A2_CCSM3x",
             "L244.HDDCDD_A2_HadCM3",
             "L244.HDDCDD_B1_CCSM3x",
             "L244.HDDCDD_B1_HadCM3",
             "L244.HDDCDD_constdd_no_GCM",
             "L244.GompFnParam"))
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
      L244.SatiationAdder_SSP5 <- L244.GenericServiceSatiation_SSP5 <- scenario <- L244.HDDCDD_A2_CCSM3x <-
      L244.HDDCDD_A2_HadCM3 <- L244.HDDCDD_B1_CCSM3x <- L244.HDDCDD_B1_HadCM3 <- L244.HDDCDD_constdd_no_GCM <-
      area_thouskm2 <- flsp <- flsp_pc <- unadjust.satiation <- land.density.param <- tot.dens <-
      b.param <- income.param <- gdp_pc <- flsp_est <- base_flsp <- bias.adjust.param <- NULL

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
    A44.fuelprefElasticity_SSP3 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP3", strip_attributes = TRUE)
    A44.fuelprefElasticity_SSP4 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP4", strip_attributes = TRUE)
    A44.fuelprefElasticity_SSP15 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP15", strip_attributes = TRUE)
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
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    L144.end_use_eff <- get_data(all_data, "L144.end_use_eff", strip_attributes = TRUE)
    L144.shell_eff_R_Y <- get_data(all_data, "L144.shell_eff_R_Y", strip_attributes = TRUE)
    L144.NEcost_75USDGJ <- get_data(all_data, "L144.NEcost_75USDGJ", strip_attributes = TRUE)
    L144.internal_gains <- get_data(all_data, "L144.internal_gains", strip_attributes = TRUE)
    L143.HDDCDD_scen_R_Y <- get_data(all_data, "L143.HDDCDD_scen_R_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y") # year comes in as double
    L144.flsp_param <- get_data(all_data, "L144.flsp_param", strip_attributes = TRUE)
    L144.hab_land_flsp_fin<-get_data(all_data, "L144.hab_land_flsp_fin", strip_attributes = TRUE)

    # ===================================================
    # Subregional population and income shares: need to be read in because these default to 0
    # L244.SubregionalShares: subregional population and income shares (not currently used)
    L244.SubregionalShares <- write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["DeleteConsumer"]],
                                                   GCAM_region_names = GCAM_region_names) %>%
      mutate(pop.year.fillout = min(MODEL_BASE_YEARS),
             inc.year.fillout = min(MODEL_BASE_YEARS),
             subregional.population.share = 1,
             subregional.income.share = 1)

    # Internal gains
    # L244.PriceExp_IntGains: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains <- write_to_all_regions(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]],
                                                   GCAM_region_names = GCAM_region_names)

    # L244.Floorspace: base year floorspace

    # Building residential floorspace in the base years
    # Keep all historical years for now - these are needed in calculating satiation adders later on
    A44.gcam_consumer_resid <- A44.gcam_consumer %>%
      filter(grepl("res", A44.gcam_consumer$gcam.consumer))

    L244.Floorspace_resid <- L144.flsp_bm2_R_res_Yh %>%
      mutate(base.building.size = round(value, energy.DIGITS_FLOORSPACE)) %>%
      select(-value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(gcam.consumer = A44.gcam_consumer_resid$gcam.consumer,
             nodeInput = A44.gcam_consumer_resid$nodeInput,
             building.node.input = A44.gcam_consumer_resid$building.node.input) %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)

    # Commercial floorspace
    A44.gcam_consumer_comm <- A44.gcam_consumer %>%
      filter(grepl("comm", A44.gcam_consumer$gcam.consumer))

    L244.Floorspace_comm <- L144.flsp_bm2_R_comm_Yh %>%
      mutate(base.building.size = round(value, energy.DIGITS_FLOORSPACE)) %>%
      select(-value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(gcam.consumer = A44.gcam_consumer_comm$gcam.consumer,
             nodeInput = A44.gcam_consumer_comm$nodeInput,
             building.node.input = A44.gcam_consumer_comm$building.node.input) %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)

    L244.Floorspace <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm) %>%
      filter(year %in% MODEL_BASE_YEARS)

    # Demand function
    # L244.DemandFunction_serv and L244.DemandFunction_flsp: demand function types
    L244.DemandFunction_serv <- write_to_all_regions(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]],
                                                     GCAM_region_names = GCAM_region_names)
    L244.DemandFunction_flsp <- write_to_all_regions(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]],
                                                     GCAM_region_names = GCAM_region_names)

    # Floorspace demand satiation
    # L244.Satiation_flsp: Satiation levels assumed for floorspace
    L244.Satiation_flsp_class <- A44.satiation_flsp %>%
      gather(sector, value, resid, comm) %>%
      # Converting from square meters per capita to million square meters per capita
      mutate(satiation.level = value * CONV_THOUS_BIL) %>%
      select(-value)

    L244.Satiation_flsp <- write_to_all_regions(A44.gcam_consumer, c("region", "gcam.consumer", "nodeInput", "building.node.input"), # replace with LEVEL2_DATA_NAMES[["BldNodes]]
                                                GCAM_region_names = GCAM_region_names) %>%
      # Match in the region class, and use this to then match in the satiation floorspace
      left_join_error_no_match(A_regions %>% select(region, region.class),
                               by = "region") %>%
      # Residential floorspace does not use the satiation demand function, so filter the commercial floorspace
      filter(!grepl("resid",gcam.consumer)) %>%
      left_join_error_no_match(L244.Satiation_flsp_class, by = c("region.class", "gcam.consumer" = "sector")) %>%
      select(LEVEL2_DATA_NAMES[["Satiation_flsp"]])

    # Satiation adder - Required for shaping the future floorspace growth trajectories in each region
    # The satiation adder allows the starting (final calibration year) position of any region and sector to be set along the satiation demand function
    # L244.SatiationAdder: Satiation adders in floorspace demand function
    # First, prepare socioeconomics tables by adding region names
    L102.pcgdp_thous90USD_Scen_R_Y <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pcGDP_thous90USD = value)
    L101.Pop_thous_R_Yh <- L101.Pop_thous_R_Yh %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(pop_thous = value)

    # In order to pass timeshift, we need floorspace for energy satiation year, which we don't have under timeshift conditions
    # So instead, we will take floorspace for maximum year (this ONLY affects the data in the timeshift right now)
    Floorspace_timeshift_pass <- L244.Floorspace %>%
      filter(year == max(L244.Floorspace$year)) %>%
      select(-year)

    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)
    L244.SatiationAdder <- L244.Satiation_flsp %>%
      left_join_keep_first_only(L102.pcgdp_thous90USD_Scen_R_Y %>%
                                  filter(year == energy.SATIATION_YEAR), by = "region") %>%
      left_join_error_no_match(Floorspace_timeshift_pass, by = c("region", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("region", "year", "GCAM_region_ID")) %>%
      mutate(pcFlsp_mm2 = base.building.size / pop_thous,
             # We now have all of the data required for calculating the satiation adder in each region
             satiation.adder = round(satiation.level - exp(log(2) * pcGDP_thous90USD / energy.GDP_MID_SATIATION) *
                                       (satiation.level - pcFlsp_mm2), energy.DIGITS_SATIATION_ADDER),
             pcFlsp_mm2_fby = base.building.size / pop_thous,
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
             # Need to match in the demand in the final calibration year to check this.
             satiation.adder = if_else(satiation.adder > pcFlsp_mm2_fby, pcFlsp_mm2_fby * 0.999, satiation.adder)) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # L244.Satiation_flsp_SSPs: Satiation levels assumed for floorspace in the SSPs
    L244.Satiation_flsp_class_SSPs <- A44.satiation_flsp_SSPs %>%
      gather(sector, value, resid, comm) %>%
      mutate(satiation.level = value * CONV_THOUS_BIL)

    L244.Satiation_flsp_SSPs <- write_to_all_regions(A44.gcam_consumer, c("region", "gcam.consumer", "nodeInput", "building.node.input"), # replace with LEVEL2_DATA_NAMES[["BldNodes]]
                                                     GCAM_region_names = GCAM_region_names) %>%
      repeat_add_columns(tibble(SSP = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
      # Match in the region class, and use this to then match in the satiation floorspace
      left_join_error_no_match(A_regions %>% select(region, region.class), by = "region") %>%
      # Residential floorspace does not use the satiation demand function, so filter the commercial floorspace
      filter(!grepl("resid",gcam.consumer)) %>%
      left_join_error_no_match(L244.Satiation_flsp_class_SSPs, by = c("SSP", "region.class", "gcam.consumer" = "sector")) %>%
      # Calculate pcFlsp and make sure it is smaller than the satiation level
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>%
                                 # This used to be filtered to energy.SATIATION_YEAR, changed to pass timeshift test
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "SSP" = "scenario")) %>%
      left_join_error_no_match(L244.Floorspace, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("GCAM_region_ID", "year", "region")) %>%
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

    # Satiation adder - Required for shaping the future floorspace growth trajectories in each region
    # L244.SatiationAdder_SSPs: Satiation adders in floorspace demand function for the SSPs
    L244.SatiationAdder_SSPs <- L244.Satiation_flsp_SSPs %>%
      # Calculate satiation.adder
      mutate(satiation.adder = round(satiation.level - (exp(log(2) * pcGDP_thous90USD / energy.GDP_MID_SATIATION) *
                                                          (satiation.level - pcFlsp_mm2)),
                                     energy.DIGITS_SATIATION_ADDER)) %>%
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


    #------------------------------------------------------
    # Updated floorspace Gompertz function
    # - Calculate the bias correction parameter (k)
    # - Write parameters for the updated floorspace function

    # Write the function parameters
    L244.GompFnParam<-L144.flsp_param %>%
      left_join_error_no_match(L144.hab_land_flsp_fin %>% filter(year==MODEL_FINAL_BASE_YEAR),by="region") %>%
      rename(area_thouskm2=value) %>%
      left_join_error_no_match(GCAM_region_names, by="region") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_Scen_R_Y %>% filter(scenario== socioeconomics.BASE_GDP_SCENARIO), by=c("GCAM_region_ID","year")) %>%
      rename(gdp_pc=pcGDP_thous90USD) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by=c("GCAM_region_ID","year")) %>%
      left_join_error_no_match(L144.flsp_bm2_R_res_Yh,by=c("GCAM_region_ID","year")) %>%
      rename(flsp=value) %>%
      mutate(tot.dens=pop_thous/area_thouskm2,
             flsp_pc=(flsp*1E9)/(pop_thous*1E3),
             base_flsp=flsp_pc,
             flsp_est=(`unadjust.satiation` +(-`land.density.param`*log(tot.dens)))*exp(-`b.param`
                                                                                        *exp(-`income.param`*log(gdp_pc))),
             bias.adjust.param=flsp_pc-flsp_est,
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

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)

    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.internal_gains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    # Base-service: filter only the model base years and change names as indicated in calibrated_techs_bld_det
    L244.base_service <- L144.base_service_EJ_serv %>%
      rename(base.service = value) %>%
      mutate(base.service = round(base.service, energy.DIGITS_CALOUTPUT)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_keep_first_only(calibrated_techs_bld_det, by = c("sector", "service")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input = supplysector, year, base.service)

    # Separate thermal and generic services into separate tibbles
    L244.GenericBaseService <- L244.base_service %>%
      filter(building.service.input %in% generic_services)

    L244.ThermalBaseService <- L244.base_service %>%
      filter(building.service.input %in% thermal_services) %>%
      rename(thermal.building.service.input = building.service.input)

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
    L244.HDDCDD <- L244.ThermalBaseService %>%
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

    # L244.GenericServiceSatiation: Satiation levels assumed for non-thermal building services
    # First, calculate the service output per unit floorspace in the USA region
    L244.ServiceSatiation_USA <- L144.base_service_EJ_serv %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      # Using left_join_keep_first_only b/c there are repeats and we only need to keep generic columns
      left_join_keep_first_only(calibrated_techs_bld_det, by = c("sector", "service")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, sector, service, gcam.consumer, nodeInput, building.node.input, building.service.input = supplysector, year, value)

    # Floorspace should be matched in for a specified year, from the full floorspace table (i.e. not one that is subsetted to model base years)
    L144.flsp_bm2_R_res_Yh <- L144.flsp_bm2_R_res_Yh %>%
      mutate(gcam.consumer = A44.gcam_consumer$gcam.consumer[grepl("res", A44.gcam_consumer$gcam.consumer)]) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L144.flsp_bm2_R_comm_Yh <- L144.flsp_bm2_R_comm_Yh %>%
      mutate(gcam.consumer = A44.gcam_consumer$gcam.consumer[grepl("com", A44.gcam_consumer$gcam.consumer)]) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L244.flsp_bm2_R <- bind_rows(L144.flsp_bm2_R_res_Yh, L144.flsp_bm2_R_comm_Yh) %>%
      # Again, used to be energy.SATIATION_YEAR, changed to pass timeshift test
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year)

    L244.ServiceSatiation_USA <- L244.ServiceSatiation_USA %>%
      left_join_error_no_match(L244.flsp_bm2_R %>%
                                 rename(floorspace_bm2 = value), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      group_by(region, sector, service) %>%
      mutate(satiation.level = round(value[year == max(HISTORICAL_YEARS)] * multiplier / floorspace_bm2, energy.DIGITS_CALOUTPUT)) %>%
      ungroup() %>%
      select(-multiplier)

    # Generic services: read these values to all regions because they're all the same
    L244.GenericServiceSatiation <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% generic_services) %>%
      select(-region, -GCAM_region_ID, -year, -value) %>%
      distinct() %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], GCAM_region_names = GCAM_region_names)

    # Need to match in the floorspace into the base service table, divide to calculate the service demand
    # per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
    L244.BS <- L244.GenericBaseService %>%
      left_join_error_no_match(L244.Floorspace, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      mutate(service.per.flsp = base.service / base.building.size) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input, service.per.flsp)

    L244.GenericServiceSatiation <- L244.GenericServiceSatiation %>%
      left_join_error_no_match(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
      mutate(satiation.level = pmax(satiation.level, service.per.flsp * 1.0001)) %>%
      select(-service.per.flsp)

    # L244.GenericServiceSatiation_SSPs: Satiation levels assumed for non-thermal building services in the SSPs
    # First, calculate the service output per unit floorspace in the USA region
    L244.ServiceSatiation_USA_SSPs <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% generic_services) %>%
      repeat_add_columns(tibble(SSP = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))) %>%
      left_join_error_no_match(A44.demand_satiation_mult_SSPs, by = c("SSP", "building.service.input" = "supplysector")) %>%
      group_by(region, sector, service) %>%
      mutate(satiation.level = round(value[year == max(HISTORICAL_YEARS)] * multiplier / floorspace_bm2, energy.DIGITS_CALOUTPUT)) %>%
      ungroup()

    # Generic services: read these values to all regions because they're all the same
    L244.GenericServiceSatiation_SSPs <- L244.ServiceSatiation_USA_SSPs %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], SSP, -region) %>%
      distinct() %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]], "SSP"), GCAM_region_names = GCAM_region_names)

    # Need to match in the floorspace into the base service table, divide to calculate the service demand
    # per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
    L244.GenericServiceSatiation_SSPs <- L244.GenericServiceSatiation_SSPs %>%
      left_join_error_no_match(L244.BS, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "building.service.input")) %>%
      mutate(satiation.level = pmax(satiation.level, service.per.flsp * 1.0001)) %>%
      select(-service.per.flsp) %>%
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

    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    # Write USA thermal satiation levels to all regions
    L244.ThermalServiceSatiation <- L244.ServiceSatiation_USA %>%
      filter(building.service.input %in% thermal_services) %>%
      select(-region, -GCAM_region_ID, -value, -year) %>%
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
    L244.tmp <- L244.ThermalBaseService %>%
      left_join_error_no_match(L244.Floorspace, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      mutate(service.per.flsp = base.service / base.building.size) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-base.service, - base.building.size, -year)

    # Then, match in this quantity into the thermal service satiation and take the max
    L244.ThermalServiceSatiation <- L244.ThermalServiceSatiation %>%
      left_join_error_no_match(L244.tmp, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "thermal.building.service.input")) %>%
      mutate(satiation.level = round(pmax(satiation.level, service.per.flsp * 1.0001),
                                     digits = energy.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.ShellConductance_bld: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld <- L144.shell_eff_R_Y %>%
      rename(shell.conductance = value) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(shell.conductance = round(shell.conductance, digits = energy.DIGITS_EFFICIENCY)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("supplysector" = "gcam.consumer")) %>%
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
    L244.Tech_bld <- L144.end_use_eff %>%
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

    # L244.FuelPrefElast_bld: Fuel preference elasticities for buildings
    L244.FuelPrefElast_bld <- A44.fuelprefElasticity %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FuelPrefElast"]], GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))

    # L244.FuelPrefElast_bld_SSP3: Fuel preference elasticities for buildings in SSP 3
    L244.FuelPrefElast_bld_SSP3 <- A44.fuelprefElasticity_SSP3 %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FuelPrefElast"]], GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))

    # L244.FuelPrefElast_bld_SSP4: Fuel preference elasticities for buildings in SSP 4
    L244.FuelPrefElast_bld_SSP4 <- A44.fuelprefElasticity_SSP4 %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FuelPrefElast"]], GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))

    # L244.FuelPrefElast_bld_SSP15: Fuel preference elasticities for buildings in SSP 1 & 5
    L244.FuelPrefElast_bld_SSP15 <- A44.fuelprefElasticity_SSP15 %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FuelPrefElast"]], GCAM_region_names = GCAM_region_names) %>%
      semi_join(L244.Tech_bld, by = c("region", "supplysector", "subsector"))

    # L244.StubTech_bld: Identification of stub technologies for buildings
    L244.StubTech_bld <- L244.Tech_bld %>%
      rename(stub.technology = technology)

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    L244.StubTechCalInput_bld <- L144.in_EJ_R_bld_serv_F_Yh %>%
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
    L244.StubTechEff_bld <- L144.end_use_eff %>%
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
    L244.GlobalTechCost_bld <- L144.NEcost_75USDGJ %>%
      mutate(input.cost = round(NEcostPerService, energy.DIGITS_COST)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # L244.StubTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services
    L244.StubTechIntGainOutputRatio <- L144.internal_gains %>%
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
      left_join_error_no_match(A44.gcam_consumer, by = "building.node.input") %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    InternalGainsScalar_USA <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)
    US.base.scalar <- tibble(variable, InternalGainsScalar_USA)

    L244.Intgains_scalar <- L244.ThermalServiceSatiation %>%
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      left_join_error_no_match(US.base.scalar, by = "variable") %>%
      left_join_error_no_match(L244.HDDCDD_normal_R_Y, by = c("region", "variable")) %>%
      group_by(thermal.building.service.input) %>%
      mutate(scalar_mult = degree.days / degree.days[region == gcam.USA_REGION]) %>%
      ungroup() %>%
      mutate(internal.gains.scalar = round(InternalGainsScalar_USA * scalar_mult, energy.DIGITS_HDDCDD)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    # Need to remove any services (supplysectors and building-service-inputs) and intgains trial markets for services that don't exist in any years
    # L244.DeleteThermalService and L244.DeleteGenericService: Removing non-existent services, likely related to 0 HDD or CDD
    L244.DeleteThermalService <- L244.ThermalBaseService %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, thermal.building.service.input) %>%
      summarise(base.service = max(base.service)) %>%
      ungroup() %>%
      filter(base.service == 0) %>%
      select(-base.service) %>%
      mutate(supplysector = thermal.building.service.input)

    L244.DeleteGenericService <- L244.GenericBaseService %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, building.service.input) %>%
      summarise(base.service = max(base.service)) %>%
      ungroup()

    # This tibble is empty because no base.service = 0, so we will make an if statement to produce correct empty tibble
    if(any(L244.DeleteGenericService$base.service == 0)) {
      L244.DeleteGenericService <- L244.DeleteGenericService%>%
        filter(base.service == 0) %>%
        select(-base.service) %>%
        mutate(supplysector = building.service.input)
    } else {
      rm("L244.DeleteGenericService")
    }



    # ===================================================
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

    L244.FuelPrefElast_bld_SSP3 %>%
      add_title("Fuel preference elasticities for buildings: SSP3") %>%
      add_units("Unitless") %>%
      add_comments("A44.fuelprefElasticity_SSP3 written to all regions") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP3") %>%
      add_precursors("energy/A44.fuelprefElasticity_SSP3", "common/GCAM_region_names", "L144.end_use_eff") ->
      L244.FuelPrefElast_bld_SSP3

    L244.FuelPrefElast_bld_SSP4 %>%
      add_title("Fuel preference elasticities for buildings: SSP4") %>%
      add_units("Unitless") %>%
      add_comments("A44.fuelprefElasticity_SSP4 written to all regions") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP4") %>%
      add_precursors("energy/A44.fuelprefElasticity_SSP4", "common/GCAM_region_names", "L144.end_use_eff") ->
      L244.FuelPrefElast_bld_SSP4

    L244.FuelPrefElast_bld_SSP15 %>%
      add_title("Fuel preference elasticities for buildings: SSP 1 & 5") %>%
      add_units("Unitless") %>%
      add_comments("A44.fuelprefElasticity_SSP15 written to all regions") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP15") %>%
      add_precursors("energy/A44.fuelprefElasticity_SSP15", "common/GCAM_region_names", "L144.end_use_eff") ->
      L244.FuelPrefElast_bld_SSP15

    L244.DeleteThermalService %>%
      add_title("Removing non-existent thermal services") %>%
      add_units("NA") %>%
      add_comments("Categories from L244.ThermalBaseService with no base.service") %>%
      add_legacy_name("L244.DeleteThermalService") %>%
      same_precursors_as(L244.ThermalBaseService) ->
      L244.DeleteThermalService

    return_data(L244.SubregionalShares, L244.PriceExp_IntGains, L244.Floorspace, L244.DemandFunction_serv, L244.DemandFunction_flsp,
                L244.Satiation_flsp, L244.SatiationAdder, L244.ThermalBaseService, L244.GenericBaseService, L244.ThermalServiceSatiation,
                L244.GenericServiceSatiation, L244.Intgains_scalar, L244.ShellConductance_bld,
                L244.Supplysector_bld, L244.FinalEnergyKeyword_bld, L244.SubsectorShrwt_bld, L244.SubsectorShrwtFllt_bld, L244.SubsectorInterp_bld,
                L244.SubsectorInterpTo_bld, L244.FuelPrefElast_bld,
                L244.StubTech_bld, L244.StubTechEff_bld, L244.StubTechCalInput_bld, L244.GlobalTechShrwt_bld,
                L244.GlobalTechCost_bld, L244.DeleteGenericService, L244.Satiation_flsp_SSP1, L244.SatiationAdder_SSP1,
                L244.GenericServiceSatiation_SSP1, L244.Satiation_flsp_SSP2,
                L244.SatiationAdder_SSP2, L244.GenericServiceSatiation_SSP2, L244.Satiation_flsp_SSP3, L244.SatiationAdder_SSP3,
                L244.GenericServiceSatiation_SSP3, L244.FuelPrefElast_bld_SSP3, L244.Satiation_flsp_SSP4,
                L244.SatiationAdder_SSP4, L244.GenericServiceSatiation_SSP4, L244.FuelPrefElast_bld_SSP4,
                L244.Satiation_flsp_SSP5, L244.SatiationAdder_SSP5, L244.GenericServiceSatiation_SSP5, L244.FuelPrefElast_bld_SSP15,
                L244.DeleteThermalService, L244.SubsectorLogit_bld, L244.StubTechIntGainOutputRatio,
                L244.HDDCDD_A2_CCSM3x, L244.HDDCDD_A2_HadCM3, L244.HDDCDD_B1_CCSM3x, L244.HDDCDD_B1_HadCM3, L244.HDDCDD_constdd_no_GCM,
                L244.GompFnParam)
  } else {
    stop("Unknown command")
  }
}
