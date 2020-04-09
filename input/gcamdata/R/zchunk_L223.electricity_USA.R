# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L223.electricity_USA
#'
#' Generates GCAM-USA model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.DeleteSubsector_USAelec},
#' \code{L223.Supplysector_USAelec}, \code{L223.SubsectorShrwtFllt_USAelec}, \code{L223.SubsectorInterp_USAelec},
#' \code{L223.SubsectorLogit_USAelec}, \code{L223.TechShrwt_USAelec}, \code{L223.TechCoef_USAelec},
#' \code{L223.Production_USAelec},\code{L223.PassthroughSector_elec_USA}, \code{L223.PassthroughTech_elec_FERC},
#' \code{L223.Supplysector_elec_FERC}, \code{L223.SubsectorShrwtFllt_elec_FERC}, \code{L223.SubsectorInterp_elec_FERC},
#' \code{L223.SubsectorLogit_elec_FERC}, \code{L223.TechShrwt_elec_FERC}, \code{L223.TechCoef_elec_FERC},
#' \code{L223.Production_elec_FERC}, \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC},
#' \code{L223.LaborForceFillout_FERC},\code{L223.Supplysector_elec_USA}, \code{L223.ElecReserve_USA},
#' \code{L223.SubsectorLogit_elec_USA}, \code{L223.SubsectorShrwtFllt_elec_USA}, \code{L223.SubsectorShrwt_nuc_USA},
#' \code{L223.SubsectorShrwt_renew_USA}, \code{L223.SubsectorInterp_elec_USA}, \code{L223.SubsectorInterpTo_elec_USA},
#' \code{L223.StubTech_elec_USA}, \code{L223.StubTechEff_elec_USA}, \code{L223.StubTechCapFactor_elec_USA},
#' \code{L223.StubTechFixOut_elec_USA}, \code{L223.StubTechFixOut_hydro_USA}, \code{L223.StubTechProd_elec_USA},
#' \code{L223.StubTechMarket_elec_USA}, \code{L223.StubTechMarket_backup_USA}, \code{L223.StubTechElecMarket_backup_USA},
#' \code{L223.StubTechCapFactor_elec_wind_USA}, \code{L223.StubTechCapFactor_elec_solar_USA}, \code{L223.StubTechCost_offshore_wind_USA}.
#' The corresponding file in the original data system was \code{L223.electricity_USA.R} (gcam-usa level2).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join matches mutate select semi_join summarise transmute rename
#' @author RC Oct 2017
module_gcamusa_L223.electricity_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             "L114.CapacityFactor_wind_state",
             "L119.CapFacScaler_PV_state",
             "L119.CapFacScaler_CSP_state",
             "L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.StubTechEff_elec",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechBackup_elec",
             "L1231.in_EJ_state_elec_F_tech",
             "L1231.out_EJ_state_elec_F_tech",
             "L1232.out_EJ_sR_elec",
             "L120.RsrcCurves_EJ_R_offshore_wind_USA",
             "L120.RegCapFactor_offshore_wind_USA",
             "L120.GridCost_offshore_wind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.DeleteSubsector_USAelec",
             "L223.Supplysector_USAelec",
             "L223.SubsectorShrwtFllt_USAelec",
             "L223.SubsectorInterp_USAelec",
             "L223.SubsectorLogit_USAelec",
             "L223.TechShrwt_USAelec",
             "L223.TechCoef_USAelec",
             "L223.Production_USAelec",
             "L223.PassthroughSector_elec_USA",
             "L223.PassthroughTech_elec_FERC",
             "L223.Supplysector_elec_FERC",
             "L223.SubsectorShrwtFllt_elec_FERC",
             "L223.SubsectorInterp_elec_FERC",
             "L223.SubsectorLogit_elec_FERC",
             "L223.TechShrwt_elec_FERC",
             "L223.TechCoef_elec_FERC",
             "L223.Production_elec_FERC",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.LaborForceFillout_FERC",
             "L223.Supplysector_elec_USA",
             "L223.ElecReserve_USA",
             "L223.SubsectorLogit_elec_USA",
             "L223.SubsectorShrwtFllt_elec_USA",
             "L223.SubsectorShrwt_nuc_USA",
             "L223.SubsectorShrwt_renew_USA",
             "L223.SubsectorInterp_elec_USA",
             "L223.SubsectorInterpTo_elec_USA",
             "L223.StubTech_elec_USA",
             "L223.StubTechEff_elec_USA",
             "L223.StubTechCapFactor_elec_USA",
             "L223.StubTechFixOut_elec_USA",
             "L223.StubTechFixOut_hydro_USA",
             "L223.StubTechProd_elec_USA",
             "L223.StubTechMarket_elec_USA",
             "L223.StubTechMarket_backup_USA",
             "L223.StubTechElecMarket_backup_USA",
             "L223.StubTechCapFactor_elec_wind_USA",
             "L223.StubTechCapFactor_elec_solar_USA",
             "L223.StubTechCost_offshore_wind_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- Geothermal_Hydrothermal_GWh <- state <- geo_state_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <- tech <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- CFmax <- grid.cost <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L114.CapacityFactor_wind_state <- get_data(all_data, "L114.CapacityFactor_wind_state")
    L119.CapFacScaler_PV_state <- get_data(all_data, "L119.CapFacScaler_PV_state")
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.CapFacScaler_CSP_state")
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")
    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, "L1231.in_EJ_state_elec_F_tech")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")
    L1232.out_EJ_sR_elec <- get_data(all_data, "L1232.out_EJ_sR_elec")
    L120.RsrcCurves_EJ_R_offshore_wind_USA <- get_data(all_data, "L120.RsrcCurves_EJ_R_offshore_wind_USA")
    L120.RegCapFactor_offshore_wind_USA <- get_data(all_data, "L120.RegCapFactor_offshore_wind_USA")
    L120.GridCost_offshore_wind_USA <- get_data(all_data, "L120.GridCost_offshore_wind_USA")


    # A vector of USA grid region names
    states_subregions %>%
      select(grid_region) %>%
      unique %>%
      arrange(grid_region) %>%
      unlist ->
      grid_regions

    elec_gen_names <- "electricity"

    # A vector indicating states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      left_join(states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      transmute(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      unlist ->
      geo_states_noresource

    # A vector indicating states where CSP electric technologies will not be created
    L119.CapFacScaler_CSP_state %>%
      # states with effectively no resource are assigned a capacity factor scalar of 0.001
      # remove these states to avoid creating CSP technologies there
      filter(scaler <= 0.01) %>%
      pull(state) -> CSP_states_noresource

    # PART 2: THE FERC REGIONS
    # NOTE: FERC grid regions function in similar fashion to the USA region:
    # competing electricity from subregions

    # L223.Supplysector_elec_FERC: supplysector for electricity sector in the grid regions,
    # including logit exponent between states within grid region
    # NOTE: use the same logit exponent for states within FERC region as for FERC regions within the USA
    tibble(region = grid_regions,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcamusa.GRID_REGION_LOGIT,
           logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L223.Supplysector_elec_FERC

    # L223.SubsectorShrwtFllt_elec_FERC: subsector (state) share-weights in grid regions
    states_subregions %>%
      select(region = grid_region, state) %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(state, supplysector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(-state) %>%
      arrange(region) ->
      L223.SubsectorShrwtFllt_elec_FERC

    # L223.SubsectorInterp_elec_FERC: temporal interpolation of subsector (state) share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L223.SubsectorInterp_elec_FERC

    # L223.SubsectorLogit_elec_FERC: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector, so the logit choice does not matter
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L223.SubsectorLogit_elec_FERC

    # L223.TechShrwt_elec_FERC: technology share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L223.TechShrwt_elec_FERC

    # L223.TechCoef_elec_FERC: technology coefficients and market names in grid regions
    L223.TechShrwt_elec_FERC %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L223.TechCoef_elec_FERC

    # L223.PassthroughSector_elec_USA: passthrough sector of US states
    # The marginal revenue sector is the region's electricity sector
    # whereas the marginal revenue market is the grid region.
    states_subregions %>%
      select(region = state, grid_region) %>%
      mutate(passthrough.sector = "electricity",
             marginal.revenue.sector = "electricity",
             marginal.revenue.market = grid_region) %>%
      select(-grid_region) ->
      L223.PassthroughSector_elec_USA

    # L223.PassthroughTech_elec_FERC: passthrough technology of grid regions
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    L223.TechShrwt_elec_FERC %>%
      select(region, supplysector, subsector, technology) ->
      L223.PassthroughTech_elec_FERC

    # L223.Production_elec_FERC: calibrated electricity production in grid region (consuming output of grid subregions)
    L1231.out_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
      mutate(subsector = paste(state, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the subsector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L223.TechCoef_elec_FERC %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L223.out_EJ_state_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             # tech.share.weights are set at technology level
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      # sub.share.weights are set the the subsector level in case with multiple technologies
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L223.Production_elec_FERC

    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)

    # L223.InterestRate_FERC: Interest rates in the FERC grid regions
    tibble(region = grid_regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L223.InterestRate_FERC

    # L223.Pop_FERC: Population
    tibble(region = grid_regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.Pop_FERC

    # L223.BaseGDP_FERC: Base GDP in FERC grid regions
    tibble(region = grid_regions,
           baseGDP = 1)  ->
      L223.BaseGDP_FERC

    # L223.LaborForceFillout_FERC: labor force in the grid regions
    tibble(region = grid_regions,
           year.fillout = min(MODEL_BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.LaborForceFillout_FERC


    # PART 3: THE STATES
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.USA_REGION) %>%
        write_to_all_states(names(data))

      if("subsector" %in% names(data_new)) {
        data_new <- data_new %>%
          filter(!paste(region, subsector) %in% geo_states_noresource)
      }

      if("stub.technology" %in% names(data_new)) {
        data_new <- data_new %>%
          filter(!(region %in% CSP_states_noresource) | !grepl("CSP", stub.technology))
      }

      # Re-set markets from USA to regional markets, if called for in the GCAM-USA assumptions for selected fuels
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions,state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      data_new
    }

    process_USA_to_states(L223.Supplysector_elec) -> L223.Supplysector_elec_USA
    process_USA_to_states(L223.ElecReserve) -> L223.ElecReserve_USA
    process_USA_to_states(L223.SubsectorLogit_elec) -> L223.SubsectorLogit_elec_USA
    process_USA_to_states(L223.SubsectorShrwtFllt_elec) -> L223.SubsectorShrwtFllt_elec_USA
    process_USA_to_states(L223.SubsectorShrwt_nuc) -> L223.SubsectorShrwt_nuc_USA
    process_USA_to_states(L223.SubsectorShrwt_renew) -> L223.SubsectorShrwt_renew_USA
    process_USA_to_states(L223.SubsectorInterp_elec) -> L223.SubsectorInterp_elec_USA
    process_USA_to_states(L223.SubsectorInterpTo_elec) -> L223.SubsectorInterpTo_elec_USA
    process_USA_to_states(L223.StubTech_elec) -> L223.StubTech_elec_USA
    process_USA_to_states(L223.StubTechEff_elec) -> L223.StubTechEff_elec_USA
    process_USA_to_states(L223.StubTechCapFactor_elec) -> L223.StubTechCapFactor_elec_USA

    # NOTE: Modify the share-weight path for nuclear to include state preferences
    L1231.out_EJ_state_elec_F_tech %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(state) %>%
      summarise(elec = sum(value)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L1231.out_EJ_state_elec_F_tech %>%
      filter(fuel == "nuclear", year == max(HISTORICAL_YEARS)) %>%
      left_join_error_no_match(L223.out_EJ_state_elec, by = "state") %>%
      mutate(share = value / elec,
             avg.share = sum(value) / sum(elec),
             pref = share / avg.share) %>%
      select(state, pref) %>%
      # Just set some bounds on the share weight multiplier
      mutate(share.weight.mult = pref,
             share.weight.mult = replace(share.weight.mult, share.weight.mult < 0.1, 0.1),
             share.weight.mult = replace(share.weight.mult, share.weight.mult > 2, 2),
             # Set the state of VT to zero because VT has already shut down the only nuclear plant
             # which used to account for about 70% of generation
             share.weight.mult = replace(share.weight.mult, state == "VT", 0)) ->
      L223.state_nuc_pref

    L223.SubsectorShrwt_nuc_USA %>%
      left_join_error_no_match(L223.state_nuc_pref, by = c("region" = "state")) %>%
      mutate(share.weight = round(share.weight * share.weight.mult, digits = energy.DIGITS_COST)) %>%
      select(-pref, -share.weight.mult) ->
      L223.SubsectorShrwt_nuc_USA

    # Stub technology information for state electricity generation
    # calibration
    L1231.in_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) %>%
      filter(calibration == "input") ->
      L223.in_EJ_state_elec_F_tech

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year
    L1231.out_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_YEARS, year %in% HISTORICAL_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) ->
      L223.out_EJ_state_elec_F_tech

    L223.out_EJ_state_elec_F_tech %>%
      filter(calibration == "fixed output") ->
      L223.fixout_EJ_state_elec_F_tech

    L223.out_EJ_state_elec_F_tech %>%
      filter(calibration != "fixed output") ->
      L223.calout_EJ_state_elec_F_tech

    # L223.StubTechFixOut_elec_USA: fixed output of electricity generation technologies
    L223.fixout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(fixedOutput = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(-calOutputValue) ->
      L223.StubTechFixOut_elec_USA

    # Add in future hydropower generation here
    # L223.StubTechFixOut_hydro_USA: fixed output of future hydropower
    # NOTE: This just holds it constant for now;
    # at some point, should downscale of the (almost completely flat) nation-level projection
    L223.StubTechFixOut_elec_USA %>%
      filter(grepl("hydro", stub.technology), year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L223.StubTechFixOut_hydro_USA

    # L223.StubTechProd_elec_USA: calibrated output of electricity generation technologies
    L223.calout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) %>%
      filter(!(region %in% CSP_states_noresource) | !grepl("CSP", stub.technology)) ->
      L223.StubTechProd_elec_USA

    # L223.StubTechMarket_elec_USA: market names of inputs to state electricity sectors
    L223.StubTech_elec_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # For rooftop_pv (technology), match in distributed_solar instead of backup_electricity (minicam.energy.input)
      left_join_keep_first_only(select(A23.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Remove NA rows for hydro
      na.omit %>%
      mutate(market.name = gcam.USA_REGION,
             market.name = replace(market.name,
                                   minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, gcamusa.STATE_UNLIMITED_RESOURCES),
                                   region[minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, gcamusa.STATE_UNLIMITED_RESOURCES)])) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.StubTechMarket_elec_USA

    L223.StubTechMarket_elec_USA %>%
      left_join_error_no_match(select(states_subregions, grid_region, state), by = c("region" = "state")) %>%
      mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
      select(-grid_region) ->
      L223.StubTechMarket_elec_USA

    # L223.StubTechMarket_backup_USA: market names of backup inputs to state electricity sectors
    L223.GlobalIntTechBackup_elec %>%
      mutate(supplysector = sector.name, subsector = subsector.name) %>%
      write_to_all_states(names = c(names(.), 'region')) %>%
      filter(!(region %in% CSP_states_noresource) | !grepl("CSP", technology)) %>%
      mutate(market.name = gcam.USA_REGION, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L223.StubTechMarket_backup_USA

    # L223.StubTechCapFactor_elec_wind_USA: capacity factors for wind electricity in the states
    # Just use the subsector for matching - technologies include storage technologies as well
    L114.CapacityFactor_wind_state %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector),
                               by = c("sector", "fuel")) ->
      L223.CapacityFactor_wind_state

    L223.StubTechCapFactor_elec %>%
      filter(region == gcam.USA_REGION) %>%
      semi_join(L223.CapacityFactor_wind_state, by = c("supplysector", "subsector")) %>%
      select(-region, -capacity.factor) %>%
      write_to_all_states(names = c(names(.), "region")) %>%
      left_join_error_no_match(L223.CapacityFactor_wind_state,
                               by = c("region" = "state", "supplysector", "subsector")) %>%
      mutate(capacity.factor = round(capacity.factor, digits = energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_wind_USA

    # L223.StubTechCapFactor_elec_solar_USA: capacity factors by state and solar electric technology
    L119.CapFacScaler_PV_state %>%
      bind_rows(L119.CapFacScaler_CSP_state) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel")) ->
      L223.CapFacScaler_solar_state

    # Just use the subsector for matching - technologies include storage technologies as well
    L223.StubTechCapFactor_elec %>%
      filter(region == gcam.USA_REGION) %>%
      semi_join(L223.CapFacScaler_solar_state, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      write_to_all_states(., c(names(.), "region")) %>%
      # For matching capacity factors to technologies, create a variable (tech) that matches what's in the capacity factor table
      mutate(tech = sub("_storage", "", stub.technology)) %>%
      left_join_error_no_match(L223.CapFacScaler_solar_state,
                               by = c("region" = "state", "supplysector", "subsector", "tech" = "technology")) %>%
      mutate(capacity.factor = round(capacity.factor * scaler, digits = energy.DIGITS_COST)) %>%
      filter(!(region %in% CSP_states_noresource) | !grepl("CSP", tech)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_solar_USA

    # Modifications for offshore wind
    # Remove states with no offshore wind resources
    offshore_wind_states <- unique(L120.RsrcCurves_EJ_R_offshore_wind_USA$region)

    L223.StubTech_elec_USA %>%
      filter(stub.technology != "wind_offshore") %>%
      bind_rows(L223.StubTech_elec_USA %>%
                  filter(stub.technology == "wind_offshore",
                         region %in% offshore_wind_states)) -> L223.StubTech_elec_USA

    L223.StubTechCapFactor_elec_USA %>%
      filter(stub.technology != "wind_offshore") %>%
      bind_rows(L223.StubTechCapFactor_elec_USA %>%
                  filter(stub.technology == "wind_offshore",
                         region %in% offshore_wind_states)) -> L223.StubTechCapFactor_elec_USA

    L223.StubTechMarket_elec_USA %>%
      filter(stub.technology != "wind_offshore") %>%
      bind_rows(L223.StubTechMarket_elec_USA %>%
                  filter(stub.technology == "wind_offshore",
                         region %in% offshore_wind_states)) -> L223.StubTechMarket_elec_USA

    L223.StubTechMarket_backup_USA %>%
      filter(stub.technology != "wind_offshore") %>%
      bind_rows(L223.StubTechMarket_backup_USA %>%
                  filter(stub.technology == "wind_offshore",
                         region %in% offshore_wind_states)) -> L223.StubTechMarket_backup_USA

    # Replacing with correct state-specific offshore wind capacity factors
    L223.StubTechCapFactor_elec_wind_USA %>%
      filter(stub.technology == "wind_offshore",
             region %in% offshore_wind_states) %>%
      select(-capacity.factor) %>%
      left_join_error_no_match(L120.RegCapFactor_offshore_wind_USA,
                               by= c("region" = "State")) %>%
      mutate(capacity.factor= round(CFmax,energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year,
             capacity.factor) -> L223.StubTechCapFactor_elec_offshore_wind_USA

    L223.StubTechCapFactor_elec_wind_USA %>%
      filter(stub.technology != "wind_offshore") %>%
      bind_rows(L223.StubTechCapFactor_elec_offshore_wind_USA) -> L223.StubTechCapFactor_elec_wind_USA

    # L223.StubTechCost_offshore_wind_USA: State-specific non-energy cost adder for offshore wind grid connection cost
    L223.StubTechCapFactor_elec_wind_USA %>%
      filter(stub.technology == "wind_offshore") %>%
      select(region, supplysector, subsector, stub.technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join(L120.GridCost_offshore_wind_USA, by = c("region" = "State")) %>%
      rename(input.cost = grid.cost) -> L223.StubTechCost_offshore_wind_USA


    # Produce outputs

    missing_data() %>%
      add_legacy_name("L223.DeleteSubsector_USAelec") ->
      L223.DeleteSubsector_USAelec

    missing_data() %>%
        add_legacy_name("L223.Supplysector_USAelec") ->
        L223.Supplysector_USAelec

    missing_data() %>%
        add_legacy_name("L223.SubsectorShrwtFllt_USAelec") ->
        L223.SubsectorShrwtFllt_USAelec

    missing_data() %>%
      add_legacy_name("L223.SubsectorInterp_USAelec") ->
      L223.SubsectorInterp_USAelec

    missing_data() %>%
      add_legacy_name("L223.SubsectorLogit_USAelec") ->
      L223.SubsectorLogit_USAelec

    missing_data() %>%
      add_legacy_name("L223.TechShrwt_USAelec") ->
      L223.TechShrwt_USAelec

    missing_data() %>%
      add_legacy_name("L223.TechCoef_USAelec") ->
      L223.TechCoef_USAelec

    missing_data() %>%
      add_legacy_name("L223.Production_USAelec") %>%
      add_precursors("L1232.out_EJ_sR_elec") ->
      L223.Production_USAelec

    L223.Supplysector_elec_FERC %>%
      add_title("Supplysector information for electricity sector in the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include logit exponent between states") %>%
      add_comments("Use the same logit exponent for states within FERC region as for FERC regions within the USA") %>%
      add_legacy_name("L223.Supplysector_elec_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.Supplysector_elec_FERC

    L223.SubsectorShrwtFllt_elec_FERC %>%
      add_title("Subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set share-weights for states within grid region") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.SubsectorShrwtFllt_elec_FERC

    L223.SubsectorInterp_elec_FERC %>%
      add_title("Table header for temporal interpolation of subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set up temporal interterpolation of state share-weights within grid region") %>%
      add_legacy_name("L223.SubsectorInterp_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.SubsectorInterp_elec_FERC

    L223.SubsectorLogit_elec_FERC %>%
      add_title("Logit exponent of subsector (states) in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L223.SubsectorLogit_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.SubsectorLogit_elec_FERC

    L223.TechShrwt_elec_FERC %>%
      add_title("Technology share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_legacy_name("L223.TechShrwt_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.TechShrwt_elec_FERC

    L223.TechCoef_elec_FERC %>%
      add_title("Technology coefficients and market names in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_comments("Market name is state name") %>%
      add_legacy_name("L223.TechCoef_elec_FERC") %>%
      same_precursors_as("L223.TechShrwt_elec_FERC") ->
      L223.TechCoef_elec_FERC

    L223.PassthroughSector_elec_USA %>%
      add_title("Passthrough sector of the states") %>%
      add_units("Unitless") %>%
      add_comments("The marginal revenue sector is the region's electricity sector.") %>%
      add_comments("The marginal revenue market is the grid region.") %>%
      add_legacy_name("L223.PassthroughSector_elec_USA") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.PassthroughSector_elec_USA

    L223.PassthroughTech_elec_FERC %>%
      add_title("Passthrough technology of the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("This contains region, supplysector, subsector, technology for the grid regions") %>%
      add_comments("to which electricity produced in states is passed through") %>%
      add_legacy_name("L223.PassthroughTech_elec_FERC") %>%
      same_precursors_as("L223.TechShrwt_elec_FERC") ->
      L223.PassthroughTech_elec_FERC

    L223.Production_elec_FERC %>%
      add_title("Calibrated electricity production of subsectors in grid regions") %>%
      add_units("EJ") %>%
      add_comments("Subsector share-weight is zero if production of all technologies in the subsector is zero") %>%
      add_comments("Technology share-weight is zero if production of the technology is zero") %>%
      add_legacy_name("L223.Production_elec_FERC") %>%
      add_precursors("L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.Production_elec_FERC

    L223.InterestRate_FERC %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L223.InterestRate_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.InterestRate_FERC

    L223.Pop_FERC %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L223.Pop_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.Pop_FERC

    L223.BaseGDP_FERC %>%
      add_title("Base GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.BaseGDP_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.BaseGDP_FERC

    L223.LaborForceFillout_FERC %>%
      add_title("Labor force in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default labor force") %>%
      add_legacy_name("L223.LaborForceFillout_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.LaborForceFillout_FERC

    L223.Supplysector_elec_USA %>%
      add_title("Supplysector information of electricity sector in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.Supplysector_elec_USA") %>%
      add_precursors("L223.Supplysector_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.Supplysector_elec_USA

    L223.ElecReserve_USA %>%
      add_title("Electricity reserve margin and average grid capacity factor in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.ElecReserve_USA") %>%
      add_precursors("L223.ElecReserve",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.ElecReserve_USA

    L223.SubsectorLogit_elec_USA %>%
      add_title("Logit exponent of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorLogit_elec_USA") %>%
      add_precursors("L223.SubsectorLogit_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_elec_USA

    L223.SubsectorShrwtFllt_elec_USA %>%
      add_title("Subsector (fuel) share-weights in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_USA") %>%
      add_precursors("L223.SubsectorShrwtFllt_elec") ->
      L223.SubsectorShrwtFllt_elec_USA

    L223.SubsectorShrwt_nuc_USA %>%
      add_title("Share-weights for nuclear in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("Modify the share-weight path for nuclear to include state preferences") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc_USA") %>%
      add_precursors("L223.SubsectorShrwt_nuc",
                     "L1231.out_EJ_state_elec_F_tech",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_nuc_USA

    L223.SubsectorShrwt_renew_USA %>%
      add_title("Share-weights for renewable energy in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwt_renew_USA") %>%
      add_precursors("L223.SubsectorShrwt_renew",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_renew_USA

    L223.SubsectorInterp_elec_USA %>%
      add_title("Temporal (2100) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterp_elec_USA") %>%
      add_precursors("L223.SubsectorInterp_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterp_elec_USA

    L223.SubsectorInterpTo_elec_USA %>%
      add_title("Temporal (2300) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec_USA") %>%
      add_precursors("L223.SubsectorInterpTo_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterpTo_elec_USA

    L223.StubTech_elec_USA %>%
      add_title("Stub technology information for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.StubTech_elec_USA") %>%
      add_precursors("L223.StubTech_elec",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTech_elec_USA

    L223.StubTechEff_elec_USA %>%
      add_title("Stub technology efficiency for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("Re-set markets from USA to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechEff_elec_USA") %>%
      add_precursors("L223.StubTechEff_elec",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechEff_elec_USA

    L223.StubTechCapFactor_elec_USA %>%
      add_title("Capacity factor of stub technology for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_USA") %>%
      add_precursors("L223.StubTechCapFactor_elec",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechCapFactor_elec_USA

    L223.StubTechFixOut_elec_USA %>%
      add_title("Fixed outputs of stub technology electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("Applied to historical model years") %>%
      add_legacy_name("L223.StubTechFixOut_elec_USA") %>%
      add_precursors("L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.StubTechFixOut_elec_USA

    L223.StubTechFixOut_hydro_USA %>%
      add_title("Fixed outputs of future hydropower electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("This just holds it constant for now.") %>%
      add_legacy_name("L223.StubTechFixOut_hydro_USA") %>%
      same_precursors_as("L223.StubTechFixOut_elec_USA") ->
      L223.StubTechFixOut_hydro_USA

    L223.StubTechProd_elec_USA %>%
      add_title("Calibrated outputs of electricity stub technology in the states") %>%
      add_units("EJ") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec_USA") %>%
      add_precursors("L1231.in_EJ_state_elec_F_tech",
                     "L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechProd_elec_USA

    L223.StubTechMarket_elec_USA %>%
      add_title("Market names of inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Re-set markets from USA to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechMarket_elec_USA") %>%
      same_precursors_as("L223.StubTech_elec_USA") %>%
      add_precursors("energy/A23.globaltech_eff",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechMarket_elec_USA

    L223.StubTechMarket_backup_USA %>%
      add_title("Market names of backup inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Set market as USA") %>%
      add_legacy_name("L223.StubTechMarket_backup_USA") %>%
      add_precursors("L223.GlobalIntTechBackup_elec",
                     "gcam-usa/states_subregions",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechMarket_backup_USA

    if(exists("L223.StubTechElecMarket_backup_USA")) {
      L223.StubTechElecMarket_backup_USA %>%
        add_title("Market name of electricity sector for backup calculations") %>%
        add_units("Unitless") %>%
        add_comments("The backup electric market is only set here if regional electricity markets are not used") %>%
        add_legacy_name("L223.StubTechElecMarket_backup_USA") %>%
        same_precursors_as("L223.StubTechMarket_backup_USA") ->
        L223.StubTechElecMarket_backup_USA
    } else {
      # If regional electricity markets are not used,
      # then a blank tibble of the backup electric market is produced.
      missing_data() %>%
        add_legacy_name("L223.StubTechElecMarket_backup_USA") ->
        L223.StubTechElecMarket_backup_USA
    }

    L223.StubTechCapFactor_elec_wind_USA %>%
      add_title("Capacity factors for wind electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_wind_USA") %>%
      add_precursors("L114.CapacityFactor_wind_state",
                     "energy/calibrated_techs",
                     "gcam-usa/states_subregions") ->
      L223.StubTechCapFactor_elec_wind_USA

    L223.StubTechCapFactor_elec_solar_USA %>%
      add_title("Capacity factors for solar electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_solar_USA") %>%
      add_precursors("L119.CapFacScaler_PV_state",
                     "L119.CapFacScaler_CSP_state",
                     "energy/calibrated_techs",
                     "gcam-usa/states_subregions",
                     "L119.CapFacScaler_CSP_state") ->
      L223.StubTechCapFactor_elec_solar_USA

    L223.StubTechCost_offshore_wind_USA %>%
      add_title("State-specific non-energy cost adder for offshore wind grid connection cost") %>%
      add_units("Unitless") %>%
      add_comments("Adder") %>%
      add_precursors("L114.CapacityFactor_wind_state",
                     "L223.Supplysector_elec",
                     "L223.ElecReserve",
                     "L223.SubsectorLogit_elec",
                     "L223.SubsectorShrwtFllt_elec",
                     "L223.SubsectorShrwt_renew",
                     "L223.SubsectorInterp_elec",
                     "L223.SubsectorInterpTo_elec",
                     "L223.StubTech_elec",
                     "L223.StubTechEff_elec",
                     "L223.StubTechCapFactor_elec",
                     "L223.GlobalIntTechBackup_elec",
                     "L1231.in_EJ_state_elec_F_tech",
                     "L1231.out_EJ_state_elec_F_tech",
                     "L1232.out_EJ_sR_elec",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA")->
      L223.StubTechCost_offshore_wind_USA

    return_data(L223.DeleteSubsector_USAelec,
                L223.Supplysector_USAelec,
                L223.SubsectorShrwtFllt_USAelec,
                L223.SubsectorInterp_USAelec,
                L223.SubsectorLogit_USAelec,
                L223.TechShrwt_USAelec,
                L223.TechCoef_USAelec,
                L223.Production_USAelec,
                L223.PassthroughSector_elec_USA,
                L223.PassthroughTech_elec_FERC,
                L223.Supplysector_elec_FERC,
                L223.SubsectorShrwtFllt_elec_FERC,
                L223.SubsectorInterp_elec_FERC,
                L223.SubsectorLogit_elec_FERC,
                L223.TechShrwt_elec_FERC,
                L223.TechCoef_elec_FERC,
                L223.Production_elec_FERC,
                L223.InterestRate_FERC,
                L223.Pop_FERC,
                L223.BaseGDP_FERC,
                L223.LaborForceFillout_FERC,
                L223.Supplysector_elec_USA,
                L223.ElecReserve_USA,
                L223.SubsectorLogit_elec_USA,
                L223.SubsectorShrwtFllt_elec_USA,
                L223.SubsectorShrwt_nuc_USA,
                L223.SubsectorShrwt_renew_USA,
                L223.SubsectorInterp_elec_USA,
                L223.SubsectorInterpTo_elec_USA,
                L223.StubTech_elec_USA,
                L223.StubTechEff_elec_USA,
                L223.StubTechCapFactor_elec_USA,
                L223.StubTechFixOut_elec_USA,
                L223.StubTechFixOut_hydro_USA,
                L223.StubTechProd_elec_USA,
                L223.StubTechMarket_elec_USA,
                L223.StubTechMarket_backup_USA,
                L223.StubTechElecMarket_backup_USA,
                L223.StubTechCapFactor_elec_wind_USA,
                L223.StubTechCapFactor_elec_solar_USA,
                L223.StubTechCost_offshore_wind_USA)
  } else {
    stop("Unknown command")
  }
}
