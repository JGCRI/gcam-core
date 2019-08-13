#' module_gcamindia_L223.electricity
#'
#' Generates GCAM-india model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.DeleteSubsector_indiaelec},
#' \code{L223.india_state_Supplysectorelec}, \code{L223.india_state_SubsectorShrwtFlltelec}, \code{L223.india_state_SubsectorInterpelec},
#' \code{L223.india_state_SubsectorLogitelec}, \code{L223.india_state_TechShrwt_elec}, \code{L223.india_state_TechCoef_elec},
#' \code{L223.india_state_Production_elec},\code{L223.india_state_PassthroughSector_elec}, \code{L223.india_state_PassthroughTech_elec_CEA},
#' \code{L223.india_state_Supplysector_elec_CEA}, \code{L223.india_state_SubsectorShrwtFllt_elec_CEA}, \code{L223.india_state_SubsectorInterp_elec_CEA},
#' \code{L223.india_state_SubsectorLogit_elec_CEA}, \code{L223.india_state_TechShrwt_elec_CEA}, \code{L223.india_state_TechCoef_elec_CEA},
#' \code{L223.india_state_Production_elec_CEA}, \code{L223.india_state_InterestRate_CEA}, \code{L223.india_state_Pop_CEA}, \code{L223.india_state_BaseGDP_CEA},
#' \code{L223.india_state_LaborForceFillout_CEA},\code{L223.india_state_Supplysector_elec}, \code{L223.india_state_ElecReserve},
#' \code{L223.india_state_SubsectorLogit_elec}, \code{L223.india_state_SubsectorShrwtFllt_elec}, \code{L223.india_state_SubsectorShrwt_nuc},
#' \code{L223.india_state_SubsectorShrwt_renew}, \code{L223.india_state_SubsectorInterp_elec}, \code{L223.india_state_SubsectorInterpTo_elec},
#' \code{L223.india_state_StubTech_elec}, \code{L223.india_state_StubTechEff_elec}, \code{L223.india_state_StubTechCapFactor_elec},
#' \code{L223.india_state_StubTechFixOut_elec}, \code{L223.india_state_StubTechFixOut_hydro}, \code{L223.india_state_StubTechProd_elec},
#' \code{L223.india_state_StubTechMarket_elec}, \code{L223.india_state_StubTechMarket_backup}, \code{L223.india_state_StubTechElecMarket_backup},
#' \code{L223.india_state_StubTechCapFactor_elec_wind}, \code{L223.india_state_StubTechCapFactor_elec_solar}. The corresponding file in the
#' original data system was \code{L223.electricity_india.R} (gcam-india level2).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author VC July 2019
module_gcamindia_L223.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-india/A23.india_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             "L114.india_state_CapacityFactor_wind",
             "L119.india_state_CapFacScaler_PV",
             "L119.india_state_CapFacScaler_CSP",
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
             "L1231.india_state_in_EJ_elec_F_tech",
             "L1231.india_state_out_EJ_elec_F_tech",
             "L1232.india_state_out_EJ_sR_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.DeleteSubsector_indiaelec",
             "L223.india_state_Supplysectorelec",
             "L223.india_state_SubsectorShrwtFlltelec",
             "L223.india_state_SubsectorInterpelec",
             "L223.india_state_SubsectorLogitelec",
             "L223.india_state_TechShrwt_elec",
             "L223.india_state_TechCoef_elec",
             "L223.india_state_Production_elec",
             "L223.india_state_PassthroughSector_elec",
             "L223.india_state_PassthroughTech_elec_CEA",
             "L223.india_state_Supplysector_elec_CEA",
             "L223.india_state_SubsectorShrwtFllt_elec_CEA",
             "L223.india_state_SubsectorInterp_elec_CEA",
             "L223.india_state_SubsectorLogit_elec_CEA",
             "L223.india_state_TechShrwt_elec_CEA",
             "L223.india_state_TechCoef_elec_CEA",
             "L223.india_state_Production_elec_CEA",
             "L223.india_state_InterestRate_CEA",
             "L223.india_state_Pop_CEA",
             "L223.india_state_BaseGDP_CEA",
             "L223.india_state_LaborForceFillout_CEA",
             "L223.india_state_Supplysector_elec",
             "L223.india_state_ElecReserve",
             "L223.india_state_SubsectorLogit_elec",
             "L223.india_state_SubsectorShrwtFllt_elec",
             "L223.india_state_SubsectorShrwt_nuc",
             "L223.india_state_SubsectorShrwt_renew",
             "L223.india_state_SubsectorInterp_elec",
             "L223.india_state_SubsectorInterpTo_elec",
             "L223.india_state_StubTech_elec",
             "L223.india_state_StubTechEff_elec",
             "L223.india_state_StubTechCapFactor_elec",
             "L223.india_state_StubTechFixOut_elec",
             "L223.india_state_StubTechFixOut_hydro",
             "L223.india_state_StubTechProd_elec",
             "L223.india_state_StubTechMarket_elec",
             "L223.india_state_StubTechMarket_backup",
             "L223.india_state_StubTechElecMarket_backup",
             "L223.india_state_StubTechCapFactor_elec_wind",
             "L223.india_state_StubTechCapFactor_elec_solar"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- Geothermal_Hydrothermal_GWh <- state <- geo_state_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- NULL  # silence package check notes

    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.india_re_technical_potential <- get_data(all_data, "gcam-india/A23.india_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L114.CapacityFactor_wind_state <- get_data(all_data, "L114.india_state_CapacityFactor_wind")
    L119.CapFacScaler_PV_state <- get_data(all_data, "L119.india_state_CapFacScaler_PV")
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.india_state_CapFacScaler_CSP")
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
    L1231.india_state_in_EJ_elec_F_tech <- get_data(all_data, "L1231.india_state_in_EJ_elec_F_tech")
    L1231.india_state_out_EJ_elec_F_tech <- get_data(all_data, "L1231.india_state_out_EJ_elec_F_tech")
    L1232.india_state_out_EJ_sR_elec <- get_data(all_data, "L1232.india_state_out_EJ_sR_elec")


    # A vector of india grid region names
    grid_regions <- india_states_subregions %>%
      select(grid_region) %>%
      unique %>%
      arrange(grid_region) %>%
      unlist


    elec_gen_names <- "electricity"

    # A vector indicating states where geothermal electric technologies will not be created
    geo_states_noresource <- A23.india_re_technical_potential %>%
      left_join(india_states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      transmute(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      unlist


    # PART 2: THE FERC REGIONS
    # NOTE: FERC grid regions function in similar fashion to the india region:
    # competing electricity from subregions

    # L223.india_state_Supplysector_elec_CEA: supplysector for electricity sector in the grid regions,
    # including logit exponent between states within grid region
    L223.india_state_Supplysector_elec_CEA <- tibble(region = grid_regions,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcamindia.GRID_REGION_LOGIT,
           logit.type = gcamindia.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME))


    # L223.india_state_SubsectorShrwtFllt_elec_CEA: subsector (state) share-weights in grid regions
    L223.india_state_SubsectorShrwtFllt_elec_CEA <- india_states_subregions %>%
      select(region = grid_region, state) %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(state, supplysector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(-state) %>%
      arrange(region)


    # L223.india_state_SubsectorInterp_elec_CEA: temporal interpolation of subsector (state) share-weights in grid regions
    L223.india_state_SubsectorInterp_elec_CEA <- L223.india_state_SubsectorShrwtFllt_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed")


    # L223.india_state_SubsectorLogit_elec_CEA: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector, so the logit choice does not matter
    L223.india_state_SubsectorLogit_elec_CEA <- L223.india_state_SubsectorShrwtFllt_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamindia.GRID_REGION_LOGIT,
             logit.type = gcamindia.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME))


    # L223.india_state_TechShrwt_elec_CEA: technology share-weights in grid regions
    L223.india_state_TechShrwt_elec_CEA <- L223.india_state_SubsectorShrwtFllt_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1)


    # L223.india_state_TechCoef_elec_CEA: technology coefficients and market names in grid regions
    L223.india_state_TechShrwt_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L223.india_state_TechCoef_elec_CEA

    # L223.india_state_PassthroughSector_elec: passthrough sector of US states
    # The marginal revenue sector is the region's electricity sector
    # whereas the marginal revenue market is the grid region.
    india_states_subregions %>%
      select(region = state, grid_region) %>%
      mutate(passthrough.sector = "electricity",
             marginal.revenue.sector = "electricity",
             marginal.revenue.market = grid_region) %>%
      select(-grid_region) ->
      L223.india_state_PassthroughSector_elec

    # L223.india_state_PassthroughTech_elec_CEA: passthrough technology of grid regions
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    L223.india_state_TechShrwt_elec_CEA %>%
      select(region, supplysector, subsector, technology) ->
      L223.india_state_PassthroughTech_elec_CEA

    # L223.india_state_Production_elec_CEA: calibrated electricity production in grid region (consuming output of grid subregions)
    L1231.india_state_out_EJ_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
      mutate(subsector = paste(state, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the subsector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L223.india_state_TechCoef_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L223.out_EJ_state_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             # tech.share.weights are set at technology level
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      # sub.share.weights are set the the subsector level in case with multiple technologies
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L223.india_state_Production_elec_CEA

    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)

    # L223.india_state_InterestRate_CEA: Interest rates in the FERC grid regions
    tibble(region = grid_regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L223.india_state_InterestRate_CEA

    # L223.india_state_Pop_CEA: Population
    tibble(region = grid_regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.india_state_Pop_CEA

    # L223.india_state_BaseGDP_CEA: Base GDP in grid regions
    tibble(region = grid_regions,
           baseGDP = 1)  ->
      L223.india_state_BaseGDP_CEA

    # L223.india_state_LaborForceFillout_CEA: labor force in the grid regions
    tibble(region = grid_regions,
           year.fillout = min(MODEL_BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.india_state_LaborForceFillout_CEA


    # PART 3: THE STATES
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_india_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.india_REGION) %>%
        write_to_all_india_states(names(data))

      if("subsector" %in% names(data_new)) {
        data_new <- data_new %>%
          filter(!paste(region, subsector) %in% geo_states_noresource)
      }

      # Re-set markets from india to regional markets, if called for in the gcam-india assumptions for selected fuels
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(india_states_subregions,state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      data_new
    }

    process_india_to_states(L223.Supplysector_elec) -> L223.india_state_Supplysector_elec
    process_india_to_states(L223.ElecReserve) -> L223.india_state_ElecReserve
    process_india_to_states(L223.SubsectorLogit_elec) -> L223.india_state_SubsectorLogit_elec
    process_india_to_states(L223.SubsectorShrwtFllt_elec) -> L223.india_state_SubsectorShrwtFllt_elec
    process_india_to_states(L223.SubsectorShrwt_nuc) -> L223.india_state_SubsectorShrwt_nuc
    process_india_to_states(L223.SubsectorShrwt_renew) -> L223.india_state_SubsectorShrwt_renew
    process_india_to_states(L223.SubsectorInterp_elec) -> L223.india_state_SubsectorInterp_elec
    process_india_to_states(L223.SubsectorInterpTo_elec) -> L223.india_state_SubsectorInterpTo_elec
    process_india_to_states(L223.StubTech_elec) -> L223.india_state_StubTech_elec
    process_india_to_states(L223.StubTechEff_elec) -> L223.india_state_StubTechEff_elec
    process_india_to_states(L223.StubTechCapFactor_elec) -> L223.india_state_StubTechCapFactor_elec

    # NOTE: Modify the share-weight path for nuclear to include state preferences
    L1231.india_state_out_EJ_elec_F_tech %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(state) %>%
      summarise(elec = sum(value)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L1231.india_state_out_EJ_elec_F_tech %>%
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

    L223.india_state_SubsectorShrwt_nuc %>%
      left_join_error_no_match(L223.state_nuc_pref, by = c("region" = "state")) %>%
      mutate(share.weight = round(share.weight * share.weight.mult, digits = energy.DIGITS_COST)) %>%
      select(-pref, -share.weight.mult) ->
      L223.india_state_SubsectorShrwt_nuc

    # Stub technology information for state electricity generation
    # calibration
    L1231.india_state_in_EJ_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) %>%
      filter(calibration == "input") ->
      L223.in_EJ_state_elec_F_tech

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year
    L1231.india_state_out_EJ_elec_F_tech %>%
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

    # L223.india_state_StubTechFixOut_elec: fixed output of electricity generation technologies
    L223.fixout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(fixedOutput = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(-calOutputValue) ->
      L223.india_state_StubTechFixOut_elec

    # Add in future hydropower generation here
    # L223.india_state_StubTechFixOut_hydro: fixed output of future hydropower
    # NOTE: This just holds it constant for now;
    # at some point, should downscale of the (almost completely flat) nation-level projection
    L223.india_state_StubTechFixOut_elec %>%
      filter(grepl("hydro", stub.technology), year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L223.india_state_StubTechFixOut_hydro

    # L223.india_state_StubTechProd_elec: calibrated output of electricity generation technologies
    L223.calout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.india_state_StubTechProd_elec

    # L223.india_state_StubTechMarket_elec: market names of inputs to state electricity sectors
    L223.india_state_StubTech_elec %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # For rooftop_pv (technology), match in distributed_solar instead of backup_electricity (minicam.energy.input)
      left_join_keep_first_only(select(A23.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Remove NA rows for hydro
      na.omit %>%
      mutate(market.name = gcam.india_REGION,
             market.name = replace(market.name,
                                   minicam.energy.input %in% c(gcamindia.STATE_RENEWABLE_RESOURCES, gcamindia.STATE_UNLIMITED_RESOURCES),
                                   region[minicam.energy.input %in% c(gcamindia.STATE_RENEWABLE_RESOURCES, gcamindia.STATE_UNLIMITED_RESOURCES)])) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.india_state_StubTechMarket_elec

    L223.india_state_StubTechMarket_elec %>%
      left_join_error_no_match(select(india_states_subregions, grid_region, state), by = c("region" = "state")) %>%
      mutate(market.name = replace(market.name, minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS,
                                   grid_region[minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS])) %>%
      select(-grid_region) ->
      L223.india_state_StubTechMarket_elec

    # L223.india_state_StubTechMarket_backup: market names of backup inputs to state electricity sectors
    L223.GlobalIntTechBackup_elec %>%
      mutate(supplysector = sector.name, subsector = subsector.name) %>%
      write_to_all_india_states(names = c(names(.), 'region')) %>%
      mutate(market.name = gcam.india_REGION, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L223.india_state_StubTechMarket_backup

    # L223.india_state_StubTechCapFactor_elec_wind: capacity factors for wind electricity in the states
    # Just use the subsector for matching - technologies include storage technologies as well
    L223.CapacityFactor_wind_state <- L114.CapacityFactor_wind_state %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector),
                               by = c("sector", "fuel"))


    L223.india_state_StubTechCapFactor_elec_wind <- L223.StubTechCapFactor_elec %>%
      filter(region == gcam.india_REGION) %>%
      semi_join(L223.CapacityFactor_wind_state, by = c("supplysector", "subsector")) %>%
      select(-region, -capacity.factor) %>%
      write_to_all_india_states(names = c(names(.), "region")) %>%
      left_join_error_no_match(L223.CapacityFactor_wind_state,
                               by = c("region" = "state", "supplysector", "subsector")) %>%
      mutate(capacity.factor = round(capacity.factor, digits = energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]])


    # L223.india_state_StubTechCapFactor_elec_solar: capacity factors by state and solar electric technology
    L223.CapFacScaler_solar_state <- L119.CapFacScaler_PV_state %>%
      bind_rows(L119.CapFacScaler_CSP_state) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel"))


    # Just use the subsector for matching - technologies include storage technologies as well
    L223.india_state_StubTechCapFactor_elec_solar <- L223.StubTechCapFactor_elec %>%
      filter(region == gcam.india_REGION) %>%
      semi_join(L223.CapFacScaler_solar_state, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      write_to_all_india_states(., c(names(.), "region")) %>%
      # For matching capacity factors to technologies, create a variable (tech) that matches what's in the capacity factor table
      mutate(tech = sub("_storage", "", stub.technology)) %>%
      left_join_error_no_match(L223.CapFacScaler_solar_state,
                               by = c("region" = "state", "supplysector", "subsector", "tech" = "technology")) %>%
      mutate(capacity.factor = round(capacity.factor * scaler, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]])



    # Produce outputs

    missing_data() %>%
      add_legacy_name("L223.DeleteSubsector_indiaelec") ->
      L223.DeleteSubsector_indiaelec

    missing_data() %>%
        add_legacy_name("L223.india_state_Supplysectorelec") ->
        L223.india_state_Supplysectorelec

    missing_data() %>%
        add_legacy_name("L223.india_state_SubsectorShrwtFlltelec") ->
        L223.india_state_SubsectorShrwtFlltelec

    missing_data() %>%
      add_legacy_name("L223.india_state_SubsectorInterpelec") ->
      L223.india_state_SubsectorInterpelec

    missing_data() %>%
      add_legacy_name("L223.india_state_SubsectorLogitelec") ->
      L223.india_state_SubsectorLogitelec

    missing_data() %>%
      add_legacy_name("L223.india_state_TechShrwt_elec") ->
      L223.india_state_TechShrwt_elec

    missing_data() %>%
      add_legacy_name("L223.india_state_TechCoef_elec") ->
      L223.india_state_TechCoef_elec

    missing_data() %>%
      add_legacy_name("L223.india_state_Production_elec") %>%
      add_precursors("L1232.india_state_out_EJ_sR_elec") ->
      L223.india_state_Production_elec

    L223.india_state_Supplysector_elec_CEA %>%
      add_title("Supplysector information for electricity sector in the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include logit exponent between states") %>%
      add_comments("Use the same logit exponent for states within FERC region as for FERC regions within the india") %>%
      add_legacy_name("L223.india_state_Supplysector_elec_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_Supplysector_elec_CEA

    L223.india_state_SubsectorShrwtFllt_elec_CEA %>%
      add_title("Subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set share-weights for states within grid region") %>%
      add_legacy_name("L223.india_state_SubsectorShrwtFllt_elec_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_SubsectorShrwtFllt_elec_CEA

    L223.india_state_SubsectorInterp_elec_CEA %>%
      add_title("Table header for temporal interpolation of subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set up temporal interterpolation of state share-weights within grid region") %>%
      add_legacy_name("L223.india_state_SubsectorInterp_elec_CEA") %>%
      same_precursors_as("L223.india_state_SubsectorShrwtFllt_elec_CEA") ->
      L223.india_state_SubsectorInterp_elec_CEA

    L223.india_state_SubsectorLogit_elec_CEA %>%
      add_title("Logit exponent of subsector (states) in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L223.india_state_SubsectorLogit_elec_CEA") %>%
      same_precursors_as("L223.india_state_SubsectorShrwtFllt_elec_CEA") ->
      L223.india_state_SubsectorLogit_elec_CEA

    L223.india_state_TechShrwt_elec_CEA %>%
      add_title("Technology share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_legacy_name("L223.india_state_TechShrwt_elec_CEA") %>%
      same_precursors_as("L223.india_state_SubsectorShrwtFllt_elec_CEA") ->
      L223.india_state_TechShrwt_elec_CEA

    L223.india_state_TechCoef_elec_CEA %>%
      add_title("Technology coefficients and market names in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_comments("Market name is state name") %>%
      add_legacy_name("L223.india_state_TechCoef_elec_CEA") %>%
      same_precursors_as("L223.india_state_TechShrwt_elec_CEA") ->
      L223.india_state_TechCoef_elec_CEA

    L223.india_state_PassthroughSector_elec %>%
      add_title("Passthrough sector of the states") %>%
      add_units("Unitless") %>%
      add_comments("The marginal revenue sector is the region's electricity sector.") %>%
      add_comments("The marginal revenue market is the grid region.") %>%
      add_legacy_name("L223.india_state_PassthroughSector_elec") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_PassthroughSector_elec

    L223.india_state_PassthroughTech_elec_CEA %>%
      add_title("Passthrough technology of the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("This contains region, supplysector, subsector, technology for the grid regions") %>%
      add_comments("to which electricity produced in states is passed through") %>%
      add_legacy_name("L223.india_state_PassthroughTech_elec_CEA") %>%
      same_precursors_as("L223.india_state_TechShrwt_elec_CEA") ->
      L223.india_state_PassthroughTech_elec_CEA

    L223.india_state_Production_elec_CEA %>%
      add_title("Calibrated electricity production of subsectors in grid regions") %>%
      add_units("EJ") %>%
      add_comments("Subsector share-weight is zero if production of all technologies in the subsector is zero") %>%
      add_comments("Technology share-weight is zero if production of the technology is zero") %>%
      add_legacy_name("L223.india_state_Production_elec_CEA") %>%
      add_precursors("L1231.india_state_out_EJ_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.india_state_Production_elec_CEA

    L223.india_state_InterestRate_CEA %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L223.india_state_InterestRate_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_InterestRate_CEA

    L223.india_state_Pop_CEA %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L223.india_state_Pop_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_Pop_CEA

    L223.india_state_BaseGDP_CEA %>%
      add_title("Base GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.india_state_BaseGDP_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_BaseGDP_CEA

    L223.india_state_LaborForceFillout_CEA %>%
      add_title("Labor force in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default labor force") %>%
      add_legacy_name("L223.india_state_LaborForceFillout_CEA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L223.india_state_LaborForceFillout_CEA

    L223.india_state_Supplysector_elec %>%
      add_title("Supplysector information of electricity sector in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_legacy_name("L223.india_state_Supplysector_elec") %>%
      add_precursors("L223.Supplysector_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_Supplysector_elec

    L223.india_state_ElecReserve %>%
      add_title("Electricity reserve margin and average grid capacity factor in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_legacy_name("L223.india_state_ElecReserve") %>%
      add_precursors("L223.ElecReserve",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_ElecReserve

    L223.india_state_SubsectorLogit_elec %>%
      add_title("Logit exponent of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_SubsectorLogit_elec") %>%
      add_precursors("L223.SubsectorLogit_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_SubsectorLogit_elec

    L223.india_state_SubsectorShrwtFllt_elec %>%
      add_title("Subsector (fuel) share-weights in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_SubsectorShrwtFllt_elec") %>%
      add_precursors("L223.SubsectorShrwtFllt_elec") ->
      L223.india_state_SubsectorShrwtFllt_elec

    L223.india_state_SubsectorShrwt_nuc %>%
      add_title("Share-weights for nuclear in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("Modify the share-weight path for nuclear to include state preferences") %>%
      add_legacy_name("L223.india_state_SubsectorShrwt_nuc") %>%
      add_precursors("L223.SubsectorShrwt_nuc",
                     "L1231.india_state_out_EJ_elec_F_tech",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_SubsectorShrwt_nuc

    L223.india_state_SubsectorShrwt_renew %>%
      add_title("Share-weights for renewable energy in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_SubsectorShrwt_renew") %>%
      add_precursors("L223.SubsectorShrwt_renew",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_SubsectorShrwt_renew

    L223.india_state_SubsectorInterp_elec %>%
      add_title("Temporal (2100) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_SubsectorInterp_elec") %>%
      add_precursors("L223.SubsectorInterp_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_SubsectorInterp_elec

    L223.india_state_SubsectorInterpTo_elec %>%
      add_title("Temporal (2300) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_SubsectorInterpTo_elec") %>%
      add_precursors("L223.SubsectorInterpTo_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_SubsectorInterpTo_elec

    L223.india_state_StubTech_elec %>%
      add_title("Stub technology information for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.india_state_StubTech_elec") %>%
      add_precursors("L223.StubTech_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_StubTech_elec

    L223.india_state_StubTechEff_elec %>%
      add_title("Stub technology efficiency for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_comments("Re-set markets from india to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.india_state_StubTechEff_elec") %>%
      add_precursors("L223.StubTechEff_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_StubTechEff_elec

    L223.india_state_StubTechCapFactor_elec %>%
      add_title("Capacity factor of stub technology for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same india region values are repeated for each state") %>%
      add_legacy_name("L223.india_state_StubTechCapFactor_elec") %>%
      add_precursors("L223.StubTechCapFactor_elec",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_StubTechCapFactor_elec

    L223.india_state_StubTechFixOut_elec %>%
      add_title("Fixed outputs of stub technology electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("Applied to historical model years") %>%
      add_legacy_name("L223.india_state_StubTechFixOut_elec") %>%
      add_precursors("L1231.india_state_out_EJ_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.india_state_StubTechFixOut_elec

    L223.india_state_StubTechFixOut_hydro %>%
      add_title("Fixed outputs of future hydropower electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("This just holds it constant for now.") %>%
      add_legacy_name("L223.india_state_StubTechFixOut_hydro") %>%
      same_precursors_as("L223.india_state_StubTechFixOut_elec") ->
      L223.india_state_StubTechFixOut_hydro

    L223.india_state_StubTechProd_elec %>%
      add_title("Calibrated outputs of electricity stub technology in the states") %>%
      add_units("EJ") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.india_state_StubTechProd_elec") %>%
      add_precursors("L1231.india_state_in_EJ_elec_F_tech",
                     "L1231.india_state_out_EJ_elec_F_tech",
                     "energy/calibrated_techs",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_StubTechProd_elec

    L223.india_state_StubTechMarket_elec %>%
      add_title("Market names of inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Re-set markets from india to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.india_state_StubTechMarket_elec") %>%
      same_precursors_as("L223.india_state_StubTech_elec") %>%
      add_precursors("energy/A23.globaltech_eff",
                     "gcam-india/A23.india_re_technical_potential") ->
      L223.india_state_StubTechMarket_elec

    L223.india_state_StubTechMarket_backup %>%
      add_title("Market names of backup inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Set market as india") %>%
      add_legacy_name("L223.india_state_StubTechMarket_backup") %>%
      add_precursors("L223.GlobalIntTechBackup_elec",
                     "gcam-india/india_states_subregions") ->
      L223.india_state_StubTechMarket_backup

    if(exists("L223.india_state_StubTechElecMarket_backup")) {
      L223.india_state_StubTechElecMarket_backup %>%
        add_title("Market name of electricity sector for backup calculations") %>%
        add_units("Unitless") %>%
        add_comments("The backup electric market is only set here if regional electricity markets are not used") %>%
        add_legacy_name("L223.india_state_StubTechElecMarket_backup") %>%
        same_precursors_as("L223.india_state_StubTechMarket_backup") ->
        L223.india_state_StubTechElecMarket_backup
    } else {
      # If regional electricity markets are not used,
      # then a blank tibble of the backup electric market is produced.
      missing_data() %>%
        add_legacy_name("L223.india_state_StubTechElecMarket_backup") ->
        L223.india_state_StubTechElecMarket_backup
    }

    L223.india_state_StubTechCapFactor_elec_wind %>%
      add_title("Capacity factors for wind electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.india_state_StubTechCapFactor_elec_wind") %>%
      add_precursors("L114.india_state_CapacityFactor_wind",
                     "energy/calibrated_techs",
                     "gcam-india/india_states_subregions") ->
      L223.india_state_StubTechCapFactor_elec_wind

    L223.india_state_StubTechCapFactor_elec_solar <- L223.india_state_StubTechCapFactor_elec_solar %>%
      add_title("Capacity factors for solar electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.india_state_StubTechCapFactor_elec_solar") %>%
      add_precursors("L119.india_state_CapFacScaler_PV",
                     "L119.india_state_CapFacScaler_CSP",
                     "energy/calibrated_techs",
                     "gcam-india/india_states_subregions")

    return_data(L223.DeleteSubsector_indiaelec,
                L223.india_state_Supplysectorelec,
                L223.india_state_SubsectorShrwtFlltelec,
                L223.india_state_SubsectorInterpelec,
                L223.india_state_SubsectorLogitelec,
                L223.india_state_TechShrwt_elec,
                L223.india_state_TechCoef_elec,
                L223.india_state_Production_elec,
                L223.india_state_PassthroughSector_elec,
                L223.india_state_PassthroughTech_elec_CEA,
                L223.india_state_Supplysector_elec_CEA,
                L223.india_state_SubsectorShrwtFllt_elec_CEA,
                L223.india_state_SubsectorInterp_elec_CEA,
                L223.india_state_SubsectorLogit_elec_CEA,
                L223.india_state_TechShrwt_elec_CEA,
                L223.india_state_TechCoef_elec_CEA,
                L223.india_state_Production_elec_CEA,
                L223.india_state_InterestRate_CEA,
                L223.india_state_Pop_CEA,
                L223.india_state_BaseGDP_CEA,
                L223.india_state_LaborForceFillout_CEA,
                L223.india_state_Supplysector_elec,
                L223.india_state_ElecReserve,
                L223.india_state_SubsectorLogit_elec,
                L223.india_state_SubsectorShrwtFllt_elec,
                L223.india_state_SubsectorShrwt_nuc,
                L223.india_state_SubsectorShrwt_renew,
                L223.india_state_SubsectorInterp_elec,
                L223.india_state_SubsectorInterpTo_elec,
                L223.india_state_StubTech_elec,
                L223.india_state_StubTechEff_elec,
                L223.india_state_StubTechCapFactor_elec,
                L223.india_state_StubTechFixOut_elec,
                L223.india_state_StubTechFixOut_hydro,
                L223.india_state_StubTechProd_elec,
                L223.india_state_StubTechMarket_elec,
                L223.india_state_StubTechMarket_backup,
                L223.india_state_StubTechElecMarket_backup,
                L223.india_state_StubTechCapFactor_elec_wind,
                L223.india_state_StubTechCapFactor_elec_solar)
  } else {
    stop("Unknown command")
  }
}
