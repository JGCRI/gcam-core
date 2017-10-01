#' module_gcam.usa_L223.electricity_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.SectorNodeEquiv}, \code{L223.TechNodeEquiv}, \code{L223.DeleteSubsector_USAelec}, \code{L223.Supplysector_USAelec}, \code{L223.SubsectorShrwtFllt_USAelec}, \code{L223.SubsectorInterp_USAelec}, \code{L223.SubsectorLogit_USAelec}, \code{L223.TechShrwt_USAelec}, \code{L223.TechCoef_USAelec}, \code{L223.Production_USAelec},
#' \code{L223.PassthroughSector_elec_USA}, \code{L223.PassthroughTech_elec_FERC}, \code{L223.Supplysector_elec_FERC}, \code{L223.SubsectorShrwtFllt_elec_FERC}, \code{L223.SubsectorInterp_elec_FERC}, \code{L223.SubsectorLogit_elec_FERC}, \code{L223.TechShrwt_elec_FERC}, \code{L223.TechCoef_elec_FERC}, \code{L223.Production_elec_FERC}, \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC}, \code{L223.LaborForceFillout_FERC},
#' \code{L223.Supplysector_elec_USA}, \code{L223.ElecReserve_USA}, \code{L223.SubsectorLogit_elec_USA}, \code{L223.SubsectorShrwtFllt_elec_USA}, \code{L223.SubsectorShrwt_nuc_USA}, \code{L223.SubsectorShrwt_renew_USA}, \code{L223.SubsectorInterp_elec_USA}, \code{L223.SubsectorInterpTo_elec_USA}, \code{L223.StubTech_elec_USA}, \code{L223.StubTechEff_elec_USA}, \code{L223.StubTechCapFactor_elec_USA},
#' \code{L223.StubTechFixOut_elec_USA}, \code{L223.StubTechFixOut_hydro_USA}, \code{L223.StubTechProd_elec_USA}, \code{L223.StubTechMarket_elec_USA}, \code{L223.StubTechMarket_backup_USA}, \code{L223.StubTechElecMarket_backup_USA}, \code{L223.StubTechCapFactor_elec_wind_USA}, \code{L223.StubTechCapFactor_elec_solar_USA}. The corresponding file in the
#' original data system was \code{L223.electricity_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L223.electricity_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             FILE = "temp-data-inject/L114.CapacityFactor_wind_state",
             "L119.CapFacScaler_PV_state",
             "L119.CapFacScaler_CSP_state",
             FILE = "temp-data-inject/L223.Supplysector_elec",
             FILE = "temp-data-inject/L223.ElecReserve",
             FILE = "temp-data-inject/L223.SubsectorLogit_elec",
             FILE = "temp-data-inject/L223.SubsectorShrwtFllt_elec",
             FILE = "temp-data-inject/L223.SubsectorShrwt_nuc",
             FILE = "temp-data-inject/L223.SubsectorShrwt_renew",
             FILE = "temp-data-inject/L223.SubsectorInterp_elec",
             FILE = "temp-data-inject/L223.SubsectorInterpTo_elec",
             FILE = "temp-data-inject/L223.StubTech_elec",
             FILE = "temp-data-inject/L223.StubTechEff_elec",
             FILE = "temp-data-inject/L223.StubTechCapFactor_elec",
             FILE = "temp-data-inject/L223.GlobalIntTechBackup_elec",
             "L1231.in_EJ_state_elec_F_tech",
             "L1231.out_EJ_state_elec_F_tech",
             "L1232.out_EJ_sR_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.SectorNodeEquiv",
             "L223.TechNodeEquiv",
             "L223.DeleteSubsector_USAelec",
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
             "L223.StubTechCapFactor_elec_solar_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L114.CapacityFactor_wind_state <- get_data(all_data, "temp-data-inject/L114.CapacityFactor_wind_state")
    L119.CapFacScaler_PV_state <- get_data(all_data, "L119.CapFacScaler_PV_state")
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.CapFacScaler_CSP_state")
    L223.Supplysector_elec <- get_data(all_data, "temp-data-inject/L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "temp-data-inject/L223.ElecReserve")
    L223.SubsectorLogit_elec <- get_data(all_data, "temp-data-inject/L223.SubsectorLogit_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "temp-data-inject/L223.SubsectorShrwtFllt_elec")
    L223.SubsectorShrwt_nuc <- get_data(all_data, "temp-data-inject/L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "temp-data-inject/L223.SubsectorShrwt_renew")
    L223.SubsectorInterp_elec <- get_data(all_data, "temp-data-inject/L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "temp-data-inject/L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "temp-data-inject/L223.StubTech_elec")
    L223.StubTechEff_elec <- get_data(all_data, "temp-data-inject/L223.StubTechEff_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "temp-data-inject/L223.StubTechCapFactor_elec")
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "temp-data-inject/L223.GlobalIntTechBackup_elec")
    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, "L1231.in_EJ_state_elec_F_tech")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")
    L1232.out_EJ_sR_elec <- get_data(all_data, "L1232.out_EJ_sR_elec")

    # Set up equivalent sector and technology tag names.

    # L223.SectorNodeEquiv: Sets up equivalent sector tag names to avoid having to partition input tables
    tibble(X1 = "SectorXMLTags", X2 = "supplysector", X3 = "pass-through-sector") ->
      L223.SectorNodeEquiv

    # L223.TechNodeEquiv: Sets up equivalent technology tag names to avoid having to partition input tables" )
    tibble(X1 = "TechnologyXMLTags", X2 = "technology",
           X3 = "intermittent-technology", X4 = "pass-through-technology") ->
      L223.TechNodeEquiv

    # create a vector of USA grid region names
    states_subregions %>%
      select(grid_region) %>%
      unique %>%
      arrange(grid_region) %>%
      unlist ->
      grid_regions

    elec_gen_names <- "electricity"

    # Indicate states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      left_join(states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      mutate(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      select(geo_state_noresource) %>%
      unlist ->
      geo_states_noresource

    # gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE, indicating to resolve electricity demands at the level of the grid regions.
    # The entire loop below produces outputs assoicated with resolving demand at the national level, and is currently disabled.
    if(!gcamusa.USE_REGIONAL_ELEC_MARKETS){
      # PART 1: THE USA REGION
      # Define the sector(s) that will be used in this code file. Can be one or multiple sectors
      # The subsectors of the existing USA electricity sector are deleted.
      # Keeping the supplysector info, incl. reserve margin
      # NOTE: This also removes the rooftop PV subsector of the USA elect_td_bld sector
      L223.SubsectorLogit_elec %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        filter(region == "USA") ->
        L223.DeleteSubsector_USAelec

      # L223.Supplysector_USAelec: supplysector for electricity sector in the USA region,
      # including logit exponent between grid regions
      # All of the supplysector information is the same as before, except the logit exponent
      tibble(region = "USA",
             supplysector = elec_gen_names,
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = min(BASE_YEARS),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
        L223.Supplysector_USAelec

      # No need to read in subsector logit exponents, which are applied to the technology competition
      # L223.SubsectorShrwtFllt_USAelec: subsector (grid region) shareweights in USA electricity
      tibble(region = "USA",
             supplysector = elec_gen_names,
             subsector = paste(grid_regions, elec_gen_names, sep = " " ),
             year.fillout = min(BASE_YEARS),
             share.weight = 1) ->
        L223.SubsectorShrwtFllt_USAelec

      # L223.SubsectorInterp_USAelec: subsector (grid region) shareweights in USA electricity
      L223.SubsectorShrwtFllt_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(apply.to = "share-weight",
               from.year = max(BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L223.SubsectorInterp_USAelec

      # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
      L223.SubsectorShrwtFllt_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = gcamusa.GRID_REGION_LOGIT,
               logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
        select(one_of(LEVEL2_DATA_NAMES[["SubsectorLogit"]])) ->
        L223.SubsectorLogit_USAelec

      # L223.TechShrwt_USAelec: technology shareweights, USA region
      L223.SubsectorShrwtFllt_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(technology = subsector) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(share.weight = 1) ->
        L223.TechShrwt_USAelec

      # L223.TechCoef_USAelec: technology coefficients and market names, USA region
      L223.TechShrwt_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
        mutate(minicam.energy.input = supplysector,
               coefficient = 1,
               market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
        L223.TechCoef_USAelec

      # L223.Production_USAelec: calibrated electricity production in USA (consuming output of grid subregions)
      L1232.out_EJ_sR_elec %>%
        filter(year %in% BASE_YEARS) %>%
        mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
        left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
        mutate(subsector = paste(grid_region, supplysector, sep = " ")) ->
      L223.out_EJ_sR_elec

      L223.TechCoef_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(L223.out_EJ_sR_elec, by = c("supplysector", "subsector", "year")) %>%
        mutate(share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]]))->
        L223.Production_USAelec
    }

    # PART 2: THE FERC REGIONS
    # NOTE: FERC regions function in similar fashion to the USA region: competing electricity from subregions

    # L223.Supplysector_elec_FERC: supplysector for electricity sector in the USA region, including logit exponent between grid regions
    # NOTE: using the same logit exponent for states within FERC region as for FERC regions within the USA
    tibble(region = grid_regions,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(BASE_YEARS),
           logit.exponent = gcamusa.GRID_REGION_LOGIT,
           logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
      L223.Supplysector_elec_FERC

    # L223.SubsectorShrwtFllt_elec_FERC: subsector (grid region) shareweights in USA electricity
    states_subregions %>%
      select(region = grid_region, state) %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(state, supplysector, sep = " "),
             year.fillout = min(BASE_YEARS),
             share.weight = 1) %>%
      select(-state) %>%
      arrange(region) ->
      L223.SubsectorShrwtFllt_elec_FERC

    # L223.SubsectorInterp_elec_FERC: subsector (grid region) shareweights in USA electricity
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(apply.to = "share-weight",
             from.year = max(BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L223.SubsectorInterp_elec_FERC

    # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(logit.year.fillout = min(BASE_YEARS),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) %>%
      select(one_of(LEVEL2_DATA_NAMES[["SubsectorLogit"]])) ->
      L223.SubsectorLogit_elec_FERC

    # L223.TechShrwt_elec_FERC: technology shareweights, USA region
    L223.SubsectorShrwtFllt_elec_FERC %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L223.TechShrwt_elec_FERC

    # L223.TechCoef_elec_FERC: technology coefficients and market names, USA region
    L223.TechShrwt_elec_FERC %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L223.TechCoef_elec_FERC

    # Create a L223.PassthroughSector_elec_USA dataframe (to be converted into a csv table later).
    # The marginal revenue sector is the region's electricity sector
    # whereas the marginal revenue market is the grid region.
    states_subregions %>%
      select(region = state, grid_region) %>%
      mutate(passthrough.sector="electricity",
             marginal.revenue.sector = "electricity",
             marginal.revenue.market = grid_region) %>%
      select(-grid_region) ->
      L223.PassthroughSector_elec_USA

    # Create a L223.PassthroughTech_elec_FERC dataframe (to be converted into a csv table later).
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    # Note that the "technology" in this data-frame will be called "passthrough technology"
    L223.TechShrwt_elec_FERC %>%
      select(region, supplysector, subsector, technology) ->
      L223.PassthroughTech_elec_FERC

    # L223.Production_elec_FERC: calibrated electricity production in USA (consuming output of grid subregions)
    L1231.out_EJ_state_elec_F_tech %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
      mutate(subsector = paste(state, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the supplysector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L223.TechCoef_elec_FERC %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(L223.out_EJ_state_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
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
           year.fillout = min(BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.LaborForceFillout_FERC


    # PART 3: THE STATES
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data){
      data_new <- data %>%
        filter(region == "USA") %>%
        write_to_all_states(names(data))

      if("subsector" %in% names(data_new))
        data_new <- data_new %>%
          filter(!paste(region, subsector) %in% geo_states_noresource)

      # Re-set markets from USA to regional markets, if called for in the GCAM-USA assumptions
      if(gcamusa.USE_REGIONAL_FUEL_MARKETS & "market.name" %in% names(data_new))
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions,state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)

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

    # NOTE: Modifying the shareweight path for nuclear to include state preferences
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
             # Set VT to zero because they have already shut down their only nuclear plant
             # which used to account for ~70% of generation
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
      filter(year %in% BASE_YEARS) %>%
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
      select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]]), calOutputValue) %>%
      mutate(fixedOutput = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(-calOutputValue) ->
      L223.StubTechFixOut_elec_USA

    # Adding in future hydropower generation here
    # L223.StubTechFixOut_hydro_USA: fixed output of future hydropower
    # NOTE: This just holds it constant for now;
    # at some point, should downscale of the (almost completely flat) nation-level projection
    L223.StubTechFixOut_elec_USA %>%
      filter(grepl("hydro", stub.technology), year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) ->
      L223.StubTechFixOut_hydro_USA

    # L223.StubTechProd_elec_USA: calibrated output of electricity generation technologies
    L223.calout_EJ_state_elec_F_tech %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]]), calOutputValue) %>%
      mutate(share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.StubTechProd_elec_USA

    # L223.StubTechMarket_elec_USA: market names of inputs to state electricity sectors
    L223.StubTech_elec_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_keep_first_only(select(A23.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Remove NA rows for hydro
      na.omit %>%
      mutate(market.name = "USA",
             market.name = replace(market.name,
                                   minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, gcamusa.STATE_UNLIMITED_RESOURCES),
                                   region[minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, gcamusa.STATE_UNLIMITED_RESOURCES)])) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.StubTechMarket_elec_USA

    if(gcamusa.USE_REGIONAL_ELEC_MARKETS){
      L223.StubTechMarket_elec_USA %>%
        left_join_error_no_match(select(states_subregions, grid_region, state), by = c("region" = "state")) %>%
        mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                     grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
        select(-grid_region) ->
        L223.StubTechMarket_elec_USA
    }

    # L223.StubTechMarket_backup_USA: market names of backup inputs to state electricity sectors
    L223.GlobalIntTechBackup_elec %>%
      mutate(supplysector = sector.name, subsector = subsector.name) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(market.name = "USA", stub.technology = technology) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechMarket"]])) ->
      L223.StubTechMarket_backup_USA

    # The backup electric market is only set here if regional electricity markets are not used (i.e. one national grid)
    if(!gcamusa.USE_REGIONAL_ELEC_MARKETS){
      # L223.StubTechElecMarket_backup_USA: market name of electricity sector for backup calculations
      L223.StubTechMarket_backup_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]])) %>%
        mutate(electric.sector.market = "USA")
    }

    # L223.StubTechCapFactor_elec_wind_USA: capacity factors for wind electricity in the states
    # Just use the subsector for matching - technologies include storage technologies as well
    L114.CapacityFactor_wind_state %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector),
                               by = c("sector", "fuel")) ->
      L223.CapacityFactor_wind_state

    L223.StubTechCapFactor_elec %>%
      filter(region == "USA") %>%
      semi_join(L223.CapacityFactor_wind_state, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      left_join_error_no_match(L223.CapacityFactor_wind_state,
                               by = c("region" = "state", "supplysector", "subsector")) %>%
      mutate(capacity.factor.capital = round(capacity.factor, digits = energy.DIGITS_CAPACITY_FACTOR),
             capacity.factor.OM = capacity.factor.capital) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCapFactor"]])) ->
      L223.StubTechCapFactor_elec_wind_USA

    # L223.StubTechCapFactor_elec_solar_USA: capacity factors by state and solar electric technology
    L119.CapFacScaler_PV_state %>%
      bind_rows(L119.CapFacScaler_CSP_state) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel")) ->
      L223.CapFacScaler_solar_state

    # Just use the subsector for matching - technologies include storage technologies as well
    L223.StubTechCapFactor_elec %>%
      filter(region == "USA") %>%
      semi_join(L223.CapFacScaler_solar_state, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      # For matching capacity factors to technologies, need to have a name that matches what's in the capacity factor table (which doesn't include storage techs)
      mutate(tech = sub("_storage", "", stub.technology)) %>%
      left_join_error_no_match(L223.CapFacScaler_solar_state,
                               by = c("region" = "state", "supplysector", "subsector", "tech" = "technology")) %>%
      mutate(capacity.factor.capital = round(capacity.factor.capital * scaler, digits = energy.DIGITS_COST),
             capacity.factor.OM = capacity.factor.capital) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCapFactor"]])) ->
      L223.StubTechCapFactor_elec_solar_USA

    # Produce outputs
    L223.SectorNodeEquiv %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SectorNodeEquiv") ->
      L223.SectorNodeEquiv

    L223.TechNodeEquiv %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechNodeEquiv") ->
      L223.TechNodeEquiv

    if (exists("L223.DeleteSubsector_USAelec")){
      L223.DeleteSubsector_USAelec %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L223.DeleteSubsector_USAelec") %>%
        add_precursors("temp-data-inject/L223.SubsectorLogit_elec") ->
        L223.DeleteSubsector_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.DeleteSubsector_USAelec") ->
        L223.DeleteSubsector_USAelec
    }

    if (exists("L223.Supplysector_USAelec")){
    L223.Supplysector_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_USAelec") ->
      L223.Supplysector_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.Supplysector_USAelec") ->
        L223.Supplysector_USAelec
    }

    if (exists("L223.SubsectorShrwtFllt_USAelec")){
    L223.SubsectorShrwtFllt_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_USAelec") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.SubsectorShrwtFllt_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.SubsectorShrwtFllt_USAelec") ->
        L223.SubsectorShrwtFllt_USAelec
    }

    if (exists("L223.SubsectorInterp_USAelec")){
    L223.SubsectorInterp_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_USAelec") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_USAelec") ->
      L223.SubsectorInterp_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.SubsectorInterp_USAelec") ->
        L223.SubsectorInterp_USAelec
    }

    if (exists("L223.SubsectorLogit_USAelec")){
    L223.SubsectorLogit_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_USAelec") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_USAelec") ->
      L223.SubsectorLogit_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.SubsectorLogit_USAelec") ->
        L223.SubsectorLogit_USAelec
    }

    if (exists("L223.TechShrwt_USAelec")){
    L223.TechShrwt_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechShrwt_USAelec") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_USAelec") ->
      L223.TechShrwt_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.TechShrwt_USAelec") ->
        L223.TechShrwt_USAelec
    }

    if (exists("L223.TechCoef_USAelec")){
    L223.TechCoef_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechCoef_USAelec") %>%
      same_precursors_as("L223.TechShrwt_USAelec") ->
      L223.TechCoef_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.TechCoef_USAelec") ->
        L223.TechCoef_USAelec
    }

    if (exists("L223.Production_USAelec")){
    L223.Production_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Production_USAelec") %>%
      add_precursors("L1232.out_EJ_sR_elec",
                     "energy/calibrated_techs") ->
      L223.Production_USAelec
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L223.Production_USAelec") ->
        L223.Production_USAelec
    }

    L223.Supplysector_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_elec_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.Supplysector_elec_FERC

    L223.SubsectorShrwtFllt_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.SubsectorShrwtFllt_elec_FERC

    L223.SubsectorInterp_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.SubsectorInterp_elec_FERC

    L223.SubsectorLogit_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.SubsectorLogit_elec_FERC

    L223.TechShrwt_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechShrwt_elec_FERC") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_FERC") ->
      L223.TechShrwt_elec_FERC

    L223.TechCoef_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechCoef_elec_FERC") %>%
      same_precursors_as("L223.TechShrwt_elec_FERC") ->
      L223.TechCoef_elec_FERC

    L223.PassthroughSector_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PassthroughSector_elec_USA") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.PassthroughSector_elec_USA

    L223.PassthroughTech_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PassthroughTech_elec_FERC") %>%
      same_precursors_as("L223.TechShrwt_elec_FERC") ->
      L223.PassthroughTech_elec_FERC

    L223.Production_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Production_elec_FERC") %>%
      add_precursors("L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.Production_elec_FERC

    L223.InterestRate_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.InterestRate_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.InterestRate_FERC

    L223.Pop_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Pop_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.Pop_FERC

    L223.BaseGDP_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.BaseGDP_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.BaseGDP_FERC

    L223.LaborForceFillout_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LaborForceFillout_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.LaborForceFillout_FERC

    L223.Supplysector_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_elec_USA") %>%
      add_precursors("temp-data-inject/L223.Supplysector_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.Supplysector_elec_USA

    L223.ElecReserve_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.ElecReserve_USA") %>%
      add_precursors("temp-data-inject/L223.ElecReserve",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.ElecReserve_USA

    L223.SubsectorLogit_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_elec_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorLogit_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_elec_USA

    L223.SubsectorShrwtFllt_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorShrwtFllt_elec") ->
      L223.SubsectorShrwtFllt_elec_USA

    L223.SubsectorShrwt_nuc_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorShrwt_nuc",
                     "L1231.out_EJ_state_elec_F_tech",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_nuc_USA

    L223.SubsectorShrwt_renew_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_renew_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorShrwt_renew",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_renew_USA

    L223.SubsectorInterp_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_elec_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorInterp_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterp_elec_USA

    L223.SubsectorInterpTo_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec_USA") %>%
      add_precursors("temp-data-inject/L223.SubsectorInterpTo_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterpTo_elec_USA

    L223.StubTech_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTech_elec_USA") %>%
      add_precursors("temp-data-inject/L223.StubTech_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.StubTech_elec_USA

    L223.StubTechEff_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechEff_elec_USA") %>%
      add_precursors("temp-data-inject/L223.StubTechEff_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.StubTechEff_elec_USA

    L223.StubTechCapFactor_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_USA") %>%
      add_precursors("temp-data-inject/L223.StubTechCapFactor_elec",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.StubTechCapFactor_elec_USA

    L223.StubTechFixOut_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_elec_USA") %>%
      add_precursors("L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.StubTechFixOut_elec_USA

    L223.StubTechFixOut_hydro_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_hydro_USA") %>%
      same_precursors_as("L223.StubTechFixOut_elec_USA") ->
      L223.StubTechFixOut_hydro_USA

    L223.StubTechProd_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec_USA") %>%
      add_precursors("L1231.in_EJ_state_elec_F_tech",
                     "L1231.out_EJ_state_elec_F_tech",
                     "energy/calibrated_techs",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.StubTechProd_elec_USA

    L223.StubTechMarket_elec_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechMarket_elec_USA") %>%
      same_precursors_as("L223.StubTech_elec_USA") %>%
      add_precursors("energy/A23.globaltech_eff",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.StubTechMarket_elec_USA

    L223.StubTechMarket_backup_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechMarket_backup_USA") %>%
      add_precursors("temp-data-inject/L223.GlobalIntTechBackup_elec",
                     "gcam-usa/states_subregions") ->
      L223.StubTechMarket_backup_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechElecMarket_backup_USA") %>%
      same_precursors_as("L223.StubTechMarket_backup_USA") ->
      L223.StubTechElecMarket_backup_USA

    L223.StubTechCapFactor_elec_wind_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_wind_USA") %>%
      add_precursors("temp-data-inject/L114.CapacityFactor_wind_state",
                     "energy/calibrated_techs",
                     "gcam-usa/states_subregions") ->
      L223.StubTechCapFactor_elec_wind_USA

    L223.StubTechCapFactor_elec_solar_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_solar_USA") %>%
      add_precursors("L119.CapFacScaler_PV_state",
                     "L119.CapFacScaler_CSP_state",
                     "energy/calibrated_techs",
                     "gcam-usa/states_subregions") ->
      L223.StubTechCapFactor_elec_solar_USA

    return_data(L223.SectorNodeEquiv,
                L223.TechNodeEquiv,
                L223.DeleteSubsector_USAelec,
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
                L223.StubTechCapFactor_elec_solar_USA)
  } else {
    stop("Unknown command")
  }
}
