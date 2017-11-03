#' module_gcam.usa_L222.en_transformation_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.DeleteStubTech_USAen}, \code{L222.SectorEQUIV}, \code{L222.PassThroughSector_USAen}, \code{object}, \code{L222.TechEQUIV}, \code{L222.Tech_USAen}, \code{L222.TechShrwt_USAen}, \code{L222.TechInterp_USAen}, \code{L222.TechShrwt_USAen}, \code{L222.TechCoef_USAen}, \code{L222.Production_USArefining}, \code{L222.SectorLogitTables_USA[[ curr_table ]]$data}, \code{L222.Supplysector_en_USA}, \code{L222.SubsectorShrwtFllt_en_USA}, \code{L222.StubTechProd_refining_USA}, \code{L222.StubTechMarket_en_USA}, \code{L222.CarbonCoef_en_USA}. The corresponding file in the
#' original data system was \code{L222.en_transformation_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L222.en_transformation_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             "L222.Supplysector_en",
             "L222.SubsectorLogit_en",
             "L222.StubTech_en",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             #"L222.GlobalTechShutdownProfit_en",
             "L222.GlobalTechShutdown_en",
             #"L222.GlobalTechSCurveProfit_en",
             "L222.GlobalTechSCurve_en",
             #"L222.GlobalTechLifetimeProfit_en",
             "L222.GlobalTechLifetime_en",
             "L122.out_EJ_state_refining_F",
             "L202.CarbonCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.DeleteStubTech_USAen",
             "L222.SectorEQUIV",
             "L222.PassThroughSector_USAen",
             "L222.TechEQUIV",
             "L222.Tech_USAen",
             "L222.TechShrwt_USAen",
             "L222.TechInterp_USAen",
             "L222.TechCoef_USAen",
             "L222.Production_USArefining",
             "L222.Supplysector_en_USA",
             "L222.SubsectorShrwtFllt_en_USA",
             "L222.StubTechProd_refining_USA",
             "L222.StubTechMarket_en_USA",
             "L222.CarbonCoef_en_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en")
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en")
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining")
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en")
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en")
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en")
    # L222.GlobalTechShutdownProfit_en <- get_data(all_data, "L222.GlobalTechShutdownProfit_en")
    L222.GlobalTechShutdown_en <- get_data(all_data, "L222.GlobalTechShutdown_en")
    # L222.GlobalTechSCurveProfit_en <- get_data(all_data, "L222.GlobalTechSCurveProfit_en")
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en")
    # L222.GlobalTechLifetimeProfit_en <- get_data(all_data, "L222.GlobalTechLifetimeProfit_en")
    L222.GlobalTechLifetime_en <- get_data(all_data, "L222.GlobalTechLifetime_en")
    L122.out_EJ_state_refining_F <- get_data(all_data, "L122.out_EJ_state_refining_F")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")


    # silence check package notes



    # build tables

    # L222.TechEQUIV: not used in this code, would probably be best defined externally as a constant or assumption
    L222.TechEQUIV <- tibble(group.name=c("technology"), tag1=c("technology"),
                              tag2=c("pass-through-technology"))

    # L222.SectorEQUIV: not used in this code, would probably be best defined externally as a constant or assumption
    L222.SectorEQUIV <- tibble(group.name=c("sector"), tag1=c("supplysector"),
                               tag2=c("pass-through-sector"))


    # Oil refining sectors are only created in states where the production is > 0 in the historical period.
    # Collect these states. Other techs are available everywhere
    L122.out_EJ_state_refining_F %>%
      filter(sector == "oil refining",
             year %in% HISTORICAL_YEARS) %>%
      group_by(state, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      filter(value > 0) %>%
      pull(state) ->
      oil_refining_states


    # L222.DeleteStubTech_USAen: remove existing stub technologies in the USA region.
    # The supplysector and subsector structure in the sectors defined in gcamusa.SECTOR_EN_NAMES are retained
    L222.StubTech_en %>%
      filter(region == "USA",
             supplysector %in% gcamusa.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_USAen


    # L222.Tech_USAen: Just the technology pass-throughs used to set the proper node name, USA region
    L222.SubsectorLogit_en %>%
      select(region, supplysector, subsector) %>%
      filter(region == "USA",
             supplysector %in% gcamusa.SECTOR_EN_NAMES) %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      filter((subsector == "oil refining" & state %in% oil_refining_states) |
               subsector != "oil refining") %>%
      mutate(technology = paste(state, subsector, sep = gcamusa.STATE_SUBSECTOR_DELIMITER)) ->
      L222.Tech_USAen


    # save some of this information for the PassThroughSector information
    # L222.PassThroughSector_USAen: PassThroughSector information to send vintaging info from states to USA.
    L222.Tech_USAen %>%
      select(state, subsector, supplysector, region) %>%
      rename(marginal.revenue.market = region,
             region = state,
             pass.through.sector = subsector,
             marginal.revenue.sector = supplysector) ->
      L222.PassThroughSector_USAen


    # select only relevant columns for L222.Tech_USAen, particularly dropping state
    L222.Tech_USAen %>%
      select(one_of(LEVEL2_DATA_NAMES[["Tech"]])) ->
      L222.Tech_USAen


    # L222.TechInterp_USAen: technology shareweights, USA region
    # Technology interpolation only applies to calibrated technologies.
    # For biomass liquids, allow state shares to shift over time
    # (future techs are different than present techs).
    L222.Tech_USAen %>%
      filter(subsector %in% c("oil refining", "biomass liquids")) %>%
      mutate(apply.to = "share-weight",
             from.year = max(BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = if_else(subsector == "biomass liquids", "s-curve","fixed")) ->
      L222.TechInterp_USAen


    # L222.TechShrwt_USAen: technology shareweights in each year, USA region
    # Default the base year shareweights to 0. This will be over-ridden in calibration
    # Default the future year shareweights to 1.
    L222.Tech_USAen %>%
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
      mutate(share.weight = 0) ->
      tmp

    L222.Tech_USAen %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      mutate(share.weight = 1) %>%
      bind_rows(tmp) ->
      L222.TechShrwt_USAen


    # L222.TechCoef_USAen: technology coefficients and market names, USA region
    L222.TechShrwt_USAen %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = subsector,
             coefficient = 1,
             market.name = technology) %>%
      separate(market.name, c("market.name", "trash"), extra = "merge", sep = gcamusa.STATE_SUBSECTOR_DELIMITER) %>%
      select(-trash) ->
      L222.TechCoef_USAen


    # L222.Production_USArefining: calibrated refinery production in USA (consuming output of states)
    # Aggregated to the supplysector/subsector/technology level
    ### I'm pretty sure all this aggregation does is remove the state column, since the state information is
    ### implicit in the technology column. THat is, the aggregation doesn't actually change any values - is
    ### this intended?
      L122.out_EJ_state_refining_F %>%
        filter(year %in% BASE_YEARS) %>%
        mutate(region = "USA") %>%
        rename(calOutputValue = value) %>%
        left_join_error_no_match(distinct(select(calibrated_techs, sector, supplysector, subsector)), by = "sector") %>%
        mutate(technology = paste(state, subsector, sep = gcamusa.STATE_SUBSECTOR_DELIMITER),
               minicam.energy.input = subsector) %>%
        filter((subsector == "oil refining" & state %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        # Aggregate
        group_by(region, supplysector, subsector, technology, minicam.energy.input, year) %>%
        summarise(calOutputValue = sum(calOutputValue)) %>%
        ungroup %>%
        mutate(share.weight.year = year) %>%
        set_subsector_shrwt %>%
        mutate(tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L222.Production_USArefining









    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.DeleteStubTech_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.DeleteStubTech_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SectorEQUIV") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SectorEQUIV

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.PassThroughSector_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.PassThroughSector_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.TechEQUIV") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.TechEQUIV

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Tech_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.Tech_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.TechShrwt_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.TechShrwt_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.TechInterp_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.TechInterp_USAen

    # tibble() %>%
    #   add_title("descriptive title of data") %>%
    #   add_units("units") %>%
    #   add_comments("comments describing how data generated") %>%
    #   add_comments("can be multiple lines") %>%
    #   add_legacy_name("L222.TechShrwt_USAen") %>%
    #   add_precursors("precursor1", "precursor2", "etc") %>%
    #   # typical flags, but there are others--see `constants.R`
    #   add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    #   L222.TechShrwt_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.TechCoef_USAen") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.TechCoef_USAen

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Production_USArefining") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.Production_USArefining

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.Supplysector_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorShrwtFllt_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTechProd_refining_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTechProd_refining_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTechMarket_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTechMarket_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.CarbonCoef_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "L222.Supplysector_en",
                     "L222.SubsectorLogit_en",
                     "L222.StubTech_en",
                     "L222.StubTechCoef_refining",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     #"L222.GlobalTechShutdownProfit_en",
                     "L222.GlobalTechShutdown_en",
                     #"L222.GlobalTechSCurveProfit_en",
                     "L222.GlobalTechSCurve_en",
                     #"L222.GlobalTechLifetimeProfit_en",
                     "L222.GlobalTechLifetime_en",
                     "L122.out_EJ_state_refining_F",
                     "L202.CarbonCoef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.CarbonCoef_en_USA

    return_data(L222.DeleteStubTech_USAen, L222.SectorEQUIV, L222.PassThroughSector_USAen, L222.TechEQUIV, L222.Tech_USAen,
                L222.TechShrwt_USAen, L222.TechInterp_USAen, L222.TechCoef_USAen, L222.Production_USArefining,
                L222.Supplysector_en_USA, L222.SubsectorShrwtFllt_en_USA, L222.StubTechProd_refining_USA, L222.StubTechMarket_en_USA,
                L222.CarbonCoef_en_USA)
  } else {
    stop("Unknown command")
  }
}
