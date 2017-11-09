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



    # Correct some of the inputs
    L222.Supplysector_en %>%
      mutate(logit.year.fillout = as.integer(logit.year.fillout)) -> # was character
      L222.Supplysector_en


    L222.SubsectorLogit_en  %>%
      mutate(logit.year.fillout = as.integer(logit.year.fillout)) -> # was character
      L222.SubsectorLogit_en


    L222.StubTechCoef_refining %>%
      mutate(year = as.integer(year)) -> # was double
      L222.StubTechCoef_refining


    L222.GlobalTechInterp_en %>%
      mutate(from.year = as.integer(from.year), # was character
             to.year = as.integer(to.year)) ->
      L222.GlobalTechInterp_en


    L222.GlobalTechCoef_en %>%
      mutate(year  = as.integer(year)) -> # was character
      L222.GlobalTechCoef_en


    L222.GlobalTechCost_en %>%
      mutate(year = as.integer(year)) -> # was double
      L222.GlobalTechCost_en


    L222.GlobalTechCapture_en %>%
      mutate(year = as.integer(year)) -> # was character
      L222.GlobalTechCapture_en


    L222.GlobalTechSCurve_en %>%
      mutate(year = as.integer(year)) -> # was character
      L222.GlobalTechSCurve_en


    # Some helpful functions:
    #
    # global_energy_to_USA_nonGlobalTech - takes global energy inputs for non global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_USA_nonGlobalTech <- function(data){

      data %>%
        filter(region == "USA",
               supplysector %in% gcamusa.SECTOR_EN_NAMES) %>%
        write_to_all_states(names(data)) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        mutate(supplysector = subsector)

    } # global_energy_to_USA_nonGlobalTech

    # global_energy_to_USA_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_USA_GlobalTech <- function(data){

      data %>%
        filter(sector.name %in% gcamusa.SECTOR_EN_NAMES) %>%
        mutate(sector.name = subsector.name)

    } # global_energy_to_USA_GlobalTech


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
    ### implicit in the technology column. That is, the aggregation doesn't actually change any values - is
    ### this intended?
      L122.out_EJ_state_refining_F %>%
        filter(year %in% BASE_YEARS) %>%
        rename(calOutputValue = value) %>%
        mutate(calOutputvalue = round(calOutputValue, gcamusa.DIGITS_CALOUTPUT),
               region = "USA") %>%
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


      # Process energy files from L222.en_transformation.R for use in the USA,
      # slightly differently processing for global tech vs not inputs
      L222.SubsectorLogit_en %>%
        global_energy_to_USA_nonGlobalTech() ->
        L222.SubsectorLogit_en_USA


      L222.StubTech_en %>%
        global_energy_to_USA_nonGlobalTech() ->
        L222.StubTech_en_USA


      L222.StubTechCoef_refining %>%
        global_energy_to_USA_nonGlobalTech() ->
        L222.StubTechCoef_refining_USA


      L222.GlobalTechInterp_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechInterp_en_USA


      L222.GlobalTechCoef_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechCoef_en_USA


      L222.GlobalTechCost_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechCost_en_USA


      L222.GlobalTechShrwt_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechShrwt_en_USA


      L222.GlobalTechCapture_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechCapture_en_USA


      L222.GlobalTechSCurve_en %>%
        global_energy_to_USA_GlobalTech() ->
        L222.GlobalTechSCurve_en_USA


      ### The same processing for Optional/currently NULL inputs

      # L222.GlobalTechShutdownProfit_en  %>%
      #   global_energy_to_USA_GlobalTech()->
      #   L222.GlobalTechShutdownProfit_en_USA

      # L222.GlobalTechShutdown_en %>%
      #   global_energy_to_USA_GlobalTech() ->
      #   L222.GlobalTechShutdown_en_USA

      # L222.GlobalTechSCurveProfit_en %>%
      #   global_energy_to_USA_GlobalTech() ->
      #   L222.GlobalTechSCurveProfit_en_USA

      # L222.GlobalTechLifetimeProfit_en %>%
      #   global_energy_to_USA_GlobalTech ->
      #   L222.GlobalTechLifetimeProfit_en_USA

      # L222.GlobalTechLifetime_en %>%
      #   global_energy_to_USA_GlobalTech ->
      #   L222.GlobalTechLifetime_en_USA


      # L222.Supplysector_en_USA: Supplysector information, replace name of supplysector with the subsector names
      L222.SubsectorLogit_en_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        left_join_error_no_match(distinct(select(L222.SubsectorLogit_en, supplysector, subsector)),
                                 by = "subsector") %>%
        rename(supplysector = supplysector.x,
               old_supplysector = supplysector.y) %>%
        left_join_error_no_match(distinct(select(L222.Supplysector_en, -region)),
                                 by = c("old_supplysector" = "supplysector")) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
        L222.Supplysector_en_USA


      # L222.Supplysector_en_USA_logit.type - Note there is no competition here so just use the default logit type
      L222.Supplysector_en_USA %>%
        mutate(logit.type = gcamusa.DEFAULT_LOGIT_TYPE) ->
        L222.Supplysector_en_USA_logit.type


      # L222.SubsectorShrwtFllt_en_USA: Subsector shareweights, there is no competition here, so just fill out with 1s
      # (will be over-ridden by base year calibration where necessary)
      L222.SubsectorLogit_en_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(year = min(MODEL_YEARS),
               share.weight = gcamusa.DEFAULT_SHAREWEIGHT) ->
        L222.SubsectorShrwtFllt_en_USA


      # L222.StubTechProd_refining_USA: calibrated fuel production by state.
      # Only take the tech IDs where the calibration is identified as output.
      #
      # Step 1, process the table of calibrated_techs to only include calibration=output and relevant columns
      calibrated_techs %>%
        filter(calibration == "output") %>%
        select(sector, supplysector, subsector, technology) %>%
        distinct ->
        calibrated_techs_tmp

      # Step 2, process L122.out_EJ_state_refining_F, joining the processed table of calibrated_techs from step 1,
      # to create L222.StubTechProd_refining_USA. Note the supplysector is the same as the subsector within the states.
      L122.out_EJ_state_refining_F %>%
        filter(year %in% BASE_YEARS) %>%
        rename(region = state,
               calOutputValue = value) %>%
        mutate(calOutputValue = round(calOutputValue, gcamusa.DIGITS_CALOUTPUT)) %>%
        left_join_error_no_match(calibrated_techs_tmp, by = "sector") %>%
        mutate(supplysector = subsector,
               stub.technology = technology,
               share.weight.year = year) %>%
        set_subsector_shrwt() %>%
        mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") ->
        L222.StubTechProd_refining_USA


      # L222.StubTechMarket_en_USA: market names of inputs to state refining sectors
      L222.GlobalTechCoef_en_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["GlobalTechInput"]])) %>%
        repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name,
               stub.technology = technology) %>%
        mutate(market.name = "USA") ->
        L222.StubTechMarket_en_USA


      # If designated, switch fuel market names to the regional markets
      if(gcamusa.USE_REGIONAL_FUEL_MARKETS){
        L222.StubTechMarket_en_USA %>%
          select(-market.name) %>%
          filter(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS) %>%
          left_join_error_no_match(distinct(select(states_subregions, state, grid_region)), by = c("region" = "state")) %>%
          rename(market.name = grid_region) ->
          tmp

        L222.StubTechMarket_en_USA %>%
          filter(!(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS)) %>%
          bind_rows(tmp) ->
          L222.StubTechMarket_en_USA
      }


      # Finish L222.StubTechMarket_en_USA by Setting electricity to the state markets
      L222.StubTechMarket_en_USA %>%
        filter(minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS) %>%
        mutate(market.name = region) ->
        tmp

      # create a key for filtering
      L222.StubTech_en_USA %>%
        select(supplysector, subsector, stub.technology) %>%
        unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
        distinct ->
        L222.StubTech_en_USA_key

      L222.StubTechMarket_en_USA %>%
        filter(!(minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS)) %>%
        bind_rows(tmp) %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechMarket"]])) %>%
        unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
        filter(key %in% L222.StubTech_en_USA_key$key) %>%
        separate(key, c("supplysector", "subsector", "stub.technology"), sep = "~") %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") ->
        L222.StubTechMarket_en_USA


      # L222.CarbonCoef_en_USA: energy carbon coefficients in USA
      #
      # Step 1, process L202.CarbonCoef for joining
      L202.CarbonCoef %>%
        filter(region == "USA") %>%
        select(-region) %>%
        distinct ->
        L202.CarbonCoef_tmp

      # Step 2, create L222.CarbonCoef_en_USA by joining the table from step 1.
      L222.Supplysector_en_USA %>%
        select(region, supplysector) %>%
        distinct %>%
        left_join_error_no_match(distinct(select(L222.TechShrwt_USAen, subsector, supplysector)),
                                 by = c("supplysector" = "subsector")) %>%
        rename(PrimaryFuelCO2Coef.name = supplysector.y) %>%
        left_join_error_no_match(L202.CarbonCoef_tmp, by =  "PrimaryFuelCO2Coef.name") %>%
        select(-supplysector) ->
        L222.CarbonCoef_en_USA




    # Produce outputs
    L222.DeleteStubTech_USAen %>%
      add_title("Removes existing stub technologies in the USA region") %>%
      add_units("NA") %>%
      add_comments("Removes existing stub technologies in the USA region from L222.StubTech_en.") %>%
      add_comments("The supplysector and subsector structure in the sectors defined in gcamusa.SECTOR_EN_NAMES are retained") %>%
      add_legacy_name("L222.DeleteStubTech_USAen") %>%
      add_precursors("L222.StubTech_en")  ->
      L222.DeleteStubTech_USAen

    L222.SectorEQUIV %>%
      add_title("table of sector equivalencies for pass-through-sector") %>%
      add_units("NA") %>%
      add_comments("user defined.") %>%
      add_legacy_name("L222.SectorEQUIV") ->
      L222.SectorEQUIV

    L222.PassThroughSector_USAen %>%
      add_title("PassThroughSector information to send vintaging info from states to USA") %>%
      add_units("NA") %>%
      add_comments("state, subsector, supplysector, and region fromj L222.Tech_USAen is renamed.") %>%
      add_legacy_name("L222.PassThroughSector_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.PassThroughSector_USAen

    L222.TechEQUIV %>%
      add_title("table of technology equivalencies for pass-through-technology") %>%
      add_units("NA") %>%
      add_comments("user defined.") ->
      L222.TechEQUIV

    L222.Tech_USAen %>%
      add_title("The technology pass-throughs used to set the proper node name, USA region.") %>%
      add_units("units") %>%
      add_comments("USA supplysector and subsector information from L222.SubsectorLogit_en is") %>%
      add_comments("repeated for all US states and updated.") %>%
      add_legacy_name("L222.Tech_USAen") %>%
      add_precursors("L222.SubsectorLogit_en") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.Tech_USAen

    L222.TechShrwt_USAen %>%
      add_title("technology shareweights in each year, USA region") %>%
      add_units("NA") %>%
      add_comments("L222.Tech_USAen is repeated for model base year and future years and shareweights of 0 and") %>%
      add_comments("1 are added for each, respectively. Overwritten in calibration.") %>%
      add_legacy_name("L222.TechShrwt_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechShrwt_USAen

    L222.TechInterp_USAen %>%
      add_title("Technology shareweights, USA region") %>%
      add_units("NA") %>%
      add_comments("Technology interpolation only applies to calibrated technologies.For biomass liquids,") %>%
      add_comments("allow state shares to shift over time since future techs are different than present techs.") %>%
      add_legacy_name("L222.TechInterp_USAen")  %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechInterp_USAen

    L222.TechCoef_USAen %>%
      add_title("technology coefficients and market names, USA region") %>%
      add_units("units") %>%
      add_comments("Data from L222.TechShrwt_USAen is renamed and filled out.") %>%
      add_legacy_name("L222.TechCoef_USAen") %>%
      same_precursors_as(L222.TechShrwt_USAen) ->
      L222.TechCoef_USAen

    L222.Production_USArefining %>%
      add_title("Calibrated refinery production in USA (consuming output of states)") %>%
      add_units("NA") %>%
      add_comments("L122.out_EJ_state_refining_F is aggregated to the supplysector/subsector/technology level.") %>%
      add_legacy_name("L222.Production_USArefining") %>%
      add_precursors( "energy/calibrated_techs",
                      "L122.out_EJ_state_refining_F") ->
      L222.Production_USArefining

    L222.Supplysector_en_USA %>%
      add_title("Supplysector information, replace name of supplysector with the subsector names") %>%
      add_units("Varies") %>%
      add_comments("L222.Supplysector_en and L222.SubsectorLogit_en is repeated and filtered for use in USA states.") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en_USA") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SubsectorLogit_en") ->
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
