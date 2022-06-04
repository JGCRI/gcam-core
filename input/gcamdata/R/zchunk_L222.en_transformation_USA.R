# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L222.en_transformation_USA
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies specific to USA sectors and/or states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.DeleteStubTech_USAen}, \code{L222.PassThroughSector_USAen}, \code{L222.Tech_USAen},
#' \code{L222.TechShrwt_USAen}, \code{L222.TechInterp_USAen}, \code{L222.TechCoef_USAen},
#' \code{L222.Production_USArefining}, \code{L222.SectorLogitTables_USA[[ curr_table ]]$data},
#' \code{L222.Supplysector_en_USA}, \code{L222.SubsectorShrwtFllt_en_USA}, \code{L222.StubTechProd_refining_USA},
#' \code{L222.StubTechMarket_en_USA}, \code{L222.CarbonCoef_en_USA}, \code{L222.GlobalTechSCurve_en_USA},
#' \code{L222.GlobalTechProfitShutdown_en_USA}, \code{L222.GlobalTechCost_en_USA}, \code{L222.SubsectorLogit_en_USA},
#' \code{L222.StubTech_en_USA}, \code{L222.StubTechCoef_refining_USA}, \code{L222.GlobalTechInterp_en_USA},
#' \code{L222.GlobalTechCoef_en_USA}, \code{L222.GlobalTechShrwt_en_USA}, \code{L222.GlobalTechCapture_en_USA}.
#' The corresponding file in the original data system was \code{L222.en_transformation_USA.R} (gcam-usa level2).
#' @details This chunk sets up the USA energy transformation technology databases as well as writing out assumptions to all states/sectors/markets for shareweights and logits.
#' Calibrated outputs and I:O coefficients are updated from global values produced by \code{\link{module_energy_L222.en_transformation}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate one_of pull select summarise
#' @importFrom tidyr separate unite
#' @author ACS Nov 2017
module_gcamusa_L222.en_transformation_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/SEDS_refining_feedstock_prod",
             "L222.Supplysector_en",
             "L222.SubsectorLogit_en",
             "L222.StubTech_en",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechSCurve_en",
             "L222.GlobalTechProfitShutdown_en",
             "L122.out_EJ_state_refining_F",
             "L202.CarbonCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.DeleteStubTech_USAen",
             "L222.PassThroughSector_USAen",
             "L222.Tech_USAen",
             "L222.TechShrwt_USAen",
             "L222.TechInterp_USAen",
             "L222.TechCoef_USAen",
             "L222.Production_USArefining",
             "L222.Supplysector_en_USA",
             "L222.SubsectorShrwtFllt_en_USA",
             "L222.StubTechProd_refining_USA",
             "L222.StubTechMarket_en_USA",
             "L222.CarbonCoef_en_USA",
             "L222.GlobalTechSCurve_en_USA",
             "L222.GlobalTechProfitShutdown_en_USA",
             "L222.GlobalTechCost_en_USA",
             "L222.SubsectorLogit_en_USA",
             "L222.StubTech_en_USA",
             "L222.StubTechCoef_refining_USA",
             "L222.GlobalTechInterp_en_USA",
             "L222.GlobalTechCoef_en_USA",
             "L222.GlobalTechShrwt_en_USA",
             "L222.GlobalTechCapture_en_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    SEDS_refining_feedstock_prod <- get_data(all_data, "gcam-usa/SEDS_refining_feedstock_prod")
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en")
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en", strip_attributes = TRUE)
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en", strip_attributes = TRUE)
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining", strip_attributes = TRUE)
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en", strip_attributes = TRUE)
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en", strip_attributes = TRUE)
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en", strip_attributes = TRUE)
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en", strip_attributes = TRUE)
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en", strip_attributes = TRUE)
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en", strip_attributes = TRUE)
    L222.GlobalTechProfitShutdown_en <- get_data(all_data, "L222.GlobalTechProfitShutdown_en", strip_attributes = TRUE)
    L122.out_EJ_state_refining_F <- get_data(all_data, "L122.out_EJ_state_refining_F", strip_attributes = TRUE)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")

    # silence check package notes
    logit.year.fillout <- year <- from.year <- to.year <- region <- supplysector <- subsector <-
      technology <- sector.name <- subsector.name <- sector <- state <- fuel <- value <- market.name <-
      trash <- calOutputValue <- minicam.energy.input <- supplysector.x <- supplysector.y <-
      calibration <- grid_region <- stub.technology <- key <- coal <- natural_gas <- coal_fract <-
      share.weight <- gas_fract <- NULL

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
    global_energy_to_USA_nonGlobalTech <- function(data) {
      data %>%
        filter(region == gcam.USA_REGION,
               supplysector %in% gcamusa.SECTOR_EN_NAMES) %>%
        write_to_all_states(names = c(names(data), "region")) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        mutate(supplysector = subsector)
    } # global_energy_to_USA_nonGlobalTech

    # global_energy_to_USA_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_USA_GlobalTech <- function(data) {
      data %>%
        filter(sector.name %in% gcamusa.SECTOR_EN_NAMES) %>%
        mutate(sector.name = subsector.name)
    } # global_energy_to_USA_GlobalTech

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
      filter(region == gcam.USA_REGION,
             supplysector %in% gcamusa.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_USAen

    # L222.Tech_USAen: Just the technology pass-throughs used to set the proper node name, USA region
    L222.SubsectorLogit_en %>%
      select(region, supplysector, subsector) %>%
      filter(region == gcam.USA_REGION,
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
    # Oil refining and biomass liquids shareweights are fixed at calibration values through max model year
    L222.Tech_USAen %>%
      filter(subsector %in% c("oil refining", "biomass liquids")) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L222.TechInterp_USAen

    # L222.TechShrwt_USAen: technology shareweights in each year, USA region
    L222.Tech_USAen %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Split the state names out and because the refining tech names have spaces drop the extra
      separate(technology, c("state"), sep = " ", remove = F, extra = "drop") %>%
      left_join_error_no_match(SEDS_refining_feedstock_prod, by = c("state")) %>%
      mutate(coal_fract = coal / max(coal), gas_fract = natural_gas / max(natural_gas),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             # Scaling coal to liquids and gas to liquids shareweights to 2015 resource production levels
             share.weight = if_else(grepl("coal", subsector), coal_fract, share.weight),
             share.weight = if_else(grepl("gas", subsector), gas_fract, share.weight),
             # Default the base year shareweights to 0. This will be over-ridden in calibration,
             share.weight = if_else(year %in% MODEL_BASE_YEARS, 0, round(share.weight, energy.DIGITS_SHRWT))) %>%
      select(region, supplysector, subsector, technology, year, share.weight) -> L222.TechShrwt_USAen


    # L222.TechCoef_USAen: technology coefficients and market names, USA region
    L222.TechShrwt_USAen %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = subsector,
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             market.name = technology) %>%
      separate(market.name, c("market.name", "trash"), extra = "merge", sep = gcamusa.STATE_SUBSECTOR_DELIMITER) %>%
      select(-trash) ->
      L222.TechCoef_USAen

    # L222.Production_USArefining: calibrated refinery production in USA (consuming output of states)
    # Aggregated to the supplysector/subsector/technology level
      L122.out_EJ_state_refining_F %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        rename(calOutputValue = value) %>%
        mutate(calOutputvalue = round(calOutputValue, gcamusa.DIGITS_CALOUTPUT),
               region = gcam.USA_REGION) %>%
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
        # The following line is equivalent to (but slightly faster than): mutate(tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        mutate(tech.share.weight = abs(sign(calOutputValue))) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L222.Production_USArefining

      # Process energy files from L222.en_transformation.R for use in the USA,
      # slightly differently processing for global tech vs not inputs
      L222.SubsectorLogit_en_USA      <- global_energy_to_USA_nonGlobalTech(L222.SubsectorLogit_en)
      L222.StubTech_en_USA            <- global_energy_to_USA_nonGlobalTech(L222.StubTech_en)
      L222.StubTechCoef_refining_USA  <- global_energy_to_USA_nonGlobalTech(L222.StubTechCoef_refining)
      L222.GlobalTechInterp_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechInterp_en)
      L222.GlobalTechCoef_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCoef_en)
      L222.GlobalTechCost_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCost_en)
      L222.GlobalTechShrwt_en_USA     <- global_energy_to_USA_GlobalTech(L222.GlobalTechShrwt_en)
      L222.GlobalTechCapture_en_USA   <- global_energy_to_USA_GlobalTech(L222.GlobalTechCapture_en)
      L222.GlobalTechSCurve_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechSCurve_en)

      if(!is.null(L222.GlobalTechProfitShutdown_en)) {
        L222.GlobalTechProfitShutdown_en_USA <- global_energy_to_USA_GlobalTech(L222.GlobalTechProfitShutdown_en)
      }

      # TODO: figure out a better strategy.  We need to have at least one technology be available in the final
      # calibration year so we can get a base cost for the absolute cost logit.  Having a share weight of zero
      # at the subsector is sufficient then to ensure we get no production in the calibration years
      L222.GlobalTechShrwt_en_USA %>%
        mutate(share.weight = if_else(technology == "coal to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight),
               share.weight = if_else(technology == "gas to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight)) ->
        L222.GlobalTechShrwt_en_USA

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
        filter(year %in% MODEL_BASE_YEARS) %>%
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
        write_to_all_states(names = c(LEVEL2_DATA_NAMES[["GlobalTechInput"]], "region")) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name,
               stub.technology = technology) %>%
        mutate(market.name = gcam.USA_REGION) %>%
        # switch designated fuel market names to the regional markets
        left_join_error_no_match(states_subregions %>%
                                   select(state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                     grid_region, market.name)) %>%
        select(-grid_region) -> L222.StubTechMarket_en_USA

      # Finish L222.StubTechMarket_en_USA by assigning state fuel markets
      L222.StubTechMarket_en_USA %>%
        filter(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS) %>%
        mutate(market.name = region) ->
        tmp

      # create a key for filtering
      L222.StubTech_en_USA %>%
        select(supplysector, subsector, stub.technology) %>%
        unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
        distinct ->
        L222.StubTech_en_USA_key

      L222.StubTechMarket_en_USA %>%
        filter(!(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS)) %>%
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
        filter(region == gcam.USA_REGION) %>%
        select(-region) %>%
        distinct ->
        L202.CarbonCoef_tmp

      # Step 2, create L222.CarbonCoef_en_USA by joining the table from step 1.
      L222.Supplysector_en_USA %>%
        select(region, supplysector) %>%
        distinct %>%
        left_join_error_no_match(distinct(select(L222.TechShrwt_USAen, subsector, supplysector)),
                                 by = c("supplysector" = "subsector")) %>%
        left_join_error_no_match(L202.CarbonCoef_tmp, by =  c("supplysector.y" = "PrimaryFuelCO2Coef.name")) %>%
        select(-supplysector.y) %>%
        rename(PrimaryFuelCO2Coef.name = supplysector) ->
        L222.CarbonCoef_en_USA

    # Produce outputs
    L222.DeleteStubTech_USAen %>%
      mutate(region = region) %>%  # strip off attributes so we can re-write title, etc.
      add_title("Removes existing stub technologies in the USA region") %>%
      add_units("NA") %>%
      add_comments("Removes existing stub technologies in the USA region from L222.StubTech_en.") %>%
      add_comments("The supplysector and subsector structure in the sectors defined in gcamusa.SECTOR_EN_NAMES are retained")  %>%
      add_legacy_name("L222.DeleteStubTech_USAen") %>%
      add_precursors("L222.StubTech_en") ->
      L222.DeleteStubTech_USAen

    L222.PassThroughSector_USAen %>%
      add_title("PassThroughSector information to send vintaging info from states to USA") %>%
      add_units("NA") %>%
      add_comments("state, subsector, supplysector, and region fromj L222.Tech_USAen is renamed.") %>%
      add_legacy_name("L222.PassThroughSector_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.PassThroughSector_USAen

    L222.Tech_USAen %>%
      add_title("The technology pass-throughs used to set the proper node name, USA region.") %>%
      add_units("units") %>%
      add_comments("USA supplysector and subsector information from L222.SubsectorLogit_en is") %>%
      add_comments("repeated for all US states and updated.") %>%
      add_legacy_name("L222.Tech_USAen") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "gcam-usa/SEDS_refining_feedstock_prod") ->
      L222.Tech_USAen

    L222.TechShrwt_USAen %>%
      add_title("Technology shareweights in each year, USA region") %>%
      add_units("NA") %>%
      add_comments("L222.Tech_USAen is repeated for model base year and future years and shareweights of 0 and") %>%
      add_comments("1 are added for each, respectively. Overwritten in calibration.") %>%
      add_legacy_name("L222.TechShrwt_USAen") %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechShrwt_USAen

    L222.TechInterp_USAen %>%
      add_title("Technology shareweights, USA region") %>%
      add_units("NA") %>%
      add_comments("Technology interpolation only applies to calibrated technologies.For biomass liquids, ") %>%
      add_comments("allow state shares to shift over time since future techs are different than present techs.") %>%
      add_legacy_name("L222.TechInterp_USAen")  %>%
      same_precursors_as(L222.Tech_USAen) ->
      L222.TechInterp_USAen

    L222.TechCoef_USAen %>%
      add_title("Technology coefficients and market names, USA region") %>%
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
      add_precursors("energy/calibrated_techs",
                      "L122.out_EJ_state_refining_F") ->
      L222.Production_USArefining

    L222.Supplysector_en_USA_logit.type %>%
      add_title("Supplysector information, replace name of supplysector with the subsector names") %>%
      add_units("Varies") %>%
      add_comments("L222.Supplysector_en and L222.SubsectorLogit_en is repeated and filtered for use in USA states.") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en_USA") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SubsectorLogit_en") ->
      L222.Supplysector_en_USA

    L222.SubsectorShrwtFllt_en_USA %>%
      add_title("Subsector shareweights for energy in USA") %>%
      add_units("NA") %>%
      add_comments("USA energy subsector shareweights. There is no competition here, so shareweights are defaulted to 1.") %>%
      add_comments("Shareweights will be over-ridden by base year calibration.") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en_USA") %>%
      same_precursors_as(L222.SubsectorLogit_en_USA) ->
      L222.SubsectorShrwtFllt_en_USA

    L222.StubTechProd_refining_USA %>%
      add_title("USA Calibrated fuel production by state.") %>%
      add_units("varies") %>%
      add_comments("Tech IDs where the calibration is identified as output in the calibrated_techs file are") %>%
      add_comments("are used to adjust data from L122.out_EJ_state_refining_F.") %>%
      add_legacy_name("L222.StubTechProd_refining_USA") %>%
      add_precursors("energy/calibrated_techs",
                     "L122.out_EJ_state_refining_F") ->
      L222.StubTechProd_refining_USA

    L222.StubTechMarket_en_USA %>%
      add_title("Market names of inputs to state refining sectors") %>%
      add_units("varies") %>%
      add_comments("Data from L222.GlobalTechCoef_en is adjusted for use in US states, depending") %>%
      add_comments("on whether regional markets are used.") %>%
      add_legacy_name("L222.StubTechMarket_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L222.GlobalTechCoef_en") ->
      L222.StubTechMarket_en_USA

    L222.CarbonCoef_en_USA %>%
      add_title("Energy carbon coefficients in USA") %>%
      add_units("varies") %>%
      add_comments("Carbon coefficients from L202.CarbonCoef are updated with USA energy tech shareweights to") %>%
      add_comments("produce energy carbon coefficients in USA.") %>%
      add_legacy_name("L222.CarbonCoef_en_USA") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "L202.CarbonCoef")  ->
      L222.CarbonCoef_en_USA

    L222.SubsectorLogit_en_USA %>%
      add_title("Subsector logit competition info for USA energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector logit data from L222.SubsectorLogit_en are filtered and repeated") %>%
      add_comments("for USA sectors in each state.") %>%
      add_legacy_name("L222.SubsectorLogit_en_USA") %>%
      add_precursors("L222.SubsectorLogit_en") ->
      L222.SubsectorLogit_en_USA

    L222.StubTech_en_USA %>%
      add_title("Stub technology map for USA energy states and sectors.") %>%
      add_units("NA") %>%
      add_comments("The stub technology table from L222.StubTech_en is filtered and repeated") %>%
      add_comments("for USA energy sectors in each state.") %>%
      add_legacy_name("L222.StubTech_en_USA") %>%
      add_precursors("L222.StubTech_en") ->
      L222.StubTech_en_USA

    L222.StubTechCoef_refining_USA %>%
      add_title("Refining stub tech coefficients for USA energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Coefficients for refining stub technologies in L222.StubTechCoef_refining are filtered and repeated") %>%
      add_comments("for USA energy sectors in each state.") %>%
      add_legacy_name("L222.StubTechCoef_refining_USA") %>%
      add_precursors("L222.StubTechCoef_refining") ->
      L222.StubTechCoef_refining_USA

    L222.GlobalTechSCurve_en_USA %>%
      add_title("Tech S curve parameters for USA energy sectors.") %>%
      add_units("varies") %>%
      add_comments("S curve parameters from L222.GlobalTechScurve_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechSCurve_en_USA") %>%
      add_precursors("L222.GlobalTechSCurve_en") ->
      L222.GlobalTechSCurve_en_USA

    if(exists("L222.GlobalTechProfitShutdown_en_USA")) {
      L222.GlobalTechProfitShutdown_en_USA %>%
        add_title("Global tech profit shutdown decider and parameters for USA energy sectors") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Profit-based shutdown from L222.GlobalTechProfitShutdown_en_USA are filtered for USA sectors.") %>%
        add_precursors("L222.GlobalTechProfitShutdown_en") ->
        L222.GlobalTechProfitShutdown_en_USA
    } else {
      missing_data() ->
        L222.GlobalTechProfitShutdown_en_USA
    }

    L222.GlobalTechCost_en_USA %>%
      add_title("Tech costs for USA energy sectors.") %>%
      add_units("varies") %>%
      add_comments("Tech cost data from L222.GlobalTechCost_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechCost_en_USA") %>%
      add_precursors("L222.GlobalTechCost_en") ->
      L222.GlobalTechCost_en_USA

    L222.GlobalTechInterp_en_USA %>%
      add_title("Interpolation function key for USA energy sectors.") %>%
      add_units("units") %>%
      add_comments("Interpolation function key from L222.GlobalTechInterp_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechInterp_en_USA") %>%
      add_precursors("L222.GlobalTechInterp_en") ->
      L222.GlobalTechInterp_en_USA

    L222.GlobalTechCoef_en_USA %>%
      add_title("Technology coefficients for USA energy sectors.") %>%
      add_units("NA") %>%
      add_comments("Global technology coefficients from L222.GlobalTechCoef_en are filtered for USA sectors.") %>%
      add_legacy_name("L222.GlobalTechCoef_en_USA") %>%
      add_precursors("L222.GlobalTechCoef_en") ->
      L222.GlobalTechCoef_en_USA

    L222.GlobalTechShrwt_en_USA %>%
      add_title("Technology shareweights for USA energy sectors") %>%
      add_units("NA") %>%
      add_comments("Shareweights from L222.GlobalTechShrwt_en are filtered for USA energy sectors.") %>%
      add_legacy_name("L222.GlobalTechShrwt_en_USA") %>%
      add_precursors("L222.GlobalTechShrwt_en") ->
      L222.GlobalTechShrwt_en_USA

    L222.GlobalTechCapture_en_USA %>%
      add_title("Carbon capture data for USA energy sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon capture data  from L222.GlobalTechCapture_en are filtered for USA energy sectors.") %>%
      add_legacy_name("L222.GlobalTechCapture_en_USA") %>%
      add_precursors("L222.GlobalTechCapture_en") ->
      L222.GlobalTechCapture_en_USA

    return_data(L222.DeleteStubTech_USAen, L222.PassThroughSector_USAen, L222.Tech_USAen,
                L222.TechShrwt_USAen, L222.TechInterp_USAen, L222.TechCoef_USAen, L222.Production_USArefining,
                L222.Supplysector_en_USA, L222.SubsectorShrwtFllt_en_USA, L222.StubTechProd_refining_USA, L222.StubTechMarket_en_USA,
                L222.CarbonCoef_en_USA, L222.GlobalTechSCurve_en_USA, L222.GlobalTechProfitShutdown_en_USA,
                L222.GlobalTechCost_en_USA,
                L222.SubsectorLogit_en_USA,
                L222.StubTech_en_USA,
                L222.StubTechCoef_refining_USA,
                L222.GlobalTechInterp_en_USA,
                L222.GlobalTechCoef_en_USA,
                L222.GlobalTechShrwt_en_USA,
                L222.GlobalTechCapture_en_USA)
  } else {
    stop("Unknown command")
  }
}
