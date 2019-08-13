#' module_gcamindia_L226.en_distribution
#'
#' Create a variety of energy and electricity outputs for India at the state and/or grid_region level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \itemize{
#' \item{\code{L226.india_state_DeleteSupplysector_elec}: Removing the electricity T&D sectors of the India region.}
#' \item{\code{L226.india_state_StubTechCoef_electd}: Stub technology coefficients elec T&D when using national elec markets. State elect_td sectors are treated as stub technologies.}
#' \item{\code{L226.india_state_TechShrwt_electd}: tech share weights for elec T&D when using regional electricity markets. The elect_td sectors can not use the global tech database as their input is different.}
#' \item{\code{L226.india_state_TechCost_electd}: Tech costs for elec T&D when using regional electricity markets.}
#' \item{\code{L226.india_state_TechCoef_electd}: Tech coeff for elec T&D when using regional electricity markets.}
#' \item{\code{L226.india_state_Supplysector_en}: Supply sector information for energy handling and delivery sectors for India grid regions. Currently using FERC regions as a proxy for regional energy markets.}
#' \item{\code{L226.india_state_SubsectorShrwtFllt_en}: Subsector shareweights of energy handling and delivery.}
#' \item{\code{L226.india_state_SubsectorLogit_en}: Logit info for energy subsectors. There is only one tech per subsector so the logit choice does not matter.}
#' \item{\code{L226.india_state_TechShrwt_en}: Technology shareweights of energy handling and delivery. Can't use stub technologies because these would inherit the wrong energy-inputs.}
#' \item{\code{L226.india_state_TechCoef_en}: Technology coefficients and market names of energy handling and delivery.}
#' \item{\code{L226.india_state_TechCost_en}: Regional price adjustments/cost adders for India energy.}
#' \item{\code{L226.india_state_Ccoef}: Carbon coef for India cost adder sectors.}
#' \item{\code{L226.india_state_Supplysector_electd}: India supply sector input, output, and logit info for elec T&D by state.}
#' \item{\code{L226.india_state_SubsectorLogit_electd}: India subsector logit info for elec T&D by grid_region.}
#' \item{\code{L226.india_state_SubsectorShrwtFllt_electd}: India subsector shareweight fillout for elec T&D by state.}
#' \item{\code{L226.india_state_SubsectorInterp_electd}: India interpolation info for elec T&D by state.}
#' }
#' The corresponding file in the original data system was \code{L226.india_state_en_distribution.R} (gcam-india level2).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author  PNK Aug 2019


module_gcamindia_L226.en_distribution <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-india/A26.india_state_energy_prices",
             "L202.CarbonCoef",
             "L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             FILE = "gcam-india/A26.india_state_StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.india_state_DeleteSupplysector_elec",
             "L226.india_state_StubTechCoef_electd",
             "L226.india_state_TechShrwt_electd",
             "L226.india_state_TechCost_electd",
             "L226.india_state_TechCoef_electd",
             "L226.india_state_Supplysector_electd",
             "L226.india_state_SubsectorLogit_electd",
             "L226.india_state_SubsectorShrwtFllt_electd",
             "L226.india_state_SubsectorInterp_electd",
             "L226.india_state_Supplysector_en",
             "L226.india_state_SubsectorShrwtFllt_en",
             "L226.india_state_SubsectorLogit_en",
             "L226.india_state_TechShrwt_en",
             "L226.india_state_TechCoef_en",
             "L226.india_state_TechCost_en",
             "L226.india_state_Ccoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A26.sector <- get_data(all_data, "energy/A26.sector")
    A26.india_state_energy_prices <- get_data(all_data, "gcam-india/A26.india_state_energy_prices")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    A26.india_state_StubTechCoef_electd <- get_data(all_data, "gcam-india/A26.india_state_StubTechCoef_electd")


    # silence check package notes
    region <- supplysector <- from.year <- to.year <- output.unit <- input.unit <- price.unit <- liq_adj <-
      logit.exponent <- logit.type <- . <- subsector <- State <- Coal <- Natural.gas <- Distillate.fuel.oil <-
      grid_region <- state_name <- coal_adj <- gas_adj <- liq_adju <- sector1 <- adjustment <- technology <-
      year <- minicam.non.energy.input <- tmp <- sector2 <- trash1 <- trash2 <- input.cost <- sector.name <-
      subsector.name <- stub.technology <- market.name <- state <- NULL


    # global_energy_to_india_electd - takes global energy inputs from L226.en_distribution.R
    # and processes for use in India electricity T&D
    global_energy_to_india_electd <- function(data) {
      data %>%
        filter(region == gcam.india_REGION,
               supplysector %in% gcamindia.ELECT_TD_SECTORS) %>%
        write_to_all_india_states(names(data))
    } # end global_energy_to_india_electd


    # Process inputs:
    L226.SubsectorInterp_en %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) ->
      L226.SubsectorInterp_en


    # Build tables

    # Supplysector information

    # PART 1: FUEL HANDLING AND DELIVERY SECTORS

    # L226.india_state_Supplysector_en: Supply sector information for energy handling and delivery sectors
    A21.sector %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamindia.REGIONAL_FUEL_MARKETS) ->
      A21.tmp

    A26.sector  %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamindia.REGIONAL_FUEL_MARKETS) %>%
      bind_rows(A21.tmp) %>%
      repeat_add_columns(tibble(region = unique(india_states_subregions$grid_region))) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) ->
      L226.india_state_Supplysector_en


    # L226.india_state_SubsectorShrwtFllt_en: subsector shareweights of energy handling and delivery
    L226.india_state_Supplysector_en %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamindia.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L226.india_state_SubsectorShrwtFllt_en


    # L226.india_state_SubsectorLogit_en
    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L226.india_state_SubsectorShrwtFllt_en %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamindia.DEFAULT_LOGITEXP,
             logit.type = NA) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L226.india_state_SubsectorLogit_en


    # L226.india_state_TechShrwt_en: technology shareweights of energy handling and delivery
    # NOTE: can't use stub technologies because these would inherit the wrong energy-inputs
    L226.india_state_SubsectorShrwtFllt_en %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector,
             share.weight = gcamindia.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight") ->
      L226.india_state_TechShrwt_en


    # L226.india_state_TechCoef_en: technology coefficients and market names of energy handling and delivery
    L226.india_state_TechShrwt_en %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = gcamindia.DEFAULT_COEFFICIENT,
             market.name = gcamindia.DEFAULT_MARKET) ->
      L226.india_state_TechCoef_en


    # L226.india_state_CostAdj_75USDGJ_FERC_F: grid region specific cost adders
    # NOTE: the average national costs are already accounted in the corresponding sectors of the india;
    # this table implements a price adjustment factor.
    #
    # Step 1 to calculate the cost adders:
    # Get US prices for coal, natural gas, distillate fuel oil for use in calculating the adjustments:
    A26.india_state_energy_prices %>%
      filter(State == "IND") %>%
      select(Coal, Natural.gas, Distillate.fuel.oil) ->
      india_C_NG_DFO_prices

    # Calculate the adjustment factor:
    # Step 2: Use the India prices calculated in step 1 to compute the
    # adjustment factors = (state price - India price) * some unit conversions.
    # Distillate fuel oil is used as proxy for liquid fuels to avoid composition bias in the petroleum total.
    # In other words, states with a lot of residual fuel would end up having lower apparent liquid fuel prices.
    # In states with missing values for coal, assign the maximum price.
    # For gas, the value in Hawaii is extremely high; just cap it at a max threshold
    A26.india_state_energy_prices %>%
      # save NA for processing
      left_join(select(india_states_subregions, grid_region, state_name), by = c("State" = "state_name")) %>%
      mutate(coal_adj = (Coal -  india_C_NG_DFO_prices$Coal) * CONV_BTU_KJ * gdp_deflator(1975, 2009),
             gas_adj =  if_else(State == "AP", gcamindia.GAS_ADJ_THRESH,
                                (Natural.gas - india_C_NG_DFO_prices$Natural.gas) * CONV_BTU_KJ * gdp_deflator(1975, 2009)),
             liq_adj = (Distillate.fuel.oil - india_C_NG_DFO_prices$Distillate.fuel.oil)* CONV_BTU_KJ  * gdp_deflator(1975, 2009)) ->
      Prices_tmp

    # Step 3: get maximum coal adjustment for replacing NA's:
    Prices_tmp %>%
      select(coal_adj) %>%
      na.omit %>%
      summarize(coal_adj = max(coal_adj)) %>%
      as.double ->
      maxCoalAdj

    # Step 4 use to replace NAs from step 2 and finish adjustment calculations by taking the median of each grid_region:
    Prices_tmp%>%
      replace_na(list(coal_adj = maxCoalAdj)) %>%
      group_by(grid_region) %>%
      summarize(coal_adj = median(coal_adj),
                gas_adj = median(gas_adj),
                liq_adj = median(liq_adj)) %>%
      ungroup %>%
      na.omit ->
      L226.india_state_CostAdj_75USDGJ_FERC_F


    # L226.india_state_TechCost_en: cost adders
    L226.india_state_TechShrwt_en %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L226.india_state_CostAdj_75USDGJ_FERC_F, by = c("region" = "grid_region")) %>%
      rename(coal = coal_adj,
             gas = gas_adj,
             liquids = liq_adj) %>%
      gather(sector1, adjustment, -region, -supplysector, -subsector, -technology, -year, -minicam.non.energy.input) %>%
      mutate(tmp = supplysector,
             tmp = if_else(grepl("refined liquids*", tmp), "refined liquids", tmp)) %>%
      separate(tmp, c("trash1", "sector2"), sep = " ") %>%
      filter(sector1 == sector2) %>%
      select(-trash1, -sector1, -sector2) %>%
      rename(input.cost = adjustment) %>%
      mutate(input.cost = round(input.cost, gcamindia.DIGITS_COST)) ->
      L226.india_state_TechCost_en


    # L226.india_state_Ccoef: carbon coef for cost adder sectors
    L202.CarbonCoef %>%
      filter(region == gcam.india_REGION) %>%
      select(-region) ->
      L226.india_state_Ccoef.india

    L226.india_state_TechCost_en %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(L226.india_state_Ccoef.india, by = c("supplysector" = "PrimaryFuelCO2Coef.name")) ->
      L226.india_state_Ccoef



    # PART 2: ELECTRICITY TRANSMISSION AND DISTRIBUTION

    # L226.india_state_DeleteSupplysector_elec: Removing the electricity T&D sectors of the India region
    # This should probably be converted to an assumption and read in at some point.
    L226.india_state_DeleteSupplysector_elec <- tibble(region = gcam.india_REGION, supplysector = gcamindia.ELECT_TD_SECTORS)


    # Replacing for loop starting on line 152 in old DS.
    # There's also two inputs to this chunk that are NULL, and nothing gets done to: L226.SubsectorShrwt_en, L226.SubsectorInterpTo_en
    L226.Supplysector_en %>%
      global_energy_to_india_electd() ->
      L226.india_state_Supplysector_electd

    L226.SubsectorLogit_en %>%
      global_energy_to_india_electd() ->
      L226.india_state_SubsectorLogit_electd

    L226.SubsectorShrwtFllt_en %>%
      global_energy_to_india_electd() ->
      L226.india_state_SubsectorShrwtFllt_electd

    L226.SubsectorInterp_en %>%
      global_energy_to_india_electd() ->
      L226.india_state_SubsectorInterp_electd


    # Using national electric markets
    if(!gcamindia.USE_REGIONAL_ELEC_MARKETS) {

      # L226.india_state_StubTechCoef_electd: Using national elec markets. State elect_td sectors are treated as stub technologies
      A26.india_state_StubTechCoef_electd %>%
        global_energy_to_india_electd() ->
        L226.india_state_StubTechCoef_electd

    }


    # Using regional electric markets
    if(gcamindia.USE_REGIONAL_ELEC_MARKETS) {

      # The elect_td sectors can not use the global tech database as their input is different.

      # L226.india_state_TechShrwt_electd: Tech share weights for electricity T&D
      L226.GlobalTechShrwt_en %>%
        filter(sector.name %in% gcamindia.ELECT_TD_SECTORS) %>%
        write_to_all_india_states(c("region", names(L226.GlobalTechShrwt_en))) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.india_state_TechShrwt_electd

      # L226.india_state_TechCost_electd: Tech costs for electricity T&D
      L226.GlobalTechCost_en %>%
        filter(sector.name %in% gcamindia.ELECT_TD_SECTORS) %>%
        write_to_all_india_states(c("region", names(L226.GlobalTechCost_en))) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.india_state_TechCost_electd

      # L226.india_state_TechCoef_electd: Tech coefficients for electricity T&D
      A26.india_state_StubTechCoef_electd %>%
        global_energy_to_india_electd() %>%
        rename(technology = stub.technology) %>%
        mutate(minicam.energy.input = "electricity domestic supply") %>%
        select(-market.name) %>%
        left_join_error_no_match(select(india_states_subregions, grid_region, state), by = c("region" = "state")) %>%
        rename(market.name = grid_region) ->
        L226.india_state_TechCoef_electd
    }


    # Produce outputs
    L226.india_state_DeleteSupplysector_elec %>%
      add_title("Removing the electricity T&D sectors of the India region") %>%
      add_units("NA") %>%
      add_comments("Removing the electricity T&D sectors of the India region") %>%
      add_legacy_name("L226.india_state_DeleteSupplysector_elec") ->
      L226.india_state_DeleteSupplysector_elec

    if(exists("L226.india_state_StubTechCoef_electd")) {
      L226.india_state_StubTechCoef_electd %>%
        add_title("Stub technology coefficients elec T&D when using national elec markets") %>%
        add_units("NA") %>%
        add_comments("Stub technology coefficients elec T&D when using national elec markets.") %>%
        add_comments("State elect_td sectors are treated as stub technologies.") %>%
        add_legacy_name("L226.india_state_StubTechCoef_electd") %>%
        add_precursors("gcam-india/A26.india_state_StubTechCoef_electd") ->
        L226.india_state_StubTechCoef_electd
    } else {
      # If gcamindia.USE_REGIONAL_ELEC_MARKETS is TRUE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L226.india_state_StubTechCoef_electd") %>%
        add_precursors("gcam-india/A26.india_state_StubTechCoef_electd") ->
        L226.india_state_StubTechCoef_electd
    }


    if(exists("L226.india_state_TechShrwt_electd")) {
      L226.india_state_TechShrwt_electd %>%
        add_title("Tech share weights for elec T&D when using regional electricity markets") %>%
        add_units("NA") %>%
        add_comments("Tech share weights for elec T&D when using regional electricity markets") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.india_state_TechShrwt_electd") %>%
        add_precursors("L226.GlobalTechShrwt_en") ->
        L226.india_state_TechShrwt_electd
    } else {
      # If gcamindia.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.india_state_TechShrwt_electd") %>%
        add_precursors("L226.GlobalTechShrwt_en") ->
        L226.india_state_TechShrwt_electd
    }


    if(exists("L226.india_state_TechCost_electd")) {
      L226.india_state_TechCost_electd %>%
        add_title("Tech costs for elec T&D when using regional electricity markets") %>%
        add_units("1975$") %>%
        add_comments("Tech costs for elec T&D when using regional electricity markets") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.india_state_TechCost_electd") %>%
        add_precursors("L226.GlobalTechCost_en") ->
        L226.india_state_TechCost_electd
    } else {
      # If gcamindia.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.india_state_TechCost_electd") %>%
        add_precursors("L226.GlobalTechCost_en") ->
        L226.india_state_TechCost_electd
    }


    if(exists("L226.india_state_TechCoef_electd")) {
      L226.india_state_TechCoef_electd %>%
        add_title("Tech coefficients for elec T&D when using regional electricity markets") %>%
        add_units("NA") %>%
        add_comments("Tech coeff for elec T&D when using regional electricity markets.") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.india_state_TechCoef_electd") %>%
        add_precursors("gcam-india/india_states_subregions",
                       "gcam-india/A26.india_state_StubTechCoef_electd") ->
        L226.india_state_TechCoef_electd
    } else {
      # If gcamindia.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.india_state_TechCoef_electd") %>%
        add_precursors("gcam-india/india_states_subregions",
                       "gcam-india/A26.india_state_StubTechCoef_electd") ->
        L226.india_state_TechCoef_electd
    }


    L226.india_state_Supplysector_en %>%
      add_title("Supply sector information for energy handling and delivery sectors.") %>%
      add_units("varies") %>%
      add_comments("Supply sector information for energy handling and delivery sectors for india grid regions.") %>%
      add_comments("Currently using FERC regions as a proxy for regional energy markets.") %>%
      add_legacy_name("L226.india_state_Supplysector_en") %>%
      add_precursors("energy/A21.sector",
                     "energy/A26.sector") ->
      L226.india_state_Supplysector_en

    L226.india_state_SubsectorShrwtFllt_en %>%
      add_title("Subsector shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights of energy handling and delivery") %>%
      add_legacy_name("L226.india_state_SubsectorShrwtFllt_en") %>%
      same_precursors_as(L226.india_state_Supplysector_en) ->
      L226.india_state_SubsectorShrwtFllt_en

    L226.india_state_SubsectorLogit_en %>%
      add_title("Logit info for energy subsectors") %>%
      add_units("NA") %>%
      add_comments("Logit info for energy subsectors.") %>%
      add_comments("There is only one tech per subsector so the logit choice does not matter.") %>%
      add_legacy_name("L226.india_state_SubsectorLogit_en") %>%
      same_precursors_as(L226.india_state_SubsectorShrwtFllt_en) ->
      L226.india_state_SubsectorLogit_en

    L226.india_state_TechShrwt_en %>%
      add_title("Technology shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Technology shareweights of energy handling and delivery.") %>%
      add_comments("Can't use stub technologies because these would inherit the wrong energy-inputs.") %>%
      add_legacy_name("L226.india_state_TechShrwt_en") %>%
      same_precursors_as(L226.india_state_SubsectorShrwtFllt_en) ->
      L226.india_state_TechShrwt_en

    L226.india_state_TechCoef_en %>%
      add_title("Technology coefficients and market names of energy handling and delivery") %>%
      add_units("units") %>%
      add_comments("Technology coefficients and market names of energy handling and delivery") %>%
      add_legacy_name("L226.india_state_TechCoef_en") %>%
      same_precursors_as(L226.india_state_TechShrwt_en) ->
      L226.india_state_TechCoef_en

    L226.india_state_TechCost_en %>%
      add_title("Regional price adjustments/cost adders for India energy.") %>%
      add_units("1975$/GJ") %>%
      add_comments("Regional price adjustments/cost adders for India energy") %>%
      add_legacy_name("L226.india_state_TechCost_en") %>%
      add_precursors("gcam-india/india_states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-india/A26.india_state_energy_prices") ->
      L226.india_state_TechCost_en

    L226.india_state_Ccoef %>%
      add_title("Carbon coef for cost adder sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon coef for cost adder sectors") %>%
      add_legacy_name("L226.india_state_Ccoef") %>%
      add_precursors("gcam-india/india_states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-india/A26.india_state_energy_prices",
                     "L202.CarbonCoef") ->
      L226.india_state_Ccoef

    L226.india_state_Supplysector_electd %>%
      add_title("India supply sector input, output, and logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("India supply sector input, output, and logit info for elec T&D by state") %>%
      add_legacy_name("L226.india_state_Supplysector_electd") %>%
      add_precursors("L226.Supplysector_en") ->
      L226.india_state_Supplysector_electd

    L226.india_state_SubsectorLogit_electd %>%
      add_title("India subsector logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("India subsector logit info for elec T&D by grid_region") %>%
      add_legacy_name("L226.india_state_SubsectorLogit_electd") %>%
      add_precursors("L226.SubsectorLogit_en") ->
      L226.india_state_SubsectorLogit_electd

    L226.india_state_SubsectorShrwtFllt_electd %>%
      add_title("India subsector shareweight fillout for elec T&D") %>%
      add_units("varies") %>%
      add_comments("India subsector shareweight fillout for elec T&D by state") %>%
      add_legacy_name("L226.india_state_SubsectorShrwtFllt_electd") %>%
      add_precursors("L226.SubsectorShrwtFllt_en") ->
      L226.india_state_SubsectorShrwtFllt_electd

    L226.india_state_SubsectorInterp_electd %>%
      add_title("India interpolation info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("India interpolation info for elec T&D by state") %>%
      add_legacy_name("L226.india_state_SubsectorInterp_electd") %>%
      add_precursors("L226.SubsectorInterp_en") ->
      L226.india_state_SubsectorInterp_electd

    return_data(L226.india_state_DeleteSupplysector_elec,
                L226.india_state_StubTechCoef_electd,
                L226.india_state_TechShrwt_electd,
                L226.india_state_TechCost_electd,
                L226.india_state_TechCoef_electd,
                L226.india_state_Supplysector_en,
                L226.india_state_SubsectorShrwtFllt_en,
                L226.india_state_SubsectorLogit_en,
                L226.india_state_TechShrwt_en,
                L226.india_state_TechCoef_en,
                L226.india_state_TechCost_en,
                L226.india_state_Ccoef,
                L226.india_state_Supplysector_electd,
                L226.india_state_SubsectorLogit_electd,
                L226.india_state_SubsectorShrwtFllt_electd,
                L226.india_state_SubsectorInterp_electd)
  } else {
    stop("Unknown command")
  }
}
