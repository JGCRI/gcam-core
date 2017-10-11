#' module_gcam.usa_L226.en_distribution_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.DeleteSupplysector_USAelec}, \code{object}, \code{L226.StubTechCoef_electd_USA}, \code{L226.TechShrwt_electd_USA}, \code{L226.TechCost_electd_USA}, \code{L226.TechCoef_electd_USA}, \code{L226.SectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.Supplysector_en_USA}, \code{L226.SubsectorShrwtFllt_en_USA}, \code{L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en_USA}, \code{L226.TechShrwt_en_USA}, \code{L226.TechCoef_en_USA}, \code{L226.TechCost_en_USA}, \code{L226.Ccoef}. The corresponding file in the
#' original data system was \code{L226.en_distribution_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L226.en_distribution_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-usa/EIA_state_energy_prices",
             "L202.CarbonCoef",
             "L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             "L226.SubsectorShrwt_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             "L226.SubsectorInterpTo_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.DeleteSupplysector_USAelec",
             "L226.StubTechCoef_electd_USA",
             "L226.TechShrwt_electd_USA",
             "L226.TechCost_electd_USA",
             "L226.TechCoef_electd_USA",
             "L226.Supplysector_en_USA",
             "L226.SubsectorShrwtFllt_en_USA",
             "L226.SubsectorLogit_en_USA",
             "L226.TechShrwt_en_USA",
             "L226.TechCoef_en_USA",
             "L226.TechCost_en_USA",
             "L226.Ccoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A26.sector <- get_data(all_data, "energy/A26.sector")
    EIA_state_energy_prices <- get_data(all_data, "gcam-usa/EIA_state_energy_prices")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")



    # Build tables

    # Supplysector information

    # PART 1: FUEL HANDLING AND DELIVERY SECTORS

    # L226.Supplysector_en_USA: Supply sector information for energy handling and delivery sectors
    # NOTE: Currently using FERC regions as a proxy for regional energy markets
    A21.sector %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in%gcamusa.REGIONAL_FUEL_MARKETS) ->
      A21.tmp

    A26.sector  %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in%gcamusa.REGIONAL_FUEL_MARKETS) %>%
      bind_rows(A21.tmp, .) %>%
      repeat_add_columns(tibble(region = unique(states_subregions$grid_region))) %>%
      mutate(logit.year.fillout = min(BASE_YEARS)) ->
      L226.Supplysector_en_USA


    # L226.SubsectorShrwtFllt_en_USA: subsector shareweights of energy handling and delivery
    L226.Supplysector_en_USA %>%
      mutate(subsector = supplysector,
             year.fillout = min(BASE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(one_of(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) ->
      L226.SubsectorShrwtFllt_en_USA


    # L226.SubsectorLogit_en_USA
    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L226.SubsectorShrwtFllt_en_USA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(logit.year.fillout = min(BASE_YEARS),
             logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = NA) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"))) ->
      L226.SubsectorLogit_en_USA


    # L226.TechShrwt_en_USA: technology shareweights of energy handling and delivery
    # NOTE: can't use stub technologies because these would inherit the wrong energy-inputs
    L226.SubsectorShrwtFllt_en_USA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector,
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight"))) ->
      L226.TechShrwt_en_USA


    # L226.TechCoef_en_USA: technology coefficients and market names of energy handling and delivery
    L226.TechShrwt_en_USA %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             market.name = gcamusa.DEFAULT_MARKET) ->
      L226.TechCoef_en_USA


    # L226.CostAdj_75USDGJ_FERC_F: grid region specific cost adders
    # NOTE: the average national costs are already accounted in the corresponding sectors of the USA;
    # this table implements a price adjustment factor.
    #
    # Get US prices for coal, natural gas, distillate fuel oil for use in calculating the adjustments:
    EIA_state_energy_prices %>%
      filter(State == "United States") %>%
      select(Coal, Natural.gas, Distillate.fuel.oil) ->
      EIA_US_C_NG_DFO_prices

    # Calculate the adjustment factor:
    # Distillate fuel oil is used as proxy for liquid fuels to avoid composition bias in the petroleum total.
    # In other words, states with a lot of residual fuel would end up having lower apparent liquid fuel prices.
    # In states with missing values for coal, assign the maximum price.
    # For gas, the value in Hawaii is extremely high; just cap it at a max threshold
    EIA_state_energy_prices %>%
      # save NA for processing
      left_join(select(states_subregions, grid_region, state_name), by = c("State" = "state_name")) %>%
      mutate(coal_adj = (Coal -  EIA_US_C_NG_DFO_prices$Coal) * CONV_BTU_KJ * gdp_deflator(1975, 2009),
             gas_adj =  if_else(State == "Hawaii", gcamusa.GAS_ADJ_THRESH,
                                (Natural.gas - EIA_US_C_NG_DFO_prices$Natural.gas) * CONV_BTU_KJ * gdp_deflator(1975, 2009)),
             liq_adj = (Distillate.fuel.oil - EIA_US_C_NG_DFO_prices$Distillate.fuel.oil)* CONV_BTU_KJ  * gdp_deflator(1975, 2009)) ->
      EIA_tmp

    # get maximum coal adjustment for replacing NA's:
    EIA_tmp %>%
      select(coal_adj) %>%
      na.omit %>%
      summarize(coal_adj = max(coal_adj)) %>%
      as.double ->
      maxCoalAdj

    # use to replace NAs and finish adjustment calculations:
    EIA_tmp%>%
      replace_na(list(coal_adj = maxCoalAdj)) %>%
      group_by(grid_region) %>%
      summarize(coal_adj = median(coal_adj),
                gas_adj = median(gas_adj),
                liq_adj = median(liq_adj)) %>%
      ungroup %>%
      na.omit ->
      L226.CostAdj_75USDGJ_FERC_F


    # L226.TechCost_en_USA: cost adders
    L226.TechShrwt_en_USA %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L226.CostAdj_75USDGJ_FERC_F, by = c("region" = "grid_region")) %>%
      rename(coal = coal_adj,
             gas = gas_adj,
             liquids = liq_adj) %>%
      gather(sector1, adjustment, -region, -supplysector, -subsector, -technology, -year, -minicam.non.energy.input) %>%
      mutate(tmp = supplysector) %>%
      separate(tmp, c("trash1", "sector2", "trash2"), sep = " ") %>%
      filter(sector1 == sector2) %>%
      select(-trash1, -trash2, -sector1, -sector2) %>%
      rename(input.cost = adjustment) %>%
      mutate(input.cost = round(input.cost, gcamusa.DIGITS_COST)) ->
      L226.TechCost_en_USA


    # L226.Ccoef: carbon coef for cost adder sectors
    L202.CarbonCoef %>%
      filter(region == "USA") %>%
      select(-region) ->
      L226.Ccoef.usa

    L226.TechCost_en_USA %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(L226.Ccoef.usa, by = c("supplysector" = "PrimaryFuelCO2Coef.name")) ->
      L226.Ccoef



    # PART 2: ELECTRICITY TRANSMISSION AND DISTRIBUTION

    # L226.DeleteSupplysector_USAelec: Removing the electricity T&D sectors of the USA region
    # This should probably be converted to an assumption and read in at some point.
    L226.DeleteSupplysector_USAelec <- tibble(region = "USA", supplysector = c("elect_td_bld", "elect_td_ind", "elect_td_trn"))


    ### Replacing for loop starting on line 152 in old DS. I /think/ these tibbles should be output in place of the "object" that currently is;
    ### if that's the case, that needs to be updated and should rewrite below as a function.
    ### Otherwise just delete all of the below to like 249
    ### There's also a couple inputs to this chunk that are NULL, and nothing gets done to: L226.SubsectorShrwt_en, L226.SubsectorInterpTo_en
    L226.Supplysector_en %>%
      filter(region == "USA",
             supplysector %in% L226.DeleteSupplysector_USAelec$supplysector) %>%
      select(-region) %>%
      set_years() %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) ->
      L226.Supplysector_electd_USA

    L226.SubsectorLogit_en %>%
      filter(region == "USA",
             supplysector %in% L226.DeleteSupplysector_USAelec$supplysector) %>%
      select(-region) %>%
      set_years() %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) ->
      L226.SubsectorLogit_electd_USA

    L226.SubsectorShrwtFllt_en %>%
      filter(region == "USA",
             supplysector %in% L226.DeleteSupplysector_USAelec$supplysector) %>%
      select(-region) %>%
      set_years() %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) ->
      L226.SubsectorShrwtFllt_electd_USA

    L226.SubsectorInterp_en %>%
      filter(region == "USA",
             supplysector %in% L226.DeleteSupplysector_USAelec$supplysector) %>%
      select(-region) %>%
      set_years() %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) ->
      L226.SubsectorInterp_electd_USA



    if(!gcamusa.USE_REGIONAL_ELEC_MARKETS){
      # L226.StubTechCoef_electd_USA: Using national elec markets. State elect_td sectors are treated as stub technologies
      # L226.StubTechCoef_electd %>%
      #   filter(region == "USA") %>%
      #   select(-region) %>%
      #   write_to_all_states(., names(.)) ->
      #   L226.StubTechCoef_electd_USA
    }


    if(gcamusa.USE_REGIONAL_ELEC_MARKETS){
      # L226.TechShrwt_electd_USA: using regional elec markets. The elect_td sectors can not use the global tech database as their input is different. Remaking
    }


    # Produce outputs
    L226.DeleteSupplysector_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.DeleteSupplysector_USAelec") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.DeleteSupplysector_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTechCoef_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTechCoef_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechShrwt_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechShrwt_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCost_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCost_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCoef_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCoef_electd_USA

    L226.Supplysector_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Supplysector_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.Supplysector_en_USA

    L226.SubsectorShrwtFllt_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorShrwtFllt_en_USA

    L226.SubsectorLogit_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorLogit_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorLogit_en_USA

    L226.TechShrwt_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechShrwt_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechShrwt_en_USA

    L226.TechCoef_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCoef_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCoef_en_USA

    L226.TechCost_en_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCost_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCost_en_USA

    L226.Ccoef %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Ccoef") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.Ccoef

    return_data(L226.DeleteSupplysector_USAelec, object, L226.StubTechCoef_electd_USA, L226.TechShrwt_electd_USA, L226.TechCost_electd_USA,
                L226.TechCoef_electd_USA, L226.Supplysector_en_USA, L226.SubsectorShrwtFllt_en_USA, L226.SubsectorLogit_en_USA,
                L226.TechShrwt_en_USA, L226.TechCoef_en_USA, L226.TechCost_en_USA, L226.Ccoef)
  } else {
    stop("Unknown command")
  }
}
