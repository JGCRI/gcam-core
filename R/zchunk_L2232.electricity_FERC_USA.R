#' module_gcam.usa_L2232.electricity_FERC_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2232.DeleteSupplysector_USAelec}, \code{L2232.Supplysector_USAelec},
#' \code{L2232.SubsectorShrwtFllt_USAelec}, \code{L2232.SubsectorInterp_USAelec}, \code{L2232.SubsectorLogit_USAelec},
#' \code{L2232.TechShrwt_USAelec}, \code{L2232.TechCoef_USAelec}, \code{L2232.Production_exports_USAelec},
#' \code{L2232.Supplysector_elec_FERC}, \code{L2232.ElecReserve_FERC}, \code{L2232.SubsectorShrwtFllt_elec_FERC},
#' \code{L2232.SubsectorInterp_elec_FERC}, \code{L2232.SubsectorLogit_elec_FERC}, \code{L2232.TechShrwt_elec_FERC},
#' \code{L2232.TechCoef_elec_FERC}, \code{L2232.TechCoef_elecownuse_FERC}, \code{L2232.Production_imports_FERC},
#' \code{L2232.Production_elec_gen_FERC}, \code{L2232.StubTechElecMarket_backup_USA}. The corresponding file in the
#' original data system was \code{L2232.electricity_FERC_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L2232.electricity_FERC_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/A23.sector",
             FILE = "gcam-usa/A232.structure",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec",
             "L126.in_EJ_state_td_elec",
             "L126.out_EJ_state_td_elec",
             "L132.out_EJ_state_indchp_F",
             FILE = "temp-data-inject/L223.Supplysector_elec",
             "L1232.out_EJ_sR_elec",
             "L223.StubTechMarket_backup_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2232.DeleteSupplysector_USAelec",
             "L2232.Supplysector_USAelec",
             "L2232.SubsectorShrwtFllt_USAelec",
             "L2232.SubsectorInterp_USAelec",
             "L2232.SubsectorLogit_USAelec",
             "L2232.TechShrwt_USAelec",
             "L2232.TechCoef_USAelec",
             "L2232.Production_exports_USAelec",
             "L2232.Supplysector_elec_FERC",
             "L2232.ElecReserve_FERC",
             "L2232.SubsectorShrwtFllt_elec_FERC",
             "L2232.SubsectorInterp_elec_FERC",
             "L2232.SubsectorLogit_elec_FERC",
             "L2232.TechShrwt_elec_FERC",
             "L2232.TechCoef_elec_FERC",
             "L2232.TechCoef_elecownuse_FERC",
             "L2232.Production_imports_FERC",
             "L2232.Production_elec_gen_FERC",
             "L2232.StubTechElecMarket_backup_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.sector <- get_data(all_data, "energy/A23.sector")
    A232.structure <- get_data(all_data, "gcam-usa/A232.structure")
    L123.in_EJ_state_ownuse_elec <- get_data(all_data, "L123.in_EJ_state_ownuse_elec")
    L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec")
    L126.in_EJ_state_td_elec <- get_data(all_data, "L126.in_EJ_state_td_elec")
    L126.out_EJ_state_td_elec <- get_data(all_data, "L126.out_EJ_state_td_elec")
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")
    L223.Supplysector_elec <- get_data(all_data, "temp-data-inject/L223.Supplysector_elec")
    L1232.out_EJ_sR_elec <- get_data(all_data, "L1232.out_EJ_sR_elec")
    L223.StubTechMarket_backup_USA <- get_data(all_data, "L223.StubTechMarket_backup_USA")

    # This chunk only builds the electric sector model input if gcamusa.USE_REGIONAL_ELEC_MARKETS is TRUE,
    # indicating the demand is being resolved at the level of the grid regions
    if(gcamusa.USE_REGIONAL_ELEC_MARKETS) {

      # A vector of USA grid region names
      states_subregions %>%
        select(grid_region) %>%
        unique %>%
        arrange(grid_region) %>%
        unlist ->
        grid_regions

      # PART 1: THE USA REGION
      # Remove the USA electricity sector, and replace with electricity trade
      # L2232.DeleteSupplysector_USAelec: Removing the electricity sectors of the USA region (incl. net_ownuse)
      tibble(region = "USA",
             supplysector = c("electricity", "electricity_net_ownuse")) ->
        L2232.DeleteSupplysector_USAelec

      # L2232.Supplysector_USAelec: supplysector for electricity sector in the USA region,
      # including logit exponent between grid regions
      # All of the supplysector information is the same as before, except the logit exponent
      A232.structure %>%
        filter(region == "USA") %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = subsector.logit,
               logit.type = subsector.logit.type) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
        L2232.Supplysector_USAelec

      # No need to read in subsector logit exponents, which are applied to the technology competition
      # L2232.SubsectorShrwtFllt_USAelec: subsector (grid region) shareweights in USA electricity trade
      A232.structure %>%
        filter(region == "USA") %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        repeat_add_columns(tibble(grid_region = grid_regions)) %>%
        mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                   paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
               year.fillout = min(BASE_YEARS),
               share.weight = 1) %>%
        select(one_of(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) ->
        L2232.SubsectorShrwtFllt_USAelec

      # L2232.SubsectorInterp_USAelec: subsector (grid region) shareweights in USA electricity
      # NOTE: this just carries the base year shareweights forward;
      # regions that don't export in the base year don't export at all
      L2232.SubsectorShrwtFllt_USAelec %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(apply.to = "share-weight",
               from.year = max(BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L2232.SubsectorInterp_USAelec

      # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
      L2232.SubsectorShrwtFllt_USAelec %>%
        mutate(logit.year.fillout = min(BASE_YEARS)) %>%
        left_join(select(A232.structure, region,
                         logit.exponent = technology.logit,
                         logit.type = technology.logit.type),
                  by = "region") %>%
        select(one_of(LEVEL2_DATA_NAMES[["SubsectorLogit"]])) ->
        L2232.SubsectorLogit_USAelec

      # L2232.TechShrwt_USAelec: technology shareweights, USA region
      A232.structure %>%
        filter(region == "USA") %>%
        select(one_of(LEVEL2_DATA_NAMES[["Tech"]])) %>%
        repeat_add_columns(tibble(grid_region = grid_regions)) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                   paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
               share.weight = 1) %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechYr"]]), "share.weight", "grid_region") ->
        L2232.TechShrwt_USAelec

      # L2232.TechCoef_USAelec: technology coefficients and market names, USA region
      L2232.TechShrwt_USAelec %>%
        left_join_error_no_match(select(A232.structure, region, minicam.energy.input), by = "region") %>%
        mutate(coefficient = 1, market.name = grid_region) %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechCoef"]])) ->
        L2232.TechCoef_USAelec

      # Compile flows of electricity in each FERC region: generation, cogeneration, ownuse, and consumption by all sectors
      # Generation
      L1232.out_EJ_sR_elec %>%
        filter(year %in% BASE_YEARS) %>%
        select(grid_region, year, generation = value) ->
        L2232.out_EJ_sR_elec

      # Cogeneration is not included in the grid region totals; need to add it here for balance
      L132.out_EJ_state_indchp_F %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
        group_by(grid_region, year) %>%
        summarise(cogeneration = sum(value)) %>%
        ungroup ->
        L2232.out_EJ_sR_indchp_F

      # Subtract own use of electricity prior to calculating net exports
      # Calculate own use in each FERC region
      L123.in_EJ_state_ownuse_elec %>%
        rename(in_ownuse = value) %>%
        left_join_error_no_match(L123.out_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
        mutate(net_ownuse = in_ownuse - value) %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
        group_by(grid_region, year) %>%
        summarise(ownuse = sum(net_ownuse)) %>%
        ungroup ->
        L2232.net_EJ_sR_ownuse_elec

      # Indicate the sum of all demands in each FERC region (equal to the input to the elect_td sectors)
      L126.in_EJ_state_td_elec %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
        group_by(grid_region, year) %>%
        summarise(consumption = sum(value)) %>%
        ungroup ->
        L2232.in_EJ_sR_td_elec

      L2232.TechShrwt_USAelec %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(L2232.out_EJ_sR_elec, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.out_EJ_sR_indchp_F, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.net_EJ_sR_ownuse_elec, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.in_EJ_sR_td_elec, by = c("grid_region", "year")) %>%
        # Calculate net exports: generation + cogeneration - ownuse - consumption
        mutate(net.exports = generation + cogeneration - ownuse - consumption,
               # Split net exports into gross imports and exports
               imports = pmax(0, -1 * net.exports), exports = pmax(0, net.exports),
               # Calculate consumption from domestic sources: total consumption minus imports
               net.supply = consumption - imports) ->
        L2232.elec_flows_FERC

      # L2232.Production_exports_USAelec: calibrated exports of electricity from grid regions to shared USA region
      L2232.elec_flows_FERC %>%
        mutate(calOutputValue = round(exports, digit = energy.DIGITS_CALOUTPUT),
               share.weight.year = year,
               tech.share.weight = ifelse(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L2232.Production_exports_USAelec

      # PART 2: THE FERC REGIONS
      # Some of the information read in about these regions is in the primary electricity_USA code file
      # L2232.Supplysector_elec_FERC: supplysector information for electricity passthru sectors in the FERC regions
      A232.structure %>%
        filter(region == "grid_region") %>%
        select(-region) %>%
        repeat_add_columns(tibble(region = grid_regions)) %>%
        mutate(market.name = replace(market.name, grepl("grid_region", market.name),
                                   region[grepl("grid_region", market.name)])) ->
        A232.FERCstructure

      A232.FERCstructure %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = subsector.logit,
               logit.type = subsector.logit.type) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
        L2232.Supplysector_elec_FERC

      # L2232.ElecReserve_FERC: electricity reserve margin and avg grid capacity factor in the FERC regions
      A23.sector %>%
        filter(supplysector == "electricity") %>%
        repeat_add_columns(tibble(region = grid_regions)) %>%
        select(one_of(LEVEL2_DATA_NAMES[["ElecReserve"]])) ->
        L2232.ElecReserve_FERC

      # L2232.SubsectorShrwtFllt_elec_FERC: subsector (grid region) shareweights in USA electricity
      A232.FERCstructure %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(year.fillout = min(BASE_YEARS), share.weight = 1) ->
        L2232.SubsectorShrwtFllt_elec_FERC

      # L2232.SubsectorInterp_elec_FERC: subsector (grid region) shareweights in USA electricity
      L2232.SubsectorShrwtFllt_elec_FERC %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(apply.to = "share-weight",
               from.year = max(BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L2232.SubsectorInterp_elec_FERC

      # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
      L2232.SubsectorShrwtFllt_elec_FERC %>%
        left_join(select(A232.FERCstructure, region, technology.logit, technology.logit.type), by = "region") %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = technology.logit, logit.type = technology.logit.type) %>%
        select(one_of(LEVEL2_DATA_NAMES[["SubsectorLogit"]])) ->
        L2232.SubsectorLogit_elec_FERC

      # L2232.TechShrwt_elec_FERC: technology shareweights, USA region
      A232.FERCstructure %>%
        select(one_of(LEVEL2_DATA_NAMES[["Tech"]])) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(share.weight = 1) ->
        L2232.TechShrwt_elec_FERC

      # L2232.TechCoef_elec_FERC: technology coefficients and market names
      A232.FERCstructure %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        # Own use will be done separately; extract it from the table here
        filter(supplysector != "electricity_net_ownuse") %>%
        mutate(coefficient = 1) %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechCoef"]])) ->
        L2232.TechCoef_elec_FERC

      # L2232.TechCoef_elecownuse_FERC: own use coefficients in the grid regions
      L2232.elec_flows_FERC %>%
        mutate(ownuse_coef = (generation + cogeneration) / (generation + cogeneration - ownuse)) ->
        L2232.elec_flows_FERC

      A232.FERCstructure %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        filter(supplysector == "electricity_net_ownuse") %>%
        left_join(select(L2232.elec_flows_FERC, grid_region, year, coefficient = ownuse_coef),
                                 by = c("region" = "grid_region", "year")) %>%
        group_by(region) %>%
        mutate(coefficient = replace(coefficient, year %in% FUTURE_YEARS, coefficient[year == max(BASE_YEARS)])) %>%
        ungroup %>%
        select(one_of(LEVEL2_DATA_NAMES[["TechCoef"]])) ->
        L2232.TechCoef_elecownuse_FERC

      # L2232.Production_imports_FERC: calibrated electricity imports (from USA region)
      # L2232.Production_elec_gen_FERC: calibrated net electricity generation (from within grid region)
      L2232.TechCoef_elec_FERC %>%
        filter(year %in% BASE_YEARS, market.name == "USA") %>%
        left_join_error_no_match(select(L2232.elec_flows_FERC, grid_region, year, imports, net.supply),
                                 by = c("region" = "grid_region", "year")) %>%
        mutate(calOutputValue_imp = round(imports, digit = energy.DIGITS_CALOUTPUT),
               calOutputValue_gen = round(net.supply, digit = energy.DIGITS_CALOUTPUT),
               share.weight.year = year,
               tech.share.weight_imp = ifelse(calOutputValue_imp == 0, 0, 1),
               tech.share.weight_gen = ifelse(calOutputValue_gen == 0, 0, 1)) %>%
        set_subsector_shrwt() ->
        L2232.Production_FERC

      L2232.Production_FERC %>%
        rename(calOutputValue = calOutputValue_imp, tech.share.weight = tech.share.weight_imp) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L2232.Production_imports_FERC

      L2232.Production_FERC %>%
        rename(calOutputValue = calOutputValue_gen, tech.share.weight = tech.share.weight_gen) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L2232.Production_elec_gen_FERC

      # PART 3: THE STATES
      # Resetting the electric sector market to the grid regions (for backup calculations)
      # L2232.StubTechElecMarket_backup_USA_FERC: electric sector name
      L223.StubTechMarket_backup_USA %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]])) %>%
        left_join_error_no_match(select(states_subregions, electric.sector.market = grid_region, state),
                                 by = c("region" = "state")) ->
        L2232.StubTechElecMarket_backup_USA

    }

    # Produce outputs
    L2232.DeleteSupplysector_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.DeleteSupplysector_USAelec") ->
      L2232.DeleteSupplysector_USAelec

    L2232.Supplysector_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.Supplysector_USAelec") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.Supplysector_USAelec

    L2232.SubsectorShrwtFllt_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_USAelec") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.SubsectorShrwtFllt_USAelec

    L2232.SubsectorInterp_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorInterp_USAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_USAelec") ->
      L2232.SubsectorInterp_USAelec

    L2232.SubsectorLogit_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorLogit_USAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_USAelec") ->
      L2232.SubsectorLogit_USAelec

    L2232.TechShrwt_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.TechShrwt_USAelec") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.TechShrwt_USAelec

    L2232.TechCoef_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.TechCoef_USAelec") %>%
      same_precursors_as("L2232.TechShrwt_USAelec") ->
      L2232.TechCoef_USAelec

    L2232.Production_exports_USAelec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.Production_exports_USAelec") %>%
      same_precursors_as("L2232.TechShrwt_USAelec") %>%
      add_precursors("L123.in_EJ_state_ownuse_elec",
                     "L123.out_EJ_state_ownuse_elec",
                     "L126.in_EJ_state_td_elec",
                     "L132.out_EJ_state_indchp_F",
                     "L1232.out_EJ_sR_elec") ->
      L2232.Production_exports_USAelec

    L2232.Supplysector_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.Supplysector_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.Supplysector_elec_FERC

    L2232.ElecReserve_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.ElecReserve_FERC") %>%
      add_precursors("energy/A23.sector") ->
      L2232.ElecReserve_FERC

    L2232.SubsectorShrwtFllt_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.SubsectorShrwtFllt_elec_FERC

    L2232.SubsectorInterp_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorInterp_elec_FERC") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_FERC") ->
      L2232.SubsectorInterp_elec_FERC

    L2232.SubsectorLogit_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.SubsectorLogit_elec_FERC") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_FERC") ->
      L2232.SubsectorLogit_elec_FERC

    L2232.TechShrwt_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.TechShrwt_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.TechShrwt_elec_FERC

    L2232.TechCoef_elec_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.TechShrwt_elec_FERC") ->
      L2232.TechCoef_elec_FERC

    L2232.TechCoef_elecownuse_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.TechCoef_elecownuse_FERC") %>%
      same_precursors_as("L2232.TechShrwt_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") ->
      L2232.TechCoef_elecownuse_FERC

    L2232.Production_imports_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.Production_imports_FERC") %>%
      same_precursors_as("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") ->
      L2232.Production_imports_FERC

    L2232.Production_elec_gen_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.Production_elec_gen_FERC") %>%
      same_precursors_as("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") ->
      L2232.Production_elec_gen_FERC

    L2232.StubTechElecMarket_backup_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2232.StubTechElecMarket_backup_USA") %>%
      add_precursors("L223.StubTechMarket_backup_USA") ->
      L2232.StubTechElecMarket_backup_USA

    return_data(L2232.DeleteSupplysector_USAelec, L2232.Supplysector_USAelec, L2232.SubsectorShrwtFllt_USAelec,
                L2232.SubsectorInterp_USAelec, L2232.SubsectorLogit_USAelec, L2232.TechShrwt_USAelec,
                L2232.TechCoef_USAelec, L2232.Production_exports_USAelec, L2232.Supplysector_elec_FERC,
                L2232.ElecReserve_FERC, L2232.SubsectorShrwtFllt_elec_FERC, L2232.SubsectorInterp_elec_FERC,
                L2232.SubsectorLogit_elec_FERC, L2232.TechShrwt_elec_FERC, L2232.TechCoef_elec_FERC,
                L2232.TechCoef_elecownuse_FERC, L2232.Production_imports_FERC, L2232.Production_elec_gen_FERC,
                L2232.StubTechElecMarket_backup_USA)
  } else {
    stop("Unknown command")
  }
}
