#' module_gcam.usa_L2232.electricity_FERC_USA
#'
#' Generate GCAM-USA model inputs for electrcity trade sectors at the level of grid regions.
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
#' @details This chunk generates input files to create electricity trade and passthrough sectors for the grid regions,
#' and balances electricity supply and demand for each grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Oct 2017
module_gcam.usa_L2232.electricity_FERC_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/A23.sector",
             FILE = "gcam-usa/A232.structure",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec",
             "L126.in_EJ_state_td_elec",
             "L132.out_EJ_state_indchp_F",
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

    grid_region <- state <- value <- year <- region <- supplysector <-
      calOutputValue <- coefficient <- cogeneration <- consumption <-
      generation <- imports <- in_ownuse <- market.name <- minicam.energy.input <-
      exports <- net.exports <- net.supply <- net_ownuse <- ownuse <- ownuse_coef <-
      subsector <- subsector.logit <- subsector.logit.type <- technology <-
      technology.logit <- technology.logit.type <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.sector <- get_data(all_data, "energy/A23.sector")
    A232.structure <- get_data(all_data, "gcam-usa/A232.structure")
    L123.in_EJ_state_ownuse_elec <- get_data(all_data, "L123.in_EJ_state_ownuse_elec")
    L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec")
    L126.in_EJ_state_td_elec <- get_data(all_data, "L126.in_EJ_state_td_elec")
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")
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
      # L2232.DeleteSupplysector_USAelec: Remove the electricity sectors of the USA region (incl. net_ownuse)
      # Remove the USA electricity sector, and replace with electricity trade
      tibble(region = "USA",
             supplysector = c("electricity", "electricity_net_ownuse")) ->
        L2232.DeleteSupplysector_USAelec

      # L2232.Supplysector_USAelec: supplysector for electricity trade sector in the USA region,
      # including logit exponent between grid regions
      # All of the supplysector information is the same as before, except the logit exponent
      A232.structure %>%
        filter(region == "USA") %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = subsector.logit,
               logit.type = subsector.logit.type) %>%
        select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type")) ->
        L2232.Supplysector_USAelec

      # L2232.SubsectorShrwtFllt_USAelec: subsector (grid region) share-weights in USA electricity trade
      # No need to read in subsector logit exponents, which are applied to the technology competition
      A232.structure %>%
        filter(region == "USA") %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        repeat_add_columns(tibble(grid_region = grid_regions)) %>%
        mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                   paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
               year.fillout = min(BASE_YEARS),
               share.weight = 1) %>%
        select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
        L2232.SubsectorShrwtFllt_USAelec

      # L2232.SubsectorInterp_USAelec: temporal interpolation of subsector share-weights in USA electricity trade
      # NOTE: this just carries the base year share-weights forward;
      # regions that don't export in the base year don't export at all
      L2232.SubsectorShrwtFllt_USAelec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(apply.to = "share-weight",
               from.year = max(BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L2232.SubsectorInterp_USAelec

      # L2232.SubsectorLogit_USAelec: logit exponent of subsector in USA electricity trade
      # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
      L2232.SubsectorShrwtFllt_USAelec %>%
        mutate(logit.year.fillout = min(BASE_YEARS)) %>%
        left_join(select(A232.structure, region,
                         logit.exponent = technology.logit,
                         logit.type = technology.logit.type),
                  by = "region") %>%
        select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type")) ->
        L2232.SubsectorLogit_USAelec

      # L2232.TechShrwt_USAelec: technology share-weights in USA electricity trade
      A232.structure %>%
        filter(region == "USA") %>%
        select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
        repeat_add_columns(tibble(grid_region = grid_regions)) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                   paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
               technology = replace(technology, grepl("grid_region", technology),
                                    paste(grid_region[grepl("grid_region", technology)], "electricity trade", sep = " ")),
               share.weight = 1) %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]],"share.weight", "grid_region") ->
        L2232.TechShrwt_USAelec

      # L2232.TechCoef_USAelec: technology coefficients and market names in USA electricity trade
      L2232.TechShrwt_USAelec %>%
        left_join_error_no_match(select(A232.structure, region, minicam.energy.input), by = "region") %>%
        mutate(coefficient = 1, market.name = grid_region) %>%
        select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
        L2232.TechCoef_USAelec

      # Compile flows of electricity in each FERC region:
      # generation, cogeneration, ownuse, and consumption by all sectors
      # to calculate exports, imports, and net supply

      # Generation by grid region
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

      # Calculate net own use in each grid region
      L123.in_EJ_state_ownuse_elec %>%
        rename(in_ownuse = value) %>%
        left_join_error_no_match(L123.out_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
        # Net own use is calculated as total generation minus net outputs
        mutate(net_ownuse = in_ownuse - value) %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
        group_by(grid_region, year) %>%
        summarise(ownuse = sum(net_ownuse)) %>%
        ungroup ->
        L2232.net_EJ_sR_ownuse_elec

      # Comsumption: the sum of all demands in each FERC region, equal to the input to the elect_td sectors
      L126.in_EJ_state_td_elec %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
        group_by(grid_region, year) %>%
        summarise(consumption = sum(value)) %>%
        ungroup ->
        L2232.in_EJ_sR_td_elec

      # Complie all flows and calculate exports, imports and net supply
      L2232.TechShrwt_USAelec %>%
        filter(year %in% BASE_YEARS) %>%
        left_join_error_no_match(L2232.out_EJ_sR_elec, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.out_EJ_sR_indchp_F, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.net_EJ_sR_ownuse_elec, by = c("grid_region", "year")) %>%
        left_join_error_no_match(L2232.in_EJ_sR_td_elec, by = c("grid_region", "year")) %>%
        # Calculate net exports: generation + cogeneration - ownuse - consumption
        mutate(net.exports = generation + cogeneration - ownuse - consumption,
               # Split net exports into gross imports and exports:
               # When net exports are positive, exports equal net exports, and imports are zero;
               # When net exports are negative, imports equal minus net exports, and exports are zero
               imports = pmax(0, -1 * net.exports),
               exports = pmax(0, net.exports),
               # Calculate consumption from domestic sources: total consumption minus gross imports
               net.supply = consumption - imports) ->
        L2232.elec_flows_FERC

      # L2232.Production_exports_USAelec: calibrated exports of electricity from grid regions to shared USA region
      L2232.elec_flows_FERC %>%
        mutate(calOutputValue = round(exports, digits = energy.DIGITS_CALOUTPUT),
               share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(LEVEL2_DATA_NAMES[["Production"]]) ->
        L2232.Production_exports_USAelec


      # PART 2: THE FERC REGIONS
      # Some of the information read in about these regions is in the primary electricity_USA code file

      # Create the FERC region structure tibble
      A232.structure %>%
        filter(region == "grid_region") %>%
        select(-region) %>%
        repeat_add_columns(tibble(region = grid_regions)) %>%
        mutate(market.name = replace(market.name, grepl("grid_region", market.name),
                                     region[grepl("grid_region", market.name)])) ->
        A232.FERCstructure

      # L2232.Supplysector_elec_FERC: supplysector information for electricity passthrough sectors in the FERC regions
      A232.FERCstructure %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = subsector.logit,
               logit.type = subsector.logit.type) %>%
        select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type")) ->
        L2232.Supplysector_elec_FERC

      # L2232.ElecReserve_FERC: electricity reserve margin and avg grid capacity factor in the grid regions
      A23.sector %>%
        filter(supplysector == "electricity") %>%
        repeat_add_columns(tibble(region = grid_regions)) %>%
        select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
        L2232.ElecReserve_FERC

      # L2232.SubsectorShrwtFllt_elec_FERC: subsector (states) share-weights
      # for electricity passthrough sectors in grid regions
      A232.FERCstructure %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(year.fillout = min(BASE_YEARS), share.weight = 1) ->
        L2232.SubsectorShrwtFllt_elec_FERC

      # L2232.SubsectorInterp_elec_FERC: temporal interpolation of subsector (states) share-weights
      # for electricity passthrough sectors in grid regions
      L2232.SubsectorShrwtFllt_elec_FERC %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(apply.to = "share-weight",
               from.year = max(BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L2232.SubsectorInterp_elec_FERC

      # L2232.SubsectorShrwtFllt_elec_FERC: logit exponent of subsector (states) in grid regions
      # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
      L2232.SubsectorShrwtFllt_elec_FERC %>%
        left_join(A232.FERCstructure %>%
                    select(region, technology.logit, technology.logit.type) %>%
                    unique, by = "region") %>%
        mutate(logit.year.fillout = min(BASE_YEARS),
               logit.exponent = technology.logit,
               logit.type = technology.logit.type) %>%
        select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type")) ->
        L2232.SubsectorLogit_elec_FERC

      # L2232.TechShrwt_elec_FERC: technology share-weights in grid regions
      A232.FERCstructure %>%
        select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(share.weight = 1) ->
        L2232.TechShrwt_elec_FERC

      # L2232.TechCoef_elec_FERC: technology coefficients and market names for domestic supply in grid regions
      A232.FERCstructure %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        # Own use coefficients will be done separately; delete from the table here
        filter(supplysector != "electricity_net_ownuse") %>%
        mutate(coefficient = 1) %>%
        select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
        L2232.TechCoef_elec_FERC

      # L2232.TechCoef_elecownuse_FERC: own use coefficients in the grid regions
      L2232.elec_flows_FERC %>%
        # Own use coefficients are total generation divided by total generation minus own use
        mutate(ownuse_coef = (generation + cogeneration) / (generation + cogeneration - ownuse)) ->
        L2232.elec_flows_FERC

      A232.FERCstructure %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        filter(supplysector == "electricity_net_ownuse") %>%
        left_join(select(L2232.elec_flows_FERC, grid_region, year, coefficient = ownuse_coef),
                  by = c("region" = "grid_region", "year")) %>%
        group_by(region) %>%
        # Set future year own use coefficients the same as the base year coefficients
        mutate(coefficient = replace(coefficient, year %in% FUTURE_YEARS, coefficient[year == max(BASE_YEARS)])) %>%
        ungroup %>%
        select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
        L2232.TechCoef_elecownuse_FERC

      # L2232.Production_imports_FERC: calibrated electricity imports (from USA region)
      L2232.TechCoef_elec_FERC %>%
        filter(year %in% BASE_YEARS, market.name == "USA") %>%
        left_join_error_no_match(select(L2232.elec_flows_FERC, grid_region, year, imports),
                                 by = c("region" = "grid_region", "year")) %>%
        mutate(calOutputValue = round(imports, digits = energy.DIGITS_CALOUTPUT),
               share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(LEVEL2_DATA_NAMES[["Production"]]) ->
        L2232.Production_imports_FERC

      # L2232.Production_elec_gen_FERC: calibrated net electricity generation (from within grid region)
      L2232.TechCoef_elec_FERC %>%
        filter(year %in% BASE_YEARS, market.name != "USA") %>%
        left_join_error_no_match(select(L2232.elec_flows_FERC, grid_region, year, net.supply),
                                 by = c("region" = "grid_region", "year")) %>%
        mutate(calOutputValue = round(net.supply, digits = energy.DIGITS_CALOUTPUT),
               share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(LEVEL2_DATA_NAMES[["Production"]]) ->
        L2232.Production_elec_gen_FERC

      # PART 3: THE STATES
      # L2232.StubTechElecMarket_backup_USA_FERC: electric sector name for states
      # Reset the electric sector market to the grid regions (for backup calculations)
      L223.StubTechMarket_backup_USA %>%
        select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
        left_join_error_no_match(select(states_subregions, electric.sector.market = grid_region, state),
                                 by = c("region" = "state")) ->
        L2232.StubTechElecMarket_backup_USA

    }

    # Produce outputs
    L2232.DeleteSupplysector_USAelec %>%
      add_title("Remove the electricity and net ownuse sectors of the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the USA electricity supply sectors, and replace with electricity trade") %>%
      add_legacy_name("L2232.DeleteSupplysector_USAelec") ->
      L2232.DeleteSupplysector_USAelec

    L2232.Supplysector_USAelec %>%
      add_title("Supplysector for electricity sector in the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("All of the supplysector information is the same as before") %>%
      add_comments("except including logit exponent between grid regions") %>%
      add_legacy_name("L2232.Supplysector_USAelec") %>%
      add_precursors("gcam-usa/A232.structure") ->
      L2232.Supplysector_USAelec

    L2232.SubsectorShrwtFllt_USAelec %>%
      add_title("Subsector (grid region) share-weights in USA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("No need to read in subsector logit exponents, which are applied to the technology competition") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_USAelec") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A232.structure") ->
      L2232.SubsectorShrwtFllt_USAelec

    L2232.SubsectorInterp_USAelec %>%
      add_title("Table headers for temporal interpolation of subsector (grid region) share-weights in USA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("This just carries the base year share-weights forward") %>%
      add_comments("Regions that don't export in the base year don't export at all") %>%
      add_legacy_name("L2232.SubsectorInterp_USAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_USAelec") ->
      L2232.SubsectorInterp_USAelec

    L2232.SubsectorLogit_USAelec %>%
      add_title("Logit exponent of subsector (grid region) in USA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L2232.SubsectorLogit_USAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_USAelec") ->
      L2232.SubsectorLogit_USAelec

    L2232.TechShrwt_USAelec %>%
      add_title("Technology share-weights in the USA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("Set the same value across all model years") %>%
      add_legacy_name("L2232.TechShrwt_USAelec") %>%
      add_precursors("gcam-usa/A232.structure",
                     "gcam-usa/states_subregions") ->
      L2232.TechShrwt_USAelec

    L2232.TechCoef_USAelec %>%
      add_title("Technology coefficients and market names in the USA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("Set the same value across all model years") %>%
      add_comments("Set grid region as market name") %>%
      add_legacy_name("L2232.TechCoef_USAelec") %>%
      same_precursors_as("L2232.TechShrwt_USAelec") ->
      L2232.TechCoef_USAelec

    L2232.Production_exports_USAelec %>%
      add_title("Calibrated exports of electricity from grid regions to shared USA region") %>%
      add_units("EJ") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("When net exports are positive, exports equal net exports; when negative, exports are zero") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.Production_exports_USAelec") %>%
      same_precursors_as("L2232.TechShrwt_USAelec") %>%
      add_precursors("L123.in_EJ_state_ownuse_elec",
                     "L123.out_EJ_state_ownuse_elec",
                     "L126.in_EJ_state_td_elec",
                     "L132.out_EJ_state_indchp_F",
                     "L1232.out_EJ_sR_elec") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2232.Production_exports_USAelec

    L2232.Supplysector_elec_FERC %>%
      add_title("Supplysector information for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.Supplysector_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure",
                     "gcam-usa/states_subregions") ->
      L2232.Supplysector_elec_FERC

    L2232.ElecReserve_FERC %>%
      add_title("Electricity reserve margin and avg grid capacity factor in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("For electricity sector") %>%
      add_legacy_name("L2232.ElecReserve_FERC") %>%
      add_precursors("energy/A23.sector",
                     "gcam-usa/states_subregions") ->
      L2232.ElecReserve_FERC

    L2232.SubsectorShrwtFllt_elec_FERC %>%
      add_title("Subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure",
                     "gcam-usa/states_subregions") ->
      L2232.SubsectorShrwtFllt_elec_FERC

    L2232.SubsectorInterp_elec_FERC %>%
      add_title("Table header of temporal interpolation of subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorInterp_elec_FERC") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_FERC") ->
      L2232.SubsectorInterp_elec_FERC

    L2232.SubsectorLogit_elec_FERC %>%
      add_title("Logit exponent of subsector (states) for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorLogit_elec_FERC") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_FERC") ->
      L2232.SubsectorLogit_elec_FERC

    L2232.TechShrwt_elec_FERC %>%
      add_title("Technology share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.TechShrwt_elec_FERC") %>%
      add_precursors("gcam-usa/A232.structure",
                     "gcam-usa/states_subregions") ->
      L2232.TechShrwt_elec_FERC

    L2232.TechCoef_elec_FERC %>%
      add_title("Technology coefficients and market names for electricity domestic supply in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients for electricity domestic supply") %>%
      add_legacy_name("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.TechShrwt_elec_FERC") ->
      L2232.TechCoef_elec_FERC

    L2232.TechCoef_elecownuse_FERC %>%
      add_title("Electricity own use coefficients in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated as total generation devided by total generation minus own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Set the coefficients for future years the same as in the model base year") %>%
      add_legacy_name("L2232.TechCoef_elecownuse_FERC") %>%
      same_precursors_as("L2232.TechShrwt_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") ->
      L2232.TechCoef_elecownuse_FERC

    L2232.Production_imports_FERC %>%
      add_title("Calibrated electricity imports (from other grid regions)") %>%
      add_units("EJ") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.Production_imports_FERC") %>%
      same_precursors_as("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2232.Production_imports_FERC

    L2232.Production_elec_gen_FERC %>%
      add_title("Calibrated net electricity generation (from within grid region)") %>%
      add_units("EJ") %>%
      add_comments("Net electricity generation is calculated as total consumption minus imports") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_legacy_name("L2232.Production_elec_gen_FERC") %>%
      same_precursors_as("L2232.TechCoef_elec_FERC") %>%
      same_precursors_as("L2232.Production_exports_USAelec") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2232.Production_elec_gen_FERC

    L2232.StubTechElecMarket_backup_USA %>%
      add_title("Electric sector name for states") %>%
      add_units("Unitless") %>%
      add_comments("Reset the electric sector market to the grid regions (for backup calculations)") %>%
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
