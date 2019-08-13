#' module_gcamindia_L2232.electricity_GRIDR
#'
#' Generate GCAM-India model inputs for electrcity trade sectors at the level of grid regions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2232.DeleteSupplysector_INDIAelec}, \code{L2232.india_state_Supplysector_elec},
#' \code{L2232.india_grid_SubsectorShrwtFllt_elec}, \code{L2232.india_grid_SubsectorInterp_elec}, \code{L2232.india_grid_SubsectorLogit_elec},
#' \code{L2232.india_grid_TechShrwt_elec}, \code{L2232.india_grid_TechCoef_elec}, \code{L2232.india_grid_Production_exports_elec},
#' \code{L2232.india_grid_Supplysector_elec_CEA}, \code{L2232.india_grid_ElecReserve_CEA}, \code{L2232.india_grid_SubsectorShrwtFllt_elec_CEA},
#' \code{L2232.india_grid_SubsectorInterp_elec_CEA}, \code{L2232.india_grid_SubsectorLogit_elec_CEA}, \code{L2232.india_grid_TechShrwt_elec_CEA},
#' \code{L2232.india_grid_TechCoef_elec_CEA}, \code{L2232.india_grid_TechCoef_elecownuse_CEA}, \code{L2232.india_grid_Production_imports_CEA},
#' \code{L2232.india_grid_Production_elec_gen_CEA}, \code{L2232.india_grid_StubTechElecMarket_backup}. The corresponding file in the
#' original data system was \code{L2232.electricity_CEA_USA.R} (gcam-india level2).
#' @details This chunk generates input files to create electricity trade and passthrough sectors for the grid regions,
#' and balances electricity supply and demand for each grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK July 2019
module_gcamindia_L2232.electricity_GRIDR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "energy/A23.sector",
             FILE = "gcam-india/A232.structure_india",
             FILE = "gcam-india/A23.india_state_in_EJ_td_elec",
             "L123.india_state_in_EJ_ownuse_elec",
             "L123.india_state_out_EJ_ownuse_elec",
             "L1232.india_state_out_EJ_sR_elec",
             "L223.india_state_StubTechMarket_backup"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2232.DeleteSupplysector_INDIAelec",
             "L2232.india_state_Supplysector_elec",
             "L2232.india_grid_SubsectorShrwtFllt_elec",
             "L2232.india_grid_SubsectorInterp_elec",
             "L2232.india_grid_SubsectorLogit_elec",
             "L2232.india_grid_TechShrwt_elec",
             "L2232.india_grid_TechCoef_elec",
             "L2232.india_grid_Production_exports_elec",
             "L2232.india_grid_Supplysector_elec_CEA",
             "L2232.india_grid_ElecReserve_CEA",
             "L2232.india_grid_SubsectorShrwtFllt_elec_CEA",
             "L2232.india_grid_SubsectorInterp_elec_CEA",
             "L2232.india_grid_SubsectorLogit_elec_CEA",
             "L2232.india_grid_TechShrwt_elec_CEA",
             "L2232.india_grid_TechCoef_elec_CEA",
             "L2232.india_grid_TechCoef_elecownuse_CEA",
             "L2232.india_grid_Production_imports_CEA",
             "L2232.india_grid_Production_elec_gen_CEA",
             "L2232.india_grid_StubTechElecMarket_backup"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- state <- value <- year <- region <- supplysector <-
      calOutputValue <- coefficient <- cogeneration <- consumption <-
      generation <- imports <- in_ownuse <- market.name <- minicam.energy.input <-
      exports <- net.exports <- net.supply <- net_ownuse <- ownuse <- ownuse_coef <-
      subsector <- subsector.logit <- subsector.logit.type <- technology <-
      technology.logit <- technology.logit.type <- NULL  # silence package check notes

    # Load required inputs
    india_states_subregions         <- get_data(all_data, "gcam-india/india_states_subregions")
    A23.sector                     <- get_data(all_data, "energy/A23.sector")
    A232.structure_india           <- get_data(all_data, "gcam-india/A232.structure_india")
    L123.india_state_in_EJ_ownuse_elec   <- get_data(all_data, "L123.india_state_in_EJ_ownuse_elec")
    L123.india_state_out_EJ_ownuse_elec  <- get_data(all_data, "L123.india_state_out_EJ_ownuse_elec")
    A23.india_state_in_EJ_td_elec       <- get_data(all_data, "gcam-india/A23.india_state_in_EJ_td_elec")
    L1232.india_state_out_EJ_sR_elec     <- get_data(all_data, "L1232.india_state_out_EJ_sR_elec")
    L223.india_state_StubTechMarket_backup <- get_data(all_data, "L223.india_state_StubTechMarket_backup")

    # This chunk builds the electric sector model with demand resolved at the grid region level.

    # A vector of INDIA grid region names
    grid_regions <- india_states_subregions$grid_region %>%
      unique %>%
      sort

    # PART 1: THE INDIA REGION
    # L2232.DeleteSupplysector_INDIAelec: Remove the electricity sectors of the INDIA region (incl. net_ownuse)
    # Remove the INDIA electricity sector, and replace with electricity trade
    tibble(region = gcam.india_REGION,
           supplysector = c("electricity", "electricity_net_ownuse")) ->
      L2232.DeleteSupplysector_INDIAelec

    # L2232.india_state_Supplysector_elec: supplysector for electricity trade sector in the INDIA region,
    # including logit exponent between grid regions
    # All of the supplysector information is the same as before, except the logit exponent
    A232.structure_india %>%
      filter(region == gcam.india_REGION) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.india_state_Supplysector_elec

    # L2232.india_grid_SubsectorShrwtFllt_elec: subsector (grid region) share-weights in india electricity trade
    # No need to read in subsector logit exponents, which are applied to the technology competition
    A232.structure_india %>%
      filter(region == gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(grid_region = grid_regions)) %>%
      mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                 paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L2232.india_grid_SubsectorShrwtFllt_elec

    # L2232.india_grid_SubsectorInterp_elec: temporal interpolation of subsector share-weights in India electricity trade
    # NOTE: this just carries the base year share-weights forward;
    # regions that don't export in the base year don't export at all
    L2232.india_grid_SubsectorShrwtFllt_elec %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.india_grid_SubsectorInterp_elec

    # L2232.india_grid_SubsectorLogit_elec: logit exponent of subsector in INDIA electricity trade
    # NOTE: There is only one tech per subsector in the CEA markets so the logit choice does not matter
    L2232.india_grid_SubsectorShrwtFllt_elec %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join(select(A232.structure_india, region,
                       logit.exponent = technology.logit,
                       logit.type = technology.logit.type),
                by = "region") %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.india_grid_SubsectorLogit_elec

    # L2232.india_grid_TechShrwt_elec: technology share-weights in INDIA electricity trade
    L2232.india_grid_TechShrwt_elec <- A232.structure_india %>%
      filter(region == gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(grid_region = grid_regions)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                 paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
             technology = replace(technology, grepl("grid_region", technology),
                                  paste(grid_region[grepl("grid_region", technology)], "electricity trade", sep = " ")),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight", "grid_region")


    # L2232.india_grid_TechCoef_elec: technology coefficients and market names in India electricity trade
    L2232.india_grid_TechCoef_elec <- L2232.india_grid_TechShrwt_elec %>%
      left_join_error_no_match(select(A232.structure_india, region, minicam.energy.input), by = "region") %>%
      mutate(coefficient = 1, market.name = grid_region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])


    # Compile flows of electricity in each CEA region:
    # generation, cogeneration, ownuse, and consumption by all sectors
    # to calculate exports, imports, and net supply

    # Generation by grid region
    L1232.india_state_out_EJ_sR_elec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(grid_region, year, generation = value) ->
      L2232.india_grid_out_EJ_sR_elec

   # Calculate net own use in each grid region
    L2232.india_grid_net_EJ_sR_ownuse_elec <- L123.india_state_in_EJ_ownuse_elec %>%
      rename(in_ownuse = value) %>%
      left_join_error_no_match(L123.india_state_out_EJ_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
      # Net own use is calculated as total generation minus net outputs
      mutate(net_ownuse = in_ownuse - value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(india_states_subregions, state, grid_region), by = "state") %>%
      group_by(grid_region, year) %>%
      summarise(ownuse = sum(net_ownuse)) %>%
      ungroup


    # Comsumption: the sum of all demands in each CEA region, equal to the input to the elect_td sectors
    A23.india_state_in_EJ_td_elec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(india_states_subregions, state, grid_region), by = "state") %>%
      group_by(grid_region, year) %>%
      summarise(consumption = sum(value)) %>%
      ungroup ->
      L2232.india_grid_in_EJ_sR_td_elec

    # Complie all flows and calculate exports, imports and net supply
    L2232.india_grid_elec_flows_CEA <- L2232.india_grid_TechShrwt_elec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2232.india_grid_out_EJ_sR_elec, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.india_grid_net_EJ_sR_ownuse_elec, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.india_grid_in_EJ_sR_td_elec, by = c("grid_region", "year")) %>%
      # Calculate net exports: generation + cogeneration - ownuse - consumption
      mutate(net.exports = generation - ownuse - consumption,
             # Split net exports into gross imports and exports:
             # When net exports are positive, exports equal net exports, and imports are zero;
             # When net exports are negative, imports equal minus net exports, and exports are zero
             imports = pmax(0, -1 * net.exports),
             exports = pmax(0, net.exports),
             # Calculate consumption from domestic sources: total consumption minus gross imports
             net.supply = consumption - imports)


    # L2232.india_grid_Production_exports_elec: calibrated exports of electricity from grid regions to shared India region
    L2232.india_grid_Production_exports_elec <- L2232.india_grid_elec_flows_CEA %>%
      mutate(calOutputValue = round(exports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]])



    # PART 2: THE CEA REGIONS
    # Some of the information read in about these regions is in the primary electricity_India code file

    # Create the CEA region structure tibble
    A232.structure_india %>%
      filter(region == "grid_region") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = grid_regions)) %>%
      mutate(market.name = replace(market.name, grepl("grid_region", market.name),
                                   region[grepl("grid_region", market.name)])) ->
      A232.india_grid_CEAstructure

    # L2232.Supplysector_elec_CEA: supplysector information for electricity passthrough sectors in the CEA regions
    A232.india_grid_CEAstructure %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.india_grid_Supplysector_elec_CEA

    # L2232.ElecReserve_CEA: electricity reserve margin and avg grid capacity factor in the grid regions
    A23.sector %>%
      filter(supplysector == "electricity") %>%
      repeat_add_columns(tibble(region = grid_regions)) %>%
      select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
      L2232.india_grid_ElecReserve_CEA

    # L2232.india_grid_SubsectorShrwtFllt_elec_CEA: subsector (states) share-weights
    # for electricity passthrough sectors in grid regions
    A232.india_grid_CEAstructure %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS), share.weight = 1) ->
      L2232.india_grid_SubsectorShrwtFllt_elec_CEA

    # L2232.india_grid_SubsectorInterp_elec_CEA: temporal interpolation of subsector (states) share-weights
    # for electricity passthrough sectors in grid regions
    L2232.india_grid_SubsectorShrwtFllt_elec_CEA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.india_grid_SubsectorInterp_elec_CEA

    # L2232.india_grid_SubsectorShrwtFllt_elec_CEA: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector in the CEA markets so the logit choice does not matter
    L2232.india_grid_SubsectorShrwtFllt_elec_CEA %>%
      left_join(A232.india_grid_CEAstructure %>%
                  select(region, technology.logit, technology.logit.type) %>%
                  unique, by = "region") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = technology.logit,
             logit.type = technology.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.india_grid_SubsectorLogit_elec_CEA

    # L2232.TechShrwt_elec_CEA: technology share-weights in grid regions
    A232.india_grid_CEAstructure %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2232.india_grid_TechShrwt_elec_CEA

    # L2232.TechCoef_elec_CEA: technology coefficients and market names for domestic supply in grid regions
    A232.india_grid_CEAstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Own use coefficients will be done separately; delete from the table here
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.india_grid_TechCoef_elec_CEA

    # L2232.TechCoef_elecownuse_CEA: own use coefficients in the grid regions
    L2232.india_grid_elec_flows_CEA %>%
      # Own use coefficients are total generation divided by total generation minus own use
      mutate(ownuse_coef = (generation) / (generation - ownuse)) ->
      L2232.india_grid_elec_flows_CEA

    L2232.india_grid_TechCoef_elecownuse_CEA <-
      A232.india_grid_CEAstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector == "electricity_net_ownuse") %>%
      left_join(select(L2232.india_grid_elec_flows_CEA, grid_region, year, coefficient = ownuse_coef),
                by = c("region" = "grid_region", "year")) %>%
      group_by(region) %>%
      # Set future year own use coefficients the same as the base year coefficients
      mutate(coefficient = replace(coefficient, year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)])) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    L2232.india_grid_TechCoef_elecownuse_CEA$coefficient[which(L2232.india_grid_TechCoef_elecownuse_CEA$coefficient == 0)] <- 1
    L2232.india_grid_TechCoef_elecownuse_CEA[is.na(L2232.india_grid_TechCoef_elecownuse_CEA)] <- 1

    # L2232.Production_imports_CEA: calibrated electricity imports (from INDIA region)
    L2232.india_grid_TechCoef_elec_CEA %>%
      filter(year %in% MODEL_BASE_YEARS, market.name == gcam.india_REGION) %>%
      left_join_error_no_match(select(L2232.india_grid_elec_flows_CEA, grid_region, year, imports),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(imports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.india_grid_Production_imports_CEA

    # L2232.india_grid_Production_elec_gen_CEA: calibrated net electricity generation (from within grid region)
    L2232.india_grid_TechCoef_elec_CEA %>%
      filter(year %in% MODEL_BASE_YEARS, market.name != gcam.india_REGION) %>%
      left_join_error_no_match(select(L2232.india_grid_elec_flows_CEA, grid_region, year, net.supply),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(net.supply, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.india_grid_Production_elec_gen_CEA

    # PART 3: THE STATES
    # L2232.india_grid_StubTechElecMarket_backup_CEA: electric sector name for states
    # Reset the electric sector market to the grid regions (for backup calculations)
    L223.india_state_StubTechMarket_backup %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
      left_join_error_no_match(select(india_states_subregions, electric.sector.market = grid_region, state),
                               by = c("region" = "state")) ->
      L2232.india_grid_StubTechElecMarket_backup


    # Produce outputs
    L2232.DeleteSupplysector_INDIAelec %>%
      add_title("Remove the electricity and net ownuse sectors of the INDIA region") %>%
      add_units("Unitless") %>%
      add_comments("Remove the INDIA electricity supply sectors, and replace with electricity trade") %>%
      add_legacy_name("L2232.DeleteSupplysector_INDIAelec") ->
      L2232.DeleteSupplysector_INDIAelec

    L2232.india_state_Supplysector_elec %>%
      add_title("Supplysector for electricity sector in the INDIA region") %>%
      add_units("Unitless") %>%
      add_comments("All of the supplysector information is the same as before") %>%
      add_comments("except including logit exponent between grid regions") %>%
      add_legacy_name("L2232.india_state_Supplysector_elec") %>%
      add_precursors("gcam-india/A232.structure_india") ->
      L2232.india_state_Supplysector_elec

    L2232.india_grid_SubsectorShrwtFllt_elec %>%
      add_title("Subsector (grid region) share-weights in INDIA electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("No need to read in subsector logit exponents, which are applied to the technology competition") %>%
      add_legacy_name("L2232.india_grid_SubsectorShrwtFllt_elec") %>%
      add_precursors("gcam-india/india_states_subregions",
                     "gcam-india/A232.structure_india") ->
      L2232.india_grid_SubsectorShrwtFllt_elec

    L2232.india_grid_SubsectorInterp_elec %>%
      add_title("Table headers for temporal interpolation of subsector (grid region) share-weights in India electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("This just carries the base year share-weights forward") %>%
      add_comments("Regions that don't export in the base year don't export at all") %>%
      add_legacy_name("L2232.india_grid_SubsectorInterp_elec") %>%
      same_precursors_as("L2232.india_grid_SubsectorShrwtFllt_elec") ->
      L2232.india_grid_SubsectorInterp_elec

    L2232.india_grid_SubsectorLogit_elec %>%
      add_title("Logit exponent of subsector (grid region) in INDIA electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L2232.india_grid_SubsectorLogit_elec") %>%
      same_precursors_as("L2232.india_grid_SubsectorShrwtFllt_elec") ->
      L2232.india_grid_SubsectorLogit_elec

    L2232.india_grid_TechShrwt_elec %>%
      add_title("Technology share-weights in the INDIA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("Set the same value across all model years") %>%
      add_legacy_name("L2232.india_grid_TechShrwt_elec") %>%
      add_precursors("gcam-india/A232.structure_india",
                     "gcam-india/india_states_subregions") ->
      L2232.india_grid_TechShrwt_elec

    L2232.india_grid_TechCoef_elec %>%
      add_title("Technology coefficients and market names in the India electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("Set the same value across all model years") %>%
      add_comments("Set grid region as market name") %>%
      add_legacy_name("L2232.india_grid_TechCoef_elec") %>%
      same_precursors_as("L2232.india_grid_TechShrwt_elec") ->
      L2232.india_grid_TechCoef_elec

    L2232.india_grid_Production_exports_elec %>%
      add_title("Calibrated exports of electricity from grid regions to shared India region") %>%
      add_units("EJ") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("When net exports are positive, exports equal net exports; when negative, exports are zero") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.india_grid_Production_exports_elec") %>%
      same_precursors_as("L2232.india_grid_TechShrwt_elec") %>%
      add_precursors("L123.india_state_in_EJ_ownuse_elec",
                     "L123.india_state_out_EJ_ownuse_elec",
                     "gcam-india/A23.india_state_in_EJ_td_elec",
                     "L1232.india_state_out_EJ_sR_elec") ->
      L2232.india_grid_Production_exports_elec

    L2232.india_grid_Supplysector_elec_CEA %>%
      add_title("Supplysector information for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.india_grid_Supplysector_elec_CEA") %>%
      add_precursors("gcam-india/A232.structure_india",
                     "gcam-india/india_states_subregions") ->
      L2232.india_grid_Supplysector_elec_CEA

    L2232.india_grid_ElecReserve_CEA %>%
      add_title("Electricity reserve margin and avg grid capacity factor in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("For electricity sector") %>%
      add_legacy_name("L2232.india_grid_ElecReserve_CEA") %>%
      add_precursors("energy/A23.sector",
                     "gcam-india/india_states_subregions") ->
      L2232.india_grid_ElecReserve_CEA

    L2232.india_grid_SubsectorShrwtFllt_elec_CEA %>%
      add_title("Subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.india_grid_SubsectorShrwtFllt_elec_CEA") %>%
      add_precursors("gcam-india/A232.structure_india",
                     "gcam-india/india_states_subregions") ->
      L2232.india_grid_SubsectorShrwtFllt_elec_CEA

    L2232.india_grid_SubsectorInterp_elec_CEA %>%
      add_title("Table header of temporal interpolation of subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.india_grid_SubsectorInterp_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_SubsectorShrwtFllt_elec_CEA") ->
      L2232.india_grid_SubsectorInterp_elec_CEA

    L2232.india_grid_SubsectorLogit_elec_CEA %>%
      add_title("Logit exponent of subsector (states) for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.india_grid_SubsectorLogit_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_SubsectorShrwtFllt_elec_CEA") ->
      L2232.india_grid_SubsectorLogit_elec_CEA

    L2232.india_grid_TechShrwt_elec_CEA %>%
      add_title("Technology share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.india_grid_TechShrwt_elec_CEA") %>%
      add_precursors("gcam-india/A232.structure_india",
                     "gcam-india/india_states_subregions") ->
      L2232.india_grid_TechShrwt_elec_CEA

    L2232.india_grid_TechCoef_elec_CEA %>%
      add_title("Technology coefficients and market names for electricity domestic supply in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients for electricity domestic supply") %>%
      add_legacy_name("L2232.india_grid_TechCoef_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_TechShrwt_elec_CEA") ->
      L2232.india_grid_TechCoef_elec_CEA

    L2232.india_grid_TechCoef_elecownuse_CEA %>%
      add_title("Electricity own use coefficients in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated as total generation devided by total generation minus own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Set the coefficients for future years the same as in the model base year") %>%
      add_legacy_name("L2232.india_grid_TechCoef_elecownuse_CEA") %>%
      same_precursors_as("L2232.india_grid_TechShrwt_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_Production_exports_elec") ->
      L2232.india_grid_TechCoef_elecownuse_CEA

    L2232.india_grid_Production_imports_CEA %>%
      add_title("Calibrated electricity imports (from other grid regions)") %>%
      add_units("EJ") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.india_grid_Production_imports_CEA") %>%
      same_precursors_as("L2232.india_grid_TechCoef_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_Production_exports_elec") ->
      L2232.india_grid_Production_imports_CEA

    L2232.india_grid_Production_elec_gen_CEA %>%
      add_title("Calibrated net electricity generation (from within grid region)") %>%
      add_units("EJ") %>%
      add_comments("Net electricity generation is calculated as total consumption minus imports") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_legacy_name("L2232.india_grid_Production_elec_gen_CEA") %>%
      same_precursors_as("L2232.india_grid_TechCoef_elec_CEA") %>%
      same_precursors_as("L2232.india_grid_Production_exports_elec") ->
      L2232.india_grid_Production_elec_gen_CEA

    L2232.india_grid_StubTechElecMarket_backup %>%
      add_title("Electric sector name for states") %>%
      add_units("Unitless") %>%
      add_comments("Reset the electric sector market to the grid regions (for backup calculations)") %>%
      add_legacy_name("L2232.india_grid_StubTechElecMarket_backup") %>%
      add_precursors("L223.india_state_StubTechMarket_backup") ->
      L2232.india_grid_StubTechElecMarket_backup

    return_data(L2232.DeleteSupplysector_INDIAelec, L2232.india_state_Supplysector_elec, L2232.india_grid_SubsectorShrwtFllt_elec,
                L2232.india_grid_SubsectorInterp_elec, L2232.india_grid_SubsectorLogit_elec, L2232.india_grid_TechShrwt_elec,
                L2232.india_grid_TechCoef_elec, L2232.india_grid_Production_exports_elec, L2232.india_grid_Supplysector_elec_CEA,
                L2232.india_grid_ElecReserve_CEA, L2232.india_grid_SubsectorShrwtFllt_elec_CEA, L2232.india_grid_SubsectorInterp_elec_CEA,
                L2232.india_grid_SubsectorLogit_elec_CEA, L2232.india_grid_TechShrwt_elec_CEA, L2232.india_grid_TechCoef_elec_CEA,
                L2232.india_grid_TechCoef_elecownuse_CEA, L2232.india_grid_Production_imports_CEA, L2232.india_grid_Production_elec_gen_CEA,
                L2232.india_grid_StubTechElecMarket_backup)
  } else {
    stop("Unknown command")
  }
}
