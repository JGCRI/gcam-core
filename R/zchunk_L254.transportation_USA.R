#' module_gcam.usa_L254.transportation_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.DeleteSupplysector_USAtrn}, \code{L254.DeleteFinalDemand_USAtrn},
#' \code{L254.Supplysector_trn_USA}, \code{L254.FinalEnergyKeyword_trn_USA}, \code{L254.tranSubsectorLogit_USA},
#' \code{L254.tranSubsectorShrwtFllt_USA}, \code{L254.tranSubsectorInterp_USA}, \code{L254.tranSubsectorSpeed_USA},
#' \code{L254.tranSubsectorSpeed_passthru_USA}, \code{L254.tranSubsectorSpeed_noVOTT_USA},
#' \code{L254.tranSubsectorSpeed_nonmotor_USA}, \code{L254.tranSubsectorVOTT_USA}, \code{L254.tranSubsectorFuelPref_USA},
#' \code{L254.StubTranTech_USA}, \code{L254.StubTranTech_passthru_USA}, \code{L254.StubTranTech_nonmotor_USA},
#' \code{L254.StubTranTechLoadFactor_USA}, \code{L254.StubTranTechCost_USA}, \code{L254.StubTranTechCoef_USA},
#' \code{L254.PerCapitaBased_trn_USA}, \code{L254.PriceElasticity_trn_USA}, \code{L254.IncomeElasticity_trn_USA},
#' \code{L254.StubTranTechCalInput_USA}, \code{L254.StubTranTechProd_nonmotor_USA}, \code{L254.StubTranTechCalInput_passthru_USA},
#' \code{L254.BaseService_trn_USA}. The corresponding file in the
#' original data system was \code{L254.transportation_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L254.transportation_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/UCD_techs",
             FILE = "energy/A54.globaltech_nonmotor",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.sector",
             FILE = "gcam-usa/states_subregions",
             "L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L154.in_EJ_state_trn_m_sz_tech_F",
             "L154.out_mpkm_state_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.DeleteSupplysector_USAtrn",
             "L254.DeleteFinalDemand_USAtrn",
             "L254.Supplysector_trn_USA",
             "L254.FinalEnergyKeyword_trn_USA",
             "L254.tranSubsectorLogit_USA",
             "L254.tranSubsectorShrwtFllt_USA",
             "L254.tranSubsectorInterp_USA",
             "L254.tranSubsectorSpeed_USA",
             "L254.tranSubsectorSpeed_passthru_USA",
             "L254.tranSubsectorSpeed_noVOTT_USA",
             "L254.tranSubsectorSpeed_nonmotor_USA",
             "L254.tranSubsectorVOTT_USA",
             "L254.tranSubsectorFuelPref_USA",
             "L254.StubTranTech_USA",
             "L254.StubTranTech_passthru_USA",
             "L254.StubTranTech_nonmotor_USA",
             "L254.StubTranTechLoadFactor_USA",
             "L254.StubTranTechCost_USA",
             "L254.StubTranTechCoef_USA",
             "L254.PerCapitaBased_trn_USA",
             "L254.PriceElasticity_trn_USA",
             "L254.IncomeElasticity_trn_USA",
             "L254.StubTranTechCalInput_USA",
             "L254.StubTranTechProd_nonmotor_USA",
             "L254.StubTranTechCalInput_passthru_USA",
             "L254.BaseService_trn_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")
    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor")
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru")
    A54.sector <- get_data(all_data, "energy/A54.sector")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L154.in_EJ_state_trn_m_sz_tech_F <- get_data(all_data, "L154.in_EJ_state_trn_m_sz_tech_F")
    L154.out_mpkm_state_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_state_trn_nonmotor_Yh")

    # ===================================================
    # Delete transportation sectors in the full USA region
    L254.Supplysector_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == "USA") %>%
      select(region, supplysector) ->
      L254.DeleteSupplysector_USAtrn

    # Delete energy final demand sectors in the full USA region
    L254.PerCapitaBased_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == "USA") %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["EnergyFinalDemand"]]))) ->
      L254.DeleteFinalDemand_USAtrn

    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == "USA") %>%
        write_to_all_states(names(data))

      # Re-set markets from USA to regional markets, if called for in the GCAM-USA assumptions for selected fuels
      if(gcamusa.USE_REGIONAL_FUEL_MARKETS & "market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      # NOTE: electricity is always consumed from state markets
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS,
                                       region[minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS]))
      }

      data_new
    }

    process_USA_to_states(L254.Supplysector_trn) -> L254.Supplysector_trn_USA
    process_USA_to_states(L254.FinalEnergyKeyword_trn) -> L254.FinalEnergyKeyword_trn_USA
    process_USA_to_states(L254.tranSubsectorLogit) -> L254.tranSubsectorLogit_USA
    process_USA_to_states(L254.tranSubsectorShrwtFllt) -> L254.tranSubsectorShrwtFllt_USA
    process_USA_to_states(L254.tranSubsectorInterp) -> L254.tranSubsectorInterp_USA
    process_USA_to_states(L254.tranSubsectorSpeed) -> L254.tranSubsectorSpeed_USA
    process_USA_to_states(L254.tranSubsectorSpeed_passthru) -> L254.tranSubsectorSpeed_passthru_USA
    process_USA_to_states(L254.tranSubsectorSpeed_noVOTT) -> L254.tranSubsectorSpeed_noVOTT_USA
    process_USA_to_states(L254.tranSubsectorSpeed_nonmotor) -> L254.tranSubsectorSpeed_nonmotor_USA
    process_USA_to_states(L254.tranSubsectorVOTT) -> L254.tranSubsectorVOTT_USA
    process_USA_to_states(L254.tranSubsectorFuelPref) -> L254.tranSubsectorFuelPref_USA
    process_USA_to_states(L254.StubTranTech) -> L254.StubTranTech_USA
    process_USA_to_states(L254.StubTech_passthru) -> L254.StubTranTech_passthru_USA
    process_USA_to_states(L254.StubTech_nonmotor) -> L254.StubTranTech_nonmotor_USA
    process_USA_to_states(L254.StubTranTechLoadFactor) -> L254.StubTranTechLoadFactor_USA
    process_USA_to_states(L254.StubTranTechCost) -> L254.StubTranTechCost_USA
    L254.StubTranTechCoef %>%
      mutate(coefficient = round(coefficient, digits = gcamusa.DIGITS_TRNUSA_DEFAULT)) %>%
      process_USA_to_states ->
      L254.StubTranTechCoef_USA
    process_USA_to_states(L254.PerCapitaBased_trn) -> L254.PerCapitaBased_trn_USA
    process_USA_to_states(L254.PriceElasticity_trn) -> L254.PriceElasticity_trn_USA
    process_USA_to_states(L254.IncomeElasticity_trn) -> L254.IncomeElasticity_trn_USA

    # Calibration
    # L254.StubTranTechCalInput_USA: calibrated energy consumption by all technologies
    # NOTE: NEED TO WRITE THIS OUT FOR ALL TECHNOLOGIES, NOT JUST THOSE THAT EXIST IN SOME BASE YEARS.
    # Model may make up calibration values otherwise.
    L154.in_EJ_state_trn_m_sz_tech_F %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(UCD_techs, UCD_sector, mode, size.class, UCD_technology, UCD_fuel,
                                      supplysector, tranSubsector, stub.technology = tranTechnology, minicam.energy.input),
                               by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTech"]])), year, minicam.energy.input, calibrated.value) ->
      L254.StubTranTechCalInput_USA

    L254.StubTranTechCoef_USA %>%
      filter(year %in% BASE_YEARS) %>%
      select_if(names(L254.StubTranTechCoef_USA) %in% LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]) %>%
      left_join(L254.StubTranTechCalInput_USA,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input")) %>%
      replace_na(list(calibrated.value = 0)) %>%
      mutate(share.weight.year = year,
             # Create the needed variables to use the function set_subsector_shrwt
             subsector = tranSubsector, calOutputValue = calibrated.value) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]))) ->
      L254.StubTranTechCalInput_USA

    # Non-motorized technologies
    # L254.StubTranTechProd_nonmotor_USA: service output of non-motorized transportation technologies
    L154.out_mpkm_state_trn_nonmotor_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_MPKM),
             region = state, tranSubsector = mode) %>%
      left_join_error_no_match(A54.globaltech_nonmotor, by = "tranSubsector") %>%
      mutate(stub.technology = technology) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTech"]])), year, calOutputValue) ->
      L254.StubTranTechProd_nonmotor_USA

    # L254.StubTranTechCalInput_passthru_USA: calibrated input of passthrough technologies
    # First, need to calculate the service output for all tranTechnologies
    # calInput * loadFactor * unit_conversion / (coef * unit conversion )
    L254.StubTranTechCalInput_USA %>%
      left_join_error_no_match(L254.StubTranTechLoadFactor_USA,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year")) %>%
      left_join_error_no_match(L254.StubTranTechCoef_USA,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(output = round(calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
                            digits = gcamusa.DIGITS_TRNUSA_DEFAULT))  ->
      L254.StubTranTechOutput_USA

    # The next step is to bind rows with all pass-through technologies on to this table
    # Write all possible pass-through technologies to all regions

    # Start with the table of output by tranTechnology
    L254.StubTranTechOutput_USA %>%
      group_by(region, year, supplysector) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTranTechCalInput_passthru_USA_agg

    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(region, supplysector, tranSubsector, stub.technology = technology, year, minicam.energy.input) %>%
      # Subset only the passthrough technologies that are applicable in each region
      semi_join(L254.StubTranTech_passthru_USA,
                by = c("region", "supplysector", "tranSubsector", "stub.technology")) %>%
      left_join(L254.StubTranTechCalInput_passthru_USA_agg,
                by = c("region", "year", "minicam.energy.input" = "supplysector")) %>%
      replace_na(list(output_agg = 0)) %>%
      arrange(desc(minicam.energy.input)) %>%
      group_by(region, year) %>%
      mutate(output_cum = cumsum(output_agg)) %>%
      ungroup() ->
      L254.StubTranTechCalInput_passthru_USA_cum

    LIST_supplysector <- unique(L254.StubTranTechCalInput_passthru_USA_cum$supplysector)

    L254.StubTranTechCalInput_passthru_USA_cum %>%
      mutate(calibrated.value = if_else(minicam.energy.input %in% LIST_supplysector,
                                        output_cum, output_agg)) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]))) ->
      L254.StubTranTechCalInput_passthru_USA

    # L254.BaseService_trn_USA: base-year service output of transportation final demand
    L254.StubTranTechOutput_USA %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTech"]])), year, base.service = output) %>%
      bind_rows(L254.StubTranTechProd_nonmotor_USA %>%
                  select(one_of(c(LEVEL2_DATA_NAMES[["StubTranTech"]])), year, base.service = calOutputValue)) %>%
      left_join_error_no_match(select(A54.sector, supplysector, energy.final.demand), by = "supplysector") %>%
      group_by(region, energy.final.demand, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup ->
      L254.BaseService_trn_USA

    # ===================================================

    # Produce outputs
    L254.DeleteSupplysector_USAtrn %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.DeleteSupplysector_USAtrn") %>%
      add_precursors("L254.Supplysector_trn") ->
      L254.DeleteSupplysector_USAtrn

    L254.DeleteFinalDemand_USAtrn %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.DeleteFinalDemand_USAtrn") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      L254.DeleteFinalDemand_USAtrn

    L254.Supplysector_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.Supplysector_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.Supplysector_trn") ->
      L254.Supplysector_trn_USA

    L254.FinalEnergyKeyword_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.FinalEnergyKeyword_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.FinalEnergyKeyword_trn") ->
      L254.FinalEnergyKeyword_trn_USA

    L254.tranSubsectorLogit_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorLogit_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorLogit") ->
      L254.tranSubsectorLogit_USA

    L254.tranSubsectorShrwtFllt_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorShrwtFllt_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorShrwtFllt") ->
      L254.tranSubsectorShrwtFllt_USA

    L254.tranSubsectorInterp_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorInterp_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorInterp") ->
      L254.tranSubsectorInterp_USA

    L254.tranSubsectorSpeed_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorSpeed_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L254.tranSubsectorSpeed_USA

    L254.tranSubsectorSpeed_passthru_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorSpeed_passthru_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_passthru") ->
      L254.tranSubsectorSpeed_passthru_USA

    L254.tranSubsectorSpeed_noVOTT_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorSpeed_noVOTT_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_noVOTT") ->
      L254.tranSubsectorSpeed_noVOTT_USA

    L254.tranSubsectorSpeed_nonmotor_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorSpeed_nonmotor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_nonmotor") ->
      L254.tranSubsectorSpeed_nonmotor_USA

    L254.tranSubsectorVOTT_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorVOTT_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorVOTT") ->
      L254.tranSubsectorVOTT_USA

    L254.tranSubsectorFuelPref_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.tranSubsectorFuelPref_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorFuelPref") ->
      L254.tranSubsectorFuelPref_USA

    L254.StubTranTech_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTech_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTech") ->
      L254.StubTranTech_USA

    L254.StubTranTech_passthru_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name(" L254.StubTranTech_passthru_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTech_passthru") ->
      L254.StubTranTech_passthru_USA

    L254.StubTranTech_nonmotor_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTech_nonmotor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTech_nonmotor") ->
      L254.StubTranTech_nonmotor_USA

    L254.StubTranTechLoadFactor_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechLoadFactor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechLoadFactor") ->
      L254.StubTranTechLoadFactor_USA

    L254.StubTranTechCost_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechCost_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechCost") ->
      L254.StubTranTechCost_USA

    L254.StubTranTechCoef_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechCoef_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechCoef") ->
      L254.StubTranTechCoef_USA

    L254.PerCapitaBased_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.PerCapitaBased_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.PerCapitaBased_trn") ->
      L254.PerCapitaBased_trn_USA

    L254.PriceElasticity_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.PriceElasticity_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.PriceElasticity_trn") ->
      L254.PriceElasticity_trn_USA

    L254.IncomeElasticity_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.IncomeElasticity_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.IncomeElasticity_trn") ->
      L254.IncomeElasticity_trn_USA

    L254.StubTranTechCalInput_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      add_precursors("L154.in_EJ_state_trn_m_sz_tech_F",
                     "energy/mappings/UCD_techs") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L254.StubTranTechCalInput_USA

    L254.StubTranTechProd_nonmotor_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechProd_nonmotor_USA") %>%
      add_precursors("L154.out_mpkm_state_trn_nonmotor_Yh",
                     "energy/A54.globaltech_nonmotor") ->
      L254.StubTranTechProd_nonmotor_USA

    L254.StubTranTechCalInput_passthru_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.StubTranTechCalInput_passthru_USA") %>%
      same_precursors_as("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      same_precursors_as("L254.StubTranTech_passthru_USA") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.StubTranTechCalInput_passthru_USA

    L254.BaseService_trn_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L254.BaseService_trn_USA") %>%
      same_precursors_as("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      same_precursors_as("L254.StubTranTechProd_nonmotor_USA") %>%
      add_precursors("energy/A54.sector") ->
      L254.BaseService_trn_USA

    return_data(L254.DeleteSupplysector_USAtrn, L254.DeleteFinalDemand_USAtrn,
                L254.Supplysector_trn_USA,
                L254.FinalEnergyKeyword_trn_USA,
                L254.tranSubsectorLogit_USA,
                L254.tranSubsectorShrwtFllt_USA,
                L254.tranSubsectorInterp_USA,
                L254.tranSubsectorSpeed_USA,
                L254.tranSubsectorSpeed_passthru_USA,
                L254.tranSubsectorSpeed_noVOTT_USA,
                L254.tranSubsectorSpeed_nonmotor_USA,
                L254.tranSubsectorVOTT_USA,
                L254.tranSubsectorFuelPref_USA,
                L254.StubTranTech_USA,
                L254.StubTranTech_passthru_USA,
                L254.StubTranTech_nonmotor_USA,
                L254.StubTranTechLoadFactor_USA,
                L254.StubTranTechCost_USA,
                L254.StubTranTechCoef_USA,
                L254.PerCapitaBased_trn_USA,
                L254.PriceElasticity_trn_USA,
                L254.IncomeElasticity_trn_USA,
                L254.StubTranTechCalInput_USA, L254.StubTranTechProd_nonmotor_USA,
                L254.StubTranTechCalInput_passthru_USA, L254.BaseService_trn_USA)
  } else {
    stop("Unknown command")
  }
}
