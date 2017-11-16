#' module_gcam.usa_L232.industry_USA
#'
#' Prepare level 2 industry sector files for USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.DeleteSupplysector_USAind}, \code{L232.DeleteFinalDemand_USAind}, \code{object}, \code{L232.StubTechCalInput_indenergy_USA}, \code{L232.StubTechCalInput_indfeed_USA}, \code{L232.StubTechProd_industry_USA}, \code{L232.StubTechCoef_industry_USA}, \code{L232.StubTechMarket_ind_USA}, \code{L232.StubTechSecMarket_ind_USA}, \code{L232.BaseService_ind_USA}. The corresponding file in the
#' original data system was \code{L232.industry_USA.R} (gcam-usa level2).
#' @details Prepare level 2 industry sector files for USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST October 2017
module_gcam.usa_L232.industry_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/A32.demand",
             FILE = "energy/A32.globaltech_eff",
             FILE = "energy/calibrated_techs",
             "L232.Supplysector_ind",
             "L232.StubTech_ind",
             "L232.PerCapitaBased_ind",
             "L132.in_EJ_state_indnochp_F",
             "L132.in_EJ_state_indfeed_F",
             "L132.in_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.DeleteSupplysector_USAind",
             "L232.DeleteFinalDemand_USAind",
             "L232.StubTechCalInput_indenergy_USA",
             "L232.StubTechCalInput_indfeed_USA",
             "L232.StubTechProd_industry_USA",
             "L232.StubTechCoef_industry_USA",
             "L232.StubTechMarket_ind_USA",
             "L232.StubTechSecMarket_ind_USA",
             "L232.BaseService_ind_USA"))
  } else if(command == driver.MAKE) {

    year <- value <- output_tot <- grid_region <- market.name <-
      calOutputValue <- calibrated.value <- calibration <- technology <-
      efficiency <- fuel <- minicam.energy.input <- object <-
      output_tot <- region <- secondary.output <- sector <- state <-
      subs.share.weight <- subsector <- supplysector <- value <- x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A32.demand <- get_data(all_data, "energy/A32.demand")
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind")
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
    L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind")
    L132.in_EJ_state_indnochp_F <- get_data(all_data, "L132.in_EJ_state_indnochp_F")
    L132.in_EJ_state_indfeed_F <- get_data(all_data, "L132.in_EJ_state_indfeed_F")
    L132.in_EJ_state_indchp_F <- get_data(all_data, "L132.in_EJ_state_indchp_F")

    # ===================================================

    # convert to long form
    A32.globaltech_eff %>%
      gather_years -> A32.globaltech_eff

    # delete industry sectors in the USA region (energy-final-demands and supplysectors)
    L232.Supplysector_ind %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == "USA") %>%
      select(one_of(LEVEL2_DATA_NAMES[["DeleteSupplysector"]])) ->
      L232.DeleteSupplysector_USAind  ## OUTPUT

    # deleting energy final demand sectors in the full USA region" )
    L232.PerCapitaBased_ind %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == "USA") %>%
      select(one_of(LEVEL2_DATA_NAMES[["DeleteFinalDemand"]])) ->
      L232.DeleteFinalDemand_USAind  ## OUTPUT

    # get calibrated input of industrial energy use technologies, including cogen
    L132.in_EJ_state_indnochp_F %>%
      bind_rows(L132.in_EJ_state_indchp_F) %>%
      complete(nesting(state, sector, fuel), year = c(year, BASE_YEARS)) %>%
      group_by(state, sector, fuel) %>% mutate(value = approx_fun(year, value)) %>%
      ungroup %>% filter(year %in% BASE_YEARS) %>%
      rename(region = state) %>%
      left_join_keep_first_only(calibrated_techs, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_state_indenergy_F_Yh
    L232.in_EJ_state_indenergy_F_Yh %>% select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]], "value")) %>%
      left_join_keep_first_only(select(A32.globaltech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(x = sum(calibrated.value),
             subs.share.weight = if_else(x > 0, 1, 0)) %>% ungroup %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCalInput"]])) ->
      L232.StubTechCalInput_indenergy_USA  ## OUTPUT

    # get calibrated input of industrial feedstock technologies
    L132.in_EJ_state_indfeed_F %>%
      complete(nesting(state, sector, fuel), year = c(year, BASE_YEARS)) %>%
      group_by(state, sector, fuel) %>% mutate(value = approx_fun(year, value)) %>%
      ungroup %>% filter(year %in% BASE_YEARS) %>%
      rename(region = state) %>%
      left_join_keep_first_only(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_state_indfeed_F_Yh
    L232.in_EJ_state_indfeed_F_Yh %>% select(one_of(LEVEL2_DATA_NAMES[["StubTechYr"]], "value")) %>%
      left_join_keep_first_only(select(A32.globaltech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>% mutate(x = sum(calibrated.value)) %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      mutate(subs.share.weight = if_else(x > 0, 1, 0)) %>% ungroup %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCalInput"]])) ->
      L232.StubTechCalInput_indfeed_USA  ## OUTPUT

    # get industrial sector calibrated output
    A32.globaltech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, BASE_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(value = approx_fun(year, value)) %>% ungroup %>%
      filter(year %in% BASE_YEARS) %>%
      rename(efficiency = value) %>%
      mutate(efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) ->
      A32.globaltech_eff_interp

    L232.in_EJ_state_indenergy_F_Yh %>%
      bind_rows(L232.in_EJ_state_indfeed_F_Yh) %>%
      left_join_keep_first_only(select(A32.globaltech_eff_interp, supplysector, subsector, technology, year, efficiency),
                                by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L232.out_EJ_state_ind_serv_F_Yh
      # ^^ service output, by technology, for energy-use and feedstocks

    L232.out_EJ_state_ind_serv_F_Yh %>%
      group_by(region, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>% ungroup %>%
      # ^^ aggregate to get output of industrial sector in each region
      mutate(supplysector = "industry",
             subsector = "industry",
             stub.technology = "industry",
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) ->
      L232.StubTechProd_industry_USA  ## OUTPUT

    # get calibrated output of industrial sector
    L232.out_EJ_state_ind_serv_F_Yh %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>% ungroup %>%
      # ^^ aggregate service output by sector
      left_join_keep_first_only(L232.StubTechProd_industry_USA %>%
                                  rename(output_tot = calOutputValue) %>% select(region, year, output_tot),
                                by = c("region", "year")) %>%
      mutate(coefficient = round(calOutputValue / output_tot, energy.DIGITS_COEFFICIENT)) %>%
      # ^^ get coefficient
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = "industry",
             subsector = "industry",
             stub.technology = "industry",
             market.name = region) ->
      L232.StubTechCoef_industry_USA_base
    # ^^ covers only base years

    L232.StubTechCoef_industry_USA_base %>%
      filter(year == max(BASE_YEARS)) %>% select(-year) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) ->
      L232.StubTechCoef_industry_USA_fut
    # ^^ future years copied from final base year
    # note: this is not typical, but is suitable here as no energy:feedstock evolution in the industrial...
    # ... sector is expected for USA

    bind_rows(L232.StubTechCoef_industry_USA_base, L232.StubTechCoef_industry_USA_fut) %>%
      select(one_of(LEVEL2_DATA_NAMES$StubTechCoef)) ->
      L232.StubTechCoef_industry_USA  ## OUTPUT

    # Get markets for fuels consumed by the state industrial sectors
    L232.StubTech_ind %>% filter(region == "USA") %>% select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      # ^^ replaces the write_to_all_states function in the legacy code
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_keep_first_only(A32.globaltech_eff %>% select(supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      filter(is.na(minicam.energy.input) == FALSE) %>%
      # ^^ includes generic industrial technology that is not required here...
      mutate(market.name = "USA") %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechMarket"]])) %>%
      left_join_error_no_match(states_subregions %>% select(state, grid_region), by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS & gcamusa.USE_REGIONAL_FUEL_MARKETS == TRUE,
                                   grid_region, market.name)) %>% select(-grid_region) %>%
      mutate(market.name = if_else(grepl("elect_td", minicam.energy.input), region, market.name)) ->
      L232.StubTechMarket_ind_USA  ## OUTPUT

    # markets for the cogenerated electricity (secondary output)
    A32.globaltech_eff %>%
      filter(is.na(secondary.output) == FALSE) %>%
      select(supplysector, subsector, technology) ->
      L232.chp_techs
    L232.StubTechMarket_ind_USA %>%
      # ^^ electricity is consumed from state markets
      semi_join(L232.chp_techs, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # ^^ filters for rows contained in L232.chp_techs
      mutate(secondary.output = "electricity") %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTechYr"]], "secondary.output", "market.name"))) %>%
      mutate(market.name = "USA") %>%
      # ^^ over-ride regional market names
      left_join_error_no_match(states_subregions %>% select(state, grid_region), by = c("region" = "state")) %>%
      mutate(x = gcamusa.USE_REGIONAL_ELEC_MARKETS,
             market.name = if_else(x == TRUE, grid_region, market.name)) %>% select(-x, -grid_region) ->
      L232.StubTechSecMarket_ind_USA  ## OUTPUT

    # base-year service output of industry final demand
    L232.StubTechProd_industry_USA %>%
      select(region, year, calOutputValue) %>%
      rename(base.service = calOutputValue) %>%
      mutate(energy.final.demand = A32.demand$energy.final.demand) ->
      L232.BaseService_ind_USA  # base service is equal to the output of the industry supplysector


    # OUTPUTS...

    L232.DeleteSupplysector_USAind %>%
      add_title("USA industry supply sectors") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting industry sectors from input") %>%
      add_legacy_name("L232.DeleteSupplysector_USAind") %>%
      add_precursors("L232.Supplysector_ind") ->
      L232.DeleteSupplysector_USAind

    L232.DeleteFinalDemand_USAind %>%
      add_title("USA final energy demand table for industry") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting final demand sectors") %>%
      add_legacy_name("L232.DeleteFinalDemand_USAind") %>%
      add_precursors("L232.PerCapitaBased_ind") ->
      L232.DeleteFinalDemand_USAind

    L232.StubTechCalInput_indenergy_USA %>%
      add_title("calibrated input of industrial energy use technologies (including cogen)") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.StubTechCalInput_indenergy_USA") %>%
      add_precursors("L132.in_EJ_state_indnochp_F",
                     "L132.in_EJ_state_indchp_F",
                     "energy/calibrated_techs",
                     "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indenergy_USA

    L232.StubTechCalInput_indfeed_USA %>%
      add_title("Calibrated input of industrial feedstock technologies") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.StubTechCalInput_indfeed_USA") %>%
      add_precursors("L132.in_EJ_state_indfeed_F",
                     "energy/calibrated_techs",
                     "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indfeed_USA

    L232.StubTechProd_industry_USA %>%
      add_title("industrial sector output") %>%
      add_units("Unitless") %>%
      add_comments("Service output aggregated to industrial sector for each region") %>%
      add_legacy_name("L232.StubTechProd_industry_USA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L132.in_EJ_state_indnochp_F",
                     "L132.in_EJ_state_indfeed_F") ->
      L232.StubTechProd_industry_USA

    L232.StubTechCoef_industry_USA %>%
      add_title("industrial sector calibrated output") %>%
      add_units("Unitless") %>%
      add_comments("Generated by bind base and future year coefficients") %>%
      add_legacy_name("L232.StubTechCoef_industry_USA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L132.in_EJ_state_indnochp_F",
                     "L132.in_EJ_state_indfeed_F") ->
      L232.StubTechCoef_industry_USA

    L232.StubTechMarket_ind_USA %>%
      add_title("Markets for the fuels consumed by the state industrial sectors") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L232.StubTechMarket_ind_USA") %>%
      add_precursors("L232.StubTech_ind",
                     "energy/A32.globaltech_eff",
                     "gcam-usa/states_subregions") ->
      L232.StubTechMarket_ind_USA

    L232.StubTechSecMarket_ind_USA %>%
      add_title("markets for the cogenerated electricity (secondary output)") %>%
      add_units("NA") %>%
      add_comments("derived from L232.StubTechMarket_ind_USA") %>%
      add_legacy_name("L232.StubTechSecMarket_ind_USA") %>%
      add_precursors("L232.StubTech_ind",
                     "energy/A32.globaltech_eff",
                     "gcam-usa/states_subregions") ->
      L232.StubTechSecMarket_ind_USA

    L232.BaseService_ind_USA %>%
      add_title("base-year service output of industry final demand") %>%
      add_units("NA") %>%
      add_comments("base service is equal to the output of the industry supplysector") %>%
      add_legacy_name("L232.BaseService_ind_USA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L132.in_EJ_state_indnochp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "energy/A32.demand") ->
      L232.BaseService_ind_USA

    return_data(L232.DeleteSupplysector_USAind, L232.DeleteFinalDemand_USAind, L232.StubTechCalInput_indenergy_USA,
                L232.StubTechCalInput_indfeed_USA, L232.StubTechProd_industry_USA, L232.StubTechCoef_industry_USA,
                L232.StubTechMarket_ind_USA, L232.StubTechSecMarket_ind_USA, L232.BaseService_ind_USA)
  } else {
    stop("Unknown command")
  }
}
