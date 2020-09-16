# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L224.heat
#'
#' Write district heat sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L224.SectorLogitTables[[ curr_table ]]$data}, \code{L224.Supplysector_heat}, \code{L224.SubsectorLogitTables[[ curr_table ]]$data}, \code{L224.SubsectorLogit_heat}, \code{L224.SubsectorShrwt_heat}, \code{L224.SubsectorShrwtFllt_heat}, \code{L224.SubsectorInterp_heat}, \code{L224.SubsectorInterpTo_heat}, \code{L224.StubTech_heat}, \code{L224.GlobalTechCoef_heat}, \code{L224.GlobalTechCost_heat}, \code{L224.GlobalTechShrwt_heat}, \code{L224.StubTechCalInput_heat}, \code{L224.StubTechSecOut_elec}, \code{L224.StubTechCost_elec}. The corresponding file in the
#' original data system was \code{L224.heat.R} (energy level2).
#' @details This chunk creates level 2 output files for district heat sector. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions that have district heat. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions. From the level 1 heat data, this chunk computes stub tech calibrated inputs, secondary
#' outputs from elec and modified costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select
#' @author JDH August 2017
module_energy_L224.heat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A24.sector",
             FILE = "energy/A24.subsector_logit",
             FILE = "energy/A24.subsector_shrwt",
             FILE = "energy/A24.subsector_interp",
             FILE = "energy/A24.globaltech_coef",
             FILE = "energy/A24.globaltech_cost",
             FILE = "energy/A24.globaltech_shrwt",
             "L1231.eff_R_elec_F_tech_Yh",
             "L124.in_EJ_R_heat_F_Yh",
             "L124.heatoutratio_R_elec_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L224.Supplysector_heat",
             "L224.SubsectorLogit_heat",
             "L224.SubsectorShrwt_heat",
             "L224.SubsectorShrwtFllt_heat",
             "L224.SubsectorInterp_heat",
             "L224.SubsectorInterpTo_heat",
             "L224.StubTech_heat",
             "L224.GlobalTechCoef_heat",
             "L224.GlobalTechCost_heat",
             "L224.GlobalTechShrwt_heat",
             "L224.StubTechCalInput_heat",
             "L224.StubTechSecOut_elec",
             "L224.StubTechCost_elec"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    has_district_heat <- region <- year.fillout <- to.value <- efficiency <-
      technology <- coef <- subsector <- supplysector <- minicam.energy.input <-
      input.cost <- minicam.non.energy.input <- share.weight <- sector <- fuel <-
      value <- subs.share.weight <- calibrated.value <- secondary.output <-
      stub.technology <- cost_modifier <- year <- output.ratio <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A24.sector <- get_data(all_data, "energy/A24.sector", strip_attributes = TRUE)
    A24.subsector_logit <- get_data(all_data, "energy/A24.subsector_logit", strip_attributes = TRUE)
    A24.subsector_shrwt <- get_data(all_data, "energy/A24.subsector_shrwt", strip_attributes = TRUE)
    A24.subsector_interp <- get_data(all_data, "energy/A24.subsector_interp", strip_attributes = TRUE)
    A24.globaltech_coef <- get_data(all_data, "energy/A24.globaltech_coef")
    A24.globaltech_cost <- get_data(all_data, "energy/A24.globaltech_cost")
    A24.globaltech_shrwt <- get_data(all_data, "energy/A24.globaltech_shrwt")
    L1231.eff_R_elec_F_tech_Yh <- get_data(all_data, "L1231.eff_R_elec_F_tech_Yh")
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh", strip_attributes = TRUE)
    L124.heatoutratio_R_elec_F_tech_Yh <- get_data(all_data, "L124.heatoutratio_R_elec_F_tech_Yh", strip_attributes = TRUE)

    # Changing input data into long format
    A24.globaltech_coef %>%
      gather_years(value_col = "coef") -> A24.globaltech_coef

    A24.globaltech_cost %>%
      gather_years(value_col = "input.cost") -> A24.globaltech_cost

    A24.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") -> A24.globaltech_shrwt

    # ===================================================
    # Create list of regions with district heat modeled
    A_regions %>%
      filter(has_district_heat == 1) %>%
      select(region) -> heat_region

    # Supply sector information for district heat sectors
    A24.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(region %in% heat_region$region) -> L224.Supplysector_heat

    # Subsector logit exponents of district heat sectors
    A24.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      filter(region %in% heat_region$region) -> L224.SubsectorLogit_heat

    # L224.SubsectorShrwt_heat and L224.SubsectorShrwtFllt_heat: Subsector shareweights of district heat sectors
    if(any(!is.na(A24.subsector_shrwt$year))) {
      A24.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names = GCAM_region_names ) %>%
        filter(region %in% heat_region$region) -> L224.SubsectorShrwt_heat
    }

    if(any(!is.na(A24.subsector_shrwt$year.fillout))) {
      A24.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names ) %>%
        filter(region %in% heat_region$region) -> L224.SubsectorShrwtFllt_heat
    }

    # Subsector shareweight interpolation of district heat sectors
    if(any(is.na(A24.subsector_interp$to.value))) {
      A24.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        filter(region %in% heat_region$region) -> L224.SubsectorInterp_heat
    }

    if(any(!is.na(A24.subsector_interp$to.value))) {
      A24.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        filter(region %in% heat_region$region) -> L224.SubsectorInterpTo_heat
    }

    # Identification of stub technologies of district heat
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A24.globaltech_shrwt %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      filter(region %in% heat_region$region) -> L224.StubTech_heat

    # Coefficients of global technologies
    # L224.GlobalTechCoef_heat: Energy inputs and coefficients of global technologies for district heat

    A24.globaltech_coef %>%
      select(supplysector, subsector, technology, minicam.energy.input) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A24.globaltech_coef, by = c("supplysector", "subsector", "technology", "minicam.energy.input", "year")) %>%
      mutate(coef = round(approx_fun(year, value = coef, rule = 1), energy.DIGITS_COEFFICIENT)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.energy.input, year, coefficient = coef) -> L224.GlobalTechCoef_heat

    # Costs of global technologies
    # L224.GlobalTechCost_heat: Costs of global technologies for district heat

    A24.globaltech_cost %>%
      select(supplysector, subsector, technology, minicam.non.energy.input) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A24.globaltech_cost, by = c("supplysector", "subsector", "technology", "minicam.non.energy.input", "year")) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, value = input.cost, rule = 1), energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.non.energy.input, year, input.cost) -> L224.GlobalTechCost_heat

    # Shareweights of global technologies
    # L224.GlobalTechShrwt_heat: Shareweights of global technologies for district heat

    A24.globaltech_shrwt %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A24.globaltech_shrwt, by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(share.weight = approx_fun(year, value = share.weight, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) -> L224.GlobalTechShrwt_heat

    # Calibration and region-specific data, calibrated input to district heat
    L124.in_EJ_R_heat_F_Yh %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
                  distinct, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      filter(region %in% heat_region$region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "value") %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             year.share.weight = year,
             subs.share.weight = if_else(calibrated.value == 0, 0, 1),
             share.weight = subs.share.weight) %>%
      select(-value) -> L224.StubTechCalInput_heat

    # Secondary output of heat, applied to electricity generation technologies
    # NOTE: This is complicated. Initially tried using historical information for all model periods that fall within historical time
    # (i.e. not just the model base years). However for regions like the FSU where historical periods often have very low output of heat
    # from the district heat sector, and most heat as a secondary output from the electricity sector, the secondary output heat can easily
    # exceed the demands from the end-use sectors, causing model solution failure. For this reason, the convention applied here is to
    # use the secondary output of heat from the power sector only in the model base years.
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology) %>%
                  distinct, by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology,
             secondary.output = A24.sector[["supplysector"]],
             output.ratio = round(value, energy.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecOut"]]) -> L224.StubTechSecOut_elec

    # Calculate cost adjustment, equal to the output of heat multiplied by the heat price (to minimize the distortion of including the secondary output)
    L224.StubTechSecOut_elec %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "output.ratio") %>%
      mutate(minicam.non.energy.input = "heat plant",
             input.cost = round(output.ratio * energy.HEAT_PRICE, energy.DIGITS_COST))-> L224.StubTechCost_elec

    # The secondary output of heat from CHP in the electric sector can cause the price of the technologies
    # to go very low or negative if the technology cost is not modified to reflect the additional costs of
    # CHP systems (as compared with electricity-only systems). Low technology costs can cause unrealistically
    # low electricity prices in the calibration year, distorting behavior in future years. In this method,
    # costs related to heat production and distribution are backed out from exogenous heat prices and data-derived heat:power ratios.
    L1231.eff_R_elec_F_tech_Yh %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(efficiency = value) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(region %in% heat_region$region) %>%
      filter(fuel == "gas") %>%
      filter(efficiency < energy.DEFAULT_ELECTRIC_EFFICIENCY) %>%
      mutate(cost_modifier = energy.GAS_PRICE * (1 / energy.DEFAULT_ELECTRIC_EFFICIENCY - 1 / efficiency)) -> L224.eff_cost_adj_Rh_elec_gas_sc_Y

    # Modify the costs
    L224.StubTechCost_elec %>%
      left_join(L224.eff_cost_adj_Rh_elec_gas_sc_Y %>%
                  rename(subsector = fuel, stub.technology = technology) %>%
                  select(region, subsector, stub.technology, year, cost_modifier),
                by = c("region", "subsector", "stub.technology", "year")) %>%
      mutate(input.cost = if_else(!is.na(cost_modifier), round(pmax(0, input.cost + cost_modifier), energy.DIGITS_COST), input.cost)) %>%
      select(-cost_modifier, -output.ratio) -> L224.StubTechCost_elec

    # Need to fill out object names for all model time periods
    L224.StubTechCost_elec %>%
      filter(year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(input.cost = 0) -> L224.StubTechCost_elec_fut

    L224.StubTechCost_elec %>%
      bind_rows(L224.StubTechCost_elec_fut) -> L224.StubTechCost_elec


    # ===================================================

    # Produce outputs

    L224.Supplysector_heat %>%
      add_title("Supply sector information for district heat sectors") %>%
      add_units("N/A") %>%
      add_comments("Supply sector info for district heat is written to all regions, ") %>%
      add_comments("filtered by which regions have district heat") %>%
      add_legacy_name("L224.Supplysector_heat") %>%
      add_precursors("energy/A24.sector", "energy/A_regions", "common/GCAM_region_names") ->
      L224.Supplysector_heat

    L224.SubsectorLogit_heat %>%
      add_title("Subsector logit exponents of district heat sectors") %>%
      add_units("N/A") %>%
      add_comments("Subsector logit exponents for district heat is written to all regions, ") %>%
      add_comments("filtered by which regions have district heat") %>%
      add_legacy_name("L224.SubsectorLogit_heat") %>%
      add_precursors("energy/A24.subsector_logit", "energy/A_regions", "common/GCAM_region_names") ->
      L224.SubsectorLogit_heat

    if(exists("L224.SubsectorShrwt_heat")) {
      L224.SubsectorShrwt_heat %>%
        add_title("Subsector shareweights of district heat sectors") %>%
        add_units("N/A") %>%
        add_comments("If year is not NA: Subsector shareweights for district heat written to all regions, ") %>%
        add_comments("filtered by which regions have district heat") %>%
        add_legacy_name("L224.SubsectorShrwt_heat") %>%
        add_precursors("energy/A24.subsector_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
        L224.SubsectorShrwt_heat
    } else {
      # If year column of A24.subsector_shrwt is all N/A, then a blank tibble is produced (and presumably the following tibble, using year.fillout is made)
      missing_data() %>%
        add_legacy_name("L224.SubsectorShrwt_heat") ->
        L224.SubsectorShrwt_heat
    }

    if(exists("L224.SubsectorShrwtFllt_heat")) {
      L224.SubsectorShrwtFllt_heat %>%
        add_title("Subsector shareweights of district heat sectors with fillout year") %>%
        add_units("N/A") %>%
        add_comments("If year.fillout is not NA: Subsector shareweights for district heat written to all regions, ") %>%
        add_comments("filtered by which regions have district heat, uses year.fillout") %>%
        add_legacy_name("L224.SubsectorShrwtFllt_heat") %>%
        add_precursors("energy/A24.subsector_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
        L224.SubsectorShrwtFllt_heat
    } else {
      # If year.fillout column of A24.subsector_shrwt is all N/A, then a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L224.SubsectorShrwtFllt_heat") ->
        L224.SubsectorShrwtFllt_heat
    }

    if(exists("L224.SubsectorInterp_heat")) {
      L224.SubsectorInterp_heat %>%
        add_title("Subsector shareweight interpolation of district heat sectors") %>%
        add_units("units") %>%
        add_comments("Interpolated data from A24.subsector_interp, ") %>%
        add_comments("filtered by which regions have district heat") %>%
        add_legacy_name("L224.SubsectorInterp_heat") %>%
        add_precursors("energy/A24.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
        L224.SubsectorInterp_heat
    } else {
      # If interp.to column of A24.subsector_interp contains no N/A values, then a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L224.SubsectorInterp_heat") ->
        L224.SubsectorInterp_heat
    }

    if(exists("L224.SubsectorInterpTo_heat")) {
      L224.SubsectorInterpTo_heat %>%
        add_title("Subsector shareweight interpolation of district heat sectors using to.year") %>%
        add_units("units") %>%
        add_comments("Interpolated data from A24.subsector_interp, ") %>%
        add_comments("filtered by which regions have district heat") %>%
        add_legacy_name("L224.SubsectorInterpTo_heat") %>%
        add_precursors("energy/A24.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
        L224.SubsectorInterpTo_heat
    } else {
      # If interp.to column of A24.subsector_interp contains N/A values, then a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L224.SubsectorInterpTo_heat") ->
        L224.SubsectorInterpTo_heat
    }

    L224.StubTech_heat %>%
      add_title("Identification of stub technologies of district heat") %>%
      add_units("N/A") %>%
      add_comments("A24.globaltech_shrwt written to all regions, filtered regions with district heat") %>%
      add_legacy_name("L224.StubTech_heat") %>%
      add_precursors("energy/A24.globaltech_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
      L224.StubTech_heat

    L224.GlobalTechCoef_heat %>%
      add_title("Energy inputs and coefficients of global technologies for district heat") %>%
      add_units("unitless") %>%
      add_comments("A24.globaltech_coef interpolated to all model years for sector district heat") %>%
      add_legacy_name("L224.GlobalTechCoef_heat") %>%
      add_precursors("energy/A24.globaltech_coef") ->
      L224.GlobalTechCoef_heat

    L224.GlobalTechCost_heat %>%
      add_title("Costs of global technologies for district heat") %>%
      add_units("1975$/GJ") %>%
      add_comments("A24.globaltech_cost interpolated to all model years for sector district heat") %>%
      add_legacy_name("L224.GlobalTechCost_heat") %>%
      add_precursors("energy/A24.globaltech_cost") ->
      L224.GlobalTechCost_heat

    L224.GlobalTechShrwt_heat %>%
      add_title("Shareweights of global technologies for district heat") %>%
      add_units("unitless") %>%
      add_comments("A24.globaltech_shrwt interpolated to all model years for sector district heat") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L224.GlobalTechShrwt_heat") %>%
      add_precursors("energy/A24.globaltech_shrwt") ->
      L224.GlobalTechShrwt_heat

    L224.StubTechCalInput_heat %>%
      add_title("Calibrated input to district heat") %>%
      add_units("EJ/yr") %>%
      add_comments("L124.in_EJ_R_heat_F_Yh and calibrated_techs are joined, shareweights assigned") %>%
      add_comments("as 0 if the calibrated value is 0 and 1 if it is not 0") %>%
      add_legacy_name("L224.StubTechCalInput_heat") %>%
      add_precursors("L124.in_EJ_R_heat_F_Yh", "energy/calibrated_techs", "energy/A_regions", "common/GCAM_region_names") ->
      L224.StubTechCalInput_heat

    L224.StubTechSecOut_elec %>%
      add_title("Secondary output of district heat from electricity technologies") %>%
      add_units("EJ") %>%
      add_comments("L124.heatoutratio_R_elec_F_tech_Yh used to determine secondary output heat from elec, ") %>%
      add_comments("filtering for only model base years") %>%
      add_legacy_name("L224.StubTechSecOut_elec") %>%
      add_precursors("L124.heatoutratio_R_elec_F_tech_Yh", "energy/calibrated_techs", "energy/A24.sector", "common/GCAM_region_names") ->
      L224.StubTechSecOut_elec

    L224.StubTechCost_elec %>%
      add_title("Stubtech costs with secondary output heat") %>%
      add_units("1975$/GJ") %>%
      add_comments("From L224.StubTechSecOut_elec calculate cost adjustment, equal to the output of heat multiplied by the heat price") %>%
      add_comments("modify costs for technologies with efficiencies below default, apply to all model periods") %>%
      add_legacy_name("L224.StubTechCost_elec") %>%
      add_precursors("L124.heatoutratio_R_elec_F_tech_Yh", "energy/calibrated_techs", "energy/A24.sector", "energy/A_regions", "L1231.eff_R_elec_F_tech_Yh", "common/GCAM_region_names") ->
      L224.StubTechCost_elec

    return_data(L224.Supplysector_heat, L224.SubsectorLogit_heat, L224.SubsectorShrwt_heat, L224.SubsectorShrwtFllt_heat, L224.SubsectorInterp_heat, L224.SubsectorInterpTo_heat, L224.StubTech_heat, L224.GlobalTechCoef_heat, L224.GlobalTechCost_heat, L224.GlobalTechShrwt_heat, L224.StubTechCalInput_heat, L224.StubTechSecOut_elec, L224.StubTechCost_elec)
  } else {
    stop("Unknown command")
  }
}
