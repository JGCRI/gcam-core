# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L270.EFW_input_coefs
#'
#' Energy-for-water input names and coefficients to water T&D technologies
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L270.TechCoef_EFW}. This did not exist in the original
#'   data system.
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate rename summarise select ungroup
#' @author GPK January 2019
module_water_L270.EFW_input_coefs <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/EFW_mapping",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L173.WWtrtfrac_R_ind_Yh",
             "L174.WWtrtfrac_R_muni_Yh",
             "L203.TechCoef_watertd"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.TechCoef_EFW",
             "L270.TechCoef_WWtrt_SSP"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- supplysector <- water.supplysector <- apply.to.desal.water <- minicam.energy.input <-
      EFW_input <- WWtrtfrac <- sector <- region <- coef_revised <- scenario <- value <- pcGDP <-
      baseGDP <- reduction <- coefficient <- technology <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L173.WWtrtfrac_R_ind_Yh <- get_data(all_data, "L173.WWtrtfrac_R_ind_Yh", strip_attributes = TRUE)
    L174.WWtrtfrac_R_muni_Yh <- get_data(all_data, "L174.WWtrtfrac_R_muni_Yh", strip_attributes = TRUE)
    L203.TechCoef_watertd <- get_data(all_data, "L203.TechCoef_watertd", strip_attributes = TRUE)

    # ===================================================

    # Generate the mapping table that translates from water mapping sectors to the EFW sectors
    L270.EFW_mapping <- select(EFW_mapping, supplysector, water.supplysector, apply.to.desal.water) %>%
      drop_na() %>%
      rename(EFW_input = supplysector, supplysector = water.supplysector)

    # L270.TechCoef_EFW: input coefficients from EFW sectors to water T&D technologies
    # The logic here is that the water flow volumes of the EFW sectors are the same as the water withdrawals, with the
    # exception that wastewater treatment flow volumes are lower, so the coefs will need subsequent adjustment. Because
    # these coefficients can not be assumed to be 1--for example, irrigation coefs are >1 to reflect system losses after
    # water withdrawal and before delivery to fields--they are instead copied from the level2 data input file. The
    # method of extracting the water.supplysector here is fragile - it expects that the water withdrawal T&D sectors are
    # of the form xxx_xxx_xxx....

    # EFW sectors are joined with inner_join: repeat rows of the LHS as necessary to match all combinations on the RHS,
    # and drop where the RHS is missing values (e.g., no EFW for primary or electric sector energy-for-water) Filter out
    # the irrelevant inputs to technologies that use desalinated water (e.g., water abstraction and treatment energy use
    # are already accounted in the desalinated water production sector)

    # Note - the gsub() statement is performing the opposite of the set_water_inputs_name() function, returning the generic
    # water supplysector name from the geographically specific one (e.g., water_td_irr from water_td_irr_California_W)

    L270.TechCoef_EFW_init <- L203.TechCoef_watertd %>%
      filter(technology != "water consumption") %>%
      mutate(water.supplysector = gsub('([a-z]+_[a-z]+_[a-z]+)_.*', '\\1', supplysector)) %>%
      inner_join(L270.EFW_mapping, by = c(water.supplysector = "supplysector")) %>%
      filter(!(technology == water.DESAL & apply.to.desal.water == 0)) %>%
      mutate(minicam.energy.input = EFW_input)

    # Because not all water withdrawn is later treated as wastewater, the wastewater treatment coefficients should be
    # replaced by those computed in prior steps (for historical periods) The step below temporarily fills out the last
    # historical value to all subsequent future model time periods. Note that 2010 is hard-wired as this is the base
    # year of the Liu et al inventory. These default coefficients will be supplemented with scenario-specific
    # coefficients in a subsequent step
    L270.WWtrt_coef <- bind_rows(L173.WWtrtfrac_R_ind_Yh, L174.WWtrtfrac_R_muni_Yh) %>%
      rename(coef_revised = WWtrtfrac) %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(select(EFW_mapping, water.supplysector, supplysector, sector), by = "sector") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(minicam.energy.input = supplysector) %>%
      select(region, water.supplysector, minicam.energy.input, year, coef_revised) %>%
      group_by(region, water.supplysector, minicam.energy.input) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coef_revised = if_else(year > 2010, coef_revised[year == 2010], coef_revised)) %>%
      ungroup()

    # This method uses the function used in GCAM (C++) for determining non-CO2 pollutant emissions factor reduction as a
    # function of per-capita GDP. The base-GDP scenario is set in the constants.
    L270.GDPreduction_scen_R_Y <- filter(L102.pcgdp_thous90USD_Scen_R_Y,
                                         year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)) %>%
      rename(pcGDP = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(scenario, region, year, pcGDP) %>%
      group_by(scenario, region) %>%
      mutate(baseGDP = pcGDP[year==max(MODEL_BASE_YEARS)]) %>%
      ungroup() %>%
      # Copied from C++ code for GDP control. negative values occur where GDP declines; not allowing this to reduce trtshr.
      mutate(reduction = 1 - (1 / (1 + (pcGDP - baseGDP) / efw.WWTRT_STEEPNESS)),
             reduction = if_else(reduction < 0, 0, reduction)) %>%
      select(scenario, region, year, reduction)

    # For the baseline (default, core) scenario, specify the future "reduction" in the discharge of untreated wastewater
    # This is done in two steps, so that the default (core) XML file will have one set of WW reduction rates,
    # and each alternative scenario (SSP) will have its own set that overwrites the defaults
    L270.GDPreduction_R_Y <- subset(L270.GDPreduction_scen_R_Y, scenario == efw.WWTRT_GDP_SCEN) %>%
      select(-scenario)

    # use left_join as L270.GDPreduction_R_Y doesn't have base year values
    L270.WWtrt_coef <- left_join(L270.WWtrt_coef, L270.GDPreduction_R_Y,
                                 by = c("region", "year")) %>%
      replace_na(list(reduction = 0)) %>%
      mutate(coef_revised = coef_revised + (efw.MAX_WWTRT_FRAC - coef_revised) * reduction) %>%
      select(-reduction)

    # use left_join as the L270.WWtrt_coef only apply to one of the EFW processes (wastewater treatment)
    L270.TechCoef_EFW <- left_join(L270.TechCoef_EFW_init, L270.WWtrt_coef,
                                   by = c("region", "water.supplysector", "minicam.energy.input", "year")) %>%
      mutate(coefficient = if_else(is.na(coef_revised), coefficient, coef_revised),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    # SSP scenario suite
    L270.WWtrt_coef_SSP <- inner_join(L270.WWtrt_coef, L270.GDPreduction_scen_R_Y,
                                         by = c("region", "year")) %>%
      replace_na(list(reduction = 0)) %>%
      mutate(coef_revised = coef_revised + (efw.MAX_WWTRT_FRAC - coef_revised) * reduction) %>%
      select(-reduction)

    L270.TechCoef_WWtrt_SSP <- inner_join(L270.TechCoef_EFW_init, L270.WWtrt_coef_SSP,
                                   by = c("region", "water.supplysector", "minicam.energy.input", "year")) %>%
      mutate(coefficient = if_else(is.na(coef_revised), coefficient, coef_revised),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      select(c("scenario", LEVEL2_DATA_NAMES[["TechCoef"]]))


    #==== OUTPUT ===========

    L270.TechCoef_EFW %>%
      add_title("Tech coefficients of energy-for-water processes") %>%
      add_units("Unitless (e.g., m^3/m^3)") %>%
      add_comments("Units of water flow from the EFW processes to the given technology, per unit output of the technology") %>%
      add_legacy_name("L245.Supplysector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/EFW_mapping",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L173.WWtrtfrac_R_ind_Yh",
                     "L174.WWtrtfrac_R_muni_Yh",
                     "L203.TechCoef_watertd") ->
      L270.TechCoef_EFW

    L270.TechCoef_WWtrt_SSP %>%
      add_title("Wastewater treatment fractions by scenario, region, year") %>%
      add_units("Unitless (e.g., m^3 of wastewater treated / m^3 of water withdrawn)") %>%
      add_comments("Units of water flow from the EFW processes to the given technology, per unit output of the technology") %>%
      same_precursors_as(L270.TechCoef_EFW) ->
      L270.TechCoef_WWtrt_SSP

    return_data(L270.TechCoef_EFW, L270.TechCoef_WWtrt_SSP)
  } else {
    stop("Unknown command")
  }
}
