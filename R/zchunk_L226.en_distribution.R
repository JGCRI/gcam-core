#' module_energy_L226.en_distribution
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.SectorLogitTables[[ curr_table ]]$data}, \code{L226.Supplysector_en}, \code{L226.SubsectorLogitTables[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en}, \code{L226.SubsectorShrwt_en}, \code{L226.SubsectorShrwtFllt_en}, \code{L226.SubsectorInterp_en}, \code{L226.SubsectorInterpTo_en}, \code{L226.StubTech_en}, \code{L226.GlobalTechEff_en}, \code{L226.GlobalTechCost_en}, \code{L226.GlobalTechShrwt_en}, \code{L226.StubTechCoef_elecownuse}, \code{L226.StubTechCoef_electd}, \code{L226.StubTechCoef_gaspipe}. The corresponding file in the
#' original data system was \code{L226.en_distribution.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L226.en_distribution <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/fuel_energy_input",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A26.sector",
             FILE = "energy/A26.subsector_logit",
             FILE = "energy/A26.subsector_shrwt",
             FILE = "energy/A26.subsector_interp",
             FILE = "energy/A26.globaltech_eff",
             FILE = "energy/A26.globaltech_cost",
             FILE = "energy/A26.globaltech_shrwt",
             "L126.IO_R_elecownuse_F_Yh",
             "L126.IO_R_electd_F_Yh",
             "L126.IO_R_gaspipe_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             "L226.SubsectorShrwt_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             "L226.SubsectorInterpTo_en",
             "L226.StubTech_en",
             "L226.GlobalTechEff_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             "L226.StubTechCoef_elecownuse",
             "L226.StubTechCoef_electd",
             "L226.StubTechCoef_gaspipe"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/mappings/fuel_energy_input")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A26.sector <- get_data(all_data, "energy/A26.sector")
    A26.subsector_logit <- get_data(all_data, "energy/A26.subsector_logit")
    A26.subsector_shrwt <- get_data(all_data, "energy/A26.subsector_shrwt")
    A26.subsector_interp <- get_data(all_data, "energy/A26.subsector_interp")
    A26.globaltech_eff <- get_data(all_data, "energy/A26.globaltech_eff")
    A26.globaltech_cost <- get_data(all_data, "energy/A26.globaltech_cost")
    A26.globaltech_shrwt <- get_data(all_data, "energy/A26.globaltech_shrwt")
    L126.IO_R_elecownuse_F_Yh <- get_data(all_data, "L126.IO_R_elecownuse_F_Yh")
    L126.IO_R_electd_F_Yh <- get_data(all_data, "L126.IO_R_electd_F_Yh")
    L126.IO_R_gaspipe_F_Yh <- get_data(all_data, "L126.IO_R_gaspipe_F_Yh")

    # ===================================================
    browser()
    # 2. Build tables for CSVs

    # 2a. Supplysector information
    # "L226.Supplysector_en: Supply sector information for energy distribution sectors"
    A26.sector %>%
      write_to_all_regions(c("region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent", "logit.type"), GCAM_region_names) ->
      L226.Supplysector_en

    # 2b. Subsector information
    # "L226.SubsectorLogit_en: Subsector logit exponents of energy distribution sectors"
    A26.subsector_logit %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent", "logit.type"), GCAM_region_names) ->
      L226.SubsectorLogit_en

    # set names for subsector shareweight and interpolation in energy distribution sectors
    NAMES_SUBSECTORSHRWT <- c("region", "supplysector", "subsector", "year", "share.weight")
    NAMES_SUBSECTORSHRWTFLLT <- c("region", "supplysector", "subsector", "year.fillout", "share.weight")
    NAMES_SUBSECTORINTERP <- c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "interpolation.function")
    NAMES_SUBSECTORINTERPTO <- c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")

    # L226.SubsectorShrwt_en and L226.SubsectorShrwtFllt_en: Subsector shareweights of energy distribution sectors

    if (any(!is.na(A26.subsector_shrwt$year))) {
      A26.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(NAMES_SUBSECTORSHRWT, GCAM_region_names) ->
        L226.SubsectorShrwt_en
    }

    if (any(!is.na(A26.subsector_shrwt$year.fillout))) {
      A26.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(NAMES_SUBSECTORSHRWTFLLT, GCAM_region_names) ->
        L226.SubsectorShrwtFllt_en
    }

    # L226.SubsectorInterp_en and L226.SubsectorInterpTo_en: Subsector shareweight interpolation of energy distribution sectors

    if (any(is.na(A26.subsector_interp$to.value))){
      A26.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(NAMES_SUBSECTORINTERP, GCAM_region_names) ->
        L226.SubsectorInterp_en
    }

    if (any(!is.na(A26.subsector_interp$to.value))) {
      A26.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(NAMES_SUBSECTORINTERPTO, GCAM_region_names) ->
        L226.SubsectorInterpTo_en
    }

    # 2c. Technology information
    # L226.StubTech_en: Identification of stub technologies of energy distribution
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A26.globaltech_shrwt %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology"), GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L226.StubTech_en

    #Efficiencies of global technologies
    #"L226.GlobalTechEff_en: Energy inputs and efficiencies of global technologies for energy distribution"
    A26.globaltech_eff %>%
      gather(year, efficiency, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate(efficiency = approx_fun(as.numeric(year), efficiency)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L226.GlobalTechEff_en

    # "L226.GlobalTechCost_en: Costs of global technologies for energy distribution"
    A26.globaltech_cost %>%
      gather(year, input.cost, -supplysector, -subsector, -technology, -minicam.non.energy.input) %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate (input.cost = approx_fun(as.numeric(year), input.cost)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      #Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L226.GlobalTechCost_en

    # "L226.GlobalTechShrwt_en: Shareweights of global technologies for energy distribution"
    A26.globaltech_shrwt %>%
      gather(year, share.weight, -supplysector, -subsector, -technology) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate (share.weight = approx_fun(as.numeric(year), share.weight)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      #Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L226.GlobalTechShrwt_en

    #2d. Calibration and region-specific data
    # Electricity ownuse IO coefs - filter down to the base years and append region IDs
    L126.IO_R_elecownuse_F_Yh %>%
      filter(year %in% c(BASE_YEARS)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) ->
      L226.IO_R_elecownuse_F_Yh

    #repeat final year's ownuse ratio into future years and append future years to base years (could perhaps be tied to industrial CHP...but also AUTOELEC)
    L226.IO_R_elecownuse_F_Yh %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) %>%
      bind_rows(L226.IO_R_elecownuse_F_Yh, .) ->
      L226.IO_R_elecownuse_F_Y

    #Set number of digits to round elecownuse coefficient
    DIGITS_COEFFICIENT <- 7

    #rename columns and round coefficients - L226.StubTechCoef_elecownuse: calibrated coefficients on electricity net ownuse
    L226.IO_R_elecownuse_F_Y %>%
      rename(coefficient = value, market.name = region, stub.technology = technology) %>%
      mutate(coefficient = round(coefficient, DIGITS_COEFFICIENT)) ->
      L226.StubTechCoef_elecownuse

    #Electricity transmission and distribution (#this works now but may need optional interpolation if the assumptions file changes)
    L126.IO_R_electd_F_Yh %>%
      filter(year %in% BASE_YEARS) ->
      L226.IO_R_electd_F_Yh

    # Copy to future periods
    L226.IO_R_electd_F_Yh %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L226.IO_R_electd_F_Yfut

    # append assumed techchange and calculate future values
    L226.IO_R_electd_F_Yfut %>%
      left_join(A_regions, by = "GCAM_region_ID") %>%
      mutate(value = value * (1 - elect_td_techchange ^ (year - max(BASE_YEARS)))) %>%
      select(GCAM_region_ID, sector, fuel, value, year) %>%
      bind_rows(L226.IO_R_electd_F_Yh, .) ->
      L226.IO_R_electd_F_Y

    # L226.StubTechCoef_electd: calibrated coefficients on electricity transmission and distribution
    # Electricity T&D: Because this is written out to multiple sectors, need to start with the list in calibrated_techs
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L226.IO_R_electd_F_Y$sector, L226.IO_R_electd_F_Y$fuel)) %>%
      repeat_add_columns(tibble("GCAM_region_ID" = unique(L226.IO_R_electd_F_Y$GCAM_region_ID))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble("year" = unique(L226.IO_R_electd_F_Y$year))) %>%
      left_join_error_no_match(L226.IO_R_electd_F_Y, by = c("sector", "fuel", "GCAM_region_ID", "year")) %>%
      rename(stub.technology = technology, coefficient = value) %>%
      mutate(market.name = region) %>%
      select(-sector, -fuel, -calibration, -secondary.output, -GCAM_region_ID) ->
      L226.StubTechCoef_electd

    # Gas pipeline IO coefs
    L126.IO_R_gaspipe_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, BASE_YEARS)) %>%
      arrange(GCAM_region_ID, sector, year) %>%
      group_by(GCAM_region_ID, sector) %>%
      mutate (value = approx_fun(as.numeric(year), value)) %>%
      ungroup() %>%
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output, -GCAM_region_ID, -sector, -fuel) ->
      L226.IO_R_gaspipe_F_Yh

    # Gas pipeline energy use ratios are held constant in the future
    L226.IO_R_gaspipe_F_Yh %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L226.IO_R_gaspipe_F_Yfut

    # L226.StubTechCoef_gaspipe: calibrated coefficients on gas pipeline energy use
    # Append future years to base years, add a market.name column, and round the values for the xml
    L226.IO_R_gaspipe_F_Yfut %>%
      bind_rows(L226.IO_R_gaspipe_F_Yh, .) %>%
      mutate(market.name = region, value = round(value, DIGITS_COEFFICIENT)) ->
      L226.StubTechCoef_gaspipe

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Supplysector_en") %>%
      add_precursors("A26.sector.csv") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.Supplysector_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorLogit_en") %>%
      add_precursors("A26.subsector_logit") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorLogit_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorShrwt_en") %>%
      add_precursors("A26.subsector_shrwt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en") %>%
      add_precursors("A26.subsector_shrwt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorShrwtFllt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorInterp_en") %>%
      add_precursors("A26.subsector_interp") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorInterp_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorInterpTo_en") %>%
      add_precursors("A26.subsector_interp") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorInterpTo_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTech_en") %>%
      add_precursors("A26.globaltech_shrwt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTech_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.GlobalTechEff_en") %>%
      add_precursors("L226.globaltech_coef") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.GlobalTechEff_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.GlobalTechCost_en") %>%
      add_precursors("A26.globaltech_cost") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.GlobalTechCost_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.GlobalTechShrwt_en") %>%
      add_precursors("A26.globaltech_shrwt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.GlobalTechShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTechCoef_elecownuse") %>%
      add_precursors("L126.IO_R_elecownuse_F_Yh", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTechCoef_elecownuse

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTechCoef_electd") %>%
      add_precursors("L126.IO_R_electd_F_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTechCoef_electd

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTechCoef_gaspipe") %>%
      add_precursors("L126.IO_R_gaspipe_F_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTechCoef_gaspipe

    return_data(L226.Supplysector_en, L226.SubsectorLogit_en, L226.SubsectorShrwt_en,
                L226.SubsectorShrwtFllt_en, L226.SubsectorInterp_en, L226.SubsectorInterpTo_en,
                L226.StubTech_en, L226.GlobalTechEff_en, L226.GlobalTechCost_en, L226.GlobalTechShrwt_en,
                L226.StubTechCoef_elecownuse, L226.StubTechCoef_electd, L226.StubTechCoef_gaspipe)
  } else {
    stop("Unknown command")
  }
}
