# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L273.EFW_manufacturing
#'
#' Generate manufacturing energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L273.Supplysector_ind},
#'   \code{L273.FinalEnergyKeyword_ind}, \code{L273.SubsectorLogit_ind}, \code{L273.SubsectorShrwtFllt_ind},
#'   \code{L273.SubsectorInterpTo_ind}, \code{L273.StubTech_ind}, \code{L273.GlobalTechCoef_ind},
#'   \code{L273.GlobalTechShrwt_ind}, \code{L273.StubTechCoef_ind}. This did not
#'   exist in the original data system.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author GPK January 2019
module_water_L273.EFW_manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/EFW_mapping",
             FILE = "water/A73.sector",
             FILE = "water/A73.subsector_interp",
             FILE = "water/A73.subsector_logit",
             FILE = "water/A73.subsector_shrwt",
             FILE = "water/A73.globaltech_coef",
             FILE = "water/A73.globaltech_shrwt",
             "L173.IO_GJkm3_R_indEFW_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.Supplysector_ind",
             "L273.FinalEnergyKeyword_ind",
             "L273.SubsectorLogit_ind",
             "L273.SubsectorShrwt_ind",
             "L273.SubsectorShrwtFllt_ind",
             "L273.SubsectorInterp_ind",
             "L273.SubsectorInterpTo_ind",
             "L273.StubTech_ind",
             "L273.GlobalTechCoef_ind",
             "L273.GlobalTechShrwt_ind",
             "L273.StubTechCoef_ind"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- year.fillout <- to.value <- technology <- supplysector <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- sector <-
      fuel <- region <- coefficient.default <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    A73.sector <- get_data(all_data, "water/A73.sector", strip_attributes = TRUE)
    A73.subsector_interp <- get_data(all_data, "water/A73.subsector_interp", strip_attributes = TRUE)
    A73.subsector_logit <- get_data(all_data, "water/A73.subsector_logit", strip_attributes = TRUE)
    A73.subsector_shrwt <- get_data(all_data, "water/A73.subsector_shrwt", strip_attributes = TRUE)
    A73.globaltech_coef <- get_data(all_data, "water/A73.globaltech_coef", strip_attributes = TRUE)
    A73.globaltech_shrwt <- get_data(all_data, "water/A73.globaltech_shrwt", strip_attributes = TRUE)
    L173.IO_GJkm3_R_indEFW_F_Yh <- get_data(all_data, "L173.IO_GJkm3_R_indEFW_F_Yh", strip_attributes = TRUE)

    # ===================================================

    L273.Supplysector_ind <- write_to_all_regions(A73.sector,
                                                  c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                                                  GCAM_region_names)

    L273.FinalEnergyKeyword_ind <- write_to_all_regions(A73.sector,
                                                        LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                                                        GCAM_region_names)

    L273.SubsectorLogit_ind <- write_to_all_regions(A73.subsector_logit,
                                                    c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"),
                                                    GCAM_region_names)

    if(any(!is.na(A73.subsector_shrwt$year))) {
      L273.SubsectorShrwt_ind <- A73.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names )
    }

    if(any(!is.na(A73.subsector_shrwt$year.fillout))) {
      L273.SubsectorShrwtFllt_ind <- A73.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names )
    }

    if(any(is.na(A73.subsector_interp$to.value))) {
      L273.SubsectorInterp_ind <- A73.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names)
    }

    if(any(!is.na(A73.subsector_interp$to.value))) {
      L273.SubsectorInterpTo_ind <- A73.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names)
    }

    # Technology information
    L273.StubTech_ind <- rename(A73.globaltech_shrwt, stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTech"]], GCAM_region_names)

    # Energy inputs and coefficients of global technologies for manufacturing energy-for-water
    L273.GlobalTechCoef_ind <- gather_years(A73.globaltech_coef, value_col = "coefficient") %>%
      # Interpolate to all model years
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])

    # Shareweights of global technologies for manufacturing energy-for-water
    L273.GlobalTechShrwt_ind <- gather_years(A73.globaltech_shrwt, value_col = "share.weight") %>%
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = round(approx_fun(year, share.weight), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]])

    # Calibrated output of manufacturing sector energy-for-water by technology
    L273.StubTechCoef_ind <- filter(L173.IO_GJkm3_R_indEFW_F_Yh, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # If any of the stub technology coefficients are greater than the global technology defaults, copy them forward to all future years
    # (Coefs less than the global tech defaults are only that way because of base-year energy balancing, not something to carry forward)
    L273.StubTechCoef_ind_future <- filter(L273.StubTechCoef_ind, year == max(MODEL_BASE_YEARS)) %>%
      left_join(L273.GlobalTechCoef_ind, by = c(supplysector = "sector.name", subsector = "subsector.name", stub.technology = "technology", "year"),
                suffix = c("", ".default")) %>%
      filter(coefficient > coefficient.default) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L273.StubTechCoef_ind <- bind_rows(L273.StubTechCoef_ind, L273.StubTechCoef_ind_future)

    #==== OUTPUT ===========

    L273.Supplysector_ind %>%
      add_title("Supplysector info for manufacturing energy-for-water") %>%
      add_units("km^3/yr") %>%
      add_comments("Manufacturing energy-for-water abstraction water production sector") %>%
      add_precursors("common/GCAM_region_names", "water/A73.sector") ->
      L273.Supplysector_ind

    L273.FinalEnergyKeyword_ind %>%
      add_title("Final energy keywords for manufacturing energy-for-water") %>%
      add_units("None") %>%
      add_comments("Irrigation water abstraction sector keywords") %>%
      add_precursors("common/GCAM_region_names", "water/A73.sector") ->
      L273.FinalEnergyKeyword_ind

    L273.SubsectorLogit_ind %>%
      add_title("Manufacturing energy-for-water subsector logit exponents") %>%
      add_units("None") %>%
      add_comments("Pass-through (no competition)") %>%
      add_precursors("common/GCAM_region_names", "water/A73.subsector_logit") ->
      L273.SubsectorLogit_ind

    if(exists("L273.SubsectorShrwt_ind")) {
      L273.SubsectorShrwt_ind %>%
        add_title("Subsector (fuel) shareweights of manufacturing energy-for-water sectors") %>%
        add_units("N/A") %>%
        add_comments("If year is not NA: Subsector shareweights for manufacturing energy-for-water written to all regions") %>%
        add_precursors("water/A73.subsector_shrwt", "common/GCAM_region_names") ->
        L273.SubsectorShrwt_ind
    } else {
      missing_data() %>%
        add_precursors("water/A73.subsector_shrwt") ->
        L273.SubsectorShrwt_ind
    }

    if(exists("L273.SubsectorShrwtFllt_ind")) {
      L273.SubsectorShrwtFllt_ind %>%
        add_title("Subsector (fuel) shareweights of manufacturing energy-for-water sectors with fillout year") %>%
        add_units("N/A") %>%
        add_comments("If year.fillout is not NA: Subsector shareweights for manufacturing energy-for-water written to all regions") %>%
        add_precursors("water/A73.subsector_shrwt", "common/GCAM_region_names") ->
        L273.SubsectorShrwtFllt_ind
    } else {
      missing_data() %>%
        add_precursors("water/A73.subsector_shrwt") ->
        L273.SubsectorShrwtFllt_ind
    }

    if(exists("L273.SubsectorInterp_ind")) {
      L273.SubsectorInterp_ind %>%
        add_title("Subsector (fuel) shareweight interpolation of manufacturing energy-for-water sectors") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation without a to-value specified") %>%
        add_precursors("water/A73.subsector_interp", "common/GCAM_region_names") ->
        L273.SubsectorInterp_ind
    } else {
      missing_data() %>%
        add_precursors("water/A73.subsector_interp") ->
        L273.SubsectorInterp_ind
    }

    if(exists("L273.SubsectorInterpTo_ind")) {
      L273.SubsectorInterpTo_ind %>%
        add_title("Subsector (fuel) shareweight interpolation of manufacturing energy-for-water sectors using to.year") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation with a to-value specified") %>%
        add_precursors("water/A73.subsector_interp", "common/GCAM_region_names") ->
        L273.SubsectorInterpTo_ind
    } else {
      missing_data() %>%
        add_precursors("water/A73.subsector_interp") ->
        L273.SubsectorInterpTo_ind
    }

    L273.StubTech_ind %>%
      add_title("Stub technologies of manufacturing energy-for-water") %>%
      add_units("N/A") %>%
      add_comments("Stub technology names written to all regions") %>%
      add_precursors("common/GCAM_region_names", "water/A73.globaltech_shrwt") ->
      L273.StubTech_ind

    L273.GlobalTechCoef_ind %>%
      add_title("Manufacturing energy-for-water global technology coefficients") %>%
      add_units("GJ/m^3") %>%
      add_comments("Includes both thermal and reverse osmosis manufacturing energy-for-water technologies") %>%
      add_precursors("common/GCAM_region_names", "water/A73.globaltech_coef") ->
      L273.GlobalTechCoef_ind

    L273.GlobalTechShrwt_ind %>%
      add_title("Manufacturing energy-for-water global technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through") %>%
      add_precursors("common/GCAM_region_names", "water/A73.globaltech_shrwt") ->
      L273.GlobalTechShrwt_ind

    L273.StubTechCoef_ind %>%
      add_title("Calibrated energy-water input-output coefficients of manufacturing energy-for-water") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect water source for abstraction, and available energy") %>%
      add_precursors("L173.IO_GJkm3_R_indEFW_F_Yh", "water/EFW_mapping", "common/GCAM_region_names") ->
      L273.StubTechCoef_ind

    return_data(L273.Supplysector_ind,
                L273.FinalEnergyKeyword_ind,
                L273.SubsectorLogit_ind,
                L273.SubsectorShrwt_ind,
                L273.SubsectorShrwtFllt_ind,
                L273.SubsectorInterp_ind,
                L273.SubsectorInterpTo_ind,
                L273.StubTech_ind,
                L273.GlobalTechCoef_ind,
                L273.GlobalTechShrwt_ind,
                L273.StubTechCoef_ind)
  } else {
    stop("Unknown command")
  }
}
