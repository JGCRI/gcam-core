# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L274.EFW_municipal
#'
#' Generate municipal water sector energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L274.Supplysector_muni},
#'   \code{L274.FinalEnergyKeyword_muni}, \code{L274.SubsectorLogit_muni}, \code{L274.SubsectorShrwtFllt_muni},
#'   \code{L274.SubsectorInterpTo_muni}, \code{L274.StubTech_muni}, \code{L274.GlobalTechCoef_muni},
#'   \code{L274.GlobalTechShrwt_muni}, \code{L274.StubTechCoef_muni}. This did not
#'   exist in the original data system.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author GPK January 2019
module_water_L274.EFW_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/EFW_mapping",
             FILE = "water/A74.sector",
             FILE = "water/A74.subsector_interp",
             FILE = "water/A74.subsector_logit",
             FILE = "water/A74.subsector_shrwt",
             FILE = "water/A74.globaltech_coef",
             FILE = "water/A74.globaltech_shrwt",
             "L174.IO_GJkm3_R_muniEFW_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L274.Supplysector_muni",
             "L274.FinalEnergyKeyword_muni",
             "L274.SubsectorLogit_muni",
             "L274.SubsectorShrwt_muni",
             "L274.SubsectorShrwtFllt_muni",
             "L274.SubsectorInterp_muni",
             "L274.SubsectorInterpTo_muni",
             "L274.StubTech_muni",
             "L274.GlobalTechCoef_muni",
             "L274.GlobalTechShrwt_muni",
             "L274.StubTechCoef_muni"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- year.fillout <- to.value <- technology <- supplysector <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- sector <-
      fuel <- region <- coefficient.default <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    A74.sector <- get_data(all_data, "water/A74.sector", strip_attributes = TRUE)
    A74.subsector_interp <- get_data(all_data, "water/A74.subsector_interp", strip_attributes = TRUE)
    A74.subsector_logit <- get_data(all_data, "water/A74.subsector_logit", strip_attributes = TRUE)
    A74.subsector_shrwt <- get_data(all_data, "water/A74.subsector_shrwt", strip_attributes = TRUE)
    A74.globaltech_coef <- get_data(all_data, "water/A74.globaltech_coef", strip_attributes = TRUE)
    A74.globaltech_shrwt <- get_data(all_data, "water/A74.globaltech_shrwt", strip_attributes = TRUE)
    L174.IO_GJkm3_R_muniEFW_F_Yh <- get_data(all_data, "L174.IO_GJkm3_R_muniEFW_F_Yh", strip_attributes = TRUE)

    # ===================================================

    L274.Supplysector_muni <- write_to_all_regions(A74.sector,
                                                  c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                                                  GCAM_region_names)

    L274.FinalEnergyKeyword_muni <- write_to_all_regions(A74.sector,
                                                        LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                                                        GCAM_region_names)

    L274.SubsectorLogit_muni <- write_to_all_regions(A74.subsector_logit,
                                                    c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"),
                                                    GCAM_region_names)

    if(any(!is.na(A74.subsector_shrwt$year))) {
      L274.SubsectorShrwt_muni <- A74.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names )
    }

    if(any(!is.na(A74.subsector_shrwt$year.fillout))) {
      L274.SubsectorShrwtFllt_muni <- A74.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names )
    }

    if(any(is.na(A74.subsector_interp$to.value))) {
      L274.SubsectorInterp_muni <- A74.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names)
    }

    if(any(!is.na(A74.subsector_interp$to.value))) {
      L274.SubsectorInterpTo_muni <- A74.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names)
    }

    # Technology information
    L274.StubTech_muni <- rename(A74.globaltech_shrwt, stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTech"]], GCAM_region_names)

    # Energy inputs and coefficients of global technologies for municipal water sector energy-for-water
    L274.GlobalTechCoef_muni <- gather_years(A74.globaltech_coef, value_col = "coefficient") %>%
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

    # Shareweights of global technologies for municipal water sector energy-for-water
    L274.GlobalTechShrwt_muni <- gather_years(A74.globaltech_shrwt, value_col = "share.weight") %>%
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = round(approx_fun(year, share.weight), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]])

    # Calibrated output of municipal water sector energy-for-water by technology
    L274.StubTechCoef_muni <- filter(L174.IO_GJkm3_R_muniEFW_F_Yh, year %in% MODEL_BASE_YEARS) %>%
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
    L274.StubTechCoef_muni_future <- filter(L274.StubTechCoef_muni, year == max(MODEL_BASE_YEARS)) %>%
      left_join(L274.GlobalTechCoef_muni, by = c(supplysector = "sector.name", subsector = "subsector.name", stub.technology = "technology", "year"),
                suffix = c("", ".default")) %>%
      filter(coefficient > coefficient.default) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L274.StubTechCoef_muni <- bind_rows(L274.StubTechCoef_muni, L274.StubTechCoef_muni_future)


    #==== OUTPUT ===========

    L274.Supplysector_muni %>%
      add_title("Supplysector info for municipal water sector energy-for-water") %>%
      add_units("km^3/yr") %>%
      add_comments("Municipal water sector energy-for-water supplysector info") %>%
      add_precursors("common/GCAM_region_names", "water/A74.sector") ->
      L274.Supplysector_muni

    L274.FinalEnergyKeyword_muni %>%
      add_title("Final energy keywords for municipal water sector energy-for-water") %>%
      add_units("None") %>%
      add_comments("Irrigation water abstraction sector keywords") %>%
      add_precursors("common/GCAM_region_names", "water/A74.sector") ->
      L274.FinalEnergyKeyword_muni

    L274.SubsectorLogit_muni %>%
      add_title("Municipal water energy-for-water subsector logit exponents") %>%
      add_units("None") %>%
      add_comments("Pass-through (no competition)") %>%
      add_precursors("common/GCAM_region_names", "water/A74.subsector_logit") ->
      L274.SubsectorLogit_muni

    if(exists("L274.SubsectorShrwt_muni")) {
      L274.SubsectorShrwt_muni %>%
        add_title("Subsector (fuel) shareweights of municipal water energy-for-water sectors") %>%
        add_units("N/A") %>%
        add_comments("If year is not NA: Subsector shareweights for municipal water energy-for-water written to all regions") %>%
        add_precursors("water/A74.subsector_shrwt", "common/GCAM_region_names") ->
        L274.SubsectorShrwt_muni
    } else {
      missing_data() %>%
        add_precursors("water/A74.subsector_shrwt") ->
        L274.SubsectorShrwt_muni
    }

    if(exists("L274.SubsectorShrwtFllt_muni")) {
      L274.SubsectorShrwtFllt_muni %>%
        add_title("Subsector (fuel) shareweights of municipal water energy-for-water sectors with fillout year") %>%
        add_units("N/A") %>%
        add_comments("If year.fillout is not NA: Subsector shareweights for municipal water energy-for-water written to all regions") %>%
        add_precursors("water/A74.subsector_shrwt", "common/GCAM_region_names") ->
        L274.SubsectorShrwtFllt_muni
    } else {
      missing_data() %>%
        add_precursors("water/A74.subsector_shrwt") ->
        L274.SubsectorShrwtFllt_muni
    }

    if(exists("L274.SubsectorInterp_muni")) {
      L274.SubsectorInterp_muni %>%
        add_title("Subsector (fuel) shareweight interpolation of municipal water energy-for-water sectors") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation without a to-value specified") %>%
        add_precursors("water/A74.subsector_interp", "common/GCAM_region_names") ->
        L274.SubsectorInterp_muni
    } else {
      missing_data() %>%
        add_precursors("water/A74.subsector_interp") ->
        L274.SubsectorInterp_muni
    }

    if(exists("L274.SubsectorInterpTo_muni")) {
      L274.SubsectorInterpTo_muni %>%
        add_title("Subsector (fuel) shareweight interpolation of municipal water energy-for-water sectors using to.year") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation with a to-value specified") %>%
        add_precursors("water/A74.subsector_interp", "common/GCAM_region_names") ->
        L274.SubsectorInterpTo_muni
    } else {
      missing_data() %>%
        add_precursors("water/A74.subsector_interp") ->
        L274.SubsectorInterpTo_muni
    }

    L274.StubTech_muni %>%
      add_title("Stub technologies of municipal water energy-for-water") %>%
      add_units("N/A") %>%
      add_comments("Stub technology names written to all regions") %>%
      add_precursors("common/GCAM_region_names", "water/A74.globaltech_shrwt") ->
      L274.StubTech_muni

    L274.GlobalTechCoef_muni %>%
      add_title("Municipal water energy-for-water global technology coefficients") %>%
      add_units("GJ/m^3") %>%
      add_comments("Includes both thermal and reverse osmosis municipal water energy-for-water technologies") %>%
      add_precursors("common/GCAM_region_names", "water/A74.globaltech_coef") ->
      L274.GlobalTechCoef_muni

    L274.GlobalTechShrwt_muni %>%
      add_title("Municipal water energy-for-water global technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through") %>%
      add_precursors("common/GCAM_region_names", "water/A74.globaltech_shrwt") ->
      L274.GlobalTechShrwt_muni

    L274.StubTechCoef_muni %>%
      add_title("Calibrated energy-water input-output coefficients of municipal water energy-for-water") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect water source for abstraction, and available energy") %>%
      add_precursors("L174.IO_GJkm3_R_muniEFW_F_Yh", "water/EFW_mapping", "common/GCAM_region_names") ->
      L274.StubTechCoef_muni

    return_data(L274.Supplysector_muni,
                L274.FinalEnergyKeyword_muni,
                L274.SubsectorLogit_muni,
                L274.SubsectorShrwt_muni,
                L274.SubsectorShrwtFllt_muni,
                L274.SubsectorInterp_muni,
                L274.SubsectorInterpTo_muni,
                L274.StubTech_muni,
                L274.GlobalTechCoef_muni,
                L274.GlobalTechShrwt_muni,
                L274.StubTechCoef_muni)
  } else {
    stop("Unknown command")
  }
}
