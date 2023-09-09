# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L272.EFW_irrigation
#'
#' Generate irrigation energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L272.Supplysector_irr},
#'   \code{L272.FinalEnergyKeyword_irr}, \code{L272.SubsectorLogit_irr}, \code{L272.SubsectorShrwtFllt_irr},
#'   \code{L272.SubsectorInterpTo_irr}, \code{L272.StubTech_irr}, \code{L272.GlobalTechCoef_irr},
#'   \code{L272.GlobalTechShrwt_irr}, \code{L272.StubTechCoef_irr}. This did not
#'   exist in the original data system.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author GPK January 2019
module_water_L272.EFW_irrigation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/EFW_mapping",
             FILE = "water/A72.sector",
             FILE = "water/A72.subsector_interp",
             FILE = "water/A72.subsector_logit",
             FILE = "water/A72.subsector_shrwt",
             FILE = "water/A72.globaltech_coef",
             FILE = "water/A72.globaltech_shrwt",
             "L172.Coef_GJm3_IrrEnergy_R"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L272.Supplysector_irr",
             "L272.FinalEnergyKeyword_irr",
             "L272.SubsectorLogit_irr",
             "L272.SubsectorShrwt_irr",
             "L272.SubsectorShrwtFllt_irr",
             "L272.SubsectorInterp_irr",
             "L272.SubsectorInterpTo_irr",
             "L272.StubTech_irr",
             "L272.GlobalTechCoef_irr",
             "L272.GlobalTechShrwt_irr",
             "L272.StubTechCoef_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- year.fillout <- to.value <- technology <- supplysector <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- sector <-
      fuel <- region <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    A72.sector <- get_data(all_data, "water/A72.sector", strip_attributes = TRUE)
    A72.subsector_interp <- get_data(all_data, "water/A72.subsector_interp", strip_attributes = TRUE)
    A72.subsector_logit <- get_data(all_data, "water/A72.subsector_logit", strip_attributes = TRUE)
    A72.subsector_shrwt <- get_data(all_data, "water/A72.subsector_shrwt", strip_attributes = TRUE)
    A72.globaltech_coef <- get_data(all_data, "water/A72.globaltech_coef", strip_attributes = TRUE)
    A72.globaltech_shrwt <- get_data(all_data, "water/A72.globaltech_shrwt", strip_attributes = TRUE)
    L172.Coef_GJm3_IrrEnergy_R <- get_data(all_data, "L172.Coef_GJm3_IrrEnergy_R", strip_attributes = TRUE)

    # ===================================================

    L272.Supplysector_irr <- write_to_all_regions(A72.sector,
                                                  c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                                                  GCAM_region_names)

    L272.FinalEnergyKeyword_irr <- write_to_all_regions(A72.sector,
                                                        LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                                                        GCAM_region_names)

    L272.SubsectorLogit_irr <- write_to_all_regions(A72.subsector_logit,
                                                    c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"),
                                                    GCAM_region_names)

    if(any(!is.na(A72.subsector_shrwt$year))) {
      L272.SubsectorShrwt_irr <- A72.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names )
    }

    if(any(!is.na(A72.subsector_shrwt$year.fillout))) {
      L272.SubsectorShrwtFllt_irr <- A72.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names )
    }

    if(any(is.na(A72.subsector_interp$to.value))) {
      L272.SubsectorInterp_irr <- A72.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names)
    }

    if(any(!is.na(A72.subsector_interp$to.value))) {
      L272.SubsectorInterpTo_irr <- A72.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names)
    }

    # Technology information
    L272.StubTech_irr <- rename(A72.globaltech_shrwt, stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTech"]], GCAM_region_names)

    # Energy inputs and coefficients of global technologies for irrigation water abstraction
    L272.GlobalTechCoef_irr <- gather_years(A72.globaltech_coef, value_col = "coefficient") %>%
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

    # Shareweights of global technologies for irrigation water abstraction
    L272.GlobalTechShrwt_irr <- gather_years(A72.globaltech_shrwt, value_col = "share.weight") %>%
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = round(approx_fun(year, share.weight), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]])

    # Calibrated coefficients of irrigation water abstraction by technology, carried forward to all future years
    L272.StubTechCoef_irr <- filter(L172.Coef_GJm3_IrrEnergy_R, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # Copy the coefficients forward to future years
    L272.StubTechCoef_irr_future <- filter(L272.StubTechCoef_irr, year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    L272.StubTechCoef_irr <- bind_rows(L272.StubTechCoef_irr, L272.StubTechCoef_irr_future)


    #==== OUTPUT ===========

    L272.Supplysector_irr %>%
      add_title("Supplysector info for irrigation water abstraction") %>%
      add_units("km^3/yr") %>%
      add_comments("Irrigation water abstraction water production sector") %>%
      add_precursors("common/GCAM_region_names", "water/A72.sector") ->
      L272.Supplysector_irr

    L272.FinalEnergyKeyword_irr %>%
      add_title("Final energy keywords for irrigation water abstraction") %>%
      add_units("None") %>%
      add_comments("Irrigation water abstraction sector keywords") %>%
      add_precursors("common/GCAM_region_names", "water/A72.sector") ->
      L272.FinalEnergyKeyword_irr

    L272.SubsectorLogit_irr %>%
      add_title("Irrigation water abstraction subsector logit exponents") %>%
      add_units("None") %>%
      add_comments("Pass-through (no competition)") %>%
      add_precursors("common/GCAM_region_names", "water/A72.subsector_logit") ->
      L272.SubsectorLogit_irr

    if(exists("L272.SubsectorShrwt_irr")) {
      L272.SubsectorShrwt_irr %>%
        add_title("Subsector (fuel) shareweights of irrigation water abstraction sectors") %>%
        add_units("N/A") %>%
        add_comments("If year is not NA: Subsector shareweights for irrigation water abstraction written to all regions") %>%
        add_precursors("water/A72.subsector_shrwt", "common/GCAM_region_names") ->
        L272.SubsectorShrwt_irr
    } else {
      missing_data() %>%
        add_precursors("water/A72.subsector_shrwt") ->
        L272.SubsectorShrwt_irr
    }

    if(exists("L272.SubsectorShrwtFllt_irr")) {
      L272.SubsectorShrwtFllt_irr %>%
        add_title("Subsector (fuel) shareweights of irrigation water abstraction sectors with fillout year") %>%
        add_units("N/A") %>%
        add_comments("If year.fillout is not NA: Subsector shareweights for irrigation water abstraction written to all regions") %>%
        add_precursors("water/A72.subsector_shrwt", "common/GCAM_region_names") ->
        L272.SubsectorShrwtFllt_irr
    } else {
      missing_data() %>%
        add_precursors("water/A72.subsector_shrwt") ->
        L272.SubsectorShrwtFllt_irr
    }

    if(exists("L272.SubsectorInterp_irr")) {
      L272.SubsectorInterp_irr %>%
        add_title("Subsector (fuel) shareweight interpolation of irrigation water abstraction sectors") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation without a to-value specified") %>%
        add_precursors("water/A72.subsector_interp", "common/GCAM_region_names") ->
        L272.SubsectorInterp_irr
    } else {
      missing_data() %>%
        add_precursors("water/A72.subsector_interp") ->
        L272.SubsectorInterp_irr
    }

    if(exists("L272.SubsectorInterpTo_irr")) {
      L272.SubsectorInterpTo_irr %>%
        add_title("Subsector (fuel) shareweight interpolation of irrigation water abstraction sectors using to.year") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation with a to-value specified") %>%
        add_precursors("water/A72.subsector_interp", "common/GCAM_region_names") ->
        L272.SubsectorInterpTo_irr
    } else {
      missing_data() %>%
        add_precursors("water/A72.subsector_interp") ->
        L272.SubsectorInterpTo_irr
    }

    L272.StubTech_irr %>%
      add_title("Stub technologies of irrigation water abstraction sector") %>%
      add_units("N/A") %>%
      add_comments("Stub technology names written to all regions") %>%
      add_precursors("common/GCAM_region_names", "water/A72.globaltech_shrwt") ->
      L272.StubTech_irr

    L272.GlobalTechCoef_irr %>%
      add_title("Irrigation water abstraction global technology coefficients") %>%
      add_units("GJ/m^3") %>%
      add_comments("Includes both thermal and reverse osmosis irrigation water abstraction technologies") %>%
      add_precursors("common/GCAM_region_names", "water/A72.globaltech_coef") ->
      L272.GlobalTechCoef_irr

    L272.GlobalTechShrwt_irr %>%
      add_title("Irrigation water abstraction global technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through") %>%
      add_precursors("common/GCAM_region_names", "water/A72.globaltech_shrwt") ->
      L272.GlobalTechShrwt_irr

    L272.StubTechCoef_irr %>%
      add_title("Calibrated energy-water input-output coefficients of irrigation water abstraction") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect surface:ground water ratios and available energy") %>%
      add_precursors("L172.Coef_GJm3_IrrEnergy_R", "water/EFW_mapping", "common/GCAM_region_names") ->
      L272.StubTechCoef_irr

    return_data(L272.Supplysector_irr,
                L272.FinalEnergyKeyword_irr,
                L272.SubsectorLogit_irr,
                L272.SubsectorShrwt_irr,
                L272.SubsectorShrwtFllt_irr,
                L272.SubsectorInterp_irr,
                L272.SubsectorInterpTo_irr,
                L272.StubTech_irr,
                L272.GlobalTechCoef_irr,
                L272.GlobalTechShrwt_irr,
                L272.StubTechCoef_irr)
  } else {
    stop("Unknown command")
  }
}
