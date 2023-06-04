# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L271.desalination
#'
#' Generate sector and technologies for desalination
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L271.Supplysector_desal},
#'   \code{L271.FinalEnergyKeyword_desal}, \code{L271.SubsectorLogit_desal}, \code{L271.SubsectorShrwtFllt_desal},
#'   \code{L271.SubsectorInterpTo_desal}, \code{L271.StubTech_desal}, \code{L271.GlobalTechCoef_desal},
#'   \code{L271.GlobalTechCost_desal}, \code{L271.GlobalTechShrwt_desal}, \code{L271.StubTechProd_desal}. This did not
#'   exist in the original data system.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the production by technology in each
#'   region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by left_join mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author GPK January 2019
module_water_L271.desalination <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/EFW_mapping",
             FILE = "water/A71.sector",
             FILE = "water/A71.subsector_interp",
             FILE = "water/A71.subsector_logit",
             FILE = "water/A71.subsector_shrwt",
             FILE = "water/A71.globaltech_coef",
             FILE = "water/A71.globaltech_cost",
             FILE = "water/A71.globaltech_shrwt",
             "L171.out_km3_R_desal_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.Supplysector_desal",
             "L271.FinalEnergyKeyword_desal",
             "L271.SubsectorLogit_desal",
             "L271.SubsectorShrwt_desal",
             "L271.SubsectorShrwtFllt_desal",
             "L271.SubsectorInterp_desal",
             "L271.SubsectorInterpTo_desal",
             "L271.StubTech_desal",
             "L271.GlobalTechCoef_desal",
             "L271.GlobalTechCost_desal",
             "L271.GlobalTechShrwt_desal",
             "L271.StubTechProd_desal"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- supplysector <- year.fillout <- to.value <- technology <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- minicam.non.energy.input <-
      input.cost <- sector <- fuel <- desal_km3 <- calOutputValue <- subs.share.weight <-
      NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    A71.sector <- get_data(all_data, "water/A71.sector", strip_attributes = TRUE)
    A71.subsector_interp <- get_data(all_data, "water/A71.subsector_interp", strip_attributes = TRUE)
    A71.subsector_logit <- get_data(all_data, "water/A71.subsector_logit", strip_attributes = TRUE)
    A71.subsector_shrwt <- get_data(all_data, "water/A71.subsector_shrwt", strip_attributes = TRUE)
    A71.globaltech_coef <- get_data(all_data, "water/A71.globaltech_coef", strip_attributes = TRUE)
    A71.globaltech_cost <- get_data(all_data, "water/A71.globaltech_cost", strip_attributes = TRUE)
    A71.globaltech_shrwt <- get_data(all_data, "water/A71.globaltech_shrwt", strip_attributes = TRUE)
    L171.out_km3_R_desal_F_tech_Yh <- get_data(all_data, "L171.out_km3_R_desal_F_tech_Yh", strip_attributes = TRUE)

    # ===================================================

    L271.Supplysector_desal <- write_to_all_regions(A71.sector,
                                                    c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                                                    GCAM_region_names)

    L271.FinalEnergyKeyword_desal <- write_to_all_regions(A71.sector,
                                                          LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                                                          GCAM_region_names)

    L271.SubsectorLogit_desal <- write_to_all_regions(A71.subsector_logit,
                                                      c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"),
                                                      GCAM_region_names)

    if(any(!is.na(A71.subsector_shrwt$year))) {
      L271.SubsectorShrwt_desal <- A71.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names )
    }

    if(any(!is.na(A71.subsector_shrwt$year.fillout))) {
      L271.SubsectorShrwtFllt_desal <- A71.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names )
    }

    if(any(is.na(A71.subsector_interp$to.value))) {
      L271.SubsectorInterp_desal <- A71.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names)
    }

    if(any(!is.na(A71.subsector_interp$to.value))) {
      L271.SubsectorInterpTo_desal <- A71.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names)
    }

    # Technology information
    L271.StubTech_desal <- rename(A71.globaltech_shrwt, stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTech"]], GCAM_region_names)

    # Energy inputs and coefficients of global technologies for desalination
    L271.GlobalTechCoef_desal <- gather_years(A71.globaltech_coef, value_col = "coefficient") %>%
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

    # Shareweights of global technologies for desalination
    L271.GlobalTechShrwt_desal <- gather_years(A71.globaltech_shrwt, value_col = "share.weight") %>%
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = round(approx_fun(year, share.weight), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]])

    # Costs of global technologies for desalination
    L271.GlobalTechCost_desal <- gather_years(A71.globaltech_cost, value_col = "input.cost") %>%
      complete(year = unique(c(MODEL_YEARS, year)),
               nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      arrange(year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, input.cost), energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # Calibrated output of desalinated water by technology
    L271.StubTechProd_desal <- filter(L171.out_km3_R_desal_F_tech_Yh, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, sector, fuel),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology) %>%
      mutate(calOutputValue = round(desal_km3, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])


    #==== OUTPUT ===========

    L271.Supplysector_desal %>%
      add_title("Supplysector info for desalinated water") %>%
      add_units("km^3/yr") %>%
      add_comments("Desalinated water production sector") %>%
      add_precursors("common/GCAM_region_names", "water/A71.sector") ->
      L271.Supplysector_desal

    L271.FinalEnergyKeyword_desal %>%
      add_title("Final energy keywords for desalinated water") %>%
      add_units("None") %>%
      add_comments("Desalinated water sector keywords") %>%
      add_precursors("common/GCAM_region_names", "water/A71.sector") ->
      L271.FinalEnergyKeyword_desal

    L271.SubsectorLogit_desal %>%
      add_title("Desalinated water subsector logit exponents") %>%
      add_units("None") %>%
      add_comments("Pass-through (no competition)") %>%
      add_precursors("common/GCAM_region_names", "water/A71.subsector_logit") ->
      L271.SubsectorLogit_desal

    if(exists("L271.SubsectorShrwt_desal")) {
      L271.SubsectorShrwt_desal %>%
        add_title("Subsector (fuel) shareweights of desalination sectors") %>%
        add_units("N/A") %>%
        add_comments("If year is not NA: Subsector shareweights for desalination written to all regions") %>%
        add_precursors("water/A71.subsector_shrwt", "common/GCAM_region_names") ->
        L271.SubsectorShrwt_desal
    } else {
      missing_data() %>%
        add_precursors("water/A71.subsector_shrwt") ->
        L271.SubsectorShrwt_desal
    }

    if(exists("L271.SubsectorShrwtFllt_desal")) {
      L271.SubsectorShrwtFllt_desal %>%
        add_title("Subsector (fuel) shareweights of desalination sectors with fillout year") %>%
        add_units("N/A") %>%
        add_comments("If year.fillout is not NA: Subsector shareweights for desalination written to all regions") %>%
        add_precursors("water/A71.subsector_shrwt", "common/GCAM_region_names") ->
        L271.SubsectorShrwtFllt_desal
    } else {
      missing_data() %>%
        add_precursors("water/A71.subsector_shrwt") ->
        L271.SubsectorShrwtFllt_desal
    }

    if(exists("L271.SubsectorInterp_desal")) {
      L271.SubsectorInterp_desal %>%
        add_title("Subsector (fuel) shareweight interpolation of desalination sectors") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation without a to-value specified") %>%
        add_precursors("water/A71.subsector_interp", "common/GCAM_region_names") ->
        L271.SubsectorInterp_desal
    } else {
      missing_data() %>%
        add_precursors("water/A71.subsector_interp") ->
        L271.SubsectorInterp_desal
    }

    if(exists("L271.SubsectorInterpTo_desal")) {
      L271.SubsectorInterpTo_desal %>%
        add_title("Subsector (fuel) shareweight interpolation of desalination sectors using to.year") %>%
        add_units("Unitless") %>%
        add_comments("Subsector interpolation with a to-value specified") %>%
        add_precursors("water/A71.subsector_interp", "common/GCAM_region_names") ->
        L271.SubsectorInterpTo_desal
    } else {
      missing_data() %>%
        add_precursors("water/A71.subsector_interp") ->
        L271.SubsectorInterpTo_desal
    }

    L271.StubTech_desal %>%
      add_title("Stub technologies of desalinated water sector") %>%
      add_units("N/A") %>%
      add_comments("Stub technology names written to all regions") %>%
      add_precursors("common/GCAM_region_names", "water/A71.globaltech_shrwt") ->
      L271.StubTech_desal

    L271.GlobalTechCoef_desal %>%
      add_title("Desalinated water global technology coefficients") %>%
      add_units("GJ/m^3") %>%
      add_comments("Includes both thermal and reverse osmosis desalination technologies") %>%
      add_precursors("common/GCAM_region_names", "water/A71.globaltech_coef") ->
      L271.GlobalTechCoef_desal

    L271.GlobalTechShrwt_desal %>%
      add_title("Desalinated water global technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through") %>%
      add_precursors("common/GCAM_region_names", "water/A71.globaltech_shrwt") ->
      L271.GlobalTechShrwt_desal

    L271.GlobalTechCost_desal %>%
      add_title("Desalinated water global technology costs") %>%
      add_units("1975$/m3") %>%
      add_comments("Includes levelized capital and non-fuel operating costs of desalination") %>%
      add_precursors("common/GCAM_region_names", "water/A71.globaltech_cost") ->
      L271.GlobalTechCost_desal

    L271.StubTechProd_desal %>%
      add_title("Calibrated production of desalinated water by technology") %>%
      add_units("km^3/yr") %>%
      add_comments("Desalination-only plants (excludes combined electric + desal plants)") %>%
      add_precursors("L171.out_km3_R_desal_F_tech_Yh", "water/EFW_mapping", "common/GCAM_region_names") ->
      L271.StubTechProd_desal

    return_data(L271.Supplysector_desal,
                L271.FinalEnergyKeyword_desal,
                L271.SubsectorLogit_desal,
                L271.SubsectorShrwt_desal,
                L271.SubsectorShrwtFllt_desal,
                L271.SubsectorInterp_desal,
                L271.SubsectorInterpTo_desal,
                L271.StubTech_desal,
                L271.GlobalTechCoef_desal,
                L271.GlobalTechShrwt_desal,
                L271.GlobalTechCost_desal,
                L271.StubTechProd_desal)
  } else {
    stop("Unknown command")
  }
}
