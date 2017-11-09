#' module_energy_L225.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.Supplysector_h2}, \code{L225.SubsectorLogit_h2}, \code{L225.SubsectorShrwtFllt_h2}, \code{L225.StubTech_h2}, \code{L225.GlobalTechEff_h2}, \code{L225.GlobalTechCost_h2}, \code{L225.GlobalTechShrwt_h2}, \code{L225.PrimaryRenewKeyword_h2}, \code{L225.GlobalTechCapture_h2}. The corresponding file in the
#' original data system was \code{L225.hydrogen.R} (energy level2).
#' @details Provides supply sector information, subsector information, technology information for hydrogen sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author LF Augest 2017
module_energy_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A25.sector",
             FILE = "energy/A25.subsector_logit",
             FILE = "energy/A25.subsector_shrwt",
             FILE = "energy/A25.globaltech_eff",
             FILE = "energy/A25.globaltech_cost",
             FILE = "energy/A25.globaltech_shrwt",
             FILE = "energy/A25.globaltech_keyword",
             FILE = "energy/A25.globaltech_co2capture"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwt_h2",
             "L225.SubsectorInterp_h2",
             "L225.SubsectorInterpTo_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechEff_h2",
             "L225.GlobalTechCost_h2",
             "L225.GlobalTechShrwt_h2",
             "L225.PrimaryRenewKeyword_h2",
             "L225.AvgFossilEffKeyword_h2",
             "L225.GlobalTechCapture_h2"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year.fillout <- technology <- year <- efficiency <- supplysector <- value <-
      subsector <- minicam.energy.input <- input.cost <- minicam.non.energy.input <-
      share.weight <- primary.renewable <- average.fossil.efficiency <-
      remove.fraction <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A25.sector <- get_data(all_data, "energy/A25.sector")
    A25.subsector_logit <- get_data(all_data, "energy/A25.subsector_logit")
    A25.subsector_shrwt <- get_data(all_data, "energy/A25.subsector_shrwt")
    A25.globaltech_eff <- get_data(all_data, "energy/A25.globaltech_eff")
    A25.globaltech_cost <- get_data(all_data, "energy/A25.globaltech_cost")
    A25.globaltech_shrwt <- get_data(all_data, "energy/A25.globaltech_shrwt")
    A25.globaltech_keyword <- get_data(all_data, "energy/A25.globaltech_keyword")
    A25.globaltech_co2capture <- get_data(all_data, "energy/A25.globaltech_co2capture")

    # ===================================================

    # 1. Build tables for CSVs
    # 1a. Supply sector information

    # L225.Supplysector_h2: Supply sector information for hydrogen sectors
    A25.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Supplysector"]], GCAM_region_names) ->
      L225.Supplysector_h2

    # 1b. Subsector information

    # L225.SubsectorLogit_h2: Subsector logit exponents of hydrogen sectors
    A25.subsector_logit %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorLogit"]], GCAM_region_names) ->
      L225.SubsectorLogit_h2

    # L225.SubsectorShrwt_h2 and L225.SubsectorShrwtFllt_h2: Subsector shareweights of hydrogen sectors

    # L225.SubsectorShrwt_h2 is not created
    # L225.SubsectorShrwtFllt_h2 is not created

    A25.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L225.SubsectorShrwtFllt_h2

    # L225.SubsectorInterp_h2 and L225.SubsectorInterpTo_h2: Subsector shareweight interpolation of hydrogen sectors
    # These are not created currently


    # 1c. Technology information

    # L225.StubTech_h2: Identification of stub technologies of hydrogen
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A25.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L225.StubTech_h2

    # L225.GlobalTechEff_h2: Energy inputs and efficiencies of global technologies for hydrogen
    # Efficiencies of global technologies
    A25.globaltech_eff %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.numeric(year)) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency = approx_fun(year, value, rule = 1),
             efficiency = round(efficiency,energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechEff_h2

    # L225.GlobalTechCost_h2: Costs of global technologies for hydrogen
    # Costs of global technologies
    A25.globaltech_cost %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost,energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(BASE_YEARS,FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechCost_h2

    # L225.GlobalTechShrwt_h2: Shareweights of global technologies for hydrogen
    # Shareweights of global technologies
    A25.globaltech_shrwt %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(-value) ->
      L225.GlobalTechShrwt_h2

    # L225.PrimaryRenewKeyword_h2: Keywords of primary renewable electric generation technologies
    A25.globaltech_keyword %>%
      repeat_add_columns(tibble(year = c(BASE_YEARS, FUTURE_YEARS))) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L225.AllKeyword_h2

    L225.AllKeyword_h2 %>%
      filter(!is.na(primary.renewable)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "primary.renewable")) ->
      L225.PrimaryRenewKeyword_h2

    # L225.AvgFossilEffKeyword_h2: Keywords of fossil/bio electric generation technologies
    L225.AllKeyword_h2 %>%
      filter(!is.na(average.fossil.efficiency)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "average.fossil.efficiency")) ->
      L225.AvgFossilEffKeyword_h2

    # L225.GlobalTechCapture_h2: CO2 capture fractions from global fertilizer production technologies with CCS
    # Note: No need to consider historical periods or intermittent technologies here
    A25.globaltech_co2capture %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction,energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction")) %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L225.GlobalTechCapture_h2

    # ===================================================
    # Produce outputs

    L225.Supplysector_h2 %>%
      add_title("Supply sector information for hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand sector information for all GCAM regions") %>%
      add_legacy_name("L225.Supplysector_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.sector") ->
      L225.Supplysector_h2

    L225.SubsectorLogit_h2 %>%
      add_title("Subsector logit exponents of hydrogen sectors") %>%
      add_units("Unitless") %>%
      add_comments("Expand subsector logit exponents for all GCAM regions") %>%
      add_legacy_name("L225.SubsectorLogit_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.subsector_logit") ->
      L225.SubsectorLogit_h2

    if(exists("L225.SubsectorShrwt_h2")) {
      L225.SubsectorShrwt_h2 %>%
        add_title("Subsector shareweights of hydrogen sectors") %>%
        add_units("Unitless") %>%
        add_comments("Expand Subsector shareweights for all GCAM regions") %>%
        add_legacy_name("L225.SubsectorShrwt_h2") %>%
        add_precursors("common/GCAM_region_names", "energy/A25.subsector_shrwt") ->
        L225.SubsectorShrwt_h2
    } else {
      missing_data() %>%
        add_legacy_name("L225.SubsectorShrwt_h2") ->
        L225.SubsectorShrwt_h2
    }

    if(exists("L225.SubsectorShrwtFllt_h2")) {
      L225.SubsectorShrwtFllt_h2 %>%
        add_title("Subsector shareweights of hydrogen sectors") %>%
        add_units("Unitless") %>%
        add_comments("Expand Subsector shareweights for all GCAM regions") %>%
        add_legacy_name("L225.SubsectorShrwtFllt_h2") %>%
        add_precursors("common/GCAM_region_names", "energy/A25.subsector_shrwt") ->
        L225.SubsectorShrwtFllt_h2
    } else {
      missing_data() %>%
        add_legacy_name("L225.SubsectorShrwtFllt_h2") ->
        L225.SubsectorShrwtFllt_h2
    }

    if(exists("L225.SubsectorInterpTo_h2")) {
      L225.SubsectorInterp_h2 %>%
        add_title("Subsector shareweight interpolation of hydrogen sectors") %>%
        add_units("unitless") %>%
        add_comments("Expand Subsector shareweight interpolation for all GCAM regions") %>%
        add_legacy_name("L225.SubsectorInterp_h2") %>%
        add_precursors("common/GCAM_region_names") ->
        L225.SubsectorInterp_h2
    } else {
      missing_data() %>%
        add_legacy_name("L225.SubsectorInterp_h2") ->
        L225.SubsectorInterp_h2
    }

    if(exists("L225.SubsectorInterpTo_h2")) {
      L225.SubsectorInterpTo_h2 %>%
        add_title("Subsector shareweight interpolation of hydrogen sectors") %>%
        add_units("unitless") %>%
        add_comments("Expand Subsector shareweight interpolation for all GCAM regions") %>%
        add_legacy_name("L225.SubsectorInterpTo_h2") %>%
        add_precursors("common/GCAM_region_names") ->
        L225.SubsectorInterpTo_h2
    } else {
      missing_data() %>%
        add_legacy_name("L225.SubsectorInterpTo_h2") ->
        L225.SubsectorInterpTo_h2
    }

    L225.StubTech_h2 %>%
      add_title("Identification of stub technologies of hydrogen") %>%
      add_units("NA") %>%
      add_comments("Expand stub technologies information for all GCAM regions") %>%
      add_comments("assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)") %>%
      add_legacy_name("L225.StubTech_h2") %>%
      add_precursors("common/GCAM_region_names", "energy/A25.globaltech_shrwt") ->
      L225.StubTech_h2

    L225.GlobalTechEff_h2 %>%
      add_title("Energy inputs and efficiencies of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechEff_h2") %>%
      add_precursors("energy/A25.globaltech_eff") ->
      L225.GlobalTechEff_h2

    L225.GlobalTechCost_h2 %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCost_h2") %>%
      add_precursors("energy/A25.globaltech_cost")  ->
      L225.GlobalTechCost_h2

    L225.GlobalTechShrwt_h2 %>%
      add_title("Shareweights of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechShrwt_h2") %>%
      add_precursors("energy/A25.globaltech_shrwt") ->
      L225.GlobalTechShrwt_h2

    L225.PrimaryRenewKeyword_h2 %>%
      add_title("Keywords of primary renewable electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of primary renewable electric generation technologies for all model years") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L225.PrimaryRenewKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.PrimaryRenewKeyword_h2

    L225.AvgFossilEffKeyword_h2 %>%
      add_title("Keywords of fossil/bio electric generation technologies") %>%
      add_units("NA") %>%
      add_comments("Identify Keywords of fossil/bio electric generation technologies for all model years") %>%
      add_legacy_name("L225.AvgFossilEffKeyword_h2") %>%
      add_precursors("energy/A25.globaltech_keyword") ->
      L225.AvgFossilEffKeyword_h2

    L225.GlobalTechCapture_h2 %>%
      add_title("CO2 capture fractions from global fertilizer production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCapture_h2") %>%
      add_precursors("energy/A25.globaltech_co2capture")->
      L225.GlobalTechCapture_h2

    return_data(L225.Supplysector_h2, L225.SubsectorLogit_h2, L225.StubTech_h2,
                L225.GlobalTechEff_h2, L225.GlobalTechCost_h2, L225.GlobalTechShrwt_h2,
                L225.PrimaryRenewKeyword_h2, L225.AvgFossilEffKeyword_h2,
                L225.GlobalTechCapture_h2, L225.SubsectorShrwt_h2, L225.SubsectorShrwtFllt_h2,
                L225.SubsectorInterp_h2, L225.SubsectorInterpTo_h2)
  } else {
    stop("Unknown command")
  }
}
