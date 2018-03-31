#' module_energy_L226.en_distribution
#'
#' Generate the level 2 data tables for the energy distribution sector,
#' including capital costs, shareweights, logits, and interpolations as well as energy use coefficients for electricity and gas pipeline
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.SectorLogitTables[[ curr_table ]]$data}, \code{L226.Supplysector_en}, \code{L226.SubsectorLogitTables[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en}, \code{L226.SubsectorShrwt_en}, \code{L226.SubsectorShrwtFllt_en}, \code{L226.SubsectorInterp_en}, \code{L226.SubsectorInterpTo_en}, \code{L226.StubTech_en}, \code{L226.GlobalTechEff_en}, \code{L226.GlobalTechCost_en}, \code{L226.GlobalTechShrwt_en}, \code{L226.StubTechCoef_elecownuse}, \code{L226.StubTechCoef_electd}, \code{L226.StubTechCoef_gaspipe}. The corresponding file in the
#' original data system was \code{L226.en_distribution.R} (energy level2).
#' @details Prepares Level 2 data on energy distribution sector for the generation of en_distribution.xml.
#' Creates global technology database info--cost, shareweight, logit, efficiencies, and interpolations--and regional values where applicable for electricity net ownuse, gas pipelines, and transmission and distribution.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CWR August 2017
module_energy_L226.en_distribution <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
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

    # Silence global variable package check
    year <- year.fillout <- to.value <- technology <- efficiency <- supplysector <- subsector <-
      minicam.energy.input <- input.cost <- share.weight <- calibration <-
      secondary.output <- year.x <- year.y <- . <- value <- region <- coefficient <- GCAM_region_ID <-
      sector <- fuel <- minicam.non.energy.input <- elect_td_techchange <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
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

    # 2. Build tables for CSVs

    # 2a. Supplysector information
    # "L226.Supplysector_en: Supply sector information for energy distribution sectors"
    A26.sector %>%
      write_to_all_regions(c("region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent", LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L226.Supplysector_en

    # 2b. Subsector information
    # "L226.SubsectorLogit_en: Subsector logit exponents of energy distribution sectors"
    A26.subsector_logit %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent", LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L226.SubsectorLogit_en

    # set names for subsector shareweight and interpolation in energy distribution sectors
    NAMES_SUBSECTORSHRWT <- c("region", "supplysector", "subsector", "year", "share.weight")
    NAMES_SUBSECTORSHRWTFLLT <- c("region", "supplysector", "subsector", "year.fillout", "share.weight")
    NAMES_SUBSECTORINTERP <- c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "interpolation.function")
    NAMES_SUBSECTORINTERPTO <- c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")

    # Conditionally sorts subsector shareweights of energy distribution sectors

    if(any(!is.na(A26.subsector_shrwt$year))) {
      A26.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(NAMES_SUBSECTORSHRWT, GCAM_region_names) ->
        L226.SubsectorShrwt_en
    }

    if(any(!is.na(A26.subsector_shrwt$year.fillout))) {
      A26.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(NAMES_SUBSECTORSHRWTFLLT, GCAM_region_names) ->
        L226.SubsectorShrwtFllt_en
    }

    # Conditionally sorts interpolation of energy distribution sectors depending on whether they use a to.year or to.value to do the interpolation.

    if(any(is.na(A26.subsector_interp$to.value))) {
      A26.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(NAMES_SUBSECTORINTERP, GCAM_region_names) ->
        L226.SubsectorInterp_en
    }

    if(any(!is.na(A26.subsector_interp$to.value))) {
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

    # Set number of digits to round final values of elecownuse, electd, and elecgaspipe coefficients, globaltech efficiency, and globaltech cost
    DIGITS_COEFFICIENT <- 7
    DIGITS_COST <- 4
    DIGITS_EFFICIENCY <- 3

    # Generates L226.GlobalTechEff_en: Energy inputs and efficiencies of global technologies for energy distribution by interpolating values for all model years
    A26.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate(efficiency = approx_fun(as.numeric(year), efficiency)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(efficiency = round(efficiency, DIGITS_EFFICIENCY))->
      L226.GlobalTechEff_en

    # Generates L226.GlobalTechCost_en by interpolating values of cost adders for final energy delivery for all model years
    A26.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate (input.cost = approx_fun(as.numeric(year), input.cost)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(input.cost = round(input.cost, DIGITS_COST)) ->
      L226.GlobalTechCost_en

    # Generates L226.GlobalTechShrwt_en: Shareweights of global technologies for energy distribution by interpolating values for all model years
    A26.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector) %>%
      mutate (share.weight = approx_fun(as.numeric(year), share.weight)) %>%
      ungroup() %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L226.GlobalTechShrwt_en

    # 2d. Calibration and region-specific data
    # Electricity ownuse IO coefs - filter down to the base years and append region IDs
    L126.IO_R_elecownuse_F_Yh %>%
      ungroup() %>% # this data apparently came into this chunk grouped, which was preventing deletion of grouped columns
      filter(year %in% c(BASE_YEARS)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) ->
      L226.IO_R_elecownuse_F_Yh

    # repeat final year's ownuse ratio into future years and append future years to base years (could perhaps be tied to industrial CHP...but also AUTOELEC)
    L226.IO_R_elecownuse_F_Yh %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) %>%
      bind_rows(L226.IO_R_elecownuse_F_Yh) ->
      L226.IO_R_elecownuse_F_Y

    # rename columns and round coefficients - L226.StubTechCoef_elecownuse: calibrated coefficients on electricity net ownuse
    L226.IO_R_elecownuse_F_Y %>%
      rename(coefficient = value, stub.technology = technology) %>%
      mutate(market.name = region, coefficient = round(coefficient, DIGITS_COEFFICIENT)) %>%
      select(-GCAM_region_ID, -sector, -fuel) ->
      L226.StubTechCoef_elecownuse

    # Filter electricity transmission and distribution input-output ratio historical data down to base years (this works now but may need optional interpolation if the assumptions file changes not to include base model years)
    L126.IO_R_electd_F_Yh %>%
      filter(year %in% BASE_YEARS) ->
      L226.IO_R_electd_F_Yh

    # Copy final base year value to future periods
    L226.IO_R_electd_F_Yh %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L226.IO_R_electd_F_Yfut

    # append assumed techchange value and calculate decrease in future energy use
    L226.IO_R_electd_F_Yfut %>%
      left_join(A_regions, by = "GCAM_region_ID") %>%
      mutate(value = value * ((1 - elect_td_techchange) ^ (year - max(BASE_YEARS)))) %>%
      select(GCAM_region_ID, sector, fuel, value, year) %>%
      bind_rows(L226.IO_R_electd_F_Yh, .) ->
      L226.IO_R_electd_F_Y

    # Use base and future IO ratios to generate L226.StubTechCoef_electd: calibrated coefficients on electricity transmission and distribution
    # Electricity T&D: Because this is written out to multiple sectors, need to start with the list in calibrated_techs
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L226.IO_R_electd_F_Y$sector, L226.IO_R_electd_F_Y$fuel)) %>%
      # append region names and calibrated tech info
      repeat_add_columns(tibble("GCAM_region_ID" = unique(L226.IO_R_electd_F_Y$GCAM_region_ID))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble("year" = unique(L226.IO_R_electd_F_Y$year))) %>%
      left_join_error_no_match(L226.IO_R_electd_F_Y, by = c("sector", "fuel", "GCAM_region_ID", "year")) %>%
      # adjust column names and round values to clean up final table
      rename(stub.technology = technology, coefficient = value) %>%
      mutate(market.name = region) %>%
      select(-sector, -fuel, -calibration, -secondary.output, -GCAM_region_ID) %>%
      mutate(coefficient = round(coefficient, DIGITS_COEFFICIENT))->
      L226.StubTechCoef_electd

    # Interpolate regional gas pipeline IO coefs to generate future year values, add region names, and calibrated tech information
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

    # Generate future year gas pipeline energy use ratios by holding final base year value constant
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
      mutate(market.name = region, value = round(value, DIGITS_COEFFICIENT)) %>%
      rename(coefficient = value, stub.technology = technology) ->
      L226.StubTechCoef_gaspipe

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L226.Supplysector_en %>%
      add_title("regional supplysector information") %>%
      add_units("NA") %>%
      add_comments("copied A26.sector to all regions") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Supplysector_en") %>%
      add_precursors("energy/A26.sector", "common/GCAM_region_names") ->
      L226.Supplysector_en

    L226.SubsectorLogit_en %>%
      add_title("logits for energy distribution subsectors") %>%
      add_units("unitless") %>%
      add_comments("copied to all regions from A26.subsector_logit") %>%
      add_legacy_name("L226.SubsectorLogit_en") %>%
      add_precursors("energy/A26.subsector_logit", "common/GCAM_region_names") ->
      L226.SubsectorLogit_en

    if(exists("L226.SubsectorShrwt_en")) {
      L226.SubsectorShrwt_en %>%
        add_title("Regional energy distribution subsector shareweights") %>%
        add_units("units") %>%
        add_comments("generated if A26.subsector_shrwt uses 'year' values to generate subsector shareweights") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L226.SubsectorShrwt_en") %>%
        add_precursors("energy/A26.subsector_shrwt", "common/GCAM_region_names") ->
        L226.SubsectorShrwt_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L226.SubsectorShrwt_en") ->
        L226.SubsectorShrwt_en
    }

    if(exists("L226.SubsectorShrwtFllt_en")) {
      L226.SubsectorShrwtFllt_en %>%
        add_title("regional energy distribution subsector shareweights") %>%
        add_units("unitless") %>%
        add_comments("generated if A26.subsector_shrwt uses 'year.fillout' values to generate subsector shareweights") %>%
        add_legacy_name("L226.SubsectorShrwtFllt_en") %>%
        add_precursors("energy/A26.subsector_shrwt", "common/GCAM_region_names") ->
        L226.SubsectorShrwtFllt_en
    } else {
      missing_data() %>%
        add_legacy_name("L226.SubsectorShrwtFllt_en") ->
        L226.SubsectorShrwtFllt_en
    }

    if(exists("L226.SubsectorInterp_en")) {
      L226.SubsectorInterp_en %>%
        add_title("interpolation functions for subsector shareweights") %>%
        add_units("unitless") %>%
        add_comments("subsector shareweights interpolations generated when A26.subsector_interp uses to.year") %>%
        add_legacy_name("L226.SubsectorInterp_en") %>%
        add_precursors("energy/A26.subsector_interp", "common/GCAM_region_names") ->
        L226.SubsectorInterp_en
    } else {
      missing_data() %>%
        add_legacy_name("L226.SubsectorInterp_en") ->
        L226.SubsectorInterp_en
    }

    if(exists("L226.SubsectorInterpTo_en")) {
      L226.SubsectorInterpTo_en %>%
        add_title("interpolation functions for subsector shareweights") %>%
        add_units("unitless") %>%
        add_comments("subsector shareweights interpolations generated when A26.subsector_interp uses to.value") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L226.SubsectorInterpTo_en") %>%
        add_precursors("energy/A26.subsector_interp", "common/GCAM_region_names") ->
        L226.SubsectorInterpTo_en
    } else {
      missing_data() %>%
        add_legacy_name("L226.SubsectorInterpTo_en") ->
        L226.SubsectorInterpTo_en
    }

    L226.StubTech_en %>%
      add_title("stub technology database") %>%
      add_units("unitless") %>%
      add_comments("stub technology database generated from A26.globaltech_shrwt's technology list") %>%
      add_legacy_name("L226.StubTech_en") %>%
      add_precursors("energy/A26.globaltech_shrwt") ->
      L226.StubTech_en

    L226.GlobalTechEff_en %>%
      add_title("global energy distribution subsector efficiencies") %>%
      add_units("fractions") %>%
      add_comments("interpolated efficiency values for technologies converting energy inputs") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.GlobalTechEff_en") %>%
      add_precursors("energy/A26.globaltech_eff") ->
      L226.GlobalTechEff_en

    L226.GlobalTechCost_en %>%
      add_title("global final energy delivery subsector cost adders") %>%
      add_units("?") %>%
      add_comments("interpolated to all model years based on assumptions in A26.globaltech_cost") %>%
      add_legacy_name("L226.GlobalTechCost_en") %>%
      add_precursors("energy/A26.globaltech_cost") ->
      L226.GlobalTechCost_en

    L226.GlobalTechShrwt_en %>%
      add_title("global energy distribution technology shareweights") %>%
      add_units("unitless") %>%
      add_comments("interpolated to all model years based on assumptions in A26.globaltech_shrwt") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.GlobalTechShrwt_en") %>%
      add_precursors("energy/A26.globaltech_shrwt") ->
      L226.GlobalTechShrwt_en

    L226.StubTechCoef_elecownuse %>%
      add_title("Input-Output Coefficients for electricity sectors' own electricity use") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("future values generated from final base year as a fixed constant") %>%
      add_comments("historical values from L126.IO_R_elecownuse_F_Yh filtered to base model years") %>%
      add_legacy_name("L226.StubTechCoef_elecownuse") %>%
      add_precursors("L126.IO_R_elecownuse_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L226.StubTechCoef_elecownuse

    L226.StubTechCoef_electd %>%
      add_title("coefficients of electricity use for the transmission and distribution of electricity") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("future values generated from historical values from final base year of L126.IO_R_electd_F_Yh,") %>%
      add_comments("applying an assumed tech change from A_regions (new IO = base IO * 1-techchange ^ year-baseyear)") %>%
      add_legacy_name("L226.StubTechCoef_electd") %>%
      add_precursors("L126.IO_R_electd_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A_regions") ->
      L226.StubTechCoef_electd

    L226.StubTechCoef_gaspipe %>%
      add_title("Regional Gas pipeline I:O coefficients") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("input output coefficients from historical data and future values fixed constant from final base year") %>%
      add_legacy_name("L226.StubTechCoef_gaspipe") %>%
      add_precursors("L126.IO_R_gaspipe_F_Yh") ->
      L226.StubTechCoef_gaspipe

    return_data(L226.Supplysector_en, L226.SubsectorLogit_en, L226.SubsectorShrwt_en,
                L226.SubsectorShrwtFllt_en, L226.SubsectorInterp_en, L226.SubsectorInterpTo_en,
                L226.StubTech_en, L226.GlobalTechEff_en, L226.GlobalTechCost_en, L226.GlobalTechShrwt_en,
                L226.StubTechCoef_elecownuse, L226.StubTechCoef_electd, L226.StubTechCoef_gaspipe)
  } else {
    stop("Unknown command")
  }
}
