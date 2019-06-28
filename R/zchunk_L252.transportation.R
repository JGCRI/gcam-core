# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L252.transportation
#'
#' Calculate supply sector, subsector, and technology information for the transportation sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.SectorLogitTables[[ curr_table ]]$data}, \code{L252.Supplysector_trn}, \code{L252.FinalEnergyKeyword_trn}, \code{L252.SubsectorLogitTables[[ curr_table ]]$data}, \code{L252.SubsectorLogit_trn}, \code{L252.SubsectorShrwt_trn}, \code{L252.SubsectorShrwtFllt_trn}, \code{L252.SubsectorInterp_trn}, \code{L252.SubsectorInterpTo_trn}, \code{L252.StubTech_trn}, \code{L252.GlobalTechShrwt_trn}, \code{L252.GlobalTechEff_trn}, \code{L252.GlobalTechCost_trn}, \code{L252.StubTechCalInput_trn}, \code{L252.PerCapitaBased_trn}, \code{L252.PriceElasticity_trn}, \code{L252.BaseService_trn}. The corresponding file in the
#' original data system was \code{L252.transportation.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the transportation sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr gather spread
#' @author AJS August 2017
module_energy_L252.transportation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/calibrated_techs_trn_agg",
             FILE = "energy/A52.sector",
             FILE = "energy/A52.subsector_interp",
             FILE = "energy/A52.subsector_logit",
             FILE = "energy/A52.subsector_shrwt",
             FILE = "energy/A52.globaltech_cost",
             FILE = "energy/A52.globaltech_eff",
             FILE = "energy/A52.globaltech_shrwt",
             FILE = "energy/A52.demand",
             "L152.in_EJ_R_trn_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L252.Supplysector_trn",
             "L252.FinalEnergyKeyword_trn",
             "L252.SubsectorLogit_trn",
             "L252.SubsectorShrwt_trn",
             "L252.SubsectorShrwtFllt_trn",
             "L252.SubsectorInterp_trn",
             "L252.SubsectorInterpTo_trn",
             "L252.StubTech_trn",
             "L252.GlobalTechShrwt_trn",
             "L252.GlobalTechEff_trn",
             "L252.GlobalTechCost_trn",
             "L252.StubTechCalInput_trn",
             "L252.PerCapitaBased_trn",
             "L252.PriceElasticity_trn",
             "L252.BaseService_trn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs_trn_agg <- get_data(all_data, "energy/mappings/calibrated_techs_trn_agg")
    A52.sector <- get_data(all_data, "energy/A52.sector")
    A52.subsector_interp <- get_data(all_data, "energy/A52.subsector_interp")
    A52.subsector_logit <- get_data(all_data, "energy/A52.subsector_logit")
    A52.subsector_shrwt <- get_data(all_data, "energy/A52.subsector_shrwt")
    A52.globaltech_cost <- get_data(all_data, "energy/A52.globaltech_cost")
    A52.globaltech_eff <- get_data(all_data, "energy/A52.globaltech_eff")
    A52.globaltech_shrwt <- get_data(all_data, "energy/A52.globaltech_shrwt")
    A52.demand <- get_data(all_data, "energy/A52.demand")
    L152.in_EJ_R_trn_F_Yh <- get_data(all_data, "L152.in_EJ_R_trn_F_Yh")

    # ===================================================

    # Silence Package Notes
    region <- supplysector <- output.unit <- input.unit <- price.unit <- logit.year.fillout <-
      logit.exponent <- final.energy <- subsector <- logit.type <- logit.exponent <- year <-
      share.weight <- year.fillout <- apply.to <- from.year <- to.year <- interpolation.function <-
      to.value <- technology <- stub.technology <- sector.name <- subsector.name <- minicam.energy.input <-
      efficiency <- minicam.non.energy.input <- input.cost <- calibrated.value <- share.weight.year <-
      subs.share.weight <- tech.share.weight <- energy.final.demand <- perCapitaBased <-
      price.elasticity <- GAM_region_ID <- base.serive <- coefficient <- curr_table <- value <-
      . <- output <- base.service <- NULL

    # PART A: SUPPLYSECTOR INFORMATION
    # Write supply sector information for transportation sector for all regions
    A52.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L252.Supplysector_trn # OUTPUT

    # Write supply sector keywords for transportation sector for all regions
    A52.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                           GCAM_region_names = GCAM_region_names) ->
      L252.FinalEnergyKeyword_trn # OUTPUT


    # PART B: SUBSECTOR INFORMATION
    # Write subsector logit exponents of transportation sector for all regions
    A52.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L252.SubsectorLogit_trn # OUTPUT

    # Expand subsector shareweights of transportation sector across all regions
    if(any(!is.na(A52.subsector_shrwt$year))) {
      A52.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]],
                             GCAM_region_names = GCAM_region_names) ->
        L252.SubsectorShrwt_trn # OUTPUT
    }

    if(any(!is.na(A52.subsector_shrwt$year.fillout))) {
      A52.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                             GCAM_region_names = GCAM_region_names) ->
        L252.SubsectorShrwtFllt_trn # OUTPUT
    }

    # Write subsector shareweight interpolation of transportation sector for all regions
    if(any(is.na(A52.subsector_interp$to.value))) {
      A52.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                             GCAM_region_names = GCAM_region_names) ->
        L252.SubsectorInterp_trn # OUTPUT
    }

    if(any(!is.na(A52.subsector_interp$to.value))) {
      A52.subsector_interp %>%
        filter(is.na(!to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]],
                             GCAM_region_names = GCAM_region_names) ->
        L252.SubsectorInterpTo_trn # OUTPUT (not generated at this time)
    }


    # PART C: TECHNOLOGY INFORMATION
    # Write stub technologies of the transportation sector for all regions.
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A52.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names = GCAM_region_names) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L252.StubTech_trn # OUTPUT

    # Interpolate shareweights of the global transportation sector technologies across model years
    A52.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) ->
      L252.GlobalTechShrwt_trn # OUTPUT

    # Calculate energy inputs and coefficients of global transportation energy use and feedstocks technologies
    # for all model years
    DIGITS_EFFICIENCY <- 3

    A52.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(efficiency = round(efficiency, digits = DIGITS_EFFICIENCY)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.energy.input, year, efficiency) ->
      L252.GlobalTechEff_trn # OUTPUT

    # Costs of global technologies
    # Capital costs of global transportation technologies were interpolated across model years
    A52.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.non.energy.input, year, input.cost) ->
      L252.GlobalTechCost_trn # OUTPUT

    # Calibration and region-specific data
    # L252.StubTechCalInput_trn: calibrated input of transportation energy use technologies (including cogen)
    L152.in_EJ_R_trn_F_Yh %>%
      # Expand table to include all model base years
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Match in supplysector, subsector, technology
      left_join_error_no_match(calibrated_techs_trn_agg, by = c("sector", "fuel")) %>%
      # Aggregate as indicated in the supplysector/subsector/technology mapping (dropping fuel)
      group_by(region, supplysector, subsector, stub.technology = technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L252.in_EJ_R_trn_F_Yh

    DIGITS_CALOUTPUT <- 7

    L252.in_EJ_R_trn_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      # Match in minicam.energy.input
      left_join_error_no_match(A52.globaltech_eff, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, digits = DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L252.StubTechCalInput_trn # OUTPUT

    # Write per-capita based flag for transportation final demand for all regions
    A52.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]],
                           GCAM_region_names = GCAM_region_names) ->
      L252.PerCapitaBased_trn # OUTPUT

    # Expand price elasticity of transportation final demand across all regions and future years.
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    A52.demand %>%
      repeat_add_columns(tibble(MODEL_FUTURE_YEARS)) %>%
      rename(year = MODEL_FUTURE_YEARS) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]],
                           GCAM_region_names = GCAM_region_names) ->
      L252.PriceElasticity_trn # OUTPUT

    # Base-year service output of transportation final demand
    # Base service is equal to the output of the transportation supplysector
    L252.StubTechCalInput_trn %>%
      left_join_error_no_match(L252.GlobalTechEff_trn, by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                                              "stub.technology" = "technology", "minicam.energy.input",
                                                              "share.weight.year" = "year")) %>%
      mutate(output = calibrated.value * efficiency) %>%
      group_by(region, supplysector, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() %>%
      mutate(base.service = round(output, digits = DIGITS_CALOUTPUT)) %>%
      select(region, energy.final.demand = supplysector, year, base.service) ->
      L252.BaseService_trn # OUTPUT

    ## NOTE: income elasticities are GDP-dependent and are set in the socioeconomics module

    # ===================================================

    L252.Supplysector_trn %>%
      add_title("Supply sector information for transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information for transportation sector was expanded across all regions") %>%
      add_legacy_name("L252.Supplysector_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.sector") ->
      L252.Supplysector_trn

    L252.FinalEnergyKeyword_trn %>%
      add_title("Supply sector keywords for transportation sector") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for transportation sector were written for all regions") %>%
      add_legacy_name("L252.FinalEnergyKeyword_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.sector") ->
      L252.FinalEnergyKeyword_trn

    L252.SubsectorLogit_trn %>%
      add_title("Subsector logit exponents of transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector logit exponents of transportation sector were written for all regions") %>%
      add_legacy_name("L252.SubsectorLogit_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.subsector_logit") ->
      L252.SubsectorLogit_trn

    if(exists("L252.SubsectorShrwt_trn")) {
      L252.SubsectorShrwt_trn %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions") %>%
        add_comments("Only rows with an entry for year were selected") %>%
        add_legacy_name("L252.SubsectorShrwt_trn") %>%
        add_precursors("common/GCAM_region_names", "energy/A52.subsector_shrwt") ->
        L252.SubsectorShrwt_trn
     } else {
       missing_data() %>%
        add_legacy_name("L252.SubsectorShrwt_trn") ->
        L252.SubsectorShrwt_trn
     }

    if(exists("L252.SubsectorShrwtFllt_trn")) {
      L252.SubsectorShrwtFllt_trn %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions") %>%
        add_comments("Only rows with an entry for year.fillout were selected") %>%
        add_legacy_name("L252.SubsectorShrwtFllt_trn") %>%
        add_precursors("common/GCAM_region_names", "energy/A52.subsector_shrwt") ->
        L252.SubsectorShrwtFllt_trn
    } else {
      missing_data() %>%
        add_legacy_name("L252.SubsectorShrwtFllt_trn") ->
        L252.SubsectorShrwtFllt_trn
    }

    if(exists("L252.SubsectorInterp_trn")) {
      L252.SubsectorInterp_trn %>%
        add_title("Subsector shareweight interpolation data of transportation sector") %>%
        add_units("NA") %>%
        add_comments("Subsector shareweight interpolation data of transportation sector were written for all regions") %>%
        add_comments("Only rows without an entry for to.value were selected") %>%
        add_legacy_name("L252.SubsectorInterp_trn") %>%
        add_precursors("common/GCAM_region_names", "energy/A52.subsector_interp") ->
        L252.SubsectorInterp_trn
    } else {
      missing_data() %>%
        add_legacy_name("L252.SubsectorInterp_trn") ->
        L252.SubsectorInterp_trn
    }

    if(exists("L252.SubsectorInterpTo_trn")) {
      L252.SubsectorInterpTo_trn %>%
        add_title("Subsector shareweight interpolation data of transportation sector") %>%
        add_units("NA") %>%
        add_comments("Subsector shareweight interpolation data of transportation sector were written for all regions") %>%
        add_comments("Only rows with an entry for to.value were selected") %>%
        add_legacy_name("L252.SubsectorInterpTo_trn") %>%
        add_precursors("common/GCAM_region_names", "energy/A52.subsector_interp") ->
        L252.SubsectorInterpTo_trn
    } else {
      missing_data() %>%
        add_legacy_name("L252.SubsectorInterpTo_trn") ->
        L252.SubsectorInterpTo_trn
    }

    L252.StubTech_trn %>%
      add_title("Identification of stub technologies of transportation sector") %>%
      add_units("NA") %>%
      add_comments("Identification of stub technologies of transportation sector were written across all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L252.StubTech_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.globaltech_shrwt") ->
      L252.StubTech_trn

    L252.GlobalTechShrwt_trn %>%
      add_title("Shareweights of global transportation sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated shareweight data across model years") %>%
      add_legacy_name("L252.GlobalTechShrwt_trn") %>%
      add_precursors("energy/A52.globaltech_shrwt") ->
      L252.GlobalTechShrwt_trn

    L252.GlobalTechEff_trn %>%
      add_title("Energy inputs and coefficients of global transportation energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated data across model years") %>%
      add_legacy_name("L252.GlobalTechEff_trn") %>%
      add_precursors("energy/A52.globaltech_eff") ->
      L252.GlobalTechEff_trn

    L252.GlobalTechCost_trn %>%
      add_title("Capital costs of global transportation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Capital costs were interpolated across model years") %>%
      add_legacy_name("L252.GlobalTechCost_trn") %>%
      add_precursors("energy/A52.globaltech_cost") ->
      L252.GlobalTechCost_trn

    L252.StubTechCalInput_trn %>%
      add_title("Calibrated input of transportation energy use technologies (including cogen)") %>%
      add_units("EJ") %>%
      add_comments("Data was aggregated (dropping fuel) and shareweights were determined from the calibrated value") %>%
      add_legacy_name("L252.StubTechCalInput_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/calibrated_techs_trn_agg",
                     "energy/A52.globaltech_eff", "L152.in_EJ_R_trn_F_Yh") ->
      L252.StubTechCalInput_trn

    L252.PerCapitaBased_trn %>%
      add_title("Per-capita based flag for transportation final demand") %>%
      add_units("Unitless") %>%
      add_comments("Data was expanded across all regions") %>%
      add_legacy_name("L252.PerCapitaBased_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand") ->
      L252.PerCapitaBased_trn

    L252.PriceElasticity_trn %>%
      add_title("Price elasticity of transportation final demand") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity data were written for all regions and only applied to future model years") %>%
      add_legacy_name("L252.PriceElasticity_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand") ->
      L252.PriceElasticity_trn

    L252.BaseService_trn %>%
      add_title("Base-year service output of transportation final demand") %>%
      add_units("EJ") %>%
      add_comments("Base service is equal to the output of the transportation supplysector") %>%
      add_legacy_name("L252.BaseService_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/calibrated_techs_trn_agg",
                     "energy/A52.globaltech_eff", "L152.in_EJ_R_trn_F_Yh") ->
      L252.BaseService_trn

    return_data(L252.Supplysector_trn, L252.FinalEnergyKeyword_trn, L252.SubsectorLogit_trn, L252.SubsectorShrwt_trn, L252.SubsectorShrwtFllt_trn, L252.SubsectorInterp_trn, L252.SubsectorInterpTo_trn, L252.StubTech_trn, L252.GlobalTechShrwt_trn, L252.GlobalTechEff_trn, L252.GlobalTechCost_trn, L252.StubTechCalInput_trn, L252.PerCapitaBased_trn, L252.PriceElasticity_trn, L252.BaseService_trn)
  } else {
    stop("Unknown command")
  }
}
