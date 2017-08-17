#' module_energy_L242.building_agg
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.SectorLogitTables[[ curr_table ]]$data}, \code{L242.Supplysector_bld}, \code{L242.FinalEnergyKeyword_bld}, \code{L242.SubsectorLogitTables[[ curr_table ]]$data}, \code{L242.SubsectorLogit_bld}, \code{L242.SubsectorShrwt_bld}, \code{L242.SubsectorShrwtFllt_bld}, \code{L242.SubsectorInterp_bld}, \code{L242.SubsectorInterpTo_bld}, \code{L242.StubTech_bld}, \code{L242.GlobalTechInterp_bld}, \code{L242.GlobalTechShrwt_bld}, \code{L242.GlobalTechEff_bld}, \code{L242.GlobalTechCost_bld}, \code{L242.StubTechCalInput_bld}, \code{L242.FuelPrefElast_bld}, \code{L242.PerCapitaBased_bld}, \code{L242.PriceElasticity_bld}, \code{L242.BaseService_bld}. The corresponding file in the
#' original data system was \code{L242.building_agg.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L242.building_agg_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/fuel_energy_input",
             FILE = "energy/calibrated_techs_bld_agg",
             FILE = "energy/A_regions",
             FILE = "energy/A42.sector",
             FILE = "energy/A42.subsector_interp",
             FILE = "energy/A42.subsector_logit",
             FILE = "energy/A42.subsector_shrwt",
             FILE = "energy/A42.globaltech_cost",
             FILE = "energy/A42.globaltech_eff",
             FILE = "energy/A42.globaltech_shrwt",
             FILE = "energy/A42.globaltech_interp",
             FILE = "energy/A42.fuelprefElasticity",
             FILE = "energy/A42.demand",
             "L124.in_EJ_R_heat_F_Yh",
             "L142.in_EJ_R_bld_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.Supplysector_bld",
             "L242.FinalEnergyKeyword_bld",
             "L242.SubsectorLogit_bld",
             "L242.SubsectorShrwt_bld",
             "L242.SubsectorShrwtFllt_bld",
             "L242.SubsectorInterp_bld",
             "L242.SubsectorInterpTo_bld",
             "L242.StubTech_bld",
             "L242.GlobalTechInterp_bld",
             "L242.GlobalTechShrwt_bld",
             "L242.GlobalTechEff_bld",
             "L242.GlobalTechCost_bld",
             "L242.StubTechCalInput_bld",
             "L242.FuelPrefElast_bld",
             "L242.PerCapitaBased_bld",
             "L242.PriceElasticity_bld",
             "L242.BaseService_bld"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
    calibrated_techs_bld_agg <- get_data(all_data, "energy/calibrated_techs_bld_agg")
    A_regions <- get_data(all_data, "energy/A_regions")
    A42.sector <- get_data(all_data, "energy/A42.sector")
    A42.subsector_interp <- get_data(all_data, "energy/A42.subsector_interp")
    A42.subsector_logit <- get_data(all_data, "energy/A42.subsector_logit")
    A42.subsector_shrwt <- get_data(all_data, "energy/A42.subsector_shrwt")
    A42.globaltech_cost <- get_data(all_data, "energy/A42.globaltech_cost")
    A42.globaltech_eff <- get_data(all_data, "energy/A42.globaltech_eff")
    A42.globaltech_shrwt <- get_data(all_data, "energy/A42.globaltech_shrwt")
    A42.globaltech_interp <- get_data(all_data, "energy/A42.globaltech_interp")
    A42.fuelprefElasticity <- get_data(all_data, "energy/A42.fuelprefElasticity")
    A42.demand <- get_data(all_data, "energy/A42.demand")
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L242.Supplysector_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.Supplysector_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.FinalEnergyKeyword_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.FinalEnergyKeyword_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.SubsectorLogit_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.SubsectorLogit_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.SubsectorShrwt_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.SubsectorShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.SubsectorShrwtFllt_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.SubsectorShrwtFllt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.SubsectorInterp_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.SubsectorInterp_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.SubsectorInterpTo_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.SubsectorInterpTo_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.StubTech_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.StubTech_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.GlobalTechInterp_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.GlobalTechInterp_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.GlobalTechShrwt_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.GlobalTechShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.GlobalTechEff_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.GlobalTechEff_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.GlobalTechCost_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.GlobalTechCost_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.StubTechCalInput_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.StubTechCalInput_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.FuelPrefElast_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.FuelPrefElast_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.PerCapitaBased_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.PerCapitaBased_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.PriceElasticity_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.PriceElasticity_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.BaseService_bld") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L242.BaseService_bld

    return_data(L242.Supplysector_bld, L242.FinalEnergyKeyword_bld, L242.SubsectorLogit_bld, L242.SubsectorShrwt_bld, L242.SubsectorShrwtFllt_bld, L242.SubsectorInterp_bld, L242.SubsectorInterpTo_bld, L242.StubTech_bld, L242.GlobalTechInterp_bld, L242.GlobalTechShrwt_bld, L242.GlobalTechEff_bld, L242.GlobalTechCost_bld, L242.StubTechCalInput_bld, L242.FuelPrefElast_bld, L242.PerCapitaBased_bld, L242.PriceElasticity_bld, L242.BaseService_bld)
  } else {
    stop("Unknown command")
  }
}
