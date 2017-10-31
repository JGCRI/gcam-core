#' module_energy_L2321.cement
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.SectorLogitTables[[ curr_table ]]$data}, \code{L2321.Supplysector_cement}, \code{L2321.FinalEnergyKeyword_cement}, \code{L2321.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2321.SubsectorLogit_cement}, \code{L2321.SubsectorShrwt_cement}, \code{L2321.SubsectorShrwtFllt_cement}, \code{L2321.SubsectorInterp_cement}, \code{L2321.SubsectorInterpTo_cement}, \code{L2321.StubTech_cement}, \code{L2321.GlobalTechShrwt_cement}, \code{L2321.GlobalTechCoef_cement}, \code{L2321.GlobalTechCost_cement}, \code{L2321.GlobalTechCapture_cement}, \code{L2321.StubTechProd_cement}, \code{L2321.StubTechCalInput_cement_heat}, \code{L2321.StubTechCoef_cement}, \code{L2321.PerCapitaBased_cement}, \code{L2321.BaseService_cement}, \code{L2321.PriceElasticity_cement}, \code{object}. The corresponding file in the
#' original data system was \code{L2321.cement.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L2321.cement_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A321.sector",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/A321.sector",
             FILE = "energy/A321.subsector_interp",
             FILE = "energy/A321.subsector_logit",
             FILE = "energy/A321.subsector_shrwt",
             FILE = "energy/A321.globaltech_coef",
             FILE = "energy/A321.globaltech_cost",
             FILE = "energy/A321.globaltech_shrwt",
             FILE = "energy/A321.globaltech_co2capture",
             FILE = "energy/A321.demand",
             "L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y",
             FILE = "socioeconomics/A321.inc_elas_output",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             "L2321.SubsectorShrwt_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             "L2321.SubsectorInterpTo_cement",
             "L2321.StubTech_cement",
             "L2321.GlobalTechShrwt_cement",
             "L2321.GlobalTechCoef_cement",
             "L2321.GlobalTechCost_cement",
             "L2321.GlobalTechCapture_cement",
             "L2321.StubTechProd_cement",
             "L2321.StubTechCalInput_cement_heat",
             "L2321.StubTechCoef_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.BaseService_cement",
             "L2321.PriceElasticity_cement"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A321.sector <- get_data(all_data, "energy/A321.sector")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    A321.sector <- get_data(all_data, "energy/A321.sector")
    A321.subsector_interp <- get_data(all_data, "energy/A321.subsector_interp")
    A321.subsector_logit <- get_data(all_data, "energy/A321.subsector_logit")
    A321.subsector_shrwt <- get_data(all_data, "energy/A321.subsector_shrwt")
    A321.globaltech_coef <- get_data(all_data, "energy/A321.globaltech_coef")
    A321.globaltech_cost <- get_data(all_data, "energy/A321.globaltech_cost")
    A321.globaltech_shrwt <- get_data(all_data, "energy/A321.globaltech_shrwt")
    A321.globaltech_co2capture <- get_data(all_data, "energy/A321.globaltech_co2capture")
    A321.demand <- get_data(all_data, "energy/A321.demand")
    L1321.out_Mt_R_cement_Yh <- get_data(all_data, "L1321.out_Mt_R_cement_Yh")
    L1321.IO_GJkg_R_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_R_cement_F_Yh")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "L1321.in_EJ_R_cement_F_Y")
    A321.inc_elas_output <- get_data(all_data, "socioeconomics/A321.inc_elas_output")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L102.pcgdp_thous90USD_GCAM3_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_R_Y")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

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
      add_legacy_name("L2321.Supplysector_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.Supplysector_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.FinalEnergyKeyword_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.FinalEnergyKeyword_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.SubsectorLogit_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.SubsectorLogit_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.SubsectorShrwt_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.SubsectorShrwt_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.SubsectorShrwtFllt_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.SubsectorInterp_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.SubsectorInterp_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.SubsectorInterpTo_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.SubsectorInterpTo_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTech_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTech_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.GlobalTechShrwt_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.GlobalTechShrwt_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.GlobalTechCoef_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.GlobalTechCoef_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.GlobalTechCost_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.GlobalTechCost_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.GlobalTechCapture_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.GlobalTechCapture_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechProd_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechProd_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechCalInput_cement_heat") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechCalInput_cement_heat

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechCoef_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechCoef_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.PerCapitaBased_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.PerCapitaBased_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.BaseService_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.BaseService_cement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.PriceElasticity_cement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.PriceElasticity_cement

    return_data(L2321.Supplysector_cement, L2321.FinalEnergyKeyword_cement, L2321.SubsectorLogit_cement,
                L2321.SubsectorShrwt_cement, L2321.SubsectorShrwtFllt_cement, L2321.SubsectorInterp_cement,
                L2321.SubsectorInterpTo_cement, L2321.StubTech_cement, L2321.GlobalTechShrwt_cement,
                L2321.GlobalTechCoef_cement, L2321.GlobalTechCost_cement, L2321.GlobalTechCapture_cement,
                L2321.StubTechProd_cement, L2321.StubTechCalInput_cement_heat, L2321.StubTechCoef_cement,
                L2321.PerCapitaBased_cement, L2321.BaseService_cement, L2321.PriceElasticity_cement)
  } else {
    stop("Unknown command")
  }
}
