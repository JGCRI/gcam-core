#' module_energy_L2322.Fert
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.SectorLogitTables[[ curr_table ]]$data}, \code{L2322.Supplysector_Fert}, \code{L2322.FinalEnergyKeyword_Fert}, \code{L2322.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2322.SubsectorLogit_Fert}, \code{L2322.SubsectorShrwt_Fert}, \code{L2322.SubsectorShrwtFllt_Fert}, \code{L2322.SubsectorInterp_Fert}, \code{L2322.SubsectorInterpTo_Fert}, \code{L2322.StubTech_Fert}, \code{L2322.GlobalTechShrwt_Fert}, \code{L2322.GlobalTechCoef_Fert}, \code{L2322.GlobalTechCost_Fert}, \code{L2322.GlobalTechCapture_Fert}, \code{L2322.GlobalTechShutdown_Fert}, \code{L2322.GlobalTechSCurve_Fert}, \code{L2322.GlobalTechLifetime_Fert}, \code{L2322.GlobalTechProfitShutdown_Fert}, \code{L2322.StubTechProd_Fert}, \code{L2322.StubTechCoef_Fert}, \code{L2322.StubTechFixOut_Fert_imp}, \code{L2322.StubTechFixOut_Fert_exp}, \code{L2322.PerCapitaBased_Fert}, \code{L2322.BaseService_Fert}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L2322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.sector",
             FILE = "energy/A322.subsector_interp",
             FILE = "energy/A322.subsector_logit",
             FILE = "energy/A322.subsector_shrwt",
             FILE = "energy/A322.globaltech_coef",
             FILE = "energy/A322.globaltech_shrwt",
             FILE = "energy/A322.globaltech_co2capture",
             FILE = "energy/A322.globaltech_renew",
             FILE = "energy/A322.globaltech_retirement",
             "L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.Fert_NEcost_75USDkgN_F",
             "L142.ag_Fert_NetExp_MtN_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwt_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.SubsectorInterpTo_Fert",
             "L2322.StubTech_Fert",
             "L2322.GlobalTechShrwt_Fert",
             "L2322.GlobalTechCoef_Fert",
             "L2322.GlobalTechCost_Fert",
             "L2322.GlobalTechCapture_Fert",
             "L2322.GlobalTechShutdown_Fert",
             "L2322.GlobalTechSCurve_Fert",
             "L2322.GlobalTechLifetime_Fert",
             "L2322.GlobalTechProfitShutdown_Fert",
             "L2322.StubTechProd_Fert",
             "L2322.StubTechCoef_Fert",
             "L2322.StubTechFixOut_Fert_imp",
             "L2322.StubTechFixOut_Fert_exp",
             "L2322.PerCapitaBased_Fert",
             "L2322.BaseService_Fert"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.sector <- get_data(all_data, "energy/A322.sector")
    A322.subsector_interp <- get_data(all_data, "energy/A322.subsector_interp")
    A322.subsector_logit <- get_data(all_data, "energy/A322.subsector_logit")
    A322.subsector_shrwt <- get_data(all_data, "energy/A322.subsector_shrwt")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")
    A322.globaltech_shrwt <- get_data(all_data, "energy/A322.globaltech_shrwt")
    A322.globaltech_co2capture <- get_data(all_data, "energy/A322.globaltech_co2capture")
    A322.globaltech_renew <- get_data(all_data, "energy/A322.globaltech_renew")
    A322.globaltech_retirement <- get_data(all_data, "energy/A322.globaltech_retirement")
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh")
    L1322.Fert_NEcost_75USDkgN_F <- get_data(all_data, "L1322.Fert_NEcost_75USDkgN_F")
    L142.ag_Fert_NetExp_MtN_R_Y <- get_data(all_data, "L142.ag_Fert_NetExp_MtN_R_Y")

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
      add_legacy_name("L2322.Supplysector_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.Supplysector_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.FinalEnergyKeyword_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorLogit_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorShrwt_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorShrwt_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorShrwtFllt_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorInterp_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorInterpTo_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorInterpTo_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTech_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTech_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechShrwt_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechShrwt_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechCoef_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechCoef_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechCost_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechCost_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechCapture_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechCapture_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechShutdown_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechShutdown_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechSCurve_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechSCurve_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechLifetime_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechLifetime_Fert
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.GlobalTechProfitShutdown_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.GlobalTechProfitShutdown_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechProd_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechProd_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechCoef_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechCoef_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_imp") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechFixOut_Fert_imp

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_exp") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechFixOut_Fert_exp

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.PerCapitaBased_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.PerCapitaBased_Fert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.BaseService_Fert") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.BaseService_Fert

    return_data(L2322.Supplysector_Fert, L2322.FinalEnergyKeyword_Fert, L2322.SubsectorLogit_Fert,
                L2322.SubsectorShrwt_Fert, L2322.SubsectorShrwtFllt_Fert, L2322.SubsectorInterp_Fert,
                L2322.SubsectorInterpTo_Fert, L2322.StubTech_Fert, L2322.GlobalTechShrwt_Fert,
                L2322.GlobalTechCoef_Fert, L2322.GlobalTechCost_Fert, L2322.GlobalTechCapture_Fert,
                L2322.GlobalTechShutdown_Fert, L2322.GlobalTechSCurve_Fert, L2322.GlobalTechLifetime_Fert,
                L2322.GlobalTechProfitShutdown_Fert, L2322.StubTechProd_Fert, L2322.StubTechCoef_Fert,
                L2322.StubTechFixOut_Fert_imp, L2322.StubTechFixOut_Fert_exp, L2322.PerCapitaBased_Fert,
                L2322.BaseService_Fert)
  } else {
    stop("Unknown command")
  }
}
