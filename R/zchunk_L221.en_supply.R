#' module_energy_L221.en_supply
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L221.SectorLogitTables[[ curr_table ]]$data}, \code{L221.Supplysector_en}, \code{L221.SectorUseTrialMarket_en}, \code{L221.SubsectorLogitTables[[ curr_table ]]$data}, \code{L221.SubsectorLogit_en}, \code{L221.SubsectorShrwt_en}, \code{L221.SubsectorShrwtFllt_en}, \code{L221.SubsectorInterp_en}, \code{L221.SubsectorInterpTo_en}, \code{L221.StubTech_en}, \code{L221.GlobalTechCoef_en}, \code{L221.GlobalTechCost_en}, \code{L221.GlobalTechShrwt_en}, \code{L221.PrimaryConsKeyword_en}, \code{L221.StubTechFractSecOut_en}, \code{L221.StubTechFractProd_en}, \code{L221.DepRsrc_en}, \code{L221.DepRsrcPrice_en}, \code{L221.TechCoef_en_Traded}, \code{L221.TechCost_en_Traded}, \code{L221.TechShrwt_en_Traded}, \code{L221.StubTechCoef_unoil}, \code{L221.Production_unoil}, \code{L221.StubTechProd_oil_unoil}, \code{L221.StubTechProd_oil_crude}, \code{L221.StubTechShrwt_bio}. The corresponding file in the
#' original data system was \code{L221.en_supply.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L221.en_supply_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A21.sector",
             FILE = "energy/A_regions",
             FILE = "energy/A21.subsector_logit",
             FILE = "energy/A21.subsector_shrwt",
             FILE = "energy/A21.subsector_interp",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A21.globaltech_cost",
             FILE = "energy/A21.globaltech_shrwt",
             FILE = "energy/A21.globaltech_keyword",
             FILE = "energy/A21.globaltech_secout",
             FILE = "energy/A21.rsrc_info",
             FILE = "energy/A21.tradedtech_coef",
             FILE = "energy/A21.tradedtech_cost",
             FILE = "energy/A21.tradedtech_shrwt",
             "L111.Prod_EJ_R_F_Yh",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             FILE = "aglu/A_an_input_subsector",
             "L108.ag_Feed_Mt_R_C_Y",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L221.Supplysector_en",
             "L221.SectorUseTrialMarket_en",
             "L221.SubsectorLogit_en",
             "L221.SubsectorShrwt_en",
             "L221.SubsectorShrwtFllt_en",
             "L221.SubsectorInterp_en",
             "L221.SubsectorInterpTo_en",
             "L221.StubTech_en",
             "L221.GlobalTechCoef_en",
             "L221.GlobalTechCost_en",
             "L221.GlobalTechShrwt_en",
             "L221.PrimaryConsKeyword_en",
             "L221.StubTechFractSecOut_en",
             "L221.StubTechFractProd_en",
             "L221.DepRsrc_en",
             "L221.DepRsrcPrice_en",
             "L221.TechCoef_en_Traded",
             "L221.TechCost_en_Traded",
             "L221.TechShrwt_en_Traded",
             "L221.StubTechCoef_unoil",
             "L221.Production_unoil",
             "L221.StubTechProd_oil_unoil",
             "L221.StubTechProd_oil_crude",
             "L221.StubTechShrwt_bio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A_regions <- get_data(all_data, "energy/A_regions")
    A21.subsector_logit <- get_data(all_data, "energy/A21.subsector_logit")
    A21.subsector_shrwt <- get_data(all_data, "energy/A21.subsector_shrwt")
    A21.subsector_interp <- get_data(all_data, "energy/A21.subsector_interp")
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A21.globaltech_shrwt <- get_data(all_data, "energy/A21.globaltech_shrwt")
    A21.globaltech_keyword <- get_data(all_data, "energy/A21.globaltech_keyword")
    A21.globaltech_secout <- get_data(all_data, "energy/A21.globaltech_secout")
    A21.rsrc_info <- get_data(all_data, "energy/A21.rsrc_info")
    A21.tradedtech_coef <- get_data(all_data, "energy/A21.tradedtech_coef")
    A21.tradedtech_cost <- get_data(all_data, "energy/A21.tradedtech_cost")
    A21.tradedtech_shrwt <- get_data(all_data, "energy/A21.tradedtech_shrwt")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
    L121.in_EJ_R_TPES_unoil_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh")
    L121.in_EJ_R_TPES_crude_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_crude_Yh")
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector")
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

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
      add_legacy_name("L221.Supplysector_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.Supplysector_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SectorUseTrialMarket_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SectorUseTrialMarket_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SubsectorLogit_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SubsectorLogit_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SubsectorShrwt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SubsectorShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SubsectorShrwtFllt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SubsectorShrwtFllt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SubsectorInterp_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SubsectorInterp_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.SubsectorInterpTo_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.SubsectorInterpTo_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTech_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTech_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.GlobalTechCoef_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.GlobalTechCoef_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.GlobalTechCost_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.GlobalTechCost_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.GlobalTechShrwt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.GlobalTechShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.PrimaryConsKeyword_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.PrimaryConsKeyword_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechFractSecOut_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechFractSecOut_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechFractProd_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechFractProd_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.DepRsrc_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.DepRsrc_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.DepRsrcPrice_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.DepRsrcPrice_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.TechCoef_en_Traded") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.TechCoef_en_Traded

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.TechCost_en_Traded") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.TechCost_en_Traded

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.TechShrwt_en_Traded") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.TechShrwt_en_Traded

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechCoef_unoil") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechCoef_unoil

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.Production_unoil") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.Production_unoil

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechProd_oil_unoil") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechProd_oil_unoil

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechProd_oil_crude") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechProd_oil_crude

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.StubTechShrwt_bio") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.StubTechShrwt_bio

    return_data(L221.Supplysector_en, L221.SectorUseTrialMarket_en, L221.SubsectorLogit_en,
                L221.SubsectorShrwt_en, L221.SubsectorShrwtFllt_en, L221.SubsectorInterp_en,
                L221.SubsectorInterpTo_en, L221.StubTech_en, L221.GlobalTechCoef_en,
                L221.GlobalTechCost_en, L221.GlobalTechShrwt_en, L221.PrimaryConsKeyword_en,
                L221.StubTechFractSecOut_en, L221.StubTechFractProd_en, L221.DepRsrc_en,
                L221.DepRsrcPrice_en, L221.TechCoef_en_Traded, L221.TechCost_en_Traded,
                L221.TechShrwt_en_Traded, L221.StubTechCoef_unoil, L221.Production_unoil,
                L221.StubTechProd_oil_unoil, L221.StubTechProd_oil_crude, L221.StubTechShrwt_bio)
  } else {
    stop("Unknown command")
  }
}
