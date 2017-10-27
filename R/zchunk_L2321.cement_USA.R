#' module_gcam.usa_L2321.cement_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.DeleteSupplysector_USAcement}, \code{L2321.DeleteFinalDemand_USAcement}, \code{object}, \code{L2321.StubTechProd_cement_USA}, \code{L2321.StubTechCoef_cement_USA}, \code{L2321.StubTechCalInput_cement_heat_USA}, \code{L2321.StubTechMarket_cement_USA}, \code{L2321.BaseService_cement_USA}. The corresponding file in the
#' original data system was \code{L2321.cement_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L2321.cement_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A321.demand",
             FILE = "energy/A321.globaltech_coef",
             "L2321.Supplysector_cement",
             "L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             "L2321.SubsectorShrwt_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             "L2321.SubsectorInterpTo_cement",
             "L2321.StubTech_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.PriceElasticity_cement",
             "L2321.IncomeElasticity_cement_gcam3",
             "L1321.in_EJ_state_cement_F_Y",
             "L1321.IO_GJkg_state_cement_F_Yh",
             "L1321.out_Mt_state_cement_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2321.DeleteSupplysector_USAcement",
             "L2321.DeleteFinalDemand_USAcement",
             "L2321.StubTechProd_cement_USA",
             "L2321.StubTechCoef_cement_USA",
             "L2321.StubTechCalInput_cement_heat_USA",
             "L2321.StubTechMarket_cement_USA",
             "L2321.BaseService_cement_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A321.demand <- get_data(all_data, "energy/A321.demand")
    A321.globaltech_coef <- get_data(all_data, "energy/A321.globaltech_coef")
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement")
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement")
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement")
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement")
    L2321.SubsectorShrwt_cement <- get_data(all_data, "L2321.SubsectorShrwt_cement")
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement")
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement")
    L2321.SubsectorInterpTo_cement <- get_data(all_data, "L2321.SubsectorInterpTo_cement")
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement")
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement")
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement")
    L2321.IncomeElasticity_cement_gcam3 <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3")
    L1321.in_EJ_state_cement_F_Y <- get_data(all_data, "L1321.in_EJ_state_cement_F_Y")
    L1321.IO_GJkg_state_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_state_cement_F_Yh")
    L1321.out_Mt_state_cement_Yh <- get_data(all_data, "L1321.out_Mt_state_cement_Yh")

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
      add_legacy_name("L2321.DeleteSupplysector_USAcement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.DeleteSupplysector_USAcement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.DeleteFinalDemand_USAcement") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.DeleteFinalDemand_USAcement

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechProd_cement_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechProd_cement_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechCoef_cement_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechCoef_cement_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechCalInput_cement_heat_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechCalInput_cement_heat_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.StubTechMarket_cement_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.StubTechMarket_cement_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2321.BaseService_cement_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2321.BaseService_cement_USA

    return_data(L2321.DeleteSupplysector_USAcement, L2321.DeleteFinalDemand_USAcement,
                L2321.StubTechProd_cement_USA, L2321.StubTechCoef_cement_USA, L2321.StubTechCalInput_cement_heat_USA,
                L2321.StubTechMarket_cement_USA, L2321.BaseService_cement_USA)
  } else {
    stop("Unknown command")
  }
}
