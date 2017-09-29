#' module_gcam.usa_L2322.Fert_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.DeleteSubsector_USAFert}, \code{object}, \code{L2322.FinalEnergyKeyword_USAFert}, \code{L2322.SubsectorLogitTables_USAFert[[ curr_table ]]$data}, \code{L2322.SubsectorLogit_USAFert}, \code{L2322.SubsectorShrwtFllt_USAFert}, \code{L2322.SubsectorInterp_USAFert}, \code{L2322.TechShrwt_USAFert}, \code{L2322.Production_USAFert}, \code{L2322.TechCoef_USAFert}, \code{L2322.StubTechProd_Fert_USA}, \code{L2322.StubTechCoef_Fert_USA}, \code{L2322.StubTechMarket_Fert_USA}. The corresponding file in the
#' original data system was \code{L2322.Fert_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L2322.Fert_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.globaltech_coef",
          #   "L2322.Supplysector_Fert",
          FILE = "temp-data-inject/L2322.Supplysector_Fert",
          #   "L2322.FinalEnergyKeyword_Fert",
          FILE = "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
          #   "L2322.SubsectorLogit_Fert",
          FILE = "temp-data-inject/L2322.SubsectorLogit_Fert",
          #   "L2322.SubsectorShrwt_Fert", --- FLAG idk if this exsists in the old ds
          #   "L2322.SubsectorShrwtFllt_Fert",
          FILE = "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
          #   "L2322.SubsectorInterp_Fert",
          FILE = "temp-data-inject/L2322.SubsectorInterp_Fert",
          #  "L2322.SubsectorInterpTo_Fert", --- FLAG idk if this exists in the old ds
          #  "L2322.StubTech_Fert",
          FILE = "temp-data-inject/L2322.StubTech_Fert",
             "L1322.IO_GJkg_state_Fert_F_Yh", # product in the new ds
             "L1322.out_Mt_state_Fert_Yh")) # product in the new ds
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.DeleteSubsector_USAFert",
             "L2322.FinalEnergyKeyword_USAFert",
             "L2322.SubsectorLogit_USAFert",
             "L2322.SubsectorShrwtFllt_USAFert",
             "L2322.SubsectorInterp_USAFert",
             "L2322.TechShrwt_USAFert",
             "L2322.Production_USAFert",
             "L2322.TechCoef_USAFert",
             "L2322.StubTechProd_Fert_USA",
             "L2322.StubTechCoef_Fert_USA",
             "L2322.StubTechMarket_Fert_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")

    # L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert")
    L2322.Supplysector_Fert <- get_data(all_data, "temp-data-inject/L2322.Supplysector_Fert")


    # L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert")
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "temp-data-inject/L2322.FinalEnergyKeyword_Fert")

    # L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert")
    L2322.SubsectorLogit_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorLogit_Fert")

    # L2322.SubsectorShrwt_Fert <- get_data(all_data, "L2322.SubsectorShrwt_Fert")
    # L2322.SubsectorShrwt_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorShrwt_Fert")

    # L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert")
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorShrwtFllt_Fert")

    # L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert")
    L2322.SubsectorInterp_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorInterp_Fert")

    # L2322.SubsectorInterpTo_Fert <- get_data(all_data, "L2322.SubsectorInterpTo_Fert")
    # L2322.SubsectorInterpTo_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorInterpTo_Fert")

    # L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert")
    L2322.StubTech_Fert <- get_data(all_data, "temp-data-inject/L2322.StubTech_Fert")

    L1322.IO_GJkg_state_Fert_F_Yh <- get_data(all_data, "L1322.IO_GJkg_state_Fert_F_Yh")
    L1322.out_Mt_state_Fert_Yh <- get_data(all_data, "L1322.out_Mt_state_Fert_Yh")

    # ===================================================


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
      add_legacy_name("L2322.DeleteSubsector_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.DeleteSubsector_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.FinalEnergyKeyword_USAFert

      tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorLogit_USAFert") %>%
        add_precursors("gcam-usa/states_subregions",
                       "energy/calibrated_techs",
                       "energy/A322.globaltech_coef",
                       "temp-data-inject/L2322.Supplysector_Fert",
                       "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                       "temp-data-inject/L2322.SubsectorLogit_Fert",
                       "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                       "temp-data-inject/L2322.SubsectorInterp_Fert",
                       "temp-data-inject/L2322.StubTech_Fert",
                       "L1322.IO_GJkg_state_Fert_F_Yh",
                       "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorLogit_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorShrwtFllt_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorInterp_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorInterp_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.TechShrwt_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.TechShrwt_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.Production_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.Production_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.TechCoef_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.TechCoef_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechProd_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechProd_Fert_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechCoef_Fert_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechMarket_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechMarket_Fert_USA

    return_data(L2322.DeleteSubsector_USAFert,
                L2322.FinalEnergyKeyword_USAFert,
                L2322.SubsectorLogit_USAFert,
                L2322.SubsectorShrwtFllt_USAFert,
                L2322.SubsectorInterp_USAFert,
                L2322.TechShrwt_USAFert,
                L2322.Production_USAFert,
                L2322.TechCoef_USAFert,
                L2322.StubTechProd_Fert_USA,
                L2322.StubTechCoef_Fert_USA,
                L2322.StubTechMarket_Fert_USA)
  } else {
    stop("Unknown command")
  }
}
