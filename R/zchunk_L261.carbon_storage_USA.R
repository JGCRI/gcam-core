#' module_gcam.usa_L261.carbon_storage_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.DeleteDepRsrc_USAC}, \code{L261.DeleteSubsector_USAC}, \code{L261.DepRsrc_FERC}, \code{L261.DepRsrcCurves_FERC}, \code{L261.SectorLogitTables_C_USA[[ curr_table ]]}, \code{L261.Supplysector_C_USA}, \code{L261.SubsectorLogitTables_C_USA[[ curr_table ]]}, \code{L261.SubsectorLogit_C_USA}, \code{L261.SubsectorShrwtFllt_C_USA}, \code{L261.StubTech_C_USA}, \code{L261.StubTechMarket_C_USA}. The corresponding file in the
#' original data system was \code{L261.carbon_storage_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L261.carbon_storage_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L161.Cstorage_FERC",
             "L261.DepRsrc",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L261.DeleteDepRsrc_USAC",
             "L261.DeleteSubsector_USAC",
             "L261.DepRsrc_FERC",
             "L261.DepRsrcCurves_FERC",
             "L261.Supplysector_C_USA",
             "L261.SubsectorLogit_C_USA",
             "L261.SubsectorShrwtFllt_C_USA",
             "L261.StubTech_C_USA",
             "L261.StubTechMarket_C_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L161.Cstorage_FERC <- get_data(all_data, "L161.Cstorage_FERC")
    L261.DepRsrc <- get_data(all_data, "L261.DepRsrc")
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C")
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C")
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C")
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C")
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C")

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
      add_legacy_name("L261.DeleteDepRsrc_USAC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.DeleteDepRsrc_USAC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DeleteSubsector_USAC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.DeleteSubsector_USAC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DepRsrc_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.DepRsrc_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DepRsrcCurves_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.DepRsrcCurves_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.Supplysector_C_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.Supplysector_C_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.SubsectorLogit_C_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.SubsectorLogit_C_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.SubsectorShrwtFllt_C_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.StubTech_C_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.StubTech_C_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.StubTechMarket_C_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L261.StubTechMarket_C_USA

    return_data(L261.DeleteDepRsrc_USAC, L261.DeleteSubsector_USAC, L261.DepRsrc_FERC,
                L261.DepRsrcCurves_FERC, L261.Supplysector_C_USA, L261.SubsectorLogit_C_USA,
                L261.SubsectorShrwtFllt_C_USA, L261.StubTech_C_USA, L261.StubTechMarket_C_USA)
  } else {
    stop("Unknown command")
  }
}
