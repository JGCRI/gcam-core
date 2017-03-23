#' module_energy_L261.Cstorage
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.DepRsrc}, \code{L261.UnlimitRsrc}, \code{L261.DepRsrcCurves_C}, \code{L261.SectorLogitTables[[ curr_table ]]$data}, \code{L261.Supplysector_C}, \code{L261.SubsectorLogitTables[[ curr_table ]]$data}, \code{L261.SubsectorLogit_C}, \code{L261.SubsectorShrwtFllt_C}, \code{L261.StubTech_C}, \code{L261.GlobalTechCoef_C}, \code{L261.GlobalTechCost_C}, \code{L261.GlobalTechShrwt_C}, \code{L261.GlobalTechCost_C_High}, \code{L261.GlobalTechShrwt_C_nooffshore}, \code{L261.DepRsrcCurves_C_high}, \code{L261.DepRsrcCurves_C_low}, \code{L261.DepRsrcCurves_C_lowest}. The corresponding file in the
#' original data system was \code{L261.Cstorage.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L261.Cstorage_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/A61.rsrc_info",
FILE = "energy/A61.sector",
FILE = "energy/A61.subsector_logit",
FILE = "energy/A61.subsector_shrwt",
FILE = "energy/A61.globaltech_coef",
FILE = "energy/A61.globaltech_cost",
FILE = "energy/A61.globaltech_shrwt",
 "L161.RsrcCurves_MtC_R"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L261.DepRsrc",
XML = "L261.UnlimitRsrc",
XML = "L261.DepRsrcCurves_C",
XML = "L261.SectorLogitTables[[ curr_table ]]$data",
XML = "L261.Supplysector_C",
XML = "L261.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L261.SubsectorLogit_C",
XML = "L261.SubsectorShrwtFllt_C",
XML = "L261.StubTech_C",
XML = "L261.GlobalTechCoef_C",
XML = "L261.GlobalTechCost_C",
XML = "L261.GlobalTechShrwt_C",
XML = "L261.GlobalTechCost_C_High",
XML = "L261.GlobalTechShrwt_C_nooffshore",
XML = "L261.DepRsrcCurves_C_high",
XML = "L261.DepRsrcCurves_C_low",
XML = "L261.DepRsrcCurves_C_lowest"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A61.rsrc_info <- get_data(all_data, "energy/A61.rsrc_info")
  A61.sector <- get_data(all_data, "energy/A61.sector")
  A61.subsector_logit <- get_data(all_data, "energy/A61.subsector_logit")
  A61.subsector_shrwt <- get_data(all_data, "energy/A61.subsector_shrwt")
  A61.globaltech_coef <- get_data(all_data, "energy/A61.globaltech_coef")
  A61.globaltech_cost <- get_data(all_data, "energy/A61.globaltech_cost")
  A61.globaltech_shrwt <- get_data(all_data, "energy/A61.globaltech_shrwt")
  L161.RsrcCurves_MtC_R <- get_data(all_data, "L161.RsrcCurves_MtC_R")

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
 add_legacy_name("L261.DepRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.DepRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.UnlimitRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.UnlimitRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.DepRsrcCurves_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.DepRsrcCurves_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.Supplysector_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.Supplysector_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.SubsectorLogit_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.SubsectorLogit_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.SubsectorShrwtFllt_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.SubsectorShrwtFllt_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.StubTech_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.StubTech_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.GlobalTechCoef_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.GlobalTechCoef_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.GlobalTechCost_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.GlobalTechCost_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.GlobalTechShrwt_C") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.GlobalTechShrwt_C
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.GlobalTechCost_C_High") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.GlobalTechCost_C_High
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.GlobalTechShrwt_C_nooffshore") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.GlobalTechShrwt_C_nooffshore
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.DepRsrcCurves_C_high") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.DepRsrcCurves_C_high
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.DepRsrcCurves_C_low") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.DepRsrcCurves_C_low
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L261.DepRsrcCurves_C_lowest") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L261.DepRsrcCurves_C_lowest

    return_data(L261.DepRsrc, L261.UnlimitRsrc, L261.DepRsrcCurves_C, L261.SectorLogitTables[[ curr_table ]]$data, L261.Supplysector_C, L261.SubsectorLogitTables[[ curr_table ]]$data, L261.SubsectorLogit_C, L261.SubsectorShrwtFllt_C, L261.StubTech_C, L261.GlobalTechCoef_C, L261.GlobalTechCost_C, L261.GlobalTechShrwt_C, L261.GlobalTechCost_C_High, L261.GlobalTechShrwt_C_nooffshore, L261.DepRsrcCurves_C_high, L261.DepRsrcCurves_C_low, L261.DepRsrcCurves_C_lowest)
  } else {
    stop("Unknown command")
  }
}



