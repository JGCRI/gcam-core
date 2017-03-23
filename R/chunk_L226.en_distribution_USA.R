#' module_gcam.usa_L226.en_distribution_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.DeleteSupplysector_USAelec}, \code{object}, \code{L226.StubTechCoef_electd_USA}, \code{L226.TechShrwt_electd_USA}, \code{L226.TechCost_electd_USA}, \code{L226.TechCoef_electd_USA}, \code{L226.SectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.Supplysector_en_USA}, \code{L226.SubsectorShrwtFllt_en_USA}, \code{L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en_USA}, \code{L226.TechShrwt_en_USA}, \code{L226.TechCoef_en_USA}, \code{L226.TechCost_en_USA}, \code{L226.Ccoef}. The corresponding file in the
#' original data system was \code{L226.en_distribution_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L226.en_distribution_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
FILE = "energy/A21.sector",
FILE = "energy/A26.sector",
FILE = "gcam-usa/EIA_state_energy_prices",
 "L202.CarbonCoef",
 "L226.Supplysector_en",
 "L226.SubsectorLogit_en",
 "L226.SubsectorShrwt_en",
 "L226.SubsectorShrwtFllt_en",
 "L226.SubsectorInterp_en",
 "L226.SubsectorInterpTo_en",
 "L226.GlobalTechCost_en",
 "L226.GlobalTechShrwt_en",
 "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L226.DeleteSupplysector_USAelec",
XML = "object",
XML = "L226.StubTechCoef_electd_USA",
XML = "L226.TechShrwt_electd_USA",
XML = "L226.TechCost_electd_USA",
XML = "L226.TechCoef_electd_USA",
XML = "L226.SectorLogitTables_en_USA[[ curr_table ]]$data",
XML = "L226.Supplysector_en_USA",
XML = "L226.SubsectorShrwtFllt_en_USA",
XML = "L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data",
XML = "L226.SubsectorLogit_en_USA",
XML = "L226.TechShrwt_en_USA",
XML = "L226.TechCoef_en_USA",
XML = "L226.TechCost_en_USA",
XML = "L226.Ccoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  A21.sector <- get_data(all_data, "energy/A21.sector")
  A26.sector <- get_data(all_data, "energy/A26.sector")
  EIA_state_energy_prices <- get_data(all_data, "gcam-usa/EIA_state_energy_prices")
  L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
  L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
  L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
  L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
  L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
  L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
  L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
  L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
  L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
  L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")

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
 add_legacy_name("L226.DeleteSupplysector_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.DeleteSupplysector_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("object") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   object
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.StubTechCoef_electd_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.StubTechCoef_electd_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechShrwt_electd_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechShrwt_electd_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechCost_electd_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechCost_electd_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechCoef_electd_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechCoef_electd_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SectorLogitTables_en_USA[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SectorLogitTables_en_USA[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.Supplysector_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.Supplysector_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorShrwtFllt_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorShrwtFllt_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorLogit_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorLogit_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechShrwt_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechShrwt_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechCoef_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechCoef_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.TechCost_en_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.TechCost_en_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.Ccoef") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.Ccoef

    return_data(L226.DeleteSupplysector_USAelec, object, L226.StubTechCoef_electd_USA, L226.TechShrwt_electd_USA, L226.TechCost_electd_USA, L226.TechCoef_electd_USA, L226.SectorLogitTables_en_USA[[ curr_table ]]$data, L226.Supplysector_en_USA, L226.SubsectorShrwtFllt_en_USA, L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data, L226.SubsectorLogit_en_USA, L226.TechShrwt_en_USA, L226.TechCoef_en_USA, L226.TechCost_en_USA, L226.Ccoef)
  } else {
    stop("Unknown command")
  }
}



