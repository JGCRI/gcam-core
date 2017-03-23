#' module_gcam.usa_L2232.electricity_FERC_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2232.DeleteSupplysector_USAelec}, \code{L2232.SectorLogitTables_USAelec[[ curr_table ]]$data}, \code{L2232.Supplysector_USAelec}, \code{L2232.SubsectorShrwtFllt_USAelec}, \code{L2232.SubsectorInterp_USAelec}, \code{L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data}, \code{L2232.SubsectorLogit_USAelec}, \code{L2232.TechShrwt_USAelec}, \code{L2232.TechCoef_USAelec}, \code{L2232.Production_exports_USAelec}, \code{L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data}, \code{L2232.Supplysector_elec_FERC}, \code{L2232.ElecReserve_FERC}, \code{L2232.SubsectorShrwtFllt_elec_FERC}, \code{L2232.SubsectorInterp_elec_FERC}, \code{L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data}, \code{L2232.SubsectorLogit_elec_FERC}, \code{L2232.TechShrwt_elec_FERC}, \code{L2232.TechCoef_elec_FERC}, \code{L2232.TechCoef_elecownuse_FERC}, \code{L2232.Production_imports_FERC}, \code{L2232.Production_elec_gen_FERC}, \code{L2232.StubTechElecMarket_backup_USA}. The corresponding file in the
#' original data system was \code{L2232.electricity_FERC_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L2232.electricity_FERC_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
FILE = "energy/A23.sector",
FILE = "gcam-usa/A232.structure",
 "L123.in_EJ_state_ownuse_elec",
 "L123.out_EJ_state_ownuse_elec",
 "L126.in_EJ_state_td_elec",
 "L126.out_EJ_state_td_elec",
 "L132.out_EJ_state_indchp_F",
 "L223.Supplysector_elec",
 "L1232.out_EJ_sR_elec",
 "L223.StubTechMarket_backup_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2232.DeleteSupplysector_USAelec",
XML = "L2232.SectorLogitTables_USAelec[[ curr_table ]]$data",
XML = "L2232.Supplysector_USAelec",
XML = "L2232.SubsectorShrwtFllt_USAelec",
XML = "L2232.SubsectorInterp_USAelec",
XML = "L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data",
XML = "L2232.SubsectorLogit_USAelec",
XML = "L2232.TechShrwt_USAelec",
XML = "L2232.TechCoef_USAelec",
XML = "L2232.Production_exports_USAelec",
XML = "L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data",
XML = "L2232.Supplysector_elec_FERC",
XML = "L2232.ElecReserve_FERC",
XML = "L2232.SubsectorShrwtFllt_elec_FERC",
XML = "L2232.SubsectorInterp_elec_FERC",
XML = "L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data",
XML = "L2232.SubsectorLogit_elec_FERC",
XML = "L2232.TechShrwt_elec_FERC",
XML = "L2232.TechCoef_elec_FERC",
XML = "L2232.TechCoef_elecownuse_FERC",
XML = "L2232.Production_imports_FERC",
XML = "L2232.Production_elec_gen_FERC",
XML = "L2232.StubTechElecMarket_backup_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  A23.sector <- get_data(all_data, "energy/A23.sector")
  A232.structure <- get_data(all_data, "gcam-usa/A232.structure")
  L123.in_EJ_state_ownuse_elec <- get_data(all_data, "L123.in_EJ_state_ownuse_elec")
  L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec")
  L126.in_EJ_state_td_elec <- get_data(all_data, "L126.in_EJ_state_td_elec")
  L126.out_EJ_state_td_elec <- get_data(all_data, "L126.out_EJ_state_td_elec")
  L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")
  L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
  L1232.out_EJ_sR_elec <- get_data(all_data, "L1232.out_EJ_sR_elec")
  L223.StubTechMarket_backup_USA <- get_data(all_data, "L223.StubTechMarket_backup_USA")

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
 add_legacy_name("L2232.DeleteSupplysector_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.DeleteSupplysector_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SectorLogitTables_USAelec[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SectorLogitTables_USAelec[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.Supplysector_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.Supplysector_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorShrwtFllt_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorShrwtFllt_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorInterp_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorInterp_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorLogit_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorLogit_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.TechShrwt_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.TechShrwt_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.TechCoef_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.TechCoef_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.Production_exports_USAelec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.Production_exports_USAelec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.Supplysector_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.Supplysector_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.ElecReserve_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.ElecReserve_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorShrwtFllt_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorShrwtFllt_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorInterp_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorInterp_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.SubsectorLogit_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.SubsectorLogit_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.TechShrwt_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.TechShrwt_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.TechCoef_elec_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.TechCoef_elec_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.TechCoef_elecownuse_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.TechCoef_elecownuse_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.Production_imports_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.Production_imports_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.Production_elec_gen_FERC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.Production_elec_gen_FERC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2232.StubTechElecMarket_backup_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2232.StubTechElecMarket_backup_USA

    return_data(L2232.DeleteSupplysector_USAelec, L2232.SectorLogitTables_USAelec[[ curr_table ]]$data, L2232.Supplysector_USAelec, L2232.SubsectorShrwtFllt_USAelec, L2232.SubsectorInterp_USAelec, L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data, L2232.SubsectorLogit_USAelec, L2232.TechShrwt_USAelec, L2232.TechCoef_USAelec, L2232.Production_exports_USAelec, L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data, L2232.Supplysector_elec_FERC, L2232.ElecReserve_FERC, L2232.SubsectorShrwtFllt_elec_FERC, L2232.SubsectorInterp_elec_FERC, L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data, L2232.SubsectorLogit_elec_FERC, L2232.TechShrwt_elec_FERC, L2232.TechCoef_elec_FERC, L2232.TechCoef_elecownuse_FERC, L2232.Production_imports_FERC, L2232.Production_elec_gen_FERC, L2232.StubTechElecMarket_backup_USA)
  } else {
    stop("Unknown command")
  }
}



