#' module_aglu_L202.an_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.RenewRsrc}, \code{L202.RenewRsrcPrice}, \code{L202.maxSubResource}, \code{L202.RenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcPrice}, \code{L202.SectorLogitTables_in[[ curr_table ]]$data}, \code{L202.Supplysector_in}, \code{L202.SubsectorLogitTables_in[[ curr_table ]]$data}, \code{L202.SubsectorAll_in}, \code{L202.StubTech_in}, \code{L202.StubTechInterp_in}, \code{L202.GlobalTechCoef_in}, \code{L202.GlobalTechShrwt_in}, \code{L202.StubTechProd_in}, \code{L202.SectorLogitTables_an[[ curr_table ]]$data}, \code{L202.Supplysector_an}, \code{L202.SubsectorLogitTables_an[[ curr_table ]]$data}, \code{L202.SubsectorAll_an}, \code{L202.StubTech_an}, \code{L202.StubTechInterp_an}, \code{L202.StubTechProd_an}, \code{L202.StubTechCoef_an}, \code{L202.GlobalTechCost_an}, \code{L202.GlobalRenewTech_imp_an}, \code{L202.StubTechFixOut_imp_an}. The corresponding file in the
#' original data system was \code{L202.an_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L202.an_input_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/A_regions  # need this to tell which regions have ddgs available",
FILE = "aglu/A_agRsrc",
FILE = "aglu/A_agSubRsrc",
FILE = "aglu/A_agRsrcCurves",
FILE = "aglu/A_agUnlimitedRsrcCurves",
FILE = "aglu/A_an_input_supplysector",
FILE = "aglu/A_an_input_subsector",
FILE = "aglu/A_an_input_technology",
FILE = "aglu/A_an_input_globaltech_shrwt",
FILE = "aglu/A_an_supplysector",
FILE = "aglu/A_an_subsector",
FILE = "aglu/A_an_technology",
 "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
 "L107.an_FeedIO_R_C_Sys_Fd_Y",
 "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
 "L108.ag_Feed_Mt_R_C_Y",
 "L109.an_ALL_Mt_R_C_Y",
 "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L202.RenewRsrc",
XML = "L202.RenewRsrcPrice",
XML = "L202.maxSubResource",
XML = "L202.RenewRsrcCurves",
XML = "L202.UnlimitedRenewRsrcCurves",
XML = "L202.UnlimitedRenewRsrcPrice",
XML = "L202.SectorLogitTables_in[[ curr_table ]]$data",
XML = "L202.Supplysector_in",
XML = "L202.SubsectorLogitTables_in[[ curr_table ]]$data",
XML = "L202.SubsectorAll_in",
XML = "L202.StubTech_in",
XML = "L202.StubTechInterp_in",
XML = "L202.GlobalTechCoef_in",
XML = "L202.GlobalTechShrwt_in",
XML = "L202.StubTechProd_in",
XML = "L202.SectorLogitTables_an[[ curr_table ]]$data",
XML = "L202.Supplysector_an",
XML = "L202.SubsectorLogitTables_an[[ curr_table ]]$data",
XML = "L202.SubsectorAll_an",
XML = "L202.StubTech_an",
XML = "L202.StubTechInterp_an",
XML = "L202.StubTechProd_an",
XML = "L202.StubTechCoef_an",
XML = "L202.GlobalTechCost_an",
XML = "L202.GlobalRenewTech_imp_an",
XML = "L202.StubTechFixOut_imp_an"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions  # need this to tell which regions have ddgs available <- get_data(all_data, "energy/A_regions  # need this to tell which regions have ddgs available")
  A_agRsrc <- get_data(all_data, "aglu/A_agRsrc")
  A_agSubRsrc <- get_data(all_data, "aglu/A_agSubRsrc")
  A_agRsrcCurves <- get_data(all_data, "aglu/A_agRsrcCurves")
  A_agUnlimitedRsrcCurves <- get_data(all_data, "aglu/A_agUnlimitedRsrcCurves")
  A_an_input_supplysector <- get_data(all_data, "aglu/A_an_input_supplysector")
  A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector")
  A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology")
  A_an_input_globaltech_shrwt <- get_data(all_data, "aglu/A_an_input_globaltech_shrwt")
  A_an_supplysector <- get_data(all_data, "aglu/A_an_supplysector")
  A_an_subsector <- get_data(all_data, "aglu/A_an_subsector")
  A_an_technology <- get_data(all_data, "aglu/A_an_technology")
  L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
  L107.an_FeedIO_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_FeedIO_R_C_Sys_Fd_Y")
  L107.an_Feed_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Feed_Mt_R_C_Sys_Fd_Y")
  L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
  L109.an_ALL_Mt_R_C_Y <- get_data(all_data, "L109.an_ALL_Mt_R_C_Y")
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
 add_legacy_name("L202.RenewRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.RenewRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.RenewRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.RenewRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.maxSubResource") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.maxSubResource
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.RenewRsrcCurves") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.RenewRsrcCurves
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.UnlimitedRenewRsrcCurves") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.UnlimitedRenewRsrcCurves
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.UnlimitedRenewRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.UnlimitedRenewRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SectorLogitTables_in[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SectorLogitTables_in[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.Supplysector_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.Supplysector_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SubsectorLogitTables_in[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SubsectorLogitTables_in[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SubsectorAll_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SubsectorAll_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTech_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTech_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechInterp_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechInterp_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.GlobalTechCoef_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.GlobalTechCoef_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.GlobalTechShrwt_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.GlobalTechShrwt_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechProd_in") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechProd_in
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SectorLogitTables_an[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SectorLogitTables_an[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.Supplysector_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.Supplysector_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SubsectorLogitTables_an[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SubsectorLogitTables_an[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.SubsectorAll_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.SubsectorAll_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTech_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTech_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechInterp_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechInterp_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechProd_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechProd_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechCoef_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechCoef_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.GlobalTechCost_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.GlobalTechCost_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.GlobalRenewTech_imp_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.GlobalRenewTech_imp_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L202.StubTechFixOut_imp_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L202.StubTechFixOut_imp_an

    return_data(L202.RenewRsrc, L202.RenewRsrcPrice, L202.maxSubResource, L202.RenewRsrcCurves, L202.UnlimitedRenewRsrcCurves, L202.UnlimitedRenewRsrcPrice, L202.SectorLogitTables_in[[ curr_table ]]$data, L202.Supplysector_in, L202.SubsectorLogitTables_in[[ curr_table ]]$data, L202.SubsectorAll_in, L202.StubTech_in, L202.StubTechInterp_in, L202.GlobalTechCoef_in, L202.GlobalTechShrwt_in, L202.StubTechProd_in, L202.SectorLogitTables_an[[ curr_table ]]$data, L202.Supplysector_an, L202.SubsectorLogitTables_an[[ curr_table ]]$data, L202.SubsectorAll_an, L202.StubTech_an, L202.StubTechInterp_an, L202.StubTechProd_an, L202.StubTechCoef_an, L202.GlobalTechCost_an, L202.GlobalRenewTech_imp_an, L202.StubTechFixOut_imp_an)
  } else {
    stop("Unknown command")
  }
}



