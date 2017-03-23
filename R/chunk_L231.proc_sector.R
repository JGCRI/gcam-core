#' module_emissions_L231.proc_sector
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L231.UnlimitRsrc}, \code{L231.UnlimitRsrcPrice}, \code{L231.FinalDemand_urb}, \code{L231.SectorLogitTables[[ curr_table ]]$data}, \code{L231.Supplysector_urb_ind}, \code{L231.SubsectorLogitTables[[ curr_table ]]$data}, \code{L231.SubsectorLogit_urb_ind}, \code{L231.SubsectorShrwt_ind}, \code{L231.SubsectorShrwtFllt_urb_ind}, \code{L231.SubsectorInterp_urb_ind}, \code{L231.SubsectorInterpTo_urb_ind}, \code{L231.StubTech_urb_ind}, \code{L231.GlobalTechShrwt_urb_ind}, \code{L231.GlobalTechEff_urb_ind}, \code{L231.GlobalTechCoef_urb_ind}, \code{L231.GlobalTechCost_urb_ind}, \code{L231.RegionalTechCalValue_urb_ind}, \code{L231.IndCoef}. The corresponding file in the
#' original data system was \code{L231.proc_sector.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L231.proc_sector_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/A_regions",
FILE = "emissions/A31.rsrc_info",
FILE = "emissions/A31.sector",
FILE = "emissions/A31.subsector_logit",
FILE = "emissions/A31.subsector_shrwt",
FILE = "emissions/A31.subsector_interp",
FILE = "emissions/A31.globaltech_shrwt",
FILE = "emissions/A31.globaltech_eff",
FILE = "emissions/A31.globaltech_cost",
FILE = "emissions/A31.globaltech_coef",
FILE = "energy/A32.globaltech_eff",
FILE = "emissions/GCAM_sector_tech",
 "L1322.in_EJ_R_indfeed_F_Yh",
 "L1322.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L231.UnlimitRsrc",
XML = "L231.UnlimitRsrcPrice",
XML = "L231.FinalDemand_urb",
XML = "L231.SectorLogitTables[[ curr_table ]]$data",
XML = "L231.Supplysector_urb_ind",
XML = "L231.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L231.SubsectorLogit_urb_ind",
XML = "L231.SubsectorShrwt_ind",
XML = "L231.SubsectorShrwtFllt_urb_ind",
XML = "L231.SubsectorInterp_urb_ind",
XML = "L231.SubsectorInterpTo_urb_ind",
XML = "L231.StubTech_urb_ind",
XML = "L231.GlobalTechShrwt_urb_ind",
XML = "L231.GlobalTechEff_urb_ind",
XML = "L231.GlobalTechCoef_urb_ind",
XML = "L231.GlobalTechCost_urb_ind",
XML = "L231.RegionalTechCalValue_urb_ind",
XML = "L231.IndCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "emissions/A_regions")
  A31.rsrc_info <- get_data(all_data, "emissions/A31.rsrc_info")
  A31.sector <- get_data(all_data, "emissions/A31.sector")
  A31.subsector_logit <- get_data(all_data, "emissions/A31.subsector_logit")
  A31.subsector_shrwt <- get_data(all_data, "emissions/A31.subsector_shrwt")
  A31.subsector_interp <- get_data(all_data, "emissions/A31.subsector_interp")
  A31.globaltech_shrwt <- get_data(all_data, "emissions/A31.globaltech_shrwt")
  A31.globaltech_eff <- get_data(all_data, "emissions/A31.globaltech_eff")
  A31.globaltech_cost <- get_data(all_data, "emissions/A31.globaltech_cost")
  A31.globaltech_coef <- get_data(all_data, "emissions/A31.globaltech_coef")
  A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
  GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
  L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
  L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")

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
 add_legacy_name("L231.UnlimitRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.UnlimitRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.UnlimitRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.UnlimitRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.FinalDemand_urb") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.FinalDemand_urb
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.Supplysector_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.Supplysector_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorLogit_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorLogit_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorShrwt_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorShrwt_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorShrwtFllt_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorShrwtFllt_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorInterp_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorInterp_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.SubsectorInterpTo_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.SubsectorInterpTo_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.StubTech_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.StubTech_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.GlobalTechShrwt_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.GlobalTechShrwt_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.GlobalTechEff_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.GlobalTechEff_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.GlobalTechCoef_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.GlobalTechCoef_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.GlobalTechCost_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.GlobalTechCost_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.RegionalTechCalValue_urb_ind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.RegionalTechCalValue_urb_ind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L231.IndCoef") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L231.IndCoef

    return_data(L231.UnlimitRsrc, L231.UnlimitRsrcPrice, L231.FinalDemand_urb, L231.SectorLogitTables[[ curr_table ]]$data, L231.Supplysector_urb_ind, L231.SubsectorLogitTables[[ curr_table ]]$data, L231.SubsectorLogit_urb_ind, L231.SubsectorShrwt_ind, L231.SubsectorShrwtFllt_urb_ind, L231.SubsectorInterp_urb_ind, L231.SubsectorInterpTo_urb_ind, L231.StubTech_urb_ind, L231.GlobalTechShrwt_urb_ind, L231.GlobalTechEff_urb_ind, L231.GlobalTechCoef_urb_ind, L231.GlobalTechCost_urb_ind, L231.RegionalTechCalValue_urb_ind, L231.IndCoef)
  } else {
    stop("Unknown command")
  }
}



