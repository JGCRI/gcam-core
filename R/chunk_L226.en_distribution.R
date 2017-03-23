#' module_energy_L226.en_distribution
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.SectorLogitTables[[ curr_table ]]$data}, \code{L226.Supplysector_en}, \code{L226.SubsectorLogitTables[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en}, \code{L226.SubsectorShrwt_en}, \code{L226.SubsectorShrwtFllt_en}, \code{L226.SubsectorInterp_en}, \code{L226.SubsectorInterpTo_en}, \code{L226.StubTech_en}, \code{L226.GlobalTechEff_en}, \code{L226.GlobalTechCost_en}, \code{L226.GlobalTechShrwt_en}, \code{L226.StubTechCoef_elecownuse}, \code{L226.StubTechCoef_electd}, \code{L226.StubTechCoef_gaspipe}. The corresponding file in the
#' original data system was \code{L226.en_distribution.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L226.en_distribution_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/fuel_energy_input",
FILE = "energy/calibrated_techs",
FILE = "energy/A_regions",
FILE = "energy/A26.sector",
FILE = "energy/A26.subsector_logit",
FILE = "energy/A26.subsector_shrwt",
FILE = "energy/A26.subsector_interp",
FILE = "energy/A26.globaltech_eff",
FILE = "energy/A26.globaltech_cost",
FILE = "energy/A26.globaltech_shrwt",
 "L126.IO_R_elecownuse_F_Yh",
 "L126.IO_R_electd_F_Yh",
 "L126.IO_R_gaspipe_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L226.SectorLogitTables[[ curr_table ]]$data",
XML = "L226.Supplysector_en",
XML = "L226.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L226.SubsectorLogit_en",
XML = "L226.SubsectorShrwt_en",
XML = "L226.SubsectorShrwtFllt_en",
XML = "L226.SubsectorInterp_en",
XML = "L226.SubsectorInterpTo_en",
XML = "L226.StubTech_en",
XML = "L226.GlobalTechEff_en",
XML = "L226.GlobalTechCost_en",
XML = "L226.GlobalTechShrwt_en",
XML = "L226.StubTechCoef_elecownuse",
XML = "L226.StubTechCoef_electd",
XML = "L226.StubTechCoef_gaspipe"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
  calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
  A_regions <- get_data(all_data, "energy/A_regions")
  A26.sector <- get_data(all_data, "energy/A26.sector")
  A26.subsector_logit <- get_data(all_data, "energy/A26.subsector_logit")
  A26.subsector_shrwt <- get_data(all_data, "energy/A26.subsector_shrwt")
  A26.subsector_interp <- get_data(all_data, "energy/A26.subsector_interp")
  A26.globaltech_eff <- get_data(all_data, "energy/A26.globaltech_eff")
  A26.globaltech_cost <- get_data(all_data, "energy/A26.globaltech_cost")
  A26.globaltech_shrwt <- get_data(all_data, "energy/A26.globaltech_shrwt")
  L126.IO_R_elecownuse_F_Yh <- get_data(all_data, "L126.IO_R_elecownuse_F_Yh")
  L126.IO_R_electd_F_Yh <- get_data(all_data, "L126.IO_R_electd_F_Yh")
  L126.IO_R_gaspipe_F_Yh <- get_data(all_data, "L126.IO_R_gaspipe_F_Yh")

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
 add_legacy_name("L226.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.Supplysector_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.Supplysector_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorLogit_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorLogit_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorShrwt_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorShrwt_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorShrwtFllt_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorShrwtFllt_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorInterp_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorInterp_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.SubsectorInterpTo_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.SubsectorInterpTo_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.StubTech_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.StubTech_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.GlobalTechEff_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.GlobalTechEff_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.GlobalTechCost_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.GlobalTechCost_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.GlobalTechShrwt_en") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.GlobalTechShrwt_en
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.StubTechCoef_elecownuse") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.StubTechCoef_elecownuse
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.StubTechCoef_electd") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.StubTechCoef_electd
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L226.StubTechCoef_gaspipe") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L226.StubTechCoef_gaspipe

    return_data(L226.SectorLogitTables[[ curr_table ]]$data, L226.Supplysector_en, L226.SubsectorLogitTables[[ curr_table ]]$data, L226.SubsectorLogit_en, L226.SubsectorShrwt_en, L226.SubsectorShrwtFllt_en, L226.SubsectorInterp_en, L226.SubsectorInterpTo_en, L226.StubTech_en, L226.GlobalTechEff_en, L226.GlobalTechCost_en, L226.GlobalTechShrwt_en, L226.StubTechCoef_elecownuse, L226.StubTechCoef_electd, L226.StubTechCoef_gaspipe)
  } else {
    stop("Unknown command")
  }
}



