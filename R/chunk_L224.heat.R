#' module_energy_L224.heat
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L224.SectorLogitTables[[ curr_table ]]$data}, \code{L224.Supplysector_heat}, \code{L224.SubsectorLogitTables[[ curr_table ]]$data}, \code{L224.SubsectorLogit_heat}, \code{L224.SubsectorShrwt_heat}, \code{L224.SubsectorShrwtFllt_heat}, \code{L224.SubsectorInterp_heat}, \code{L224.SubsectorInterpTo_heat}, \code{L224.StubTech_heat}, \code{L224.GlobalTechCoef_heat}, \code{L224.GlobalTechCost_heat}, \code{L224.GlobalTechShrwt_heat}, \code{L224.StubTechCalInput_heat}, \code{L224.StubTechSecOut_elec}, \code{L224.StubTechCost_elec}. The corresponding file in the
#' original data system was \code{L224.heat.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L224.heat_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/fuel_energy_input",
FILE = "energy/calibrated_techs",
FILE = "energy/A_regions",
FILE = "energy/A24.sector",
FILE = "energy/A24.subsector_logit",
FILE = "energy/A24.subsector_shrwt",
FILE = "energy/A24.subsector_interp",
FILE = "energy/A24.globaltech_coef",
FILE = "energy/A24.globaltech_cost",
FILE = "energy/A24.globaltech_shrwt",
 "L1231.eff_R_elec_F_tech_Yh",
 "L124.in_EJ_R_heat_F_Yh",
 "L124.heatoutratio_R_elec_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L224.SectorLogitTables[[ curr_table ]]$data",
XML = "L224.Supplysector_heat",
XML = "L224.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L224.SubsectorLogit_heat",
XML = "L224.SubsectorShrwt_heat",
XML = "L224.SubsectorShrwtFllt_heat",
XML = "L224.SubsectorInterp_heat",
XML = "L224.SubsectorInterpTo_heat",
XML = "L224.StubTech_heat",
XML = "L224.GlobalTechCoef_heat",
XML = "L224.GlobalTechCost_heat",
XML = "L224.GlobalTechShrwt_heat",
XML = "L224.StubTechCalInput_heat",
XML = "L224.StubTechSecOut_elec",
XML = "L224.StubTechCost_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
  calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
  A_regions <- get_data(all_data, "energy/A_regions")
  A24.sector <- get_data(all_data, "energy/A24.sector")
  A24.subsector_logit <- get_data(all_data, "energy/A24.subsector_logit")
  A24.subsector_shrwt <- get_data(all_data, "energy/A24.subsector_shrwt")
  A24.subsector_interp <- get_data(all_data, "energy/A24.subsector_interp")
  A24.globaltech_coef <- get_data(all_data, "energy/A24.globaltech_coef")
  A24.globaltech_cost <- get_data(all_data, "energy/A24.globaltech_cost")
  A24.globaltech_shrwt <- get_data(all_data, "energy/A24.globaltech_shrwt")
  L1231.eff_R_elec_F_tech_Yh <- get_data(all_data, "L1231.eff_R_elec_F_tech_Yh")
  L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
  L124.heatoutratio_R_elec_F_tech_Yh <- get_data(all_data, "L124.heatoutratio_R_elec_F_tech_Yh")

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
 add_legacy_name("L224.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.Supplysector_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.Supplysector_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorLogit_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorLogit_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorShrwt_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorShrwt_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorShrwtFllt_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorShrwtFllt_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorInterp_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorInterp_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.SubsectorInterpTo_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.SubsectorInterpTo_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.StubTech_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.StubTech_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.GlobalTechCoef_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.GlobalTechCoef_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.GlobalTechCost_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.GlobalTechCost_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.GlobalTechShrwt_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.GlobalTechShrwt_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.StubTechCalInput_heat") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.StubTechCalInput_heat
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.StubTechSecOut_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.StubTechSecOut_elec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L224.StubTechCost_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L224.StubTechCost_elec

    return_data(L224.SectorLogitTables[[ curr_table ]]$data, L224.Supplysector_heat, L224.SubsectorLogitTables[[ curr_table ]]$data, L224.SubsectorLogit_heat, L224.SubsectorShrwt_heat, L224.SubsectorShrwtFllt_heat, L224.SubsectorInterp_heat, L224.SubsectorInterpTo_heat, L224.StubTech_heat, L224.GlobalTechCoef_heat, L224.GlobalTechCost_heat, L224.GlobalTechShrwt_heat, L224.StubTechCalInput_heat, L224.StubTechSecOut_elec, L224.StubTechCost_elec)
  } else {
    stop("Unknown command")
  }
}



