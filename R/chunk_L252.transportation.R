#' module_energy_L252.transportation
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.SectorLogitTables[[ curr_table ]]$data}, \code{L252.Supplysector_trn}, \code{L252.FinalEnergyKeyword_trn}, \code{L252.SubsectorLogitTables[[ curr_table ]]$data}, \code{L252.SubsectorLogit_trn}, \code{L252.SubsectorShrwt_trn}, \code{L252.SubsectorShrwtFllt_trn}, \code{L252.SubsectorInterp_trn}, \code{L252.SubsectorInterpTo_trn}, \code{L252.StubTech_trn}, \code{L252.GlobalTechShrwt_trn}, \code{L252.GlobalTechEff_trn}, \code{L252.GlobalTechCost_trn}, \code{L252.StubTechCalInput_trn}, \code{L252.PerCapitaBased_trn}, \code{L252.PriceElasticity_trn}, \code{L252.BaseService_trn}. The corresponding file in the
#' original data system was \code{L252.transportation.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L252.transportation_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/fuel_energy_input",
FILE = "energy/calibrated_techs_trn_agg",
FILE = "energy/A52.sector",
FILE = "energy/A52.subsector_interp",
FILE = "energy/A52.subsector_logit",
FILE = "energy/A52.subsector_shrwt",
FILE = "energy/A52.globaltech_cost",
FILE = "energy/A52.globaltech_eff",
FILE = "energy/A52.globaltech_shrwt",
FILE = "energy/A52.demand",
 "L152.in_EJ_R_trn_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L252.SectorLogitTables[[ curr_table ]]$data",
XML = "L252.Supplysector_trn",
XML = "L252.FinalEnergyKeyword_trn",
XML = "L252.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L252.SubsectorLogit_trn",
XML = "L252.SubsectorShrwt_trn",
XML = "L252.SubsectorShrwtFllt_trn",
XML = "L252.SubsectorInterp_trn",
XML = "L252.SubsectorInterpTo_trn",
XML = "L252.StubTech_trn",
XML = "L252.GlobalTechShrwt_trn",
XML = "L252.GlobalTechEff_trn",
XML = "L252.GlobalTechCost_trn",
XML = "L252.StubTechCalInput_trn",
XML = "L252.PerCapitaBased_trn",
XML = "L252.PriceElasticity_trn",
XML = "L252.BaseService_trn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
  calibrated_techs_trn_agg <- get_data(all_data, "energy/calibrated_techs_trn_agg")
  A52.sector <- get_data(all_data, "energy/A52.sector")
  A52.subsector_interp <- get_data(all_data, "energy/A52.subsector_interp")
  A52.subsector_logit <- get_data(all_data, "energy/A52.subsector_logit")
  A52.subsector_shrwt <- get_data(all_data, "energy/A52.subsector_shrwt")
  A52.globaltech_cost <- get_data(all_data, "energy/A52.globaltech_cost")
  A52.globaltech_eff <- get_data(all_data, "energy/A52.globaltech_eff")
  A52.globaltech_shrwt <- get_data(all_data, "energy/A52.globaltech_shrwt")
  A52.demand <- get_data(all_data, "energy/A52.demand")
  L152.in_EJ_R_trn_F_Yh <- get_data(all_data, "L152.in_EJ_R_trn_F_Yh")

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
 add_legacy_name("L252.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.Supplysector_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.Supplysector_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.FinalEnergyKeyword_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.FinalEnergyKeyword_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorLogit_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorLogit_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorShrwt_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorShrwt_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorShrwtFllt_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorShrwtFllt_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorInterp_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorInterp_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.SubsectorInterpTo_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.SubsectorInterpTo_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.StubTech_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.StubTech_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.GlobalTechShrwt_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.GlobalTechShrwt_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.GlobalTechEff_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.GlobalTechEff_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.GlobalTechCost_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.GlobalTechCost_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.StubTechCalInput_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.StubTechCalInput_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.PerCapitaBased_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.PerCapitaBased_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.PriceElasticity_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.PriceElasticity_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.BaseService_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.BaseService_trn

    return_data(L252.SectorLogitTables[[ curr_table ]]$data, L252.Supplysector_trn, L252.FinalEnergyKeyword_trn, L252.SubsectorLogitTables[[ curr_table ]]$data, L252.SubsectorLogit_trn, L252.SubsectorShrwt_trn, L252.SubsectorShrwtFllt_trn, L252.SubsectorInterp_trn, L252.SubsectorInterpTo_trn, L252.StubTech_trn, L252.GlobalTechShrwt_trn, L252.GlobalTechEff_trn, L252.GlobalTechCost_trn, L252.StubTechCalInput_trn, L252.PerCapitaBased_trn, L252.PriceElasticity_trn, L252.BaseService_trn)
  } else {
    stop("Unknown command")
  }
}



