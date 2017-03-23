#' module_aglu_L243.bio_trade_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L243.DeleteInput_RegBio}, \code{L243.SectorLogitTables[[ curr_table ]]$data}, \code{L243.TechCoef_RegBio}, \code{L243.Supplysector_Bio}, \code{L243.SectorUseTrialMarket_Bio}, \code{L243.SubsectorLogitTables[[ curr_table ]]$data}, \code{L243.SubsectorLogit_Bio}, \code{L243.SubsectorShrwtFllt_TotBio}, \code{L243.SubsectorShrwtFllt_TradedBio}, \code{L243.GlobalTechCoef_TotBio}, \code{L243.GlobalTechShrwt_TotBio}, \code{L243.StubTech_TotBio}, \code{L243.StubTechShrwt_TotBio}, \code{L243.StubTechCoef_ImportedBio}, \code{L243.StubTechCoef_DomesticBio}, \code{L243.TechCoef_TradedBio}, \code{L243.TechShrwt_TradedBio}, \code{L243.SubsectorShrwtFllt_TotBio_SSP4}, \code{L243.SubsectorShrwtFllt_TradedBio_SSP4}, \code{L243.TechShrwt_TradedBio_SSP4}, \code{L243.StubTechShrwt_TotBio_SSP4}, \code{L243.SubsectorShrwtFllt_TotBio_SSP3}, \code{L243.StubTechShrwt_TotBio_SSP3}. The corresponding file in the
#' original data system was \code{L243.bio_trade_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L243.bio_trade_input_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "aglu/A_bio_supplysector",
FILE = "aglu/A_bio_subsector_logit",
FILE = "aglu/A_bio_subsector",
 "L120.LC_bm2_R_LT_Yh_GLU",
 "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L243.DeleteInput_RegBio",
XML = "L243.SectorLogitTables[[ curr_table ]]$data",
XML = "L243.TechCoef_RegBio",
XML = "L243.Supplysector_Bio",
XML = "L243.SectorUseTrialMarket_Bio",
XML = "L243.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L243.SubsectorLogit_Bio",
XML = "L243.SubsectorShrwtFllt_TotBio",
XML = "L243.SubsectorShrwtFllt_TradedBio",
XML = "L243.GlobalTechCoef_TotBio",
XML = "L243.GlobalTechShrwt_TotBio",
XML = "L243.StubTech_TotBio",
XML = "L243.StubTechShrwt_TotBio",
XML = "L243.StubTechCoef_ImportedBio",
XML = "L243.StubTechCoef_DomesticBio",
XML = "L243.TechCoef_TradedBio",
XML = "L243.TechShrwt_TradedBio",
XML = "L243.SubsectorShrwtFllt_TotBio_SSP4",
XML = "L243.SubsectorShrwtFllt_TradedBio_SSP4",
XML = "L243.TechShrwt_TradedBio_SSP4",
XML = "L243.StubTechShrwt_TotBio_SSP4",
XML = "L243.SubsectorShrwtFllt_TotBio_SSP3",
XML = "L243.StubTechShrwt_TotBio_SSP3"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_bio_supplysector <- get_data(all_data, "aglu/A_bio_supplysector")
  A_bio_subsector_logit <- get_data(all_data, "aglu/A_bio_subsector_logit")
  A_bio_subsector <- get_data(all_data, "aglu/A_bio_subsector")
  L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")
  L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

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
 add_legacy_name("L243.DeleteInput_RegBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.DeleteInput_RegBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.TechCoef_RegBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.TechCoef_RegBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.Supplysector_Bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.Supplysector_Bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SectorUseTrialMarket_Bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SectorUseTrialMarket_Bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorLogit_Bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorLogit_Bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorShrwtFllt_TotBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorShrwtFllt_TotBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorShrwtFllt_TradedBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorShrwtFllt_TradedBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.GlobalTechCoef_TotBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.GlobalTechCoef_TotBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.GlobalTechShrwt_TotBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.GlobalTechShrwt_TotBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTech_TotBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTech_TotBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTechShrwt_TotBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTechShrwt_TotBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTechCoef_ImportedBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTechCoef_ImportedBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTechCoef_DomesticBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTechCoef_DomesticBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.TechCoef_TradedBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.TechCoef_TradedBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.TechShrwt_TradedBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.TechShrwt_TradedBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorShrwtFllt_TotBio_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorShrwtFllt_TotBio_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorShrwtFllt_TradedBio_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorShrwtFllt_TradedBio_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.TechShrwt_TradedBio_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.TechShrwt_TradedBio_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTechShrwt_TotBio_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTechShrwt_TotBio_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.SubsectorShrwtFllt_TotBio_SSP3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.SubsectorShrwtFllt_TotBio_SSP3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L243.StubTechShrwt_TotBio_SSP3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L243.StubTechShrwt_TotBio_SSP3

    return_data(L243.DeleteInput_RegBio, L243.SectorLogitTables[[ curr_table ]]$data, L243.TechCoef_RegBio, L243.Supplysector_Bio, L243.SectorUseTrialMarket_Bio, L243.SubsectorLogitTables[[ curr_table ]]$data, L243.SubsectorLogit_Bio, L243.SubsectorShrwtFllt_TotBio, L243.SubsectorShrwtFllt_TradedBio, L243.GlobalTechCoef_TotBio, L243.GlobalTechShrwt_TotBio, L243.StubTech_TotBio, L243.StubTechShrwt_TotBio, L243.StubTechCoef_ImportedBio, L243.StubTechCoef_DomesticBio, L243.TechCoef_TradedBio, L243.TechShrwt_TradedBio, L243.SubsectorShrwtFllt_TotBio_SSP4, L243.SubsectorShrwtFllt_TradedBio_SSP4, L243.TechShrwt_TradedBio_SSP4, L243.StubTechShrwt_TotBio_SSP4, L243.SubsectorShrwtFllt_TotBio_SSP3, L243.StubTechShrwt_TotBio_SSP3)
  } else {
    stop("Unknown command")
  }
}



