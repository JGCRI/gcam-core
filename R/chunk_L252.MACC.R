#' module_emissions_L252.MACC
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.ResMAC_fos}, \code{L252.AgMAC}, \code{L252.MAC_an}, \code{L252.MAC_prc}, \code{L252.MAC_higwp}, \code{L252.MAC_Ag_TC_SSP1}, \code{L252.MAC_An_TC_SSP1}, \code{L252.MAC_prc_TC_SSP1}, \code{L252.MAC_res_TC_SSP1}, \code{L252.MAC_Ag_TC_SSP2}, \code{L252.MAC_An_TC_SSP2}, \code{L252.MAC_prc_TC_SSP2}, \code{L252.MAC_res_TC_SSP2}, \code{L252.MAC_Ag_TC_SSP5}, \code{L252.MAC_An_TC_SSP5}, \code{L252.MAC_prc_TC_SSP5}, \code{L252.MAC_res_TC_SSP5}. The corresponding file in the
#' original data system was \code{L252.MACC.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L252.MACC_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/A_regions",
FILE = "emissions/A_MACC_TechChange",
FILE = "emissions/GCAM_sector_tech",
FILE = "emissions/HFC_Abate_GV",
 "L152.MAC_pct_R_S_Proc_EPA",
 "L201.ghg_res",
 "L211.AGREmissions",
 "L211.AnEmissions",
 "L211.AGRBio",
 "L232.nonco2_prc",
 "L241.hfc_all",
 "L241.pfc_all"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L252.ResMAC_fos",
XML = "L252.AgMAC",
XML = "L252.MAC_an",
XML = "L252.MAC_prc",
XML = "L252.MAC_higwp",
XML = "L252.MAC_Ag_TC_SSP1",
XML = "L252.MAC_An_TC_SSP1",
XML = "L252.MAC_prc_TC_SSP1",
XML = "L252.MAC_res_TC_SSP1",
XML = "L252.MAC_Ag_TC_SSP2",
XML = "L252.MAC_An_TC_SSP2",
XML = "L252.MAC_prc_TC_SSP2",
XML = "L252.MAC_res_TC_SSP2",
XML = "L252.MAC_Ag_TC_SSP5",
XML = "L252.MAC_An_TC_SSP5",
XML = "L252.MAC_prc_TC_SSP5",
XML = "L252.MAC_res_TC_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "emissions/A_regions")
  A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange")
  GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
  HFC_Abate_GV <- get_data(all_data, "emissions/HFC_Abate_GV")
  L152.MAC_pct_R_S_Proc_EPA <- get_data(all_data, "L152.MAC_pct_R_S_Proc_EPA")
  L201.ghg_res <- get_data(all_data, "L201.ghg_res")
  L211.AGREmissions <- get_data(all_data, "L211.AGREmissions")
  L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
  L211.AGRBio <- get_data(all_data, "L211.AGRBio")
  L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
  L241.hfc_all <- get_data(all_data, "L241.hfc_all")
  L241.pfc_all <- get_data(all_data, "L241.pfc_all")

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
 add_legacy_name("L252.ResMAC_fos") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.ResMAC_fos
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.AgMAC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.AgMAC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_prc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_prc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_higwp") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_higwp
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_Ag_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_Ag_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_An_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_An_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_prc_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_prc_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_res_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_res_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_Ag_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_Ag_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_An_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_An_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_prc_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_prc_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_res_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_res_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_Ag_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_Ag_TC_SSP5
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_An_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_An_TC_SSP5
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_prc_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_prc_TC_SSP5
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L252.MAC_res_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L252.MAC_res_TC_SSP5

    return_data(L252.ResMAC_fos, L252.AgMAC, L252.MAC_an, L252.MAC_prc, L252.MAC_higwp, L252.MAC_Ag_TC_SSP1, L252.MAC_An_TC_SSP1, L252.MAC_prc_TC_SSP1, L252.MAC_res_TC_SSP1, L252.MAC_Ag_TC_SSP2, L252.MAC_An_TC_SSP2, L252.MAC_prc_TC_SSP2, L252.MAC_res_TC_SSP2, L252.MAC_Ag_TC_SSP5, L252.MAC_An_TC_SSP5, L252.MAC_prc_TC_SSP5, L252.MAC_res_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}



