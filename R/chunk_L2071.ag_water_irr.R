#' module_aglu_L2071.ag_water_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2071.AgCoef_IrrBphysWater_ag}, \code{L2071.AgCoef_IrrWaterWdraw_ag}, \code{L2071.AgCoef_IrrWaterCons_ag}, \code{L2071.AgCoef_RfdBphysWater_ag}, \code{L2071.AgCoef_BphysWater_bio}, \code{L2071.AgCoef_IrrWaterWdraw_bio}, \code{L2071.AgCoef_IrrWaterCons_bio}. The corresponding file in the
#' original data system was \code{L2071.ag_water_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2071.ag_water_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
 "L132.ag_an_For_Prices",
 "L161.ag_irrProd_Mt_R_C_Y_GLU",
 "L161.ag_rfdProd_Mt_R_C_Y_GLU",
 "L161.ag_irrYield_kgm2_R_C_Y_GLU",
 "L165.BlueIrr_m3kg_R_C_GLU",
 "L165.TotIrr_m3kg_R_C_GLU",
 "L165.GreenRfd_m3kg_R_C_GLU",
 "L165.ag_IrrEff_R",
 "L2051.AgCost_ag_irr",
 "L2051.AgCost_bio_irr",
FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2071.AgCoef_IrrBphysWater_ag",
XML = "L2071.AgCoef_IrrWaterWdraw_ag",
XML = "L2071.AgCoef_IrrWaterCons_ag",
XML = "L2071.AgCoef_RfdBphysWater_ag",
XML = "L2071.AgCoef_BphysWater_bio",
XML = "L2071.AgCoef_IrrWaterWdraw_bio",
XML = "L2071.AgCoef_IrrWaterCons_bio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")
  L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
  L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
  L161.ag_irrYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrYield_kgm2_R_C_Y_GLU")
  L165.BlueIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.BlueIrr_m3kg_R_C_GLU")
  L165.TotIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.TotIrr_m3kg_R_C_GLU")
  L165.GreenRfd_m3kg_R_C_GLU <- get_data(all_data, "L165.GreenRfd_m3kg_R_C_GLU")
  L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
  L2051.AgCost_ag_irr <- get_data(all_data, "L2051.AgCost_ag_irr")
  L2051.AgCost_bio_irr <- get_data(all_data, "L2051.AgCost_bio_irr")
  A03.sector <- get_data(all_data, "water/A03.sector")

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
 add_legacy_name("L2071.AgCoef_IrrBphysWater_ag") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_IrrBphysWater_ag
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_IrrWaterWdraw_ag") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_IrrWaterWdraw_ag
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_IrrWaterCons_ag") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_IrrWaterCons_ag
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_RfdBphysWater_ag") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_RfdBphysWater_ag
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_BphysWater_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_BphysWater_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_IrrWaterWdraw_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_IrrWaterWdraw_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2071.AgCoef_IrrWaterCons_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2071.AgCoef_IrrWaterCons_bio

    return_data(L2071.AgCoef_IrrBphysWater_ag, L2071.AgCoef_IrrWaterWdraw_ag, L2071.AgCoef_IrrWaterCons_ag, L2071.AgCoef_RfdBphysWater_ag, L2071.AgCoef_BphysWater_bio, L2071.AgCoef_IrrWaterWdraw_bio, L2071.AgCoef_IrrWaterCons_bio)
  } else {
    stop("Unknown command")
  }
}



