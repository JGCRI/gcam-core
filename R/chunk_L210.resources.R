#' module_energy_L210.resources
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DepRsrc}, \code{L210.RenewRsrc}, \code{L210.UnlimitRsrc}, \code{L210.DepRsrcPrice}, \code{L210.RenewRsrcPrice}, \code{L210.UnlimitRsrcPrice}, \code{L210.DepRsrcTechChange}, \code{L210.SmthRenewRsrcTechChange}, \code{L210.DepRsrcCalProd}, \code{L210.DepRsrcCurves_fos}, \code{L210.DepRsrcCurves_U}, \code{L210.SmthRenewRsrcCurvesGdpElast_MSW}, \code{L210.SmthRenewRsrcCurves_wind}, \code{L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV}, \code{L210.GrdRenewRsrcCurves_geo}, \code{L210.GrdRenewRsrcMax_geo}, \code{L210.GrdRenewRsrcCurves_EGS}, \code{L210.GrdRenewRsrcMax_EGS}, \code{L210.GrdRenewRsrcCurves_tradbio}, \code{L210.GrdRenewRsrcMax_tradbio}, \code{L210.DepRsrcTechChange_SSP1}, \code{L210.DepRsrcEnvironCost_SSP1}, \code{L210.DepRsrcTechChange_SSP2}, \code{L210.DepRsrcEnvironCost_SSP2}, \code{L210.DepRsrcTechChange_SSP3}, \code{L210.DepRsrcEnvironCost_SSP3}, \code{L210.DepRsrcTechChange_SSP4}, \code{L210.DepRsrcEnvironCost_SSP4}, \code{L210.DepRsrcTechChange_SSP5}, \code{L210.DepRsrcEnvironCost_SSP5}. The corresponding file in the
#' original data system was \code{L210.resources.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L210.resources_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/A_regions",
FILE = "energy/A10.rsrc_info",
FILE = "energy/A10.subrsrc_info",
FILE = "energy/A10.TechChange",
FILE = "energy/A10.TechChange_SSPs",
FILE = "energy/A10.EnvironCost_SSPs",
FILE = "energy/A15.roofPV_TechChange",
 "L111.RsrcCurves_EJ_R_Ffos",
 "L111.Prod_EJ_R_F_Yh",
 "L112.RsrcCurves_Mt_R_U",
 "L113.RsrcCurves_EJ_R_MSW",
 "L114.RsrcCurves_EJ_R_wind",
 "L115.RsrcCurves_EJ_R_roofPV",
 "L116.RsrcCurves_EJ_R_geo",
 "L116.RsrcCurves_EJ_R_EGS",
 "L117.RsrcCurves_EJ_R_tradbio",
 "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L210.DepRsrc",
XML = "L210.RenewRsrc",
XML = "L210.UnlimitRsrc",
XML = "L210.DepRsrcPrice",
XML = "L210.RenewRsrcPrice",
XML = "L210.UnlimitRsrcPrice",
XML = "L210.DepRsrcTechChange",
XML = "L210.SmthRenewRsrcTechChange",
XML = "L210.DepRsrcCalProd",
XML = "L210.DepRsrcCurves_fos",
XML = "L210.DepRsrcCurves_U",
XML = "L210.SmthRenewRsrcCurvesGdpElast_MSW",
XML = "L210.SmthRenewRsrcCurves_wind",
XML = "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV",
XML = "L210.GrdRenewRsrcCurves_geo",
XML = "L210.GrdRenewRsrcMax_geo",
XML = "L210.GrdRenewRsrcCurves_EGS",
XML = "L210.GrdRenewRsrcMax_EGS",
XML = "L210.GrdRenewRsrcCurves_tradbio",
XML = "L210.GrdRenewRsrcMax_tradbio",
XML = "L210.DepRsrcTechChange_SSP1",
XML = "L210.DepRsrcEnvironCost_SSP1",
XML = "L210.DepRsrcTechChange_SSP2",
XML = "L210.DepRsrcEnvironCost_SSP2",
XML = "L210.DepRsrcTechChange_SSP3",
XML = "L210.DepRsrcEnvironCost_SSP3",
XML = "L210.DepRsrcTechChange_SSP4",
XML = "L210.DepRsrcEnvironCost_SSP4",
XML = "L210.DepRsrcTechChange_SSP5",
XML = "L210.DepRsrcEnvironCost_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "energy/A_regions")
  A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info")
  A10.subrsrc_info <- get_data(all_data, "energy/A10.subrsrc_info")
  A10.TechChange <- get_data(all_data, "energy/A10.TechChange")
  A10.TechChange_SSPs <- get_data(all_data, "energy/A10.TechChange_SSPs")
  A10.EnvironCost_SSPs <- get_data(all_data, "energy/A10.EnvironCost_SSPs")
  A15.roofPV_TechChange <- get_data(all_data, "energy/A15.roofPV_TechChange")
  L111.RsrcCurves_EJ_R_Ffos <- get_data(all_data, "L111.RsrcCurves_EJ_R_Ffos")
  L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
  L112.RsrcCurves_Mt_R_U <- get_data(all_data, "L112.RsrcCurves_Mt_R_U")
  L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW")
  L114.RsrcCurves_EJ_R_wind <- get_data(all_data, "L114.RsrcCurves_EJ_R_wind")
  L115.RsrcCurves_EJ_R_roofPV <- get_data(all_data, "L115.RsrcCurves_EJ_R_roofPV")
  L116.RsrcCurves_EJ_R_geo <- get_data(all_data, "L116.RsrcCurves_EJ_R_geo")
  L116.RsrcCurves_EJ_R_EGS <- get_data(all_data, "L116.RsrcCurves_EJ_R_EGS")
  L117.RsrcCurves_EJ_R_tradbio <- get_data(all_data, "L117.RsrcCurves_EJ_R_tradbio")
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
 add_legacy_name("L210.DepRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.RenewRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.RenewRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.UnlimitRsrc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.UnlimitRsrc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.RenewRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.RenewRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.UnlimitRsrcPrice") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.UnlimitRsrcPrice
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.SmthRenewRsrcTechChange") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.SmthRenewRsrcTechChange
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcCalProd") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcCalProd
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcCurves_fos") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcCurves_fos
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcCurves_U") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcCurves_U
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.SmthRenewRsrcCurvesGdpElast_MSW") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.SmthRenewRsrcCurvesGdpElast_MSW
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.SmthRenewRsrcCurves_wind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.SmthRenewRsrcCurves_wind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcCurves_geo") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcCurves_geo
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcMax_geo") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcMax_geo
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcCurves_EGS") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcCurves_EGS
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcMax_EGS") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcMax_EGS
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcCurves_tradbio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcCurves_tradbio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.GrdRenewRsrcMax_tradbio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.GrdRenewRsrcMax_tradbio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcEnvironCost_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcEnvironCost_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcEnvironCost_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcEnvironCost_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange_SSP3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange_SSP3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcEnvironCost_SSP3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcEnvironCost_SSP3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcEnvironCost_SSP4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcEnvironCost_SSP4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcTechChange_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcTechChange_SSP5
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L210.DepRsrcEnvironCost_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L210.DepRsrcEnvironCost_SSP5

    return_data(L210.DepRsrc, L210.RenewRsrc, L210.UnlimitRsrc, L210.DepRsrcPrice, L210.RenewRsrcPrice, L210.UnlimitRsrcPrice, L210.DepRsrcTechChange, L210.SmthRenewRsrcTechChange, L210.DepRsrcCalProd, L210.DepRsrcCurves_fos, L210.DepRsrcCurves_U, L210.SmthRenewRsrcCurvesGdpElast_MSW, L210.SmthRenewRsrcCurves_wind, L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV, L210.GrdRenewRsrcCurves_geo, L210.GrdRenewRsrcMax_geo, L210.GrdRenewRsrcCurves_EGS, L210.GrdRenewRsrcMax_EGS, L210.GrdRenewRsrcCurves_tradbio, L210.GrdRenewRsrcMax_tradbio, L210.DepRsrcTechChange_SSP1, L210.DepRsrcEnvironCost_SSP1, L210.DepRsrcTechChange_SSP2, L210.DepRsrcEnvironCost_SSP2, L210.DepRsrcTechChange_SSP3, L210.DepRsrcEnvironCost_SSP3, L210.DepRsrcTechChange_SSP4, L210.DepRsrcEnvironCost_SSP4, L210.DepRsrcTechChange_SSP5, L210.DepRsrcEnvironCost_SSP5)
  } else {
    stop("Unknown command")
  }
}



