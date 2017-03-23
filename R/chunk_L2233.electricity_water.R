#' module_water_L2233.electricity_water
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{new_object}, \code{L2233.SectorNodeEquiv}, \code{L2233.TechNodeEquiv}, \code{L2233.StubTech_elecPassthru}, \code{L2233.StubTechProd_elecPassthru}, \code{L2233.GlobalPassThroughTech}, \code{L2233.GlobalTechEff_elecPassthru}, \code{L2233.GlobalTechShrwt_elecPassthru}, \code{L2233.GlobalIntTechCapital_elec}, \code{L2233.GlobalTechCapital_elecPassthru}, \code{L2233.GlobalIntTechOMfixed_elec}, \code{L2233.GlobalTechOMfixed_elecPassthru}, \code{L2233.GlobalIntTechOMvar_elec}, \code{L2233.GlobalTechOMvar_elecPassthru}, \code{L2233.PassThroughSector_elec_cool}, \code{L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data}, \code{L2233.Supplysector_elec_cool}, \code{L2233.ElecReserve_elec_cool}, \code{L2233.SubsectorShrwtFllt_elec_cool}, \code{L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data}, \code{L2233.SubsectorLogit_elec_cool}, \code{L2233.StubTech_elec_cool}, \code{L2233.StubTechEff_elec_cool}, \code{L2233.StubTechProd_elec_cool}, \code{L2233.StubTechFixOut_hydro}, \code{L2233.StubTechShrwt_elec_cool}, \code{L2233.GlobalTechCapital_elec_cool}, \code{L2233.GlobalIntTechCapital_elec_cool}, \code{L2233.GlobalTechCoef_elec_cool}, \code{L2233.GlobalIntTechCoef_elec_cool}, \code{L2233.InputEmissCoeff_hist_elecPassthru}, \code{L2233.InputEmissCoeff_fut_elecPassthru}. The corresponding file in the
#' original data system was \code{L2233.electricity_water.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L2233.electricity_water_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/calibrated_techs",
FILE = "energy/A23.globalinttech",
FILE = "energy/A23.globaltech_shrwt",
FILE = "energy/A23.sector",
FILE = "water/elec_tech_water_map",
FILE = "water/A03.sector",
FILE = "water/A23.CoolingSystemCosts",
FILE = "water/Macknick_elec_water_m3MWh",
 "L1231.out_EJ_R_elec_F_tech_Yh",
 "L1233.out_EJ_R_elec_F_tech_Yh_cool",
 "L1233.in_EJ_R_elec_F_tech_Yh_cool",
 "L1233.shrwt_R_elec_cool_Yf",
 "L223.StubTechEff_elec",
 "i",
 "L201.en_bcoc_emissions",
 "L241.nonco2_tech_coeff"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "object",
XML = "new_object",
XML = "L2233.SectorNodeEquiv",
XML = "L2233.TechNodeEquiv",
XML = "L2233.StubTech_elecPassthru",
XML = "L2233.StubTechProd_elecPassthru",
XML = "L2233.GlobalPassThroughTech",
XML = "L2233.GlobalTechEff_elecPassthru",
XML = "L2233.GlobalTechShrwt_elecPassthru",
XML = "L2233.GlobalIntTechCapital_elec",
XML = "L2233.GlobalTechCapital_elecPassthru",
XML = "L2233.GlobalIntTechOMfixed_elec",
XML = "L2233.GlobalTechOMfixed_elecPassthru",
XML = "L2233.GlobalIntTechOMvar_elec",
XML = "L2233.GlobalTechOMvar_elecPassthru",
XML = "L2233.PassThroughSector_elec_cool",
XML = "L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data",
XML = "L2233.Supplysector_elec_cool",
XML = "L2233.ElecReserve_elec_cool",
XML = "L2233.SubsectorShrwtFllt_elec_cool",
XML = "L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data",
XML = "L2233.SubsectorLogit_elec_cool",
XML = "L2233.StubTech_elec_cool",
XML = "L2233.StubTechEff_elec_cool",
XML = "L2233.StubTechProd_elec_cool",
XML = "L2233.StubTechFixOut_hydro",
XML = "L2233.StubTechShrwt_elec_cool",
XML = "L2233.GlobalTechCapital_elec_cool",
XML = "L2233.GlobalIntTechCapital_elec_cool",
XML = "L2233.GlobalTechCoef_elec_cool",
XML = "L2233.GlobalIntTechCoef_elec_cool",
XML = "L2233.InputEmissCoeff_hist_elecPassthru",
XML = "L2233.InputEmissCoeff_fut_elecPassthru"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
  A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech")
  A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
  A23.sector <- get_data(all_data, "energy/A23.sector")
  elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map")
  A03.sector <- get_data(all_data, "water/A03.sector")
  A23.CoolingSystemCosts <- get_data(all_data, "water/A23.CoolingSystemCosts")
  Macknick_elec_water_m3MWh <- get_data(all_data, "water/Macknick_elec_water_m3MWh")
  L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
  L1233.out_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.out_EJ_R_elec_F_tech_Yh_cool")
  L1233.in_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.in_EJ_R_elec_F_tech_Yh_cool")
  L1233.shrwt_R_elec_cool_Yf <- get_data(all_data, "L1233.shrwt_R_elec_cool_Yf")
  L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
  i <- get_data(all_data, "i")
  L201.en_bcoc_emissions <- get_data(all_data, "L201.en_bcoc_emissions")
  L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff")

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
 add_legacy_name("object") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   object
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("new_object") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   new_object
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.SectorNodeEquiv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.SectorNodeEquiv
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.TechNodeEquiv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.TechNodeEquiv
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTech_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTech_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTechProd_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTechProd_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalPassThroughTech") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalPassThroughTech
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechEff_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechEff_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechShrwt_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechShrwt_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalIntTechCapital_elec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechCapital_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechCapital_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalIntTechOMfixed_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalIntTechOMfixed_elec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechOMfixed_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechOMfixed_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalIntTechOMvar_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalIntTechOMvar_elec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechOMvar_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechOMvar_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.PassThroughSector_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.PassThroughSector_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.Supplysector_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.Supplysector_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.ElecReserve_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.ElecReserve_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.SubsectorShrwtFllt_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.SubsectorShrwtFllt_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.SubsectorLogit_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTech_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTech_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTechEff_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTechEff_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTechProd_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTechProd_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTechFixOut_hydro") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTechFixOut_hydro
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.StubTechShrwt_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.StubTechShrwt_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechCapital_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalIntTechCapital_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalIntTechCapital_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalTechCoef_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.GlobalIntTechCoef_elec_cool") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.GlobalIntTechCoef_elec_cool
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.InputEmissCoeff_hist_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.InputEmissCoeff_hist_elecPassthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2233.InputEmissCoeff_fut_elecPassthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2233.InputEmissCoeff_fut_elecPassthru

    return_data(object, new_object, L2233.SectorNodeEquiv, L2233.TechNodeEquiv, L2233.StubTech_elecPassthru, L2233.StubTechProd_elecPassthru, L2233.GlobalPassThroughTech, L2233.GlobalTechEff_elecPassthru, L2233.GlobalTechShrwt_elecPassthru, L2233.GlobalIntTechCapital_elec, L2233.GlobalTechCapital_elecPassthru, L2233.GlobalIntTechOMfixed_elec, L2233.GlobalTechOMfixed_elecPassthru, L2233.GlobalIntTechOMvar_elec, L2233.GlobalTechOMvar_elecPassthru, L2233.PassThroughSector_elec_cool, L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data, L2233.Supplysector_elec_cool, L2233.ElecReserve_elec_cool, L2233.SubsectorShrwtFllt_elec_cool, L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data, L2233.SubsectorLogit_elec_cool, L2233.StubTech_elec_cool, L2233.StubTechEff_elec_cool, L2233.StubTechProd_elec_cool, L2233.StubTechFixOut_hydro, L2233.StubTechShrwt_elec_cool, L2233.GlobalTechCapital_elec_cool, L2233.GlobalIntTechCapital_elec_cool, L2233.GlobalTechCoef_elec_cool, L2233.GlobalIntTechCoef_elec_cool, L2233.InputEmissCoeff_hist_elecPassthru, L2233.InputEmissCoeff_fut_elecPassthru)
  } else {
    stop("Unknown command")
  }
}



