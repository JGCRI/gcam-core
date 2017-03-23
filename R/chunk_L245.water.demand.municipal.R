#' module_water_L245.water.demand.municipal
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L245.SectorLogitTables[[ curr_table ]]$data}, \code{L245.Supplysector}, \code{L245.SubsectorLogitTables[[ curr_table ]]$data}, \code{L245.SubsectorLogit}, \code{L245.SubsectorShrwtFllt}, \code{L245.TechShrwt}, \code{L245.TechCoef}, \code{L245.TechCost}, \code{L245.PerCapitaBased}, \code{L245.BaseService}, \code{L245.IncomeElasticity}, \code{L245.PriceElasticity}, \code{L245.aeei}. The corresponding file in the
#' original data system was \code{L245.water.demand.municipal.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L245.water.demand.municipal_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/A03.sector",
FILE = "water/A45.sector",
FILE = "water/A45.tech_cost",
FILE = "water/A45.demand",
 "L145.municipal_water_R_W_Yh_km3",
 "L145.municipal_water_cost_R_75USD_m3",
 "L145.municipal_water_eff_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L245.SectorLogitTables[[ curr_table ]]$data",
XML = "L245.Supplysector",
XML = "L245.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L245.SubsectorLogit",
XML = "L245.SubsectorShrwtFllt",
XML = "L245.TechShrwt",
XML = "L245.TechCoef",
XML = "L245.TechCost",
XML = "L245.PerCapitaBased",
XML = "L245.BaseService",
XML = "L245.IncomeElasticity",
XML = "L245.PriceElasticity",
XML = "L245.aeei"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A03.sector <- get_data(all_data, "water/A03.sector")
  A45.sector <- get_data(all_data, "water/A45.sector")
  A45.tech_cost <- get_data(all_data, "water/A45.tech_cost")
  A45.demand <- get_data(all_data, "water/A45.demand")
  L145.municipal_water_R_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_R_W_Yh_km3")
  L145.municipal_water_cost_R_75USD_m3 <- get_data(all_data, "L145.municipal_water_cost_R_75USD_m3")
  L145.municipal_water_eff_R_Y <- get_data(all_data, "L145.municipal_water_eff_R_Y")

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
 add_legacy_name("L245.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.Supplysector") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.Supplysector
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.SubsectorLogit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.SubsectorLogit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.SubsectorShrwtFllt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.SubsectorShrwtFllt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.TechShrwt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.TechShrwt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.TechCoef") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.TechCoef
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.TechCost") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.TechCost
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.PerCapitaBased") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.PerCapitaBased
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.BaseService") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.BaseService
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.IncomeElasticity") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.IncomeElasticity
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.PriceElasticity") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.PriceElasticity
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L245.aeei") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L245.aeei

    return_data(L245.SectorLogitTables[[ curr_table ]]$data, L245.Supplysector, L245.SubsectorLogitTables[[ curr_table ]]$data, L245.SubsectorLogit, L245.SubsectorShrwtFllt, L245.TechShrwt, L245.TechCoef, L245.TechCost, L245.PerCapitaBased, L245.BaseService, L245.IncomeElasticity, L245.PriceElasticity, L245.aeei)
  } else {
    stop("Unknown command")
  }
}



