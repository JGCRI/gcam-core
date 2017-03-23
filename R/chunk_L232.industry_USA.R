#' module_gcam.usa_L232.industry_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.DeleteSupplysector_USAind}, \code{L232.DeleteFinalDemand_USAind}, \code{object}, \code{L232.StubTechCalInput_indenergy_USA}, \code{L232.StubTechCalInput_indfeed_USA}, \code{L232.StubTechProd_industry_USA}, \code{L232.StubTechCoef_industry_USA}, \code{L232.StubTechMarket_ind_USA}, \code{L232.StubTechSecMarket_ind_USA}, \code{L232.BaseService_ind_USA}. The corresponding file in the
#' original data system was \code{L232.industry_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L232.industry_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
FILE = "energy/A32.demand",
FILE = "energy/A32.globaltech_coef",
FILE = "energy/A32.globaltech_eff",
FILE = "energy/A32.globaltech_shrwt",
FILE = "energy/calibrated_techs",
 "L232.Supplysector_ind",
 "L232.FinalEnergyKeyword_ind",
 "L232.SubsectorLogit_ind",
 "L232.SubsectorShrwt_ind",
 "L232.SubsectorShrwtFllt_ind",
 "L232.SubsectorInterp_ind",
 "L232.SubsectorInterpTo_ind",
 "L232.FuelPrefElast_indenergy",
 "L232.StubTech_ind",
 "L232.StubTechInterp_ind",
 "L232.PerCapitaBased_ind",
 "L232.PriceElasticity_ind",
 "L232.IncomeElasticity_ind_gcam3",
 "L132.in_EJ_state_indnochp_F",
 "L132.in_EJ_state_indfeed_F",
 "L132.in_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L232.DeleteSupplysector_USAind",
XML = "L232.DeleteFinalDemand_USAind",
XML = "object",
XML = "L232.StubTechCalInput_indenergy_USA",
XML = "L232.StubTechCalInput_indfeed_USA",
XML = "L232.StubTechProd_industry_USA",
XML = "L232.StubTechCoef_industry_USA",
XML = "L232.StubTechMarket_ind_USA",
XML = "L232.StubTechSecMarket_ind_USA",
XML = "L232.BaseService_ind_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  A32.demand <- get_data(all_data, "energy/A32.demand")
  A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef")
  A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
  A32.globaltech_shrwt <- get_data(all_data, "energy/A32.globaltech_shrwt")
  calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
  L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind")
  L232.FinalEnergyKeyword_ind <- get_data(all_data, "L232.FinalEnergyKeyword_ind")
  L232.SubsectorLogit_ind <- get_data(all_data, "L232.SubsectorLogit_ind")
  L232.SubsectorShrwt_ind <- get_data(all_data, "L232.SubsectorShrwt_ind")
  L232.SubsectorShrwtFllt_ind <- get_data(all_data, "L232.SubsectorShrwtFllt_ind")
  L232.SubsectorInterp_ind <- get_data(all_data, "L232.SubsectorInterp_ind")
  L232.SubsectorInterpTo_ind <- get_data(all_data, "L232.SubsectorInterpTo_ind")
  L232.FuelPrefElast_indenergy <- get_data(all_data, "L232.FuelPrefElast_indenergy")
  L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
  L232.StubTechInterp_ind <- get_data(all_data, "L232.StubTechInterp_ind")
  L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind")
  L232.PriceElasticity_ind <- get_data(all_data, "L232.PriceElasticity_ind")
  L232.IncomeElasticity_ind_gcam3 <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3")
  L132.in_EJ_state_indnochp_F <- get_data(all_data, "L132.in_EJ_state_indnochp_F")
  L132.in_EJ_state_indfeed_F <- get_data(all_data, "L132.in_EJ_state_indfeed_F")
  L132.in_EJ_state_indchp_F <- get_data(all_data, "L132.in_EJ_state_indchp_F")

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
 add_legacy_name("L232.DeleteSupplysector_USAind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.DeleteSupplysector_USAind
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.DeleteFinalDemand_USAind") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.DeleteFinalDemand_USAind
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
 add_legacy_name("L232.StubTechCalInput_indenergy_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechCalInput_indenergy_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.StubTechCalInput_indfeed_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechCalInput_indfeed_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.StubTechProd_industry_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechProd_industry_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.StubTechCoef_industry_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechCoef_industry_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.StubTechMarket_ind_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechMarket_ind_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.StubTechSecMarket_ind_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.StubTechSecMarket_ind_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.BaseService_ind_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.BaseService_ind_USA

    return_data(L232.DeleteSupplysector_USAind, L232.DeleteFinalDemand_USAind, object, L232.StubTechCalInput_indenergy_USA, L232.StubTechCalInput_indfeed_USA, L232.StubTechProd_industry_USA, L232.StubTechCoef_industry_USA, L232.StubTechMarket_ind_USA, L232.StubTechSecMarket_ind_USA, L232.BaseService_ind_USA)
  } else {
    stop("Unknown command")
  }
}



