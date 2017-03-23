#' module_gcam.usa_L254.transportation_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.DeleteSupplysector_USAtrn}, \code{L254.DeleteFinalDemand_USAtrn}, \code{object}, \code{L254.StubTranTechCalInput_USA}, \code{L254.StubTranTechProd_nonmotor_USA}, \code{L254.StubTranTechCalInput_passthru_USA}, \code{L254.BaseService_trn_USA}. The corresponding file in the
#' original data system was \code{L254.transportation_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L254.transportation_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/UCD_techs",
FILE = "energy/A54.globaltech_nonmotor",
FILE = "energy/A54.globaltech_passthru",
FILE = "energy/A54.sector",
FILE = "gcam-usa/states_subregions",
 "L254.Supplysector_trn",
 "L254.FinalEnergyKeyword_trn",
 "L254.tranSubsectorLogit",
 "L254.tranSubsectorShrwt",
 "L254.tranSubsectorShrwtFllt",
 "L254.tranSubsectorInterp",
 "L254.tranSubsectorInterpTo",
 "L254.tranSubsectorSpeed",
 "L254.tranSubsectorSpeed_passthru",
 "L254.tranSubsectorSpeed_noVOTT",
 "L254.tranSubsectorSpeed_nonmotor",
 "L254.tranSubsectorVOTT",
 "L254.tranSubsectorFuelPref",
 "L254.StubTranTech",
 "L254.StubTech_passthru",
 "L254.StubTech_nonmotor",
 "L254.StubTranTechLoadFactor",
 "L254.StubTranTechCost",
 "L254.StubTranTechCoef",
 "L254.PerCapitaBased_trn",
 "L254.PriceElasticity_trn",
 "L254.IncomeElasticity_trn",
 "L154.in_EJ_state_trn_m_sz_tech_F",
 "L154.out_mpkm_state_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L254.DeleteSupplysector_USAtrn",
XML = "L254.DeleteFinalDemand_USAtrn",
XML = "object",
XML = "L254.StubTranTechCalInput_USA",
XML = "L254.StubTranTechProd_nonmotor_USA",
XML = "L254.StubTranTechCalInput_passthru_USA",
XML = "L254.BaseService_trn_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      UCD_techs <- get_data(all_data, "energy/UCD_techs")
  A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor")
  A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru")
  A54.sector <- get_data(all_data, "energy/A54.sector")
  states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
  L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
  L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
  L254.tranSubsectorShrwt <- get_data(all_data, "L254.tranSubsectorShrwt")
  L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
  L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")
  L254.tranSubsectorInterpTo <- get_data(all_data, "L254.tranSubsectorInterpTo")
  L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
  L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
  L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
  L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
  L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
  L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")
  L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
  L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
  L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
  L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
  L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
  L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
  L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
  L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
  L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
  L154.in_EJ_state_trn_m_sz_tech_F <- get_data(all_data, "L154.in_EJ_state_trn_m_sz_tech_F")
  L154.out_mpkm_state_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_state_trn_nonmotor_Yh")

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
 add_legacy_name("L254.DeleteSupplysector_USAtrn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.DeleteSupplysector_USAtrn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.DeleteFinalDemand_USAtrn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.DeleteFinalDemand_USAtrn
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
 add_legacy_name("L254.StubTranTechCalInput_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechCalInput_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechProd_nonmotor_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechProd_nonmotor_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechCalInput_passthru_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechCalInput_passthru_USA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.BaseService_trn_USA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.BaseService_trn_USA

    return_data(L254.DeleteSupplysector_USAtrn, L254.DeleteFinalDemand_USAtrn, object, L254.StubTranTechCalInput_USA, L254.StubTranTechProd_nonmotor_USA, L254.StubTranTechCalInput_passthru_USA, L254.BaseService_trn_USA)
  } else {
    stop("Unknown command")
  }
}



