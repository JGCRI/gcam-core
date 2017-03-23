#' module_energy_L254.transportation_UCD
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.SectorLogitTables[[ curr_table ]]$data}, \code{L254.Supplysector_trn}, \code{L254.FinalEnergyKeyword_trn}, \code{L254.SubsectorLogitTables[[ curr_table ]]$data}, \code{L254.tranSubsectorLogit}, \code{L254.tranSubsectorShrwt}, \code{L254.tranSubsectorShrwtFllt}, \code{L254.tranSubsectorInterp}, \code{L254.tranSubsectorInterpTo}, \code{L254.tranSubsectorSpeed}, \code{L254.tranSubsectorSpeed_passthru}, \code{L254.tranSubsectorSpeed_noVOTT}, \code{L254.tranSubsectorSpeed_nonmotor}, \code{L254.tranSubsectorVOTT}, \code{L254.tranSubsectorFuelPref}, \code{L254.StubTranTech}, \code{L254.StubTech_passthru}, \code{L254.StubTech_nonmotor}, \code{L254.GlobalTechShrwt_passthru}, \code{L254.GlobalTechShrwt_nonmotor}, \code{L254.GlobalTechCoef_passthru}, \code{L254.GlobalRenewTech_nonmotor}, \code{L254.GlobalTranTechInterp}, \code{L254.GlobalTranTechShrwt}, \code{L254.GlobalTranTechSCurve}, \code{L254.StubTranTechCalInput}, \code{L254.StubTranTechLoadFactor}, \code{L254.StubTranTechCost}, \code{L254.StubTranTechCoef}, \code{L254.StubTechCalInput_passthru}, \code{L254.StubTechProd_nonmotor}, \code{L254.PerCapitaBased_trn}, \code{L254.PriceElasticity_trn}, \code{L254.IncomeElasticity_trn}, \code{L254.BaseService_trn}. The corresponding file in the
#' original data system was \code{L254.transportation_UCD.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L254.transportation_UCD_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "energy/UCD_techs",
FILE = "energy/A54.demand",
FILE = "energy/A54.demand_ssp1",
FILE = "energy/A54.incelas_R",
FILE = "energy/A54.sector",
FILE = "energy/A54.tranSubsector_logit",
FILE = "energy/A54.tranSubsector_shrwt",
FILE = "energy/A54.tranSubsector_interp",
FILE = "energy/A54.tranSubsector_VOTT",
FILE = "energy/A54.tranSubsector_VOTT_ssp1",
FILE = "energy/A54.globaltech_passthru",
FILE = "energy/A54.globaltech_nonmotor",
FILE = "energy/A54.globaltranTech_shrwt",
FILE = "energy/A54.globaltranTech_interp",
FILE = "energy/A54.globaltranTech_retire",
 "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
 "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
 "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
 "L154.loadfactor_R_trn_m_sz_tech_F_Y",
 "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
 "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L254.SectorLogitTables[[ curr_table ]]$data",
XML = "L254.Supplysector_trn",
XML = "L254.FinalEnergyKeyword_trn",
XML = "L254.SubsectorLogitTables[[ curr_table ]]$data",
XML = "L254.tranSubsectorLogit",
XML = "L254.tranSubsectorShrwt",
XML = "L254.tranSubsectorShrwtFllt",
XML = "L254.tranSubsectorInterp",
XML = "L254.tranSubsectorInterpTo",
XML = "L254.tranSubsectorSpeed",
XML = "L254.tranSubsectorSpeed_passthru",
XML = "L254.tranSubsectorSpeed_noVOTT",
XML = "L254.tranSubsectorSpeed_nonmotor",
XML = "L254.tranSubsectorVOTT",
XML = "L254.tranSubsectorFuelPref",
XML = "L254.StubTranTech",
XML = "L254.StubTech_passthru",
XML = "L254.StubTech_nonmotor",
XML = "L254.GlobalTechShrwt_passthru",
XML = "L254.GlobalTechShrwt_nonmotor",
XML = "L254.GlobalTechCoef_passthru",
XML = "L254.GlobalRenewTech_nonmotor",
XML = "L254.GlobalTranTechInterp",
XML = "L254.GlobalTranTechShrwt",
XML = "L254.GlobalTranTechSCurve",
XML = "L254.StubTranTechCalInput",
XML = "L254.StubTranTechLoadFactor",
XML = "L254.StubTranTechCost",
XML = "L254.StubTranTechCoef",
XML = "L254.StubTechCalInput_passthru",
XML = "L254.StubTechProd_nonmotor",
XML = "L254.PerCapitaBased_trn",
XML = "L254.PriceElasticity_trn",
XML = "L254.IncomeElasticity_trn",
XML = "L254.BaseService_trn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  UCD_techs <- get_data(all_data, "energy/UCD_techs")
  A54.demand <- get_data(all_data, "energy/A54.demand")
  A54.demand_ssp1 <- get_data(all_data, "energy/A54.demand_ssp1")
  A54.incelas_R <- get_data(all_data, "energy/A54.incelas_R")
  A54.sector <- get_data(all_data, "energy/A54.sector")
  A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit")
  A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt")
  A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp")
  A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT")
  A54.tranSubsector_VOTT_ssp1 <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1")
  A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru")
  A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor")
  A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt")
  A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp")
  A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire")
  L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
  L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y")
  L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y")
  L154.loadfactor_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.loadfactor_R_trn_m_sz_tech_F_Y")
  L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.speed_kmhr_R_trn_m_sz_tech_F_Y")
  L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")

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
 add_legacy_name("L254.SectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.SectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.Supplysector_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.Supplysector_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.FinalEnergyKeyword_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.FinalEnergyKeyword_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.SubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.SubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorLogit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorLogit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorShrwt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorShrwt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorShrwtFllt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorShrwtFllt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorInterp") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorInterp
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorInterpTo") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorInterpTo
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorSpeed") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorSpeed
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorSpeed_passthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorSpeed_passthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorSpeed_noVOTT") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorSpeed_noVOTT
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorSpeed_nonmotor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorSpeed_nonmotor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorVOTT") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorVOTT
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.tranSubsectorFuelPref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.tranSubsectorFuelPref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTech") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTech
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTech_passthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTech_passthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTech_nonmotor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTech_nonmotor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTechShrwt_passthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTechShrwt_passthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTechShrwt_nonmotor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTechShrwt_nonmotor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTechCoef_passthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTechCoef_passthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalRenewTech_nonmotor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalRenewTech_nonmotor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTranTechInterp") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTranTechInterp
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTranTechShrwt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTranTechShrwt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.GlobalTranTechSCurve") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.GlobalTranTechSCurve
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechCalInput") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechCalInput
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechLoadFactor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechLoadFactor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechCost") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechCost
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTranTechCoef") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTranTechCoef
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTechCalInput_passthru") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTechCalInput_passthru
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.StubTechProd_nonmotor") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.StubTechProd_nonmotor
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.PerCapitaBased_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.PerCapitaBased_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.PriceElasticity_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.PriceElasticity_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.IncomeElasticity_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.IncomeElasticity_trn
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L254.BaseService_trn") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L254.BaseService_trn

    return_data(L254.SectorLogitTables[[ curr_table ]]$data, L254.Supplysector_trn, L254.FinalEnergyKeyword_trn, L254.SubsectorLogitTables[[ curr_table ]]$data, L254.tranSubsectorLogit, L254.tranSubsectorShrwt, L254.tranSubsectorShrwtFllt, L254.tranSubsectorInterp, L254.tranSubsectorInterpTo, L254.tranSubsectorSpeed, L254.tranSubsectorSpeed_passthru, L254.tranSubsectorSpeed_noVOTT, L254.tranSubsectorSpeed_nonmotor, L254.tranSubsectorVOTT, L254.tranSubsectorFuelPref, L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor, L254.GlobalTechShrwt_passthru, L254.GlobalTechShrwt_nonmotor, L254.GlobalTechCoef_passthru, L254.GlobalRenewTech_nonmotor, L254.GlobalTranTechInterp, L254.GlobalTranTechShrwt, L254.GlobalTranTechSCurve, L254.StubTranTechCalInput, L254.StubTranTechLoadFactor, L254.StubTranTechCost, L254.StubTranTechCoef, L254.StubTechCalInput_passthru, L254.StubTechProd_nonmotor, L254.PerCapitaBased_trn, L254.PriceElasticity_trn, L254.IncomeElasticity_trn, L254.BaseService_trn)
  } else {
    stop("Unknown command")
  }
}



