#' module_gcam.usa_L244.building_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.DeleteConsumer_USAbld}, \code{L244.DeleteSupplysector_USAbld}, \code{L244.SubregionalShares}, \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv}, \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService}, \code{object}, \code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld}, \code{L244.SectorLogitTables_bld[[ curr_table ]]$data}, \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld}, \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogitTables_bld[[ curr_table ]]$data}, \code{L244.SubsectorLogit_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechCalInput_bld}, \code{L244.StubTechMarket_bld}, \code{L244.GlobalTechIntGainOutputRatio}, \code{L244.GlobalTechInterpTo_bld}, \code{L244.GlobalTechEff_bld}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld}, \code{L244.GlobalTechSCurve_bld}. The corresponding file in the
#' original data system was \code{L244.building_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L244.building_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A44.gcam_consumer",
FILE = "energy/A44.sector",
FILE = "gcam-usa/calibrated_techs_bld_usa",
FILE = "gcam-usa/states_subregions",
FILE = "gcam-usa/A44.bld_shell_conductance",
FILE = "gcam-usa/A44.demandFn_flsp",
FILE = "gcam-usa/A44.demandFn_serv",
FILE = "gcam-usa/A44.gcam_consumer",
FILE = "gcam-usa/A44.satiation_flsp",
FILE = "gcam-usa/A44.sector",
FILE = "gcam-usa/A44.subsector_interp",
FILE = "gcam-usa/A44.subsector_logit",
FILE = "gcam-usa/A44.subsector_shrwt",
FILE = "gcam-usa/A44.globaltech_cost",
FILE = "gcam-usa/A44.globaltech_eff",
FILE = "gcam-usa/A44.globaltech_eff_avg",
FILE = "gcam-usa/A44.globaltech_shares",
FILE = "gcam-usa/A44.globaltech_intgains",
FILE = "gcam-usa/A44.globaltech_retirement",
FILE = "gcam-usa/A44.globaltech_shrwt",
FILE = "gcam-usa/A44.globaltech_interp",
FILE = "gcam-usa/A44.demand_satiation_mult",
 "L144.flsp_bm2_state_res",
 "L144.flsp_bm2_state_comm",
 "L144.in_EJ_state_comm_F_U_Y",
 "L144.in_EJ_state_res_F_U_Y",
 "L143.HDDCDD_scen_state",
 "L100.Pop_thous_state",
 "L100.pcGDP_thous90usd_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L244.DeleteConsumer_USAbld",
XML = "L244.DeleteSupplysector_USAbld",
XML = "L244.SubregionalShares",
XML = "L244.PriceExp_IntGains",
XML = "L244.Floorspace",
XML = "L244.DemandFunction_serv",
XML = "L244.DemandFunction_flsp",
XML = "L244.Satiation_flsp",
XML = "L244.SatiationAdder",
XML = "L244.ThermalBaseService",
XML = "L244.GenericBaseService",
XML = "object",
XML = "L244.ThermalServiceSatiation",
XML = "L244.GenericServiceSatiation",
XML = "L244.Intgains_scalar",
XML = "L244.ShellConductance_bld",
XML = "L244.SectorLogitTables_bld[[ curr_table ]]$data",
XML = "L244.Supplysector_bld",
XML = "L244.FinalEnergyKeyword_bld",
XML = "L244.SubsectorShrwt_bld",
XML = "L244.SubsectorShrwtFllt_bld",
XML = "L244.SubsectorInterp_bld",
XML = "L244.SubsectorInterpTo_bld",
XML = "L244.SubsectorLogitTables_bld[[ curr_table ]]$data",
XML = "L244.SubsectorLogit_bld",
XML = "L244.StubTech_bld",
XML = "L244.StubTechCalInput_bld",
XML = "L244.StubTechMarket_bld",
XML = "L244.GlobalTechIntGainOutputRatio",
XML = "L244.GlobalTechInterpTo_bld",
XML = "L244.GlobalTechEff_bld",
XML = "L244.GlobalTechShrwt_bld",
XML = "L244.GlobalTechCost_bld",
XML = "L244.GlobalTechSCurve_bld"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      A44.gcam_consumer <- get_data(all_data, "energy/A44.gcam_consumer")
  A44.sector <- get_data(all_data, "energy/A44.sector")
  calibrated_techs_bld_usa <- get_data(all_data, "gcam-usa/calibrated_techs_bld_usa")
  states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  A44.bld_shell_conductance <- get_data(all_data, "gcam-usa/A44.bld_shell_conductance")
  A44.demandFn_flsp <- get_data(all_data, "gcam-usa/A44.demandFn_flsp")
  A44.demandFn_serv <- get_data(all_data, "gcam-usa/A44.demandFn_serv")
  A44.gcam_consumer <- get_data(all_data, "gcam-usa/A44.gcam_consumer")
  A44.satiation_flsp <- get_data(all_data, "gcam-usa/A44.satiation_flsp")
  A44.sector <- get_data(all_data, "gcam-usa/A44.sector")
  A44.subsector_interp <- get_data(all_data, "gcam-usa/A44.subsector_interp")
  A44.subsector_logit <- get_data(all_data, "gcam-usa/A44.subsector_logit")
  A44.subsector_shrwt <- get_data(all_data, "gcam-usa/A44.subsector_shrwt")
  A44.globaltech_cost <- get_data(all_data, "gcam-usa/A44.globaltech_cost")
  A44.globaltech_eff <- get_data(all_data, "gcam-usa/A44.globaltech_eff")
  A44.globaltech_eff_avg <- get_data(all_data, "gcam-usa/A44.globaltech_eff_avg")
  A44.globaltech_shares <- get_data(all_data, "gcam-usa/A44.globaltech_shares")
  A44.globaltech_intgains <- get_data(all_data, "gcam-usa/A44.globaltech_intgains")
  A44.globaltech_retirement <- get_data(all_data, "gcam-usa/A44.globaltech_retirement")
  A44.globaltech_shrwt <- get_data(all_data, "gcam-usa/A44.globaltech_shrwt")
  A44.globaltech_interp <- get_data(all_data, "gcam-usa/A44.globaltech_interp")
  A44.demand_satiation_mult <- get_data(all_data, "gcam-usa/A44.demand_satiation_mult")
  L144.flsp_bm2_state_res <- get_data(all_data, "L144.flsp_bm2_state_res")
  L144.flsp_bm2_state_comm <- get_data(all_data, "L144.flsp_bm2_state_comm")
  L144.in_EJ_state_comm_F_U_Y <- get_data(all_data, "L144.in_EJ_state_comm_F_U_Y")
  L144.in_EJ_state_res_F_U_Y <- get_data(all_data, "L144.in_EJ_state_res_F_U_Y")
  L143.HDDCDD_scen_state <- get_data(all_data, "L143.HDDCDD_scen_state")
  L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
  L100.pcGDP_thous90usd_state <- get_data(all_data, "L100.pcGDP_thous90usd_state")

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
 add_legacy_name("L244.DeleteConsumer_USAbld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.DeleteConsumer_USAbld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.DeleteSupplysector_USAbld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.DeleteSupplysector_USAbld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubregionalShares") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubregionalShares
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.PriceExp_IntGains") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.PriceExp_IntGains
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.Floorspace") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.Floorspace
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.DemandFunction_serv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.DemandFunction_serv
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.DemandFunction_flsp") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.DemandFunction_flsp
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.Satiation_flsp") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.Satiation_flsp
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SatiationAdder") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SatiationAdder
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.ThermalBaseService") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.ThermalBaseService
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GenericBaseService") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GenericBaseService
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
 add_legacy_name("L244.ThermalServiceSatiation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.ThermalServiceSatiation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GenericServiceSatiation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GenericServiceSatiation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.Intgains_scalar") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.Intgains_scalar
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.ShellConductance_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.ShellConductance_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SectorLogitTables_bld[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SectorLogitTables_bld[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.Supplysector_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.Supplysector_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.FinalEnergyKeyword_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorShrwt_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorShrwt_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorShrwtFllt_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorInterp_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorInterp_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorInterpTo_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorInterpTo_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorLogitTables_bld[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorLogitTables_bld[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.SubsectorLogit_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.SubsectorLogit_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.StubTech_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.StubTech_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.StubTechCalInput_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.StubTechCalInput_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.StubTechMarket_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.StubTechMarket_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechIntGainOutputRatio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechIntGainOutputRatio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechInterpTo_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechInterpTo_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechEff_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechEff_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechShrwt_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechShrwt_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechCost_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechCost_bld
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L244.GlobalTechSCurve_bld") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L244.GlobalTechSCurve_bld

    return_data(L244.DeleteConsumer_USAbld, L244.DeleteSupplysector_USAbld, L244.SubregionalShares, L244.PriceExp_IntGains, L244.Floorspace, L244.DemandFunction_serv, L244.DemandFunction_flsp, L244.Satiation_flsp, L244.SatiationAdder, L244.ThermalBaseService, L244.GenericBaseService, object, L244.ThermalServiceSatiation, L244.GenericServiceSatiation, L244.Intgains_scalar, L244.ShellConductance_bld, L244.SectorLogitTables_bld[[ curr_table ]]$data, L244.Supplysector_bld, L244.FinalEnergyKeyword_bld, L244.SubsectorShrwt_bld, L244.SubsectorShrwtFllt_bld, L244.SubsectorInterp_bld, L244.SubsectorInterpTo_bld, L244.SubsectorLogitTables_bld[[ curr_table ]]$data, L244.SubsectorLogit_bld, L244.StubTech_bld, L244.StubTechCalInput_bld, L244.StubTechMarket_bld, L244.GlobalTechIntGainOutputRatio, L244.GlobalTechInterpTo_bld, L244.GlobalTechEff_bld, L244.GlobalTechShrwt_bld, L244.GlobalTechCost_bld, L244.GlobalTechSCurve_bld)
  } else {
    stop("Unknown command")
  }
}



