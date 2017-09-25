#' module_energy_L244.building_det
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.SubregionalShares}, \code{L244.PriceExp_IntGains}, \code{L244.Floorspace}, \code{L244.DemandFunction_serv},
#' \code{L244.DemandFunction_flsp}, \code{L244.Satiation_flsp}, \code{L244.SatiationAdder}, \code{L244.ThermalBaseService}, \code{L244.GenericBaseService},
#'\code{L244.ThermalServiceSatiation}, \code{L244.GenericServiceSatiation}, \code{L244.Intgains_scalar}, \code{L244.ShellConductance_bld},
#' \code{L244.Supplysector_bld}, \code{L244.FinalEnergyKeyword_bld}, \code{L244.SubsectorShrwt_bld}, \code{L244.SubsectorShrwtFllt_bld}, \code{L244.SubsectorInterp_bld},
#' \code{L244.SubsectorInterpTo_bld}, \code{L244.SubsectorLogit_bld}, \code{L244.FuelPrefElast_bld}, \code{L244.StubTech_bld}, \code{L244.StubTechEff_bld},
#' \code{L244.StubTechCalInput_bld}, \code{L244.StubTechIntGainOutputRatio}, \code{L244.GlobalTechShrwt_bld}, \code{L244.GlobalTechCost_bld},
#' \code{L244.DeleteGenericService}, \code{L244.Satiation_flsp_SSP1}, \code{L244.SatiationAdder_SSP1}, \code{L244.GenericServiceSatiation_SSP1},
#' \code{L244.Satiation_flsp_SSP2}, \code{L244.SatiationAdder_SSP2}, \code{L244.GenericServiceSatiation_SSP2}, \code{L244.Satiation_flsp_SSP3},
#' \code{L244.SatiationAdder_SSP3}, \code{L244.GenericServiceSatiation_SSP3}, \code{L244.FuelPrefElast_bld_SSP3}, \code{L244.Satiation_flsp_SSP4},
#' \code{L244.SatiationAdder_SSP4}, \code{L244.GenericServiceSatiation_SSP4}, \code{L244.FuelPrefElast_bld_SSP4}, \code{L244.Satiation_flsp_SSP5},
#' \code{L244.SatiationAdder_SSP5}, \code{L244.GenericServiceSatiation_SSP5}, \code{L244.FuelPrefElast_bld_SSP15}, \code{L244.DeleteThermalService}. The corresponding file in the
#' original data system was \code{L244.building_det.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH September 2017
#' @export
module_energy_L244.building_det <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = "energy/A_regions",
             FILE = "energy/A44.sector",
             FILE = "energy/A44.subsector_interp",
             FILE = "energy/A44.subsector_logit",
             FILE = "energy/A44.subsector_shrwt",
             FILE = "energy/A44.fuelprefElasticity", # units
             FILE = "energy/A44.fuelprefElasticity_SSP3", # units
             FILE = "energy/A44.fuelprefElasticity_SSP4", # units
             FILE = "energy/A44.fuelprefElasticity_SSP15", # units
             FILE = "energy/A44.globaltech_shrwt",
             FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.demandFn_serv",
             FILE = "energy/A44.demandFn_flsp",
             FILE = "energy/A44.internal_gains",
             FILE = "energy/A44.satiation_flsp",
             FILE = "energy/A44.satiation_flsp_SSPs",
             FILE = "energy/A44.demand_satiation_mult",
             FILE = "energy/A44.demand_satiation_mult_SSPs",
             "L144.flsp_bm2_R_res_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             "L144.base_service_EJ_serv",
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L144.end_use_eff",
             "L144.shell_eff_R_Y",
             "L144.NEcost_75USDGJ",
             "L143.HDDCDD_scen_R_Y",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.SubregionalShares",
             "L244.PriceExp_IntGains",
             "L244.Floorspace",
             "L244.DemandFunction_serv",
             "L244.DemandFunction_flsp",
             "L244.Satiation_flsp",
             "L244.SatiationAdder",
             "L244.ThermalBaseService",
             "L244.GenericBaseService",
             "L244.ThermalServiceSatiation",
             "L244.GenericServiceSatiation",
             "L244.Intgains_scalar",
             "L244.ShellConductance_bld",
             "L244.Supplysector_bld",
             "L244.FinalEnergyKeyword_bld",
             "L244.SubsectorShrwt_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorInterpTo_bld",
             "L244.SubsectorLogit_bld",
             "L244.FuelPrefElast_bld",
             "L244.StubTech_bld",
             "L244.StubTechEff_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechIntGainOutputRatio",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.DeleteGenericService",
             "L244.Satiation_flsp_SSP1",
             "L244.SatiationAdder_SSP1",
             "L244.GenericServiceSatiation_SSP1",
             "L244.Satiation_flsp_SSP2",
             "L244.SatiationAdder_SSP2",
             "L244.GenericServiceSatiation_SSP2",
             "L244.Satiation_flsp_SSP3",
             "L244.SatiationAdder_SSP3",
             "L244.GenericServiceSatiation_SSP3",
             "L244.FuelPrefElast_bld_SSP3",
             "L244.Satiation_flsp_SSP4",
             "L244.SatiationAdder_SSP4",
             "L244.GenericServiceSatiation_SSP4",
             "L244.FuelPrefElast_bld_SSP4",
             "L244.Satiation_flsp_SSP5",
             "L244.SatiationAdder_SSP5",
             "L244.GenericServiceSatiation_SSP5",
             "L244.FuelPrefElast_bld_SSP15",
             "L244.DeleteThermalService"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
    A_regions <- get_data(all_data, "energy/A_regions")
    A44.sector <- get_data(all_data, "energy/A44.sector")
    A44.subsector_interp <- get_data(all_data, "energy/A44.subsector_interp")
    A44.subsector_logit <- get_data(all_data, "energy/A44.subsector_logit")
    A44.subsector_shrwt <- get_data(all_data, "energy/A44.subsector_shrwt")
    A44.fuelprefElasticity <- get_data(all_data, "energy/A44.fuelprefElasticity")
    A44.fuelprefElasticity_SSP3 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP3")
    A44.fuelprefElasticity_SSP4 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP4")
    A44.fuelprefElasticity_SSP15 <- get_data(all_data, "energy/A44.fuelprefElasticity_SSP15")
    A44.globaltech_shrwt <- get_data(all_data, "energy/A44.globaltech_shrwt")
    A44.gcam_consumer <- get_data(all_data, "energy/A44.gcam_consumer")
    A44.demandFn_serv <- get_data(all_data, "energy/A44.demandFn_serv")
    A44.demandFn_flsp <- get_data(all_data, "energy/A44.demandFn_flsp")
    A44.internal_gains <- get_data(all_data, "energy/A44.internal_gains")
    A44.satiation_flsp <- get_data(all_data, "energy/A44.satiation_flsp")
    A44.satiation_flsp_SSPs <- get_data(all_data, "energy/A44.satiation_flsp_SSPs")
    A44.demand_satiation_mult <- get_data(all_data, "energy/A44.demand_satiation_mult")
    A44.demand_satiation_mult_SSPs <- get_data(all_data, "energy/A44.demand_satiation_mult_SSPs")
    L144.flsp_bm2_R_res_Yh <- get_data(all_data, "L144.flsp_bm2_R_res_Yh")
    L144.flsp_bm2_R_comm_Yh <- get_data(all_data, "L144.flsp_bm2_R_comm_Yh")
    L144.base_service_EJ_serv <- get_data(all_data, "L144.base_service_EJ_serv")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    L144.end_use_eff <- get_data(all_data, "L144.end_use_eff")
    L144.shell_eff_R_Y <- get_data(all_data, "L144.shell_eff_R_Y")
    L144.NEcost_75USDGJ <- get_data(all_data, "L144.NEcost_75USDGJ")
    L143.HDDCDD_scen_R_Y <- get_data(all_data, "L143.HDDCDD_scen_R_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # ===================================================

    # ===================================================
    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubregionalShares

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.PriceExp_IntGains

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Floorspace

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.DemandFunction_serv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.DemandFunction_flsp

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.ThermalBaseService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericBaseService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.ThermalServiceSatiation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Intgains_scalar

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.ShellConductance_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Supplysector_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.FinalEnergyKeyword_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorShrwt_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubsectorShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubsectorShrwtFllt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorInterp_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubsectorInterp_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorInterpTo_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubsectorInterpTo_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SubsectorLogit_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FuelPrefElast_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.FuelPrefElast_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.StubTech_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTechEff_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.StubTechEff_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.StubTechCalInput_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.StubTechIntGainOutputRatio") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.StubTechIntGainOutputRatio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GlobalTechShrwt_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GlobalTechCost_bld

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DeleteGenericService") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.DeleteGenericService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp_SSP1") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder_SSP1") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation_SSP1") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp_SSP2") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder_SSP2") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation_SSP2") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp_SSP3") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp_SSP3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder_SSP3") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder_SSP3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation_SSP3") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation_SSP3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP3") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.FuelPrefElast_bld_SSP3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp_SSP4") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp_SSP4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder_SSP4") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder_SSP4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation_SSP4") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation_SSP4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP4") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.FuelPrefElast_bld_SSP4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.Satiation_flsp_SSP5") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.Satiation_flsp_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.SatiationAdder_SSP5") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.SatiationAdder_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.GenericServiceSatiation_SSP5") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.GenericServiceSatiation_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.FuelPrefElast_bld_SSP15") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.FuelPrefElast_bld_SSP15

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L244.DeleteThermalService") %>%
      add_precursors("L144.base_service_EJ_serv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L244.DeleteThermalService

    return_data(L244.SubregionalShares, L244.PriceExp_IntGains, L244.Floorspace, L244.DemandFunction_serv, L244.DemandFunction_flsp,
                L244.Satiation_flsp, L244.SatiationAdder, L244.ThermalBaseService, L244.GenericBaseService, object, L244.ThermalServiceSatiation,
                L244.GenericServiceSatiation, L244.Intgains_scalar, L244.ShellConductance_bld, L244.SectorLogitTables[[ curr_table ]]$data,
                L244.Supplysector_bld, L244.FinalEnergyKeyword_bld, L244.SubsectorShrwt_bld, L244.SubsectorShrwtFllt_bld, L244.SubsectorInterp_bld,
                L244.SubsectorInterpTo_bld, L244.SubsectorLogitTables[[ curr_table ]]$data, L244.SubsectorLogit_bld, L244.FuelPrefElast_bld,
                L244.StubTech_bld, L244.StubTechEff_bld, L244.StubTechCalInput_bld, L244.StubTechIntGainOutputRatio, L244.GlobalTechShrwt_bld,
                L244.GlobalTechCost_bld, L244.DeleteGenericService, L244.Satiation_flsp_SSP1, L244.SatiationAdder_SSP1,
                L244.GenericServiceSatiation_SSP1, L244.Satiation_flsp_SSP2,
                L244.SatiationAdder_SSP2, L244.GenericServiceSatiation_SSP2, L244.Satiation_flsp_SSP3, L244.SatiationAdder_SSP3,
                L244.GenericServiceSatiation_SSP3, L244.FuelPrefElast_bld_SSP3, L244.Satiation_flsp_SSP4,
                L244.SatiationAdder_SSP4, L244.GenericServiceSatiation_SSP4, L244.FuelPrefElast_bld_SSP4,
                L244.Satiation_flsp_SSP5, L244.SatiationAdder_SSP5, L244.GenericServiceSatiation_SSP5, L244.FuelPrefElast_bld_SSP15,
                L244.DeleteThermalService)
  } else {
    stop("Unknown command")
  }
}
