#' module_gcam.usa_L223.electricity_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.DeleteSubsector_USAelec}, \code{L223.SectorNodeEquiv}, \code{L223.TechNodeEquiv}, \code{L223.PassThroughSector_elec}, \code{L223.PassThroughTech_elec_FERC}, \code{object}, \code{L223.SectorLogitTables_USAelec[[ curr_table ]]$data}, \code{L223.Supplysector_USAelec}, \code{L223.SubsectorShrwtFllt_USAelec}, \code{L223.SubsectorInterp_USAelec}, \code{L223.SubsectorLogitTables_USAelec[[ curr_table ]]$data}, \code{L223.SubsectorLogit_USAelec}, \code{L223.TechShrwt_USAelec}, \code{L223.TechCoef_USAelec}, \code{L223.Production_USAelec}, \code{L223.SectorLogitTables_elec_FERC[[ curr_table ]]$data}, \code{L223.Supplysector_elec_FERC}, \code{L223.SubsectorShrwtFllt_elec_FERC}, \code{L223.SubsectorInterp_elec_FERC}, \code{L223.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data}, \code{L223.SubsectorLogit_elec_FERC}, \code{L223.TechShrwt_elec_FERC}, \code{L223.TechCoef_elec_FERC}, \code{L223.Production_elec_FERC}, \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC}, \code{L223.LaborForceFillout_FERC}, \code{L223.SubsectorShrwt_nuc_USA}, \code{L223.StubTechFixOut_elec_USA}, \code{L223.StubTechFixOut_hydro_USA}, \code{L223.StubTechProd_elec_USA}, \code{L223.StubTechMarket_elec_USA}, \code{L223.StubTechMarket_backup_USA}, \code{L223.StubTechElecMarket_backup_USA}, \code{L223.StubTechCapFactor_elec_wind_USA}, \code{L223.StubTechCapFactor_elec_solar_USA}. The corresponding file in the
#' original data system was \code{L223.electricity_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L223.electricity_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             "L114.CapacityFactor_wind_state",
             "L119.CapFacScaler_PV_state",
             "L119.CapFacScaler_CSP_state",
             "L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwt_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.StubTechEff_elec",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechBackup_elec",
             "L1231.in_EJ_state_elec_F_tech",
             "L1231.out_EJ_state_elec_F_tech",
             "L1232.out_EJ_sR_elec",
             "L126.out_EJ_state_td_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.DeleteSubsector_USAelec",
             "L223.SectorNodeEquiv",
             "L223.TechNodeEquiv",
             "L223.PassThroughSector_elec",
             "L223.PassThroughTech_elec_FERC",
             "L223.Supplysector_USAelec",
             "L223.SubsectorShrwtFllt_USAelec",
             "L223.SubsectorInterp_USAelec",
             "L223.SubsectorLogit_USAelec",
             "L223.TechShrwt_USAelec",
             "L223.TechCoef_USAelec",
             "L223.Production_USAelec",
             "L223.Supplysector_elec_FERC",
             "L223.SubsectorShrwtFllt_elec_FERC",
             "L223.SubsectorInterp_elec_FERC",
             "L223.SubsectorLogit_elec_FERC",
             "L223.TechShrwt_elec_FERC",
             "L223.TechCoef_elec_FERC",
             "L223.Production_elec_FERC",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.LaborForceFillout_FERC",
             "L223.SubsectorShrwt_nuc_USA",
             "L223.StubTechFixOut_elec_USA",
             "L223.StubTechFixOut_hydro_USA",
             "L223.StubTechProd_elec_USA",
             "L223.StubTechMarket_elec_USA",
             "L223.StubTechMarket_backup_USA",
             "L223.StubTechElecMarket_backup_USA",
             "L223.StubTechCapFactor_elec_wind_USA",
             "L223.StubTechCapFactor_elec_solar_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L114.CapacityFactor_wind_state <- get_data(all_data, "L114.CapacityFactor_wind_state")
    L119.CapFacScaler_PV_state <- get_data(all_data, "L119.CapFacScaler_PV_state")
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.CapFacScaler_CSP_state")
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwt_elec <- get_data(all_data, "L223.SubsectorShrwt_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")
    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, "L1231.in_EJ_state_elec_F_tech")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")
    L1232.out_EJ_sR_elec <- get_data(all_data, "L1232.out_EJ_sR_elec")
    L126.out_EJ_state_td_elec <- get_data(all_data, "L126.out_EJ_state_td_elec")

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
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L223.DeleteSubsector_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.DeleteSubsector_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SectorNodeEquiv") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SectorNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechNodeEquiv") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.TechNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PassThroughSector_elec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.PassThroughSector_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PassThroughTech_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.PassThroughTech_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Supplysector_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwtFllt_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorInterp_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorLogit_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechShrwt_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.TechShrwt_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechCoef_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.TechCoef_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Production_USAelec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Production_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Supplysector_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwtFllt_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorInterp_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorLogit_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechShrwt_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.TechShrwt_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.TechCoef_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.TechCoef_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Production_elec_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Production_elec_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.InterestRate_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.InterestRate_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Pop_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Pop_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.BaseGDP_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.BaseGDP_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LaborForceFillout_FERC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LaborForceFillout_FERC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwt_nuc_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_elec_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechFixOut_elec_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_hydro_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechFixOut_hydro_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechProd_elec_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechMarket_elec_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechMarket_elec_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechMarket_backup_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechMarket_backup_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechElecMarket_backup_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechElecMarket_backup_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_wind_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechCapFactor_elec_wind_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_solar_USA") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechCapFactor_elec_solar_USA

    return_data(L223.DeleteSubsector_USAelec, L223.SectorNodeEquiv, L223.TechNodeEquiv, L223.PassThroughSector_elec,
                L223.PassThroughTech_elec_FERC, object, L223.Supplysector_USAelec, L223.SubsectorShrwtFllt_USAelec,
                L223.SubsectorInterp_USAelec, L223.SubsectorLogit_USAelec, L223.TechShrwt_USAelec, L223.TechCoef_USAelec,
                L223.Production_USAelec, L223.Supplysector_elec_FERC, L223.SubsectorShrwtFllt_elec_FERC,
                L223.SubsectorInterp_elec_FERC, L223.SubsectorLogit_elec_FERC, L223.TechShrwt_elec_FERC,
                L223.TechCoef_elec_FERC, L223.Production_elec_FERC, L223.InterestRate_FERC, L223.Pop_FERC,
                L223.BaseGDP_FERC, L223.LaborForceFillout_FERC, L223.SubsectorShrwt_nuc_USA, L223.StubTechFixOut_elec_USA,
                L223.StubTechFixOut_hydro_USA, L223.StubTechProd_elec_USA, L223.StubTechMarket_elec_USA,
                L223.StubTechMarket_backup_USA, L223.StubTechElecMarket_backup_USA, L223.StubTechCapFactor_elec_wind_USA,
                L223.StubTechCapFactor_elec_solar_USA)
  } else {
    stop("Unknown command")
  }
}
