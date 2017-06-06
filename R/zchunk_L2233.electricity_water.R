#' module_water_L2233.electricity_water
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{new_object}, \code{L2233.SectorNodeEquiv}, \code{L2233.TechNodeEquiv}, \code{L2233.StubTech_elecPassthru}, \code{L2233.StubTechProd_elecPassthru}, \code{L2233.GlobalPassThroughTech}, \code{L2233.GlobalTechEff_elecPassthru}, \code{L2233.GlobalTechShrwt_elecPassthru}, \code{L2233.GlobalIntTechCapital_elec}, \code{L2233.GlobalTechCapital_elecPassthru}, \code{L2233.GlobalIntTechOMfixed_elec}, \code{L2233.GlobalTechOMfixed_elecPassthru}, \code{L2233.GlobalIntTechOMvar_elec}, \code{L2233.GlobalTechOMvar_elecPassthru}, \code{L2233.PassThroughSector_elec_cool}, \code{L2233.SectorLogitTables_elec_cool}, \code{L2233.Supplysector_elec_cool}, \code{L2233.ElecReserve_elec_cool}, \code{L2233.SubsectorShrwtFllt_elec_cool}, \code{L2233.SubsectorLogitTables_elec_cooling}, \code{L2233.SubsectorLogit_elec_cool}, \code{L2233.StubTech_elec_cool}, \code{L2233.StubTechEff_elec_cool}, \code{L2233.StubTechProd_elec_cool}, \code{L2233.StubTechFixOut_hydro}, \code{L2233.StubTechShrwt_elec_cool}, \code{L2233.GlobalTechCapital_elec_cool}, \code{L2233.GlobalIntTechCapital_elec_cool}, \code{L2233.GlobalTechCoef_elec_cool}, \code{L2233.GlobalIntTechCoef_elec_cool}, \code{L2233.InputEmissCoeff_hist_elecPassthru}, \code{L2233.InputEmissCoeff_fut_elecPassthru}. The corresponding file in the
#' original data system was \code{L2233.electricity_water.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST June 2017
#' @export
module_water_L2233.electricity_water <- function(command, ...) {
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
             FILE = "temp-data-inject/L1233.out_EJ_R_elec_F_tech_Yh_cool",
             FILE = "temp-data-inject/L1233.in_EJ_R_elec_F_tech_Yh_cool",
             FILE = "temp-data-inject/L1233.shrwt_R_elec_cool_Yf",
             FILE = "temp-data-inject/L223.StubTechEff_elec",
             FILE = "temp-data-inject/L201.en_bcoc_emissions",
             FILE = "temp-data-inject/L241.nonco2_tech_coeff"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("object",
             "new_object",
             "L2233.SectorNodeEquiv",
             "L2233.TechNodeEquiv",
             "L2233.StubTech_elecPassthru",
             "L2233.StubTechProd_elecPassthru",
             "L2233.GlobalPassThroughTech",
             "L2233.GlobalTechEff_elecPassthru",
             "L2233.GlobalTechShrwt_elecPassthru",
             "L2233.GlobalIntTechCapital_elec",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalIntTechOMfixed_elec",
             "L2233.GlobalTechOMfixed_elecPassthru",
             "L2233.GlobalIntTechOMvar_elec",
             "L2233.GlobalTechOMvar_elecPassthru",
             "L2233.PassThroughSector_elec_cool",
             "L2233.SectorLogitTables_elec_cool", #[[ curr_table ]]$data", (*note: also removed above)
             "L2233.Supplysector_elec_cool",
             "L2233.ElecReserve_elec_cool",
             "L2233.SubsectorShrwtFllt_elec_cool",
             "L2233.SubsectorLogitTables_elec_cooling", #[[ curr_table ]]$data", *
             "L2233.SubsectorLogit_elec_cool",
             "L2233.StubTech_elec_cool",
             "L2233.StubTechEff_elec_cool",
             "L2233.StubTechProd_elec_cool",
             "L2233.StubTechFixOut_hydro",
             "L2233.StubTechShrwt_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool",
             "L2233.InputEmissCoeff_hist_elecPassthru",
             "L2233.InputEmissCoeff_fut_elecPassthru"))
  } else if(command == driver.MAKE) {

    curr_table <- NULL                  # silence package checks

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
    L1233.out_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "temp-data-inject/L1233.out_EJ_R_elec_F_tech_Yh_cool")
    L1233.in_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "temp-data-inject/L1233.in_EJ_R_elec_F_tech_Yh_cool")
    L1233.shrwt_R_elec_cool_Yf <- get_data(all_data, "temp-data-inject/L1233.shrwt_R_elec_cool_Yf")

    # Read in 40 L223 files as a list object
    L223_fileNames <- c("AvgFossilEffKeyword_elec",
                        "ElecReserve",
                        "EQUIV_TABLE",
                        "GlobalIntTechBackup_elec",
                        "GlobalIntTechCapital_elec",
                        "GlobalIntTechEff_elec",
                        "GlobalIntTechLifetime_elec",
                        "GlobalIntTechOMfixed_elec",
                        "GlobalIntTechOMvar_elec",
                        "GlobalIntTechShrwt_elec",
                        "GlobalTechCapture_elec",
                        "GlobalTechCapital_elec",
                        "GlobalTechEff_elec",
                        "GlobalTechInterp_elec",
                        "GlobalTechLifetime_elec",
                        "GlobalTechOMfixed_elec",
                        "GlobalTechOMvar_elec",
                        "GlobalTechProfitShutdown_elec",
                        "GlobalTechSCurve_elec",
                        "GlobalTechShrwt_elec",
                        "PrimaryRenewKeyword_elec",
                        "PrimaryRenewKeywordInt_elec",
                        "StubTech_elec",
                        "StubTechCalInput_elec",
                        "StubTechCapFactor_elec",
                        "StubTechEff_elec",
                        "StubTechFixOut_elec",
                        "StubTechFixOut_hydro",
                        "StubTechProd_elec",
                        "SubsectorInterp_elec",
                        "SubsectorInterpTo_elec",
                        "SubsectorLogit_absolute-cost-logit",
                        "SubsectorLogit_elec",
                        "SubsectorLogit_relative-cost-logit",
                        "SubsectorShrwt_nuc",
                        "SubsectorShrwt_renew",
                        "SubsectorShrwtFllt_elec",
                        "Supplysector_absolute-cost-logit",
                        "Supplysector_elec",
                        "Supplysector_relative-cost-logit")



    get_data_ <- function(name, all_data) {
      assertthat::assert_that(is_data_list(all_data))
      if(is.null(all_data[[name]])) {
        stop("Data system: couldn't find ", name)
      }
      all_data[[name]]
    }

    L223_data <- sapply(L223_fileNames, get_data, expr = name, all_data = all_data)

    L223.StubTechEff_elec <- get_data(all_data, "temp-data-inject/L223.StubTechEff_elec")

    # Need to change this (used to be a loop in old code) into explicit get_data calls
    #    i <- get_data(all_data, "i")
    # L223.AvgFossilEffKeyword_elec <- get_data(all_data, "L223.AvgFossilEffKeyword_elec")
    # L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    # L223.EQUIV_TABLE <- get_data(all_data, "L223.EQUIV_TABLE")
    # L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")
    # L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec")
    # L223.GlobalIntTechEff_elec <- get_data(all_data, "L223.GlobalIntTechEff_elec")
    # L223.GlobalIntTechLifetime_elec <- get_data(all_data, "L223.GlobalIntTechLifetime_elec")
    # L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec")
    # L223.GlobalIntTechOMvar_elec <- get_data(all_data, "L223.GlobalIntTechOMvar_elec")
    # L223.GlobalIntTechShrwt_elec <- get_data(all_data, "L223.GlobalIntTechShrwt_elec")
    # L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec")
    # L223.GlobalTechCapture_elec <- get_data(all_data, "L223.GlobalTechCapture_elec")
    # L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")
    # L223.GlobalTechInterp_elec <- get_data(all_data, "L223.GlobalTechInterp_elec")
    # L223.GlobalTechLifetime_elec <- get_data(all_data, "L223.GlobalTechLifetime_elec")
    # L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec")
    # L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec")
    # L223.GlobalTechProfitShutdown_elec <- get_data(all_data, "L223.GlobalTechProfitShutdown_elec")
    # L223.GlobalTechSCurve_elec <- get_data(all_data, "L223.GlobalTechSCurve_elec")
    # L223.GlobalTechShrwt_elec <- get_data(all_data, "L223.GlobalTechShrwt_elec")
    # L223.PrimaryRenewKeyword_elec <- get_data(all_data, "L223.PrimaryRenewKeyword_elec")
    # L223.PrimaryRenewKeywordInt_elec <- get_data(all_data, "L223.PrimaryRenewKeywordInt_elec")
    # L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    # L223.StubTechCalInput_elec <- get_data(all_data, "L223.StubTechCalInput_elec")
    # L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    # L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    # L223.StubTechFixOut_elec <- get_data(all_data, "L223.StubTechFixOut_elec")
    # L223.StubTechFixOut_hydro <- get_data(all_data, "L223.StubTechFixOut_hydro")
    # L223.StubTechProd_elec <- get_data(all_data, "L223.StubTechProd_elec")
    # L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    # L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    # L223.SubsectorLogit_absolute-cost-logit <- get_data(all_data, "L223.SubsectorLogit_absolute-cost-logit")
    # L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    # L223.SubsectorLogit_relative-cost-logit <- get_data(all_data, "L223.SubsectorLogit_relative-cost-logit")
    # L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    # L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    # L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    # L223.Supplysector_absolute-cost-logit <- get_data(all_data, "L223.Supplysector_absolute-cost-logit")
    # L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    # L223.Supplysector_relative-cost-logit <- get_data(all_data, "L223.Supplysector_relative-cost-logit")


    ## ^^ CHECK IF ALL THESE INPUTS ARE REALLY REQUIRED

    L201.en_bcoc_emissions <- get_data(all_data, "temp-data-inject/L201.en_bcoc_emissions")
    L241.nonco2_tech_coeff <- get_data(all_data, "temp-data-inject/L241.nonco2_tech_coeff")

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
    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("new_object") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      new_object

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SectorNodeEquiv") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SectorNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.TechNodeEquiv") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.TechNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTech_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTech_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechProd_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechProd_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalPassThroughTech") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalPassThroughTech

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechEff_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechEff_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechShrwt_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCapital_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCapital_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapital_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechOMfixed_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechOMfixed_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechOMvar_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechOMvar_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.PassThroughSector_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.PassThroughSector_elec_cool

    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SectorLogitTables_elec_cool#[[ curr_table ]]$data

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.Supplysector_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.Supplysector_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.ElecReserve_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.ElecReserve_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorShrwtFllt_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorShrwtFllt_elec_cool

    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorLogitTables_elec_cooling#[[ curr_table ]]$data

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorLogit_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTech_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTech_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechEff_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechEff_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechProd_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechProd_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechFixOut_hydro") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechFixOut_hydro

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechShrwt_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechShrwt_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapital_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCapital_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCoef_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCoef_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCoef_elec_cool
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.InputEmissCoeff_hist_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.InputEmissCoeff_hist_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.InputEmissCoeff_fut_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.InputEmissCoeff_fut_elecPassthru

    return_data(object,
                new_object,
                L2233.SectorNodeEquiv,
                L2233.TechNodeEquiv,
                L2233.StubTech_elecPassthru,
                L2233.StubTechProd_elecPassthru,
                L2233.GlobalPassThroughTech,
                L2233.GlobalTechEff_elecPassthru,
                L2233.GlobalTechShrwt_elecPassthru,
                L2233.GlobalIntTechCapital_elec,
                L2233.GlobalTechCapital_elecPassthru,
                L2233.GlobalIntTechOMfixed_elec,
                L2233.GlobalTechOMfixed_elecPassthru,
                L2233.GlobalIntTechOMvar_elec,
                L2233.GlobalTechOMvar_elecPassthru,
                L2233.PassThroughSector_elec_cool,
                L2233.SectorLogitTables_elec_cool, #[[ curr_table ]]$data,
                L2233.Supplysector_elec_cool,
                L2233.ElecReserve_elec_cool,
                L2233.SubsectorShrwtFllt_elec_cool,
                L2233.SubsectorLogitTables_elec_cooling,#[[ curr_table ]]$data,
                L2233.SubsectorLogit_elec_cool,
                L2233.StubTech_elec_cool,
                L2233.StubTechEff_elec_cool,
                L2233.StubTechProd_elec_cool,
                L2233.StubTechFixOut_hydro,
                L2233.StubTechShrwt_elec_cool,
                L2233.GlobalTechCapital_elec_cool,
                L2233.GlobalIntTechCapital_elec_cool,
                L2233.GlobalTechCoef_elec_cool,
                L2233.GlobalIntTechCoef_elec_cool,
                L2233.InputEmissCoeff_hist_elecPassthru,
                L2233.InputEmissCoeff_fut_elecPassthru)
  } else {
    stop("Unknown command")
  }
}
