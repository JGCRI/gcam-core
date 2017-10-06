#' module_energy_L232.industry
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.SectorLogitTables[[ curr_table ]]$data}, \code{L232.Supplysector_ind}, \code{L232.SubsectorLogitTables[[ curr_table ]]$data}, \code{L232.SubsectorLogit_ind}, \code{L232.FinalEnergyKeyword_ind}, \code{L232.SubsectorShrwt_ind}, \code{L232.SubsectorShrwtFllt_ind}, \code{L232.SubsectorInterp_ind}, \code{L232.SubsectorInterpTo_ind}, \code{L232.StubTech_ind}, \code{L232.GlobalTechShrwt_ind}, \code{L232.StubTechInterp_ind}, \code{L232.GlobalTechEff_ind}, \code{L232.GlobalTechCoef_ind}, \code{L232.GlobalTechCost_ind}, \code{L232.GlobalTechSecOut_ind}, \code{L232.GlobalTechCSeq_ind}, \code{L232.StubTechCalInput_indenergy}, \code{L232.StubTechCalInput_indfeed}, \code{L232.StubTechProd_industry}, \code{L232.StubTechCoef_industry}, \code{L232.FuelPrefElast_indenergy}, \code{L232.PerCapitaBased_ind}, \code{L232.PriceElasticity_ind}, \code{L232.BaseService_ind}, \code{object}. The corresponding file in the
#' original data system was \code{L232.industry.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L232.industry_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/fuel_energy_input",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/A32.sector",
             FILE = "energy/A32.subsector_interp",
             FILE = "energy/A32.subsector_logit",
             FILE = "energy/A32.subsector_shrwt",
             FILE = "energy/A32.globaltech_coef",
             FILE = "energy/A32.globaltech_cost",
             FILE = "energy/A32.globaltech_eff",
             FILE = "energy/A32.globaltech_shrwt",
             FILE = "energy/A32.globaltech_interp",
             FILE = "energy/A32.nonenergy_Cseq",
             FILE = "energy/A32.fuelprefElasticity",
             FILE = "energy/A32.demand",
             "L123.in_EJ_R_indchp_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh",
             FILE = "socioeconomics/A32.inc_elas_output",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.Supplysector_ind",
             "L232.SubsectorLogit_ind",
             "L232.FinalEnergyKeyword_ind",
             "L232.SubsectorShrwt_ind",
             "L232.SubsectorShrwtFllt_ind",
             "L232.SubsectorInterp_ind",
             "L232.SubsectorInterpTo_ind",
             "L232.StubTech_ind",
             "L232.GlobalTechShrwt_ind",
             "L232.StubTechInterp_ind",
             "L232.GlobalTechEff_ind",
             "L232.GlobalTechCoef_ind",
             "L232.GlobalTechCost_ind",
             "L232.GlobalTechSecOut_ind",
             "L232.GlobalTechCSeq_ind",
             "L232.StubTechCalInput_indenergy",
             "L232.StubTechCalInput_indfeed",
             "L232.StubTechProd_industry",
             "L232.StubTechCoef_industry",
             "L232.FuelPrefElast_indenergy",
             "L232.PerCapitaBased_ind",
             "L232.PriceElasticity_ind",
             "L232.BaseService_ind"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A23.chp_elecratio <- get_data(all_data, "energy/A23.chp_elecratio")
    A32.sector <- get_data(all_data, "energy/A32.sector")
    A32.subsector_interp <- get_data(all_data, "energy/A32.subsector_interp")
    A32.subsector_logit <- get_data(all_data, "energy/A32.subsector_logit")
    A32.subsector_shrwt <- get_data(all_data, "energy/A32.subsector_shrwt")
    A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef")
    A32.globaltech_cost <- get_data(all_data, "energy/A32.globaltech_cost")
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
    A32.globaltech_shrwt <- get_data(all_data, "energy/A32.globaltech_shrwt")
    A32.globaltech_interp <- get_data(all_data, "energy/A32.globaltech_interp")
    A32.nonenergy_Cseq <- get_data(all_data, "energy/A32.nonenergy_Cseq")
    A32.fuelprefElasticity <- get_data(all_data, "energy/A32.fuelprefElasticity")
    A32.demand <- get_data(all_data, "energy/A32.demand")
    L123.in_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
    A32.inc_elas_output <- get_data(all_data, "socioeconomics/A32.inc_elas_output")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L102.pcgdp_thous90USD_GCAM3_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_R_Y")
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
      add_legacy_name("L232.Supplysector_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.Supplysector_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.SubsectorLogit_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.SubsectorLogit_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.FinalEnergyKeyword_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.FinalEnergyKeyword_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.SubsectorShrwt_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.SubsectorShrwt_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.SubsectorShrwtFllt_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.SubsectorInterp_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.SubsectorInterp_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.SubsectorInterpTo_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.SubsectorInterpTo_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTech_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTech_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechShrwt_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechShrwt_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTechInterp_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTechInterp_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechEff_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechEff_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechCoef_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechCoef_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechCost_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechCost_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechSecOut_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechSecOut_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.GlobalTechCSeq_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.GlobalTechCSeq_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTechCalInput_indenergy") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTechCalInput_indenergy

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTechCalInput_indfeed") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTechCalInput_indfeed

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTechProd_industry") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTechProd_industry

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.StubTechCoef_industry") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.StubTechCoef_industry

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.FuelPrefElast_indenergy") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.FuelPrefElast_indenergy

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.PerCapitaBased_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.PerCapitaBased_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.PriceElasticity_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.PriceElasticity_ind

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.BaseService_ind") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.BaseService_ind

    return_data(L232.Supplysector_ind, L232.SubsectorLogit_ind, L232.FinalEnergyKeyword_ind,
                L232.SubsectorShrwt_ind, L232.SubsectorShrwtFllt_ind, L232.SubsectorInterp_ind,
                L232.SubsectorInterpTo_ind, L232.StubTech_ind, L232.GlobalTechShrwt_ind,
                L232.StubTechInterp_ind, L232.GlobalTechEff_ind, L232.GlobalTechCoef_ind,
                L232.GlobalTechCost_ind, L232.GlobalTechSecOut_ind, L232.GlobalTechCSeq_ind,
                L232.StubTechCalInput_indenergy, L232.StubTechCalInput_indfeed, L232.StubTechProd_industry,
                L232.StubTechCoef_industry, L232.FuelPrefElast_indenergy, L232.PerCapitaBased_ind,
                L232.PriceElasticity_ind, L232.BaseService_ind)
  } else {
    stop("Unknown command")
  }
}
