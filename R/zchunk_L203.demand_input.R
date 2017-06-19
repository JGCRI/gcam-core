#' module_aglu_L203.demand_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.SectorLogitTables[[ curr_table ]]$data}, \code{L203.Supplysector_demand}, \code{L203.SubsectorLogitTables[[ curr_table ]]$data}, \code{L203.SubsectorAll_demand}, \code{L203.StubTech_demand}, \code{L203.GlobalTechCoef_demand}, \code{L203.GlobalTechShrwt_demand}, \code{L203.StubTechProd_food_crop}, \code{L203.StubTechProd_food_meat}, \code{L203.StubTechProd_nonfood_crop}, \code{L203.StubTechProd_nonfood_meat}, \code{L203.StubTechProd_For}, \code{L203.StubTechFixOut_exp}, \code{L203.StubCalorieContent_crop}, \code{L203.StubCalorieContent_meat}, \code{L203.PerCapitaBased}, \code{L203.BaseService}, \code{L203.IncomeElasticity}, \code{L203.PriceElasticity}, \code{L203.FuelPrefElast_ssp1}, \code{L203.IncomeElasticity_SSP1}, \code{L203.IncomeElasticity_SSP2}, \code{L203.IncomeElasticity_SSP3}, \code{L203.IncomeElasticity_SSP4}, \code{L203.IncomeElasticity_SSP5}. The corresponding file in the
#' original data system was \code{L203.demand_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L203.demand_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_demand_supplysector",
             FILE = "aglu/A_demand_subsector",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_fuelprefElasticity_ssp1",
             "L101.ag_Food_Pcal_R_C_Y",
             "L101.ag_kcalg_R_C_Y",
             "L105.an_Food_Pcal_R_C_Y",
             "L105.an_kcalg_R_C_Y",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y",
             "L110.For_ALL_bm3_R_Y",
             "L134.pcFood_kcald_R_Dmnd_Y",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp5",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.SectorLogitTables[[ curr_table ]]$data",
             "L203.Supplysector_demand",
             "L203.SubsectorLogitTables[[ curr_table ]]$data",
             "L203.SubsectorAll_demand",
             "L203.StubTech_demand",
             "L203.GlobalTechCoef_demand",
             "L203.GlobalTechShrwt_demand",
             "L203.StubTechProd_food_crop",
             "L203.StubTechProd_food_meat",
             "L203.StubTechProd_nonfood_crop",
             "L203.StubTechProd_nonfood_meat",
             "L203.StubTechProd_For",
             "L203.StubTechFixOut_exp",
             "L203.StubCalorieContent_crop",
             "L203.StubCalorieContent_meat",
             "L203.PerCapitaBased",
             "L203.BaseService",
             "L203.IncomeElasticity",
             "L203.PriceElasticity",
             "L203.FuelPrefElast_ssp1",
             "L203.IncomeElasticity_SSP1",
             "L203.IncomeElasticity_SSP2",
             "L203.IncomeElasticity_SSP3",
             "L203.IncomeElasticity_SSP4",
             "L203.IncomeElasticity_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_demand_supplysector <- get_data(all_data, "aglu/A_demand_supplysector")
    A_demand_subsector <- get_data(all_data, "aglu/A_demand_subsector")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_fuelprefElasticity_ssp1 <- get_data(all_data, "aglu/A_fuelprefElasticity_ssp1")
    L101.ag_Food_Pcal_R_C_Y <- get_data(all_data, "L101.ag_Food_Pcal_R_C_Y")
    L101.ag_kcalg_R_C_Y <- get_data(all_data, "L101.ag_kcalg_R_C_Y")
    L105.an_Food_Pcal_R_C_Y <- get_data(all_data, "L105.an_Food_Pcal_R_C_Y")
    L105.an_kcalg_R_C_Y <- get_data(all_data, "L105.an_kcalg_R_C_Y")
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y")
    L109.an_ALL_Mt_R_C_Y <- get_data(all_data, "L109.an_ALL_Mt_R_C_Y")
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y")
    L134.pcFood_kcald_R_Dmnd_Y <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y")
    L134.pcFood_kcald_R_Dmnd_Y_ssp1 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp1")
    L134.pcFood_kcald_R_Dmnd_Y_ssp2 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp2")
    L134.pcFood_kcald_R_Dmnd_Y_ssp3 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp3")
    L134.pcFood_kcald_R_Dmnd_Y_ssp4 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp4")
    L134.pcFood_kcald_R_Dmnd_Y_ssp5 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp5")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
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
      add_legacy_name("L203.SectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.Supplysector_demand
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SubsectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SubsectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SubsectorAll_demand
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTech_demand
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechCoef_demand") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.GlobalTechCoef_demand
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechShrwt_demand") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.GlobalTechShrwt_demand
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_food_crop") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_food_crop
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_food_meat") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_food_meat
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_nonfood_crop") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_nonfood_crop
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_nonfood_meat") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_nonfood_meat
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechFixOut_exp") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechFixOut_exp
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_crop") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubCalorieContent_crop
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_meat") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubCalorieContent_meat
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PerCapitaBased") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.PerCapitaBased
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.BaseService") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.BaseService
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PriceElasticity") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.PriceElasticity
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.FuelPrefElast_ssp1") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.FuelPrefElast_ssp1
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP1") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP1
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP2") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP2
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP3") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP4") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP4
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP5") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP5

    # THIS DOESN'T SEEM TO BE FULL, CORRECT RETURN DATA - looks like L203 has a bunch of funkiness in it
    # Take a look at original code and talk to Ben as necessary!
    return_data(L203.SectorLogitTables[[ curr_table ]]$data, L203.Supplysector_demand, L203.SubsectorLogitTables[[ curr_table ]]$data, L203.SubsectorAll_demand, L203.StubTech_demand, L203.GlobalTechCoef_demand, L203.GlobalTechShrwt_demand, L203.StubTechProd_food_crop, L203.StubTechProd_food_meat, L203.StubTechProd_nonfood_crop, L203.StubTechProd_nonfood_meat, L203.StubTechProd_For, L203.StubTechFixOut_exp, L203.StubCalorieContent_crop, L203.StubCalorieContent_meat, L203.PerCapitaBased, L203.BaseService, L203.IncomeElasticity, L203.PriceElasticity, L203.FuelPrefElast_ssp1, L203.IncomeElasticity_SSP1, L203.IncomeElasticity_SSP2, L203.IncomeElasticity_SSP3, L203.IncomeElasticity_SSP4, L203.IncomeElasticity_SSP5)
  } else {
    stop("Unknown command")
  }
}
