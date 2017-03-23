#' module_aglu_LB134.Diet_Rfao
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L134.pcFood_kcald_R_Dmnd_Y}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp1}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp2}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp3}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp4}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp5}. The corresponding file in the
#' original data system was \code{LB134.Diet_Rfao.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB134.Diet_Rfao_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/A_FoodDemand_SSPs",
FILE = "common/iso_GCAM_regID",
FILE = "aglu/AGLU_ctry",
FILE = "aglu/FAO2050_items_cal",
FILE = "aglu/FAO2050_Diet",
 "L100.FAO_ag_Food_t",
 "L100.FAO_an_Food_t",
 "L101.ag_Food_Pcal_R_C_Y",
 "L105.an_Food_Pcal_R_C_Y",
 "L101.Pop_thous_R_Yh",
 "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L134.pcFood_kcald_R_Dmnd_Y",
"L134.pcFood_kcald_R_Dmnd_Y_ssp1",
"L134.pcFood_kcald_R_Dmnd_Y_ssp2",
"L134.pcFood_kcald_R_Dmnd_Y_ssp3",
"L134.pcFood_kcald_R_Dmnd_Y_ssp4",
"L134.pcFood_kcald_R_Dmnd_Y_ssp5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      A_FoodDemand_SSPs <- get_data(all_data, "aglu/A_FoodDemand_SSPs")
  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
  FAO2050_items_cal <- get_data(all_data, "aglu/FAO2050_items_cal")
  FAO2050_Diet <- get_data(all_data, "aglu/FAO2050_Diet")
  L100.FAO_ag_Food_t <- get_data(all_data, "L100.FAO_ag_Food_t")
  L100.FAO_an_Food_t <- get_data(all_data, "L100.FAO_an_Food_t")
  L101.ag_Food_Pcal_R_C_Y <- get_data(all_data, "L101.ag_Food_Pcal_R_C_Y")
  L105.an_Food_Pcal_R_C_Y <- get_data(all_data, "L105.an_Food_Pcal_R_C_Y")
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
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y_ssp1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y_ssp2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y_ssp3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y_ssp4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L134.pcFood_kcald_R_Dmnd_Y_ssp5

    return_data(L134.pcFood_kcald_R_Dmnd_Y, L134.pcFood_kcald_R_Dmnd_Y_ssp1, L134.pcFood_kcald_R_Dmnd_Y_ssp2, L134.pcFood_kcald_R_Dmnd_Y_ssp3, L134.pcFood_kcald_R_Dmnd_Y_ssp4, L134.pcFood_kcald_R_Dmnd_Y_ssp5)
  } else {
    stop("Unknown command")
  }
}



