#' module_socioeconomics_L201.Pop_GDP_scenarios
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.Pop_GCAM3}, \code{L201.BaseGDP_GCAM3}, \code{L201.LaborForceFillout}, \code{L201.LaborProductivity_GCAM3}, \code{L201.PPPConvert}, \code{object}, \code{L201.BaseGDP_Scen}, \code{L201.LaborForceFillout}, \code{object2}, \code{L201.PPPConvert}. The corresponding file in the
#' original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author HM&RH June 2017
#' @export
module_socioeconomics_L201.Pop_GDP_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.PPP_MER_R"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate",
             "L201.Pop_GCAM3",
             "L201.BaseGDP_GCAM3",
             "L201.LaborForceFillout",
             "L201.LaborProductivity_GCAM3",
             "L201.PPPConvert",
             "object",
             "L201.BaseGDP_Scen",
             "object2"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y")
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L102.PPP_MER_R <- get_data(all_data, "L102.PPP_MER_R")

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
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L201.InterestRate") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.InterestRate
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.Pop_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.Pop_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.BaseGDP_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.BaseGDP_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.LaborForceFillout
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.LaborProductivity_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.LaborProductivity_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.PPPConvert
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.BaseGDP_Scen") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.BaseGDP_Scen
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object2") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object2


    return_data(L201.InterestRate, L201.Pop_GCAM3, L201.BaseGDP_GCAM3, L201.LaborForceFillout, L201.LaborProductivity_GCAM3, L201.PPPConvert, object, L201.BaseGDP_Scen, object2)
  } else {
    stop("Unknown command")
  }
}
