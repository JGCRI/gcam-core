#' module_socioeconomics_L102.GDP
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.gdp_mil90usd_Scen_R_Y}, \code{L102.pcgdp_thous90USD_Scen_R_Y}, \code{L102.gdp_mil90usd_GCAM3_R_Y}, \code{L102.gdp_mil90usd_GCAM3_ctry_Y}, \code{L102.pcgdp_thous90USD_GCAM3_R_Y}, \code{L102.pcgdp_thous90USD_GCAM3_ctry_Y}, \code{L102.pcgdp_thous90USD_GCAM3_ctry_Y}, \code{L102.PPP_MER_R}. The corresponding file in the
#' original data system was \code{L102.GDP.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_socioeconomics_L102.GDP_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/GCAM3_GDP",
             FILE = "socioeconomics/IMF_GDP_growth",
             "L100.gdp_mil90usd_ctry_Yh",
             # Temporary data injection from old data system
             FILE = "temp-data-inject/L101.Pop_thous_GCAM3_R_Y",
             FILE = "temp-data-inject/L101.Pop_thous_GCAM3_ctry_Y",
             FILE = "temp-data-inject/L101.Pop_thous_R_Yh",
             FILE = "temp-data-inject/L101.Pop_thous_Scen_R_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.gdp_mil90usd_Scen_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.gdp_mil90usd_GCAM3_R_Y",
             "L102.gdp_mil90usd_GCAM3_ctry_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
             "L102.PPP_MER_R"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    GCAM3_GDP <- get_data(all_data, "socioeconomics/GCAM3_GDP")
    IMF_GDP_growth <- get_data(all_data, "socioeconomics/IMF_GDP_growth")
    L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")
    # Temporary data injection from old data system
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L101.Pop_thous_GCAM3_R_Y")
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "temp-data-inject/L101.Pop_thous_GCAM3_ctry_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "temp-data-inject/L101.Pop_thous_R_Yh")
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "temp-data-inject/L101.Pop_thous_Scen_R_Yfut")

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
    # NOTE: there are `merge` and/or 'match' calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Merge-and-Match
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
      add_legacy_name("L102.gdp_mil90usd_Scen_R_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.gdp_mil90usd_Scen_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.pcgdp_thous90USD_Scen_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.gdp_mil90usd_GCAM3_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_ctry_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.gdp_mil90usd_GCAM3_ctry_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_R_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.pcgdp_thous90USD_GCAM3_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_ctry_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.pcgdp_thous90USD_GCAM3_ctry_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_ctry_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.pcgdp_thous90USD_GCAM3_ctry_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.PPP_MER_R") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.PPP_MER_R

    return_data(L102.gdp_mil90usd_Scen_R_Y, L102.pcgdp_thous90USD_Scen_R_Y, L102.gdp_mil90usd_GCAM3_R_Y, L102.gdp_mil90usd_GCAM3_ctry_Y, L102.pcgdp_thous90USD_GCAM3_R_Y, L102.pcgdp_thous90USD_GCAM3_ctry_Y, L102.pcgdp_thous90USD_GCAM3_ctry_Y, L102.PPP_MER_R)
  } else {
    stop("Unknown command")
  }
}



