# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_modeltime_L200.modeltime
#'
#' Generate the model time information needed to interact with Hector or MAGICC.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L200.ModelTime}, \code{L200.ModelTimeInterYears}, \code{L200.MAGICC}, \code{L200.hector}. The corresponding file in the
#' original data system was \code{L200.modeltime.R} (modeltime level2).
#' @details Generate the model time information needed to interact with Hector or MAGICC.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @author BBL
module_modeltime_L200.modeltime <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(NULL)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L200.ModelTime",
             "L200.ModelTimeInterYears",
             "L200.MAGICC",
             "L200.hector"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # No required inputs

    # Calculate the read-in timesteps in the model
    GCAM_timesteps <- diff(MODEL_YEARS)

    tibble(start.year.timestep     = GCAM_timesteps[1],
           start.year              = min(MODEL_BASE_YEARS),
           final.calibration.year  = max(MODEL_BASE_YEARS),
           end.year                = max(MODEL_FUTURE_YEARS),
           carbon.model.start.year = modeltime.MAGICC_C_START_YEAR) %>%
      add_title("GCAM time information") %>%
      add_units("years") %>%
      add_legacy_name("L200.ModelTime") %>%
      add_comments("GCAM time information generated from constants") ->
      L200.ModelTime

    changeyears <- c(FALSE, GCAM_timesteps[-1] != GCAM_timesteps[-length(GCAM_timesteps)])
    GCAM_interyears <- MODEL_YEARS[changeyears]
    GCAM_interyear.timesteps <- GCAM_timesteps[changeyears]

    tibble(inter.year.timestep = GCAM_interyear.timesteps,
           inter.year          = GCAM_interyears,
           dummy.tag           = seq_along(GCAM_interyears)) %>% # dummy tag avoids later years over-writing prior years that have the same timestep length
      add_title("GCAM timestep change information") %>%
      add_units("years") %>%
      add_legacy_name("L200.ModelTimeInterYears") %>%
      add_comments("GCAM timestep change points, generated from constants") ->
      L200.ModelTimeInterYears

    tibble(last.historical.year    = modeltime.MAGICC_LAST_HISTORICAL_YEAR,
           bc.unit.forcing         = modeltime.MAGICC_BC_UNIT_FORCING,
           default.emiss.file      = modeltime.MAGICC_DEFAULT_EMISS_FILE) %>%
      add_title("MAGICC time information") %>%
      add_units("years") %>%
      add_legacy_name("L200.MAGICC") %>%
      add_comments("MAGICC time information generated from constants") ->
      L200.MAGICC

    tibble(hector.end.year         = modeltime.HECTOR_END_YEAR,
           emissions.switch.year   = modeltime.HECTOR_EMISSIONS_YEAR,
           hector.ini.file         = modeltime.HECTOR_INI_FILE) %>%
      add_title("Hector time and INI file information") %>%
      add_units("various") %>%
      add_legacy_name("L200.hector") %>%
      add_comments("Hector time and INI file information generated from constants") ->
      L200.hector

    return_data(L200.ModelTime, L200.ModelTimeInterYears, L200.MAGICC, L200.hector)
  } else {
    stop("Unknown command")
  }
}
