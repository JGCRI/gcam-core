
#' module_modeltime_L200.modeltime
#'
#' Construct the \code{modeltime} data structures
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}
#' @author BBL
#' @export
module_modeltime_L200.modeltime <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("L200.ModelTime",
      "L200.ModelTimeInterYears",
      "L200.MAGICC",
      "L200.hector"))
    } else if(command == driver.DECLARE_INPUTS) {
    return(NULL)   # modeltime doesn't depend on anything
  } else if(command == driver.MAKE) {
    modeltime_makedata(...)
  } else {
    stop("Unknown command")
  }
}

#' modeltime_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all available modeltime data
#' @importFrom tibble tibble
modeltime_makedata <- function(all_data) {

  # Calculate the read-in timesteps in the model
  GCAM_timesteps <- diff(modeltime.YEARS)

  L200.ModelTime <- tibble(start.year.timestep    = GCAM_timesteps[1],
                           start.year             = min(modeltime.BASE_YEARS),
                           final.calibration.year = max(modeltime.BASE_YEARS),
                           end.year               = max(modeltime.FUTURE_YEARS))

  changeyears <- c(FALSE, GCAM_timesteps[-1] != GCAM_timesteps[-length(GCAM_timesteps)])
  GCAM_interyears <- modeltime.YEARS[changeyears]
  GCAM_interyear.timesteps <- GCAM_timesteps[changeyears]

  L200.ModelTimeInterYears <- tibble(inter.year.timestep = GCAM_interyear.timesteps,
                                     inter.year          = GCAM_interyears)

  L200.MAGICC <- tibble(last.historical.year    = modeltime.MAGICC_LAST_HISTORICAL_YEAR,
                        bc.unit.forcing         = modeltime.MAGICC_BC_UNIT_FORCING,
                        carbon.model.start.year = modeltime.MAGICC_C_START_YEAR)

  L200.hector <- tibble(hector.end.year         = modeltime.HECTOR_END_YEAR,
                        emissions.switch.year   = modeltime.HECTOR_EMISSIONS_YEAR,
                        hector.ini.file         = modeltime.HECTOR_INI_FILE,
                        carbon.model.start.year = modeltime.MAGICC_C_START_YEAR)

  list("L200.ModelTime"           = L200.ModelTime,
       "L200.ModelTimeInterYears" = L200.ModelTimeInterYears,
       "L200.MAGICC"              = L200.MAGICC,
       "L200.hector"              = L200.hector)
}
