#' module_gcam.usa_LA119.Solar
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L119.CapFacScaler_PV_state}, \code{L119.CapFacScaler_CSP_state}. The corresponding file in the
#' original data system was \code{LA119.Solar.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA119.Solar <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_capacity_factors",
             FILE = "gcam-usa/NREL_us_re_technical_potential"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L119.CapFacScaler_PV_state",
             "L119.CapFacScaler_CSP_state"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_us_re_capacity_factors <- get_data(all_data, "gcam-usa/NREL_us_re_capacity_factors")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")   #This data is not being used in this chunk

    # Perform computations: Create scalars to scale capacity factors read in in the assumptions file in the energy folder to adjust for varying irradiance by state.

    NREL_us_re_capacity_factors %>% gather(fuel,average,-State) %>%
      filter (State=="Average") %>%
      select (-State) -> Average_fuel

    NREL_us_re_capacity_factors %>%
      gather(fuel,value,-State) %>%
      filter (State != "Average") %>%
      full_join(Average_fuel, by = "fuel") %>%
      mutate(scaler = value/average, sector = "electricity generation") %>%
      select(State,sector,fuel, scaler) -> NREL_us_re_capacity_factors

    states_subregions %>%
      mutate(State = state_name) %>%
      select(state,State) %>%
      full_join(NREL_us_re_capacity_factors, by = "State") %>%
      select (-State) -> NREL_us_re_capacity_factors

    NREL_us_re_capacity_factors %>%

      filter(fuel == "Urban_Utility_scale_PV") %>%
      mutate(fuel = "solar PV")      -> L119.CapFacScaler_PV_state

    NREL_us_re_capacity_factors %>%
      filter(fuel == "CSP") %>%
      mutate(fuel = "solar CSP")   -> L119.CapFacScaler_CSP_state


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
    L119.CapFacScaler_PV_state %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L119.CapFacScaler_PV_state") %>%
      add_precursors("gcam-usa/states_subregions","gcam-usa/NREL_us_re_capacity_factors", "gcam-usa/NREL_us_re_technical_potential") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L119.CapFacScaler_PV_state
    L119.CapFacScaler_CSP_state %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L119.CapFacScaler_CSP_state") %>%
      add_precursors("gcam-usa/states_subregions","gcam-usa/NREL_us_re_capacity_factors", "gcam-usa/NREL_us_re_technical_potential") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L119.CapFacScaler_CSP_state

    return_data(L119.CapFacScaler_PV_state, L119.CapFacScaler_CSP_state)
  } else {
    stop("Unknown command")
  }
}
