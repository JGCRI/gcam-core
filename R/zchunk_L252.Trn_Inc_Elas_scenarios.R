#' module_socioeconomics_L252.Trn_Inc_Elas_scenarios
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.IncomeElasticity_trn_GCAM3}, \code{object}. The corresponding file in the
#' original data system was \code{L252.Trn_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH May 2017
#' @export
module_socioeconomics_L252.Trn_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A52.demand",
             FILE = "socioeconomics/A52.inc_elas",
             "L101.Pop_thous_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L252.IncomeElasticity_trn_GCAM3",
             "object"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A52.demand <- get_data(all_data, "energy/A52.demand")
    A52.inc_elas <- get_data(all_data, "socioeconomics/A52.inc_elas")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y") %>%
      rename(pop = value)
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      # Temporary for temp-data-inject data
      gather(year, gdp, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # Temporary for temp-data-inject data
      gather(year, pcgdp, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================
    # For the GCAM 3.0 scenario, calculate the per-capita GDP
    L252.pcgdp_GCAM3_R_Y <- left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD = gdp/pop) %>%
      select(-pop, -gdp)

    # Linearly interpolate income elasticity at each level of per-capita GDP
    L252.IncomeElasticity_trn_GCAM3 <- L252.pcgdp_GCAM3_R_Y %>%
      mutate(income.elasticity = approx(x = A52.inc_elas$pcgdp_90thousUSD, y = A52.inc_elas$inc_elas,
                                        # Rule 2 info
                                        xout = pcgdp_90thousUSD, rule = 2 )$y,
             energy.final.demand = A52.demand$energy.final.demand) %>%
      # Add in region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup(GCAM_region_ID) %>%
      select(-pcgdp_90thousUSD, -GCAM_region_ID)

    # For SSP scenarios, linearly interpolate income elasticity at each level of per-capita GDP
    #L102.pcgdp_thous90USD_Scen_R_Y

    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.IncomeElasticity_trn_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand", "socioeconomics/A52.inc_elas",
                     "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.IncomeElasticity_trn_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object

    return_data(L252.IncomeElasticity_trn_GCAM3, object)
  } else {
    stop("Unknown command")
  }
}
