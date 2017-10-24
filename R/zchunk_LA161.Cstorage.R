#' module_gcam.usa_LA161.Cstorage
#'
#' Calculates onshore CO2 storage by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.Cstorage_FERC}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (gcam-usa level1).
#' @details Calculates onshore CO2 storage by grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH October 2017

module_gcam.usa_LA161.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Dooley_CCS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.Cstorage_FERC"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    state <- grid_region <- Cost_2005USDtCO2 <- CO2_Mt <- MtC <- Cumul_MtC <- quantile <-
      grade <- Cost_1990USDtC <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions") %>%
      select(state, grid_region)
    Dooley_CCS_USA <- get_data(all_data, "gcam-usa/Dooley_CCS_USA")

    # ===================================================
    L161.Csupply_state <- Dooley_CCS_USA %>%
      # Add grid_region
      left_join_error_no_match(states_subregions, by = "state") %>%
      # Convert prices to 1990 USD per ton C and amounts to C
      mutate(Cost_1990USDtC = Cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1990),
             MtC = CO2_Mt * (1 / emissions.CONV_C_CO2))

    # NOTE: there may be grid regions (e.g. AK, HI) that do not have CO2 storage points. These are not assigned onshore CO2 storage curves
    C_grid_regions <- sort(unique(L161.Csupply_state$grid_region))

    # The construction of supply curves takes place within each grid region.
    # In each region, there are four cost points assigned, for each quartile of the supply curve
    region_quartiles <- L161.Csupply_state %>%
      group_by(grid_region) %>%
      # Calculate cumulative sum, then filter to the quantiles
      mutate(Cumul_MtC = cumsum(MtC)) %>%
      # use "type=1" to indicate that we want to return only exact matches to the data in the table
      # only take the 2nd to 5th points in the quartiles (25%, 50%, 75%, 100%)
      filter(Cumul_MtC %in% quantile(Cumul_MtC, type = 1)[2:5]) %>%
      # From the cumulative totals, return to the marginal quantities associated with each grade
      mutate(MtC = Cumul_MtC - lag(Cumul_MtC),
             # this will create grade equal to grade 1 for 25%, grade 2 for 50%, etc.
             grade = paste("grade",1:n(), sep = " ")) %>%
      ungroup() %>%
      # For the first grade, set the marginal quantity with the cumulative total
      mutate(MtC = replace(MtC, is.na(MtC), Cumul_MtC[is.na(MtC)]))

    L161.Cstorage_FERC <- region_quartiles %>%
      select(grid_region, grade, Cost_1990USDtC, MtC) %>%
      # Setting a minimum cost of 0 on CO2 storage and transport projects
      mutate(Cost_1990USDtC = pmax(Cost_1990USDtC, 0)) %>%
    # ===================================================
    # Produce outputs
      add_title("CO2 storage curves by FERC regions and grades") %>%
      add_units("MtC; 1990 USD per tC") %>%
      add_comments("Cumulative MtC calculated by grid region, then filtered to quartiles") %>%
      add_comments("Final MtC value is marginal increase") %>%
      add_legacy_name("L161.Cstorage_FERC") %>%
      add_precursors("gcam-usa/states_subregions", "gcam-usa/Dooley_CCS_USA") ->
      L161.Cstorage_FERC

    return_data(L161.Cstorage_FERC)
  } else {
    stop("Unknown command")
  }
}
