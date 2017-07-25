#' module_emissions_L152.MACC
#'
#' Create Marginal Abatement Cost Curves, in percent reduction by 1990 USD abatement costs from EPA cost curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L152.MAC_pct_R_S_Proc_EPA}. The corresponding file in the
#' original data system was \code{L152.MACC.R} (emissions level1).
#' @details Create Marginal abatement cost curves, in percent reduction by 1990 USD costs from EPA cost curves.
#' Choose between 2020 or 2030 data in constants file - emissions.EPA_MACC_YEAR.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RMH May 2017

module_emissions_L152.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/EPA_MACC_baselines_MtCO2e",
             FILE = "emissions/EPA_MACC_2020_MtCO2e",
             FILE = "emissions/EPA_MACC_2030_MtCO2e"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L152.MAC_pct_R_S_Proc_EPA"))
  } else if(command == driver.MAKE) {

    Process <- EPA_region <- cost_2010USD_tCO2e <- reduction_MtCO2e <- Sector <-
        EPA_region_code <- cost_1990USD_tCe <- year <- baseline_MtCO2e <-
            reduction_pct <- NULL       # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    EPA_MACC_baselines_MtCO2e_in <- get_data(all_data, "emissions/EPA_MACC_baselines_MtCO2e")
    EPA_MACC_2020_MtCO2e <- get_data(all_data, "emissions/EPA_MACC_2020_MtCO2e")
    EPA_MACC_2030_MtCO2e <- get_data(all_data, "emissions/EPA_MACC_2030_MtCO2e")

    # Assign MACC data based on MACC curve year assumption (emissions.EPA_MACC_YEAR)
    if(emissions.EPA_MACC_YEAR %in% c(2020, 2030)) {
        if(emissions.EPA_MACC_YEAR == 2020) EPA_MACC_MtCO2e <- EPA_MACC_2020_MtCO2e
        if(emissions.EPA_MACC_YEAR == 2030) EPA_MACC_MtCO2e <- EPA_MACC_2030_MtCO2e
    } else{
      stop("MAC curve year needs to be either 2020 or 2030")
      }

    # Make processes and region names consistent
    EPA_MACC_baselines_MtCO2e <- EPA_MACC_baselines_MtCO2e_in %>%
      mutate(Process = sub("\\&", "and", Process)) %>%
      mutate(EPA_region = sub("\\&", "and", EPA_region)) %>%
      mutate(EPA_region = sub("World", "Global", EPA_region)) %>%
      mutate(EPA_region = sub("Global Total", "Global", EPA_region))

    EPA_MACC_MtCO2e <- EPA_MACC_MtCO2e %>%
      mutate(Process = sub("\\&", "and", Process))

    # Convert MAC curves to long form
    # Convert from 2010$/tCO2e to 1990$/tC
    L152.EPA_MACC_MtCO2e <- EPA_MACC_MtCO2e %>%
      gather(cost_2010USD_tCO2e, reduction_MtCO2e, -Sector, -Process, -EPA_region, -EPA_region_code) %>%
      mutate(cost_2010USD_tCO2e = as.numeric(cost_2010USD_tCO2e)) %>%
      mutate(cost_1990USD_tCe = round(cost_2010USD_tCO2e * emissions.CONV_C_CO2 * gdp_deflator(1990, base_year = 2010), 0)) %>%
      select(-cost_2010USD_tCO2e)

    # For in abatement and basebline data:
    # Combine aluminum and magnesium processes: define function, then call in both instances
    combine_Al_Mg <- function(x){
      x %>%
        mutate(Process = sub("Primary Aluminum Production", "Aluminum and Magnesium Production", Process)) %>%
        mutate(Process = sub("Magnesium Manufacturing", "Aluminum and Magnesium Production", Process))
     }

    # Abatement data
    L152.EPA_MACC_MtCO2e <- L152.EPA_MACC_MtCO2e %>%
      ungroup %>%
      combine_Al_Mg %>%
      group_by(Sector, Process, EPA_region, EPA_region_code, cost_1990USD_tCe) %>%
      summarize_at(vars(reduction_MtCO2e), sum)

    # Baseline data
    # Also filter for only EPA MACC year
    L152.EPA_MACC_baselines_MtCO2e <- EPA_MACC_baselines_MtCO2e %>%
      combine_Al_Mg %>%
      gather(year, baseline_MtCO2e, -Sector, -Process, -EPA_region) %>%
      mutate(year = as.integer(year)) %>%
      filter(year == emissions.EPA_MACC_YEAR) %>%
      group_by(Sector, Process, EPA_region) %>%
      summarize_at(vars(baseline_MtCO2e), sum)

    # Match in the baseline emissions quantities to abatement tibble then calculate abatement percentages
    # Use left_join - there should be NAs (i.e., there are sectors where the baseline is zero) - then drop those NAs
    # (ie. MAC curves in regions where the sector/process does not exist - the baseline is zero)
    L152.EPA_MACC_percent_MtCO2e <- L152.EPA_MACC_MtCO2e %>%
      left_join(L152.EPA_MACC_baselines_MtCO2e ,
                 by = c("Sector", "Process", "EPA_region")) %>%
      mutate(reduction_pct = reduction_MtCO2e / baseline_MtCO2e) %>%
      filter(!is.na(reduction_pct)) %>%
      ungroup %>%
      select(-EPA_region_code, -reduction_MtCO2e, -baseline_MtCO2e)


    # Select reduction percentage data for the given tax levels,
    # tax levels in emissions.MAC_TAXES are simply a range of costs in $1990 USD so we aren't retaining superfluous detail
    # create a new df with all rows for all costs for each unique Sector-Process-Region,
    # then add reduction percentages at those costs
    L152.MAC_pct_R_S_Proc_EPA <- L152.EPA_MACC_percent_MtCO2e %>%
      select(Sector, Process, EPA_region) %>%
      unique %>%
      repeat_add_columns(tibble(cost_1990USD_tCe = emissions.MAC_TAXES)) %>%
      left_join_error_no_match(L152.EPA_MACC_percent_MtCO2e,
                               by = c("Sector", "Process", "EPA_region", "cost_1990USD_tCe")) %>%
      spread(cost_1990USD_tCe, reduction_pct)

    # ===================================================
    # Produce outputs
    L152.MAC_pct_R_S_Proc_EPA <- L152.MAC_pct_R_S_Proc_EPA %>%
      add_title("Marginal abatement cost curves by EPA region / EPA sector / process") %>%
      add_units("%") %>%
      add_comments("Marginal abatement cost curves, in percent reduction by 1990 USD abatement costs from EPA cost curves") %>%
      add_legacy_name("L152.MAC_pct_R_S_Proc_EPA") %>%
      add_precursors("emissions/EPA_MACC_baselines_MtCO2e",
                     "emissions/EPA_MACC_2020_MtCO2e",
                     "emissions/EPA_MACC_2030_MtCO2e")

    return_data(L152.MAC_pct_R_S_Proc_EPA)
  } else {
    stop("Unknown command")
  }
}
