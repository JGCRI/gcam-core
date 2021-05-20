# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select vars summarize_at
#' @importFrom tidyr gather spread
#' @author RMH May 2017 / YO Mar 2020

module_emissions_L152.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/EPA/EPA_2019_raw",
             FILE = "emissions/EPA/EPA_2019_MACC_Ag_updated_baseline",
             FILE = "emissions/EPA/EPA_2019_MACC_raw",
             FILE = "emissions/EPA_MACC_mapping",
             FILE = "emissions/EPA_MACC_control_mapping",
             FILE = "emissions/EPA_MAC_missing_region",
             FILE = "emissions/EPA_country_map"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L152.MAC_pct_R_S_Proc_EPA"))
  } else if(command == driver.MAKE) {

    Process <- EPA_region <- cost_2010USD_tCO2e <- reduction_MtCO2e <- Sector <-
        EPA_region_code <- cost_1990USD_tCe <- year <- baseline_MtCO2e <-
            reduction_pct <- NULL       # silence package check.

    all_data <- list(...)[[1]]

    #silence packages
    cum_reduction_MtCO2e <- p <- value <- sector <- Sector <- GCAM_region_ID <- EPA_country <- iso <- EPA_sector <- NULL

    # Load required inputs
    EPA_master <- get_data(all_data, "emissions/EPA/EPA_2019_raw")
    EPA_ag <- get_data(all_data, "emissions/EPA/EPA_2019_MACC_Ag_updated_baseline")
    EPA_MACC_master <- get_data(all_data, "emissions/EPA/EPA_2019_MACC_raw")
    EPA_MACC_mapping <- get_data(all_data, "emissions/EPA_MACC_mapping")
    EPA_MACC_control_mapping <- get_data(all_data, "emissions/EPA_MACC_control_mapping")
    EPA_MAC_missing_region <- get_data(all_data, "emissions/EPA_MAC_missing_region")
    EPA_country_map <- get_data(all_data, "emissions/EPA_country_map")

    # updated agriculture data
    # EPA provides a seperate baseline agriculture data for MAC calculation
    EPA_ag %>%
      left_join_error_no_match(EPA_MACC_control_mapping, by = "source") %>%
      left_join_error_no_match(EPA_country_map %>% select(-iso) %>% rename(country = EPA_country), by = "country") %>%
      group_by(GCAM_region_ID, Sector, Process, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year %in% emissions.EPA_MACC_YEAR) ->
      EPA_ag_update

    # baseline data
    EPA_master %>%
      left_join_error_no_match(EPA_country_map %>% select(-iso) %>% rename(country = EPA_country), by = "country") %>%
      left_join(EPA_MACC_mapping %>% select(-sector), by = c("source", "subsource")) %>%
      filter(!is.na(Process)) %>%
      rename(Sector = sector) %>%
      group_by(GCAM_region_ID, Sector, Process, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year %in% emissions.EPA_MACC_YEAR) %>%
      filter(Sector != "Agriculture") %>%
      bind_rows(EPA_ag_update) ->
      EPA_MACC_baselines_MtCO2e

    # mac data
    # Convert from 2010$/tCO2e to 1990$/tC
    EPA_MACC_master %>%
      left_join(EPA_country_map %>% select(-iso) %>% rename(country = EPA_country), by = "country") %>%
      left_join_error_no_match(EPA_MACC_control_mapping, by = c("sector", "source")) %>%
      rename(cost_2010USD_tCO2e = p, reduction_MtCO2e = q) %>%
      select(GCAM_region_ID, Sector, Process, year, cost_2010USD_tCO2e, reduction_MtCO2e) %>%
      mutate(cost_2010USD_tCO2e = as.numeric(cost_2010USD_tCO2e),
             cost_1990USD_tCe = round(cost_2010USD_tCO2e * emissions.CONV_C_CO2 * gdp_deflator(1990, base_year = 2010), 0)) %>%
      select(-cost_2010USD_tCO2e) ->
      L152.EPA_MACC_MtCO2e_ungrouped

    # For in abatement and basebline data:
    # Combine aluminum and magnesium processes: define function, then call in both instances
    combine_Al_Mg <- function(x) {
      x %>%
        mutate(Process = sub("Primary Aluminum Production", "Aluminum and Magnesium Production", Process),
               Process = sub("Magnesium Manufacturing", "Aluminum and Magnesium Production", Process))
    }

    # Abatement data
    L152.EPA_MACC_MtCO2e_ungrouped %>%
      ungroup %>%
      combine_Al_Mg %>%
      group_by(Sector, Process, GCAM_region_ID, year, cost_1990USD_tCe) %>%
      summarize_at(vars(reduction_MtCO2e), sum) %>%
      ungroup() %>%
      group_by(Sector, Process, GCAM_region_ID, year) %>%
      mutate(cum_reduction_MtCO2e = cumsum(reduction_MtCO2e)) %>%
      ungroup() %>%
      replace_na(list(cum_reduction_MtCO2e = 0)) ->
      L152.EPA_MACC_MtCO2e

    # Baseline data
    # Also filter for only EPA MACC year
    EPA_MACC_baselines_MtCO2e %>%
      combine_Al_Mg %>%
      group_by(GCAM_region_ID, Sector, Process, year) %>%
      summarise(baseline_MtCO2e = sum(value)) %>%
      replace_na(list(baseline_MtCO2e = 0)) %>%
      ungroup() ->
      L152.EPA_MACC_baselines_MtCO2e

    # Match in the baseline emissions quantities to abatement tibble then calculate abatement percentages
    # Use left_join - there should be NAs (i.e., there are sectors where the baseline is zero) - then drop those NAs
    # (ie. MAC curves in regions where the sector/process does not exist - the baseline is zero)
    # emissions.MAC_highestReduction is 0.95, defined in constant.R

    L152.EPA_MACC_MtCO2e %>%
      left_join(L152.EPA_MACC_baselines_MtCO2e ,
                by = c("Sector", "Process", "GCAM_region_ID", "year")) %>%
      mutate(reduction_pct = cum_reduction_MtCO2e / baseline_MtCO2e) %>%
      mutate(reduction_pct = ifelse(is.na(reduction_pct) | is.infinite(reduction_pct), 0, reduction_pct)) %>%
      mutate(reduction_pct = ifelse(reduction_pct >=1, emissions.MAC_highestReduction, reduction_pct)) %>%
      ungroup() %>%
      select(Sector, Process, GCAM_region_ID, year, cost_1990USD_tCe, reduction_pct) ->
      L152.EPA_MACC_percent_MtCO2e

    price_cut <- round(emissions.MAC_TAXES * emissions.CONV_C_CO2 * gdp_deflator(1990, base_year = 2010), 0)

    # create a template based on standarized price-cuts
    L152.EPA_MACC_percent_MtCO2e %>%
      select(Sector, Process, GCAM_region_ID, year) %>%
      distinct() %>%
      repeat_add_columns(tibble::tibble(cost_1990USD_tCe = price_cut)) ->
      L152.EPA_MACC_percent_MtCO2e_standardized

    # insert "standard MAC taxes" into the MAC table and complete the table
    L152.EPA_MACC_percent_MtCO2e %>%
      full_join(L152.EPA_MACC_percent_MtCO2e_standardized,
                by = c("Sector", "Process", "GCAM_region_ID", "year", "cost_1990USD_tCe")) %>%
      group_by(Sector, Process, GCAM_region_ID, year) %>%
      mutate(reduction_pct = approx_fun(cost_1990USD_tCe, reduction_pct)) %>%
      ungroup() %>%
      filter(cost_1990USD_tCe %in% price_cut) %>%
      arrange(Sector, Process, GCAM_region_ID, year) ->
      L152.EPA_MACC_percent_MtCO2e_complete

    # Select reduction percentage data for the given tax levels,
    # tax levels in emissions.MAC_TAXES are simply a range of costs in $1990 USD so we aren't retaining superfluous detail
    # create a new df with all rows for all costs for each unique Sector-Process-Region,
    # then add reduction percentages at those costs
    # keep MAC data for MODEL_FUTURE_YEARS

    L152.EPA_MACC_percent_MtCO2e_complete %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(tax = cost_1990USD_tCe) %>%
      rename(mac.reduction = reduction_pct) %>%
      rename(mac.control = Process) ->
      L152.MAC_pct_R_S_Proc_EPA_missing

    # fill in missing MAC regions based on a mapping file - EPA_MAC_missing_region
    # currently just add Taiwan becuase EPA 2019 does not have it
    # assign all measures same as China (region 11) for data completeness

    if(!is.na(EPA_MAC_missing_region$GCAM_region_ID_missing)){
      L152.MAC_pct_R_S_Proc_EPA_missing %>%
        filter(GCAM_region_ID %in% EPA_MAC_missing_region$GCAM_region_ID_alternative) %>%
        left_join_error_no_match(EPA_MAC_missing_region, by = c("GCAM_region_ID" = "GCAM_region_ID_alternative")) %>%
        mutate(GCAM_region_ID = GCAM_region_ID_missing) %>%
        select(-GCAM_region_ID_missing) %>%
        bind_rows(L152.MAC_pct_R_S_Proc_EPA_missing) ->
        L152.MAC_pct_R_S_Proc_EPA
    } else {
      L152.MAC_pct_R_S_Proc_EPA_missing ->
        L152.MAC_pct_R_S_Proc_EPA
    }

    # ===================================================
    # Produce outputs
    L152.MAC_pct_R_S_Proc_EPA %>%
      add_title("Marginal abatement cost curves by GCAM region / EPA sector / process /year") %>%
      add_units("%") %>%
      add_comments("Marginal abatement cost curves, in percent reduction by 1990 USD abatement costs from EPA cost curves") %>%
      add_legacy_name("L152.MAC_pct_R_S_Proc_EPA") %>%
      add_precursors("emissions/EPA/EPA_2019_raw",
                     "emissions/EPA/EPA_2019_MACC_Ag_updated_baseline",
                     "emissions/EPA/EPA_2019_MACC_raw",
                     "emissions/EPA_MACC_mapping",
                     "emissions/EPA_MACC_control_mapping",
                     "emissions/EPA_MAC_missing_region",
                     "emissions/EPA_country_map") ->
      L152.MAC_pct_R_S_Proc_EPA

    return_data(L152.MAC_pct_R_S_Proc_EPA)
  } else {
    stop("Unknown command")
  }
}
