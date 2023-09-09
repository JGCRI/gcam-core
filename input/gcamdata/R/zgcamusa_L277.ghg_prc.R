# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L277.ghg_prc
#'
#' Generates input ghg emissions and marginal abatement curves for industrial processes and urban processes by
#' energy technology for U.S. states
#' Writes out input emissions and MAC components to all process-related energy technologies and states
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L277.ghg_prc_USA}, \code{L277.MAC_prc_USA}, \code{L277.MAC_prc_tc_average_USA}, \code{L277.MAC_prc_phaseInTime_USA}.
#' @details Generates input ghg emissions and marginal abatement curves for industrial processes and urban processes by
#' energy technology for U.S. states and writes out input emissions and MAC components to all process-related energy technologies and states
#' @importFrom dplyr filter mutate select
#' @author Yang Ou Feb 2023
#'
module_gcamusa_L277.ghg_prc <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-usa/emissions/EPA_state_ghg_emission_2022",
             FILE="gcam-usa/emissions/EPA_state_ghg_emission_2022_mapping",
             "L131.nonco2_tg_R_prc_S_S_Yh",
             "L252.MAC_prc",
             "L252.MAC_prc_tc_average",
             "L252.MAC_prc_phaseInTime"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L277.ghg_prc_USA",
             "L277.MAC_prc_USA",
             "L277.MAC_prc_tc_average_USA",
             "L277.MAC_prc_phaseInTime_USA"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    category <- GCAM_sector <- region <- Non.CO2 <- year <- value <- share <- national.total <- supplysector <-
      subsector <- stub.technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    EPA_GHG_states <- get_data(all_data, "gcam-usa/emissions/EPA_state_ghg_emission_2022", strip_attributes = TRUE)
    EPA_GHG_states_mapping <- get_data(all_data, "gcam-usa/emissions/EPA_state_ghg_emission_2022_mapping", strip_attributes = TRUE)
    L131.nonco2_tg_R_prc_S_S_Yh <- get_data(all_data, "L131.nonco2_tg_R_prc_S_S_Yh", strip_attributes = TRUE)
    L252.MAC_prc <- get_data(all_data, "L252.MAC_prc", strip_attributes = TRUE)
    L252.MAC_prc_tc_average <- get_data(all_data, "L252.MAC_prc_tc_average", strip_attributes = TRUE)
    L252.MAC_prc_phaseInTime <- get_data(all_data, "L252.MAC_prc_phaseInTime", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # isolate industrial process sectors from US state-level GHG inventory
    # EPA data contains data from 1990 to 2020
    EPA_GHG_states_ind_proc_EPA_years <- EPA_GHG_states %>%
      gather_years() %>%
      # keep CH4 and N2O
      filter(grepl("CH4|N2O", category)) %>%
      # remove sum rows or LULUCF rows
      filter(!grepl("Sum|LULUCF", category)) %>%
      separate(category, into = c("Non.CO2", "source"), sep = "/") %>%
      left_join_error_no_match(EPA_GHG_states_mapping, by = "source") %>%
      filter(GCAM_sector %in% unique(L131.nonco2_tg_R_prc_S_S_Yh$stub.technology)) %>%
      group_by(region, Non.CO2, GCAM_sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Temporarily fill in historical years before 1990 using the earliest available year, i.e. 1990
    # then scale to CEDS emissions starting from 1975

    EPA_GHG_states_ind_proc_previous_years <- EPA_GHG_states_ind_proc_EPA_years %>%
      complete(nesting(region, Non.CO2, GCAM_sector), year = HISTORICAL_YEARS) %>%
      group_by(region, Non.CO2, GCAM_sector) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(!year %in% EPA_GHG_states_ind_proc_EPA_years$year)

    # combine EPA data to have all historical years
    EPA_GHG_states_ind_proc_Yh <- bind_rows(EPA_GHG_states_ind_proc_EPA_years,
                                            EPA_GHG_states_ind_proc_previous_years) %>%
      filter(year %in% MODEL_BASE_YEARS)

    # develop state_shares
    EPA_GHG_states_ind_proc_Yh_shares <- EPA_GHG_states_ind_proc_Yh %>%
      group_by(Non.CO2, GCAM_sector, year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      select(region, Non.CO2, GCAM_sector, year, share)

    # apply state shares by Non.CO2, GCAM_sector, year to the national total emissions from CEDS
    L131.nonco2_tg_R_prc_S_S_Yh %>%
      filter(GCAM_region_ID == gcam.USA_CODE & Non.CO2 %in% c("CH4", "N2O")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(national.total = value) %>%
      # use left_join because here we copy national total to all states so the number of rows are changed
      left_join(EPA_GHG_states_ind_proc_Yh_shares, by = c("subsector" = "GCAM_sector", "Non.CO2", "year")) %>%
      # drop CH4 of other industrial process and solvent, these sectors have nonGHG emissions but no CH4 emissions
      na.omit() %>%
      mutate(value = national.total * share) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions = value) -> L277.ghg_prc_USA

    # copy marginal abatement curves to states that with the corresponding sector/emissions
    L277.ghg_prc_USA %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      distinct() %>%
      inner_join(L252.MAC_prc %>%
                   filter(region == gcam.USA_REGION) %>%
                   select(-region),
                 by = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MAC"]])->
      L277.MAC_prc_USA

    L277.ghg_prc_USA %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      distinct() %>%
      inner_join(L252.MAC_prc_tc_average %>%
                   filter(region == gcam.USA_REGION) %>%
                   select(-region),
                 by = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MACTC"]]) ->
      L277.MAC_prc_tc_average_USA

    L277.ghg_prc_USA %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      distinct() %>%
      inner_join(L252.MAC_prc_phaseInTime %>%
                   filter(region == gcam.USA_REGION) %>%
                   select(-region),
                 by = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      select(LEVEL2_DATA_NAMES[["MACPhaseIn"]]) ->
      L277.MAC_prc_phaseInTime_USA

    # ===================================================
    # Produce outputs

    L277.ghg_prc_USA %>%
      add_title("Processes GHG emissions by technology for all U.S. states") %>%
      add_units("Tg") %>%
      add_comments("Apply state shares by Non.CO2 and GCAM_sector and year to the national total emissions from CEDS") %>%
      add_precursors("gcam-usa/emissions/EPA_state_ghg_emission_2022",
                     "gcam-usa/emissions/EPA_state_ghg_emission_2022_mapping",
                     "L131.nonco2_tg_R_prc_S_S_Yh") ->
      L277.ghg_prc_USA

    L277.MAC_prc_USA %>%
      add_title("copy USA proc MAC curves to states") %>%
      add_units("%") %>%
      add_comments("copy USA proc MAC curves to states") %>%
      same_precursors_as("L277.ghg_prc_USA") %>%
      add_precursors("L252.MAC_prc") ->
      L277.MAC_prc_USA

    L277.MAC_prc_tc_average_USA %>%
      add_title("copy USA proc MAC curves tech change to states") %>%
      add_units("% increase per year") %>%
      add_comments("copy USA proc MAC curves tech change to states") %>%
      same_precursors_as("L277.ghg_prc_USA") %>%
      add_precursors("L252.MAC_prc_tc_average") ->
      L277.MAC_prc_tc_average_USA

    L277.MAC_prc_phaseInTime_USA %>%
      add_title("copy USA proc MAC curves phase in to states") %>%
      add_units("years") %>%
      add_comments("copy USA proc MAC curves phase in to states") %>%
      same_precursors_as("L277.ghg_prc_USA") %>%
      add_precursors("L252.MAC_prc_phaseInTime") ->
      L277.MAC_prc_phaseInTime_USA

    return_data(L277.ghg_prc_USA,
                L277.MAC_prc_USA,
                L277.MAC_prc_tc_average_USA,
                L277.MAC_prc_phaseInTime_USA)
  } else {
    stop("Unknown command")
  }
}
