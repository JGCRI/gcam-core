# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA143.HDDCDD
#'
#' Estimate heating and cooling degree days for gcam-usa.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L143.share_state_Pop_CDD_sR9}, \code{L143.share_state_Pop_CDD_sR13}, \code{L143.share_state_Pop_HDD_sR9}, \code{L143.share_state_Pop_HDD_sR13}, \code{L143.HDDCDD_scen_state}. The corresponding file in the
#' original data system was \code{LA143.HDDCDD.R} (gcam-usa level1).
#' @details Estimate heating and cooling degree days for gcam-usa.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr complete nesting separate
#' @importFrom tibble tibble
#' @author KRD Nov 2017
module_gcamusa_LA143.HDDCDD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop",
             FILE = "gcam-usa/GIS/HDD_His",
             FILE = "gcam-usa/GIS/HDD_hist_constdds",
             FILE = "gcam-usa/GIS/HDD_GFDL_A2",
             FILE = "gcam-usa/GIS/CDD_His",
             FILE = "gcam-usa/GIS/CDD_hist_constdds",
             FILE = "gcam-usa/GIS/CDD_GFDL_A2",
             FILE = "gcam-usa/AEO_2015_HDDCDD"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L143.share_state_Pop_CDD_sR9",
             "L143.share_state_Pop_CDD_sR13",
             "L143.share_state_Pop_HDD_sR9",
             "L143.share_state_Pop_HDD_sR13",
             "L143.HDDCDD_scen_state"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Census_pop <- get_data(all_data, "gcam-usa/Census_pop")
    CDD_His <- get_data(all_data, "gcam-usa/GIS/CDD_His")
    CDD_hist_constdds <- get_data(all_data, "gcam-usa/GIS/CDD_hist_constdds")
    CDD_GFDL_A2 <- get_data(all_data, "gcam-usa/GIS/CDD_GFDL_A2")
    HDD_His <- get_data(all_data, "gcam-usa/GIS/HDD_His")
    HDD_hist_constdds <- get_data(all_data, "gcam-usa/GIS/HDD_hist_constdds")
    HDD_GFDL_A2 <- get_data(all_data, "gcam-usa/GIS/HDD_GFDL_A2")
    AEO_2015_HDDCDD <- get_data(all_data, "gcam-usa/AEO_2015_HDDCDD")

    # Silence package
    state <- subregion9 <- subregion13 <- year <- value <- degree_day <- population <-
      value_sR9 <- variable <- value_sR13 <- GCM <- Scen <- historical_value <-
      hist_value <- base_year <- scn_final_historical_year <- pop <- pop_SR9 <- DD <-
      value_AEO <- NULL

    # ===================================================

    # Part 1: Create tibbles of state share of subregion population's degree days.

    # Add subregion13 (for RECS) and subregion9 (for CBECS) columns to the population by state tibble
    # and transform to long format. This tibble will be used to calculate the share of person heating
    # degree days within census division (subregion) and state.
    Census_pop %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, subregion9, subregion13),
                               by = "state") %>%
      gather_years ->
      Census_pop_hist_subregion

    # Add subregion13 (for RECS) and subregion9 (for CBECS) columns to the historic cooling degree days
    # latter and heating degree days. Latter on this will be used to  to calculate the share of person heating
    # degree days within census division (subregion).
    #
    # Historic cooling degree days.
    CDD_His %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, subregion9, subregion13),
                               by = "state") %>%
      gather_years ->
      CDD_His_subregion

    # Historic heating degree days.
    HDD_His %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, subregion9, subregion13),
                               by = "state") %>%
      gather_years ->
      HDD_His_subregion


    # Interpolate to fill out any missing historical years cooling degree days.
    CDD_His_subregion %>%
      complete(nesting(state, subregion9, subregion13), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(state, subregion9, subregion13, year) %>%
      group_by(state, subregion9, subregion13) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L143.CDD_state

    # Interpolate to fill out any missing historical years heating degree days.
    HDD_His_subregion %>%
      complete(nesting(state, subregion9, subregion13), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(state, subregion9, subregion13, year) %>%
      group_by(state, subregion9, subregion13) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L143.HDD_state


    # Multiply the historic cooling degree days tibble by the census population to calculate
    # the population cooling degree days.
    L143.CDD_state %>%
      rename(degree_day = value) %>%
      left_join_error_no_match(Census_pop_hist_subregion %>% rename(population = value),
                               by = c("year", "state", "subregion9", "subregion13")) %>%
      mutate(value = degree_day * population) %>%
      select(state, subregion9, subregion13, year, value) ->
      L143.Pop_CDD_state

    # Multiply the historic heating degree days tibble by the census population to calculate
    # the population heating degree days.
    L143.HDD_state %>%
      rename(degree_day = value) %>%
      left_join_error_no_match(Census_pop_hist_subregion %>% rename(population = value),
                               by = c("year", "state", "subregion9", "subregion13")) %>%
      mutate(value = degree_day * population) %>%
      select(state, subregion9, subregion13, year, value) ->
      L143.Pop_HDD_state

    # Aggregate the total population for each census subregion. The total populatin will be used
    # to calcualte the state's share of the subregion's population heating and cooling degree days.
    #
    # Population cooling degree days in subregion 9.
    L143.Pop_CDD_state %>%
      group_by(year, subregion9) %>%
      summarise(value_sR9 = sum(value)) %>%
      ungroup() ->
      L143.Pop_CDD_sR9

    # Population cooling degree days in subregion 13.
    L143.Pop_CDD_state %>%
      group_by(year, subregion13) %>%
      summarise(value_sR13 = sum(value)) %>%
      ungroup() ->
      L143.Pop_CDD_sR13

    # Population heating degree days in subregion 9.
    L143.Pop_HDD_state %>%
      group_by(year, subregion9) %>%
      summarise(value_sR9 = sum(value)) %>%
      ungroup() ->
      L143.Pop_HDD_sR9

    # Population heating degree days in subregion 13.
    L143.Pop_HDD_state %>%
      group_by(year, subregion13) %>%
      summarise(value_sR13 = sum(value)) %>%
      ungroup() ->
      L143.Pop_HDD_sR13


    # Divide state degree days by the subregions' population degree days to calculate the
    # state's share of its subregions' population degree days.
    #
    # State share of subregion 9 population cooling degree days.
    L143.Pop_CDD_state %>%
      left_join_error_no_match(L143.Pop_CDD_sR9, by = c("year", "subregion9")) %>%
      mutate(value = value / value_sR9, variable = "CDD") %>%
      select(state, subregion9, variable, year, value) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L143.share_state_Pop_CDD_sR9

    # State share of subregion 13 population cooling degree days.
    L143.Pop_CDD_state %>%
      left_join_error_no_match(L143.Pop_CDD_sR13, by = c("year", "subregion13")) %>%
      mutate(value = value / value_sR13, variable = "CDD") %>%
      select(state, subregion13, variable, year, value) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L143.share_state_Pop_CDD_sR13

    # State share of subregion 9 population heating degree days.
    L143.Pop_HDD_state %>%
      left_join_error_no_match(L143.Pop_HDD_sR9, by = c("year", "subregion9")) %>%
      mutate(value = value / value_sR9, variable = "HDD") %>%
      select(state, subregion9, variable, year, value) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L143.share_state_Pop_HDD_sR9

    # State share of subregion 13 population heating degree days.
    L143.Pop_HDD_state %>%
      left_join_error_no_match(L143.Pop_HDD_sR13, by = c("year", "subregion13")) %>%
      mutate(value = value / value_sR13, variable = "HDD") %>%
      select(state, subregion13, variable, year, value)  %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L143.share_state_Pop_HDD_sR13



    # Part 2: Process degree days in future scenarios

    # Create a list of the future scenarios data frames to process. This method preserves the
    # file name as name of the data frames in the list.
    HDDCDD_list <- list(CDD_GFDL_A2 = CDD_GFDL_A2, HDD_GFDL_A2 = HDD_GFDL_A2,
                        CDD_hist_constdds = CDD_hist_constdds, HDD_hist_constdds = HDD_hist_constdds)

    # Add the file name to each data frame and concatenate future scenario data frames together
    # to form a single tibble.
    HDDCDD_data <- bind_rows(HDDCDD_list, .id = 'file')

    # Parse out variable, GCM, and scenario information from the the file name in to individual columns.
    HDDCDD_data %>%
      separate(col = file, into = c("variable", "GCM", "Scen"), sep = "_") ->
      HDDCDD_data

    # Interpolate heating and cooling degree days for all historical and future years
    HDDCDD_data %>%
      gather_years %>%
      complete(nesting(variable, GCM, Scen, state), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(variable, GCM, Scen, state, year) %>%
      group_by(variable, GCM, Scen, state) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L143.HDDCDD_scen_state

    # Set aside the scenario's final historical year because this will be used to normalize the degree days
    # latter on.
    Scen_final_historical_year <- filter(L143.HDDCDD_scen_state, year == max(HISTORICAL_YEARS))



    # Part 3: Combine historic and future scenario degree days

    # Fill in any missing historical cooling degree days through interpolation.
    CDD_His_subregion %>%
      complete(nesting(state, subregion9, subregion13), year = c(HISTORICAL_YEARS)) %>%
      arrange(state, subregion9, subregion13, year) %>%
      group_by(state, subregion9, subregion13) %>%
      mutate(value = approx_fun(year, value, rule = 2), variable = "CDD") %>%
      ungroup() ->
      CDD_His_complete

    # Fill in any missing historical heating degree days through interpolation.
    HDD_His_subregion %>%
      complete(nesting(state, subregion9, subregion13), year = c(HISTORICAL_YEARS)) %>%
      arrange(state, subregion9, subregion13, year) %>%
      group_by(state, subregion9, subregion13) %>%
      mutate(value = approx_fun(year, value, rule = 2), variable = "HDD") %>%
      ungroup() ->
      HDD_His_complete

    # Combine the interpolated cooling and heating historic degree days into a single tibble and
    # add a column called historical value. The historical_value column will be using in the next
    # step to replace degree days for model historical years in the future scenario with historical
    # observations.
    CDD_His_complete %>%
      bind_rows(HDD_His_complete) %>%
      mutate(historical_value = TRUE)->
      DD_His


    # Replace the heating and cooling degree days values in model historic years in the future scenario
    # tibble with historic data.
    L143.HDDCDD_scen_state %>%
      # Use left_join because we expect NAs to occur in the hist_value column when in future years.
      left_join(DD_His %>%
                  select(hist_value = value, year, variable, state, historical_value),
                by = c("state", "variable", "year")) %>%
      # When applicatble replace the GCM degree days with the histroic observations.
      mutate(value = if_else(!is.na(hist_value), hist_value, value)) %>%
      select(variable, GCM, Scen, state, year, value, historical_value) ->
      L143.HDDCDD_scen_state_w_historical_observations


    # Normalize the future degree days by the base year value. This is necessary because sometimes there are large
    # large discrepancies between the GCM model output and the observed historical degree.

    # Start by combining the the GCM, scenario, and state degree days from the  scenario final model historical
    # year, with the scenario tibble that has the historical observation replaced values.
    L143.HDDCDD_scen_state_w_historical_observations %>%
      # Use left_join here because we do not expect a 1:1, the GCM scenario final historical year degree day
      # will be used to normalize the future degree days in the following step.
      left_join(Scen_final_historical_year %>%
                                 select(scn_final_historical_year = value, variable, GCM, Scen, state),
                by = c("variable", "GCM", "Scen", "state")) %>%
      # Use inner join (as we do not expect a 1:1 match) to add a column of the GCM degree days in the final historical
      # year of the historical observations to the tibble.
      inner_join(L143.HDDCDD_scen_state_w_historical_observations %>%
                  filter(year == max(HISTORICAL_YEARS)) %>%
                  select(variable, GCM, Scen, state, base_year = value),
                by = c("variable", "GCM", "Scen", "state")) %>%
      # If the historical_value is NA (aka it is a future year) then multiply by the fraction of the observation
      # degree day base year over the scenario degree day base year.
      mutate(value = if_else(is.na(historical_value), value * base_year / scn_final_historical_year, value)) ->
      L143.HDDCDD_scen_state

    # Format for output.
    L143.HDDCDD_scen_state %>%
      select(state, Scen, GCM, variable, year, value) ->
      L143.HDDCDD_scen_state

    # AEO harmonization by census division and year, to 2040
    Census_pop %>%
      gather_years("pop") %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, subregion9),
                               by = c("state")) %>%
      group_by(subregion9, year) %>%
      summarise(pop_SR9 = sum(pop)) %>%
      ungroup() -> L143.Pop_SR9_USA

    L143.Pop_CDD_sR9 %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      mutate(variable = "CDD") %>%
      bind_rows(L143.Pop_HDD_sR9%>%
                  filter(year == max(HISTORICAL_YEARS)) %>%
                  mutate(variable = "HDD")) %>%
      left_join_error_no_match(L143.Pop_SR9_USA, by = c("subregion9", "year")) %>%
      mutate(DD = value_sR9 / pop_SR9) %>%
      select(subregion9, variable, DD) %>%
      repeat_add_columns(tibble(year = gcamusa.AEO_DD_YEARS)) %>%
      left_join_error_no_match(AEO_2015_HDDCDD %>%
                  gather_years("value_AEO") %>%
                  filter(year %in% gcamusa.AEO_DD_YEARS,
                         subregion9 != "USA"),
                by = c("subregion9", "variable", "year")) %>%
      mutate(value_AEO = value_AEO / DD) -> L143.DDmult_sR9_Y_AEO

    # Apply these multipliers by subregion9 to the state-level historical HDDCDD data,
    # to create a scenario to 2050
    # historical years first
    DD_His %>%
      select(-subregion13, -historical_value) %>%
      left_join_error_no_match(L143.DDmult_sR9_Y_AEO %>%
                                 filter(year == min(gcamusa.AEO_DD_YEARS)) %>%
                                 distinct(subregion9, variable, value_AEO),
                               by = c("subregion9", "variable")) %>%
      mutate(value = value * value_AEO) -> L143.HDDCDD_AEO_hist_years

    # AEO years second
    DD_His %>%
      select(-subregion13, -historical_value) %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      # join is intended to duplicate rows
      # left_join_error_no_match throws error, so left_join is used
      left_join(L143.DDmult_sR9_Y_AEO %>%
                  select(-DD),
                by = c("subregion9", "variable")) %>%
      mutate(value = value * value_AEO) -> L143.HDDCDD_AEO_AEO_years

    # HDD / CDD values are held constant at 2050 values past 2050 (final AEO year)
    L143.HDDCDD_AEO_AEO_years %>%
      filter(year == max(gcamusa.AEO_DD_YEARS)) %>%
      distinct(state, subregion9, variable, value) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      filter(year > max(gcamusa.AEO_DD_YEARS)) -> L143.HDDCDD_AEO_postAEO_years

    # bind new AEO scenario into L143.HDDCDD_scen_state
    L143.HDDCDD_scen_state %>%
      bind_rows(L143.HDDCDD_AEO_hist_years %>%
                  bind_rows(L143.HDDCDD_AEO_AEO_years,
                            L143.HDDCDD_AEO_postAEO_years) %>%
                  mutate(Scen = "AEO_2015",
                         GCM = "AEO_2015")  %>%
                  # getting rid of duplicate 2010 values & extra columns
                  distinct(state, Scen, GCM, variable, year, value)) ->
      L143.HDDCDD_scen_state


    # ===================================================
    # Produce outputs
    L143.share_state_Pop_CDD_sR9 %>%
      add_title("State-level share of person cooling degree days within census division (subregion9)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolate historic cooling degree data to fill in missing model historical years.") %>%
      add_comments("Divide state cooling degree days by the census subregion 9 total population") %>%
      add_legacy_name("L143.share_state_Pop_CDD_sR9") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/GIS/CDD_His") ->
      L143.share_state_Pop_CDD_sR9

    L143.share_state_Pop_CDD_sR13 %>%
      add_title("State-level share of person cooling degree days within subregion13") %>%
      add_units("Unitless") %>%
      add_comments("Interpolate historic cooling degree data to fill in missing model historical years.") %>%
      add_comments("Divide state cooling degree days by the census subregion 13 total population.") %>%
      add_legacy_name("L143.share_state_Pop_CDD_sR13") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/GIS/CDD_His") ->
      L143.share_state_Pop_CDD_sR13

    L143.share_state_Pop_HDD_sR9 %>%
      add_title("State-level share of person heating degree days within census division (subregion9)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolate historic heating degree data to fill in missing model historical years.") %>%
      add_comments("Divide state heating degree days by the census subregion 9 total population.") %>%
      add_legacy_name("L143.share_state_Pop_HDD_sR9") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/GIS/HDD_His") ->
      L143.share_state_Pop_HDD_sR9

    L143.share_state_Pop_HDD_sR13 %>%
      add_title("State-level share of person heating degree days within census division (subregion13)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolate historic heating degree data to fill in missing model historical years.") %>%
      add_comments("Divide state heating degree days by the census subregion 13 total population.") %>%
      add_legacy_name("L143.share_state_Pop_HDD_sR13") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/GIS/HDD_His") ->
      L143.share_state_Pop_HDD_sR13

    L143.HDDCDD_scen_state %>%
      add_title("Heating and cooling degree days by state and scenario") %>%
      add_units("Degree F days") %>%
      add_comments("Replace GCM degree days with historical observations.") %>%
      add_comments("Normalize future GCM degree days by the fraction of observed degree days to GCM degree days in the base year.") %>%
      add_comments("Includes a scenario with HDD / CDD harmonized to AEO 2015") %>%
      add_legacy_name("L143.HDDCDD_scen_state") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/GIS/CDD_His",
                     "gcam-usa/GIS/HDD_His",
                     "gcam-usa/GIS/CDD_hist_constdds",
                     "gcam-usa/GIS/HDD_hist_constdds",
                     "gcam-usa/GIS/HDD_GFDL_A2",
                     "gcam-usa/GIS/CDD_GFDL_A2",
                     "gcam-usa/AEO_2015_HDDCDD") ->
      L143.HDDCDD_scen_state

    return_data(L143.share_state_Pop_CDD_sR9,
                L143.share_state_Pop_CDD_sR13,
                L143.share_state_Pop_HDD_sR9,
                L143.share_state_Pop_HDD_sR13,
                L143.HDDCDD_scen_state)
  } else {
    stop("Unknown command")
  }
}
