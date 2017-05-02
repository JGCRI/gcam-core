#' module_energy_LA126.distribution
#'
#' This chunk adjusts for onsite, transmission, and distribution losses for electricity and gas. Energy input, output, and input/output ratio are calculated for the following three processes:
#'     Electricity net ownuse, i.e., electricity consumed onsite prior to any transmission and distribution losses
#'     Electricity transmission and distribution
#'     Gas pipeline
#' Nine tables are created in total, 3x3=9. Energy is reported by GCAM_region, sector, fuel, and from 1971-2010.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L126.in_EJ_R_elecownuse_F_Yh}, \code{L126.out_EJ_R_elecownuse_F_Yh}, \code{L126.IO_R_elecownuse_F_Yh}, \code{L126.in_EJ_R_electd_F_Yh}, \code{L126.out_EJ_R_electd_F_Yh}, \code{L126.IO_R_electd_F_Yh}, \code{L126.in_EJ_R_gaspipe_F_Yh}, \code{L126.out_EJ_R_gaspipe_F_Yh}, \code{L126.IO_R_gaspipe_F_Yh}. The corresponding file in the
#' original data system was \code{LA126.distribution.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS May 2017
#'
module_energy_LA126.distribution <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "temp-data-inject/L122.out_EJ_R_gasproc_F_Yh",
             FILE = "L123.out_EJ_R_elec_F_Yh",
             FILE = "L123.out_EJ_R_indchp_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L126.IO_R_elecownuse_F_Yh",
             "L126.in_EJ_R_electd_F_Yh",
             "L126.out_EJ_R_electd_F_Yh",
             "L126.IO_R_electd_F_Yh",
             "L126.in_EJ_R_gaspipe_F_Yh",
             "L126.out_EJ_R_gaspipe_F_Yh",
             "L126.IO_R_gaspipe_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh")
    L122.out_EJ_R_gasproc_F_Yh <- get_data(all_data, "temp-data-inject/L122.out_EJ_R_gasproc_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L123.out_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.out_EJ_R_indchp_F_Yh")

    # ===================================================
    # ELECTRICITY OWNUSE, i.e., electricity consumed onsite prior to any transmission and distribution losses
      # Summing industrial CHP electricity generation and electricity generation by GCAM region ID and year
    L123.out_EJ_R_indchp_F_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value_indchp_F_Yh = sum(value)) %>%
      left_join_error_no_match( # Joining L123.out_EJ_R_elec_F_Yh, but first summing by GCAM region ID and year
        summarise(
          group_by(L123.out_EJ_R_elec_F_Yh, GCAM_region_ID, year),
          value_elec_F_Yh = sum(value)),
        by = c("GCAM_region_ID", "year")) %>%
      mutate(value_b = value_elec_F_Yh + value_indchp_F_Yh) %>% # Summing electricity generation and industrial CHP electricity generation. This creates values for table, L126.in_EJ_R_elecownuse_F_Yh (i.e., input).
      select(-value_elec_F_Yh, -value_indchp_F_Yh) ->
      a

      # Filtering energy balance by "net_electricity ownuse" sector and joining with previous table
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(sector == "net_electricity ownuse") %>%
      mutate(sector = substr(sector, 5, 30)) %>% # Renaming sector as "electricity ownuse"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_a = sum(value)) %>%
      left_join_error_no_match(a, by = c("GCAM_region_ID", "year")) %>% # Joining previous table (electricity sector generation, which includes industrial CHP electricity generation)
      mutate(value_c = value_b - value_a) %>% # Creating values for table, L126.out_EJ_R_elecownuse_F_Yh (i.e., output), by subtracting electricity generation by elecricity ownuse
      mutate(value_d = value_b / value_c) -> # Creating values for table, L126.IO_R_elecownuse_F_Yh,  by dividing input by output
      b

      # Table b is separated to create the final tables
    b %>% select(GCAM_region_ID, sector, fuel, year, value = value_b) -> L126.in_EJ_R_elecownuse_F_Yh
    b %>% select(GCAM_region_ID, sector, fuel, year, value = value_c) -> L126.out_EJ_R_elecownuse_F_Yh
    b %>% select(GCAM_region_ID, sector, fuel, year, value = value_d) -> L126.IO_R_elecownuse_F_Yh

    # ELECTRICITY TRANSMISSION AND DISTRIBUTION
      # Filtering energy balance by "net_electricity distribution" sector and joining with electricity generation output (i.e., L126.out_EJ_R_elecownuse_F_Yh)
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(sector == "net_electricity distribution") %>%
      mutate(sector = substr(sector, 5, 30)) %>% # Renaming sector as "electricity distribution"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_a = sum(value)) %>%
      left_join_error_no_match( # Joining electricity generation output (L126.out_EJ_R_elecownuse_F_Yh, wherein electricity ownuse was subtracted from electricity generation)
        select(
          ungroup(L126.out_EJ_R_elecownuse_F_Yh), # Need to ungroup to deselect sector and fuel
          -sector, -fuel),
        by = c("GCAM_region_ID", "year")) %>%
      mutate(value_b = value - value_a) %>% # Creating values for table, L126.out_EJ_R_electd_F_Yh (i.e. ouput), by subtracting electricity generation (without ownuse) by transmission and distribution consumption
      mutate(value_c = value / value_b) -> # Creating values for table, L126.IO_R_electd_F_Yh, by dividing input by output
      c

      # Table c is separated to create the final tables
    c %>% select(GCAM_region_ID, sector, fuel, year, value) -> L126.in_EJ_R_electd_F_Yh
    c %>% select(GCAM_region_ID, sector, fuel, year, value = value_b) -> L126.out_EJ_R_electd_F_Yh
    c %>% select(GCAM_region_ID, sector, fuel, year, value = value_c) -> L126.IO_R_electd_F_Yh

    # GAS PIPELINE
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(sector == "net_gas pipeline") %>%
      mutate(sector = substr(sector, 5, 30)) %>% # Renaming sector as "gas pipeline"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_a = sum(value)) %>%
      left_join( # Joining L122.out_EJ_R_gasproc_F_Yh, but first converting to long form, stripping years of X's, and summing by GCAM region ID and year
        summarise(
          group_by(
            mutate(
              gather(L122.out_EJ_R_gasproc_F_Yh, year, value, -GCAM_region_ID, -sector, -fuel),
              year = as.integer(substr(year, 2, 5))),
            GCAM_region_ID, year),
          value_b = sum(value)), # Creating values for table, L126.in_EJ_R_gaspipe_F_Yh (i.e., input)
        by = c("GCAM_region_ID", "year")) %>%
      mutate(value_c = value_b - value_a) %>% # Creating values for table, L126.out_EJ_R_gaspipe_F_Yh (i.e., output), by subtracting gas pipeline input by gas pipeline consumption
      mutate(value_d = value_b / value_c) %>% # Creating values for table, L126.IO_R_gaspipe_F_Yh, by dividing input by output. Note that some regions have zero gas in some of the base years. Reset their IO coefs to 1 in the next step.
      mutate(value_d = if_else(is.na(value_d),1,value_d)) -> # Reset NaN IO coefs to 1, since some regions have no gas in some base years.
      d

      # Table d is separated to create the final tables
    d %>% select(GCAM_region_ID, sector, fuel, year, value = value_b) -> L126.in_EJ_R_gaspipe_F_Yh
    d %>% select(GCAM_region_ID, sector, fuel, year, value = value_c) -> L126.out_EJ_R_gaspipe_F_Yh
    d %>% select(GCAM_region_ID, sector, fuel, year, value = value_d) -> L126.IO_R_gaspipe_F_Yh

    # ===================================================
    L126.in_EJ_R_elecownuse_F_Yh %>%
      add_title("Electricity onsite energy input") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation, including industrial CHP secondary input") %>%
      add_legacy_name("L126.in_EJ_R_elecownuse_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.in_EJ_R_elecownuse_F_Yh
    L126.out_EJ_R_elecownuse_F_Yh %>%
      add_title("Electricity onsite energy output") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite losses") %>%
      add_legacy_name("L126.out_EJ_R_elecownuse_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.out_EJ_R_elecownuse_F_Yh
    L126.IO_R_elecownuse_F_Yh %>%
      add_title("Electricity onsite energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_legacy_name("L126.IO_R_elecownuse_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.IO_R_elecownuse_F_Yh
    L126.in_EJ_R_electd_F_Yh %>%
      add_title("Electricity transmission and distribution energy input") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite losses") %>%
      add_legacy_name("L126.in_EJ_R_electd_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.in_EJ_R_electd_F_Yh
    L126.out_EJ_R_electd_F_Yh %>%
      add_title("Electricity transmission and distribution energy output") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite, transmission, and distribution losses") %>%
      add_legacy_name("L126.out_EJ_R_electd_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.out_EJ_R_electd_F_Yh
    L126.IO_R_electd_F_Yh %>%
      add_title("Electricity transmission and distribution energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_legacy_name("L126.IO_R_electd_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.out_EJ_R_indchp_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.IO_R_electd_F_Yh
    L126.in_EJ_R_gaspipe_F_Yh %>%
      add_title("Gas pipeline energy input") %>%
      add_units("EJ") %>%
      add_comments("Energy input to gas pipeline") %>%
      add_legacy_name("L126.in_EJ_R_gaspipe_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "temp-data-inject/L122.out_EJ_R_gasproc_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.in_EJ_R_gaspipe_F_Yh
    L126.out_EJ_R_gaspipe_F_Yh %>%
      add_title("Gas pipeline energy output") %>%
      add_units("EJ") %>%
      add_comments("Energy output of gas pipeline, accounting for pipeline losses") %>%
      add_legacy_name("L126.out_EJ_R_gaspipe_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "temp-data-inject/L122.out_EJ_R_gasproc_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.out_EJ_R_gaspipe_F_Yh
    L126.IO_R_gaspipe_F_Yh %>%
      add_title("Gas pipeline energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_legacy_name("L126.IO_R_gaspipe_F_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "temp-data-inject/L122.out_EJ_R_gasproc_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L126.IO_R_gaspipe_F_Yh

    return_data(L126.in_EJ_R_elecownuse_F_Yh, L126.out_EJ_R_elecownuse_F_Yh, L126.IO_R_elecownuse_F_Yh,
                L126.in_EJ_R_electd_F_Yh, L126.out_EJ_R_electd_F_Yh, L126.IO_R_electd_F_Yh,
                L126.in_EJ_R_gaspipe_F_Yh, L126.out_EJ_R_gaspipe_F_Yh, L126.IO_R_gaspipe_F_Yh)
  } else {
    stop("Unknown command")
  }
}
