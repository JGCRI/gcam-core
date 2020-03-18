# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA143.HDDCDD
#'
#' Reads in country level heating and cooling degree day data and returns GCAM region degree days via population weighting
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L143.HDDCDD_scen_R_Y}, \code{L143.HDDCDD_scen_RG3_Y}, \code{L143.HDDCDD_scen_ctry_Y}. The corresponding file in the
#' original data system was \code{LA143.HDDCDD.R} (energy level1).
#' @details Population weights HDDCDD from country level to GCAM region
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select summarise
#' @author RH April 2017
module_energy_LA143.HDDCDD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/GIS_ctry",
             "L101.Pop_thous_GCAM3_ctry_Y",
             FILE = "energy/GIS/population_weighted_CDD_CCSM3x_A2",
             FILE = "energy/GIS/population_weighted_CDD_CCSM3x_B1",
             FILE = "energy/GIS/population_weighted_CDD_HadCM3_A2",
             FILE = "energy/GIS/population_weighted_CDD_HadCM3_B1",
             FILE = "energy/GIS/population_weighted_CDD_no_GCM_constdd",
             FILE = "energy/GIS/population_weighted_HDD_CCSM3x_A2",
             FILE = "energy/GIS/population_weighted_HDD_CCSM3x_B1",
             FILE = "energy/GIS/population_weighted_HDD_HadCM3_A2",
             FILE = "energy/GIS/population_weighted_HDD_HadCM3_B1",
             FILE = "energy/GIS/population_weighted_HDD_no_GCM_constdd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L143.HDDCDD_scen_R_Y",
             "L143.HDDCDD_scen_RG3_Y",
             "L143.HDDCDD_scen_ctry_Y"))
  } else if(command == driver.MAKE) {

    value <- year <- `2099` <- country <- iso <- population <- GCAM_region_ID <-
      SRES <- GCM <- variable <- wtDD <- aggpop <- region_GCAM3 <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GIS_ctry <- get_data(all_data, "energy/GIS_ctry")
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_ctry_Y") %>%
      rename(population = value) %>% mutate(year = as.integer(year))
    CDD_CCSM3x_A2 <- get_data(all_data, "energy/GIS/population_weighted_CDD_CCSM3x_A2")
    CDD_CCSM3x_B1 <- get_data(all_data, "energy/GIS/population_weighted_CDD_CCSM3x_B1")
    CDD_HadCM3_A2 <- get_data(all_data, "energy/GIS/population_weighted_CDD_HadCM3_A2")
    CDD_HadCM3_B1 <- get_data(all_data, "energy/GIS/population_weighted_CDD_HadCM3_B1")
    CDD_no_GCM_constdd <- get_data(all_data, "energy/GIS/population_weighted_CDD_no_GCM_constdd")
    HDD_CCSM3x_A2 <- get_data(all_data, "energy/GIS/population_weighted_HDD_CCSM3x_A2")
    HDD_CCSM3x_B1 <- get_data(all_data, "energy/GIS/population_weighted_HDD_CCSM3x_B1")
    HDD_HadCM3_A2 <- get_data(all_data, "energy/GIS/population_weighted_HDD_HadCM3_A2")
    HDD_HadCM3_B1 <- get_data(all_data, "energy/GIS/population_weighted_HDD_HadCM3_B1")
    HDD_no_GCM_constdd <- get_data(all_data, "energy/GIS/population_weighted_HDD_no_GCM_constdd")

    # ===================================================

    # Reading HDDCDD files in a list so that row binding will include a filename
    HDDCDD_data_list <- list(CDD_CCSM3x_A2 = CDD_CCSM3x_A2, CDD_CCSM3x_B1 = CDD_CCSM3x_B1,
                             CDD_HadCM3_A2 = CDD_HadCM3_A2, CDD_HadCM3_B1 = CDD_HadCM3_B1,
                             CDD_no_GCM_constdd = CDD_no_GCM_constdd, HDD_CCSM3x_A2 = HDD_CCSM3x_A2,
                             HDD_CCSM3x_B1 = HDD_CCSM3x_B1, HDD_HadCM3_A2 = HDD_HadCM3_A2,
                             HDD_HadCM3_B1 = HDD_HadCM3_B1, HDD_no_GCM_constdd = HDD_no_GCM_constdd)
    HDDCDD_data <- bind_rows(HDDCDD_data_list, .id = 'file')

    # Currently the HDDCDD data stops at 2099. If this is the case, add 2100
    if(!"2100" %in% names( HDDCDD_data)) {
      HDDCDD_data <- HDDCDD_data %>% mutate(`2100` = `2099`)
    }

    # Convert data to long format and add in id variables
    HDDCDD_data <- HDDCDD_data %>%
      gather_years %>%
      mutate(# Assuming that the variable is the first three letters
             variable = substr(file, 1, 3),
             # Assuming that the GCM comes after "DD_" and is 6 letters
             GCM = substr(file, 5, 10),
             # Assuming that the last word is the scenario, starting at twelve letters
             SRES = substr(file, 12, length(file)),
             # Set all negative values to 0
             value = if_else(value < 0, 0, value)
      )

    # Add in country iso
    L143.HDDCDD_scen_ctry_Y <- HDDCDD_data %>%
      # Drop file name
      select(-file) %>%
      # Filter only useful years. The method assumes that HDD/CDD is printed annually -> no interpolation performed
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      # Remove apostrophe in Cote d'Ivoire and add in country iso by country name
      mutate(country = if_else(country == "Cote d'Ivoire", "Cote dIvoire", country)) %>%
      left_join_error_no_match(GIS_ctry, by = 'country')

    # Serbia and Montenegro are currently combined. Copy to separated countries, assigning the same HDD and CDD to each
    if("scg" %in% L143.HDDCDD_scen_ctry_Y$iso) {
      # Create Serbia tibble
      L143.HDDCDD_scen_srb_Y <- L143.HDDCDD_scen_ctry_Y %>%
        filter(iso == "scg") %>%
        mutate(iso = "srb")

      # Insert Serbia tibble, change 'scg' iso to 'mne' iso
      L143.HDDCDD_scen_ctry_Y <- L143.HDDCDD_scen_ctry_Y %>%
        bind_rows(L143.HDDCDD_scen_srb_Y) %>%
        mutate(iso = if_else(iso == "scg", "mne", iso))
    }

    # Extend population data to all years
    iso_list <- tibble(iso = L101.Pop_thous_GCAM3_ctry_Y$iso %>% unique())
    all_years <- tibble(year = seq(min(HISTORICAL_YEARS), max(FUTURE_YEARS)))
    GCAM3_population_df <- repeat_add_columns(iso_list, all_years) %>%
      left_join(L101.Pop_thous_GCAM3_ctry_Y, by = c("iso", "year")) %>%
      group_by(iso) %>%
      mutate(population = approx_fun(year, population)) %>%
      ungroup()

    # Add population data and region data
    L143.wtHDDCDD_scen_ctry_Y <- L143.HDDCDD_scen_ctry_Y %>%
      # Join with population data converted to long format
      left_join_error_no_match(GCAM3_population_df,
                               by = c("iso", "year")) %>%
      # Join with region ID data
      left_join_error_no_match(iso_GCAM_regID, by = "iso")

    # Find weighted mean with DD as values and population as weights.

    # Calculate weighted degree day by GCAM 4 regions
    L143.HDDCDD_scen_R_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
      group_by(GCAM_region_ID, SRES, GCM, variable, year) %>%
      summarise(value = weighted.mean(value, population)) %>%
      ungroup()

    # Calculate weighted degree day by GCAM 3 regions
    L143.HDDCDD_scen_RG3_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
      group_by(region_GCAM3, SRES, GCM, variable, year) %>%
      summarise(value = weighted.mean(value, population)) %>%
      ungroup()


    # ===================================================

    # Produce outputs
    L143.HDDCDD_scen_R_Y %>%
      add_title("HDDCDD by GCAM region") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("Population weighted country HDDCDD data from multiple ESMs and scenarios to GCAM region") %>%
      add_legacy_name("L143.HDDCDD_scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/GIS_ctry", "L101.Pop_thous_GCAM3_ctry_Y",
                     "energy/GIS/population_weighted_CDD_CCSM3x_A2",
                     "energy/GIS/population_weighted_CDD_CCSM3x_B1",
                     "energy/GIS/population_weighted_CDD_HadCM3_A2",
                     "energy/GIS/population_weighted_CDD_HadCM3_B1",
                     "energy/GIS/population_weighted_CDD_no_GCM_constdd",
                     "energy/GIS/population_weighted_HDD_CCSM3x_A2",
                     "energy/GIS/population_weighted_HDD_CCSM3x_B1",
                     "energy/GIS/population_weighted_HDD_HadCM3_A2",
                     "energy/GIS/population_weighted_HDD_HadCM3_B1",
                     "energy/GIS/population_weighted_HDD_no_GCM_constdd") ->
      L143.HDDCDD_scen_R_Y

    L143.HDDCDD_scen_RG3_Y %>%
      add_title("HDDCDD by GCAM3 region") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("Population weighted country HDDCDD data from multiple ESMs and scenarios to GCAM3 region") %>%
      add_legacy_name("L143.HDDCDD_scen_RG3_Y") %>%
      same_precursors_as(L143.HDDCDD_scen_R_Y) ->
      L143.HDDCDD_scen_RG3_Y

    L143.HDDCDD_scen_ctry_Y %>%
      add_title("HDDCDD by country") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("Combined data from multiple ESMs and scenarios") %>%
      add_legacy_name("L143.HDDCDD_scen_ctry_Y") %>%
      same_precursors_as(L143.HDDCDD_scen_R_Y) ->
      L143.HDDCDD_scen_ctry_Y

    return_data(L143.HDDCDD_scen_R_Y, L143.HDDCDD_scen_RG3_Y, L143.HDDCDD_scen_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
