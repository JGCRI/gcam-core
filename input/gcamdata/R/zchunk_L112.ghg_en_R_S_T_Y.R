#' module_emissions_L112.ghg_en_R_S_T_Y
#'
#' Calculates emissions and emissions factors using EPA emissions factors and scales to EDGAR emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.ghg_tg_R_en_S_F_Yh}, \code{L112.ghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L112.ghg_en_R_S_T_Y.R} (emissions level1).
#' @details Calculates emissions using EPA emissions factors and energy data. Then scales to EDGAR emissions and calculates emissions factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH July 2017
module_emissions_L112.ghg_en_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/mappings/EPA_ghg_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L101.in_EJ_R_en_Si_F_Yh",
             "L102.ghg_tgej_USA_en_Sepa_F_2005",
             FILE = "emissions/EDGAR/EDGAR_CH4",
             FILE = "emissions/EDGAR/EDGAR_N2O"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.ghg_tg_R_en_S_F_Yh",
             "L112.ghg_tgej_R_en_S_F_Yh"))
  } else if(command == driver.MAKE) {

    input_emissions <- emfact <- EDGAR_agg_sector <- EDGAR_emissions <- EPA_agg_fuel <-
      EPA_agg_sector <- GCAM_region_ID <- IPCC <- ch4_em_factor <- n2o_em_factor <-
      emiss_factor <- emissions <- energy <- epa_emissions <- fuel <- input_emissions <-
      scaler <- sector <- stub.technology <- subsector <- supplysector <- technology <-
      value <- year <- variable <- CH4 <- N2O <- Non.CO2 <- agg_sector <- NULL
    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    EPA_ghg_tech <- get_data(all_data, "emissions/mappings/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
      gather_years(value_col = "energy")
    L102.ghg_tgej_USA_en_Sepa_F_2005 <- get_data(all_data, "L102.ghg_tgej_USA_en_Sepa_F_2005") %>%
      rename(CH4 = ch4_em_factor, N2O = n2o_em_factor) %>%
      gather(variable, emiss_factor, CH4, N2O)
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR/EDGAR_CH4") %>%
      mutate(Non.CO2 = "CH4")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR/EDGAR_N2O") %>%
      mutate(Non.CO2 = "N2O")

    # Computing unscaled EPA emissions by country and technology
    L112.ghg_tg_R_en_Si_F_Yh <- L101.in_EJ_R_en_Si_F_Yh %>%
      left_join_keep_first_only(GCAM_sector_tech %>%
                                  select(EPA_agg_sector, EPA_agg_fuel, EDGAR_agg_sector, fuel, sector, technology),
                                by = c("fuel", "sector", "technology")) %>%
      # Remove years where we don't have emissions data and add Non.CO2 column
      filter(year %in% emissions.EDGAR_YEARS) %>%
      repeat_add_columns(tibble(Non.CO2 = c("CH4", "N2O"))) %>%
      # Match in emissions factors
      # Use left_join because some "cement" only in EPA sector
      left_join(L102.ghg_tgej_USA_en_Sepa_F_2005,
                by = c("Non.CO2" = "variable", "EPA_agg_sector" = "sector", "EPA_agg_fuel" = "fuel")) %>%
      # Compute unscaled emissions
      mutate(epa_emissions = energy * emiss_factor) %>%
      na.omit()

    # Aggregate EPA emissions to EDGAR sector
    L112.ghg_tg_R_en_Sedgar_Yh <- L112.ghg_tg_R_en_Si_F_Yh %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(epa_emissions = sum(epa_emissions)) %>%
      ungroup()

    # Compute EDGAR emissions by region and sector
    L112.EDGAR <- bind_rows(EDGAR_CH4, EDGAR_N2O) %>%
      # Use left_join because EDGAR_sector does not have IPCC 5D (Peat fires) or 5F2 (Forest Fires-Post burn decay)
      fast_left_join(EDGAR_sector %>% select(IPCC, EDGAR_agg_sector = agg_sector), by = "IPCC") %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      na.omit() %>%
      gather_years %>%
      filter(year %in% emissions.EDGAR_YEARS) %>%
      # Aggregate by region, GHG, and EDGAR sector
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(value = sum(value))

    # Scale EPA emissions by tech to match EDGAR totals
    # First compute scalers
    L112.emiss_scaler <- L112.ghg_tg_R_en_Sedgar_Yh %>%
      # Use left_join because a few regions missing oil_gas and coal in L112.EDGAR
      left_join(L112.EDGAR %>% rename(EDGAR_emissions = value),
                by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      mutate(scaler = EDGAR_emissions / epa_emissions / 1000) %>%
      select(GCAM_region_ID, year, Non.CO2, EDGAR_agg_sector, scaler)

    # Now, scale EPA emissions
    L112.ghg_tg_R_en_Si_F_Yh <- L112.ghg_tg_R_en_Si_F_Yh %>%
      left_join(L112.emiss_scaler, by = c("GCAM_region_ID", "year", "Non.CO2", "EDGAR_agg_sector")) %>%
      mutate(emissions = epa_emissions * scaler) %>%
      replace_na(replace = list(emissions = 0, scaler = 0))

    # Map in GCAM sector, technology, and driver type
    L112.ghg_tg_R_en_S_F_Yh <- L112.ghg_tg_R_en_Si_F_Yh %>%
      left_join_keep_first_only(GCAM_sector_tech %>% select(sector, fuel, technology, supplysector, subsector, stub.technology),
                                by = c("sector", "fuel", "technology")) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year) %>%
      summarise(value = sum(emissions)) %>%
      na.omit() %>%
      ungroup

    # Compute emissions factor by GCAM sector, technology, and driver type
    # First compute energy by sector
    L112.in_EJ_R_en_S_F_Yh <- L101.in_EJ_R_en_Si_F_Yh %>%
      left_join_keep_first_only(GCAM_sector_tech %>% select(sector, fuel, technology, supplysector, subsector, stub.technology),
                                by = c("sector", "fuel", "technology")) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, year) %>%
      summarise(energy = sum(energy))

    # Now join emissions and energy data together to calculate emissions factors
    L112.ghg_tgej_R_en_S_F_Yh <- L112.ghg_tg_R_en_S_F_Yh %>%
      rename(input_emissions = value) %>%
      left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh,
                               by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(emfact = input_emissions / energy) %>%
      replace_na(list(emfact = 0)) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact)

    # ===================================================
    # Produce outputs
    L112.ghg_tg_R_en_S_F_Yh %>%
      add_title("GHG emissions by energy sector, gas, region, and historical year") %>%
      add_units("Tg") %>%
      add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
      add_legacy_name("L112.ghg_tg_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "emissions/mappings/EPA_ghg_tech",
                     "emissions/mappings/GCAM_sector_tech", "L101.in_EJ_R_en_Si_F_Yh",
                     "L102.ghg_tgej_USA_en_Sepa_F_2005", "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O") ->
      L112.ghg_tg_R_en_S_F_Yh

    L112.ghg_tgej_R_en_S_F_Yh %>%
      add_title("GHG emissions factors by energy sector, gas, region, and historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
      add_comments("Then, emissions factors computed by dividing calculated emissions by energy data") %>%
      add_legacy_name("L112.ghg_tgej_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "emissions/mappings/EPA_ghg_tech",
                     "emissions/mappings/GCAM_sector_tech", "L101.in_EJ_R_en_Si_F_Yh",
                     "L102.ghg_tgej_USA_en_Sepa_F_2005", "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O") ->
      L112.ghg_tgej_R_en_S_F_Yh

    return_data(L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh)
  } else {
    stop("Unknown command")
  }
}
