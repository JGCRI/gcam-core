# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L111.nonghg_en_R_S_T_Y
#'
#' Calculate non-ghg emission totals and non-ghg emission shares of total emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.nonghg_tg_R_en_S_F_Yh}, \code{L111.nonghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L111.nonghg_en_R_S_T_Y.R} (emissions level1).
#' @details This code produces two outputs: non-ghg emission totals and non-ghg emission shares of total emissions.
#' First, non-ghg gas emissions are combined and grouped by sector and region, emissions are scaled, and international
#' shipping & aviation emission data calculated based on total emission and total emission shares. Finally, non-ghg emission
#' totals and shares are calculated by GCAM sector, fuel, technology, and driver type for EDGAR historical years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate right_join select summarise summarise_if
#' @importFrom tidyr gather replace_na
#' @author RC April 2018
module_emissions_L111.nonghg_en_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/EDGAR/EDGAR_nation",
             FILE = "emissions/mappings/EPA_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/GCAM_sector_tech_Revised",
             "L101.in_EJ_R_en_Si_F_Yh",
             "L101.so2_tgej_USA_en_Sepa_F_Yh",
             "L101.co_tgej_USA_en_Sepa_F_Yh",
             "L101.nox_tgej_USA_en_Sepa_F_Yh",
             "L101.voc_tgej_USA_en_Sepa_F_Yh",
             "L101.nh3_tgej_USA_en_Sepa_F_Yh",
             FILE = "emissions/EDGAR/EDGAR_SO2",
             FILE = "emissions/EDGAR/EDGAR_CO",
             FILE = "emissions/EDGAR/EDGAR_NOx",
             FILE = "emissions/EDGAR/EDGAR_NMVOC",
             FILE = "emissions/EDGAR/EDGAR_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.nonghg_tg_R_en_S_F_Yh",
             "L111.nonghg_tgej_R_en_S_F_Yh"))
  } else if(command == driver.MAKE) {

    # silence package check
    Non.CO2 <- sector <- fuel <- year <- technology <- sector <- stub.technology <- energy <-
      fuel <- supplysector <- subsector <- GCAM_region_ID <- iso <- value <- epa_emissions <-
      tot_energy <- tot_emiss <- energy_share <- emissions <- `IPCC-Annex` <- `World Region` <-
      Name <- IPCC <- IPCC_description <- agg_sector <- emfact <- scaler <- ISO_A3 <- `2009` <- `2010` <-
      EPA_agg_sector <- EPA_agg_fuel <- EDGAR_agg_sector <- EPA_emissions <- EDGAR_emissions <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    EDGAR_nation <- get_data(all_data, "emissions/EDGAR/EDGAR_nation")
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")

    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_Revised")

    }


    L101.co_tgej_USA_en_Sepa_F_Yh  <- get_data(all_data, "L101.co_tgej_USA_en_Sepa_F_Yh")
    L101.so2_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.so2_tgej_USA_en_Sepa_F_Yh")
    L101.nox_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nox_tgej_USA_en_Sepa_F_Yh")
    L101.voc_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.voc_tgej_USA_en_Sepa_F_Yh")
    L101.nh3_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nh3_tgej_USA_en_Sepa_F_Yh")

    L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh")

    EDGAR_SO2   <- get_data(all_data, "emissions/EDGAR/EDGAR_SO2")
    EDGAR_CO    <- get_data(all_data, "emissions/EDGAR/EDGAR_CO")
    EDGAR_NOx   <- get_data(all_data, "emissions/EDGAR/EDGAR_NOx")
    EDGAR_NMVOC <- get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC")
    EDGAR_NH3   <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3")

    # Perform computations
    # First add gas name and bind all emission factor data together
    L111.nonghg_tgej_USA_en_Sepa_F_Yh <- bind_rows(L101.co_tgej_USA_en_Sepa_F_Yh %>% mutate(Non.CO2 = "CO"),
                                                   L101.so2_tgej_USA_en_Sepa_F_Yh %>% mutate(Non.CO2 = "SO2"),
                                                   L101.nox_tgej_USA_en_Sepa_F_Yh %>% mutate(Non.CO2 = "NOx"),
                                                   L101.voc_tgej_USA_en_Sepa_F_Yh %>% mutate(Non.CO2 = "NMVOC"),
                                                   L101.nh3_tgej_USA_en_Sepa_F_Yh %>% mutate(Non.CO2 = "NH3")) %>%
      arrange(Non.CO2, sector, fuel, year) %>%
      # For each gas, sector and fuel, replace 2003-2008 data with 2002 values
      group_by(Non.CO2, sector, fuel) %>%
      mutate(value = replace(value, year >= 2003 & year <= 2008, value[year == 2002])) %>%
      rename(emfact = value) %>%
      ungroup

    # Compute unscaled emissions by country and technology
    L111.nonghg_tg_R_en_Si_F_Yh <- L101.in_EJ_R_en_Si_F_Yh %>%
      gather(year, energy, -GCAM_region_ID, -sector, -fuel, -technology) %>%
      mutate(year = as.integer(year)) %>%
      # Match in EPA sector and fuel categories, use left_join due to NAs in the EPA categories
      left_join(GCAM_sector_tech %>% select(sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel) %>% unique,
                by = c("sector", "fuel", "technology")) %>%
      # Duplicate value for all gases
      repeat_add_columns(tibble::tibble(Non.CO2 = emissions.NONGHG_GASES)) %>%
      # Match in emissions factors
      left_join(L111.nonghg_tgej_USA_en_Sepa_F_Yh, by = c("Non.CO2", "EPA_agg_sector" = "sector", "EPA_agg_fuel" = "fuel", "year")) %>%
      # Compute unscaled emissions
      mutate(epa_emissions = energy * emfact) %>%
      na.omit %>%
      # Match in EDGAR sector
      left_join_error_no_match(GCAM_sector_tech %>% select(sector, fuel, EDGAR_agg_sector) %>% unique,
                               by = c("sector", "fuel"))

    # Aggregate EPA emissions by EDGAR sector and GCAM region
    L111.nonghg_tg_R_en_Sedgar_Yh <- L111.nonghg_tg_R_en_Si_F_Yh %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(EPA_emissions = sum(epa_emissions)) %>%
      ungroup

    # Compute EDGAR emissions by region and sector
    # Add gas names and combine all data together

    # Need to avoid triggering a (false positive) consecutive-mutate error
    e_SO2 <- EDGAR_SO2 %>% mutate(Non.CO2 = "SO2")
    e_CO <- EDGAR_CO %>% mutate(Non.CO2 = "CO")
    e_NOx <- EDGAR_NOx %>% mutate(Non.CO2 = "NOx")
    e_NH3 <- EDGAR_NH3 %>% mutate(Non.CO2 = "NH3")
    e_NMVOC <- EDGAR_NMVOC %>% mutate(Non.CO2 = "NMVOC") %>% select(-`2009`, -`2010`)

    L111.EDGAR <- bind_rows(e_SO2, e_CO, e_NOx, e_NH3, e_NMVOC) %>%
      # Match in EDGAR sectors, use left_join because there are NAs in the data
      left_join(EDGAR_sector %>% select(IPCC, EDGAR_agg_sector = agg_sector) %>% unique, by = "IPCC") %>%
      # Match in EDGAR countries, use left_join because there are NAs in the data
      left_join(EDGAR_nation %>% unique, by = "ISO_A3") %>%
      # Match in GCAM region
      left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso")

    # Save international shipping & aviation emissions in a separate tibble
    L111.EDGAR_intl <- L111.EDGAR %>%
      filter(ISO_A3 %in% c("SEA", "AIR")) %>%
      select(-`IPCC-Annex`, -`World Region`, -ISO_A3, -iso, -Name, -IPCC, -IPCC_description, -GCAM_region_ID) %>%
      na.omit %>%
      gather(year, tot_emiss, -EDGAR_agg_sector, -Non.CO2) %>%
      mutate(year = as.integer(year))

    # Aggregate EDGAR emissions by GCAM region and EDGAR sector
    L111.EDGAR.agg <- L111.EDGAR %>%
      na.omit %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup %>%
      gather(year, EDGAR_emissions, -GCAM_region_ID, -Non.CO2, -EDGAR_agg_sector) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% emissions.EDGAR_HISTORICAL)

    # Scale EPA emissions by technology to match EDGAR totals
    # First compute scalers
    L111.emiss_scaler <- L111.nonghg_tg_R_en_Sedgar_Yh %>%
      # Match EAP and EDGAR emissions, use left_join due to NAs
      left_join(L111.EDGAR.agg, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      # Convert units, EPA emissions are in Tg, and EDGAR emissions are in Gg, 1 Tg = 1000 Gg
      mutate(scaler = EDGAR_emissions / EPA_emissions / 1000.0)

    # Scale EPA emissions
    L111.nonghg_tg_R_en_Si_F_Yh <- L111.nonghg_tg_R_en_Si_F_Yh %>%
      # Match in scalers
      left_join(L111.emiss_scaler %>% select(-EDGAR_emissions, -EPA_emissions), by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      # Computed scaled EPA emissions
      mutate(emissions = epa_emissions * scaler) %>%
      replace_na(list(emissions = 0, scaler = 0))

    # Separate domestic emissions - to be combined later
    L111.nonghg_tg_R_en_Si_F_Yh_dom <- L111.nonghg_tg_R_en_Si_F_Yh %>%
      filter(!EDGAR_agg_sector %in% emissions.TRN_INTL_SECTORS)

    # Compute international shipping and international aviation emissions
    # These are provided in the EDGAR inventory at the global level only
    # Use energy data to downscale the global total to regional data

    # Separate international emission data
    L111.nonghg_tg_R_en_Si_F_Yh_intl <- L111.nonghg_tg_R_en_Si_F_Yh %>%
      filter(EDGAR_agg_sector %in% emissions.TRN_INTL_SECTORS)

    # Downscale global total into regions
    # First, calculate total energy use in international shipping and aviation
    L111.nonghg_tg_R_en_Si_F_Yh <- L111.nonghg_tg_R_en_Si_F_Yh_intl %>%
      group_by(EDGAR_agg_sector, Non.CO2, year) %>%
      summarise(tot_energy = sum(energy)) %>%
      ungroup %>%
      # Combine total energy with energy use by region
      right_join(L111.nonghg_tg_R_en_Si_F_Yh_intl, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      # Match in EDGAR international shipping and international aviation emissions
      left_join(L111.EDGAR_intl, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      # Convert EDGAR emissions in Gg to Tg
      mutate(tot_emiss = tot_emiss / 1000.0,
             energy_share = energy / tot_energy,
             # Downscale global emissions based on energy use
             emissions = tot_emiss * energy_share) %>%
      # Remove unnecessary columns and rows
      select(-tot_energy, -tot_emiss, -energy_share) %>%
      na.omit %>%
      # Recombine domestic emission data
      bind_rows(L111.nonghg_tg_R_en_Si_F_Yh_dom)

    # Map in final GCAM sector, technology, and driver type
    L111.nonghg_tg_R_en_S_F_Yh <- L111.nonghg_tg_R_en_Si_F_Yh %>%
      left_join_keep_first_only(GCAM_sector_tech %>% select(sector, fuel, technology, supplysector, subsector, stub.technology) %>% unique,
                                by = c("sector", "fuel", "technology")) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year) %>%
      # Calculate total input emissions
      summarise(value = sum(emissions)) %>%
      ungroup %>%
      # Remove Non-EDGAR years
      filter(year %in% emissions.EDGAR_HISTORICAL)

    # Compute emissions factor by GCAM sector, technology, and driver type
    # First compute energy by sector
    L111.in_EJ_R_en_S_F_Yh <- L101.in_EJ_R_en_Si_F_Yh %>%
      gather(year, energy, -GCAM_region_ID, -sector, -fuel, -technology) %>%
      mutate(year = as.integer(year)) %>%
      left_join_keep_first_only(GCAM_sector_tech %>% select(sector, fuel, technology, supplysector, subsector, stub.technology) %>% unique,
                                by = c("sector", "fuel", "technology")) %>%
      na.omit %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, year) %>%
      summarise(energy = sum(energy)) %>%
      ungroup

    # Match input emission data with energy data
    L111.nonghg_tgej_R_en_S_F_Yh <- L111.nonghg_tg_R_en_S_F_Yh %>%
      left_join_error_no_match(L111.in_EJ_R_en_S_F_Yh, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
      # Calculate emission factors
      mutate(value = value / energy) %>%
      replace_na(list(value = 0)) %>%
      select(-energy)

    # Produce outputs

    L111.nonghg_tg_R_en_S_F_Yh %>%
      add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg") %>%
      add_comments("Compute unscaled non-ghg emissions by country and technology, and EDGAR emissions by region and sector.") %>%
      add_comments("Then, scale EPA emissions by tech to match EDGAR totals, compute international shipping and international aviation emissions, ") %>%
      add_comments("and finally calculate non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector",
                     "emissions/EDGAR/EDGAR_nation",
                     "emissions/mappings/EPA_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "L101.in_EJ_R_en_Si_F_Yh",
                     "L101.so2_tgej_USA_en_Sepa_F_Yh",
                     "L101.co_tgej_USA_en_Sepa_F_Yh",
                     "L101.nox_tgej_USA_en_Sepa_F_Yh",
                     "L101.voc_tgej_USA_en_Sepa_F_Yh",
                     "L101.nh3_tgej_USA_en_Sepa_F_Yh",
                     "emissions/EDGAR/EDGAR_SO2",
                     "emissions/EDGAR/EDGAR_CO",
                     "emissions/EDGAR/EDGAR_NOx",
                     "emissions/EDGAR/EDGAR_NMVOC",
                     "emissions/EDGAR/EDGAR_NH3") ->
      L111.nonghg_tg_R_en_S_F_Yh

    L111.nonghg_tgej_R_en_S_F_Yh %>%
      add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg/EJ") %>%
      add_comments("Use non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years to derive emission shares.") %>%
      add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
      same_precursors_as(L111.nonghg_tg_R_en_S_F_Yh) ->
      L111.nonghg_tgej_R_en_S_F_Yh

    return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh)
  } else {
    stop("Unknown command")
  }
}
