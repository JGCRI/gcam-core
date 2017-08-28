#' module_emissions_L111.nonghg_en_R_S_T_Y
#'
#' This code produces non-ghg emission totals and non-ghg emission shares by total emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.nonghg_tg_R_en_S_F_Yh}, \code{L111.nonghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L111.nonghg_en_R_S_T_Y.R} (emissions level1).
#' @details This code produces two outputs: non-ghg emission totals and non-ghg emission shares by total emissions.
#' First, non-ghg gas emissions are combined and grouped by sector and region, and then emissions are scaled, and international
#' shipping & aviation emission data calculated based on total emission and total emission shares. Finally, non-ghg emission
#' totals and shares are calculated by GCAM sector, fuel, technology, and driver type for EDGAR historical years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL August 2017
#' @export
module_emissions_L111.nonghg_en_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/mappings/EPA_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
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

    sector <- fuel <- service <- IPCC_Annex <- World_Region <- ISO_A3 <- Name <- IPCC <-
      IPCC_description <- Gas <- agg_sector <- EDGAR_agg_sector <- Sector <- supplysector <-
      subsector <- stub.technology <- EDGAR_emissions <- region <- emissions <- year <-
      iso <- awb <- Non.CO2 <- GCAM_region_ID <- EPA_Category <- technology <- value <-
      EPA_agg_sector <- EPA_agg_fuel <- EPA_agg_fuel_ghg <- RCP_agg_sector <- BCOC_agg_sector <-
      BCOC_agg_fuel <- EPA_MACC_Sector <- IIASA_sector <- EPA_sector <- MAC_type1 <- Gas <-
      HFC_PFC <- GWP <- country_name <- region_GCAM3 <- . <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L101.co_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.co_tgej_USA_en_Sepa_F_Yh")
    L101.so2_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.so2_tgej_USA_en_Sepa_F_Yh")
    L101.nox_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nox_tgej_USA_en_Sepa_F_Yh")
    L101.voc_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.voc_tgej_USA_en_Sepa_F_Yh")
    L101.nh3_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nh3_tgej_USA_en_Sepa_F_Yh")

    get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
      gather(year, value,  matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) ->
      L101.in_EJ_R_en_Si_F_Yh

    EDGAR_SO2 <- get_data(all_data, "emissions/EDGAR/EDGAR_SO2")
    EDGAR_CO <- get_data(all_data, "emissions/EDGAR/EDGAR_CO")
    EDGAR_NOx <- get_data(all_data, "emissions/EDGAR/EDGAR_NOx")
    EDGAR_NMVOC <- get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3")

    # First, add gas name and bind all dataframes together.

    L101.so2_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "SO2"
    L101.co_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "CO"
    L101.nox_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NOx"
    L101.voc_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NMVOC"

    L101.nh3_tgej_USA_en_Sepa_F_Yh %>%
      mutate(Non.CO2 = "NH3") %>%
      bind_rows(L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh,
                L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh) %>%
      # Post-2002 values all have the value "1".  Change post-2002 values to 2002 values.
      # Spread data first for ease in replacing values.
      spread(year, value) %>%
      mutate(`2003` = `2002`,
             `2004` = `2002`,
             `2005` = `2002`,
             `2006` = `2002`,
             `2007` = `2002`,
             `2008` = `2002`) %>%
      # Reformat into long form.
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      # Rename columns to match other tables.
      rename(EPA_agg_sector = sector, EPA_agg_fuel = fuel) ->
      L111.nonghg_tgej_USA_en_Sepa_F_Yh.mlt

    # Compute unscaled emissions by country and technology.

    # Add non-ghg gases for combination of sector, fuel, and technology.
    L101.in_EJ_R_en_Si_F_Yh %>%
      # Rename columns for joining.
      rename(energy = value) %>%
      left_join_keep_first_only(select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel),
                                by = c("sector", "fuel", "technology")) %>%
      repeat_add_columns(tibble(Non.CO2 = emissions.NONGHG_GASES)) %>%
      # Match in emissions factors and then compute unscaled emissions
      # Aggregate by EDGAR sector and region
      # Join in USA non-ghg emission factor data; keep NAs for now
      left_join(L111.nonghg_tgej_USA_en_Sepa_F_Yh.mlt, by = c("Non.CO2", "EPA_agg_sector", "EPA_agg_fuel", "year"))  %>%
      rename(emfact = value)  %>%
      # Scale emission factors by energy sectors
      mutate(epa_emissions = energy * emfact) %>%
      na.omit() %>%
      left_join_keep_first_only(select(GCAM_sector_tech, EDGAR_agg_sector, sector, fuel),
                                by = c("sector", "fuel")) ->
      L111.nonghg_tg_R_en_Si_F_Yh.mlt

    # Create column of total EPA emissions by EDGAR sector and region.
    L111.nonghg_tg_R_en_Si_F_Yh.mlt %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(EPA_emissions = sum(epa_emissions)) %>%
      ungroup ->
      L111.nonghg_tg_R_en_Sedgar_Yh.mlt

    # Compute EDGAR emissions by region and sector.

    # Add gas name and bind all dataframes together.
    EDGAR_SO2$Non.CO2 <- "SO2"
    EDGAR_CO$Non.CO2  <- "CO"
    EDGAR_NOx$Non.CO2 <- "NOx"
    EDGAR_NH3$Non.CO2 <- "NH3"

    EDGAR_NMVOC %>%
      mutate(Non.CO2 = "NMVOC") %>%
      bind_rows(EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_NH3) %>%
      select(-`2009`, -`2010`) %>%
      left_join(EDGAR_sector, by = c("IPCC", "IPCC_description")) %>%
      rename(IPCC_Annex = `IPCC-Annex`, World_Region = `World Region`, EDGAR_agg_sector = agg_sector) %>%
      # Convert from EDGAR iso to GCAM_region_ID, and change Romanian iso code.
      standardize_iso(col = "ISO_A3", delete_original = FALSE) %>%
      change_iso_code('rou', 'rom') %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # In the old data system, the EDGAR_nation file was missing three ISO_A3 values: ATA, SEA, and AIR
      # as a result, when we matched `ISO_A3` to `iso`, the latter was NA for these three
      # Here we replicate this behavior -BBL
      mutate(iso = if_else(iso %in% c("ata", "sea", "air"), NA_character_, iso)) ->
      L111.EDGAR

    # Create new table for international shipping & aviation emissions only and drop unnecessary columns.
    L111.EDGAR %>%
      filter(ISO_A3 %in% c("SEA", "AIR")) %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      select(year, value, Non.CO2, EDGAR_agg_sector) %>%
      na.omit() ->
      L111_EDGAR_intl

    # Drop unnecessary columns, aggregate by region, and group by region, sector, NONGHG, and year.
    L111.EDGAR %>%
      select(-IPCC_Annex, -World_Region, -iso, -Name, -IPCC, -IPCC_description) %>%
      na.omit %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% emissions.EDGAR_YEARS) %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L111.EDGAR.mlt

    # Scale EPA emissions by tech to match EDGAR totals.

    # First, compute scalers.
    L111.nonghg_tg_R_en_Sedgar_Yh.mlt %>%
      left_join(L111.EDGAR.mlt, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year"))  %>%
      rename(EDGAR_emissions = value) %>%
      # EDGAR emissions are in Gg, EPA emissions are in Tg, so convert EDGAR to Tg first for unitless scaler.
      mutate(scaler = (EDGAR_emissions * CONV_GG_TG) / EPA_emissions) ->
      L111.emiss_scaler

    # Then, scale EPA emissions.
    L111.nonghg_tg_R_en_Si_F_Yh.mlt %>%
      left_join(select(L111.emiss_scaler, GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year, scaler),
                by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      mutate(emissions = epa_emissions * scaler) %>%
      replace_na(replace = list(emissions = 0, scaler = 0)) ->
      L111.nonghg_tg_R_en_Sedgar_Yh.mlt

    # Compute international shipping and international aviation emissions.

    # These are provided in the EDGAR inventory at the global level only.
    L111.nonghg_tg_R_en_Sedgar_Yh.mlt %>%
      filter(EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air")) ->
      L111.nonghg_tg_R_en_Si_F_Yh_intl.mlt

    L111.nonghg_tg_R_en_Sedgar_Yh.mlt %>%
      filter(!(EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air"))) ->
      L111.nonghg_tg_R_en_Si_F_Yh_dom.mlt

    # Calculate total energy for international aviation emissions.
    L111.nonghg_tg_R_en_Si_F_Yh_intl.mlt %>%
      group_by(EDGAR_agg_sector, Non.CO2, year) %>%
      summarise(energy = sum(energy)) %>%
      ungroup ->
      L111.en_Si_F_Yh_intl.mlt

    # Compute international input emissions based on total emission and total emission shares.
    L111.nonghg_tg_R_en_Si_F_Yh_intl.mlt %>%
      left_join(L111.en_Si_F_Yh_intl.mlt, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      rename(tot_energy = energy.y, energy = energy.x) %>%
      left_join(L111_EDGAR_intl, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      # EDGAR emissions are in Gg, need to convert to Tg.
      mutate(emissions = (value * CONV_GG_TG) * (energy / tot_energy)) %>%
      select(-tot_energy, -value) %>%
      na.omit()  %>%
      # Add domestic emissions back in.
      bind_rows(L111.nonghg_tg_R_en_Si_F_Yh_dom.mlt) ->
      L111.nonghg_tg_R_en_Si_F_Yh.mlt

    # Compute non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.

    L111.nonghg_tg_R_en_Si_F_Yh.mlt %>%
      left_join_keep_first_only(GCAM_sector_tech, by = c("sector", "fuel", "technology")) %>%
      # Sum emissions by region, non-CO2, sector, technology, and year.
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year) %>%
      summarise(value = sum(emissions)) %>%
      ungroup %>%
      filter(year %in% emissions.EDGAR_YEARS) ->
      L111.nonghg_tg_R_en_S_F_Yh

    # Compute non-ghg emission factors for GCAM sector, fuel, technology, and driver type for EDGAR historical years.

    # Calculate energy totals by region, sector, and fuel.
    L101.in_EJ_R_en_Si_F_Yh %>%
      left_join_keep_first_only(select(GCAM_sector_tech, sector, fuel, technology, supplysector, subsector, stub.technology),
                                by = c("sector", "fuel", "technology")) %>%
      # need to filter here to replicate behavior of original aggregate()
      filter(!is.na(supplysector), !is.na(subsector), !is.na(stub.technology)) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, year) %>%
      summarise(energy = sum(value)) ->
      L101.in_EJ_R_en_S_F_Yh.mlt

    # Calculate emission shares by energy totals.

    L111.nonghg_tg_R_en_S_F_Yh %>%
      left_join(L101.in_EJ_R_en_S_F_Yh.mlt, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(value = value / energy) %>%
      select(-energy) %>%
      replace_na(list(value = 0)) ->
      L111.nonghg_tgej_R_en_S_F_Yh


    # Produce outputs

    L111.nonghg_tg_R_en_S_F_Yh %>%
      add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg") %>%
      add_comments("First, compute unscaled non-ghg emissions by country and technology.") %>%
      add_comments("Then, compute EDGAR emissions by region and sector.") %>%
      add_comments("Then, scale EPA emissions by tech to match EDGAR totals.") %>%
      add_comments("Then, compute international shipping and international aviation emissions.") %>%
      add_comments("Finally, calculate non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector",
                     "emissions/mappings/EPA_tech",
                     "emissions/mappings/GCAM_sector_tech",
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
                     "emissions/EDGAR/EDGAR_NH3") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L111.nonghg_tg_R_en_S_F_Yh

    L111.nonghg_tgej_R_en_S_F_Yh %>%
      add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg/EJ") %>%
      add_comments("Use non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years to derive emission shares.") %>%
      add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
      same_precursors_as(L111.nonghg_tg_R_en_S_F_Yh) ->
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.nonghg_tgej_R_en_S_F_Yh

    return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh)
  } else {
    stop("Unknown command")
  }
}
