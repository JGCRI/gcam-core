#' module_emissions_L111.nonghg_en_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.nonghg_tg_R_en_S_F_Yh}, \code{L111.nonghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L111.nonghg_en_R_S_T_Y.R} (emissions level1).
#' @details This code produces two outputs: non-ghg emission totals and non-ghg emission shares by total emissions.
#' First, non-ghg gas emissions were combined and grouped by sector and region.  Then, emissions were scaled, and international
#' shipping & aviation emission data was calculated based on total emission and total emission shares. Then non-ghg emission
#' totals and shares were calculated by GCAM sector, fuel, technology, and driver type for EDGAR historical years.
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

    sector <- fuel <- service <- IPCC_Annex <- World_Region <- ISO_A3 <- Name <- IPCC <- IPCC_description <-
      Gas <- agg_sector <- EDGAR_agg_sector <- Sector <- supplysector <- subsector <- stub.technology <-
      EDGAR_emissions <- region <- emissions <- year <- iso <- awb <- Non.CO2 <- GCAM_region_ID <-
      EPA_Category <- technology <- value <- EPA_agg_sector <- EPA_agg_fuel <- EPA_agg_fuel_ghg <-
      RCP_agg_sector <- BCOC_agg_sector <- BCOC_agg_fuel <- EPA_MACC_Sector <- IIASA_sector <-
      EPA_sector <- MAC_type1 <- Gas <- HFC_PFC <- GWP <- country_name <- region_GCAM3 <-. <-
      NULL # silence package check.

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
      gather(year, value, -GCAM_region_ID, -fuel, -technology, -sector) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      L101.in_EJ_R_en_Si_F_Yh
    get_data(all_data, "emissions/EDGAR/EDGAR_SO2") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_SO2
    get_data(all_data, "emissions/EDGAR/EDGAR_CO") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_CO
    get_data(all_data, "emissions/EDGAR/EDGAR_NOx") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NOx
    get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NMVOC
    get_data(all_data, "emissions/EDGAR/EDGAR_NH3") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NH3

    # First, add gas name and bind all dataframes together.

    L101.so2_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "SO2"
    L101.co_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "CO"
    L101.nox_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NOx"
    L101.voc_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NMVOC"

    L101.nh3_tgej_USA_en_Sepa_F_Yh %>%
      mutate(Non.CO2 = "NH3") %>%
      bind_rows(L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh,
                L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh, .) %>%
      group_by(Non.CO2, sector, fuel) %>%
      # Post-2002 values all have the value "1".  Change post-2002 values to 2002 values.
      # Spread data first for ease in replacing values.
      spread(year,value) %>%
      mutate(`2003` = replace(`2003`, `2003` == 1, `2002`),
             `2004` = replace(`2004`, `2004` == 1, `2002`),
             `2005` = replace(`2005`, `2005` == 1, `2002`),
             `2006` = replace(`2006`, `2006` == 1, `2002`),
             `2007` = replace(`2007`, `2007` == 1, `2002`),
             `2008` = replace(`2008`, `2008` == 1, `2002`)) %>%
      # Reformat into long form.
      gather(year, value, -sector, -fuel, -Non.CO2) %>%
      # Convert to integer (or numeric) to extrapolate
      mutate(year = as.integer(year)) %>%
      # Rename columns to match other tables.
      rename(EPA_agg_sector = sector) %>%
      rename(EPA_agg_fuel = fuel) ->
      L111.nonghg_tgej_USA_en_Sepa_F_Yh.tmp1

    # Compute unscaled emissions by country and technology.

    # Add non-ghg gases for combination of sector, fuel, and technology.
    L101.in_EJ_R_en_Si_F_Yh %>%
      # Rename columns for joining.
      rename(energy = value) %>%
      left_join_keep_first_only(select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel),
                                by =c("sector", "fuel", "technology"))  %>%
      repeat_add_columns(tibble(Non.CO2 = emissions.NONGHG_GASES)) ->
      L111.nonghg_tg_R_en_Si_F_Yh.tmp1

    # Match in emissions factors and then compute unscaled emissions.

    # Aggregate by EDGAR sector and region
    L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
      # Join in USA non-ghg emission factor data.  Keep NAs for now.
      left_join(L111.nonghg_tgej_USA_en_Sepa_F_Yh.tmp1, by = c("Non.CO2", "EPA_agg_sector", "EPA_agg_fuel", "year"))  %>%
      rename(emfact = value)  %>%
      # Scale emission factors by energy sectors.
      mutate(epa_emissions = energy * emfact) %>%
      na.omit() %>%
      left_join_keep_first_only(select(GCAM_sector_tech, EDGAR_agg_sector, sector, fuel),
                                by =c("sector", "fuel")) ->
      L111.nonghg_tg_R_en_Si_F_Yh.tmp1

    # Create column of total EPA emissions by EDGAR sector and region
    L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
      group_by(GCAM_region_ID,Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(EPA_emissions = sum(epa_emissions)) ->
      L111.nonghg_tg_R_en_Sedgar_Yh.tmp1

    # Compute EDGAR emissions by region and sector.

    # Add gas name and bind all dataframes together.
    EDGAR_SO2$Non.CO2 <- "SO2"
    EDGAR_CO$Non.CO2 <- "CO"
    EDGAR_NOx$Non.CO2 <- "NOx"
    EDGAR_NH3$Non.CO2 <- "NH3"

    EDGAR_NMVOC %>%
      mutate(Non.CO2 = "NMVOC") %>%
      bind_rows(EDGAR_SO2, EDGAR_CO, EDGAR_NOx, ., EDGAR_NH3) %>%
      # Filter for EDGAR years (1971-2008) and 1970.
      filter(year %in% emissions.EDGAR_YEARS_PLUS) %>%
      left_join(EDGAR_sector, by = c("IPCC", "IPCC_description"))  %>%
      rename(IPCC_Annex = `IPCC-Annex`, World_Region = `World Region`, EDGAR_agg_sector = agg_sector) %>%
      # Convert from EDGAR iso to GCAM_region_ID, and change Romanian iso code.
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") ->
      L111.EDGAR

    # Create new table for international shipping & aviation emissions only and drop unnecessary columns.
    L111.EDGAR %>%
      filter(iso %in% c("sea", "air")) %>%
      select(year, value, Non.CO2, EDGAR_agg_sector) %>%
      na.omit() ->
      L111_EDGAR_intl

    # Drop unnecessary columns, aggregate by region, and group by region, sector, NONGHG, and year.
    L111.EDGAR %>%
      select(year, value, Non.CO2, EDGAR_agg_sector, GCAM_region_ID) %>%
      na.omit() %>%
      filter(year %in% emissions.EDGAR_YEARS)  %>%
      group_by(GCAM_region_ID, Non.CO2, EDGAR_agg_sector, year) %>%
      summarize(value = sum(value))  ->
      L111.EDGAR.tmp1

    # Scale EPA emissions by tech to match EDGAR totals.

    # First, compute scalers.
    L111.nonghg_tg_R_en_Sedgar_Yh.tmp1 %>%
      left_join(L111.EDGAR.tmp1, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year"))  %>%
      rename(EDGAR_emissions = value)  %>%
      # EDGAR emissions are in Gg, EPA emissions are in Tg, so convert EDGAR to Tg first for unitless scaler.
      mutate(scaler = (EDGAR_emissions * CONV_GG_TG) / EPA_emissions) ->
      L111.emiss_scaler

    # Then, scale EPA emissions.
    L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
      left_join(L111.emiss_scaler, by=c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year")) %>%
      mutate(emissions = epa_emissions * scaler)  %>%
      replace_na(replace = list(EDGAR_emissions = 0, emissions = 0, scaler = 0))  %>%
      select(-EDGAR_emissions, -EPA_emissions) ->
      L111.nonghg_tg_R_en_Si_F_Yh_all.tmp1

    # Compute international shipping and international aviation emissions.

    # These are provided in the EDGAR inventory at the global level only.
    L111.nonghg_tg_R_en_Si_F_Yh_all.tmp1 %>%
      filter(EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air")) ->
      L111.nonghg_tg_R_en_Si_F_Yh_intl.tmp1

    L111.nonghg_tg_R_en_Si_F_Yh_all.tmp1 %>%
      filter(!(EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air"))) ->
      L111.nonghg_tg_R_en_Si_F_Yh_dom.tmp1

    # Calculate total energy for international aviation emissions.
    L111.nonghg_tg_R_en_Si_F_Yh_intl.tmp1 %>%
      group_by(EDGAR_agg_sector, Non.CO2, year) %>%
      summarize(energy = sum(energy))->
      L111.en_Si_F_Yh_intl.tmp1

    # Compute international input emissions based on total emission and total emission shares.
    L111.nonghg_tg_R_en_Si_F_Yh_intl.tmp1 %>%
      left_join(L111.en_Si_F_Yh_intl.tmp1, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      rename(tot_energy = energy.y, energy = energy.x) %>%
      left_join(L111_EDGAR_intl, by = c("EDGAR_agg_sector", "Non.CO2", "year")) %>%
      # EDGAR emissions are in Gg, need to convert to Tg.
      mutate(emissions = (value * CONV_GG_TG) * (energy / tot_energy))  %>%
      # Remove unnecessary columns and rows.
      select(-tot_energy, -value) %>%
      na.omit()  %>%
      # Add domestic domestic emissions back in.
      bind_rows(., L111.nonghg_tg_R_en_Si_F_Yh_dom.tmp1) ->
      L111.nonghg_tg_R_en_Si_F_Yh.tmp2

    # Compute non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.

    L111.nonghg_tg_R_en_Si_F_Yh.tmp2 %>%
      left_join_keep_first_only(GCAM_sector_tech, by = c("sector", "fuel", "technology")) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year) %>%
      # Sum emissions by region, non-CO2, sector, technology, and year.
      summarize(value = sum(emissions)) %>%
      filter(year %in% emissions.EDGAR_YEARS) ->
      L111.nonghg_tg_R_en_S_F_Yh

    # Compute non-ghg emission factors for GCAM sector, fuel, technology, and driver type for EDGAR historical years.

    # Calculate energy totals by region, sector, and fuel.
    L101.in_EJ_R_en_Si_F_Yh %>%
      left_join(GCAM_sector_tech, by = c("sector", "fuel", "technology")) %>%
      # Remove unneeded columns.
      select(GCAM_region_ID, sector, fuel, technology, year, value, supplysector, subsector, stub.technology) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, year) %>%
      summarize(energy = sum(value)) ->
      L101.in_EJ_R_en_S_F_Yh.tmp

    # Calculate emission shares by energy totals.
    L111.nonghg_tg_R_en_S_F_Yh %>%
      left_join(L101.in_EJ_R_en_S_F_Yh.tmp, by=c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(emfact = value / energy) %>%
      # Remove unneeded columns, and rename columns to match old.
      select(-value, -energy) %>%
      rename(value = emfact) %>%
      replace_na(list(value = 0)) ->
      L111.nonghg_tgej_R_en_S_F_Yh

    # Produce outputs

    L111.nonghg_tg_R_en_S_F_Yh %>%
      add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg") %>%
      add_comments("First, computed unscaled non-ghg emissions by country and technology.") %>%
      add_comments("Then, computed EDGAR emissions by region and sector.") %>%
      add_comments("Then, scaled EPA emissions by tech to match EDGAR totals.") %>%
      add_comments("Then, computed international shipping and international aviation emissions.") %>%
      add_comments("Finally, calculated non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.nonghg_tg_R_en_S_F_Yh

    L111.nonghg_tgej_R_en_S_F_Yh %>%
      add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
      add_units("Tg/EJ") %>%
      add_comments("Used non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years to derive emission shares.") %>%
      add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
      same_precursors_as("L111.nonghg_tg_R_en_S_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.nonghg_tgej_R_en_S_F_Yh

    return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh)
  } else {
    stop("Unknown command")
  }
}
