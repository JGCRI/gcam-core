# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L141.hfc_R_S_T_Y
#'
#' Calculate HFC emissions from EDGAR, by residential and commercial cooling shares,
#' adjusted to match the Guus Velders HFC inventory.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.hfc_R_S_T_Yh}, \code{L141.hfc_ef_R_cooling_Yh}. The corresponding file in the
#' original data system was \code{L141.hfc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join matches mutate select summarise vars
#' @importFrom tidyr replace_na
#' @author RMH Aug 2017
module_emissions_L141.hfc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/gcam_fgas_tech",
             FILE = "emissions/other_f_gases",
             "L144.in_EJ_R_bld_serv_F_Yh",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EPA_country_map",
             FILE = "emissions/EPA/EPA_HCFC22",
             FILE = "emissions/EPA/EPA_ODSS_Aerosols",
             FILE = "emissions/EPA/EPA_ODSS_FireExt",
             FILE = "emissions/EPA/EPA_ODSS_Foams",
             FILE = "emissions/EPA/EPA_ODSS_RefAC",
             FILE = "emissions/EPA/EPA_ODSS_Solvents",
             FILE = "emissions/EPA/EPA_Semi_HFCs",
             FILE = "emissions/EPA/EPA_FPD_HFCs",
             FILE = "emissions/EPA_fgas_sector_map",
             FILE = "emissions/EPA_GWPs",
             FILE = "emissions/EDGAR/EDGAR_sector_fgas",
             FILE = "emissions/EDGAR/EDGAR_HFC125",
             FILE = "emissions/EDGAR/EDGAR_HFC134a",
             FILE = "emissions/EDGAR/EDGAR_HFC143a",
             FILE = "emissions/EDGAR/EDGAR_HFC152a",
             FILE = "emissions/EDGAR/EDGAR_HFC227ea",
             FILE = "emissions/EDGAR/EDGAR_HFC23",
             FILE = "emissions/EDGAR/EDGAR_HFC236fa",
             FILE = "emissions/EDGAR/EDGAR_HFC245fa",
             FILE = "emissions/EDGAR/EDGAR_HFC32",
             FILE = "emissions/EDGAR/EDGAR_HFC365mfc",
             FILE = "emissions/EDGAR/EDGAR_HFC43"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.hfc_R_S_T_Yh",
             "L141.hfc_ef_R_cooling_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    #silence packages
    emiss_share <- supply <- emscalar <- tot_emissions <- MAC_type1 <- gwp <- EPA_emissions <- EPA_sector <- NULL


    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    gcam_fgas_tech <- get_data(all_data, "emissions/gcam_fgas_tech", strip_attributes = TRUE)
    other_f_gases <- get_data(all_data, "emissions/other_f_gases")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EPA_HCFC22 <- get_data(all_data, "emissions/EPA/EPA_HCFC22")
    EPA_ODSS_Aerosols <- get_data(all_data, "emissions/EPA/EPA_ODSS_Aerosols")
    EPA_ODSS_FireExt <- get_data(all_data, "emissions/EPA/EPA_ODSS_FireExt")
    EPA_ODSS_Foams <- get_data(all_data, "emissions/EPA/EPA_ODSS_Foams")
    EPA_ODSS_RefAC <- get_data(all_data, "emissions/EPA/EPA_ODSS_RefAC")
    EPA_ODSS_Solvents <- get_data(all_data, "emissions/EPA/EPA_ODSS_Solvents")
    EPA_Semi_HFCs <- get_data(all_data, "emissions/EPA/EPA_Semi_HFCs")
    EPA_FPD_HFCs <- get_data(all_data, "emissions/EPA/EPA_FPD_HFCs")
    EPA_fgas_sector_map <- get_data(all_data, "emissions/EPA_fgas_sector_map")
    EPA_country_map <- get_data(all_data, "emissions/EPA_country_map")
    EPA_GWPs <- get_data(all_data, "emissions/EPA_GWPs")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector_fgas")
    EDGAR_HFC125 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC125")
    EDGAR_HFC134a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC134a")
    EDGAR_HFC143a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC143a")
    EDGAR_HFC152a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC152a")
    EDGAR_HFC227ea <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC227ea")
    EDGAR_HFC23 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC23")
    EDGAR_HFC236fa <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC236fa")
    EDGAR_HFC245fa <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC245fa")
    EDGAR_HFC32 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC32")
    EDGAR_HFC365mfc <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC365mfc")
    EDGAR_HFC43 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC43")

    . <- year <- value <- GCAM_region_ID <- sector <- fuel <- service <-
      IPCC_description <- agg_sector <- ISO_A3 <- iso <- EDGAR_agg_sector <-
      Non.CO2 <- emissions <- Sector <- total <- share <- Species <- Emissions <-
      SSP2_tot <- EDGAR_tot <- scaler <- supplysector <- subsector <- stub.technology <-
      adj_emissions <- energy <- em_fact <- NULL  # silence package check notes


    # Processing and Mapping EDGAR HFC emissions to GCAM technologies
    # ===============================================================

    # Add column with Non.CO2 gas name
    EDGAR_HFC125$Non.CO2 <- "HFC125"
    EDGAR_HFC134a$Non.CO2 <- "HFC134a"
    EDGAR_HFC143a$Non.CO2 <- "HFC143a"
    EDGAR_HFC152a$Non.CO2 <- "HFC152a"
    EDGAR_HFC227ea$Non.CO2 <- "HFC227ea"
    EDGAR_HFC23$Non.CO2 <- "HFC23"
    EDGAR_HFC236fa$Non.CO2 <- "HFC236fa"
    EDGAR_HFC245fa$Non.CO2 <- "HFC245fa"
    EDGAR_HFC32$Non.CO2 <- "HFC32"
    EDGAR_HFC365mfc$Non.CO2 <- "HFC365mfc"
    EDGAR_HFC43$Non.CO2 <- "HFC43"

    # Process all F gasses files then combine and aggregate
    # define function to format and process F gases, map to GCAM regions
    format_F_Gas <- function(x) {
      x %>%
        left_join_error_no_match(EDGAR_sector %>% select(-IPCC_description), by = "IPCC") %>% # Add Edgar agg_sector
        rename(EDGAR_agg_sector = agg_sector) %>% # rename agg_sector to EDGAR_agg_sector
        mutate(iso = tolower(ISO_A3), ISO_A3 = NULL) %>% # convert to Edgar ISO
        change_iso_code('rou', 'rom') %>% # Convert Romania iso code to pre-2002 value
        left_join_error_no_match(iso_GCAM_regID, by = "iso") %>% # Map iso to GCAM region
        select(GCAM_region_ID, iso, EDGAR_agg_sector, Non.CO2, matches(YEAR_PATTERN)) %>%
        gather_years(value_col = "emissions") %>%
        mutate(emissions = as.numeric(emissions))
    }
    # combine all F gasses files into list
    F_gases <- list(EDGAR_HFC125, EDGAR_HFC134a, EDGAR_HFC143a,
                    EDGAR_HFC152a, EDGAR_HFC227ea, EDGAR_HFC23,
                    EDGAR_HFC236fa, EDGAR_HFC245fa, EDGAR_HFC32,
                    EDGAR_HFC365mfc, EDGAR_HFC43)

    # Use function to convert datat to numeric values and combine to one tibble
    # Map to GCAM countries and technologies
    F_gases_formatted <- lapply(F_gases, format_F_Gas)

    L141.EDGAR_HFC <- bind_rows(F_gases_formatted) %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2,  year) %>%
      summarise(emissions = sum(emissions))

    # Map to other f-gas sector, which varies by gas
    L141.EDGAR_hfc_R_S_T_Yh_rest <- L141.EDGAR_HFC %>%
      filter(EDGAR_agg_sector != "other_f_gases")
    L141.EDGAR_hfc_R_S_T_Yh_other <- L141.EDGAR_HFC %>%
      filter(EDGAR_agg_sector == "other_f_gases") %>%
      ungroup() %>%
      select(-EDGAR_agg_sector) %>%
      left_join_error_no_match(other_f_gases, by = c("Non.CO2" = "Gas")) %>%
      rename(EDGAR_agg_sector=Sector)
    L141.EDGAR_hfc_R_S_T_Yh.long <- bind_rows(L141.EDGAR_hfc_R_S_T_Yh_rest,
                                              L141.EDGAR_hfc_R_S_T_Yh_other) %>%
      mutate(year = as.numeric(year))

    # Map Emissions to GCAM technologies
    L141.hfc_R_S_T_Yh.long <- gcam_fgas_tech %>%
      repeat_add_columns(tibble(GCAM_region_ID = unique(GCAM_region_names$GCAM_region_ID))) %>%
      repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L141.EDGAR_hfc_R_S_T_Yh.long$Non.CO2))) %>%
      left_join(L141.EDGAR_hfc_R_S_T_Yh.long,
                by = c("EDGAR_agg_sector", "GCAM_region_ID", "year", "Non.CO2")) %>% # there should be NAs as some sectors have no emissions
      replace_na(list(emissions = 0)) # replace NAs with zero

    # Calculates shares of IEA cooling energy balances that go to residential and commercial
    # ======================================================================================
    # Select residential and cooling emisssions from L144.in_EJ_R_bld_serv_F_Yh
    L141.R_cooling_T_Yh.long <- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(service %in% c("comm cooling", "resid cooling"), fuel == "electricity")
    # Group by GCAM region and ID and year in new data frame (use to calculate share of total later)
    L141.R_cooling_Yh <-  L141.R_cooling_T_Yh.long %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(total = sum(value))
    # Join in the totals we just calculated and calulate the shares
    L141.R_cooling_T_Yh.long <- left_join_error_no_match(L141.R_cooling_T_Yh.long,L141.R_cooling_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(share = value / total) %>%
      select(GCAM_region_ID, year, service, share, value)

    # Add res/com cooling share to HFC and make adjustment
    L141.hfc_R_S_T_Yh.long %>%
      left_join(L141.R_cooling_T_Yh.long, by = c("GCAM_region_ID", "year", "supplysector" = "service")) %>% # there should be NAs.
      # there are sectors that don't have emissions
      replace_na(list(share = 1)) %>% # replace those shares with "1"
      mutate(emissions = emissions * share) %>%
      select(-share, -value) ->
      L141.hfc_R_S_T_Yh_coolshare

    # make sure EDGAR have data till 2015 for EPA BAU calibration
    # remaing_years is the missing base years that EPA has but EDGAR does not
    # Duplicate EDGAR last-year data and rename as missing years just as placeholders, which will be scaled to EPA later
    # Note: here additional years are for all missing years (2009, 2010, 2011 etc), not just GCAM modeling years
    # So we can deal with base-year time shifting
    additional_years <-
      HISTORICAL_YEARS[HISTORICAL_YEARS > emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]]

    if(length(additional_years) > 0){
      TEMP <- filter(L141.hfc_R_S_T_Yh_coolshare, year == emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]) %>%
        select(-year) %>%
        repeat_add_columns(tibble(year = additional_years))

      L141.hfc_R_S_T_Yh_coolshare <- bind_rows(L141.hfc_R_S_T_Yh_coolshare, TEMP)
    }


    # Prepare EPA data for matching
    # =============================

    # Manually add sector names for individual csvs
    EPA_HCFC22$EPA_sector <- "Production of HCFC-22"
    EPA_ODSS_Aerosols$EPA_sector <- "ODS Substitutes: MDI Aerosols"
    EPA_ODSS_FireExt$EPA_sector <- "ODS Substitutes: Fire Extinguishing"
    EPA_ODSS_Foams$EPA_sector <- "ODS Substitutes: Foam Blowing"
    EPA_ODSS_RefAC$EPA_sector <- "ODS Substitutes: Refrigeration / Air Conditioning"
    EPA_ODSS_Solvents$EPA_sector <- "ODS Substitutes: Solvents"
    EPA_Semi_HFCs$EPA_sector <- "Manufacture of Semiconductors"
    EPA_FPD_HFCs$EPA_sector <- "Flat Panel Display Manufacturing"

    # Combine two main HFC category emissions files, convert to long form, and drop unnecessary secondary region column
    EPA_HCFC22 %>%
      bind_rows(EPA_Semi_HFCs) %>%
      bind_rows(EPA_FPD_HFCs) %>%
      gather_years(value_col = "EPA_emissions") ->
      L141.EPA_HFCs_main

    # Combine with all ODS Substitute emissions subcategory files, convert to long form
    EPA_ODSS_Aerosols %>%
      bind_rows(EPA_ODSS_FireExt, EPA_ODSS_Foams, EPA_ODSS_RefAC, EPA_ODSS_Solvents) %>%
      gather_years(value_col = "EPA_emissions")  %>%
      # bind all HFCs together, join to GCAM regions and aggregate country emissions to GCAM regions
      bind_rows(L141.EPA_HFCs_main) %>%
      left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
      group_by(GCAM_region_ID, EPA_sector, year) %>%
      summarise(EPA_emissions = sum(EPA_emissions)) %>%
      ungroup() %>%
      # Map EPA sectors to EDGAR_agg_sectors and GCAM supplysectors for matching
      left_join_error_no_match(EPA_fgas_sector_map, by = "EPA_sector") %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector, year, EPA_emissions) %>%
      complete(nesting(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector) %>%
      mutate(EPA_emissions = approx_fun(year, EPA_emissions, rule = 1)) %>%
      ungroup() ->
      L141.EPA_HFCs_sector_full

    # Prepare EDGAR data for matching
    # ===============================

    # Adjust EDGAR individual ggs of gas to EPA's GWP values to be able to aggregate multiple gases together for scaling
    L141.hfc_R_S_T_Yh_coolshare %>%
      left_join_error_no_match(EPA_GWPs, by = c("Non.CO2" = "gas")) %>%
      mutate(emissions = emissions * gwp * CONV_GG_TG) %>%
      select(-gwp) ->
      L141.hfc_R_S_T_Yh_GWP

    # Aggregate comm/resid cooling to a single sector to match to EPA totals
    L141.hfc_R_S_T_Yh_GWP %>%
      mutate(supplysector = if_else(grepl('cooling', supplysector), 'cooling', supplysector)) %>%
      group_by(supplysector, subsector, stub.technology, EPA_sector, EDGAR_agg_sector, MAC_type1, GCAM_region_ID, year) %>%
      summarise(emissions = sum(emissions)) %>%
      rename(tot_emissions = emissions) %>%
      ungroup() ->
      L141.hfc_R_S_T_Yh_totalHFC

    # Match data sets and perform scaling calculations
    # ================================================

    # Calculate share of EDGAR emissions to resid and comm cooling and to each individual gas
    L141.hfc_R_S_T_Yh_totalHFC %>%
      left_join(L141.EPA_HFCs_sector_full, by = c("supplysector", "subsector", "stub.technology", "EDGAR_agg_sector",
                                                  "GCAM_region_ID", "year"))  %>%
      mutate(emscalar = EPA_emissions/tot_emissions) %>%
      replace_na(list(emscalar = 0)) -> # replace those shares with "0"
      L141.EPA_hfc_R_S_T_Yh_scalar

    # Multiply EPA emissions totals by share of total to each EDGAR sector, leaving out unmatched cooling sectors
    L141.hfc_R_S_T_Yh_GWP %>%
      inner_join(L141.EPA_hfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology",
                                                      "EPA_sector", "EDGAR_agg_sector", "MAC_type1", "year")) %>%
      mutate(adj_emissions = emissions * emscalar) ->
      L141.EPA_EDGAR_HFCmatches_nocool

    # Fixing cooling sector mismatch by separating out resid and comm cooling rows, adding matching "cooling" sector name
    # and then performing scaling and rebinding to other rows.
    L141.hfc_R_S_T_Yh_GWP %>%
      anti_join(L141.EPA_hfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology",
                                                     "EPA_sector", "EDGAR_agg_sector", "MAC_type1", "year")) %>%
      mutate(supply = "cooling") %>%
      left_join(L141.EPA_hfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supply" = "supplysector", "subsector", "stub.technology",
                                                     "EPA_sector", "EDGAR_agg_sector", "MAC_type1", "year")) %>%
      select(-supply) %>%
      mutate(adj_emissions = emissions * emscalar) %>%
      bind_rows(L141.EPA_EDGAR_HFCmatches_nocool) ->
      L141.EPA_EDGAR_HFCmatches

    # Fixing infinite values
    # ======================

    # We need to calculate replacement values for infinite values in which there is no 1-1 map from EPA_emissions to EDGAR emissions categories
    # (i.e. there is more than one type of PFC gas and the EPA emissions need to be shared out to the individual EDGAR gases)

    L141.hfc_R_S_T_Yh_GWP %>%
      group_by(supplysector, subsector, stub.technology, EPA_sector, EDGAR_agg_sector, MAC_type1, year, Non.CO2) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() ->
      L141.hfc_R_S_T_Yh_gas

    # Calculate global emissions totals by HFC
    L141.hfc_R_S_T_Yh_totalHFC %>%
      group_by(supplysector, subsector, stub.technology, EPA_sector, EDGAR_agg_sector, MAC_type1, year) %>%
      summarise(tot_emissions = sum(tot_emissions)) %>%
      ungroup() ->
      L141.hfc_R_S_T_Yh_totalHFC2

    # Append cooling name so below match can identify resid and comm cooling as cooling sectors
    L141.hfc_R_S_T_Yh_gas %>%
      mutate(supply = if_else(grepl('cooling', supplysector), 'cooling', supplysector)) %>%
      # Calculates global share of emissions in each year and sector to individual HFC gases
      left_join(L141.hfc_R_S_T_Yh_totalHFC2, by = c("supply" = "supplysector", "subsector", "stub.technology",
                                                    "EPA_sector", "EDGAR_agg_sector","MAC_type1" , "year")) %>%
      mutate(emiss_share = emissions / tot_emissions) %>%
      select(-emissions, -tot_emissions, -supply) ->
      L141.hfc_R_S_T_Yh_share

    # Calculate replacement values for EPA sectors with multiple HFC gases with no corresponding EDGAR emissions by multiplying
    # default EPA emissions by global emissions share of gas by sector (resid and comm cooling are combined)
    L141.EPA_EDGAR_HFCmatches %>%
      left_join(L141.hfc_R_S_T_Yh_share, by = c("supplysector", "subsector", "stub.technology",
                                                "EPA_sector", "EDGAR_agg_sector","MAC_type1" ,"year", "Non.CO2")) %>%
      filter(is.infinite(emscalar)) %>%
      mutate(adj_emissions = EPA_emissions * emiss_share) -> L141.EPA_EDGAR_HFCmatches_inf

    # Rebind replaced infinite values to the original df
    L141.EPA_EDGAR_HFCmatches %>%
      filter(!is.infinite(emscalar)) %>%
      bind_rows(L141.EPA_EDGAR_HFCmatches_inf) -> L141.EPA_EDGAR_HFCmatches_adj

    # Clean up data to match data system format (year, unit, columns)
    # ===============================================================

    L141.EPA_EDGAR_HFCmatches_adj %>% #remove columns used in calculation (once testing is completed, integrate with above pipeline)
      select(-EPA_emissions, -tot_emissions, -emscalar, -emissions, -emiss_share) %>%
      rename(value = adj_emissions) %>%
      # remove CO2 equivalence used for matching, returning units to gg
      left_join_error_no_match(EPA_GWPs, by = c("Non.CO2" = "gas")) %>%
      mutate(value = value / gwp / CONV_GG_TG) %>%
      select(-gwp) ->
      L141.EPA_HFC_R_S_T_Yh_adj

    # Overwrite original output file
    L141.hfc_R_S_T_Yh <- L141.EPA_HFC_R_S_T_Yh_adj

    # Compute final cooling HFC emissions factors
    L141.EPA_HFC_R_S_T_Yh_adj %>%
      filter(supplysector %in% c("comm cooling", "resid cooling"), year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L141.R_cooling_T_Yh.long %>% select(GCAM_region_ID, year, service, energy = value),
                               by = c("GCAM_region_ID", "year", "supplysector" = "service")) %>%
      mutate(em_fact = value / energy) %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, em_fact) %>%
      replace_na(list(em_fact = 0)) %>%
      rename(value = em_fact) -> L141.hfc_ef_R_cooling_Yh

    # ===============
    # Produce outputs
    L141.hfc_R_S_T_Yh %>%
      add_title("HFC emissions by region / sector / technology / gas / historical year") %>%
      add_units("Gg") %>%
      add_comments("Edgar emissions, scaled to Guus HFC inventory for residential and commercial cooling") %>%
      add_legacy_name("L141.hfc_R_S_T_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EPA/EPA_HCFC22",
                     "emissions/EPA/EPA_ODSS_Aerosols",
                     "emissions/EPA/EPA_ODSS_FireExt",
                     "emissions/EPA/EPA_ODSS_Foams",
                     "emissions/EPA/EPA_ODSS_RefAC",
                     "emissions/EPA/EPA_ODSS_Solvents",
                     "emissions/EPA/EPA_Semi_HFCs",
                     "emissions/EPA/EPA_FPD_HFCs",
                     "emissions/EPA_country_map",
                     "emissions/EPA_fgas_sector_map",
                     "emissions/EPA_GWPs",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43") ->
      L141.hfc_R_S_T_Yh

    L141.hfc_ef_R_cooling_Yh %>%
      add_title("HFC emissions factors for cooling by region / sector / technology / gas / historical year") %>%
      add_units("Gg / EJ") %>%
      add_comments("HFC emissions (scaled to Guus data) divided by GCAM cooling energy use") %>%
      add_legacy_name("L141.hfc_ef_R_cooling_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "common/iso_GCAM_regID",
                     "emissions/EPA/EPA_HCFC22",
                     "emissions/EPA/EPA_ODSS_Aerosols",
                     "emissions/EPA/EPA_ODSS_FireExt",
                     "emissions/EPA/EPA_ODSS_Foams",
                     "emissions/EPA/EPA_ODSS_RefAC",
                     "emissions/EPA/EPA_ODSS_Solvents",
                     "emissions/EPA/EPA_Semi_HFCs",
                     "emissions/EPA/EPA_FPD_HFCs",
                     "emissions/EPA_country_map",
                     "emissions/EPA_fgas_sector_map",
                     "emissions/EPA_GWPs",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43") ->
      L141.hfc_ef_R_cooling_Yh

    return_data(L141.hfc_R_S_T_Yh, L141.hfc_ef_R_cooling_Yh)
  } else {
    stop("Unknown command")
  }
}
