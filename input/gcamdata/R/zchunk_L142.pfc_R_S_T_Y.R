# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L142.pfc_R_S_T_Y
#'
#' Map HFC emission shares by region, sector, technology, gas, and year for years 1971-2008.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.pfc_R_S_T_Yh}. The corresponding file in the
#' original data system was \code{L142.pfc_R_S_T_Y.R} (emissions level1).
#' @details First, a table was created that has all historical HFCs values.
#' Then, regional and sector information was added.  And finally, HFC emission shares
#' were calculated by suming emissions over region, sector, technology, and gas by year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL June 2017 and YO Feburary 2020
module_emissions_L142.pfc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/gcam_fgas_tech",
             FILE = "emissions/other_f_gases",
             "L144.in_EJ_R_bld_serv_F_Yh",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector_fgas",
             FILE = "emissions/A41.GWP",
             FILE = "emissions/EDGAR/EDGAR_SF6",
             FILE = "emissions/EDGAR/EDGAR_C2F6",
             FILE = "emissions/EDGAR/EDGAR_CF4",
             FILE = "emissions/EPA/EPA_PFC_Al",
             FILE = "emissions/EPA/EPA_PFC_FPD",
             FILE = "emissions/EPA/EPA_PFC_PV",
             FILE = "emissions/EPA/EPA_PFC_Semi",
             FILE = "emissions/EPA/EPA_SF6_EPS",
             FILE = "emissions/EPA/EPA_SF6_FPD",
             FILE = "emissions/EPA/EPA_SF6_Magn",
             FILE = "emissions/EPA/EPA_SF6_Semi",
             FILE = "emissions/EPA_fgas_sector_map",
             FILE = "emissions/EPA_GWPs",
             FILE = "emissions/EPA_country_map"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.pfc_R_S_T_Yh"))
  } else if(command == driver.MAKE) {

    sector <- fuel <- service <- IPCC_Annex <- World_Region <- ISO_A3 <- Name <- IPCC <- IPCC_description <-
      Gas <- agg_sector <- EDGAR_agg_sector <- Sector <- supplysector <- subsector <- stub.technology <-
      EDGAR_emissions <- value.y <- value.x <- share <- emissions <-year <- iso <- awb <- Non.CO2 <-
      GCAM_region_ID <- AWB_emiss_share <- awb_emission <- value <- . <- NULL # silence package check.

    all_data <- list(...)[[1]]

    #silence packages
    gas <- adj_emissions  <- emiss_share <- supply <- emscalar <- tot_emissions <- MAC_type1 <- gwp <- EPA_emissions <- EPA_sector <- NULL

    # Load required inputs

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    GCAM_tech <- get_data(all_data, "emissions/gcam_fgas_tech")
    Other_F <- get_data(all_data, "emissions/other_f_gases")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector_fgas")
    GWP <- get_data(all_data, "emissions/A41.GWP")

    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    get_data(all_data, "emissions/EDGAR/EDGAR_SF6") %>%
      gather_years ->
      EDGAR_SF6
    get_data(all_data, "emissions/EDGAR/EDGAR_C2F6") %>%
      gather_years ->
      EDGAR_C2F6
    get_data(all_data, "emissions/EDGAR/EDGAR_CF4") %>%
      gather_years ->
      EDGAR_CF4
    EPA_PFC_Al <- get_data(all_data, "emissions/EPA/EPA_PFC_Al")
    EPA_PFC_FPD <- get_data(all_data, "emissions/EPA/EPA_PFC_FPD")
    EPA_PFC_PV <- get_data(all_data, "emissions/EPA/EPA_PFC_PV")
    EPA_PFC_Semi <- get_data(all_data, "emissions/EPA/EPA_PFC_Semi")
    EPA_SF6_EPS <- get_data(all_data, "emissions/EPA/EPA_SF6_EPS")
    EPA_SF6_FPD <- get_data(all_data, "emissions/EPA/EPA_SF6_FPD")
    EPA_SF6_Magn <- get_data(all_data, "emissions/EPA/EPA_SF6_Magn")
    EPA_SF6_Semi <- get_data(all_data, "emissions/EPA/EPA_SF6_Semi")
    EPA_fgas_sector_map <- get_data(all_data, "emissions/EPA_fgas_sector_map")
    EPA_GWPs <- get_data(all_data, "emissions/EPA_GWPs")
    EPA_country_map <- get_data(all_data, "emissions/EPA_country_map")

    # This chunk maps EDGAR HFC emissions to GCAM technologies
    # First, create a table with all EDGAR HFCs, and prep "Other_F" table for use by renaming column.

    EDGAR_SF6$Non.CO2 <- "SF6"
    EDGAR_C2F6$Non.CO2 <- "C2F6"
    EDGAR_CF4 %>%
      mutate(Non.CO2 = "CF4") %>%
      bind_rows(EDGAR_SF6, EDGAR_C2F6, .) ->
      ALL_EDGAR_HFC
    Other_F %>%
      rename(Non.CO2 = Gas) ->
      Other_F

    # Then, prepare EDGAR data for use.
    # Map in region ID and sector name, and remove year 1970 to match "HISTORICAL_YEARS" constant.

    ALL_EDGAR_HFC %>%
      left_join_error_no_match(EDGAR_sector, by = "IPCC_description") %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%                                        # Switch Romania iso code to its pre-2002 value
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(GCAM_region_ID, EDGAR_agg_sector = agg_sector, Non.CO2, year, value) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L142.EDGAR_HFC

    L142.EDGAR_HFC %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
      summarise(EDGAR_emissions = sum(value)) %>%
      ungroup() ->
      L142.EDGAR_PFC_R_S_T_Yh.tmp1

    # Map in other f-gas sector, which varies by gas.
    # Seperate PFCs and add non-CO2 gas information, then recombine.

    L142.EDGAR_PFC_R_S_T_Yh.tmp1 %>%
      filter(EDGAR_agg_sector != "other_f_gases") ->
      L142.EDGAR_PFC_R_S_T_Yh_rest

    L142.EDGAR_PFC_R_S_T_Yh.tmp1 %>%
      filter(EDGAR_agg_sector == "other_f_gases") %>%
      ungroup() %>%
      left_join_error_no_match(Other_F, by = "Non.CO2") %>%
      select(-EDGAR_agg_sector) %>%
      rename(EDGAR_agg_sector = Sector) %>%
      bind_rows(L142.EDGAR_PFC_R_S_T_Yh_rest, .) %>%
      ungroup() ->                         # ungroup needed for later `left_join_keep_first_only`
      L142.EDGAR_PFC_R_S_T_Yh.tmp1

    # Map PFC gases to GCAM technologies.

    GCAM_tech %>%
      repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
      repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble("Non.CO2" = emissions.PFCS)) %>%
      left_join_keep_first_only(L142.EDGAR_PFC_R_S_T_Yh.tmp1, by = c("GCAM_region_ID", "year", "EDGAR_agg_sector", "Non.CO2")) %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, EDGAR_emissions) %>%
      replace_na(list(EDGAR_emissions = 0)) ->
      L142.pfc_R_S_T_Yh.tmp1

    # Disaggregate cooling emissions to residential and commercial sectors.

    L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(service %in% c("comm cooling", "resid cooling") & fuel == "electricity") ->
      L142.R_cooling_T_Yh

    L142.R_cooling_T_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L142.R_cooling_Yh

    # Calculate emission shares for residential and commerical sectors.
    # Use 'left_join' because the number of rows is not equal, and NA values are needed.

    L142.R_cooling_Yh %>%
      left_join(L142.R_cooling_T_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(share = value.y / value.x) ->
      L142.R_cooling_T_Yh.tmp1

    # Calculate emissions by share, then sum up emissions over region, sector, technology, and gas by year.

    L142.R_cooling_T_Yh.tmp1 %>%
      rename(supplysector = service) %>%
      right_join(L142.pfc_R_S_T_Yh.tmp1, by = c("GCAM_region_ID", "year", "supplysector")) %>%
      replace_na(list(share = 1)) %>%
      mutate(emissions = EDGAR_emissions * share) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() %>%
      replace_na(list(emissions = 0)) %>%
      rename(value = emissions) ->
      L142.pfc_R_S_T_Yh

    # make sure EDGAR have data till 2015 for EPA BAU calibration
    # remaing_years is the missing base years that EPA has but EDGAR does not
    # Duplicate EDGAR last-year data and rename as missing years just as placeholders, which will be scaled to EPA later
    # Note: here additional years are for all missing years (2009, 2010, 2011 etc), not just GCAM modeling years
    # So we can deal with base-year time shifting
    additional_years <-
      HISTORICAL_YEARS[HISTORICAL_YEARS > emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]]

    if(length(additional_years) == 0){
      L142.pfc_R_S_T_Yh_base <- L142.pfc_R_S_T_Yh
    } else{
      TEMP <- filter(L142.pfc_R_S_T_Yh, year == emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]) %>%
        select(-year) %>%
        repeat_add_columns(tibble(year = additional_years))

      L142.pfc_R_S_T_Yh_base <- bind_rows(L142.pfc_R_S_T_Yh, TEMP)
    }


      # Add EPA sector names to individual EPA files
      EPA_PFC_Al$EPA_sector <- "Aluminum"
      EPA_PFC_FPD$EPA_sector <- "Flat Panel Display Manufacturing"
      EPA_PFC_PV$EPA_sector <- "Photovoltaic Manufacturing"
      EPA_PFC_Semi$EPA_sector <- "Manufacture of Semiconductors"
      EPA_SF6_EPS$EPA_sector <- "Electric Power Systems"
      EPA_SF6_FPD$EPA_sector <- "Flat Panel Display Manufacturing"
      EPA_SF6_Magn$EPA_sector <- "Magnesium Manufacturing"
      EPA_SF6_Semi$EPA_sector <- "Manufacture of Semiconductors"

      # Combine two main PFC category emissions files, convert to long form, and drop unnecessary secondary region column
      # Assign PFC gas name to PFC emissions sources
      EPA_PFC_Al %>%
        bind_rows(EPA_PFC_FPD, EPA_PFC_PV, EPA_PFC_Semi) %>%
        gather_years(value_col = "EPA_emissions") ->
        L142.EPA_PFCs_main
      L142.EPA_PFCs_main$gas <- "PFC"

      # Assign SF6 gas name to SF6 emissions sources
      EPA_SF6_EPS %>%
        bind_rows(EPA_SF6_FPD, EPA_SF6_Magn, EPA_SF6_Semi) %>%
        gather_years(value_col = "EPA_emissions") ->
        L142.EPA_SF6_main
      L142.EPA_SF6_main$gas <- "SF6"

      # Combine SF6 and PFC emissions into one data frame
      L142.EPA_SF6_main %>%
        bind_rows(L142.EPA_PFCs_main) ->
        L142.EPA_PFCs

      # Replace EPA's "no data" dash mark with 0 values and convert columns from char to dbl (unneeded with new csvs)
      # L142.EPA_PFCs[,][L142.EPA_PFCs[,] == "-"] <- 0
      # L142.EPA_PFCs <- mutate(L142.EPA_PFCs, EPA_emissions = as.numeric(EPA_emissions))

      # Map countries to GCAM regions and aggregate country emissions to GCAM regions
      L142.EPA_PFCs %>%
        left_join(EPA_country_map, by = c("country" = "EPA_country")) %>%
        group_by(GCAM_region_ID, EPA_sector, gas, year) %>%
        summarise(EPA_emissions = sum(EPA_emissions)) %>%
        ungroup() %>%

      # Map EPA emissions data to GCAM sectors and aggregate EPA sectors to level of GCAM sectors (combine FPD, PV, and Semiconductors)
        left_join(EPA_fgas_sector_map, by = "EPA_sector") %>%
        select(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector, gas, year, EPA_emissions) %>%
        group_by(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector, gas, year) %>%
        summarise(EPA_emissions = sum(EPA_emissions)) %>%
        ungroup() %>%

      # Interpolate model years not in EPA (between start and end year)
        complete(nesting(GCAM_region_ID, supplysector, subsector, stub.technology, EDGAR_agg_sector, gas), year = c(year, HISTORICAL_YEARS)) %>%
        arrange(supplysector, gas, year) %>%
        group_by(GCAM_region_ID, supplysector, subsector, stub.technology, gas, EDGAR_agg_sector) %>%
        mutate(EPA_emissions = approx_fun(year, EPA_emissions, rule = 1)) %>%
        ungroup() ->
        L142.EPA_PFCs_sector_full


      # Prepare and aggregate EDGAR emissions for matching
      # --------------------------------------------------

      # Adjust EDGAR individual ggs of gas to EPA's GWP values to be able to aggregate multiple gases together for scaling
      L142.pfc_R_S_T_Yh_base %>%
        left_join_error_no_match(EPA_GWPs, by = c("Non.CO2" = "gas")) %>%
        mutate(emissions = value * gwp * CONV_GG_TG) %>%
        select(-gwp, -value) ->
        L142.pfc_R_S_T_Yh_GWP

      # Aggregate comm/resid cooling to a single sector to match to EPA totals
      L142.pfc_R_S_T_Yh_GWP %>%
        filter(supplysector == "comm cooling" | supplysector == "resid cooling") %>%
        group_by(subsector, stub.technology, GCAM_region_ID, year, Non.CO2) %>%
        summarise(emissions = sum(emissions)) %>%
        ungroup() %>%
        # add name of "cooling" sector to match EPA sector map file
        mutate(supplysector = "cooling") ->
        L142.pfc_R_S_T_Yh_coolingonly

      # Rebind aggregated cooling to main HFC emissions
      L142.pfc_R_S_T_Yh_GWP %>%
        filter(supplysector != "comm cooling" & supplysector != "resid cooling") %>%
        bind_rows(L142.pfc_R_S_T_Yh_coolingonly) ->
        L142.pfc_R_S_T_Yh_cool

      # Separate SF6 to skip PFC aggregation
      L142.pfc_R_S_T_Yh_cool %>%
        filter(Non.CO2 == "SF6") ->
        L142.pfc_R_S_T_Yh_SF6

      # Aggregate individual PFC gas emissions from EDGAR to total PFC emissions to prepare to match to EPA
      L142.pfc_R_S_T_Yh_cool %>%
        ungroup() %>%
        filter(Non.CO2 != "SF6") %>%
        group_by(GCAM_region_ID, supplysector, subsector, stub.technology, year) %>%
        summarise(emissions = sum(emissions)) %>%
        ungroup() %>%
        mutate(Non.CO2 = "PFC") %>%
        # Rebind with SF6 emissions, which already match 1-to-1
        bind_rows(L142.pfc_R_S_T_Yh_SF6) %>%
        rename(tot_emissions = emissions) ->
        L142.pfc_R_S_T_Yh_totalPFC

      # Combine data sets and scale emissions EDGAR emissions by EPA totals
      # -------------------------------------------------------------------
      # Calculate share of EDGAR emissions to resid and comm cooling and to each individual gas
      L142.pfc_R_S_T_Yh_totalPFC %>%
        left_join(L142.EPA_PFCs_sector_full, by = c("supplysector", "subsector", "stub.technology",
                                               "GCAM_region_ID", "year", "Non.CO2" = "gas"))  %>%
        mutate(emscalar = EPA_emissions/tot_emissions) %>%
        replace_na(list(emscalar = 0)) -> # replace those shares with "0"
        L142.EPA_pfc_R_S_T_Yh_scalar

      # Add additional 'gas' column to identify C2F6 and CF4 as PFCs for match with EPA
      L142.pfc_R_S_T_Yh_GWP %>%
        filter(Non.CO2 != "SF6") %>%
        mutate(gas = "PFC") -> L142.pfc_R_S_T_Yh_gas

      L142.pfc_R_S_T_Yh_GWP %>%
        filter(Non.CO2 == "SF6") %>%
        mutate(gas = "SF6") %>%
        bind_rows(L142.pfc_R_S_T_Yh_gas) -> L142.pfc_R_S_T_Yh_gas2

      # Multiply EPA emissions totals by share of total to each EDGAR sector and gas, leaving out unmatched cooling sectors
      L142.pfc_R_S_T_Yh_gas2 %>%
        inner_join(L142.EPA_pfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology",
                                            "gas" = "Non.CO2", "year")) %>%
        mutate(adj_emissions = emissions * emscalar) ->
        L142.EPA_EDGAR_PFCmatch_nocool

      # Fixing cooling sector mismatch
      L142.pfc_R_S_T_Yh_gas2 %>%
        anti_join(L142.EPA_pfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology",
                                                       "gas" = "Non.CO2", "year")) %>%
        mutate(supply = "cooling") %>%
        left_join(L142.EPA_pfc_R_S_T_Yh_scalar, by = c("GCAM_region_ID", "supply" = "supplysector", "subsector",
                                                       "stub.technology", "gas" = "Non.CO2", "year")) %>%
        select(-supply) %>%
        mutate(adj_emissions = emissions * emscalar) %>%
        bind_rows(L142.EPA_EDGAR_PFCmatch_nocool) ->
        L142.EPA_EDGAR_PFCmatch

      # FIXING INFINITE VALUES
      # We need to calculate replacement values for infinite values in which there is no 1-1 map from EPA_emissions to EDGAR emissions categories
      # (i.e. there is more than one type of PFC gas and the EPA emissions need to be shared out to the individual EDGAR gases)
      # This currently works despite cooling sector mismatch (comm cooling/resid cooling vs. cooling) because there are no infinite values in cooling.
      # If infinite values are found in cooling sector, this sector aggregation will need to be coded in here.
      # Calculate global emissions totals by PFC and SF6
      L142.pfc_R_S_T_Yh_totalPFC %>%
        group_by(supplysector, subsector, stub.technology, year, Non.CO2) %>%
        summarise(tot_emissions = sum(tot_emissions)) %>%
        ungroup() ->
        L142.pfc_R_S_T_Yh_totalPFC2

      L142.pfc_R_S_T_Yh_GWP %>%
        group_by(supplysector, subsector, stub.technology, year, Non.CO2) %>%
        summarise(emissions = sum(emissions)) %>%
        ungroup() ->
        L142.pfc_R_S_T_Yh_gas3

      # Adds gas column back in for matching with PFC gases
      L142.pfc_R_S_T_Yh_gas3 %>%
        filter(Non.CO2 != "SF6") %>%
        mutate(gas = "PFC") -> TEMPPFC

      L142.pfc_R_S_T_Yh_gas3 %>%
        filter(Non.CO2 == "SF6") %>%
        mutate(gas = "SF6") %>%
        bind_rows(TEMPPFC) %>%

      # Calculates global share of emissions in each year and sector to CF4 and C2F6
        left_join(L142.pfc_R_S_T_Yh_totalPFC2, by = c("supplysector", "subsector",
                                                                    "stub.technology", "gas" = "Non.CO2", "year")) %>%
        mutate(emiss_share = emissions / tot_emissions) %>%
        select(-emissions, -tot_emissions) ->
        L142.pfc_R_S_T_Yh_share

      # Calculate replacement values for EPA sectors with multiple PFC gases with no corresponding EDGAR emissions
      L142.EPA_EDGAR_PFCmatch %>%
        left_join(L142.pfc_R_S_T_Yh_share, by = c("supplysector", "subsector", "stub.technology", "Non.CO2", "gas", "year")) %>%
        filter(is.infinite(emscalar)) %>%
        mutate(adj_emissions = EPA_emissions * emiss_share) %>%
        select(-emiss_share) -> L142.EPA_EDGAR_PFCmatch_inf

      # Rebind replaced infinite values to the original df
      L142.EPA_EDGAR_PFCmatch %>%
        filter(!is.infinite(emscalar)) %>%
        bind_rows(L142.EPA_EDGAR_PFCmatch_inf) -> L142.EPA_EDGAR_PFCmatch_adj
      # END OF INFINITE VALUES FIX

      # Clean up data to include base years (annually, not just modeling years)
      L142.EPA_EDGAR_PFCmatch_adj %>% #remove columns used in calculation (once testing is completed, integrate with above pipeline)
        select(-EPA_emissions, -tot_emissions, -EPA_emissions, -emscalar, -gas, -emissions) %>%
        rename(value = adj_emissions) %>%
        # remove CO2 equivalence used for matching, returning units to gg
        left_join_error_no_match(EPA_GWPs, by = c("Non.CO2" = "gas")) %>%
        mutate(value = value / gwp / CONV_GG_TG) %>%
        select(-gwp) ->
        L142.EPA_PFC_R_S_T_Yh

      # Replace original output with scaled values
      L142.pfc_R_S_T_Yh <- L142.EPA_PFC_R_S_T_Yh

    # Produce outputs
    # ===============
    L142.pfc_R_S_T_Yh %>%
      add_title("HFC emissions by region, sector, technology, gas, and historical year") %>%
      add_units("Gg") %>%
      add_comments("Created a table with all historical HFCs values.") %>%
      add_comments("Regional and sector information was added.") %>%
      add_comments("Calculated HFC emissions by share over region, sector, technology, and gas by year.") %>%
      add_legacy_name("L142.pfc_R_S_T_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/A41.GWP",
                     "emissions/EDGAR/EDGAR_SF6",
                     "emissions/EDGAR/EDGAR_C2F6",
                     "emissions/EDGAR/EDGAR_CF4",
                     "emissions/EPA/EPA_PFC_Al",
                     "emissions/EPA/EPA_PFC_FPD",
                     "emissions/EPA/EPA_PFC_PV",
                     "emissions/EPA/EPA_PFC_Semi",
                     "emissions/EPA/EPA_SF6_EPS",
                     "emissions/EPA/EPA_SF6_FPD",
                     "emissions/EPA/EPA_SF6_Magn",
                     "emissions/EPA/EPA_SF6_Semi",
                     "emissions/EPA_fgas_sector_map",
                     "emissions/EPA_GWPs",
                     "emissions/EPA_country_map") ->
      L142.pfc_R_S_T_Yh

    return_data(L142.pfc_R_S_T_Yh)
  } else {
    stop("Unknown command")
  }
}
