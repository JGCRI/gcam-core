# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.Population_downscale_ctry
#'
#'  Clean and interpolate both Maddison historical population data (1700-max(UN_HISTORICAL_YEARS)) and SSP population scenarios.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_ctry_Yh}, \code{L100.Pop_thous_SSP_ctry_Yfut}. The corresponding file in the
#' original data system was \code{L100.Population_downscale_ctry.R} (socioeconomics level1).
#' @details (1) Cleans Maddison historical population data and interpolates to country and year (1700-2010). (2) Cleans SSP population scenarios for smooth join with final base year population.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate order_by select summarize
#' @importFrom tidyr complete nesting replace_na
#' @author STW May 2017
module_socio_L100.Population_downscale_ctry <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "socioeconomics/POP/iso_ctry_Maddison",
      FILE = "socioeconomics/POP/Maddison_population",
      FILE = "socioeconomics/SSP/SSP_database_2024",
      FILE = "socioeconomics/SSP/iso_SSP_regID",
      FILE = "socioeconomics/POP/UN_popTot")

  MODULE_OUTPUTS <-
    c("L100.Pop_thous_ctry_Yh",
      "L100.Pop_thous_SSP_ctry_Yfut")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    ## silence package check.
    Country <- value <- Maddison_ctry <- year <- pop <- Downscale_from <- ratio <-
      year.x <- iso <- pop_scale <- pop2 <- pop.x <- pop.y <- pop_allocate <- X1900 <-
      X1950 <- X1850 <- X1800 <- X1750 <- X1700 <- pop_ratio <- scg <-
      idn <- mne <- Scenario <- Region <- Sex <- Year <- Value <- MODEL <-
      VARIABLE <- REGION <- SCENARIO <- UNIT <- scenario <- ratio_iso_ssp <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    Maddison_population %>%
      select(-deleteme) %>%
      gather_years %>%
      # Remove all the blanks and "Total..." lines
      filter(!(substr(Country, 1, 5) == "Total" & Country != "Total Former USSR"),
             !is.na(Country)) %>%
      mutate(year = as.integer(year)) ->
      Maddison_population

    # ===================================================

    ## (1) Historical population by country

    # First clean up Maddison raw data -- NOTE: Maddison data are used to develop population ratios relative to 1950 to combine with UN data from 1950 onward
    pop_thous_ctry_reg <- Maddison_population %>%
      rename(Maddison_ctry = Country, pop = value) %>%  # Change name to match iso_ctry_Maddison mapping file
      left_join(iso_ctry_Maddison, by = "Maddison_ctry") # Join with iso codes

    # Second, estimate population values prior to 1950 for countries in aggregate regions. This is what we want: pop_country_t = (pop_aggregate_t / pop_aggregate_1950) * pop_country_1950
    # Generate a scalar for population in each aggregate region in 1950 (to generate the population ratios)
    agg_ratio <- pop_thous_ctry_reg %>%
      select(Maddison_ctry, year, pop) %>%
      filter(year <=  min(socioeconomics.UN_HISTORICAL_YEARS),
             Maddison_ctry %in% c("Czechoslovakia", "Eritrea and Ethiopia", "Total Former USSR", "Yugoslavia")) %>% # Only want years prior to 1951 for the four aggregate regions
      group_by(Maddison_ctry) %>%  # Group to perform action on each aggregate region individually
      mutate(ratio = pop / pop[year == min(socioeconomics.UN_HISTORICAL_YEARS)]) %>%  # Create ratio of population in prior years to population in 1950
      rename(Downscale_from = Maddison_ctry) %>%  # Will match each country in the region to this ratio
      select(-pop) %>%
      left_join(filter(pop_thous_ctry_reg, year == min(socioeconomics.UN_HISTORICAL_YEARS)), by = "Downscale_from") %>%  # Join with the 1950 populations for each member country
      mutate(pop_scale = pop * ratio) %>%  # Create population values prior to 1950 for downscaled countries based on their 1950 populations and pop ratio from aggregate regions
      rename(year = year.x) %>%  # Want to keep the scaled years
      ungroup() %>%  # Need to remove grouping so that we can keep only the variables we want and join with the primary data
      filter(year != min(socioeconomics.UN_HISTORICAL_YEARS)) %>% # Remove base aggregation year (it's in the next step, don't want to duplicate it)
      select(Maddison_ctry, iso, year, pop_scale)
    # Bind these with the other countries
    reg_scaled <- pop_thous_ctry_reg %>%
      left_join(agg_ratio, by = c("Maddison_ctry", "iso", "year")) %>%
      mutate(pop = if_else((year < min(socioeconomics.UN_HISTORICAL_YEARS) & !is.na(pop_scale)), pop_scale, pop), # Replace NA values for countries with downscaled population values
             iso = if_else(Maddison_ctry == "World Total", "world_total", iso)) %>% # Will need total world population in step 4
      filter(!is.na(iso)) %>%  # Remove rows with no iso code (generally aggregate regions)
      select(iso, year, pop)  # Keep only necessary variables

    # Third, interpolate available population data to generate values for required historical years (1700, 1750, 1800, 1850, 1900)
    hist_interp <- tibble(year = socioeconomics.MADDISON_HISTORICAL_YEARS) %>%  # Create tibble with required years (not all are in the original Maddison data)
      full_join(reg_scaled, by = "year") %>%  # Join with available population data (creates NAs for years not available in raw data)
      complete(year, nesting(iso)) %>%  # Completes tibble to include all years for all iso (creates missing values)
      filter(!is.na(iso)) %>%  # iso NA values created with complete on year and iso table
      group_by(iso) %>%
      mutate(pop = approx_fun(year, pop),  # Interpolate -- note that there will still be missing values for countries that do not have end values (1500, 1600, or 1700)
             pop = if_else(is.na(pop) & year == 1800, pop[year == 1820], pop)) %>%  # Replace missing 1800 values with 1820 for countries that begin in 1820 (note that Panama has a value in 1820 = 0, not NA)
      filter(year %in% c(socioeconomics.MADDISON_HISTORICAL_YEARS, min(socioeconomics.UN_HISTORICAL_YEARS))) # Keep only needed years

    # Fourth, assign population values to countries with missing values in historical years
    # Extract total world population for historic years
    pop_global <- filter(hist_interp, iso == "world_total") %>%
      ungroup() %>%
      select(-iso)
    # Sum all countries' populations in each period
    pop_notmissing <- filter(hist_interp, iso != "world_total") %>%
      replace_na(list(pop = 0)) %>%
      group_by(year) %>%
      summarize(pop = sum(pop))

    # Subtract reported countries from total global - these are the total population values in each period that will be allocated to countries with missing values
    pop_missing <- left_join(pop_global, pop_notmissing, by = "year") %>%
      mutate(pop_allocate = pop.x - pop.y) %>%
      select(year, pop_allocate)

    # Fifth, estimate historic population values for countries with missing values and calculate ratios relative to last historical year (1950)
    maddison_hist_ratio <-
      filter(hist_interp, iso != "world_total") %>%
      left_join(pop_missing, by = "year")

    maddison_hist_years <- unique(maddison_hist_ratio$year)

    # Fill in missing values for period t using ratio of country to total population in period t+1 times the missing share in period t.
    for( i in rev(maddison_hist_years)[-1]) {
      maddison_hist_ratio %>%
        ungroup() %>%
        mutate(pop2 = if_else(is.na(pop), (dplyr::lead(pop, n = 1L, order_by = iso)), 0)) %>%
        group_by(year) %>%
        mutate(pop = if_else(year == i, pmax(pop, pop2/sum(pop2) * pop_allocate, na.rm = TRUE), pop)) ->
        maddison_hist_ratio
    }

    # Population ratios for historic years compared to last historical year (1950) for all countries (will be used to scale UN populations from 1950)
    maddison_hist_ratio <- maddison_hist_ratio %>%
      group_by(iso) %>%
      mutate(pop_ratio = pop / pop[year == rev(maddison_hist_years)[1]]) %>%
      filter(year != rev(maddison_hist_years)[1]) %>%
      select(iso, year, pop_ratio) %>%
      mutate(year = as.integer(year)) %>%
      ungroup

    # Need to create population ratios & iso codes for countries in UN, but not Maddison.
    # Renaming (Romania), adding countries (Timor-Leste, South Sudan, Vatican, Kosovo), in other cases Iso disaggregation,
    # where population ratios will remain the same from parent to child (X---> A,B,C where X=A=B=C)
    new_UN_iso <- filter(maddison_hist_ratio, iso %in% c("idn", "scg", "chi", "sdn", "rom", "ita","alb", "ant", "glp", "shn", "nru")) %>%
      mutate(iso = replace(iso, iso == "idn", "tls"), #Create Timor-Leste iso using Indonesia pop ratio
             iso = replace(iso, iso == "scg", "mne"), #Create Montenegro iso using Serbia & Montenegro pop ratio
             iso = replace(iso, iso == "chi", "jey"), #Create Jersey iso using the Channel Islands pop ratio
             iso = replace(iso, iso == "sdn", "ssd"), #Create South Sudan iso using Sudan pop ratio
             iso = replace(iso, iso == "rom", "rou"), #Create Romania iso using Romania pop ratio (renaming iso)
             iso = replace(iso, iso == "ita", "vat"), #Create Vatican(Holy See) iso using Italy pop ratio
             iso = replace(iso, iso == "alb", "xkx"), #Create Kosovo iso using Albania pop ratio
             iso = replace(iso, iso == "ant", "bes"), #Create Bonaire, Sint Eustatius and Saba iso using Netherlands Antilles pop ratio
             iso = replace(iso, iso == "glp", "blm"), #Create Saint Barthelemy iso using Guadalupe pop ratio
             iso = replace(iso, iso == "shn", "flk"), #Create the Falkland Islands iso using Saint Helena, Ascension and Tristan da Cunha pop ratio
             iso = replace(iso, iso == "nru", "niu")  #Create Niue iso using the Republic of Nauru pop ratio
             ) %>%
      bind_rows(filter(maddison_hist_ratio, iso %in% c("scg", "chi", "ant", "glp", "nru"))) %>%
      mutate(iso = replace(iso, iso == "scg", "srb"), #Create Serbia iso using Serbia & Montenegro pop ratio
             iso = replace(iso, iso == "chi", "ggy"), #Create Guernsey iso using the Channel Islands pop ratio
             iso = replace(iso, iso == "ant", "cuw"), #Create Curacao iso using Netherlands Antilles pop ratio
             iso = replace(iso, iso == "glp", "maf"), #Create Saint Martin iso using Guadalupe pop ratio
             iso = replace(iso, iso == "nru", "tkl")  #Create Tokelau iso using the Republic of Nauru pop ratio
             ) %>%
      bind_rows(filter(maddison_hist_ratio, iso %in% c("ant"))) %>%
      mutate(iso = replace(iso, iso == "ant", "sxm"))  #Create Sint Maarten iso using Netherlands Antilles pop ratio

    # Combine with additional UN pop ratio_iso values
    maddison_hist_ratio <- maddison_hist_ratio %>%
      bind_rows(new_UN_iso) %>%
      #Remove disaggregated/renamed iso's
      filter(iso != "scg") %>%
      filter(iso != "ant") %>%
      filter(iso != "chi") %>%
      filter(iso != "rom")

    # Adding check here to see if there are still any iso's in UN data are not in updated madison data and probably error if so.
    # Since the UN data will be regularly updated, this will help debugging future updates
    UN_iso <- tolower(unique(UN_popTot$Country))
    maddison_iso <- unique(maddison_hist_ratio$iso)
    missing_iso <- UN_iso[!(UN_iso %in% maddison_iso)]
    if (length(missing_iso > 0)) {
      stop(paste0("Error: The following iso's are in UN data but are missing from madison data: ", toString(missing_iso)))
    }

    # Sixth, apply Maddison ratios for historic periods to UN population data that begin in 1950
    # Clean raw UN population data
    un_clean <- UN_popTot %>%
      filter(Scenario == "Estimates") %>%  # "Estimates" is the UN historical estimates, not projections
      select(-Region, -Sex, -Scenario) %>%
      rename(iso = Country, year = Year, pop = Value) %>%
      mutate(iso = tolower(iso)) %>%
      filter(year %in% socioeconomics.UN_HISTORICAL_YEARS) %>%  # Keep only the years that we are using
      mutate(iso = gsub("xea", "twn", iso)) # Correct iso code for Taiwan

    # Multiply UN 1950 population by Maddison historic ratios to get values for pre-1950 years
    un_maddison_hist <- filter(un_clean, year == min(socioeconomics.UN_HISTORICAL_YEARS)) %>%
      select(-year) %>%
      full_join(maddison_hist_ratio, by = "iso") %>%
      complete(year, nesting(iso)) %>%  # Completes tibble to include all years for all iso (creates missing values)
      mutate(pop = if_else(!is.na(pop_ratio), (pop * pop_ratio), 0)) %>% # Set the remaining mismatched (very small) countries to zero
      select(iso, year, pop)

    # Combine scaled population for Maddison historic years with UN population 1950+
    L100.Pop_thous_ctry_Yh <- bind_rows(un_clean, un_maddison_hist) %>%
      filter(year %in% c(socioeconomics.MADDISON_HISTORICAL_YEARS, socioeconomics.UN_HISTORICAL_YEARS)) %>%
      replace_na(list(pop = 0)) %>%
      rename(value = pop) %>%
      mutate(iso = gsub("rou", "rom", iso), #Revert new Romania iso (ROU) back to old iso (ROM) for uniformity throughout GCAM
             iso = gsub("bes", "ant", iso),
             iso = gsub("cuw", "ant", iso),
             iso = gsub("sxm", "ant", iso)#Aggregating Netherlands Antilles islands (broken out in newer UN Population Data)
             ) %>%
      group_by(iso, year) %>%
      mutate(value = sum(value)) %>%
      ungroup() %>%
      distinct()

    # Some iso's don't have data out to the base year, so we need to extrapolate
    # Create a dataframe that has all iso's and all years, and fill with the values we have from Maddison and UN
    L100.Pop_thous_ctry_Yh %>%
      repeat_add_columns(tibble::tibble(iso=unique(L100.Pop_thous_ctry_Yh$iso))) %>%
      repeat_add_columns(tibble::tibble(year=unique(L100.Pop_thous_ctry_Yh$year))) %>%
      select(-c(iso.x,year.x,value)) %>%
      distinct() %>%
      # Must use left_join because we want NAs in this dataframe to show what is missing from IEA
      left_join(L100.Pop_thous_ctry_Yh, by=c("iso.y" = "iso", "year.y" = "year")) %>%
      rename(iso = iso.y,
             year = year.y) ->
      # This results in a df that has NAs, so we can interpolate / extrapolate
      L100.Pop_thous_ctry_UNpopYh_NAs

    # Write out a warning with the number of NAs
    L100.Pop_thous_ctry_UNpopYh_NAs %>%
      filter(is.na(value)) -> value_NAs

    if ( dim(value_NAs)[1] != 0 ) {
      paste0("Warning: There are ", nrow(value_NAs), " NAs in the dataframe. These values will be interpolated or extrapolated.")
    }

    # Interpolate and/or extrapolate to fill NAs
    # If there are no NAs, this will not do anything but change the dataframe name
    L100.Pop_thous_ctry_UNpopYh_NAs %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L100.Pop_thous_ctry_UNpopYh
    # L100.Pop_thous_ctry_UNpopYh - NOTE: _popYh indicates data set is for historical(h), population(pop) years (Y).
    # This distinction  is important because the population data is updated to a more recent year than the GCAM base year.
    # Meaning the final Historical Year output will be a subset (out to the max base year) of the Historical Population Years

    ## (2) SSP population projections by country

    # First, extract the final historical population from UN
    pop_final_hist <- filter(L100.Pop_thous_ctry_Yh, year == socioeconomics.FINAL_HIST_YEAR) %>%
      rename(pop_final_hist = value) %>%
      select(-year)

    # Second, generate ratios of future population to base year for all SSPs. The ratios will be applied to the historical year populations so there are no jumps/inconsistencies.

    # use the IIASA-WiC POP model from the SSP database; IIASA-WiC is the official SSP population data set
    SSP_database_2024 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region)) %>%
      filter(model == "IIASA-WiC POP 2023", variable == "Population") %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years() ->
      SSP_pop_0

    # Using the Historical Reference scenario to fill history of SSPs
    SSP_pop_0 %>%
      filter(scenario != "Historical Reference") %>%
      left_join(
        SSP_pop_0 %>% filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2020 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(year < socioeconomics.SSP_DB_BASEYEAR, hist, value)) %>%
      select(iso, scenario, year, pop = value) ->
      L100.Pop_thous_SSP_ctry_Yfut_0

    L100.Pop_thous_SSP_ctry_Yfut <-
      L100.Pop_thous_SSP_ctry_Yfut_0 %>%
      # need to have socioeconomics.SSP_DB_BASEYEAR in the data as the initial point for interpolation
      # otherwise 2021:2024 could be the same with 2025 (rule = 2 below)
      complete(nesting(scenario, iso),
               year = c(socioeconomics.SSP_DB_BASEYEAR:max(FUTURE_YEARS))) %>%
      filter(year %in% c(socioeconomics.SSP_DB_BASEYEAR:max(FUTURE_YEARS))) %>%
      group_by(scenario, iso) %>%
      # Data is in five year intervals, so interpolate so get data for the base-year before calculating ratios
      mutate(pop = approx_fun(year, pop, rule = 2),
             ratio_iso_ssp = pop / pop[year == socioeconomics.FINAL_HIST_YEAR]) %>%  # Calculate population ratios to final historical year (2010), no units
      select(-pop) %>%
      filter(year >= socioeconomics.FINAL_HIST_YEAR) %>%
      # Third, project country population values using SSP ratios and final historical year populations.
      # Not all countries in the UN data are in SSP data. Create complete tibble with all UN countries & SSP years.
      ungroup() %>%
      complete(scenario = unique(scenario),
               year = unique(year),
               iso = unique(L100.Pop_thous_ctry_Yh$iso)) %>%
      # For these countries, the ratio will be set to 1 (per the old data system).
      replace_na(list(ratio_iso_ssp = 1)) %>%
      ## Note: In the old data system, Taiwan is in this category and has constant population. Issue has been opened to deal with this later. ##
      right_join(pop_final_hist, by = "iso") %>% # Join with final historic period population
      mutate(value = pop_final_hist * ratio_iso_ssp) %>%  # Units are 1000 persons (UN 2010 value is in thousands)
      filter(year != socioeconomics.FINAL_HIST_YEAR) %>% # Keep only SSP future years
      select(-pop_final_hist, -ratio_iso_ssp)



    # ===================================================

    # Produce outputs
    L100.Pop_thous_ctry_Yh %>%
      add_title("Population by country, 1700-2010") %>%
      add_units("thousand") %>%
      add_comments("Maddison population data cleaned to develop complete data for all years, (dis)aggregated to modern country boundaries") %>%
      add_legacy_name("L100.Pop_thous_ctry_Yh") %>%
      add_precursors("socioeconomics/POP/iso_ctry_Maddison",
                     "socioeconomics/POP/Maddison_population") ->
      L100.Pop_thous_ctry_Yh

    L100.Pop_thous_SSP_ctry_Yfut %>%
      add_title("SSP population projections by country, 2010-2100") %>%
      add_units("thousand") %>%
      add_comments("Future population calculated as final historical year (2010) population times ratio of SSP future years to SSP 2010") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors("socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "socioeconomics/POP/UN_popTot") ->
      L100.Pop_thous_SSP_ctry_Yfut

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
