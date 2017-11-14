#' module_socioeconomics_L100.Population_downscale_ctry
#'
#'  Clean and interpolate both Maddison historical population data (1700-2010) and SSP population scenarios.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_ctry_Yh}, \code{L100.Pop_thous_SSP_ctry_Yfut}. The corresponding file in the
#' original data system was \code{L100.Population_downscale_ctry.R} (socioeconomics level1).
#' @details (1) Cleans Maddison historical population data and interpolates to country and year (1700-2010). (2) Cleans SSP population scenarios for smooth join with final base year population.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author STW May 2017
module_socioeconomics_L100.Population_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/socioeconomics_ctry",
             "Maddison_population",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/UN_popTot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    Country <- value <- Maddison_ctry <- year <- pop <- Downscale_from <- ratio <-
      year.x <- iso <- pop_scale <- pop2 <- pop.x <- pop.y <- pop_allocate <- X1900 <-
      X1950 <- X1850 <- X1800 <- X1750 <- X1700 <- pop_ratio <- scg <-
      idn <- mne <- Scenario <- Region <- Sex <- Year <- Value <- MODEL <-
      VARIABLE <- REGION <- SCENARIO <- UNIT <- scenario <- ratio_iso_ssp <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    socioeconomics_ctry <- get_data(all_data, "socioeconomics/socioeconomics_ctry")
    Maddison_population <- get_data(all_data, "Maddison_population")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    UN_popTot <- get_data(all_data, "socioeconomics/UN_popTot")

    # ===================================================

    ## (1) Historical population by country

    # First clean up Maddison raw data -- NOTE: Maddison data are used to develop population ratios relative to 1950 to combine with UN data from 1950 onward
    pop_thous_ctry_reg <- Maddison_population %>%
      rename(Maddison_ctry = Country, pop = value) %>%  # Change name to match socioeconomics_ctry mapping file
      left_join(socioeconomics_ctry, by = "Maddison_ctry") # Join with iso codes

    # Second, estimate population values prior to 1950 for countries in aggregate regions. This is what we want: pop_country_t = (pop_aggregate_t / pop_aggregate_1950) * pop_country_1950
    # Generate a scalar for population in each aggregate region in 1950 (to generate the population ratios)
    agg_ratio <- pop_thous_ctry_reg %>%
      select(Maddison_ctry, year, pop)
    # OLD DATA SYSTEM BEHAVIOR: There is one additional aggregate region (Eritrea & Ethiopia) that wasn't included in the downscaling. New behavior includes it.
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      agg_ratio <- filter(agg_ratio, year <= min(socioeconomics.UN_HISTORICAL_YEARS) &
                            Maddison_ctry %in% c("Total Former USSR", "Czechoslovakia", "Yugoslavia"))  # Only want years prior to 1951 for the three regions
      # Because we changed the mapping file, adding in Eritrea & Ethiopia, for old behavior we need to strip these back out from pop_thous_ctry_reg
      pop_thous_ctry_reg <- mutate(pop_thous_ctry_reg, Downscale_from = gsub("Eritrea and Ethiopia", NA, Downscale_from))
    } else {
      agg_ratio <- filter(agg_ratio, year <=  min(socioeconomics.UN_HISTORICAL_YEARS) &
                            Maddison_ctry %in% c("Czechoslovakia", "Eritrea and Ethiopia", "Total Former USSR", "Yugoslavia")) # Only want years prior to 1951 for the four aggregate regions
    }
    agg_ratio <- agg_ratio %>%
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
      mutate(pop = if_else((year < min(socioeconomics.UN_HISTORICAL_YEARS) & !is.na(pop_scale)), pop_scale, pop)) %>% # Replace NA values for countries with downscaled population values
      mutate(iso = if_else(Maddison_ctry == "World Total", "world_total", iso)) %>% # Will need total world population in step 4
      filter(!is.na(iso)) %>%  # Remove rows with no iso code (generally aggregate regions)
      select(iso, year, pop)  # Keep only necessary variables

    # Third, interpolate available population data to generate values for required historical years (1700, 1750, 1800, 1850, 1900)
    hist_interp <- tibble(year = socioeconomics.MADDISON_HISTORICAL_YEARS) %>%  # Create tibble with required years (not all are in the original Maddison data)
      full_join(reg_scaled, by = "year") %>%  # Join with available population data (creates NAs for years not available in raw data)
      complete(year, nesting(iso)) %>%  # Completes tibble to include all years for all iso (creates missing values)
      filter(!is.na(iso)) %>%  # iso NA values created with complete on year and iso table
      group_by(iso) %>%
      mutate(pop = approx_fun(year, pop)) %>%  # Interpolate -- note that there will still be missing values for countries that do not have end values (1500, 1600, or 1700)
      mutate(pop = if_else(is.na(pop) & year == 1800, pop[year == 1820], pop)) %>%  # Replace missing 1800 values with 1820 for countries that begin in 1820 (note that Panama has a value in 1820 = 0, not NA)
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
        mutate(pop2 = if_else(is.na(pop), (lead(pop, n = 1L, order_by = iso)), 0)) %>%
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

    # Need to create matching iso codes for three countries in UN, but not Maddison.
    # Set Serbia and Montenegro ratio equal to Serbia & Montenegro and use Indonesia population ratio for East Timor
    mne_srb_tls <- filter(maddison_hist_ratio, iso %in% c("idn", "scg")) %>%
      mutate(iso = replace(iso, iso == "idn", "tls"), iso = replace(iso, iso == "scg", "mne"))

    # Combine with other ratio_iso values
    maddison_hist_ratio <- maddison_hist_ratio %>%
      bind_rows(mne_srb_tls) %>%
      mutate(iso = replace(iso, iso == "scg", "srb"))

    # Sixth, apply Maddison ratios for historic periods to UN populatio data that begin in 1950
    # Clean raw UN population data
    un_clean <- UN_popTot %>%
      filter(Scenario == "EST") %>%  # "EST" is the UN historical estimates, not projections
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
      rename(value = pop)

    ## (2) SSP population projections by country

    # First, extract the final historical population from UN
    pop_final_hist <- filter(L100.Pop_thous_ctry_Yh, year == socioeconomics.FINAL_HIST_YEAR) %>%
      rename(pop_final_hist = value) %>%
      select(-year)

    # Second, generate ratios of future population to base year (2010) for all SSPs. The ratios will be applied to the historical year populations so there are no jumps/inconsistencies.
    L100.Pop_thous_SSP_ctry_Yfut <- SSP_database_v9 %>% # Note units in SSP database are millions, but convert to thousands when we multiply by historic year
      filter(MODEL == "IIASA-WiC POP", VARIABLE == "Population") %>%  # IIASA-WiC is the official SSP population data set
      mutate(iso = tolower(REGION),
             scenario = substr(SCENARIO, 1, 4)) %>%
      select(-MODEL, -VARIABLE, -UNIT, -REGION, -SCENARIO) %>%
      mutate(iso = gsub("rou", "rom", iso)) %>%  # SSP uses "rou" for the iso for Romania; replace with "rom" for consistency with other data sources
      gather_years(value_col = "pop") %>%  # Long format
      mutate(pop = as.numeric(pop)) %>%  # Clean year variable
      filter(year %in% c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>% # Retain only years needed for GCAM
      group_by(scenario, iso) %>%
      mutate(ratio_iso_ssp = pop / pop[year == socioeconomics.FINAL_HIST_YEAR]) %>%  # Calculate population ratios to final historical year (2010), no units
      select(-pop) %>%
      # Third, project country population values using SSP ratios and final historical year populations.
      # Not all countries in the UN data are in SSP data. Create complete tibble with all UN countries & SSP years.
      ungroup() %>%
      complete(scenario = unique(scenario),
               year = unique(year),
               iso = unique(L100.Pop_thous_ctry_Yh$iso)) %>%
      # For these countries, the ratio will be set to 1 (per the old data system).
      replace_na(list(ratio_iso_ssp = 1)) %>%
      ## Note: In the old data system, Taiwan is in this category and has constant population. Issue has been opened to deal with this later. ##
      left_join(pop_final_hist, by = "iso") %>% # Join with final historic period population
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
      add_precursors("socioeconomics/socioeconomics_ctry", "Maddison_population") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_ctry_Yh

    L100.Pop_thous_SSP_ctry_Yfut %>%
      add_title("SSP population projections by country, 2010-2100") %>%
      add_units("thousand") %>%
      add_comments("Future population calculated as final historical year (2010) population times ratio of SSP future years to SSP 2010") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors("socioeconomics/socioeconomics_ctry", "socioeconomics/SSP_database_v9", "socioeconomics/UN_popTot") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L100.Pop_thous_SSP_ctry_Yfut

    return_data(L100.Pop_thous_ctry_Yh, L100.Pop_thous_SSP_ctry_Yfut)
  } else {
    stop("Unknown command")
  }
}
