#' module_energy_LA118.hydro
#'
#' This chunk calculates hydro potential in EJ from 2010 to 2100 by GCAM region ID
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L118.out_EJ_R_elec_hydro_Yfut}. The corresponding file in the
#' original data system was \code{LA118.hydro.R} (energy level1).
#' @details Different proxies are used to calculate hydro potential.
#' @details In most cases, a growth potential for each country was calculated, multiplied by its share in the region, and added to the base-year ouput
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS May 2017
module_energy_LA118.hydro <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/Hydropower_potential",
             FILE = "temp-data-inject/L100.IEA_en_bal_ctry_hist",
             FILE = "energy/A18.hydro_output"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L118.out_EJ_R_elec_hydro_Yfut"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Hydropower_potential <- get_data(all_data, "energy/Hydropower_potential")

    get_data(all_data, "temp-data-inject/L100.IEA_en_bal_ctry_hist") %>%
      gather(year, value, -iso, -FLOW, -PRODUCT) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L100.IEA_en_bal_ctry_hist

    A18.hydro_output <- get_data(all_data, "energy/A18.hydro_output")

    # ===================================================

    # Calculation of economic hydropower potential by country, in EJ/yr
    # Calculate a capacity factor for translating MW to GWh, using weighted average capacity factor of all existing dams
    Hydropower_potential %>%
      gather(variable, value, -Country, -iso) %>%
      group_by(variable) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      # Calculation of capacity factor
      summarise(value = value[variable == "Installed_GWh"] / (value[variable == "Installed_MW"] * CONV_YEAR_HOURS * CONV_MIL_BIL)) %>%
      .$value -> # Convert table to single number
      Hydro_capfac

    # Economic potential is what we are interested in from this database; however it is often not reported. Many countries without reported
    # economic potential nevertheless have estimates of the technical potential.
    # Calculate a translation from Technical potential to Economic potential, using weighted average among regions where both are reported
    # First, for countries reporting in MW, convert to GWh (most countries have potentials as GWh but some are in MW)
    Hydropower_potential %>%
      mutate_if(is.integer, as.numeric) %>% # Convert columns that are getting read in as integers to numbers
      mutate(Technical_GWh = if_else(is.na(Technical_GWh), Technical_MW * CONV_YEAR_HOURS * CONV_MIL_BIL * Hydro_capfac, Technical_GWh)) %>%
      mutate(Economic_GWh = if_else(is.na(Economic_GWh), Economic_MW * CONV_YEAR_HOURS * CONV_MIL_BIL * Hydro_capfac, Economic_GWh)) ->
      Hydropower_potential

    # Among countries with both technical and economic potential reported, calculate an average translation from one to the other
    Hydropower_potential %>%
      summarise(value = sum(Economic_GWh[!is.na(Technical_GWh) & !is.na(Economic_GWh)]) / sum(Technical_GWh[!is.na(Technical_GWh) & !is.na(Economic_GWh)])) %>%
      .$value -> # Convert table to single number
      Hydro_tech_econ

    # For countries with technical potential reported but no economic potential, estimate the economic potential
    Hydropower_potential %>%
      mutate(Economic_GWh = if_else(is.na(Economic_GWh), Technical_GWh * Hydro_tech_econ, Economic_GWh)) %>%
      # This still leaves a few countries for which no estimates of hydro potentials are provided.
      # The largest among them is North Korea; most of the others are islands or deserts that probably don't have much beyond present-day production.
      # Not worrying about these (future growth rates will be set to 0)
      mutate(Economic_EJ = Economic_GWh * CONV_GWH_EJ) %>% # Convert GWh to EJ
      select(iso, Economic_EJ) -> # Stripping down unneeded information from table
      Hydropower_potential

    # Calculate the growth potential by country, as the economic potential minus the actual generation in the most recent historical year (from the IEA balances)
      L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW == "ELOUTPUT", PRODUCT == "Hydro", year == max(HISTORICAL_YEARS)) %>%
        mutate(value_base = value * CONV_GWH_EJ) %>%
        left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
        select(iso, region_GCAM3, year, value_base) ->
        L118.out_EJ_ctry_elec_hydro_fby

      L118.out_EJ_ctry_elec_hydro_fby %>%
        group_by(region_GCAM3, year) %>%
        summarise(value = sum(value_base)) %>%
        ungroup() ->
        L118.out_EJ_RG3_elec_hydro_fby

    # Calculate the future growth potential in each country as the economic potential minus the present-day generation
    Hydropower_potential %>%
      left_join(L118.out_EJ_ctry_elec_hydro_fby, by = "iso") %>%
      mutate(Growth_potential_EJ = Economic_EJ - value_base) %>%
      mutate(Growth_potential_EJ = if_else(is.na(Growth_potential_EJ) | Growth_potential_EJ < 0, 0, Growth_potential_EJ)) %>%
      select(-region_GCAM3, -Economic_EJ, -value_base) %>%
      left_join(
        select(iso_GCAM_regID, -country_name, -GCAM_region_ID), # Some countries (e.g., Bostwana) have NAs for RG3 names. This step is updating them (except Kosovo).
        by = "iso") ->
      Hydropower_potential

    # Downscaling of GCAM 3.0 scenario to the country level: Assign future growth according to shares of growth potential by country
    # Interpolate any between years, and use the final year as a proxy for years thereafter
    # Drop 2020 because it is lower than 2010 in many regions
    L118.out_EJ_RG3_elec_hydro_fby %>%
      bind_rows(
        mutate(year = as.integer(year),
          gather(A18.hydro_output, year, value, -region_GCAM3))) %>%
      filter(year != 2020) -> # Dropping 2020, as described above
      L118.out_EJ_RG3_elec_hydro_Y_with_values

    L118.out_EJ_RG3_elec_hydro_fby %>%
      select(region_GCAM3) %>%
      repeat_add_columns(tibble::tibble(year = c(max(HISTORICAL_YEARS), FUTURE_YEARS))) %>% # Years include historical max and future
      left_join(L118.out_EJ_RG3_elec_hydro_Y_with_values, by = c("region_GCAM3", "year")) %>%
      group_by(region_GCAM3) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>% # Interpolation step
      ungroup() ->
      L118.out_EJ_RG3_elec_hydro_Y_interp

    # Calculate the growth from the base year in each region_GCAM3. This will be partitioned to countries according to potential
    L118.out_EJ_RG3_elec_hydro_Y_interp %>%
      filter(year != max(HISTORICAL_YEARS)) %>% # Deleting historical max year to be used in another column
      left_join(
        select(filter(L118.out_EJ_RG3_elec_hydro_Y_interp, year == max(HISTORICAL_YEARS)), -year), # Adding historical max year column so as to add base value to future years
        by = "region_GCAM3") %>%
      mutate(value_growth = value.x - value.y) %>%
      select(-value.x, -value.y) ->
      L118.growth_EJ_RG3_elec_hydro_Y

    # Calculate the share of the growth potential by each country within its GCAM 3.0 region
    Hydropower_potential %>%
      group_by(region_GCAM3) %>%
      summarise(Growth_potential_EJ_R = sum(Growth_potential_EJ)) %>%
      filter(!is.na(region_GCAM3)) -> # Getting rid of the NA caused by Kosova not having a RG3 name
      Hydropower_potential_RG3 # Represents total pie of each country

    Hydropower_potential %>%
      left_join(Hydropower_potential_RG3, by = "region_GCAM3") %>%
      mutate(share = Growth_potential_EJ / Growth_potential_EJ_R) %>% # Calculating share
    # Add future years to table of base-year hydropower generation as base-year output plus region_GCAM3 growth times country-wise share
      select(iso, share) %>% # Need only share of each iso
      group_by(iso) %>%
      summarise(share = sum(share)) -> # Summing up Serbia...was listed twice
      Hydropower_potential

      # Adding future years
    L118.out_EJ_ctry_elec_hydro_fby %>%
      left_join(Hydropower_potential, by = "iso") %>% # Adding share column
      left_join(L118.growth_EJ_RG3_elec_hydro_Y, by = "region_GCAM3") %>% # Adding RG3 growth
      mutate(value_future = value_base + share * value_growth) %>% # Base-year output plus RG3 growth times country-wise share
      select(-share, -value_growth) %>%
      spread(year.x, value_base) %>%
      spread(year.y, value_future) %>%
      gather(year, value, -iso, -region_GCAM3) ->
      L118.out_EJ_ctry_elec_hydro_Y

    # For countries not in the world dams database (all are very small), copy final historical year forward
      # First create list to use as filter
    iso_list <- Hydropower_potential$iso

    L118.out_EJ_ctry_elec_hydro_Y %>%
      filter(!iso %in% iso_list) %>% # Filtering for what's excluded from list
      arrange(iso, year) %>%
      fill(value) -> # Copying final historical year forward
      countries_not_in_WDdatabase

      # Adding these countries not in the world dams database back in
    L118.out_EJ_ctry_elec_hydro_Y %>%
      filter(iso %in% iso_list) %>%
      bind_rows(countries_not_in_WDdatabase) ->
      L118.out_EJ_ctry_elec_hydro_Y

    # Aggregate by region
    L118.out_EJ_ctry_elec_hydro_Y %>%
      left_join_error_no_match( # Adding GCAM region ID back in for final table
        select(iso_GCAM_regID, iso, GCAM_region_ID),
        by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      mutate(sector = "electricity generation", fuel = "hydro") %>%
      select(GCAM_region_ID, sector, fuel, year, value) ->
      L118.out_EJ_R_elec_hydro_Yfut

    # ===================================================

    L118.out_EJ_R_elec_hydro_Yfut %>%
      add_title("L118.out_EJ_R_elec_hydro_Yfut") %>%
      add_units("EJ") %>%
      add_comments("Hydro potential was determined using various proxies") %>%
      add_comments("In most cases, a growth potential for each country was calculated,
                   multiplied by its share in the region, and added to the base-year ouput") %>%
      add_legacy_name("L118.out_EJ_R_elec_hydro_Yfut") %>%
      add_precursors("common/iso_GCAM_regID", "energy/Hydropower_potential",
                     "temp-data-inject/L100.IEA_en_bal_ctry_hist", "energy/A18.hydro_output") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L118.out_EJ_R_elec_hydro_Yfut

    return_data(L118.out_EJ_R_elec_hydro_Yfut)
  } else {
    stop("Unknown command")
  }
}
