# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA118.hydro
#'
#' Calculate hydro potential in EJ from 2010 to 2100 by GCAM region ID
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
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate pull select summarise
#' @importFrom tidyr fill spread
#' @author AS May 2017
module_energy_LA118.hydro <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/Hydropower_potential",
             FILE = "energy/mappings/IEA_product_fuel",
             "L100.IEA_en_bal_ctry_hist",
             FILE = "energy/A18.hydro_output"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L118.out_EJ_R_elec_hydro_Yfut"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Hydropower_potential <- get_data(all_data, "energy/Hydropower_potential")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    A18.hydro_output <- get_data(all_data, "energy/A18.hydro_output")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist")

    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running
    # without the proprietary IEA data files). If this is the case, we substitute a
    # pre-built output dataset and exit.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      L118.out_EJ_R_elec_hydro_Yfut <- extract_prebuilt_data("L118.out_EJ_R_elec_hydro_Yfut")
    } else {
      L100.IEA_en_bal_ctry_hist %>%
        gather_years ->
        L100.IEA_en_bal_ctry_hist

      # ===================================================

      iso <- region_GCAM3 <- year <- value_base <- value_growth <- value_max_hist <-
        value <- Economic_EJ <- country_name <- GCAM_region_ID <- share <- sector <-
        fuel <- Technical_GWh <- Economic_GWh <-  Installed_GWh <- Installed_MW <-
        FLOW <- PRODUCT <- .  <- Technical_MW <- Economic_MW <- Growth_potential_EJ <-
        value_interpolated <- Growth_potential_EJ_sum <- year.x <- year.y <- year_base <-
        year_future <- value_future <- NULL   # silence package check notes

      # Historical years to include
      # Under normal situation, max(HISTORICAL_YEARS) is before MODEL_FUTURE_YEARS
      # so we only include max(HISTORICAL_YEARS)
      # If we are hindcasting, this will also include MODEL_FUTURE_YEARS before max(HISTORICAL_YEARS)
      HYDRO_HIST_YEARS <- union(intersect(HISTORICAL_YEARS, MODEL_FUTURE_YEARS), max(HISTORICAL_YEARS))

      # Calculation of economic hydropower potential by country, in EJ/yr
      # Calculate a capacity factor for translating MW to GWh, using weighted average capacity factor of all existing dams
      Hydropower_potential %>%
        select(Installed_GWh, Installed_MW) %>%
        summarise(Installed_GWh = sum(Installed_GWh), Installed_MW = sum(Installed_MW)) %>%
        mutate(value = Installed_GWh / (Installed_MW * CONV_YEAR_HOURS * CONV_MIL_BIL)) %>%
        pull(value) -> # Convert table to single number
        Hydro_capfac

      # Economic potential is what we are interested in from this database; however it is often not reported. Many countries without reported
      # economic potential nevertheless have estimates of the technical potential.
      # Calculate a translation from Technical potential to Economic potential, using weighted average among regions where both are reported
      # First, for countries reporting in MW, convert to GWh (most countries have potentials as GWh but some are in MW)
      Hydropower_potential %>%
        dplyr::mutate_if(is.integer, as.numeric) %>% # Convert columns that are getting read in as integers to numbers
        mutate(Technical_GWh = replace(Technical_GWh, is.na(Technical_GWh), (Technical_MW * CONV_YEAR_HOURS * CONV_MIL_BIL * Hydro_capfac)[is.na(Technical_GWh)]),
               Economic_GWh = replace(Economic_GWh, is.na(Economic_GWh), (Economic_MW * CONV_YEAR_HOURS * CONV_MIL_BIL * Hydro_capfac)[is.na(Economic_GWh)])) ->
        Hydropower_potential

      # Among countries with both technical and economic potential reported, calculate an average translation from one to the other
      Hydropower_potential %>%
        filter(!is.na(Technical_GWh), !is.na(Economic_GWh)) %>%
        summarise(value = sum(Economic_GWh) / sum(Technical_GWh)) %>%
        pull(value) -> # Convert table to single number
        Hydro_tech_econ

      # For countries with technical potential reported but no economic potential, estimate the economic potential
      Hydropower_potential %>%
        mutate(Economic_GWh = replace(Economic_GWh, is.na(Economic_GWh), (Technical_GWh * Hydro_tech_econ)[is.na(Economic_GWh)])) %>%
        # This still leaves a few countries for which no estimates of hydro potentials are provided.
        # The largest among them is North Korea; most of the others are islands or deserts that probably don't have much beyond present-day production.
        # Not worrying about these (future growth rates will be set to 0)
        mutate(Economic_EJ = Economic_GWh * CONV_GWH_EJ) %>% # Convert GWh to EJ
        select(iso, Economic_EJ) -> # Stripping down unneeded information from table
        Hydropower_potential

      # Calculate the growth potential by country, which is the economic potential minus
      # the actual generation in the most recent historical year (from the IEA balances)
      L100.IEA_en_bal_ctry_hist %>%
        left_join(rename(IEA_product_fuel, PRODUCT = product), by = "PRODUCT") %>%
        filter(FLOW == "ELOUTPUT", fuel == "elec_hydro", year %in% HYDRO_HIST_YEARS) %>%
        mutate(value_base = value * CONV_GWH_EJ) %>%
        left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
        group_by(iso, region_GCAM3, year) %>%
        summarise(value_base = sum(value_base)) %>%
        ungroup()->
        L118.out_EJ_ctry_elec_hydro_fby

      # Aggregate by region
      L118.out_EJ_ctry_elec_hydro_fby %>%
        group_by(region_GCAM3, year) %>%
        summarise(value = sum(value_base)) %>%
        ungroup() ->
        L118.out_EJ_RG3_elec_hydro_fby

      # Calculate the future growth potential in each country as the economic potential minus the present-day generation
      Hydropower_potential %>%
        left_join(L118.out_EJ_ctry_elec_hydro_fby, by = "iso") %>%
        filter(year == max(HISTORICAL_YEARS)) %>%
        mutate(Growth_potential_EJ = Economic_EJ - value_base,
               Growth_potential_EJ = if_else(is.na(Growth_potential_EJ) | Growth_potential_EJ < 0, 0, Growth_potential_EJ)) %>%
        select(-region_GCAM3, -Economic_EJ, -value_base) %>%
        # Some countries (e.g., Bostwana) have NAs for RG3 names. This step is updating them (except Kosovo).
        left_join(select(iso_GCAM_regID, -country_name, -GCAM_region_ID), by = "iso") ->
        Hydropower_potential

      # Downscaling of GCAM 3.0 scenario to the country level: Assign future growth according to shares of growth potential by country
      # Interpolate any between years, and use the final year as a proxy for years thereafter

      # First, convert A18.hydro_output to long form
      A18.hydro_output %>%
        gather_years ->
        A18.hydro_output_long

      # Now combine with L118.out_EJ_RG3_elec_hydro_fby
      L118.out_EJ_RG3_elec_hydro_fby %>%
        bind_rows(A18.hydro_output_long) ->
        L118.out_EJ_RG3_elec_hydro_Y_with_values

      # Create a table from HYDRO_HIST_YEARS to 2100 in all "FUTURE_YEARS" and interpolate for the missing values
      L118.out_EJ_RG3_elec_hydro_fby %>%
        distinct(region_GCAM3) %>%
        repeat_add_columns(tibble::tibble(year = c(HYDRO_HIST_YEARS, FUTURE_YEARS))) %>% # Years include HYDRO_HIST_YEARS and future
        left_join(L118.out_EJ_RG3_elec_hydro_Y_with_values, by = c("region_GCAM3", "year")) %>%
        group_by(region_GCAM3) %>%
        mutate(value_interpolated = approx_fun(year, value, rule = 2)) %>% # Interpolation step
        ungroup() %>%
        select(-value) ->
        L118.out_EJ_RG3_elec_hydro_Y_interp

      # Calculate the growth from the base year in each region_GCAM3. This will be partitioned to countries according to potential
      # First, separate historical max year to use added later as its own column
      L118.out_EJ_RG3_elec_hydro_Y_interp %>%
        filter(year == max(HISTORICAL_YEARS)) %>%
        rename(value_max_hist = value_interpolated) %>%
        select(-year) ->
        L118.out_EJ_RG3_elec_hydro_Y_interp_maxhist

      # Now add the historical max year as its own column, and delete in interpolated column
      L118.out_EJ_RG3_elec_hydro_Y_interp %>%
        filter(!year %in% HYDRO_HIST_YEARS) %>% # Deleting historical years to be used in another column
        left_join(L118.out_EJ_RG3_elec_hydro_Y_interp_maxhist, by = "region_GCAM3") %>%
        # Calculate growth, which is total value (interpolated) minus the base (historical max)
        # For regions whose 2015 output is higher than GCAM3's estimated 2100 output, don't allow the output to decrease
        mutate(value_growth = pmax(0, value_interpolated - value_max_hist)) %>%
        select(-value_interpolated, -value_max_hist) ->
        L118.growth_EJ_RG3_elec_hydro_Y

      # Calculate the share of the growth potential by each country within its GCAM 3.0 region
      Hydropower_potential %>%
        group_by(region_GCAM3) %>%
        summarise(Growth_potential_EJ_sum = sum(Growth_potential_EJ)) %>%
        ungroup() %>%
        filter(!is.na(region_GCAM3)) -> # Getting rid of the NA caused by Kosova not having a RG3 name
        Hydropower_potential_RG3 # Represents total pie of each country

      # Here the countries' shares will be calculated
      Hydropower_potential %>%
        left_join(Hydropower_potential_RG3, by = "region_GCAM3") %>%
        mutate(share = Growth_potential_EJ / Growth_potential_EJ_sum) %>% # Calculating share
        # Add future years to table of base-year hydropower generation as base-year output plus region_GCAM3 growth times country-wise share
        select(iso, share) %>% # Need only share of each iso
        group_by(iso) %>%
        summarise(share = sum(share)) -> # Summing up Serbia...was listed twice
        Hydropower_potential

      # Adding future years
      L118.out_EJ_ctry_elec_hydro_fby %>%
        filter(year == max(HISTORICAL_YEARS)) %>%
        left_join(Hydropower_potential, by = "iso") %>% # Adding share column
        left_join(L118.growth_EJ_RG3_elec_hydro_Y, by = "region_GCAM3") %>% # Adding RG3 growth
        mutate(value_future = value_base + share * value_growth) %>% # Base-year output plus RG3 growth times country-wise share
        select(-share, -value_growth, -year.x, -value_base,
               year = year.y, value = value_future) %>%
        # Insert back historical years
        bind_rows(rename(L118.out_EJ_ctry_elec_hydro_fby, value = value_base)) %>%
        arrange(iso, year) ->
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
        ungroup %>%
        mutate(sector = "electricity generation", fuel = "hydro") %>%
        select(GCAM_region_ID, sector, fuel, year, value) %>%
        add_title("L118.out_EJ_R_elec_hydro_Yfut") ->
        L118.out_EJ_R_elec_hydro_Yfut

      # ===================================================

      L118.out_EJ_R_elec_hydro_Yfut %>%
        add_units("EJ") %>%
        add_comments("Hydro potential was determined using various proxies") %>%
        add_comments("In most cases, a growth potential for each country was calculated,
                   multiplied by its share in the region, and added to the base-year ouput") %>%
        add_legacy_name("L118.out_EJ_R_elec_hydro_Yfut") %>%
        add_precursors("common/iso_GCAM_regID", "energy/Hydropower_potential", "energy/mappings/IEA_product_fuel",
                       "L100.IEA_en_bal_ctry_hist", "energy/A18.hydro_output") ->
        L118.out_EJ_R_elec_hydro_Yfut

      # At this point output should be identical to the prebuilt version
      verify_identical_prebuilt(L118.out_EJ_R_elec_hydro_Yfut)
    }

    return_data(L118.out_EJ_R_elec_hydro_Yfut)
  } else {
    stop("Unknown command")
  }
}
