#' module_energy_LA144.building_det_flsp
#'
#' This chunk calculates residential and commercial floorspace - and floorspace prices - by GCAM region and historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_R_res_Yh}, \code{L144.flsp_bm2_R_comm_Yh}, \code{L144.flspPrice_90USDm2_R_bld_Yh}. The corresponding file in the
#' original data system was \code{LA144.building_det_flsp.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS July 2017
#' @export
module_energy_LA144.building_det_flsp <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A44.flsp_bm2_state_res",
             FILE = "energy/A44.flsp_bm2_state_comm",
             FILE = "energy/A44.pcflsp_default",
             FILE = "energy/A44.HouseholdSize",
             FILE = "energy/CEDB_ResFloorspace_chn",
             FILE = "energy/Other_pcflsp_m2_ctry_Yh",
             FILE = "energy/IEA_PCResFloorspace",
             FILE = "energy/Odyssee_ResFloorspacePerHouse",
             "L100.Pop_thous_ctry_Yh",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_R_res_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             "L144.flspPrice_90USDm2_R_bld_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A44.flsp_bm2_state_res <- get_data(all_data, "energy/A44.flsp_bm2_state_res")
    A44.flsp_bm2_state_comm <- get_data(all_data, "energy/A44.flsp_bm2_state_comm")
    A44.pcflsp_default <- get_data(all_data, "energy/A44.pcflsp_default")
    A44.HouseholdSize <- get_data(all_data, "energy/A44.HouseholdSize")
    CEDB_ResFloorspace_chn <- get_data(all_data, "energy/CEDB_ResFloorspace_chn")
    Other_pcflsp_m2_ctry_Yh <- get_data(all_data, "energy/Other_pcflsp_m2_ctry_Yh")
    IEA_PCResFloorspace <- get_data(all_data, "energy/IEA_PCResFloorspace")
    Odyssee_ResFloorspacePerHouse <- get_data(all_data, "energy/Odyssee_ResFloorspacePerHouse")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
                                            gather(year, value, -GCAM_region_ID) %>%
                                            mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================

    # Silence package notes
    year <- value <- iso <- NULL


    # FLOORSPACE CALCULATION

    Odyssee_ResFloorspacePerHouse_long <- gather(Odyssee_ResFloorspacePerHouse, year, value_phflsp, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year))

    #IEA_PCResFloorspace_long <- gather(IEA_PCResFloorspace, year, value_bm2, matches(YEAR_PATTERN)) %>%
      #mutate(year = as.integer(year))

    IEA_PCResFloorspace_long <- gather(IEA_PCResFloorspace, year, value_pcflsp, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year),
             value_pcflsp = as.numeric(value_pcflsp))

    Other_pcflsp_m2_ctry_Yh_long <- gather(Other_pcflsp_m2_ctry_Yh, year, value_pcflsp, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year))

    A44.flsp_bm2_state_res_long <- gather(A44.flsp_bm2_state_res, year, value_bm2, -state, -GCAM_sector) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    A44.pcflsp_default_long <- gather(A44.pcflsp_default, year, value_pcflsp, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year))

    A44.flsp_bm2_state_comm_long <- gather(A44.flsp_bm2_state_comm, year, value_bm2, -state, -GCAM_sector) %>%
      mutate(year = as.integer(substr(year, 2, 5)))


    # China - divide by population and extrapolate to all historical years
    CEDB_ResFloorspace_chn %>%
      gather(year, value_CHNfloorspace, -country, -iso) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      # Divide by population to get per capita
      mutate(value_pcflsp = value_CHNfloorspace / value * 1e6) %>% # change to CONV_BIL_THOUS
      select(iso, year, value_pcflsp) %>%
      mutate(year = as.numeric(year)) %>%
      complete(year = HISTORICAL_YEARS,
               iso = "chn") %>%
      # Extrapolate to all historical years
      group_by(iso) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() ->
      L144.pcflsp_m2_chn_Yh


    # First, fill out the household size to all relevant years
    # Use year range from Odyssee data
    Odyssee_ResFloorspacePerHouse %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.numeric(year)) %>%
      .[["year"]] %>%
      unique() ->
      Odyssee_flsp_years

    # Expand table of persons/dwelling to Odyssee years
    A44.HouseholdSize %>%
      select(-Variable, -Unit) %>%
      gather(year, value_pcdwelling) %>%
      mutate(year = as.numeric(year)) %>%
      complete(year = Odyssee_flsp_years,
               fill = list(value_pcdwelling = NA)) %>%
      # Extrapolate to all Odyssee years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcdwelling = approx_fun(year, value_pcdwelling, rule = 2)) %>%
      mutate(year = as.integer(year)) ->
      L144.HouseholdSize # 1973 was dropped, may have to roll it back in with rbind

    # Drop the Odyssee countries that we have estimates for in the IEA dataset (IEA assumed to be better as it presents per-capita flsp)
    Odyssee_ResFloorspacePerHouse_long %>%
      mutate(year = as.numeric(year)) %>%
      .[["year"]] %>%
      unique() ->
      Odyssee_flsp_years

    list_iso_IEAflsp <- unique(IEA_PCResFloorspace_long$iso)
    list_years_IEAflsp <- unique(IEA_PCResFloorspace_long$year)

    Odyssee_ResFloorspacePerHouse_long %>%
      filter(!iso %in% list_iso_IEAflsp) ->
      L144.Odyssee_phflsp_Yh

    L144.Odyssee_phflsp_Yh %>%
      left_join(L144.HouseholdSize, by = "year") %>%
      mutate(value_pcflsp = value_phflsp / value_pcdwelling) %>%
      select(iso, year, value_pcflsp) ->
      L144.Odyssee_pcflsp_Yh

    L144.Odyssee_pcflsp_Yh %>%
      bind_rows(
        select(IEA_PCResFloorspace_long, iso, year, value_pcflsp)) %>%
      # Drop any countries with all missing values
      filter(!is.na(value_pcflsp)) %>%
      group_by(iso) %>%
      complete(year = list_years_IEAflsp) %>%
      ungroup() -> # also got rid of some later years
      L144.OECD_pcflsp_Yh


    # Fill out missing values in specified countries
    L144.OECD_pcflsp_Yh %>%
      # Derived from RECS; see RGCAM data system for documentation
      mutate(value_pcflsp = replace(value_pcflsp, iso == "usa" & year == "1980", 49.5)) %>%
      spread(year, value_pcflsp) %>%
      mutate(`1990` = replace(`1990`, is.na(`1990`), `1991`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1992`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1994`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1995`[is.na(`1990`)])) ->
      # gather(year, value_pcflsp, -iso) ->
      L144.OECD_pcflsp_Yh_2

    # Calculate average 1980-1990 growth rates for countries with 1980 data. Apply this to the 1990 data to return estimated 1980 floorspace
    L144.OECD_pcflsp_Yh_2 %>%
      filter(!is.na(`1980`)) %>%
      summarise(`1980` = sum(`1980`),
                `1990` = sum(`1990`)) %>%
      mutate(growthrate_1980_1990 = `1990` / `1980`) %>%
      .[["growthrate_1980_1990"]] -> # Save as single value
      growthrate_1980_1990

    L144.OECD_pcflsp_Yh_2 %>%
      mutate(`1980` = replace(`1980`, is.na(`1980`), `1990`[is.na(`1980`)] / growthrate_1980_1990)) ->
      L144.OECD_pcflsp_Yh_3

    # Fill out australia and belgium 2004 data
         # The ratio for USA is used as a proxy for Australia, and France for Belgium
    L144.OECD_pcflsp_Yh_3 %>%
      filter(iso == "usa") %>%
      mutate(value_usa_ratio = `2004` / `1998`) %>%
      .[["value_usa_ratio"]] ->
      value_usa_ratio

    L144.OECD_pcflsp_Yh_3 %>%
      filter(iso == "fra") %>%
      mutate(value_usa_ratio = `2004` / `2001`) %>%
      .[["value_usa_ratio"]] ->
      value_fra_ratio

    L144.OECD_pcflsp_Yh_3 %>%
      mutate(`2004` = replace(`2004`, iso == "aus", (`1998` * value_usa_ratio)[iso == "aus"]),
             `2004` = replace(`2004`, iso == "bel", (`2001` * value_usa_ratio)[iso == "bel"])) ->
      L144.OECD_pcflsp_Yh_4

    # Interpolate and extrapolate the time series to all historical years
    L144.OECD_pcflsp_Yh_4 %>%
      gather(year, value_pcflsp, -iso) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(iso) %>%
      mutate(value_pcflsp = approx_fun(year, value_pcflsp)) %>% # Interpolation step
      ungroup() ->
      L144.OECD_pcflsp_Yh_5

    # Time series doesn't span entire "historical" range; need to extrapolate
    # Just for now, use constant floorspace outside of available time series
    Other_pcflsp_m2_ctry_Yh_long %>%
      filter(gcam.consumer == "resid") %>%
      mutate(year = as.numeric(year)) %>% # Convert year to numeric as needed by the interpolation function.
      # Extrapolate to all historical years
      select(iso, year, value_pcflsp) %>%
      complete(year = HISTORICAL_YEARS,
               iso = "zaf") %>%
      #group_by(iso) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) ->
      L144.pcflsp_m2_otherctry_Yh_long_2

    L144.OECD_pcflsp_Yh_5 %>%
      group_by(iso) %>%
      complete(year = HISTORICAL_YEARS) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() ->
      L144.OECD_pcflsp_Yh_6

    # Combine all available national inventories
    L144.ALL_pcflsp_Yh <- bind_rows(L144.OECD_pcflsp_Yh_6, L144.pcflsp_m2_chn_Yh,
                                    L144.pcflsp_m2_otherctry_Yh_long_2)


    # Replace the USA data with 50-state-derived data
    USA_flsp_years <- unique(A44.flsp_bm2_state_res_long$year) # is this necessary?

    A44.flsp_bm2_state_res_long %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(iso = "usa") %>%
      group_by(iso, year) %>%
      summarise(value_bm2 = sum(value_bm2)) %>%
      ungroup() ->
      L144.flsp_bm2_USA_res

    L144.flsp_bm2_USA_res %>%
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_pcflsp = value_bm2 / value * 1e6) %>% # change to CONV_BIL_THOUS
      select(iso, year, value_pcflsp) ->
      L144.pcflsp_m2_USA_res

    # Extrapolate the US data to all years, and match into the table of OECD countries' floorspace
    L144.pcflsp_m2_USA_res %>%
      complete(year = HISTORICAL_YEARS,
               iso = "usa") %>%
      mutate(year = as.numeric(year)) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) ->
      L144.pcflsp_m2_USA_res_2

    L144.ALL_pcflsp_Yh %>%
      filter(iso != "usa") %>% # Remove USA. Our custom values will be added next.
      bind_rows(L144.pcflsp_m2_USA_res_2) ->
      L144.ALL_pcflsp_Yh_2


    # Apply default estimates of per-capita floorspace to remaining countries in the world
    # Extrapolate the defaults to all years
    list_iso_calc <- unique(L144.ALL_pcflsp_Yh_2$iso)
    list_iso_pop <- unique(L100.Pop_thous_ctry_Yh$iso) # may want to remove

    A44.pcflsp_default_long %>%
      filter(gcam.consumer == "resid") %>%
      # Left_join_error_no_match cannot be used because the number of rows will change. Each region will be expanded
           # into their individual countries
      left_join(iso_GCAM_regID, by = "region_GCAM3") %>%
      filter(iso %in% list_iso_pop) %>% # added here for analysis
      filter(!iso %in% list_iso_calc) %>% # Filter out iso's already calculated
      select(iso, year, value_pcflsp) %>%
      group_by(iso) %>%
      complete(year = HISTORICAL_YEARS) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() %>%
      bind_rows(L144.ALL_pcflsp_Yh_2) -> # Combine altogether
      L144.pcflsp_m2_ctry_Yh


    # Calculate total floorspace by GCAM region:
    # Multiply by population, match in the region names, and aggregate by (new) GCAM region
    # This produces a final output table.
    L144.pcflsp_m2_ctry_Yh %>%
      # left_join_error_no_match cannot be used because the population file does not have all the countries
      left_join(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      mutate(value_flsp = value_pcflsp * value / 1e6) %>% filter(!is.na(value_flsp)) %>% # change to CONV_BIL_THOUS
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value_flsp, na.rm = T)) %>% # Ignore NAs that were introduced via left_join step
      ungroup() ->
      L144.flsp_bm2_R_res_Yh # This is a final output table.


    #----------------------------------------------------------------------------------------------------------------

    # Commercial Floorspace calculations
    # For USA, use RGCAM output
    A44.flsp_bm2_state_comm_long %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(iso = "usa") %>%
      group_by(iso, year) %>%
      summarise(value_bm2 = sum(value_bm2)) %>%
      ungroup() %>%
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_pcflsp = value_bm2 / value * 1e6) %>% # change to CONV_BIL_THOUS
      select(iso, year, value_pcflsp) %>%
      # Extrapolate the US data to all years, and match into the table of OECD countries' floorspace
      complete(year = HISTORICAL_YEARS,
               iso = "usa") %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp_USA = approx_fun(year, value_pcflsp, rule = 2)) %>%
      select(iso, year, value_pcflsp_USA) ->
      L144.pcflsp_m2_USA_comm

    # Do the same for data from other sources, where available
    Other_pcflsp_m2_ctry_Yh_long %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      filter(gcam.consumer == "comm") %>%
      #mutate(year = as.numeric(year)) %>% # Convert year to numeric as needed by the interpolation function.
      # Extrapolate to all historical years
      select(iso, year, value_pcflsp) %>%
      complete(year = HISTORICAL_YEARS,
               iso = "zaf") %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      # Convert to total floorspace
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2 = value_pcflsp * value / 1e6) %>% # change to CONV_THOUS_BIL
      select(iso, year, value_bm2) ->
      L144.flsp_bm2_comm_otherctry_Yh

    A44.pcflsp_default_long %>%
      filter(gcam.consumer == "comm") %>%
      #filter(region_GCAM3 != "USA") %>%
      group_by(region_GCAM3) %>%
      complete(year = HISTORICAL_YEARS) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.pcflsp_m2_USA_comm, by = "year") %>%
      mutate(value_pcflsp = replace(value_pcflsp, region_GCAM3 == "USA", value_pcflsp_USA[region_GCAM3 == "USA"])) %>%
      select(region_GCAM3, year, value_pcflsp) ->
      L144.comm_flsp

    L144.comm_flsp %>%
      # Left_join_error_no_match cannot be used because the number of rows will change. Each region will be expanded
      # into their individual countries
      left_join(iso_GCAM_regID, by = "region_GCAM3") %>%
      # left_join_error_no_match cannot be used because the population file does not have all the countries
      left_join(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2 = value_pcflsp * value / 1e6) %>% # note to change to CONV_BIL_THOUS
      select(iso, region_GCAM3, GCAM_region_ID, year, value_bm2) ->
      L144.flsp_bm2_ctry_comm_Yh

    list_iso_other_comm <- unique(L144.flsp_bm2_comm_otherctry_Yh$iso)

    # This produces a final output table.
    L144.flsp_bm2_ctry_comm_Yh %>%
      # left_join_error_no_match cannot be used because there will be rows left unmatched
      left_join(L144.flsp_bm2_comm_otherctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2 = replace(value_bm2.x, iso %in% list_iso_other_comm, value_bm2.y[iso %in% list_iso_other_comm])) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value_bm2, na.rm = T)) %>%
      ungroup() ->
      L144.flsp_bm2_R_comm_Yh # This is a final output table.


    # Calculation of floorspace prices
    # This produces a final output table.
    bld_frac_of_income <- 0.2

    L144.flsp_bm2_R_res_Yh %>%
      rename(value_flsp = value) %>%
      left_join_error_no_match(L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID", "year")) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(value = value * CONV_MIL_BIL * bld_frac_of_income / value_flsp) %>%
      select(GCAM_region_ID, year, value) ->
      L144.flspPrice_90USDm2_R_bld_Yh # This is a final output table.

    # ===================================================

    L144.flsp_bm2_R_res_Yh %>%
      add_title("Residential floorspace by GCAM region / historical year") %>%
      add_units("billion m2") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L144.flsp_bm2_R_res_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A44.flsp_bm2_state_res", "energy/A44.pcflsp_default",
                     "energy/A44.HouseholdSize", "energy/CEDB_ResFloorspace_chn", "energy/Other_pcflsp_m2_ctry_Yh",
                     "energy/IEA_PCResFloorspace", "energy/Odyssee_ResFloorspacePerHouse", "L100.Pop_thous_ctry_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flsp_bm2_R_res_Yh

    L144.flsp_bm2_R_comm_Yh %>%
      add_title("Commercial floorspace by GCAM region / historical year") %>%
      add_units("billion m2") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L144.flsp_bm2_R_comm_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A44.flsp_bm2_state_comm", "energy/A44.pcflsp_default",
                     "energy/Other_pcflsp_m2_ctry_Yh", "L100.Pop_thous_ctry_Yh",
                     "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flsp_bm2_R_comm_Yh

    L144.flspPrice_90USDm2_R_bld_Yh %>%
      add_title("Building floorspace prices by GCAM region / historical year") %>%
      add_units("1990$ / m2") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L144.flspPrice_90USDm2_R_bld_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A44.flsp_bm2_state_res", "energy/A44.pcflsp_default",
                     "energy/A44.HouseholdSize", "energy/CEDB_ResFloorspace_chn", "energy/Other_pcflsp_m2_ctry_Yh",
                     "energy/IEA_PCResFloorspace", "energy/Odyssee_ResFloorspacePerHouse", "L100.Pop_thous_ctry_Yh",
                     "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flspPrice_90USDm2_R_bld_Yh

    return_data(L144.flsp_bm2_R_res_Yh, L144.flsp_bm2_R_comm_Yh, L144.flspPrice_90USDm2_R_bld_Yh)
  } else {
    stop("Unknown command")
  }
}
