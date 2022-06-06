# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA144.building_det_flsp
#'
#' Calculate residential and commercial floorspace - and floorspace prices - by GCAM region and historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_R_res_Yh}, \code{L144.flsp_bm2_R_comm_Yh}, \code{L144.flspPrice_90USDm2_R_bld_Yh},
#' \code{L144.hab_land_flsp_fin}, \code{L144.flsp_param}. The corresponding file in the
#' original data system was \code{LA144.building_det_flsp.R} (energy level1).
#' @details Commercial and residential floorspace was calculated at the country level, before aggregating to the regional level.
#' When available, floorspace was calculated from country-specific datasets, including those for the USA, China, and South Africa.
#' Floorspace for countries that did not have country-level data was calculated using GCAM 3.0 assumptions.
#' Floorspace prices were calculated by dividing an assumed fraction of GDP for buildings by residential floorspace.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join matches mutate pull select summarise
#' @importFrom tidyr complete gather spread
#' @author AJS July 2017
module_energy_LA144.building_det_flsp <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/A44.flsp_bm2_state_comm",
             FILE = "energy/A44.pcflsp_default",
             FILE = "energy/A44.HouseholdSize",
             FILE = "energy/CEDB_ResFloorspace_chn",
             FILE = "energy/RECS_ResFloorspace_usa",
             FILE = "energy/Other_pcflsp_m2_ctry_Yh",
             FILE = "energy/IEA_PCResFloorspace",
             FILE = "energy/Odyssee_ResFloorspacePerHouse",
             "L100.Pop_thous_ctry_Yh",
             "L102.gdp_mil90usd_GCAM3_R_Y",
             "L221.LN0_Land",
             "L221.LN1_UnmgdAllocation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_R_res_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             "L144.flspPrice_90USDm2_R_bld_Yh",
             "L144.hab_land_flsp_fin",
             "L144.flsp_param"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A44.flsp_bm2_state_comm <- get_data(all_data, "energy/A44.flsp_bm2_state_comm")
    A44.pcflsp_default <- get_data(all_data, "energy/A44.pcflsp_default")
    A44.HouseholdSize <- get_data(all_data, "energy/A44.HouseholdSize")
    CEDB_ResFloorspace_chn <- get_data(all_data, "energy/CEDB_ResFloorspace_chn")
    RECS_ResFloorspace_usa <- get_data(all_data, "energy/RECS_ResFloorspace_usa")
    Other_pcflsp_m2_ctry_Yh <- get_data(all_data, "energy/Other_pcflsp_m2_ctry_Yh")
    IEA_PCResFloorspace <- get_data(all_data, "energy/IEA_PCResFloorspace")
    Odyssee_ResFloorspacePerHouse <- get_data(all_data, "energy/Odyssee_ResFloorspacePerHouse")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_R_Y")
    L221.LN0_Land<-get_data(all_data, "L221.LN0_Land", strip_attributes = TRUE)
    L221.LN1_UnmgdAllocation<-get_data(all_data, "L221.LN1_UnmgdAllocation", strip_attributes = TRUE)
    # ===================================================

    # Silence package notes
    . <- `1980` <- `1990` <- `1991` <- `1992` <- `1995` <- `1996` <- `1998` <- `2001` <- `2004` <-
      GCAM_region_ID <- GCAM_sector <- gcam.consumer <- region_GCAM3 <- state <- value_bm2 <-
      value_bm2_other <- value_flsp <- value_pcdwelling <- value_pcflsp <- value_pcflsp_USA <-
      value_phflsp <- year <- value <- iso <- country <- Variable <- Unit <- LandNode1 <- allocation <-
      region <- nonHab <- landAllocation <- totland <- Units <- gdp <- pop <- flps_bm2 <- area_thous_km2 <-
      nls <- coef <- gdp_mil <- area_thouskm2 <- unadjust.satiation <- land.density.param <- tot.dens <-
      b.param <- income.param <- pc_gdp_thous <- flsp_pc_est <- flsp_est <- NULL

    # FLOORSPACE CALCULATION - RESIDENTIAL

    # In this section, we aim to create a final output table of residential floorspace per GCAM region across all historical years
    # Before aggregating to the regional level, floorspace will be calculated at the country level using the following base datasets:
    # CEDB_ResFloorspace_chn (China Energy Databook) provides residential floorspace (billions m2) from 1985 to 2006 for China
    # IEA_PCResFloorspace provides residential floorspace (m2) per person for 16 selected countries for 1980 to 2004
    # Odyssee_ResFloorspacePerHouse provides residential floorspace (m2) per house (not person) from 1980 to 2009 for 29 countries
    # A44.HouseholdSize provides number of persons/dwelling data, which will be used to calculate floorspace per capita
    # Note that IEA data will be chosen over Odyssee for duplicate countries b/c it reports per capita instead of per house
    # Other_pcflsp_m2_ctry_Yh provides residential and commercial floorspace (m2) per person for 2004 and 2005
    # for other countries (only South Africa at this time)
    # Note that this table is written by LA144.Residential.R from an earlier version of GCAM-USA
    # A44.pcflsp_default provides residential and commercial floorspace (m2) per person for 1975, 1990, and 2005 for GCAM3 regions
    # L100.Pop_thous_ctry_Yh provides country-level population data and will be used to switch between total floorspace and per capita data

    # China
    # China Energy Databook, CEDB_ResFloorspace_chn, provides residential floorspace (billions m2) from 1985 to 2015 for China.
    # Divide floorspace by population to get per capita floorspace, and extrapolate to all historical years
    CEDB_ResFloorspace_chn %>%
      gather_years(value_col = "value_flsp") %>% # Needs to be integer to combine with L100.Pop_thous_ctry_Yh
      filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical time period
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      # Divide floorspace by population to get per capita floorspace
      mutate(value_pcflsp = value_flsp / value * CONV_BIL_THOUS) %>% # Note: converting to thousand m2 because population is in thousands
      select(iso, year, value_pcflsp) %>%
      group_by(iso) %>%
      # Expand table to include all historical years
      complete(year = HISTORICAL_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() ->
      L144.pcflsp_m2_chn_Yh


    # USA
    # The multiple RECS databases, RECS_ResFloorspace_USA, provides residential floorspace (billions m2) from 1971 to 2015 for USA.
    # Divide floorspace by population to get per capita floorspace, and extrapolate to all historical years
    RECS_ResFloorspace_usa %>%
      gather_years(value_col = "value_flsp") %>% # Needs to be integer to combine with L100.Pop_thous_ctry_Yh
      filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical time period
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      # Divide floorspace by population to get per capita floorspace
      mutate(value_pcflsp = value_flsp / value * CONV_BIL_THOUS) %>% # Note: converting to thousand m2 because population is in thousands
      select(iso, year, value_pcflsp) %>%
      group_by(iso) %>%
      # Expand table to include all historical years
      complete(year = HISTORICAL_YEARS) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() ->
      L144.pcflsp_m2_usa_Yh


    # Odyssee_ResFloorspacePerHouse provides residential floorspace (m2) per house (not person) from 1980 to 2009 for 29 countries
    # A44.HouseholdSize provides number of persons/dwelling data, which will be used to calculate floorspace per capita

    # We need to prepare some lists and reshape tables first
    # First, convert household data to long form so it can be joined at a later step
    A44.HouseholdSize_long <- A44.HouseholdSize %>%
      select(-Variable, -Unit) %>%
      gather_years(value_col = "value_pcdwelling")

    # IEA_PCResFloorspace provides residential floorspace (m2) per person for 16 selected countries for 1980 to 2004
    # Note that IEA data will be chosen over Odyssee for duplicate countries b/c it reports per capita instead of per house

    # Reshape IEA data to long form
    IEA_PCResFloorspace %>%
      gather_years(value_col = "value_pcflsp") %>%
      mutate(value_pcflsp = as.numeric(value_pcflsp)) ->
      IEA_PCResFloorspace_long

    # Create list of IEA iso's. It will be used to remove these iso's from Odyssee data.
    # Also, create a list of years from IEA data in order to be used for filtering.
    list_iso_IEA <- unique(IEA_PCResFloorspace_long$iso)
    list_years_IEA <- unique(IEA_PCResFloorspace_long$year)

    Odyssee_ResFloorspacePerHouse %>%
      gather(year, value_phflsp, matches(YEAR_PATTERN)) %>% # Convert to long form
      mutate(year = as.integer(year)) %>% # Convert year to integer to both join with A44.HouseholdSize_long and be able to extrapolote later on
      filter(!iso %in% list_iso_IEA) %>% # Remove iso's that are in IEA dataset
      # left_join_error_no_match cannot be used because joining table does not contain every year, which will introduce NAs
      left_join(A44.HouseholdSize_long, by = "year") %>%
      # Extrapolate, using rule 2 so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcdwelling = approx_fun(year, value_pcdwelling, rule = 2),
             # Calculate per capita floorspace
             value_pcflsp = value_phflsp / value_pcdwelling) %>%
      select(iso, year, value_pcflsp) ->
      L144.Odyssee_pcflsp_Yh

    IEA_PCResFloorspace_long %>%
      select(-country) %>% # Drop country column to bind with Odyssee table
      bind_rows(L144.Odyssee_pcflsp_Yh) %>% # Bind with Odyssee table
      filter(year %in% list_years_IEA) %>% # Restrict to year range from IEA table
      # We want to drop any countries with all missing values. First we will drop all rows with missing per capita values,
      # and then expand to all historical years. Countries with no per capita data for any year will consequently be removed.
      # Note that this permanently removes Cyprus, which has data post 2004.
      filter(!is.na(value_pcflsp)) %>%
      group_by(iso) %>%
      complete(year = HISTORICAL_YEARS) %>% # Exand table to all historical years.
      ungroup() %>%
      # Fill out missing values in specified countries
      # USA data derived from RECS; see RGCAM data system for documentation
      mutate(value_pcflsp = replace(value_pcflsp, iso == "usa" & year == "1980", 49.5)) %>%
      spread(year, value_pcflsp) %>% # Spread data to be able to do inter-year calculations
      # Missing 1990 values will be replaced by 1991, 1992, 1995, and 1996 (in that order)
      mutate(`1990` = replace(`1990`, is.na(`1990`), `1991`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1992`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1995`[is.na(`1990`)]),
             `1990` = replace(`1990`, is.na(`1990`), `1996`[is.na(`1990`)])) ->
      L144.OECD_pcflsp_Yh_wide # More inter-year math will be performed, so keep in wide format

    # Calculate average 1980-1990 growth rates for countries with 1980 data. Apply this to the 1990 data to return estimated 1980 floorspace.
    L144.OECD_pcflsp_Yh_wide %>%
      filter(!is.na(`1980`)) %>%
      summarise(`1980` = sum(`1980`),
                `1990` = sum(`1990`)) %>%
      mutate(growthrate_1980_1990 = `1990` / `1980`) %>%
      pull(growthrate_1980_1990) -> # Save as single value
      growthrate_1980_1990

    # Calculate 1980 data from 1990 using calculated growth rate
    L144.OECD_pcflsp_Yh_wide %>%
      mutate(`1980` = replace(`1980`, is.na(`1980`), `1990`[is.na(`1980`)] / growthrate_1980_1990)) ->
      L144.OECD_pcflsp_Yh_wide_2

    # Fill out Australia and Belgium 2004 data
    # The ratio for USA is used as a proxy for Australia, and France for Belgium
    L144.OECD_pcflsp_Yh_wide_2 %>%
      filter(iso == "usa") %>%
      mutate(value_usa_ratio = `2004` / `1998`) %>%
      pull(value_usa_ratio) ->
      value_usa_ratio

    L144.OECD_pcflsp_Yh_wide_2 %>%
      filter(iso == "fra") %>%
      mutate(value_fra_ratio = `2004` / `2001`) %>%
      pull(value_fra_ratio) ->
      value_fra_ratio

    L144.OECD_pcflsp_Yh_wide_2 %>%
      mutate(`2004` = replace(`2004`, iso == "aus", (`1998` * value_usa_ratio)[iso == "aus"]),
             `2004` = replace(`2004`, iso == "bel", (`2001` * value_fra_ratio)[iso == "bel"])) %>%
      # Extrapolate the time series to all historical years
      gather_years(value_col = "value_pcflsp") %>%
      group_by(iso) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>% # Interpolation step
      ungroup() %>%
      # Filter out usa, which is calculated from region-specific data (RECS)
      filter(iso != "usa") ->
      L144.OECD_pcflsp_Yh_final

    # Other country - South Africa
    # Other_pcflsp_m2_ctry_Yh provides residential and commercial floorspace (m2) per person for 2004 and 2005

    # Time series doesn't span entire "historical" range; need to extrapolate
    # For now, use constant floorspace outside of available time series
    Other_pcflsp_m2_ctry_Yh %>%
      gather_years(value_col = "value_pcflsp") %>%
      filter(gcam.consumer == "resid") %>%
      # Extrapolate to all historical years
      select(iso, year, value_pcflsp) %>%
      complete(year = HISTORICAL_YEARS,
               iso = "zaf") %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) ->
      L144.pcflsp_m2_otherctry_Yh_final

    # Combine all available national inventories
    L144.ALL_pcflsp_Yh <- bind_rows(L144.pcflsp_m2_chn_Yh,L144.pcflsp_m2_usa_Yh ,L144.OECD_pcflsp_Yh_final,
                                    L144.pcflsp_m2_otherctry_Yh_final)


    # Apply default estimates of per-capita floorspace to remaining countries in the world
    # Extrapolate the defaults to all years
    # First, create list of countries already calculated, so that they can be removed from this more general list
    list_iso_calc <- unique(L144.ALL_pcflsp_Yh$iso)

    A44.pcflsp_default %>%
      gather_years(value_col = "value_pcflsp") %>%
      filter(gcam.consumer == "resid") %>%
      # Left_join_error_no_match cannot be used because the number of rows will change. Each region will be expanded
      # into their individual countries
      left_join(iso_GCAM_regID, by = "region_GCAM3") %>%
      filter(!iso %in% list_iso_calc) %>% # Filter out iso's already calculated
      select(iso, year, value_pcflsp) %>%
      group_by(iso) %>%
      complete(year = HISTORICAL_YEARS) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() %>%
      bind_rows(L144.ALL_pcflsp_Yh) -> # Combine altogether
      L144.pcflsp_m2_ctry_Yh

    # Per capita floorspace was calculated for all countries.
    # Now we can calculate total floorspace and aggregate by GCAM region.
    # Multiply by population, match in the region names, and aggregate by (new) GCAM region
    # This produces the final output table for the residential sector.
    L144.pcflsp_m2_ctry_Yh %>%
      # left_join_error_no_match cannot be used because the population file does not have all the countries
      left_join(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>% # Need GCAM region ID
      mutate(value_flsp = value_pcflsp * value * CONV_THOUS_BIL) %>% # Convert from per capita to billions m2
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value_flsp, na.rm = T)) %>% # Ignore NAs that were introduced via left_join step
      ungroup() ->
      L144.flsp_bm2_R_res_Yh_pre

    # Considering the lack of country-level data on per capita floorspace, for those regions with no data for 2015,
    # values are estimated using the Gompertz function.
    # Then, values are linearly extrapolated from the final observed year to 2015.
    # In case there is new available data for 2015, it can be implemented here, and it will not cause an "imbalance" in the model
    # due to the bias.correction.adder incorporated to the floorspace demand.

    # First, define which is by default the final observed year and save the regions with observed data beyond that point (up to the final calibration year)
    avg_fin_obs_year<-2005
    regions_with_obs_data<-c("China","USA")
    `%notin%` <- Negate(`%in%`) # A ancillary function to help data processing

    # Then, calculate the habitable land, which is going to be used to estimate the floorspace per capita in final calibration year for the rest of regions.
    # The following table extracts the non habitable land per region, adding up the Tundra and the RockIceDesert categories
    L144.non_hab_land_pre<-L221.LN1_UnmgdAllocation %>%
      filter(!grepl("Urban",LandNode1)) %>%
      rename(nonHab=allocation) %>%
      group_by(region,year) %>%
      summarise(nonHab=sum(nonHab)) %>%
      ungroup()

    # Some regions do not have any Tundra or RockIceDesert, so the prior table needs to be completed by back-filling zeroes
    L144.adj_reg<-anti_join(L221.LN1_UnmgdAllocation,L144.non_hab_land_pre, by = c("region", "year")) %>%
      select(region,year)%>%
      distinct(region,year) %>%
      mutate(nonHab=0)

    L144.non_hab_land<- bind_rows(L144.non_hab_land_pre,L144.adj_reg)

    # Habitable land is calculated by subtracting the non-habitable land from total land
    L144.hab_land_flsp<-L221.LN0_Land %>%
      select(region, totland=landAllocation) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L144.non_hab_land,by=c("region","year")) %>%
      mutate(value=totland-nonHab,
             Units="thous km2") %>%
      select(region,year,Units,value)

    L144.hab_land_flsp_fin<-L144.hab_land_flsp %>%
      filter(year==MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L144.hab_land_flsp) %>%
      arrange(region,year) # This dataset is written to be used in module_energy_L244.building_det

    # ----------------------------------
    # Population per GCAM region is also used for the floorspace estimation:
    L100.Pop_thous_R_Y<-L100.Pop_thous_ctry_Yh %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(GCAM_region_ID,iso), by="iso") %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(value=sum(value)*1E3) %>%
      ungroup()

    # Estimation of the Gompertz parameters(land.density.param,b.param,and income.param):
    # pcap_flsp~(obs_sat +(land.density.param * log(tot_dens)))* exp(-b.param * exp(-income.param * log(pcap_income)))
    # USA has a different unique behaviour, as observed pcap flsp is significantly higher than countries with:
    # - Similar (or higher) per capita income
    # - Similar (or lower) population density
    # Therefore, we substitute the parameters for USA by those estimated using subregional data (not included in the DS).
    L144.flsp_param_pre<-L144.flsp_bm2_R_res_Yh_pre %>%
      left_join_error_no_match(GCAM_region_names, by="GCAM_region_ID") %>%
      # take all periods from regions with observed data:
      filter(region %in% regions_with_obs_data) %>%
      bind_rows(L144.flsp_bm2_R_res_Yh_pre %>%
                  left_join_error_no_match(GCAM_region_names, by="GCAM_region_ID") %>%
                  filter(region %notin% regions_with_obs_data,
                          year<=avg_fin_obs_year)) %>%
      rename(flps_bm2 = value) %>%
      #add GDP
      left_join_error_no_match(L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(gdp=value * 1E6) %>%
      select(-value) %>%
      #Add population to estimate pc_GDP
      left_join_error_no_match(L100.Pop_thous_R_Y, by = c("GCAM_region_ID", "year")) %>%
      rename(pop = value) %>%
      mutate(pc_gdp_thous= gdp / (pop * 1E3),
             pc_flsp = (flps_bm2* 1E9) / pop) %>%
      left_join_error_no_match(L144.hab_land_flsp_fin %>%
                                 group_by(region,Units) %>%
                                 complete(nesting(year=min(L144.flsp_bm2_R_res_Yh_pre$year):max(L144.flsp_bm2_R_res_Yh_pre$year))) %>%
                                 mutate(value=if_else(is.na(value),approx_fun(year,value,rule = 2),value)) %>%
                                 ungroup() %>%
                                 select(-Units),
                              by = c("year", "region")) %>%
      rename(area_thous_km2 = value) %>%
      mutate(tot_dens = pop/(area_thous_km2* 1E3))

    # Filter out USA because it already has the parameters (estimated offline using subregional data, but following the same procedure applied here for the 31 GCAM regions):
    L144.flsp_param_pre_nonusa<- L144.flsp_param_pre %>%
      filter(region != "USA")

    # Estimation of the parameters for non USA:
    formula.gomp<- "pc_flsp~(100 -(a*log(tot_dens)))*exp(-b*exp(-c*log(pc_gdp_thous)))"
    start.value<-c(a=-0.5,b=0.005,c=0.05)
    fit.gomp<-nls(formula.gomp, L144.flsp_param_pre_nonusa, start.value)

    # Tibble with the USA parameters
    L144.flsp_param_USA<-tibble(region ="USA",
                                unadjust.satiation = gcamusa.OBS_UNADJ_SAT,
                                land.density.param = gcamusa.LAND_DENSITY_PARAM,
                                b.param = gcamusa.B_PARAM,
                                income.param = gcamusa.INCOME_PARAM)

    # Write the dataset with the fitted parameters for the 31 GCAM regions
    # Add the tibble with USA-specific parameters
    L144.flsp_param<-L144.flsp_param_pre %>%
      select(region) %>%
      distinct() %>%
      filter(region != "USA") %>%
      arrange(region) %>%
      mutate(unadjust.satiation = energy.OBS_UNADJ_SAT,
             land.density.param = coef(fit.gomp)[1],
             b.param = coef(fit.gomp)[2],
             income.param = coef(fit.gomp)[3]) %>%
      bind_rows(L144.flsp_param_USA)



    # ----------------------------------
    # With all this data, estimate the per capita floorspace in the final calibration year using the Gompertz function:
    L144.flsp_bm2_R_res_Yh_finBaseYear_est<-L144.flsp_bm2_R_res_Yh_pre %>%
      rename(flsp_bm2 = value) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L102.gdp_mil90usd_GCAM3_R_Y %>% filter(year == MODEL_FINAL_BASE_YEAR)
                               , by = c("GCAM_region_ID","year")) %>%
      rename(gdp_mil = value) %>%
      left_join_error_no_match(L100.Pop_thous_R_Y, by = c("GCAM_region_ID", "year")) %>%
      rename(pop = value) %>%
      mutate(pc_gdp_thous = gdp_mil*1E3/pop) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L144.flsp_param, by = "region") %>%
      left_join_error_no_match(L144.hab_land_flsp_fin %>% filter(year==MODEL_FINAL_BASE_YEAR),by=c("region","year")) %>%
      rename(area_thouskm2=value) %>%
      mutate(tot.dens=(pop/1E3)/area_thouskm2,
             flsp_pc_est=(`unadjust.satiation` +(-`land.density.param`*log(tot.dens)))*exp(-`b.param`
                                                                                        *exp(-`income.param`*log(pc_gdp_thous))),
             flsp_est = flsp_pc_est * pop / 1E9) %>%
      select(GCAM_region_ID,flsp_est) %>%
      mutate(year = MODEL_FINAL_BASE_YEAR)

    # ----------------------------------
    # Finally, substitute the values from final observed year (avg_fin_obs_year) to final calibration year for the regions without observed data (regions_with_obs_data)
    L144.flsp_bm2_R_res_Yh<-L144.flsp_bm2_R_res_Yh_pre %>%
      left_join(L144.flsp_bm2_R_res_Yh_finBaseYear_est, by = c("GCAM_region_ID", "year")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = if_else(region %notin% regions_with_obs_data & year %in% c(avg_fin_obs_year:MODEL_FINAL_BASE_YEAR),flsp_est,value)) %>%
      select(-region,-flsp_est) %>%
      group_by(GCAM_region_ID) %>%
      mutate(value = if_else(is.na(value), approx_fun(year, value, rule = 1), value)) %>%
      ungroup()


    #-------------------------------------
    # FLOORSPACE CALCULATION - COMMERCIAL

    # In this section, we aim to create a final output table of commercial floorspace per GCAM region across all historical years
    # Before aggregating to the regional level, floorspace will be calculated at the country level using the following base datasets:
    # A44.flsp_bm2_state_comm provides commercial floorspace by U.S. state from 1975-2005 (in 5-year increments) and 2008
    # Note that this table is written by LA144.Commercial.R from an earlier version of GCAM-USA
    # Other_pcflsp_m2_ctry_Yh provides residential and commercial floorspace (m2) per person for 2004 and 2005
    # for other countries (only South Africa at this time)
    # A44.pcflsp_default provides residential and commercial floorspace (m2) per person for 1975, 1990, and 2005 for GCAM3 regions

    # For the USA, use the 50-state-derived data (written by LA144.Commercial.R from an earlier version of GCAM-USA)
    A44.flsp_bm2_state_comm %>%
      gather(year, value_bm2, -state, -GCAM_sector) %>% # Convert to long form
      mutate(year = as.integer(substr(year, 2, 5))) %>% # Strip X's, convert year to integer
      filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical years
      mutate(iso = "usa") %>% # Add column with USA iso name
      group_by(iso, year) %>%
      summarise(value_bm2 = sum(value_bm2)) %>% # Aggregate to country (USA) level
      ungroup() %>%
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>% # Join population
      mutate(value_pcflsp = value_bm2 / value * CONV_BIL_THOUS) %>% # Calculate per capita
      select(iso, year, value_pcflsp) %>%
      # Extrapolate the US data to all years, and match into the table of OECD countries' floorspace
      complete(year = HISTORICAL_YEARS,
               iso = "usa") %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp_USA = approx_fun(year, value_pcflsp, rule = 2)) %>%
      select(iso, year, value_pcflsp_USA) ->
      L144.pcflsp_m2_USA_comm

    # Do the same for data from other sources, where available.

    # Other country - South Africa
    # Other_pcflsp_m2_ctry_Yh provides residential and commercial floorspace (m2) per person for 2004 and 2005
    Other_pcflsp_m2_ctry_Yh %>%
      gather_years(value_col = "value_pcflsp") %>%
      filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical years
      filter(gcam.consumer == "comm") %>% # Filter only for commercial
      # Extrapolate to all historical years
      select(iso, year, value_pcflsp) %>%
      complete(year = HISTORICAL_YEARS,
               iso = "zaf") %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      # Convert to total floorspace
      left_join_error_no_match(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2_other = value_pcflsp * value * CONV_THOUS_BIL) %>% # Convert from per capita to billions m2
      select(iso, year, value_bm2_other) ->
      L144.flsp_bm2_comm_otherctry_Yh

    # Create list of countries calculated, so it can be used to filter the more general data (presently this is only South Africa)
    list_iso_other_comm <- unique(L144.flsp_bm2_comm_otherctry_Yh$iso)

    # GCAM3 region per capita floorspace data for 1975, 1990, and 2005
    # Regions will be downscaled to the country level.
    # USA and South Africa will be joined.
    A44.pcflsp_default %>%
      gather_years(value_col = "value_pcflsp") %>%
      filter(gcam.consumer == "comm") %>%
      group_by(region_GCAM3) %>%
      complete(year = HISTORICAL_YEARS) %>%
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(value_pcflsp = approx_fun(year, value_pcflsp, rule = 2)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.pcflsp_m2_USA_comm, by = "year") %>% # Add column fo USA
      # Replace GCAM3 region data for USA
      mutate(value_pcflsp = replace(value_pcflsp, region_GCAM3 == "USA", value_pcflsp_USA[region_GCAM3 == "USA"])) %>%
      select(region_GCAM3, year, value_pcflsp) %>%
      # Left_join_error_no_match cannot be used because the number of rows will change. Each region will be expanded
      # into their individual countries
      left_join(iso_GCAM_regID, by = "region_GCAM3") %>%
      # left_join_error_no_match cannot be used because the population file does not have all the countries
      left_join(L100.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2 = value_pcflsp * value * CONV_THOUS_BIL) %>% # Calculate total floorspace from per capita data
      select(iso, region_GCAM3, GCAM_region_ID, year, value_bm2) %>%
      # Join South Africa and replace old SA data with new SA data
      # left_join_error_no_match cannot be used because there will be rows left unmatched
      left_join(L144.flsp_bm2_comm_otherctry_Yh, by = c("iso", "year")) %>%
      mutate(value_bm2 = replace(value_bm2, iso %in% list_iso_other_comm, value_bm2_other[iso %in% list_iso_other_comm])) %>%
      select(iso, GCAM_region_ID, year, value_bm2) ->
      L144.flsp_bm2_ctry_comm_Yh

    # Floorspace was calculated for all countries.
    # Now we can aggregate by GCAM region.
    # This produces the final output table for the commercial sector.
    L144.flsp_bm2_ctry_comm_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value_bm2, na.rm = TRUE)) %>% # Ignore NAs that were introduced via left_join step
      ungroup() ->
      L144.flsp_bm2_R_comm_Yh # This is a final output table.

    #-------------------------------------
    # CALCULATON OF FLOORSPACE PRICES

    # Buildings is assumed to be 20% of GDP
    BLD_FRAC_OF_INCOME <- 0.2

    # The residential table will be used to calculate building floorspace prices. Units will be 1990$ / m2
    # Note that this produces a final output table.
    L144.flsp_bm2_R_res_Yh %>%
      rename(value_flsp = value) %>%
      left_join_error_no_match(L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID", "year")) %>% # Join GDP
      filter(year %in% HISTORICAL_YEARS) %>%
      # Convert to billion $ and divide by floorspace (billion m2), so that final units will be $ / m2
      # Buildings is assumed to be 20% of GDP
      mutate(value = value * CONV_MIL_BIL * BLD_FRAC_OF_INCOME / value_flsp) %>%
      select(GCAM_region_ID, year, value) ->
      L144.flspPrice_90USDm2_R_bld_Yh # This is a final output table.

    # ===================================================
    L144.hab_land_flsp_fin %>%
      add_title("Habitable land per GCAM region") %>%
      add_units("thous km2") %>%
      add_comments("Used for the estimation of residential floorspace") %>%
      add_legacy_name("L144.hab_land_flsp_fin") %>%
      add_precursors( "L221.LN0_Land","L221.LN1_UnmgdAllocation") ->
      L144.hab_land_flsp_fin


    L144.flsp_bm2_R_res_Yh %>%
      add_title("Residential floorspace by GCAM region / historical year") %>%
      add_units("billion m2") %>%
      add_comments("Residential floorspace was calculated at the country level, before aggregating to the regional level") %>%
      add_comments("Floorspace was calculated from various datasets, including those for the USA, China, and South Africa") %>%
      add_comments("Floorspace for the remaining countries were calculated using GCAM 3.0 assumptions") %>%
      add_legacy_name("L144.flsp_bm2_R_res_Yh") %>%
      add_precursors("common/iso_GCAM_regID","common/GCAM_region_names", "energy/A44.pcflsp_default",
                     "energy/A44.HouseholdSize", "energy/CEDB_ResFloorspace_chn", "energy/Other_pcflsp_m2_ctry_Yh",
                     "energy/IEA_PCResFloorspace", "energy/Odyssee_ResFloorspacePerHouse",
                     "L100.Pop_thous_ctry_Yh", "energy/RECS_ResFloorspace_usa") ->
      L144.flsp_bm2_R_res_Yh

    L144.flsp_bm2_R_comm_Yh %>%
      add_title("Commercial floorspace by GCAM region / historical year") %>%
      add_units("billion m2") %>%
      add_comments("Commercial floorspace was calculated at the country level, before aggregating to the regional level") %>%
      add_comments("USA and South Africa have their own datasets; other countries were calculated by GCAM3 regional floorspace data") %>%
      add_legacy_name("L144.flsp_bm2_R_comm_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A44.flsp_bm2_state_comm", "energy/A44.pcflsp_default",
                     "energy/Other_pcflsp_m2_ctry_Yh", "L100.Pop_thous_ctry_Yh",
                     "L102.gdp_mil90usd_GCAM3_R_Y") ->
      L144.flsp_bm2_R_comm_Yh

    L144.flspPrice_90USDm2_R_bld_Yh %>%
      add_title("Building floorspace prices by GCAM region / historical year") %>%
      add_units("1990$ / m2") %>%
      add_comments("A fraction of GDP pertaining to buildings was divided by residential floorspace") %>%
      add_legacy_name("L144.flspPrice_90USDm2_R_bld_Yh") %>%
      add_precursors("common/iso_GCAM_regID",  "energy/A44.pcflsp_default",
                     "energy/A44.HouseholdSize", "energy/CEDB_ResFloorspace_chn", "energy/Other_pcflsp_m2_ctry_Yh",
                     "energy/IEA_PCResFloorspace", "energy/Odyssee_ResFloorspacePerHouse", "L100.Pop_thous_ctry_Yh",
                     "L102.gdp_mil90usd_GCAM3_R_Y") ->
      L144.flspPrice_90USDm2_R_bld_Yh

    L144.flsp_param %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Estimated based on historical/observed floorspace values") %>%
      add_legacy_name("L144.flsp_param") %>%
      add_precursors("common/iso_GCAM_regID","common/GCAM_region_names", "energy/A44.pcflsp_default",
                     "energy/A44.HouseholdSize", "energy/CEDB_ResFloorspace_chn", "energy/Other_pcflsp_m2_ctry_Yh",
                     "energy/IEA_PCResFloorspace", "energy/Odyssee_ResFloorspacePerHouse",
                     "L100.Pop_thous_ctry_Yh", "energy/RECS_ResFloorspace_usa") ->
      L144.flsp_param

    return_data(L144.flsp_bm2_R_res_Yh, L144.flsp_bm2_R_comm_Yh, L144.flspPrice_90USDm2_R_bld_Yh,L144.hab_land_flsp_fin, L144.flsp_param)
  } else {
    stop("Unknown command")
  }
}
