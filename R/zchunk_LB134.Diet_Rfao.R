#' module_aglu_LB134.Diet_Rfao
#'
#' Build historical and future time series of per-capita caloric demands.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L134.pcFood_kcald_R_Dmnd_Y}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp1}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp2}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp3}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp4}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp5}. The corresponding file in the
#' original data system was \code{LB134.Diet_Rfao.R} (aglu level1).
#' @details Build historical time series of per-capita caloric demands;
#' divide by population to calculate the historical per-capita food demands;
#' extrapolate this to future periods based on FAO projections;
#' calculate the future demand ratios by FAO2050 region for each demand type;
#' aggregate by GCAM demand and FAO region, and compute future diet ratios by FAO2050 region;
#' extend the projected diets to all years, assuming convergence year and demand levels;
#' make SSP-specific projections based on GDP changes.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL May 2017
module_aglu_LB134.Diet_Rfao <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/A_FoodDemand_SSPs",
             FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO2050_items_cal",
             FILE = "aglu/FAO2050_Diet",
             "L100.FAO_ag_Food_t",
             "L100.FAO_an_Food_t",
             "L101.ag_Food_Pcal_R_C_Y",
             FILE = "temp-data-inject/L105.an_Food_Pcal_R_C_Y",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L134.pcFood_kcald_R_Dmnd_Y",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_FoodDemand_SSPs <- get_data(all_data, "aglu/A_FoodDemand_SSPs")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO2050_items_cal <- get_data(all_data, "aglu/FAO2050_items_cal")
    get_data(all_data, "aglu/FAO2050_Diet")  %>%
      gather(year, value, -FAO2050_reg, -FAO2050_item) %>%
      mutate(year = as.integer(year)) ->
      FAO2050_Diet
    L100.FAO_ag_Food_t <- get_data(all_data, "L100.FAO_ag_Food_t")
    L100.FAO_an_Food_t <- get_data(all_data, "L100.FAO_an_Food_t")
    L101.ag_Food_Pcal_R_C_Y <- get_data(all_data, "L101.ag_Food_Pcal_R_C_Y")
    L105.an_Food_Pcal_R_C_Y <- get_data(all_data, "temp-data-inject/L105.an_Food_Pcal_R_C_Y") %>%
      # TEMPORARY - for temp-data-inject data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>% mutate(year = as.integer(substr(year, 2, 5)))
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Build historical time series of per-capita caloric demands

    # Historical time series of ag and animal product consumption
    # Original lines 41-49
    # Start by summing food and animal (below) demand for each year and GCAM region
    L101.ag_Food_Pcal_R_C_Y %>%
      filter(year %in% AGLU_HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(consumption = sum(value)) %>%
      mutate(GCAM_demand = "crops") ->
      L134.ag_Food_Pcal_R_Y

    L105.an_Food_Pcal_R_C_Y %>%
      filter(year %in% AGLU_HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(consumption = sum(value)) %>%
      mutate(GCAM_demand = "meat") %>%
      bind_rows(L134.ag_Food_Pcal_R_Y) %>%

      # Divide by population to calculate the historical per-capita food demands, in kcal per person per day
      # Original lines 51-56
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(food_demand_percapita = consumption * CONV_DAYS_YEAR * (1 / CONV_MCAL_PCAL ) / value) %>%
      select(-consumption, -value) ->
      L134.pcFood_kcald_R_Dmnd_Y

    # In preparation for future extrapolation, get the last historical numbers for both
    # food and animal demand and aggregate by ISO
    # Original lines 58-69
    L100.FAO_ag_Food_t %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(iso) %>%
      summarise(end_history_demand = sum(value)) %>%
      mutate(GCAM_demand = "crops") ->
      L134.FAO_ag_Food_t

    L100.FAO_an_Food_t %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(iso) %>%
      summarise(end_history_demand = sum(value)) %>%
      mutate(GCAM_demand = "meat") %>%

      # bind these two data frames together, to produce a unified food demand dataset,
      # and match (by ISO code) with FAO2050 region name
      bind_rows(L134.FAO_ag_Food_t) %>%
      left_join_keep_first_only(select(AGLU_ctry, FAO2050_reg, iso), by = "iso") ->
      L134.Food_t_ctry_Dmnd_Y

    # Calculate the future demand ratios by FAO2050 region for each demand type
    # Drop unnecessary composite regions from FAO diet table
    # Original lines 71-85
    FAO2050_Diet %>%
      arrange(year) %>%
      filter(FAO2050_reg %in% L134.Food_t_ctry_Dmnd_Y$FAO2050_reg) %>%
      # Calculate FAO2050 diet for specified diet years
      # Use linear interpolation to convert FAO2050 model time periods to GCAM "diet years"
      group_by(FAO2050_reg, FAO2050_item) %>%
      # Extend years in dataset to include DIET_YEARS
      complete(year = unique(c(year, aglu.DIET_YEARS))) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Add caloric content and demand category
      left_join(select(FAO2050_items_cal, FAO2050_item, GCAM_demand, kcalkg, conv_d), by = "FAO2050_item") %>%
      # Build new table with only diet years subsetted
      # Multiply through by caloric contents to get all measures in kcal/pers/d, by FAO region and food categories
      # Original lines 87-92
      filter(year %in% aglu.DIET_YEARS) %>%
      mutate(demand_kcal = value * kcalkg / conv_d) %>%
      select(FAO2050_reg, FAO2050_item, GCAM_demand, year, demand_kcal) %>%

      # Aggregate by GCAM demand and FAO region
      # Original lines 94-104
      filter(year %in% aglu.DIET_YEARS, !is.na(GCAM_demand)) %>%
      group_by(FAO2050_reg, GCAM_demand, year) %>%
      summarise(demand_kcal = sum(demand_kcal)) %>%
      ungroup %>%
      # everything assigned as "total" demand gets re-assigned to "crops" because...
      mutate(GCAM_demand = if_else(GCAM_demand == "total", "crops", GCAM_demand)) ->
      L134.Diet_kcald_Rfao_Dmnd_Y

    # Compute diet ratios (change from last historical year) based on meat caloric contents and demand
    # Note on this next section from Page:
    # Can't just use exogenous caloric contents for the crop and meat commodities, which we're estimating
    # as averages without knowing the exact composition of the commodities. (We can't know exactly where
    # their split is in any year because this FAO AT2050 table just uses a few food groupings that are far
    # coarser than the FAOSTAT data underlying these reported trends, and generally reports the flows in
    # kilograms per person per day. While the total is reported in kcal/pers/d, ideally we'd this information
    # for all of groupings, or at least for crop and meat subtotals.)
    # In particular, we want to calculate what portion of "cereals" is corn vs wheat in each region, as they
    # have different caloric contents, and still hit the target reported "Total food (kcal/person/day)" in
    # each region and time period. There are three options to do so:
    # (1) assign the crop caloric contents exogenously and allow the meat caloric contents to float;
    # (2) vice versa; or
    # (3) assign both crop and meat caloric contents, and then scale them all to hit the target value.
    # This code does (2) (because GPK figured that the meat commodities probably have less
    # heterogeneity in caloric contents, and it was the easiest to implement).

    # Compute diet ratios based on meat caloric contents and demand (option 2 above)
    L134.Diet_kcald_Rfao_Dmnd_Y %>%
      filter(GCAM_demand == "meat") %>%
      select(-GCAM_demand) %>%
      rename(meat_demand_kcal = demand_kcal) %>%
      right_join(L134.Diet_kcald_Rfao_Dmnd_Y, by = c("FAO2050_reg", "year")) %>%
      mutate(demand_kcal = if_else(GCAM_demand == "crops", demand_kcal - meat_demand_kcal, demand_kcal)) %>%
      select(-meat_demand_kcal) %>%
      # compute future diet ratios by FAO2050 region
      group_by(FAO2050_reg, GCAM_demand) %>%
      arrange(year) %>%
      mutate(demand_ratio = demand_kcal / first(demand_kcal)) %>%
      select(-demand_kcal) ->
      L134.DietRatio_Rfao_Dmnd_Y

    # Multiply these ratios by the starting values (final historical year value, fhy_value) at the country level
    # Original lines 106-110
    L134.DietRatio_Rfao_Dmnd_Y %>%
      left_join(L134.Food_t_ctry_Dmnd_Y, by = c("FAO2050_reg", "GCAM_demand")) %>%
      mutate(demand_kcal = demand_ratio * end_history_demand) %>%
      select(-end_history_demand, -demand_ratio) %>%

      # Match in GCAM regions, aggregate, and compute ratios from final historical year
      # Original lines 112-125
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, GCAM_demand, year) %>%
      summarise(demand_kcal = sum(demand_kcal)) ->
      L134.Food_t_R_Dmnd_Y

    L134.Food_t_R_Dmnd_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      rename(demand_fhy = demand_kcal) %>%
      left_join(L134.Food_t_R_Dmnd_Y, by = c("GCAM_region_ID", "GCAM_demand")) %>%
      mutate(demand_ratio = demand_kcal / demand_fhy) %>%
      select(-demand_fhy, -demand_kcal) %>%
      # Fill in missing values for regions that do not exist
      ungroup %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               nesting(GCAM_demand, year), fill = list(demand_ratio = 1.0)) ->
      L134.FoodRatio_R_Dmnd_Y

    # Multiply ratios by the caloric demands in the final historical year
    # Original lines 127-131
    L134.pcFood_kcald_R_Dmnd_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(L134.FoodRatio_R_Dmnd_Y, by = c("GCAM_region_ID", "GCAM_demand")) %>%
      mutate(food_demand_percapita = demand_ratio * food_demand_percapita) %>%
      bind_rows(filter(L134.pcFood_kcald_R_Dmnd_Y, year < max(HISTORICAL_YEARS))) %>%
      select(-demand_ratio) ->
      L134.pcFood_kcald_R_Dmnd_Y

    # Extend the projected diets to all years, assuming convergence year and demand levels
    # Original lines 133-138
    DIET_CONVERGENCE_YEAR <- 9999
    CONVERENCE_KCALD_CROPS <- 2500
    CONVERENCE_KCALD_MEAT <- 1000

    L134.pcFood_kcald_R_Dmnd_Y %>%
      rename(demand_kcal = food_demand_percapita) %>%
      complete(GCAM_region_ID, GCAM_demand,
               year = unique(c(year, HISTORICAL_YEARS, FUTURE_YEARS, DIET_CONVERGENCE_YEAR))) %>%
      # fill in convergence year (9999) value
      mutate(demand_kcal = if_else(year == DIET_CONVERGENCE_YEAR,
                                   if_else(GCAM_demand == "crops", CONVERENCE_KCALD_CROPS, CONVERENCE_KCALD_MEAT),
                                   demand_kcal)) %>%
      # interpolate out to that convergence year
      group_by(GCAM_region_ID, GCAM_demand) %>%
      mutate(demand_kcal = approx_fun(year, demand_kcal)) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) ->
      L134.pcFood_kcald_R_Dmnd_Y

    # Alternative diet scenarios for the SSPs
    # Original lines 140-
    # this same logic happens over and over, so *use a function*
    # scen - scenario
    create_ssp_diet <- function(L102.pcgdp_thous90USD_Scen_R_Y, scen, demand, L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs) {
      if(demand == "crops") {
        a <- 4545    # a and b are parameters for demand scaling function below
        b <- -0.099
      } else if(demand == "meat") {
        a <- 818
        b <- -2.31
      } else {
        stop("Unknown demand ", demand)
      }
      L102.pcgdp_thous90USD_Scen_R_Y %>%
        filter(scenario == scen) %>%
        mutate(GCAM_demand = demand,
               value = a / (1 + exp(b * log(value)))) %>%
        select(GCAM_region_ID, GCAM_demand, year, value) %>%
        filter(year %in% c(max(HISTORICAL_YEARS), FUTURE_YEARS)) ->
        ssp_gdp

      ssp_gdp %>%
        # compute gdp ratios for future years
        arrange(GCAM_region_ID, GCAM_demand, desc(year)) %>%
        group_by(GCAM_region_ID, GCAM_demand) %>%
        mutate(ratio = value / lead(value)) %>%
        select(-value) %>%
        # ...and use those ratios to scale future food demand
        left_join(filter(L134.pcFood_kcald_R_Dmnd_Y, GCAM_demand == demand), ., by = c("GCAM_region_ID", "GCAM_demand", "year")) %>%
        mutate(ratio = if_else(is.na(ratio), 1.0, ratio)) %>%
        arrange(GCAM_region_ID, GCAM_demand, year) %>%
        group_by(GCAM_region_ID, GCAM_demand) %>%
        # computing the cumulative product, and then multiplying by the last historical year value, is the
        # same mathematically as multiplying each year's ratio by the previous year's value
        mutate(ratio = cumprod(ratio),
               demand_kcal = if_else(year > max(HISTORICAL_YEARS), ratio * nth(demand_kcal, which(year == max(HISTORICAL_YEARS))), demand_kcal)) %>%
        # ensure the year-to-year change, and absolute values, don't exceed certain levels (given in 'A_FoodDemand_SSPs')
        mutate(scenario = scen) %>%
        left_join_error_no_match(A_FoodDemand_SSPs, by = c("scenario", "GCAM_demand")) ->
        ssp_gdp

      # Cap future values, both maximum annual change (ratio)
      # and absolute value, based on values in A_FoodDemand_SSPs

      # Note that the old code has a bug: in line 168 a loop starts without
      # correctly resetting the `prev_i` variable; as a result, if the value in
      # the final historical year is 0, incorrect values get written to all future
      # years -- but only for meat. We replicate this faulty behavior below.
      if(OLD_DATA_SYSTEM_BEHAVIOR) {
        prev_yr <- max(FUTURE_YEARS)  # bug
      } else {
        prev_yr <- max(HISTORICAL_YEARS)
      }
      # Because of the dependencies involved, I couldn't figure out a way
      # not to use a loop here.  :(
      for(yr in sort(FUTURE_YEARS)) {
        d <- filter(ssp_gdp, year == yr)
        d_prev <- filter(ssp_gdp, year == prev_yr)
        capped_ratio <- pmin(d$demand_kcal / d_prev$demand_kcal, d$max.mult)
        capped_value <- pmin(d_prev$demand_kcal * capped_ratio, d$satiation.level)
        ssp_gdp$demand_kcal[ssp_gdp$year == yr] <- capped_value
        prev_yr <- yr
      }

      # Finally, replace any NAs with zeroes
      ssp_gdp %>%
        ungroup %>%
        select(GCAM_region_ID, GCAM_demand, year, demand_kcal) %>%
        replace_na(list(demand_kcal = 0.0))
    }

    L134.pcFood_est_R_Dmnd_Y_ssp1_crops <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP1", "crops", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp1_meat <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP1", "meat", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp2_crops <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP2", "crops", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp2_meat <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP2", "meat", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp3_crops <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP3", "crops", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp3_meat <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP3", "meat", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp4_crops <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP4", "crops", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp4_meat <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP4", "meat", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp5_crops <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP5", "crops", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)
    L134.pcFood_est_R_Dmnd_Y_ssp5_meat <- create_ssp_diet(L102.pcgdp_thous90USD_Scen_R_Y, "SSP5", "meat", L134.pcFood_kcald_R_Dmnd_Y, A_FoodDemand_SSPs)

    # Produce outputs
    L134.pcFood_kcald_R_Dmnd_Y %>%
      rename(value = demand_kcal) %>%
      add_title("Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Built from historical and future time series of per-capita caloric demands") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/AGLU_ctry",
                     "aglu/FAO2050_items_cal", "aglu/FAO2050_Diet",
                     "L100.FAO_ag_Food_t", "L100.FAO_an_Food_t",
                     "L101.ag_Food_Pcal_R_C_Y", "L101.Pop_thous_R_Yh",
                     "temp-data-inject/L105.an_Food_Pcal_R_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y

    bind_rows(L134.pcFood_est_R_Dmnd_Y_ssp1_crops,
              L134.pcFood_est_R_Dmnd_Y_ssp1_meat) %>%
      rename(value = demand_kcal) %>%
      add_title("SSP1 Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Scales using SSP1 GDP changes") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp1") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp1

    bind_rows(L134.pcFood_est_R_Dmnd_Y_ssp2_crops,
              L134.pcFood_est_R_Dmnd_Y_ssp2_meat) %>%
      rename(value = demand_kcal) %>%
      add_title("SSP2 Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Scales using SSP2 GDP changes") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp2") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp2

    bind_rows(L134.pcFood_est_R_Dmnd_Y_ssp3_crops,
              L134.pcFood_est_R_Dmnd_Y_ssp3_meat) %>%
      rename(value = demand_kcal) %>%
      add_title("SSP3 Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Scales using SSP3 GDP changes") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp3") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp3

    bind_rows(L134.pcFood_est_R_Dmnd_Y_ssp4_crops,
              L134.pcFood_est_R_Dmnd_Y_ssp4_meat) %>%
      rename(value = demand_kcal) %>%
      add_title("SSP5 Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Scales using SSP4 GDP changes") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp4") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp4

    bind_rows(L134.pcFood_est_R_Dmnd_Y_ssp5_crops,
              L134.pcFood_est_R_Dmnd_Y_ssp5_meat) %>%
      rename(value = demand_kcal) %>%
      add_title("SSP5 Per-capita food demands by region / demand type / year (historical and future)") %>%
      add_units("kcal / person / day") %>%
      add_comments("Scales using SSP5 GDP changes") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp5") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp5

    return_data(L134.pcFood_kcald_R_Dmnd_Y, L134.pcFood_kcald_R_Dmnd_Y_ssp1, L134.pcFood_kcald_R_Dmnd_Y_ssp2, L134.pcFood_kcald_R_Dmnd_Y_ssp3, L134.pcFood_kcald_R_Dmnd_Y_ssp4, L134.pcFood_kcald_R_Dmnd_Y_ssp5)
  } else {
    stop("Unknown command")
  }
}
