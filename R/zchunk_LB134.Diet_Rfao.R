#' module_aglu_LB134.Diet_Rfao
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L134.pcFood_kcald_R_Dmnd_Y}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp1}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp2}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp3}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp4}, \code{L134.pcFood_kcald_R_Dmnd_Y_ssp5}. The corresponding file in the
#' original data system was \code{LB134.Diet_Rfao.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL May 2017
#' @export
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

    if(0) {

      # Build historical time series of per-capita caloric demands

      # Historical time series of ag and animal product consumption
      # Original lines 41-49
      L101.ag_Food_Pcal_R_C_Y %>%
        filter(year %in% AGLU_HISTORICAL_YEARS) %>%
        group_by(GCAM_region_ID, year) %>%
        summarise(value = sum(value)) %>%
        mutate(GCAM_demand = "crops") ->
        L134.ag_Food_Pcal_R_Y

      L105.an_Food_Pcal_R_C_Y %>%
        filter(year %in% AGLU_HISTORICAL_YEARS) %>%
        group_by(GCAM_region_ID, year) %>%
        summarise(value = sum(value)) %>%
        mutate(GCAM_demand = "meat") %>%
        bind_rows(L134.ag_Food_Pcal_R_Y) ->
        L134.Food_Pcal_R_Dmnd_Y

      # Divide by population to calculate the historical per-capita food demands, in kcal per person per day
      # Original lines 51-56
      L134.Food_Pcal_R_Dmnd_Y %>%
        left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("GCAM_region_ID", "year")) %>%
        mutate(value = value.x * CONV_DAYS_YEAR * (1 / CONV_MCAL_PCAL ) / value.y) %>%
        select(-value.x, -value.y) ->
        L134.pcFood_kcald_R_Dmnd_Y

      # Extrapolate this to the future periods based on the FAO projections
      # Original lines 58-69
      L100.FAO_ag_Food_t %>%
        filter(year == max(HISTORICAL_YEARS)) %>%
        group_by(iso) %>%
        summarise(value = sum(value)) %>%
        mutate(GCAM_demand = "crops") ->
        L134.FAO_ag_Food_t

      L100.FAO_an_Food_t %>%
        filter(year == max(HISTORICAL_YEARS)) %>%
        group_by(iso) %>%
        summarise(value = sum(value)) %>%
        mutate(GCAM_demand = "meat") %>%
        bind_rows(L134.FAO_ag_Food_t) %>%
        left_join_keep_first_only(select(AGLU_ctry, FAO2050_reg, iso), by = "iso") ->
        L134.Food_t_ctry_Dmnd_Y

      # ===== HAVE RUN TO HERE, ALL GOOD

      # Calculate the future demand ratios by FAO2050 region for each demand type
      # Drop unnecessary composite regions from FAO diet table
      # Original lines 71-85
      FAO2050_Diet %>%
        filter(FAO2050_reg %in% L134.Food_t_ctry_Dmnd_Y$FAO2050_reg) %>%
        # Calculate FAO2050 diet for specified diet years
        # Use linear interpolation to convert FAO2050 model time periods to GCAM "diet years"
        mutate(diet_years = approx_fun(year, value)) %>%
        # Add caloric content and demand category
        left_join(select(FAO2050_items_cal, FAO2050_item, GCAM_demand, kcalkg, conv_d), by = "FAO2050_item") ->
        L134.Diet_Rfao_Cfao_Yfao

      # Build new table with only diet years subsetted
      # Multiply through by caloric contents to get all measures in kcal/pers/d, by FAO region and food categories
      # Original lines 87-92
      L134.Diet_Rfao_Cfao_Yfao %>%
        filter(year %in% DIET_YEARS) %>%
        mutate(value = value * kcalkg / conv_d) %>%
        select(FAO2050_reg, FAO2050_item, GCAM_demand) ->
        L134.Diet_kcald_Rfao_Cfao_Y



      # Aggregate by GCAM demand and FAO region, and compute future diet ratios by FAO2050 region
      # Original lines 94-104
      L134.Diet_kcald_Rfao_Cfao_Y %>%
        group_by(FAO2050_reg, GCAM_demand) %>%
        summarise(value = sum(value)) ->
        L134.Diet_kcald_Rfao_Dmnd_Y

      # not sure following will work
      L134.Diet_kcald_Rfao_Dmnd_Y %>%
        mutate(GCAM_demand = if_else(GCAM_demand == "total", "crops", GCAM_demand),
               value = if_else(GCAM_demand == "crops", value - filter(L134.Diet_kcald_Rfao_Dmnd_Y, GCAM_demand == "meat")), value) ->
        L134.Diet_kcald_Rfao_Dmnd_Y

      #    L134.DietRatio_Rfao_Dmnd_Y[ X_diet_years ] <- L134.Diet_kcald_Rfao_Dmnd_Y[ X_diet_years ] / L134.Diet_kcald_Rfao_Dmnd_Y[[ X_diet_years[1] ]]
      # ??
      L134.Diet_kcald_Rfao_Dmnd_Y %>%
        group_by() %>%
        mutate(value = value - first(value, order_by = year)) ->
        L134.DietRatio_Rfao_Dmnd_Y

      # Multiply these ratios by the starting values at the country level
      # Original lines 106-110
      L134.Food_t_ctry_Dmnd_Y

      L134.Food_t_ctry_Dmnd_Y[ X_diet_years ] <- L134.Food_t_ctry_Dmnd_Y[[ X_final_historical_year ]] * L134.DietRatio_Rfao_Dmnd_Y[
        match( vecpaste( L134.Food_t_ctry_Dmnd_Y[ c( "FAO2050_reg", "GCAM_demand" ) ] ),
               vecpaste( L134.DietRatio_Rfao_Dmnd_Y[ c( "FAO2050_reg", "GCAM_demand" ) ] ) ),
        X_diet_years ]

    }

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y") %>%
      add_precursors("aglu/A_FoodDemand_SSPs", "common/iso_GCAM_regID", "aglu/AGLU_ctry",
                     "aglu/FAO2050_items_cal", "aglu/FAO2050_Diet",
                     "L100.FAO_ag_Food_t", "L100.FAO_an_Food_t",
                     "L101.ag_Food_Pcal_R_C_Y", "L101.Pop_thous_R_Yh",
                     "temp-data-inject/L105.an_Food_Pcal_R_C_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp1") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp2") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp3") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp4") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L134.pcFood_kcald_R_Dmnd_Y_ssp5") %>%
      same_precursors_as(L134.pcFood_kcald_R_Dmnd_Y) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L134.pcFood_kcald_R_Dmnd_Y_ssp5

    return_data(L134.pcFood_kcald_R_Dmnd_Y, L134.pcFood_kcald_R_Dmnd_Y_ssp1, L134.pcFood_kcald_R_Dmnd_Y_ssp2, L134.pcFood_kcald_R_Dmnd_Y_ssp3, L134.pcFood_kcald_R_Dmnd_Y_ssp4, L134.pcFood_kcald_R_Dmnd_Y_ssp5)
  } else {
    stop("Unknown command")
  }
}
