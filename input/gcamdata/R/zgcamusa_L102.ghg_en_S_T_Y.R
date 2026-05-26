# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L102.ghg_en_S_T_Y
#'
#' Calculates CH4 and N2O emission factors derived from EPA GHG inventory and GCAM energy balances for the US in 2005.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.ghg_tgej_USA_en_Sepa_F_2005}, \code{L102.res_ghg_tgej_USA}. The corresponding file in the
#' original data system was \code{L102.ghg_en_USA_S_T_Y.R} (emissions level1).
#' @details Divides CH4 and N2O emissions from EPA GHG inventory by GCAM energy sector activity to get emissions factors for a single historical year 2005 in the US.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter if_else group_by left_join mutate select summarize summarize_if
#' @author HCM April 2017
module_gcamusa_L102.ghg_en_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "emissions/mappings/EPA_ghg_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/GCAM_sector_tech_Revised",
             FILE = "gcam-usa/emissions/EPA_fossil_rsrc_GHG_mapping",
             "L101.in_EJ_R_en_Si_F_Yh",
             "L111.Prod_EJ_R_F_Yh_USA",
             FILE = "emissions/EPA_FCCC_GHG_2005",
             FILE = "gcam-usa/emissions/EPA_state_res_ghg_2021"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.ghg_tgej_USA_en_Sepa_F_2005",
             "L102.res_ghg_tgej_USA"))
  } else if(command == driver.MAKE) {

    year <- energy <- sector <- fuel <- technology <- GCAM_region_ID <- CO2 <-
      . <- EPA_agg_sector <- EPA_agg_sector <- EPA_agg_fuel_ghg <- CH4 <-
      N2O <- ch4_em_factor <- n2o_em_factor <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    EPA_ghg_tech <- get_data(all_data, "emissions/mappings/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")

    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_Revised")

    }


    get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
      gather_years(value_col = "energy") ->
      L101.in_EJ_R_en_Si_F_Yh
    EPA_FCCC_GHG_2005 <- get_data(all_data, "emissions/EPA_FCCC_GHG_2005")
    EPA_state_res_ghg_2021 <- get_data(all_data, "gcam-usa/emissions/EPA_state_res_ghg_2021")
    EPA_fossil_rsrc_GHG_mapping <- get_data(all_data, "gcam-usa/emissions/EPA_fossil_rsrc_GHG_mapping")
    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = T)

    # Convert EPA GHG emissions inventory to Tg and aggregate by sector and fuel
    EPA_FCCC_GHG_2005 %>% # start from EPA GHG
      left_join(EPA_ghg_tech, by = "Source_Category") %>% # define category
      select(-CO2) %>% # non-CO2 only
      replace_na(list(CH4 = 0, N2O = 0)) %>%
      # Primary fossil production (coal, oil, gas) emissions does not exist in the EPA
      # inventory.  We will leave a place holder for now which will get scaled
      # to match EDGAR downstream.
      mutate(CH4 = if_else(sector %in% c("coal", "oil_gas"), 1, CH4),
             N2O = if_else(sector %in% c("coal", "oil_gas"), 1, N2O)) %>%
      group_by(sector, fuel) %>%
      summarize(CH4 = sum(CH4) * CONV_GG_TG,
                N2O = sum(N2O) * CONV_GG_TG) %>% # sum by sector and fuel and change units
      filter(!is.na(sector), !is.na(fuel)) %>% # delete NA sectors and fuel
      ungroup() ->
      L102.ghg_tg_USA_en_Sepa_F_2005 # GHG balance in 2005

    # organize energy balances in USA 2005
    L101.in_EJ_R_en_Si_F_Yh %>% # start from energy balances
      filter(GCAM_region_ID == gcam.USA_CODE, year == 2005) %>% # 2005 USA data only
      select(-GCAM_region_ID, -year) %>%
      left_join(select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel_ghg),
                by = c("sector", "fuel", "technology")) %>% # assign aggregate sector and fuel names
      group_by(EPA_agg_sector, EPA_agg_fuel_ghg) %>%
      summarize_if(is.numeric, sum) -> # sum by aggregate sector and fuel
      L102.in_EJ_USA_en_Sepa_F_2005 # energy balance in 2005

    # combine emissions and energy to get emission factors
    L102.ghg_tg_USA_en_Sepa_F_2005 %>%
      left_join(L102.in_EJ_USA_en_Sepa_F_2005, by = c("sector" = "EPA_agg_sector", "fuel" = "EPA_agg_fuel_ghg")) %>% # energy data and emission data joined
      mutate(ch4_em_factor = CH4 / energy, # emission factor calculated
             n2o_em_factor = N2O / energy) %>% # emission factor calculated
      select(-CH4, -N2O, -energy) %>% # delete orignal data
      arrange(fuel) %>%
      mutate(ch4_em_factor = if_else(is.na(ch4_em_factor) | is.infinite(ch4_em_factor), 0, ch4_em_factor), # set NA and INF to zero
             n2o_em_factor = if_else(is.na(n2o_em_factor) | is.infinite(n2o_em_factor), 0, n2o_em_factor)) -> # set NA and INF to zero
      L102.ghg_tgej_USA_en_Sepa_F_2005


    #.....................................
    # fossil resource production emissions
    #.....................................

    EPA_state_res_emiss <- EPA_state_res_ghg_2021 %>%
      # year column headers have "Y" at start so need to gather manually
      tidyr::gather(-c(CATEGORY, SUBCATEGORY1, STATE, GHG),
             key = "year", value = "value") %>%
      mutate(year = as.numeric(gsub("Y", "", year))) %>%
      rename(region = STATE, ghg = GHG)

    # convert emissions to Tg and map to resources, subcategories, and subresources
    EPA_state_res_emiss %>%
      # units are CO2eq so need GWPs
      mutate(value = case_when(ghg == "CO2" ~ value/CONV_T_MT*CONV_T_KG*CONV_KG_TO_TG,
                               ghg == "CH4" ~ value/emissions.CH4.GWP.AR5/CONV_T_MT*CONV_T_KG*CONV_KG_TO_TG,
                               ghg == "N2O" ~ value/emissions.N2O.GWP.AR5/CONV_T_MT*CONV_T_KG*CONV_KG_TO_TG),
             units = "Tg") %>%
      # can't use ljenm because some of the epa activities don't correspond to
      # specific subresources/ technologies (only at the resource level)
      # Leave NAs in place and subsequently fill them using resource-level data where sub-resource information is missing.
      left_join(EPA_fossil_rsrc_GHG_mapping,
                by = c("CATEGORY", "SUBCATEGORY1")) %>%
      group_by(region, resource, SUBCATEGORY1, reserve.subresource,
              ghg, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS,
             # filter out subcategories we aren't including (refining, transport, etc.)
             resource != "none") ->
    L102.res_ghg_tg_USA

    # split into resource and subresource level portions
    L102.res_ghg_tg_USA %>%
      filter(is.na(reserve.subresource),
             region %in% gcamusa.STATES) %>%
      select(region, resource, ghg, year, value) ->
      L102.res_ghg_tg_USA_resource

    L102.res_ghg_tg_USA %>%
      filter(!is.na(reserve.subresource),
             region %in% gcamusa.STATES) %>%
      select(region, resource, reserve.subresource, ghg, year, value) ->
    L102.res_ghg_tg_USA_subresource


    # combine emissions and energy to calculate
    # emissions factors at the resource level
    L102.res_ghg_tg_USA_resource %>%
      rename(emiss = value) %>%
      # remove rows with 0 emissions
      filter(emiss != 0) %>%
      left_join_error_no_match(L111.Prod_EJ_R_F_Yh_USA %>%
                                 group_by(region, resource, year) %>%
                                 summarize(energy = sum(value)) %>%
                                 ungroup(),
                               by = c("region", "resource", "year")) %>%
      mutate(emfact = emiss/energy) %>%
      select(region, resource, ghg, year, emfact) %>%
      # copy resource emissions factors to each subresource
      # use left join because we're adding rows for each subresource within a resource
      left_join(unique(select(L111.Prod_EJ_R_F_Yh_USA, region, year, resource, reserve.subresource)),
                by = c("region", "year", "resource")) ->
    L102.res_ghg_tgej_USA_resource


    # emissions factors at the subresource level
    L102.res_ghg_tg_USA_subresource %>%
      rename(emiss = value) %>%
      # filter out rows with 0 emissions
      filter(emiss != 0) %>%
      # using left join because there are a few rows that have emissions from EPA
      # but not energy from GCAM. These will be filtered out and given a default
      # emissions factor later
      left_join(L111.Prod_EJ_R_F_Yh_USA %>%
                  rename(energy = value),
                by = c("region", "resource", "reserve.subresource", "year")) %>%
      filter(!is.na(energy)) %>%
      mutate(emfact = emiss/energy) %>%
      select(region, resource, reserve.subresource, ghg, year, emfact) ->
      L102.res_ghg_tgej_USA_subresource

    # combine all emissions factors for each subresource
    rbind(L102.res_ghg_tgej_USA_resource,
          L102.res_ghg_tgej_USA_subresource) %>%
      group_by(region, resource, reserve.subresource, ghg, year) %>%
      summarize(emfact = sum(emfact)) %>%
      ungroup() ->
      L102.res_ghg_tgej_USA_with_missing


    # calculate national median emfacts for all subresource- ghg combinations.
    # For combinations with no emissions value in any state, the na.rm will result
    # in a median of 0 which will be assigned to each state (these can be filtered out later)
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(resource, reserve.subresource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      repeat_add_columns(tibble(ghg = unique(L102.res_ghg_tg_USA_resource$ghg))) %>%
      left_join(L102.res_ghg_tgej_USA_with_missing,
                by = c("region", "resource", "reserve.subresource", "ghg", "year")) %>%
      # remove Inf emfacts so as not to skew the median (these will be replaced)
      filter(emfact != Inf | is.na(emfact)) %>%
      group_by(resource, reserve.subresource, ghg, year) %>%
      summarise(median_emfact = median(emfact, na.rm = T)) %>%
      replace_na(list(median_emfact = 0)) ->
    usa_national_median_res_emfacts

    # replace missing and Inf emissions factors with the national median
    # (this includes cases with zero production and nonzero emissions and vice versa)
    # create structure with all state-subresource-ghg combinations
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(resource, reserve.subresource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      repeat_add_columns(tibble(ghg = unique(L102.res_ghg_tg_USA_resource$ghg))) %>%
      # using left_join because there are some region/subresource/ghg combinations
      # that are missing EFs (the NAs will be filled in below with the national median)
      left_join(L102.res_ghg_tgej_USA_with_missing,
                by = c("region", "resource", "reserve.subresource", "ghg", "year")) %>%
      # join with national medians and use them to replace missing EFs and
      # EFs above a threshold (20 times the national median)
      left_join_error_no_match(usa_national_median_res_emfacts,
                               by = c("resource", "reserve.subresource", "ghg", "year")) %>%
      mutate(threshold = median_emfact * 20,
             emfact = case_when(is.na(emfact) | emfact == "Inf" | emfact > threshold ~ median_emfact,
                                T ~ emfact)) %>%
      select(region, resource, reserve.subresource, ghg, year, emfact) ->
      L102.res_ghg_tgej_USA_base

    # there are a couple subcategories (coal mining CO2 from methane flaring and
    # underground recovered & used for energy) that are not given at the state level
    # in the EPA data. Downscale these to the state level by production and add
    # to the other emfacts where appropriate
    L102.res_ghg_tg_USA %>%
      filter(region == "National") %>%
      select(-region) ->
      national_res_ghg

    # get the subresource level rows
    national_res_ghg %>%
      filter(!is.na(reserve.subresource)) %>%
      group_by(year, resource, reserve.subresource, ghg) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      national_res_ghg_subresource

    # get the resource level rows
    national_res_ghg %>%
      filter(is.na(reserve.subresource)) %>%
      group_by(year, resource, ghg) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      national_res_ghg_resource

    # calculate national emissions factors to add to all states
    # resource level
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(resource %in% unique(national_res_ghg_resource$resource),
             year >= min(national_res_ghg_resource$year)) %>%
      group_by(resource, year) %>%
      summarize(energy = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(national_res_ghg_resource,
                               by = c("resource", "year")) %>%
      mutate(emfact = value/energy) %>%
      select(resource, ghg, year, emfact) ->
      national_emfacts_resource

    # subresource level
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(reserve.subresource %in% unique(national_res_ghg_subresource$reserve.subresource),
             year >= min(national_res_ghg_subresource$year)) %>%
      group_by(resource, reserve.subresource, year) %>%
      summarize(energy = sum(value)) %>%
      left_join_error_no_match(national_res_ghg_subresource,
                               by = c("resource", "reserve.subresource", "year")) %>%
      mutate(emfact = value/energy) %>%
      select(resource, reserve.subresource, ghg, year, emfact) ->
      national_emfacts_subresource

    # add resource level emfacts to each state
    L102.res_ghg_tgej_USA_base %>%
      # using left_join because there are only national emfacts for a couple
      # resource- gas combinations
      left_join(national_emfacts_resource %>%
                  rename(national_emfact_rsrc = emfact),
                by = c("resource", "ghg", "year")) %>%
      left_join(national_emfacts_subresource %>%
                  rename(national_emfact_subrsrc = emfact),
                by = c("resource", "reserve.subresource", "ghg", "year")) %>%
      # add the national level emissions factors to the state level ones
      replace_na(list(national_emfact_rsrc = 0, national_emfact_subrsrc = 0)) %>%
      mutate(emfact = emfact + national_emfact_rsrc + national_emfact_subrsrc) %>%
      select(region, resource, reserve.subresource, ghg, year, emfact) ->
      L102.res_ghg_tgej_USA_with_national_emfacts

    # copy emissions factors from 1990 back to all previous historical years
    # (1990 is the first year with EPA emisssions data and earlier years have
    # been given placeholder 0s)
    L102.res_ghg_tgej_USA_with_national_emfacts %>%
      mutate(emfact = case_when(year < min(L102.res_ghg_tg_USA$year) ~ NA_real_,
                                T ~ emfact)) %>%
      group_by(region, resource, reserve.subresource, ghg) %>%
      mutate(emfact = approx_fun(year, emfact, rule = 2)) %>%
      ungroup() ->
      L102.res_ghg_tgej_USA


    # Produce outputs
    L102.ghg_tgej_USA_en_Sepa_F_2005 %>%
      add_title("GHG emissions factors for the USA energy sector by sector / fuel / 2005") %>%
      add_units("Tg/EJ") %>%
      add_comments("CH4 and N2O emission factors derived from EPA GHG inventory and GCAM energy balances for the US in 2005") %>%
      add_legacy_name("L102.ghg_tgej_USA_en_Sepa_F_2005") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/mappings/IEA_flow_sector",
                     "energy/mappings/IEA_product_fuel",
                     "emissions/mappings/EPA_ghg_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "emissions/EPA_FCCC_GHG_2005",
                     "L101.in_EJ_R_en_Si_F_Yh") ->
      L102.ghg_tgej_USA_en_Sepa_F_2005

    L102.res_ghg_tgej_USA %>%
      add_title("GHG emissions factors for USA state fossil resource production") %>%
      add_units("Tg/EJ") %>%
      add_comments("Emission factors derived from EPA state-level GHG inventory and GCAM-USA fossil resource production") %>%
      add_precursors("gcam-usa/emissions/EPA_state_res_ghg_2021",
                     "gcam-usa/emissions/EPA_fossil_rsrc_GHG_mapping",
                     "L111.Prod_EJ_R_F_Yh_USA") ->
      L102.res_ghg_tgej_USA

    return_data(L102.ghg_tgej_USA_en_Sepa_F_2005,
                L102.res_ghg_tgej_USA)
  } else {
    stop("Unknown command")
  }
}
