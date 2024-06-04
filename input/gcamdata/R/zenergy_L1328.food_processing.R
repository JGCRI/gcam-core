# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1328.food_processing
#'
#' Sets up input, output, and IO coefficients for food processing and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1328.in_EJ_R_food_F_Yh}, \code{L1328.out_Pcal_R_food_Yh},
#' \code{L1328.IO_EJPcal_R_food_F_Yh},\code{L1328.in_EJ_R_indenergy_F_Yh}, \code{L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh},
#' \code{L1328.en_bal_frac_industry_food_inonspec_R_Yh}.
#' @details This chunk obtains input and output and coefficients for the food processing sector and subtracts input energy from industry
#' energy use. Also infills some historical food processing energy use data for regions where data are limited.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author SAS Dec 2022
module_energy_L1328.food_processing <- function(command, ...) {

  MODULE_INPUTS <-
    c("L1012.en_bal_EJ_R_Si_Fi_Yh",
      FILE = "energy/A_regions",
      FILE = "energy/mappings/enduse_fuel_aggregation",
      "L1327.in_EJ_R_indenergy_F_Yh",
      FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "energy/A328.globaltech_coef",
      #"L127.in_EJ_R_indchp_F_Yh",
      "L101.CropMeat_Food_Pcal_R_C_Y",
      "L100.IEA_en_bal_ctry_hist",
      FILE = "energy/mappings/IEA_product_fuel",
      FILE = "energy/mappings/enduse_fuel_aggregation",
      FILE = "energy/mappings/IEA_flow_sector",
      FILE = "energy/A328.energy_infill_model_coefs",
      FILE = "energy/A328.energy_infill_model_intercepts_R",
      FILE = "aglu/A_demand_technology",
      "L102.gdp_mil90usd_Scen_R_Y")

  MODULE_OUTPUTS <-
    c("L1328.in_EJ_R_food_F_Yh",
      "L1328.out_Pcal_R_food_Yh",
      "L1328.IO_EJPcal_R_food_F_Yh",
      "L1328.in_EJ_R_indenergy_F_Yh",
      "L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh",
      "L1328.en_bal_frac_industry_food_inonspec_R_Yh")
      #"L1328.in_EJ_R_indchp_F_Yh"))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    GCAM_region_ID <- year <- value <- calOutputValue <- sector <- fuel <-
      industry <- has_district_heat <- region_heat <- raw <- value.x <- value.y <-
      input <- output <- supplysector <- subsector <- technology <- minicam.energy.input <-
      secondary.output <- coef <- value_heat <- frac <- flow_code <- conversion <- iso <-
      FLOW <- PRODUCT <- country_name <- industry <- EJ <- Pcal <- EJ_per_Pcal <- flow_type <- frac_food <-
      frac_nonspec <- food_EJ_per_Pcal <- variable <- coefficient <- intercept <- EJ_est <-
      nonstaples_Pcal <- GDP_mil90USD <- infill_EJ <- infill_fuel <- infill_total <- share <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # --------------------------------------------------------------------------
    # First get food production and energy use data

    # aggregate food production (from AGLU module) to total calories produced
    L101.CropMeat_Food_Pcal_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(value = sum(calOutputValue)) %>%
      ungroup() ->
      L1328.out_Pcal_R_food_Yh

    # also aggregate to calories of staples and nonstaples
    L101.CropMeat_Food_Pcal_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A_demand_technology %>%
                                 select(supplysector, technology) %>%
                                 filter(supplysector %in% c("FoodDemand_Staples", "FoodDemand_NonStaples")),
                               by = c("GCAM_commodity" = "technology")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      group_by(GCAM_region_ID, year, supplysector) %>%
      summarize(value = sum(calOutputValue)) %>%
      ungroup() ->
      L1328.Pcal_sector_R_food_Yh

    # get food processing energy use from IEA energy balances
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("food", sector)) ->
      L1328.in_EJ_R_food_F_Yh

    # map fuel used to fuel aggregations
    L1328.in_EJ_R_food_F_Yh %>%
      left_join_error_no_match(
        select(enduse_fuel_aggregation, fuel, industry) %>% na.omit(), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()->
      L1328.in_EJ_R_food_F_Yh


    # Drop heat in regions where heat is not modeled as a final fuel
    A_regions %>%
      filter(has_district_heat == 0) %>%
      select(GCAM_region_ID) %>%
      unique %>%
      mutate(fuel = "heat") ->
      region_heat

    L1328.in_EJ_R_food_F_Yh  %>%
      anti_join(region_heat, by = c("GCAM_region_ID", "fuel")) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L1328.in_EJ_R_food_F_Yh

    # Calculate remaining industrial energy use
    L1327.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID, nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      # NA is expected for fuel = heat sector and replaced with 0
      left_join(L1328.in_EJ_R_food_F_Yh %>% select(-sector), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1328.in_EJ_R_indenergy_F_Yh_tmp

    # Check for negative energy use in remaining industry
    L1328.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) ->
      indenergy_tmp

    # Adjust negative energy use in remaining industry by adjusting energy in food processing sector downward
    L1328.in_EJ_R_food_F_Yh %>%
      # NA is expected and replaced with 0 later
      left_join(indenergy_tmp %>% select(-sector), by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value.y = replace_na(value.y, 0),
             value = value.x + value.y) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1328.in_EJ_R_food_F_Yh_recal

    # Recalculate remaining industrial energy
    L1327.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      # NA is expected and replaced with 0 later
      left_join(L1328.in_EJ_R_food_F_Yh_recal %>% select(-sector), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value, raw = NULL) ->
      L1328.in_EJ_R_indenergy_F_Yh_tmp_2

    # --------------------------------------------------------------------------
    # Allocate some of the "other industry" energy use to food processing energy use in regions that have little or no
    # energy used in food processing as well as a large fraction of their industry energy in "non-specified industry energy"
    # in the IEA data

    # First calculate the fraction of total industry energy use that is in food processing and non-specified industry, respectively,
    # from the IEA energy balances, assuming the IEA energy balances data are available - otherwise, use prebuilt data for this
    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running without the proprietary IEA data files).
    # If this is the case, we substitute pre-built dataset.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used saved outputs
      L1328.en_bal_frac_industry_food_inonspec_R_Yh <- extract_prebuilt_data("L1328.en_bal_frac_industry_food_inonspec_R_Yh")
    } else {
      # Get industry consumption of fuel by sector and fuel for GCAM regions
      ind_en_IEA_conv <- unique(IEA_flow_sector %>%
                                  filter(flow_code %in% energy.FOOD_PROCESSING.IEA_INDUSTRY_FLOWS & !is.na(conversion)) %>%
                                  pull(conversion))[1]

      L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW %in% energy.FOOD_PROCESSING.IEA_INDUSTRY_FLOWS) %>%
        select(iso, FLOW, PRODUCT, as.character(HISTORICAL_YEARS)) %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID, country_name), by = "iso") %>%
        # fuel could has NA
        left_join(select(IEA_product_fuel, fuel, PRODUCT = product), by = "PRODUCT") %>%
        left_join(enduse_fuel_aggregation %>% dplyr::select(fuel, industry), by = "fuel") %>%
        # remap biomass_tradbio to biomass, as is done for industry in other chunks
        mutate(industry = if_else(fuel == "biomass_tradbio", "biomass", industry)) %>%
        gather_years() %>%
        rename(fuel_orig = fuel, fuel = industry) %>%
        filter(!is.na(fuel)) %>%
        group_by(GCAM_region_ID, year, fuel, FLOW) %>%
        # summarize and convert to EJ
        summarize(value = sum(value) * ind_en_IEA_conv) %>%
        ungroup() ->
        IEA_ind_EJ_sect_R_F_Yh

      # Aggregate to total energy consumption by sector
      IEA_ind_EJ_sect_R_F_Yh %>%
        group_by(GCAM_region_ID, year, FLOW) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        # make sure all sectors and years are present
        complete(nesting(GCAM_region_ID),
                 year = HISTORICAL_YEARS,
                 FLOW = energy.FOOD_PROCESSING.IEA_INDUSTRY_FLOWS) %>%
        mutate(value = replace_na(value, 0)) %>%
        # calculate the fraction of each sector to total industry energy
        group_by(GCAM_region_ID, year) %>%
        mutate(frac = value / sum(value)) %>%
        ungroup() %>%
        # there could be NAs in sector mappings; dealed with NAs later
        left_join(IEA_flow_sector %>% select(sector, flow_code), by = c("FLOW" = "flow_code")) ->
        IEA_ind_EJ_sect_R_Yh

      # Just select the fraction of total energy in food processing and non-specified industry
      # This will be stored as a prebuilt data output
      IEA_ind_EJ_sect_R_Yh %>%
        filter(FLOW == energy.FOOD_PROCESSING.IEA_FOODPRO_FLOW | FLOW == energy.FOOD_PROCESSING.IEA_INONSPEC_FLOW) %>%
        select(GCAM_region_ID, year, FLOW, frac) %>%
        mutate(flow_type = if_else(FLOW == energy.FOOD_PROCESSING.IEA_FOODPRO_FLOW, "frac_food", "frac_nonspec")) %>%
        select(-FLOW) %>%
        spread(key = flow_type, value = frac) ->
        L1328.en_bal_frac_industry_food_inonspec_R_Yh

      # Produce output
      L1328.en_bal_frac_industry_food_inonspec_R_Yh %>%
        add_title("Fraction of the total industry energy from the IEA energy balances that is in the food processing sector and the non-specified industry sector by region") %>%
        add_units("none") %>%
        add_comments("Calculated as the share of total industry energy (from all of the IEA industry sectors) that is in food processing") %>%
        add_comments("And the share that is in non-specified industry energy") %>%
        add_legacy_name("L1328.en_bal_frac_industry_food_inonspec_R_Yh") %>%
        add_precursors("common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation", "L100.IEA_en_bal_ctry_hist",
                       "energy/mappings/IEA_product_fuel", "energy/mappings/IEA_flow_sector") ->
        L1328.en_bal_frac_industry_food_inonspec_R_Yh

      # At this point output should be identical to the prebuilt versions
      verify_identical_prebuilt(L1328.en_bal_frac_industry_food_inonspec_R_Yh)
    }

    # Next obtain the coefficient of total food processing energy use per calorie consumed
    L1328.in_EJ_R_food_F_Yh_recal %>%
      group_by(GCAM_region_ID, year, sector) %>%
      summarize(EJ = sum(value)) %>%
      ungroup() %>%
      # only select years shared by the calorie data
      filter(year %in% unique(L1328.out_Pcal_R_food_Yh$year)) %>%
      left_join_error_no_match(L1328.out_Pcal_R_food_Yh %>%
                                 rename(Pcal = value),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(EJ_per_Pcal = EJ / Pcal) ->
      L1328.in_EJ_out_Pcal_R_food_Yh

    # Find regions and years that will have their data infilled - these are regions where either
    # a) the fraction of total industry energy in food processing is too low
    # b) the fraction of total industry energy that is in non-specified industry is too high, AND food processing energy fraction is still relatively low
    # AND the coefficient of food processing energy use (energy per calorie consumed) is below the minimum value for regions with good data (which was calculated separately)
    L1328.en_bal_frac_industry_food_inonspec_R_Yh %>%
      # only select years shared by the calorie data
      filter(year %in% unique(L1328.out_Pcal_R_food_Yh$year)) %>%
      # add EJ per Pcal coefficient
      left_join_error_no_match(L1328.in_EJ_out_Pcal_R_food_Yh %>%
                                 select(GCAM_region_ID, year, food_EJ_per_Pcal = EJ_per_Pcal),
                               by = c("GCAM_region_ID", "year")) ->
      IEA_ind_frac_food_nonspec_EJ_Pcal_R_Yh_temp

    # filter to just regions and years that meet the criteria to infill data
    IEA_ind_frac_food_nonspec_EJ_Pcal_R_Yh_temp %>%
      filter(year >= energy.FOOD_PROCESSING.ENERGY_INFILL_START_YEAR &
               (frac_food < energy.FOOD_PROCESSING.ENERGY_INFILL_MIN_FOODPRO_FRAC |
                  (frac_nonspec > energy.FOOD_PROCESSING.ENERGY_INFILL_MAX_INONSPEC_FRAC &
                     frac_food < energy.FOOD_PROCESSING.ENERGY_INFILL_FOODPRO_FRAC_OVERRIDE)) &
               food_EJ_per_Pcal < energy.FOOD_PROCESSING.ENERGY_INFILL_MIN_EJ_PCAL_COEF) %>%
      select(GCAM_region_ID, year) ->
      R_Yh_to_infill

    # Calculate the amount of energy that should be infilled, based on the derived relationship between non-staple calorie consumption,
    # GDP, and food processing energy use
    # first obtain the coefficients of non-staple calories and GDP
    nonstaples_coef_EJ_Pcal <- A328.energy_infill_model_coefs %>% filter(variable == "nonstaples calories") %>% pull(coefficient)
    GDP_coef_EJ_mil90USD <- A328.energy_infill_model_coefs %>% filter(variable == "GDP") %>% pull(coefficient)
    # get default intercept value for regions that were not used in the model-fitting
    intercept_default <- A328.energy_infill_model_intercepts_R %>% filter(region == "default") %>% pull(intercept)
    # generate warning if any region was not explicitly specified in the intercepts input file
    # we are using the default intercept value for the new region in that case
    if(!all(unique(L1328.Pcal_sector_R_food_Yh$GCAM_region_ID) %in% unique(A328.energy_infill_model_intercepts_R$GCAM_region_ID))) {
      warning("New region added that is not specified in A328.energy_infill_model_intercepts_R.csv, default intercept value used")
    }

    L1328.Pcal_sector_R_food_Yh %>%
      mutate(supplysector = if_else(grepl("nonstaples", tolower(supplysector)), "nonstaples_Pcal", "staples_Pcal")) %>%
      spread(supplysector, value) %>%
      # add GDP, selecting just historical values
      left_join_error_no_match(L102.gdp_mil90usd_Scen_R_Y %>%
                                 # NOTE: at present historical GDP is the same in all SSP scenarios
                                 # if that assumption changes we will want to revist this methodology
                                 filter(scenario == socioeconomics.CORE_GCAM_SCENARIO, year %in% HISTORICAL_YEARS) %>%
                                 select(GCAM_region_ID, year, GDP_mil90USD = value),
                               by = c("GCAM_region_ID", "year")) %>%
      # add the regional intercepts of the model
      left_join_error_no_match(A328.energy_infill_model_intercepts_R %>%
                  select(GCAM_region_ID, intercept),
                by = c("GCAM_region_ID")) %>%
      # for any region not explicitly specified in the intercepts input file, use default intercept value
      mutate(intercept = replace_na(intercept, intercept_default),
             # calculate estimated energy use in food processing
             EJ_est = nonstaples_Pcal * nonstaples_coef_EJ_Pcal + GDP_mil90USD * GDP_coef_EJ_mil90USD + intercept,
             # if any estimated values are negative, replace them with 0; this only happens for a couple regions in historical (pre-2010) years
             EJ_est = if_else(EJ_est < 0, 0, EJ_est)) ->
      L1328.EJ_R_food_Yh_est

    # Select just for regions to infill
    R_Yh_to_infill %>%
      left_join_error_no_match(L1328.EJ_R_food_Yh_est %>% select(GCAM_region_ID, year, EJ_est),
                               by = c("GCAM_region_ID", "year")) ->
      L1328.EJ_R_food_Yh_est_to_infill

    # Calculate the difference between current food processing energy use and the estimated values for these regions and years
    L1328.EJ_R_food_Yh_est_to_infill %>%
      left_join_error_no_match(L1328.in_EJ_R_food_F_Yh_recal %>%
                                 group_by(GCAM_region_ID, year) %>%
                                 summarize(EJ = sum(value)) %>%
                                 ungroup(),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(infill_EJ = EJ_est - EJ,
             # again check to make sure the infilling actually adds energy
             infill_EJ = if_else(infill_EJ < 0, 0, infill_EJ)) ->
      L1328.EJ_R_food_Yh_est_to_infill_adders

    # Now do the infilling
    # First obtain the shares of each fuel to total remaining industry energy use
    L1328.in_EJ_R_indenergy_F_Yh_tmp_2 %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() ->
      L1328.in_EJ_R_indenergy_F_Yh_tmp_2_shares

    # Obtain the amount of each fuel to pull from remaining industry energy use
    # This is computed as the total amount of energy that needs to be infilled multiplied by the fuel's share of remaining industry energy use
    L1328.in_EJ_R_indenergy_F_Yh_tmp_2_shares %>%
      right_join(L1328.EJ_R_food_Yh_est_to_infill_adders %>%
                                 select(GCAM_region_ID, year, infill_total = infill_EJ),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(infill_fuel = infill_total * share,
             # if the amount demanded for infilling is larger than the available energy in other industry for that fuel,
             # just take all of the available energy in other industry, to ensure there is no negative energy values later
             # in other industry
             infill_fuel = if_else(infill_fuel > value, value, infill_fuel)) ->
      L1328.EJ_R_food_F_Yh_est_to_infill_adders

    # Recalculate food processing energy use - add the infilled values to food processing energy use
    L1328.in_EJ_R_food_F_Yh_recal %>%
      full_join(L1328.EJ_R_food_F_Yh_est_to_infill_adders %>% select(GCAM_region_ID, fuel, year, infill_fuel),
                by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = replace_na(value, 0),
             infill_fuel = replace_na(infill_fuel, 0),
             value = value + infill_fuel) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1328.in_EJ_R_food_F_Yh_recal_2

    # Recalculate other industry energy use - subtract off the infilled values allocated to food processing
    L1328.in_EJ_R_indenergy_F_Yh_tmp_2 %>%
      # replace NA later
      left_join(L1328.EJ_R_food_F_Yh_est_to_infill_adders %>% select(GCAM_region_ID, fuel, year, infill_fuel),
                by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(infill_fuel = replace_na(infill_fuel, 0),
             removed_frac = infill_fuel / value,
             removed_frac = replace_na(removed_frac, 0),
             value = value - infill_fuel) ->
      L1328.in_EJ_R_indenergy_F_Yh_tmp_3

    L1328.in_EJ_R_indenergy_F_Yh_tmp_3 %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1328.in_EJ_R_indenergy_F_Yh

    L1328.in_EJ_R_indenergy_F_Yh_tmp_3 %>%
      select(GCAM_region_ID, fuel, year, sector, value, infill_fuel, removed_frac) ->
      L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh

    # --------------------------------------------------------------------------
    # Future development could allocate some of the CHP energy from other industry to food processing

    # For biomass CHP, could allocate 100% of the remaining biomass CHP to food processing (as pulp and paper has already been accounted for)

    L1328.in_EJ_R_food_F_Yh_recal_2 ->
      L1328.in_EJ_R_food_F_Yh

    # --------------------------------------------------------------------------
    # Break out process heating

    # interpolate global tech coefficients to all years
    A328.globaltech_coef %>%
      filter(grepl("process heat", supplysector)) %>% # only want process heat technologies
      select(technology) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, FUTURE_YEARS))) %>%
      full_join(
        A328.globaltech_coef %>%
          gather(-c(supplysector, subsector, technology, minicam.energy.input, secondary.output) , key = 'year', value = 'coef') %>%
          select(technology, year, coef) %>%
          mutate(year = as.numeric(year)),
        by = c("technology", "year")) %>%
      group_by(technology) %>%
      mutate(coef = approx_fun(year, coef, rule = 2)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      distinct() %>%
      na.omit() %>%
      ungroup() ->
      L1328.globaltech_coef_process_heat

    # combine with energy inputs by fuel and calculate process heat output using global tech coefficients
    process_heat_techs <- unique(L1328.globaltech_coef_process_heat$technology)
    electric_techs <- unique((A328.globaltech_coef %>% filter(grepl("electric", technology)))$technology)

    L1328.in_EJ_R_food_F_Yh %>%
      rename(technology = fuel) %>%
      # assume no electricity in heat production in historical years
      filter(technology %in% process_heat_techs & !technology %in% electric_techs) %>%
      left_join_error_no_match(L1328.globaltech_coef_process_heat, by = c("technology", "year")) %>%
      mutate(value_heat = value / coef) %>%
      group_by(GCAM_region_ID, year, sector) %>%
      summarize(value = sum(value_heat)) %>%
      ungroup() %>%
      mutate(fuel = "heat") -> # assigning all non-electricity fuels to process heat
      L1328.total_process_heat_calculated

    # calculate input output coefficients for electricity and process heat
    L1328.in_EJ_R_food_F_Yh %>%
      filter(fuel == "electricity") %>%
      bind_rows(L1328.total_process_heat_calculated) %>%
      rename(input = value) %>%
      # unimportant year diff & replace NA later
      left_join(L1328.out_Pcal_R_food_Yh %>% rename(output = value), by = c("GCAM_region_ID", "year")) %>%
      mutate(output = replace_na(output, 0),
             value = if_else(output > 0, input / output, 0)) ->
      L1328.IO_EJPcal_R_food_F_Yh

    # =======================================================
    # Produce other outputs

    L1328.in_EJ_R_food_F_Yh %>%
      add_title("Historical input energy use for the food processing sector") %>%
      add_units("EJ") %>%
      add_comments("FOODPRO sector from IEA energy balances aggregated to GCAM regions") %>%
      add_comments("Some regions' energy use data are infilled (by using an externally-derived relationship") %>%
      add_comments("between food processing energy use and calorie consumption and GDP) when IEA data are limited") %>%
      add_legacy_name("L1328.in_EJ_R_food_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/A_regions",
                     "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation",
                     "L1327.in_EJ_R_indenergy_F_Yh", "L101.CropMeat_Food_Pcal_R_C_Y", "L100.IEA_en_bal_ctry_hist",
                     "energy/mappings/IEA_product_fuel", "energy/mappings/IEA_flow_sector", "energy/A328.energy_infill_model_coefs",
                     "energy/A328.energy_infill_model_intercepts_R", "aglu/A_demand_technology", "L102.gdp_mil90usd_Scen_R_Y") ->
      L1328.in_EJ_R_food_F_Yh

    L1328.out_Pcal_R_food_Yh %>%
      add_title("Historical food production by region and year") %>%
      add_units("Pcal") %>%
      add_comments("Output from the AGLU module aggregated across all crops") %>%
      add_legacy_name("L1328.out_Pcal_R_food_Yh") %>%
      add_precursors("L101.CropMeat_Food_Pcal_R_C_Y", "common/GCAM_region_names") ->
      L1328.out_Pcal_R_food_Yh

    L1328.IO_EJPcal_R_food_F_Yh %>%
      add_title("Input-output coefficients for food processing") %>%
      add_units("EJ/Pcal food") %>%
      add_legacy_name("L1328.IO_EJPcal_R_food_F_Yh") %>%
      add_comments("IO coefficients for heat energy and electricity are calculated from IEA and infilled energy consumption and AGLU module calorie production data") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/A_regions", "common/iso_GCAM_regID",
                     "energy/mappings/enduse_fuel_aggregation", "L1327.in_EJ_R_indenergy_F_Yh",
                     "energy/A328.globaltech_coef", "L101.CropMeat_Food_Pcal_R_C_Y",
                     "common/GCAM_region_names", "L100.IEA_en_bal_ctry_hist",
                     "energy/mappings/IEA_product_fuel", "energy/mappings/IEA_flow_sector", "energy/A328.energy_infill_model_coefs",
                     "energy/A328.energy_infill_model_intercepts_R", "aglu/A_demand_technology", "L102.gdp_mil90usd_Scen_R_Y") ->
      L1328.IO_EJPcal_R_food_F_Yh

    L1328.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("EJ") %>%
      add_comments("Subtracted food processing energy use from industrial energy use values in L1327.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use; taking into account infilling of food processing energy use data") %>%
      add_legacy_name("L1328.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh", "L1327.in_EJ_R_indenergy_F_Yh",
                     "energy/A_regions", "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation", "L101.CropMeat_Food_Pcal_R_C_Y", "L100.IEA_en_bal_ctry_hist",
                     "energy/mappings/IEA_product_fuel", "energy/mappings/IEA_flow_sector", "energy/A328.energy_infill_model_coefs",
                     "energy/A328.energy_infill_model_intercepts_R", "aglu/A_demand_technology", "L102.gdp_mil90usd_Scen_R_Y") ->
      L1328.in_EJ_R_indenergy_F_Yh

    L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh %>%
      add_title("Adjustments to the historical input energy balances for industrial energy use indicating the amount of energy infilled for food processing") %>%
      add_units("EJ") %>%
      add_comments("From the initial adjusted industrial energy use (subtracting food processing energy use from L1327.in_EJ_R_indenergy_F_Yh)") %>%
      add_comments("Energy that is used to infill for food processing for some regions is further subtracted") %>%
      add_legacy_name("L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh", "L1327.in_EJ_R_indenergy_F_Yh",
                     "energy/A_regions", "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation", "L101.CropMeat_Food_Pcal_R_C_Y", "L100.IEA_en_bal_ctry_hist",
                     "energy/mappings/IEA_product_fuel", "energy/mappings/IEA_flow_sector", "energy/A328.energy_infill_model_coefs",
                     "energy/A328.energy_infill_model_intercepts_R", "aglu/A_demand_technology", "L102.gdp_mil90usd_Scen_R_Y") ->
      L1328.in_EJ_R_indenergy_infilled_for_food_F_Yh

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
