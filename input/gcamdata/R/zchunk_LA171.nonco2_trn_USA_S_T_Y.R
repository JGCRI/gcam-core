#' module_gcam.usa_LA171.nonco2_trn_USA_S_T_Y.R
#'
#' Generates GCAM-USA model inputs for transportation sector by states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{}, \code{},
# The corresponding file in the
#' original data system was \code{LA171.nonco2_trn_USA_S_T_Y.R} (gcam-usa level2).
#' @details Generates GCAM-USA model inputs for transportation sector by states.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD Aug 2018
module_gcam.usa_LA171.nonco2_trn_USA_S_T_Y.R <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/GREET2014_LDV_CNG_EFs_tgEJ",
             FILE = "gcam-usa/MARKAL_LDV_EFs_gpm",
             FILE = "gcam-usa/MARKAL_HDV_EFs_gpm",
             FILE = "gcam-usa/MARKAL_LDV_eff_vmtMJ",
             FILE = "gcam-usa/MARKAL_HDV_eff_vmtMJ",
             FILE = "gcam-usa/MOVES_vehicle_age_fractions",
             FILE = "gcam-usa/MOVES_source_type_pop",
             FILE = "gcam-usa/MOVES_src_type_reg_class_fractions",
             FILE = "gcam-usa/state_census_region",
             FILE = "gcam-usa/MARKAL_fuel_name_code",
             FILE = "gcam-usa/MARKAL_MOVES_class"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA",
             "L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GREET2014_LDV_CNG_EFs_tgEJ <- get_data(all_data, "gcam-usa/GREET2014_LDV_CNG_EFs_tgEJ")
    MARKAL_LDV_EFs_gpm <- get_data(all_data, "gcam-usa/MARKAL_LDV_EFs_gpm")
    MARKAL_HDV_EFs_gpm <- get_data(all_data, "gcam-usa/MARKAL_HDV_EFs_gpm")
    MARKAL_LDV_eff_vmtMJ <- get_data(all_data, "gcam-usa/MARKAL_LDV_eff_vmtMJ")
    MARKAL_HDV_eff_vmtMJ <- get_data(all_data, "gcam-usa/MARKAL_HDV_eff_vmtMJ")
    MOVES_vehicle_age_fractions <- get_data(all_data, "gcam-usa/MOVES_vehicle_age_fractions")
    MOVES_source_type_pop <- get_data(all_data, "gcam-usa/MOVES_source_type_pop")
    MOVES_src_type_reg_class_fractions <- get_data(all_data, "gcam-usa/MOVES_src_type_reg_class_fractions")
    state_census_region <- get_data(all_data, "gcam-usa/state_census_region")
    MARKAL_fuel_name_code <- get_data(all_data, "gcam-usa/MARKAL_fuel_name_code")
    MARKAL_MOVES_class <- get_data(all_data, "gcam-usa/MARKAL_MOVES_class")

    # Silence pacakge checks
    `2005` <- `2010` <- `2015` <- Class <- Fuel <- Fuel_name <- MARKAL_Class <-
      MOVES_Reg_Class <- MOVES_Source_Type <- Technology <- Unit <- Vintage <-
      ageFraction <- ageID <- fuel <- modelYearID <- palette <- pollutant <-
      regClassID <- region <- share <- sourceTypeID <- sourceTypePopulation <-
      sourceTypePopulation_agg <- state <- stmyFraction <- value <- value.x <-
      value.y <- variable <- vintage <- weight <- year <- yearID <- NULL
    # ==============================================================================

    MOVES_vehicle_age_fractions %>%
      filter(yearID %in% c(2005, 2010)) %>%
      mutate(Vintage = as.integer(yearID - ageID)) %>%
      # Categorize vintages into five-year bins since emissions factor data is provided
      # in 5 year increments
      mutate(Vintage = if_else(yearID == 2005 & Vintage <= 1990, 1990,
                              if_else(Vintage <= 1995, 1995,
                                     if_else(Vintage <= 2000, 2000,
                                            if_else(Vintage <= 2005, 2005, 2010))))) %>%
      group_by(sourceTypeID,yearID,Vintage) %>%
      summarise(ageFraction = sum(ageFraction)) ->
      MOVES_trn_age_fractions_Yb

    #MARKAL car classes all received emissions factors from one MOVES source category
    MARKAL_MOVES_class %>%
      filter(MARKAL_Class %in% c("Mini car", "Full size car", "Compact car")) ->
      MARKAL_MOVES_class_car

    MOVES_trn_age_fractions_Yb %>%
      filter(sourceTypeID %in% MARKAL_MOVES_class_car$MOVES_Source_Type) %>%
      repeat_add_columns(tibble("Class" = MARKAL_MOVES_class_car$MARKAL_Class)) ->
      MOVES_car_age_fractions_Yb


    #MARKAL "Commercial truck" class uses data from MOVES regulatory classes, which in turn map to source IDs.
    #Thus, will have to weight the source ID-sorted age fractions by the number of vehicles in each source ID
    #category that correspond to the relevant regulatory classes, and take an average.
    #First identify the regulatory classes corresponding to the "Commercial truck" MARKAL class
    MARKAL_MOVES_class %>%
      filter(MARKAL_Class == "Commercial truck") %>%
      pull(MOVES_Reg_Class) ->
      MOVES_comm_truck_reg_classes

    #Subset the table mapping the regulatory classes to MOVES source IDs
    MOVES_src_type_reg_class_fractions %>%
      filter(regClassID %in% MOVES_comm_truck_reg_classes & modelYearID %in% MOVES_trn_age_fractions_Yb$yearID) ->
      MOVES_comm_truck_reg_src_fractions

    #Subset the table mapping the regulatory classes to MOVES source IDs
    MOVES_source_type_pop %>%
      filter(yearID %in% MOVES_trn_age_fractions_Yb$yearID &
               sourceTypeID %in% MOVES_comm_truck_reg_src_fractions$sourceTypeID) %>%
      # TODO Use a left_join becasue we do not expect a 1:1 match. --- okay there was a bad match in the old data system, idk what is
      # was trying to do and if we want to
      left_join_keep_first_only(MOVES_comm_truck_reg_src_fractions, by = c("yearID" = "modelYearID", "sourceTypeID")) %>%
      mutate(weight = sourceTypePopulation * stmyFraction) %>%
      select(yearID, sourceTypeID, sourceTypePopulation, weight) ->
      MOVES_comm_truck_srcID_wts

    #Subset the relevant age fraction data, and use the weights to compute the average age fractions
    MOVES_trn_age_fractions_Yb %>%
      filter(sourceTypeID %in% MOVES_comm_truck_srcID_wts$sourceTypeID) %>%
      # TODO bad match in the old data system use left_join_keep_first_only to preserve old data sytem
      # behavior
      left_join_keep_first_only(MOVES_comm_truck_srcID_wts, by = c("yearID" , "sourceTypeID")) %>%
      group_by(yearID,Vintage) %>%
      summarise(ageFraction = weighted.mean(ageFraction, weight)) %>%
      mutate(Class = "Commercial truck") ->
      MOVES_comm_truck_age_fractions_Yb

    # For MARKAL classes including SUVs, Minivans, Pickups, buses, and heavy duty trucks,
    # data from multiple MOVES source categories were used, so the age fractions must be
    # multiplied by shares of each source type ID out of the total vehicle population of
    # the source IDs used in the corresponding MARKAL class. This will be done for each
    # vehicle using the age_fraction_share_function function.

    age_fraction_share_function <- function(input_vehicle){

      MARKAL_MOVES_class %>%
        filter(MARKAL_Class == input_vehicle) %>%
        pull(MOVES_Source_Type) ->
        MOVES_srcIDs

      # First compute the total vehicle population for these source IDs in the relevant years
      MOVES_source_type_pop %>%
        filter(sourceTypeID %in% MOVES_srcIDs & yearID %in% MOVES_trn_age_fractions_Yb$yearID) %>%
        group_by(yearID) %>%
        summarise(sourceTypePopulation = sum(sourceTypePopulation)) ->
        MOVES_agg_pop

      # Compute shares of the source ID categories out of the corresponding total vehicle population
      MOVES_source_type_pop %>%
        filter(sourceTypeID %in% MOVES_srcIDs & yearID %in% MOVES_trn_age_fractions_Yb$yearID) %>%
        left_join_keep_first_only(MOVES_agg_pop %>%
                                    rename(sourceTypePopulation_agg = sourceTypePopulation), by = "yearID") %>%
        mutate(share = sourceTypePopulation / sourceTypePopulation_agg) %>%
        select(yearID, sourceTypeID, sourceTypePopulation, share) ->
        MOVES_pop_shares

      #Now multiply the source ID shares by the age fractions to yield new age fractions for the aggregate MARKAL category
      MOVES_trn_age_fractions_Yb %>%
        filter(sourceTypeID %in% MOVES_srcIDs) %>%
        left_join_keep_first_only(MOVES_pop_shares, by = c("sourceTypeID", "yearID")) %>%
        mutate(ageFraction = ageFraction * share) %>%
        group_by(yearID,Vintage) %>%
        summarise(ageFraction = sum(ageFraction)) %>%
        mutate(Class = input_vehicle)

    }

    bind_rows(age_fraction_share_function("Minivan"),
              age_fraction_share_function("Pickup"),
              age_fraction_share_function("Large SUV"),
              age_fraction_share_function("Small SUV"),
              age_fraction_share_function("Bus"),
              age_fraction_share_function("Heavy duty long haul truck"),
              age_fraction_share_function("Heavy duty short haul truck")) ->
      MOVES_rest_age_fractions_Yb


    # Bind together the tables containing the age fractions of the LDVs in the base years
    bind_rows(MOVES_rest_age_fractions_Yb,
              MOVES_car_age_fractions_Yb,
              MOVES_comm_truck_age_fractions_Yb) %>%
      select(-sourceTypeID) %>%
      rename(year = yearID) ->
      MARKAL_trn_age_fractions_Yb

    # 2b. Light-duty vehicle emissions factors
    # long the raw data
    MARKAL_LDV_EFs_gpm %>%
      gather(variable, value, -Class, -Fuel, -Vintage, convert=T) %>%
      separate(variable, into = c("pollutant", "year", "region"), sep="\\.", convert = T) %>%
      mutate(pollutant = gsub("PM2_5", "PM2.5", pollutant)) %>%
      ###NOTE: filtering out some fuels for now for lack of efficiency/service demand data, and also the CO2 data
      filter(!pollutant %in% "CO2" & !Fuel %in% c("B20", "PH10G", "PH10E")) ->
      MARKAL_LDV_EFs_gpm.long

    # Gather a table for use in calculating degradation of EFs for each vintage
    MARKAL_LDV_EFs_gpm.long %>%
      filter(Vintage >= 2010 & Vintage != 2050 & !is.na(value)) ->
      L171.LDV_USA_emiss_degrates

    # Clean up and subset base year vintages
    MARKAL_LDV_EFs_gpm.long %>%
      filter(year %in% BASE_YEARS & Vintage <= year) %>%
      filter(!(Vintage == 1990 & year ==2010)) %>%
      ### MISSING VALUES: emission factors pollutants with no data for ELC vehicles, and 2010 EFs for 1990 vintages
      na.omit ->
      MARKAL_LDV_EFs_gpm_Yb

    # Match base year age fractions onto the table containing the EFs in the base years
    MARKAL_LDV_EFs_gpm_Yb %>%
      left_join(MARKAL_trn_age_fractions_Yb, by = c("Class", "Vintage", "year")) ->
      MARKAL_LDV_EFs_gpm_Yb

    # The base year EFs will be weighted means of the vintaged data using the age fractions as weights
    MARKAL_LDV_EFs_gpm_Yb %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      summarise(value = weighted.mean(value, ageFraction)) ->
      MARKAL_LDV_EFs_gpm_Yb.avg

    # Clean up and subset future vintages for emissions coefficients
    MARKAL_LDV_EFs_gpm.long %>%
      filter(Vintage >= 2015 & Vintage == year & !is.na(value)) ->
      MARKAL_LDV_EFs_gpm_Yf_NAs

    # Add on the base year emission factors
    MARKAL_LDV_EFs_gpm_Y.long <- bind_rows(MARKAL_LDV_EFs_gpm_Yb.avg, MARKAL_LDV_EFs_gpm_Yf_NAs)

    # Prefficiency data
    MARKAL_LDV_eff_vmtMJ %>%
      filter(Technology %in% c("Electric 100mi", "Flex fuel", "Flex fuel plugin-hybrid 20mi",
                               "Flex fuel plugin-hybrid 40mi", "ICE", "Plugin-hybrid 20mi","Plugin-hybrid 40mi") &
               !Fuel %in% c("CNG or gasoline","CNG","LPG","LPG or gasoline")) %>%
      # Manually interpolate to add 2005
      mutate(`2005` = 2*`2010`-`2015`) %>%
      select(-Technology,-Unit) %>%
      gather(year,value,-Class,-Fuel) ->
      MARKAL_LDV_eff_vmtMJ

    # Use MARKAL vehicle fuel intensities to convert to mass emissions per fuel input
    # There are some werid as matching going on here... I am going to have to look more closely into it!
    MARKAL_LDV_EFs_gpm_Y.long %>%
      # Add the Fuel name from the MARKAL_fuel_name_code mapping file.
      left_join_error_no_match(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      left_join(MARKAL_LDV_eff_vmtMJ %>%
                  mutate(year = as.integer(year)),
                by = c("Class", "Fuel_name" = "Fuel","year")) %>%
      mutate(value = value.x * value.y) %>%
      filter(!is.na(value)) %>%
      select(region, Class, Fuel, pollutant, year, value, Fuel_name) ->
      MARKAL_LDV_EFs_tgEJ_Y.long

    # Import LDV CNG emission factors from GREET data and copy for all categories in the LDV table
    GREET2014_LDV_CNG_EFs_tgEJ %>%
      repeat_add_columns(tibble("region" = unique(MARKAL_LDV_EFs_tgEJ_Y.long$region))) %>%
      repeat_add_columns(tibble("Class" = unique(MARKAL_LDV_EFs_tgEJ_Y.long$Class))) %>%
      repeat_add_columns(tibble("year" = unique(MARKAL_LDV_EFs_tgEJ_Y.long$year))) %>%
      mutate(Fuel_name = "Natural Gas") ->
      GREET_LDV_CNG_EFs_tgEJ_Y

    # Bind CNG data onto MARKAL table
    # MARKAL_LDV_EFs_tgEJ_Y.long <- bind_rows(MARKAL_LDV_EFs_tgEJ_Y.long, GREET_LDV_CNG_EFs_tgEJ_Y)
    # Spread to wide format
    # MISSING VALUES: ELC Minivans in 2005 and all vehicles using E85 in 2005. Fill with 2010 data for now
    # Also fill in zeroes in 2005 with 2010 data
    MARKAL_LDV_EFs_tgEJ_Y.long %>%
      bind_rows(GREET_LDV_CNG_EFs_tgEJ_Y) %>%
      spread(year, value) %>%
      mutate(`2005` = if_else(is.na(`2005`), `2010`, `2005`)) %>%
      mutate(`2005` = if_else(`2005` == 0, `2010`, `2005`)) %>%
      select(-Fuel_name) %>%
      arrange(region, Class, Fuel, pollutant) ->
      MARKAL_LDV_EFs_tgEJ_Y

    # Also convert table containing emission coefficient degradation paths
    ### MISSING VALUES: where there is no fuel intensity data for Mini car fuels other than
    ### gasoline or electricity. Remove for now
    L171.LDV_USA_emiss_degrates %>%
      left_join_error_no_match(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      # We do not expect a 1:1 match so use left_join
      left_join(MARKAL_LDV_eff_vmtMJ %>%
                  mutate(year = as.integer(year)),
                by = c("Class", "Fuel_name" = "Fuel","year")) %>%
      mutate(value = value.x * value.y) %>%
      filter(!is.na(value)) %>%
      select(Class, Fuel, Vintage, pollutant, year, region, value, Fuel_name) ->
      L171.LDV_USA_emiss_degrates_tgEJ

    # Add a Vintage column to the CNG data and bind it to the table containing the degradation paths
    GREET_LDV_CNG_EFs_tgEJ_Y %>%
      repeat_add_columns(tibble("Vintage" = unique(L171.LDV_USA_emiss_degrates_tgEJ$Vintage))) ->
      GREET_LDV_CNG_EFs_tgEJ_Y.vintage

    L171.LDV_USA_emiss_degrates_tgEJ %>%
      bind_rows(GREET_LDV_CNG_EFs_tgEJ_Y.vintage) ->
      L171.LDV_USA_emiss_degrates_tgEJ

    # 2c. Heavy duty vehicle emission factors
    # long the raw data
    MARKAL_HDV_EFs_gpm %>%
      gather(variable, value, -Class, -Fuel, -Vintage, convert = T) %>%
      separate(variable, into = c("pollutant", "year", "region"), sep="\\.", convert=T) %>%
      mutate(pollutant = gsub("PM2_5","PM2.5", pollutant)) %>%
      ### NOTE: filtering out fuels with no intensity data, and also the CO2 data
      filter(!pollutant %in% "CO2" & Fuel %in% c("B0","B20","CNG")) ->
      MARKAL_HDV_EFs_gpm.long

    # Gather a table for use in calculating degradation of EFs for each vintage
    MARKAL_HDV_EFs_gpm.long %>%
      filter(Vintage >= 2010 & Vintage != 2050 & !is.na(value)) ->
      L171.HDV_USA_emiss_degrates

    # Clean up and subset base year vintages
    MARKAL_HDV_EFs_gpm.long %>%
      filter(year %in% BASE_YEARS & Vintage <= year) %>%
      filter(!(year == 2010 & Vintage == 1990)) %>%
      na.omit ->
      MARKAL_HDV_EFs_gpm_Yb

    # Match base year age fractions onto the table containing the EFs in the base years
    MARKAL_HDV_EFs_gpm_Yb %>%
      left_join_error_no_match(MARKAL_trn_age_fractions_Yb,
                               by = c("Vintage", "Class", "year")) ->
      MARKAL_HDV_EFs_gpm_Yb

    # The base year EFs will be weighted means of the vintaged data using the age fractions as weights
    MARKAL_HDV_EFs_gpm_Yb %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      summarise(value = weighted.mean(value, ageFraction)) ->
      MARKAL_HDV_EFs_gpm_Yb.avg

    # Clean up and subset future vintages for emissions coefficients
    MARKAL_HDV_EFs_gpm.long %>%
      filter(Vintage >= 2015 & Vintage == year & !is.na(value)) ->
      MARKAL_HDV_EFs_gpm_Yf_NAs

    # Add on the base year emission factors
    MARKAL_HDV_EFs_gpm_Yb.avg %>%
      bind_rows(MARKAL_HDV_EFs_gpm_Yf_NAs) ->
      MARKAL_HDV_EFs_gpm_Y.long

    # Prepare efficiency data
    MARKAL_HDV_eff_vmtMJ %>%
      ###NOTE: no emissions data for hydrogen or LPG
      filter(Fuel %in% c("B0","Biodiesel (B20)","CNG") &
               Technology %in% c("Existing","Conventional","Flex fuel")) %>%
      select(-Technology, -Unit) %>%
      gather(year, value, -Class, -Fuel, convert=T) %>%
      filter(!is.na(value)) ->
      MARKAL_HDV_eff_vmtMJ

    # Fill in 2005 data using manual linear interpolation
    MARKAL_HDV_eff_vmtMJ %>%
      complete(nesting(Fuel, Class), year = unique(MARKAL_HDV_eff_vmtMJ$year)) %>%
      arrange(Fuel, Class, year) %>%
      group_by(Fuel, Class) %>%
      # Linerally extrapolate to fill in missing values for the year 2005.
      mutate(value = approx_fun(year, value, rule = 2)) ->
      MARKAL_HDV_eff_vmtMJ

    # MARKAL_HDV_EFs_gpm_Y.long foing into the pipeline is good!!
    MARKAL_HDV_EFs_gpm_Y.long %>%
      left_join_error_no_match(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      # We do not expect a 1:1 match
      left_join(MARKAL_HDV_eff_vmtMJ, by = c("Class", "Fuel_name" = "Fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(region, Class, Fuel, pollutant, year, value, Fuel_name) ->
      MARKAL_HDV_EFs_tgEJ_Y.long

    # Also convert table containing emission coefficient degradation paths
    L171.HDV_USA_emiss_degrates %>%
      left_join_error_no_match(MARKAL_fuel_name_code, by = c('Fuel' = 'Fuel_code')) %>%
      left_join_error_no_match(MARKAL_HDV_eff_vmtMJ, by = c("Class", "Fuel_name" = "Fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(Class, Fuel, Vintage, pollutant, year, region, value, Fuel_name) ->
      L171.HDV_USA_emiss_degrates_tgEJ

    # 2d. Apportion to states
    # Combine LDV and HDV tables
    MARKAL_LDV_EFs_tgEJ_Y %>%
      gather(year, value, -region, -Class, -Fuel, -pollutant) %>%
      mutate(year = as.integer(year)) %>%
      bind_rows(MARKAL_HDV_EFs_tgEJ_Y.long) ->
      MARKAL_trn_EFs_tgEJ_Y

    L171.trn_USA_emiss_degrates <- bind_rows(L171.HDV_USA_emiss_degrates_tgEJ, L171.LDV_USA_emiss_degrates_tgEJ)

    # For now keep the degradation rates at the census region level to save time in level2 processing
    L171.trn_USA_emiss_degrates %>%
      rename(class = Class, fuel = Fuel, vintage = Vintage) %>%
      select(region, class, fuel, vintage, pollutant, year, value) %>%
      arrange(region, class, fuel, vintage, pollutant) ->
      L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y

    # Write the emission factors to each state, assuming they are uniform in their census regions
    MARKAL_trn_EFs_tgEJ_Y %>%
      left_join(state_census_region, by = c('region' = 'census_region_id')) %>%
      ungroup %>%
      select(state, class = Class, fuel = Fuel, pollutant, year, value) ->
      L171.nonco2_tgej_state_trn_SMarkal_F_Y

    stopifnot(!any(is.na(L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y)))
    stopifnot(!any(is.na(L171.nonco2_tgej_state_trn_SMarkal_F_Y)))

    # ==============================================================================

    # Produce outputs
    L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y %>%
      add_title("Transportation non-co2 emission factor by U.S. census region / MARKAL vehicle class / fuel / vintage / pollutant / year") %>%
      add_units("Tg / EJ") %>%
      add_comments("calculate fraction of the age of the vehicles by regulatory class") %>%
      add_comments("match with emission factors and fuel intensity") %>%
      add_comments("map to census regions") %>%
      add_legacy_name("L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA") %>%
      add_precursors("gcam-usa/GREET2014_LDV_CNG_EFs_tgEJ",
                     "gcam-usa/MARKAL_LDV_EFs_gpm",
                     "gcam-usa/MARKAL_HDV_EFs_gpm",
                     "gcam-usa/MARKAL_LDV_eff_vmtMJ",
                     "gcam-usa/MARKAL_HDV_eff_vmtMJ",
                     "gcam-usa/MOVES_vehicle_age_fractions",
                     "gcam-usa/MOVES_vehicle_age_fractions",
                     "gcam-usa/MOVES_source_type_pop",
                     "gcam-usa/MOVES_src_type_reg_class_fractions",
                     "gcam-usa/state_census_region",
                     "gcam-usa/MARKAL_fuel_name_code",
                     "gcam-usa/MARKAL_MOVES_class") ->
      L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA

    L171.nonco2_tgej_state_trn_SMarkal_F_Y %>%
      add_title("Transportation non-co2 emission factor by U.S. state / MARKAL vehicle class / fuel / pollutant / year") %>%
      add_units("Tg / EJ") %>%
      add_comments("calculate fraction of the age of the vehicles by regulatory class") %>%
      add_comments("match with emission factors and fuel intensity") %>%
      add_comments("map to states") %>%
      add_legacy_name("L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA") %>%
      add_precursors("gcam-usa/GREET2014_LDV_CNG_EFs_tgEJ",
                     "gcam-usa/MARKAL_LDV_EFs_gpm",
                     "gcam-usa/MARKAL_HDV_EFs_gpm",
                     "gcam-usa/MARKAL_LDV_eff_vmtMJ",
                     "gcam-usa/MARKAL_HDV_eff_vmtMJ",
                     "gcam-usa/MOVES_vehicle_age_fractions",
                     "gcam-usa/MOVES_vehicle_age_fractions",
                     "gcam-usa/MOVES_source_type_pop",
                     "gcam-usa/MOVES_src_type_reg_class_fractions",
                     "gcam-usa/state_census_region",
                     "gcam-usa/MARKAL_fuel_name_code",
                     "gcam-usa/MARKAL_MOVES_class") ->
      L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA

    return_data(L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA,
                L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA)

  } else {
    stop("Unknown command")
  }
}
