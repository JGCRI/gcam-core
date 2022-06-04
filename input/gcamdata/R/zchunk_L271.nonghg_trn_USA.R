#' module_gcamusa_L271.nonghg_trn_USA
#'
#' Non-GHG emissions parameters for transportation technologies in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.nonco2_trn_tech_coeff_USA}, \code{L271.nonco2_trn_emiss_control_USA}
#' The corresponding file in the original data system was \code{L271.trn_nonCO2_USA.R} (gcam-usa level2).
#' @details Prepare level 2 transportation sector emissions files for USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread unite
#' @importFrom stats lm
#' @author BY September 2019, MAW March 2022

module_gcamusa_L271.nonghg_trn_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.StubTranTech_USA",
             FILE="gcam-usa/emissions/MARKAL_UCD_class",
             FILE="gcam-usa/emissions/MARKAL_UCD_LDV_fuel",
             FILE="gcam-usa/emissions/MARKAL_UCD_HDV_fuel",
             "L171.nonco2_tgpkm_state_trn_SMarkal_F_Y",
             "L254.StubTranTechOutput_USA",
             "L170.NEI_1990_2017_GCAM_sectors",
             FILE="gcam-usa/emissions/NEI_pollutant_mapping",
             "L254.GlobalTranTechSCurve",
             "L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y",
             FILE="gcam-usa/states_subregions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.nonco2_trn_tech_coeff_USA",
             "L271.nonco2_trn_emiss_control_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    stub.technology  <- add_predictions <- supplysector <- year <- fuel <- value <- state <- pollutant <-
      region <- tranSubsector <- Non.CO2 <- emiss.coef <- MARKAL_class <- MARKAL_HDV_fuel <- output <- emission <-
      emiss.coef.cali <- lifetime <- MARKAL_LDV_fuel <- DIVISION <- sector.name <- half.life <- steepness <-
      vintage <- lm <- . <- MARKAL_fuel <- end.year <- start.year <- census_region <- pred <- final.emissions.coefficient <-
      linear.control <- input.emissions <- fuel_input <- value <- emiss.coef_new <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    L254.StubTranTech_USA <- get_data(all_data, "L254.StubTranTech_USA")
    MARKAL_UCD_class <- get_data(all_data, "gcam-usa/emissions/MARKAL_UCD_class")
    MARKAL_UCD_LDV_fuel <- get_data(all_data, "gcam-usa/emissions/MARKAL_UCD_LDV_fuel")
    MARKAL_UCD_HDV_fuel <- get_data(all_data, "gcam-usa/emissions/MARKAL_UCD_HDV_fuel")
    L171.nonco2_tgpkm_state_trn_SMarkal_F_Y <- get_data(all_data, "L171.nonco2_tgpkm_state_trn_SMarkal_F_Y")
    L254.StubTranTechOutput_USA <- get_data(all_data, "L254.StubTranTechOutput_USA")
    L170.NEI_1990_2017_GCAM_sectors <- get_data(all_data, "L170.NEI_1990_2017_GCAM_sectors")
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping")
    L254.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve")
    L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y <- get_data(all_data, "L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")

    # ===================================================
    # ===================================================

    # 1. Perform computations
    # ===================================================
    # Pollutant emissions for transportation technologies in all U.S. states
    # 1.1 LDV emission coefficients
    # ===================================================
    L254.StubTranTech_USA_LDV <- L254.StubTranTech_USA %>%
      filter( supplysector %in% gcamusa.LDV_SUPPLYSECTORS ) %>%
      # this has to be a left join b/c of MARKAL to UCD mapping issues (explained below)
      left_join(MARKAL_UCD_class, by = c("tranSubsector" = "UCD_class")) %>%
      left_join_error_no_match(MARKAL_UCD_LDV_fuel, by = c("stub.technology" = "UCD_LDV_fuel")) %>%
      repeat_add_columns(tibble::tibble(year = gcamusa.TRN_EMISSION_YEARS))
    # At this point, the DF has UCD classes mapped to multiple MARKAL classes
    # and some MARKAL classes mapped to multiple UCD classes. This is remediated later.

    # Subset relevant classes and fuels from emission factor table
    L171.nonco2_tgpkm_state_LDV_S_F_Y <- L171.nonco2_tgpkm_state_trn_SMarkal_F_Y %>%
      filter(class %in% L254.StubTranTech_USA_LDV$MARKAL_class &
               fuel %in% L254.StubTranTech_USA_LDV$MARKAL_LDV_fuel) %>%
      tidyr::gather( year, value, -c( state, class, fuel, pollutant ) ) %>%
      mutate( year = as.numeric( year ) )

    # Match the emission coefficients onto the stub technology table
    L271.nonco2_LDV_USA <- L254.StubTranTech_USA_LDV %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L171.nonco2_tgpkm_state_LDV_S_F_Y$pollutant))) %>%
      #Need to use left_join, as there are several NA values (EV EFs, NH3 EFs for CNG) (will be filtered out in the next step)
      left_join(L171.nonco2_tgpkm_state_LDV_S_F_Y,
                               by = c("region" = "state", "MARKAL_class" = "class", "MARKAL_LDV_fuel" = "fuel", "year", "Non.CO2" = "pollutant")) %>%
      ###MISSING VALUES: EV emission factors for pollutants where there is no data, and NH3 and NOx EFs for CNG vehicles
      # TODO: Use Ellie's method to add these
      ###where there is also no data. OK to remove for now
      na.omit() %>%
      # since MARKAL classes don't directly map to new UCD classes, we must use averaging
      # Ex. UCD_class = Car maps to MARKAL_class = Compact car AND MARKAL_class = Full size car
      # Thus, "Car" will be assigned an EF that is the average of Compact car and Full size car
      group_by( region, supplysector, tranSubsector, stub.technology, MARKAL_LDV_fuel, year, Non.CO2 ) %>%
      mutate( value = mean( value ) ) %>%
      # only need to keep one entry per tranSubsector
      distinct( region, supplysector, tranSubsector, stub.technology, MARKAL_LDV_fuel, year, Non.CO2, value ) %>%
      rename(emiss.coef = value) %>%
      ungroup() %>%
      # select relevant columns
      select( -MARKAL_LDV_fuel )

    # 1.2. HDV emission coefficients
    # ===================================================
    L254.StubTranTech_USA_HDV <- filter(L254.StubTranTech_USA, supplysector %in% gcamusa.HDV_SUPPLYSECTORS ) %>%
      # this has to be a left join b/c of MARKAL to UCD mapping issues (explained below)
      left_join(MARKAL_UCD_class, by = c("tranSubsector" = "UCD_class")) %>%
      left_join_error_no_match(MARKAL_UCD_HDV_fuel, by = c("stub.technology" = "UCD_HDV_fuel")) %>%
      repeat_add_columns(tibble::tibble(year = gcamusa.TRN_EMISSION_YEARS)) %>%
      # Fix: change "Pickup" fuel to gasoline instead of B0
      mutate(MARKAL_HDV_fuel = if_else(MARKAL_class == "Pickup" & stub.technology == "Liquids", "GSL", MARKAL_HDV_fuel))
    # At this point, the DF has UCD classes mapped to multiple MARKAL classes
    # and some MARKAL classes mapped to multiple UCD classes. This is remediated later.

    # Subset relevant classes and fuels from emission factor table
    L171.nonco2_tgpkm_state_HDV_S_F_Y <- L171.nonco2_tgpkm_state_trn_SMarkal_F_Y %>%
      filter(class %in% L254.StubTranTech_USA_HDV$MARKAL_class &
               fuel %in% L254.StubTranTech_USA_HDV$MARKAL_HDV_fuel) %>%
      tidyr::gather( year, value, -c( state, class, fuel, pollutant ) ) %>%
      mutate( year = as.numeric( year ) )

    # Match the emission coefficients onto the stub technology table
    L271.nonco2_HDV_USA <- L254.StubTranTech_USA_HDV %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L171.nonco2_tgpkm_state_HDV_S_F_Y$pollutant))) %>%
      # Need to use left_join, as there are several NA values (CNG EFs) (will be filtered out in the next step)
      left_join(L171.nonco2_tgpkm_state_HDV_S_F_Y,
                by = c("region" = "state", "MARKAL_class" = "class", "MARKAL_HDV_fuel" = "fuel", "year", "Non.CO2" = "pollutant")) %>%
      ###MISSING VALUES: Rows with missing values are CNG-fueled vehicle emission factors for technologies where there is no data.
      ###where there is also no data. OK to remove for now
      na.omit() %>%
      # since MARKAL classes don't directly map to new UCD classes, we must use averaging
      # Ex. UCD_class = Medium truck maps to MARKAL_class = Commercial truck AND MARKAL_class = Heavy duty short haul truck
      # Thus, "Car" will be assigned an EF that is the average of Compact car and Full size car
      group_by( region, supplysector, tranSubsector, stub.technology, MARKAL_HDV_fuel, year, Non.CO2 ) %>%
      mutate( value = mean( value ) ) %>%
      # only need to keep one entry per tranSubsector
      distinct( region, supplysector, tranSubsector, stub.technology, MARKAL_HDV_fuel, year, Non.CO2, value ) %>%
      rename(emiss.coef = value) %>%
      ungroup() %>%
      # select relevant columns
      select( -MARKAL_HDV_fuel )

    # Bind the LDV and HDV tables and clean up
    L271.nonco2_trn_tech_coeff_USA_LDV_HDV <- bind_rows(L271.nonco2_HDV_USA,L271.nonco2_LDV_USA) %>%
      # Re-name the pollutants to match names of existing gases
      mutate(Non.CO2 = gsub("VOC","NMVOC",Non.CO2),
             Non.CO2 = gsub("NOX","NOx",Non.CO2))

    # 2. Calculating output based EFs using NEI emissions/own service demand
    # ===================================================
    # EFs have 2005 as the earliest year. Copy 2005 values to previous base years
    L271.nonco2_trn_tech_coeff_USA <- L271.nonco2_trn_tech_coeff_USA_LDV_HDV %>%
      filter(year == min(year) ) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = c(1990))) %>%
      # rebind with dataframe that has all years
      bind_rows( L271.nonco2_trn_tech_coeff_USA_LDV_HDV ) %>%
      #change SO2 to SO2_1
      mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) )

    # Separate into base year and future year tables.
    L271.nonco2_trn_tech_coeff_USA_Yb <- L271.nonco2_trn_tech_coeff_USA %>%
      filter( year %in% MODEL_BASE_YEARS )

    L271.nonco2_trn_tech_coeff_USA_Yf <- L271.nonco2_trn_tech_coeff_USA %>%
      filter( year %in% MODEL_FUTURE_YEARS )

    # Prepare the NEI scaled to CEDS emissions
    CEDS_emissions <- L170.NEI_1990_2017_GCAM_sectors %>%
      rename( region = state,
              CEDS_emissions = emissions ) %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      select( - pollutant ) %>%
      filter( GCAM_sector %in% gcamusa.GCAM_TRANSPORT_SECTORS,
              year %in% MODEL_BASE_YEARS ) %>%
      group_by( year, Non.CO2 ) %>%
      # convert TON to Tg
      mutate( CEDS_emissions = sum( CEDS_emissions * CONV_TST_TG / 1000 )) %>%
      ungroup() %>%
      mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
      distinct( year, Non.CO2, CEDS_emissions )

    # For the table containing base year EFs, we will calculate inferred emissions using GCAM service output.
    # We will then scale these emissions to CEDS at the national level by year and pollutant, and scale EFs by that same factor.
    L271.nonco2_trn_tech_coeff_USA_Yb_scaled <- L271.nonco2_trn_tech_coeff_USA_Yb %>%
      left_join_error_no_match( L254.StubTranTechOutput_USA, by = c( "region", "supplysector", "tranSubsector",
                                                                     "stub.technology", "year" ) ) %>%
      # mutliply the MARKAL EFs by the GCAM service output to get emissions
      mutate( emissions = emiss.coef * output ) %>%
      # remove unnecessary columns
      select( c( "region", "supplysector", "tranSubsector", "stub.technology", "Non.CO2", "emiss.coef", "output", "year", "emissions" )) %>%
      # group emissions nationally by year and Non.CO2 to get total emissions to scale to CEDS
      group_by( year, Non.CO2 ) %>%
      mutate( total_emissions = sum( emissions ) ) %>%
      # join with the CEDS data
      left_join( CEDS_emissions, by = c( "year", "Non.CO2" ) ) %>%
      # calculate scaling factor
      mutate( scaling_factor = CEDS_emissions / total_emissions,
      # multiple EF by scaling factor to get scaled EF
              emiss.coef = emiss.coef * scaling_factor ) %>%
      # CH4 and N20 become NA, which is ok because these are GHGs
      na.omit() %>%
      # remove unnecessary columns
      select( -c( "total_emissions", "CEDS_emissions", "emissions", "output", "scaling_factor" ) )

    # Need a dataframe that has scaling factors for the current base year, which will be applied to EFs in the degradation table
    # use same approach as above
    scaling_factor <- L271.nonco2_trn_tech_coeff_USA_Yb %>%
      left_join_error_no_match( L254.StubTranTechOutput_USA, by = c( "region", "supplysector", "tranSubsector",
                                                                     "stub.technology", "year" ) ) %>%
      # mutliply the MARKAL EFs by the GCAM service output to get emissions
      mutate( emissions = emiss.coef * output ) %>%
      # remove unnecessary columns
      select( c( "region", "supplysector", "tranSubsector", "stub.technology", "Non.CO2", "emiss.coef", "output", "year", "emissions" )) %>%
      # group emissions nationally by year and Non.CO2 to get total emissions to scale to CEDS
      group_by( year, Non.CO2 ) %>%
      mutate( total_emissions = sum( emissions ) ) %>%
      # join with the CEDS data
      left_join( CEDS_emissions, by = c( "year", "Non.CO2" ) ) %>%
      # calculate scaling factor
      mutate( scaling_factor = CEDS_emissions / total_emissions ) %>%
      # CH4 and N20 become NA, which is ok because these are GHGs
      na.omit() %>%
      # filter for the most recent year (which is the last base year) and use this factor
      filter( year == max( MODEL_BASE_YEARS ) ) %>%
      ungroup() %>%
      # the scaling factor is the same for all regions / sectors / techs within a pollutant
      distinct( Non.CO2, scaling_factor )

    # Scale the future year EFs and rebind the base years
    L271.nonco2_trn_tech_coeff_USA_scaled <- L271.nonco2_trn_tech_coeff_USA_Yf %>%
      # must use left join because the L271 datafame has CH4 and N2O, which we don't have scaling factors for
      left_join( scaling_factor, by = c( "Non.CO2" ) ) %>%
      # scale EF
      mutate( emiss.coef = emiss.coef * scaling_factor ) %>%
      # CH4 and N20 become NA, which is ok because these are GHGs
      na.omit() %>%
      # remove scaling factor column
      select( -scaling_factor ) %>%
      # bind with the dataframe containing the base year EFs
      bind_rows( L271.nonco2_trn_tech_coeff_USA_Yb_scaled )


    # 3. Emissions coeff controls for transportation technologies in all U.S. states
    # ===================================================
    # Need vehicle lifetimes in order to calculate final-emissions-coeff parameter.

    # The vehicle lifetimes parameters are written out for future years, so to get lifetimes
    # for the base years, we will assume they are all equal to the current base year

    # copy max base years values for all other base years
    L254.GlobalTranTechSCurve_Yb <- L254.GlobalTranTechSCurve %>%
      filter(year == max( MODEL_BASE_YEARS ) ) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      # remove the max model base year info, as this is already in the previous table
      filter( year != max( MODEL_BASE_YEARS ) )

    # bind all years back together
    L254.GlobalTranTechSCurve <- bind_rows(L254.GlobalTranTechSCurve,
                                           L254.GlobalTranTechSCurve_Yb) %>%
      filter(!is.na(lifetime))

    # 4, Create table for emissions control
    # Here, we take the table that contains LDV and HDV EFs by year computed from MARKAL, and the table
    # that contains EF degradation / evolution by year and vintage and apply a linear fit to the decline in emission controls.
    # ===================================================
    L254.StubTranTech_USA_LDV.coeff <- L254.StubTranTech_USA_LDV %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L171.nonco2_tgpkm_state_LDV_S_F_Y$pollutant))) %>%
      # Need to use left_join, as there are several NA values (such as EV EFs) (will be filtered out in the next step)
      left_join(L171.nonco2_tgpkm_state_LDV_S_F_Y, by = c("region" = "state",
                                                          "MARKAL_class" = "class",
                                                          "MARKAL_LDV_fuel" = "fuel",
                                                          "Non.CO2" = "pollutant",
                                                          "year")) %>%
      #MISSING VALUES: EV emission factors for pollutants where there is no data, and NH3 and NOx emission factors
      #for CNG vehicles, for which there is no data. OK to remove for now
      na.omit() %>%
      rename(MARKAL_fuel = MARKAL_LDV_fuel)

    L254.StubTranTech_USA_HDV.coeff <- L254.StubTranTech_USA_HDV %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L171.nonco2_tgpkm_state_HDV_S_F_Y$pollutant))) %>%
      # Need to use left_join, as there are several NA values (such as CNG EFs) (will be filtered out in the next step)
      left_join(L171.nonco2_tgpkm_state_HDV_S_F_Y, by = c("region" = "state",
                                                          "MARKAL_class" = "class",
                                                          "MARKAL_HDV_fuel" = "fuel",
                                                          "Non.CO2" = "pollutant",
                                                          "year")) %>%
      #MISSING VALUES:  Emission factors for CNG vehicles, for which there is no data. OK to remove for now
      na.omit() %>%
      rename(MARKAL_fuel = MARKAL_HDV_fuel)

    state_census_region <- states_subregions %>%
      select(state, DIVISION)

    # Combine LDV and HDV tables
    L254.StubTranTech_USA_trn <- bind_rows(L254.StubTranTech_USA_LDV.coeff,L254.StubTranTech_USA_HDV.coeff) %>%
      filter(year %in% unique(L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y$vintage)) %>%
      # Add on lifetime for each technology that can be used to calculate final emissions coefficient
      # need to use left join (some vehicles (Buses) have NA for lifetime)
      left_join(L254.GlobalTranTechSCurve, by = c("tranSubsector" = "subsector.name",
                                                               "stub.technology" = "tranTechnology",
                                                               "year")) %>%
      ###MISSING VALUES: It seems that "Bus" does not have a lifetime. Give it 25 years for now
      mutate(lifetime = if_else(is.na(lifetime), as.integer(gcamusa.STUBTRANTECH_LIFETIME_2045V), lifetime),
             # Add on start year and end year, census region, and id column for applying linear fits to estimate
             # final emissions coefficient
             start.year = year,
             end.year = year + lifetime) %>%
      left_join_error_no_match(state_census_region, by = c("region" = "state")) %>%
      rename(census_region = DIVISION,
             emiss.coef = value) %>%
      select(-sector.name, -half.life, -steepness) %>%
      unite(id, MARKAL_class, MARKAL_fuel, start.year, Non.CO2, census_region, sep = " ", remove = FALSE)

    # Prepare table containing emissions factor degradation data
    degrades <- L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y %>%
      filter(class %in% unique(L254.StubTranTech_USA_trn$MARKAL_class), fuel %in% unique(L254.StubTranTech_USA_trn$MARKAL_fuel)) %>%
      unite(id, class, fuel, vintage, pollutant, region, sep = " ", remove = FALSE)

    # Create a list that contains linear fits for each degradation rate id, with labels
    model_rslt <- lapply(X = split(degrades, degrades$id), FUN = function(X){ lm(value ~ year, data = X) })

    # Extract the needed information (unique end year and id) from the emission control table and
    # compute the linear prediction using the model corresponding to each id
    prediction_table <- L254.StubTranTech_USA_trn %>%
      filter(!(MARKAL_class %in% gcamusa.DEGRADES_FILTER_OUT_MARKAL_CLASS &
                 MARKAL_fuel %in% gcamusa.DEGRADES_FILTER_OUT_MARKAL_FUEL &
                 Non.CO2 %in% gcamusa.DEGRADES_FILTER_OUT_NONCO2)) %>%
      select(end.year, id) %>%
      distinct() %>%
      rename(year = end.year)

    invisible(lapply(X = as.list(prediction_table$id), FUN = function(X){

      curr_predict_id <- prediction_table$id == X
      num_degrades_entries <- sum(degrades$id == X)

      if(num_degrades_entries == 0) {
        stop("Error: There are 0 entries for this ID.")
      } else if(num_degrades_entries == 1) {
        # Some IDs only have a single entry. In this case, we want to assign the final emissions coefficient to what it is in the degrades table.
        prediction_table[curr_predict_id, "final.emissions.coefficient"] <<- degrades[degrades$id == X, "value", drop = TRUE]
      } else {
        prediction_table[curr_predict_id, "final.emissions.coefficient"]  <<- predict.lm(model_rslt[[X]], newdata = prediction_table[curr_predict_id, ])
      }

    }))


      # Add the final emissions coefficients onto the emissions control table
    L271.nonco2_trn_emiss_control_USA <- L254.StubTranTech_USA_trn %>%
      filter(!(MARKAL_class %in% gcamusa.DEGRADES_FILTER_OUT_MARKAL_CLASS &
                 MARKAL_fuel %in% gcamusa.DEGRADES_FILTER_OUT_MARKAL_FUEL &
                 Non.CO2 %in% gcamusa.DEGRADES_FILTER_OUT_NONCO2)) %>%
        select("region", "supplysector", "tranSubsector", "stub.technology", "MARKAL_class",
               "MARKAL_fuel", "year", "Non.CO2", "emiss.coef", "lifetime",
               "start.year", "end.year", "census_region", "id") %>%
        left_join_error_no_match(prediction_table,  by = c("id", "end.year" = "year")) %>%
    # remove duplicate entries that existed before due to having CORE and highEV sces
        distinct()

    # Some final emissions coefficients are negative. In these cases, change the final value to the last known
    negative_degrades <- L271.nonco2_trn_emiss_control_USA %>%
      filter(final.emissions.coefficient < 0)

    last_avail_coeff <- negative_degrades %>%
      select(census_region, MARKAL_class, MARKAL_fuel, start.year, Non.CO2) %>%
      #must use left_join, we are changing the number of rows (multiple years, values per ID)
      left_join(degrades, by = c("census_region" = "region",
                                               "MARKAL_class" = "class",
                                               "MARKAL_fuel" = "fuel",
                                               "start.year" = "vintage",
                                               "Non.CO2" = "pollutant")) %>%
      group_by(census_region, MARKAL_class, MARKAL_fuel, start.year, Non.CO2) %>%
      summarise(value = max(value)) %>%
      ungroup()

    negative_degrades_replaced <- negative_degrades %>%
      left_join_error_no_match(last_avail_coeff, by = c("census_region", "MARKAL_class", "MARKAL_fuel", "start.year", "Non.CO2")) %>%
      mutate(final.emissions.coefficient = value) %>%
      select(-value)

    # bind table back together
    L271.nonco2_trn_emiss_control_USA <- L271.nonco2_trn_emiss_control_USA %>%
      filter(final.emissions.coefficient >= 0) %>%
      bind_rows(negative_degrades_replaced) %>%
      # Add linear control object and round to 15 decimal places
      mutate(linear.control = "Emissions Coefficient Degradation",
             final.emissions.coefficient = round(final.emissions.coefficient,gcamusa.DIGITS_TRN_EF_DEGRADE)) %>%
      select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,linear.control,start.year,end.year,final.emissions.coefficient)

    # Subset the table containing the max final emissions coefficients to match onto the future years
    emiss_control_max <- filter(L271.nonco2_trn_emiss_control_USA, year == max(year)) %>%
      select(-year, -start.year, -end.year)

    # Add on the remaining future years (2050 and after), keeping the final emissions coefficient constant at last known data point
    remaining_future_years <- bind_rows(L254.StubTranTech_USA_LDV.coeff,L254.StubTranTech_USA_HDV.coeff) %>%
      # remove sce column and remove duplicate entries (CORE and highEV sce's had same values)
      select(-sce) %>%
      distinct() %>%
      filter(year >= max(gcamusa.TRN_MARKAL_EMISSION_YEARS)) %>%
      # repeat these values for all future years
      complete(nesting(region, supplysector, tranSubsector, stub.technology, MARKAL_class, UCD_class_old,
                       MARKAL_fuel, Non.CO2), year = c(MODEL_FUTURE_YEARS)) %>%
      filter(year >= max(gcamusa.TRN_MARKAL_EMISSION_YEARS)) %>%
      group_by(region, supplysector, tranSubsector, stub.technology, MARKAL_class, UCD_class_old,
               MARKAL_fuel, Non.CO2) %>%
      mutate(value = approx_fun_constant(year, value, rule = 2)) %>%
      ungroup() %>%
      # Add on lifetime for each technology that can be used to calculate final emissions coefficient
      # need to use left join (some vehicles (Buses) have NA for lifetime)
      left_join(L254.GlobalTranTechSCurve, by = c("tranSubsector" = "subsector.name",
                                                  "stub.technology" = "tranTechnology",
                                                  "year")) %>%
      ###MISSING VALUES: It seems that "Motorcycle" does not have a lifetime. Give it 20 years for now
      mutate(year = as.numeric(year),
             lifetime = if_else(is.na(lifetime), as.integer(gcamusa.STUBTRANTECH_LIFETIME_2050V), lifetime),
             # Add on start year and end year, census region, and id column for applying linear fits to estimate
             # final emissions coefficient
             start.year = year,
             end.year = year + lifetime,
             linear.control =  "Emissions Coefficient Degradation") %>%
      # Must use left_join, there are NA values for final emissions coefs (classes that were filtered out), these will be removed
      left_join(emiss_control_max, by = c("region", "supplysector", "tranSubsector", "stub.technology", "Non.CO2", "linear.control")) %>%
      select(region,supplysector,tranSubsector,stub.technology,year,Non.CO2,linear.control,start.year,end.year,final.emissions.coefficient)

    #Bind the future years to the existing emissions control table and adjusting final emissions coefficients
    L271.nonco2_trn_emiss_control_USA_scaled <- bind_rows( L271.nonco2_trn_emiss_control_USA, remaining_future_years ) %>%
      # set allow EF increase (boolean) to 1
      mutate( allow.ef.increase = 1,
      #Re-name pollutants to match names of GCAM gases
        Non.CO2 = gsub( "VOC", "NMVOC", Non.CO2 ),
        Non.CO2 = gsub( "NOX", "NOx", Non.CO2 ),
        Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
      # adjust the EFs by the base year scaling factor
      left_join( scaling_factor, by = "Non.CO2" ) %>%
      mutate( final.emissions.coefficient = final.emissions.coefficient * scaling_factor ) %>%
      # CH4 and N20 become NA, which is ok because these are GHGs
      na.omit() %>%
      select( -scaling_factor) %>%
      # TODO: In the future, this should be done further upstream and be a weighted average.
      # There are multiple entries for the same supplysector/subsector/tranSubsector
      # due to multiple MARKAL classes mapping to single UCD classes. Use averaging.
      group_by(region, supplysector, tranSubsector, stub.technology, year, Non.CO2, linear.control, start.year, end.year, allow.ef.increase) %>%
      mutate( final.emissions.coefficient = mean( final.emissions.coefficient ) ) %>%
      distinct() %>%
      ungroup()

    # ===================================================

    # Produce outputs

    L271.nonco2_trn_tech_coeff_USA_scaled %>%
      add_title("Non-CO2 transportation emissions coefficients by state / supplysector / tranSubsector / stub.technology / year / Non.CO2") %>%
      add_units("Tg/million pass-km or Tg/million ton-km") %>%
      add_comments("Efs for base and future years from MARKAL, scaled to CEDS") %>%
      add_legacy_name("L274.nonghg_bld_tech_coeff_USA") %>%
      add_precursors("L254.StubTranTech_USA",
                     "gcam-usa/emissions/MARKAL_UCD_class",
                     "gcam-usa/emissions/MARKAL_UCD_LDV_fuel",
                     "gcam-usa/emissions/MARKAL_UCD_HDV_fuel",
                     "L171.nonco2_tgpkm_state_trn_SMarkal_F_Y",
                     "L254.StubTranTechOutput_USA",
                     "L170.NEI_1990_2017_GCAM_sectors",
                     "gcam-usa/emissions/NEI_pollutant_mapping") ->
      L271.nonco2_trn_tech_coeff_USA

    L271.nonco2_trn_emiss_control_USA_scaled %>%
      add_title("Non-CO2 new transportation linear control final emissions coefficients by state / supplysector / tranSubsector / stub.technology / year / Non.CO2") %>%
      add_units("Tg/million pass-km or Tg/million ton-km") %>%
      add_comments("Efs for base and future years") %>%
      add_legacy_name("L274.nonghg_bld_tech_coeff_USA") %>%
      add_precursors("L254.StubTranTech_USA",
                     "gcam-usa/emissions/MARKAL_UCD_class",
                     "gcam-usa/emissions/MARKAL_UCD_LDV_fuel",
                     "gcam-usa/emissions/MARKAL_UCD_HDV_fuel",
                     "L254.GlobalTranTechSCurve",
                     "L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y",
                     "gcam-usa/states_subregions") ->
      L271.nonco2_trn_emiss_control_USA

    return_data(L271.nonco2_trn_tech_coeff_USA,
                L271.nonco2_trn_emiss_control_USA)

  } else {
    stop("Unknown command")
  }
}
