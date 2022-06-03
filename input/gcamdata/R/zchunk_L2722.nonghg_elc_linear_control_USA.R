#' module_gcamusa_L2722.nonghg_elc_linear_control_USA
#'
#' Calculate NOx and SO2 emission factors and linear control parameters for coal - electricity technologies in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L272.nonghg_elec_tech_coeff_USA_linear_control}, \code{L2722.nonghg_elec_tech_coeff_USA_linear_control_off}.
#' @details This chunk calculates NOx and SO2 emission factors and linear control parameters for coal - electricity technologies in the USA
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW October 2020
module_gcamusa_L2722.nonghg_elc_linear_control_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.inEIA_EJ_state_S_F_all_years",
             "L270.nonghg_tg_state_elec_F_Yb",
             "L272.nonghg_elec_tech_coeff_USA",
             "L2233.StubTechProd_elecS_cool_USA",
             FILE = "gcam-usa/emissions/state_tier1_caps"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2722.nonghg_elec_tech_coeff_USA_linear_control",
             "L2722.nonghg_elec_tech_coeff_USA_linear_control_off"))
  } else if(command == driver.MAKE) {

    # silence package check
    fuel <- Non.CO2 <- value <- sector <- input.emissions <-
      electricity.consumption <- year <- subsector <-
      supplysector <- emiss.coef <- GCAM.emissions.factor <-
      observed.emission.factor <- scaling <- minicam.energy.input <-
      market.name <- year.y <- region <- stub.technology <-
      end.year <- final.emissions.coefficient <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    L101.inEIA_EJ_state_S_F_all_years <- get_data(all_data, "L101.inEIA_EJ_state_S_F_all_years", strip_attributes = TRUE)
    L270.nonghg_tg_state_elec_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_elec_F_Yb", strip_attributes = TRUE)
    L272.nonghg_elec_tech_coeff_USA <- get_data(all_data, "L272.nonghg_elec_tech_coeff_USA", strip_attributes = TRUE)
    L2233.StubTechProd_elecS_cool_USA <- get_data(all_data, 'L2233.StubTechProd_elecS_cool_USA', strip_attributes = TRUE)
    state_tier1_caps <- get_data(all_data, "gcam-usa/emissions/state_tier1_caps", strip_attributes = TRUE)

    # Filter NEI data for fuel = coal, and pollutants = NOx or SO2
    NEI_emissions <- L270.nonghg_tg_state_elec_F_Yb %>%
      filter( fuel == "coal",
              Non.CO2 == "NOx" | Non.CO2 == "SO2") %>%
      rename( input.emissions = value )

    # Filter the EIA data for electricity inputs and coal
    # SEDS (EIA) indicates electricity generation technologies either in terms of fuel inputs or fuel outputs (not both)
    # ELECTRICITY_INPUT: coal, gas, oil, biomass
    # ELECTRICITY_OUTPUT: nuclear and renewables
    # Note: EIA has coal from electricity consumption as 0 for a few states, while GCAM has all non-zero
    # We will apply the median emissions factor to these cases
    EIA_electricity_consumption <- L101.inEIA_EJ_state_S_F_all_years %>%
      filter( sector == "electricity_input",
             fuel == "coal" ) %>%
      select( -c( sector, fuel ) ) %>%
      rename( electricity.consumption = value )

    # Join the table containing NEI emissions from coal - electricity
    # With the table containing EIA consumption of coal - electricity
    elec_emiss_coeffs.NA <- EIA_electricity_consumption %>%
      filter( year == max( MODEL_BASE_YEARS ) | year == max( year ) ) %>%
      left_join( NEI_emissions, by = c( "state", "year") ) %>%
      # calculate the emissions factor by dividing emissions by consumption
      mutate( observed.emission.factor = input.emissions / electricity.consumption ) %>%
      select( -c( input.emissions, electricity.consumption ) )
    # this results in some states having NAs or Inf due to having no input emissions (NA), or 0 input emissions (Inf)
    # In these cases, we apply the average emissions factor for that year and pollutant

    # Generate national median emissions factors
    # Remove NAs so as to not skew the median
    # Some states (VT, DC, RI, and ID), are not assigned median EFs because they have no electricity consumption from coal.
    # Some states (ME), are not assigned median EFs because they have no electric generation emissions from coal,
    # Previously, we were assigning medians to these states.
    elec_emiss_coeffs.median <- elec_emiss_coeffs.NA %>%
      filter(!is.na(observed.emission.factor)) %>%
      group_by(year, Non.CO2) %>%
      summarise(observed.emission.factor = median(observed.emission.factor)) %>%
      ungroup() %>%
      rename(nationalEF = observed.emission.factor)

    # Replace all emissions factors above a given value (currently 1000) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
    elec_emiss_coeffs <- elec_emiss_coeffs.NA %>%
      # remove NAs so LJENM works
      # These NAs are the states mentioned above
      filter(!is.na(observed.emission.factor)) %>%
      left_join_error_no_match(elec_emiss_coeffs.median, by = c("year", "Non.CO2")) %>%
      mutate(observed.emission.factor = if_else(observed.emission.factor > emissions.HIGH_EM_FACTOR_THRESHOLD | is.na(observed.emission.factor), nationalEF, observed.emission.factor)) %>%
      select(state, year, sector, fuel, Non.CO2, observed.emission.factor) %>%
      mutate(observed.emission.factor = if_else(is.infinite(observed.emission.factor), 1, observed.emission.factor)) %>%
      rename(final.emissions.coefficient = observed.emission.factor)

    # Assign emission coefficients to the electricity technologies
    # The linear control will not apply to IGCC or CCS technologies
    # It will apply to all other technologies and states where there is non-zero energy consumption in the base year
      # first, make a table with states, supplysectors, subsectors, and technologies with energy consumption in the max model base year
    technology_table <- L2233.StubTechProd_elecS_cool_USA %>%
      filter( subsector0 == "coal",
              year == max( MODEL_BASE_YEARS ),
              calOutputValue > 0,
              !grepl( 'IGCC|CCS', subsector ),
              !grepl( 'peak|subpeak', supplysector ) ) %>%
      select( -c( calOutputValue, share.weight.year, subs.share.weight, tech.share.weight ) ) %>%
      distinct( region, supplysector, subsector0, subsector, technology, year )


  # Assign the emission factors to the technologies
    nonghg_elec_tech_coeff_USA <- technology_table %>%
      # Add pollutant column, adding NOx and SO2 for each technology entry
      repeat_add_columns(tibble::tibble(Non.CO2=unique(NEI_emissions$Non.CO2))) %>%
      # Match on emission factors
      # Can't use left_join_error_no_match because the technology table includes all states
      left_join(elec_emiss_coeffs, by = c("region" = "state", "subsector0" = "fuel", "Non.CO2")) %>%
      # Set end year for the linear control
      rename( end.year = year.y ) %>%
      select(region, supplysector, subsector0, subsector, technology, Non.CO2, end.year, final.emissions.coefficient) %>%
      # remove NAs (from ME, mentioned above)
      filter( !is.na( end.year ) )

    # Breaking into separate pollutant dataframes for additional processing
    # Adding a start year, linear control name, and period/year
    nonghg_elec_tech_coeff_NOx_USA <- nonghg_elec_tech_coeff_USA %>%
      filter( end.year == max( end.year ),
              Non.CO2 == "NOx") %>%
      mutate( start.year = max( MODEL_BASE_YEARS ),
              linear.control = "NOx_control",
              year = max( MODEL_BASE_YEARS ) )

    nonghg_elec_tech_coeff_SO2_USA <- nonghg_elec_tech_coeff_USA %>%
      filter( end.year == max( end.year ),
              Non.CO2 == "SO2") %>%
      mutate( start.year = max( MODEL_BASE_YEARS ),
              linear.control = "SO2_control",
              year = max( MODEL_BASE_YEARS ) )

    # Bind them all back together
    L2722.nonghg_elec_tech_coeff_USA_linear_control <- bind_rows(nonghg_elec_tech_coeff_NOx_USA,
                                                          nonghg_elec_tech_coeff_SO2_USA) %>%
    #change SO2 to SO2_1
    mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) )

    # Define a utility function that returns the next model year.
    # Because we vectorize the function, the argument can be a vector (or column) of years
    # If year passed in is last model year, return last model year
    get_next_model_year <- Vectorize(function(year) {
      if(!year %in% MODEL_YEARS) {
        stop("Year is not a GCAM model year")
      }
      else if(year != tail(MODEL_YEARS, n = 1)){
        MODEL_YEARS[which(MODEL_YEARS == year)+1]
      }
      else {return(year)}
    })

    # Turn off linear control after end year of the last control for that emission/technology/region
    L2722.nonghg_elec_tech_coeff_USA_linear_control %>%
      arrange(desc(year)) %>%
      distinct(region, supplysector, subsector0, subsector, technology, Non.CO2,
               start.year, final.emissions.coefficient, linear.control, end.year, .keep_all = TRUE) %>%
      mutate(period = get_next_model_year(year),
             start.year = period,
             end.year = tail(MODEL_FUTURE_YEARS, n=1),
             disable.em.control = 1) %>%
      select(-c(year, final.emissions.coefficient)) -> L2722.nonghg_elec_tech_coeff_USA_linear_control_off

    # Produce outputs
    L2722.nonghg_elec_tech_coeff_USA_linear_control %>%
      add_title("Non-GHG emissions linear controls for electricity coal technologies in the USA") %>%
      add_units("Tg/EJ") %>%
      add_comments("Non-GHG emissions linear controls parameters for electricity coal technologies in the USA") %>%
      add_precursors("L101.inEIA_EJ_state_S_F_all_years",
                     "L270.nonghg_tg_state_elec_F_Yb",
                     "L272.nonghg_elec_tech_coeff_USA",
                     "L2233.StubTechProd_elecS_cool_USA",
                     "gcam-usa/emissions/state_tier1_caps") ->
      L2722.nonghg_elec_tech_coeff_USA_linear_control

    L2722.nonghg_elec_tech_coeff_USA_linear_control_off %>%
      add_title("Disables the GCAM-USA elec linear control for years after the linear control end year") %>%
      add_units("Unitless") %>%
      add_comments("Required to remove wanrings in the .out file when running GCAM") %>%
      add_precursors("L101.inEIA_EJ_state_S_F_all_years",
                     "L270.nonghg_tg_state_elec_F_Yb",
                     "L272.nonghg_elec_tech_coeff_USA",
                     "L2233.StubTechProd_elecS_cool_USA",
                     "gcam-usa/emissions/state_tier1_caps") ->
      L2722.nonghg_elec_tech_coeff_USA_linear_control_off

    return_data(L2722.nonghg_elec_tech_coeff_USA_linear_control, L2722.nonghg_elec_tech_coeff_USA_linear_control_off)
  } else {
    stop("Unknown command")
  }
}
