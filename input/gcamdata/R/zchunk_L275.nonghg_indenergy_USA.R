#' module_gcamusa_L275.indenergy_nonghg_USA
#'
#' Non-GHG input emissions parameters for industrial energy use sector in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L275.nonghg_indenergy_tech_coeff_USA}
#' The corresponding file in the original data system was \code{L275.indenergy_nonghg_USA.R} (gcam-usa level2).
#' @details Non-GHG input emissions parameters for industrial energy use technologies in the USA
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY Aug 2019, MAW March 2022

module_gcamusa_L275.indenergy_nonghg_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.StubTechCalInput_indenergy_USA",
             "L270.nonghg_tg_state_indenergy_F_Yb",
             "L232.StubTech_ind_USA",
             FILE="gcam-usa/emissions/BC_OC_assumptions",
             FILE="gcam-usa/emissions/BCOC_PM25_ratios",
             FILE = "gcam-usa/emissions/MARKAL_nonghg_indenergy_tech_coeff_USA_dhl"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L275.nonghg_indenergy_tech_coeff_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- region <- supplysector <- subsector <- calibrated.value <- Non.CO2 <- value <- input.emissions <-
      fuel_input <- emiss.coef <- fuel <- sector <- emiss.coeff <- stub.technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.StubTechCalInput_indenergy_USA <- get_data(all_data, "L232.StubTechCalInput_indenergy_USA", strip_attributes = TRUE)
    L270.nonghg_tg_state_indenergy_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_indenergy_F_Yb", strip_attributes = TRUE)
    L232.StubTech_ind_USA <- get_data(all_data, "L232.StubTech_ind_USA", strip_attributes = TRUE)
    BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions")
    BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
      # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
      select( -c( "Region", "Gains_Sector" ) ) %>%
      filter( !is.na( sector ) )
    MARKAL_nonghg_indenergy_tech_coeff_USA_dhl <- get_data(all_data, "gcam-usa/emissions/MARKAL_nonghg_indenergy_tech_coeff_USA_dhl", strip_attributes = TRUE)


    # ===================================================

    # Perform computations

    # The technology information in the industrial energy use sector has the same level of detail as the
    # NEI data so all that remains to be done is to assign the emissions to states and compute coefficients
    # and assign a national median if needed
    # Calculating industry fuel use emission factors in the base year for U.S. states

    # Fuel use is divided between cogen and non-cogen. Aggregate these to subsector level
    agg_fuel_input_indenergy_Yb <- L232.StubTechCalInput_indenergy_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise(fuel_input = sum(calibrated.value)) %>%
      ungroup()

    # Take the table containing the emissions, match on the fuel inputs, and compute the emission coefficients by state
    agg_nonghg_indenergy_emiss_coeffs_F_Yb <- L270.nonghg_tg_state_indenergy_F_Yb %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(input.emissions = value) %>%
      mutate(sector = "other industrial energy use") %>%
      # need to use left join, as some NA values are retained (some states have NA fuel input in refined liquids)
      left_join(agg_fuel_input_indenergy_Yb, by = c("state" = "region",
                                                     "sector" = "supplysector",
                                                     "fuel" = "subsector",
                                                     "year")) %>%
      mutate(emiss.coef = input.emissions / fuel_input)

    # 2b. Assign emission coefficients to the industry energy use technologies in the base years
    # L275.nonghg_indenergy_tech_coeff_USA: emission factors for industry energy use technologies in the base year in all U.S. states
    L275.nonghg_indenergy_tech_coeff_Yb_USA_NAs <- L232.StubTech_ind_USA %>%
      filter(supplysector == "other industrial energy use" & subsector %in% L270.nonghg_tg_state_indenergy_F_Yb$fuel) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=unique(L270.nonghg_tg_state_indenergy_F_Yb$Non.CO2))) %>%
      # need to use left join, as some NA values are retained (several states have NA emissions and fuel input for gas)
      left_join(agg_nonghg_indenergy_emiss_coeffs_F_Yb, by = c("region" = "state",
                                                                "subsector" = "fuel",
                                                                "Non.CO2", "year")) %>%
      select(-sector, -input.emissions)

    # Generate national median emissions factors for base years
    # Remove NAs so as to not skew the median
    L275.nonghg_indenergy_tech_coeff_Yb_USA.median.true <- L275.nonghg_indenergy_tech_coeff_Yb_USA_NAs %>%
      filter(!is.na(emiss.coef)) %>%
      group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
      summarise(emiss.coef = median(emiss.coef)) %>%
      ungroup() %>%
      rename(nationalEF = emiss.coef)

    # Some year / pollutant / sector / subsector / tech are NA for all entries, and should be set to 0
    L275.nonghg_indenergy_tech_coeff_Yb_USA.median.skewed <- L275.nonghg_indenergy_tech_coeff_Yb_USA_NAs %>%
      replace_na(list(emiss.coef = 0)) %>%
      group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
      summarise(emiss.coef = median(emiss.coef)) %>%
      ungroup() %>%
      rename(nationalEF = emiss.coef)

    # We want to join these tables so that only the entries not in median.true are retained from median.skewed
    # These all have EFs of 0
    L275.nonghg_indenergy_tech_coeff_Yb_USA.median <- L275.nonghg_indenergy_tech_coeff_Yb_USA.median.skewed %>%
      anti_join(L275.nonghg_indenergy_tech_coeff_Yb_USA.median.true, by=c("year", "Non.CO2", "supplysector", "subsector", "stub.technology") ) %>%
      # rebind to median.true
      bind_rows(L275.nonghg_indenergy_tech_coeff_Yb_USA.median.true)

    # Replace all emissions factors above a given value (20 * median) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
    L275.nonghg_indenergy_tech_coeff_Yb_USA.noBCOC <- L275.nonghg_indenergy_tech_coeff_Yb_USA_NAs %>%
      left_join_error_no_match(L275.nonghg_indenergy_tech_coeff_Yb_USA.median, by = c("year", "Non.CO2", "supplysector", "subsector", "stub.technology")) %>%
      # create a new column that has the threshold value
      mutate( threshold = nationalEF * 20,
              emiss.coef = if_else(emiss.coef > threshold | is.na(emiss.coef), nationalEF, emiss.coef)) %>%
      select(region, Non.CO2, year, supplysector, subsector, stub.technology, emiss.coef) %>%
      mutate(emiss.coef = if_else(is.infinite(emiss.coef), 1, emiss.coef))

    # Use fractions of PM2.5 to calculate BC/OC emissions.
    # We need to modify the BC_OC_assumptions table, as the BCOC_PM25_ratios table has updated values that are time dependent
    # If there are sector/subsector/tech combos that are in BCOC_PM25_ratios, we want to replace those entries in
    # the BC_OC_assumptions table. We also need to extend the data.
    # Extrapolate the data to future model years, and format the table
    BCOC_PM25_ratios_ext <- BCOC_PM25_ratios %>%
      gather_years() %>%
      complete(nesting(Parameter,sector,subsector,technology), year = MODEL_YEARS) %>%
      # extrapolate missing years
      group_by(Parameter,sector,subsector,technology) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      spread( Parameter, value ) %>%
      rename( "BC_fraction" = `BC Fraction`,
              "OC_fraction" = `OC Fraction`)

    BC_OC_assumptions_ext <- BC_OC_assumptions %>%
      mutate( year = min( MODEL_BASE_YEARS ) ) %>%
      complete(nesting(sector,subsector,technology, BC_fraction, OC_fraction), year = MODEL_YEARS)

      # Join the tables, keeping values from BC_OC_assumptions_ext that do not appear in BCOC_PM25_ratios
    BC_OC_assumptions_years <- BC_OC_assumptions_ext %>%
      anti_join( BCOC_PM25_ratios_ext, by = c("sector", "subsector", "technology", "year") ) %>%
      # bind to BCOC_PM25_assumptions
      bind_rows( BCOC_PM25_ratios_ext )

    L275.nonghg_indenergy_tech_coeff_USA_Yb <- compute_BC_OC(L275.nonghg_indenergy_tech_coeff_Yb_USA.noBCOC, BC_OC_assumptions_years) %>%
      # at this point we have some NA EFs due to renewable technologies not having BC/OC emissions
      # we can omit these
      na.omit()

    # Add industrial EFs for coal, gas, biomass, and refined liquids for future years from MARKAL
    # this is assuming industrial sector vintaging is being used. Could lead to discontinuity if not
    L275.nonghg_indenergy_tech_coeff_USA_Yf <- MARKAL_nonghg_indenergy_tech_coeff_USA_dhl %>%
      rename(emiss.coef = emiss.coeff) %>%
      mutate(supplysector = "other industrial energy use",
             subsector = gsub("liquid fuels", "refined liquids", subsector),
             stub.technology = gsub("liquid fuels", "refined liquids", stub.technology)) %>%
      filter(!(Non.CO2 %in% c("CH4","N2O","CO2")),
             year %in% MODEL_FUTURE_YEARS)

    # Bind all data together
    L275.nonghg_indenergy_tech_coeff_USA_no_driver <- bind_rows(L275.nonghg_indenergy_tech_coeff_USA_Yb,
                                                      L275.nonghg_indenergy_tech_coeff_USA_Yf)

    # Add an input name column to drive emissions
    L275.nonghg_indenergy_tech_coeff_USA <- L275.nonghg_indenergy_tech_coeff_USA_no_driver %>%
      # L232.StubTechCalInput_indenergy_USA has the fuel inputs that should be used to drive emissions
      left_join_error_no_match( L232.StubTechCalInput_indenergy_USA %>%
                                  select( c( "supplysector", "subsector", "stub.technology",
                                             "minicam.energy.input" ) ) %>%
                                  distinct( supplysector, subsector, stub.technology, minicam.energy.input ),
                                by = c("supplysector", "subsector", "stub.technology") ) %>%
      # rename to input.name to match header
      rename( input.name = minicam.energy.input ) %>%
      # change SO2 to SO2_1
      mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
      # filter out the minimum model base year. This is 1975, and is not used
      filter( year > min( MODEL_BASE_YEARS ) ) %>%
      # distinct to remove any potential duplicate rows
      distinct()

    # ===================================================

    # Produce outputs

    L275.nonghg_indenergy_tech_coeff_USA %>%
      add_title("Non-GHG input emissions parameters for industrial energy use sector in the USA") %>%
      add_units("Tg/EJ") %>%
      add_comments("EFs by state where available. In states without EFs for a given technology, but EFs present for other states, an average of other state EFs is used.") %>%
      add_legacy_name("L275.nonghg_indenergy_tech_coeff_USA") %>%
      add_precursors("L232.StubTechCalInput_indenergy_USA",
                     "L270.nonghg_tg_state_indenergy_F_Yb",
                     "L232.StubTech_ind_USA",
                     "gcam-usa/emissions/BC_OC_assumptions",
                     "gcam-usa/emissions/BCOC_PM25_ratios",
                     "gcam-usa/emissions/MARKAL_nonghg_indenergy_tech_coeff_USA_dhl") ->
      L275.nonghg_indenergy_tech_coeff_USA

    return_data(L275.nonghg_indenergy_tech_coeff_USA)

  } else {
    stop("Unknown command")
  }
}
