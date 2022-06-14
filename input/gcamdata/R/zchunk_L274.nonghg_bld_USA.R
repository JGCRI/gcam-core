#' module_gcamusa_L274.nonghg_bld_USA
#'
#' Non-GHG input emissions parameters for buildings technologies in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L274.nonghg_bld_tech_coeff_USA}. The corresponding file in the
#' original data system was \code{L274.bld_nonghg_USA.R} (gcam-usa level2).
#' @details Non-GHG input emissions parameters for buildings technologies in the USA
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY Aug 2019, MAW July 2021

module_gcamusa_L274.nonghg_bld_USA <- function(command, ...) {
if(command == driver.DECLARE_INPUTS) {
  return(c("L244.StubTechCalInput_bld_gcamusa",
           "L244.StubTech_bld_gcamusa",
           "L270.nonghg_tg_state_bld_F_Yb",
           FILE="gcam-usa/emissions/BC_OC_assumptions",
           FILE="gcam-usa/emissions/BCOC_PM25_ratios",
           FILE="gcam-usa/emissions/EPA_resid_wood_furnace_PM_EF_future_factors",
           FILE="gcam-usa/emissions/base_year_EF_techs"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c("L274.nonghg_bld_tech_coeff_USA"))
} else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # silence package check notes
  year <- supplysector <- region <- sector <- subsector <-calibrated.value <- calibrated.value.agg <- stub.technology <-
    S_F_tech_share <- value <- input.emissions <- fuel_input <- emiss.coef <- emiss.coef.avg <-
    stub.technology.new <- stub.technology.orig <- subsector.new <- subsector.orig <- supplysector.new <- supplysector.orig <- NULL

  # Load required inputs
  L244.StubTechCalInput_bld_gcamusa <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa", strip_attributes = TRUE)
  L244.StubTech_bld_gcamusa <- get_data(all_data, "L244.StubTech_bld_gcamusa", strip_attributes = TRUE)

  L270.nonghg_tg_state_bld_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_bld_F_Yb", strip_attributes = TRUE)

  BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions")
  BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
    # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
    select( -c( "Region", "Gains_Sector" ) ) %>%
    filter( !is.na( sector ) )
  EPA_resid_wood_furnace_PM_EF_future_factors <- get_data(all_data, "gcam-usa/emissions/EPA_resid_wood_furnace_PM_EF_future_factors")
  base_year_EF_techs <- get_data( all_data, "gcam-usa/emissions/base_year_EF_techs")

  # ===================================================

  # Perform computations
  # Compute technology shares based on level of detail available in NEI emissions data (state/sector/fuel)
  # State/sector/fuel shares of each buildings sector technology based on fuel input
  # All calculations done at the state level
  L244.StubTechCalInput_bld_gcamusa %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    mutate(sector = if_else(grepl("comm", supplysector), "comm", "resid")) %>%
    # The subsector is the same as the fuel in the buildings sector so use this as id variable
    group_by(region, sector, subsector, year) %>%
    summarise(calibrated.value = sum(calibrated.value)) %>%
    ungroup() ->
    agg_CalInput_bld_S_F_Ybf

  # Compute technology shares for each buildings sector technology, and match on the emissions
  L244.StubTechCalInput_bld_gcamusa %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    mutate(sector = if_else(grepl("comm", supplysector),"comm","resid")) %>%
    left_join_error_no_match(agg_CalInput_bld_S_F_Ybf, by = c("region", "sector", "subsector", "year"), suffix = c("", ".agg")) %>%
    ###MISSING VALUES: where agg fuel input is zero. Set to zero for now
    mutate(S_F_tech_share = if_else(is.na(calibrated.value/calibrated.value.agg), 0, calibrated.value/calibrated.value.agg)) %>%
    select(region, sector, supplysector, subsector, stub.technology, S_F_tech_share, year) ->
    bld_fuel_input_tech_shares

  Non.CO2 <- unique(L270.nonghg_tg_state_bld_F_Yb$Non.CO2)

  # Share out NEI emissions to the buildings technologies using the state/sector/fuel technology share
  # Base-year input emissions for buildings sector technologies in the USA
  L244.StubTech_bld_gcamusa %>%
    # Filter out electricity because its use in buildings does not emit pollutants
    filter(subsector != "electricity") %>%
    mutate(sector = if_else(grepl("comm", supplysector), "comm", "resid")) %>%
    repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
    repeat_add_columns(tibble::tibble(Non.CO2)) %>%
    # Match on the tech shares
    left_join_error_no_match(bld_fuel_input_tech_shares,
                             by = c("region", "supplysector", "subsector", "stub.technology", "sector", "year")) %>%
    # Cannot do left_join_error_no_match because of
    ###MISSING VALUES: there are no emissions in some state permutations that have fuel use in the base year. Leave them in
    left_join(L270.nonghg_tg_state_bld_F_Yb,
                             by = c("region" = "state", "sector", "subsector" = "fuel", "Non.CO2", "year")) %>%
    mutate(input.emissions = S_F_tech_share * value) %>%
    select(-sector,-S_F_tech_share) %>%
    distinct() ->
    L274.bld_nonghg_emissions_USA

  # Compute emission tech coefficients: divide emissions by fuel use
  # Base-year input emissions coefficients for buildings sector technologies in the USA (at the state-level)
  L274.nonghg_bld_tech_coeff_Yb_USA.NAs <- L274.bld_nonghg_emissions_USA %>%
    # Add on fuel inputs to the emissions table
    left_join_error_no_match(L244.StubTechCalInput_bld_gcamusa, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
    rename(fuel_input = calibrated.value) %>%
    # Compute tech coefficients. ###MISSING VALUES: corresponding to NAs in the emissions table
    mutate(emiss.coef = input.emissions / fuel_input)

  # Generate national median emissions factors for base years
  # Remove NAs so as to not skew the median
  L274.nonghg_bld_tech_coeff_Yb_USA.median.true <- L274.nonghg_bld_tech_coeff_Yb_USA.NAs %>%
    filter(!is.na(emiss.coef)) %>%
    group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
    summarise(emiss.coef = median(emiss.coef)) %>%
    ungroup() %>%
    rename(nationalEF = emiss.coef)

  # Some year / pollutant / sector / subsector / tech are NA for all entries, and should be set to 0
  L274.nonghg_bld_tech_coeff_Yb_USA.median.skewed <- L274.nonghg_bld_tech_coeff_Yb_USA.NAs %>%
    replace_na(list(emiss.coef = 0)) %>%
    group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
    summarise(emiss.coef = median(emiss.coef)) %>%
    ungroup() %>%
    rename(nationalEF = emiss.coef)

  # We want to join these tables so that only the entries not in median.true are retained from median.skewed
  # These all have EFs of 0
  L274.nonghg_bld_tech_coeff_Yb_USA.median <- L274.nonghg_bld_tech_coeff_Yb_USA.median.skewed %>%
    anti_join( L274.nonghg_bld_tech_coeff_Yb_USA.median.true, by=c("year", "Non.CO2", "supplysector", "subsector", "stub.technology") ) %>%
    # rebind to median.true
    bind_rows(L274.nonghg_bld_tech_coeff_Yb_USA.median.true)

  # Replace all emissions factors above a given value (20 * median) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
  L274.nonghg_bld_tech_coeff_Yb_USA.noBCOC <- L274.nonghg_bld_tech_coeff_Yb_USA.NAs %>%
    left_join_error_no_match(L274.nonghg_bld_tech_coeff_Yb_USA.median, by = c("year", "Non.CO2", "supplysector","subsector", "stub.technology")) %>%
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

  # Compute BC and OC EFs based off of PM2.5
  L274.nonghg_bld_tech_coeff_Yb_USA <- compute_BC_OC(L274.nonghg_bld_tech_coeff_Yb_USA.noBCOC, BC_OC_assumptions_years)

  # adding future PM EFs for residential wood furnace
  L274.nonghg_bld_tech_coeff_USA_ResWoodF_PM <- EPA_resid_wood_furnace_PM_EF_future_factors %>%
    # need to use left join because we are changing the number of rows in the table, having a row for every region (state)
    filter(year > MODEL_FINAL_BASE_YEAR) %>%
    left_join((L274.nonghg_bld_tech_coeff_Yb_USA %>% filter(year == MODEL_FINAL_BASE_YEAR)),
              by = c("supplysector", "stub.technology", "Non.CO2")) %>%
    mutate(emiss.coef = factor * emiss.coef) %>%
    rename(year = year.x) %>%
    select(-c(factor, year.y))

  # Compute future BC and OC EFs for residential wood furnace based off of PM2.5
  L274.nonghg_bld_tech_coeff_USA_ResWoodF <- compute_BC_OC(L274.nonghg_bld_tech_coeff_USA_ResWoodF_PM, BC_OC_assumptions_years)

  # bind into the full EF table
  L274.nonghg_bld_tech_coeff_USA <- bind_rows(L274.nonghg_bld_tech_coeff_Yb_USA, L274.nonghg_bld_tech_coeff_USA_ResWoodF) %>%
    # remove duplicate (PM emissions for resid heating wood furnace in future year) %>%
    distinct()

  # Copy EFs for technologies that don't exist in the base years from corresponding (similar?) technologies
  L274.nonghg_bld_tech_coeff_USA.missing_techs <- base_year_EF_techs %>%
    # need to use left join because we are changing the number of rows in the table, having a row for every region (state) and Non.CO2
    left_join(L274.nonghg_bld_tech_coeff_USA, by = c("supplysector.orig" = "supplysector",
                                                     "subsector.orig" = "subsector",
                                                     "stub.technology.orig" = "stub.technology")) %>%
    select(-supplysector.orig, -subsector.orig, -stub.technology.orig) %>%
    rename(supplysector = supplysector.new,
           subsector = subsector.new,
           stub.technology = stub.technology.new)

  L274.nonghg_bld_tech_coeff_USA_no_driver <- bind_rows(L274.nonghg_bld_tech_coeff_USA,
                                              L274.nonghg_bld_tech_coeff_USA.missing_techs)

  # Add an input name column to drive emissions
  L274.nonghg_bld_tech_coeff_USA <- L274.nonghg_bld_tech_coeff_USA_no_driver %>%
    # L244.StubTechCalInput_bld_gcamusa has the fuel inputs that should be used to drive emissions
    left_join_error_no_match( L244.StubTechCalInput_bld_gcamusa %>% select( c( "supplysector", "subsector", "stub.technology",
                                                                               "minicam.energy.input" ) ) %>%
                 distinct( supplysector, subsector, stub.technology, minicam.energy.input ),
                              by = c("supplysector", "subsector", "stub.technology") ) %>%
    # rename to input.name to match header
    rename( input.name = minicam.energy.input) %>%
    # change SO2 to SO2_1
    mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
    # filter out the minimum model base year. This is 1975, and is not used
    filter( year > min( MODEL_BASE_YEARS ) ) %>%
    # distinct to remove any potential duplicate rows
    distinct()


  # Now, L274.nonghg_bld_tech_coeff_USA must contain every non-electric technology. If not, throw an error.
  full_techs <- L244.StubTech_bld_gcamusa %>%
    filter(subsector != "electricity") %>%
    select(-region) %>%
    distinct()

  existing_base_year_techs <- L274.nonghg_bld_tech_coeff_USA %>%
    select(supplysector, subsector, stub.technology) %>%
    distinct()

  needed_base_year_techs <- anti_join(full_techs, existing_base_year_techs, by = c("supplysector", "subsector", "stub.technology"))

  if (nrow(needed_base_year_techs) != 0) {
    stop("Need to add all non-electric technologies not present in the base year to base_year_EF_techs.csv")
  }

  # ===================================================

  # Produce outputs
  L274.nonghg_bld_tech_coeff_USA %>%
    add_title("Non-GHG input emissions parameters for buildings technologies in the USA") %>%
    add_units("Tg/EJ") %>%
    add_comments("EFs by state where available. In states without EFs for a given technology, but EFs present for other states, an average of other state EFs is used.") %>%
    add_legacy_name("L274.nonghg_bld_tech_coeff_USA") %>%
    add_precursors("L244.StubTechCalInput_bld_gcamusa",
                   "L244.StubTech_bld_gcamusa",
                   "L270.nonghg_tg_state_bld_F_Yb",
                   "gcam-usa/emissions/BC_OC_assumptions",
                   "gcam-usa/emissions/BCOC_PM25_ratios",
                   "gcam-usa/emissions/EPA_resid_wood_furnace_PM_EF_future_factors",
                   "gcam-usa/emissions/base_year_EF_techs") ->
    L274.nonghg_bld_tech_coeff_USA

  return_data(L274.nonghg_bld_tech_coeff_USA)

} else {
    stop("Unknown command")
  }
}

