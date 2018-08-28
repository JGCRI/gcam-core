#' module_gcam.usa_L2236.elecS_ghg_emissions_USA
#'
#' U.S. electricity non CO2 and GHG emission coefficients by technology sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2236.elecS_ghg_tech_coeff_USA} and \code{L2236.elecS_ghg_emissions_USA}. The corresponding file in the
#' original data system was \code{L2236.elecS_ghg_emissions_USA} (gcam-usa level2).
#' @details Write electricity emission coefficients to multiple load segments then compute state
#' shares for each category in the fuel input table.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD August 2018
module_gcam.usa_L2236.elecS_ghg_emissions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c('L1231.in_EJ_state_elec_F_tech',
             'L201.en_ghg_emissions',
             'L241.nonco2_tech_coeff',
             FILE = 'gcam-usa/A23.elecS_tech_associations',
             FILE = 'gcam-usa/A23.elecS_tech_availability',
             'L2234.StubTechMarket_elecS_USA',
             'L2234.StubTechProd_elecS_USA'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2236.elecS_ghg_tech_coeff_USA',
             'L2236.elecS_ghg_emissions_USA'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, 'L1231.in_EJ_state_elec_F_tech')
    L201.en_ghg_emissions <- get_data(all_data, 'L201.en_ghg_emissions')
    L241.nonco2_tech_coeff <- get_data(all_data, 'L241.nonco2_tech_coeff')
    A23.elecS_tech_associations <- get_data(all_data, 'gcam-usa/A23.elecS_tech_associations')
    A23.elecS_tech_availability <- get_data(all_data, 'gcam-usa/A23.elecS_tech_availability')
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, 'L2234.StubTechMarket_elecS_USA')
    L2234.StubTechProd_elecS_USA <- get_data(all_data, 'L2234.StubTechProd_elecS_USA')

    # Silence package checks
    CH4 <- Electric.sector <- Electric.sector.technology <- N2O <- Non.CO2 <-
      calOutputValue <- emiss.coeff <- fuel <- fuel_input <- fuel_input_USA <-
      fuel_input_share <- input.emissions <- palette <- region <- segment_share <-
      state <- stub.technology <- subsector <- subsector_1 <- supplysector <-
      tech_calOuput <- tech_fuel_input <- technology <- value <- year <- NULL

    # ===========================================================================================

    # 2.  Build tables for CSVs
    # 2a. Emission coefficients
    # Write electricity emission coefficients to multiple load segments
    # Remove technologies in the electricity load segments that don't make sense
    L2234.load_segments <- unique(A23.elecS_tech_associations$Electric.sector)

    A23.elecS_tech_associations %>%
      anti_join(A23.elecS_tech_availability,
                by = c("Electric.sector.technology" = "stub.technology")) %>%
      mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
      arrange(subsector, Electric.sector) %>%
      mutate(Electric.sector = as.character(Electric.sector)) ->
      A23.elecS_tech_associations

    L241.nonco2_tech_coeff %>%
      filter(region == "USA", supplysector == "electricity", Non.CO2 %in% c("N2O","CH4") ) %>%
      left_join(A23.elecS_tech_associations %>%
                  select(-subsector),
                by = c("supplysector", "subsector" =  "subsector_1", "stub.technology" =  "technology")) %>%
      select(Electric.sector, subsector, Electric.sector.technology, year, Non.CO2, emiss.coeff) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      repeat_add_columns(tibble('region' = gcamusa.STATES)) %>%
      semi_join(L2234.StubTechMarket_elecS_USA %>%
                  select(region, supplysector, subsector, stub.technology, year),
                by = c("supplysector", "subsector", "stub.technology", "year", "region")) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L2236.elecS_ghg_tech_coeff_USA

    # 2b. Input Emissions
    # L236.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state
    # Filter the emissions data for USA & electricity sector
    L201.en_ghg_emissions %>%
      filter(region == "USA" & grepl("electricity", supplysector)) %>%
      spread(Non.CO2, input.emissions) ->
      L2236.elec_ghg_emissions_USA

    # Organize the state fuel input data
    # Electricity segments
    L1231.in_EJ_state_elec_F_tech %>%
      mutate(sector = "electricity") %>%
      filter(year %in% L2236.elec_ghg_emissions_USA$year & technology %in% L2236.elec_ghg_emissions_USA$stub.technology) %>%
      # We do not expect at 1:1 match may use a left_join here.
      left_join(A23.elecS_tech_associations %>%
                  select(-subsector_1),
                by = c("sector" = "supplysector", "technology")) %>%
      select(state, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology,
             year, technology, fuel, tech_fuel_input = value) %>%
      # We do not expect a 1:1 match so we can use a left_join here
      left_join(L2234.StubTechProd_elecS_USA %>%
                  select(LEVEL2_DATA_NAMES[['StubTechYr']], calOutputValue),
                by = c("state" = "region", "supplysector", "subsector", "stub.technology", "year")) %>%
      group_by(state, technology, year) %>%
      mutate(tech_calOuput = sum(calOutputValue),
             segment_share = calOutputValue / tech_calOuput,
             fuel_input = round(tech_fuel_input * segment_share, 6),
             fuel_input = if_else(is.na(fuel_input), 0, fuel_input)) %>%
      ungroup %>%
      select(state, supplysector, stub.technology, year, technology, fuel, fuel_input) %>%
      semi_join(L2234.StubTechMarket_elecS_USA %>%
                  select(LEVEL2_DATA_NAMES[['StubTechYr']]),
                by = c("supplysector", "stub.technology", "year", "state"  = "region")) ->
      L2236.elecS_fuel_input_state

    # Compute state shares for each category in the fuel input table
    # Share out CH4 and N2O emissions by state based on the fuel input shares
    L2236.elecS_fuel_input_state %>%
      mutate(sector = "electricity") %>%
      # We do not expect a 1:1 match so we can use a left join here.
      left_join(L2236.elec_ghg_emissions_USA %>%
                                 select(-region) %>%
                                 rename(sector = supplysector, technology = stub.technology),
                               by = c("technology", "year", "sector")) %>%
      group_by(technology, year) %>%
      mutate(fuel_input_USA = sum(fuel_input),
             fuel_input_share = signif(fuel_input, 5) / signif(fuel_input_USA, 8),
             CH4 = fuel_input_share * CH4,
             N2O = fuel_input_share * N2O) %>%
      ungroup %>%
      select(region = state, supplysector, subsector, stub.technology, year, CH4, N2O) ->
      L2236.elecS_ghg_emissions_state

    # Format for csv file
    L2236.elecS_ghg_emissions_state %>%
      gather(Non.CO2, input.emissions, -region, -supplysector, -subsector, -stub.technology, -year, convert=TRUE) %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      select(LEVEL2_DATA_NAMES[['StubTechYr']], Non.CO2, input.emissions) %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) ->
      L2236.elecS_ghg_emissions_USA

    # Check for missing values
    stopifnot(!any(is.na(L2236.elecS_ghg_tech_coeff_USA)))
    stopifnot(!any(is.na(L2236.elecS_ghg_emissions_USA)))

    # ===========================================================================================

    # Produce outputs
    L2236.elecS_ghg_tech_coeff_USA %>%
      add_title("U.S. electricity GHG emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_legacy_name("L2236.elecS_ghg_tech_coeff_USA") %>%
      add_precursors('L1231.in_EJ_state_elec_F_tech',
                     'L201.en_ghg_emissions',
                     'L241.nonco2_tech_coeff',
                     'gcam-usa/A23.elecS_tech_associations',
                     'gcam-usa/A23.elecS_tech_availability',
                     'L2234.StubTechMarket_elecS_USA',
                     'L2234.StubTechProd_elecS_USA') ->
      L2236.elecS_ghg_tech_coeff_USA

    L2236.elecS_ghg_emissions_USA %>%
      add_title("U.S. electricity non CO2 emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_legacy_name("L2236.elecS_ghg_emissions_USA") %>%
      add_precursors('L1231.in_EJ_state_elec_F_tech',
                     'L201.en_ghg_emissions',
                     'L241.nonco2_tech_coeff',
                     'gcam-usa/A23.elecS_tech_associations',
                     'gcam-usa/A23.elecS_tech_availability',
                     'L2234.StubTechMarket_elecS_USA',
                     'L2234.StubTechProd_elecS_USA') ->
      L2236.elecS_ghg_emissions_USA

    return_data(L2236.elecS_ghg_tech_coeff_USA,
                L2236.elecS_ghg_emissions_USA)

  } else {
    stop("Unknown command")
  }
}
