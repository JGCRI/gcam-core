#' module_gcamusa_L2236.elecS_ghg_emissions_USA
#'
#' U.S. electricity non CO2 GHG emission and future emission coefficients by sector, fuel and cooling techs
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:  \code{L2236.elecS_ghg_emissions_USA}, \code{L2236.elecS_cool_ghg_tech_coeff_USA}.
#' The corresponding file in the original data system was \code{L2236.elecS_ghg_emissions_USA} (gcam-usa level2).
#' @details Write electricity emission coefficients to multiple load segments and cooling technology then compute state
#' shares for each category in the fuel input table.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD August 2018 / YO March 2022
module_gcamusa_L2236.elecS_ghg_emissions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c('L1231.in_EJ_state_elec_F_tech',
             'L201.OutputEmissions_elec',
             'L241.OutputEmissCoeff_elec',
             FILE = 'gcam-usa/A23.elecS_tech_mapping',
             FILE = 'gcam-usa/A23.elecS_tech_availability',
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             # the following files to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/A22.globaltech_input_driver",
             FILE = "energy/A23.globaltech_input_driver",
             FILE = "energy/A25.globaltech_input_driver",
             'L2233.StubTechMarket_elecS_cool_USA',
             'L2233.StubTechProd_elecS_cool_USA'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2236.elecS_cool_ghg_tech_coeff_USA',
             'L2236.elecS_cool_ghg_emissions_USA'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, 'L1231.in_EJ_state_elec_F_tech', strip_attributes = TRUE)
    L201.OutputEmissions_elec <- get_data(all_data, 'L201.OutputEmissions_elec', strip_attributes = TRUE)
    L241.OutputEmissCoeff_elec <- get_data(all_data, 'L241.OutputEmissCoeff_elec', strip_attributes = TRUE)
    A23.elecS_tech_mapping <- get_data(all_data, 'gcam-usa/A23.elecS_tech_mapping', strip_attributes = TRUE)
    A23.elecS_tech_availability <- get_data(all_data, 'gcam-usa/A23.elecS_tech_availability', strip_attributes = TRUE)
    A23.elecS_tech_mapping_cool <- get_data(all_data, 'gcam-usa/A23.elecS_tech_mapping_cool', strip_attributes = TRUE)
    L2233.StubTechMarket_elecS_cool_USA <- get_data(all_data, 'L2233.StubTechMarket_elecS_cool_USA', strip_attributes = TRUE)
    L2233.StubTechProd_elecS_cool_USA <- get_data(all_data, 'L2233.StubTechProd_elecS_cool_USA', strip_attributes = TRUE)

    # Silence package checks
    CH4 <- Electric.sector <- Electric.sector.technology <- N2O <- Non.CO2 <-
      calOutputValue <- emiss.coeff <- fuel <- fuel_input <- fuel_input_USA <-
      fuel_input_share <- input.emissions <- palette <- region <- segment_share <-
      state <- stub.technology <- subsector <- subsector_1 <- supplysector <-
      tech_calOuput <- tech_fuel_input <- technology <- value <- year <-
      to.technology <- plant_type <- cooling_system <- water_type <-
      to.technology <- supplysector.y <- supplysector.x <- subsector0 <- NULL



    # ===========================================================================================

    # 2.  Build tables for CSVs
    # 2a. Emission coefficients
    # Write electricity emission coefficients to multiple load segments
    # Remove technologies in the electricity load segments that don't make sense
    L2234.load_segments <- unique(A23.elecS_tech_mapping$Electric.sector)

    add_cooling_techs <- function(data){
      data %>%
        # use left_join becuase the number of rows will change since we map the same fuel technology
        # to different cooling options
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology) -> data_new
      return(data_new)
    }

    A23.elecS_tech_mapping %>%
      anti_join(A23.elecS_tech_availability,
                by = c("Electric.sector.technology" = "stub.technology")) %>%
      mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
      arrange(subsector, Electric.sector) %>%
      mutate(Electric.sector = as.character(Electric.sector)) ->
      A23.elecS_tech_mapping

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver
    bind_rows(
      get_data(all_data, "energy/A22.globaltech_input_driver"),
      get_data(all_data, "energy/A23.globaltech_input_driver"),
      get_data(all_data, "energy/A25.globaltech_input_driver")
    ) %>%
      rename(stub.technology = technology) ->
      EnTechInput

    EnTechInput %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="technology",
                     "supplysector","subsector")) %>%
      na.omit() %>%
      select(-supplysector,-stub.technology,-subsector_1,-plant_type,-cooling_system,-water_type)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = Electric.sector.technology,
             supplysector = Electric.sector)->
      EnTechInputMapCool

    EnTechInput %>%
      left_join(A23.elecS_tech_mapping,
                by=c("stub.technology" = "technology",
                     "supplysector", "subsector")) %>%
      na.omit() %>%
      select(-supplysector, -subsector_1) %>%
      rename(technology = stub.technology,
             subsector0 = subsector,
             subsector = Electric.sector.technology,
             supplysector = Electric.sector) ->
      EnTechInputMap

    L241.OutputEmissCoeff_elec %>%
      filter(region == gcam.USA_REGION, supplysector == "electricity", Non.CO2 %in% emissions.GHG_NAMES ) %>%
      left_join(A23.elecS_tech_mapping %>%
                  select(-subsector),
                by = c("supplysector", "subsector" =  "subsector_1", "stub.technology" =  "technology")) %>%
      select(Electric.sector, subsector, Electric.sector.technology, year, Non.CO2, emiss.coeff) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      add_cooling_techs() %>%
      select(-supplysector.y) %>%
      repeat_add_columns(tibble('region' = gcamusa.STATES)) %>%
      semi_join(L2233.StubTechMarket_elecS_cool_USA %>%
                  select(region, supplysector, subsector0, subsector, technology, year),
                by = c("supplysector", "subsector0", "subsector", "technology", "year", "region")) %>%
      select(region, supplysector, subsector0, subsector, technology, year, Non.CO2, emiss.coeff) %>%
      left_join_error_no_match(EnTechInputMapCool,
                               by = c("supplysector", "subsector0", "subsector", "technology")) ->
      L2236.elecS_cool_ghg_tech_coeff_USA

    # 2b. Input Emissions
    # L236.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state
    # Filter the emissions data for USA & electricity sector
    L201.OutputEmissions_elec %>%
      filter(region == gcam.USA_REGION & grepl("electricity", supplysector) & Non.CO2 %in% emissions.GHG_NAMES) %>%
      spread(Non.CO2, input.emissions) ->
      L2236.elec_ghg_emissions_USA

    # Organize the state fuel input data
    # Electricity segments
    L1231.in_EJ_state_elec_F_tech %>%
      mutate(sector = "electricity") %>%
      filter(year %in% L2236.elec_ghg_emissions_USA$year & technology %in% L2236.elec_ghg_emissions_USA$stub.technology) %>%
      # We do not expect at 1:1 match may use a left_join here.
      left_join(A23.elecS_tech_mapping %>%
                  select(-subsector_1),
                by = c("sector" = "supplysector", "technology")) %>%
      select(state, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology,
             year, technology, fuel, tech_fuel_input = value) %>%
      # use left_join becuase the number of rows will change (same fuel into multiple cooling techs)
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector","technology")) %>%
      select(-technology,-subsector_1,-supplysector.y)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology) %>%
      # We do not expect a 1:1 match so we can use a left_join here
      left_join(L2233.StubTechProd_elecS_cool_USA %>% rename("stub.technology"="technology") %>%
                  select(LEVEL2_DATA_NAMES[['StubTechYr']], subsector0, calOutputValue),
                by = c("state" = "region", "supplysector","subsector0", "subsector", "technology"="stub.technology", "year")) %>%
      group_by(state, technology, year) %>%
      mutate(tech_calOuput = sum(calOutputValue),
             segment_share = calOutputValue / tech_calOuput,
             fuel_input = round(tech_fuel_input * segment_share, 6),
             fuel_input = if_else(is.na(fuel_input), 0, fuel_input)) %>%
      ungroup() %>%
      select(state, supplysector,subsector0,subsector, technology, year, technology, fuel, fuel_input) %>%
      semi_join(L2233.StubTechMarket_elecS_cool_USA %>% rename("stub.technology"="technology") %>%
                  select(LEVEL2_DATA_NAMES[['StubTechYr']],subsector0),
                by = c("supplysector", "technology"="stub.technology", "year", "state"  = "region")) ->
      L2236.elecS_cool_fuel_input_state

    # Compute state shares for each category in the fuel input table
    # Share out CH4 and N2O emissions by state based on the fuel input shares
    L2236.elecS_cool_fuel_input_state %>%
      # add back electric technologies without cooling
      left_join_error_no_match(A23.elecS_tech_mapping_cool %>%
                                 select(stub.technology = technology, technology = to.technology) %>% distinct(),
                               by = "technology") %>%
      left_join_error_no_match(L2236.elec_ghg_emissions_USA %>% select(-region, -supplysector, -subsector),
                               by = c("stub.technology", "year")) %>%
      group_by(stub.technology, year) %>%
      mutate(fuel_input_USA = sum(fuel_input),
             fuel_input_share = signif(fuel_input, 5) / signif(fuel_input_USA, 8),
             CH4 = fuel_input_share * CH4,
             N2O = fuel_input_share * N2O) %>%
      ungroup() %>%
      select(region = state, supplysector, subsector0, subsector, technology, year, CH4, N2O) %>%
      replace_na(list(CH4 = 0, N2O = 0)) ->
      L2236.elecS_cool_ghg_emissions_state

    # Format for csv file
    L2236.elecS_cool_ghg_emissions_state %>%
      gather(Non.CO2, input.emissions, -region, -supplysector, -subsector0, -subsector, -technology, -year, convert=TRUE) %>%
      arrange(region, supplysector, subsector0, subsector, technology, Non.CO2, year) %>%
      rename(stub.technology=technology) %>%
      select(LEVEL2_DATA_NAMES[['StubTechYr']], subsector0, Non.CO2, input.emissions) %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) ->
      L2236.elecS_cool_ghg_emissions_USA

    # Check for missing values
    stopifnot(!any(is.na(L2236.elecS_cool_ghg_tech_coeff_USA)))
    stopifnot(!any(is.na(L2236.elecS_cool_ghg_emissions_USA)))

    # ===========================================================================================

    # Produce outputs

    L2236.elecS_cool_ghg_tech_coeff_USA %>%
      add_title("U.S. electricity GHG emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_legacy_name("L2236.elecS_cool_ghg_tech_coeff_USA") %>%
      add_precursors('L1231.in_EJ_state_elec_F_tech',
                     'L201.OutputEmissions_elec',
                     'L241.OutputEmissCoeff_elec',
                     'gcam-usa/A23.elecS_tech_mapping',
                     'gcam-usa/A23.elecS_tech_availability',
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "energy/A22.globaltech_input_driver",
                     "energy/A23.globaltech_input_driver",
                     "energy/A25.globaltech_input_driver",
                     'L2233.StubTechMarket_elecS_cool_USA',
                     'L2233.StubTechProd_elecS_cool_USA') ->
      L2236.elecS_cool_ghg_tech_coeff_USA

    L2236.elecS_cool_ghg_emissions_USA %>%
      add_title("U.S. electricity non CO2 emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_legacy_name("L2236.elecS_cool_ghg_emissions_USA") %>%
      add_precursors('L1231.in_EJ_state_elec_F_tech',
                     'L201.OutputEmissions_elec',
                     'L241.OutputEmissCoeff_elec',
                     'gcam-usa/A23.elecS_tech_mapping',
                     'gcam-usa/A23.elecS_tech_availability',
                     "energy/A22.globaltech_input_driver",
                     "energy/A23.globaltech_input_driver",
                     "energy/A25.globaltech_input_driver",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     'L2233.StubTechMarket_elecS_cool_USA',
                     'L2233.StubTechProd_elecS_cool_USA') ->
      L2236.elecS_cool_ghg_emissions_USA

    return_data(L2236.elecS_cool_ghg_tech_coeff_USA,
                L2236.elecS_cool_ghg_emissions_USA)

  } else {
    stop("Unknown command")
  }
}
