#' module_gcamusa_L277.nonghg_prc_USA
#'
#' Generates input emissions for process and cement sectors by energy technology for Non.CO2 air pollutants for U.S. states
#' Writes out max emissions reductions and steepness to all process-related energy technologies and states
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L277.nonghg_prc_USA}, \code{L277.nonghg_max_reduction_USA}, \code{L277.nonghg_steepness}.
#' @details Generates input emissions by energy technology for Non.CO2 air pollutants for U.S. states, Writes out max emissions reductions and steepness to all process-related energy technologies and states
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY September 2019, MAW May 2021
#'
module_gcamusa_L277.nonghg_prc_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.nonco2_max_reduction",
             "L232.nonco2_steepness",
             "L270.nonghg_tg_state_prc_F_Yb",
             FILE="gcam-usa/emissions/BC_OC_assumptions",
             FILE="gcam-usa/emissions/BCOC_PM25_ratios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L277.nonghg_prc_USA",
             "L277.nonghg_max_reduction_USA",
             "L277.nonghg_steepness_USA"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    CEDS_Fuel <- GCAM_sector <- emissions <- state <- sector <- Non.CO2 <- year <- region <- stub.technology <-
      input.emissions <- emissions.total.Non.CO2 <- supplysector <- subsector <- share <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.nonco2_max_reduction <- get_data(all_data, "L232.nonco2_max_reduction", strip_attributes = TRUE)
    L232.nonco2_steepness <- get_data(all_data, "L232.nonco2_steepness", strip_attributes = TRUE)
    L270.nonghg_tg_state_prc_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_prc_F_Yb", strip_attributes = TRUE) %>%
    # don't need fuel column because "process" is not a fuel, and just assigned to identify process emissions
      select( -fuel )
    BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions")
    BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
      # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
      select( -c( "Region", "Gains_Sector" ) ) %>%
      filter( !is.na( sector ) )

    # create a template which has all sectors and Non.CO2 pollutants for each state
    L277.NEI_agg_all_states <- L270.nonghg_tg_state_prc_F_Yb %>%
      # select GCAM base years
      filter( year %in% MODEL_BASE_YEARS ) %>%
      select(-c( state, value )) %>%
      distinct() %>%
      write_to_all_states(c("region", "sector", "Non.CO2", "year")) %>%
      # need to use left_join because there will be some NAs - certain states do not have emissions for a particular Non.CO2 and sector
      left_join(L270.nonghg_tg_state_prc_F_Yb, by = c("region" = "state", "sector", "Non.CO2", "year")) %>%
      # need to fill in states with no emissions with 0
      mutate(value = if_else(is.na(value), 0, value))

    # INDUSTRY PROCESSES
    # get the industrial process emissions by state, s/s/t and Non.CO2
    L277.nonghg_ind_prc_USA <- L277.NEI_agg_all_states %>%
      filter(sector %in% gcamusa.IND_PROC_EM_NEI_GCAM_SECTORS) %>%
      rename(subsector = sector) %>%
      mutate(supplysector = "industrial processes",
             # change industrial processes subsector and stub.tech to other industrial processes
             subsector = gsub( "industry_processes", "other industrial processes", subsector ),
             stub.technology = subsector ) %>%
      rename(input.emissions = value) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions)

    # URBAN PROCESSES
    # get the solvent and other industrial process emissions by state, s/s/t, and Non.CO2
    L277.nonghg_urb_prc_USA_noBCOC <- L277.NEI_agg_all_states %>%
      filter(sector %in% gcamusa.URB_PROC_EM_NEI_GCAM_SECTORS) %>%
      rename(subsector = sector) %>%
      mutate(stub.technology = subsector,
             supplysector = "urban processes") %>%
      # these are input emissions, not emission coefficients. Just naming it this for use in
      # the compute_BC_OC function
      rename(emiss.coef = value) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coef)

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

    # Compute BC and OC emissions based off of PM2.5
    L277.nonghg_urb_prc_USA <- compute_BC_OC(L277.nonghg_urb_prc_USA_noBCOC, BC_OC_assumptions_years) %>%
      # renaming back to input emissions
      rename(input.emissions = emiss.coef) %>%
      # BC and OC are NA for some subsectors due to being non-combustion
      filter( !is.na(input.emissions) )

    # CEMENT
    # get the cement emissions by state, s/s/t, and Non.CO2
    # NOTE: in the future, this really should be in the "process heat cement" and have EFs by fuel,
    # multiplied out to get a default split, and then scale everything so that it matches the inventory.
    # Most of these are not actual process emissions, except for PM2.5
    L277.nonghg_cement_USA <- L277.NEI_agg_all_states %>%
      filter(sector %in% gcamusa.CEMENT_NEI_GCAM_SECTORS) %>%
      rename(supplysector = sector) %>%
      mutate(subsector = supplysector) %>%
      repeat_add_columns(tibble(stub.technology = gcamusa.CEMENT_TECHS)) %>%
      rename(input.emissions = value) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions) %>%
      # ensure we do not have duplicate entries
      group_by(region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      mutate(input.emissions = sum(input.emissions)) %>%
      distinct()

    # join industrial processes and urban processes, and cement
    # NOTE: when cement is redone to have an input driver, it will need to be its own table and have a different header
    L277.nonghg_prc_USA <- bind_rows(L277.nonghg_ind_prc_USA, L277.nonghg_urb_prc_USA, L277.nonghg_cement_USA) %>%
      #change SO2 to SO2_1
      mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) )

    # Add GDP controls for the process sectors
    # We want overwrite the global control values and assign a max reduction of 30 to all process sector pollutants.
    # This should give a reasonable future trajectory that is consistent between emission species.
    # We also need to add "wastewater treatment" into the control

    # Copy max reduction format for air pollutants to the states
    # PM and NH3 are not included in the L232 table, so will be added manually
    L277.nonghg_max_reduction_USA_noPM <- L232.nonco2_max_reduction %>%
      # Filter the L277 inputs to just the USA region and non-GHGs
      filter(region == gcam.USA_REGION, Non.CO2 %in% emissions.NONGHG_PROC_SECTORS) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["GDPCtrlMax"]]) %>%
      mutate(year = max(MODEL_BASE_YEARS),
             # this is a somewhat arbitrary value, but consistent between pollutants
             max.reduction = gcamusa.NONGHG_PROC_SECTORS.gdp_max_reduction ) %>%
      distinct()

    # Adding controls for PM and NH3 emissions
    L277.nonghg_max_reduction_USA_nowastewater <- L277.nonghg_max_reduction_USA_noPM %>%
      # Filtering for a single pollutant so we have one entry per state / sector / year
      filter(Non.CO2 == "NMVOC") %>%
      # then removing this column
      select( -Non.CO2 ) %>%
      repeat_add_columns(tibble(Non.CO2 = gcamusa.NONGHG_PROC_SECTORS.missing_pollutants)) %>%
      # rebind to table without PMs and NH3
      bind_rows( L277.nonghg_max_reduction_USA_noPM )

    # Adding controls for wastewater
    L277.nonghg_max_reduction_USA <- L277.nonghg_max_reduction_USA_nowastewater %>%
      # Filtering for a single subsector (also in urban processes) so we have one entry per state /  year / pollutant
      filter(subsector == "landfills") %>%
      # then removing this column
      select( -subsector ) %>%
      repeat_add_columns(tibble(subsector = gcamusa.NONGHG_PROC_SECTORS.missing_subsectors)) %>%
      # making the stub.technology name match the subsector name
      mutate( stub.technology = subsector ) %>%
      # rebind to table without wastewater
      bind_rows( L277.nonghg_max_reduction_USA_nowastewater )

    # Doing the same thing for steepness
    L277.nonghg_steepness_USA_noPM <- L232.nonco2_steepness %>%
      # Filter the L277 inputs to just the USA region and non-GHGs
      filter(region == gcam.USA_REGION, Non.CO2 %in% emissions.NONGHG_PROC_SECTORS) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["GDPCtrlSteep"]]) %>%
      mutate(year = max(MODEL_BASE_YEARS),
             # this is the current value for all pollutants anyway
             steepness = gcamusa.NONGHG_PROC_SECTORS.gdp_steepness) %>%
      distinct()

    # Adding controls for PM and NH3 emissions
    L277.nonghg_steepness_USA_nowastewater <- L277.nonghg_steepness_USA_noPM %>%
      # Filtering for a single pollutant so we have one entry per state / sector / year
      filter(Non.CO2 == "NMVOC") %>%
      # then removing this column
      select( -Non.CO2 ) %>%
      repeat_add_columns(tibble(Non.CO2 = gcamusa.NONGHG_PROC_SECTORS.missing_pollutants)) %>%
      # rebind to table without PMs and NH3
      bind_rows( L277.nonghg_steepness_USA_noPM )

    # Adding controls for wastewater
    L277.nonghg_steepness_USA <- L277.nonghg_steepness_USA_nowastewater %>%
      # Filtering for a single subsector (also in urban processes) so we have one entry per state /  year / pollutant
      filter(subsector == "landfills") %>%
      # then removing this column
      select( -subsector ) %>%
      repeat_add_columns(tibble(subsector = gcamusa.NONGHG_PROC_SECTORS.missing_subsectors)) %>%
      # making the stub.technology name match the subsector name
      mutate( stub.technology = subsector ) %>%
      # rebind to table without wastewater
      bind_rows( L277.nonghg_steepness_USA_nowastewater )



    # ===================================================
    # Produce outputs
    L277.nonghg_prc_USA %>%
      add_title("Non-GHG by technology for all U.S. states") %>%
      add_units("Tg") %>%
      add_comments("Uses state-level Non.CO2 data from NEI, tech shares from L277.nonco2_prc to disaggregate into supplysector / subsector / stub.technology") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L270.nonghg_tg_state_prc_F_Yb",
                     "gcam-usa/emissions/BC_OC_assumptions",
                     "gcam-usa/emissions/BCOC_PM25_ratios") ->
      L277.nonghg_prc_USA

    L277.nonghg_max_reduction_USA %>%
      add_title("Maximum Non-GHG reduction by energy technology, region, gas, and base year for all U.S. states") %>%
      add_units("Percentage reduction from baseline") %>%
      add_comments("Applied maximum reductions by technology and gas to all regions") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L232.nonco2_max_reduction") ->
      L277.nonghg_max_reduction_USA

    L277.nonghg_steepness_USA %>%
      add_title("Non-GHG reduction steepness by energy technology, region, gas, and base year for all U.S. states") %>%
      add_units("Unitless") %>%
      add_comments("Applied maximum steepness by technology and gas to all regions") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L232.nonco2_steepness") ->
      L277.nonghg_steepness_USA



    return_data(L277.nonghg_prc_USA,
                L277.nonghg_max_reduction_USA,
                L277.nonghg_steepness_USA)
  } else {
    stop("Unknown command")
  }
}
