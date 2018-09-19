#' module_gcam.usa_L2243.ren_soft_cal_USA
#'
#' Create state-level fixed output for renewable (wind and solar) electricity generation in GCAM-USA with
#' multiple load segments to "soft calibrate" 2015 generation based on EIA historical data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2243.StubTechFixOut_ren_soft_cal_2015_USA}, \code{L2243.GlobalTechShrwt_ren_soft_cal_2015_USA},
#' \code{L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA}.
#' The corresponding file in the original data system was \code{L2243.ren_soft_cal.R} (gcam-usa level2).
#' @details Create fixed outputs to "soft calibrate" 2015 electricity generation to historical data for GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB September 2018
module_gcam.usa_L2243.ren_soft_cal_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/states_subregions',
             FILE = 'gcam-usa/A23.elec_tech_associations_ren_soft_cal',
             FILE = 'gcam-usa/EIA_net_gen_renewables',
             FILE = 'gcam-usa/EIA_tech_mapping',
             'L223.StubTechProd_elec_USA'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2243.StubTechFixOut_ren_soft_cal_2015_USA",
             "L2243.GlobalTechShrwt_ren_soft_cal_2015_USA",
             "L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, 'gcam-usa/states_subregions')
    A23.elec_tech_ren_soft_cal <- get_data(all_data, 'gcam-usa/A23.elec_tech_associations_ren_soft_cal')
    EIA_net_gen_renewables <- get_data(all_data, 'gcam-usa/EIA_net_gen_renewables')
    EIA_tech_mapping <- get_data(all_data, 'gcam-usa/EIA_tech_mapping')
    L223.StubTechProd_elec_USA <- get_data(all_data, 'L223.StubTechProd_elec_USA')

    # Silence package checks
    source.key <- description <- fuel <- year <- value <- state <- state_name <- gcam_tech <-
      stub.technology <- region <- calOutputValue <- . <- gen2015 <- gen2010 <- fixedOutput <-
      supplysector <- subsector <- new.technology <- sector.name <- subsector.name <- technology <-
      primary.renewable <- NULL

    # ===================================================
    # Data Processing

    EIA_net_gen_renewables %>%
      select(-source.key) %>%
      filter(!is.na(units)) %>%
      separate(description, c("state", "fuel"), sep = " : ", remove = TRUE) %>%
      filter(!is.na(fuel)) %>%
      gather(year, value, -state, -fuel, -units) %>%
      group_by(state, units, year, value) %>%
      distinct(fuel) %>%
      ungroup() %>%
      mutate(value = as.numeric(value),
             value = if_else(is.na(value), 0, value),
             units = "GWh") %>%
      mutate(year = gsub("X","",year)) %>%
      mutate(year = as.numeric(year)) %>%
      # Fix DC state name - EIA uses  District "Of" Columbia while we use "of"
      mutate(state = gsub("Of","of", state)) %>%
      rename(state_name = state) %>%
      left_join_error_no_match(states_subregions %>%
                  select(state, state_name),
                by = c("state_name")) %>%
      select(-state_name)%>%
      left_join_error_no_match(EIA_tech_mapping, by = "fuel") %>%
      filter(year == min(FUTURE_YEARS)) %>%
      mutate(value = value * CONV_GWH_EJ) %>%
      select(state, stub.technology = gcam_tech, year, gen2015 = value, -units) -> L2243.ren_gen_2015

    # Using 2010 renewable generation data from GCAM-USA data-system, calculate the difference between 2015 and
    # 2010 generation by technology. In states with positive differences, we will then read in the differences
    # as fixed output for "_2015" technologies in corresponding subsectors.
    # We'll keep the fixed outputs constant over time.
    L223.StubTechProd_elec_USA %>%
      filter(year == max(BASE_YEARS),
             stub.technology %in% unique(L2243.ren_gen_2015$stub.technology)) %>%
      select(region, stub.technology, gen2010 = calOutputValue) %>%
      # need a right join here because LHS does not have rooftop_pv but RHS does
      right_join(L2243.ren_gen_2015 %>%
                   select(-year),
                 by = c("region" = "state", "stub.technology")) %>%
      replace(is.na(.), 0) %>%
      mutate(fixedOutput = gen2015 - gen2010) %>%
      mutate(fixedOutput = if_else(fixedOutput < 0, 0, fixedOutput)) %>%
      left_join_error_no_match(A23.elec_tech_ren_soft_cal, by = c("stub.technology" = "technology")) %>%
      select(region, supplysector, subsector, stub.technology = new.technology, fixedOutput) %>%
      repeat_add_columns(tibble::tibble(year = FUTURE_YEARS)) %>%
      # NOTE:  read in share-weight year as 1975 to avoid over-writing subsector shareweights that are read in for the electricity sector
      mutate(share.weight.year = min(BASE_YEARS),
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES$StubTechFixOut) %>%
      arrange(region, year) -> L2243.StubTechFixOut_ren_soft_cal_2015_USA

    # Create table to read in share-weights in global technology database
    A23.elec_tech_ren_soft_cal %>%
      select(sector.name = supplysector, subsector.name = subsector, technology = new.technology) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES$GlobalTechShrwt) -> L2243.GlobalTechShrwt_ren_soft_cal_2015_USA

    # Create table to read in Primary renewable Keyword for the new technologies
    L2243.GlobalTechShrwt_ren_soft_cal_2015_USA %>%
      distinct(sector.name, subsector.name, technology, year) %>%
      mutate(primary.renewable = paste(subsector.name, "-elect", sep = ""),
             primary.renewable = if_else(subsector.name == "rooftop_pv", "solar-elect", primary.renewable)) %>%
      select(LEVEL2_DATA_NAMES$PrimaryRenewKeyword) -> L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA


    # ===================================================
    # Produce outputs

    L2243.StubTechFixOut_ren_soft_cal_2015_USA %>%
      add_title("Fixed Outputs to Soft Calibrate State-Level Renewable Electricity Generation in 2015") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Based on EIA historical data for 2015") %>%
      add_comments("Fixed outputs are held constant over time") %>%
      add_comments("Share-weight year set as 1975 to avoid over-writing electricity subsector shareweights") %>%
      add_legacy_name("L2243.StubTechFixOut_ren_soft_cal_2015") %>%
      add_precursors('gcam-usa/states_subregions',
                     'gcam-usa/A23.elec_tech_associations_ren_soft_cal',
                     'gcam-usa/EIA_net_gen_renewables',
                     'gcam-usa/EIA_tech_mapping',
                     'L223.StubTechProd_elec_USA') ->
      L2243.StubTechFixOut_ren_soft_cal_2015_USA

    L2243.GlobalTechShrwt_ren_soft_cal_2015_USA %>%
      add_title("Technology Shareweights for Renewable Soft Calibration Technologies") %>%
      add_units("unitless") %>%
      add_comments("Share weights are zero in every model period as new investment in these soft calibration technologies is not permitted") %>%
      add_legacy_name("L2243.GlobalTechShrwt_ren_soft_cal_2015") %>%
      add_precursors('gcam-usa/A23.elec_tech_associations_ren_soft_cal') ->
      L2243.GlobalTechShrwt_ren_soft_cal_2015_USA

    L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA %>%
      add_title("Primary Renewable Keywords for Renewable Soft Calibration Technologies") %>%
      add_units("NA") %>%
      add_comments("Primary renewable keywords for renewable soft calibration technologies") %>%
      add_legacy_name("L2243.PrimaryRenewKeyword_ren_soft_cal_2015") %>%
      add_precursors('gcam-usa/states_subregions',
                     'gcam-usa/A23.elec_tech_associations_ren_soft_cal',
                     'gcam-usa/EIA_net_gen_renewables',
                     'gcam-usa/EIA_tech_mapping',
                     'L223.StubTechProd_elec_USA') ->
      L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA


    return_data(L2243.StubTechFixOut_ren_soft_cal_2015_USA,
                L2243.GlobalTechShrwt_ren_soft_cal_2015_USA,
                L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA)

  } else {
    stop("Unknown command")
  }
}
