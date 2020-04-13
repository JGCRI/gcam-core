# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2242.elec_hydro_USA
#'
#' Update hydro-electricity fixed output to match historical data for 2015 and AEO-2018 for the future.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2242.StubTechFixOut_hydro_USA}.
#' The corresponding file in the original data system was \code{L2242.elec_hydro_USA.R} (gcam-usa level2).
#' @details Update state-level hydro-electricity fixed outputs
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MTB September 2018
module_gcamusa_L2242.elec_hydro_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/EIA_elec_gen_hydro',
             FILE = 'gcam-usa/AEO_2018_elec_gen_hydro',
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = 'gcam-usa/AEO_2020_elec_gen_hydro',
             'L2234.StubTechFixOut_elecS_USA'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2242.StubTechFixOut_hydro_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    EIA_elec_gen_hydro <- get_data(all_data, 'gcam-usa/EIA_elec_gen_hydro')
    AEO_2018_elec_gen_hydro <- get_data(all_data, 'gcam-usa/AEO_2018_elec_gen_hydro')
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    AEO_2020_elec_gen_hydro <- get_data(all_data, 'gcam-usa/AEO_2020_elec_gen_hydro')
    L2234.StubTechFixOut_elecS_USA <- get_data(all_data, 'L2234.StubTechFixOut_elecS_USA')

    # Silence package checks
    subsector <- year <- fixedOutput <- state <- EIA <- EIA_ratio <- fixedOutput_2015 <-
      AEO <- AEO_2015_ratio <- region <- supplysector <- stub.technology <-
      share.weight.year <- subs.share.weight <- tech.share.weight <- NULL

    # ===================================================
    # Data Processing

    # Isolate GCAM 2010 hydro fixedOutput
    L2234.StubTechFixOut_elecS_USA %>%
      filter(subsector == "hydro",
             year == max(year),
             fixedOutput != 0) -> L2242.hydro_2010_fixedOutput

    # Filter EIA data to get annual hydro net generation for 2010, 2015
    # Compute ratio of 2015 EIA to 2010 EIA
    EIA_elec_gen_hydro %>%
      filter(year %in% MODEL_YEARS) %>%
      group_by(state) %>%
      mutate(EIA_ratio =  EIA / lag(EIA)) %>%
      filter(year == gcamusa.HYDRO_HIST_YEAR) %>%
      distinct(state, EIA_ratio) %>%
      ungroup() -> L2242.hydro_EIA_ratio

    # Apply the ratio to GCAM 2010 fixed output to calculate 2015 fixed output
    L2242.hydro_2010_fixedOutput %>%
      left_join_error_no_match(L2242.hydro_EIA_ratio, by = c("region" = "state")) %>%
      mutate(fixedOutput = fixedOutput * EIA_ratio,
             year = gcamusa.HYDRO_HIST_YEAR,
             share.weight.year = year) %>%
      select(-EIA_ratio) -> L2242.hydro_fixedOutput_2015

    # Compute ratio of AEO-2018 hydro generation relative to EIA 2015 at the national level
    EIA_elec_gen_hydro %>%
      filter(year == gcamusa.HYDRO_HIST_YEAR) %>%
      group_by(year) %>%
      summarise(hydro_EIA_US_2015 = sum(EIA) * CONV_MWH_EJ) %>%
      ungroup() -> L2242.hydro_EIA_US_2015
    L2242.hydro_EIA_US_2015 <- unique(L2242.hydro_EIA_US_2015$hydro_EIA_US_2015)

    # Apply the national level ratio of AEO future years to 2015 to GCAM fixed output and create table through 2050.
    # The same ratio is assumed for all states
    AEO_2020_elec_gen_hydro %>%
      mutate(AEO_2015_ratio = (AEO * CONV_TWH_EJ) / L2242.hydro_EIA_US_2015) %>%
      filter(year %in% MODEL_YEARS,
             year >= gcamusa.HYDRO_HIST_YEAR) %>%
      select(year, AEO_2015_ratio) %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
      # filtering out states with no hydro generation
      semi_join(L2242.hydro_2010_fixedOutput, by = c("region")) %>%
      left_join_error_no_match(L2242.hydro_fixedOutput_2015 %>%
                                 rename(fixedOutput_2015 = fixedOutput) %>%
                                 select(-year),
                               by = c("region")) %>%
      mutate(fixedOutput = fixedOutput_2015 * AEO_2015_ratio,
             share.weight.year = year) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput,
             share.weight.year, subs.share.weight, tech.share.weight) %>%
      bind_rows(L2242.hydro_fixedOutput_2015) %>%
      arrange(region, year) -> L2242.StubTechFixOut_hydro_USA_2050

    # Copy 2050 values for the remaining years
    L2242.StubTechFixOut_hydro_USA_2050 %>%
      complete(year = MODEL_FUTURE_YEARS, nesting(region, supplysector, subsector, stub.technology,
                                            subs.share.weight, tech.share.weight)) %>%
      filter(year >= gcamusa.HYDRO_HIST_YEAR) %>%
      group_by(region, supplysector, subsector, stub.technology) %>%
      mutate(fixedOutput = replace(fixedOutput, year > gcamusa.HYDRO_FINAL_AEO_YEAR,
                                   fixedOutput[year == gcamusa.HYDRO_FINAL_AEO_YEAR])) %>%
      ungroup() %>%
      mutate(share.weight.year = year,
             fixedOutput = round(fixedOutput, energy.DIGITS_CALOUTPUT)) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput,
             share.weight.year, subs.share.weight, tech.share.weight) %>%
      arrange(region, year) -> L2242.StubTechFixOut_hydro_USA

    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs
    add_cooling_techs <- function(data){
      data_new <- data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology)%>%
        arrange(region,year)
      return(data_new)
    }
      L2242.StubTechFixOut_hydro_USA <- add_cooling_techs(L2242.StubTechFixOut_hydro_USA)

    # ===================================================
    # Produce outputs

    L2242.StubTechFixOut_hydro_USA %>%
      add_title("Updated Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Future year fixed outputs for hydro electricity load segments technologies") %>%
      add_comments("2015 values based on EIA historical data") %>%
      add_comments("Post-2015 values based on USA-level hydro electricity growth from AEO-2018") %>%
      add_legacy_name("L2242.StubTechFixOut_hydro_USA") %>%
      add_precursors('gcam-usa/EIA_elec_gen_hydro',
                     'gcam-usa/AEO_2018_elec_gen_hydro',
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     'gcam-usa/AEO_2020_elec_gen_hydro',
                     'L2234.StubTechFixOut_elecS_USA') ->
      L2242.StubTechFixOut_hydro_USA


    return_data(L2242.StubTechFixOut_hydro_USA)

  } else {
    stop("Unknown command")
  }
}
