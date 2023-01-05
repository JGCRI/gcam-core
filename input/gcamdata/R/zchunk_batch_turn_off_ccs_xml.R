# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_turn_off_ccs_xml
#'
#' Construct XML data structure for \code{turn_off_ccs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{turn_off_ccs.xml}.
#' @author EL 2022
module_energy_batch_turn_off_ccs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A22.globaltech_co2capture",
             FILE = "energy/A23.globaltech_co2capture",
             FILE = "energy/A25.globaltech_co2capture",
             FILE = "energy/A321.globaltech_co2capture",
             FILE = "energy/A322.globaltech_co2capture",
             FILE = "energy/A323.globaltech_co2capture",
             FILE = "energy/A325.globaltech_co2capture",
             FILE = "energy/A326.globaltech_co2capture",
             FILE = "energy/A62.globaltech_co2capture"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "turn_off_ccs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load data
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A22.globaltech_co2capture <- get_data(all_data, "energy/A22.globaltech_co2capture")
    A23.globaltech_co2capture <- get_data(all_data, "energy/A23.globaltech_co2capture")
    A25.globaltech_co2capture <- get_data(all_data, "energy/A25.globaltech_co2capture")
    A321.globaltech_co2capture <- get_data(all_data, "energy/A321.globaltech_co2capture")
    A322.globaltech_co2capture <- get_data(all_data, "energy/A322.globaltech_co2capture")
    A323.globaltech_co2capture <- get_data(all_data, "energy/A323.globaltech_co2capture")
    A325.globaltech_co2capture <- get_data(all_data, "energy/A325.globaltech_co2capture")
    A326.globaltech_co2capture <- get_data(all_data, "energy/A326.globaltech_co2capture")
    A62.globaltech_co2capture <- get_data(all_data, "energy/A62.globaltech_co2capture")

    # Shareweight No CCS Dataframe
    no_ccs_sw <- bind_rows(A22.globaltech_co2capture, A23.globaltech_co2capture,
                        A25.globaltech_co2capture, A321.globaltech_co2capture,
                        A322.globaltech_co2capture, A323.globaltech_co2capture,
                        A325.globaltech_co2capture, A326.globaltech_co2capture,
                        A62.globaltech_co2capture) %>%
      select(supplysector, subsector, technology) %>%
      mutate(share.weight = 0,
             year = min(MODEL_FUTURE_YEARS)) %>%
      rename(stub.technology = technology)%>%
      full_join(select(GCAM_region_names, region), by = character())

    # Filter into DAC and no DAC tables so we can use no-creates
    no_ccs_sw %>%
      filter(supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_sw_dac

    no_ccs_sw %>%
      filter(!supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_sw

    # Interp No CCS Dataframe
    no_ccs_interp <- bind_rows(A22.globaltech_co2capture, A23.globaltech_co2capture,
                        A25.globaltech_co2capture, A321.globaltech_co2capture,
                        A322.globaltech_co2capture, A323.globaltech_co2capture,
                        A325.globaltech_co2capture, A326.globaltech_co2capture,
                        A62.globaltech_co2capture) %>%
      select(supplysector, subsector, technology) %>%
      mutate(apply.to = "share-weight",
             from.year = min(MODEL_FUTURE_YEARS),
             to.year =  max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed") %>%
      rename(stub.technology = technology)%>%
      full_join(select(GCAM_region_names, region), by = character())

    # Filter into DAC and no DAC tables so we can use no-creates
    no_ccs_interp %>%
      filter(supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_interp_dac

    no_ccs_interp %>%
      filter(!supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_interp


    create_xml("turn_off_ccs.xml") %>%
      add_xml_data(no_ccs_sw, "StubTechShrwt",) %>%
      add_xml_data(no_ccs_sw_dac, "StubTechShrwt_NC",) %>%
      add_xml_data(no_ccs_interp, "StubTechInterp") %>%
      add_xml_data(no_ccs_interp_dac, "StubTechInterp_NC") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A22.globaltech_co2capture",
                     "energy/A23.globaltech_co2capture",
                     "energy/A25.globaltech_co2capture",
                     "energy/A321.globaltech_co2capture",
                     "energy/A322.globaltech_co2capture",
                     "energy/A323.globaltech_co2capture",
                     "energy/A325.globaltech_co2capture",
                     "energy/A326.globaltech_co2capture",
                     "energy/A62.globaltech_co2capture") ->
      turn_off_ccs.xml

    return_data(turn_off_ccs.xml)
  } else {
    stop("Unknown command")
  }
}
