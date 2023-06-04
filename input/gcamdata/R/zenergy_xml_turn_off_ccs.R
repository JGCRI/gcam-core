# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_turn_off_ccs_xml
#'
#' Construct XML data structure for \code{turn_off_ccs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{turn_off_ccs.xml}.
#' @author EL 2022
module_energy_turn_off_ccs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L222.GlobalTechCapture_en",
             "L223.GlobalTechCapture_elec",
             "L225.GlobalTechCapture_h2",
             "L2321.GlobalTechCapture_cement",
             "L2322.GlobalTechCapture_Fert",
             "L2323.GlobalTechCapture_iron_steel",
             "L2325.GlobalTechCapture_chemical",
             'L2326.GlobalTechCapture_aluminum',
             "L262.GlobalTechCapture_dac"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "turn_off_ccs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load data
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en")
    L223.GlobalTechCapture_elec <- get_data(all_data, "L223.GlobalTechCapture_elec")
    L225.GlobalTechCapture_h2 <- get_data(all_data, "L225.GlobalTechCapture_h2")
    L2321.GlobalTechCapture_cement <- get_data(all_data, "L2321.GlobalTechCapture_cement")
    L2322.GlobalTechCapture_Fert <- get_data(all_data, "L2322.GlobalTechCapture_Fert")
    L2323.GlobalTechCapture_iron_steel <- get_data(all_data, "L2323.GlobalTechCapture_iron_steel")
    L2325.GlobalTechCapture_chemical <- get_data(all_data, "L2325.GlobalTechCapture_chemical")
    L2326.GlobalTechCapture_aluminum <- get_data(all_data, "L2326.GlobalTechCapture_aluminum")
    L262.GlobalTechCapture_dac <- get_data(all_data, "L262.GlobalTechCapture_dac")

    # Get all CCS technologies for each region in one dataframe
    CCS_Techs <- bind_rows(L222.GlobalTechCapture_en, L223.GlobalTechCapture_elec,
                           L225.GlobalTechCapture_h2, L2321.GlobalTechCapture_cement,
                           L2322.GlobalTechCapture_Fert, L2323.GlobalTechCapture_iron_steel,
                           L2325.GlobalTechCapture_chemical, L2326.GlobalTechCapture_aluminum,
                           L262.GlobalTechCapture_dac) %>%
      select(sector.name, subsector.name, technology) %>%
      unique() %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      full_join(select(GCAM_region_names, region), by = character())

    # Set share weights to 0
    CCS_Techs %>%
      mutate(share.weight = 0,
             year = min(MODEL_FUTURE_YEARS)) -> no_ccs_sw

    # Filter into DAC and no DAC tables so we can use no-creates
    no_ccs_sw %>%
      filter(supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_sw_dac

    no_ccs_sw %>%
      filter(!supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_sw

    # Set interpolation rules
    CCS_Techs %>%
      mutate(apply.to = "share-weight",
             from.year = min(MODEL_FUTURE_YEARS),
             to.year =  max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed") -> no_ccs_interp

    # Filter into DAC and no DAC tables so we can use no-creates
    no_ccs_interp %>%
      filter(supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_interp_dac

    no_ccs_interp %>%
      filter(!supplysector %in% c("process heat dac", "CO2 removal")) -> no_ccs_interp


    create_xml("turn_off_ccs.xml") %>%
      add_xml_data(no_ccs_sw, "StubTechShrwt") %>%
      add_xml_data(no_ccs_sw_dac, "StubTechShrwt_NC", "StubTechShrwt") %>%
      add_xml_data(no_ccs_interp, "StubTechInterp") %>%
      add_xml_data(no_ccs_interp_dac, "StubTechInterp_NC", "StubTechInterp") %>%
      add_precursors("common/GCAM_region_names",
                     "L222.GlobalTechCapture_en",
                     "L223.GlobalTechCapture_elec",
                     "L225.GlobalTechCapture_h2",
                     "L2321.GlobalTechCapture_cement",
                     "L2322.GlobalTechCapture_Fert",
                     "L2323.GlobalTechCapture_iron_steel",
                     "L2325.GlobalTechCapture_chemical",
                     'L2326.GlobalTechCapture_aluminum',
                     "L262.GlobalTechCapture_dac") ->
      turn_off_ccs.xml

    return_data(turn_off_ccs.xml)
  } else {
    stop("Unknown command")
  }
}
