# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_solar_adv_xml
#'
#' Construct XML data structure for \code{solar_adv.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{solar_adv.xml}. The corresponding file in the
#' original data system was \code{batch_solar_adv_xml.R} (energy XML).
module_energy_solar_adv_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.GlobalTechCapital_sol_adv",
              "L223.GlobalIntTechCapital_sol_adv",
             # for adjusting rooftop_pv for capital tracking purposes
             "L223.StubTechCapFactor_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "solar_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_sol_adv <- get_data(all_data, "L223.GlobalTechCapital_sol_adv")
    L223.GlobalIntTechCapital_sol_adv <- get_data(all_data, "L223.GlobalIntTechCapital_sol_adv")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")

    # need to convert to standard non energy input for rooftop_pv for capital tracking purposes
    L223.StubTechCapFactor_elec %>%
      filter(stub.technology == "rooftop_pv") %>%
      left_join_error_no_match(L223.GlobalIntTechCapital_sol_adv, by=c("supplysector" = "sector.name",
                                                                     "subsector" = "subsector.name",
                                                                     "stub.technology" = "intermittent.technology",
                                                                     "year")) %>%
      mutate(input.cost = capital.overnight * fixed.charge.rate / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ)) %>%
      select(-capacity.factor, -capital.overnight, -fixed.charge.rate) %>%
      rename(minicam.non.energy.input = input.capital) ->
      rooftop_pv_adv
    L223.GlobalIntTechCapital_sol_adv %>%
      filter(subsector.name != "rooftop_pv") ->
      L223.GlobalIntTechCapital_sol_adv

    # ===================================================

    # Produce outputs
    create_xml("solar_adv.xml") %>%
      add_xml_data(L223.GlobalTechCapital_sol_adv, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_sol_adv, "GlobalIntTechCapital") %>%
      add_xml_data(rooftop_pv_adv, "StubTechCost") %>%
      add_precursors("L223.GlobalTechCapital_sol_adv", "L223.GlobalIntTechCapital_sol_adv",
                     "L223.StubTechCapFactor_elec") ->
      solar_adv.xml

    return_data(solar_adv.xml)
  } else {
    stop("Unknown command")
  }
}
