# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_ind_urb_proc_emissions_xml
#'
#' Construct XML data structure for \code{ind_urb_proc_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ind_urb_proc_emissions_USA.xml}, \code{ind_urb_proc_ghg_emissions_USA.xml}.
module_gcamusa_ind_urb_proc_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L277.nonghg_prc_USA",
             "L277.nonghg_max_reduction_USA",
             "L277.nonghg_steepness_USA",
             "L277.ghg_prc_USA",
             "L277.MAC_prc_USA",
             "L277.MAC_prc_tc_average_USA",
             "L277.MAC_prc_phaseInTime_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ind_urb_proc_emissions_USA.xml",
             XML = "ind_urb_proc_ghg_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L277.nonghg_prc_USA <- get_data(all_data, "L277.nonghg_prc_USA")
    L277.nonghg_max_reduction_USA <- get_data(all_data, "L277.nonghg_max_reduction_USA")
    L277.nonghg_steepness_USA <- get_data(all_data, "L277.nonghg_steepness_USA")
    L277.ghg_prc_USA <- get_data(all_data, "L277.ghg_prc_USA")
    L277.MAC_prc_USA <- get_data(all_data, "L277.MAC_prc_USA")
    L277.MAC_prc_tc_average_USA <- get_data(all_data, "L277.MAC_prc_tc_average_USA")
    L277.MAC_prc_phaseInTime_USA <- get_data(all_data, "L277.MAC_prc_phaseInTime_USA")

    # ===================================================
    # Produce outputs
    create_xml("ind_urb_proc_emissions_USA.xml") %>%
      add_xml_data(L277.nonghg_prc_USA, "StbTechOutputEmissions") %>%
      add_xml_data(L277.nonghg_max_reduction_USA, "GDPCtrlMax") %>%
      add_xml_data(L277.nonghg_steepness_USA, "GDPCtrlSteep") %>%
      add_precursors("L277.nonghg_prc_USA",
                     "L277.nonghg_max_reduction_USA",
                     "L277.nonghg_steepness_USA") ->
      ind_urb_proc_emissions_USA.xml

    create_xml("ind_urb_proc_ghg_emissions_USA.xml") %>%
      add_xml_data(L277.ghg_prc_USA, "StbTechOutputEmissions") %>%
      add_xml_data(L277.MAC_prc_USA, "MAC") %>%
      add_xml_data(L277.MAC_prc_tc_average_USA, "MACTC") %>%
      add_xml_data(L277.MAC_prc_phaseInTime_USA, "MACPhaseIn") %>%
      add_precursors("L277.ghg_prc_USA",
                     "L277.MAC_prc_USA",
                     "L277.MAC_prc_tc_average_USA",
                     "L277.MAC_prc_phaseInTime_USA") ->
      ind_urb_proc_ghg_emissions_USA.xml

    return_data(ind_urb_proc_emissions_USA.xml,
                ind_urb_proc_ghg_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
