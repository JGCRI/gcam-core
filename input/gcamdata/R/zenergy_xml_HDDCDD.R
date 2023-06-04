# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_HDDCDD_xml
#'
#' Construct XML data structure for all the \code{HDDCDD.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_A2_CCSM3x.xml}, \code{HDDCDD_A2_HadCM3.xml}, \code{HDDCDD_B1_CCSM3x.xml},
#' \code{HDDCDD_B1_HadCM3.xml}, and \code{HDDCDD_constdd_no_GCM.xml}.
module_energy_HDDCDD_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_A2_CCSM3x",
             "L244.HDDCDD_A2_HadCM3",
             "L244.HDDCDD_B1_CCSM3x",
             "L244.HDDCDD_B1_HadCM3",
             "L244.HDDCDD_constdd_no_GCM"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_A2_CCSM3x.xml",
             XML = "HDDCDD_A2_HadCM3.xml",
             XML = "HDDCDD_B1_CCSM3x.xml",
             XML = "HDDCDD_B1_HadCM3.xml",
             XML = "HDDCDD_constdd_no_GCM.xml"))
  } else if(command == driver.MAKE) {

    # silence package checks
    HDDCDD_A2_CCSM3x.xml <- HDDCDD_A2_HadCM3.xml <- HDDCDD_B1_CCSM3x.xml <- HDDCDD_B1_HadCM3.xml <-
      HDDCDD_constdd_no_GCM.xml <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_A2_CCSM3x <- get_data(all_data, "L244.HDDCDD_A2_CCSM3x")
    L244.HDDCDD_A2_HadCM3 <- get_data(all_data, "L244.HDDCDD_A2_HadCM3")
    L244.HDDCDD_B1_CCSM3x <- get_data(all_data, "L244.HDDCDD_B1_CCSM3x")
    L244.HDDCDD_B1_HadCM3 <- get_data(all_data, "L244.HDDCDD_B1_HadCM3")
    L244.HDDCDD_constdd_no_GCM <- get_data(all_data, "L244.HDDCDD_constdd_no_GCM")

      # Produce outputs
    create_xml("HDDCDD_A2_CCSM3x.xml") %>%
      add_xml_data(L244.HDDCDD_A2_CCSM3x, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_CCSM3x") ->
      HDDCDD_A2_CCSM3x.xml

    create_xml("HDDCDD_A2_HadCM3.xml") %>%
      add_xml_data(L244.HDDCDD_A2_HadCM3, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_HadCM3") ->
      HDDCDD_A2_HadCM3.xml

    create_xml("HDDCDD_B1_CCSM3x.xml") %>%
      add_xml_data(L244.HDDCDD_B1_CCSM3x, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_B1_CCSM3x") ->
      HDDCDD_B1_CCSM3x.xml

    create_xml("HDDCDD_B1_HadCM3.xml") %>%
      add_xml_data(L244.HDDCDD_B1_HadCM3, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_B1_HadCM3") ->
      HDDCDD_B1_HadCM3.xml

    create_xml("HDDCDD_constdd_no_GCM.xml") %>%
      add_xml_data(L244.HDDCDD_constdd_no_GCM, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_constdd_no_GCM") ->
      HDDCDD_constdd_no_GCM.xml

    return_data(HDDCDD_A2_CCSM3x.xml, HDDCDD_A2_HadCM3.xml, HDDCDD_B1_CCSM3x.xml, HDDCDD_B1_HadCM3.xml,
                HDDCDD_constdd_no_GCM.xml)
  } else {
    stop("Unknown command")
  }
}
