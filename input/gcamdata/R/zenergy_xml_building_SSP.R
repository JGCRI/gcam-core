# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_building_SSP_xml
#'
#' Construct XML data structure for all the \code{building_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_SSP1.xml}, \code{building_SSP2.xml}, \code{building_SSP3.xml},
#' \code{building_SSP4.xml},and \code{building_SSP5.xml}.
module_energy_building_SSP_xml <- function(command, ...) {
  SSP_NUMS <- seq(1, 5)
  fuelpref_NUMS <- c("1" = "15", "3" = "3", "4" = "4", "5" = "15")

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L244.Satiation_flsp_SSP", SSP_NUMS),
             paste0("L244.SatiationAdder_SSP", SSP_NUMS),
             paste0("L244.Satiation_impedance_SSP", SSP_NUMS),
             paste0("L244.SubregionalShares_SSP", SSP_NUMS)))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_SSP1.xml",
             XML = "building_SSP2.xml",
             XML = "building_SSP3.xml",
             XML = "building_SSP4.xml",
             XML = "building_SSP5.xml"))
  } else if(command == driver.MAKE) {

    # silence package checks
    building_SSP1.xml <- building_SSP2.xml <- building_SSP3.xml <- building_SSP4.xml <-
      building_SSP5.xml <- NULL

    all_data <- list(...)[[1]]

    for (i in SSP_NUMS) {
      # Names of inputs and outputs
      Satiation_flsp <- paste0("L244.Satiation_flsp_SSP", i)
      SatiationAdder <- paste0("L244.SatiationAdder_SSP", i)
      Satiation_impedance <- paste0("L244.Satiation_impedance_SSP", i)
      Subregional_shares <- paste0("L244.SubregionalShares_SSP", i)

      xmlfn <- paste0("building_SSP", i, ".xml")

      # Load required inputs
      L244.Satiation_flsp_SSP <- get_data(all_data, Satiation_flsp)
      L244.SatiationAdder_SSP <- get_data(all_data, SatiationAdder)
      L244.Satiation_impedance_SSP <- get_data(all_data, Satiation_impedance)
      L244.SubregionalShares_SSP <- get_data(all_data, Subregional_shares)


      # Produce outputs
      if(i == 2) {
        create_xml(xmlfn) %>%
          add_xml_data(L244.Satiation_flsp_SSP, "Satiation_flsp") %>%
          add_xml_data(L244.SatiationAdder_SSP, "SatiationAdder") %>%
          add_xml_data(L244.Satiation_impedance_SSP, "SatiationImpedance") %>%
          add_xml_data(L244.SubregionalShares_SSP, "SubregionalShares") %>%
          add_precursors(Satiation_flsp, SatiationAdder,Satiation_impedance,Subregional_shares) -> xml_obj

      } else {
        create_xml(xmlfn) %>%
          add_xml_data(L244.Satiation_flsp_SSP, "Satiation_flsp") %>%
          add_xml_data(L244.SatiationAdder_SSP, "SatiationAdder") %>%
          add_xml_data(L244.Satiation_impedance_SSP, "SatiationImpedance") %>%
          add_xml_data(L244.SubregionalShares_SSP, "SubregionalShares") %>%
          add_precursors(Satiation_flsp, SatiationAdder,Satiation_impedance,Subregional_shares) -> xml_obj
      }

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(building_SSP1.xml, building_SSP2.xml, building_SSP3.xml, building_SSP4.xml,
                building_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
