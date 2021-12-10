# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_building_SSP_xml
#'
#' Construct XML data structure for all the \code{building_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_SSP1.xml}, \code{building_SSP2.xml}, \code{building_SSP3.xml},
#' \code{building_SSP4.xml},and \code{building_SSP5.xml}.
module_energy_batch_building_SSP_xml <- function(command, ...) {
  SSP_NUMS <- seq(1, 5)
  fuelpref_NUMS <- c("1" = "15", "3" = "3", "4" = "4", "5" = "15")

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L244.Satiation_flsp_SSP", SSP_NUMS),
             paste0("L244.SatiationAdder_SSP", SSP_NUMS),
             paste0("L244.GenericServiceSatiation_SSP", SSP_NUMS),
             paste0("L244.FuelPrefElast_bld_SSP", unique(fuelpref_NUMS)),
             "L244.DeleteThermalService"))
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
      GenericServiceSatiation <- paste0("L244.GenericServiceSatiation_SSP", i)

      # SSP2 is unique, uses fewer files
      if(i != 2) {
        FuelPrefElast <- paste0("L244.FuelPrefElast_bld_SSP", fuelpref_NUMS[toString(i)])
      }
      xmlfn <- paste0("building_SSP", i, ".xml")

      # Load required inputs
      L244.Satiation_flsp_SSP <- get_data(all_data, Satiation_flsp)
      L244.SatiationAdder_SSP <- get_data(all_data, SatiationAdder)
      L244.GenericServiceSatiation_SSP <- get_data(all_data, GenericServiceSatiation)
      if(i != 2) {
        L244.FuelPrefElast_SSP <- get_data(all_data, FuelPrefElast)
        L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService")
      }

      # Produce outputs
      if(i == 2) {
        create_xml(xmlfn) %>%
          add_xml_data(L244.Satiation_flsp_SSP, "Satiation_flsp") %>%
          add_xml_data(L244.SatiationAdder_SSP, "SatiationAdder") %>%
          add_xml_data(L244.GenericServiceSatiation_SSP, "GenericServiceSatiation") %>%
          add_precursors(Satiation_flsp, SatiationAdder,GenericServiceSatiation) ->
          xml_obj
      } else {
        create_xml(xmlfn) %>%
          add_xml_data(L244.Satiation_flsp_SSP, "Satiation_flsp") %>%
          add_xml_data(L244.SatiationAdder_SSP, "SatiationAdder") %>%
          add_xml_data(L244.GenericServiceSatiation_SSP, "GenericServiceSatiation") %>%
          add_xml_data(L244.FuelPrefElast_SSP, "FuelPrefElast") %>%
          add_xml_data(L244.DeleteThermalService, "DeleteThermalService") %>%
          add_precursors(Satiation_flsp, SatiationAdder, GenericServiceSatiation, FuelPrefElast,
                         "L244.DeleteThermalService") ->
          xml_obj
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
