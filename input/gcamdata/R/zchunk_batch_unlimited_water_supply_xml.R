# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_unlimited_water_supply_xml
#'
#' Construct XML data structure for \code{unlimited_water_supply.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{unlimited_water_supply.xml}. The corresponding file in the
#' original data system was \code{batch_unlimited_water_supply.xml.R} (water XML).
module_water_batch_unlimited_water_supply_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L202.UnlimitRsrc_mapped",
             "L202.Rsrc_mapped",
             "L202.UnlimitRsrc_nonmapped",
             "L202.UnlimitRsrcPrice_mapped",
             "L202.UnlimitRsrcPrice_nonmapped"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "unlimited_water_supply.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L202.UnlimitRsrc_mapped <- get_data(all_data, "L202.UnlimitRsrc_mapped")
    L202.Rsrc_mapped <- get_data(all_data, "L202.Rsrc_mapped")
    L202.UnlimitRsrc_nonmapped <- get_data(all_data, "L202.UnlimitRsrc_nonmapped")
    L202.UnlimitRsrcPrice_mapped <- get_data(all_data, "L202.UnlimitRsrcPrice_mapped")
    L202.UnlimitRsrcPrice_nonmapped <- get_data(all_data, "L202.UnlimitRsrcPrice_nonmapped")


    # ===================================================

    # Produce outputs
    create_xml("unlimited_water_supply.xml") %>%
      add_xml_data(L202.UnlimitRsrc_mapped, "UnlimitRsrc") %>%
      add_xml_data(L202.Rsrc_mapped, "Rsrc") %>%
      add_xml_data(L202.UnlimitRsrc_nonmapped, "UnlimitRsrc") %>%
      add_xml_data(L202.UnlimitRsrcPrice_mapped, "UnlimitRsrcPrice") %>%
      add_xml_data(L202.UnlimitRsrcPrice_nonmapped, "UnlimitRsrcPrice") %>%
      add_precursors("L202.UnlimitRsrc_mapped", "L202.Rsrc_mapped", "L202.UnlimitRsrc_nonmapped",
                     "L202.UnlimitRsrcPrice_mapped", "L202.UnlimitRsrcPrice_nonmapped") ->
      unlimited_water_supply.xml

    return_data(unlimited_water_supply.xml)
  } else {
    stop("Unknown command")
  }
}
