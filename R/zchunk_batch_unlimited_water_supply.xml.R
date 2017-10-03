#' module_water_batch_unlimited_water_supply.xml
#'
#' Construct XML data structure for \code{unlimited_water_supply.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{unlimited_water_supply.xml}. The corresponding file in the
#' original data system was \code{batch_unlimited_water_supply.xml.R} (water XML).
module_water_batch_unlimited_water_supply.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L202.UnlimitRsrc",
              "L202.UnlimitRsrcPrice"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "unlimited_water_supply.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L202.UnlimitRsrc <- get_data(all_data, "L202.UnlimitRsrc")
    L202.UnlimitRsrcPrice <- get_data(all_data, "L202.UnlimitRsrcPrice")

    # ===================================================

    # Produce outputs
    create_xml("unlimited_water_supply.xml") %>%
      add_xml_data(L202.UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L202.UnlimitRsrcPrice, "UnlimitRsrcPrice") %>%
      add_precursors("L202.UnlimitRsrc", "L202.UnlimitRsrcPrice") ->
      unlimited_water_supply.xml

    return_data(unlimited_water_supply.xml)
  } else {
    stop("Unknown command")
  }
}
