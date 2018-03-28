#' module_energy_batch_solar_adv_xml
#'
#' Construct XML data structure for \code{solar_adv.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{solar_adv.xml}. The corresponding file in the
#' original data system was \code{batch_solar_adv_xml.R} (energy XML).
module_energy_batch_solar_adv_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L223.GlobalTechCapital_sol_adv",
              "L223.GlobalIntTechCapital_sol_adv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "solar_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_sol_adv <- get_data(all_data, "L223.GlobalTechCapital_sol_adv")
    L223.GlobalIntTechCapital_sol_adv <- get_data(all_data, "L223.GlobalIntTechCapital_sol_adv")

    # ===================================================

    # Produce outputs
    create_xml("solar_adv.xml") %>%
      add_xml_data(L223.GlobalTechCapital_sol_adv,"GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_sol_adv,"GlobalIntTechCapital") %>%
      add_precursors("L223.GlobalTechCapital_sol_adv", "L223.GlobalIntTechCapital_sol_adv") ->
      solar_adv.xml

    return_data(solar_adv.xml)
  } else {
    stop("Unknown command")
  }
}
