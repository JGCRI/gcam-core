# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_food_SSP4_xml
#'
#' Construct XML data structure for \code{food_SSP4.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_SSP4.xml}. The corresponding file in the
#' original data system was \code{batch_food_SSP4_xml.R} (aglu XML).
module_aglu_batch_food_SSP4_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.IncomeElasticity_SSP4"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_SSP4.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.IncomeElasticity_SSP4 <- get_data(all_data, "L203.IncomeElasticity_SSP4")

    # ===================================================

    # Produce outputs
    create_xml("food_SSP4.xml") %>%
      add_xml_data(L203.IncomeElasticity_SSP4, "IncomeElasticity") %>%
      add_precursors("L203.IncomeElasticity_SSP4") ->
      food_SSP4.xml

    return_data(food_SSP4.xml)
  } else {
    stop("Unknown command")
  }
}
