#' module_gcamusa_batch_elecS_costs_USA_xml
#'
#' Construct XML data structure for \code{elecS_costs_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elecS_costs_USA.xml}. The corresponding file in the
#' original data system was \code{batch_elecS_costs_USA.xml} (gcamusa xml-batch).
module_gcamusa_batch_elecS_costs_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2247.GlobalTechCapitalOnly_elecS_USA",
             "L2247.GlobalIntTechCapitalOnly_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_costs_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2247.GlobalTechCapitalOnly_elecS_USA <- get_data(all_data, "L2247.GlobalTechCapitalOnly_elecS_USA")
    L2247.GlobalIntTechCapitalOnly_elecS_USA <- get_data(all_data, "L2247.GlobalIntTechCapitalOnly_elecS_USA")

    # Produce outputs
    create_xml("elecS_costs_USA.xml") %>%
      add_xml_data(L2247.GlobalTechCapitalOnly_elecS_USA, "GlobalTechCapitalOnly") %>%
      add_xml_data(L2247.GlobalIntTechCapitalOnly_elecS_USA, "GlobalIntTechCapitalOnly") %>%
      add_precursors("L2247.GlobalTechCapitalOnly_elecS_USA",
                     "L2247.GlobalIntTechCapitalOnly_elecS_USA") ->
      elecS_costs_USA.xml

    return_data(elecS_costs_USA.xml)
  } else {
    stop("Unknown command")
  }
}
