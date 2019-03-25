#' module_gcamusa_batch_elecS_costs_USA_itc_xml
#'
#' Construct XML data structure for \code{elecS_costs_USA_itc.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elecS_costs_USA_itc.xml}. The corresponding file in the
#' original data system was \code{batch_elecS_costs_USA_itc.xml} (gcamusa xml-batch).
module_gcamusa_batch_elecS_costs_USA_itc_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2247.GlobalTechFCROnly_elecS_itc_USA",
             "L2247.GlobalIntTechFCROnly_elecS_itc_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_costs_USA_itc.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2247.GlobalTechFCROnly_elecS_itc_USA <- get_data(all_data, "L2247.GlobalTechFCROnly_elecS_itc_USA")
    L2247.GlobalIntTechFCROnly_elecS_itc_USA <- get_data(all_data, "L2247.GlobalIntTechFCROnly_elecS_itc_USA")

    # Produce outputs
    create_xml("elecS_costs_USA_itc.xml") %>%
      add_xml_data(L2247.GlobalTechFCROnly_elecS_itc_USA, "GlobalTechFCROnly") %>%
      add_xml_data(L2247.GlobalIntTechFCROnly_elecS_itc_USA, "GlobalIntTechFCROnly") %>%
      add_precursors("L2247.GlobalTechFCROnly_elecS_itc_USA",
                     "L2247.GlobalIntTechFCROnly_elecS_itc_USA") ->
      elecS_costs_USA_itc.xml

    return_data(elecS_costs_USA_itc.xml)
  } else {
    stop("Unknown command")
  }
}
