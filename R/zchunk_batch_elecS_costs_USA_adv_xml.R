# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_elecS_costs_USA_adv_xml
#'
#' Construct XML data structure for \code{elecS_costs_USA_adv.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elecS_costs_USA_adv.xml}. The corresponding file in the
#' original data system was \code{batch_elecS_costs_USA_adv.xml} (gcamusa xml-batch).
module_gcamusa_batch_elecS_costs_USA_adv_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2247.GlobalTechCapitalOnly_elecS_adv_USA",
             "L2247.GlobalIntTechCapitalOnly_elecS_adv_USA",
             "L2247.GlobalTechOMfixedOnly_elecS_adv_USA",
             "L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA",
             "L2247.GlobalTechOMvar_elecS_adv_USA",
             "L2247.GlobalIntTechOMvar_elecS_adv_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_costs_USA_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2247.GlobalTechCapitalOnly_elecS_adv_USA <- get_data(all_data, "L2247.GlobalTechCapitalOnly_elecS_adv_USA")
    L2247.GlobalIntTechCapitalOnly_elecS_adv_USA <- get_data(all_data, "L2247.GlobalIntTechCapitalOnly_elecS_adv_USA")
    L2247.GlobalTechOMfixedOnly_elecS_adv_USA <- get_data(all_data, "L2247.GlobalTechOMfixedOnly_elecS_adv_USA")
    L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA <- get_data(all_data, "L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA")
    L2247.GlobalTechOMvar_elecS_adv_USA <- get_data(all_data, "L2247.GlobalTechOMvar_elecS_adv_USA")
    L2247.GlobalIntTechOMvar_elecS_adv_USA <- get_data(all_data, "L2247.GlobalIntTechOMvar_elecS_adv_USA")

    # Produce outputs
    create_xml("elecS_costs_USA_adv.xml") %>%
      add_xml_data(L2247.GlobalTechCapitalOnly_elecS_adv_USA, "GlobalTechCapitalOnly") %>%
      add_xml_data(L2247.GlobalIntTechCapitalOnly_elecS_adv_USA, "GlobalIntTechCapitalOnly") %>%
      add_xml_data(L2247.GlobalTechOMfixedOnly_elecS_adv_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2247.GlobalTechOMvar_elecS_adv_USA, "GlobalTechOMvar") %>%
      add_xml_data(L2247.GlobalIntTechOMvar_elecS_adv_USA, "GlobalIntTechOMvar") %>%
      add_precursors("L2247.GlobalTechCapitalOnly_elecS_adv_USA",
                     "L2247.GlobalIntTechCapitalOnly_elecS_adv_USA",
                     "L2247.GlobalTechOMfixedOnly_elecS_adv_USA",
                     "L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA",
                     "L2247.GlobalTechOMvar_elecS_adv_USA",
                     "L2247.GlobalIntTechOMvar_elecS_adv_USA") ->
      elecS_costs_USA_adv.xml

    return_data(elecS_costs_USA_adv.xml)
  } else {
    stop("Unknown command")
  }
}
