#' module_gcamusa_batch_socioeconomics_udpate_USA_xml
#'
#' Construct XML data structure for \code{socioeconomics_update_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_update_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA_update.xml} (gcamusa XML).
module_gcamusa_batch_socioeconomics_udpate_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2011.Pop_updated_USA",
             "L2011.BaseGDP_updated_USA",
             "L2011.LaborProductivity_updated_USA",
             "L2011.Pop_national_updated_USA",
             "L2011.BaseGDP_national_updated_USA",
             "L2011.LaborProductivity_national_updated_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_update_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2011.Pop_updated_USA <- get_data(all_data, "L2011.Pop_updated_USA")
    L2011.BaseGDP_updated_USA <- get_data(all_data, "L2011.BaseGDP_updated_USA")
    L2011.LaborProductivity_updated_USA <- get_data(all_data, "L2011.LaborProductivity_updated_USA")
    L2011.Pop_national_updated_USA <- get_data(all_data, "L2011.Pop_national_updated_USA")
    L2011.BaseGDP_national_updated_USA <- get_data(all_data, "L2011.BaseGDP_national_updated_USA")
    L2011.LaborProductivity_national_updated_USA <- get_data(all_data, "L2011.LaborProductivity_national_updated_USA")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_update_USA.xml") %>%
      add_xml_data(L2011.Pop_updated_USA, "Pop") %>%
      add_xml_data(L2011.BaseGDP_updated_USA, "BaseGDP") %>%
      add_xml_data(L2011.LaborProductivity_updated_USA, "LaborProductivity") %>%
      add_xml_data(L2011.Pop_national_updated_USA, "Pop") %>%
      add_xml_data(L2011.BaseGDP_national_updated_USA, "BaseGDP") %>%
      add_xml_data(L2011.LaborProductivity_national_updated_USA, "LaborProductivity") %>%
      add_precursors("L2011.Pop_updated_USA",
                     "L2011.BaseGDP_updated_USA",
                     "L2011.LaborProductivity_updated_USA",
                     "L2011.Pop_national_updated_USA",
                     "L2011.BaseGDP_national_updated_USA",
                     "L2011.LaborProductivity_national_updated_USA") ->
      socioeconomics_update_USA.xml

    return_data(socioeconomics_update_USA.xml)
  } else {
    stop("Unknown command")
  }
}
