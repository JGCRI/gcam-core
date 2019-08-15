#' module_gcamusa_batch_water_mapping_xml
#'
#' Construct XML data structure for \code{water_mapping.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_mapping.xml}. The corresponding file in the
#' original data system was \code{batch_water_mapping.xml.R} (water XML).
module_gcamusa_batch_water_mapping_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector_USA",
             "L203.SubsectorLogit_USA",
             "L203.SubsectorShrwtFllt_USA",
             "L203.TechShrwt_USA",
             "L203.TechCoef_USA",
             "L203.DeleteSubsector_USAls"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_mapping_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector_USA <- get_data(all_data, "L203.Supplysector_USA")
    L203.SubsectorLogit_USA <- get_data(all_data, "L203.SubsectorLogit_USA")
    L203.SubsectorShrwtFllt_USA <- get_data(all_data, "L203.SubsectorShrwtFllt_USA")
    L203.TechShrwt_USA <- get_data(all_data, "L203.TechShrwt_USA")
    L203.TechCoef_USA <- get_data(all_data, "L203.TechCoef_USA")
    L203.DeleteSubsector_USAls <- get_data(all_data, "L203.DeleteSubsector_USAls")

    # ===================================================

    # Produce outputs
    create_xml("water_mapping_USA.xml") %>%
      add_logit_tables_xml(L203.Supplysector_USA, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_USA, "SubsectorLogit") %>%
      add_xml_data(L203.DeleteSubsector_USAls, "DeleteSubsector") %>%
      add_xml_data(L203.SubsectorShrwtFllt_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L203.TechShrwt_USA, "TechShrwt") %>%
      add_xml_data(L203.TechCoef_USA, "TechCoef") %>%
      add_precursors("L203.Supplysector_USA", "L203.SubsectorLogit_USA", "L203.DeleteSubsector_USAls", "L203.SubsectorShrwtFllt_USA", "L203.TechShrwt_USA", "L203.TechCoef_USA") ->
      water_mapping_USA.xml

    return_data(water_mapping_USA.xml)
  } else {
    stop("Unknown command")
  }
}
