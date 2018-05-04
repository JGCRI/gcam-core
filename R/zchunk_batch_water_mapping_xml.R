#' module_water_batch_water_mapping_xml
#'
#' Construct XML data structure for \code{water_mapping.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_mapping.xml}. The corresponding file in the
#' original data system was \code{batch_water_mapping.xml.R} (water XML).
module_water_batch_water_mapping_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector",
             "L203.SubsectorLogit",
             "L203.SubsectorShrwtFllt",
             "L203.TechShrwt",
             "L203.TechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_mapping.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector <- get_data(all_data, "L203.Supplysector")
    L203.SubsectorLogit <- get_data(all_data, "L203.SubsectorLogit")
    L203.SubsectorShrwtFllt <- get_data(all_data, "L203.SubsectorShrwtFllt")
    L203.TechShrwt <- get_data(all_data, "L203.TechShrwt")
    L203.TechCoef <- get_data(all_data, "L203.TechCoef")

    # ===================================================

    # Produce outputs
    create_xml("water_mapping.xml") %>%
      add_logit_tables_xml(L203.Supplysector, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit, "SubsectorLogit") %>%
      add_xml_data(L203.SubsectorShrwtFllt, "SubsectorShrwtFllt") %>%
      add_xml_data(L203.TechShrwt, "TechShrwt") %>%
      add_xml_data(L203.TechCoef, "TechCoef") %>%
      add_precursors("L203.Supplysector", "L203.SubsectorLogit", "L203.SubsectorShrwtFllt", "L203.TechShrwt", "L203.TechCoef") ->
      water_mapping.xml

    return_data(water_mapping.xml)
  } else {
    stop("Unknown command")
  }
}
