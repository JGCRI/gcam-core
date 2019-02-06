#' module_water_batch_water_td_xml
#'
#' Construct XML data structure for \code{water_td.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_td.xml}. The corresponding file in the
#' original data system was \code{batch_water_mapping.xml.R} (water XML).
module_water_batch_water_td_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector_watertd",
             "L203.SubsectorLogit_watertd",
             "L203.SubsectorShrwtFllt_watertd",
             "L203.SubsectorInterp_watertd",
             "L203.TechShrwt_watertd",
             "L203.TechCoef_watertd",
             "L203.Production_watertd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_td.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector_watertd <- get_data(all_data, "L203.Supplysector_watertd")
    L203.SubsectorLogit_watertd <- get_data(all_data, "L203.SubsectorLogit_watertd")
    L203.SubsectorShrwtFllt_watertd <- get_data(all_data, "L203.SubsectorShrwtFllt_watertd")
    L203.SubsectorInterp_watertd <- get_data(all_data, "L203.SubsectorInterp_watertd")
    L203.TechShrwt_watertd <- get_data(all_data, "L203.TechShrwt_watertd")
    L203.TechCoef_watertd <- get_data(all_data, "L203.TechCoef_watertd")
    L203.Production_watertd <- get_data(all_data, "L203.Production_watertd")

    # ===================================================

    # Produce outputs
    create_xml("water_td.xml") %>%
      add_logit_tables_xml(L203.Supplysector_watertd, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_watertd, "SubsectorLogit") %>%
      add_xml_data(L203.SubsectorShrwtFllt_watertd, "SubsectorShrwtFllt") %>%
      add_xml_data(L203.SubsectorInterp_watertd, "SubsectorInterp") %>%
      add_xml_data(L203.TechShrwt_watertd, "TechShrwt") %>%
      add_xml_data(L203.TechCoef_watertd, "TechCoef") %>%
      add_xml_data(L203.Production_watertd, "Production") %>%
      add_precursors("L203.Supplysector_watertd", "L203.SubsectorLogit_watertd", "L203.SubsectorShrwtFllt_watertd",
                     "L203.SubsectorInterp_watertd", "L203.TechShrwt_watertd", "L203.TechCoef_watertd",
                     "L203.Production_watertd") ->
      water_td.xml

    return_data(water_td.xml)
  } else {
    stop("Unknown command")
  }
}
