#' module_gcamusa_batch_water_elec_USA_ref_xml
#'
#' Construct XML data structure for \code{water_elec_USA_ref.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_elec_USA_ref.xml}. The corresponding file in the
#' original data system was \code{batch_water_elec_USA_ref_xml.R} (water XML).
module_gcamusa_batch_water_elec_USA_ref_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2233.StubTech_WaterCoef_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_elec_USA_ref.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.StubTech_WaterCoef_ref <- get_data(all_data, "L2233.StubTech_WaterCoef_ref")

    # ===================================================

    # Produce outputs
    create_xml("water_elec_USA_ref.xml") %>%
      add_xml_data(L2233.StubTech_WaterCoef_ref, "StubTechCoef") %>%
      add_precursors("L2233.StubTech_WaterCoef_ref") ->
      water_elec_USA_ref.xml

    return_data(water_elec_USA_ref.xml)
  } else {
    stop("Unknown command")
  }
}
