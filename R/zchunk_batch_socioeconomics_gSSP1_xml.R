#' module_socio_batch_gSSP1.xml
#'
#' Construct XML data structure for \code{socioeconomics_gSSP1.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_gSSP1.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_gSSP1.xml} (socio XML).
module_socio_batch_gSSP1.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_gSSP1",
             "L201.BaseGDP_Scen",
             "L201.LaborForceFillout",
             "L201.LaborProductivity_gSSP1",
             "L201.PPPConvert"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_gSSP1.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_gSSP1 <- get_data(all_data, "L201.Pop_gSSP1")
    L201.BaseGDP_Scen <- get_data(all_data, "L201.BaseGDP_Scen")
    L201.LaborForceFillout <- get_data(all_data, "L201.LaborForceFillout")
    L201.LaborProductivity_gSSP1 <- get_data(all_data, "L201.LaborProductivity_gSSP1")
    L201.PPPConvert <- get_data(all_data, "L201.PPPConvert")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_gSSP1.xml") %>%
      add_xml_data(L201.Pop_gSSP1, "Pop_gSSP1") %>%
      add_xml_data(L201.BaseGDP_Scen, "BaseGDP_Scen") %>%
      add_xml_data(L201.LaborForceFillout, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_gSSP1, "LaborProductivity_gSSP1") %>%
      add_xml_data(L201.PPPConvert, "PPPConvert") %>%
      add_precursors("L201.Pop_gSSP1", "L201.BaseGDP_Scen", "L201.LaborForceFillout", "L201.LaborProductivity_gSSP1", "L201.PPPConvert") ->
      socioeconomics_gSSP1.xml

    return_data(socioeconomics_gSSP1.xml)
  } else {
    stop("Unknown command")
  }
}
