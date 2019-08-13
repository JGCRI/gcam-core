#' module_gcamindia_batch_resources_xml
#'
#' Construct XML data structure for \code{resources_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_india.xml}. The corresponding file in the
#' original data system was \code{batch_resources_india_xml.R} (gcamindia XML).
module_gcamindia_batch_resources_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteRenewRsrc_indiarsrc",
             "L210.DeleteUnlimitRsrc_indiarsrc",
             "L210.india_state_RenewRsrc",
             "L210.india_state_UnlimitRsrc",
             "L210.india_state_UnlimitRsrcPrice",
             "L210.india_state_SmthRenewRsrcTechChange",
             "L210.india_state_SmthRenewRsrcCurves_wind",
             "L210.india_state_GrdRenewRsrcCurves_geo",
             "L210.india_state_GrdRenewRsrcMax_geo",
             "L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteRenewRsrc_indiarsrc <- get_data(all_data, "L210.DeleteRenewRsrc_indiarsrc")
    L210.DeleteUnlimitRsrc_indiarsrc <- get_data(all_data, "L210.DeleteUnlimitRsrc_indiarsrc")
    L210.india_state_RenewRsrc <- get_data(all_data, "L210.india_state_RenewRsrc")
    L210.india_state_UnlimitRsrc <- get_data(all_data, "L210.india_state_UnlimitRsrc")
    L210.india_state_UnlimitRsrcPrice <- get_data(all_data, "L210.india_state_UnlimitRsrcPrice")
    L210.india_state_SmthRenewRsrcTechChange <- get_data(all_data, "L210.india_state_SmthRenewRsrcTechChange")
    L210.india_state_SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.india_state_SmthRenewRsrcCurves_wind")
    L210.india_state_GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.india_state_GrdRenewRsrcCurves_geo")
    L210.india_state_GrdRenewRsrcMax_geo <- get_data(all_data, "L210.india_state_GrdRenewRsrcMax_geo")
    L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV")

    # ===================================================

    # Produce outputs
    create_xml("resources_india.xml") %>%
      add_xml_data(L210.DeleteRenewRsrc_indiarsrc, "DeleteRenewRsrc") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_indiarsrc, "DeleteUnlimitRsrc") %>%
      add_xml_data(L210.india_state_RenewRsrc, "RenewRsrc") %>%
      add_xml_data(L210.india_state_UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L210.india_state_UnlimitRsrcPrice, "UnlimitRsrcPrice") %>%
      add_xml_data(L210.india_state_SmthRenewRsrcTechChange, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.india_state_SmthRenewRsrcCurves_wind, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.india_state_GrdRenewRsrcCurves_geo, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.india_state_GrdRenewRsrcMax_geo, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV, "SmthRenewRsrcCurvesGdpElast") %>%
      add_precursors("L210.DeleteRenewRsrc_indiarsrc",
                     "L210.DeleteUnlimitRsrc_indiarsrc",
                     "L210.india_state_RenewRsrc",
                     "L210.india_state_UnlimitRsrc",
                     "L210.india_state_UnlimitRsrcPrice",
                     "L210.india_state_SmthRenewRsrcTechChange",
                     "L210.india_state_SmthRenewRsrcCurves_wind",
                     "L210.india_state_GrdRenewRsrcCurves_geo",
                     "L210.india_state_GrdRenewRsrcMax_geo",
                     "L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV") ->
      resources_india.xml

    return_data(resources_india.xml)
  } else {
    stop("Unknown command")
  }
}
