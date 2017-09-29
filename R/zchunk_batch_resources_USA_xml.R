#' module_gcamusa_batch_resources_USA_xml
#'
#' Construct XML data structure for \code{resources_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_USA.xml}. The corresponding file in the
#' original data system was \code{batch_resources_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_resources_USA_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteRenewRsrc_USArsrc",
             "L210.DeleteUnlimitRsrc_USArsrc",
             "L210.RenewRsrc_USA",
             "L210.UnlimitRsrc_USA",
             "L210.UnlimitRsrcPrice_USA",
             "L210.SmthRenewRsrcTechChange_USA",
             "L210.SmthRenewRsrcCurves_wind_USA",
             "L210.GrdRenewRsrcCurves_geo_USA",
             "L210.GrdRenewRsrcMax_geo_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteRenewRsrc_USArsrc <- get_data(all_data, "L210.DeleteRenewRsrc_USArsrc")
    L210.DeleteUnlimitRsrc_USArsrc <- get_data(all_data, "L210.DeleteUnlimitRsrc_USArsrc")
    L210.RenewRsrc_USA <- get_data(all_data, "L210.RenewRsrc_USA")
    L210.UnlimitRsrc_USA <- get_data(all_data, "L210.UnlimitRsrc_USA")
    L210.UnlimitRsrcPrice_USA <- get_data(all_data, "L210.UnlimitRsrcPrice_USA")
    L210.SmthRenewRsrcTechChange_USA <- get_data(all_data, "L210.SmthRenewRsrcTechChange_USA")
    L210.SmthRenewRsrcCurves_wind_USA <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind_USA")
    L210.GrdRenewRsrcCurves_geo_USA <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo_USA")
    L210.GrdRenewRsrcMax_geo_USA <- get_data(all_data, "L210.GrdRenewRsrcMax_geo_USA")

    # ===================================================

    # Produce outputs
    create_xml("resources_USA.xml") %>%
      add_xml_data(L210.DeleteRenewRsrc_USArsrc,"DeleteRenewRsrc") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_USArsrc,"DeleteUnlimitRsrc") %>%
      add_xml_data(L210.RenewRsrc_USA,"RenewRsrc") %>%
      add_xml_data(L210.UnlimitRsrc_USA,"UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_USA,"UnlimitRsrcPrice") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_USA,"SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind_USA,"SmthRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_geo_USA,"GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_geo_USA,"GrdRenewRsrcMax") %>%
      add_precursors("L210.DeleteRenewRsrc_USArsrc", "L210.DeleteUnlimitRsrc_USArsrc", "L210.RenewRsrc_USA", "L210.UnlimitRsrc_USA", "L210.UnlimitRsrcPrice_USA", "L210.SmthRenewRsrcTechChange_USA", "L210.SmthRenewRsrcCurves_wind_USA", "L210.GrdRenewRsrcCurves_geo_USA", "L210.GrdRenewRsrcMax_geo_USA") ->
      resources_USA.xml

    return_data(resources_USA.xml)
  } else {
    stop("Unknown command")
  }
}
