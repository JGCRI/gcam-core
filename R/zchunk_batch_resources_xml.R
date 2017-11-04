#' module_energy_batch_resources_xml
#'
#' Construct XML data structure for \code{resources.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources.xml}. The corresponding file in the
#' original data system was \code{batch_resources_xml.R} (energy XML).
module_energy_batch_resources_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DepRsrc",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.DepRsrcPrice",
             "L210.RenewRsrcPrice",
             "L210.UnlimitRsrcPrice",
             "L210.DepRsrcTechChange",
             "L210.SmthRenewRsrcTechChange",
             "L210.DepRsrcCalProd",
             "L210.DepRsrcCurves_fos",
             "L210.DepRsrcCurves_U",
             "L210.SmthRenewRsrcCurvesGdpElast_MSW",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo",
             "L210.GrdRenewRsrcCurves_tradbio",
             "L210.GrdRenewRsrcMax_tradbio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DepRsrc <- get_data(all_data, "L210.DepRsrc")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.DepRsrcPrice <- get_data(all_data, "L210.DepRsrcPrice")
    L210.RenewRsrcPrice <- get_data(all_data, "L210.RenewRsrcPrice")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.DepRsrcTechChange <- get_data(all_data, "L210.DepRsrcTechChange")
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange")
    L210.DepRsrcCalProd <- get_data(all_data, "L210.DepRsrcCalProd")
    L210.DepRsrcCurves_fos <- get_data(all_data, "L210.DepRsrcCurves_fos")
    L210.DepRsrcCurves_U <- get_data(all_data, "L210.DepRsrcCurves_U")
    L210.SmthRenewRsrcCurvesGdpElast_MSW <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_MSW")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV")
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo")
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo")
    L210.GrdRenewRsrcCurves_tradbio <- get_data(all_data, "L210.GrdRenewRsrcCurves_tradbio")
    L210.GrdRenewRsrcMax_tradbio <- get_data(all_data, "L210.GrdRenewRsrcMax_tradbio")

    # ===================================================

    # Produce outputs
    create_xml("resources.xml") %>%
      add_xml_data(L210.DepRsrc,"DepRsrc") %>%
      add_xml_data(L210.RenewRsrc,"RenewRsrc") %>%
      add_xml_data(L210.UnlimitRsrc,"UnlimitRsrc") %>%
      add_xml_data(L210.DepRsrcPrice,"DepRsrcPrice") %>%
      add_xml_data(L210.RenewRsrcPrice,"RenewRsrcPrice") %>%
      add_xml_data(L210.UnlimitRsrcPrice,"UnlimitRsrcPrice") %>%
      add_xml_data(L210.DepRsrcTechChange,"DepRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange,"SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.DepRsrcCalProd,"DepRsrcCalProd") %>%
      add_xml_data(L210.DepRsrcCurves_fos,"DepRsrcCurves") %>%
      add_xml_data(L210.DepRsrcCurves_U,"DepRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurvesGdpElast_MSW,"SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind,"SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV,"SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_geo,"GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_geo,"GrdRenewRsrcMax") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_tradbio,"GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_tradbio,"GrdRenewRsrcMax") %>%
      add_precursors("L210.DepRsrc", "L210.RenewRsrc", "L210.UnlimitRsrc", "L210.DepRsrcPrice", "L210.RenewRsrcPrice", "L210.UnlimitRsrcPrice", "L210.DepRsrcTechChange", "L210.SmthRenewRsrcTechChange", "L210.DepRsrcCalProd", "L210.DepRsrcCurves_fos", "L210.DepRsrcCurves_U", "L210.SmthRenewRsrcCurvesGdpElast_MSW", "L210.SmthRenewRsrcCurves_wind", "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV", "L210.GrdRenewRsrcCurves_geo", "L210.GrdRenewRsrcMax_geo", "L210.GrdRenewRsrcCurves_tradbio", "L210.GrdRenewRsrcMax_tradbio") ->
      resources.xml

    return_data(resources.xml)
  } else {
    stop("Unknown command")
  }
}
