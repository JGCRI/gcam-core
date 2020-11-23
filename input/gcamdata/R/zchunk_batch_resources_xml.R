# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
module_energy_batch_resources_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.Rsrc",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.RsrcPrice",
             "L210.RenewRsrcPrice",
             "L210.UnlimitRsrcPrice",
             "L210.RsrcTechChange",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcTechChange_offshore_wind",
             "L210.RsrcCalProd",
             "L210.ReserveCalReserve",
             "L210.RsrcCurves_fos",
             "L210.RsrcCurves_U",
             "L210.SmthRenewRsrcCurves_MSW",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurves_offshore_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo",
             "L210.GrdRenewRsrcCurves_tradbio",
             "L210.GrdRenewRsrcMax_tradbio",
             "L210.ResSubresourceProdLifetime",
             "L210.SubresourcePriceAdder",
             "L210.ResReserveTechLifetime",
             "L210.ResReserveTechDeclinePhase",
             "L210.ResReserveTechProfitShutdown",
             "L210.ResTechShrwt",
             "L210.ResTechCoef",
             "L210.ResTechCost"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.Rsrc <- get_data(all_data, "L210.Rsrc")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.RsrcPrice <- get_data(all_data, "L210.RsrcPrice")
    L210.RenewRsrcPrice <- get_data(all_data, "L210.RenewRsrcPrice")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.RsrcTechChange <- get_data(all_data, "L210.RsrcTechChange")
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange")
    L210.SmthRenewRsrcTechChange_offshore_wind <- get_data(all_data, "L210.SmthRenewRsrcTechChange_offshore_wind")
    L210.RsrcCalProd <- get_data(all_data, "L210.RsrcCalProd")
    L210.ReserveCalReserve <- get_data(all_data, "L210.ReserveCalReserve")
    L210.RsrcCurves_fos <- get_data(all_data, "L210.RsrcCurves_fos")
    L210.RsrcCurves_U <- get_data(all_data, "L210.RsrcCurves_U")
    L210.SmthRenewRsrcCurves_MSW <- get_data(all_data, "L210.SmthRenewRsrcCurves_MSW")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    L210.SmthRenewRsrcCurves_offshore_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_offshore_wind")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV")
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo")
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo")
    L210.GrdRenewRsrcCurves_tradbio <- get_data(all_data, "L210.GrdRenewRsrcCurves_tradbio")
    L210.GrdRenewRsrcMax_tradbio <- get_data(all_data, "L210.GrdRenewRsrcMax_tradbio")
    L210.ResSubresourceProdLifetime <- get_data(all_data, "L210.ResSubresourceProdLifetime")
    L210.SubresourcePriceAdder <- get_data(all_data, "L210.SubresourcePriceAdder")
    L210.ResReserveTechLifetime <- get_data(all_data, "L210.ResReserveTechLifetime")
    L210.ResReserveTechDeclinePhase <- get_data(all_data, "L210.ResReserveTechDeclinePhase")
    L210.ResReserveTechProfitShutdown <- get_data(all_data, "L210.ResReserveTechProfitShutdown")
    L210.ResTechShrwt <- get_data(all_data, "L210.ResTechShrwt")
    L210.ResTechCost <- get_data(all_data, "L210.ResTechCost")
    L210.ResTechCoef <- get_data(all_data, "L210.ResTechCoef")

    # ===================================================

    # Produce outputs
    create_xml("resources.xml") %>%
      add_xml_data(L210.Rsrc, "Rsrc") %>%
      add_xml_data(L210.RenewRsrc, "RenewRsrc") %>%
      add_xml_data(L210.UnlimitRsrc, "UnlimitRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L210.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L210.SubresourcePriceAdder, "SubresourcePriceAdder") %>%
      add_xml_data(L210.ReserveCalReserve, "ReserveCalReserve") %>%
      add_xml_data(L210.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L210.ResTechCost, "ResReserveTechCost") %>%
      add_xml_data(L210.ResTechCoef, "ResReserveTechCoef") %>%
      add_xml_data(L210.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L210.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L210.RsrcPrice, "RsrcPrice") %>%
      add_xml_data(L210.RenewRsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L210.UnlimitRsrcPrice, "UnlimitRsrcPrice") %>%
      add_xml_data(L210.RsrcTechChange, "RsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_offshore_wind, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.RsrcCalProd, "RsrcCalProd") %>%
      add_xml_data(L210.RsrcCurves_fos, "RsrcCurves") %>%
      add_xml_data(L210.RsrcCurves_U, "RsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_MSW, "SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_offshore_wind, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurvesGdpElast_roofPV, "SmthRenewRsrcCurvesGdpElast") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_geo, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_geo, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_tradbio, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_tradbio, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.ResTechShrwt, "ResTechShrwt") %>%
      add_precursors("L210.Rsrc", "L210.RenewRsrc", "L210.UnlimitRsrc", "L210.RsrcPrice", "L210.RenewRsrcPrice",
                     "L210.UnlimitRsrcPrice", "L210.RsrcTechChange", "L210.SmthRenewRsrcTechChange", "L210.SmthRenewRsrcTechChange_offshore_wind", "L210.RsrcCalProd", "L210.ReserveCalReserve",
                     "L210.RsrcCurves_fos", "L210.RsrcCurves_U", "L210.SmthRenewRsrcCurves_MSW", "L210.SmthRenewRsrcCurves_wind", "L210.SmthRenewRsrcCurves_offshore_wind",
                     "L210.SmthRenewRsrcCurvesGdpElast_roofPV", "L210.GrdRenewRsrcCurves_geo", "L210.GrdRenewRsrcMax_geo",
                     "L210.GrdRenewRsrcCurves_tradbio", "L210.GrdRenewRsrcMax_tradbio", "L210.ResSubresourceProdLifetime", "L210.SubresourcePriceAdder",
                     "L210.ResReserveTechLifetime", "L210.ResReserveTechDeclinePhase", "L210.ResReserveTechProfitShutdown", "L210.ResTechShrwt","L210.ResTechCoef","L210.ResTechCost") ->
      resources.xml

    return_data(resources.xml)
  } else {
    stop("Unknown command")
  }
}
