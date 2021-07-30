# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_water_supply_constrained_xml
#'
#' Construct XML data structure for \code{water_supply_constrained.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_supply_constrained.xml}. The corresponding file in the
#' original data system was \code{batch_water_supply_x.xml} (water XML).
module_water_batch_water_supply_constrained_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_calib",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground",
             "L201.RenewRsrcTechShrwt",
             "L201.RsrcTechShrwt",
             "L201.RsrcTechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_supply_constrained.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.DeleteUnlimitRsrc <- get_data(all_data, "L201.DeleteUnlimitRsrc")
    L201.Rsrc <- get_data(all_data, "L201.Rsrc")
    L201.RsrcPrice <- get_data(all_data, "L201.RsrcPrice")
    L201.RenewRsrcCurves_calib <- get_data(all_data, "L201.RenewRsrcCurves_calib")
    L201.GrdRenewRsrcMax_runoff <- get_data(all_data, "L201.GrdRenewRsrcMax_runoff")
    L201.DepRsrcCurves_ground <- get_data(all_data, "L201.DepRsrcCurves_ground")
    L201.RenewRsrcTechShrwt <- get_data(all_data, "L201.RenewRsrcTechShrwt")
    L201.RsrcTechShrwt <- get_data(all_data, "L201.RsrcTechShrwt")
    L201.RsrcTechCoef <- get_data(all_data, "L201.RsrcTechCoef")

    resource <- NULL    # silence package check notes

    # ===================================================

    # Produce outputs
    create_xml("water_supply_constrained.xml") %>%
      add_node_equiv_xml("resource") %>%
      add_xml_data(L201.DeleteUnlimitRsrc, "DeleteUnlimitRsrc") %>%
      add_xml_data(L201.Rsrc, "Rsrc") %>%
      add_xml_data(L201.RsrcPrice, "RsrcPrice") %>%
      # Note we are going to use the RenewRsrcCurves header to avoid having to create
      # duplicate headers.  The resource type will remain "resource" because we set
      # the "resource" node_equiv_xml above.  However we still need to set the
      # column names appropriately if we want the column re-ordering to work.
      add_xml_data(L201.RenewRsrcCurves_calib %>% rename(renewresource = resource), "RenewRsrcCurves") %>%
      add_xml_data(L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxNoFillOut") %>%
      add_xml_data(L201.DepRsrcCurves_ground, "RsrcCurves") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L201.RenewRsrcTechShrwt, "ResTechShrwt") %>%
      add_xml_data(L201.RsrcTechShrwt, "ResTechShrwt") %>%
      add_xml_data(L201.RsrcTechCoef, "ResTechCoef") %>%
      add_precursors("L201.DeleteUnlimitRsrc",
                     "L201.Rsrc",
                     "L201.RsrcPrice",
                     "L201.RenewRsrcCurves_calib",
                     "L201.GrdRenewRsrcMax_runoff",
                     "L201.DepRsrcCurves_ground",
                     "L201.RenewRsrcTechShrwt",
                     "L201.RsrcTechShrwt",
                     "L201.RsrcTechCoef") ->
    water_supply_constrained.xml
    return_data(water_supply_constrained.xml)
  } else {
    stop("Unknown command")
  }
}
