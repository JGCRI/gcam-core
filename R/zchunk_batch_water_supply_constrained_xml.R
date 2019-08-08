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
    return(c("L201.NodeEquiv",
             "L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_calib",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_supply_constrained.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.NodeEquiv <- get_data(all_data, "L201.NodeEquiv")
    L201.DeleteUnlimitRsrc <- get_data(all_data, "L201.DeleteUnlimitRsrc")
    L201.Rsrc <- get_data(all_data, "L201.Rsrc")
    L201.RsrcPrice <- get_data(all_data, "L201.RsrcPrice")
    L201.RenewRsrcCurves_calib <- get_data(all_data, "L201.RenewRsrcCurves_calib")
    L201.GrdRenewRsrcMax_runoff <- get_data(all_data, "L201.GrdRenewRsrcMax_runoff")
    L201.DepRsrcCurves_ground <- get_data(all_data, "L201.DepRsrcCurves_ground")


    # ===================================================

    # Produce outputs
    create_xml("water_supply_constrained.xml") %>%
      add_xml_data(L201.NodeEquiv, "EQUIV_TABLE") %>%
      add_xml_data(L201.DeleteUnlimitRsrc, "DeleteUnlimitRsrc") %>%
      add_xml_data(L201.Rsrc, "Rsrc") %>%
      add_xml_data(L201.RsrcPrice, "RsrcPrice") %>%
      add_xml_data(L201.RenewRsrcCurves_calib, "RenewRsrcCurvesWater") %>%
      add_xml_data(L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxWaterNoFO") %>%
      add_xml_data(L201.DepRsrcCurves_ground, "RsrcCurves") %>%
      add_precursors("L201.NodeEquiv",
                     "L201.DeleteUnlimitRsrc",
                     "L201.Rsrc",
                     "L201.RsrcPrice",
                     "L201.RenewRsrcCurves_calib",
                     "L201.GrdRenewRsrcMax_runoff",
                     "L201.DepRsrcCurves_ground") ->
      water_supply_constrained.xml


    return_data(water_supply_constrained.xml)
  } else {
    stop("Unknown command")
  }
}
