#' module_water_batch_water_supply_uncalibrated_xml
#'
#' Construct XML data structure for \code{water_supply_uncalibrated.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_supply_uncalibrated.xml}. The corresponding file in the
#' original data system was \code{batch_water_supply_uncalibrated.xml} (water XML).
module_water_batch_water_supply_uncalibrated_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.NodeEquiv",
             "L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_uncalibrated",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground_uniform"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_supply_uncalibrated.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.NodeEquiv <- get_data(all_data, "L201.NodeEquiv")
    L201.DeleteUnlimitRsrc <- get_data(all_data, "L201.DeleteUnlimitRsrc")
    L201.Rsrc <- get_data(all_data, "L201.Rsrc")
    L201.RsrcPrice <- get_data(all_data, "L201.RsrcPrice")
    L201.RenewRsrcCurves_uncalibrated <- get_data(all_data, "L201.RenewRsrcCurves_uncalibrated")
    L201.GrdRenewRsrcMax_runoff <- get_data(all_data, "L201.GrdRenewRsrcMax_runoff")
    L201.DepRsrcCurves_ground_uniform <- get_data(all_data, "L201.DepRsrcCurves_ground_uniform")


    # ===================================================

    # Produce outputs
    create_xml("water_supply_uncalibrated.xml") %>%
      add_xml_data(L201.NodeEquiv, "EQUIV_TABLE") %>%
      add_xml_data(L201.DeleteUnlimitRsrc, "DeleteUnlimitRsrc") %>%
      add_xml_data(L201.Rsrc, "Rsrc") %>%
      add_xml_data(L201.RsrcPrice, "RsrcPrice") %>%
      add_xml_data(L201.RenewRsrcCurves_uncalibrated, "RenewRsrcCurvesWater") %>%
      add_xml_data(L201.GrdRenewRsrcMax_runoff, "GrdRenewRsrcMaxWaterNoFO") %>%
      add_xml_data(L201.DepRsrcCurves_ground_uniform, "RsrcCurves") %>%
      add_precursors("L201.NodeEquiv",
                     "L201.DeleteUnlimitRsrc",
                     "L201.Rsrc",
                     "L201.RsrcPrice",
                     "L201.RenewRsrcCurves_uncalibrated",
                     "L201.GrdRenewRsrcMax_runoff",
                     "L201.DepRsrcCurves_ground_uniform") ->
      water_supply_uncalibrated.xml


    return_data(water_supply_uncalibrated.xml)
  } else {
    stop("Unknown command")
  }
}
