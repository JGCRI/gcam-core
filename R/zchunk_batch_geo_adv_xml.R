# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_geo_adv_xml
#'
#' Construct XML data structure for \code{geo_adv.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{geo_adv.xml}. The corresponding file in the
#' original data system was \code{batch_geo_adv_xml.R} (energy XML).
module_energy_batch_geo_adv_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.GrdRenewRsrcCurves_EGS",
             "L210.GrdRenewRsrcMax_EGS",
             "L210.ResTechShrwt_EGS"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "geo_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.GrdRenewRsrcCurves_EGS <- get_data(all_data, "L210.GrdRenewRsrcCurves_EGS")
    L210.GrdRenewRsrcMax_EGS <- get_data(all_data, "L210.GrdRenewRsrcMax_EGS")
    L210.ResTechShrwt_EGS <- get_data(all_data, "L210.ResTechShrwt_EGS")

    # ===================================================

    # Produce outputs
    create_xml("geo_adv.xml") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_EGS, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_EGS, "GrdRenewRsrcMax") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L210.ResTechShrwt_EGS, "ResTechShrwt") %>%
      add_precursors("L210.GrdRenewRsrcCurves_EGS", "L210.GrdRenewRsrcMax_EGS",
                     "L210.ResTechShrwt_EGS") ->
      geo_adv.xml

    return_data(geo_adv.xml)
  } else {
    stop("Unknown command")
  }
}
