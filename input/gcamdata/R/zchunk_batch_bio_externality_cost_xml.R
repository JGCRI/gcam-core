# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_bio_externality_xml
#'
#' Construct XML data structure for \code{bio_externality.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{bio_externality.xml}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_batch_bio_externality_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.RenewRsrc",
             "L270.RenewRsrcPrice",
             "L270.GrdRenewRsrcCurves",
             "L270.GrdRenewRsrcMax",
             "L270.ResTechShrwt",
             "L270.AgCoef_bioext"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "bio_externality.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.RenewRsrc <- get_data(all_data, "L270.RenewRsrc")
    L270.RenewRsrcPrice <- get_data(all_data, "L270.RenewRsrcPrice")
    L270.GrdRenewRsrcCurves <- get_data(all_data, "L270.GrdRenewRsrcCurves")
    L270.GrdRenewRsrcMax <- get_data(all_data, "L270.GrdRenewRsrcMax")
    L270.ResTechShrwt <- get_data(all_data, "L270.ResTechShrwt")
    L270.AgCoef_bioext <- get_data(all_data, "L270.AgCoef_bioext")

    # ===================================================

    # Produce outputs
    create_xml("bio_externality.xml") %>%
      add_xml_data(L270.RenewRsrc, "RenewRsrc") %>%
      add_xml_data(L270.RenewRsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L270.GrdRenewRsrcCurves, "GrdRenewRsrcCurves") %>%
      add_xml_data(L270.GrdRenewRsrcMax, "GrdRenewRsrcMax") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L270.ResTechShrwt, "ResTechShrwt") %>%
      add_xml_data(L270.AgCoef_bioext, "AgCoef") %>%
      add_precursors("L270.RenewRsrc",
                     "L270.RenewRsrcPrice",
                     "L270.GrdRenewRsrcCurves",
                     "L270.GrdRenewRsrcMax",
                     "L270.ResTechShrwt",
                     "L270.AgCoef_bioext") ->
      bio_externality.xml

    return_data(bio_externality.xml)
  } else {
    stop("Unknown command")
  }
}
