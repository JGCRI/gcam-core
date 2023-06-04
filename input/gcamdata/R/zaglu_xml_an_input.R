# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_an_input_xml
#'
#' Construct XML data structure for \code{an_input.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{an_input.xml}. The corresponding file in the
#' original data system was \code{batch_an_input.xml.R} (aglu XML).
module_aglu_an_input_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L202.RenewRsrc",
      "L202.RenewRsrcPrice",
      "L202.maxSubResource",
      "L202.RenewRsrcCurves",
      "L202.ResTechShrwt",
      "L202.UnlimitedRenewRsrcCurves",
      "L202.UnlimitedRenewRsrcPrice",
      "L202.Supplysector_in",
      "L202.SubsectorAll_in",
      "L202.SubsectorInterpTo_in",
      "L202.StubTech_in",
      "L202.StubTechInterp_in",
      "L202.GlobalTechCoef_in",
      "L202.GlobalTechShrwt_in",
      "L202.StubTechProd_in",
      "L202.Supplysector_an",
      "L202.SubsectorAll_an",
      "L202.GlobalTechShrwt_an",
      "L202.StubTechInterp_an",
      "L202.StubTechProd_an",
      "L202.StubTechCoef_an",
      "L202.StubTechCost_an")

  MODULE_OUTPUTS <-
    c(XML = "an_input.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L202.RenewRsrc <- get_data(all_data, "L202.RenewRsrc")
    L202.RenewRsrcPrice <- get_data(all_data, "L202.RenewRsrcPrice")
    L202.maxSubResource <- get_data(all_data, "L202.maxSubResource")
    L202.RenewRsrcCurves <- get_data(all_data, "L202.RenewRsrcCurves")
    L202.ResTechShrwt <- get_data(all_data, "L202.ResTechShrwt")
    L202.UnlimitedRenewRsrcCurves <- get_data(all_data, "L202.UnlimitedRenewRsrcCurves")
    L202.UnlimitedRenewRsrcPrice <- get_data(all_data, "L202.UnlimitedRenewRsrcPrice")
    L202.Supplysector_in <- get_data(all_data, "L202.Supplysector_in")
    L202.SubsectorAll_in <- get_data(all_data, "L202.SubsectorAll_in")
    L202.SubsectorInterpTo_in <- get_data(all_data, "L202.SubsectorInterpTo_in")
    L202.StubTech_in <- get_data(all_data, "L202.StubTech_in")
    L202.StubTechInterp_in <- get_data(all_data, "L202.StubTechInterp_in")
    L202.GlobalTechCoef_in <- get_data(all_data, "L202.GlobalTechCoef_in")
    L202.GlobalTechShrwt_in <- get_data(all_data, "L202.GlobalTechShrwt_in")
    L202.StubTechProd_in <- get_data(all_data, "L202.StubTechProd_in")
    L202.Supplysector_an <- get_data(all_data, "L202.Supplysector_an")
    L202.SubsectorAll_an <- get_data(all_data, "L202.SubsectorAll_an")
    L202.GlobalTechShrwt_an <- get_data(all_data, "L202.GlobalTechShrwt_an")
    L202.StubTechInterp_an <- get_data(all_data, "L202.StubTechInterp_an")
    L202.StubTechProd_an <- get_data(all_data, "L202.StubTechProd_an")
    L202.StubTechCoef_an <- get_data(all_data, "L202.StubTechCoef_an")
    L202.StubTechCost_an <- get_data(all_data, "L202.StubTechCost_an")


    # ===================================================

    # Produce outputs
    create_xml("an_input.xml") %>%
      add_xml_data(L202.RenewRsrc, "RenewRsrc") %>%
      add_xml_data(L202.RenewRsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L202.maxSubResource, "maxSubResource") %>%
      add_xml_data(L202.RenewRsrcCurves, "RenewRsrcCurves") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L202.ResTechShrwt, "ResTechShrwt") %>%
      add_xml_data(L202.UnlimitedRenewRsrcCurves, "UnlimitRsrc") %>%
      add_xml_data(L202.UnlimitedRenewRsrcPrice, "UnlimitRsrcPrice") %>%
      add_logit_tables_xml(L202.Supplysector_in, "Supplysector") %>%
      add_logit_tables_xml(L202.SubsectorAll_in, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L202.SubsectorInterpTo_in, "SubsectorInterpTo") %>%
      add_xml_data(L202.StubTech_in, "StubTech") %>%
      add_xml_data(L202.StubTechInterp_in, "StubTechInterp") %>%
      add_xml_data(L202.GlobalTechCoef_in, "GlobalTechCoef") %>%
      add_xml_data(L202.GlobalTechShrwt_in, "GlobalTechShrwt") %>%
      add_xml_data(L202.StubTechProd_in, "StubTechProd") %>%
      add_logit_tables_xml(L202.Supplysector_an, "Supplysector") %>%
      add_logit_tables_xml(L202.SubsectorAll_an, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L202.GlobalTechShrwt_an, "GlobalTechShrwt") %>%
      add_xml_data(L202.StubTechInterp_an, "StubTechInterp") %>%
      add_xml_data(L202.StubTechProd_an, "StubTechProd") %>%
      add_xml_data(L202.StubTechCoef_an, "StubTechCoef") %>%
      add_xml_data(L202.StubTechCost_an, "StubTechCost") %>%
      add_precursors("L202.RenewRsrc",
                     "L202.RenewRsrcPrice",
                     "L202.maxSubResource",
                     "L202.RenewRsrcCurves",
                     "L202.ResTechShrwt",
                     "L202.UnlimitedRenewRsrcCurves",
                     "L202.UnlimitedRenewRsrcPrice",
                     "L202.Supplysector_in",
                     "L202.SubsectorAll_in",
                     "L202.SubsectorInterpTo_in",
                     "L202.StubTech_in",
                     "L202.StubTechInterp_in",
                     "L202.GlobalTechCoef_in",
                     "L202.GlobalTechShrwt_in",
                     "L202.StubTechProd_in",
                     "L202.Supplysector_an",
                     "L202.SubsectorAll_an",
                     "L202.GlobalTechShrwt_an",
                     "L202.StubTechInterp_an",
                     "L202.StubTechProd_an",
                     "L202.StubTechCoef_an",
                     "L202.StubTechCost_an"
                     ) ->
      an_input.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
