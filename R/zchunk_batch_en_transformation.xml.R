#' module_energy_batch_en_transformation.xml
#'
#' Construct XML data structure for \code{en_transformation.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation.xml.R} (energy XML).
module_energy_batch_en_transformation.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L222.SubsectorLogit_en",
              "L222.SubsectorShrwt_en",
              "L222.SubsectorInterp_en",
              "L222.SubsectorInterpTo_en",
              "L222.StubTech_en",
              "L222.GlobalTechInterp_en",
              "L222.GlobalTechCoef_en",
              "L222.GlobalTechCost_en",
              "L222.GlobalTechShrwt_en",
              "L222.GlobalTechCapture_en",
              "L222.StubTechProd_gasproc",
              "L222.StubTechProd_refining",
              "L222.StubTechCoef_refining"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en")
    L222.SubsectorShrwt_en <- get_data(all_data, "L222.SubsectorShrwt_en")
    L222.SubsectorInterp_en <- get_data(all_data, "L222.SubsectorInterp_en")
    L222.SubsectorInterpTo_en <- get_data(all_data, "L222.SubsectorInterpTo_en")
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en")
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en")
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en")
    L222.StubTechProd_gasproc <- get_data(all_data, "L222.StubTechProd_gasproc")
    L222.StubTechProd_refining <- get_data(all_data, "L222.StubTechProd_refining")
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining")

    # ===================================================

    # Produce outputs
    create_xml("en_transformation.xml") %>%
      add_xml_data(L222.SubsectorLogit_en,"SubsectorLogit") %>%
      add_xml_data(L222.SubsectorShrwt_en,"SubsectorShrwt") %>%
      add_xml_data(L222.SubsectorInterp_en,"SubsectorInterp") %>%
      add_xml_data(L222.SubsectorInterpTo_en,"SubsectorInterpTo") %>%
      add_xml_data(L222.StubTech_en,"StubTech") %>%
      add_xml_data(L222.GlobalTechInterp_en,"GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en,"GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechCost_en,"GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en,"GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en,"GlobalTechCapture") %>%
      add_xml_data(L222.StubTechProd_gasproc,"StubTechProd") %>%
      add_xml_data(L222.StubTechProd_refining,"StubTechProd") %>%
      add_xml_data(L222.StubTechCoef_refining,"StubTechCoef") %>%
      add_precursors("L222.SubsectorLogit_en", "L222.SubsectorShrwt_en", "L222.SubsectorInterp_en", "L222.SubsectorInterpTo_en", "L222.StubTech_en", "L222.GlobalTechInterp_en", "L222.GlobalTechCoef_en", "L222.GlobalTechCost_en", "L222.GlobalTechShrwt_en", "L222.GlobalTechCapture_en", "L222.StubTechProd_gasproc", "L222.StubTechProd_refining", "L222.StubTechCoef_refining") ->
      en_transformation.xml

    return_data(en_transformation.xml)
  } else {
    stop("Unknown command")
  }
}
