#' module_energy_batch_en_distribution.xml
#'
#' Construct XML data structure for \code{en_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_distribution.xml}. The corresponding file in the
#' original data system was \code{batch_en_distribution.xml.R} (energy XML).
module_energy_batch_en_distribution.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.SubsectorLogit_en",
              "L226.SubsectorShrwt_en",
              "L226.SubsectorInterp_en",
              "L226.SubsectorInterpTo_en",
              "L226.StubTech_en",
              "L226.GlobalTechEff_en",
              "L226.GlobalTechCost_en",
              "L226.GlobalTechShrwt_en",
              "L226.StubTechCoef_elecownuse",
              "L226.StubTechCoef_electd",
              "L226.StubTechCoef_gaspipe"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_distribution.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.StubTech_en <- get_data(all_data, "L226.StubTech_en")
    L226.GlobalTechEff_en <- get_data(all_data, "L226.GlobalTechEff_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_elecownuse <- get_data(all_data, "L226.StubTechCoef_elecownuse")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")
    L226.StubTechCoef_gaspipe <- get_data(all_data, "L226.StubTechCoef_gaspipe")

    # ===================================================

    # Produce outputs
    create_xml("en_distribution.xml") %>%
      add_xml_data(L226.SubsectorLogit_en,"SubsectorLogit") %>%
      add_xml_data(L226.SubsectorShrwt_en,"SubsectorShrwt") %>%
      add_xml_data(L226.SubsectorInterp_en,"SubsectorInterp") %>%
      add_xml_data(L226.SubsectorInterpTo_en,"SubsectorInterpTo") %>%
      add_xml_data(L226.StubTech_en,"StubTech") %>%
      add_xml_data(L226.GlobalTechEff_en,"GlobalTechEff") %>%
      add_xml_data(L226.GlobalTechCost_en,"GlobalTechCost") %>%
      add_xml_data(L226.GlobalTechShrwt_en,"GlobalTechShrwt") %>%
      add_xml_data(L226.StubTechCoef_elecownuse,"StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_electd,"StubTechCoef") %>%
      add_xml_data(L226.StubTechCoef_gaspipe,"StubTechCoef") %>%
      add_precursors("L226.SubsectorLogit_en", "L226.SubsectorShrwt_en", "L226.SubsectorInterp_en", "L226.SubsectorInterpTo_en", "L226.StubTech_en", "L226.GlobalTechEff_en", "L226.GlobalTechCost_en", "L226.GlobalTechShrwt_en", "L226.StubTechCoef_elecownuse", "L226.StubTechCoef_electd", "L226.StubTechCoef_gaspipe") ->
      en_distribution.xml

    return_data(en_distribution.xml)
  } else {
    stop("Unknown command")
  }
}
