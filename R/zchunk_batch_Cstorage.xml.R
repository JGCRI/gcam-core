#' module_energy_batch_Cstorage.xml
#'
#' Construct XML data structure for \code{Cstorage.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Cstorage.xml}. The corresponding file in the
#' original data system was \code{batch_Cstorage.xml.R} (energy XML).
module_energy_batch_Cstorage.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.DepRsrc",
              "L261.UnlimitRsrc",
              "L261.DepRsrcCurves_C",
              "L261.Supplysector_C",
              "L261.SubsectorLogit_C",
              "L261.SubsectorShrwtFllt_C",
              "L261.StubTech_C",
              "L261.GlobalTechCoef_C",
              "L261.GlobalTechCost_C",
              "L261.GlobalTechShrwt_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Cstorage.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.DepRsrc <- get_data(all_data, "L261.DepRsrc")
    L261.UnlimitRsrc <- get_data(all_data, "L261.UnlimitRsrc")
    L261.DepRsrcCurves_C <- get_data(all_data, "L261.DepRsrcCurves_C")
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C")
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C")
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C")
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C")
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C")
    L261.GlobalTechCost_C <- get_data(all_data, "L261.GlobalTechCost_C")
    L261.GlobalTechShrwt_C <- get_data(all_data, "L261.GlobalTechShrwt_C")

    # ===================================================

    # Produce outputs
    create_xml("Cstorage.xml") %>%
      add_xml_data(L261.DepRsrc,"DepRsrc") %>%
      add_xml_data(L261.UnlimitRsrc,"UnlimitRsrc") %>%
      add_xml_data(L261.DepRsrcCurves_C,"DepRsrcCurves") %>%
      add_xml_data(L261.Supplysector_C,"Supplysector") %>%
      add_xml_data(L261.SubsectorLogit_C,"SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C,"SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C,"StubTech") %>%
      add_xml_data(L261.GlobalTechCoef_C,"GlobalTechCoef") %>%
      add_xml_data(L261.GlobalTechCost_C,"GlobalTechCost") %>%
      add_xml_data(L261.GlobalTechShrwt_C,"GlobalTechShrwt") %>%
      add_precursors("L261.DepRsrc", "L261.UnlimitRsrc", "L261.DepRsrcCurves_C", "L261.Supplysector_C", "L261.SubsectorLogit_C", "L261.SubsectorShrwtFllt_C", "L261.StubTech_C", "L261.GlobalTechCoef_C", "L261.GlobalTechCost_C", "L261.GlobalTechShrwt_C") ->
      Cstorage.xml

    return_data(Cstorage.xml)
  } else {
    stop("Unknown command")
  }
}
