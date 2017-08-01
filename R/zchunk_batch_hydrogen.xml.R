#' module_energy_batch_hydrogen.xml
#'
#' Construct XML data structure for \code{hydrogen.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_batch_hydrogen.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L225.SubsectorLogit_h2",
 "L225.SubsectorShrwt_h2",
 "L225.SubsectorInterp_h2",
 "L225.SubsectorInterpTo_h2",
 "L225.StubTech_h2",
 "L225.GlobalTechEff_h2",
 "L225.GlobalTechCost_h2",
 "L225.GlobalTechShrwt_h2",
 "L225.PrimaryRenewKeyword_h2",
 "L225.AvgFossilEffKeyword_h2",
 "L225.GlobalTechCapture_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2")
  L225.SubsectorShrwt_h2 <- get_data(all_data, "L225.SubsectorShrwt_h2")
  L225.SubsectorInterp_h2 <- get_data(all_data, "L225.SubsectorInterp_h2")
  L225.SubsectorInterpTo_h2 <- get_data(all_data, "L225.SubsectorInterpTo_h2")
  L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2")
  L225.GlobalTechEff_h2 <- get_data(all_data, "L225.GlobalTechEff_h2")
  L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2")
  L225.GlobalTechShrwt_h2 <- get_data(all_data, "L225.GlobalTechShrwt_h2")
  L225.PrimaryRenewKeyword_h2 <- get_data(all_data, "L225.PrimaryRenewKeyword_h2")
  L225.AvgFossilEffKeyword_h2 <- get_data(all_data, "L225.AvgFossilEffKeyword_h2")
  L225.GlobalTechCapture_h2 <- get_data(all_data, "L225.GlobalTechCapture_h2")

    # ===================================================

    # Produce outputs
    create_xml("hydrogen.xml") %>%
add_xml_data(L225.SubsectorLogit_h2,"SubsectorLogit") %>%
add_xml_data(L225.SubsectorShrwt_h2,"SubsectorShrwt") %>%
add_xml_data(L225.SubsectorInterp_h2,"SubsectorInterp") %>%
add_xml_data(L225.SubsectorInterpTo_h2,"SubsectorInterpTo") %>%
add_xml_data(L225.StubTech_h2,"StubTech") %>%
add_xml_data(L225.GlobalTechEff_h2,"GlobalTechEff") %>%
add_xml_data(L225.GlobalTechCost_h2,"GlobalTechCost") %>%
add_xml_data(L225.GlobalTechShrwt_h2,"GlobalTechShrwt") %>%
add_xml_data(L225.PrimaryRenewKeyword_h2,"PrimaryRenewKeyword") %>%
add_xml_data(L225.AvgFossilEffKeyword_h2,"AvgFossilEffKeyword") %>%
add_xml_data(L225.GlobalTechCapture_h2,"GlobalTechCapture") %>%
add_precursors("L225.SubsectorLogit_h2", "L225.SubsectorShrwt_h2", "L225.SubsectorInterp_h2", "L225.SubsectorInterpTo_h2", "L225.StubTech_h2", "L225.GlobalTechEff_h2", "L225.GlobalTechCost_h2", "L225.GlobalTechShrwt_h2", "L225.PrimaryRenewKeyword_h2", "L225.AvgFossilEffKeyword_h2", "L225.GlobalTechCapture_h2") ->
hydrogen.xml

    return_data(hydrogen.xml)
  } else {
    stop("Unknown command")
  }
}



