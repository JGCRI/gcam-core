#' module_water_batch_electricity_water.xml
#'
#' Construct XML data structure for \code{electricity_water.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
#module_water_batch_electricity_water.xml_DISABLED <- function(command, ...) {
module_water_batch_electricity_water.xml <- function(command, ...) {
    if(command == driver.DECLARE_INPUTS) {
    return(c( "L2233.StubTech_elecPassthru",
              "L2233.StubTechProd_elecPassthru",
              "L2233.GlobalPassThroughTech",
              "L2233.GlobalTechEff_elecPassthru",
              "L2233.GlobalTechShrwt_elecPassthru",
              "L2233.GlobalIntTechCapital_elec",
              "L2233.GlobalTechCapital_elecPassthru",
              "L2233.GlobalIntTechOMfixed_elec",
              "L2233.GlobalTechOMfixed_elecPassthru",
              "L2233.GlobalIntTechOMvar_elec",
              "L2233.GlobalTechOMvar_elecPassthru",
              "L2233.PassThroughSector_elec_cool",
              "L2233.Supplysector_elec_cool",
              "L2233.ElecReserve_elec_cool",
              "L2233.SubsectorShrwtFllt_elec_cool",
              "L2233.SubsectorLogit_elec_cool",
              "L2233.StubTech_elec_cool",
              "L2233.StubTechEff_elec_cool",
              "L2233.StubTechProd_elec_cool",
              "L2233.StubTechFixOut_hydro",
              "L2233.StubTechShrwt_elec_cool",
              "L2233.GlobalTechCapital_elec_cool",
              "L2233.GlobalIntTechCapital_elec_cool"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.StubTech_elecPassthru <- get_data(all_data, "L2233.StubTech_elecPassthru")
    L2233.StubTechProd_elecPassthru <- get_data(all_data, "L2233.StubTechProd_elecPassthru")
    L2233.GlobalPassThroughTech <- get_data(all_data, "L2233.GlobalPassThroughTech")
    L2233.GlobalTechEff_elecPassthru <- get_data(all_data, "L2233.GlobalTechEff_elecPassthru")
    L2233.GlobalTechShrwt_elecPassthru <- get_data(all_data, "L2233.GlobalTechShrwt_elecPassthru")
    L2233.GlobalIntTechCapital_elec <- get_data(all_data, "L2233.GlobalIntTechCapital_elec")
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru")
    L2233.GlobalIntTechOMfixed_elec <- get_data(all_data, "L2233.GlobalIntTechOMfixed_elec")
    L2233.GlobalTechOMfixed_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMfixed_elecPassthru")
    L2233.GlobalIntTechOMvar_elec <- get_data(all_data, "L2233.GlobalIntTechOMvar_elec")
    L2233.GlobalTechOMvar_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMvar_elecPassthru")
    L2233.PassThroughSector_elec_cool <- get_data(all_data, "L2233.PassThroughSector_elec_cool")
    L2233.Supplysector_elec_cool <- get_data(all_data, "L2233.Supplysector_elec_cool")
    L2233.ElecReserve_elec_cool <- get_data(all_data, "L2233.ElecReserve_elec_cool")
    L2233.SubsectorShrwtFllt_elec_cool <- get_data(all_data, "L2233.SubsectorShrwtFllt_elec_cool")
    L2233.SubsectorLogit_elec_cool <- get_data(all_data, "L2233.SubsectorLogit_elec_cool")
    L2233.StubTech_elec_cool <- get_data(all_data, "L2233.StubTech_elec_cool")
    L2233.StubTechEff_elec_cool <- get_data(all_data, "L2233.StubTechEff_elec_cool")
    L2233.StubTechProd_elec_cool <- get_data(all_data, "L2233.StubTechProd_elec_cool")
    L2233.StubTechFixOut_hydro <- get_data(all_data, "L2233.StubTechFixOut_hydro")
    L2233.StubTechShrwt_elec_cool <- get_data(all_data, "L2233.StubTechShrwt_elec_cool")
    L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool")
    L2233.GlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapital_elec_cool")

    # ===================================================

    # Produce outputs
    create_xml("electricity_water.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2233.StubTech_elecPassthru,"StubTech") %>%
      add_xml_data(L2233.StubTechProd_elecPassthru,"StubTechProd") %>%
      add_xml_data(L2233.GlobalPassThroughTech,"GlobalPassThroughTech") %>%
      add_xml_data(L2233.GlobalTechEff_elecPassthru,"GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechShrwt_elecPassthru,"GlobalTechShrwt") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elec,"GlobalIntTechCapital","GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechCapital_elecPassthru,"GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elec,"GlobalIntTechOMfixed","GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecPassthru,"GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elec,"GlobalIntTechOMvar","GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecPassthru,"GlobalTechOMvar") %>%
      add_xml_data(L2233.PassThroughSector_elec_cool,"PassThroughSector") %>%
      add_xml_data(L2233.Supplysector_elec_cool,"Supplysector") %>%
      add_xml_data(L2233.ElecReserve_elec_cool,"ElecReserve") %>%
      add_xml_data(L2233.SubsectorShrwtFllt_elec_cool,"SubsectorShrwtFllt") %>%
      add_xml_data(L2233.SubsectorLogit_elec_cool,"SubsectorLogit") %>%
      add_xml_data(L2233.StubTech_elec_cool,"StubTech") %>%
      add_xml_data(L2233.StubTechEff_elec_cool,"StubTechEff") %>%
      add_xml_data(L2233.StubTechProd_elec_cool,"StubTechProd") %>%
      add_xml_data(L2233.StubTechFixOut_hydro,"StubTechFixOut") %>%
      add_xml_data(L2233.StubTechShrwt_elec_cool,"StubTechShrwt") %>%
      add_xml_data(L2233.GlobalTechCapital_elec_cool,"GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elec_cool,"GlobalIntTechCapital","GlobalTechCapital") %>%
      add_precursors("L2233.StubTech_elecPassthru",
                     "L2233.StubTechProd_elecPassthru", 
                     "L2233.GlobalPassThroughTech", 
                     "L2233.GlobalTechEff_elecPassthru", 
                     "L2233.GlobalTechShrwt_elecPassthru", 
                     "L2233.GlobalIntTechCapital_elec", 
                     "L2233.GlobalTechCapital_elecPassthru", 
                     "L2233.GlobalIntTechOMfixed_elec", 
                     "L2233.GlobalTechOMfixed_elecPassthru", 
                     "L2233.GlobalIntTechOMvar_elec", 
                     "L2233.GlobalTechOMvar_elecPassthru", 
                     "L2233.PassThroughSector_elec_cool", 
                     "L2233.Supplysector_elec_cool", 
                     "L2233.ElecReserve_elec_cool", 
                     "L2233.SubsectorShrwtFllt_elec_cool", 
                     "L2233.SubsectorLogit_elec_cool", 
                     "L2233.StubTech_elec_cool", 
                     "L2233.StubTechEff_elec_cool", 
                     "L2233.StubTechProd_elec_cool", 
                     "L2233.StubTechFixOut_hydro", 
                     "L2233.StubTechShrwt_elec_cool", 
                     "L2233.GlobalTechCapital_elec_cool", 
                     "L2233.GlobalIntTechCapital_elec_cool") ->
      electricity_water.xml

    return_data(electricity_water.xml)
  } else {
    stop("Unknown command")
  }
}
