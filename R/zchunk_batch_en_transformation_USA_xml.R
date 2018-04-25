#' module_gcamusa_batch_en_transformation_USA_xml
#'
#' Construct XML data structure for \code{en_transformation_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_USA.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_en_transformation_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.DeleteStubTech_USAen",
             "L222.PassThroughSector_USAen",
             "L222.SubsectorLogit_en_USA",
             "L222.StubTech_en_USA",
             "L222.StubTechCoef_refining_USA",
             "L222.GlobalTechInterp_en_USA",
             "L222.GlobalTechCoef_en_USA",
             "L222.GlobalTechCost_en_USA",
             "L222.GlobalTechShrwt_en_USA",
             "L222.GlobalTechCapture_en_USA",
             "L222.GlobalTechSCurve_en_USA",
             "L222.Tech_USAen",
             "L222.TechShrwt_USAen",
             "L222.TechInterp_USAen",
             "L222.TechShrwt_USAen",
             "L222.TechCoef_USAen",
             "L222.Production_USArefining",
             "L222.Supplysector_en_USA",
             "L222.SubsectorShrwtFllt_en_USA",
             "L222.StubTechProd_refining_USA",
             "L222.StubTechMarket_en_USA",
             "L222.CarbonCoef_en_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.DeleteStubTech_USAen <- get_data(all_data, "L222.DeleteStubTech_USAen")
    L222.PassThroughSector_USAen <- get_data(all_data, "L222.PassThroughSector_USAen")
    L222.SubsectorLogit_en_USA <- get_data(all_data, "L222.SubsectorLogit_en_USA")
    L222.StubTech_en_USA <- get_data(all_data, "L222.StubTech_en_USA")
    L222.StubTechCoef_refining_USA <- get_data(all_data, "L222.StubTechCoef_refining_USA")
    L222.GlobalTechInterp_en_USA <- get_data(all_data, "L222.GlobalTechInterp_en_USA")
    L222.GlobalTechCoef_en_USA <- get_data(all_data, "L222.GlobalTechCoef_en_USA")
    L222.GlobalTechCost_en_USA <- get_data(all_data, "L222.GlobalTechCost_en_USA")
    L222.GlobalTechShrwt_en_USA <- get_data(all_data, "L222.GlobalTechShrwt_en_USA")
    L222.GlobalTechCapture_en_USA <- get_data(all_data, "L222.GlobalTechCapture_en_USA")
    L222.GlobalTechSCurve_en_USA <- get_data(all_data, "L222.GlobalTechSCurve_en_USA")
    L222.Tech_USAen <- get_data(all_data, "L222.Tech_USAen")
    L222.TechShrwt_USAen <- get_data(all_data, "L222.TechShrwt_USAen")
    L222.TechInterp_USAen <- get_data(all_data, "L222.TechInterp_USAen")
    L222.TechShrwt_USAen <- get_data(all_data, "L222.TechShrwt_USAen")
    L222.TechCoef_USAen <- get_data(all_data, "L222.TechCoef_USAen")
    L222.Production_USArefining <- get_data(all_data, "L222.Production_USArefining")
    L222.Supplysector_en_USA <- get_data(all_data, "L222.Supplysector_en_USA")
    L222.SubsectorShrwtFllt_en_USA <- get_data(all_data, "L222.SubsectorShrwtFllt_en_USA")
    L222.StubTechProd_refining_USA <- get_data(all_data, "L222.StubTechProd_refining_USA")
    L222.StubTechMarket_en_USA <- get_data(all_data, "L222.StubTechMarket_en_USA")
    L222.CarbonCoef_en_USA <- get_data(all_data, "L222.CarbonCoef_en_USA")

    # ===================================================
    # Rename tibble columns to match the header information.
    L222.Tech_USAen <- rename(L222.Tech_USAen, pass.through.technology = technology)
    L222.SubsectorShrwtFllt_en_USA <- rename(L222.SubsectorShrwtFllt_en_USA, year.fillout = year)

    # Produce outputs
    create_xml("en_transformation_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L222.DeleteStubTech_USAen,"DeleteStubTech") %>%
      add_xml_data(L222.PassThroughSector_USAen,"PassThroughSector") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en_USA, "SubsectorLogit") %>%
      add_xml_data(L222.StubTech_en_USA, "StubTech") %>%
      add_xml_data(L222.StubTechCoef_refining_USA, "StubTechCoef") %>%
      add_xml_data(L222.GlobalTechInterp_en_USA, "GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en_USA, "GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechCost_en_USA, "GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en_USA, "GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en_USA, "GlobalTechCapture") %>%
      add_xml_data(L222.GlobalTechSCurve_en_USA, "GlobalTechSCurve") %>%
      add_xml_data(L222.Tech_USAen,"PassThroughTech") %>%
      add_xml_data(L222.TechInterp_USAen,"TechInterp") %>%
      add_xml_data(L222.TechShrwt_USAen,"TechShrwt") %>%
      add_xml_data(L222.TechShrwt_USAen,"TechShrwt") %>%
      add_xml_data(L222.TechCoef_USAen,"TechCoef") %>%
      add_xml_data(L222.Production_USArefining,"Production") %>%
      add_logit_tables_xml(L222.Supplysector_en_USA,"Supplysector") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en_USA,"SubsectorShrwtFllt") %>%
      add_xml_data(L222.StubTechProd_refining_USA,"StubTechProd") %>%
      add_xml_data(L222.StubTechMarket_en_USA,"StubTechMarket") %>%
      add_xml_data(L222.CarbonCoef_en_USA,"CarbonCoef") %>%
      add_precursors("L222.DeleteStubTech_USAen",
                     "L222.PassThroughSector_USAen",
                     "L222.SubsectorLogit_en_USA",
                     "L222.StubTech_en_USA",
                     "L222.StubTechCoef_refining_USA",
                     "L222.GlobalTechInterp_en_USA",
                     "L222.GlobalTechCoef_en_USA",
                     "L222.GlobalTechCost_en_USA",
                     "L222.GlobalTechShrwt_en_USA",
                     "L222.GlobalTechCapture_en_USA",
                     "L222.GlobalTechSCurve_en_USA",
                     "L222.Tech_USAen",
                     "L222.TechShrwt_USAen",
                     "L222.TechInterp_USAen",
                     "L222.TechShrwt_USAen",
                     "L222.TechCoef_USAen",
                     "L222.Production_USArefining",
                     "L222.Supplysector_en_USA",
                     "L222.SubsectorShrwtFllt_en_USA",
                     "L222.StubTechProd_refining_USA",
                     "L222.StubTechMarket_en_USA",
                     "L222.CarbonCoef_en_USA") ->
      en_transformation_USA.xml

    return_data(en_transformation_USA.xml)
  } else {
    stop("Unknown command")
  }
}
