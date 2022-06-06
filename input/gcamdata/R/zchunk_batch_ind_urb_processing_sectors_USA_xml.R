#' module_gcamusa_batch_ind_urb_processing_sectors_USA_xml
#'
#' Construct XML data structure for \code{ind_urb_processing_sectors_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ind_urb_processing_sectors.xml}. The corresponding file in the
#' original data system was \code{batch_ind_urb_processing_sectors.xml} (emissions XML).
module_gcamusa_batch_ind_urb_processing_sectors_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L231.DeleteSupplysector_industry_USA",
             "L231.DeleteSupplysector_urban_processes_USA",
             "L231.DeleteFinalDemand_urban_processes_USA",
             "L231.UnlimitRsrc_USA",
             "L231.UnlimitRsrcPrice_USA",
             "L231.FinalDemand_urb_USA",
             "L231.Supplysector_urb_ind_USA",
             "L231.SubsectorLogit_urb_ind_USA",
             "L231.SubsectorShrwt_urb_ind_USA",
             "L231.SubsectorShrwtFllt_urb_ind_USA",
             "L231.SubsectorInterp_urb_ind_USA",
             "L231.SubsectorInterpTo_urb_ind_USA",
             "L231.StubTech_urb_ind_USA",
             "L231.GlobalTechShrwt_urb_ind",
             "L231.GlobalTechEff_urb_ind",
             "L231.GlobalTechCoef_urb_ind",
             "L231.GlobalTechCost_urb_ind",
             "L231.RegionalTechCalValue_urb_ind_USA",
             "L231.IndCoef_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ind_urb_processing_sectors_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L231.DeleteSupplysector_industry_USA <- get_data(all_data, "L231.DeleteSupplysector_industry_USA")
    L231.DeleteSupplysector_urban_processes_USA <- get_data(all_data, "L231.DeleteSupplysector_urban_processes_USA")
    L231.DeleteFinalDemand_urban_processes_USA <- get_data(all_data, "L231.DeleteFinalDemand_urban_processes_USA")
    L231.UnlimitRsrc_USA <- get_data(all_data,"L231.UnlimitRsrc_USA")
    L231.UnlimitRsrcPrice_USA <- get_data(all_data, "L231.UnlimitRsrcPrice_USA")
    L231.FinalDemand_urb_USA <- get_data(all_data, "L231.FinalDemand_urb_USA")
    L231.Supplysector_urb_ind_USA <- get_data(all_data, "L231.Supplysector_urb_ind_USA")
    L231.SubsectorLogit_urb_ind_USA <- get_data(all_data, "L231.SubsectorLogit_urb_ind_USA")
    L231.SubsectorShrwt_urb_ind_USA <- get_data(all_data, "L231.SubsectorShrwt_urb_ind_USA")
    L231.SubsectorShrwtFllt_urb_ind_USA <- get_data(all_data, "L231.SubsectorShrwtFllt_urb_ind_USA")
    L231.SubsectorInterp_urb_ind_USA <- get_data(all_data, "L231.SubsectorInterp_urb_ind_USA")
    L231.SubsectorInterpTo_urb_ind_USA <- get_data(all_data, "L231.SubsectorInterpTo_urb_ind_USA")
    L231.GlobalTechShrwt_urb_ind <- get_data(all_data, "L231.GlobalTechShrwt_urb_ind")
    L231.GlobalTechEff_urb_ind <- get_data(all_data, "L231.GlobalTechEff_urb_ind")
    L231.GlobalTechCoef_urb_ind <- get_data(all_data, "L231.GlobalTechCoef_urb_ind")
    L231.GlobalTechCost_urb_ind <- get_data(all_data, "L231.GlobalTechCost_urb_ind")
    L231.StubTech_urb_ind_USA <- get_data(all_data, "L231.StubTech_urb_ind_USA")
    L231.RegionalTechCalValue_urb_ind_USA <- get_data(all_data, "L231.RegionalTechCalValue_urb_ind_USA")
    L231.IndCoef_USA <- get_data(all_data, "L231.IndCoef_USA")
    # ===================================================

    # Produce outputs
    create_xml("ind_urb_processing_sectors_USA.xml") %>%
      add_xml_data(L231.DeleteSupplysector_industry_USA, "DeleteSupplysector") %>%
      add_xml_data(L231.DeleteSupplysector_urban_processes_USA, "DeleteSupplysector") %>%
      add_xml_data(L231.DeleteFinalDemand_urban_processes_USA, "DeleteFinalDemand") %>%
      add_xml_data(L231.UnlimitRsrc_USA, "UnlimitRsrc") %>%
      add_xml_data(L231.UnlimitRsrcPrice_USA, "UnlimitRsrcPrice") %>%
      add_xml_data(L231.FinalDemand_urb_USA, "FinalDemandInfo") %>%
      add_xml_data(L231.StubTech_urb_ind_USA, "StubTech") %>%
      add_xml_data(L231.RegionalTechCalValue_urb_ind_USA, "StubTechCalInputIndUrb") %>%
      add_xml_data(L231.IndCoef_USA, "StubTechCoefIndUrb") %>%
      add_xml_data(L231.GlobalTechShrwt_urb_ind, "GlobalTechShrwt") %>%
      add_xml_data(L231.GlobalTechEff_urb_ind, "GlobalTechEff") %>%
      add_xml_data(L231.GlobalTechCoef_urb_ind, "GlobalTechCoef") %>%
      add_xml_data(L231.GlobalTechCost_urb_ind, "GlobalTechCost") %>%
      add_logit_tables_xml(L231.Supplysector_urb_ind_USA, "Supplysector") %>%
      add_logit_tables_xml(L231.SubsectorLogit_urb_ind_USA, "SubsectorLogit") %>%
      add_precursors("L231.DeleteSupplysector_industry_USA",
                     "L231.DeleteSupplysector_urban_processes_USA",
                     "L231.DeleteFinalDemand_urban_processes_USA",
                     "L231.UnlimitRsrc_USA",
                     "L231.UnlimitRsrcPrice_USA",
                     "L231.FinalDemand_urb_USA",
                     "L231.Supplysector_urb_ind_USA",
                     "L231.SubsectorLogit_urb_ind_USA",
                     "L231.SubsectorShrwt_urb_ind_USA",
                     "L231.SubsectorShrwtFllt_urb_ind_USA",
                     "L231.SubsectorInterp_urb_ind_USA",
                     "L231.SubsectorInterpTo_urb_ind_USA",
                     "L231.StubTech_urb_ind_USA",
                     "L231.GlobalTechShrwt_urb_ind",
                     "L231.GlobalTechEff_urb_ind",
                     "L231.GlobalTechCoef_urb_ind",
                     "L231.GlobalTechCost_urb_ind",
                     "L231.RegionalTechCalValue_urb_ind_USA",
                     "L231.IndCoef_USA")->
      ind_urb_processing_sectors_USA.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(!is.null(L231.SubsectorShrwt_urb_ind_USA)) {
      ind_urb_processing_sectors_USA.xml <- ind_urb_processing_sectors_USA.xml %>%
        add_xml_data(L231.SubsectorShrwt_urb_ind_USA, "SubsectorShrwt")
    }
    if(!is.null(L231.SubsectorShrwtFllt_urb_ind_USA)) {
      ind_urb_processing_sectors_USA.xml <- ind_urb_processing_sectors_USA.xml %>%
        add_xml_data(L231.SubsectorShrwtFllt_urb_ind_USA, "SubsectorShrwtFllt")
    }
    if(!is.null(L231.SubsectorInterp_urb_ind_USA)) {
      ind_urb_processing_sectors_USA.xml <- ind_urb_processing_sectors_USA.xml %>%
        add_xml_data(L231.SubsectorInterp_urb_ind_USA, "SubsectorInterp")
    }
    if(!is.null(L231.SubsectorInterpTo_urb_ind_USA)) {
      ind_urb_processing_sectors_USA.xml <- ind_urb_processing_sectors_USA.xml %>%
        add_xml_data(L231.SubsectorInterpTo_urb_ind_USA, "SubsectorInterpTo")
    }

    return_data(ind_urb_processing_sectors_USA.xml)
  } else {
    stop("Unknown command")
  }
}
