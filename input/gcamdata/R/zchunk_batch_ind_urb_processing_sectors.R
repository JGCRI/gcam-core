# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_ind_urb_processing_sectors_xml
#'
#' Construct XML data structure for \code{ind_urb_processing_sectors.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ind_urb_processing_sectors.xml}, \code{ind_urb_processing_sectors_MAC.xml}.
#' The corresponding file in the original data system was
#' \code{batch_ind_urb_processing_sectors.xml} (emissions XML).
module_emissions_batch_ind_urb_processing_sectors_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L231.UnlimitRsrc",
             "L231.UnlimitRsrcPrice",
             "L231.FinalDemand_urb",
             "L231.Supplysector_urb_ind",
             "L231.SubsectorLogit_urb_ind",
             "L231.SubsectorShrwt_urb_ind",
             "L231.SubsectorShrwtFllt_urb_ind",
             "L231.SubsectorInterp_urb_ind",
             "L231.SubsectorInterpTo_urb_ind",
             "L231.StubTech_urb_ind",
             "L231.GlobalTechShrwt_urb_ind",
             "L231.GlobalTechEff_urb_ind",
             "L231.GlobalTechCoef_urb_ind",
             "L231.GlobalTechCost_urb_ind",
             "L231.RegionalTechCalValue_urb_ind",
             "L231.IndCoef",
             "L252.MAC_prc",
             "L252.MAC_prc_phaseInTime",
             "L252.MAC_prc_tc_average"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ind_urb_processing_sectors.xml",
             XML = "ind_urb_processing_sectors_MAC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- NULL #Silence package check

    # Load required inputs
    L231.UnlimitRsrc <- get_data(all_data, "L231.UnlimitRsrc")
    L231.UnlimitRsrcPrice <- get_data(all_data, "L231.UnlimitRsrcPrice")
    L231.FinalDemand_urb <- get_data(all_data, "L231.FinalDemand_urb")
    L231.Supplysector_urb_ind <- get_data(all_data, "L231.Supplysector_urb_ind")
    L231.SubsectorLogit_urb_ind <- get_data(all_data, "L231.SubsectorLogit_urb_ind")
    L231.SubsectorShrwt_urb_ind <- get_data(all_data, "L231.SubsectorShrwt_urb_ind")
    L231.SubsectorShrwtFllt_urb_ind <- get_data(all_data, "L231.SubsectorShrwtFllt_urb_ind")
    L231.SubsectorInterp_urb_ind <- get_data(all_data, "L231.SubsectorInterp_urb_ind")
    L231.SubsectorInterpTo_urb_ind <- get_data(all_data, "L231.SubsectorInterpTo_urb_ind")
    L231.StubTech_urb_ind <- get_data(all_data, "L231.StubTech_urb_ind")
    L231.GlobalTechShrwt_urb_ind <- get_data(all_data, "L231.GlobalTechShrwt_urb_ind")
    L231.GlobalTechEff_urb_ind <- get_data(all_data, "L231.GlobalTechEff_urb_ind")
    L231.GlobalTechCoef_urb_ind <- get_data(all_data, "L231.GlobalTechCoef_urb_ind")
    L231.GlobalTechCost_urb_ind <- get_data(all_data, "L231.GlobalTechCost_urb_ind")
    L231.RegionalTechCalValue_urb_ind <- get_data(all_data, "L231.RegionalTechCalValue_urb_ind")
    L231.IndCoef <- get_data(all_data, "L231.IndCoef")
    L252.MAC_prc <- get_data(all_data, "L252.MAC_prc")
    L252.MAC_prc_phaseInTime <- get_data(all_data, "L252.MAC_prc_phaseInTime")
    L252.MAC_prc_tc_average <- get_data(all_data, "L252.MAC_prc_tc_average")
    # ===================================================

    # Produce outputs
    create_xml("ind_urb_processing_sectors.xml") %>%
      add_xml_data(L231.UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L231.UnlimitRsrcPrice, "UnlimitRsrcPrice") %>%
      add_xml_data(L231.FinalDemand_urb, "FinalDemandInfo") %>%
      add_xml_data(L231.StubTech_urb_ind, "StubTech") %>%
      add_xml_data(L231.GlobalTechShrwt_urb_ind, "GlobalTechShrwt") %>%
      add_xml_data(L231.GlobalTechEff_urb_ind, "GlobalTechEff") %>%
      add_xml_data(L231.GlobalTechCoef_urb_ind, "GlobalTechCoef") %>%
      add_xml_data(L231.GlobalTechCost_urb_ind, "GlobalTechCost") %>%
      add_xml_data(L231.RegionalTechCalValue_urb_ind, "StubTechCalInputIndUrb") %>%
      add_xml_data(L231.IndCoef, "StubTechCoefIndUrb") %>%
      add_logit_tables_xml(L231.Supplysector_urb_ind, "Supplysector") %>%
      add_logit_tables_xml(L231.SubsectorLogit_urb_ind, "SubsectorLogit") %>%
      add_precursors("L231.UnlimitRsrc", "L231.UnlimitRsrcPrice", "L231.FinalDemand_urb", "L231.Supplysector_urb_ind", "L231.SubsectorLogit_urb_ind",
                     "L231.SubsectorShrwt_urb_ind", "L231.SubsectorShrwtFllt_urb_ind", "L231.SubsectorInterp_urb_ind", "L231.SubsectorInterpTo_urb_ind",
                     "L231.StubTech_urb_ind", "L231.GlobalTechShrwt_urb_ind", "L231.GlobalTechEff_urb_ind", "L231.GlobalTechCoef_urb_ind",
                     "L231.GlobalTechCost_urb_ind", "L231.RegionalTechCalValue_urb_ind", "L231.IndCoef")->
      ind_urb_processing_sectors.xml

    create_xml("ind_urb_processing_sectors_MAC.xml") %>%
      add_xml_data(L252.MAC_prc, "MAC") %>%
      add_xml_data(L252.MAC_prc_tc_average, "MACTC") %>%
      add_xml_data(L252.MAC_prc_phaseInTime, "MACPhaseIn") %>%
      add_precursors("L252.MAC_prc", "L252.MAC_prc_tc_average", "L252.MAC_prc_phaseInTime") ->
      ind_urb_processing_sectors_MAC.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(!is.null(L231.SubsectorShrwt_urb_ind)) {
      ind_urb_processing_sectors.xml <- ind_urb_processing_sectors.xml %>%
        add_xml_data(L231.SubsectorShrwt_urb_ind, "SubsectorShrwt")
    }
    if(!is.null(L231.SubsectorShrwtFllt_urb_ind)) {
      ind_urb_processing_sectors.xml <- ind_urb_processing_sectors.xml %>%
        add_xml_data(L231.SubsectorShrwtFllt_urb_ind, "SubsectorShrwtFllt")
    }
    if(!is.null(L231.SubsectorInterp_urb_ind)) {
      ind_urb_processing_sectors.xml <- ind_urb_processing_sectors.xml %>%
        add_xml_data(L231.SubsectorInterp_urb_ind, "SubsectorInterp")
    }
    if(!is.null(L231.SubsectorInterpTo_urb_ind)) {
      ind_urb_processing_sectors.xml <- ind_urb_processing_sectors.xml %>%
        add_xml_data(L231.SubsectorInterpTo_urb_ind, "SubsectorInterpTo")
    }

    return_data(ind_urb_processing_sectors.xml,
                ind_urb_processing_sectors_MAC.xml)
  } else {
    stop("Unknown command")
  }
}
