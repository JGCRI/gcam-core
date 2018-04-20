#' module_gcamusa_batch_building_USA_xml
#'
#' Construct XML data structure for \code{building_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_USA.xml}. The corresponding file in the
#' original data system was \code{batch_building_USA.xml} (gcamusa XML).
module_gcamusa_batch_building_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteConsumer_USAbld",
             "L244.DeleteSupplysector_USAbld",
             "L244.SubregionalShares",
             "L244.PriceExp_IntGains",
             "L244.Floorspace",
             "L244.DemandFunction_serv",
             "L244.DemandFunction_flsp",
             "L244.Satiation_flsp",
             "L244.SatiationAdder",
             "L244.ThermalBaseService",
             "L244.GenericBaseService",
             "L244.ThermalServiceSatiation",
             "L244.GenericServiceSatiation",
             "L244.Intgains_scalar",
             "L244.ShellConductance_bld",
             "L244.Supplysector_bld",
             "L244.FinalEnergyKeyword_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorInterpTo_bld",
             "L244.SubsectorLogit_bld",
             "L244.StubTech_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.GlobalTechSCurve_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_USAbld <- get_data(all_data, "L244.DeleteConsumer_USAbld")
    L244.DeleteSupplysector_USAbld <- get_data(all_data, "L244.DeleteSupplysector_USAbld")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld")
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld")
    L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld")

    # ===================================================

    # Produce outputs
    create_xml("building_USA.xml") %>%
      add_xml_data(L244.DeleteConsumer_USAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_USAbld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp" ) %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder" ) %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService" ) %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService" ) %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation" ) %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation" ) %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar" ) %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance" ) %>%
      add_xml_data(L244.Supplysector_bld, "Supplysector" ) %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword" ) %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt" ) %>%
      add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp" ) %>%
      add_xml_data(L244.SubsectorInterpTo_bld, "" ) %>%
      add_xml_data(L244.SubsectorLogit_bld, "SubsectorInterpTo" ) %>%
      add_xml_data(L244.StubTech_bld, "StubTech" ) %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput" ) %>%
      add_xml_data(L244.StubTechMarket_bld, "StubTechMarket" ) %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio" ) %>%
      add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo" ) %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff" ) %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt" ) %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost" ) %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve" ) %>%
      add_precursors("L244.DeleteConsumer_USAbld",
                     "L244.DeleteSupplysector_USAbld",
                     "L244.SubregionalShares",
                     "L244.PriceExp_IntGains",
                     "L244.Floorspace",
                     "L244.DemandFunction_serv",
                     "L244.DemandFunction_flsp",
                     "L244.Satiation_flsp",
                     "L244.SatiationAdder",
                     "L244.ThermalBaseService",
                     "L244.GenericBaseService",
                     "L244.ThermalServiceSatiation",
                     "L244.GenericServiceSatiation",
                     "L244.Intgains_scalar",
                     "L244.ShellConductance_bld",
                     "L244.Supplysector_bld",
                     "L244.FinalEnergyKeyword_bld",
                     "L244.SubsectorShrwtFllt_bld",
                     "L244.SubsectorInterp_bld",
                     "L244.SubsectorInterpTo_bld",
                     "L244.SubsectorLogit_bld",
                     "L244.StubTech_bld",
                     "L244.StubTechCalInput_bld",
                     "L244.StubTechMarket_bld",
                     "L244.GlobalTechIntGainOutputRatio",
                     "L244.GlobalTechInterpTo_bld",
                     "L244.GlobalTechEff_bld",
                     "L244.GlobalTechShrwt_bld",
                     "L244.GlobalTechCost_bld",
                     "L244.GlobalTechSCurve_bld") ->
      building_USA.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    # if (!is.null(L244.SubsectorShrwt_bld_gcamusa)){
    #   building_USA.xml %>%
    #     add_xml_data(L244.SubsectorShrwt_bld_gcamusa, "SubsectorShrwt") ->
    #     building_USA.xml
    # }
    # if (!is.null(L244.SubsectorShrwtFllt_bld_gcamusa)){
    #   building_USA.xml %>%
    #     add_xml_data(L244.SubsectorShrwtFllt_bld_gcamusa, "SubsectorShrwtFllt") ->
    #     building_USA.xml
    # }
    # if (!is.null(L244.SubsectorInterp_bld_gcamusa)){
    #   building_USA.xml %>%
    #     add_xml_data(L244.SubsectorInterp_bld_gcamusa, "SubsectorInterp") ->
    #     building_USA.xml
    # }
    # if (!is.null(L244.SubsectorInterpTo_bld_gcamusa)){
    #   building_USA.xml %>%
    #     add_xml_data(L244.SubsectorInterpTo_bld_gcamusa, "SubsectorInterp") ->
    #     building_USA.xml
    # }

    return_data(building_USA.xml)
  } else {
    stop("Unknown command")
  }
}
