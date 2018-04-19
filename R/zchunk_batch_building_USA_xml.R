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
             "L244.SubregionalShares_gcamusa",
             "L244.PriceExp_IntGains_gcamusa",
             "L244.Floorspace_gcamusa",
             "L244.DemandFunction_flsp_gcamusa",
             "L244.DemandFunction_serv_gcamusa",
             "L244.Satiation_flsp_gcamusa",
             "L244.SatiationAdder_gcamusa",
             "L244.ThermalBaseService_gcamusa",
             "L244.GenericBaseService_gcamusa",
             "L244.ThermalServiceSatiation_gcamusa",
             "L244.GenericServiceSatiation_gcamusa",
             "L244.Intgains_scalar_gcamusa",
             "L244.ShellConductance_bld_gcamusa",
             "L244.Supplysector_bld_gcamusa",
             "L244.FinalEnergyKeyword_bld_gcamusa",
             "L244.SubsectorLogit_bld_gcamusa",
             "L244.StubTech_bld_gcamusa",
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechShrwt_bld_gcamusa",
             "L244.GlobalTechCost_bld_gcamusa",
             "L244.GlobalTechIntGainOutputRatio",
             "L244.GlobalTechInterpTo_bld",
             "L244.GlobalTechEff_bld",
             "L244.GlobalTechSCurve_bld",
             "L244.SubsectorShrwt_bld_gcamusa",
             "L244.SubsectorShrwtFllt_bld_gcamusa",
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_USAbld <- get_data(all_data, "L244.DeleteConsumer_USAbld")
    L244.DeleteSupplysector_USAbld <- get_data(all_data, "L244.DeleteSupplysector_USAbld")
    L244.SubregionalShares_gcamusa <- get_data(all_data, "L244.SubregionalShares_gcamusa")
    L244.PriceExp_IntGains_gcamusa <- get_data(all_data, "L244.PriceExp_IntGains_gcamusa")
    L244.Floorspace_gcamusa <- get_data(all_data, "L244.Floorspace_gcamusa")
    L244.DemandFunction_flsp_gcamusa <- get_data(all_data, "L244.DemandFunction_flsp_gcamusa")
    L244.DemandFunction_serv_gcamusa <- get_data(all_data, "L244.DemandFunction_serv_gcamusa")
    L244.Satiation_flsp_gcamusa <- get_data(all_data, "L244.Satiation_flsp_gcamusa")
    L244.SatiationAdder_gcamusa <- get_data(all_data, "L244.SatiationAdder_gcamusa")
    L244.ThermalBaseService_gcamusa <- get_data(all_data, "L244.ThermalBaseService_gcamusa")
    L244.GenericBaseService_gcamusa <- get_data(all_data, "L244.GenericBaseService_gcamusa")
    L244.ThermalServiceSatiation_gcamusa <- get_data(all_data, "L244.ThermalServiceSatiation_gcamusa")
    L244.GenericServiceSatiation_gcamusa <- get_data(all_data, "L244.GenericServiceSatiation_gcamusa")
    L244.Intgains_scalar_gcamusa <- get_data(all_data, "L244.Intgains_scalar_gcamusa")
    L244.ShellConductance_bld_gcamusa <- get_data(all_data, "L244.ShellConductance_bld_gcamusa")
    L244.Supplysector_bld_gcamusa <- get_data(all_data, "L244.Supplysector_bld_gcamusa")
    L244.FinalEnergyKeyword_bld_gcamusa <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamusa")
    L244.SubsectorLogit_bld_gcamusa <- get_data(all_data, "L244.SubsectorLogit_bld_gcamusa")
    L244.StubTech_bld_gcamusa <- get_data(all_data, "L244.StubTech_bld_gcamusa")
    L244.StubTechCalInput_bld_gcamusa <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa")
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld")
    L244.GlobalTechShrwt_bld_gcamusa <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamusa")
    L244.GlobalTechCost_bld_gcamusa <- get_data(all_data, "L244.GlobalTechCost_bld_gcamusa")
    L244.GlobalTechIntGainOutputRatio <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld")
    L244.SubsectorShrwt_bld_gcamusa <- get_data(all_data, "L244.SubsectorShrwt_bld_gcamusa")
    L244.SubsectorShrwtFllt_bld_gcamusa <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamusa")
    L244.SubsectorInterp_bld_gcamusa <- get_data(all_data, "L244.SubsectorInterp_bld_gcamusa")
    L244.SubsectorInterpTo_bld_gcamusa <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamusa")

    # ===================================================

    # Produce outputs
    create_xml("building_USA.xml") %>%
      add_xml_data(L244.DeleteConsumer_USAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_USAbld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares_gcamusa, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains_gcamusa, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace_gcamusa, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_flsp_gcamusa, "DemandFunction_flsp") %>%
      add_xml_data(L244.DemandFunction_serv_gcamusa, "DemandFunction_serv") %>%
      add_xml_data(L244.Satiation_flsp_gcamusa, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder_gcamusa, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService_gcamusa, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService_gcamusa, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation_gcamusa, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation_gcamusa, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar_gcamusa, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld_gcamusa, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld_gcamusa, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld_gcamusa, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld_gcamusa,"SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld_gcamusa, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld_gcamusa, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechShrwt_bld_gcamusa, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld_gcamusa, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve") %>%
      add_precursors("L244.DeleteConsumer_USAbld", "L244.DeleteSupplysector_USAbld", "L244.SubregionalShares_gcamusa",
                     "L244.PriceExp_IntGains_gcamusa", "L244.Floorspace_gcamusa", "L244.DemandFunction_flsp_gcamusa",
                     "L244.DemandFunction_serv_gcamusa", "L244.Satiation_flsp_gcamusa", "L244.SatiationAdder_gcamusa",
                     "L244.ThermalBaseService_gcamusa", "L244.GenericBaseService_gcamusa", "L244.ThermalServiceSatiation_gcamusa",
                     "L244.GenericServiceSatiation_gcamusa", "L244.Intgains_scalar_gcamusa", "L244.ShellConductance_bld_gcamusa",
                     "L244.Supplysector_bld_gcamusa", "L244.FinalEnergyKeyword_bld_gcamusa", "L244.SubsectorLogit_bld_gcamusa",
                     "L244.StubTech_bld_gcamusa", "L244.StubTechCalInput_bld_gcamusa", "L244.StubTechMarket_bld",
                     "L244.GlobalTechShrwt_bld_gcamusa", "L244.GlobalTechCost_bld_gcamusa", "L244.GlobalTechIntGainOutputRatio",
                     "L244.GlobalTechInterpTo_bld", "L244.GlobalTechEff_bld", "L244.GlobalTechSCurve_bld", "L244.SubsectorShrwt_bld_gcamusa",
                     "L244.SubsectorShrwtFllt_bld_gcamusa", "L244.SubsectorInterp_bld_gcamusa", "L244.SubsectorInterpTo_bld_gcamusa") ->
      building_USA.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if (!is.null(L244.SubsectorShrwt_bld_gcamusa)){
      building_USA.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld_gcamusa, "SubsectorShrwt") ->
        building_USA.xml
    }
    if (!is.null(L244.SubsectorShrwtFllt_bld_gcamusa)){
      building_USA.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld_gcamusa, "SubsectorShrwtFllt") ->
        building_USA.xml
    }
    if (!is.null(L244.SubsectorInterp_bld_gcamusa)){
      building_USA.xml %>%
        add_xml_data(L244.SubsectorInterp_bld_gcamusa, "SubsectorInterp") ->
        building_USA.xml
    }
    if (!is.null(L244.SubsectorInterpTo_bld_gcamusa)){
      building_USA.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld_gcamusa, "SubsectorInterp") ->
        building_USA.xml
    }

    return_data(building_USA.xml)
  } else {
    stop("Unknown command")
  }
}
