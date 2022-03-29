# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_building_det_xml
#'
#' Construct XML data structure for \code{building_det.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_det.xml}. The corresponding file in the
#' original data system was \code{batch_building_det.xml} (energy XML).
module_energy_batch_building_det_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.SubsectorInterpTo_bld",
             "L244.SubsectorInterp_bld",
             "L244.SubsectorShrwtFllt_bld",
             "L244.SubsectorShrwt_bld",
             "L244.FinalEnergyKeyword_bld",
             "L244.Supplysector_bld",
             "L244.ShellConductance_bld",
             "L244.Intgains_scalar",
             "L244.GenericServiceSatiation",
             "L244.ThermalServiceSatiation",
             "L244.GenericBaseService",
             "L244.ThermalBaseService",
             "L244.SatiationAdder",
             "L244.Satiation_flsp",
             "L244.DemandFunction_flsp",
             "L244.DemandFunction_serv",
             "L244.Floorspace",
             "L244.PriceExp_IntGains",
             "L244.SubregionalShares",
             "L244.SubsectorLogit_bld",
             "L244.FuelPrefElast_bld",
             "L244.StubTech_bld",
             "L244.StubTechEff_bld",
             "L244.StubTechCalInput_bld",
             "L244.StubTechIntGainOutputRatio",
             "L244.GlobalTechShrwt_bld",
             "L244.GlobalTechCost_bld",
             "L244.DeleteThermalService",
             "L244.GompFnParam",
             "L244.DeleteGenericService"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_det.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld")
    L244.SubsectorShrwt_bld <- get_data(all_data, "L244.SubsectorShrwt_bld")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld")
    L244.FuelPrefElast_bld <- get_data(all_data, "L244.FuelPrefElast_bld")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld")
    L244.StubTechEff_bld <- get_data(all_data, "L244.StubTechEff_bld")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld")
    L244.StubTechIntGainOutputRatio <- get_data(all_data, "L244.StubTechIntGainOutputRatio")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld")
    L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService")
    L244.DeleteGenericService <- get_data(all_data, "L244.DeleteGenericService")
    L244.GompFnParam <- get_data(all_data, "L244.GompFnParam")

    # ===================================================

    # Produce outputs
    create_xml("building_det.xml") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance") %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.GompFnParam, "GompFnParam") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.SubregionalShares, "SubregionalShares") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.FuelPrefElast_bld, "FuelPrefElast") %>%
      add_xml_data(L244.StubTech_bld, "StubTech") %>%
      add_xml_data(L244.StubTechEff_bld, "StubTechEff") %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechIntGainOutputRatio, "StubTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_precursors("L244.SubsectorInterpTo_bld", "L244.SubsectorInterp_bld" , "L244.SubsectorShrwtFllt_bld",
                     "L244.SubsectorShrwt_bld", "L244.FinalEnergyKeyword_bld", "L244.Supplysector_bld",
                     "L244.ShellConductance_bld", "L244.Intgains_scalar", "L244.GenericServiceSatiation",
                     "L244.ThermalServiceSatiation", "L244.GenericBaseService", "L244.ThermalBaseService", "L244.SatiationAdder",
                     "L244.Satiation_flsp", "L244.GompFnParam",
                     "L244.DemandFunction_flsp", "L244.DemandFunction_serv",
                     "L244.Floorspace", "L244.SubregionalShares", "L244.SubsectorLogit_bld",
                     "L244.FuelPrefElast_bld", "L244.StubTech_bld", "L244.StubTechEff_bld",
                     "L244.StubTechCalInput_bld", "L244.StubTechIntGainOutputRatio", "L244.GlobalTechShrwt_bld",
                     "L244.GlobalTechCost_bld", "L244.DeleteThermalService", "L244.DeleteGenericService",
                     "L244.PriceExp_IntGains") ->
      building_det.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(nrow(L244.DeleteThermalService) > 0) {
      building_det.xml %>%
        add_xml_data(L244.DeleteThermalService, "DeleteThermalService") ->
        building_det.xml
    }

    if(!is.null(L244.DeleteGenericService)) {
      building_det.xml %>%
        add_xml_data(L244.DeleteGenericService, "DeleteGenericService") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorShrwt_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorShrwt_bld, "SubsectorShrwt") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorShrwtFllt_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorInterp_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp") ->
        building_det.xml
    }
    if(!is.null(L244.SubsectorInterpTo_bld)) {
      building_det.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld, "SubsectorInterp") ->
        building_det.xml
    }

    return_data(building_det.xml)
  } else {
    stop("Unknown command")
  }
}
