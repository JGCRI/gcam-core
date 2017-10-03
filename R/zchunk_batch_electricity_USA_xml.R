#' module_gcamusa_batch_electricity_USA_xml
#'
#' Construct XML data structure for \code{electricity_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_USA.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_electricity_USA_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.DeleteSubsector_USAelec",
              "L223.SectorNodeEquiv",
              "L223.TechNodeEquiv",
              "L223.PassThroughSector_elec",
              "L223.PassThroughTech_elec_FERC",
              "L223.Supplysector_USAelec",
              "L223.SubsectorShrwtFllt_USAelec",
              "L223.SubsectorInterp_USAelec",
              "L223.SubsectorLogit_USAelec",
              "L223.TechShrwt_USAelec",
              "L223.TechCoef_USAelec",
              "L223.Production_USAelec",
              "L223.Supplysector_elec_FERC",
              "L223.SubsectorShrwtFllt_elec_FERC",
              "L223.SubsectorInterp_elec_FERC",
              "L223.SubsectorLogit_elec_FERC",
              "L223.TechShrwt_elec_FERC",
              "L223.TechCoef_elec_FERC",
              "L223.Production_elec_FERC",
              "L223.InterestRate_FERC",
              "L223.Pop_FERC",
              "L223.BaseGDP_FERC",
              "L223.LaborForceFillout_FERC",
              "L223.SubsectorShrwt_nuc_USA",
              "L223.StubTechFixOut_elec_USA",
              "L223.StubTechFixOut_hydro_USA",
              "L223.StubTechProd_elec_USA",
              "L223.StubTechMarket_elec_USA",
              "L223.StubTechMarket_backup_USA",
              "L223.StubTechElecMarket_backup_USA",
              "L223.StubTechCapFactor_elec_wind_USA",
              "L223.StubTechCapFactor_elec_solar_USA",
              "L2232.DeleteSupplysector_USAelec",
              "L2232.Supplysector_USAelec",
              "L2232.SubsectorShrwtFllt_USAelec",
              "L2232.SubsectorInterp_USAelec",
              "L2232.SubsectorLogit_USAelec",
              "L2232.TechShrwt_USAelec",
              "L2232.TechCoef_USAelec",
              "L2232.Production_exports_USAelec",
              "L2232.Supplysector_elec_FERC",
              "L2232.ElecReserve_FERC",
              "L2232.SubsectorShrwtFllt_elec_FERC",
              "L2232.SubsectorInterp_elec_FERC",
              "L2232.SubsectorLogit_elec_FERC",
              "L2232.TechShrwt_elec_FERC",
              "L2232.TechCoef_elec_FERC",
              "L2232.TechCoef_elecownuse_FERC",
              "L2232.Production_imports_FERC",
              "L2232.Production_elec_gen_FERC",
              "L2232.StubTechElecMarket_backup_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.DeleteSubsector_USAelec <- get_data(all_data, "L223.DeleteSubsector_USAelec")
    L223.SectorNodeEquiv <- get_data(all_data, "L223.SectorNodeEquiv")
    L223.TechNodeEquiv <- get_data(all_data, "L223.TechNodeEquiv")
    L223.PassThroughSector_elec <- get_data(all_data, "L223.PassThroughSector_elec")
    L223.PassThroughTech_elec_FERC <- get_data(all_data, "L223.PassThroughTech_elec_FERC")
    L223.Supplysector_USAelec <- get_data(all_data, "L223.Supplysector_USAelec")
    L223.SubsectorShrwtFllt_USAelec <- get_data(all_data, "L223.SubsectorShrwtFllt_USAelec")
    L223.SubsectorInterp_USAelec <- get_data(all_data, "L223.SubsectorInterp_USAelec")
    L223.SubsectorLogit_USAelec <- get_data(all_data, "L223.SubsectorLogit_USAelec")
    L223.TechShrwt_USAelec <- get_data(all_data, "L223.TechShrwt_USAelec")
    L223.TechCoef_USAelec <- get_data(all_data, "L223.TechCoef_USAelec")
    L223.Production_USAelec <- get_data(all_data, "L223.Production_USAelec")
    L223.Supplysector_elec_FERC <- get_data(all_data, "L223.Supplysector_elec_FERC")
    L223.SubsectorShrwtFllt_elec_FERC <- get_data(all_data, "L223.SubsectorShrwtFllt_elec_FERC")
    L223.SubsectorInterp_elec_FERC <- get_data(all_data, "L223.SubsectorInterp_elec_FERC")
    L223.SubsectorLogit_elec_FERC <- get_data(all_data, "L223.SubsectorLogit_elec_FERC")
    L223.TechShrwt_elec_FERC <- get_data(all_data, "L223.TechShrwt_elec_FERC")
    L223.TechCoef_elec_FERC <- get_data(all_data, "L223.TechCoef_elec_FERC")
    L223.Production_elec_FERC <- get_data(all_data, "L223.Production_elec_FERC")
    L223.InterestRate_FERC <- get_data(all_data, "L223.InterestRate_FERC")
    L223.Pop_FERC <- get_data(all_data, "L223.Pop_FERC")
    L223.BaseGDP_FERC <- get_data(all_data, "L223.BaseGDP_FERC")
    L223.LaborForceFillout_FERC <- get_data(all_data, "L223.LaborForceFillout_FERC")
    L223.SubsectorShrwt_nuc_USA <- get_data(all_data, "L223.SubsectorShrwt_nuc_USA")
    L223.StubTechFixOut_elec_USA <- get_data(all_data, "L223.StubTechFixOut_elec_USA")
    L223.StubTechFixOut_hydro_USA <- get_data(all_data, "L223.StubTechFixOut_hydro_USA")
    L223.StubTechProd_elec_USA <- get_data(all_data, "L223.StubTechProd_elec_USA")
    L223.StubTechMarket_elec_USA <- get_data(all_data, "L223.StubTechMarket_elec_USA")
    L223.StubTechMarket_backup_USA <- get_data(all_data, "L223.StubTechMarket_backup_USA")
    L223.StubTechElecMarket_backup_USA <- get_data(all_data, "L223.StubTechElecMarket_backup_USA")
    L223.StubTechCapFactor_elec_wind_USA <- get_data(all_data, "L223.StubTechCapFactor_elec_wind_USA")
    L223.StubTechCapFactor_elec_solar_USA <- get_data(all_data, "L223.StubTechCapFactor_elec_solar_USA")
    L2232.DeleteSupplysector_USAelec <- get_data(all_data, "L2232.DeleteSupplysector_USAelec")
    L2232.Supplysector_USAelec <- get_data(all_data, "L2232.Supplysector_USAelec")
    L2232.SubsectorShrwtFllt_USAelec <- get_data(all_data, "L2232.SubsectorShrwtFllt_USAelec")
    L2232.SubsectorInterp_USAelec <- get_data(all_data, "L2232.SubsectorInterp_USAelec")
    L2232.SubsectorLogit_USAelec <- get_data(all_data, "L2232.SubsectorLogit_USAelec")
    L2232.TechShrwt_USAelec <- get_data(all_data, "L2232.TechShrwt_USAelec")
    L2232.TechCoef_USAelec <- get_data(all_data, "L2232.TechCoef_USAelec")
    L2232.Production_exports_USAelec <- get_data(all_data, "L2232.Production_exports_USAelec")
    L2232.Supplysector_elec_FERC <- get_data(all_data, "L2232.Supplysector_elec_FERC")
    L2232.ElecReserve_FERC <- get_data(all_data, "L2232.ElecReserve_FERC")
    L2232.SubsectorShrwtFllt_elec_FERC <- get_data(all_data, "L2232.SubsectorShrwtFllt_elec_FERC")
    L2232.SubsectorInterp_elec_FERC <- get_data(all_data, "L2232.SubsectorInterp_elec_FERC")
    L2232.SubsectorLogit_elec_FERC <- get_data(all_data, "L2232.SubsectorLogit_elec_FERC")
    L2232.TechShrwt_elec_FERC <- get_data(all_data, "L2232.TechShrwt_elec_FERC")
    L2232.TechCoef_elec_FERC <- get_data(all_data, "L2232.TechCoef_elec_FERC")
    L2232.TechCoef_elecownuse_FERC <- get_data(all_data, "L2232.TechCoef_elecownuse_FERC")
    L2232.Production_imports_FERC <- get_data(all_data, "L2232.Production_imports_FERC")
    L2232.Production_elec_gen_FERC <- get_data(all_data, "L2232.Production_elec_gen_FERC")
    L2232.StubTechElecMarket_backup_USA <- get_data(all_data, "L2232.StubTechElecMarket_backup_USA")

    # ===================================================

    # Produce outputs
    create_xml("electricity_USA.xml") %>%
      add_xml_data(L223.DeleteSubsector_USAelec,"DeleteSubsector") %>%
      add_xml_data(L223.SectorNodeEquiv,"EQUIV_TABLE", column_order_lookup=NULL) %>%
      add_xml_data(L223.TechNodeEquiv,"EQUIV_TABLE", column_order_lookup=NULL) %>%
      add_xml_data(L223.PassThroughSector_elec,"PassThroughSector") %>%
      add_xml_data(L223.PassThroughTech_elec_FERC,"PassThroughTech") %>%
      add_xml_data(L223.Supplysector_USAelec,"Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_USAelec,"SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorInterp_USAelec,"SubsectorInterp") %>%
      add_xml_data(L223.SubsectorLogit_USAelec,"SubsectorLogit") %>%
      add_xml_data(L223.TechShrwt_USAelec,"TechShrwt") %>%
      add_xml_data(L223.TechCoef_USAelec,"TechCoef") %>%
      add_xml_data(L223.Production_USAelec,"Production") %>%
      add_xml_data(L223.Supplysector_elec_FERC,"Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_FERC,"SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorInterp_elec_FERC,"SubsectorInterp") %>%
      add_xml_data(L223.SubsectorLogit_elec_FERC,"SubsectorLogit") %>%
      add_xml_data(L223.TechShrwt_elec_FERC,"TechShrwt") %>%
      add_xml_data(L223.TechCoef_elec_FERC,"TechCoef") %>%
      add_xml_data(L223.Production_elec_FERC,"Production") %>%
      add_xml_data(L223.InterestRate_FERC,"InterestRate") %>%
      add_xml_data(L223.Pop_FERC,"Pop") %>%
      add_xml_data(L223.BaseGDP_FERC,"BaseGDP") %>%
      add_xml_data(L223.LaborForceFillout_FERC,"LaborForceFillout") %>%
      add_xml_data(L223.SubsectorShrwt_nuc_USA,"SubsectorShrwt") %>%
      add_xml_data(L223.StubTechFixOut_elec_USA,"StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro_USA,"StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec_USA,"StubTechProd") %>%
      add_xml_data(L223.StubTechMarket_elec_USA,"StubTechMarket") %>%
      add_xml_data(L223.StubTechMarket_backup_USA,"StubTechMarket") %>%
      add_xml_data(L223.StubTechElecMarket_backup_USA,"StubTechElecMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec_wind_USA,"StubTechCapFactor") %>%
      add_xml_data(L223.StubTechCapFactor_elec_solar_USA,"StubTechCapFactor") %>%
      add_xml_data(L2232.DeleteSupplysector_USAelec,"DeleteSupplysector") %>%
      add_xml_data(L2232.Supplysector_USAelec,"Supplysector") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_USAelec,"SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_USAelec,"SubsectorInterp") %>%
      add_xml_data(L2232.SubsectorLogit_USAelec,"SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_USAelec,"TechShrwt") %>%
      add_xml_data(L2232.TechCoef_USAelec,"TechCoef") %>%
      add_xml_data(L2232.Production_exports_USAelec,"Production") %>%
      add_xml_data(L2232.Supplysector_elec_FERC,"Supplysector") %>%
      add_xml_data(L2232.ElecReserve_FERC,"ElecReserve") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_FERC,"SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_FERC,"SubsectorInterp") %>%
      add_xml_data(L2232.SubsectorLogit_elec_FERC,"SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_elec_FERC,"TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_FERC,"TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_FERC,"TechCoef") %>%
      add_xml_data(L2232.Production_imports_FERC,"Production") %>%
      add_xml_data(L2232.Production_elec_gen_FERC,"Production") %>%
      add_xml_data(L2232.StubTechElecMarket_backup_USA,"StubTechElecMarket") %>%
      add_precursors("L223.DeleteSubsector_USAelec", "L223.SectorNodeEquiv", "L223.TechNodeEquiv", "L223.PassThroughSector_elec", "L223.PassThroughTech_elec_FERC", "object", "L223.Supplysector_USAelec", "L223.SubsectorShrwtFllt_USAelec", "L223.SubsectorInterp_USAelec", "L223.SubsectorLogit_USAelec", "L223.TechShrwt_USAelec", "L223.TechCoef_USAelec", "L223.Production_USAelec", "L223.Supplysector_elec_FERC", "L223.SubsectorShrwtFllt_elec_FERC", "L223.SubsectorInterp_elec_FERC", "L223.SubsectorLogit_elec_FERC", "L223.TechShrwt_elec_FERC", "L223.TechCoef_elec_FERC", "L223.Production_elec_FERC", "L223.InterestRate_FERC", "L223.Pop_FERC", "L223.BaseGDP_FERC", "L223.LaborForceFillout_FERC", "L223.SubsectorShrwt_nuc_USA", "L223.StubTechFixOut_elec_USA", "L223.StubTechFixOut_hydro_USA", "L223.StubTechProd_elec_USA", "L223.StubTechMarket_elec_USA", "L223.StubTechMarket_backup_USA", "L223.StubTechElecMarket_backup_USA", "L223.StubTechCapFactor_elec_wind_USA", "L223.StubTechCapFactor_elec_solar_USA", "L2232.DeleteSupplysector_USAelec", "L2232.Supplysector_USAelec", "L2232.SubsectorShrwtFllt_USAelec", "L2232.SubsectorInterp_USAelec", "L2232.SubsectorLogit_USAelec", "L2232.TechShrwt_USAelec", "L2232.TechCoef_USAelec", "L2232.Production_exports_USAelec", "L2232.Supplysector_elec_FERC", "L2232.ElecReserve_FERC", "L2232.SubsectorShrwtFllt_elec_FERC", "L2232.SubsectorInterp_elec_FERC", "L2232.SubsectorLogit_elec_FERC", "L2232.TechShrwt_elec_FERC", "L2232.TechCoef_elec_FERC", "L2232.TechCoef_elecownuse_FERC", "L2232.Production_imports_FERC", "L2232.Production_elec_gen_FERC", "L2232.StubTechElecMarket_backup_USA") ->
      electricity_USA.xml

    return_data(electricity_USA.xml)
  } else {
    stop("Unknown command")
  }
}
