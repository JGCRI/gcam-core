#' module_gcamusa_batch_elec_segments_water_USA_xml
#'
#' Construct XML data structure for \code{elec_segments_water_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_gcamusa_batch_elec_segments_water_USA_xml <- function(command, ...) {
    if(command == driver.DECLARE_INPUTS) {
    return(c("L2233.GlobalTechEff_elecS_cool_USA",
             "L2233.GlobalTechShrwt_elecS_cool_USA",
             "L2233.GlobalTechProfitShutdown_elecS_cool_USA",
             "L2233.GlobalTechOMvar_elecS_cool_USA",
             "L2233.GlobalTechOMfixed_elecS_cool_USA",
             "L2233.GlobalTechCapital_elecS_USA",
             "L2233.GlobalTechCapital_elecS_cool_USA",
             "L2233.GlobalTechCapFac_elecS_cool_USA",
             "L2233.GlobalTechSCurve_elecS_cool_USA",
             "L2233.GlobalTechCoef_elecS_cool_USA",
             "L2233.GlobalTechCapture_elecS_cool_USA",
             "L2233.GlobalTechLifetime_elecS_cool_USA",
             "L2233.AvgFossilEffKeyword_elecS_cool_USA",
             "L2233.GlobalIntTechBackup_elecS_cool_USA",
             "L2233.GlobalIntTechCapital_elecS_USA",
             "L2233.GlobalIntTechCapital_elecS_cool_USA",
             "L2233.GlobalIntTechEff_elecS_USA",
             "L2233.GlobalIntTechEff_elecS_cool_USA",
             "L2233.GlobalIntTechLifetime_elecS_cool_USA",
             "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
             "L2233.GlobalIntTechOMvar_elecS_cool_USA",
             "L2233.GlobalIntTechShrwt_elecS_cool_USA",
             "L2233.PrimaryRenewKeyword_elecS_cool_USA",
             "L2233.PrimaryRenewKeywordInt_elecS_cool_USA",
             "L2233.StubTechEff_elecS_cool_USA",
             "L2233.StubTechMarket_elecS_cool_USA",
             "L2233.StubTechProd_elecS_cool_USA",
             "L2233.StubTechSCurve_elecS_cool_USA",
             "L2233.StubTechCapFactor_elecS_solar_USA",
             "L2233.StubTechCapFactor_elecS_wind_USA",
             "L2233.StubTechElecMarket_backup_elecS_cool_USA",
             "L2233.StubTechFixOut_elecS_cool_USA",
             "L2233.StubTechFixOut_hydro_elecS_cool_USA",
             "L2233.StubTechMarket_backup_elecS_cool_USA",
             "L2233.StubTechLifetime_elecS_cool_USA",
             "L2233.SubsectorLogit_elecS_USA",
             "L2233.SubsectorLogit_elecS_cool_USA",
             "L2233.SubsectorShrwt_elecS_USA",
             "L2233.SubsectorShrwt_elecS_cool_USA",
             "L2233.SubsectorShrwtInterp_elecS_cool_USA",
             "L2233.SubsectorShrwtInterpTo_elecS_cool_USA",
             "L2233.Supplysector_elecS_cool_USA",

             "L2234.PassThroughSector_elecS_USA",
             "L2234.PassThroughTech_elecS_grid_USA",
             "L2234.ElecReserve_elecS_USA",
             "L2234.TechShrwt_elecS_grid_USA",
             "L2234.TechCoef_elecS_grid_USA",
             "L2234.TechProd_elecS_grid_USA",
             "L2234.SubsectorShrwtFllt_elecS_grid_USA",
             "L2234.SubsectorShrwtInterp_elecS_grid_USA",

             "L2235.DeleteSupplysector_elec_USA",
             "L2235.InterestRate_FERC_USA",
             "L2235.Pop_FERC_USA",
             "L2235.BaseGDP_FERC_USA",
             "L2235.LaborForceFillout_FERC_USA",
             "L2235.Supplysector_elec_USA",
             "L2235.ElecReserve_elecS_grid_vertical_USA",
             "L2235.SubsectorLogit_elec_USA",
             "L2235.SubsectorShrwtFllt_elec_USA",
             "L2235.SubsectorInterp_elec_USA",
             "L2235.SubsectorShrwt_elec_USA",
             "L2235.SubsectorInterpTo_elec_USA",
             "L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA",
             "L2235.SubsectorShrwtInterp_elecS_grid_vertical_USA",
             "L2235.TechShrwt_elec_USA",
             "L2235.TechCoef_elec_USA",
             "L2235.Production_exports_elec_USA",
             "L2235.TechShrwt_elecS_grid_vertical_USA",
             "L2235.TechCoef_elecS_grid_vertical_USA",
             "L2235.Supplysector_elec_FERC_USA",
             "L2235.SubsectorLogit_elec_FERC_USA",
             "L2235.SubsectorShrwtFllt_elec_FERC_USA",
             "L2235.SubsectorInterp_elec_FERC_USA",
             "L2235.SubsectorShrwt_elec_FERC",
             "L2235.SubsectorInterpTo_elec_FERC",
             "L2235.TechShrwt_elec_FERC_USA",
             "L2235.TechCoef_elec_FERC_USA",
             "L2235.TechCoef_elecownuse_FERC_USA",
             "L2235.Production_imports_FERC_USA",
             "L2235.Production_elec_gen_FERC_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_segments_water_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.GlobalTechEff_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechEff_elecS_cool_USA")
    L2233.GlobalTechShrwt_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechShrwt_elecS_cool_USA")
    L2233.GlobalTechProfitShutdown_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechProfitShutdown_elecS_cool_USA")
    L2233.GlobalTechOMvar_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechOMvar_elecS_cool_USA")
    L2233.GlobalTechOMfixed_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechOMfixed_elecS_cool_USA")
    L2233.GlobalTechCapital_elecS_USA<- get_data(all_data,"L2233.GlobalTechCapital_elecS_USA")
    L2233.GlobalTechCapital_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechCapital_elecS_cool_USA")
    L2233.GlobalTechCapFac_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechCapFac_elecS_cool_USA")
    L2233.GlobalTechSCurve_elecS_cool_USA <- get_data(all_data,"L2233.GlobalTechSCurve_elecS_cool_USA")
    L2233.GlobalTechCoef_elecS_cool_USA<- get_data(all_data,"L2233.GlobalTechCoef_elecS_cool_USA")
    L2233.GlobalTechCapture_elecS_cool_USA <- get_data(all_data, "L2233.GlobalTechCapture_elecS_cool_USA")
    L2233.GlobalTechLifetime_elecS_cool_USA <- get_data(all_data, "L2233.GlobalTechLifetime_elecS_cool_USA")
    L2233.AvgFossilEffKeyword_elecS_cool_USA <- get_data(all_data,"L2233.AvgFossilEffKeyword_elecS_cool_USA")
    L2233.GlobalIntTechBackup_elecS_cool_USA<- get_data(all_data,"L2233.GlobalIntTechBackup_elecS_cool_USA")
    L2233.GlobalIntTechCapital_elecS_USA<- get_data(all_data,"L2233.GlobalIntTechCapital_elecS_USA")
    L2233.GlobalIntTechCapital_elecS_cool_USA <- get_data(all_data,"L2233.GlobalIntTechCapital_elecS_cool_USA")
    L2233.GlobalIntTechEff_elecS_USA <- get_data(all_data, "L2233.GlobalIntTechEff_elecS_USA")
    L2233.GlobalIntTechEff_elecS_cool_USA<- get_data(all_data,"L2233.GlobalIntTechEff_elecS_cool_USA")
    L2233.GlobalIntTechLifetime_elecS_cool_USA<- get_data(all_data,"L2233.GlobalIntTechLifetime_elecS_cool_USA")
    L2233.GlobalIntTechOMfixed_elecS_cool_USA<- get_data(all_data,"L2233.GlobalIntTechOMfixed_elecS_cool_USA")
    L2233.GlobalIntTechOMvar_elecS_cool_USA<- get_data(all_data,"L2233.GlobalIntTechOMvar_elecS_cool_USA")
    L2233.GlobalIntTechShrwt_elecS_cool_USA <- get_data(all_data,"L2233.GlobalIntTechShrwt_elecS_cool_USA")
    L2233.PrimaryRenewKeyword_elecS_cool_USA <- get_data(all_data,"L2233.PrimaryRenewKeyword_elecS_cool_USA")
    L2233.PrimaryRenewKeywordInt_elecS_cool_USA <- get_data(all_data,"L2233.PrimaryRenewKeywordInt_elecS_cool_USA")
    L2233.StubTechEff_elecS_cool_USA <- get_data(all_data,"L2233.StubTechEff_elecS_cool_USA")
    L2233.StubTechMarket_elecS_cool_USA<- get_data(all_data,"L2233.StubTechMarket_elecS_cool_USA")
    L2233.StubTechProd_elecS_cool_USA<- get_data(all_data,"L2233.StubTechProd_elecS_cool_USA")
    L2233.StubTechSCurve_elecS_cool_USA<- get_data(all_data,"L2233.StubTechSCurve_elecS_cool_USA")
    L2233.StubTechCapFactor_elecS_solar_USA <- get_data(all_data,"L2233.StubTechCapFactor_elecS_solar_USA")
    L2233.StubTechCapFactor_elecS_wind_USA<- get_data(all_data,"L2233.StubTechCapFactor_elecS_wind_USA")
    L2233.StubTechElecMarket_backup_elecS_cool_USA <- get_data(all_data,"L2233.StubTechElecMarket_backup_elecS_cool_USA")
    L2233.StubTechFixOut_elecS_cool_USA<- get_data(all_data,"L2233.StubTechFixOut_elecS_cool_USA")
    L2233.StubTechFixOut_hydro_elecS_cool_USA<- get_data(all_data,"L2233.StubTechFixOut_hydro_elecS_cool_USA")
    L2233.StubTechMarket_backup_elecS_cool_USA <- get_data(all_data,"L2233.StubTechMarket_backup_elecS_cool_USA")
    L2233.StubTechLifetime_elecS_cool_USA <- get_data(all_data, "L2233.StubTechLifetime_elecS_cool_USA")
    L2233.SubsectorLogit_elecS_USA <- get_data(all_data,"L2233.SubsectorLogit_elecS_USA")
    L2233.SubsectorLogit_elecS_cool_USA <- get_data(all_data,"L2233.SubsectorLogit_elecS_cool_USA")
    L2233.SubsectorShrwt_elecS_USA <- get_data(all_data,"L2233.SubsectorShrwt_elecS_USA")
    L2233.SubsectorShrwt_elecS_cool_USA <- get_data(all_data,"L2233.SubsectorShrwt_elecS_cool_USA")
    L2233.SubsectorShrwtInterp_elecS_cool_USA <- get_data(all_data,"L2233.SubsectorShrwtInterp_elecS_cool_USA")
    L2233.SubsectorShrwtInterpTo_elecS_cool_USA<- get_data(all_data,"L2233.SubsectorShrwtInterpTo_elecS_cool_USA")
    L2233.Supplysector_elecS_cool_USA<- get_data(all_data,"L2233.Supplysector_elecS_cool_USA")


    L2234.PassThroughSector_elecS_USA <- get_data(all_data,"L2234.PassThroughSector_elecS_USA")
    L2234.PassThroughTech_elecS_grid_USA<- get_data(all_data,"L2234.PassThroughTech_elecS_grid_USA")
    L2234.ElecReserve_elecS_USA<- get_data(all_data,"L2234.ElecReserve_elecS_USA")
    L2234.TechCoef_elecS_grid_USA <- get_data(all_data,"L2234.TechCoef_elecS_grid_USA")
    L2234.TechShrwt_elecS_grid_USA <- get_data(all_data,"L2234.TechShrwt_elecS_grid_USA")
    L2234.TechProd_elecS_grid_USA <- get_data(all_data,"L2234.TechProd_elecS_grid_USA")
    L2234.SubsectorShrwtFllt_elecS_grid_USA <- get_data(all_data,"L2234.SubsectorShrwtFllt_elecS_grid_USA")
    L2234.SubsectorShrwtInterp_elecS_grid_USA <- get_data(all_data,"L2234.SubsectorShrwtInterp_elecS_grid_USA")

    L2235.DeleteSupplysector_elec_USA <- get_data(all_data, "L2235.DeleteSupplysector_elec_USA")
    L2235.InterestRate_FERC_USA <- get_data(all_data, "L2235.InterestRate_FERC_USA")
    L2235.Pop_FERC_USA <- get_data(all_data, "L2235.Pop_FERC_USA")
    L2235.BaseGDP_FERC_USA <- get_data(all_data, "L2235.BaseGDP_FERC_USA")
    L2235.LaborForceFillout_FERC_USA <- get_data(all_data, "L2235.LaborForceFillout_FERC_USA")
    L2235.Supplysector_elec_USA <- get_data(all_data, "L2235.Supplysector_elec_USA")
    L2235.ElecReserve_elecS_grid_vertical_USA <- get_data(all_data, "L2235.ElecReserve_elecS_grid_vertical_USA")
    L2235.SubsectorLogit_elec_USA <- get_data(all_data, "L2235.SubsectorLogit_elec_USA")
    L2235.SubsectorShrwtFllt_elec_USA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elec_USA")
    L2235.SubsectorInterp_elec_USA <- get_data(all_data, "L2235.SubsectorInterp_elec_USA")
    L2235.SubsectorShrwt_elec_USA <- get_data(all_data, "L2235.SubsectorShrwt_elec_USA")
    L2235.SubsectorInterpTo_elec_USA <- get_data(all_data, "L2235.SubsectorInterpTo_elec_USA")
    L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA")
    L2235.SubsectorShrwtInterp_elecS_grid_vertical_USA <- get_data(all_data, "L2235.SubsectorShrwtInterp_elecS_grid_vertical_USA")
    L2235.TechShrwt_elec_USA <- get_data(all_data, "L2235.TechShrwt_elec_USA")
    L2235.TechCoef_elec_USA <- get_data(all_data, "L2235.TechCoef_elec_USA")
    L2235.Production_exports_elec_USA <- get_data(all_data, "L2235.Production_exports_elec_USA")
    L2235.TechShrwt_elecS_grid_vertical_USA <- get_data(all_data, "L2235.TechShrwt_elecS_grid_vertical_USA")
    L2235.TechCoef_elecS_grid_vertical_USA <- get_data(all_data, "L2235.TechCoef_elecS_grid_vertical_USA")
    L2235.Supplysector_elec_FERC_USA <- get_data(all_data, "L2235.Supplysector_elec_FERC_USA")
    L2235.SubsectorLogit_elec_FERC_USA <- get_data(all_data, "L2235.SubsectorLogit_elec_FERC_USA")
    L2235.SubsectorShrwtFllt_elec_FERC_USA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elec_FERC_USA")
    L2235.SubsectorInterp_elec_FERC_USA <- get_data(all_data, "L2235.SubsectorInterp_elec_FERC_USA")
    L2235.SubsectorShrwt_elec_FERC <- get_data(all_data, "L2235.SubsectorShrwt_elec_FERC")
    L2235.SubsectorInterpTo_elec_FERC <- get_data(all_data, "L2235.SubsectorInterpTo_elec_FERC")
    L2235.TechShrwt_elec_FERC_USA <- get_data(all_data, "L2235.TechShrwt_elec_FERC_USA")
    L2235.TechCoef_elec_FERC_USA <- get_data(all_data, "L2235.TechCoef_elec_FERC_USA")
    L2235.TechCoef_elecownuse_FERC_USA <- get_data(all_data, "L2235.TechCoef_elecownuse_FERC_USA")
    L2235.Production_imports_FERC_USA <- get_data(all_data, "L2235.Production_imports_FERC_USA")
    L2235.Production_elec_gen_FERC_USA <- get_data(all_data, "L2235.Production_elec_gen_FERC_USA")


    # Silence package checks
    technology <- NULL

    L2234.PassThroughSector_elecS_USA <- rename(L2234.PassThroughSector_elecS_USA, pass.through.sector = passthrough.sector)
    L2234.PassThroughTech_elecS_grid_USA <- rename(L2234.PassThroughTech_elecS_grid_USA, pass.through.technology = technology)
    # ===================================================


    # Produce outputs
    create_xml("elec_segments_water_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2234.PassThroughSector_elecS_USA, "PassThroughSector") %>%
      add_xml_data(L2234.PassThroughTech_elecS_grid_USA, "PassThroughTech") %>%
      add_logit_tables_xml(L2233.Supplysector_elecS_cool_USA, "Supplysector") %>%
      add_xml_data(L2234.ElecReserve_elecS_USA, "ElecReserve") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elecS_USA, "SubsectorLogit") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elecS_cool_USA, "SubsectorLogit") %>%
      add_xml_data_generate_levels(L2233.GlobalTechShrwt_elecS_cool_USA, "GlobalTechShrwt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechShrwt_elecS_cool_USA, "GlobalIntTechShrwt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.PrimaryRenewKeyword_elecS_cool_USA, "PrimaryRenewKeyword","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.PrimaryRenewKeywordInt_elecS_cool_USA, "PrimaryRenewKeywordInt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.AvgFossilEffKeyword_elecS_cool_USA, "AvgFossilEffKeyword","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechCapital_elecS_USA, "GlobalTechCapital","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechCapital_elecS_cool_USA, "GlobalTechCapital","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechCapital_elecS_USA, "GlobalIntTechCapital","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechCapital_elecS_cool_USA, "GlobalIntTechCapital","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechOMfixed_elecS_cool_USA, "GlobalTechOMfixed","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechOMfixed_elecS_cool_USA, "GlobalIntTechOMfixed","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechOMvar_elecS_cool_USA, "GlobalTechOMvar","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechOMvar_elecS_cool_USA, "GlobalIntTechOMvar","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechCapFac_elecS_cool_USA, "GlobalTechCapFac","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechEff_elecS_cool_USA, "GlobalTechEff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechEff_elecS_USA, "GlobalIntTechEff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechEff_elecS_cool_USA, "GlobalIntTechEff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechLifetime_elecS_USA, "GlobalTechLifetime","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechLifetime_elecS_cool_USA, "GlobalIntTechLifetime","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechProfitShutdown_elecS_cool_USA, "GlobalTechProfitShutdown","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechSCurve_elecS_cool_USA, "GlobalTechSCurve","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechCapture_elecS_cool_USA, "GlobalTechCapture","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalTechLifetime_elecS_cool_USA, "GlobalTechLifetime","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.GlobalIntTechBackup_elecS_cool_USA, "GlobalIntTechBackup","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechMarket_elecS_cool_USA, "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechMarket_backup_elecS_cool_USA, "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechElecMarket_backup_elecS_cool_USA, "StubTechElecMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechProd_elecS_cool_USA, "StubTechProd","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.SubsectorShrwt_elecS_cool_USA, "SubsectorShrwt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.SubsectorShrwtInterp_elecS_cool_USA, "SubsectorInterp","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.SubsectorShrwtInterpTo_elecS_cool_USA, "SubsectorInterpTo","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechCapFactor_elecS_wind_USA, "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechCapFactor_elecS_solar_USA, "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechFixOut_elecS_cool_USA, "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechEff_elecS_cool_USA, "StubTechEff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechFixOut_hydro_elecS_cool_USA, "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2233.StubTechLifetime_elecS_cool_USA, "StubTechLifetime", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2234.TechShrwt_elecS_grid_USA, "TechShrwt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2234.TechCoef_elecS_grid_USA, "TechCoef","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2234.TechProd_elecS_grid_USA, "Production","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2234.SubsectorShrwtFllt_elecS_grid_USA, "SubsectorShrwtFllt","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2234.SubsectorShrwtInterp_elecS_grid_USA, "SubsectorInterp","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2235.DeleteSupplysector_elec_USA, "DeleteSupplysector") %>%
      add_xml_data(L2235.InterestRate_FERC_USA, "InterestRate") %>%
      add_xml_data(L2235.Pop_FERC_USA, "Pop") %>%
      add_xml_data(L2235.BaseGDP_FERC_USA, "BaseGDP") %>%
      add_xml_data(L2235.LaborForceFillout_FERC_USA, "LaborForceFillout") %>%
      add_logit_tables_xml(L2235.Supplysector_elec_USA, "Supplysector") %>%
      add_xml_data(L2235.ElecReserve_elecS_grid_vertical_USA, "ElecReserve") %>%
      add_logit_tables_xml(L2235.SubsectorLogit_elec_USA, "SubsectorLogit") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elec_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorInterp_elec_USA, "SubsectorInterp") %>%
      add_xml_data(L2235.SubsectorShrwt_elec_USA, "SubsectorShrwt") %>%
      add_xml_data(L2235.SubsectorInterpTo_elec_USA, "SubsectorInterpTo") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorShrwtInterp_elecS_grid_vertical_USA, "SubsectorInterp") %>%
      add_xml_data(L2235.TechShrwt_elec_USA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elec_USA, "TechCoef") %>%
      add_xml_data(L2235.Production_exports_elec_USA, "Production") %>%
      add_xml_data(L2235.TechShrwt_elecS_grid_vertical_USA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elecS_grid_vertical_USA, "TechCoef") %>%
      add_logit_tables_xml(L2235.Supplysector_elec_FERC_USA, "Supplysector") %>%
      add_logit_tables_xml(L2235.SubsectorLogit_elec_FERC_USA, "SubsectorLogit") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elec_FERC_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorInterp_elec_FERC_USA, "SubsectorInterp") %>%
      add_xml_data(L2235.SubsectorShrwt_elec_FERC, "SubsectorShrwt") %>%
      add_xml_data(L2235.SubsectorInterpTo_elec_FERC, "SubsectorInterpTo") %>%
      add_xml_data(L2235.TechShrwt_elec_FERC_USA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elec_FERC_USA, "TechCoef") %>%
      add_xml_data(L2235.TechCoef_elecownuse_FERC_USA, "TechCoef") %>%
      add_xml_data(L2235.Production_imports_FERC_USA, "Production") %>%
      add_xml_data(L2235.Production_elec_gen_FERC_USA, "Production") %>%
      add_precursors("L2233.GlobalTechEff_elecS_cool_USA",
                     "L2233.GlobalTechShrwt_elecS_cool_USA",
                     "L2233.GlobalTechProfitShutdown_elecS_cool_USA",
                     "L2233.GlobalTechOMvar_elecS_cool_USA",
                     "L2233.GlobalTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalTechCapital_elecS_USA",
                     "L2233.GlobalTechCapital_elecS_cool_USA",
                     "L2233.GlobalTechCapFac_elecS_cool_USA",
                     "L2233.GlobalTechSCurve_elecS_cool_USA",
                     "L2233.GlobalTechCoef_elecS_cool_USA",
                     "L2233.GlobalTechCapture_elecS_cool_USA",
                     "L2233.GlobalTechLifetime_elecS_cool_USA",
                     "L2233.AvgFossilEffKeyword_elecS_cool_USA",
                     "L2233.GlobalIntTechBackup_elecS_cool_USA",
                     "L2233.GlobalIntTechCapital_elecS_USA",
                     "L2233.GlobalIntTechCapital_elecS_cool_USA",
                     "L2233.GlobalIntTechEff_elecS_USA",
                     "L2233.GlobalIntTechEff_elecS_cool_USA",
                     "L2233.GlobalIntTechLifetime_elecS_cool_USA",
                     "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalIntTechOMvar_elecS_cool_USA",
                     "L2233.GlobalIntTechShrwt_elecS_cool_USA",
                     "L2233.PrimaryRenewKeyword_elecS_cool_USA",
                     "L2233.PrimaryRenewKeywordInt_elecS_cool_USA",
                     "L2233.StubTechEff_elecS_cool_USA",
                     "L2233.StubTechMarket_elecS_cool_USA",
                     "L2233.StubTechProd_elecS_cool_USA",
                     "L2233.StubTechSCurve_elecS_cool_USA",
                     "L2233.StubTechCapFactor_elecS_solar_USA",
                     "L2233.StubTechCapFactor_elecS_wind_USA",
                     "L2233.StubTechElecMarket_backup_elecS_cool_USA",
                     "L2233.StubTechFixOut_elecS_cool_USA",
                     "L2233.StubTechFixOut_hydro_elecS_cool_USA",
                     "L2233.StubTechMarket_backup_elecS_cool_USA",
                     "L2233.SubsectorLogit_elecS_USA",
                     "L2233.SubsectorLogit_elecS_cool_USA",
                     "L2233.SubsectorShrwt_elecS_cool_USA",
                     "L2233.SubsectorShrwtInterp_elecS_cool_USA",
                     "L2233.SubsectorShrwtInterpTo_elecS_cool_USA",
                     "L2233.Supplysector_elecS_cool_USA",

                     "L2234.PassThroughSector_elecS_USA",
                     "L2234.PassThroughTech_elecS_grid_USA",
                     "L2234.ElecReserve_elecS_USA",
                     "L2234.TechShrwt_elecS_grid_USA",
                     "L2234.TechCoef_elecS_grid_USA",
                     "L2234.TechProd_elecS_grid_USA",
                     "L2234.SubsectorShrwtFllt_elecS_grid_USA",
                     "L2234.SubsectorShrwtInterp_elecS_grid_USA",

                     "L2235.DeleteSupplysector_elec_USA",
                     "L2235.InterestRate_FERC_USA",
                     "L2235.Pop_FERC_USA",
                     "L2235.BaseGDP_FERC_USA",
                     "L2235.LaborForceFillout_FERC_USA",
                     "L2235.Supplysector_elec_USA",
                     "L2235.ElecReserve_elecS_grid_vertical_USA",
                     "L2235.SubsectorLogit_elec_USA",
                     "L2235.SubsectorShrwtFllt_elec_USA",
                     "L2235.SubsectorInterp_elec_USA",
                     "L2235.SubsectorShrwt_elec_USA",
                     "L2235.SubsectorInterpTo_elec_USA",
                     "L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA",
                     "L2235.SubsectorShrwtInterp_elecS_grid_vertical_USA",
                     "L2235.TechShrwt_elec_USA",
                     "L2235.TechCoef_elec_USA",
                     "L2235.Production_exports_elec_USA",
                     "L2235.TechShrwt_elecS_grid_vertical_USA",
                     "L2235.TechCoef_elecS_grid_vertical_USA",
                     "L2235.Supplysector_elec_FERC_USA",
                     "L2235.SubsectorLogit_elec_FERC_USA",
                     "L2235.SubsectorShrwtFllt_elec_FERC_USA",
                     "L2235.SubsectorInterp_elec_FERC_USA",
                     "L2235.SubsectorShrwt_elec_FERC",
                     "L2235.SubsectorInterpTo_elec_FERC",
                     "L2235.TechShrwt_elec_FERC_USA",
                     "L2235.TechCoef_elec_FERC_USA",
                     "L2235.TechCoef_elecownuse_FERC_USA",
                     "L2235.Production_imports_FERC_USA",
                     "L2235.Production_elec_gen_FERC_USA") ->
      elec_segments_water_USA.xml

    return_data(elec_segments_water_USA.xml)
  } else {
    stop("Unknown command")
  }
}
