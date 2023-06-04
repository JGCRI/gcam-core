# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_elec_segments_xml
#'
#' Construct XML data structure for \code{elec_segments_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elec_segments_USA.xml}. The corresponding file in the
#' original data system was \code{batch_elec_segments_USA.xml} (gcamusa xml-batch).
module_gcamusa_elec_segments_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2234.Supplysector_elecS_USA",
             "L2234.ElecReserve_elecS_USA",
             "L2234.SubsectorLogit_elecS_USA",
             "L2234.SubsectorShrwtInterp_elecS_USA",
             "L2234.SubsectorShrwtInterpTo_elecS_USA",
             "L2234.SubsectorShrwt_elecS_USA",
             "L2234.StubTechEff_elecS_USA",
             "L2234.StubTechCapFactor_elecS_solar_USA",
             "L2234.StubTechCapFactor_elecS_wind_USA",
             "L2234.SubsectorShrwtFllt_elecS_grid_USA",
             "L2234.SubsectorShrwtInterp_elecS_grid_USA",
             "L2234.PassThroughSector_elecS_USA",
             "L2234.PassThroughTech_elecS_grid_USA",
             "L2234.GlobalTechShrwt_elecS_USA",
             "L2234.GlobalIntTechShrwt_elecS_USA",
             "L2234.PrimaryRenewKeyword_elecS_USA",
             "L2234.PrimaryRenewKeywordInt_elecS_USA",
             "L2234.AvgFossilEffKeyword_elecS_USA",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalIntTechCapital_elecS_USA",
             "L2234.GlobalTechOMfixed_elecS_USA",
             "L2234.GlobalIntTechOMfixed_elecS_USA",
             "L2234.GlobalTechOMvar_elecS_USA",
             "L2234.GlobalIntTechOMvar_elecS_USA",
             "L2234.GlobalTechCapFac_elecS_USA",
             "L2234.GlobalTechEff_elecS_USA",
             "L2234.GlobalIntTechEff_elecS_USA",
             "L2234.GlobalTechLifetime_elecS_USA",
             "L2234.GlobalIntTechLifetime_elecS_USA",
             "L2234.GlobalTechProfitShutdown_elecS_USA",
             "L2234.GlobalTechSCurve_elecS_USA",
             "L2234.GlobalTechCapture_elecS_USA",
             "L2234.GlobalIntTechBackup_elecS_USA",
             "L2234.StubTechMarket_elecS_USA",
             "L2234.StubTechMarket_backup_elecS_USA",
             "L2234.StubTechElecMarket_backup_elecS_USA",
             "L2234.StubTechProd_elecS_USA",
             "L2234.StubTechFixOut_elecS_USA",
             "L2234.StubTechFixOut_hydro_elecS_USA",
             "L2234.StubTechCost_offshore_wind_elecS_USA",
             "L2234.TechShrwt_elecS_grid_USA",
             "L2234.TechCoef_elecS_grid_USA",
             "L2234.TechProd_elecS_grid_USA",
             "L2235.DeleteSupplysector_elec_USA",
             "L2235.InterestRate_FERC_USA",
             "L2235.Pop_FERC_USA",
             "L2235.GDP_FERC_USA",
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
    return(c(XML = "elec_segments_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- intermittent.technology <- supplysector <- subsector <- NULL
    # silence package check notes

    # Load required inputs
    L2234.Supplysector_elecS_USA <- get_data(all_data, "L2234.Supplysector_elecS_USA")
    L2234.ElecReserve_elecS_USA <- get_data(all_data, "L2234.ElecReserve_elecS_USA")
    L2234.SubsectorLogit_elecS_USA <- get_data(all_data, "L2234.SubsectorLogit_elecS_USA")
    L2234.SubsectorShrwtInterp_elecS_USA <- get_data(all_data, "L2234.SubsectorShrwtInterp_elecS_USA")
    L2234.SubsectorShrwtInterpTo_elecS_USA <- get_data(all_data, "L2234.SubsectorShrwtInterpTo_elecS_USA")
    L2234.SubsectorShrwt_elecS_USA <- get_data(all_data, "L2234.SubsectorShrwt_elecS_USA")
    L2234.StubTechEff_elecS_USA <- get_data(all_data, "L2234.StubTechEff_elecS_USA")
    L2234.StubTechCapFactor_elecS_solar_USA <- get_data(all_data, "L2234.StubTechCapFactor_elecS_solar_USA")
    L2234.StubTechCapFactor_elecS_wind_USA <- get_data(all_data, "L2234.StubTechCapFactor_elecS_wind_USA")
    L2234.SubsectorShrwtFllt_elecS_grid_USA <- get_data(all_data, "L2234.SubsectorShrwtFllt_elecS_grid_USA")
    L2234.SubsectorShrwtInterp_elecS_grid_USA <- get_data(all_data, "L2234.SubsectorShrwtInterp_elecS_grid_USA")
    L2234.PassThroughSector_elecS_USA <- get_data(all_data, "L2234.PassThroughSector_elecS_USA")
    L2234.PassThroughTech_elecS_grid_USA <- get_data(all_data, "L2234.PassThroughTech_elecS_grid_USA")
    L2234.GlobalTechShrwt_elecS_USA <- get_data(all_data, "L2234.GlobalTechShrwt_elecS_USA")
    L2234.GlobalIntTechShrwt_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechShrwt_elecS_USA")
    L2234.PrimaryRenewKeyword_elecS_USA <- get_data(all_data, "L2234.PrimaryRenewKeyword_elecS_USA")
    L2234.PrimaryRenewKeywordInt_elecS_USA <- get_data(all_data, "L2234.PrimaryRenewKeywordInt_elecS_USA")
    L2234.AvgFossilEffKeyword_elecS_USA <- get_data(all_data, "L2234.AvgFossilEffKeyword_elecS_USA")
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_USA")
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechCapital_elecS_USA")
    L2234.GlobalTechOMfixed_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMfixed_elecS_USA")
    L2234.GlobalIntTechOMfixed_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechOMfixed_elecS_USA")
    L2234.GlobalTechOMvar_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMvar_elecS_USA")
    L2234.GlobalIntTechOMvar_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechOMvar_elecS_USA")
    L2234.GlobalTechCapFac_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapFac_elecS_USA")
    L2234.GlobalTechEff_elecS_USA <- get_data(all_data, "L2234.GlobalTechEff_elecS_USA")
    L2234.GlobalIntTechEff_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechEff_elecS_USA")
    L2234.GlobalTechLifetime_elecS_USA <- get_data(all_data, "L2234.GlobalTechLifetime_elecS_USA")
    L2234.GlobalIntTechLifetime_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechLifetime_elecS_USA")
    L2234.GlobalTechProfitShutdown_elecS_USA <- get_data(all_data, "L2234.GlobalTechProfitShutdown_elecS_USA")
    L2234.GlobalTechSCurve_elecS_USA <- get_data(all_data, "L2234.GlobalTechSCurve_elecS_USA")
    L2234.GlobalTechCapture_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapture_elecS_USA")
    L2234.GlobalIntTechBackup_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechBackup_elecS_USA")
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, "L2234.StubTechMarket_elecS_USA")
    L2234.StubTechMarket_backup_elecS_USA <- get_data(all_data, "L2234.StubTechMarket_backup_elecS_USA")
    L2234.StubTechElecMarket_backup_elecS_USA <- get_data(all_data, "L2234.StubTechElecMarket_backup_elecS_USA")
    L2234.StubTechProd_elecS_USA <- get_data(all_data, "L2234.StubTechProd_elecS_USA")
    L2234.StubTechFixOut_elecS_USA <- get_data(all_data, "L2234.StubTechFixOut_elecS_USA")
    L2234.StubTechFixOut_hydro_elecS_USA <- get_data(all_data, "L2234.StubTechFixOut_hydro_elecS_USA")
    L2234.StubTechCost_offshore_wind_elecS_USA <- get_data(all_data, "L2234.StubTechCost_offshore_wind_elecS_USA")
    L2234.TechShrwt_elecS_grid_USA <- get_data(all_data, "L2234.TechShrwt_elecS_grid_USA")
    L2234.TechCoef_elecS_grid_USA <- get_data(all_data, "L2234.TechCoef_elecS_grid_USA")
    L2234.TechProd_elecS_grid_USA <- get_data(all_data, "L2234.TechProd_elecS_grid_USA")

    L2235.DeleteSupplysector_elec_USA <- get_data(all_data, "L2235.DeleteSupplysector_elec_USA")
    L2235.InterestRate_FERC_USA <- get_data(all_data, "L2235.InterestRate_FERC_USA")
    L2235.Pop_FERC_USA <- get_data(all_data, "L2235.Pop_FERC_USA")
    L2235.GDP_FERC_USA <- get_data(all_data, "L2235.GDP_FERC_USA")
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


    # ===================================================
    # Rename tibble columns to match the L2 data names.
    L2234.PassThroughSector_elecS_USA <- rename(L2234.PassThroughSector_elecS_USA, pass.through.sector = passthrough.sector)
    L2234.PassThroughTech_elecS_grid_USA <- rename(L2234.PassThroughTech_elecS_grid_USA, pass.through.technology = technology)
    # NOTE:  below is an issue with LEVEL2_DATA_NAMES... PrimaryRenewKeywordInt name should be intermittent.technology,
    # as the table is for intermittent technologies and the old DS MI header name is intermittent.technology
    L2234.PrimaryRenewKeywordInt_elecS_USA <- rename(L2234.PrimaryRenewKeywordInt_elecS_USA, technology = intermittent.technology)

    # Function to fix GlobalTech / GlobalIntTech sector & subsector names, which is a recurring issue
    fix_global_tech_names <- function(data){
      data_new <- data %>%
        rename(sector.name = supplysector,
               subsector.name = subsector)
      return(data_new)
    }

    L2234.GlobalTechCapital_elecS_USA <- fix_global_tech_names(L2234.GlobalTechCapital_elecS_USA)
    L2234.GlobalIntTechCapital_elecS_USA <- fix_global_tech_names(L2234.GlobalIntTechCapital_elecS_USA)
    L2234.GlobalTechOMfixed_elecS_USA <- fix_global_tech_names(L2234.GlobalTechOMfixed_elecS_USA)
    L2234.GlobalIntTechOMfixed_elecS_USA <- fix_global_tech_names(L2234.GlobalIntTechOMfixed_elecS_USA)
    L2234.GlobalTechOMvar_elecS_USA <- fix_global_tech_names(L2234.GlobalTechOMvar_elecS_USA)
    L2234.GlobalIntTechOMvar_elecS_USA <- fix_global_tech_names(L2234.GlobalIntTechOMvar_elecS_USA)
    L2234.GlobalTechCapFac_elecS_USA <- fix_global_tech_names(L2234.GlobalTechCapFac_elecS_USA)
    L2234.GlobalTechEff_elecS_USA <- fix_global_tech_names(L2234.GlobalTechEff_elecS_USA)
    L2234.GlobalIntTechEff_elecS_USA <- fix_global_tech_names(L2234.GlobalIntTechEff_elecS_USA)
    L2234.GlobalTechLifetime_elecS_USA <- fix_global_tech_names(L2234.GlobalTechLifetime_elecS_USA)
    L2234.GlobalIntTechLifetime_elecS_USA <- fix_global_tech_names(L2234.GlobalIntTechLifetime_elecS_USA)
    L2234.GlobalTechProfitShutdown_elecS_USA <- fix_global_tech_names(L2234.GlobalTechProfitShutdown_elecS_USA)
    L2234.GlobalTechSCurve_elecS_USA <- fix_global_tech_names(L2234.GlobalTechSCurve_elecS_USA)
    L2234.GlobalTechCapture_elecS_USA <- fix_global_tech_names(L2234.GlobalTechCapture_elecS_USA)
    # NOTE:  below is an issue with LEVEL2_DATA_NAMES... GlobalIntTechBackup name should be intermittent.technology,
    # as the table is for intermittent technologies and the old DS MI header name is intermittent.technology
    L2234.GlobalIntTechBackup_elecS_USA <- L2234.GlobalIntTechBackup_elecS_USA %>%
      fix_global_tech_names() %>%
      rename(technology = intermittent.technology)

    L2234.StubTechProd_elecS_USA <- rename(L2234.StubTechProd_elecS_USA, tech.share.weight = share.weight)
    L2234.TechProd_elecS_grid_USA <- rename(L2234.TechProd_elecS_grid_USA, tech.share.weight = share.weight)


    # Produce outputs
    create_xml("elec_segments_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2234.PassThroughSector_elecS_USA, "PassThroughSector") %>%
      add_xml_data(L2234.PassThroughTech_elecS_grid_USA, "PassThroughTech") %>%
      add_logit_tables_xml(L2234.Supplysector_elecS_USA, "Supplysector") %>%
      add_xml_data(L2234.ElecReserve_elecS_USA, "ElecReserve") %>%
      add_logit_tables_xml(L2234.SubsectorLogit_elecS_USA, "SubsectorLogit") %>%
      add_xml_data(L2234.GlobalTechShrwt_elecS_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2234.GlobalIntTechShrwt_elecS_USA, "GlobalIntTechShrwt") %>%
      add_xml_data(L2234.PrimaryRenewKeyword_elecS_USA, "PrimaryRenewKeyword") %>%
      add_xml_data(L2234.PrimaryRenewKeywordInt_elecS_USA, "PrimaryRenewKeywordInt") %>%
      add_xml_data(L2234.AvgFossilEffKeyword_elecS_USA, "AvgFossilEffKeyword") %>%
      add_xml_data(L2234.GlobalTechCapital_elecS_USA, "GlobalTechCapital") %>%
      add_xml_data(L2234.GlobalIntTechCapital_elecS_USA, "GlobalIntTechCapital") %>%
      add_xml_data(L2234.GlobalTechOMfixed_elecS_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2234.GlobalIntTechOMfixed_elecS_USA, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2234.GlobalTechOMvar_elecS_USA, "GlobalTechOMvar") %>%
      add_xml_data(L2234.GlobalIntTechOMvar_elecS_USA, "GlobalIntTechOMvar") %>%
      add_xml_data(L2234.GlobalTechCapFac_elecS_USA, "GlobalTechCapFac") %>%
      add_xml_data(L2234.GlobalTechEff_elecS_USA, "GlobalTechEff") %>%
      add_xml_data(L2234.GlobalIntTechEff_elecS_USA, "GlobalIntTechEff") %>%
      add_xml_data(L2234.GlobalTechLifetime_elecS_USA, "GlobalTechLifetime") %>%
      add_xml_data(L2234.GlobalIntTechLifetime_elecS_USA, "GlobalIntTechLifetime") %>%
      add_xml_data(L2234.GlobalTechProfitShutdown_elecS_USA, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2234.GlobalTechSCurve_elecS_USA, "GlobalTechSCurve") %>%
      add_xml_data(L2234.GlobalTechCapture_elecS_USA, "GlobalTechCapture") %>%
      add_xml_data(L2234.GlobalIntTechBackup_elecS_USA, "GlobalIntTechBackup") %>%
      add_xml_data(L2234.StubTechMarket_elecS_USA, "StubTechMarket") %>%
      add_xml_data(L2234.StubTechMarket_backup_elecS_USA, "StubTechMarket") %>%
      add_xml_data(L2234.StubTechElecMarket_backup_elecS_USA, "StubTechElecMarket") %>%
      add_xml_data(L2234.StubTechProd_elecS_USA, "StubTechProd") %>%
      add_xml_data(L2234.SubsectorShrwt_elecS_USA, "SubsectorShrwt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_USA, "SubsectorInterp") %>%
      add_xml_data(L2234.SubsectorShrwtInterpTo_elecS_USA, "SubsectorInterpTo") %>%
      add_xml_data(L2234.StubTechCapFactor_elecS_wind_USA, "StubTechCapFactor") %>%
      add_xml_data(L2234.StubTechCapFactor_elecS_solar_USA, "StubTechCapFactor") %>%
      add_xml_data(L2234.StubTechFixOut_elecS_USA, "StubTechFixOut") %>%
      add_xml_data(L2234.StubTechEff_elecS_USA, "StubTechEff") %>%
      add_xml_data(L2234.StubTechFixOut_hydro_elecS_USA, "StubTechFixOut") %>%
      add_xml_data(L2234.StubTechCost_offshore_wind_elecS_USA, "StubTechCost") %>%
      add_xml_data(L2234.TechShrwt_elecS_grid_USA, "TechShrwt") %>%
      add_xml_data(L2234.TechCoef_elecS_grid_USA, "TechCoef") %>%
      add_xml_data(L2234.TechProd_elecS_grid_USA, "Production") %>%
      add_xml_data(L2234.SubsectorShrwtFllt_elecS_grid_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_grid_USA, "SubsectorInterp") %>%
      add_xml_data(L2235.DeleteSupplysector_elec_USA, "DeleteSupplysector") %>%
      add_xml_data(L2235.InterestRate_FERC_USA, "InterestRate") %>%
      add_xml_data(L2235.Pop_FERC_USA, "Pop") %>%
      add_xml_data(L2235.GDP_FERC_USA, "GDP") %>%
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
      add_precursors("L2234.Supplysector_elecS_USA",
                     "L2234.ElecReserve_elecS_USA",
                     "L2234.SubsectorLogit_elecS_USA",
                     "L2234.SubsectorShrwtInterp_elecS_USA",
                     "L2234.SubsectorShrwtInterpTo_elecS_USA",
                     "L2234.SubsectorShrwt_elecS_USA",
                     "L2234.StubTechEff_elecS_USA",
                     "L2234.StubTechCapFactor_elecS_solar_USA",
                     "L2234.StubTechCapFactor_elecS_wind_USA",
                     "L2234.SubsectorShrwtFllt_elecS_grid_USA",
                     "L2234.SubsectorShrwtInterp_elecS_grid_USA",
                     "L2234.PassThroughSector_elecS_USA",
                     "L2234.PassThroughTech_elecS_grid_USA",
                     "L2234.GlobalTechShrwt_elecS_USA",
                     "L2234.GlobalIntTechShrwt_elecS_USA",
                     "L2234.PrimaryRenewKeyword_elecS_USA",
                     "L2234.PrimaryRenewKeywordInt_elecS_USA",
                     "L2234.AvgFossilEffKeyword_elecS_USA",
                     "L2234.GlobalTechCapital_elecS_USA",
                     "L2234.GlobalIntTechCapital_elecS_USA",
                     "L2234.GlobalTechOMfixed_elecS_USA",
                     "L2234.GlobalIntTechOMfixed_elecS_USA",
                     "L2234.GlobalTechOMvar_elecS_USA",
                     "L2234.GlobalIntTechOMvar_elecS_USA",
                     "L2234.GlobalTechCapFac_elecS_USA",
                     "L2234.GlobalTechEff_elecS_USA",
                     "L2234.GlobalIntTechEff_elecS_USA",
                     "L2234.GlobalTechLifetime_elecS_USA",
                     "L2234.GlobalIntTechLifetime_elecS_USA",
                     "L2234.GlobalTechProfitShutdown_elecS_USA",
                     "L2234.GlobalTechSCurve_elecS_USA",
                     "L2234.GlobalTechCapture_elecS_USA",
                     "L2234.GlobalIntTechBackup_elecS_USA",
                     "L2234.StubTechMarket_elecS_USA",
                     "L2234.StubTechMarket_backup_elecS_USA",
                     "L2234.StubTechElecMarket_backup_elecS_USA",
                     "L2234.StubTechProd_elecS_USA",
                     "L2234.StubTechFixOut_elecS_USA",
                     "L2234.StubTechFixOut_hydro_elecS_USA",
                     "L2234.StubTechCost_offshore_wind_elecS_USA",
                     "L2234.TechShrwt_elecS_grid_USA",
                     "L2234.TechCoef_elecS_grid_USA",
                     "L2234.TechProd_elecS_grid_USA",
                     "L2235.DeleteSupplysector_elec_USA",
                     "L2235.InterestRate_FERC_USA",
                     "L2235.Pop_FERC_USA",
                     "L2235.GDP_FERC_USA",
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
      elec_segments_USA.xml

    return_data(elec_segments_USA.xml)
  } else {
    stop("Unknown command")
  }
}
