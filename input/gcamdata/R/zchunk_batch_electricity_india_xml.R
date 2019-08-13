#' module_gcamindia_batch_electricity_xml
#'
#' Construct XML data structure for \code{electricity_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_india.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_india_xml.R} (gcamindia XML).
module_gcamindia_batch_electricity_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.india_state_PassthroughSector_elec",
             "L223.india_state_PassthroughTech_elec_CEA",
             "L223.india_state_Supplysector_elec_CEA",
             "L223.india_state_SubsectorShrwtFllt_elec_CEA",
             "L223.india_state_SubsectorInterp_elec_CEA",
             "L223.india_state_SubsectorLogit_elec_CEA",
             "L223.india_state_TechShrwt_elec_CEA",
             "L223.india_state_TechCoef_elec_CEA",
             "L223.india_state_Production_elec_CEA",
             "L223.india_state_InterestRate_CEA",
             "L223.india_state_Pop_CEA",
             "L223.india_state_BaseGDP_CEA",
             "L223.india_state_LaborForceFillout_CEA",
             "L223.india_state_Supplysector_elec",
             "L223.india_state_ElecReserve",
             "L223.india_state_SubsectorLogit_elec",
             "L223.india_state_SubsectorShrwtFllt_elec",
             "L223.india_state_SubsectorShrwt_nuc",
             "L223.india_state_SubsectorShrwt_renew",
             "L223.india_state_SubsectorInterp_elec",
             "L223.india_state_SubsectorInterpTo_elec",
             "L223.india_state_StubTech_elec",
             "L223.india_state_StubTechEff_elec",
             "L223.india_state_StubTechCapFactor_elec",
             "L223.india_state_StubTechFixOut_elec",
             "L223.india_state_StubTechFixOut_hydro",
             "L223.india_state_StubTechProd_elec",
             "L223.india_state_StubTechMarket_elec",
             "L223.india_state_StubTechMarket_backup",
             "L223.india_state_StubTechElecMarket_backup",
             "L223.india_state_StubTechCapFactor_elec_wind",
             "L223.india_state_StubTechCapFactor_elec_solar",
             "L2232.DeleteSupplysector_INDIAelec",
             "L2232.india_state_Supplysector_elec",
             "L2232.india_grid_SubsectorShrwtFllt_elec",
             "L2232.india_grid_SubsectorInterp_elec",
             "L2232.india_grid_SubsectorLogit_elec",
             "L2232.india_grid_TechShrwt_elec",
             "L2232.india_grid_TechCoef_elec",
             "L2232.india_grid_Production_exports_elec",
             "L2232.india_grid_Supplysector_elec_CEA",
             "L2232.india_grid_ElecReserve_CEA",
             "L2232.india_grid_SubsectorShrwtFllt_elec_CEA",
             "L2232.india_grid_SubsectorInterp_elec_CEA",
             "L2232.india_grid_SubsectorLogit_elec_CEA",
             "L2232.india_grid_TechShrwt_elec_CEA",
             "L2232.india_grid_TechCoef_elec_CEA",
             "L2232.india_grid_TechCoef_elecownuse_CEA",
             "L2232.india_grid_Production_imports_CEA",
             "L2232.india_grid_Production_elec_gen_CEA",
             "L2232.india_grid_StubTechElecMarket_backup"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- NULL # silence package check notes

    # Load required inputs

    L223.india_state_PassthroughSector_elec <- get_data(all_data, "L223.india_state_PassthroughSector_elec")
    L223.india_state_PassthroughTech_elec_CEA <- get_data(all_data, "L223.india_state_PassthroughTech_elec_CEA")
    L223.india_state_Supplysector_elec_CEA <- get_data(all_data, "L223.india_state_Supplysector_elec_CEA")
    L223.india_state_SubsectorShrwtFllt_elec_CEA <- get_data(all_data, "L223.india_state_SubsectorShrwtFllt_elec_CEA")
    L223.india_state_SubsectorInterp_elec_CEA <- get_data(all_data, "L223.india_state_SubsectorInterp_elec_CEA")
    L223.india_state_SubsectorLogit_elec_CEA <- get_data(all_data, "L223.india_state_SubsectorLogit_elec_CEA")
    L223.india_state_TechShrwt_elec_CEA <- get_data(all_data, "L223.india_state_TechShrwt_elec_CEA")
    L223.india_state_TechCoef_elec_CEA <- get_data(all_data, "L223.india_state_TechCoef_elec_CEA")
    L223.india_state_Production_elec_CEA <- get_data(all_data, "L223.india_state_Production_elec_CEA")
    L223.india_state_InterestRate_CEA <- get_data(all_data, "L223.india_state_InterestRate_CEA")
    L223.india_state_Pop_CEA <- get_data(all_data, "L223.india_state_Pop_CEA")
    L223.india_state_BaseGDP_CEA <- get_data(all_data, "L223.india_state_BaseGDP_CEA")
    L223.india_state_LaborForceFillout_CEA <- get_data(all_data, "L223.india_state_LaborForceFillout_CEA")
    L223.india_state_Supplysector_elec <- get_data(all_data, "L223.india_state_Supplysector_elec")
    L223.india_state_ElecReserve <- get_data(all_data, "L223.india_state_ElecReserve")
    L223.india_state_SubsectorLogit_elec <- get_data(all_data, "L223.india_state_SubsectorLogit_elec")
    L223.india_state_SubsectorShrwtFllt_elec <- get_data(all_data, "L223.india_state_SubsectorShrwtFllt_elec")
    L223.india_state_SubsectorShrwt_nuc <- get_data(all_data, "L223.india_state_SubsectorShrwt_nuc")
    L223.india_state_SubsectorShrwt_renew <- get_data(all_data, "L223.india_state_SubsectorShrwt_renew")
    L223.india_state_SubsectorInterp_elec <- get_data(all_data, "L223.india_state_SubsectorInterp_elec")
    L223.india_state_SubsectorInterpTo_elec <- get_data(all_data, "L223.india_state_SubsectorInterpTo_elec")
    L223.india_state_StubTech_elec <- get_data(all_data, "L223.india_state_StubTech_elec")
    L223.india_state_StubTechEff_elec <- get_data(all_data, "L223.india_state_StubTechEff_elec")
    L223.india_state_StubTechCapFactor_elec <- get_data(all_data, "L223.india_state_StubTechCapFactor_elec")
    L223.india_state_StubTechFixOut_elec <- get_data(all_data, "L223.india_state_StubTechFixOut_elec")
    L223.india_state_StubTechFixOut_hydro <- get_data(all_data, "L223.india_state_StubTechFixOut_hydro")
    L223.india_state_StubTechProd_elec <- get_data(all_data, "L223.india_state_StubTechProd_elec")
    L223.india_state_StubTechMarket_elec <- get_data(all_data, "L223.india_state_StubTechMarket_elec")
    L223.india_state_StubTechMarket_backup <- get_data(all_data, "L223.india_state_StubTechMarket_backup")
    L223.india_state_StubTechElecMarket_backup <- get_data(all_data, "L223.india_state_StubTechElecMarket_backup")
    L223.india_state_StubTechCapFactor_elec_wind <- get_data(all_data, "L223.india_state_StubTechCapFactor_elec_wind")
    L223.india_state_StubTechCapFactor_elec_solar <- get_data(all_data, "L223.india_state_StubTechCapFactor_elec_solar")
    L2232.DeleteSupplysector_INDIAelec <- get_data(all_data, "L2232.DeleteSupplysector_INDIAelec")
    L2232.india_state_Supplysector_elec <- get_data(all_data, "L2232.india_state_Supplysector_elec")
    L2232.india_grid_SubsectorShrwtFllt_elec <- get_data(all_data, "L2232.india_grid_SubsectorShrwtFllt_elec")
    L2232.india_grid_SubsectorInterp_elec <- get_data(all_data, "L2232.india_grid_SubsectorInterp_elec")
    L2232.india_grid_SubsectorLogit_elec <- get_data(all_data, "L2232.india_grid_SubsectorLogit_elec")
    L2232.india_grid_TechShrwt_elec <- get_data(all_data, "L2232.india_grid_TechShrwt_elec")
    L2232.india_grid_TechCoef_elec <- get_data(all_data, "L2232.india_grid_TechCoef_elec")
    L2232.india_grid_Production_exports_elec <- get_data(all_data, "L2232.india_grid_Production_exports_elec")
    L2232.india_grid_Supplysector_elec_CEA <- get_data(all_data, "L2232.india_grid_Supplysector_elec_CEA")
    L2232.india_grid_ElecReserve_CEA <- get_data(all_data, "L2232.india_grid_ElecReserve_CEA")
    L2232.india_grid_SubsectorShrwtFllt_elec_CEA <- get_data(all_data, "L2232.india_grid_SubsectorShrwtFllt_elec_CEA")
    L2232.india_grid_SubsectorInterp_elec_CEA <- get_data(all_data, "L2232.india_grid_SubsectorInterp_elec_CEA")
    L2232.india_grid_SubsectorLogit_elec_CEA <- get_data(all_data, "L2232.india_grid_SubsectorLogit_elec_CEA")
    L2232.india_grid_TechShrwt_elec_CEA <- get_data(all_data, "L2232.india_grid_TechShrwt_elec_CEA")
    L2232.india_grid_TechCoef_elec_CEA <- get_data(all_data, "L2232.india_grid_TechCoef_elec_CEA")
    L2232.india_grid_TechCoef_elecownuse_CEA <- get_data(all_data, "L2232.india_grid_TechCoef_elecownuse_CEA")
    L2232.india_grid_Production_imports_CEA <- get_data(all_data, "L2232.india_grid_Production_imports_CEA")
    L2232.india_grid_Production_elec_gen_CEA <- get_data(all_data, "L2232.india_grid_Production_elec_gen_CEA")
    L2232.india_grid_StubTechElecMarket_backup <- get_data(all_data, "L2232.india_grid_StubTechElecMarket_backup")

    # ===================================================
    # Rename tibble columns to match the L2 data names.
    L223.india_state_PassthroughSector_elec <- rename(L223.india_state_PassthroughSector_elec, pass.through.sector = passthrough.sector)
    L223.india_state_PassthroughTech_elec_CEA  <- rename(L223.india_state_PassthroughTech_elec_CEA, pass.through.technology = technology)
    L223.india_state_StubTechProd_elec      <- rename(L223.india_state_StubTechProd_elec, tech.share.weight = share.weight)

    # Produce outputs
    create_xml("electricity_india.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L223.india_state_PassthroughSector_elec, "PassThroughSector") %>%
      add_xml_data(L223.india_state_PassthroughTech_elec_CEA, "PassThroughTech") %>%
      add_logit_tables_xml(L223.india_state_Supplysector_elec_CEA, "Supplysector") %>%
      add_xml_data(L223.india_state_SubsectorShrwtFllt_elec_CEA, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.india_state_SubsectorInterp_elec_CEA, "SubsectorInterp") %>%
      add_logit_tables_xml(L223.india_state_SubsectorLogit_elec_CEA, "SubsectorLogit") %>%
      add_xml_data(L223.india_state_TechShrwt_elec_CEA, "TechShrwt") %>%
      add_xml_data(L223.india_state_TechCoef_elec_CEA, "TechCoef") %>%
      add_xml_data(L223.india_state_Production_elec_CEA, "Production") %>%
      add_xml_data(L223.india_state_InterestRate_CEA, "InterestRate") %>%
      add_xml_data(L223.india_state_Pop_CEA, "Pop") %>%
      add_xml_data(L223.india_state_BaseGDP_CEA, "BaseGDP") %>%
      add_xml_data(L223.india_state_LaborForceFillout_CEA, "LaborForceFillout") %>%
      add_logit_tables_xml(L223.india_state_Supplysector_elec, "Supplysector") %>%
      add_xml_data(L223.india_state_ElecReserve, "ElecReserve") %>%
      add_logit_tables_xml(L223.india_state_SubsectorLogit_elec, "SubsectorLogit") %>%
      add_xml_data(L223.india_state_SubsectorShrwtFllt_elec, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.india_state_SubsectorShrwt_nuc, "SubsectorShrwt") %>%
      add_xml_data(L223.india_state_SubsectorShrwt_renew, "SubsectorShrwt") %>%
      add_xml_data(L223.india_state_SubsectorInterp_elec, "SubsectorInterp") %>%
      add_xml_data(L223.india_state_SubsectorInterpTo_elec, "SubsectorInterpTo") %>%
      add_xml_data(L223.india_state_StubTech_elec, "StubTech") %>%
      add_xml_data(L223.india_state_StubTechEff_elec, "StubTechEff") %>%
      add_xml_data(L223.india_state_StubTechCapFactor_elec, "StubTechCapFactor") %>%
      add_xml_data(L223.india_state_StubTechFixOut_elec, "StubTechFixOut") %>%
      add_xml_data(L223.india_state_StubTechFixOut_hydro, "StubTechFixOut") %>%
      add_xml_data(L223.india_state_StubTechProd_elec, "StubTechProd") %>%
      add_xml_data(L223.india_state_StubTechMarket_elec, "StubTechMarket") %>%
      add_xml_data(L223.india_state_StubTechMarket_backup, "StubTechMarket") %>%
      add_xml_data(L223.india_state_StubTechElecMarket_backup, "StubTechElecMarket") %>%
      add_xml_data(L223.india_state_StubTechCapFactor_elec_wind, "StubTechCapFactor") %>%
      add_xml_data(L223.india_state_StubTechCapFactor_elec_solar, "StubTechCapFactor") %>%
      add_xml_data(L2232.DeleteSupplysector_INDIAelec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L2232.india_state_Supplysector_elec, "Supplysector") %>%
      add_xml_data(L2232.india_grid_SubsectorShrwtFllt_elec, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.india_grid_SubsectorInterp_elec, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.india_grid_SubsectorLogit_elec, "SubsectorLogit") %>%
      add_xml_data(L2232.india_grid_TechShrwt_elec, "TechShrwt") %>%
      add_xml_data(L2232.india_grid_TechCoef_elec, "TechCoef") %>%
      add_xml_data(L2232.india_grid_Production_exports_elec, "Production") %>%
      add_logit_tables_xml(L2232.india_grid_Supplysector_elec_CEA, "Supplysector") %>%
      add_xml_data(L2232.india_grid_ElecReserve_CEA, "ElecReserve") %>%
      add_xml_data(L2232.india_grid_SubsectorShrwtFllt_elec_CEA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.india_grid_SubsectorInterp_elec_CEA, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.india_grid_SubsectorLogit_elec_CEA, "SubsectorLogit") %>%
      add_xml_data(L2232.india_grid_TechShrwt_elec_CEA, "TechShrwt") %>%
      add_xml_data(L2232.india_grid_TechCoef_elec_CEA, "TechCoef") %>%
      add_xml_data(L2232.india_grid_TechCoef_elecownuse_CEA, "TechCoef") %>%
      add_xml_data(L2232.india_grid_Production_imports_CEA, "Production") %>%
      add_xml_data(L2232.india_grid_Production_elec_gen_CEA, "Production") %>%
      add_xml_data(L2232.india_grid_StubTechElecMarket_backup, "StubTechElecMarket") %>%
      add_precursors("L223.india_state_PassthroughSector_elec",
                     "L223.india_state_PassthroughTech_elec_CEA",
                     "L223.india_state_Supplysector_elec_CEA",
                     "L223.india_state_SubsectorShrwtFllt_elec_CEA",
                     "L223.india_state_SubsectorInterp_elec_CEA",
                     "L223.india_state_SubsectorLogit_elec_CEA",
                     "L223.india_state_TechShrwt_elec_CEA",
                     "L223.india_state_TechCoef_elec_CEA",
                     "L223.india_state_Production_elec_CEA",
                     "L223.india_state_InterestRate_CEA",
                     "L223.india_state_Pop_CEA",
                     "L223.india_state_BaseGDP_CEA",
                     "L223.india_state_LaborForceFillout_CEA",
                     "L223.india_state_Supplysector_elec",
                     "L223.india_state_ElecReserve",
                     "L223.india_state_SubsectorLogit_elec",
                     "L223.india_state_SubsectorShrwtFllt_elec",
                     "L223.india_state_SubsectorShrwt_nuc",
                     "L223.india_state_SubsectorShrwt_renew",
                     "L223.india_state_SubsectorInterp_elec",
                     "L223.india_state_SubsectorInterpTo_elec",
                     "L223.india_state_StubTech_elec",
                     "L223.india_state_StubTechEff_elec",
                     "L223.india_state_StubTechCapFactor_elec",
                     "L223.india_state_StubTechFixOut_elec",
                     "L223.india_state_StubTechFixOut_hydro",
                     "L223.india_state_StubTechProd_elec",
                     "L223.india_state_StubTechMarket_elec",
                     "L223.india_state_StubTechMarket_backup",
                     "L223.india_state_StubTechElecMarket_backup",
                     "L223.india_state_StubTechCapFactor_elec_wind",
                     "L223.india_state_StubTechCapFactor_elec_solar",
                     "L2232.DeleteSupplysector_INDIAelec",
                     "L2232.india_state_Supplysector_elec",
                     "L2232.india_grid_SubsectorShrwtFllt_elec",
                     "L2232.india_grid_SubsectorInterp_elec",
                     "L2232.india_grid_SubsectorLogit_elec",
                     "L2232.india_grid_TechShrwt_elec",
                     "L2232.india_grid_TechCoef_elec",
                     "L2232.india_grid_Production_exports_elec",
                     "L2232.india_grid_Supplysector_elec_CEA",
                     "L2232.india_grid_ElecReserve_CEA",
                     "L2232.india_grid_SubsectorShrwtFllt_elec_CEA",
                     "L2232.india_grid_SubsectorInterp_elec_CEA",
                     "L2232.india_grid_SubsectorLogit_elec_CEA",
                     "L2232.india_grid_TechShrwt_elec_CEA",
                     "L2232.india_grid_TechCoef_elec_CEA",
                     "L2232.india_grid_TechCoef_elecownuse_CEA",
                     "L2232.india_grid_Production_imports_CEA",
                     "L2232.india_grid_Production_elec_gen_CEA",
                     "L2232.india_grid_StubTechElecMarket_backup") ->
      electricity_india.xml

    return_data(electricity_india.xml)
  } else {
    stop("Unknown command")
  }
}
