# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_electricity_water_xml
#'
#' Construct XML data structure for \code{electricity_water.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water.xml.R} (water XML).
module_water_batch_electricity_water_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.Supplysector_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.ElecReserve",
             "L223.SectorUseTrialMarket_elec",
             "L223.StubTechCapFactor_elec",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwt_coal",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L2233.AvgFossilEffKeyword_elec_cool",
             "L2233.GlobalIntTechBackup_elec_cool",
             "L2233.GlobalIntTechCapFac_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.GlobalIntTechLifetime_elec_cool",
             "L2233.GlobalIntTechShrwt_elec_cool",
             "L2233.GlobalTechCapFac_elec_cool",
             "L2233.GlobalTechCapture_elec_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool",
             "L2233.GlobalTechProfitShutdown_elec_cool",
             "L2233.GlobalTechSCurve_elec_cool",
             "L2233.GlobalTechShrwt_elec_cool",
             "L2233.PrimaryRenewKeyword_elec_cool",
             "L2233.PrimaryRenewKeywordInt_elec_cool",
             "L2233.StubTech_elecPassthru",
             "L2233.StubTechProd_elecPassthru",
             "L2233.GlobalPassThroughTech",
             "L2233.GlobalTechEff_elecPassthru",
             "L2233.GlobalTechShrwt_elecPassthru",
             "L2233.GlobalIntTechCapital_elec",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalIntTechOMfixed_elec",
             "L2233.GlobalTechOMfixed_elecPassthru",
             "L2233.GlobalIntTechOMvar_elec",
             "L2233.GlobalTechOMvar_elecPassthru",
             "L2233.GlobalTechInterp_elecPassthru",
             "L2233.PassThroughSector_elec_cool",
             "L2233.Supplysector_elec_cool",
             "L2233.ElecReserve_elec_cool",
             "L2233.SubsectorShrwtFllt_elec_cool",
             "L2233.SubsectorLogit_elec_cool",
             "L2233.StubTech_elec_cool",
             "L2233.StubTechEff_elec_cool",
             "L2233.StubTechProd_elec_cool",
             "L2233.StubTechCapFactor_elec_cool",
             "L2233.StubTechSecOut_desal_elec_cool",
             "L2233.StubTechFixOut_hydro",
             "L2233.StubTechShrwt_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L223.GlobalTechCapFac_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SectorUseTrialMarket_elec <- get_data(all_data, "L223.SectorUseTrialMarket_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwt_coal <- get_data(all_data, "L223.SubsectorShrwt_coal")
    L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    L2233.AvgFossilEffKeyword_elec_cool <- get_data(all_data, "L2233.AvgFossilEffKeyword_elec_cool")
    L2233.GlobalIntTechBackup_elec_cool <- get_data(all_data, "L2233.GlobalIntTechBackup_elec_cool")
    L2233.GlobalIntTechCapFac_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapFac_elec_cool")
    L2233.GlobalIntTechEff_elec_cool <- get_data(all_data, "L2233.GlobalIntTechEff_elec_cool")
    L2233.GlobalIntTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalIntTechLifetime_elec_cool")
    L2233.GlobalIntTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalIntTechShrwt_elec_cool")
    L2233.GlobalTechCapFac_elec_cool <- get_data(all_data, "L2233.GlobalTechCapFac_elec_cool")
    L2233.GlobalTechCapture_elec_cool <- get_data(all_data, "L2233.GlobalTechCapture_elec_cool")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")
    L2233.GlobalTechLifetime_elec_cool <- get_data(all_data, "L2233.GlobalTechLifetime_elec_cool")
    L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.GlobalTechProfitShutdown_elec_cool")
    L2233.GlobalTechSCurve_elec_cool <- get_data(all_data, "L2233.GlobalTechSCurve_elec_cool")
    L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalTechShrwt_elec_cool")
    L2233.PrimaryRenewKeyword_elec_cool <- get_data(all_data, "L2233.PrimaryRenewKeyword_elec_cool")
    L2233.PrimaryRenewKeywordInt_elec_cool <- get_data(all_data, "L2233.PrimaryRenewKeywordInt_elec_cool")
    L2233.StubTech_elecPassthru <- get_data(all_data, "L2233.StubTech_elecPassthru")
    L2233.StubTechProd_elecPassthru <- get_data(all_data, "L2233.StubTechProd_elecPassthru")
    L2233.GlobalPassThroughTech <- get_data(all_data, "L2233.GlobalPassThroughTech")
    L2233.GlobalTechEff_elecPassthru <- get_data(all_data, "L2233.GlobalTechEff_elecPassthru")
    L2233.GlobalTechShrwt_elecPassthru <- get_data(all_data, "L2233.GlobalTechShrwt_elecPassthru")
    L2233.GlobalIntTechCapital_elec <- get_data(all_data, "L2233.GlobalIntTechCapital_elec")
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru")
    L2233.GlobalIntTechOMfixed_elec <- get_data(all_data, "L2233.GlobalIntTechOMfixed_elec")
    L2233.GlobalTechOMfixed_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMfixed_elecPassthru")
    L2233.GlobalIntTechOMvar_elec <- get_data(all_data, "L2233.GlobalIntTechOMvar_elec")
    L2233.GlobalTechOMvar_elecPassthru <- get_data(all_data, "L2233.GlobalTechOMvar_elecPassthru")
    L2233.GlobalTechInterp_elecPassthru <- get_data(all_data, "L2233.GlobalTechInterp_elecPassthru")
    L2233.PassThroughSector_elec_cool <- get_data(all_data, "L2233.PassThroughSector_elec_cool")
    L2233.Supplysector_elec_cool <- get_data(all_data, "L2233.Supplysector_elec_cool")
    L2233.ElecReserve_elec_cool <- get_data(all_data, "L2233.ElecReserve_elec_cool")
    L2233.SubsectorShrwtFllt_elec_cool <- get_data(all_data, "L2233.SubsectorShrwtFllt_elec_cool")
    L2233.SubsectorLogit_elec_cool <- get_data(all_data, "L2233.SubsectorLogit_elec_cool")
    L2233.StubTech_elec_cool <- get_data(all_data, "L2233.StubTech_elec_cool")
    L2233.StubTechEff_elec_cool <- get_data(all_data, "L2233.StubTechEff_elec_cool")
    L2233.StubTechSecOut_desal_elec_cool <- get_data(all_data, "L2233.StubTechSecOut_desal_elec_cool")
    L2233.StubTechProd_elec_cool <- get_data(all_data, "L2233.StubTechProd_elec_cool")
    L2233.StubTechCapFactor_elec_cool <- get_data(all_data, "L2233.StubTechCapFactor_elec_cool")
    L2233.StubTechFixOut_hydro <- get_data(all_data, "L2233.StubTechFixOut_hydro")
    L2233.StubTechShrwt_elec_cool <- get_data(all_data, "L2233.StubTechShrwt_elec_cool")
    L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool")
    L2233.GlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapital_elec_cool")
    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec")


    # Silence package checks
    technology <- NULL


    # ===================================================

    # Rename columns to match add_xml_data header expeectations.
    L2233.GlobalIntTechEff_elec_cool      <- rename(L2233.GlobalIntTechEff_elec_cool, `intermittent.technology` = technology)
    L2233.GlobalIntTechLifetime_elec_cool <- rename(L2233.GlobalIntTechLifetime_elec_cool, `intermittent.technology` = technology )
    L2233.GlobalIntTechShrwt_elec_cool    <- rename(L2233.GlobalIntTechShrwt_elec_cool,  `intermittent.technology` = technology )
    L2233.GlobalIntTechCapFac_elec_cool   <- rename(L2233.GlobalIntTechCapFac_elec_cool,  `intermittent.technology` = technology )


    # Produce outputs
    create_xml("electricity_water.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Supplysector_elec, "Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.ElecReserve, "ElecReserve") %>%
      add_xml_data(L223.SectorUseTrialMarket_elec, "SectorUseTrialMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec, "StubTechCapFactor") %>%
      add_xml_data(L223.SubsectorInterp_elec, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwt_coal, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew, "SubsectorShrwt") %>%
      add_xml_data(L2233.AvgFossilEffKeyword_elec_cool, "AvgFossilEffKeyword") %>%
      add_xml_data(L2233.GlobalIntTechBackup_elec_cool, "GlobalIntTechBackup") %>%
      add_xml_data(L2233.GlobalIntTechCapFac_elec_cool, "GlobalIntTechCapFac") %>%
      add_xml_data(L2233.GlobalIntTechEff_elec_cool, "GlobalIntTechEff") %>%
      add_xml_data(L2233.GlobalIntTechLifetime_elec_cool, "GlobalIntTechLifetime") %>%
      add_xml_data(L2233.GlobalIntTechShrwt_elec_cool, "GlobalIntTechShrwt") %>%
      add_xml_data(L2233.GlobalTechCapFac_elec_cool, "GlobalTechCapFac") %>%
      add_xml_data(L2233.GlobalTechCapture_elec_cool, "GlobalTechCapture") %>%
      add_xml_data(L2233.GlobalTechEff_elec_cool, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechLifetime_elec_cool, "GlobalTechLifetime") %>%
      add_xml_data(L2233.GlobalTechProfitShutdown_elec_cool, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2233.GlobalTechSCurve_elec_cool, "GlobalTechSCurve") %>%
      add_xml_data(L2233.GlobalTechShrwt_elec_cool, "GlobalTechShrwt") %>%
      add_xml_data(L2233.PrimaryRenewKeyword_elec_cool, "PrimaryRenewKeyword") %>%
      add_xml_data(L2233.PrimaryRenewKeywordInt_elec_cool, "PrimaryRenewKeywordInt") %>%
      add_xml_data(L2233.StubTech_elecPassthru, "StubTech") %>%
      add_xml_data(L2233.StubTechProd_elecPassthru, "StubTechProd") %>%
      add_xml_data(L2233.GlobalPassThroughTech, "GlobalPassThroughTech") %>%
      add_xml_data(L2233.GlobalTechEff_elecPassthru, "GlobalTechEff") %>%
      add_xml_data(L2233.GlobalTechShrwt_elecPassthru, "GlobalTechShrwt") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elec, "GlobalIntTechCapital", "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechCapital_elecPassthru, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed", "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecPassthru, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar", "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecPassthru, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalTechInterp_elecPassthru, "GlobalTechInterp") %>%
      add_xml_data(L2233.PassThroughSector_elec_cool, "PassThroughSector") %>%
      add_logit_tables_xml(L2233.Supplysector_elec_cool, "Supplysector") %>%
      add_xml_data(L2233.ElecReserve_elec_cool, "ElecReserve") %>%
      add_xml_data(L2233.SubsectorShrwtFllt_elec_cool, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L2233.SubsectorLogit_elec_cool, "SubsectorLogit") %>%
      add_xml_data(L2233.StubTech_elec_cool, "StubTech") %>%
      add_xml_data(L2233.StubTechEff_elec_cool, "StubTechEff") %>%
      add_xml_data(L2233.StubTechSecOut_desal_elec_cool, "StubTechSecOut") %>%
      add_xml_data(L2233.StubTechProd_elec_cool, "StubTechProd") %>%
      add_xml_data(L2233.StubTechCapFactor_elec_cool, "StubTechCapFactor") %>%
      add_xml_data(L2233.StubTechFixOut_hydro, "StubTechFixOut") %>%
      add_xml_data(L2233.StubTechShrwt_elec_cool, "StubTechShrwt") %>%
      add_xml_data(L2233.GlobalTechCapital_elec_cool, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elec_cool, "GlobalIntTechCapital", "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalTechCapFac_elec, "GlobalTechCapFac") %>%
      add_precursors("L223.Supplysector_elec",
                     "L223.SubsectorShrwtFllt_elec",
                     "L223.ElecReserve",
                     "L223.SectorUseTrialMarket_elec",
                     "L223.StubTechCapFactor_elec",
                     "L223.SubsectorInterp_elec",
                     "L223.SubsectorInterpTo_elec",
                     "L223.SubsectorLogit_elec",
                     "L223.SubsectorShrwt_coal",
                     "L223.SubsectorShrwt_nuc",
                     "L223.SubsectorShrwt_renew",
                     "L2233.AvgFossilEffKeyword_elec_cool",
                     "L2233.GlobalIntTechBackup_elec_cool",
                     "L2233.GlobalIntTechCapFac_elec_cool",
                     "L2233.GlobalIntTechEff_elec_cool",
                     "L2233.GlobalIntTechLifetime_elec_cool",
                     "L2233.GlobalIntTechShrwt_elec_cool",
                     "L2233.GlobalTechCapFac_elec_cool",
                     "L2233.GlobalTechCapture_elec_cool",
                     "L2233.GlobalTechEff_elec_cool",
                     "L2233.GlobalTechLifetime_elec_cool",
                     "L2233.GlobalTechProfitShutdown_elec_cool",
                     "L2233.GlobalTechSCurve_elec_cool",
                     "L2233.GlobalTechShrwt_elec_cool",
                     "L2233.PrimaryRenewKeyword_elec_cool",
                     "L2233.PrimaryRenewKeywordInt_elec_cool",
                     "L2233.StubTech_elecPassthru",
                     "L2233.StubTechProd_elecPassthru",
                     "L2233.GlobalPassThroughTech",
                     "L2233.GlobalTechEff_elecPassthru",
                     "L2233.GlobalTechShrwt_elecPassthru",
                     "L2233.GlobalIntTechCapital_elec",
                     "L2233.GlobalTechCapital_elecPassthru",
                     "L2233.GlobalIntTechOMfixed_elec",
                     "L2233.GlobalTechOMfixed_elecPassthru",
                     "L2233.GlobalIntTechOMvar_elec",
                     "L2233.GlobalTechOMvar_elecPassthru",
                     "L2233.GlobalTechInterp_elecPassthru",
                     "L2233.PassThroughSector_elec_cool",
                     "L2233.Supplysector_elec_cool",
                     "L2233.ElecReserve_elec_cool",
                     "L2233.SubsectorShrwtFllt_elec_cool",
                     "L2233.SubsectorLogit_elec_cool",
                     "L2233.StubTech_elec_cool",
                     "L2233.StubTechEff_elec_cool",
                     "L2233.StubTechProd_elec_cool",
                     "L2233.StubTechCapFactor_elec_cool",
                     "L2233.StubTechSecOut_desal_elec_cool",
                     "L2233.StubTechFixOut_hydro",
                     "L2233.StubTechShrwt_elec_cool",
                     "L2233.GlobalTechCapital_elec_cool",
                     "L2233.GlobalIntTechCapital_elec_cool",
                     "L223.GlobalTechCapFac_elec") ->
      electricity_water.xml

    return_data(electricity_water.xml)
  } else {
    stop("Unknown command")
  }
}
