# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_electricity_xml
#'
#' Construct XML data structure for \code{electricity.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity.xml}. The corresponding file in the
#' original data system was \code{batch_electricity.xml.R} (energy XML).
module_energy_batch_electricity_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.Supplysector_elec",
              "L223.ElecReserve",
              "L223.SectorUseTrialMarket_elec",
              "L223.SubsectorLogit_elec",
              "L223.SubsectorShrwtFllt_elec",
              "L223.SubsectorShrwt_elec",
              "L223.SubsectorShrwt_coal",
              "L223.SubsectorShrwt_nuc",
              "L223.SubsectorShrwt_renew",
              "L223.SubsectorInterp_elec",
              "L223.SubsectorInterpTo_elec",
              "L223.StubTech_elec",
              "L223.GlobalIntTechEff_elec",
              "L223.GlobalTechEff_elec",
              "L223.GlobalTechCapFac_elec",
              "L223.GlobalIntTechCapFac_elec",
              "L223.GlobalTechCapital_elec",
              "L223.GlobalIntTechCapital_elec",
              "L223.GlobalTechOMfixed_elec",
              "L223.GlobalIntTechOMfixed_elec",
              "L223.GlobalTechOMvar_elec",
              "L223.GlobalIntTechOMvar_elec",
              "L223.GlobalTechShrwt_elec",
              "L223.GlobalTechInterp_elec",
              "L223.GlobalIntTechShrwt_elec",
              "L223.PrimaryRenewKeyword_elec",
              "L223.PrimaryRenewKeywordInt_elec",
              "L223.AvgFossilEffKeyword_elec",
              "L223.GlobalTechCapture_elec",
              "L223.GlobalIntTechBackup_elec",
              "L223.StubTechCapFactor_elec",
              "L223.StubTechCost_offshore_wind",
              "L223.GlobalTechSCurve_elec",
              "L223.GlobalTechLifetime_elec",
              "L223.GlobalIntTechLifetime_elec",
              "L223.GlobalTechProfitShutdown_elec",
              "L223.StubTechCalInput_elec",
              "L223.StubTechFixOut_elec",
              "L223.StubTechFixOut_hydro",
              "L223.StubTechProd_elec",
              "L223.StubTechEff_elec",
              "L223.StubTechSecOut_desal"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SectorUseTrialMarket_elec <- get_data(all_data, "L223.SectorUseTrialMarket_elec")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    L223.SubsectorShrwt_elec <- get_data(all_data, "L223.SubsectorShrwt_elec")
    L223.SubsectorShrwt_coal <- get_data(all_data, "L223.SubsectorShrwt_coal")
    L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    L223.GlobalIntTechEff_elec <- get_data(all_data, "L223.GlobalIntTechEff_elec")
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")
    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec")
    L223.GlobalIntTechCapFac_elec <- get_data(all_data, "L223.GlobalIntTechCapFac_elec")
    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec")
    L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec")
    L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec")
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec")
    L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec")
    L223.GlobalIntTechOMvar_elec <- get_data(all_data, "L223.GlobalIntTechOMvar_elec")
    L223.GlobalTechShrwt_elec <- get_data(all_data, "L223.GlobalTechShrwt_elec")
    L223.GlobalTechInterp_elec <- get_data(all_data, "L223.GlobalTechInterp_elec")
    L223.GlobalIntTechShrwt_elec <- get_data(all_data, "L223.GlobalIntTechShrwt_elec")
    L223.PrimaryRenewKeyword_elec <- get_data(all_data, "L223.PrimaryRenewKeyword_elec")
    L223.PrimaryRenewKeywordInt_elec <- get_data(all_data, "L223.PrimaryRenewKeywordInt_elec")
    L223.AvgFossilEffKeyword_elec <- get_data(all_data, "L223.AvgFossilEffKeyword_elec")
    L223.GlobalTechCapture_elec <- get_data(all_data, "L223.GlobalTechCapture_elec")
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.StubTechCost_offshore_wind<- get_data(all_data, "L223.StubTechCost_offshore_wind")
    L223.GlobalTechSCurve_elec <- get_data(all_data, "L223.GlobalTechSCurve_elec")
    L223.GlobalTechLifetime_elec <- get_data(all_data, "L223.GlobalTechLifetime_elec")
    L223.GlobalIntTechLifetime_elec <- get_data(all_data, "L223.GlobalIntTechLifetime_elec")
    L223.GlobalTechProfitShutdown_elec <- get_data(all_data, "L223.GlobalTechProfitShutdown_elec")
    L223.StubTechCalInput_elec <- get_data(all_data, "L223.StubTechCalInput_elec")
    L223.StubTechFixOut_elec <- get_data(all_data, "L223.StubTechFixOut_elec")
    L223.StubTechFixOut_hydro <- get_data(all_data, "L223.StubTechFixOut_hydro")
    L223.StubTechProd_elec <- get_data(all_data, "L223.StubTechProd_elec")
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    L223.StubTechSecOut_desal <- get_data(all_data, "L223.StubTechSecOut_desal")

    share.weight <- subsector.share.weight <- intermittent.technology <- NULL # silence package checks

    # ===================================================

    # Rename columns to match header information.
    L223.PrimaryRenewKeywordInt_elec <- rename(L223.PrimaryRenewKeywordInt_elec, technology = intermittent.technology)
    L223.StubTechFixOut_elec <- rename(L223.StubTechFixOut_elec, subs.share.weight = subsector.share.weight, tech.share.weight = share.weight)
    L223.StubTechProd_elec   <- rename(L223.StubTechProd_elec, tech.share.weight = share.weight)


    # Produce outputs
    create_xml("electricity.xml") %>%
      add_logit_tables_xml(L223.Supplysector_elec, "Supplysector") %>%
      add_xml_data(L223.ElecReserve, "ElecReserve") %>%
      add_xml_data(L223.SectorUseTrialMarket_elec, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorShrwt_elec, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_coal, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_elec, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_elec, "StubTech") %>%
      add_xml_data(L223.GlobalIntTechEff_elec, "GlobalIntTechEff") %>%
      add_xml_data(L223.GlobalTechEff_elec, "GlobalTechEff") %>%
      add_xml_data(L223.GlobalTechCapFac_elec, "GlobalTechCapFac") %>%
      add_xml_data(L223.GlobalIntTechCapFac_elec, "GlobalIntTechCapFac") %>%
      add_xml_data(L223.GlobalTechCapital_elec, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalIntTechCapital_elec, "GlobalIntTechCapital") %>%
      add_xml_data(L223.GlobalTechOMfixed_elec, "GlobalTechOMfixed") %>%
      add_xml_data(L223.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed") %>%
      add_xml_data(L223.GlobalTechOMvar_elec, "GlobalTechOMvar") %>%
      add_xml_data(L223.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar") %>%
      add_xml_data(L223.GlobalTechShrwt_elec, "GlobalTechShrwt") %>%
      add_xml_data(L223.GlobalTechInterp_elec, "GlobalTechInterp") %>%
      add_xml_data(L223.GlobalIntTechShrwt_elec, "GlobalIntTechShrwt") %>%
      add_xml_data(L223.PrimaryRenewKeyword_elec, "PrimaryRenewKeyword") %>%
      add_xml_data(L223.PrimaryRenewKeywordInt_elec, "PrimaryRenewKeywordInt") %>%
      add_xml_data(L223.AvgFossilEffKeyword_elec, "AvgFossilEffKeyword") %>%
      add_xml_data(L223.GlobalTechCapture_elec, "GlobalTechCapture") %>%
      add_xml_data(L223.GlobalIntTechBackup_elec, "GlobalIntTechBackup") %>%
      add_xml_data(L223.StubTechCapFactor_elec, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechCost_offshore_wind, "StubTechCost") %>%
      add_xml_data(L223.GlobalTechSCurve_elec, "GlobalTechSCurve") %>%
      add_xml_data(L223.GlobalTechLifetime_elec, "GlobalTechLifetime") %>%
      add_xml_data(L223.GlobalIntTechLifetime_elec, "GlobalIntTechLifetime") %>%
      add_xml_data(L223.GlobalTechProfitShutdown_elec, "GlobalTechProfitShutdown") %>%
      add_xml_data(L223.StubTechCalInput_elec, "StubTechCalInput") %>%
      add_xml_data(L223.StubTechFixOut_elec, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec, "StubTechProd") %>%
      add_xml_data(L223.StubTechEff_elec, "StubTechEff") %>%
      add_xml_data(L223.StubTechSecOut_desal, "StubTechSecOut") %>%
      add_precursors("L223.Supplysector_elec",
                     "L223.ElecReserve",
                     "L223.SectorUseTrialMarket_elec",
                     "L223.SubsectorLogit_elec",
                     "L223.SubsectorShrwtFllt_elec",
                     "L223.SubsectorShrwt_elec",
                     "L223.SubsectorShrwt_coal",
                     "L223.SubsectorShrwt_nuc",
                     "L223.SubsectorShrwt_renew",
                     "L223.SubsectorInterp_elec",
                     "L223.SubsectorInterpTo_elec",
                     "L223.StubTech_elec",
                     "L223.GlobalIntTechEff_elec",
                     "L223.GlobalTechEff_elec",
                     "L223.GlobalTechCapFac_elec",
                     "L223.GlobalIntTechCapFac_elec",
                     "L223.GlobalTechCapital_elec",
                     "L223.GlobalIntTechCapital_elec",
                     "L223.GlobalTechOMfixed_elec",
                     "L223.GlobalIntTechOMfixed_elec",
                     "L223.GlobalTechOMvar_elec",
                     "L223.GlobalIntTechOMvar_elec",
                     "L223.GlobalTechShrwt_elec",
                     "L223.GlobalTechInterp_elec",
                     "L223.GlobalIntTechShrwt_elec",
                     "L223.PrimaryRenewKeyword_elec",
                     "L223.PrimaryRenewKeywordInt_elec",
                     "L223.AvgFossilEffKeyword_elec",
                     "L223.GlobalTechCapture_elec",
                     "L223.GlobalIntTechBackup_elec",
                     "L223.StubTechCapFactor_elec",
                     "L223.StubTechCost_offshore_wind",
                     "L223.GlobalTechSCurve_elec",
                     "L223.GlobalTechLifetime_elec",
                     "L223.GlobalIntTechLifetime_elec",
                     "L223.GlobalTechProfitShutdown_elec",
                     "L223.StubTechCalInput_elec",
                     "L223.StubTechFixOut_elec",
                     "L223.StubTechFixOut_hydro",
                     "L223.StubTechProd_elec",
                     "L223.StubTechEff_elec",
                     "L223.StubTechSecOut_desal") ->
      electricity.xml

    return_data(electricity.xml)
  } else {
    stop("Unknown command")
  }
}
