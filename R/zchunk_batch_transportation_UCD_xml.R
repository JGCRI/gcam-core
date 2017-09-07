#' module_energy_batch_transportation_UCD_xml
#'
#' Construct XML data structure for \code{transportation_UCD_xxx.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_xxx.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_xml.R} (energy XML).
module_energy_batch_transportation_UCD_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
               "L254.FinalEnergyKeyword_trn",
               "L254.tranSubsectorLogit",
               "L254.tranSubsectorShrwt",
               "L254.tranSubsectorShrwtFllt",
               "L254.tranSubsectorInterp",
               "L254.tranSubsectorInterpTo",
               "L254.tranSubsectorSpeed",
               "L254.tranSubsectorSpeed_passthru",
               "L254.tranSubsectorSpeed_noVOTT",
               "L254.tranSubsectorSpeed_nonmotor",
               "L254.tranSubsectorVOTT",
               "L254.tranSubsectorFuelPref",
               "L254.StubTranTech",
               "L254.StubTech_passthru",
               "L254.StubTech_nonmotor",
               "L254.GlobalTechShrwt_passthru",
               "L254.GlobalTechShrwt_nonmotor",
               "L254.GlobalTechCoef_passthru",
               "L254.GlobalRenewTech_nonmotor",
               "L254.GlobalTranTechInterp",
               "L254.GlobalTranTechShrwt",
               "L254.GlobalTranTechSCurve",
               "L254.StubTranTechCalInput",
               "L254.StubTranTechLoadFactor",
               "L254.StubTranTechCost",
               "L254.StubTranTechCoef",
               "L254.StubTechCalInput_passthru",
               "L254.StubTechProd_nonmotor",
               "L254.PerCapitaBased_trn",
               "L254.PriceElasticity_trn",
               "L254.IncomeElasticity_trn",
               "L254.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = paste0("transportation_agg_", TRN_SSP, ".xml")))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    # AARON - you'll need to change these calls to match inputs listed above
    L252.FinalEnergyKeyword_trn <- get_data(all_data, "L252.FinalEnergyKeyword_trn")
    L252.SubsectorLogit_trn <- get_data(all_data, "L252.SubsectorLogit_trn")
    L252.SubsectorShrwt_trn <- get_data(all_data, "L252.SubsectorShrwt_trn")
    L252.SubsectorInterp_trn <- get_data(all_data, "L252.SubsectorInterp_trn")
    L252.SubsectorInterpTo_trn <- get_data(all_data, "L252.SubsectorInterpTo_trn")
    L252.StubTech_trn <- get_data(all_data, "L252.StubTech_trn")
    L252.GlobalTechShrwt_trn <- get_data(all_data, "L252.GlobalTechShrwt_trn")
    L252.GlobalTechEff_trn <- get_data(all_data, "L252.GlobalTechEff_trn")
    L252.GlobalTechCost_trn <- get_data(all_data, "L252.GlobalTechCost_trn")
    L252.StubTechCalInput_trn <- get_data(all_data, "L252.StubTechCalInput_trn")
    L252.PerCapitaBased_trn <- get_data(all_data, "L252.PerCapitaBased_trn")
    L252.PriceElasticity_trn <- get_data(all_data, "L252.PriceElasticity_trn")
    L252.BaseService_trn <- get_data(all_data, "L252.BaseService_trn")

    # ===================================================

    # Produce outputs
    # ...AARON - similarly, need to change these to use inputs above
    create_xml("transportation_agg.xml") %>%
      add_xml_data(L252.FinalEnergyKeyword_trn,"FinalEnergyKeyword") %>%
      add_xml_data(L252.SubsectorLogit_trn,"SubsectorLogit") %>%
      add_xml_data(L252.SubsectorShrwt_trn,"SubsectorShrwt") %>%
      add_xml_data(L252.SubsectorInterp_trn,"SubsectorInterp") %>%
      add_xml_data(L252.SubsectorInterpTo_trn,"SubsectorInterpTo") %>%
      add_xml_data(L252.StubTech_trn,"StubTech") %>%
      add_xml_data(L252.GlobalTechShrwt_trn,"GlobalTechShrwt") %>%
      add_xml_data(L252.GlobalTechEff_trn,"GlobalTechEff") %>%
      add_xml_data(L252.GlobalTechCost_trn,"GlobalTechCost") %>%
      add_xml_data(L252.StubTechCalInput_trn,"StubTechCalInput") %>%
      add_xml_data(L252.PerCapitaBased_trn,"PerCapitaBased") %>%
      add_xml_data(L252.PriceElasticity_trn,"PriceElasticity") %>%
      add_xml_data(L252.BaseService_trn,"BaseService") %>%
      add_precursors("L252.FinalEnergyKeyword_trn", "L252.SubsectorLogit_trn", "L252.SubsectorShrwt_trn", "L252.SubsectorInterp_trn", "L252.SubsectorInterpTo_trn", "L252.StubTech_trn", "L252.GlobalTechShrwt_trn", "L252.GlobalTechEff_trn", "L252.GlobalTechCost_trn", "L252.StubTechCalInput_trn", "L252.PerCapitaBased_trn", "L252.PriceElasticity_trn", "L252.BaseService_trn") ->
      transportation_agg.xml

    return_data(transportation_agg.xml)
  } else {
    stop("Unknown command")
  }
}
