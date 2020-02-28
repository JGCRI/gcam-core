# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_transportation_UCD_CORE_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_batch_transportation_UCD_CORE_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.
  outfile <- paste0("transportation_UCD_", energy.TRN_SSP, ".xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
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
    return(c(XML = outfile))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.GlobalTechShrwt_passthru<- get_data(all_data, "L254.GlobalTechShrwt_passthru")
    L254.GlobalTechShrwt_nonmotor <- get_data(all_data, "L254.GlobalTechShrwt_nonmotor")
    L254.GlobalTechCoef_passthru <- get_data(all_data, "L254.GlobalTechCoef_passthru")
    L254.GlobalRenewTech_nonmotor <- get_data(all_data, "L254.GlobalRenewTech_nonmotor")
    L254.GlobalTranTechInterp <- get_data(all_data, "L254.GlobalTranTechInterp")
    L254.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt")
    L254.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve")
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.StubTechCalInput_passthru <- get_data(all_data, "L254.StubTechCalInput_passthru")
    L254.StubTechProd_nonmotor <- get_data(all_data, "L254.StubTechProd_nonmotor")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn")


    # ===================================================

    # Produce outputs
    create_xml(outfile) %>%
      add_logit_tables_xml(L254.Supplysector_trn, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT, "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech, "StubTranTech") %>%
      add_xml_data(L254.StubTech_passthru, "StubTranTech") %>%
      add_xml_data(L254.StubTech_nonmotor, "StubTranTech") %>%
      add_xml_data(L254.GlobalTechShrwt_passthru, "GlobalTechShrwt") %>%
      add_xml_data(L254.GlobalTechShrwt_nonmotor, "GlobalTechShrwt") %>%
      add_xml_data(L254.GlobalTechCoef_passthru, "GlobalTechCoef") %>%
      add_xml_data(L254.GlobalRenewTech_nonmotor, "GlobalRenewTech") %>%
      add_xml_data(L254.GlobalTranTechInterp, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt, "GlobalTranTechShrwt") %>%
      add_xml_data(L254.GlobalTranTechSCurve, "GlobalTranTechSCurve") %>%
      add_xml_data(L254.StubTranTechCalInput, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechLoadFactor, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef, "StubTranTechCoef") %>%
      add_xml_data(L254.StubTechCalInput_passthru, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTechProd_nonmotor, "StubTranTechProd") %>%
      add_xml_data(L254.PerCapitaBased_trn, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn, "IncomeElasticity") %>%
      add_xml_data(L254.BaseService_trn, "BaseService") %>%
      add_precursors("L254.Supplysector_trn",
                     "L254.FinalEnergyKeyword_trn",
                     "L254.tranSubsectorLogit",
                     "L254.tranSubsectorShrwtFllt",
                     "L254.tranSubsectorInterp",
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
                     "L254.BaseService_trn") ->
      xml_tmp

    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
      transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- NULL  # silence package check notes
    assign(paste0("transportation_UCD_", energy.TRN_SSP, ".xml"), xml_tmp)
    if(energy.TRN_SSP == "CORE") {
      return_data(transportation_UCD_CORE.xml)
    } else if(energy.TRN_SSP == "SSP1") {
      return_data(transportation_UCD_SSP1.xml)
    } else if(energy.TRN_SSP == "SSP2") {
      return_data(transportation_UCD_SSP2.xml)
    } else if(energy.TRN_SSP == "SSP3") {
      return_data(transportation_UCD_SSP3.xml)
    } else if(energy.TRN_SSP == "SSP5") {
      return_data(transportation_UCD_SSP5.xml)
    } else {
      stop("Unknown energy.TRN_SSP value:", energy.TRN_SSP)
    }

  } else {
    stop("Unknown command")
  }
}
