# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_dac_xml
#'
#' Construct XML data structure for \code{dac.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac.xml}. The corresponding file in the
#' original data system was \code{batch_dac_xml.R} (energy XML).
#' @author JF March 2021
module_energy_batch_dac_xml <- function(command, ...) {


  TECH_PARAMETRIZATION_INPUTS <- paste0("ssp", 1:5)
  if(command == driver.DECLARE_INPUTS) {
    return(c("L262.CarbonCoef_dac",
             "L262.Supplysector_dac",
             "L262.FinalEnergyKeyword_dac",
             "L262.SubsectorLogit_dac",
             "L262.SubsectorShrwtFllt_dac",
             "L262.SubsectorInterp_dac",
             "L262.StubTech_dac",
             "L262.GlobalTechShrwt_dac",
             c(paste("L262.GlobalTechShrwt_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             c(paste("L262.GlobalTechCoef_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             c(paste("L262.GlobalTechCost_dac", tolower(TECH_PARAMETRIZATION_INPUTS), sep = "_")),
             "L262.GlobalTechCapture_dac",
             "L262.StubTechProd_dac",
             "L262.PerCapitaBased_dac",
             "L262.BaseService_dac",
             "L262.PriceElasticity_dac",
             "L262.GlobalTechSCurve_dac",
             "L262.GlobalTechProfitShutdown_dac"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_ssp1.xml",
             XML = "dac_ssp2.xml",
             XML = "dac_ssp3.xml",
             XML = "dac_ssp4.xml",
             XML = "dac_ssp5.xml"))
  } else if(command == driver.MAKE) {

    # Silence package check notes
    dac_ssp1.xml <- dac_ssp2.xml <- dac_ssp3.xml <- dac_ssp4.xml <- dac_ssp5.xml <- NULL

    all_data <- list(...)[[1]]

    for(sce in TECH_PARAMETRIZATION_INPUTS){
    # Load required inputs
    L262.CarbonCoef_dac <- get_data(all_data, "L262.CarbonCoef_dac")
    L262.Supplysector_dac <- get_data(all_data, "L262.Supplysector_dac")
    L262.FinalEnergyKeyword_dac <- get_data(all_data, "L262.FinalEnergyKeyword_dac")
    L262.SubsectorLogit_dac <- get_data(all_data, "L262.SubsectorLogit_dac")
    L262.SubsectorShrwtFllt_dac <- get_data(all_data, "L262.SubsectorShrwtFllt_dac")
    L262.SubsectorInterp_dac <- get_data(all_data, "L262.SubsectorInterp_dac")
    L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac")
    L262.GlobalTechShrwt_dac <- get_data(all_data, "L262.GlobalTechShrwt_dac")

    coef_name <- paste0("L262.GlobalTechCoef_dac_",tolower(sce))
    cost_name <- paste0("L262.GlobalTechCost_dac_",tolower(sce))
    shwt_name <- paste0("L262.GlobalTechShrwt_dac_",tolower(sce))

    L262.GlobalTechCoef_dac <- get_data(all_data, coef_name)
    L262.GlobalTechCost_dac <- get_data(all_data, cost_name)
    L262.GlobalTechShrwt_dac <- get_data(all_data, shwt_name)

    L262.GlobalTechCapture_dac <- get_data(all_data, "L262.GlobalTechCapture_dac")
    L262.StubTechProd_dac <- get_data(all_data, "L262.StubTechProd_dac")
    L262.PerCapitaBased_dac <- get_data(all_data, "L262.PerCapitaBased_dac")
    L262.BaseService_dac <- get_data(all_data, "L262.BaseService_dac")
    L262.PriceElasticity_dac <- get_data(all_data, "L262.PriceElasticity_dac")
    L262.GlobalTechSCurve_dac <- get_data(all_data, "L262.GlobalTechSCurve_dac")
    L262.GlobalTechProfitShutdown_dac <- get_data(all_data, "L262.GlobalTechProfitShutdown_dac")



    xmlfn <- paste0("dac_",tolower(sce), '.xml')

    # ===================================================
    # Produce outputs
    create_xml(xmlfn) %>%
      add_xml_data(L262.CarbonCoef_dac, "CarbonCoef") %>%
      add_logit_tables_xml(L262.Supplysector_dac, "Supplysector") %>%
      add_xml_data(L262.FinalEnergyKeyword_dac, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L262.SubsectorLogit_dac, "SubsectorLogit") %>%
      add_xml_data(L262.SubsectorShrwtFllt_dac, "SubsectorShrwtFllt") %>%
      add_xml_data(L262.SubsectorInterp_dac, "SubsectorInterp") %>%
      add_xml_data(L262.StubTech_dac, "StubTech") %>%
      add_xml_data(L262.GlobalTechShrwt_dac, "GlobalTechShrwt") %>%
      add_xml_data(L262.GlobalTechCoef_dac, "GlobalTechCoef") %>%
      add_xml_data(L262.GlobalTechCost_dac, "GlobalTechCost") %>%
      add_xml_data(L262.GlobalTechCapture_dac, "GlobalTechCapture") %>%
      add_xml_data(L262.StubTechProd_dac, "StubTechProd") %>%
      add_xml_data(L262.PerCapitaBased_dac, "PerCapitaBased") %>%
      add_xml_data(L262.BaseService_dac, "BaseService") %>%
      add_xml_data(L262.PriceElasticity_dac, "PriceElasticity") %>%
      add_xml_data(L262.GlobalTechSCurve_dac, "GlobalTechSCurve") %>%
      add_xml_data(L262.GlobalTechProfitShutdown_dac, "GlobalTechProfitShutdown") %>%
      add_precursors("L262.CarbonCoef_dac",
                     "L262.Supplysector_dac",
                     "L262.FinalEnergyKeyword_dac",
                     "L262.SubsectorLogit_dac",
                     "L262.SubsectorShrwtFllt_dac",
                     "L262.SubsectorInterp_dac",
                     "L262.StubTechProd_dac",
                     "L262.StubTech_dac",
                     "L262.GlobalTechShrwt_dac",
                     paste0("L262.GlobalTechShrwt_dac_",tolower(sce)),
                     paste0("L262.GlobalTechCoef_dac_",tolower(sce)),
                     paste0("L262.GlobalTechCost_dac_",tolower(sce)),
                     "L262.GlobalTechCapture_dac",
                     "L262.PerCapitaBased_dac",
                     "L262.BaseService_dac",
                     "L262.PriceElasticity_dac",
                     "L262.GlobalTechSCurve_dac",
                     "L262.GlobalTechProfitShutdown_dac") ->
      xmlobj
    assign(xmlfn, xmlobj)


  }
  return_data(dac_ssp1.xml,
              dac_ssp2.xml,
              dac_ssp3.xml,
              dac_ssp4.xml,
              dac_ssp5.xml)}
  else {
    stop("Unknown command")
  }
}
