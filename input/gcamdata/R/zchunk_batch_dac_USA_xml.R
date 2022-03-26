# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_dac_USA_xml
#'
#' Construct XML data structure for \code{dac_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac_USA.xml}. The corresponding file in the
#' original data system was \code{batch_dac_USA_xml.R} (gcamusa XML).
#' @author JF March 2021
module_gcamusa_batch_dac_USA_xml <- function(command, ...) {

  TECH_PARAMETRIZATION_INPUTS <- paste0("ssp", 1:5)
  if(command == driver.DECLARE_INPUTS) {
    return(c("L262.DeleteSupplysector_USAdac",
             "L262.Supplysector_dac_USA",
             "L262.FinalEnergyKeyword_dac_USA",
             "L262.SubsectorLogit_dac_USA",
             "L262.SubsectorShrwtFllt_dac_USA",
             "L262.SubsectorInterp_dac_USA",
             "L262.StubTech_dac_USA",
             "L262.PerCapitaBased_dac_USA",
             "L262.PriceElasticity_dac_USA",
             "L262.DeleteFinalDemand_USAdac",
             "L262.StubTechProd_dac_USA",
             "L262.StubTechCoef_dac_USA_ssp1",
             "L262.StubTechCoef_dac_USA_ssp2",
             "L262.StubTechCoef_dac_USA_ssp3",
             "L262.StubTechCoef_dac_USA_ssp4",
             "L262.StubTechCoef_dac_USA_ssp5",
             "L262.BaseService_dac_USA",
             "L262.CarbonCoef_dac_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_USA_ssp1.xml",
             XML = "dac_USA_ssp2.xml",
             XML = "dac_USA_ssp3.xml",
             XML = "dac_USA_ssp4.xml",
             XML = "dac_USA_ssp5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L262.DeleteSupplysector_USAdac <- get_data(all_data, "L262.DeleteSupplysector_USAdac")
    L262.FinalEnergyKeyword_dac_USA <- get_data(all_data, "L262.FinalEnergyKeyword_dac_USA")
    L262.SubsectorLogit_dac_USA <- get_data(all_data, "L262.SubsectorLogit_dac_USA")
    L262.SubsectorShrwtFllt_dac_USA <- get_data(all_data, "L262.SubsectorShrwtFllt_dac_USA")
    L262.SubsectorInterp_dac_USA <- get_data(all_data, "L262.SubsectorInterp_dac_USA")
    L262.StubTech_dac_USA <- get_data(all_data, "L262.StubTech_dac_USA")
    L262.PerCapitaBased_dac_USA <- get_data(all_data, "L262.PerCapitaBased_dac_USA")
    L262.PriceElasticity_dac_USA <- get_data(all_data, "L262.PriceElasticity_dac_USA")
    L262.DeleteFinalDemand_USAdac <- get_data(all_data, "L262.DeleteFinalDemand_USAdac")
    L262.Supplysector_dac_USA <- get_data(all_data, "L262.Supplysector_dac_USA")
    L262.StubTechProd_dac_USA <- get_data(all_data, "L262.StubTechProd_dac_USA")
    L262.BaseService_dac_USA <- get_data(all_data, "L262.BaseService_dac_USA")
    L262.CarbonCoef_dac_USA <- get_data(all_data, "L262.CarbonCoef_dac_USA")


    for(sce in TECH_PARAMETRIZATION_INPUTS){

      coef_name <- paste0("L262.StubTechCoef_dac_USA_",tolower(sce))
      L262.StubTechCoef_dac_USA <- get_data(all_data, coef_name)

      # ===================================================

      # Produce outputs
      xmlfn <- paste0("dac_USA_",tolower(sce), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(L262.DeleteSupplysector_USAdac, "DeleteSupplysector") %>%
        add_xml_data(L262.DeleteFinalDemand_USAdac, "DeleteFinalDemand") %>%
        add_logit_tables_xml(L262.Supplysector_dac_USA, "Supplysector") %>%
        add_xml_data(L262.FinalEnergyKeyword_dac_USA, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L262.SubsectorLogit_dac_USA, "SubsectorLogit") %>%
        add_xml_data(L262.SubsectorShrwtFllt_dac_USA, "SubsectorShrwtFllt") %>%
        add_xml_data(L262.SubsectorInterp_dac_USA, "SubsectorInterp") %>%
        add_xml_data(L262.StubTech_dac_USA, "StubTech") %>%
        add_xml_data(L262.PerCapitaBased_dac_USA, "PerCapitaBased") %>%
        add_xml_data(L262.PriceElasticity_dac_USA, "PriceElasticity") %>%
        add_xml_data(L262.StubTechProd_dac_USA, "StubTechProd") %>%
        add_xml_data(L262.StubTechCoef_dac_USA, "StubTechCoef") %>%
        add_xml_data(L262.BaseService_dac_USA, "BaseService") %>%
        add_xml_data(L262.CarbonCoef_dac_USA, "CarbonCoef") %>%
        add_precursors("L262.DeleteSupplysector_USAdac",
                       "L262.Supplysector_dac_USA",
                       "L262.FinalEnergyKeyword_dac_USA",
                       "L262.SubsectorLogit_dac_USA",
                       "L262.SubsectorShrwtFllt_dac_USA",
                       "L262.SubsectorInterp_dac_USA",
                       "L262.StubTech_dac_USA",
                       "L262.PerCapitaBased_dac_USA",
                       "L262.PriceElasticity_dac_USA",
                       "L262.DeleteFinalDemand_USAdac",
                       "L262.StubTechProd_dac_USA",
                       paste0("L262.StubTechCoef_dac_USA_",tolower(sce)),
                       "L262.BaseService_dac_USA",
                       "L262.CarbonCoef_dac_USA") ->
        xmlobj
      assign(xmlfn, xmlobj)
    }

    return_data(dac_USA_ssp1.xml,
                dac_USA_ssp2.xml,
                dac_USA_ssp3.xml,
                dac_USA_ssp4.xml,
                dac_USA_ssp5.xml)
  } else {
    stop("Unknown command")
  }
}
