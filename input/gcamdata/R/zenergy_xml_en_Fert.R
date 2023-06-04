# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_en_Fert_xml
#'
#' Construct XML data structure for \code{en_Fert.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_Fert.xml}. The corresponding file in the
#' original data system was \code{batch_en_Fert_xml.R} (energy XML).
module_energy_en_Fert_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L2322.GlobalTechShrwt_Fert",
             "L2322.GlobalTechCoef_Fert",
             "L2322.GlobalTechCost_Fert",
             "L2322.GlobalTechCapture_Fert",
             "L2322.GlobalTechSCurve_Fert",
             "L2322.GlobalTechProfitShutdown_Fert",
             "L2322.StubTechProd_Fert",
             "L2322.StubTechCoef_Fert",
             "L2322.StubTechFixOut_Fert_imp",
             "L2322.StubTechFixOut_Fert_exp",
             "L2322.PerCapitaBased_Fert",
             "L2322.BaseService_Fert"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_Fert.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert")
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert")
    L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert")
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert")
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert")
    L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert")
    L2322.GlobalTechShrwt_Fert <- get_data(all_data, "L2322.GlobalTechShrwt_Fert")

    L2322.GlobalTechCoef_Fert <- get_data(all_data, "L2322.GlobalTechCoef_Fert")
    L2322.GlobalTechCost_Fert <- get_data(all_data, "L2322.GlobalTechCost_Fert")
    L2322.GlobalTechCapture_Fert <- get_data(all_data, "L2322.GlobalTechCapture_Fert")
    L2322.GlobalTechSCurve_Fert <- get_data(all_data, "L2322.GlobalTechSCurve_Fert")
    L2322.GlobalTechProfitShutdown_Fert <- get_data(all_data, "L2322.GlobalTechProfitShutdown_Fert")
    L2322.StubTechProd_Fert <- get_data(all_data, "L2322.StubTechProd_Fert")
    L2322.StubTechCoef_Fert <- get_data(all_data, "L2322.StubTechCoef_Fert")
    L2322.StubTechFixOut_Fert_imp <- get_data(all_data, "L2322.StubTechFixOut_Fert_imp")
    L2322.StubTechFixOut_Fert_exp <- get_data(all_data, "L2322.StubTechFixOut_Fert_exp")
    L2322.PerCapitaBased_Fert <- get_data(all_data, "L2322.PerCapitaBased_Fert")
    L2322.BaseService_Fert <- get_data(all_data, "L2322.BaseService_Fert")

    # ===================================================

    # Produce outputs
    create_xml("en_Fert.xml") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert, "Supplysector") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert, "StubTech") %>%
      add_xml_data(L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt") %>%
      add_xml_data(L2322.GlobalTechCoef_Fert, "GlobalTechCoef") %>%
      add_xml_data(L2322.GlobalTechCost_Fert, "GlobalTechCost") %>%
      add_xml_data(L2322.GlobalTechCapture_Fert, "GlobalTechCapture") %>%
      add_xml_data(L2322.GlobalTechSCurve_Fert, "GlobalTechSCurve") %>%
      add_xml_data(L2322.GlobalTechProfitShutdown_Fert, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2322.StubTechProd_Fert, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_imp, "StubTechFixOut") %>%
      add_xml_data(L2322.StubTechFixOut_Fert_exp, "StubTechFixOut") %>%
      add_xml_data(L2322.PerCapitaBased_Fert, "PerCapitaBased") %>%
      add_xml_data(L2322.BaseService_Fert, "BaseService") %>%
      add_precursors("L2322.Supplysector_Fert",
                     "L2322.FinalEnergyKeyword_Fert",
                     "L2322.SubsectorLogit_Fert",
                     "L2322.SubsectorShrwtFllt_Fert",
                     "L2322.SubsectorInterp_Fert",
                     "L2322.StubTech_Fert",
                     "L2322.GlobalTechShrwt_Fert",
                     "L2322.GlobalTechCoef_Fert",
                     "L2322.GlobalTechCost_Fert",
                     "L2322.GlobalTechCapture_Fert",
                     "L2322.GlobalTechSCurve_Fert",
                     "L2322.GlobalTechProfitShutdown_Fert",
                     "L2322.StubTechProd_Fert",
                     "L2322.StubTechCoef_Fert",
                     "L2322.StubTechFixOut_Fert_imp",
                     "L2322.StubTechFixOut_Fert_exp",
                     "L2322.PerCapitaBased_Fert",
                     "L2322.BaseService_Fert") ->
      en_Fert.xml

    return_data(en_Fert.xml)
  } else {
    stop("Unknown command")
  }
}
