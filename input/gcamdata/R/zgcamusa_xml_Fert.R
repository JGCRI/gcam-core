# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_Fert_xml
#'
#' Construct XML data structure for \code{Fert_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Fert_USA.xml}. The corresponding file in the
#' original data system was \code{batch_Fert_USA_xml.R} (gcamusa XML).
module_gcamusa_Fert_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.DeleteSubsector_USAFert",
             "L2322.FinalEnergyKeyword_Fert_USA",
             "L2322.Supplysector_Fert_USA",
             "L2322.SubsectorLogit_Fert_USA",
             "L2322.SubsectorShrwtFllt_Fert_USA",
             "L2322.SubsectorInterp_Fert_USA",
             "L2322.StubTech_Fert_USA",
             "L2322.FinalEnergyKeyword_USAFert",
             "L2322.SubsectorLogit_USAFert",
             "L2322.SubsectorShrwtFllt_USAFert",
             "L2322.SubsectorInterp_USAFert",
             "L2322.TechShrwt_USAFert",
             "L2322.Production_USAFert",
             "L2322.TechCoef_USAFert",
             "L2322.StubTechProd_FertProd_USA",
             "L2322.StubTechCoef_Fert_USA",
             "L2322.StubTechMarket_Fert_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Fert_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.DeleteSubsector_USAFert <- get_data(all_data, "L2322.DeleteSubsector_USAFert")
    L2322.FinalEnergyKeyword_Fert_USA <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert_USA")
    L2322.Supplysector_Fert_USA <- get_data(all_data, "L2322.Supplysector_Fert_USA")
    L2322.SubsectorLogit_Fert_USA <- get_data(all_data, "L2322.SubsectorLogit_Fert_USA")
    L2322.SubsectorShrwtFllt_Fert_USA <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_USA")
    L2322.SubsectorInterp_Fert_USA <- get_data(all_data, "L2322.SubsectorInterp_Fert_USA")
    L2322.StubTech_Fert_USA <- get_data(all_data, "L2322.StubTech_Fert_USA")
    L2322.FinalEnergyKeyword_USAFert <- get_data(all_data, "L2322.FinalEnergyKeyword_USAFert")
    L2322.SubsectorLogit_USAFert <- get_data(all_data, "L2322.SubsectorLogit_USAFert")
    L2322.SubsectorShrwtFllt_USAFert <- get_data(all_data, "L2322.SubsectorShrwtFllt_USAFert")
    L2322.SubsectorInterp_USAFert <- get_data(all_data, "L2322.SubsectorInterp_USAFert")
    L2322.TechShrwt_USAFert <- get_data(all_data, "L2322.TechShrwt_USAFert")
    L2322.Production_USAFert <- get_data(all_data, "L2322.Production_USAFert")
    L2322.TechCoef_USAFert <- get_data(all_data, "L2322.TechCoef_USAFert")
    L2322.StubTechProd_FertProd_USA <- get_data(all_data, "L2322.StubTechProd_FertProd_USA")
    L2322.StubTechCoef_Fert_USA <- get_data(all_data, "L2322.StubTechCoef_Fert_USA")
    L2322.StubTechMarket_Fert_USA <- get_data(all_data, "L2322.StubTechMarket_Fert_USA")

    # ===================================================

    # Produce outputs
    create_xml("Fert_USA.xml") %>%
      add_xml_data(L2322.DeleteSubsector_USAFert, "DeleteSubsector") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert_USA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert_USA, "Supplysector") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert_USA, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert_USA, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert_USA, "StubTech") %>%
      add_xml_data(L2322.FinalEnergyKeyword_USAFert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_USAFert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_USAFert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_USAFert, "SubsectorInterp") %>%
      add_xml_data(L2322.TechShrwt_USAFert, "TechShrwt") %>%
      add_xml_data(L2322.Production_USAFert, "Production") %>%
      add_xml_data(L2322.TechCoef_USAFert, "TechCoef") %>%
      add_xml_data(L2322.StubTechProd_FertProd_USA, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert_USA, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechMarket_Fert_USA, "StubTechMarket") %>%
      add_precursors("L2322.DeleteSubsector_USAFert",
                     "L2322.FinalEnergyKeyword_Fert_USA",
                     "L2322.Supplysector_Fert_USA",
                     "L2322.SubsectorLogit_Fert_USA",
                     "L2322.SubsectorShrwtFllt_Fert_USA",
                     "L2322.SubsectorInterp_Fert_USA",
                     "L2322.StubTech_Fert_USA",
                     "L2322.FinalEnergyKeyword_USAFert",
                     "L2322.SubsectorLogit_USAFert",
                     "L2322.SubsectorShrwtFllt_USAFert",
                     "L2322.SubsectorInterp_USAFert",
                     "L2322.TechShrwt_USAFert",
                     "L2322.Production_USAFert",
                     "L2322.TechCoef_USAFert",
                     "L2322.StubTechProd_FertProd_USA",
                     "L2322.StubTechCoef_Fert_USA",
                     "L2322.StubTechMarket_Fert_USA") ->
      Fert_USA.xml

    return_data(Fert_USA.xml)
  } else {
    stop("Unknown command")
  }
}
