#' module_energy_batch_transportation_agg_xml
#'
#' Construct XML data structure for \code{transportation_agg.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_agg.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_agg.xml.R} (energy XML).
module_energy_batch_transportation_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L252.Supplysector_trn",
              "L252.FinalEnergyKeyword_trn",
              "L252.SubsectorLogit_trn",
              "L252.SubsectorShrwt_trn",
              "L252.SubsectorShrwtFllt_trn",
              "L252.SubsectorInterp_trn",
              "L252.SubsectorInterpTo_trn",
              "L252.StubTech_trn",
              "L252.GlobalTechShrwt_trn",
              "L252.GlobalTechEff_trn",
              "L252.GlobalTechCost_trn",
              "L252.StubTechCalInput_trn",
              "L252.PerCapitaBased_trn",
              "L252.PriceElasticity_trn",
              "L252.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_agg.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L252.Supplysector_trn <- get_data(all_data, "L252.Supplysector_trn")
    L252.FinalEnergyKeyword_trn <- get_data(all_data, "L252.FinalEnergyKeyword_trn")
    L252.SubsectorLogit_trn <- get_data(all_data, "L252.SubsectorLogit_trn")
    L252.SubsectorShrwt_trn <- get_data(all_data, "L252.SubsectorShrwt_trn")
    L252.SubsectorShrwtFllt_trn <- get_data(all_data, "L252.SubsectorShrwtFllt_trn")
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
    create_xml("transportation_agg.xml") %>%
      add_logit_tables_xml(L252.Supplysector_trn,"Supplysector") %>%
      add_xml_data(L252.FinalEnergyKeyword_trn,"FinalEnergyKeyword") %>%
      add_logit_tables_xml(L252.SubsectorLogit_trn,"SubsectorLogit") -> transportation_agg.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L252.SubsectorShrwt_trn)) {
      transportation_agg.xml %>%
        add_xml_data(L252.SubsectorShrwt_trn, "SubsectorShrwt") ->
        transportation_agg.xml
    }

    if(!is.null(L252.SubsectorShrwtFllt_trn)) {
      transportation_agg.xml %>%
        add_xml_data(L252.SubsectorShrwtFllt_trn, "SubsectorShrwtFllt") ->
        transportation_agg.xml
    }

    if(!is.null(L252.SubsectorInterp_trn)) {
      transportation_agg.xml %>%
        add_xml_data(L252.SubsectorInterp_trn, "SubsectorInterp") ->
        transportation_agg.xml
    }

    if(!is.null(L252.SubsectorInterpTo_trn)) {
      transportation_agg.xml %>%
        add_xml_data(L252.SubsectorInterpTo_trn, "SubsectorInterpTo") ->
        transportation_agg.xml
    }
    transportation_agg.xml <- transportation_agg.xml %>%
      add_xml_data(L252.StubTech_trn,"StubTech") %>%
      add_xml_data(L252.GlobalTechShrwt_trn,"GlobalTechShrwt") %>%
      add_xml_data(L252.GlobalTechEff_trn,"GlobalTechEff") %>%
      add_xml_data(L252.GlobalTechCost_trn,"GlobalTechCost") %>%
      add_xml_data(L252.StubTechCalInput_trn,"StubTechCalInput") %>%
      add_xml_data(L252.PerCapitaBased_trn,"PerCapitaBased") %>%
      add_xml_data(L252.PriceElasticity_trn,"PriceElasticity") %>%
      add_xml_data(L252.BaseService_trn,"BaseService") %>%
      add_precursors("L252.Supplysector_trn", "L252.FinalEnergyKeyword_trn", "L252.SubsectorLogit_trn",
                     "L252.SubsectorShrwt_trn", "L252.SubsectorShrwtFllt_trn", "L252.SubsectorInterp_trn",
                     "L252.SubsectorInterpTo_trn", "L252.StubTech_trn", "L252.GlobalTechShrwt_trn",
                     "L252.GlobalTechEff_trn", "L252.GlobalTechCost_trn", "L252.StubTechCalInput_trn",
                     "L252.PerCapitaBased_trn", "L252.PriceElasticity_trn", "L252.BaseService_trn") ->
      transportation_agg.xml



    return_data(transportation_agg.xml)
  } else {
    stop("Unknown command")
  }
}
