# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_regional_biomass_xml
#'
#' Construct XML data structure for \code{regional_biomass_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{regional_biomass_USA.xml}.
#' The corresponding file in the original data system was \code{batch_regional_biomass_USA.xml} (gcamusa XML batch).
module_gcamusa_regional_biomass_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2261.DeleteSupplysector_bio_USA",
             "L2261.Supplysector_bio_USA",
             "L2261.SubsectorShrwtFllt_bio_USA",
             "L2261.SubsectorInterp_bio_USA",
             "L2261.SubsectorLogit_bio_USA",
             "L2261.StubTech_bio_USA",
             "L2261.StubTechMarket_bio_USA",
             "L2261.StubTechCoef_bioOil_USA",
             "L2261.StubTechShrwt_rbO_USA",
             "L2261.StubTechFractSecOut_bio_USA",
             "L2261.StubTechFractProd_bio_USA",
             "L2261.StubTechFractCalPrice_bio_USA",
             "L2261.StubTechCalInput_bio_USA",
             "L2261.StubTechInterp_bio_USA",
             "L2261.Rsrc_DDGS_USA",
             "L2261.RsrcPrice_DDGS_USA",
             "L2261.Tech_rbm_USA",
             "L2261.TechShrwt_rbm_USA",
             "L2261.TechCoef_rbm_USA",
             "L2261.Tech_dbm_USA",
             "L2261.TechShrwt_dbm_USA",
             "L2261.TechEff_dbm_USA",
             "L2261.TechCost_dbm_USA",
             "L2261.CarbonCoef_bio_USA",
             "L2261.StubTechMarket_en_USA",
             "L2261.StubTechMarket_elecS_USA",
             "L2261.StubTechMarket_ind_USA",
             "L2261.StubTechMarket_cement_USA",
             "L2261.StubTechMarket_bld_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "regional_biomass_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2261.DeleteSupplysector_bio_USA <- get_data(all_data, "L2261.DeleteSupplysector_bio_USA")
    L2261.Supplysector_bio_USA <- get_data(all_data, "L2261.Supplysector_bio_USA")
    L2261.SubsectorShrwtFllt_bio_USA <- get_data(all_data, "L2261.SubsectorShrwtFllt_bio_USA")
    L2261.SubsectorInterp_bio_USA <- get_data(all_data, "L2261.SubsectorInterp_bio_USA")
    L2261.SubsectorLogit_bio_USA <- get_data(all_data, "L2261.SubsectorLogit_bio_USA")
    L2261.StubTech_bio_USA <- get_data(all_data, "L2261.StubTech_bio_USA")
    L2261.StubTechMarket_bio_USA <- get_data(all_data, "L2261.StubTechMarket_bio_USA")
    L2261.StubTechCoef_bioOil_USA   <- get_data(all_data,"L2261.StubTechCoef_bioOil_USA")
    L2261.StubTechShrwt_rbO_USA <- get_data(all_data, "L2261.StubTechShrwt_rbO_USA")
    L2261.StubTechFractSecOut_bio_USA <- get_data(all_data, "L2261.StubTechFractSecOut_bio_USA")
    L2261.StubTechFractProd_bio_USA <- get_data(all_data, "L2261.StubTechFractProd_bio_USA")
    L2261.StubTechFractCalPrice_bio_USA <- get_data(all_data, "L2261.StubTechFractCalPrice_bio_USA")
    L2261.StubTechCalInput_bio_USA <- get_data(all_data, "L2261.StubTechCalInput_bio_USA")
    L2261.StubTechInterp_bio_USA <- get_data(all_data, "L2261.StubTechInterp_bio_USA")
    L2261.Rsrc_DDGS_USA <- get_data(all_data, "L2261.Rsrc_DDGS_USA")
    L2261.RsrcPrice_DDGS_USA <- get_data(all_data, "L2261.RsrcPrice_DDGS_USA")
    L2261.Tech_rbm_USA <- get_data(all_data, "L2261.Tech_rbm_USA")
    L2261.TechShrwt_rbm_USA <- get_data(all_data, "L2261.TechShrwt_rbm_USA")
    L2261.TechCoef_rbm_USA <- get_data(all_data, "L2261.TechCoef_rbm_USA")
    L2261.Tech_dbm_USA <- get_data(all_data, "L2261.Tech_dbm_USA")
    L2261.TechShrwt_dbm_USA <- get_data(all_data, "L2261.TechShrwt_dbm_USA")
    L2261.TechEff_dbm_USA <- get_data(all_data, "L2261.TechEff_dbm_USA")
    L2261.TechCost_dbm_USA <- get_data(all_data, "L2261.TechCost_dbm_USA")
    L2261.CarbonCoef_bio_USA <- get_data(all_data, "L2261.CarbonCoef_bio_USA")
    L2261.StubTechMarket_en_USA <- get_data(all_data, "L2261.StubTechMarket_en_USA")
    L2261.StubTechMarket_elecS_USA <- get_data(all_data, "L2261.StubTechMarket_elecS_USA")
    L2261.StubTechMarket_ind_USA <- get_data(all_data, "L2261.StubTechMarket_ind_USA")
    L2261.StubTechMarket_cement_USA <- get_data(all_data, "L2261.StubTechMarket_cement_USA")
    L2261.StubTechMarket_bld_USA <- get_data(all_data, "L2261.StubTechMarket_bld_USA")

    # ===================================================

    # Produce outputs
    create_xml("regional_biomass_USA.xml") %>%
      add_xml_data(L2261.DeleteSupplysector_bio_USA, "DeleteSupplysector") %>%
      add_node_equiv_xml("sector") %>%
      add_logit_tables_xml(L2261.Supplysector_bio_USA, "Supplysector") %>%
      add_xml_data(L2261.SubsectorShrwtFllt_bio_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2261.SubsectorInterp_bio_USA, "SubsectorInterp") %>%
      add_logit_tables_xml(L2261.SubsectorLogit_bio_USA, "SubsectorLogit") %>%
      add_xml_data(L2261.StubTech_bio_USA, "StubTech") %>%
      add_xml_data(L2261.StubTechMarket_bio_USA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechCoef_bioOil_USA,"StubTechCoef") %>%
      add_xml_data(L2261.StubTechShrwt_rbO_USA, "StubTechShrwt") %>%
      add_xml_data(L2261.StubTechFractSecOut_bio_USA, "StubTechFractSecOut") %>%
      add_xml_data(L2261.StubTechFractProd_bio_USA, "StubTechFractProd") %>%
      add_xml_data(L2261.StubTechFractCalPrice_bio_USA, "StubTechFractCalPrice") %>%
      add_xml_data(L2261.StubTechCalInput_bio_USA, "StubTechCalInput") %>%
      add_xml_data(L2261.StubTechInterp_bio_USA, "StubTechInterp") %>%
      add_xml_data(L2261.Rsrc_DDGS_USA, "Rsrc") %>%
      add_xml_data(L2261.RsrcPrice_DDGS_USA, "RsrcPrice") %>%
      add_xml_data(L2261.Tech_rbm_USA, "Tech") %>%
      add_xml_data(L2261.TechShrwt_rbm_USA, "TechShrwt") %>%
      add_xml_data(L2261.TechCoef_rbm_USA, "TechCoef") %>%
      add_xml_data(L2261.Tech_dbm_USA, "Tech") %>%
      add_xml_data(L2261.TechShrwt_dbm_USA, "TechShrwt") %>%
      add_xml_data(L2261.TechEff_dbm_USA, "TechEff") %>%
      add_xml_data(L2261.TechCost_dbm_USA, "TechCost") %>%
      add_xml_data(L2261.CarbonCoef_bio_USA, "CarbonCoef") %>%
      add_xml_data(L2261.StubTechMarket_en_USA, "StubTechMarket") %>%
      add_xml_data_generate_levels(L2261.StubTechMarket_elecS_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2261.StubTechMarket_ind_USA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechMarket_cement_USA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechMarket_bld_USA, "StubTechMarket") %>%
      add_precursors("L2261.DeleteSupplysector_bio_USA",
                     "L2261.Supplysector_bio_USA",
                     "L2261.SubsectorShrwtFllt_bio_USA",
                     "L2261.SubsectorInterp_bio_USA",
                     "L2261.SubsectorLogit_bio_USA",
                     "L2261.StubTech_bio_USA",
                     "L2261.StubTechMarket_bio_USA",
                     "L2261.StubTechCoef_bioOil_USA",
                     "L2261.StubTechShrwt_rbO_USA",
                     "L2261.StubTechFractSecOut_bio_USA",
                     "L2261.StubTechFractProd_bio_USA",
                     "L2261.StubTechFractCalPrice_bio_USA",
                     "L2261.StubTechCalInput_bio_USA",
                     "L2261.StubTechInterp_bio_USA",
                     "L2261.Rsrc_DDGS_USA",
                     "L2261.RsrcPrice_DDGS_USA",
                     "L2261.Tech_rbm_USA",
                     "L2261.TechShrwt_rbm_USA",
                     "L2261.TechCoef_rbm_USA",
                     "L2261.Tech_dbm_USA",
                     "L2261.TechShrwt_dbm_USA",
                     "L2261.TechEff_dbm_USA",
                     "L2261.TechCost_dbm_USA",
                     "L2261.CarbonCoef_bio_USA",
                     "L2261.StubTechMarket_en_USA",
                     "L2261.StubTechMarket_elecS_USA",
                     "L2261.StubTechMarket_ind_USA",
                     "L2261.StubTechMarket_cement_USA",
                     "L2261.StubTechMarket_bld_USA") ->
      regional_biomass_USA.xml

    return_data(regional_biomass_USA.xml)
  } else {
    stop("Unknown command")
  }
}
