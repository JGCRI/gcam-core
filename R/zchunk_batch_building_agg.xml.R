#' module_energy_batch_building_agg.xml
#'
#' Construct XML data structure for \code{building_agg.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_agg.xml}. The corresponding file in the
#' original data system was \code{batch_building_agg.xml.R} (energy XML).
module_energy_batch_building_agg.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L242.FinalEnergyKeyword_bld",
              "L242.SubsectorLogit_bld",
              "L242.SubsectorShrwt_bld",
              "L242.SubsectorInterp_bld",
              "L242.SubsectorInterpTo_bld",
              "L242.StubTech_bld",
              "L242.GlobalTechInterp_bld",
              "L242.GlobalTechShrwt_bld",
              "L242.GlobalTechEff_bld",
              "L242.GlobalTechCost_bld",
              "L242.StubTechCalInput_bld",
              "L242.FuelPrefElast_bld",
              "L242.PerCapitaBased_bld",
              "L242.PriceElasticity_bld",
              "L242.BaseService_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_agg.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.FinalEnergyKeyword_bld <- get_data(all_data, "L242.FinalEnergyKeyword_bld")
    L242.SubsectorLogit_bld <- get_data(all_data, "L242.SubsectorLogit_bld")
    L242.SubsectorShrwt_bld <- get_data(all_data, "L242.SubsectorShrwt_bld")
    L242.SubsectorInterp_bld <- get_data(all_data, "L242.SubsectorInterp_bld")
    L242.SubsectorInterpTo_bld <- get_data(all_data, "L242.SubsectorInterpTo_bld")
    L242.StubTech_bld <- get_data(all_data, "L242.StubTech_bld")
    L242.GlobalTechInterp_bld <- get_data(all_data, "L242.GlobalTechInterp_bld")
    L242.GlobalTechShrwt_bld <- get_data(all_data, "L242.GlobalTechShrwt_bld")
    L242.GlobalTechEff_bld <- get_data(all_data, "L242.GlobalTechEff_bld")
    L242.GlobalTechCost_bld <- get_data(all_data, "L242.GlobalTechCost_bld")
    L242.StubTechCalInput_bld <- get_data(all_data, "L242.StubTechCalInput_bld")
    L242.FuelPrefElast_bld <- get_data(all_data, "L242.FuelPrefElast_bld")
    L242.PerCapitaBased_bld <- get_data(all_data, "L242.PerCapitaBased_bld")
    L242.PriceElasticity_bld <- get_data(all_data, "L242.PriceElasticity_bld")
    L242.BaseService_bld <- get_data(all_data, "L242.BaseService_bld")

    # ===================================================

    # Produce outputs
    create_xml("building_agg.xml") %>%
      add_xml_data(L242.FinalEnergyKeyword_bld,"FinalEnergyKeyword") %>%
      add_xml_data(L242.SubsectorLogit_bld,"SubsectorLogit") %>%
      add_xml_data(L242.SubsectorShrwt_bld,"SubsectorShrwt") %>%
      add_xml_data(L242.SubsectorInterp_bld,"SubsectorInterp") %>%
      add_xml_data(L242.SubsectorInterpTo_bld,"SubsectorInterpTo") %>%
      add_xml_data(L242.StubTech_bld,"StubTech") %>%
      add_xml_data(L242.GlobalTechInterp_bld,"GlobalTechInterp") %>%
      add_xml_data(L242.GlobalTechShrwt_bld,"GlobalTechShrwt") %>%
      add_xml_data(L242.GlobalTechEff_bld,"GlobalTechEff") %>%
      add_xml_data(L242.GlobalTechCost_bld,"GlobalTechCost") %>%
      add_xml_data(L242.StubTechCalInput_bld,"StubTechCalInput") %>%
      add_xml_data(L242.FuelPrefElast_bld,"FuelPrefElast") %>%
      add_xml_data(L242.PerCapitaBased_bld,"PerCapitaBased") %>%
      add_xml_data(L242.PriceElasticity_bld,"PriceElasticity") %>%
      add_xml_data(L242.BaseService_bld,"BaseService") %>%
      add_precursors("L242.FinalEnergyKeyword_bld", "L242.SubsectorLogit_bld", "L242.SubsectorShrwt_bld", "L242.SubsectorInterp_bld", "L242.SubsectorInterpTo_bld", "L242.StubTech_bld", "L242.GlobalTechInterp_bld", "L242.GlobalTechShrwt_bld", "L242.GlobalTechEff_bld", "L242.GlobalTechCost_bld", "L242.StubTechCalInput_bld", "L242.FuelPrefElast_bld", "L242.PerCapitaBased_bld", "L242.PriceElasticity_bld", "L242.BaseService_bld") ->
      building_agg.xml

    return_data(building_agg.xml)
  } else {
    stop("Unknown command")
  }
}
