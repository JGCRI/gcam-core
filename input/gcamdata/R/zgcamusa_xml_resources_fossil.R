# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_resources_fossil_USA_xml
#'
#' Construct XML data structure for \code{resources_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_fossil_USA.xml}. The corresponding file in the
#' original data system was \code{batch_resources_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_resources_fossil_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L211.PrimaryCO2Coef_USA",
             "L211.Rsrc_F_USA",
             "L211.RsrcPrice_F_USA",
             "L211.RsrcCalProd_USA",
             "L211.RsrcTechChange_USA",
             "L211.RsrcCurves_fos_USA",
             "L211.ResSubresourceProdLifetime_USA",
             "L211.SubresourcePriceAdder_USA",
             "L211.ReserveCalReserve_USA",
             "L211.ResReserveTechLifetime_USA",
             "L211.ResReserveTechDeclinePhase_USA",
             "L211.ResReserveTechProfitShutdown_USA",
             "L211.ResReserveTechInvestmentInput_USA",
             "L211.ResReserveTechShrwt_fossil_USA",
             "L211.Sector_prod_USA",
             "L211.Subsector_prod_USA",
             "L211.SubsShrwtFlt_USA",
             "L211.SubsInterpRule_USA",
             "L211.TechShrwt_USA",
             "L211.TechCoef_USA",
             "L211.TechCal_USA",
             "L211.TFFSubsectorLogit",
             "L211.TFFTechProduction_USA",
             "L211.TFFTechCoef_USA",
             "L211.TFFTechShrwtInterp_USA",
             "L211.ResTechCoef_USA",
             "L211.DeleteRsrc_fos_USA"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_fossil_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L211.PrimaryCO2Coef_USA <- get_data(all_data, "L211.PrimaryCO2Coef_USA")
    L211.Rsrc_F_USA <- get_data(all_data, "L211.Rsrc_F_USA")
    L211.RsrcPrice_F_USA <- get_data(all_data, "L211.RsrcPrice_F_USA")
    L211.RsrcCalProd_USA <- get_data(all_data, "L211.RsrcCalProd_USA")
    L211.RsrcTechChange_USA <- get_data(all_data, "L211.RsrcTechChange_USA")
    L211.RsrcCurves_fos_USA <- get_data(all_data, "L211.RsrcCurves_fos_USA")

    L211.ResSubresourceProdLifetime_USA <- get_data(all_data, "L211.ResSubresourceProdLifetime_USA")
    L211.SubresourcePriceAdder_USA <- get_data(all_data, "L211.SubresourcePriceAdder_USA")
    L211.ReserveCalReserve_USA <- get_data(all_data, "L211.ReserveCalReserve_USA")
    L211.ResReserveTechLifetime_USA <- get_data(all_data, "L211.ResReserveTechLifetime_USA")
    L211.ResReserveTechDeclinePhase_USA <- get_data(all_data, "L211.ResReserveTechDeclinePhase_USA")
    L211.ResReserveTechProfitShutdown_USA <- get_data(all_data, "L211.ResReserveTechProfitShutdown_USA")
    L211.ResReserveTechInvestmentInput_USA <- get_data(all_data, "L211.ResReserveTechInvestmentInput_USA")
    L211.ResReserveTechShrwt_fossil_USA <- get_data(all_data, "L211.ResReserveTechShrwt_fossil_USA")

    L211.Sector_prod_USA <- get_data(all_data, "L211.Sector_prod_USA")
    L211.Subsector_prod_USA <- get_data(all_data, "L211.Subsector_prod_USA")
    L211.SubsShrwtFlt_USA <- get_data(all_data, "L211.SubsShrwtFlt_USA")
    L211.SubsInterpRule_USA <- get_data(all_data, "L211.SubsInterpRule_USA")
    L211.TechShrwt_USA <- get_data(all_data, "L211.TechShrwt_USA")
    L211.TechCoef_USA <- get_data(all_data, "L211.TechCoef_USA")
    L211.TechCal_USA <- get_data(all_data, "L211.TechCal_USA")
    L211.TFFSubsectorLogit <- get_data(all_data, "L211.TFFSubsectorLogit")
    L211.TFFTechProduction_USA <- get_data(all_data, "L211.TFFTechProduction_USA")
    L211.TFFTechCoef_USA <- get_data(all_data, "L211.TFFTechCoef_USA")
    L211.TFFTechShrwtInterp_USA <- get_data(all_data, "L211.TFFTechShrwtInterp_USA")
    L211.ResTechCoef_USA <- get_data(all_data, "L211.ResTechCoef_USA")

    L211.DeleteRsrc_fos_USA <- get_data(all_data, "L211.DeleteRsrc_fos_USA")

    # ===================================================

    L211.RsrcCurves_fos_USA <- L211.RsrcCurves_fos_USA %>%
      rename(subresource = reserve.subresource)

    L211.RsrcCalProd_USA <- L211.RsrcCalProd_USA %>%
      rename(subresource = reserve.subresource)

    L211.RsrcTechChange_USA <- L211.RsrcTechChange_USA %>%
      rename(subresource = reserve.subresource)

    L211.SubresourcePriceAdder_USA <- L211.SubresourcePriceAdder_USA %>%
      rename(subresource = reserve.subresource)

    L211.ResReserveTechShrwt_fossil_USA <- L211.ResReserveTechShrwt_fossil_USA %>%
      rename(subresource = reserve.subresource,
             technology = resource.reserve.technology)

    L211.SectorUseTrialMarket_tra <- L211.Sector_prod_USA %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) %>%
      mutate(use.trial.market = 1)

    # Produce outputs
    create_xml("resources_fossil_USA.xml") %>%
      add_xml_data(L211.DeleteRsrc_fos_USA, "DeleteRsrc") %>%
      # resource curve
      add_xml_data(L211.PrimaryCO2Coef_USA, "CarbonCoef") %>%
      add_xml_data(L211.Rsrc_F_USA, "Rsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      # add resource.reserve.technology details
      add_xml_data(L211.ResSubresourceProdLifetime_USA, "ResSubresourceProdLifetime") %>%
      add_xml_data(L211.SubresourcePriceAdder_USA, "SubresourcePriceAdder") %>%
      add_xml_data(L211.ReserveCalReserve_USA, "ReserveCalReserve") %>%
      add_xml_data(L211.ResReserveTechLifetime_USA, "ResReserveTechLifetime") %>%
      add_xml_data(L211.ResReserveTechInvestmentInput_USA, "ResReserveTechInvestmentInput") %>%
      add_xml_data(L211.ResReserveTechDeclinePhase_USA, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L211.ResReserveTechProfitShutdown_USA, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L211.ResReserveTechShrwt_fossil_USA, "ResTechShrwt") %>%
      add_xml_data(L211.RsrcPrice_F_USA, "RsrcPrice") %>%
      add_xml_data(L211.RsrcTechChange_USA, "RsrcTechChange") %>%
      add_xml_data(L211.RsrcCalProd_USA, "RsrcCalProd") %>%
      add_xml_data(L211.RsrcCurves_fos_USA, "RsrcCurves") %>%
      # [fossil energy] production sector
      add_logit_tables_xml(L211.Sector_prod_USA, "Supplysector") %>%
      add_xml_data(L211.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L211.Subsector_prod_USA, "SubsectorLogit") %>%
      add_xml_data(L211.SubsShrwtFlt_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L211.SubsInterpRule_USA, "SubsectorInterp") %>%
      add_xml_data(L211.TechShrwt_USA, "TechShrwt") %>%
      add_xml_data(L211.TechCoef_USA, "TechCoef") %>%
      add_xml_data(L211.TechCal_USA, "StubTechCalInput") %>%
      add_logit_tables_xml(L211.TFFSubsectorLogit, "SubsectorLogit") %>%
      add_xml_data(L211.TFFTechProduction_USA, "Production") %>%
      add_xml_data(L211.TFFTechCoef_USA, "TechCoef") %>%
      add_xml_data(L211.TFFTechShrwtInterp_USA,"TechInterp") %>%
      # inputs to unconventional heavy oil
      add_xml_data(L211.ResTechCoef_USA, "ResReserveTechCoefMkt") %>%
      add_precursors("L211.PrimaryCO2Coef_USA",
                     "L211.Rsrc_F_USA",
                     "L211.RsrcPrice_F_USA",
                     "L211.RsrcCalProd_USA",
                     "L211.RsrcTechChange_USA",
                     "L211.RsrcCurves_fos_USA",
                     "L211.ResSubresourceProdLifetime_USA",
                     "L211.SubresourcePriceAdder_USA",
                     "L211.ReserveCalReserve_USA",
                     "L211.ResReserveTechLifetime_USA",
                     "L211.ResReserveTechDeclinePhase_USA",
                     "L211.ResReserveTechProfitShutdown_USA",
                     "L211.ResReserveTechShrwt_fossil_USA",
                     "L211.ResReserveTechInvestmentInput_USA",
                     "L211.Sector_prod_USA",
                     "L211.Subsector_prod_USA",
                     "L211.SubsShrwtFlt_USA",
                     "L211.SubsInterpRule_USA",
                     "L211.TechShrwt_USA",
                     "L211.TechCoef_USA",
                     "L211.TechCal_USA",
                     "L211.TFFSubsectorLogit",
                     "L211.TFFTechProduction_USA",
                     "L211.TFFTechCoef_USA",
                     "L211.TFFTechShrwtInterp_USA",
                     "L211.ResTechCoef_USA",
                     "L211.DeleteRsrc_fos_USA"
                     ) ->
      resources_fossil_USA.xml

    return_data(resources_fossil_USA.xml)
  } else {
    stop("Unknown command")
  }
}
