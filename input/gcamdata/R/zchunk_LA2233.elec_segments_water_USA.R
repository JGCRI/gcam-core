#' module_gcamusa_LA2233.elec_segments_water_USA
#'
#' Weighted water coefficient for reference scenario and load segment classification
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.StubTech_WaterCoef_ref}
#' The corresponding file in the
#' original data system was \code{LA2233.electricity_water_USA} (gcam-usa level2)
#' @details Weighted water coefficient for reference scenario and load segment classification
#' @author NTG October 2019
module_gcamusa_LA2233.elec_segments_water_USA <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire_new",
             FILE = "water/elec_tech_water_map",
             "L1233.out_EJ_state_elec_F_tech_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.GlobalTechShrwt_elec_cool",
             "L2233.GlobalTechProfitShutdown_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2234.AvgFossilEffKeyword_elecS_USA",
             "L2234.GlobalIntTechBackup_elecS_USA",
             "L2234.GlobalIntTechCapital_elecS_USA",
             "L2234.GlobalIntTechEff_elecS_USA",
             "L2234.GlobalIntTechLifetime_elecS_USA",
             "L2234.GlobalIntTechOMfixed_elecS_USA",
             "L2234.GlobalIntTechOMvar_elecS_USA",
             "L2234.GlobalIntTechShrwt_elecS_USA",
             "L2234.GlobalTechCapFac_elecS_USA",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalTechCapture_elecS_USA",
             "L2234.GlobalTechEff_elecS_USA",
             "L2234.GlobalTechLifetime_elecS_USA",
             "L2234.GlobalTechOMfixed_elecS_USA",
             "L2234.GlobalTechOMvar_elecS_USA",
             "L2234.GlobalTechProfitShutdown_elecS_USA",
             "L2234.GlobalTechSCurve_elecS_USA",
             "L2234.GlobalTechShrwt_elecS_USA",
             "L2234.PrimaryRenewKeyword_elecS_USA",
             "L2234.PrimaryRenewKeywordInt_elecS_USA",
             "L2234.StubTechCapFactor_elecS_solar_USA",
             "L2234.StubTechCapFactor_elecS_wind_USA",
             "L2234.StubTechEff_elecS_USA",
             "L2234.StubTechElecMarket_backup_elecS_USA",
             "L2234.StubTechFixOut_elecS_USA",
             "L2234.StubTechFixOut_hydro_elecS_USA",
             "L2234.StubTechMarket_backup_elecS_USA",
             "L2234.StubTechMarket_elecS_USA",
             "L2234.StubTechProd_elecS_USA",
             "L2234.SubsectorLogit_elecS_USA",
             "L2234.SubsectorShrwt_elecS_USA",
             "L2234.SubsectorShrwtInterp_elecS_USA",
             "L2234.SubsectorShrwtInterpTo_elecS_USA",
             "L2234.Supplysector_elecS_USA",
             "L2240.GlobalTechCapFac_elec_coalret_USA",
             "L2240.GlobalTechCapital_elec_coalret_USA",
             "L2240.GlobalTechEff_elec_coalret_USA",
             "L2240.GlobalTechOMfixed_elec_coalret_USA",
             "L2240.GlobalTechOMvar_elec_coalret_USA",
             "L2240.GlobalTechProfitShutdown_elec_coalret_USA",
             "L2240.GlobalTechShrwt_elec_coalret_USA",
             "L2240.StubTechEff_elec_coalret_USA",
             "L2240.StubTechMarket_elec_coalret_USA",
             "L2240.StubTechProd_elec_coalret_USA",
             "L2240.StubTechSCurve_elec_coalret_USA",
             "L2246.GlobalTechCapFac_coal_vintage_USA",
             "L2246.GlobalTechCapital_coal_vintage_USA",
             "L2246.GlobalTechEff_coal_vintage_USA",
             "L2246.GlobalTechOMfixed_coal_vintage_USA",
             "L2246.GlobalTechOMvar_coal_vintage_USA",
             "L2246.GlobalTechShrwt_coal_vintage_USA",
             "L2246.StubTechEff_coal_vintage_USA",
             "L2246.StubTechLifetime_coal_vintage_USA",
             "L2246.StubTechMarket_coal_vintage_USA",
             "L2246.StubTechProd_coal_vintage_USA",
             "L2246.StubTechProfitShutdown_coal_vintage_USA",
             "L2246.StubTechSCurve_coal_vintage_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.GlobalTechEff_elecS_cool_USA",
             "L2233.GlobalTechShrwt_elecS_cool_USA",
             "L2233.GlobalTechProfitShutdown_elecS_cool_USA",
             "L2233.GlobalTechOMvar_elecS_cool_USA",
             "L2233.GlobalTechOMfixed_elecS_cool_USA",
             "L2233.GlobalTechCapital_elecS_USA",
             "L2233.GlobalTechCapital_elecS_cool_USA",
             "L2233.GlobalTechCapFac_elecS_cool_USA",
             "L2233.GlobalTechSCurve_elecS_cool_USA",
             "L2233.GlobalTechCoef_elecS_cool_USA",
             "L2233.GlobalTechCapture_elecS_cool_USA",
             "L2233.GlobalTechLifetime_elecS_cool_USA",
             "L2233.AvgFossilEffKeyword_elecS_cool_USA",
             "L2233.GlobalIntTechBackup_elecS_cool_USA",
             "L2233.GlobalIntTechCapital_elecS_USA",
             "L2233.GlobalIntTechCapital_elecS_cool_USA",
             "L2233.GlobalIntTechEff_elecS_USA",
             "L2233.GlobalIntTechEff_elecS_cool_USA",
             "L2233.GlobalIntTechLifetime_elecS_cool_USA",
             "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
             "L2233.GlobalIntTechOMvar_elecS_cool_USA",
             "L2233.GlobalIntTechShrwt_elecS_cool_USA",
             "L2233.PrimaryRenewKeyword_elecS_cool_USA",
             "L2233.PrimaryRenewKeywordInt_elecS_cool_USA",
             "L2233.StubTechEff_elecS_cool_USA",
             "L2233.StubTechMarket_elecS_cool_USA",
             "L2233.StubTechProd_elecS_cool_USA",
             "L2233.StubTechSCurve_elecS_cool_USA",
             "L2233.StubTechCapFactor_elecS_solar_USA",
             "L2233.StubTechCapFactor_elecS_wind_USA",
             "L2233.StubTechElecMarket_backup_elecS_cool_USA",
             "L2233.StubTechFixOut_elecS_cool_USA",
             "L2233.StubTechFixOut_hydro_elecS_cool_USA",
             "L2233.StubTechMarket_backup_elecS_cool_USA",
             "L2233.StubTechLifetime_elecS_cool_USA",
             "L2233.SubsectorLogit_elecS_USA",
             "L2233.SubsectorLogit_elecS_cool_USA",
             "L2233.SubsectorShrwt_elecS_USA",
             "L2233.SubsectorShrwt_elecS_cool_USA",
             "L2233.SubsectorShrwtInterp_elecS_cool_USA",
             "L2233.SubsectorShrwtInterpTo_elecS_cool_USA",
             "L2233.Supplysector_elecS_cool_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region<- supplysector <- subsector <-  technology <- year <- minicam.energy.input <-
      coefficient<- market.name <- Electric.sector <- Electric.sector.intermittent.technology <-
      intermittent.technology <- subsector_1 <- sector <- fuel <- Electric.sector.technology <-
      water_sector <- water_type <- state <- state_name <- ':=' <- x <- plant_type <-
      State <- value <- NULL

    # ===================================================
    # 1. Read files

    # Load required inputs
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    A23.elec_tech_mapping_coal_retire_new <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_coal_retire_new")
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map")
    L1233.out_EJ_state_elec_F_tech_cool <- get_data(all_data, "L1233.out_EJ_state_elec_F_tech_cool")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")
    L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalTechShrwt_elec_cool")
    L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.GlobalTechProfitShutdown_elec_cool")
    L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool")
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru")
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool")
    L2233.GlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapital_elec_cool")
    L2233.GlobalIntTechEff_elec_cool <- get_data(all_data, "L2233.GlobalIntTechEff_elec_cool")
    L2234.AvgFossilEffKeyword_elecS_USA <- get_data(all_data, "L2234.AvgFossilEffKeyword_elecS_USA")
    L2234.GlobalIntTechBackup_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechBackup_elecS_USA")
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechCapital_elecS_USA")
    L2234.GlobalIntTechEff_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechEff_elecS_USA")
    L2234.GlobalIntTechLifetime_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechLifetime_elecS_USA")
    L2234.GlobalIntTechOMfixed_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechOMfixed_elecS_USA")
    L2234.GlobalIntTechOMvar_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechOMvar_elecS_USA")
    L2234.GlobalIntTechShrwt_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechShrwt_elecS_USA")
    L2234.GlobalTechCapFac_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapFac_elecS_USA")
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapital_elecS_USA")
    L2234.GlobalTechCapture_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapture_elecS_USA")
    L2234.GlobalTechEff_elecS_USA <- get_data(all_data,"L2234.GlobalTechEff_elecS_USA")
    L2234.GlobalTechLifetime_elecS_USA <- get_data(all_data,"L2234.GlobalTechLifetime_elecS_USA")
    L2234.GlobalTechOMfixed_elecS_USA <- get_data(all_data,"L2234.GlobalTechOMfixed_elecS_USA")
    L2234.GlobalTechOMvar_elecS_USA <- get_data(all_data,"L2234.GlobalTechOMvar_elecS_USA")
    L2234.GlobalTechProfitShutdown_elecS_USA <- get_data(all_data,"L2234.GlobalTechProfitShutdown_elecS_USA")
    L2234.GlobalTechSCurve_elecS_USA <- get_data(all_data,"L2234.GlobalTechSCurve_elecS_USA")
    L2234.GlobalTechShrwt_elecS_USA <- get_data(all_data,"L2234.GlobalTechShrwt_elecS_USA")
    L2234.PrimaryRenewKeyword_elecS_USA <- get_data(all_data,"L2234.PrimaryRenewKeyword_elecS_USA")
    L2234.PrimaryRenewKeywordInt_elecS_USA <- get_data(all_data,"L2234.PrimaryRenewKeywordInt_elecS_USA")
    L2234.StubTechCapFactor_elecS_solar_USA <- get_data(all_data,"L2234.StubTechCapFactor_elecS_solar_USA")
    L2234.StubTechCapFactor_elecS_wind_USA <- get_data(all_data,"L2234.StubTechCapFactor_elecS_wind_USA")
    L2234.StubTechEff_elecS_USA <- get_data(all_data,"L2234.StubTechEff_elecS_USA")
    L2234.StubTechElecMarket_backup_elecS_USA <- get_data(all_data,"L2234.StubTechElecMarket_backup_elecS_USA")
    L2234.StubTechFixOut_elecS_USA <- get_data(all_data,"L2234.StubTechFixOut_elecS_USA")
    L2234.StubTechFixOut_hydro_elecS_USA <- get_data(all_data,"L2234.StubTechFixOut_hydro_elecS_USA")
    L2234.StubTechMarket_backup_elecS_USA <- get_data(all_data,"L2234.StubTechMarket_backup_elecS_USA")
    L2234.StubTechMarket_elecS_USA <- get_data(all_data,"L2234.StubTechMarket_elecS_USA")
    L2234.StubTechProd_elecS_USA <- get_data(all_data,"L2234.StubTechProd_elecS_USA")
    L2234.SubsectorLogit_elecS_USA <- get_data(all_data,"L2234.SubsectorLogit_elecS_USA")
    L2234.SubsectorShrwt_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwt_elecS_USA")
    L2234.SubsectorShrwtInterp_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwtInterp_elecS_USA")
    L2234.SubsectorShrwtInterpTo_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwtInterpTo_elecS_USA")
    L2234.Supplysector_elecS_USA <- get_data(all_data,"L2234.Supplysector_elecS_USA")
    L2240.GlobalTechCapFac_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechCapFac_elec_coalret_USA")
    L2240.GlobalTechCapital_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechCapital_elec_coalret_USA")
    L2240.GlobalTechEff_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechEff_elec_coalret_USA")
    L2240.GlobalTechOMfixed_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechOMfixed_elec_coalret_USA")
    L2240.GlobalTechOMvar_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechOMvar_elec_coalret_USA")
    L2240.GlobalTechProfitShutdown_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechProfitShutdown_elec_coalret_USA")
    L2240.GlobalTechShrwt_elec_coalret_USA <- get_data(all_data,"L2240.GlobalTechShrwt_elec_coalret_USA")
    L2240.StubTechEff_elec_coalret_USA <- get_data(all_data,"L2240.StubTechEff_elec_coalret_USA")
    L2240.StubTechMarket_elec_coalret_USA <- get_data(all_data,"L2240.StubTechMarket_elec_coalret_USA")
    L2240.StubTechProd_elec_coalret_USA <- get_data(all_data,"L2240.StubTechProd_elec_coalret_USA")
    L2240.StubTechSCurve_elec_coalret_USA <- get_data(all_data,"L2240.StubTechSCurve_elec_coalret_USA")
    L2246.GlobalTechCapFac_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechCapFac_coal_vintage_USA")
    L2246.GlobalTechCapital_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechCapital_coal_vintage_USA")
    L2246.GlobalTechEff_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechEff_coal_vintage_USA")
    L2246.GlobalTechOMfixed_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechOMfixed_coal_vintage_USA")
    L2246.GlobalTechOMvar_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechOMvar_coal_vintage_USA")
    L2246.GlobalTechShrwt_coal_vintage_USA <- get_data(all_data,"L2246.GlobalTechShrwt_coal_vintage_USA")
    L2246.StubTechEff_coal_vintage_USA <- get_data(all_data,"L2246.StubTechEff_coal_vintage_USA")
    L2246.StubTechLifetime_coal_vintage_USA <- get_data(all_data,"L2246.StubTechLifetime_coal_vintage_USA")
    L2246.StubTechMarket_coal_vintage_USA <- get_data(all_data,"L2246.StubTechMarket_coal_vintage_USA")
    L2246.StubTechProd_coal_vintage_USA <- get_data(all_data,"L2246.StubTechProd_coal_vintage_USA")
    L2246.StubTechProfitShutdown_coal_vintage_USA <- get_data(all_data,"L2246.StubTechProfitShutdown_coal_vintage_USA")
    L2246.StubTechSCurve_coal_vintage_USA <- get_data(all_data,"L2246.StubTechSCurve_coal_vintage_USA")

    # -----------------------------------------------------------------------------
    A23.elecS_tech_mapping_cool %>%
      select(-supplysector, -water_type, -cooling_system, -plant_type) ->
      A23.elecS_tech_mapping_cool


    # Here we will combine coal retire + coal vintages + other electricity subsectors that are
    # currently located in separate files
    # This also adds each cooling technology and trasfers coefficients calculated for cooling techs if need
    # All variables here are global
   L2234.GlobalTechEff_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows((L2246.GlobalTechEff_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name)),
                L2240.GlobalTechEff_elec_coalret_USA)  %>%
       left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
                select(-subsector_1, -efficiency, -technology.y)%>%
                rename(sector.name=supplysector, subsector.name0=subsector, subsector.name = technology, technology = to.technology)%>%
      left_join_keep_first_only(L2233.GlobalTechEff_elec_cool %>%
                  select(technology, efficiency), by=c("technology")) %>%
      arrange(sector.name,year)->
      L2233.GlobalTechEff_elec_cool_USA

    L2234.GlobalTechShrwt_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.GlobalTechShrwt_coal_vintage_USA,
                L2240.GlobalTechShrwt_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "sector.name"="Electric.sector","subsector.name" ="subsector")) %>%
                select(-subsector_1, -share.weight, -technology.y)%>%
                rename(subsector.name0=subsector.name, subsector.name = technology, technology = to.technology)%>%
      left_join_keep_first_only(L2233.GlobalTechShrwt_elec_cool %>%
                  select(technology, share.weight), by=c("technology")) %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechShrwt_elec_cool_USA

    # Add all vintages to profit shutdown tibbles as they initially exist at state level, not global
    L2234.GlobalTechProfitShutdown_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
                bind_rows(L2240.GlobalTechProfitShutdown_elec_coalret_USA,
                          (select(L2246.StubTechProfitShutdown_coal_vintage_USA, -region) %>%
                             unique() %>% rename(technology=stub.technology))
                          ) %>%
                          left_join(A23.elecS_tech_mapping_cool,
                                    by=c("technology"="Electric.sector.technology",
                                         "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1, -median.shutdown.point, -technology.y)%>%
      rename(subsector0=subsector, subsector = technology, technology = to.technology)%>%
      left_join_keep_first_only(L2233.GlobalTechProfitShutdown_elec_cool %>%
                  select(technology, median.shutdown.point), by=c("technology")) %>%
      arrange(supplysector,year) ->
      L2233.GlobalTechProfitShutdown_elec_cool_USA

    L2234.GlobalTechOMvar_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.GlobalTechOMvar_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechOMvar_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1, -technology.y)%>%
      rename(sector.name = supplysector, subsector.name0=subsector,subsector.name = technology, technology = to.technology)%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechOMvar_elec_USA

    L2234.GlobalTechOMfixed_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.GlobalTechOMfixed_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechOMfixed_elec_coalret_USA) %>%
                left_join(A23.elecS_tech_mapping_cool,
                          by=c("technology"="Electric.sector.technology",
                               "supplysector"="Electric.sector","subsector")) %>%
              select(-subsector_1,  -technology.y)%>%
      rename(sector.name = supplysector, subsector.name0=subsector,subsector.name = technology, technology = to.technology)%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechOMfixed_elec_USA

    L2234.GlobalTechCapital_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.GlobalTechCapital_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechCapital_elec_coalret_USA) %>%
       left_join(A23.elecS_tech_mapping_cool,
                          by=c("technology"="Electric.sector.technology",
                               "supplysector"="Electric.sector","subsector")) %>%
                          select(-technology.y,-subsector_1)%>%
      rename(sector.name = supplysector, subsector.name0=subsector,subsector.name = technology, technology = to.technology)%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechCapital_elec_USA

    L2233.GlobalTechCapital_elec_USA%>%
      select(-input.capital, - capital.overnight, - fixed.charge.rate) %>%
      left_join_keep_first_only(
        bind_rows(L2233.GlobalTechCapital_elec_cool,
                  L2233.GlobalTechCapital_elecPassthru) %>%
                  select(technology, input.capital, capital.overnight, fixed.charge.rate),by=c("technology")) %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechCapital_elec_cool_USA

    L2234.GlobalTechCapFac_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.GlobalTechCapFac_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechCapFac_elec_coalret_USA) %>%
                left_join(A23.elecS_tech_mapping_cool,
                          by=c("technology"="Electric.sector.technology",
                               "supplysector"="Electric.sector","subsector")) %>%
                          select(-technology.y,-subsector_1)%>%
      rename(sector.name = supplysector, subsector.name0=subsector,subsector.name = technology, technology = to.technology)%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechCapFac_elec_USA

    # If there are not s-curves, specifically for coal, defined at the state level (L2233.StubTechSCurve_elec_USA below), we will
    # we will assume that they follow the predefined lifetime and s-curves for all other techs
    L2234.GlobalTechSCurve_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology.y)%>%
      rename(subsector0=subsector,
             subsector=technology,
             technology = to.technology)%>%
      arrange(supplysector,year) ->
      L2233.GlobalTechSCurve_elecS_USA

    L2233.GlobalTechCoef_elec_cool %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="technology",
                     "technology"="to.technology")) %>%
      select(-sector.name,-subsector_1,-subsector.name)%>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology)%>%
      arrange(supplysector,subsector0,subsector,technology,year) ->
      L2233.GlobalTechCoef_elec_cool_USA

    L2234.GlobalTechCapture_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology.y)%>%
      rename(subsector0=subsector,
             subsector=technology,
             technology = to.technology)%>%
      arrange(supplysector,year) ->
      L2233.GlobalTechCapture_elecS_USA

    L2234.GlobalTechLifetime_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology.y)%>%
      rename(subsector0=subsector,
             subsector=technology,
             technology = to.technology)%>%
      arrange(supplysector,year) ->
      L2233.GlobalTechLifetime_elecS_cool_USA

    L2234.AvgFossilEffKeyword_elecS_USA %>%
      filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "sector.name"="Electric.sector","subsector.name"="subsector")) %>%
      select(-subsector_1,-technology.y)%>%
      rename(subsector = technology,
             technology = to.technology,
             subsector.name0 = subsector.name)%>%
      arrange(sector.name,year) ->
      L2233.AvgFossilEffKeyword_elecS_USA

    # Introduce GlobalInt techs

    # no changes needed for backup techs as cooling is not directly associated in previous cooling files
    L2234.GlobalIntTechBackup_elecS_USA %>%
      mutate(subsector0=subsector,subsector = intermittent.technology) ->
      L2233.GlobalIntTechBackup_elecS_USA

    # isolate int techs that do not have cooling techs associated
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(!grepl("CSP",intermittent.technology)) %>%
      rename(subsector0=subsector, subsector = intermittent.technology) %>%
      mutate(intermittent.technology=subsector)->
      L2233.GlobalIntTechCapital_elecS_USA

    # Isolate CSP techs that have two capital costs
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(grepl("CSP",intermittent.technology)) %>%
      left_join(A23.elecS_tech_mapping_cool,
               by=c("intermittent.technology"="Electric.sector.technology",
                    "supplysector"="Electric.sector", "subsector")) %>%
      select(-technology,-subsector_1,)%>%
      rename(subsector0=subsector,
             subsector = intermittent.technology,
             intermittent.technology = to.technology)%>%
      arrange(supplysector,year) ->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    L2233.GlobalIntTechCapital_elec_cool %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="subsector_1")) %>%
      select(-sector.name,-subsector.name,-technology.x,-technology.y)%>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology,
             intermittent.technology=to.technology)%>%
      bind_rows(L2233.GlobalIntTechCapital_elecS_USA,
                L2233.GlobalIntTechCapital_elecS_cool_USA) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    # isolate CSP sectors which will need cooling tech layer added
    L2234.GlobalIntTechEff_elecS_USA %>%
      filter(!grepl("CSP",intermittent.technology)) %>%
      rename(subsector0=subsector,
        subsector = intermittent.technology) %>%
      mutate(intermittent.technology=subsector)->
      L2233.GlobalIntTechEff_elecS_USA

    L2233.GlobalIntTechEff_elec_cool %>%
      filter(grepl("CSP",technology)) %>%
      mutate(technology = gsub("CSP","CSP_storage",technology)) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="subsector_1",
                     "technology" = "to.technology")) %>%
      select(-sector.name,-subsector.name,-technology.y)%>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology,
             intermittent.technology = technology)%>%
      bind_rows(L2233.GlobalIntTechEff_elecS_USA) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechEff_elecS_cool_USA

    L2234.GlobalIntTechLifetime_elecS_USA %>%
      filter(!grepl("CSP",intermittent.technology)) %>%
      rename(subsector0=subsector,
        subsector = intermittent.technology) %>%
      mutate(intermittent.technology=subsector) %>%
      bind_rows(L2234.GlobalIntTechLifetime_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            by=c("supplysector"="Electric.sector", "intermittent.technology"="Electric.sector.technology", "subsector")) %>%
                  select(-technology, -subsector_1)%>%
                  rename(subsector0=subsector,
                    subsector=intermittent.technology,
                         intermittent.technology = to.technology)

      ) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechLifetime_elecS_cool_USA

    L2234.GlobalIntTechOMfixed_elecS_USA %>%
      filter(!grepl("CSP",intermittent.technology)) %>%
      rename(subsector0=subsector,
             subsector = intermittent.technology) %>%
      mutate(intermittent.technology=subsector) %>%
      bind_rows(L2234.GlobalIntTechOMfixed_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            by=c("supplysector"="Electric.sector", "intermittent.technology"="Electric.sector.technology", "subsector")) %>%
                  select(-technology, -subsector_1)%>%
                  rename(subsector0=subsector,
                         subsector=intermittent.technology,
                         intermittent.technology = to.technology)

      ) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechOMfixed_elecS_cool_USA

    L2234.GlobalIntTechOMvar_elecS_USA %>%
      mutate(subsector0=subsector, subsector = intermittent.technology) ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2234.GlobalIntTechShrwt_elecS_USA %>%
      filter(!grepl("CSP",intermittent.technology)) %>%
      rename(subsector.name0=subsector.name,
             subsector.name = intermittent.technology) %>%
      mutate(intermittent.technology=subsector.name) %>%
      bind_rows(L2234.GlobalIntTechShrwt_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            by=c("sector.name"="Electric.sector", "intermittent.technology"="Electric.sector.technology", "subsector.name"= "subsector")) %>%
                  select(-technology, -subsector_1)%>%
                  rename(subsector.name0=subsector.name,
                         subsector.name=intermittent.technology,
                         intermittent.technology = to.technology)

      ) %>%
      arrange(sector.name,subsector.name0,subsector.name,intermittent.technology,year)->
      L2233.GlobalIntTechShrwt_elecS_cool_USA

    L2234.PrimaryRenewKeyword_elecS_USA %>%
      mutate(subsector0=subsector, subsector = intermittent.technology) ->
      L2233.PrimaryRenewKeyword_elecS_cool_USA

    L2234.PrimaryRenewKeywordInt_elecS_USA %>%
      mutate(subsector0=subsector, subsector = intermittent.technology) ->
      L2233.PrimaryRenewKeywordInt_elecS_cool_USA

    # We can now look to state level data, once again combining vintages and adding cooling technologies
    # Efficiencies do not change with the addition of cooling techs
    L2234.StubTechEff_elecS_USA %>%
      filter(stub.technology!="coal_base_conv pul"&stub.technology!="coal_int_conv pul"&stub.technology!="coal_peak_conv pul"&stub.technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.StubTechEff_coal_vintage_USA,
                L2240.StubTechEff_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0=subsector,
             subsector=stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechEff_elec_USA

    L2234.StubTechMarket_elecS_USA %>%
      filter(stub.technology!="coal_base_conv pul"&stub.technology!="coal_int_conv pul"&stub.technology!="coal_peak_conv pul"&stub.technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.StubTechMarket_coal_vintage_USA,
                L2240.StubTechMarket_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0=subsector,
             subsector=stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechMarket_elec_USA

    L1233.out_EJ_state_elec_F_tech_cool %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(select(elec_tech_water_map,
                                      -from.supplysector, -from.subsector, -from.technology, -minicam.energy.input, -sector, -to.subsector,-to.supplysector),
                               by = c("fuel", "technology", "cooling_system", "water_type")) %>%
      select(-plant_type,-technology,-cooling_system,-water_type,) %>%
      rename(supplysector = sector, subsector0 = fuel,  technology=to.technology, region=state) %>%
      mutate(value = round(value, 7)) ->
      L1233.out_EJ_state_elec_F_tech_cool


    L2234.StubTechProd_elecS_USA %>%
      filter(stub.technology!="coal_base_conv pul"&stub.technology!="coal_int_conv pul"&stub.technology!="coal_peak_conv pul"&stub.technology!="coal_subpeak_conv pul") %>%
      bind_rows(L2246.StubTechProd_coal_vintage_USA,
                L2240.StubTechProd_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      left_join(L1233.out_EJ_state_elec_F_tech_cool, by=c("supplysector","subsector0","subsector","technology","region","year")) %>%
      mutate(calOutputValue = calOutputValue*value) %>% replace_na(list(calOutputValue=0)) %>%
      select(-value) %>%
      arrange(region,year) ->
      L2233.StubTechProd_elec_USA


    # State level s-curves, combining vintages and initial slow and fast retire plants
    bind_rows(L2246.StubTechSCurve_coal_vintage_USA,
              L2240.StubTechSCurve_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechSCurve_elec_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechCapFactor_elecS_solar_USA


    L2234.StubTechCapFactor_elecS_wind_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechCapFactor_elecS_wind_USA

    L2234.StubTechElecMarket_backup_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      mutate( technology = ifelse(subsector=="rooftop_pv",subsector,technology)) %>%
      arrange(region,year) ->
      L2233.StubTechElecMarket_backup_elecS_USA

    L2234.StubTechFixOut_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechFixOut_elecS_USA

    L2234.StubTechFixOut_hydro_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      arrange(region,year) ->
      L2233.StubTechFixOut_hydro_elecS_USA

    L2234.StubTechMarket_backup_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-technology,-subsector_1)%>%
      rename(technology = to.technology,
             subsector0 = subsector,
             subsector = stub.technology)%>%
      mutate( technology = ifelse(subsector=="rooftop_pv",subsector,technology)) %>%
      arrange(region,year) ->
      L2233.StubTechMarket_backup_elecS_USA

    L2246.StubTechLifetime_coal_vintage_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("stub.technology"="Electric.sector.technology",
                     "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology)%>%
      rename(subsector0=subsector,
             subsector=stub.technology,
             technology = to.technology)%>%
      arrange(supplysector,year) ->
      L2233.StubTechLifetime_elecS_cool_USA

    L2234.SubsectorLogit_elecS_USA %>%
      rename(subsector0=subsector) %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.SubsectorLogit_elecS_USA

    L2234.SubsectorLogit_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology, -to.technology)%>%
      rename(subsector0=subsector,
             subsector=Electric.sector.technology)%>%
      arrange(supplysector) ->
      L2233.SubsectorLogit_elecS_cool_USA

    L2234.SubsectorShrwt_elecS_USA %>%
      rename(subsector0=subsector) %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.SubsectorShrwt_elecS_USA

    L2234.SubsectorShrwt_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1,-technology, -to.technology)%>%
      rename(subsector0=subsector,
             subsector=Electric.sector.technology)%>%
      arrange(supplysector) ->
      L2233.SubsectorShrwt_elecS_cool_USA

    L2234.SubsectorShrwtInterp_elecS_USA %>%
      rename(subsector0=subsector) %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.SubsectorShrwtInterp_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      rename(subsector0=subsector) %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.SubsectorShrwtInterpTo_elecS_USA

    # Only done to manipulate variable and not throw an error
    L2234.Supplysector_elecS_USA %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.Supplysector_elecS_USA


    # Outputs

    L2233.GlobalTechEff_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechEff_elec_cool") %>%
      add_precursors("L2234.GlobalTechEff_elecS_USA",
                     "L2246.GlobalTechEff_coal_vintage_USA",
                     "L2240.GlobalTechEff_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechEff_elec_cool") ->
      L2233.GlobalTechEff_elecS_cool_USA

    L2233.GlobalTechShrwt_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elec_cool") %>%
      add_precursors("L2234.GlobalTechShrwt_elecS_USA",
                     "L2246.GlobalTechShrwt_coal_vintage_USA",
                     "L2240.GlobalTechShrwt_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechShrwt_elec_cool") ->
      L2233.GlobalTechShrwt_elecS_cool_USA

    L2233.GlobalTechProfitShutdown_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechProfitShutdown_elec_cool") %>%
      add_precursors("L2234.GlobalTechProfitShutdown_elecS_USA",
                     "L2240.GlobalTechProfitShutdown_elec_coalret_USA",
                     "L2246.StubTechProfitShutdown_coal_vintage_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechProfitShutdown_elec_cool") ->
      L2233.GlobalTechProfitShutdown_elecS_cool_USA

    L2233.GlobalTechOMvar_elec_USA %>%
      add_title("Electricity Load Segments Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elec") %>%
      add_precursors("L2234.GlobalTechOMvar_elecS_USA",
                     "L2246.GlobalTechOMvar_coal_vintage_USA",
                     "L2240.GlobalTechOMvar_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechOMvar_elecS_cool_USA

    L2233.GlobalTechOMfixed_elec_USA %>%
      add_title("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elec") %>%
      add_precursors("L2234.GlobalTechOMfixed_elecS_USA",
                     "L2246.GlobalTechOMfixed_coal_vintage_USA",
                     "L2240.GlobalTechOMfixed_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechOMfixed_elecS_cool_USA

    L2233.GlobalTechCapital_elec_USA %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec") %>%
      add_precursors("L2234.GlobalTechCapital_elecS_USA",
                     "L2246.GlobalTechCapital_coal_vintage_USA",
                     "L2240.GlobalTechCapital_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCapital_elecS_USA

    L2233.GlobalTechCapital_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("L2234.GlobalTechCapital_elecS_USA",
                     "L2246.GlobalTechCapital_coal_vintage_USA",
                     "L2240.GlobalTechCapital_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechCapital_elec_cool",
                     "L2233.GlobalTechCapital_elecPassthru") ->
      L2233.GlobalTechCapital_elecS_cool_USA

    L2233.GlobalTechCapFac_elec_USA %>%
      add_title("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechCapFac_elec") %>%
      add_precursors("L2234.GlobalTechCapFac_elecS_USA",
                     "L2246.GlobalTechCapFac_coal_vintage_USA",
                     "L2240.GlobalTechCapFac_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCapFac_elecS_cool_USA

    L2233.GlobalTechSCurve_elecS_USA %>%
      add_title("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechSCurve_elec") %>%
      add_precursors("L2234.GlobalTechSCurve_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechSCurve_elecS_cool_USA

    L2233.GlobalTechCoef_elec_cool_USA %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCoef_elecS_cool_USA

    L2233.GlobalTechCapture_elecS_USA %>%
      add_title("Electricity Load Segments CCS Technology Characteristics") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechCapture_elec") %>%
      add_precursors("L2234.GlobalTechCapture_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elec_tech_mapping_coal_retire_new")->
      L2233.GlobalTechCapture_elecS_cool_USA

    L2233.GlobalTechLifetime_elecS_cool_USA %>%
    add_title("Electricity Lifetime") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechLifetime_elec_cool") %>%
      add_precursors("L2234.GlobalTechLifetime_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool")->
    L2233.GlobalTechLifetime_elecS_cool_USA

    L2233.AvgFossilEffKeyword_elecS_USA %>%
      add_title("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.AvgFossilEffKeyword_elec") %>%
      add_precursors("L2234.AvgFossilEffKeyword_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.AvgFossilEffKeyword_elecS_cool_USA

    L2233.GlobalIntTechBackup_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechBackup_elec") %>%
      add_precursors("L2234.GlobalIntTechBackup_elecS_USA") ->
      L2233.GlobalIntTechBackup_elecS_cool_USA

    L2233.GlobalIntTechCapital_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
      add_precursors("L2234.GlobalIntTechCapital_elecS_USA") ->
      L2233.GlobalIntTechCapital_elecS_USA

    L2233.GlobalIntTechCapital_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name(" L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechCapital_elecS_USA",
                     "L2233.GlobalIntTechCapital_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    L2233.GlobalIntTechEff_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechEff_elec") %>%
      add_precursors("L2234.GlobalIntTechEff_elecS_USA") ->
      L2233.GlobalIntTechEff_elecS_USA

    L2233.GlobalIntTechEff_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechEff_elec") %>%
      add_precursors("L2234.GlobalIntTechEff_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalIntTechEff_elec_cool") ->
      L2233.GlobalIntTechEff_elecS_cool_USA

    L2233.GlobalIntTechLifetime_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechLifetime_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechLifetime_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechLifetime_elecS_cool_USA

    L2233.GlobalIntTechOMfixed_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechOMfixed_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechOMfixed_elecS_cool_USA

    L2233.GlobalIntTechOMvar_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechOMvar_elecS_USA") ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2233.GlobalIntTechShrwt_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechShrwt_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechShrwt_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechShrwt_elecS_cool_USA

    L2233.PrimaryRenewKeyword_elecS_cool_USA %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.PrimaryRenewKeyword_elec_cool") %>%
      add_precursors("L2234.PrimaryRenewKeyword_elecS_USA") ->
      L2233.PrimaryRenewKeyword_elecS_cool_USA

    L2233.PrimaryRenewKeywordInt_elecS_cool_USA %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.PrimaryRenewKeywordInt_elec_cool") %>%
      add_precursors("L2234.PrimaryRenewKeywordInt_elecS_USA") ->
      L2233.PrimaryRenewKeywordInt_elecS_cool_USA

    L2233.StubTechEff_elec_USA %>%
      add_title("Electricity Load Segments Base Year Efficiencies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechEff_elec") %>%
      add_precursors("L2234.StubTechEff_elecS_USA",
                     "L2246.StubTechEff_coal_vintage_USA",
                     "L2240.StubTechEff_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechEff_elecS_cool_USA

    L2233.StubTechMarket_elec_USA %>%
      add_title("Energy Inputs for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechMarket_elec") %>%
      add_precursors("L2234.StubTechMarket_elecS_USA",
                     "L2246.StubTechMarket_coal_vintage_USA",
                     "L2240.StubTechMarket_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechMarket_elecS_cool_USA

    L2233.StubTechProd_elec_USA %>%
      add_title("Electricity Load Segments Technology Calibration Outputs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechProd_elec") %>%
      add_precursors("L2234.StubTechProd_elecS_USA",
                     "L2246.StubTechProd_coal_vintage_USA",
                     "L2240.StubTechProd_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L1233.out_EJ_state_elec_F_tech_cool",
                     "water/elec_tech_water_map") ->
      L2233.StubTechProd_elecS_cool_USA

    L2233.StubTechSCurve_elec_USA %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechSCurve_elec") %>%
      add_precursors("L2246.StubTechSCurve_coal_vintage_USA",
                     "L2240.StubTechSCurve_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechSCurve_elecS_cool_USA

    L2233.StubTechCapFactor_elecS_solar_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Solar Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechCapFactor_elec_solar") %>%
      add_precursors("L2234.StubTechCapFactor_elecS_solar_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCapFactor_elecS_solar_USA

    L2233.StubTechCapFactor_elecS_wind_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Wind Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechCapFactor_elec_wind") %>%
      add_precursors("L2234.StubTechCapFactor_elecS_wind_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCapFactor_elecS_wind_USA

    L2233.StubTechElecMarket_backup_elecS_USA %>%
      add_title("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechElecMarket_backup_elec") %>%
      add_precursors("L2234.StubTechElecMarket_backup_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechElecMarket_backup_elecS_cool_USA

    L2233.StubTechFixOut_elecS_USA %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechFixOut_elec") %>%
      add_precursors("L2234.StubTechFixOut_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechFixOut_elecS_cool_USA

    L2233.StubTechFixOut_hydro_elecS_USA %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechFixOut_hydro_elec") %>%
      add_precursors("L2234.StubTechFixOut_hydro_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechFixOut_hydro_elecS_cool_USA

    L2233.StubTechMarket_backup_elecS_USA %>%
      add_title("Backup Energy Inputs for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechMarket_backup_elec") %>%
      add_precursors("L2234.StubTechMarket_backup_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechMarket_backup_elecS_cool_USA

    L2233.StubTechLifetime_elecS_cool_USA %>%
      add_title("Backup Energy Inputs for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalTechLifetime_elec_cool") %>%
      add_precursors("L2246.StubTechLifetime_coal_vintage_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
    L2233.StubTechLifetime_elecS_cool_USA

    L2233.SubsectorLogit_elecS_USA %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L223.SubsectorLogit_elec") %>%
      add_precursors("L2234.SubsectorLogit_elecS_USA") ->
      L2233.SubsectorLogit_elecS_USA

    L2233.SubsectorLogit_elecS_cool_USA %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
      add_precursors("L2234.SubsectorLogit_elecS_USA") ->
      L2233.SubsectorLogit_elecS_cool_USA

    L2233.SubsectorShrwt_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.SubsectorShrwt_elec") %>%
      add_precursors("L2234.SubsectorShrwt_elecS_USA") ->
      L2233.SubsectorShrwt_elecS_USA

    L2233.SubsectorShrwt_elecS_cool_USA%>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.SubsectorShrwt_elec") %>%
      add_precursors("L2234.SubsectorShrwt_elecS_USA") ->
    L2233.SubsectorShrwt_elecS_cool_USA

    L2233.SubsectorShrwtInterp_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.SubsectorShrwtInterp_elec") %>%
      add_precursors("L2234.SubsectorShrwtInterp_elecS_USA") ->
      L2233.SubsectorShrwtInterp_elecS_cool_USA

    L2233.SubsectorShrwtInterpTo_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.SubsectorShrwtInterpTo_elec") %>%
      add_precursors("L2234.SubsectorShrwtInterpTo_elecS_USA") ->
      L2233.SubsectorShrwtInterpTo_elecS_cool_USA

    L2233.Supplysector_elecS_USA %>%
      add_title("Supply Sector Information for Electricity Load Segments") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.Supplysector_elec") %>%
      add_precursors("L2234.Supplysector_elecS_USA") ->
      L2233.Supplysector_elecS_cool_USA


    return_data(L2233.GlobalTechEff_elecS_cool_USA,
                L2233.GlobalTechShrwt_elecS_cool_USA,
                L2233.GlobalTechProfitShutdown_elecS_cool_USA,
                L2233.GlobalTechOMvar_elecS_cool_USA,
                L2233.GlobalTechOMfixed_elecS_cool_USA,
                L2233.GlobalTechCapital_elecS_USA,
                L2233.GlobalTechCapital_elecS_cool_USA,
                L2233.GlobalTechCapFac_elecS_cool_USA,
                L2233.GlobalTechSCurve_elecS_cool_USA,
                L2233.GlobalTechCoef_elecS_cool_USA,
                L2233.GlobalTechCapture_elecS_cool_USA,
                L2233.GlobalTechLifetime_elecS_cool_USA,
                L2233.AvgFossilEffKeyword_elecS_cool_USA,
                L2233.GlobalIntTechBackup_elecS_cool_USA,
                L2233.GlobalIntTechCapital_elecS_USA,
                L2233.GlobalIntTechCapital_elecS_cool_USA,
                L2233.GlobalIntTechEff_elecS_USA,
                L2233.GlobalIntTechEff_elecS_cool_USA,
                L2233.GlobalIntTechLifetime_elecS_cool_USA,
                L2233.GlobalIntTechOMfixed_elecS_cool_USA,
                L2233.GlobalIntTechOMvar_elecS_cool_USA,
                L2233.GlobalIntTechShrwt_elecS_cool_USA,
                L2233.PrimaryRenewKeyword_elecS_cool_USA,
                L2233.PrimaryRenewKeywordInt_elecS_cool_USA,
                L2233.StubTechEff_elecS_cool_USA,
                L2233.StubTechMarket_elecS_cool_USA,
                L2233.StubTechProd_elecS_cool_USA,
                L2233.StubTechSCurve_elecS_cool_USA,
                L2233.StubTechCapFactor_elecS_solar_USA,
                L2233.StubTechCapFactor_elecS_wind_USA,
                L2233.StubTechElecMarket_backup_elecS_cool_USA,
                L2233.StubTechFixOut_elecS_cool_USA,
                L2233.StubTechFixOut_hydro_elecS_cool_USA,
                L2233.StubTechMarket_backup_elecS_cool_USA,
                L2233.StubTechLifetime_elecS_cool_USA,
                L2233.SubsectorLogit_elecS_USA,
                L2233.SubsectorLogit_elecS_cool_USA,
                L2233.SubsectorShrwt_elecS_USA,
                L2233.SubsectorShrwt_elecS_cool_USA,
                L2233.SubsectorShrwtInterp_elecS_cool_USA,
                L2233.SubsectorShrwtInterpTo_elecS_cool_USA,
                L2233.Supplysector_elecS_cool_USA)
  } else {
    stop("Unknown command")
  }
}
