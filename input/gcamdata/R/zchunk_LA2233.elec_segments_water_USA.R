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
#' @author NTG December 2019
module_gcamusa_LA2233.elec_segments_water_USA <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire_new",
             FILE = "gcam-usa/elec_tech_water_map",
             "L1233.out_EJ_state_elec_F_tech_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.GlobalTechShrwt_elec_cool",
             "L2233.GlobalTechProfitShutdown_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool",
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
             "L2234.StubTechCost_offshore_wind_elecS_USA",
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
             "L2233.GlobalIntTechCoef_elecS_cool_USA",
             "L2233.PrimaryRenewKeyword_elecS_cool_USA",
             "L2233.PrimaryRenewKeywordInt_elecS_cool_USA",
             "L2233.StubTechEff_elecS_cool_USA",
             "L2233.StubTechCoef_elecS_cool_USA",
             "L2233.StubTechMarket_elecS_cool_USA",
             "L2233.StubTechProd_elecS_cool_USA",
             "L2233.StubTechSCurve_elecS_cool_USA",
             "L2233.StubTechCapFactor_elecS_solar_USA",
             "L2233.StubTechCapFactor_elecS_wind_USA",
             "L2233.StubTechElecMarket_backup_elecS_cool_USA",
             "L2233.StubTechFixOut_elecS_cool_USA",
             "L2233.StubTechFixOut_hydro_elecS_cool_USA",
             "L2233.StubTechMarket_backup_elecS_cool_USA",
             "L2233.StubTechProfitShutdown_elecS_cool_USA",
             "L2233.StubTechLifetime_elecS_cool_USA",
             "L2233.StubTechShrwt_elecS_cool_USA",
             "L2233.StubTechInterp_elecS_cool_USA",
             "L2233.StubTechCost_offshore_wind_elecS_cool_USA",
             "L2233.SubsectorLogit_elecS_USA",
             "L2233.SubsectorLogit_elecS_cool_USA",
             "L2233.SubsectorShrwt_elecS_USA",
             "L2233.SubsectorShrwt_elecS_cool_USA",
             "L2233.SubsectorShrwtInterp_elecS_USA",
             "L2233.SubsectorShrwtInterpTo_elecS_USA",
             #"L2233.SubsectorShrwtInterp_elecS_cool_USA",
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
    elec_tech_water_map <- get_data(all_data, "gcam-usa/elec_tech_water_map")
    L1233.out_EJ_state_elec_F_tech_cool <- get_data(all_data, "L1233.out_EJ_state_elec_F_tech_cool")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")
    L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalTechShrwt_elec_cool")
    L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.GlobalTechProfitShutdown_elec_cool")
    L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool")
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru")
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool")
    L2233.GlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapital_elec_cool")
    L2233.GlobalIntTechEff_elec_cool <- get_data(all_data, "L2233.GlobalIntTechEff_elec_cool")
    L2233.GlobalIntTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCoef_elec_cool")
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
    L2234.StubTechCost_offshore_wind_elecS_USA <- get_data(all_data,"L2234.StubTechCost_offshore_wind_elecS_USA")
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

    ### Define several filtering and renaming fuctions that will be applied
    ### throughout the zchunk

    ## We filter out all non-vintaged or retired coal technologies
    coal_filter <- function(data){
      data %>%
        filter(technology!="coal_base_conv pul"&technology!="coal_int_conv pul"&technology!="coal_peak_conv pul"&technology!="coal_subpeak_conv pul") ->
        data_new
      return(data_new)
    }

    coal_stub_filter <- function(data){
      data %>%
        filter(stub.technology!="coal_base_conv pul"&stub.technology!="coal_int_conv pul"&stub.technology!="coal_peak_conv pul"&stub.technology!="coal_subpeak_conv pul") ->
        data_new
      return(data_new)
    }

    ## To account for new nesting-subsector structure and to add cooling technologies,
    ## we must expand certain outputs
    add_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  ## Left_join used as each power plant type will now be multiplied
                  ## by up to five cooling technologies, thus increasing tibble size
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology) -> data_new

      data_new %>% filter(grepl("seawater",technology)) %>% filter(!(region %in% gcamusa.NO_SEAWATER_STATES)) %>%
        bind_rows(data_new %>% filter(!grepl("seawater",technology))) %>%
        arrange(region,year) -> data_new
      return(data_new)
    }

    add_global_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  ## Left_join used as each power plant type will now be multiplied
                  ## by up to five cooling technologies, thus increasing tibble size
                  by=c("technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-subsector_1, -technology.y)%>%
        rename(sector.name = supplysector, subsector.name0=subsector,subsector.name = technology, technology = to.technology)%>%
        arrange(sector.name,year) %>%
        mutate(technology = if_else(subsector.name0=="wind"|subsector.name0=="solar",subsector.name,
                                    if_else(subsector.name0=="grid_storage",subsector.name0,technology))) ->
        data_new
      return(data_new)
    }

    add_int_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("supplysector"="Electric.sector", "intermittent.technology"="Electric.sector.technology", "subsector")) %>%
        select(-technology, -subsector_1)%>%
        rename(subsector0=subsector,
               subsector=intermittent.technology,
               intermittent.technology = to.technology) ->
        data_new
      return(data_new)
    }


    # Here we will consolidate coal retire + coal vintages + other electricity subsectors that are
    # currently located in separate files
    # This also adds each cooling technology and trasfers coefficients calculated for cooling techs if needed
    # All variables here are global
   L2234.GlobalTechEff_elecS_USA %>%
     coal_filter() %>%
      bind_rows((L2246.GlobalTechEff_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name)),
                L2240.GlobalTechEff_elec_coalret_USA)  %>%
      add_global_cooling_techs() %>%
     select(-efficiency) %>%
     # Bring in global cooling tech efficiencies from GCAM-core
      left_join_keep_first_only(L2233.GlobalTechEff_elec_cool %>% mutate(technology = gsub("_storage.*","_base_storage",technology)) %>%
                  select(technology, efficiency,year), by=c("technology","year")) %>%
      arrange(sector.name,year)->
      L2233.GlobalTechEff_elec_cool_USA

    L2234.GlobalTechShrwt_elecS_USA %>%
      coal_filter() %>%
      bind_rows(L2246.GlobalTechShrwt_coal_vintage_USA,
                L2240.GlobalTechShrwt_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "sector.name"="Electric.sector","subsector.name" ="subsector")) %>%
                select(-subsector_1, -technology.y)%>%
                rename(subsector.name0=subsector.name, subsector.name = technology, technology = to.technology)%>%
      mutate(technology = if_else(subsector.name0=="wind"|subsector.name0=="solar",subsector.name,
                                  if_else(subsector.name0=="grid_storage",subsector.name0,technology))) %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechShrwt_elec_cool_USA

    # Add all vintages to profit shutdown tibbles as they initially exist at state level, not global
    L2234.GlobalTechProfitShutdown_elecS_USA %>%
      coal_filter() %>%
                bind_rows(L2240.GlobalTechProfitShutdown_elec_coalret_USA
                          ) %>%
                          left_join(A23.elecS_tech_mapping_cool,
                                    by=c("technology"="Electric.sector.technology",
                                         "supplysector"="Electric.sector","subsector")) %>%
      select(-subsector_1, -median.shutdown.point, -technology.y)%>%
      rename(subsector0=subsector, subsector = technology, technology = to.technology)%>%
      left_join_keep_first_only(L2233.GlobalTechProfitShutdown_elec_cool %>%
                  select(technology, median.shutdown.point), by=c("technology")) %>%
      arrange(supplysector,year) %>%
      mutate(technology = if_else(subsector0=="wind"|subsector0=="solar",subsector,
                                  if_else(subsector0=="grid_storage",subsector0,technology))) ->
      L2233.GlobalTechProfitShutdown_elec_cool_USA

    L2234.GlobalTechOMvar_elecS_USA %>%
      coal_filter() %>%
      bind_rows(L2246.GlobalTechOMvar_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechOMvar_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechOMvar_elec_USA

    L2234.GlobalTechOMfixed_elecS_USA %>%
      coal_filter() %>%
      bind_rows(L2246.GlobalTechOMfixed_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechOMfixed_elec_coalret_USA) %>%
      add_global_cooling_techs()%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechOMfixed_elec_USA

    L2234.GlobalTechCapital_elecS_USA %>%
      coal_filter() %>%
      bind_rows(L2246.GlobalTechCapital_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechCapital_elec_coalret_USA) %>%
      add_global_cooling_techs()%>%
      arrange(sector.name,year) ->
      L2233.GlobalTechCapital_elec_USA

    L2233.GlobalTechCapital_elec_USA%>%
      select(-input.capital, - capital.overnight, - fixed.charge.rate) %>%
      left_join_keep_first_only(
        bind_rows(L2233.GlobalTechCapital_elec_cool %>% mutate(technology=gsub("storage","base_storage",technology)),
                  L2233.GlobalTechCapital_elecPassthru%>% mutate(technology=gsub("storage","base_storage",technology))) %>%
                  select(technology, input.capital, capital.overnight, fixed.charge.rate),by=c("technology")) %>%
      arrange(sector.name,year) %>%
      mutate(technology = if_else(subsector.name0=="wind"|subsector.name0=="solar",subsector.name,
                                  if_else(subsector.name0=="grid_storage",subsector.name0,technology))) %>%
      na.omit()->
      # remove base_storage options as they have no cooling options
      L2233.GlobalTechCapital_elec_cool_USA

    L2234.GlobalTechCapFac_elecS_USA %>%
      coal_filter() %>%
      bind_rows(L2246.GlobalTechCapFac_coal_vintage_USA%>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2240.GlobalTechCapFac_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechCapFac_elec_USA


    # If there are not s-curves, specifically for coal, defined at the state level (L2233.StubTechSCurve_elec_USA below), we will
    # we will assume that they follow the predefined lifetime and s-curves for all other techs
    L2234.GlobalTechSCurve_elecS_USA %>%
      coal_filter() %>%
      add_global_cooling_techs() %>%
      rename(supplysector=sector.name) %>%
      arrange(supplysector,year) ->
      L2233.GlobalTechSCurve_elecS_USA

    ## ADD DESCRIPTION OF WHAT THIS IS DOING
    L2233.GlobalTechCoef_elec_cool %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="technology",
                     "technology"="to.technology")) %>%
      select(-sector.name,-subsector_1,-subsector.name)%>%
      na.omit() %>%
      bind_rows(
        L2233.GlobalTechCoef_elec_cool %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            by=c("subsector.name"="technology")) %>%
                            filter(grepl("base_storage",to.technology)) %>%
                  select(-sector.name,-subsector_1,-subsector.name, -technology) %>%
          rename(technology=to.technology) %>% na.omit(),
        L2233.GlobalTechCoef_elec_cool %>%
          left_join(A23.elecS_tech_mapping_cool,
                    by=c("technology")) %>%
          filter(grepl("hydro",to.technology)|grepl("PV_base_storage",to.technology)) %>%
          select(-sector.name,-subsector_1,-subsector.name, -technology) %>%
          rename(technology=to.technology)) %>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology)%>%
      right_join(L2233.GlobalTechEff_elec_cool_USA %>% select(sector.name,subsector.name0,subsector.name,technology),
                 by=c("supplysector"="sector.name",
                      "subsector0"="subsector.name0",
                      "subsector"="subsector.name",
                      "technology")) %>%
      arrange(supplysector,subsector0,subsector,technology,year) %>%
      na.omit()->
      L2233.GlobalTechCoef_elecS_cool_USA

    L2234.GlobalTechCapture_elecS_USA %>%
      add_global_cooling_techs() %>%
      rename(supplysector=sector.name) %>%
      arrange(supplysector,year) ->
      L2233.GlobalTechCapture_elecS_USA

    # Coal vintage lifetimes are added at state level, therefore we remove all coal conv pul from the Global Lifetime values
    L2234.GlobalTechLifetime_elecS_USA %>%
      coal_filter() %>%
      add_global_cooling_techs() %>%
      rename(supplysector=sector.name) %>%
      arrange(supplysector,year) ->
      L2233.GlobalTechLifetime_elecS_cool_USA

    L2234.AvgFossilEffKeyword_elecS_USA %>%
      coal_filter() %>%
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


    ## We filter out all non-csp techs initially as these are these are the techs
    ## are the ones which need cooling techs added
    csp_filter <- function(data){
      data %>%
        filter(!grepl("CSP",intermittent.technology)) %>%
        rename(subsector0=subsector,
               subsector = intermittent.technology) %>%
        mutate(intermittent.technology=subsector) ->
        data_new
      return(data_new)
    }

    # no changes needed for backup techs as cooling is not directly associated in previous cooling files
    L2234.GlobalIntTechBackup_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechBackup_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            by=c("supplysector"="Electric.sector", "intermittent.technology"="Electric.sector.technology", "subsector")) %>%
                  select(-technology, -subsector_1)%>%
                  rename(subsector0=subsector,
                         subsector=intermittent.technology,
                         intermittent.technology = to.technology)

      ) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechBackup_elecS_USA

    # isolate int techs that do not have cooling techs associated
    L2234.GlobalIntTechCapital_elecS_USA %>%
      csp_filter() %>%
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
      filter(!grepl("CSP_base",subsector)) %>%
      # There are no additional cases of CSP_base across other Int Techs, therefore, we will remove for now
      # as this has been introduced by calling gcam-core file L2233.GlobalIntTechCapital_elec_cool
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    # isolate CSP sectors which will need cooling tech layer added
    L2234.GlobalIntTechEff_elecS_USA %>%
      csp_filter()->
      L2233.GlobalIntTechEff_elecS_USA

    L2233.GlobalIntTechEff_elec_cool %>%
      filter(grepl("CSP",technology)) %>%
      mutate(technology = gsub("CSP","CSP_storage",technology)) %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="subsector_1")) %>%
      select(-sector.name,-subsector.name,-technology.y, -technology.x)%>%
      filter((grepl("dry_hybrid",to.technology)&efficiency<0.96)) %>%
      bind_rows( L2233.GlobalIntTechEff_elec_cool %>%
                   filter(grepl("CSP",technology)) %>%
                   mutate(technology = gsub("CSP","CSP_storage",technology)) %>%
                   left_join(A23.elecS_tech_mapping_cool,
                             by=c("subsector.name"="subsector_1")) %>%
                   select(-sector.name,-subsector.name,-technology.y, -technology.x)%>%
                   filter((grepl("recirculating",to.technology)&efficiency>0.96))

      ) %>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology,
             intermittent.technology = to.technology)%>%
      bind_rows(L2233.GlobalIntTechEff_elecS_USA) %>%
      filter(subsector!="CSP_base") %>%
      # There are no additional cases of CSP_base across other Int Techs, therefore, we will remove for now
      # as this has been introduced by calling gcam-core file L2233.GlobalIntTechEff_elec_cool
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechEff_elecS_cool_USA

    # Add global water coefs from gcam-core to GCAM-USA. Maintain the global coefs
    # Recommended to create state level water demand coefs in the future as state-level coefs are not currently applied
    L2233.GlobalIntTechCoef_elec_cool%>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("subsector.name"="subsector_1")) %>%
      select(-sector.name,-subsector.name,-technology.y, -technology.x)%>%
      na.omit() %>%
      bind_rows(
        L2233.GlobalIntTechCoef_elec_cool%>%
          left_join(A23.elecS_tech_mapping_cool,
                    by=c("technology"="subsector_1")) %>%
          select(-sector.name,-subsector.name,-technology.y, -technology)%>%
          na.omit() %>% filter(!grepl("base_storage",to.technology))
      ) %>%
      rename(supplysector=Electric.sector,
             subsector0=subsector,
             subsector=Electric.sector.technology,
             intermittent.technology = to.technology)%>%
      filter(!grepl("CSP_base",subsector)) %>%
      # There are no additional cases of CSP_base across other Int Techs, therefore, we will remove for now
      # as this has been introduced by calling gcam-core file L2233.GlobalIntTechEff_elec_cool
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechCoef_elecS_cool_USA

    L2234.GlobalIntTechLifetime_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechLifetime_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  add_int_cooling_techs

      ) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechLifetime_elecS_cool_USA

    L2234.GlobalIntTechOMfixed_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechOMfixed_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  add_int_cooling_techs

      ) %>%
      arrange(supplysector,subsector0,subsector,intermittent.technology,year)->
      L2233.GlobalIntTechOMfixed_elecS_cool_USA

    L2234.GlobalIntTechOMvar_elecS_USA %>%
      mutate(subsector0=subsector, subsector = intermittent.technology) ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2234.GlobalIntTechShrwt_elecS_USA %>%
      csp_filter()%>%
      bind_rows(L2234.GlobalIntTechShrwt_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  add_int_cooling_techs()

      ) %>%
      arrange(sector.name,subsector.name0,subsector.name,intermittent.technology,year)->
      L2233.GlobalIntTechShrwt_elecS_cool_USA

    L2234.PrimaryRenewKeyword_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology"="Electric.sector.technology",
                     "sector.name"="Electric.sector","subsector.name"="subsector")) %>%
      select(-subsector_1,-technology.y)%>%
      rename(subsector = technology,
             technology = to.technology,
             subsector.name0 = subsector.name)%>%
      arrange(sector.name,year)->
      L2233.PrimaryRenewKeyword_elecS_cool_USA

    L2234.PrimaryRenewKeywordInt_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.PrimaryRenewKeywordInt_elecS_USA %>%
                  filter(grepl("CSP",intermittent.technology)) %>%
                  add_int_cooling_techs

      ) %>%
      arrange(sector.name,subsector.name0,subsector.name,intermittent.technology,year)->
      L2233.PrimaryRenewKeywordInt_elecS_cool_USA

    # We can now look to state level data, once again combining vintages and adding cooling technologies
    # Efficiencies do not change with the addition of cooling techs


    L2234.StubTechEff_elecS_USA %>%
      coal_stub_filter() %>%
      bind_rows(L2246.StubTechEff_coal_vintage_USA,
                L2240.StubTechEff_elec_coalret_USA) %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(subsector0=="grid_storage", subsector0,technology))->
      L2233.StubTechEff_elec_USA

    L2234.StubTechMarket_elecS_USA %>%
      coal_stub_filter() %>%
      bind_rows(L2246.StubTechMarket_coal_vintage_USA,
                L2240.StubTechMarket_elec_coalret_USA) %>%
      add_cooling_techs()%>%
      mutate(technology = if_else(grepl("base_storage",subsector0),subsector0,
                                  if_else(grepl("wind_base",subsector),subsector,technology)))->
      # temporary placement of base_storage cooling techs = nesting subsector
      L2233.StubTechMarket_elec_USA


    L1233.out_EJ_state_elec_F_tech_cool %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(select(elec_tech_water_map,
                                      -from.supplysector, -from.subsector, -from.technology, -minicam.energy.input, -sector, -to.subsector,-to.supplysector),
                               by = c("fuel", "technology", "cooling_system", "water_type")) %>%
      select(-plant_type,-technology,-cooling_system,-water_type,) %>%
      rename(supplysector = sector, subsector0 = fuel,  technology=to.technology, region=state) %>%
      ungroup()->
      L1233.out_EJ_state_elec_F_tech_cool


    L2234.StubTechProd_elecS_USA %>% rename(tech.share.weight=share.weight) %>%
      coal_stub_filter() %>%
      bind_rows(L2246.StubTechProd_coal_vintage_USA,
                L2240.StubTechProd_elec_coalret_USA) %>%
      add_cooling_techs() ->
      L2233.StubTechProd_elec_USA

    L2233.StubTechProd_elec_USA %>%
      filter(calOutputValue > 0) %>%
      left_join(L1233.out_EJ_state_elec_F_tech_cool,
                by=c("supplysector","subsector0","subsector","technology","region","year")) %>%
      replace_na(list(share=0)) %>%
      group_by(supplysector, subsector0, subsector, region, year) %>%
      mutate(calOutputValue = if_else(grepl("CSP",subsector),calOutputValue/2,
                                      if_else(subsector0=="wind"|grepl("PV",subsector), calOutputValue,calOutputValue*share))) %>% replace_na(list(calOutputValue=0)) %>%
      ## divide by 2 to account for recirculating and dry_cooling technology types in CSP. This will need to be addressed later
      select(-value, -share) %>%
      ungroup() %>%
      bind_rows(L2233.StubTechProd_elec_USA %>% filter(calOutputValue == 0)) %>%
      mutate(technology = if_else(grepl("wind_base",subsector),subsector,technology),
             subs.share.weight = if_else(grepl("offshore",subsector),subs.share.weight,tech.share.weight),
             tech.share.weight = if_else(calOutputValue == 0,0,tech.share.weight)) %>%
      arrange(region,supplysector,subsector0,year) ->
      L2233.StubTechProd_elec_USA

    L2233.StubTechProd_elec_USA %>%
      select(-technology, -tech.share.weight,-share.weight.year) %>%
      rename(share.weight=subs.share.weight) %>%
      unique() %>%
      select(-calOutputValue) ->
      L2233.SubsectorShrwt_elecS_cool_USA


    # State level s-curves, combining vintages and initial slow and fast retire plants
    bind_rows(L2246.StubTechSCurve_coal_vintage_USA,
              L2240.StubTechSCurve_elec_coalret_USA) %>%
      add_cooling_techs() ->
      L2233.StubTechSCurve_elec_USA

    L2246.StubTechProfitShutdown_coal_vintage_USA %>%
      add_cooling_techs() ->
      L2233.StubTechProfitShutdown_elecS_cool_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      add_cooling_techs() ->
      L2233.StubTechCapFactor_elecS_solar_USA


    L2234.StubTechCapFactor_elecS_wind_USA %>%
      add_cooling_techs()%>%
      mutate(technology = if_else(grepl("wind_base",subsector),subsector,technology))->
      L2233.StubTechCapFactor_elecS_wind_USA

    L2234.StubTechElecMarket_backup_elecS_USA %>%
      add_cooling_techs() %>%
      mutate( technology = ifelse(subsector=="rooftop_pv"|grepl("wind_base",subsector),subsector,technology)) ->
      L2233.StubTechElecMarket_backup_elecS_USA

    L2234.StubTechFixOut_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechFixOut_elecS_USA

    L2234.StubTechFixOut_hydro_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechFixOut_hydro_elecS_USA

    L2234.StubTechMarket_backup_elecS_USA %>%
      add_cooling_techs()%>%
      mutate( technology = ifelse(subsector=="rooftop_pv"|grepl("wind_base",subsector),subsector,technology))  ->
      L2233.StubTechMarket_backup_elecS_USA

    L2234.StubTechCost_offshore_wind_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechCost_offshore_wind_elecS_USA

    L2246.StubTechLifetime_coal_vintage_USA %>%
      add_cooling_techs() %>%
      arrange(supplysector,year)->
      L2233.StubTechLifetime_elecS_cool_USA

    L2234.SubsectorLogit_elecS_USA %>%
      rename(subsector0=subsector) %>%
      mutate(subsector.1 = subsector0)%>%
      select(-subsector.1) %>%
      arrange(supplysector)->
      L2233.SubsectorLogit_elecS_USA

    L2234.SubsectorLogit_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                by=c("supplysector"="Electric.sector","subsector")) %>%
      rename(subsector0=subsector,
             subsector=Electric.sector.technology)%>%
      right_join(
        bind_rows(L2233.StubTechMarket_elec_USA %>% select(subsector,region,technology),
                  L2233.StubTechFixOut_hydro_elecS_USA %>% select(subsector,region,technology),
                  L2233.StubTechEff_elec_USA %>% filter(subsector=="battery") %>% select(subsector,region,technology)),
        ## Including all possible technologies here
                 by=c("subsector","region","to.technology"="technology"))%>%
      select(-subsector_1,-technology, -to.technology, region,supplysector,subsector0,subsector, logit.year.fillout, logit.exponent,logit.type)%>%
      # Acknowledge that not all vintages are in each state, therefore shareweights and logits are
      # not needed for these vintages
      unique() %>%
      arrange(supplysector) %>% select(-logit.year.fillout, -logit.exponent, -logit.type, dplyr::everything())  ->
      L2233.SubsectorLogit_elecS_cool_USA

    L2233.GlobalTechCoef_elecS_cool_USA %>%
      right_join(L2233.SubsectorLogit_elecS_cool_USA %>% select(region,supplysector,subsector0,subsector),
                by=c("supplysector","subsector0","subsector")) %>%
    mutate(market.name = if_else(minicam.energy.input=="seawater",gcam.USA_REGION,region)) %>%
      na.omit() -> L2233.StubTechCoef_elecS_cool_USA

    L2233.StubTechCoef_elecS_cool_USA %>%
      filter(grepl("seawater",technology)) %>% filter(!(region %in% gcamusa.NO_SEAWATER_STATES)) %>%
      bind_rows(L2233.StubTechCoef_elecS_cool_USA %>% filter(!grepl("seawater",technology))) %>%
      arrange(region,year)->
      ## Not all techs have cooling options, therefore we remove these
      L2233.StubTechCoef_elecS_cool_USA

    L2234.SubsectorShrwt_elecS_USA %>%
      rename(subsector0=subsector) %>%
      bind_rows(
        L2234.StubTechProd_elecS_USA %>% select(region, supplysector, subsector, year, subs.share.weight) %>%
          rename(share.weight= subs.share.weight, subsector0=subsector) %>% unique(),
        L2234.StubTechFixOut_elecS_USA%>% select(region, supplysector, subsector, year, subs.share.weight) %>%
          rename(share.weight= subs.share.weight, subsector0=subsector) %>% unique()
      ) %>%
      arrange(region,supplysector,year)->
      L2233.SubsectorShrwt_elecS_USA


    L2234.SubsectorShrwtInterp_elecS_USA %>%
      rename(subsector0=subsector)%>%
      mutate(subsector.1 = subsector0)%>%
      select(-subsector.1) %>%
      arrange(supplysector) ->
      L2233.SubsectorShrwtInterp_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      rename(subsector0=subsector)%>%
      mutate(subsector.1 = subsector0)%>%
      select(-subsector.1) %>%
      arrange(supplysector) ->
      L2233.SubsectorShrwtInterpTo_elecS_USA

      L2233.SubsectorShrwtInterpTo_elecS_USA %>%
      left_join(L2233.SubsectorLogit_elecS_cool_USA %>% select(-logit.year.fillout, -logit.exponent, -logit.type), by= c("region","supplysector","subsector0")
      ) %>% mutate(to.value = if_else(from.year>=2015&subsector=="nuc_base_Gen II",0,to.value))->
        #eliminate future nuclear Gen II from falsely being interpolated past 2030
      L2233.SubsectorShrwtInterpTo_elecS_cool_USA

      ## Add subsector shareweights in future periods to allow model to interpolate from 2015.
         L2233.SubsectorShrwt_elecS_cool_USA %>%
           bind_rows(L2233.SubsectorShrwtInterpTo_elecS_cool_USA %>%
                       mutate(year = if_else(interpolation.function=="linear",to.year,from.year),
                              share.weight = to.value) %>% select(-apply.to, -from.year, -to.year, -to.value, -interpolation.function)
                     ) %>%
           arrange(region,supplysector,year) %>% unique() ->
           L2233.SubsectorShrwt_elecS_cool_USA

    ## We want to do a couple of manipulations to make sure that
    ## future cooling technologies are in line with state-level historical representations
    ## First, we set an assumption that if the power plant exists in the historical period,
    ## the cooling technology share weights will continue into the future
    ## Second, if that fuel type did not exist in the historical period (i.e. solar),
    ## then the share weights for all available cooling technologies will be set to 1
    ## Finally, if a fuel and power plant combination did exist in the historical period,
    ## but switches to a new power plant type (i.e. Nuclear Gen 2 -> Gen 3), we assume
    ## that share weights of cooling technologies for that particular state remain similar
    ## to the old power plant (e.g. Gen 3 will have the same cooling tech share weights as Gen 2)
    L2233.StubTechProd_elec_USA %>% dplyr::filter(year==max(MODEL_BASE_YEARS)) %>%
      select(-share.weight.year,-calOutputValue) %>%
      left_join(L2233.SubsectorShrwtInterpTo_elecS_cool_USA %>%
                  rename(share.weight=to.value) %>%
                  group_by(region,supplysector,subsector0,subsector)  %>%
                  ungroup(), by=c("region","supplysector","subsector0","subsector")) %>%
      unique() %>%
      group_by(region,supplysector, subsector0,subsector) %>%
      mutate(share.weight.sum = sum(tech.share.weight)) %>%
      ungroup() %>%
      left_join(elec_tech_water_map %>%
                  select(cooling_system,to.technology), by=c("technology"="to.technology")) %>%
      left_join(
        bind_rows(L2233.GlobalTechShrwt_elec_cool_USA,
                  L2233.GlobalIntTechShrwt_elecS_cool_USA %>% rename(technology=intermittent.technology) %>%
                  mutate(share.weight = if_else(subsector.name=="wind_base_offshore",1,share.weight))
                  ##Fix to make sure that the share weight of offshore wind is properly handled and carried through the rest of the script
                  ) %>% rename(global.share.weight=share.weight, supplysector=sector.name, subsector0=subsector.name0, subsector=subsector.name), by=c("supplysector","subsector0","subsector","technology","year")
        )->
      L2233.StubTechShrwt_elecS_cool_USA

    #Initially remove all technologies that are not present in future periods so that there
    #are no interpolation rules applied to these technologies
    L2233.StubTechShrwt_elecS_cool_USA %>%
      na.omit() %>%
      filter(subs.share.weight>0) %>%
      mutate(state.cooling.share.weight = if_else(share.weight>0&share.weight.sum>0,tech.share.weight,
                                                                            if_else(share.weight==0&tech.share.weight==0,0,1))) %>%
      select(region,supplysector,subsector0,cooling_system,from.year,state.cooling.share.weight) %>%
      unique() %>%
      group_by(region,supplysector,subsector0,cooling_system,from.year) %>%
      summarise(state.cooling.share.weight= sum(state.cooling.share.weight))%>%
      ungroup() %>%
      mutate(state.cooling.share.weight = if_else(state.cooling.share.weight>1,1,state.cooling.share.weight)) ->
      L2233.StubTechShrwt_state_elecS_cool_USA

    L2233.StubTechShrwt_elecS_cool_USA %>%
      filter(global.share.weight>0) %>%
      left_join(L2233.StubTechShrwt_state_elecS_cool_USA, by=c("region","supplysector","subsector0","from.year","cooling_system")) %>%
      mutate(updated.share.weight = if_else(share.weight>0&share.weight.sum>0,tech.share.weight,
                                            if_else(share.weight==0,0,
                                                    if_else(share.weight>0&share.weight.sum==0&!is.na(state.cooling.share.weight),state.cooling.share.weight,
                                                            if_else(share.weight>0&share.weight.sum==0&subs.share.weight>0&is.na(state.cooling.share.weight)&!grepl("offshore",subsector),0,1)))),
             interpolation.function = if_else(share.weight.sum>0&from.year == max(MODEL_BASE_YEARS)&share.weight==1,"fixed",interpolation.function),
             from.year = if_else(from.year==2015&to.year==2100&share.weight.sum>1&interpolation.function=="fixed",2010,as.double(from.year))) %>%
      select(-subs.share.weight, -tech.share.weight, -share.weight, -share.weight.sum, -year,  -cooling_system) %>%
      filter(from.year!=to.year) %>%
      select(-state.cooling.share.weight, -global.share.weight) %>% unique() %>%
      rename(to.value=updated.share.weight, stub.technology=technology)->
      L2233.StubTechInterp_elecS_cool_USA

    L2233.StubTechInterp_elecS_cool_USA %>%
      select(-interpolation.function,-apply.to) %>%
      filter(from.year>=2015) %>%
      mutate(year = if_else(from.year==2015,as.double(from.year),as.double(to.year))) %>%
      bind_rows(L2233.StubTechInterp_elecS_cool_USA %>%
                  filter(interpolation.function=="linear") %>%
                  select(-interpolation.function,-apply.to) %>%
                  mutate(year = to.year)

      ) %>% select(-to.year, -from.year) %>%
      unique() %>%
      rename(share.weight=to.value)->
      L2233.StubTechShrwt_elecS_cool_USA

    # Only done to manipulate variable and not throw an error
    L2234.Supplysector_elecS_USA %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1)->
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

    L2233.GlobalTechCoef_elecS_cool_USA %>%
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

    L2233.GlobalIntTechCoef_elecS_cool_USA %>%
    add_title("Water demand coefs for int techs") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.GlobalIntTechCoef_elec_cool") %>%
      add_precursors("L2233.GlobalIntTechCoef_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
    L2233.GlobalIntTechCoef_elecS_cool_USA

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

    L2233.StubTechCoef_elecS_cool_USA %>%
      add_title("Water demand coefficients at state level") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechCoef_elec") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCoef_elecS_cool_USA

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
                     "gcam-usa/elec_tech_water_map") ->
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

    L2233.StubTechProfitShutdown_elecS_cool_USA %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechSCurve_elec") %>%
      add_precursors("L2246.StubTechProfitShutdown_coal_vintage_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechProfitShutdown_elecS_cool_USA

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

    L2233.StubTechCost_offshore_wind_elecS_USA %>%
      add_title("Electricity Load Segments Offshore Wind Cost Adjustment") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechCost_offshore_wind_elec_USA") %>%
      add_precursors("L2234.StubTechCost_offshore_wind_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCost_offshore_wind_elecS_cool_USA

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

    L2233.StubTechShrwt_elecS_cool_USA %>%
      add_title("Electricity Load Segments Stub Tech Shareweights") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechShrwt_elec") %>%
      add_precursors("L2234.StubTechProd_elecS_USA",
                     "L2234.SubsectorShrwtInterpTo_elecS_USA") ->
      L2233.StubTechShrwt_elecS_cool_USA

    L2233.StubTechInterp_elecS_cool_USA %>%
      add_title("Electricity Load Segments Stub Tech Interpolation rules") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
      add_legacy_name("L2233.StubTechShrwt_elec") %>%
      add_precursors("L2234.StubTechProd_elecS_USA",
                     "L2234.SubsectorShrwtInterpTo_elecS_USA") ->
      L2233.StubTechInterp_elecS_cool_USA

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
      add_precursors("L2234.StubTechProd_elecS_USA") ->
    L2233.SubsectorShrwt_elecS_cool_USA

     L2233.SubsectorShrwtInterp_elecS_USA %>%
         add_title("Electricity Load Segments Nesting-Subsector Shareweights") %>%
         add_units("none") %>%
         add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
         add_legacy_name("L2233.SubsectorShrwtInterp_elec") %>%
         add_precursors("L2234.SubsectorShrwtInterp_elecS_USA") ->
         L2233.SubsectorShrwtInterp_elecS_USA

       #L2233.SubsectorShrwtInterp_elecS_cool_USA %>%
         #add_title("Electricity Load Segments Subsector Shareweights") %>%
         #add_units("none") %>%
         #add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
         #add_legacy_name("L2233.SubsectorShrwtInterp_elec") %>%
         #add_precursors("L2234.SubsectorShrwtInterp_elecS_USA") ->
         #L2233.SubsectorShrwtInterp_elecS_cool_USA

       L2233.SubsectorShrwtInterpTo_elecS_USA %>%
         add_title("Electricity Load Segments Nesting-Subsector Shareweights") %>%
         add_units("none") %>%
         add_comments("Generated using zchunk_LA2233.elec_segments_water_USA") %>%
         add_legacy_name("L2233.SubsectorShrwtInterpTo_elec") %>%
         add_precursors("L2234.SubsectorShrwtInterpTo_elecS_USA") ->
         L2233.SubsectorShrwtInterpTo_elecS_USA

       L2233.SubsectorShrwtInterpTo_elecS_cool_USA %>%
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
                    L2233.GlobalIntTechCoef_elecS_cool_USA,
                    L2233.PrimaryRenewKeyword_elecS_cool_USA,
                    L2233.PrimaryRenewKeywordInt_elecS_cool_USA,
                    L2233.StubTechEff_elecS_cool_USA,
                    L2233.StubTechCoef_elecS_cool_USA,
                    L2233.StubTechMarket_elecS_cool_USA,
                    L2233.StubTechProd_elecS_cool_USA,
                    L2233.StubTechSCurve_elecS_cool_USA,
                    L2233.StubTechCapFactor_elecS_solar_USA,
                    L2233.StubTechCapFactor_elecS_wind_USA,
                    L2233.StubTechElecMarket_backup_elecS_cool_USA,
                    L2233.StubTechFixOut_elecS_cool_USA,
                    L2233.StubTechFixOut_hydro_elecS_cool_USA,
                    L2233.StubTechMarket_backup_elecS_cool_USA,
                    L2233.StubTechProfitShutdown_elecS_cool_USA,
                    L2233.StubTechLifetime_elecS_cool_USA,
                    L2233.StubTechShrwt_elecS_cool_USA,
                    L2233.StubTechInterp_elecS_cool_USA,
                    L2233.SubsectorLogit_elecS_USA,
                    L2233.SubsectorLogit_elecS_cool_USA,
                    L2233.SubsectorShrwt_elecS_USA,
                    L2233.SubsectorShrwt_elecS_cool_USA,
                    L2233.SubsectorShrwtInterp_elecS_USA,
                    L2233.StubTechCost_offshore_wind_elecS_cool_USA,
                    L2233.SubsectorShrwtInterpTo_elecS_USA,
                    #L2233.SubsectorShrwtInterp_elecS_cool_USA,
                    L2233.SubsectorShrwtInterpTo_elecS_cool_USA,
                    L2233.Supplysector_elecS_cool_USA)
      } else {
        stop("Unknown command")
      }
    }
