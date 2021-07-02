# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA2233.elec_segments_water_USA
#'
#' Generates GCAM-USA model inputs for multiple load segments electricity sector and cooling systems by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.GlobalTechEff_elecS_cool_USA}, \code{L2233.GlobalTechShrwt_elecS_cool_USA}, \code{L2233.GlobalTechProfitShutdown_elecS_cool_USA},
#' \code{L2233.GlobalTechOMvar_elecS_cool_USA}, \code{L2233.GlobalTechOMfixed_elecS_cool_USA}, \code{L2233.GlobalTechCapital_elecS_USA}, \code{L2233.GlobalTechCapital_elecS_cool_USA},
#' \code{L2233.GlobalTechCapFac_elecS_cool_USA}, \code{L2233.GlobalTechSCurve_elecS_cool_USA}, \code{L2233.GlobalTechCoef_elecS_cool_USA}, \code{L2233.GlobalTechCapture_elecS_cool_USA},
#' \code{L2233.GlobalTechLifetime_elecS_cool_USA}, \code{L2233.AvgFossilEffKeyword_elecS_cool_USA}, \code{L2233.GlobalIntTechBackup_elecS_cool_USA}, \code{L2233.GlobalIntTechCapital_elecS_USA},
#' \code{L2233.GlobalIntTechCapital_elecS_cool_USA}, \code{L2233.GlobalIntTechEff_elecS_USA}, \code{L2233.GlobalIntTechEff_elecS_cool_USA}, \code{L2233.GlobalIntTechLifetime_elecS_cool_USA},
#' \code{L2233.GlobalIntTechOMfixed_elecS_cool_USA}, \code{L2233.GlobalIntTechOMvar_elecS_cool_USA}, \code{L2233.GlobalIntTechShrwt_elecS_cool_USA}, \code{L2233.GlobalIntTechCoef_elecS_cool_USA},
#' \code{L2233.PrimaryRenewKeyword_elecS_cool_USA}, \code{L2233.PrimaryRenewKeywordInt_elecS_cool_USA}, \code{L2233.StubTechEff_elecS_cool_USA}, \code{L2233.StubTechCoef_elecS_cool_USA},
#' \code{L2233.StubTechMarket_elecS_cool_USA}, \code{L2233.StubTechProd_elecS_cool_USA}, \code{L2233.StubTechSCurve_elecS_cool_USA}, \code{L2233.StubTechCapFactor_elecS_solar_USA},
#' \code{L2233.StubTechCapFactor_elecS_wind_USA}, \code{L2233.StubTechElecMarket_backup_elecS_cool_USA}, \code{L2233.StubTechFixOut_elecS_cool_USA}, \code{L2233.StubTechFixOut_hydro_elecS_cool_USA},
#' \code{L2233.StubTechMarket_backup_elecS_cool_USA}, \code{L2233.StubTechProfitShutdown_elecS_cool_USA}, \code{L2233.StubTechShrwt_elecS_cool_USA}, \code{L2233.StubTechInterp_elecS_cool_USA},
#' \code{L2233.StubTechCost_offshore_wind_elecS_cool_USA}, \code{L2233.SubsectorLogit_elecS_USA}, \code{L2233.SubsectorLogit_elecS_cool_USA}, \code{L2233.SubsectorShrwt_elecS_USA},
#' \code{L2233.SubsectorShrwt_elecS_cool_USA}, \code{L2233.SubsectorShrwtInterp_elecS_USA}, \code{L2233.SubsectorShrwtInterpTo_elecS_USA}, \code{L2233.Supplysector_elecS_cool_USA}.
#' The corresponding file in the original data system was \code{LA2233.electricity_water_USA} (gcam-usa level2)
#' @details This chunk generates input files to create an electricity generation sector with multiple load segments
#' for each state and creates the demand by load, fuel, power plant, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter if_else mutate select semi_join summarise_if bind_rows
#' @importFrom tidyr complete nesting replace_na
#' @author NTG May 2020
module_gcamusa_LA2233.elec_segments_water_USA <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/elec_tech_water_map",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool_shares_fut",
             FILE = "gcam-usa/usa_seawater_states_basins",
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
             "L2241.GlobalTechCapFac_elec_coalret_USA",
             "L2241.GlobalTechCapital_elec_coalret_USA",
             "L2241.GlobalTechEff_elec_coalret_USA",
             "L2241.GlobalTechOMfixed_elec_coalret_USA",
             "L2241.GlobalTechOMvar_elec_coalret_USA",
             "L2241.GlobalTechProfitShutdown_elec_coalret_USA",
             "L2241.GlobalTechShrwt_elec_coalret_USA",
             "L2241.StubTechEff_elec_coalret_USA",
             "L2241.StubTechMarket_elec_coalret_USA",
             "L2241.StubTechProd_elec_coalret_USA",
             "L2241.StubTechSCurve_elec_coalret_USA",
             "L2241.GlobalTechCapFac_coal_vintage_USA",
             "L2241.GlobalTechCapital_coal_vintage_USA",
             "L2241.GlobalTechEff_coal_vintage_USA",
             "L2241.GlobalTechOMfixed_coal_vintage_USA",
             "L2241.GlobalTechOMvar_coal_vintage_USA",
             "L2241.GlobalTechShrwt_coal_vintage_USA",
             "L2241.StubTechEff_coal_vintage_USA",
             "L2241.StubTechMarket_coal_vintage_USA",
             "L2241.StubTechProd_coal_vintage_USA",
             "L2241.StubTechProfitShutdown_coal_vintage_USA",
             "L2241.StubTechSCurve_coal_vintage_USA"))
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
             "L2233.StubTechShrwt_elecS_cool_USA",
             "L2233.StubTechInterp_elecS_cool_USA",
             "L2233.StubTechCost_offshore_wind_elecS_cool_USA",
             "L2233.SubsectorLogit_elecS_USA",
             "L2233.SubsectorLogit_elecS_cool_USA",
             "L2233.SubsectorShrwt_elecS_USA",
             "L2233.SubsectorShrwt_elecS_cool_USA",
             "L2233.SubsectorShrwtInterp_elecS_USA",
             "L2233.SubsectorShrwtInterpTo_elecS_USA",
             "L2233.Supplysector_elecS_cool_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region<- supplysector <- subsector <-  technology <- year <- minicam.energy.input <-
      coefficient<- market.name <- Electric.sector <- Electric.sector.intermittent.technology <-
      intermittent.technology <- subsector_1 <- sector <- fuel <- Electric.sector.technology <-
      water_sector <- water_type <- state <- state_name <- ':=' <- x <- plant_type <- State <-
      value <- Basin_name <- Electric.sector <- Electric.sector.technology <-
      apply.to <- GCAM_basin_ID <- basin_name <- calOutputValue <- capital.overnight <-
      cooling_system <- deflator <- efficiency <- fixed.charge.rate <- from.subsector <-
      from.supplysector <- from.technology <- from.year <- global.share.weight <-
      input.capital <- input.cost <- input.unit <- intensity_CORE <-
      intermittent.technology <- interpolation.function <- loadfactor_CORE <-
      logit.exponent <- logit.type <- logit.year.fillout <- median.shutdown.point <-
      non_fuel_cost_core <- out_MWh.sea <- out_MWh_sea <- output.unit <-
      passthrough.sector <- plant_type <- price.unit <- region <- sce <- sector.name <- share <-
      share.weight <- share.weight.sum <- share.weight.year <- share_nat.sea <- state <-
      state.cooling.share.weight <- state.to.country.share <- state_abbr <-
      stub.technology <- subs.share.weight <- subsector <- subsector.1 <-
      subsector.name <- subsector.name0 <- subsector0 <- subsector_1 <- supplysector <-
      supplysector.1 <- tag <- tech.share.weight <- technology <- technology.x <-
      technology.y <- to.subsector <- to.supplysector <- to.technology <- to.value <-
      to.year <- updated.share.weight <- value <- volume <- water_sector <- water_type <-
      weight_EJ_core <- wt_short <- year <- GLU_name <- future.subs.shrwt <- hist.share <- NULL

    # ===================================================
    # 1. Read files

    # Load required inputs
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool", strip_attributes = TRUE)
    elec_tech_water_map <- get_data(all_data, "gcam-usa/elec_tech_water_map", strip_attributes = TRUE)
    A23.elecS_tech_mapping_cool_shares_fut <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool_shares_fut", strip_attributes = TRUE)
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins", strip_attributes = TRUE)
    L1233.out_EJ_state_elec_F_tech_cool <- get_data(all_data, "L1233.out_EJ_state_elec_F_tech_cool", strip_attributes = TRUE)
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.GlobalTechShrwt_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.GlobalTechProfitShutdown_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalTechCapital_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCapital_elecPassthru <- get_data(all_data, "L2233.GlobalTechCapital_elecPassthru", strip_attributes = TRUE)
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCapital_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechEff_elec_cool <- get_data(all_data, "L2233.GlobalIntTechEff_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCoef_elec_cool", strip_attributes = TRUE)
    L2234.AvgFossilEffKeyword_elecS_USA <- get_data(all_data, "L2234.AvgFossilEffKeyword_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechBackup_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechBackup_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechCapital_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechEff_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechEff_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechLifetime_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechLifetime_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechOMfixed_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechOMfixed_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechOMvar_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechOMvar_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechShrwt_elecS_USA <- get_data(all_data,"L2234.GlobalIntTechShrwt_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechCapFac_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapFac_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapital_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechCapture_elecS_USA <- get_data(all_data,"L2234.GlobalTechCapture_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechEff_elecS_USA <- get_data(all_data,"L2234.GlobalTechEff_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechLifetime_elecS_USA <- get_data(all_data,"L2234.GlobalTechLifetime_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechOMfixed_elecS_USA <- get_data(all_data,"L2234.GlobalTechOMfixed_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechOMvar_elecS_USA <- get_data(all_data,"L2234.GlobalTechOMvar_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechProfitShutdown_elecS_USA <- get_data(all_data,"L2234.GlobalTechProfitShutdown_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechSCurve_elecS_USA <- get_data(all_data,"L2234.GlobalTechSCurve_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalTechShrwt_elecS_USA <- get_data(all_data,"L2234.GlobalTechShrwt_elecS_USA", strip_attributes = TRUE)
    L2234.PrimaryRenewKeyword_elecS_USA <- get_data(all_data,"L2234.PrimaryRenewKeyword_elecS_USA", strip_attributes = TRUE)
    L2234.PrimaryRenewKeywordInt_elecS_USA <- get_data(all_data,"L2234.PrimaryRenewKeywordInt_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechCapFactor_elecS_solar_USA <- get_data(all_data,"L2234.StubTechCapFactor_elecS_solar_USA", strip_attributes = TRUE)
    L2234.StubTechCapFactor_elecS_wind_USA <- get_data(all_data,"L2234.StubTechCapFactor_elecS_wind_USA", strip_attributes = TRUE)
    L2234.StubTechEff_elecS_USA <- get_data(all_data,"L2234.StubTechEff_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechElecMarket_backup_elecS_USA <- get_data(all_data,"L2234.StubTechElecMarket_backup_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechFixOut_elecS_USA <- get_data(all_data,"L2234.StubTechFixOut_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechFixOut_hydro_elecS_USA <- get_data(all_data,"L2234.StubTechFixOut_hydro_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechMarket_backup_elecS_USA <- get_data(all_data,"L2234.StubTechMarket_backup_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_USA <- get_data(all_data,"L2234.StubTechMarket_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechProd_elecS_USA <- get_data(all_data,"L2234.StubTechProd_elecS_USA", strip_attributes = TRUE)
    L2234.SubsectorLogit_elecS_USA <- get_data(all_data,"L2234.SubsectorLogit_elecS_USA", strip_attributes = TRUE)
    L2234.SubsectorShrwt_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwt_elecS_USA", strip_attributes = TRUE)
    L2234.SubsectorShrwtInterp_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwtInterp_elecS_USA", strip_attributes = TRUE)
    L2234.SubsectorShrwtInterpTo_elecS_USA <- get_data(all_data,"L2234.SubsectorShrwtInterpTo_elecS_USA", strip_attributes = TRUE)
    L2234.Supplysector_elecS_USA <- get_data(all_data,"L2234.Supplysector_elecS_USA", strip_attributes = TRUE)
    L2234.StubTechCost_offshore_wind_elecS_USA <- get_data(all_data,"L2234.StubTechCost_offshore_wind_elecS_USA", strip_attributes = TRUE)
    L2241.GlobalTechCapFac_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechCapFac_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechCapital_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechCapital_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechEff_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechEff_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechOMfixed_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechOMfixed_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechOMvar_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechOMvar_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechProfitShutdown_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechProfitShutdown_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechShrwt_elec_coalret_USA <- get_data(all_data,"L2241.GlobalTechShrwt_elec_coalret_USA", strip_attributes = TRUE)
    L2241.StubTechEff_elec_coalret_USA <- get_data(all_data,"L2241.StubTechEff_elec_coalret_USA", strip_attributes = TRUE)
    L2241.StubTechMarket_elec_coalret_USA <- get_data(all_data,"L2241.StubTechMarket_elec_coalret_USA", strip_attributes = TRUE)
    L2241.StubTechProd_elec_coalret_USA <- get_data(all_data,"L2241.StubTechProd_elec_coalret_USA", strip_attributes = TRUE)
    L2241.StubTechSCurve_elec_coalret_USA <- get_data(all_data,"L2241.StubTechSCurve_elec_coalret_USA", strip_attributes = TRUE)
    L2241.GlobalTechCapFac_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechCapFac_coal_vintage_USA", strip_attributes = TRUE)
    L2241.GlobalTechCapital_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechCapital_coal_vintage_USA", strip_attributes = TRUE)
    L2241.GlobalTechEff_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechEff_coal_vintage_USA", strip_attributes = TRUE)
    L2241.GlobalTechOMfixed_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechOMfixed_coal_vintage_USA", strip_attributes = TRUE)
    L2241.GlobalTechOMvar_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechOMvar_coal_vintage_USA", strip_attributes = TRUE)
    L2241.GlobalTechShrwt_coal_vintage_USA <- get_data(all_data,"L2241.GlobalTechShrwt_coal_vintage_USA", strip_attributes = TRUE)
    L2241.StubTechEff_coal_vintage_USA <- get_data(all_data,"L2241.StubTechEff_coal_vintage_USA", strip_attributes = TRUE)
    L2241.StubTechMarket_coal_vintage_USA <- get_data(all_data,"L2241.StubTechMarket_coal_vintage_USA", strip_attributes = TRUE)
    L2241.StubTechProd_coal_vintage_USA <- get_data(all_data,"L2241.StubTechProd_coal_vintage_USA", strip_attributes = TRUE)
    L2241.StubTechProfitShutdown_coal_vintage_USA <- get_data(all_data,"L2241.StubTechProfitShutdown_coal_vintage_USA", strip_attributes = TRUE)
    L2241.StubTechSCurve_coal_vintage_USA <- get_data(all_data,"L2241.StubTechSCurve_coal_vintage_USA", strip_attributes = TRUE)


    # ===================================================
    # Define unique states and basins that have access to seawater that will
    # allow for seawate cooling

    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    # Process data

    A23.elecS_tech_mapping_cool %>%
      select(-supplysector, -water_type, -cooling_system, -plant_type) ->
      A23.elecS_tech_mapping_cool

    ## Define several filtering and renaming fuctions that will be applied
    ## throughout the zchunk

    # To account for new nesting-subsector structure and to add cooling technologies,
    # we must expand certain outputs
    add_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  # Left_join used as each power plant type will now be multiplied
                  # by up to five cooling technologies, thus increasing tibble size
                  by = c("stub.technology" = "Electric.sector.technology",
                         "supplysector" = "Electric.sector","subsector")) %>%
        select(-technology, -subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology)  -> data_new

      data_new %>%
        filter(grepl(gcamusa.WATER_TYPE_SEAWATER,technology),
               (region %in% seawater_states_basins)) %>%
        bind_rows(data_new %>%
                    filter(!grepl(gcamusa.WATER_TYPE_SEAWATER,technology))) %>%
        arrange(region,year)  -> data_new
      return(data_new)
    }

    add_global_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  # Left_join used as each power plant type will now be multiplied
                  # by up to five cooling technologies, thus increasing tibble size
                  by = c("technology" = "Electric.sector.technology",
                         "supplysector" = "Electric.sector",
                         "subsector")) %>%
        select(-subsector_1, -technology.y) %>%
        rename(sector.name = supplysector,
               subsector.name0 = subsector,
               subsector.name = technology,
               technology = to.technology) %>%
        mutate(technology = if_else(subsector.name0 == "wind" | subsector.name0 == "solar", subsector.name,
                                    if_else(subsector.name0 == "grid_storage", subsector.name0, technology))) ->
        data_new
      return(data_new)
    }

    add_int_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  # Left_join used as each power plant type will now be multiplied
                  # by up to five cooling technologies, thus increasing tibble size
                  by = c("supplysector" = "Electric.sector",
                         "intermittent.technology" = "Electric.sector.technology",
                         "subsector")) %>%
        select(-technology, -subsector_1)%>%
        rename(subsector0 = subsector,
               subsector = intermittent.technology,
               intermittent.technology = to.technology) ->
        data_new
      return(data_new)
    }


    # Here we will consolidate coal retire + coal vintages + other electricity subsectors that are
    # currently located in separate files
    # This also adds each cooling technology and trasfers coefficients calculated for cooling techs if needed
    # All variables here are global
    L2234.GlobalTechEff_elecS_USA %>%
      bind_rows((L2241.GlobalTechEff_coal_vintage_USA %>%
                   rename(supplysector = sector.name,
                          subsector = subsector.name)),
                L2241.GlobalTechEff_elec_coalret_USA)  %>%
      add_global_cooling_techs() %>%
      select(-efficiency) %>%
      # Bring in global cooling tech efficiencies from GCAM-core
      left_join_keep_first_only(L2233.GlobalTechEff_elec_cool %>%
                                  mutate(technology = gsub("_storage.*", "_base_storage", technology)) %>%
                                  select(technology, efficiency,year),
                                by = c("technology", "year")) %>%
      arrange(sector.name,year)->
      L2233.GlobalTechEff_elec_cool_USA

    L2234.GlobalTechShrwt_elecS_USA %>%
      bind_rows(L2241.GlobalTechShrwt_coal_vintage_USA,
                L2241.GlobalTechShrwt_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                # Left_join used as each power plant type will now be multiplied
                # by up to five cooling technologies, thus increasing tibble size
                by = c("technology" = "Electric.sector.technology",
                       "sector.name" = "Electric.sector",
                       "subsector.name" = "subsector")) %>%
      select(-subsector_1, -technology.y) %>%
      rename(subsector.name0 = subsector.name, subsector.name = technology, technology = to.technology) %>%
      mutate(technology = if_else(subsector.name0 == "wind" | subsector.name0 == "solar", subsector.name,
                                  if_else(subsector.name0 == "grid_storage", subsector.name0, technology))) %>%
      arrange(sector.name,year) ->
      L2233.GlobalTechShrwt_elec_cool_USA

    # Add all vintages to profit shutdown tibbles as they initially exist at state level, not global
    L2234.GlobalTechProfitShutdown_elecS_USA %>%
      bind_rows(L2241.GlobalTechProfitShutdown_elec_coalret_USA) %>%
      left_join(A23.elecS_tech_mapping_cool,
                # Left_join used as each power plant type will now be multiplied
                # by up to five cooling technologies, thus increasing tibble size
                by = c("technology" = "Electric.sector.technology",
                       "supplysector" = "Electric.sector",
                       "subsector")) %>%
      select(-subsector_1, -median.shutdown.point, -technology.y) %>%
      rename(subsector0 = subsector, subsector = technology, technology = to.technology)%>%
      left_join_keep_first_only(L2233.GlobalTechProfitShutdown_elec_cool %>%
                                  ## Keep_first_only is used to avoid unnessary
                                  ## multiplication of rows. We only want to bring
                                  ## in the global profit shutdowns for the technologies
                                  ## defined
                                  select(technology, median.shutdown.point),
                                by=c("technology")) %>%
      arrange(supplysector, year) %>%
      mutate(technology = if_else(subsector0 == "wind" | subsector0 == "solar", subsector,
                                  if_else(subsector0 == "grid_storage", subsector0, technology))) ->
      L2233.GlobalTechProfitShutdown_elec_cool_USA

    L2234.GlobalTechOMvar_elecS_USA %>%
      bind_rows(L2241.GlobalTechOMvar_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2241.GlobalTechOMvar_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name, year) ->
      L2233.GlobalTechOMvar_elec_USA

    L2234.GlobalTechOMfixed_elecS_USA %>%
      bind_rows(L2241.GlobalTechOMfixed_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2241.GlobalTechOMfixed_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name, year) ->
      L2233.GlobalTechOMfixed_elec_USA

    L2234.GlobalTechCapital_elecS_USA %>%
      bind_rows(L2241.GlobalTechCapital_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2241.GlobalTechCapital_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name, year) ->
      L2233.GlobalTechCapital_elec_USA

    L2233.GlobalTechCapital_elec_USA %>%
      select(-input.capital, -capital.overnight, -fixed.charge.rate) %>%
      left_join_keep_first_only(
        ## keep first only allows for capital costs to be pulled in the GCAM-core
        ## without unnecessary duplication caused by LJ. LJENM throws error.
        bind_rows(L2233.GlobalTechCapital_elec_cool %>%
                    mutate(technology = gsub("storage", "base_storage", technology)),
                  L2233.GlobalTechCapital_elecPassthru %>%
                    mutate(technology=gsub("storage", "base_storage", technology))) %>%
          select(technology, input.capital, capital.overnight, fixed.charge.rate),
        by = c("technology")) %>%
      arrange(sector.name,year) %>%
      mutate(technology = if_else(subsector.name0 == "wind" | subsector.name0 == "solar", subsector.name,
                                  if_else(subsector.name0 == "grid_storage", subsector.name0, technology))) %>%
      na.omit()->
      # remove battery options as they have no cooling options
      L2233.GlobalTechCapital_elec_cool_USA

    L2234.GlobalTechCapFac_elecS_USA %>%
      bind_rows(L2241.GlobalTechCapFac_coal_vintage_USA %>%
                  rename(supplysector = sector.name,
                         subsector = subsector.name),
                L2241.GlobalTechCapFac_elec_coalret_USA) %>%
      add_global_cooling_techs() %>%
      arrange(sector.name, year) ->
      L2233.GlobalTechCapFac_elec_USA


    # If there are not s-curves, specifically for coal, defined at the state level (L2233.StubTechSCurve_elec_USA below), we will
    # we will assume that they follow the predefined lifetime and s-curves for all other techs
    L2234.GlobalTechSCurve_elecS_USA %>%
      add_global_cooling_techs() %>%
      rename(supplysector = sector.name) %>%
      arrange(supplysector, year) ->
      L2233.GlobalTechSCurve_elecS_USA

    # Assume that global tech coefficients map to all segments
    L2233.GlobalTechCoef_elec_cool %>%
      left_join(A23.elecS_tech_mapping_cool,
                # Left_join used as each power plant type will now be multiplied
                # by up to five cooling technologies, thus increasing tibble size
                by = c("subsector.name" = "technology",
                       "technology" = "to.technology")) %>%
      select(-sector.name, -subsector_1, -subsector.name) %>%
      na.omit() %>%
      bind_rows(
        L2233.GlobalTechCoef_elec_cool %>%
          # Left_join used as each power plant type will now be multiplied
          # by up to five cooling technologies, thus increasing tibble size
          left_join(A23.elecS_tech_mapping_cool,
                    by = c("subsector.name" = "technology")) %>%
          filter(grepl("base_storage", to.technology)) %>%
          select(-sector.name, -subsector_1, -subsector.name, -technology) %>%
          rename(technology = to.technology) %>%
          na.omit(),
        L2233.GlobalTechCoef_elec_cool %>%
          # Left_join used as each power plant type will now be multiplied
          # by up to five cooling technologies, thus increasing tibble size
          left_join(A23.elecS_tech_mapping_cool,
                    by = c("technology")) %>%
          filter(grepl("hydro", to.technology) | grepl("PV_base_storage", to.technology)) %>%
          select(-sector.name, -subsector_1, -subsector.name, -technology) %>%
          rename(technology = to.technology)) %>%
      rename(supplysector = Electric.sector,
             subsector0 = subsector,
             subsector = Electric.sector.technology) %>%
      right_join(L2233.GlobalTechEff_elec_cool_USA %>%
                   # Add all fuel, power plant, cooling tech, and all years
                   select(sector.name,subsector.name0,subsector.name,technology),
                 by = c("supplysector" = "sector.name",
                        "subsector0" = "subsector.name0",
                        "subsector" = "subsector.name",
                        "technology")) %>%
      arrange(supplysector, subsector0, subsector, technology, year) %>%
      na.omit() %>%
      unique() ->
      L2233.GlobalTechCoef_elecS_cool_USA

    L2234.GlobalTechCapture_elecS_USA %>%
      add_global_cooling_techs() %>%
      rename(supplysector = sector.name) %>%
      arrange(supplysector, year) ->
      L2233.GlobalTechCapture_elecS_USA

    # Coal vintage lifetimes are added at state level, therefore we remove all coal conv pul from the Global Lifetime values
    L2234.GlobalTechLifetime_elecS_USA %>%
      add_global_cooling_techs() %>%
      rename(supplysector = sector.name) %>%
      arrange(supplysector, year) ->
      L2233.GlobalTechLifetime_elecS_cool_USA

    L2234.AvgFossilEffKeyword_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                # Left_join used as each power plant type will now be multiplied
                # by up to five cooling technologies, thus increasing tibble size
                by = c("technology" = "Electric.sector.technology",
                       "sector.name" = "Electric.sector",
                       "subsector.name" = "subsector")) %>%
      select(-subsector_1, -technology.y) %>%
      rename(subsector = technology,
             technology = to.technology,
             subsector.name0 = subsector.name) %>%
      arrange(sector.name,year) ->
      L2233.AvgFossilEffKeyword_elecS_USA

    # Introduce GlobalInt techs

    # We filter out all non-csp techs initially as these are these are the techs
    # are the ones which need cooling techs added
    csp_filter <- function(data){
      data %>%
        filter(!grepl("CSP", intermittent.technology)) %>%
        rename(subsector0 = subsector,
               subsector = intermittent.technology) %>%
        mutate(intermittent.technology = subsector) ->
        data_new
      return(data_new)
    }

    # no changes needed for backup techs as cooling is not directly associated in previous cooling files
    L2234.GlobalIntTechBackup_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechBackup_elecS_USA %>%
                  filter(grepl("CSP", intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            # Left_join used as each power plant type will now be multiplied
                            # by up to five cooling technologies, thus increasing tibble size
                            by = c("supplysector" = "Electric.sector",
                                   "intermittent.technology" = "Electric.sector.technology",
                                   "subsector")) %>%
                  select(-technology, -subsector_1) %>%
                  rename(subsector0 = subsector,
                         subsector = intermittent.technology,
                         intermittent.technology = to.technology)) %>%
      arrange(supplysector, subsector0, subsector, intermittent.technology, year)->
      L2233.GlobalIntTechBackup_elecS_USA

    # isolate int techs that do not have cooling techs associated
    L2234.GlobalIntTechCapital_elecS_USA %>%
      csp_filter() ->
      L2233.GlobalIntTechCapital_elecS_USA

    # Isolate CSP techs that have two capital costs
    L2234.GlobalIntTechCapital_elecS_USA %>%
      filter(grepl("CSP", intermittent.technology)) %>%
      # Left_join used as each power plant type will now be multiplied
      # by up to five cooling technologies, thus increasing tibble size
      left_join(A23.elecS_tech_mapping_cool,
                by = c("intermittent.technology" = "Electric.sector.technology",
                       "supplysector" = "Electric.sector", "subsector")) %>%
      select(-technology, -subsector_1) %>%
      rename(subsector0 = subsector,
             subsector = intermittent.technology,
             intermittent.technology = to.technology) ->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    L2233.GlobalIntTechCapital_elec_cool %>%
      # Left_join used as each power plant type will now be multiplied
      # by up to five cooling technologies, thus increasing tibble size
      left_join(A23.elecS_tech_mapping_cool,
                by = c("subsector.name" = "subsector_1")) %>%
      select(-sector.name, -subsector.name, -technology.x, -technology.y) %>%
      rename(supplysector = Electric.sector,
             subsector0 = subsector,
             subsector = Electric.sector.technology,
             intermittent.technology = to.technology) %>%
      bind_rows(L2233.GlobalIntTechCapital_elecS_USA,
                L2233.GlobalIntTechCapital_elecS_cool_USA) %>%
      filter(!grepl("CSP_base", subsector)) %>%
      # There are no additional cases of CSP_base across other Int Techs, therefore, we will remove
      # as this has been introduced by calling gcam-core file L2233.GlobalIntTechCapital_elec_cool
      arrange(supplysector, subsector0, subsector, intermittent.technology, year)->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    # isolate CSP sectors which will need cooling tech layer added
    L2234.GlobalIntTechEff_elecS_USA %>%
      csp_filter()->
      L2233.GlobalIntTechEff_elecS_USA

    L2233.GlobalIntTechEff_elec_cool %>%
      filter(grepl("CSP", technology)) %>%
      mutate(technology = gsub("CSP", "CSP_storage", technology)) %>%
      # Left_join used as each power plant type will now be multiplied
      # by up to five cooling technologies, thus increasing tibble size
      left_join(A23.elecS_tech_mapping_cool,
                by = c("subsector.name" = "subsector_1")) %>%
      select(-sector.name, -subsector.name, -technology.y, -technology.x) %>%
      filter((grepl("dry_hybrid", to.technology) & efficiency < 0.96)) %>%
      # As we are not matching CSP names directly here, we want to make sure that the efficiency
      # designated to dry_hybrid, 0.95, is the one that is adopted for all CSP (dry_hybrid) segments
      bind_rows(L2233.GlobalIntTechEff_elec_cool %>%
                  filter(grepl("CSP", technology)) %>%
                  mutate(technology = gsub("CSP", "CSP_storage", technology)) %>%
                  # Left_join used as each power plant type will now be multiplied
                  # by up to five cooling technologies, thus increasing tibble size
                  left_join(A23.elecS_tech_mapping_cool,
                            by = c("subsector.name" = "subsector_1")) %>%
                  select(-sector.name, -subsector.name, -technology.y, -technology.x) %>%
                  # As we are not matching CSP names directly here, we want to make sure that the efficiency
                  # designated to dry_hybrid, 0.95, is the one that is adopted for all CSP (dry_hybrid) segments
                  filter((grepl("recirculating", to.technology) & efficiency > 0.96))) %>%
      rename(supplysector = Electric.sector,
             subsector0 = subsector,
             subsector = Electric.sector.technology,
             intermittent.technology = to.technology) %>%
      bind_rows(L2233.GlobalIntTechEff_elecS_USA) %>%
      filter(subsector != "CSP_base") %>%
      # There are no additional cases of CSP_base across other Int Techs, therefore, we will remove
      # as this has been introduced by calling gcam-core file L2233.GlobalIntTechEff_elec_cool
      arrange(supplysector, subsector0, subsector, intermittent.technology, year) ->
      L2233.GlobalIntTechEff_elecS_cool_USA

    # Add global water coefs from gcam-core to GCAM-USA. Maintain the global coefs
    # Recommended to create state level water demand coefs in the future as state-level coefs are not currently applied
    L2233.GlobalIntTechCoef_elec_cool %>%
      # GCAM-core data has the subsector names for PV = solar, yet CSP = CSP, so we change the subsector
      # name to allow for the mapping to be applied to PV as well
      mutate(subsector.name = if_else(grepl("solar", subsector.name), technology, subsector.name)) %>%
      # Left_join used as each power plant type will now be multiplied
      # by up to five cooling technologies, thus increasing tibble size
      left_join(A23.elecS_tech_mapping_cool,
                by = c("subsector.name" = "subsector_1")) %>%
      select(-sector.name, -subsector.name, -technology.y, -technology.x) %>%
      rename(supplysector = Electric.sector,
             subsector0 = subsector,
             subsector = Electric.sector.technology,
             intermittent.technology = to.technology) %>%
      filter(!grepl("base_storage", subsector)) %>%
      # PV and CSP base storage are not intermittent techs, therefore, we will remove
      # as these have been introduced by calling gcam-core file L2233.GlobalIntTechEff_elec_cool
      arrange(supplysector, subsector0, subsector, intermittent.technology, year) ->
      L2233.GlobalIntTechCoef_elecS_cool_USA

    L2234.GlobalIntTechLifetime_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechLifetime_elecS_USA %>%
                  filter(grepl("CSP", intermittent.technology)) %>%
                  add_int_cooling_techs) %>%
      arrange(supplysector, subsector0, subsector, intermittent.technology, year) ->
      L2233.GlobalIntTechLifetime_elecS_cool_USA

    L2234.GlobalIntTechOMfixed_elecS_USA %>%
      csp_filter() %>%
      bind_rows(L2234.GlobalIntTechOMfixed_elecS_USA %>%
                  filter(grepl("CSP", intermittent.technology)) %>%
                  add_int_cooling_techs) %>%
      arrange(supplysector, subsector0, subsector, intermittent.technology, year) ->
      L2233.GlobalIntTechOMfixed_elecS_cool_USA

    L2234.GlobalIntTechOMvar_elecS_USA %>%
      mutate(subsector0 = subsector,
             subsector = intermittent.technology) ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2234.GlobalIntTechOMvar_elecS_USA %>%
      mutate(subsector0 = subsector,
             subsector = intermittent.technology) ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2234.GlobalIntTechShrwt_elecS_USA %>%
      filter(!grepl("CSP", intermittent.technology)) %>%
      rename(subsector.name0 = subsector.name,
             subsector.name = intermittent.technology) %>%
      mutate(intermittent.technology = subsector.name) %>%
      bind_rows(L2234.GlobalIntTechShrwt_elecS_USA %>%
                  filter(grepl("CSP", intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            # Left_join used as each power plant type will now be multiplied
                            # by up to five cooling technologies, thus increasing tibble size
                            by = c("sector.name" = "Electric.sector",
                                   "intermittent.technology" = "Electric.sector.technology",
                                   "subsector.name" = "subsector")) %>%
                  select(-technology, -subsector_1) %>%
                  rename(subsector.name0 = subsector.name,
                         subsector.name = intermittent.technology,
                         intermittent.technology = to.technology)) %>%
      arrange(sector.name, subsector.name0, subsector.name, intermittent.technology, year) ->
      L2233.GlobalIntTechShrwt_elecS_cool_USA

    L2234.PrimaryRenewKeyword_elecS_USA %>%
      # Left_join used as each power plant type will now be multiplied
      # by up to five cooling technologies, thus increasing tibble size
      left_join(A23.elecS_tech_mapping_cool,
                by=c("technology" = "Electric.sector.technology",
                     "sector.name" = "Electric.sector",
                     "subsector.name" = "subsector")) %>%
      select(-subsector_1, -technology.y) %>%
      rename(subsector = technology,
             technology = to.technology,
             subsector.name0 = subsector.name) %>%
      arrange(sector.name, year) ->
      L2233.PrimaryRenewKeyword_elecS_cool_USA

    L2234.PrimaryRenewKeywordInt_elecS_USA %>%
      filter(!grepl("CSP", intermittent.technology)) %>%
      rename(subsector.name0 = subsector.name,
             subsector.name = intermittent.technology) %>%
      mutate(intermittent.technology = subsector.name) %>%
      bind_rows(L2234.PrimaryRenewKeywordInt_elecS_USA %>%
                  filter(grepl("CSP", intermittent.technology)) %>%
                  left_join(A23.elecS_tech_mapping_cool,
                            # Left_join used as each power plant type will now be multiplied
                            # by up to five cooling technologies, thus increasing tibble size
                            by = c("sector.name" = "Electric.sector",
                                   "intermittent.technology" = "Electric.sector.technology",
                                   "subsector.name" = "subsector")) %>%
                  select(-technology, -subsector_1) %>%
                  rename(subsector.name0 = subsector.name,
                         subsector.name = intermittent.technology,
                         intermittent.technology = to.technology)) %>%
      arrange(sector.name, subsector.name0, subsector.name, intermittent.technology, year) ->
      L2233.PrimaryRenewKeywordInt_elecS_cool_USA

    # We can now look to state level data, once again combining vintages and adding cooling technologies
    # Efficiencies do not change with the addition of cooling techs

    L2234.StubTechEff_elecS_USA %>%
      bind_rows(L2241.StubTechEff_coal_vintage_USA,
                L2241.StubTechEff_elec_coalret_USA) %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(subsector0 == "grid_storage", subsector0, technology)) ->
      L2233.StubTechEff_elec_USA

    L2234.StubTechMarket_elecS_USA %>%
      bind_rows(L2241.StubTechMarket_coal_vintage_USA,
                L2241.StubTechMarket_elec_coalret_USA) %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(grepl("base_storage", subsector0), subsector0,
                                  if_else(grepl("wind_base", subsector), subsector, technology))) ->
      # temporary placement of base_storage cooling techs = nesting subsector
      L2233.StubTechMarket_elec_USA

    L1233.out_EJ_state_elec_F_tech_cool %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(elec_tech_water_map %>%
                                 select(-from.supplysector, -from.subsector, -from.technology,
                                        -minicam.energy.input, -sector, -to.subsector,-to.supplysector),
                               by = c("fuel", "technology", "cooling_system", "water_type")) %>%
      select(-plant_type, -technology, -cooling_system, -water_type) %>%
      rename(supplysector = sector, subsector0 = fuel,  technology = to.technology, region = state) %>%
      ungroup() ->
      L1233.out_EJ_state_elec_F_tech_cool

    # Combine elec segments, coal retire, and coal vintage calibrated production tables
    L2234.StubTechProd_elecS_USA %>%
      rename(tech.share.weight=share.weight) %>%
      # use anti join to filter out entries in elec segments table which will be
      # overwritten by coal retire technologies
      anti_join(L2241.StubTechProd_elec_coalret_USA,
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      bind_rows(L2241.StubTechProd_elec_coalret_USA) %>%
      # use anti join to filter out entries in table which will be overwritten
      # by coal vintage technologies
      anti_join(L2241.StubTechProd_coal_vintage_USA,
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      bind_rows(L2241.StubTechProd_coal_vintage_USA) %>%
      add_cooling_techs() ->
      L2233.StubTechProd_elec_USA

    L2233.StubTechProd_elec_USA %>%
      filter(calOutputValue > 0) %>%
      left_join_keep_first_only(L1233.out_EJ_state_elec_F_tech_cool,
                                by = c("supplysector", "subsector0", "subsector", "technology", "region", "share.weight.year" = "year")) %>%
      replace_na(list(share = 0)) %>%
      group_by(supplysector, subsector0, subsector, region, year) %>%
      # Divide by 2 to account for recirculating and dry_cooling technology types in CSP.
      # These cooling shares are not included in L1233.out_EJ_state_elec_F_tech_cool.
      mutate(calOutputValue = if_else(grepl("CSP", subsector), calOutputValue / 2,
                                      # wind & pv don't have cooling techs and are assigned zero shares based on the replace_na above
                                      # assign full calOutput value
                                      if_else(subsector0 == "wind" | grepl("PV", subsector), calOutputValue, calOutputValue * share))) %>%
      replace_na(list(calOutputValue = 0)) %>%
      select(-value, -share) %>%
      ungroup() %>%
      unique() %>%
      bind_rows(L2233.StubTechProd_elec_USA %>%
                  filter(calOutputValue == 0)) %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      group_by(supplysector, subsector0, subsector, region, year) %>%
      mutate(subs.share.weight = if_else(sum(calOutputValue) > 0, 1, 0)) %>%
      ungroup() %>%
      arrange(region, supplysector, subsector0, year) ->
      L2233.StubTechProd_elec_USA

    # State level s-curves, combining vintages and initial slow and fast retire plants
    bind_rows(L2241.StubTechSCurve_coal_vintage_USA,
              L2241.StubTechSCurve_elec_coalret_USA) %>%
      add_cooling_techs() ->
      L2233.StubTechSCurve_elec_USA

    L2241.StubTechProfitShutdown_coal_vintage_USA %>%
      add_cooling_techs() ->
      L2233.StubTechProfitShutdown_elecS_cool_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      add_cooling_techs() ->
      L2233.StubTechCapFactor_elecS_solar_USA

    L2234.StubTechCapFactor_elecS_wind_USA %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(grepl("wind_base", subsector), subsector, technology)) ->
      L2233.StubTechCapFactor_elecS_wind_USA

    L2234.StubTechElecMarket_backup_elecS_USA %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(subsector == "rooftop_pv" | grepl("wind_base", subsector), subsector, technology)) ->
      L2233.StubTechElecMarket_backup_elecS_USA

    L2234.StubTechFixOut_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechFixOut_elecS_USA

    L2234.StubTechFixOut_hydro_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechFixOut_hydro_elecS_USA

    L2234.StubTechMarket_backup_elecS_USA %>%
      add_cooling_techs() %>%
      mutate(technology = if_else(subsector == "rooftop_pv" | grepl("wind_base", subsector), subsector, technology)) ->
      L2233.StubTechMarket_backup_elecS_USA

    L2234.StubTechCost_offshore_wind_elecS_USA %>%
      add_cooling_techs() ->
      L2233.StubTechCost_offshore_wind_elecS_USA

    L2234.SubsectorLogit_elecS_USA %>%
      rename(subsector0 = subsector) %>%
      mutate(subsector.1 = subsector0) %>%
      select(-subsector.1) %>%
      arrange(supplysector) ->
      L2233.SubsectorLogit_elecS_USA

    # Create logit table at subsector level by including all possible fule types (i.e. hydro, battery, etc.)

    L2234.SubsectorLogit_elecS_USA %>%
      left_join(A23.elecS_tech_mapping_cool,
                # Left_join used as each power plant type will now be multiplied
                # by up to five cooling technologies, thus increasing tibble size
                by = c("supplysector" = "Electric.sector", "subsector")) %>%
      rename(subsector0 = subsector,
             subsector = Electric.sector.technology) %>%
      right_join(bind_rows(L2233.StubTechMarket_elec_USA %>%
                             select(subsector, region, technology),
                           L2233.StubTechFixOut_hydro_elecS_USA %>%
                             select(subsector, region, technology),
                           L2233.StubTechEff_elec_USA %>%
                             filter(subsector == "battery") %>%
                             select(subsector, region, technology)),
                 # Including all possible technologies here
                 by = c("subsector", "region", "to.technology" = "technology")) %>%
      select(region, supplysector, subsector0, subsector, logit.year.fillout, logit.exponent,logit.type) %>%
      # Acknowledge that not all vintages are in each state, therefore shareweights and logits are
      # not needed for these vintages
      unique() %>%
      arrange(supplysector) ->
      L2233.SubsectorLogit_elecS_cool_USA

    L2233.SubsectorLogit_elecS_cool_USA %>%
      select(region, supplysector, subsector0, subsector) %>%
      # select which load segment fuel + power plant exist in each state
      left_join(L2233.GlobalTechCoef_elecS_cool_USA,
                ## LJ used as coefficients are added for all historical years for only the
                ## load segement, fuel, and power plants that exist. This increases data set size,
                ## requiring LJ.
                by = c("supplysector", "subsector0", "subsector")) %>%
      mutate(market.name = if_else(minicam.energy.input == gcamusa.WATER_TYPE_SEAWATER, gcam.USA_REGION, region)) %>%
      na.omit() ->
      L2233.StubTechCoef_elecS_cool_USA

    L2233.StubTechCoef_elecS_cool_USA %>%
      filter(grepl(gcamusa.WATER_TYPE_SEAWATER, technology),
             (region %in% seawater_states_basins)) %>%
      bind_rows(L2233.StubTechCoef_elecS_cool_USA %>%
                  filter(!grepl(gcamusa.WATER_TYPE_SEAWATER, technology))) %>%
      arrange(region, year) ->
      # Not all techs have cooling options, therefore we remove these
      L2233.StubTechCoef_elecS_cool_USA

    L2234.SubsectorShrwt_elecS_USA %>%
      rename(subsector0 = subsector) %>%
      bind_rows(
        L2234.StubTechProd_elecS_USA %>%
          select(region, supplysector, subsector, year, subs.share.weight) %>%
          rename(share.weight = subs.share.weight, subsector0 = subsector) %>%
          unique(),
        L2234.StubTechFixOut_elecS_USA %>%
          select(region, supplysector, subsector, year, subs.share.weight) %>%
          rename(share.weight = subs.share.weight,
                 subsector0 = subsector) %>%
          unique()
      ) %>%
      arrange(region, supplysector, year) ->
      L2233.SubsectorShrwt_elecS_USA

    L2234.SubsectorShrwtInterp_elecS_USA %>%
      rename(subsector0 = subsector) %>%
      mutate(subsector.1 = subsector0) %>%
      select(-subsector.1) %>%
      arrange(supplysector) ->
      L2233.SubsectorShrwtInterp_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      rename(subsector0 = subsector) %>%
      mutate(subsector.1 = subsector0) %>%
      select(-subsector.1) %>%
      arrange(supplysector) ->
      L2233.SubsectorShrwtInterpTo_elecS_USA

    # Subsector shareweights are really generation technology shareweights
    # These use the global tech object in L2234, so copy to all regions
    # No interpolation function is needed
    L2233.StubTechProd_elec_USA %>%
      # calculate historical subsector (generation technology) shareweights
      group_by(region, supplysector, subsector0, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      # if no historical production, assign 0 shareweight; otherwise, assign 1 shareweight
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(-calOutputValue) ->
      L2233.SubsectorShrwt_elecS_cool_USA_hist

    L2234.GlobalTechShrwt_elecS_USA %>%
      bind_rows(L2234.GlobalIntTechShrwt_elecS_USA %>%
                  rename(technology = intermittent.technology),
                L2241.GlobalTechShrwt_elec_coalret_USA,
                L2241.GlobalTechShrwt_coal_vintage_USA) %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
      rename(supplysector = sector.name,
             subsector0 = subsector.name,
             subsector = technology) %>%
      # we only want future years here, because past years are dictated by state-level production
      filter(year %in% MODEL_FUTURE_YEARS)  %>%
      # semi_join historical table to filter out techs that shouldn't be created,
      # such as CSP and geothermal in regions without this resource
      semi_join(L2233.SubsectorShrwt_elecS_cool_USA_hist,
                by = c("region", "supplysector", "subsector0", "subsector")) %>%
      select(region, supplysector, subsector0, subsector, year, share.weight) ->
      L2233.SubsectorShrwt_elecS_cool_USA_fut

    L2233.SubsectorShrwt_elecS_cool_USA_hist %>%
      bind_rows(L2233.SubsectorShrwt_elecS_cool_USA_fut) %>%
      arrange(region,supplysector,year) ->
      L2233.SubsectorShrwt_elecS_cool_USA

    # Prepare interpolation rules for all power plant + cooling system combinations
    # First, we assume that if the generation technology exists in the historical period
    # and is allowed to continue into future periods, the cooling technology shares will
    # be held constant into the future.  The exception is once through cooling, whose
    # share weights will be 0 from 2020-2100 to mirror GCAM-core.
    L2233.StubTechProd_elec_USA %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # join in subsector shareweights so we know which generation technologies
      # are allowed to deploy in the future
      left_join_error_no_match(L2233.SubsectorShrwt_elecS_cool_USA %>%
                                 filter(year %in% (MODEL_FUTURE_YEARS)) %>%
                                 group_by(region, supplysector, subsector0, subsector) %>%
                                 # check if a generation tech has non-zero share-weight in any future period (i.e. sum > 0)
                                 summarise(future.subs.shrwt = if_else(sum(share.weight) > 0, 1, 0)) %>%
                                 ungroup(),
                               by = c("region", "supplysector", "subsector0", "subsector")) -> L2233.StubTechProd_elec_cool_SW_USA

    # Set all once through technologies to zero in all future periods.
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(grepl(gcamusa.DISALLOWED_COOLING_TECH, technology)) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = 0) %>%
      rename(stub.technology = technology) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L2233.StubTechInterpTo_elecS_oncethrough_USA

    # If the particular load segment / generation technology produced historically,
    # fix technology (cooling system) share weights to calibration values
    # for all future periods.  We do this even if the particular load segment /
    # generation technology is not allowed to deploy in the future - these assumptions
    # are handled at the subsector (generation technology) level.
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, technology),
             subs.share.weight > 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT) %>%
      rename(stub.technology = technology) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, interpolation.function)  -> L2233.StubTechInterp_elecS_cool_USA

    # Second, if a generation technology did not exist in the historical period (i.e. CSP, IGCC, etc.),
    # but exists in the future, then the share weights for all non-once through cooling technologies
    # will be set to 1.
    # NOTE: In a very few instances, this includes generation technologies which produced historically
    # but not in the given load segment in question. In these cases, we still set future share weights
    # for all cooling techs to 1 in future periods, even though they may have deployed to different
    # extents in the load segment where the generation technology did produce histroically.
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, technology),
             subs.share.weight == 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      rename(stub.technology = technology) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L2233.StubTechInterpTo_elecS_fut_USA

    # Third, if a fuel and power plant combination did exist in the historical period,
    # but switches to a new power plant type (i.e. Nuclear Gen 2 -> Gen 3), we assume
    # that share weights of cooling technologies for that particular state remain similar
    # to the old power plant (e.g. Gen 3 will have the same cooling tech share weights as Gen 2).

    # Create a table with future techs whose cooling shares we can infer from existing ones
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, technology),
             subs.share.weight == 0 & future.subs.shrwt > 0) %>%
      # Get rid of info related to calibrated values, which we've just confirmed are zero
      select(-calOutputValue, -share.weight.year, -subs.share.weight, -tech.share.weight) %>%
      # use semi_join to filter for future generation technologies which are mapped to current gen techs
      semi_join(A23.elecS_tech_mapping_cool_shares_fut,
                by = "subsector") %>%
      left_join_error_no_match(A23.elecS_tech_mapping_cool_shares_fut,
                               by = "subsector") %>%
      left_join_error_no_match(elec_tech_water_map %>%
                                 select(to.technology, cooling_system),
                               by = c("technology" = "to.technology")) -> L2233.elec_cool_SW_future_techs_USA

    # Create a table with existing techs whose cooling shares we can use to inform future ones
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, technology)) %>%
      # use semi_join to filter for current generation technologies which are used to inform
      # future gen tech cooling shares
      semi_join(A23.elecS_tech_mapping_cool_shares_fut,
                by = c("subsector" = "mapped_subsector")) %>%
      left_join_error_no_match(elec_tech_water_map %>%
                                 select(to.technology, cooling_system),
                               by = c("technology" = "to.technology")) -> L2233.elec_cool_SW_mapped_techs_USA

    # Join tables and match up share info
    L2233.elec_cool_SW_future_techs_USA %>%
      left_join_error_no_match(L2233.elec_cool_SW_mapped_techs_USA %>%
                                 select(-technology, -future.subs.shrwt),
                               by = c("region", "supplysector", "subsector0", "year",
                                      "mapped_subsector" = "subsector", "cooling_system")) %>%
      group_by(region, supplysector, subsector0, subsector) %>%
      # check if a generation tech has non-zero share-weight in any future period (i.e. sum > 0)
      mutate(subs.share.weight = if_else(sum(calOutputValue) > 0, 1, 0)) %>%
      # Remove any generation techs with zero generation by mapped technologies,
      # which will result in zero shares for every cooling tech.
      # This could happen if once through had 100% share historically, since
      # once through is not included in this (future-oriented) table.
      filter(subs.share.weight != 0) %>%
      # calculate new cooling tech shares (share weights) without once through
      mutate(share.weight = round(calOutputValue / sum(calOutputValue), energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = share.weight,
             apply.to = gcamusa.INTERP_APPLY_TO) %>%
      rename(stub.technology = technology) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L2233.StubTechInterpTo_elecS_mapped_USA

    # Finally, all power plant and cooling technology combinations that do not exist in
    # in future years are given 0 share weights in the future periods.
    L2233.StubTechProd_elec_cool_SW_USA %>%
      filter(future.subs.shrwt == 0) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = 0) %>%
      rename(stub.technology = technology) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) ->
      L2233.StubTechInterpTo_elecS_nofut_USA

    # Combine all "InterpTo" cases which specify a particular future shareweight value
    L2233.StubTechInterpTo_elecS_oncethrough_USA %>%
      bind_rows(L2233.StubTechInterpTo_elecS_fut_USA,
                L2233.StubTechInterpTo_elecS_nofut_USA) %>%
      # use anti-join to avoid duplicates, since the scope of L2233.StubTechInterpTo_elecS_mapped_USA
      # overlaps with that of L2233.StubTechInterpTo_elecS_nofut_USA
      anti_join(L2233.StubTechInterpTo_elecS_mapped_USA,
                by = c("region", "supplysector", "subsector0", "subsector", "stub.technology")) %>%
      bind_rows(L2233.StubTechInterpTo_elecS_mapped_USA) %>%
      # use anti-join to remove any technologies which are fixed at calibration values
      anti_join(L2233.StubTechInterp_elecS_cool_USA,
                by = c("region", "supplysector", "subsector0", "subsector", "stub.technology")) ->
      L2233.StubTechInterpTo_elecS_cool_USA

    # Make a table with shareweights for start and end points of "InterpTo" rules
    L2233.StubTechInterpTo_elecS_cool_USA %>%
      gather(drop, year, from.year, to.year) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, year, share.weight = to.value) ->
      L2233.StubTechShrwt_elecS_cool_USA

    # Combine all interpolation cases
    L2233.StubTechInterp_elecS_cool_USA %>%
      bind_rows(L2233.StubTechInterpTo_elecS_cool_USA %>%
                  # get rid of to.value to consolidate to one table
                  # to.value is now reflected in L2233.StubTechShrwt_elecS_cool_USA
                  select(-to.value)) -> L2233.StubTechInterp_elecS_cool_USA

    # Only done to manipulate variable and not throw an error
    L2234.Supplysector_elecS_USA %>%
      mutate(supplysector.1 = supplysector) %>%
      select(-supplysector.1) ->
      L2233.Supplysector_elecS_USA


    # ===================================================
    # Produce outputs

    L2233.GlobalTechEff_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Efficiencies") %>%
      add_legacy_name("L2233.GlobalTechEff_elec_cool") %>%
      add_precursors("L2234.GlobalTechEff_elecS_USA",
                     "L2241.GlobalTechEff_coal_vintage_USA",
                     "L2241.GlobalTechEff_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechEff_elec_cool") ->
      L2233.GlobalTechEff_elecS_cool_USA

    L2233.GlobalTechShrwt_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Shareweights") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elec_cool") %>%
      add_precursors("L2234.GlobalTechShrwt_elecS_USA",
                     "L2241.GlobalTechShrwt_coal_vintage_USA",
                     "L2241.GlobalTechShrwt_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechShrwt_elec_cool") ->
      L2233.GlobalTechShrwt_elecS_cool_USA

    L2233.GlobalTechProfitShutdown_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_legacy_name("L2233.GlobalTechProfitShutdown_elec_cool") %>%
      add_precursors("L2234.GlobalTechProfitShutdown_elecS_USA",
                     "L2241.GlobalTechProfitShutdown_elec_coalret_USA",
                     "L2241.StubTechProfitShutdown_coal_vintage_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechProfitShutdown_elec_cool") ->
      L2233.GlobalTechProfitShutdown_elecS_cool_USA

    L2233.GlobalTechOMvar_elec_USA %>%
      add_title("Electricity Load Segments Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Variable OM Costs") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elec") %>%
      add_precursors("L2234.GlobalTechOMvar_elecS_USA",
                     "L2241.GlobalTechOMvar_coal_vintage_USA",
                     "L2241.GlobalTechOMvar_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechOMvar_elecS_cool_USA

    L2233.GlobalTechOMfixed_elec_USA %>%
      add_title("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elec") %>%
      add_precursors("L2234.GlobalTechOMfixed_elecS_USA",
                     "L2241.GlobalTechOMfixed_coal_vintage_USA",
                     "L2241.GlobalTechOMfixed_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechOMfixed_elecS_cool_USA

    L2233.GlobalTechCapital_elec_USA %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Capital Costs") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec") %>%
      add_precursors("L2234.GlobalTechCapital_elecS_USA",
                     "L2241.GlobalTechCapital_coal_vintage_USA",
                     "L2241.GlobalTechCapital_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCapital_elecS_USA

    L2233.GlobalTechCapital_elec_cool_USA %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology Capital Costs") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("L2234.GlobalTechCapital_elecS_USA",
                     "L2241.GlobalTechCapital_coal_vintage_USA",
                     "L2241.GlobalTechCapital_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalTechCapital_elec_cool",
                     "L2233.GlobalTechCapital_elecPassthru") ->
      L2233.GlobalTechCapital_elecS_cool_USA

    L2233.GlobalTechCapFac_elec_USA %>%
      add_title("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_legacy_name("L2233.GlobalTechCapFac_elec") %>%
      add_precursors("L2234.GlobalTechCapFac_elecS_USA",
                     "L2241.GlobalTechCapFac_coal_vintage_USA",
                     "L2241.GlobalTechCapFac_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCapFac_elecS_cool_USA

    L2233.GlobalTechSCurve_elecS_USA %>%
      add_title("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_legacy_name("L2233.GlobalTechSCurve_elec") %>%
      add_precursors("L2234.GlobalTechSCurve_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechSCurve_elecS_cool_USA

    L2233.GlobalTechCoef_elecS_cool_USA %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalTechCoef_elecS_cool_USA

    L2233.GlobalTechCapture_elecS_USA %>%
      add_title("Electricity Load Segments CCS Technology Characteristics") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments CCS Technology Characteristics") %>%
      add_legacy_name("L2233.GlobalTechCapture_elec") %>%
      add_precursors("L2234.GlobalTechCapture_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool")->
      L2233.GlobalTechCapture_elecS_cool_USA

    L2233.GlobalTechLifetime_elecS_cool_USA %>%
      add_title("Electricity Load Segments Lifetime") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Lifetime") %>%
      add_legacy_name("L2233.GlobalTechLifetime_elec_cool") %>%
      add_precursors("L2234.GlobalTechLifetime_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool")->
      L2233.GlobalTechLifetime_elecS_cool_USA

    L2233.AvgFossilEffKeyword_elecS_USA %>%
      add_title("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_legacy_name("L2233.AvgFossilEffKeyword_elec") %>%
      add_precursors("L2234.AvgFossilEffKeyword_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.AvgFossilEffKeyword_elecS_cool_USA

    L2233.GlobalIntTechBackup_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_legacy_name("L2233.GlobalIntTechBackup_elec") %>%
      add_precursors("L2234.GlobalIntTechBackup_elecS_USA") ->
      L2233.GlobalIntTechBackup_elecS_cool_USA

    L2233.GlobalIntTechCapital_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
      add_precursors("L2234.GlobalIntTechCapital_elecS_USA") ->
      L2233.GlobalIntTechCapital_elecS_USA

    L2233.GlobalIntTechCapital_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_legacy_name(" L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechCapital_elecS_USA",
                     "L2233.GlobalIntTechCapital_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechCapital_elecS_cool_USA

    L2233.GlobalIntTechEff_elecS_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_legacy_name("L2233.GlobalIntTechEff_elec") %>%
      add_precursors("L2234.GlobalIntTechEff_elecS_USA") ->
      L2233.GlobalIntTechEff_elecS_USA

    L2233.GlobalIntTechEff_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_legacy_name("L2233.GlobalIntTechEff_elec") %>%
      add_precursors("L2234.GlobalIntTechEff_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2233.GlobalIntTechEff_elec_cool") ->
      L2233.GlobalIntTechEff_elecS_cool_USA

    L2233.GlobalIntTechLifetime_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_legacy_name("L2233.GlobalIntTechLifetime_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechLifetime_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechLifetime_elecS_cool_USA

    L2233.GlobalIntTechOMfixed_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechOMfixed_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechOMfixed_elecS_cool_USA

    L2233.GlobalIntTechOMvar_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechOMvar_elecS_USA") ->
      L2233.GlobalIntTechOMvar_elecS_cool_USA

    L2233.GlobalIntTechShrwt_elecS_cool_USA %>%
      add_title("Electricity Load Segments Intermittent Technology Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Intermittent Technology Shareweights") %>%
      add_legacy_name("L2233.GlobalIntTechShrwt_elec_cool") %>%
      add_precursors("L2234.GlobalIntTechShrwt_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechShrwt_elecS_cool_USA

    L2233.GlobalIntTechCoef_elecS_cool_USA %>%
      add_title("Water demand coefs for int techs") %>%
      add_units("none") %>%
      add_comments("Water demand coefs for int techs") %>%
      add_legacy_name("L2233.GlobalIntTechCoef_elec_cool") %>%
      add_precursors("L2233.GlobalIntTechCoef_elec_cool",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.GlobalIntTechCoef_elecS_cool_USA

    L2233.PrimaryRenewKeyword_elecS_cool_USA %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_units("none") %>%
      add_comments("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_legacy_name("L2233.PrimaryRenewKeyword_elec_cool") %>%
      add_precursors("L2234.PrimaryRenewKeyword_elecS_USA") ->
      L2233.PrimaryRenewKeyword_elecS_cool_USA

    L2233.PrimaryRenewKeywordInt_elecS_cool_USA %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_legacy_name("L2233.PrimaryRenewKeywordInt_elec_cool") %>%
      add_precursors("L2234.PrimaryRenewKeywordInt_elecS_USA") ->
      L2233.PrimaryRenewKeywordInt_elecS_cool_USA

    L2233.StubTechEff_elec_USA %>%
      add_title("Electricity Load Segments Base Year Efficiencies") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Base Year Efficiencies") %>%
      add_legacy_name("L2233.StubTechEff_elec") %>%
      add_precursors("L2234.StubTechEff_elecS_USA",
                     "L2241.StubTechEff_coal_vintage_USA",
                     "L2241.StubTechEff_elec_coalret_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechEff_elecS_cool_USA

    L2233.StubTechCoef_elecS_cool_USA %>%
      add_title("Water demand coefficients at state level for Electricity Load Segments") %>%
      add_units("none") %>%
      add_comments("Water demand coefficients at state level for Electricity Load Segments") %>%
      add_legacy_name("L2233.StubTechCoef_elec") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCoef_elecS_cool_USA

    L2233.StubTechMarket_elec_USA %>%
      add_title("Energy Inputs for Electricity Load Segments and associated Cooling Technologies") %>%
      add_units("none") %>%
      add_comments("Energy Inputs for Electricity Load Segments and associated Cooling Technologies") %>%
      add_legacy_name("L2233.StubTechMarket_elec") %>%
      add_precursors("L2234.StubTechMarket_elecS_USA",
                     "L2241.StubTechMarket_coal_vintage_USA",
                     "L2241.StubTechMarket_elec_coalret_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechMarket_elecS_cool_USA

    L2233.StubTechProd_elec_USA %>%
      add_title("Electricity Load Segments and Cooling Technology Calibration Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments and Cooling Technology Calibration Outputs") %>%
      add_legacy_name("L2233.StubTechProd_elec") %>%
      add_precursors("L2234.StubTechProd_elecS_USA",
                     "L2241.StubTechProd_coal_vintage_USA",
                     "L2241.StubTechProd_elec_coalret_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L1233.out_EJ_state_elec_F_tech_cool",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/elec_tech_water_map") ->
      L2233.StubTechProd_elecS_cool_USA

    L2233.StubTechSCurve_elec_USA %>%
      add_title("Electricity Load Segment and Cooling Technology S-Curves") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segment and Cooling Technology S-Curves") %>%
      add_legacy_name("L2233.StubTechSCurve_elec") %>%
      add_precursors("L2241.StubTechSCurve_coal_vintage_USA",
                     "L2241.StubTechSCurve_elec_coalret_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechSCurve_elecS_cool_USA

    L2233.StubTechProfitShutdown_elecS_cool_USA %>%
      add_title("Electricity Load Segment and Cooling Technology Profit Shutdown Decider") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segment and Cooling Technology Profit Shutdown Decider") %>%
      add_legacy_name("L2233.StubTechSCurve_elec") %>%
      add_precursors("L2241.StubTechProfitShutdown_coal_vintage_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechProfitShutdown_elecS_cool_USA

    L2233.StubTechCapFactor_elecS_solar_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Solar and Cooling Technologies") %>%
      add_units("none") %>%
      add_comments("State-specific Capacity Factors for Electricity Load Segments Solar and Cooling Technologies") %>%
      add_legacy_name("L2233.StubTechCapFactor_elec_solar") %>%
      add_precursors("L2234.StubTechCapFactor_elecS_solar_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCapFactor_elecS_solar_USA

    L2233.StubTechCapFactor_elecS_wind_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Wind Technologies") %>%
      add_units("none") %>%
      add_comments("State-specific Capacity Factors for Electricity Load Segments Wind Technologies") %>%
      add_legacy_name("L2233.StubTechCapFactor_elec_wind") %>%
      add_precursors("L2234.StubTechCapFactor_elecS_wind_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCapFactor_elecS_wind_USA

    L2233.StubTechElecMarket_backup_elecS_USA %>%
      add_title("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_legacy_name("L2233.StubTechElecMarket_backup_elec") %>%
      add_precursors("L2234.StubTechElecMarket_backup_elecS_USA",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechElecMarket_backup_elecS_cool_USA

    L2233.StubTechFixOut_elecS_USA %>%
      add_title("Electricity Load Segments Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Fixed Outputs") %>%
      add_legacy_name("L2233.StubTechFixOut_elec") %>%
      add_precursors("L2234.StubTechFixOut_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechFixOut_elecS_cool_USA

    L2233.StubTechFixOut_hydro_elecS_USA %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_legacy_name("L2233.StubTechFixOut_hydro_elec") %>%
      add_precursors("L2234.StubTechFixOut_hydro_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechFixOut_hydro_elecS_cool_USA

    L2233.StubTechCost_offshore_wind_elecS_USA %>%
      add_title("Electricity Load Segments Offshore Wind Cost Adjustment") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Offshore Wind Cost Adjustment") %>%
      add_legacy_name("L2233.StubTechCost_offshore_wind_elec_USA") %>%
      add_precursors("L2234.StubTechCost_offshore_wind_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechCost_offshore_wind_elecS_cool_USA

    L2233.StubTechMarket_backup_elecS_USA %>%
      add_title("Backup Energy Inputs for Electricity Load Segments Intermittent Technologies") %>%
      add_units("none") %>%
      add_comments("Backup Energy Inputs for Electricity Load Segments Intermittent Technologies") %>%
      add_legacy_name("L2233.StubTechMarket_backup_elec") %>%
      add_precursors("L2234.StubTechMarket_backup_elecS_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L2233.StubTechMarket_backup_elecS_cool_USA

    L2233.StubTechInterp_elecS_cool_USA %>%
      add_title("Electricity Load Segments Stub Tech Interpolation Rules") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Stub Tech Interpolation Rules - fixed at calibration values") %>%
      add_legacy_name("L2233.StubTechShrwt_elec") %>%
      same_precursors_as(L2233.StubTechProd_elec_USA) %>%
      same_precursors_as(L2233.SubsectorShrwt_elecS_cool_USA) %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping_cool_shares_fut") ->
      L2233.StubTechInterp_elecS_cool_USA

    L2233.StubTechShrwt_elecS_cool_USA %>%
      add_title("Electricity Load Segments Cooling Technology Stub Tech Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Cooling Technology Stub Tech Shareweights") %>%
      add_legacy_name("L2233.StubTechShrwt_elec") %>%
      same_precursors_as(L2233.StubTechInterp_elecS_cool_USA) ->
      L2233.StubTechShrwt_elecS_cool_USA

    L2233.SubsectorLogit_elecS_USA %>%
      add_title("Nested Subsector Information for Horizontal Electricity Load Segments",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Nested Subsector Information for Horizontal Electricity Load Segments") %>%
      add_legacy_name("L223.SubsectorLogit_elec") %>%
      add_precursors("L2234.SubsectorLogit_elecS_USA") ->
      L2233.SubsectorLogit_elecS_USA

    L2233.SubsectorLogit_elecS_cool_USA %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments with Cooling Technologies",overwrite=TRUE) %>%
      add_units("none") %>%
      add_comments("Subsector Information for Horizontal Electricity Load Segments with Cooling Technologies") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
      add_precursors("L2234.SubsectorLogit_elecS_USA") ->
      L2233.SubsectorLogit_elecS_cool_USA

    L2233.SubsectorShrwt_elecS_USA %>%
      add_title("Nested Electricity Load Segments Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Nested Electricity Load Segments Subsector Shareweights") %>%
      add_legacy_name("L2233.SubsectorShrwt_elec") %>%
      add_precursors("L2234.SubsectorShrwt_elecS_USA") ->
      L2233.SubsectorShrwt_elecS_USA

    L2233.SubsectorShrwt_elecS_cool_USA %>%
      add_title("Electricity Load Segments and Cooling Technology Subsector Shareweights") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments and Cooling Technology Subsector Shareweights") %>%
      add_legacy_name("L2233.SubsectorShrwt_elec") %>%
      add_precursors("L2234.StubTechProd_elecS_USA") ->
      L2233.SubsectorShrwt_elecS_cool_USA

    L2233.SubsectorShrwtInterp_elecS_USA %>%
      add_title("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_legacy_name("L2233.SubsectorShrwtInterp_elec") %>%
      add_precursors("L2234.SubsectorShrwtInterp_elecS_USA") ->
      L2233.SubsectorShrwtInterp_elecS_USA

    L2233.SubsectorShrwtInterpTo_elecS_USA %>%
      add_title("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_units("none") %>%
      add_comments("Electricity Load Segments Nesting-Subsector Shareweight Interpolation") %>%
      add_legacy_name("L2233.SubsectorShrwtInterpTo_elec") %>%
      add_precursors("L2234.SubsectorShrwtInterpTo_elecS_USA") ->
      L2233.SubsectorShrwtInterpTo_elecS_USA

    L2233.Supplysector_elecS_USA %>%
      add_title("Supply Sector Information for Electricity Load Segments") %>%
      add_units("none") %>%
      add_comments("Supply Sector Information for Electricity Load Segments") %>%
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
                L2233.StubTechShrwt_elecS_cool_USA,
                L2233.StubTechInterp_elecS_cool_USA,
                L2233.SubsectorLogit_elecS_USA,
                L2233.SubsectorLogit_elecS_cool_USA,
                L2233.SubsectorShrwt_elecS_USA,
                L2233.SubsectorShrwt_elecS_cool_USA,
                L2233.SubsectorShrwtInterp_elecS_USA,
                L2233.StubTechCost_offshore_wind_elecS_cool_USA,
                L2233.SubsectorShrwtInterpTo_elecS_USA,
                L2233.Supplysector_elecS_cool_USA)
  } else {
    stop("Unknown command")
  }
}
