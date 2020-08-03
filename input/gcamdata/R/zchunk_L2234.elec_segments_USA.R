# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2234.elec_segments_USA
#'
#' Generates GCAM-USA model inputs for multiple load segments electricity sector by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2234.Supplysector_elecS_USA}, \code{L2234.ElecReserve_elecS_USA}, \code{L2234.SubsectorLogit_elecS_USA},
#' \code{L2234.SubsectorShrwtInterp_elecS_USA}, \code{L2234.SubsectorShrwtInterpTo_elecS_USA}, \code{L2234.SubsectorShrwt_elecS_USA},
#' \code{L2234.StubTechEff_elecS_USA}, \code{L2234.StubTechCapFactor_elecS_solar_USA}, \code{L2234.StubTechCapFactor_elecS_wind_USA},
#' \code{L2234.SubsectorShrwtFllt_elecS_grid_USA}, \code{L2234.SubsectorShrwtInterp_elecS_grid_USA}, \code{L2234.PassThroughSector_elecS_USA},
#' \code{L2234.PassThroughTech_elecS_grid_USA}, \code{L2234.GlobalTechShrwt_elecS_USA}, \code{L2234.GlobalIntTechShrwt_elecS_USA},
#' \code{L2234.PrimaryRenewKeyword_elecS_USA},\code{L2234.PrimaryRenewKeywordInt_elecS_USA}, \code{L2234.AvgFossilEffKeyword_elecS_USA},
#' \code{L2234.GlobalTechCapital_elecS_USA}, \code{L2234.GlobalIntTechCapital_elecS_USA}, \code{L2234.GlobalTechOMfixed_elecS_USA},
#' \code{L2234.GlobalIntTechOMfixed_elecS_USA}, \code{L2234.GlobalTechOMvar_elecS_USA}, \code{L2234.GlobalIntTechOMvar_elecS_USA},
#' \code{L2234.GlobalTechCapFac_elecS_USA}, \code{L2234.GlobalTechEff_elecS_USA}, \code{L2234.GlobalIntTechEff_elecS_USA},
#' \code{L2234.GlobalTechLifetime_elecS_USA}, \code{L2234.GlobalIntTechLifetime_elecS_USA}, \code{L2234.GlobalTechProfitShutdown_elecS_USA},
#' \code{L2234.GlobalTechSCurve_elecS_USA}, \code{L2234.GlobalTechCapture_elecS_USA}, \code{L2234.GlobalIntTechBackup_elecS_USA},
#' \code{L2234.StubTechMarket_elecS_USA}, \code{L2234.StubTechMarket_backup_elecS_USA}, \code{L2234.StubTechElecMarket_backup_elecS_USA},
#' \code{L2234.StubTechProd_elecS_USA}, \code{L2234.StubTechFixOut_elecS_USA}, \code{L2234.StubTechFixOut_hydro_elecS_USA}, \code{L2234.StubTechCost_offshore_wind_elecS_USA},
#' \code{L2234.TechShrwt_elecS_grid_USA}, \code{L2234.TechCoef_elecS_grid_USA}, \code{L2234.TechProd_elecS_grid_USA}.
#' The corresponding file in the original data system was \code{L2234.elec_segments_USA.R} (gcam-usa level2).
#' @details This chunk generates input files to create an electricity generation sector with multiple load segments
#' for each state and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter if_else mutate select semi_join summarise_if bind_rows
#' @importFrom tidyr complete nesting replace_na
#' @author MTB Aug 2018
module_gcamusa_L2234.elec_segments_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A23.elecS_sector",
             FILE = "gcam-usa/A23.elecS_metainfo",
             FILE = "gcam-usa/A23.elecS_subsector_logit",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt_interp",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt_interpto",
             FILE = "gcam-usa/A23.elecS_globaltech_shrwt",
             FILE = "gcam-usa/A23.elecS_globalinttech_shrwt",
             FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elecS_inttech_mapping",
             FILE = "gcam-usa/A23.elecS_tech_availability",
             FILE = "gcam-usa/A23.elecS_stubtech_energy_inputs",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt_state_adj",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt_interp_state_adj",
             FILE = "gcam-usa/A23.elecS_subsector_shrwt_interpto_state_adj",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "gcam-usa/elecS_time_fraction",
             FILE = "gcam-usa/A10.renewable_resource_delete",
      		 "L113.elecS_globaltech_capital_battery_ATB",
             "L119.CapFacScaler_CSP_state",
             "L1239.state_elec_supply_USA",
             "L223.StubTechEff_elec_USA",
             "L223.StubTechMarket_elec_USA",
             "L223.StubTechProd_elec_USA",
             "L223.StubTechFixOut_elec_USA",
             "L223.StubTechFixOut_hydro_USA",
             "L223.StubTechMarket_backup_USA",
             "L223.StubTechCapFactor_elec_wind_USA",
             "L223.StubTechCapFactor_elec_solar_USA",
             "L223.GlobalTechCapFac_elec",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalIntTechCapital_elec",
             "L123.eff_R_elec_F_Yh",
             "L223.GlobalTechEff_elec",
             "L223.GlobalIntTechEff_elec",
             "L223.GlobalTechLifetime_elec",
             "L223.GlobalIntTechLifetime_elec",
             "L223.GlobalTechOMfixed_elec",
             "L223.GlobalIntTechOMfixed_elec",
             "L223.GlobalTechOMvar_elec",
             "L223.GlobalIntTechOMvar_elec",
             "L223.GlobalTechSCurve_elec",
             "L223.GlobalTechProfitShutdown_elec",
             "L223.GlobalTechCapture_elec",
             "L223.GlobalIntTechBackup_elec",
             "L223.PrimaryRenewKeyword_elec",
             "L223.PrimaryRenewKeywordInt_elec",
             "L223.AvgFossilEffKeyword_elec",
             "L223.StubTechCost_offshore_wind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2234.Supplysector_elecS_USA",
             "L2234.ElecReserve_elecS_USA",
             "L2234.SubsectorLogit_elecS_USA",
             "L2234.SubsectorShrwtInterp_elecS_USA",
             "L2234.SubsectorShrwtInterpTo_elecS_USA",
             "L2234.SubsectorShrwt_elecS_USA",
             "L2234.StubTechEff_elecS_USA",
             "L2234.StubTechCapFactor_elecS_solar_USA",
             "L2234.StubTechCapFactor_elecS_wind_USA",
             "L2234.SubsectorShrwtFllt_elecS_grid_USA",
             "L2234.SubsectorShrwtInterp_elecS_grid_USA",
             "L2234.PassThroughSector_elecS_USA",
             "L2234.PassThroughTech_elecS_grid_USA",
             "L2234.GlobalTechShrwt_elecS_USA",
             "L2234.GlobalIntTechShrwt_elecS_USA",
             "L2234.PrimaryRenewKeyword_elecS_USA",
             "L2234.PrimaryRenewKeywordInt_elecS_USA",
             "L2234.AvgFossilEffKeyword_elecS_USA",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalIntTechCapital_elecS_USA",
             "L2234.GlobalTechOMfixed_elecS_USA",
             "L2234.GlobalIntTechOMfixed_elecS_USA",
             "L2234.GlobalTechOMvar_elecS_USA",
             "L2234.GlobalIntTechOMvar_elecS_USA",
             "L2234.GlobalTechCapFac_elecS_USA",
             "L2234.GlobalTechEff_elecS_USA",
             "L2234.GlobalIntTechEff_elecS_USA",
             "L2234.GlobalTechLifetime_elecS_USA",
             "L2234.GlobalIntTechLifetime_elecS_USA",
             "L2234.GlobalTechProfitShutdown_elecS_USA",
             "L2234.GlobalTechSCurve_elecS_USA",
             "L2234.GlobalTechCapture_elecS_USA",
             "L2234.GlobalIntTechBackup_elecS_USA",
             "L2234.StubTechMarket_elecS_USA",
             "L2234.StubTechMarket_backup_elecS_USA",
             "L2234.StubTechElecMarket_backup_elecS_USA",
             "L2234.StubTechProd_elecS_USA",
             "L2234.StubTechFixOut_elecS_USA",
             "L2234.StubTechFixOut_hydro_elecS_USA",
             "L2234.StubTechCost_offshore_wind_elecS_USA",
             "L2234.TechShrwt_elecS_grid_USA",
             "L2234.TechCoef_elecS_grid_USA",
             "L2234.TechProd_elecS_grid_USA"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- grid_region <- state <- year <- supplysector <- Electric.sector <- sector <- subsector <- subsector_1 <-
      sector.name <- subsector.name <- technology <- intermittent.technology <- Electric.sector.technology <-
      Electric.sector.intermittent.technology <- logit.exponent <- logit.type <- logit.year.fillout <-
      output.unit <- input.unit <- share.weight <- apply.to <- from.year <- to.year <- to.value <-
      interpolation.function <- price.unit <- electricity.reserve.margin <- average.grid.capacity.factor <-
      primary.renewable <- average.fossil.efficiency <- input.capital <- capital.overnight <- fixed.charge.rate <-
      capacity.factor <- input.OM.fixed <- OM.fixed <- input.OM.var <- OM.var <- minicam.energy.input <-
      efficiency <- type <- lifetime <- steepness <- half.life <- median.shutdown.point <- profit.shutdown.steepness <-
      remove.fraction <- storage.market <- backup.capacity.factor <- backup.capital.cost <- capacity.limit <-
      electric.sector.name <- flag <- minicam.non.energy.input <- trial.market.name <- stub.technology <-
      market.name <-  electric.sector.market <- calOutputValue <- count_tech <- share.weight.year <-
      subs.share.weight <- tech.share.weight <- subscalOutputValue <- fraction <- subsector.cal.value <-
      fixedOutput <- eff_actual <- GCAM_region_ID <- geothermal_resource <- L2234.TechMarket_elecS_grid <-
      tech.share <- supplysector.y <- supplysector.x <- segment <- share.weight.x <- year.x <- year.y <- State <-
      period <- capital.cost <- fcr <- fixed.om <- variable.om <- coefficient <- passthrough.sector <-
      marginal.revenue.sector <- marginal.revenue.market <- Geothermal_Hydrothermal_GWh <- geo_state_noresource <-
      fuel <- scaler <- n <- NULL # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    A23.elecS_sector <- get_data(all_data, "gcam-usa/A23.elecS_sector", strip_attributes = TRUE)
    A23.elecS_metainfo <- get_data(all_data, "gcam-usa/A23.elecS_metainfo", strip_attributes = TRUE)
    A23.elecS_subsector_logit <- get_data(all_data, "gcam-usa/A23.elecS_subsector_logit", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt_interp <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt_interp", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt_interpto <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt_interpto", strip_attributes = TRUE)
    A23.elecS_globaltech_shrwt <- get_data(all_data, "gcam-usa/A23.elecS_globaltech_shrwt", strip_attributes = TRUE)
    A23.elecS_globalinttech_shrwt <- get_data(all_data, "gcam-usa/A23.elecS_globalinttech_shrwt", strip_attributes = TRUE)
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping", strip_attributes = TRUE)
    A23.elecS_inttech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_inttech_mapping", strip_attributes = TRUE)
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability", strip_attributes = TRUE)
    L113.elecS_globaltech_capital_battery_ATB <- get_data(all_data, "L113.elecS_globaltech_capital_battery_ATB", strip_attributes = TRUE)
    A23.elecS_stubtech_energy_inputs <- get_data(all_data, "gcam-usa/A23.elecS_stubtech_energy_inputs", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt_state_adj <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt_state_adj", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt_interp_state_adj <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt_interp_state_adj", strip_attributes = TRUE)
    A23.elecS_subsector_shrwt_interpto_state_adj <- get_data(all_data, "gcam-usa/A23.elecS_subsector_shrwt_interpto_state_adj", strip_attributes = TRUE)
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential", strip_attributes = TRUE)
    elecS_time_fraction <- get_data(all_data, "gcam-usa/elecS_time_fraction", strip_attributes = TRUE)
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete", strip_attributes = TRUE)
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.CapFacScaler_CSP_state", strip_attributes = TRUE)
    L1239.state_elec_supply_USA <- get_data(all_data, "L1239.state_elec_supply_USA", strip_attributes = TRUE)
    L223.StubTechEff_elec_USA <- get_data(all_data, "L223.StubTechEff_elec_USA", strip_attributes = TRUE)
    L223.StubTechMarket_elec_USA <- get_data(all_data, "L223.StubTechMarket_elec_USA", strip_attributes = TRUE)
    L223.StubTechProd_elec_USA <- get_data(all_data, "L223.StubTechProd_elec_USA", strip_attributes = TRUE)
    L223.StubTechFixOut_elec_USA <- get_data(all_data, "L223.StubTechFixOut_elec_USA", strip_attributes = TRUE)
    L223.StubTechFixOut_hydro_USA <- get_data(all_data, "L223.StubTechFixOut_hydro_USA", strip_attributes = TRUE)
    L223.StubTechMarket_backup_USA <- get_data(all_data, "L223.StubTechMarket_backup_USA", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec_wind_USA <- get_data(all_data, "L223.StubTechCapFactor_elec_wind_USA", strip_attributes = TRUE)
    L223.StubTechCapFactor_elec_solar_USA <- get_data(all_data, "L223.StubTechCapFactor_elec_solar_USA", strip_attributes = TRUE)
    L223.GlobalTechCapFac_elec <- get_data(all_data, "L223.GlobalTechCapFac_elec", strip_attributes = TRUE)
    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec", strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec", strip_attributes = TRUE)
    L123.eff_R_elec_F_Yh <- get_data(all_data, "L123.eff_R_elec_F_Yh", strip_attributes = TRUE)
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec", strip_attributes = TRUE)
    L223.GlobalIntTechEff_elec <- get_data(all_data, "L223.GlobalIntTechEff_elec", strip_attributes = TRUE)
    L223.GlobalTechLifetime_elec <- get_data(all_data, "L223.GlobalTechLifetime_elec", strip_attributes = TRUE)
    L223.GlobalIntTechLifetime_elec <- get_data(all_data, "L223.GlobalIntTechLifetime_elec", strip_attributes = TRUE)
    L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec", strip_attributes = TRUE)
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec", strip_attributes = TRUE)
    L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec", strip_attributes = TRUE)
    L223.GlobalIntTechOMvar_elec <- get_data(all_data, "L223.GlobalIntTechOMvar_elec", strip_attributes = TRUE)
    L223.GlobalTechSCurve_elec <- get_data(all_data, "L223.GlobalTechSCurve_elec", strip_attributes = TRUE)
    L223.GlobalTechProfitShutdown_elec <- get_data(all_data, "L223.GlobalTechProfitShutdown_elec", strip_attributes = TRUE)
    L223.GlobalTechCapture_elec <- get_data(all_data, "L223.GlobalTechCapture_elec", strip_attributes = TRUE)
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec", strip_attributes = TRUE)
    L223.PrimaryRenewKeyword_elec <- get_data(all_data, "L223.PrimaryRenewKeyword_elec", strip_attributes = TRUE)
    L223.PrimaryRenewKeywordInt_elec <- get_data(all_data, "L223.PrimaryRenewKeywordInt_elec", strip_attributes = TRUE)
    L223.AvgFossilEffKeyword_elec <- get_data(all_data, "L223.AvgFossilEffKeyword_elec", strip_attributes = TRUE)
    L223.StubTechCost_offshore_wind_USA <- get_data(all_data, "L223.StubTechCost_offshore_wind_USA", strip_attributes = TRUE)

    # ===================================================
    # Data Processing

    # There are several segment / technology combinations that we think do not make sense.
    # These combinations are outlined in A23.elecS_tech_availability.
    # To avoid creating these technologies and then deleting them (which eats up memory and
    # causes a lot of error messages), we remove them from the relevant association files here.

    L2234.load_segments <- unique(A23.elecS_inttech_mapping$Electric.sector)

    A23.elecS_tech_mapping %>%
      anti_join(A23.elecS_tech_availability,
                by = c("Electric.sector.technology" = "stub.technology")) %>%
      mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
      arrange(subsector, Electric.sector) %>%
      mutate(Electric.sector = as.character(Electric.sector)) -> A23.elecS_tech_mapping

    A23.elecS_inttech_mapping %>%
      anti_join(A23.elecS_tech_availability,
                by = c("Electric.sector.intermittent.technology" = "stub.technology")) %>%
      mutate(Electric.sector = factor(Electric.sector, levels = L2234.load_segments)) %>%
      arrange(subsector, Electric.sector) %>%
      mutate(Electric.sector = as.character(Electric.sector)) -> A23.elecS_inttech_mapping

    A23.elecS_globaltech_shrwt %>%
      anti_join(A23.elecS_tech_availability,
                by = c("technology" = "stub.technology")) -> A23.elecS_globaltech_shrwt

    A23.elecS_globalinttech_shrwt %>%
      anti_join(A23.elecS_tech_availability,
                by = c("technology" = "stub.technology")) -> A23.elecS_globalinttech_shrwt

    # Binding tech_shrwt files together to create master list of available technologies, then
    # reducing to sector / subsector combinations to deal with A23.elecS_subsector assumption files

    A23.elecS_globaltech_shrwt %>%
      bind_rows(A23.elecS_globalinttech_shrwt) %>%
      select(supplysector, subsector, technology) -> L2234.avail_techs

    L2234.avail_techs %>%
      distinct(supplysector, subsector) -> L2234.avail_seg_subsector

    A23.elecS_subsector_logit %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_logit

    A23.elecS_subsector_shrwt %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt

    A23.elecS_subsector_shrwt_interp %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt_interp

    A23.elecS_subsector_shrwt_interpto %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt_interpto

    A23.elecS_subsector_shrwt_state_adj %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt_state_adj

    A23.elecS_subsector_shrwt_interp_state_adj %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt_interp_state_adj

    A23.elecS_subsector_shrwt_interpto_state_adj  %>%
      semi_join(L2234.avail_seg_subsector,
                by =c("supplysector", "subsector")) -> A23.elecS_subsector_shrwt_interpto_state_adj

    # -----------------------------------------------------------------------------
    # Build state-level tables
    # Supplysector information
    # Create horizontal generation supplysectors
    L2234.Supplysector_elecS_USA <-
      write_to_all_states(A23.elecS_sector, c("region", "supplysector", "output.unit", "input.unit", "price.unit",
                                               "logit.year.fillout", "logit.exponent", "logit.type" ))

    L2234.ElecReserve_elecS_USA <-
      write_to_all_states(A23.elecS_metainfo, c("region", "supplysector","electricity.reserve.margin",
                                                 "average.grid.capacity.factor"))

    # 2b. Subsector information
    L2234.SubsectorLogit_elecS_USA <-
      write_to_all_states(A23.elecS_subsector_logit, c("region", "supplysector", "subsector", "logit.year.fillout",
                                                        "logit.exponent" , "logit.type" ))  %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector"))

    # Note that the subsector share-weights are updated after processing calibration year output values.
    L2234.SubsectorShrwt_elecS_USA <-
      write_to_all_states(A23.elecS_subsector_shrwt, c("region", "supplysector","subsector","year", "share.weight"))  %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector"))

    L2234.SubsectorShrwtInterp_elecS_USA <-
      write_to_all_states(A23.elecS_subsector_shrwt_interp, c("region", "supplysector","subsector","apply.to",
                                                               "from.year","to.year", "interpolation.function"))  %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector"))

    L2234.SubsectorShrwtInterpTo_elecS_USA <-
      write_to_all_states(A23.elecS_subsector_shrwt_interpto, c("region", "supplysector","subsector", "apply.to",
                                                                 "from.year","to.year","to.value", "interpolation.function"))  %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector"))

    # Filter out hydro since it is dealt with separately in the fixed output tables
    L2234.SubsectorShrwt_elecS_USA %>%
      filter(subsector != "hydro") -> L2234.SubsectorShrwt_elecS_USA

    L2234.SubsectorShrwtInterp_elecS_USA %>%
      filter(subsector != "hydro") -> L2234.SubsectorShrwtInterp_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      filter(subsector != "hydro") -> L2234.SubsectorShrwtInterpTo_elecS_USA

    # 2c. Technology information
    # Shareweights
    A23.elecS_globaltech_shrwt %>%
      gather_years("share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector,
             technology, year, share.weight) -> L2234.GlobalTechShrwt_elecS

    A23.elecS_globalinttech_shrwt %>%
      gather_years("share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector,
             intermittent.technology = technology, year, share.weight) -> L2234.GlobalIntTechShrwt_elecS

    # Primary energy keywords
    L223.PrimaryRenewKeyword_elec %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(A23.elecS_tech_mapping %>%
                  select(-subsector_1),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      select(Electric.sector, subsector.name, Electric.sector.technology, year, primary.renewable) %>%
      rename(sector.name = Electric.sector, technology = Electric.sector.technology) %>%
      arrange(year, subsector.name, sector.name, technology) -> L2234.PrimaryRenewKeyword_elecS

    L223.PrimaryRenewKeywordInt_elec %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(A23.elecS_inttech_mapping %>%
                  select(-subsector_1),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector",
                       "intermittent.technology" = "intermittent.technology")) %>%
      select(Electric.sector, subsector.name, Electric.sector.intermittent.technology, year, primary.renewable) %>%
      rename(sector.name = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) %>%
      arrange(year, subsector.name, sector.name, intermittent.technology) -> L2234.PrimaryRenewKeywordInt_elecS

    L223.AvgFossilEffKeyword_elec %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(A23.elecS_tech_mapping %>%
                  select(-subsector_1),
                by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      select(Electric.sector, subsector.name, Electric.sector.technology, year, average.fossil.efficiency) %>%
      rename(sector.name = Electric.sector, technology = Electric.sector.technology) %>%
      filter(!is.na(technology)) %>%
      arrange(year, subsector.name, sector.name, technology) -> L2234.AvgFossilEffKeyword_elecS

    # Capital Costs of detailed electric sector technologies
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechCapital_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(capital.overnight)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechCapital_elecS

    # Read in lower capacity factors for non-baseload technologies. The fractions are based on the
    # elecS_time_fraction data on the fraction of demand supplied by vertical segment
    L2234.int_CF_adj <- round(sum(mean(elecS_time_fraction$intermediate.electricity.time) +
                                    mean(elecS_time_fraction$subpeak.electricity.time) +
                                    mean(elecS_time_fraction$peak.electricity.time)), 3)

    L2234.subpeak_CF_adj <- round(sum(mean(elecS_time_fraction$subpeak.electricity.time) +
                                        mean(elecS_time_fraction$peak.electricity.time)), 3)

    L2234.peak_CF_adj <- round(mean(elecS_time_fraction$peak.electricity.time), 3)

    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechCapFac_elec , by = c("subsector" = "subsector.name", "technology")) %>%
      filter(technology != "hydro") %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename (supplysector = Electric.sector, technology = Electric.sector.technology) %>%
      mutate(capacity.factor = if_else(supplysector == "intermediate generation",
                                       L2234.int_CF_adj * capacity.factor, capacity.factor ),
             capacity.factor = if_else(supplysector == "subpeak generation",
                                       L2234.subpeak_CF_adj * capacity.factor, capacity.factor ),
             capacity.factor = if_else(supplysector == "peak generation",
                                       L2234.peak_CF_adj * capacity.factor, capacity.factor )) ->
      L2234.GlobalTechCapFac_elecS

    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechCapital_elec, by= c("subsector"= "subsector.name" , "intermittent.technology")) %>%
      filter(!is.na(capital.overnight)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechCapital_elecS

    # O&M Costs of detailed electric sector technologies
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechOMfixed_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(OM.fixed)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechOMfixed_elecS

    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechOMfixed_elec, by= c("subsector"= "subsector.name" , "intermittent.technology")) %>%
      filter(!is.na(OM.fixed)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechOMfixed_elecS

    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechOMvar_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(OM.var)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechOMvar_elecS

    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechOMvar_elec, by= c("subsector"= "subsector.name" , "intermittent.technology")) %>%
      filter(!is.na(OM.var)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechOMvar_elecS

    # Efficiencies
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechEff_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(efficiency)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechEff_elecS

    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechEff_elec, by= c("subsector"= "subsector.name" , "intermittent.technology")) %>%
      filter(!is.na(efficiency)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechEff_elecS

    # Technology Lifetimes
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechLifetime_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(lifetime)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechLifetime_elecS

    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechLifetime_elec, by= c("subsector"= "subsector.name" , "intermittent.technology")) %>%
      filter(!is.na(lifetime)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechLifetime_elecS

    # S-curve shut-down decider
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechSCurve_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(lifetime)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechSCurve_elecS

    # Profit shut-down decider
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechProfitShutdown_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) ->
      L2234.GlobalTechProfitShutdown_elecS

    # Additional characteristics for CCS technologies
    A23.elecS_tech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalTechCapture_elec, by = c("subsector" = "subsector.name", "technology")) %>%
      filter(!is.na(remove.fraction)) %>%
      select(-supplysector, -subsector_1, -technology, -sector.name ) %>%
      rename(supplysector = Electric.sector, technology = Electric.sector.technology) -> L2234.GlobalTechCapture_elecS

    # Additional characteristics for intermittent technologies
    A23.elecS_inttech_mapping %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.GlobalIntTechBackup_elec, by= c("subsector"= "subsector.name", "intermittent.technology" = "technology")) %>%
      filter(!is.na(capacity.limit)) %>%
      select(-supplysector, -subsector_1, -intermittent.technology, -sector.name) %>%
      rename(supplysector = Electric.sector, intermittent.technology = Electric.sector.intermittent.technology) ->
      L2234.GlobalIntTechBackup_elecS

    # Energy inputs
    A23.elecS_inttech_mapping %>%
      rename(Electric.sector.technology = Electric.sector.intermittent.technology, technology = intermittent.technology) %>%
      bind_rows(A23.elecS_tech_mapping) -> L2234.StubTechMarket_elecS_USA_temp

    L223.StubTechMarket_elec_USA %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L2234.StubTechMarket_elecS_USA_temp,
                by= c("supplysector","subsector", "stub.technology" = "technology")) %>%
      filter(!is.na(minicam.energy.input), !is.na(Electric.sector.technology)) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology,
             year, minicam.energy.input, market.name) -> L2234.StubTechMarket_elecS_USA

    # Backup markets
    L223.StubTechMarket_backup_USA %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(A23.elecS_inttech_mapping,
                by = c("supplysector", "subsector", "stub.technology" = "intermittent.technology")) %>%
      filter(!is.na(minicam.energy.input)) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.intermittent.technology,
             year, minicam.energy.input, market.name) -> L2234.StubTechMarket_backup_elecS_USA

    L2234.StubTechMarket_backup_elecS_USA %>%
      select(-minicam.energy.input, -market.name) %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      select(region, supplysector, subsector, stub.technology, year, electric.sector.market = grid_region) ->
      L2234.StubTechElecMarket_backup_elecS_USA

    # Calibration Year Outputs. Note that all technologies in GCAM-USA are calibrated on the output.
    A23.elecS_inttech_mapping %>%
      rename(Electric.sector.technology = Electric.sector.intermittent.technology, technology = intermittent.technology) %>%
      bind_rows(A23.elecS_tech_mapping) -> L2234.StubTechProd_elecS_USA_temp

    L2234.StubTechProd_elecS_USA_temp %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_states(c("region","Electric.sector", "supplysector", "subsector",
                            "Electric.sector.technology", "technology","year")) -> L2234.StubTechProd_elecS_USA_temp

    L2234.StubTechProd_elecS_USA_temp %>%
      # L223.StubTechProd_elec_USA does not contain technologies which do not exist historically, such as IGCC or CCS
      # because of this, the join produces NAs; left_join_error_no_match throws an error, so left_join is used
      left_join(L223.StubTechProd_elec_USA,
                by = c("region","supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      filter(subsector != "hydro") %>%
      select(-supplysector, -technology) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      mutate(share.weight.year = year) %>%
      replace_na(list(calOutputValue = 0,
                      # Note that these subsector share weights are later over-written to read in zero share-weights for subsectors
                      # in base-years with zero base-year calibration values and 1 for subsectors with non-zero calibration values.
                      subs.share.weight = 0,
                      share.weight = 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(count_tech = dplyr::n()) %>%
      ungroup() %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(tech.share = if_else(count_tech == 1, 1, calOutputValue / sum(calOutputValue)),
             tech.share = if_else(tech.share == "NaN", 0, tech.share)) %>%
      ungroup() -> L2234.StubTechProd_elecS_USA

    L223.StubTechProd_elec_USA %>%
      group_by(region,supplysector,subsector, year ) %>%
      summarise(subscalOutputValue = sum(calOutputValue)) %>%
      ungroup() -> L2234.SubsCalOutputVal_elecS_USA

    L2234.StubTechProd_elecS_USA %>%
      # join will produce NAs; left_join_error_no_match throws error, so left_join used
      left_join(L2234.SubsCalOutputVal_elecS_USA, by = c("region", "year", "subsector")) %>%
      mutate(subscalOutputValue = if_else(is.na(subscalOutputValue), 0, subscalOutputValue)) %>%
      select(-supplysector.y) %>%
      rename(supplysector = supplysector.x) -> L2234.StubTechProd_elecS_USA

    L1239.state_elec_supply_USA %>%
      select(state, fuel, segment, year, fraction)%>%
      rename(region = state, supplysector = segment, subsector = fuel) %>%
      mutate(year = as.numeric(year)) -> L2234.fuelfractions_segment_USA

    L2234.StubTechProd_elecS_USA %>%
      # join will produce NAs; left_join_error_no_match throws error, so left_join used
      left_join(L2234.fuelfractions_segment_USA, by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(calOutputValue = subscalOutputValue * fraction * tech.share) %>%
      select(-fraction, -tech.share, -subscalOutputValue) %>%
      replace_na(list(calOutputValue = 0)) %>%
      # load segment solver returns a negative fuel share for one grid / segment / fuel combination
      # (Central East grid / peak generation / gas in 2015)
      # reset these calibrated values to zero
      mutate(calOutputValue = if_else(calOutputValue < 0, 0, calOutputValue),
             # round calibrated output values to appropriate digits
             calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT)) -> L2234.StubTechProd_elecS_USA

    # Adjust subsector share-weights to read in zero share-weights for subsectors and technologies
    # in base-years with zero base-year calibration values
    L2234.StubTechProd_elecS_USA %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      select(region, supplysector, subsector, year, subsector.cal.value = calOutputValue) %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L2234.StubTechProd_elecS_USA, by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(subs.share.weight = if_else(subsector.cal.value == 0, 0, 1),
             share.weight = if_else(calOutputValue == 0, 0, 1 )) -> L2234.StubTechProd_elecS_USA

    # Removing offshore wind technologies for states that don't have any resource
    offshore_wind_states <- L223.StubTechMarket_elec_USA %>%
      filter(stub.technology == "wind_offshore") %>%
      distinct(region)

    L2234.StubTechProd_elecS_USA %>%
      filter(!grepl("_offshore", stub.technology)) %>%
      bind_rows(L2234.StubTechProd_elecS_USA %>%
                  filter(grepl("_offshore", stub.technology)) %>%
                  semi_join(offshore_wind_states, by = c("region"))) -> L2234.StubTechProd_elecS_USA


    # Update future subsector share-weights as follows:
    # 1. For coal, gas and oil - fix share-weights to calibration values. This does not require any update
    # to the L2234.SubsectorShrwtInterp_elecS_USA table.
    # 2. For nuclear - use zero  shareweights if there is no calibration year value.
    # Interpolate to a fixed value for states that have nuclear.
    # 3. For biomass, solar, wind, geothermal, rooftop_PV and storage - interpolate to a fixed value in a future year.
    # The fixed value in the above formulations are read in separately in L2234.SubsectorShrwt_elecS_USA
    # Note that we avoid particular segment / technology combinations that we think do not make sense by either
    # (1) removing them from association files so they are never created (using A23.elecS_tech_availability,
    # near top of script) or (2) zeroing out their shareweights in future model periods.

    L2234.StubTechProd_elecS_USA %>%
      filter(year == max(MODEL_BASE_YEARS)) -> L2234.StubTechProd_elecS_USA_final_cal_year

    # Adjusting nuclear subsector shareweights - states with no historical nuclear power generation
    # receive zero  shareweights.
    L2234.SubsectorShrwt_elecS_USA %>%
      # left_join_error_no_match throws an error because the grid_storage subsector does not exist historically
      # and is missing from L2234.StubTechProd_elecS_USA_final_cal_year, generating NAs for grid_storage subsector.cal.value
      # subsector.cal.value is only used to adjust nuclear shareweights, so NAs for grid_storage don't matter; left_join is used
      left_join(L2234.StubTechProd_elecS_USA_final_cal_year %>%
                  # summarize calibrated production by subsector
                  group_by(region, supplysector, subsector) %>%
                  summarize(subsector.cal.value = sum(subsector.cal.value)) %>%
                  ungroup(),
                by = c("region", "supplysector", "subsector")) %>%
      mutate(share.weight = as.double(share.weight),
             share.weight = if_else(subsector == "nuclear" & subsector.cal.value == 0, 0, share.weight)) %>%
      select(region, supplysector, subsector, year, share.weight) ->  L2234.SubsectorShrwt_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      # left_join_error_no_match throws an error because the grid_storage subsector does not exist historically
      # and is missing from L2234.StubTechProd_elecS_USA_final_cal_year, generating NAs for grid_storage subsector.cal.value
      # subsector.cal.value is only used to adjust nuclear shareweights, so NAs for grid_storage don't matter; left_join is used
      left_join(L2234.StubTechProd_elecS_USA_final_cal_year %>%
                  # summarize calibrated production by subsector
                  group_by(region, supplysector, subsector) %>%
                  summarize(subsector.cal.value = sum(subsector.cal.value)) %>%
                  ungroup(),
                by = c("region", "supplysector", "subsector")) %>%
      mutate(to.value = as.double(to.value),
             to.value = if_else(subsector == "nuclear" & subsector.cal.value == 0, 0, to.value)) %>%
      select(region, supplysector, subsector, apply.to, from.year, to.year, to.value, interpolation.function) ->
      L2234.SubsectorShrwtInterpTo_elecS_USA

    # State-specific adjustments to electricity generation subsector shareweights
    A23.elecS_subsector_shrwt_state_adj %>%
      set_years() %>%
      mutate(year = as.integer(year)) -> A23.elecS_subsector_shrwt_state_adj

    L2234.SubsectorShrwt_elecS_USA %>%
      mutate(year = as.integer(year)) %>%
      anti_join(A23.elecS_subsector_shrwt_state_adj,
                by = c("region", "supplysector", "subsector")) %>%
      bind_rows(A23.elecS_subsector_shrwt_state_adj) %>%
      arrange(region, subsector, year, supplysector) -> L2234.SubsectorShrwt_elecS_USA

    A23.elecS_subsector_shrwt_interp_state_adj %>%
      set_years() %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) -> A23.elecS_subsector_shrwt_interp_state_adj

    L2234.SubsectorShrwtInterp_elecS_USA %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) %>%
      anti_join(A23.elecS_subsector_shrwt_interp_state_adj,
                by = c("region", "supplysector", "subsector")) %>%
      bind_rows(A23.elecS_subsector_shrwt_interp_state_adj) %>%
      arrange(region, subsector, from.year, supplysector) -> L2234.SubsectorShrwtInterp_elecS_USA

    A23.elecS_subsector_shrwt_interpto_state_adj %>%
      set_years() %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) -> A23.elecS_subsector_shrwt_interpto_state_adj

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) %>%
      anti_join(A23.elecS_subsector_shrwt_interpto_state_adj, by = c("region", "supplysector", "subsector")) %>%
      bind_rows(A23.elecS_subsector_shrwt_interpto_state_adj) %>%
      arrange(region, subsector, from.year, supplysector) -> L2234.SubsectorShrwtInterpTo_elecS_USA

    # Get the L2234.StubTechProd_elecS_USA in the right form without subsector.cal.value
    L2234.StubTechProd_elecS_USA %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, share.weight) -> L2234.StubTechProd_elecS_USA

    # Fixed Output calibration for hydro
    A23.elecS_tech_mapping %>%
      filter(subsector == "hydro")  -> L2234.StubTechFixOut_elecS_USA_temp

    L223.StubTechFixOut_elec_USA %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L2234.StubTechFixOut_elecS_USA_temp,
                by = c("supplysector", "subsector","stub.technology" = "technology")) %>%
      select(-supplysector, -subsector_1, -stub.technology) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      left_join_error_no_match(L2234.fuelfractions_segment_USA, by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(fixedOutput = fixedOutput*fraction) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput,
             share.weight.year, subs.share.weight, tech.share.weight) -> L2234.StubTechFixOut_elecS_USA

    # Fixed Output for hydro in future years.
    # We apply the same fule fractions in future years as in the final calibration year
    L2234.fuelfractions_segment_USA %>%
      filter(year == max(MODEL_BASE_YEARS),
             subsector == "hydro") -> L2234.fuelfractions_segment_USA_hydro_final_calibration_year

    L223.StubTechFixOut_hydro_USA %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(A23.elecS_tech_mapping, by = c("supplysector","subsector","stub.technology" = "technology")) %>%
      select(-supplysector, -subsector_1, -stub.technology) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      left_join_error_no_match(L2234.fuelfractions_segment_USA_hydro_final_calibration_year,
                               by = c("region", "supplysector", "subsector")) %>%
      mutate(fixedOutput = fixedOutput * fraction) %>%
      select(region, supplysector, subsector, stub.technology, year = year.x, fixedOutput,
             share.weight.year, subs.share.weight, tech.share.weight) -> L2234.StubTechFixOut_hydro_elecS_USA

    # Efficiencies for biomass, coal, oil, and gas technologies in calibration years
    L2234.StubTechMarket_elecS_USA %>%
      filter(subsector %in% c("coal", "gas", "refined liquids", "biomass"),
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(A23.elecS_tech_mapping,
                               by = c("supplysector" = "Electric.sector", "subsector",
                                      "stub.technology" = "Electric.sector.technology")) %>%
      # join will produce NAs (filtered out later); left_join_error_no_match throws error, so left_join used
      left_join(L223.StubTechEff_elec_USA %>%
                  select(-supplysector),
                by = c("region", "subsector", "technology" = "stub.technology",
                       "year", "minicam.energy.input", "market.name")) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name) %>%
      filter(!is.na(efficiency)) -> L2234.StubTechEff_elecS_USA

    # Re-writing efficiencies for gas technologies since efficiencies in L223.StubTechEff_elec_USA are based on
    # those calculated in LA1231.elech_tec.R in the energy data-system. That calculation was pretty complicated involving
    # sharing out of generation and efficiency between CT and CC. In the multiple load segments version, we assume that CT
    # is generated only in the peak whereas CC is dispatched in all segments. The following code overwrites efficiencies
    # for gas technologies such that if there is only one gas technology in the calibration period, we give it the
    # efficiency based on actual historical efficiency calculated in L123.eff_R_elec_F_Yh.csv. All other fuels are OK since
    # there has always been only one technology in the calibration period per fuel so their efficiencies
    # (in L223.StubTechEff_elec_USA) are based on the actual historical efficiency in the L123.eff_R_elec_F_Yh.csv file.
    L123.eff_R_elec_F_Yh %>%
      gather_years("eff_actual") %>%
      filter(GCAM_region_ID == gcam.USA_CODE,
             year %in% MODEL_BASE_YEARS) -> L2234.fuel_eff_actual

    L2234.StubTechEff_elecS_USA %>%
      group_by (region, supplysector, subsector, year) %>%
      mutate(count_tech = dplyr::n()) %>%
      ungroup() %>%
      left_join_error_no_match(L2234.fuel_eff_actual %>%
                                 select(-GCAM_region_ID, -sector), by = c("subsector" = "fuel", "year" )) %>%
      mutate(efficiency = if_else(count_tech == 1 , eff_actual, efficiency)) %>%
      select(-count_tech, -eff_actual) -> L2234.StubTechEff_elecS_USA

    # Capacity factors by state for wind and solar
    A23.elecS_inttech_mapping %>%
      rename(stub.technology = intermittent.technology) %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.StubTechCapFactor_elec_wind_USA, by = c("subsector", "stub.technology")) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.intermittent.technology,
             year, capacity.factor) %>%
      filter(subsector != "solar",
             !is.na(capacity.factor)) -> L2234.StubTechCapFactor_elecS_wind_USA

    A23.elecS_tech_mapping %>%
      rename(stub.technology = technology) %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.StubTechCapFactor_elec_wind_USA, by = c("subsector", "stub.technology")) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology,
             year, capacity.factor) %>%
      filter(subsector != "solar",
             !is.na(capacity.factor)) -> L2234.StubTechCapFactor_elecS_wind_storage_USA

    L2234.StubTechCapFactor_elecS_wind_USA %>%
      bind_rows(L2234.StubTechCapFactor_elecS_wind_storage_USA) -> L2234.StubTechCapFactor_elecS_wind_USA

    A23.elecS_inttech_mapping %>%
      rename(stub.technology = intermittent.technology) %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.StubTechCapFactor_elec_solar_USA, by = c("subsector", "stub.technology")) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.intermittent.technology,
             year, capacity.factor) %>%
      filter(subsector != "wind",
             !is.na(capacity.factor)) -> L2234.StubTechCapFactor_elecS_solar_USA

    A23.elecS_tech_mapping %>%
      rename(stub.technology = technology) %>%
      # join is intended to duplicate rows; left_join_error_no_match throws error, so left_join used
      left_join(L223.StubTechCapFactor_elec_solar_USA, by = c("subsector", "stub.technology")) %>%
      select(region, supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology,
             year, capacity.factor) %>%
      filter(subsector != "wind",
             !is.na(capacity.factor)) -> L2234.StubTechCapFactor_elecS_solar_storage_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      bind_rows(L2234.StubTechCapFactor_elecS_solar_storage_USA) -> L2234.StubTechCapFactor_elecS_solar_USA

    # Remove geothermal in states with no geothermal resource
    # Indicate states where geothermal subsector and technologies will not be created
    NREL_us_re_technical_potential %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      select(state_name = State) %>%
      left_join_error_no_match(states_subregions, by = "state_name") %>%
      mutate(geothermal_resource = "none") %>%
      select(region = state, geothermal_resource) -> geo_states_noresource

    # Remove geothermal subsector from tables
    L2234.geo.tables <- list(L2234.SubsectorLogit_elecS_USA = L2234.SubsectorLogit_elecS_USA,
                             L2234.SubsectorShrwt_elecS_USA = L2234.SubsectorShrwt_elecS_USA,
                             L2234.SubsectorShrwtInterp_elecS_USA = L2234.SubsectorShrwtInterp_elecS_USA,
                             L2234.SubsectorShrwtInterpTo_elecS_USA = L2234.SubsectorShrwtInterpTo_elecS_USA,
                             L2234.StubTechEff_elecS_USA = L2234.StubTechEff_elecS_USA,
                             L2234.StubTechProd_elecS_USA = L2234.StubTechProd_elecS_USA)

    process_geo_tables <- function(data){
      data %>%
        # join will produce NAs because only states with no geothermal resource are present in geo_states_noresource
        # left_join_error_no_match throws error, so left_join used
        left_join(geo_states_noresource, by = "region") %>%
        mutate(geothermal_resource = paste(geothermal_resource, subsector, sep = "-")) %>%
        filter(geothermal_resource != "none-geothermal") %>%
        select(-geothermal_resource)
    }

    L2234.geo.tables_rev <- lapply(L2234.geo.tables, process_geo_tables)

    # Assign the tables back to the original dataframes
    L2234.SubsectorLogit_elecS_USA <- L2234.geo.tables_rev[["L2234.SubsectorLogit_elecS_USA"]]
    L2234.SubsectorShrwt_elecS_USA <- L2234.geo.tables_rev[["L2234.SubsectorShrwt_elecS_USA"]]
    L2234.SubsectorShrwtInterp_elecS_USA <- L2234.geo.tables_rev[["L2234.SubsectorShrwtInterp_elecS_USA"]]
    L2234.SubsectorShrwtInterpTo_elecS_USA <- L2234.geo.tables_rev[["L2234.SubsectorShrwtInterpTo_elecS_USA"]]
    L2234.StubTechEff_elecS_USA <- L2234.geo.tables_rev[["L2234.StubTechEff_elecS_USA"]]
    L2234.StubTechProd_elecS_USA <- L2234.geo.tables_rev[["L2234.StubTechProd_elecS_USA"]]

    # Remove CSP technologies from regions with no CSP resource
    # This is only a problem in L2234.StubTechProd_elecS_USA
    # A vector indicating states where CSP electric technologies will not be created
    L119.CapFacScaler_CSP_state %>%
      # states with effectively no resource are assigned a capacity factor scalar of 0.01
      # remove these states to avoid creating CSP technologies there
      filter(scaler <= 0.01) %>%
      pull(state) -> CSP_states_noresource

    L2234.StubTechProd_elecS_USA %>%
      filter(!(region %in% CSP_states_noresource) | !grepl("CSP", stub.technology)) %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector")) -> L2234.StubTechProd_elecS_USA

    # Create tables for non-energy and energy inputs for any new technologies such as battery
    # and append them with corresponding tables
    L113.elecS_globaltech_capital_battery_ATB %>%
      select(supplysector, subsector, technology, year = period, capacity.factor) -> L2234.GlobalTechCapFac_elecS_additonal

    L2234.GlobalTechCapFac_elecS %>%
      bind_rows(L2234.GlobalTechCapFac_elecS_additonal) -> L2234.GlobalTechCapFac_elecS

    L113.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.capital = "capital") %>%
      select(supplysector, subsector, technology, year = period, input.capital,
             capital.overnight = capital.cost, fixed.charge.rate = fcr) ->  L2234.GlobalTechCapital_elecS_additonal

    L2234.GlobalTechCapital_elecS %>%
      bind_rows(L2234.GlobalTechCapital_elecS_additonal) -> L2234.GlobalTechCapital_elecS

    L113.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.OM.fixed = "OM-fixed") %>%
      select(supplysector, subsector, technology, year = period,
             input.OM.fixed, OM.fixed = fixed.om) -> L2234.GlobalTechOMfixed_elecS_additonal

    L2234.GlobalTechOMfixed_elecS %>%
      bind_rows(L2234.GlobalTechOMfixed_elecS_additonal) -> L2234.GlobalTechOMfixed_elecS

    L113.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.OM.var = "OM-var") %>%
      select(supplysector, subsector, technology, year = period,
             input.OM.var, OM.var = variable.om) -> L2234.GlobalTechOMvar_elecS_additonal

    L2234.GlobalTechOMvar_elecS %>%
      bind_rows(L2234.GlobalTechOMvar_elecS_additonal) -> L2234.GlobalTechOMvar_elecS

    L113.elecS_globaltech_capital_battery_ATB %>%
      select(supplysector, subsector, technology, year = period, lifetime) -> L2234.GlobalTechLifetime_elecS_additonal

    L2234.GlobalTechLifetime_elecS %>%
      bind_rows(L2234.GlobalTechLifetime_elecS_additonal) -> L2234.GlobalTechLifetime_elecS

    L113.elecS_globaltech_capital_battery_ATB %>%
      select(supplysector, subsector, technology, year = period,
             lifetime, steepness, half.life) -> L2234.GlobalTechSCurve_elecS_additonal

    L2234.GlobalTechSCurve_elecS %>%
      bind_rows(L2234.GlobalTechSCurve_elecS_additonal) -> L2234.GlobalTechSCurve_elecS

    # Energy Inputs for additional technologies such as battery
    L2234.StubTech_energy_elecS_USA <- write_to_all_states(A23.elecS_stubtech_energy_inputs,
                                                           c("region", "supplysector","subsector","stub.technology",
                                                             "period", "minicam.energy.input", "market.name", "efficiency") )

    L2234.StubTech_energy_elecS_USA %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      mutate(market.name = if_else(market.name == "grid_region", grid_region, region)) %>%
      select(region, supplysector, subsector, stub.technology, year = period,
             minicam.energy.input, efficiency, market.name) -> L2234.StubTechEff_elecS_USA_additional

    L2234.StubTechEff_elecS_USA %>%
      bind_rows(L2234.StubTechEff_elecS_USA_additional) -> L2234.StubTechEff_elecS_USA


    # 3. Build csvs for grid region sectors and append them to state-level tables where possible.
    # Create horizontal and vertical supplysectors in grid regions
    L2234.Supplysector_elecS_grid <- write_to_all_states(A23.elecS_sector,
                                                         c( "region", "supplysector", "output.unit", "input.unit", "price.unit",
                                                            "logit.year.fillout", "logit.exponent", "logit.type" ),
                                                         # NOTE: writing to all grid regions, rather than states
                                                         region_list = gcamusa.GRID_REGIONS)

    L2234.Supplysector_elecS_USA %>%
      bind_rows(L2234.Supplysector_elecS_grid) -> L2234.Supplysector_elecS_USA

    L2234.ElecReserve_elecS_grid <- write_to_all_states(A23.elecS_metainfo %>% select(-region),
                                                        c("region", "supplysector","electricity.reserve.margin",
                                                          "average.grid.capacity.factor"),
                                                        # NOTE: writing to all grid regions, rather than states
                                                        region_list = gcamusa.GRID_REGIONS)

    L2234.ElecReserve_elecS_USA %>%
      bind_rows(L2234.ElecReserve_elecS_grid) -> L2234.ElecReserve_elecS_USA

    # Logits for subsectors in grid regions
    L2234.Supplysector_elecS_USA %>%
      filter(!grepl("grid", region)) %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      select(region, supplysector, grid_region) %>%
      mutate(subsector = paste(region,supplysector, sep = " ")) %>%
      select(grid_region, supplysector, subsector) %>%
      rename(region = grid_region) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) -> L2234.SubsectorLogit_elecS_grid

    L2234.SubsectorLogit_elecS_grid <- L2234.SubsectorLogit_elecS_grid[order(L2234.SubsectorLogit_elecS_grid$region),]

    L2234.SubsectorLogit_elecS_USA %>%
      mutate(logit.year.fillout = as.integer(logit.year.fillout)) %>%
      bind_rows(L2234.SubsectorLogit_elecS_grid) -> L2234.SubsectorLogit_elecS_USA

    # Shareweights for subsectors in grid regions
    L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) -> L2234.SubsectorShrwtFllt_elecS_grid

    L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed" ) -> L2234.SubsectorShrwtInterp_elecS_grid

    # Shareweights for technologies in grid region sectors. This is a new table that needs to created.
    # Shareweights for state-level technologies are read in the global-technology-database.
    L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = subsector,
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      repeat_add_columns((tibble::tibble(year = MODEL_YEARS))) -> L2234.TechShrwt_elecS_grid

    # Specify inputs for technologies in grid regions
    L2234.TechShrwt_elecS_grid %>%
      select(-share.weight) %>%
      mutate(minicam.energy.input = supplysector,
             market.name = substr(subsector, 1, 2)) %>%
      filter(market.name %in% states_subregions$state) -> L2234.TechMarket_elecS_grid

    # Coefficients for technologies in grid region sectors.
    # Coefficients for generation sectors are 1.
    L2234.TechMarket_elecS_grid %>%
      mutate(coefficient = gcamusa.DEFAULT_COEFFICIENT) %>%
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name) -> L2234.TechCoef_elecS_grid

    # Calibration years outputs for technologies in grid regions
    L2234.StubTechFixOut_elecS_USA %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      select(region, grid_region, supplysector, subsector, stub.technology, year, fixedOutput) %>%
      group_by (region, grid_region, supplysector, year) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() -> L2234.TechFixOut_elecS_grid

    L2234.StubTechProd_elecS_USA %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      select(region, grid_region, supplysector, subsector, stub.technology, year, calOutputValue) %>%
      group_by(region, grid_region,supplysector, year) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      # join will produce NAs; left_join_error_no_match throws error, so left_join used
      left_join(L2234.TechFixOut_elecS_grid, by = c("region", "grid_region", "supplysector", "year")) %>%
      mutate(fixedOutput = if_else(is.na(fixedOutput), 0, fixedOutput)) -> L2234.TechProd_elecS_grid

    L2234.TechProd_elecS_grid %>%
      mutate(calOutputValue = calOutputValue + fixedOutput) %>%
      select(-fixedOutput) %>%
      mutate(subsector = paste(region, supplysector, sep = " "),
             technology = subsector,
             share.weight.year = as.numeric(year),
             subs.share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(grid_region, supplysector, subsector, technology, year, calOutputValue,
             share.weight.year, subs.share.weight, share.weight) %>%
      rename(region = grid_region) -> L2234.TechProd_elecS_grid

    # Adjust subsector and technology shareweights in Calibation table to zero if calibration output is zero.
    L2234.TechProd_elecS_grid %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      select(region, supplysector, subsector, year, subsector.cal.value = calOutputValue) %>%
      left_join_error_no_match(L2234.TechProd_elecS_grid, by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(subs.share.weight = as.double(subs.share.weight),
             share.weight = as.double(share.weight),
             subs.share.weight = if_else(subsector.cal.value == 0, 0, 1),
             share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(region, supplysector, subsector, technology, year, calOutputValue,
             share.weight.year, subs.share.weight, share.weight) -> L2234.TechProd_elecS_grid

    # Create a L2234.PassThroughSector_elecS_USA table.
    # The marginal revenue sector is the region's electricity sector whereas the marginal revenue market is the grid region.
    L2234.Supplysector_elecS_USA %>%
      filter(!grepl("grid", region)) %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      mutate(marginal.revenue.sector = supplysector) %>%
      select(region, passthrough.sector = supplysector, marginal.revenue.sector, marginal.revenue.market = grid_region) ->
      L2234.PassThroughSector_elecS_USA

    # Create a L223.PassThroughTech_elec_FERC dataframe (to be converted into a csv table later).
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    # Note that the "technology" in this data-frame will be called "passthrough technology"
    L2234.TechShrwt_elecS_grid %>%
      select(-year, -share.weight) %>%
      distinct() -> L2234.PassThroughTech_elecS_grid

    # Create State-specific non-energy cost adder for offshore wind grid connection cost
    L2234.StubTechMarket_elecS_USA %>%
      filter(grepl("_offshore", stub.technology)) %>%
      select(-minicam.energy.input, -market.name) %>%
      left_join_error_no_match(L223.StubTechCost_offshore_wind_USA %>%
                                 select(-supplysector, -stub.technology),
                               by = c("region", "subsector", "year")) -> L2234.StubTechCost_offshore_wind_elecS_USA


    # ===================================================
    # Produce outputs

    L2234.Supplysector_elecS_USA %>%
      add_title("Supply Sector Information for Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Supplysector information for horizontal generation supplysectors at state and grid region level") %>%
      add_legacy_name("L2234.Supplysector_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_sector") ->
      L2234.Supplysector_elecS_USA

    L2234.ElecReserve_elecS_USA %>%
      add_title("Supply Sector Information for Horizontal Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Supplysector information for horizontal generation supplysectors at state and grid region level") %>%
      add_legacy_name("L2234.ElecReserve_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_metainfo") ->
      L2234.ElecReserve_elecS_USA

    L2234.SubsectorLogit_elecS_USA %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Subsector information for horizontal generation supplysectors at state and grid region level") %>%
      add_legacy_name("L2234.SubsectorLogit_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_subsector_logit",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_sector",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L2234.SubsectorLogit_elecS_USA

    L2234.SubsectorShrwtInterp_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors that are fixed at calibration values") %>%
      add_legacy_name("L2234.SubsectorShrwtInterp_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_subsector_shrwt_interp",
                     "gcam-usa/A23.elecS_subsector_shrwt_interp_state_adj",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L2234.SubsectorShrwtInterp_elecS_USA

    L2234.SubsectorShrwtInterpTo_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors which need a to-value") %>%
      add_legacy_name("L2234.SubsectorShrwtInterpTo_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_subsector_shrwt_interpto",
                     "gcam-usa/A23.elecS_subsector_shrwt_interpto_state_adj",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A10.renewable_resource_delete",
                     "L223.StubTechProd_elec_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.SubsectorShrwtInterpTo_elecS_USA

    L2234.SubsectorShrwt_elecS_USA %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors at points of inflexion") %>%
      add_legacy_name("L2234.SubsectorShrwt_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_subsector_shrwt",
                     "gcam-usa/A23.elecS_subsector_shrwt_state_adj",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A10.renewable_resource_delete",
                     "L223.StubTechProd_elec_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.SubsectorShrwt_elecS_USA

    L2234.StubTechEff_elecS_USA %>%
      add_title("Electricity Load Segments Base Year Efficiencies") %>%
      add_units("unitless") %>%
      add_comments("Efficiencies of multiple load segments coal, oil and gas technologies in base years") %>%
      add_legacy_name("L2234.StubTechEff_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_stubtech_energy_inputs",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L123.eff_R_elec_F_Yh",
                     "L223.StubTechEff_elec_USA") ->
      L2234.StubTechEff_elecS_USA

    L2234.StubTechCapFactor_elecS_solar_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Solar Technologies") %>%
      add_units("unitless") %>%
      add_comments("Capacity factors by state for multiple load segments solar technologies") %>%
      add_legacy_name("L2234.StubTechCapFactor_elecS_solar_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechCapFactor_elec_solar_USA") ->
      L2234.StubTechCapFactor_elecS_solar_USA

    L2234.StubTechCapFactor_elecS_wind_USA %>%
      add_title("State-specific Capacity Factors for Electricity Load Segments Wind Technologies") %>%
      add_units("unitless") %>%
      add_comments("Capacity factors by state for multiple load segments wind technologies") %>%
      add_legacy_name("L2234.StubTechCapFactor_elecS_wind_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechCapFactor_elec_wind_USA") ->
      L2234.StubTechCapFactor_elecS_wind_USA

    L2234.SubsectorShrwtFllt_elecS_grid %>%
      add_title("Electricity Load Segments Grid Region Subsector Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Grid region electricity load segment subsector shareweights") %>%
      add_legacy_name("L2234.SubsectorShrwtFllt_elecS_grid") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_sector") ->
      L2234.SubsectorShrwtFllt_elecS_grid_USA

    L2234.SubsectorShrwtInterp_elecS_grid %>%
      add_title("Electricity Load Segments Grid Region Subsector Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Grid region electricity load segment subsector shareweights that are fixed at calibration values") %>%
      add_legacy_name("L2234.SubsectorShrwtInterp_elecS_grid") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_USA") ->
      L2234.SubsectorShrwtInterp_elecS_grid_USA

    L2234.PassThroughSector_elecS_USA %>%
      add_title("Electricity Load Segments Passthrough Sectors") %>%
      add_units("NA") %>%
      add_comments("The marginal revenue sector is the region's electricity sector; the marginal revenue market is the grid region") %>%
      add_legacy_name("L2234.PassThroughSector_elecS_USA") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_USA") ->
      L2234.PassThroughSector_elecS_USA

    L2234.PassThroughTech_elecS_grid %>%
      add_title("Electricity Load Segments Passthrough Technologies - Grid Region") %>%
      add_units("NA") %>%
      add_comments("Grid region electricity load segment passthrough technologies") %>%
      add_legacy_name("L2234.PassThroughTech_elecS_grid") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_USA") ->
      L2234.PassThroughTech_elecS_grid_USA

    L2234.GlobalTechShrwt_elecS %>%
      add_title("Electricity Load Segments Technology Shareweights") %>%
      add_units("NA") %>%
      add_comments("Electricity load segment technology shareweight assumptions") %>%
      add_legacy_name("L2234.GlobalTechShrwt_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_globaltech_shrwt") ->
      L2234.GlobalTechShrwt_elecS_USA

    L2234.GlobalIntTechShrwt_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Electricity load segment intermittent technology shareweight assumptions") %>%
      add_legacy_name("L2234.GlobalIntTechShrwt_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_globalinttech_shrwt") ->
      L2234.GlobalIntTechShrwt_elecS_USA

    L2234.PrimaryRenewKeyword_elecS %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Technologies") %>%
      add_units("NA") %>%
      add_comments("Mapped to electricity load segment technologies from L223.PrimaryRenewKeyword_elec") %>%
      add_legacy_name("L2234.PrimaryRenewKeyword_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.PrimaryRenewKeyword_elec") ->
      L2234.PrimaryRenewKeyword_elecS_USA

    L2234.PrimaryRenewKeywordInt_elecS %>%
      add_title("Primary Renewable Keywords for Electricity Load Segments Intermittent Technologies") %>%
      add_units("NA") %>%
      add_comments("Mapped to electricity load segment intermittent technologies from L223.PrimaryRenewKeywordInt_elec") %>%
      add_legacy_name("L2234.PrimaryRenewKeywordInt_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.PrimaryRenewKeywordInt_elec") ->
      L2234.PrimaryRenewKeywordInt_elecS_USA

    L2234.AvgFossilEffKeyword_elecS %>%
      add_title("Average Fossil Efficiency Keywords for Electricity Load Segments Technologies") %>%
      add_units("unitless") %>%
      add_comments("Mapped to electricity load segment technologies from L223.AvgFossilEffKeyword_elec") %>%
      add_legacy_name("L2234.AvgFossilEffKeyword_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.AvgFossilEffKeyword_elec") ->
      L2234.AvgFossilEffKeyword_elecS_USA

    L2234.GlobalTechCapital_elecS %>%
      add_title("Electricity Load Segments Technology Capital Costs") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Capital costs of electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechCapital_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechCapital_elec") ->
      L2234.GlobalTechCapital_elecS_USA

    L2234.GlobalIntTechCapital_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Capital Costs") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Capital costs of intermittent lectricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalIntTechCapital_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechCapital_elec") ->
      L2234.GlobalIntTechCapital_elecS_USA

    L2234.GlobalTechOMfixed_elecS %>%
      add_title("Electricity Load Segments Technology Fixed OM Costs") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Fixed OM costs of electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechOMfixed_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechOMfixed_elec") ->
      L2234.GlobalTechOMfixed_elecS_USA

    L2234.GlobalIntTechOMfixed_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Fixed OM Costs") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Fixed OM costs of intermittent electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalIntTechOMfixed_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechOMfixed_elec") ->
      L2234.GlobalIntTechOMfixed_elecS_USA

    L2234.GlobalTechOMvar_elecS %>%
      add_title("Electricity Load Segments Technology Variable OM Costs") %>%
      add_units("1975$/MWh") %>%
      add_comments("Variable OM costs of electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechOMvar_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechOMvar_elec") ->
      L2234.GlobalTechOMvar_elecS_USA

    L2234.GlobalIntTechOMvar_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Variable OM Costs") %>%
      add_units("1975$/MWh") %>%
      add_comments("Variable OM costs of intermittent electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalIntTechOMvar_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechOMvar_elec") ->
      L2234.GlobalIntTechOMvar_elecS_USA

    L2234.GlobalTechCapFac_elecS %>%
      add_title("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_units("unitless") %>%
      add_comments("Global capacity factors for electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechCapFac_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/elecS_time_fraction",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechCapFac_elec") ->
      L2234.GlobalTechCapFac_elecS_USA

    L2234.GlobalTechEff_elecS %>%
      add_title("Electricity Load Segments Technology Efficiencies") %>%
      add_units("unitless") %>%
      add_comments("Efficiencies of electricity load segments generation technologies in future years") %>%
      add_legacy_name("L2234.GlobalTechEff_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalTechEff_elec") ->
      L2234.GlobalTechEff_elecS_USA

    L2234.GlobalIntTechEff_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Efficiencies") %>%
      add_units("unitless") %>%
      add_comments("Efficiencies of intermittent electricity load segments generation technologies in future years") %>%
      add_legacy_name("L2234.GlobalIntTechEff_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechEff_elec") ->
      L2234.GlobalIntTechEff_elecS_USA

    L2234.GlobalTechLifetime_elecS %>%
      add_title("Electricity Load Segments Technology Lifetimes") %>%
      add_units("years") %>%
      add_comments("Lifetimes of electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechLifetime_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechLifetime_elec") ->
      L2234.GlobalTechLifetime_elecS_USA

    L2234.GlobalIntTechLifetime_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Lifetimes") %>%
      add_units("years") %>%
      add_comments("Lifetimes of intermittent electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalIntTechLifetime_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechLifetime_elec") ->
      L2234.GlobalIntTechLifetime_elecS_USA

    L2234.GlobalTechProfitShutdown_elecS %>%
      add_title("Electricity Load Segments Technology Profit Shutdown Decider") %>%
      add_units("unitless") %>%
      add_comments("Profit shutdown decider for electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechProfitShutdown_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalTechProfitShutdown_elec") ->
      L2234.GlobalTechProfitShutdown_elecS_USA

    L2234.GlobalTechSCurve_elecS %>%
      add_title("Electricity Load Segments Technology S-curve Shutdown Decider") %>%
      add_units("years (lifetime); unitless") %>%
      add_comments("S-curve shutdown decider for electricity load segments generation technologies") %>%
      add_legacy_name("L2234.GlobalTechSCurve_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechSCurve_elec") ->
      L2234.GlobalTechSCurve_elecS_USA

    L2234.GlobalTechCapture_elecS %>%
      add_title("Electricity Load Segments CCS Technology Characteristics") %>%
      add_units("unitless") %>%
      add_comments("Characteristics of electricity load segments CCS technologies") %>%
      add_legacy_name("L2234.GlobalTechCapture_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalTechCapture_elec") ->
      L2234.GlobalTechCapture_elecS_USA

    L2234.GlobalIntTechBackup_elecS %>%
      add_title("Electricity Load Segments Intermittent Technology Backup Characteristics") %>%
      add_units("1975$/kW/yr (backup.capital.cost); unitless") %>%
      add_comments("Backup characteristics for electricity load segments intermittent technologies") %>%
      add_legacy_name("L2234.GlobalIntTechBackup_elecS") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.GlobalIntTechBackup_elec") ->
      L2234.GlobalIntTechBackup_elecS_USA

    L2234.StubTechMarket_elecS_USA %>%
      add_title("Energy Inputs for Electricity Load Segments Technologies") %>%
      add_units("NA") %>%
      add_comments("Energy inputs and markets for electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechMarket_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechMarket_elec_USA") ->
      L2234.StubTechMarket_elecS_USA

    L2234.StubTechMarket_backup_elecS_USA %>%
      add_title("Backup Energy Inputs for Electricity Load Segments Intermittent Technologies") %>%
      add_units("NA") %>%
      add_comments("Backup energy inputs for intermittent electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechMarket_backup_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechMarket_backup_USA") ->
      L2234.StubTechMarket_backup_elecS_USA

    L2234.StubTechElecMarket_backup_elecS_USA %>%
      add_title("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_units("NA") %>%
      add_comments("Backup market sector names for intermittent electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechElecMarket_backup_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechMarket_backup_USA") ->
      L2234.StubTechElecMarket_backup_elecS_USA

    L2234.StubTechProd_elecS_USA %>%
      add_title("Electricity Load Segments Technology Calibration Outputs") %>%
      add_units("EJ (cal.Output.value); unitless") %>%
      add_comments("Calibration outputs for electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechProd_elecS_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L119.CapFacScaler_CSP_state",
                     "L223.StubTechProd_elec_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.StubTechProd_elecS_USA

    L2234.StubTechFixOut_elecS_USA %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Calibration year fixed outputs for hydro electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechFixOut_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechFixOut_elec_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.StubTechFixOut_elecS_USA

    L2234.StubTechFixOut_hydro_elecS_USA %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Future year fixed outputs for hydro electricity load segments technologies") %>%
      add_legacy_name("L2234.StubTechFixOut_hydro_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L223.StubTechFixOut_hydro_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.StubTechFixOut_hydro_elecS_USA

    L2234.StubTechCost_offshore_wind_elecS_USA %>%
      add_title("Cost adders for offshore wind grid connection in USA") %>%
      add_units("Unitless") %>%
      add_comments("State-specific non-energy cost adder for offshore wind grid connection cost") %>%
      add_precursors("L223.StubTechCost_offshore_wind_USA") ->
      L2234.StubTechCost_offshore_wind_elecS_USA

    L2234.TechShrwt_elecS_grid %>%
      add_title("Electricity Load Segments Grid Technology Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Electricity load segment grid technology shareweight assumptions") %>%
      add_legacy_name("L2234.TechShrwt_elecS_grid") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_sector") ->
      L2234.TechShrwt_elecS_grid_USA

    L2234.TechCoef_elecS_grid %>%
      add_title("Electricity Load Segments Grid Technology Coefficients") %>%
      add_units("unitless") %>%
      add_comments("Electricity load segment grid technology coefficients") %>%
      add_legacy_name("L2234.TechCoef_elecS_grid") %>%
      same_precursors_as("L2234.TechShrwt_elecS_grid_USA") ->
      L2234.TechCoef_elecS_grid_USA

    L2234.TechProd_elecS_grid %>%
      add_title("Electricity Load Segments Grid Technology Calibration Outputs") %>%
      add_units("EJ (cal.Output.value); unitless") %>%
      add_comments("Calibration outputs for electricity load segments grid technologies") %>%
      add_legacy_name("L2234.TechProd_elecS_grid") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L223.StubTechProd_elec_USA",
                     "L223.StubTechFixOut_elec_USA",
                     "L1239.state_elec_supply_USA") ->
      L2234.TechProd_elecS_grid_USA

    return_data(L2234.Supplysector_elecS_USA,
                L2234.ElecReserve_elecS_USA,
                L2234.SubsectorLogit_elecS_USA,
                L2234.SubsectorShrwtInterp_elecS_USA,
                L2234.SubsectorShrwtInterpTo_elecS_USA,
                L2234.SubsectorShrwt_elecS_USA,
                L2234.StubTechEff_elecS_USA,
                L2234.StubTechCapFactor_elecS_solar_USA,
                L2234.StubTechCapFactor_elecS_wind_USA,
                L2234.SubsectorShrwtFllt_elecS_grid_USA,
                L2234.SubsectorShrwtInterp_elecS_grid_USA,
                L2234.PassThroughSector_elecS_USA,
                L2234.PassThroughTech_elecS_grid_USA,
                L2234.GlobalTechShrwt_elecS_USA,
                L2234.GlobalIntTechShrwt_elecS_USA,
                L2234.PrimaryRenewKeyword_elecS_USA,
                L2234.PrimaryRenewKeywordInt_elecS_USA,
                L2234.AvgFossilEffKeyword_elecS_USA,
                L2234.GlobalTechCapital_elecS_USA,
                L2234.GlobalIntTechCapital_elecS_USA,
                L2234.GlobalTechOMfixed_elecS_USA,
                L2234.GlobalIntTechOMfixed_elecS_USA,
                L2234.GlobalTechOMvar_elecS_USA,
                L2234.GlobalIntTechOMvar_elecS_USA,
                L2234.GlobalTechCapFac_elecS_USA,
                L2234.GlobalTechEff_elecS_USA,
                L2234.GlobalIntTechEff_elecS_USA,
                L2234.GlobalTechLifetime_elecS_USA,
                L2234.GlobalIntTechLifetime_elecS_USA,
                L2234.GlobalTechProfitShutdown_elecS_USA,
                L2234.GlobalTechSCurve_elecS_USA,
                L2234.GlobalTechCapture_elecS_USA,
                L2234.GlobalIntTechBackup_elecS_USA,
                L2234.StubTechMarket_elecS_USA,
                L2234.StubTechMarket_backup_elecS_USA,
                L2234.StubTechElecMarket_backup_elecS_USA,
                L2234.StubTechProd_elecS_USA,
                L2234.StubTechFixOut_elecS_USA,
                L2234.StubTechFixOut_hydro_elecS_USA,
                L2234.StubTechCost_offshore_wind_elecS_USA,
                L2234.TechShrwt_elecS_grid_USA,
                L2234.TechCoef_elecS_grid_USA,
                L2234.TechProd_elecS_grid_USA)

  } else {
    stop("Unknown command")
  }
}
