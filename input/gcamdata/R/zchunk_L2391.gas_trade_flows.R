# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2391.gas_trade_flows
#'
#' Model input for natural gas trade by LNG and regional pipeline networks.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'  \code{command} is "MAKE") all the generated outputs: \code{L2391.NG_export_calOutput_LNG},
#'  \code{L2391.NG_export_calOutput_pipeline}, \code{L2391.NG_import_calOutput_LNG},
#'  \code{L2391.NG_import_calOutput_pipeline},  \code{L2391.NG_import_calOutput_statdiff},
#'  \code{L2391.NG_export_calOutput_statdiff}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tibble tibble
#' @author MTB August 2021
module_energy_L2391.gas_trade_flows <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/GCAM_region_pipeline_bloc_export",
             FILE = "energy/GCAM_region_pipeline_bloc_import",
             "L1011.ff_GrossTrade_EJ_R_Y_LNG",
             "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
             "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe",
             "L239.Production_tra",
             "L239.Production_reg_imp"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2391.NG_export_calOutput_LNG",
             "L2391.NG_export_calOutput_pipeline",
             "L2391.NG_import_calOutput_LNG",
             "L2391.NG_import_calOutput_pipeline",
             "L2391.NG_import_calOutput_statdiff",
             "L2391.NG_export_calOutput_statdiff"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_Commodity <- GrossExp_EJ <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_EJ <- Prod_EJ <- fuel <- technology <- primary.consumption <- PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <-
      production <- consumption <- GCAM_region_ID <- NULL # silence package check notes


    # ----------------------------------------
    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    GCAM_region_pipeline_bloc_export <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_export", strip_attributes = TRUE)
    GCAM_region_pipeline_bloc_import <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_import", strip_attributes = TRUE)
    L1011.ff_GrossTrade_EJ_R_Y_LNG <- get_data(all_data, "L1011.ff_GrossTrade_EJ_R_Y_LNG", strip_attributes = TRUE)
    L1011.ff_GrossTrade_EJ_R_Y_NG_pipe <- get_data(all_data, "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe", strip_attributes = TRUE)
    L1011.ff_BilatTrade_EJ_R_Y_NG_pipe <- get_data(all_data, "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe", strip_attributes = TRUE)
    L239.Production_tra <- get_data(all_data, "L239.Production_tra", strip_attributes = TRUE) %>%
      filter(grepl("gas", supplysector))
    L239.Production_reg_imp <- get_data(all_data, "L239.Production_reg_imp", strip_attributes = TRUE) %>%
      filter(grepl("gas", supplysector))


    # ----------------------------------------
    # Process data

    # There are a lot of pieces to balance here.  We have to maintain balance of:
    # - regional natural gas imports and exports (across carriers)
    # - global LNG imports and exports
    # - pipeline exports and imports, globally and for inter-regional pipeline networks
    # Expanded natural gas trade structure has greatest detail in terms of gas pipeline networks,
    # so first calculate import and export shares by pipeline network.
    # Second, disaggreate / balance imports data (between pipeline and LNG, and between pipeline networks)
    # because it's the more complex piece (regions can import from multiple pipeline networks).
    # Third, use share out exports (between pipeline and LNG) in a manner consistent with import data.
    # Finally, re-balance regional exports with a statistical differences sector (explained below)
    # to ensure that everything is balanced out.


    # STEP 1:  Pipeline shares
    # Pipeline import shares by pipeline market
    # Regions are permitted to import from multiple pipeline markets
    L1011.ff_BilatTrade_EJ_R_Y_NG_pipe %>%
      group_by(region = destination.region, year, GCAM_Commodity, GCAM_Commodity_traded, pipeline.market) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      group_by(region, year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      # COMPTRADE data (L1011) only extends back to 2007
      # create complete set of historical years and carry 2007 shares backwards
      complete(nesting(region, GCAM_Commodity, GCAM_Commodity_traded, pipeline.market), year = MODEL_BASE_YEARS) %>%
      group_by(region, GCAM_Commodity, GCAM_Commodity_traded, pipeline.market) %>%
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup() %>%
      select(-value) -> L2391.NG_pipeline_import_shares


    # STEP 2:  Import shares
    # Start with share between pipeline and LNG
    # Combine pipeline & LNG imports, calculate shares
    L1011.ff_GrossTrade_EJ_R_Y_LNG %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      bind_rows(L1011.ff_GrossTrade_EJ_R_Y_NG_pipe) %>%
      group_by(region, year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(GrossImp_EJ = sum(GrossImp_EJ)) %>%
      ungroup() %>%
      group_by(region, year, GCAM_Commodity) %>%
      mutate(share = GrossImp_EJ / sum(GrossImp_EJ)) %>%
      ungroup() %>%
      # COMPTRADE data (L1011) only extends back to 2007
      # create complete set of historical years and carry 2007 shares backwards
      complete(nesting(region, GCAM_Commodity, GCAM_Commodity_traded), year = MODEL_BASE_YEARS) %>%
      group_by(region, GCAM_Commodity, GCAM_Commodity_traded) %>%
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup() %>%
      select(-GrossImp_EJ) %>%
      filter(year %in% MODEL_BASE_YEARS) -> L2391.NG_import_shares

    # Partition calibrated imports by region between pipeline & LNG
    # Join calibrated gross NG trade and shares, calculate calibrated value by trade vehicle
    L2391.NG_import_shares%>%
      left_join_error_no_match(L239.Production_reg_imp %>%
                                 select(region, year, calOutputValue) ,
                               by = c("region", "year")) %>%
      mutate(calOutputValue = share * calOutputValue) -> L2391.NG_import_calOutput

    # Regional imports by LNG
    L2391.NG_import_calOutput %>%
      filter(GCAM_Commodity_traded == "LNG") %>%
      select(-share) -> L2391.NG_import_calOutput_LNG

    # Split out regional imports by pipeline network
    # Map in pipeline market shares to get regional pipeline exports by market
    # (Again, the default assumption is that regions are permitted to import from multiple pipeline markets)
    L2391.NG_import_calOutput %>%
      filter(GCAM_Commodity_traded == "gas pipeline") %>%
      select(-share) %>%
      # join in pipeline market info;
      # join will duplicate rows because regions are permitted to import from multiple pipeline markets
      # LJENM will throw error, so left_join is used
      left_join(GCAM_region_pipeline_bloc_import %>%
                  select(region, pipeline.market),
                by = c("region")) %>%
      # regions with no pipeline exports won't have data points in L2391.NG_pipeline_import_shares
      # this creates NAs when these tables are joined; LJENM throws an error, so left_join is used
      left_join(L2391.NG_pipeline_import_shares,
                by = c("region", "GCAM_Commodity", "GCAM_Commodity_traded",
                       "pipeline.market", "year")) %>%
      replace_na(list(share = 0)) %>%
      mutate(calOutputValue = share * calOutputValue) %>%
      select(-share) -> L2391.NG_import_calOutput_pipeline

    # Summarise global LNG imports
    L2391.NG_import_calOutput_LNG %>%
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(global_LNG_exp = sum(calOutputValue)) %>%
      ungroup() -> L2391.NG_import_calOutput_LNG_global

    # Summarise gas pipeline imports globally and by regional pipeline network
    L2391.NG_import_calOutput_pipeline %>%
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded, pipeline.market) %>%
      summarise(regional_pipe_exp = sum(calOutputValue)) %>%
      ungroup() -> L2391.NG_import_calOutput_pipeline_network


    # STEP 3:  Export shares
    # Start with share between pipeline and LNG
    # Combine pipeline & LNG exports, calculate shares
    L1011.ff_GrossTrade_EJ_R_Y_LNG %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      bind_rows(L1011.ff_GrossTrade_EJ_R_Y_NG_pipe) %>%
      group_by(region, year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(GrossExp_EJ = sum(GrossExp_EJ)) %>%
      ungroup() %>%
      group_by(region, year, GCAM_Commodity) %>%
      mutate(share = GrossExp_EJ / sum(GrossExp_EJ)) %>%
      ungroup() %>%
      # COMPTRADE data (L1011) only extends back to 2007
      # create complete set of historical years and carry 2007 shares backwards
      complete(nesting(region, GCAM_Commodity, GCAM_Commodity_traded), year = MODEL_BASE_YEARS) %>%
      group_by(region, GCAM_Commodity, GCAM_Commodity_traded) %>%
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup() %>%
      select(-GrossExp_EJ) %>%
      filter(year %in% MODEL_BASE_YEARS) -> L2391.NG_export_shares

    # Format traded data - exporting region is actually embedded in technology name
    L239.Production_tra %>%
      select(-region) %>%
      mutate(region = gsub(" traded natural gas", "", technology)) %>%
      select(region, year, calOutputValue) -> L2391.Production_tra

    # Partition calibrated exports by region between pipeline & LNG
    # Join calibrated gross NG trade and shares, calculate calibrated value by trade vehicle
    L2391.NG_export_shares %>%
      left_join_error_no_match(L2391.Production_tra, by = c("region", "year")) %>%
      mutate(calOutputValue = share * calOutputValue) -> L2391.NG_export_unadjusted

    # Join in import data and scale export data so import and export totals match at relevant scales
    # (global for LNG, inter-regional network for pipeline)
    # LNG
    L2391.NG_export_unadjusted %>%
      filter(GCAM_Commodity_traded == "LNG") %>%
      left_join_error_no_match(L2391.NG_import_calOutput_LNG_global,
                               by = c("GCAM_Commodity", "GCAM_Commodity_traded", "year")) %>%
      # summarize global LNG exports by year
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      mutate(global_LNG_imp = sum(calOutputValue)) %>%
      ungroup() %>%
      # scale calOutputValue to ensure that global LNG exports = global LNG imports
      mutate(calOutputValue = calOutputValue * (global_LNG_exp / global_LNG_imp)) %>%
      # test that scaling worked
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      mutate(global_LNG_imp = sum(calOutputValue)) %>%
      ungroup() %>%
      select(region, GCAM_Commodity, GCAM_Commodity_traded, year, calOutputValue) ->
      L2391.NG_export_calOutput_LNG

    # Pipeline
    L2391.NG_export_unadjusted %>%
      filter(GCAM_Commodity_traded == "gas pipeline") %>%
      # join in pipeline market info
      left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("region" = "origin.region")) %>%
      left_join_error_no_match(L2391.NG_import_calOutput_pipeline_network,
                               by = c("GCAM_Commodity", "GCAM_Commodity_traded", "year", "pipeline.market")) %>%
      # summarize pipeline exports by year and network
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded, pipeline.market) %>%
      mutate(regional_pipe_imp = sum(calOutputValue)) %>%
      ungroup() %>%
      # scale calOutputValue to ensure that exports = imports for each network
      mutate(calOutputValue = calOutputValue * (regional_pipe_exp / regional_pipe_imp)) %>%
      select(region, GCAM_Commodity, GCAM_Commodity_traded, year, calOutputValue, pipeline.market) ->
      L2391.NG_export_calOutput_pipeline


    # STEP 4:  Check trade balances (across scales) and make final adjustments
    # Combine flows with a single market (LNG imports and exports, pipeline exports)
    L2391.NG_export_calOutput_pipeline %>%
      select(region, year, pipeline.market, calExport_pipe = calOutputValue) %>%
      left_join_error_no_match(L2391.NG_export_calOutput_LNG %>%
                                 select(region, year, calExport_LNG = calOutputValue),
                               by = c("region", "year")) %>%
      left_join_error_no_match(L2391.NG_import_calOutput_LNG %>%
                                 select(region, year, calImport_LNG = calOutputValue),
                               by = c("region", "year")) -> L2391.LNG.exp.imp_pipe.exp

    # Add in pipeline imports (which can come from multiple pipeline networks) as well as
    # region total imports and exports which we need to match
    L2391.NG_import_calOutput_pipeline %>%
      select(region, year, pipeline.market, calImport_pipe = calOutputValue) %>%
      # join will produce NAs because we don't want to duplicate LNG and pipeline export values
      # by multiple import pipelines by region. LJENM throws error; left_join is used and
      # NAs dealt with below
      left_join(L2391.LNG.exp.imp_pipe.exp,
                by = c("region", "year", "pipeline.market")) %>%
      replace_na(list(calExport_pipe = 0,
                      calExport_LNG = 0,
                      calImport_LNG = 0)) %>%
      left_join_error_no_match(L2391.Production_tra %>%
                                 select(region, year, reg_NG_exports = calOutputValue),
                               by = c("region", "year")) %>%
      left_join_error_no_match(L239.Production_reg_imp %>%
                                 select(region, year, reg_NG_imports = calOutputValue),
                               by = c("region", "year")) -> L2391.LNG_pipe

    # Check that all flows are balanced.  At this point this is true for all flows except for regional exports.
    # We'll calculate all balances here anyway for debugging purposes.  We're checking:
    # reg_NG_imports:  sum of LNG and pipeline imports (should match calibrated value from L239 table)
    # reg_NG_exports:  sum of LNG and pipeline exports (should match calibrated value from L239 table)
    # pipe_ntwrk: balance of imports and exports for each pipeline network (imports and exports should balance)
    # global_LNG = global balance of LNG imports and exports (imports and exports should balance)
    # global_pipe:  global balance of pipeline inports and exports (imports and exports should balance)
    L2391.LNG_pipe %>%
      # total NG imports and exports
      group_by(region, year) %>%
      mutate(reg_NG_imports_check = sum(calImport_pipe) + sum(calImport_LNG),
             reg_NG_imports_diff = round(reg_NG_imports - reg_NG_imports_check, energy.DIGITS_CALOUTPUT),
             reg_NG_exports_check = sum(calExport_pipe) + sum(calExport_LNG),
             reg_NG_exports_diff = round(reg_NG_exports - reg_NG_exports_check, energy.DIGITS_CALOUTPUT)) %>%
      ungroup() %>%
      # regional gas pipelines
      group_by(pipeline.market, year) %>%
      mutate(pipe_ntwrk_imp = sum(calImport_pipe),
             pipe_ntwrk_exp = sum(calExport_pipe),
             pipe_ntwrk_diff = round(pipe_ntwrk_imp - pipe_ntwrk_exp, energy.DIGITS_CALOUTPUT)) %>%
      ungroup() %>%
      # global totals by vehicle
      group_by(year) %>%
      mutate(global_LNG_imp = sum(calImport_LNG),
             global_LNG_exp = sum(calExport_LNG),
             global_LNG_diff = round(global_LNG_imp - global_LNG_exp, energy.DIGITS_CALOUTPUT),
             global_pipe_imp = sum(calImport_pipe),
             global_pipe_exp = sum(calExport_pipe),
             global_pipe_diff = round(global_pipe_imp - global_pipe_exp, energy.DIGITS_CALOUTPUT)) %>%
      ungroup() -> L2391.gas_flow_balances

    # At this point everything is balanced except for regional exports.
    # This is because GCAM's processing of the IEA data largely ignores stock changes
    # (which are balanced globally but not at the regional / pipeline network level).
    # so region X could export quantity Q to region Y, but some portion of Q increases stocks
    # in region Y, rather than being consumed.  So region X production and exports are correct,
    # region Y imports and consumption are correct, but they don't balance because of the stock changes.
    # To correct this, we'll create a "gas trade statistical differences" market where regions which
    # don't export enough (positive reg_NG_exports_diff) supply and regions which export too much
    # (negative reg_NG_exports_diff) demand.  The latter regions will have their LNG exports reduced
    # by the amount of the difference, and the global LNG market will import that amount from the
    # "gas trade statistical differences" market.
    L2391.gas_flow_balances %>%
      distinct(region, year, reg_NG_exports_diff) -> L2391.gas_export_diff

    # Regions that export more than allocated to pipeline & LNG
    L2391.gas_export_diff %>%
      filter(reg_NG_exports_diff > 0) %>%
      complete(nesting(region), year = MODEL_BASE_YEARS) %>%
      replace_na(list(reg_NG_exports_diff = 0)) %>%
      rename(calOutputValue = reg_NG_exports_diff) %>%
      mutate(sector = "gas trade statistical differences",
             subsector = "statistical differences",
             technology = paste0(region, " statistical differences"),
             minicam.energy.input = "natural gas",
             market.name = region,
             region = gcam.USA_REGION) -> L2391.NG_export_calOutput_statdiff

    # Regions that have excess LNG + pipeline exports
    L2391.gas_export_diff %>%
      filter(reg_NG_exports_diff < 0) %>%
      complete(nesting(region), year = MODEL_BASE_YEARS) %>%
      replace_na(list(reg_NG_exports_diff = 0)) %>%
      rename(value = reg_NG_exports_diff) %>%
      mutate(value = abs(value)) -> L2391.NG_import_calOutput_statdiff

    # Divide export difference for each region between LNG & pipelines according to historical shares,
    # and then adjust the corresponding calOutputValues accordingly.  Thiss will keep any region's
    # exports from going negative and keep LNG / pipeline shares relatively constant.
    L2391.NG_import_calOutput_statdiff %>%
      left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("region" = "origin.region")) %>%
      # L2391.NG_export_shares contains shares for LNG and pipeline
      # join will duplicate rows by each carrier
      # LJENM will error, left_join() is used
      left_join(L2391.NG_export_shares, by = c("region", "year")) %>%
      mutate(value = value * share) %>%
      select(-share) -> L2391.NG_import_calOutput_adj

    # Adjust regional export values by gas pipeline network
    L2391.NG_export_calOutput_pipeline %>%
      # not all regions are included in L2391.NG_import_calOutput_adj, so join produces NAs.
      # LJENM throws error, so left_join is used and NAs are dealt with below
      left_join(L2391.NG_import_calOutput_adj,
                by = c("region", "GCAM_Commodity", "GCAM_Commodity_traded", "year", "pipeline.market")) %>%
      replace_na(list(value = 0)) %>%
      mutate(calOutputValue = calOutputValue - value) %>%
      select(-value) -> L2391.NG_export_calOutput_pipeline

    # Adjust regional LNG export values
    L2391.NG_export_calOutput_LNG %>%
      # not all regions are included in L2391.NG_import_calOutput_adj, so join produces NAs.
      # LJENM throws error, so left_join is used and NAs are dealt with below
      left_join(L2391.NG_import_calOutput_adj %>%
                  select(-pipeline.market),
                by = c("region", "GCAM_Commodity", "GCAM_Commodity_traded", "year")) %>%
      replace_na(list(value = 0)) %>%
      mutate(calOutputValue = calOutputValue - value) %>%
      select(-value) -> L2391.NG_export_calOutput_LNG

    # Summarize regional pipeline / global LNG consumption from statistical differences sector
    L2391.NG_import_calOutput_adj %>%
      filter(GCAM_Commodity_traded == "gas pipeline") %>%
      group_by(year, pipeline.market, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(technology = "statistical differences",
             minicam.energy.input = "gas trade statistical differences",
             market.name = gcam.USA_REGION) -> L2391.NG_import_calOutput_statdiff_pipe

    L2391.NG_import_calOutput_adj %>%
      filter(GCAM_Commodity_traded == "LNG") %>%
      group_by(year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(technology = "statistical differences",
             minicam.energy.input = "gas trade statistical differences",
             market.name = gcam.USA_REGION) -> L2391.NG_import_calOutput_statdiff_LNG

    L2391.NG_import_calOutput_statdiff_pipe %>%
      bind_rows(L2391.NG_import_calOutput_statdiff_LNG) %>%
      rename(calOutputValue = value) -> L2391.NG_import_calOutput_statdiff

    # # Checking global totals by year
    # L2391.NG_export_calOutput_LNG %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_export_calOutput_LNG_global
    #
    # L2391.NG_export_calOutput_pipeline %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_export_calOutput_pipeline_global
    #
    # L2391.NG_import_calOutput_LNG %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_import_calOutput_LNG_global
    #
    # L2391.NG_import_calOutput_pipeline %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_import_calOutput_pipeline_global
    #
    # L2391.NG_import_calOutput_statdiff %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_import_calOutput_statdiff_global
    #
    # L2391.NG_export_calOutput_statdiff %>%
    #   group_by(year) %>%
    #   summarise(value = sum(calOutputValue)) %>%
    #   ungroup() -> L2391.NG_export_calOutput_statdiff_global


    # ----------------------------------------
    # Produce outputs

    L2391.NG_export_calOutput_LNG %>%
      add_title("Technology calibration for LNG export") %>%
      add_units("EJ") %>%
      add_comments("Historical regional exports of natural gas via LNG") %>%
      add_precursors("common/GCAM_region_names",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_tra") ->
      L2391.NG_export_calOutput_LNG

    L2391.NG_export_calOutput_pipeline %>%
      add_title("Technology calibration for gas pipeline export") %>%
      add_units("EJ") %>%
      add_comments("Regional exports of natural gas via pipeline") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/GCAM_region_pipeline_bloc_export",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_tra") ->
      L2391.NG_export_calOutput_pipeline

    L2391.NG_import_calOutput_LNG %>%
      add_title("Technology calibration for LNG import") %>%
      add_units("EJ") %>%
      add_comments("Regional imports of natural gas via LNG") %>%
      add_precursors("common/GCAM_region_names",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_reg_imp") ->
      L2391.NG_import_calOutput_LNG

    L2391.NG_import_calOutput_pipeline %>%
      add_title("Technology calibration for gas pipeline import") %>%
      add_units("EJ") %>%
      add_comments("Regional imports of natural gas via pipeline") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/GCAM_region_pipeline_bloc_import",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_reg_imp") ->
      L2391.NG_import_calOutput_pipeline

    L2391.NG_export_calOutput_statdiff %>%
      add_title("Calibration values for supply to statistical differences sector") %>%
      add_units("EJ") %>%
      add_comments("Regions here have greater exports/year implied by IEA data compared to COMPTRADE") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/GCAM_region_pipeline_bloc_import",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_reg_imp") ->
      L2391.NG_export_calOutput_statdiff

    L2391.NG_import_calOutput_statdiff %>%
      add_title("Calibration values for consumption of statistical differences in historical LNG and regional pipeline sectors") %>%
      add_units("EJ") %>%
      add_comments("Regions here have fewer exports implied by IEA data compared to COMPTRADE") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/GCAM_region_pipeline_bloc_import",
                     "L1011.ff_GrossTrade_EJ_R_Y_LNG",
                     "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
                     "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe",
                     "L239.Production_reg_imp") ->
      L2391.NG_import_calOutput_statdiff


    return_data(L2391.NG_export_calOutput_LNG,
                L2391.NG_export_calOutput_pipeline,
                L2391.NG_import_calOutput_LNG,
                L2391.NG_import_calOutput_pipeline,
                L2391.NG_import_calOutput_statdiff,
                L2391.NG_export_calOutput_statdiff)
  } else {
    stop("Unknown command")
  }
}
