# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.NationalAccounts
#'
#' Preprocess PWT data for National accounts.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.PWT_nationalAccounts}.
#' @details Select national accounts data from Penn World Table for all countries.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020 xz 2025
#'
module_socio_L100.NationalAccounts <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/NationalAccounts/pwt1001",
      FILE = "socioeconomics/NationalAccounts/NationalAccounts_variable_mapping",
      FILE = "socioeconomics/NationalAccounts/GMD_2025_03",
      "L100.GTAP_capital_stock")

  MODULE_OUTPUTS <-
    c("L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh",
      "L100.National_Accounts_Metrics_R_Yh",
      "L100.National_Accounts_Employment_Share_POP_R_Yh",
      "L100.National_Accounts_Depreciation_Rate_R_Yh",
      "L100.National_Accounts_En_capital_inv_share_R_Yh")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- NULL

    # 1. Read data

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 0. Help function to pull data from different sources ----
    ## given the variables and datasets defined in NationalAccounts_variable_mapping
    pull_accounts <- function(.ds_name, .lastyear = 2023){

      # get variable mapping
      NationalAccounts_variable_mapping %>%
        filter(dataset == .ds_name) %>%
        distinct(variable_code, variable_name) ->
        variable_mapping

      assertthat::assert_that(variable_mapping$variable_code %in% names(get(.ds_name)) %>% all,
                              msg = "Check NationalAccounts_variable_mapping to ensure data include all required variable_code")

      rename_map <- setNames(variable_mapping$variable_name,
                             variable_mapping$variable_code)

      get(.ds_name) %>%
        filter(year >= min(HISTORICAL_YEARS), year <= .lastyear) %>%
        select(any_of(names(rename_map))) %>%
        dplyr::rename_with(~ rename_map[.x], .cols = everything()) %>%
        gather(var, value, -iso, -year) %>%
        mutate(iso = tolower(iso))
    }


    # 1 process GMD data to get GDP decomposition shares ----
    # C (& G) + I + X - M = nGDP
    # we calculate nominal shares relative to GDP at GCAM region levels
    pull_accounts(.ds_name = "GMD_2025_03",
                  .lastyear = socioeconomics.Global_Macro_Database_LastYear) %>%
      filter(!is.na(value)) %>%
      spread(var, value) %>%
      na.omit %>%
      # convert local currencies to USD
      mutate(across(c(cons.gmd, inv.gmd, exports.gmd, imports.gmd),
                    ~ .x / ExUSD.gmd)) %>%
      select(-ExUSD.gmd) %>%
      mutate(ngdp.gmd = cons.gmd + inv.gmd + exports.gmd - imports.gmd) %>%
      # csk was dissolved later
      filter(iso != "csk") %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x, na.rm = TRUE)
      ), .groups = "drop") ->
      GDP_decomp_C_I_X_M_shares_R_Yh_0

    GDP_decomp_C_I_X_M_shares_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      # note that there was no Taiwan after 2000
      # no Russia or Ukraine before 1990
      # we simply filld them be extending the data
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      gather(var, value, -GCAM_region_ID, -year) %>%
      # calcualte shares
      group_by(GCAM_region_ID, year) %>%
      mutate(value = value / value [var == "ngdp.gmd"]) %>%
      filter(var != "ngdp.gmd") %>%
      ungroup ->
      L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh

    assertthat::assert_that(L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh$value %>% min > 0,
                            msg = "check min value in data")
    assertthat::assert_that(L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")

    L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh %>%
      add_title("GDP decomposition shares by C, I, X, and M at GCAM region levels") %>%
      add_units("share") %>%
      add_comments("Processed using GMD data") %>%
      add_legacy_name("L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "socioeconomics/NationalAccounts/NationalAccounts_variable_mapping",
                     "socioeconomics/NationalAccounts/GMD_2025_03") ->
      L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh


    # 2  process employment share and depreciation rate from pwt v1001 ----
    ## data is available up to 2019 (Socioeconomic.PWT.LastYear)

    ## 2.1 employment share ----

    pull_accounts(.ds_name = "pwt1001", Socioeconomic.PWT.LastYear) %>%
      filter(var %in% c("emp.pwt", "pop.pwt")) %>%
      spread(var, value) %>%
      na.omit %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x)
      ), .groups = "drop") ->
      Employment_Share_POP_R_Yh_0

    Employment_Share_POP_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      mutate(employed.share.pwt = emp.pwt / pop.pwt) %>%
      select(-emp.pwt, -pop.pwt) %>%
      gather(var, value, employed.share.pwt)->
      L100.National_Accounts_Employment_Share_POP_R_Yh


    assertthat::assert_that(L100.National_Accounts_Employment_Share_POP_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")
    # adding assertions to ensure the values are bounded in [0.25, 0.8] based on recent observations. Similar assertions are used later
    # to flag any potential issues in future data updates.
    assertthat::assert_that(L100.National_Accounts_Employment_Share_POP_R_Yh %>% filter(year == Socioeconomic.PWT.LastYear) %>%
                              pull(value) %>% min > 0.25, msg = "check min value in data")
    assertthat::assert_that(L100.National_Accounts_Employment_Share_POP_R_Yh %>% filter(year == Socioeconomic.PWT.LastYear) %>%
                              pull(value) %>% max < 0.8, msg = "check max value in data")

    L100.National_Accounts_Employment_Share_POP_R_Yh %>%
      add_title("Employment shares over population at GCAM region levels") %>%
      add_units("share") %>%
      add_comments("Processed using PWT data with years up to 2019") %>%
      add_legacy_name("L100.National_Accounts_Employment_Share_POP_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "socioeconomics/NationalAccounts/NationalAccounts_variable_mapping",
                     "socioeconomics/NationalAccounts/pwt1001") ->
      L100.National_Accounts_Employment_Share_POP_R_Yh


    ## 2.2 depreciation rates ----
    pull_accounts(.ds_name = "pwt1001", Socioeconomic.PWT.LastYear) %>%
      filter(var %in% c("dep.rate.pwt", "capital.stock.pwt")) %>%
      spread(var, value) %>%
      na.omit %>%
      mutate(depreciation = dep.rate.pwt * capital.stock.pwt) %>%
      select(-dep.rate.pwt) %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x)
      ), .groups = "drop") ->
      Depreciation_Rate_R_Yh_0

    Depreciation_Rate_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      mutate(depreciation.rate.pwt = depreciation / capital.stock.pwt) %>%
      select(-capital.stock.pwt, -depreciation) %>%
      gather(var, value, depreciation.rate.pwt)->
      L100.National_Accounts_Depreciation_Rate_R_Yh

    assertthat::assert_that(L100.National_Accounts_Depreciation_Rate_R_Yh$value %>% min > 0.01, msg = "check min value in data")
    assertthat::assert_that(L100.National_Accounts_Depreciation_Rate_R_Yh$value %>% max < 0.1, msg = "check max value in data")
    assertthat::assert_that(L100.National_Accounts_Depreciation_Rate_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")

    L100.National_Accounts_Depreciation_Rate_R_Yh %>%
      add_title("Depreciation rate at GCAM region levels") %>%
      add_units("share") %>%
      add_comments("Processed using PWT data with years up to 2019") %>%
      add_legacy_name("L100.National_Accounts_Depreciation_Rate_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "socioeconomics/NationalAccounts/NationalAccounts_variable_mapping",
                     "socioeconomics/NationalAccounts/pwt1001") ->
      L100.National_Accounts_Depreciation_Rate_R_Yh

    # 3  process other national account shares from pwt v1001 ----

    ## 3.1 derive capital-output ratio using PWT ----
    # PWT gdp (rgdpna or gdp.pwt) and capital stock (rnna or capital.stock.pwt)
    # both are in constant prices and in national prices
    # they are not in MER; we will derive a ratio and apply it to MER GDP from
    # other sources later to get capital stock in MER

    pull_accounts(.ds_name = "pwt1001", Socioeconomic.PWT.LastYear) %>%
      filter(var %in% c("gdp.pwt", "capital.stock.pwt")) %>%
      spread(var, value) %>%
      na.omit %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x)
      ), .groups = "drop") ->
      Capital_GDP_Ratio_R_Yh_0

    Capital_GDP_Ratio_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      mutate(capital_GDP_ratio.pwt = capital.stock.pwt / gdp.pwt) %>%
      select(-capital.stock.pwt, -gdp.pwt) %>%
      gather(var, value, capital_GDP_ratio.pwt)->
      Capital_GDP_Ratio_R_Yh

    # adding assertions to ensure the values are bounded in [0.8, 5] based on recent observations.
    assertthat::assert_that(Capital_GDP_Ratio_R_Yh$value %>% min > 0.8, msg = "check min value in data")
    assertthat::assert_that(Capital_GDP_Ratio_R_Yh$value %>% max > 5, msg = "check max value in data")
    assertthat::assert_that(Capital_GDP_Ratio_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")

    ## 3.2 derive/aggregate labor compensation share in GDP using PWT ----

    pull_accounts(.ds_name = "pwt1001", Socioeconomic.PWT.LastYear) %>%
      filter(var %in% c("gdp.pwt", "labor.share.pwt")) %>%
      spread(var, value) %>%
      na.omit  %>%
      mutate(labor.compensation = gdp.pwt * labor.share.pwt) %>%
      select(-labor.share.pwt) %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x)
      ), .groups = "drop") ->
      Labor_Compensation_Share_R_Yh_0

    Labor_Compensation_Share_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      mutate(labor.share.pwt = labor.compensation / gdp.pwt) %>%
      select(-labor.compensation, -gdp.pwt) %>%
      gather(var, value, labor.share.pwt)->
      Labor_Compensation_Share_R_Yh

    # Pakistan was missing; replace with India data
    if (Labor_Compensation_Share_R_Yh %>%
        anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% pull(region) == "Pakistan") {

      Labor_Compensation_Share_R_Yh %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(region == "India") %>%
        mutate(region = "Pakistan") %>%
        select(-GCAM_region_ID) %>%
        left_join_error_no_match(GCAM_region_names, by = "region") %>%
        select(-region) %>%
        # add back non Pakistan regions
        bind_rows(Labor_Compensation_Share_R_Yh) ->
        Labor_Compensation_Share_R_Yh
    }


    assertthat::assert_that(Labor_Compensation_Share_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")
    # adding assertions to ensure the values are bounded in [0.2, 0.8] based on recent observations.
    assertthat::assert_that(Labor_Compensation_Share_R_Yh$value %>% min > 0.2, msg = "check min value in data")
    assertthat::assert_that(Labor_Compensation_Share_R_Yh$value %>% max < 0.8, msg = "check max value in data")



    ## 3.3 derive/aggregate capital reward share in GDP using PWT IRR ----

    pull_accounts(.ds_name = "pwt1001", Socioeconomic.PWT.LastYear) %>%
      filter(var %in% c("gdp.pwt", "irr.pwt", "capital.stock.pwt")) %>%
      spread(var, value) %>%
      na.omit %>%
      mutate(capital.reward = irr.pwt * capital.stock.pwt) %>%
      select(-irr.pwt) %>%
      # join GCAM region ID and aggregate
      # Our assumptions on region mappings are simple here since we produce shares
      left_join_error_no_match(
        iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = c("iso")
      ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(across(
        .cols = -any_of(c("iso", "year", "GCAM_region_ID")),
        .fns = ~ sum(.x)
      ), .groups = "drop") ->
      Capital_Compensation_Share_R_Yh_0

    Capital_Compensation_Share_R_Yh_0 %>%
      # filter out negative value years (will be filled later)
      filter(if_all(where(is.numeric), ~ .x >= 0)) %>%
      # complete years
      complete(GCAM_region_ID, year = tidyr::full_seq(year, 1) ) %>%
      fill(everything(), .direction = "downup") %>%
      ungroup %>%
      mutate(capital.share.pwt = capital.reward / gdp.pwt,
             irr.pwt = capital.reward / capital.stock.pwt) %>%
      select(-capital.stock.pwt, -gdp.pwt, -capital.reward) %>%
      gather(var, value, capital.share.pwt, irr.pwt)->
      Capital_Compensation_Share_R_Yh

    # Pakistan was missing for both variables; replace with India data
    if (Capital_Compensation_Share_R_Yh %>%
        anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% pull(region) == "Pakistan") {

      Capital_Compensation_Share_R_Yh %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(region == "India") %>%
        mutate(region = "Pakistan") %>%
        select(-GCAM_region_ID) %>%
        left_join_error_no_match(GCAM_region_names, by = "region") %>%
        select(-region) %>%
        # add back non Pakistan regions
        bind_rows(Capital_Compensation_Share_R_Yh) ->
        Capital_Compensation_Share_R_Yh
    }



    ## Updated 3.3 derive/aggregate capital reward share in GDP using PWT IRR ----
    # We use 1 - labor share = capital share  (land is in capital)

    Labor_Compensation_Share_R_Yh %>%
      mutate(var = "capital.share.pwt",
             value = 1 - value) ->
      Capital_Compensation_Share_R_Yh

    # only capital.share.pwt will be used though irr.pwt is retained
    assertthat::assert_that(Capital_Compensation_Share_R_Yh %>%filter(var == "capital.share.pwt") %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")
    # adding assertions to ensure the values are bounded in [0.15, 0.7] based on recent observations.
    assertthat::assert_that(Capital_Compensation_Share_R_Yh %>% filter(year == Socioeconomic.PWT.LastYear) %>%
                              filter(var == "capital.share.pwt") %>% pull(value) %>% min > 0.15, msg = "check min value in data")
    assertthat::assert_that(Capital_Compensation_Share_R_Yh %>% filter(year == Socioeconomic.PWT.LastYear) %>%
                              filter(var == "capital.share.pwt") %>% pull(value) %>% max < 0.7, msg = "check max value in data")


    ## Bind all national account shares ----
    Capital_GDP_Ratio_R_Yh %>%
      bind_rows(Labor_Compensation_Share_R_Yh) %>%
      bind_rows(Capital_Compensation_Share_R_Yh) ->
      L100.National_Accounts_Metrics_R_Yh

    L100.National_Accounts_Metrics_R_Yh %>%
      add_title("National accounts metrics at GCAM region levels") %>%
      add_units("share") %>%
      add_comments("Variables including capital-output-ratio, labor-compensation-share, and capital-compensation-sahre,
                   processed using PWT data with years up to 2019.") %>%
      add_legacy_name("L100.National_Accounts_Metrics_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/NationalAccounts/NationalAccounts_variable_mapping",
                     "socioeconomics/NationalAccounts/pwt1001") ->
      L100.National_Accounts_Metrics_R_Yh



    # 4. derive shares for partitioning the total capital stock and investment by (energy) sector ----

    # We want to partition the total capital stock and investment to split out energy capital usage
    # The Penn World table does not include this level of detail so we will need to utilize GTAP
    # capital data to do this.
    L100.GTAP_capital_stock %>%
      group_by(region_GCAM, year) %>%
      # calculate regional total
      mutate(INV = VKE - VKB + VDEP,
             invest_total = sum(INV),
             stock_total = sum(VKE),
             CapitalCost_total = sum(CapitalCost)) %>%
      # calculate sector shares by region
      group_by(region_GCAM, year, GCAM_sector) %>%
      summarize(en_inv_share.gtap = sum(INV) / mean(invest_total),
                en_stock_share.gtap = sum(VKE) / mean(stock_total),
                en_capital_compensation_share.gtap = sum(CapitalCost) / mean(CapitalCost_total)) %>%
      ungroup() ->
      GTAP_inv_capital_share_R_Yh

    # filter Energy sector and complete HISTORICAL_YEARS
    GTAP_inv_capital_share_R_Yh %>%
      filter(GCAM_sector == "Energy") %>%
      select(-GCAM_sector) %>%
      rename(region = region_GCAM) %>%
      complete(region, year = HISTORICAL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(-region) %>%
      group_by(GCAM_region_ID) %>%
      mutate(en_inv_share.gtap = approx_fun(year, en_inv_share.gtap, rule = 2),
             en_stock_share.gtap = approx_fun(year, en_stock_share.gtap, rule = 2),
             en_capital_compensation_share.gtap = approx_fun(year, en_capital_compensation_share.gtap, rule = 2)) %>%
      ungroup() %>%
      gather(var, value, -GCAM_region_ID, -year) ->
      L100.National_Accounts_En_capital_inv_share_R_Yh

    assertthat::assert_that(L100.National_Accounts_En_capital_inv_share_R_Yh$value %>% min > 0.04, msg = "check min value in data")
    assertthat::assert_that(L100.National_Accounts_En_capital_inv_share_R_Yh$value %>% max < 0.5, msg = "check max value in data")
    assertthat::assert_that(L100.National_Accounts_En_capital_inv_share_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")


    L100.National_Accounts_En_capital_inv_share_R_Yh %>%
      add_title("Shares of energy capital and investment at GCAM region levels") %>%
      add_units("share") %>%
      add_comments("Derived using sectoral capital reward shares GTAP v10 data with a last year of 2014") %>%
      add_legacy_name("L100.National_Accounts_En_capital_inv_share_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "L100.GTAP_capital_stock") ->
      L100.National_Accounts_En_capital_inv_share_R_Yh


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
