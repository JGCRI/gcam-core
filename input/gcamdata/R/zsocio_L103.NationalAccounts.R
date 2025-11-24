# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L103.NationalAccounts
#'
#' National accounts information for GDP macro.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.National_Accounts_mil90usd_R_Yh}, \code{L103.LaborForceShare_Scen_R_Y}.
#' There is no corresponding file in the original data system.
#' @details Select national accounts data from Penn World Table for all countries.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020 XZ June 2025
#'
module_socio_L103.NationalAccounts <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      "L100.LaborForce_mil_SSP_ctry_Yfut_raw",
      "L100.National_Accounts_Employment_Share_POP_R_Yh",
      "L102.gdp_mil90usd_Scen_R_Y",
      "L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh",
      "L100.National_Accounts_Metrics_R_Yh",
      "L100.National_Accounts_Depreciation_Rate_R_Yh",
      "L100.National_Accounts_En_capital_inv_share_R_Yh")

  MODULE_OUTPUTS <-
    c("L103.National_Accounts_mil90usd_R_Yh",
      "L103.LaborForceShare_Scen_R_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    # 1. Read data

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Part 1 Process National Accounts Variables ----
    ## (1) GDP decompose ----

    L102.gdp_mil90usd_Scen_R_Y %>%
      # any SSP works here
      filter(year <= MODEL_FINAL_BASE_YEAR, scenario == "SSP2") %>%
      select(-scenario) ->
      L103.National_Accounts_mil90usd_R_Yh_0

    assertthat::assert_that(MODEL_BASE_YEARS %in%
                              unique(L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh$year) %>% all)

    L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(
        L103.National_Accounts_mil90usd_R_Yh_0 %>% rename(GDP = value),
        by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value * GDP,
             var = gsub(".gmd", "", var)) %>%
      select(-GDP) %>%
      spread(var, value) ->
      L103.National_Accounts_mil90usd_R_Yh_1_0

    ### balance trade ----
    ## the Global MacroDatabase (GMD) explicitly converts imports from CIF to FOB valuation, and reports exports as FOB!
    # We usually trust FOB (exports) more and will adjust imports to ensure balance
    # We will re-balance GDP using cons; In the new GMD data, the magnitude of the adjustment is pretty small
    L103.National_Accounts_mil90usd_R_Yh_1_0 %>%
      group_by(year) %>%
      mutate(exp_world = sum(exports),
             imp_world = sum(imports),
             imbal_rel = exp_world / imp_world,
             imp_adj = imbal_rel * imports,
             cons_adj = cons + imp_adj - imports,
             cons_scaler = cons_adj / cons) ->
      L103.National_Accounts_mil90usd_R_Yh_1_1

    # check adjustments in final base year
    assertthat::assert_that(
      L103.National_Accounts_mil90usd_R_Yh_1_1 %>%
        filter(year == MODEL_FINAL_BASE_YEAR) %>%
        # assert imbal_rel and cons_scaler were small and
        # trade imbal in base year < 5%  (it was ~1% in 2021)
        # it is fine to be larger
        filter(abs(imbal_rel - 1) < 0.05,
               abs(cons_scaler - 1) < 0.05) %>%
        nrow() == length(unique(L103.National_Accounts_mil90usd_R_Yh_1_1$GCAM_region_ID)),
      msg = "The adjustments in trade or consumption in some regions were larger the the thredhold"
    )

    # generate other related variables
    # capital.net.export = -net.export
    # savings = inv - capital.net.export
    L103.National_Accounts_mil90usd_R_Yh_1_1 %>%
      transmute(GCAM_region_ID, year, cons = cons_adj, inv, exports, imports = imp_adj) %>%
      mutate(net.export = exports - imports,
             capital.net.export = -net.export,
             savings = inv - capital.net.export) ->
      L103.National_Accounts_mil90usd_R_Yh_1


    ## (2) Derive and join other variables ----
    # capital stock, labor.compensation, capital.compensation , savings.rate

    L100.National_Accounts_Metrics_R_Yh %>%
      # extend PWT data from 2019 to MODEL_FINAL_BASE_YEAR
      complete(GCAM_region_ID, var, year = min(year):MODEL_FINAL_BASE_YEAR) %>%
      fill(value, .direction = "downup")  %>%
      spread(var, value) %>%
      left_join_error_no_match(
        L103.National_Accounts_mil90usd_R_Yh_0 %>% rename(GDP = value),
        by = c("GCAM_region_ID", "year")) %>%
      transmute(
        GCAM_region_ID, year,
        GDP,
        capital.stock = GDP * capital_GDP_ratio.pwt,
        labor.compensation = labor.share.pwt * GDP,
        capital.compensation = capital.share.pwt * GDP) %>%
      # join other national account variables
      left_join_error_no_match(
        L103.National_Accounts_mil90usd_R_Yh_1,
        by = c("GCAM_region_ID", "year") ) %>%
      mutate(savings.rate = savings / GDP) ->
      L103.National_Accounts_mil90usd_R_Yh_2

    ## (3) Join depreciation rates ----
    L103.National_Accounts_mil90usd_R_Yh_2 %>%
      left_join_error_no_match(
      L100.National_Accounts_Depreciation_Rate_R_Yh %>%
        complete(GCAM_region_ID, var, year = min(year):MODEL_FINAL_BASE_YEAR) %>%
        fill(value, .direction = "downup")  %>%
        spread(var, value) %>%
        rename(depreciation.rate = depreciation.rate.pwt),
      by = c("GCAM_region_ID", "year") ) ->
      L103.National_Accounts_mil90usd_R_Yh_3

    ## (4) partition sectoral (En, Materials, etc) capital & inv shares using GTAP shares
    L103.National_Accounts_mil90usd_R_Yh_3 %>%
      select(GCAM_region_ID, year, capital.stock, inv, capital.compensation) %>%
      left_join_error_no_match(
        L100.National_Accounts_En_capital_inv_share_R_Yh %>%
          spread(var, value),
        by = c("GCAM_region_ID", "year")) %>%
      transmute(GCAM_region_ID, year,
                materials.capital.stock = capital.stock * (1.0 - en_stock_share.gtap),
                energy.investment = inv * en_inv_share.gtap,
                materials.capital.compensation = capital.compensation * (1.0 - en_capital_compensation_share.gtap)) %>%
      # join back the main
      left_join_error_no_match(L103.National_Accounts_mil90usd_R_Yh_3, .,
                               by = c("GCAM_region_ID", "year")) ->
      L103.National_Accounts_mil90usd_R_Yh_4

    L103.National_Accounts_mil90usd_R_Yh_4 %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      transmute(GCAM_region_ID, region, year,
                capital.net.export,
                depreciation.rate,
                savings.rate,
                wages = labor.compensation, # Materials labor.compensation
                capital.value = materials.capital.compensation,
                capital = materials.capital.stock,
                energy.investment) ->
      L103.National_Accounts_mil90usd_R_Yh

    L103.National_Accounts_mil90usd_R_Yh %>%
      add_title("Processed National Accounts Data from PWT, GTAP, and GMD") %>%
      add_units("million 1990US$") %>%
      add_comments("National accounts data: GDP, capital, depreciation, savings rate,
               labor wages, labor productivity, energy investment") %>%
      add_legacy_name("L103.National_Accounts_mil90usd_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names","L102.gdp_mil90usd_Scen_R_Y",
                     "L100.National_Accounts_GDP_Decomp_C_I_X_M_shares_R_Yh",
                     "L100.National_Accounts_Metrics_R_Yh",
                     "L100.National_Accounts_Depreciation_Rate_R_Yh",
                     "L100.National_Accounts_En_capital_inv_share_R_Yh") ->
      L103.National_Accounts_mil90usd_R_Yh


    # Part 2 Calculate labor force (employment) share by SSP ----
    # connect SSP work age population projections to PWT employment shares

    L100.LaborForce_mil_SSP_ctry_Yfut_raw %>%
      group_by(scenario, GCAM_region_ID, year, var) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      # 2020 was in SSP database and also before GCAM base year of 2021
      # it is need for interpolations here
      filter(year >= socioeconomics.SSP_DB_Labor_StartYear) %>%
      spread(var,value) %>%
      # Interpolate for base-year year if missing
      complete(nesting(scenario, GCAM_region_ID), year = c(year, MODEL_FINAL_BASE_YEAR, FUTURE_YEARS)) %>%
      group_by(scenario, GCAM_region_ID) %>%
      mutate(pop = approx_fun(year, pop , rule = 2),
             labor.force = approx_fun(year, labor.force , rule = 2)) %>%
      ungroup() %>%
      mutate(labor.force.share = labor.force/pop) %>%
      select(-labor.force) ->
      LaborForceShare_SSP_Yfut

    # Prepare historical employment shares
    # 2019 from PWT v10 is extended to 2021
    L100.National_Accounts_Employment_Share_POP_R_Yh %>%
      complete(GCAM_region_ID, var, year = min(year):MODEL_FINAL_BASE_YEAR) %>%
      fill(value, .direction = "downup")  %>%
      spread(var, value) %>%
      repeat_add_columns(tibble(distinct(LaborForceShare_SSP_Yfut, scenario))) ->
      EmploymentShare_SSP_R_Y

    # calculate base year employment rate to explain the gap between labor force participation
    # and unemployment
    LaborForceShare_SSP_Yfut %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      # any scenario here
      filter(scenario == "SSP2") %>% select(-scenario) %>%
      left_join_error_no_match(
        EmploymentShare_SSP_R_Y %>%
          # any scenario here
          filter(scenario == "SSP2") %>% select(-scenario),
        by = c("GCAM_region_ID", "year")
      ) %>%
      mutate(employment.rate = employed.share.pwt/labor.force.share) %>%
      select(-year, -pop, -employed.share.pwt, -labor.force.share) ->
      employment.rate.FBY

    # apply those employment.rate in base year to future projection assuming no future changes

    EmploymentShare_SSP_R_Y %>%
      rename(employed.share = employed.share.pwt) %>%
      bind_rows(
        LaborForceShare_SSP_Yfut %>%
          # only > final base year
          filter(year %in% FUTURE_YEARS) %>%
          left_join_error_no_match(employment.rate.FBY, by=c("GCAM_region_ID")) %>%
          transmute(scenario, GCAM_region_ID, year,
                    #  net employment share over pop
                    employed.share = round(labor.force.share * employment.rate, 6))
      ) ->
      L103.LaborForceShare_Scen_R_Y

    L103.LaborForceShare_Scen_R_Y %>%
      add_title("National accounts data: employment share across SSPs") %>%
      add_units("shares") %>%
      add_comments("employment over population; derived by stitching PWT and SSP and assuming fixed future participation and unemployment rates") %>%
      add_legacy_name("LaborForceShare_Scen_R_Y") %>%
      add_precursors("L100.LaborForce_mil_SSP_ctry_Yfut_raw",
                     "L100.National_Accounts_Employment_Share_POP_R_Yh") ->
      L103.LaborForceShare_Scen_R_Y



    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
