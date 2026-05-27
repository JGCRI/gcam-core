# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L203.NationalAccounts
#'
#' National accounts information for GDP macro.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.NationalAccounts}, \code{L203.SavingsRateParams},
#' \code{L203.Materials_nestedCES_prod_function_intermediate},
#' \code{L203.Materials_nestedCES_prod_function_valueadded},
#' \code{L203.FactorProductivity}
#' @details National accounts data and GDP macro function parameters for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK 2020 PP 2023 XZ 2025
#'
module_socio_L203.NationalAccounts <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/A81.factor_productivity",
      FILE = "socioeconomics/gcam_macro_nested_CES_function",
      "L103.National_Accounts_mil90usd_R_Yh",
      # GTAP capital by sector (to partition energy capital)
      "L100.GTAP_capital_stock",
      # Ag value added info
      "L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded")

  MODULE_OUTPUTS <-
    c("L203.NationalAccounts",
      "L203.SavingsRateParams",
      "L203.Materials_nestedCES_prod_function_intermediate",
      "L203.Materials_nestedCES_prod_function_valueadded",
      "L203.FactorProductivity")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # (1) L203.NationalAccounts ----
    # Key variables in National Account will be for Materials so energy and Ag will be separated


    ## (1.1) Partition Ag labor and capital (land) from economy-wide variables in National Account ----

    ## Compute absolute values from Ag to partition
    L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded %>%
      select(region, year, AgMajorSector,
             Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD, Exp_Land_Bil1975USD,
             Capital_input_Bil1975USD, Labor_input_Mppl) %>%
      filter(AgMajorSector %in% c("CROP", "LIVESTOCK", "FOREST")) %>%
      group_by(region, year) %>%
      summarise(across(
        .cols = any_of(c("Exp_Labor_Bil1975USD",
                         "Exp_Capital_Bil1975USD",
                         "Exp_Land_Bil1975USD",
                         "Capital_input_Bil1975USD",
                         "Labor_input_Mppl")),
        .fns = ~ sum(.x, na.rm = TRUE)
      ), .groups = "drop") %>%
      transmute(
        region, year,
        AgVA_Labor_Mil1990USD = Exp_Labor_Bil1975USD * gdp_deflator(1990, 1975) * 1000,
        AgVA_Capital_Mil1990USD = Exp_Capital_Bil1975USD * gdp_deflator(1990, 1975) * 1000,
        AgVA_Land_Mil1990USD = Exp_Land_Bil1975USD * gdp_deflator(1990, 1975) * 1000,
        AgCap_Mil1990USD = Capital_input_Bil1975USD * gdp_deflator(1990, 1975) * 1000,
        AgEmp_Mppl = Labor_input_Mppl
      ) ->
      L203.Ag_LaborCapital_R


    ## Partition
    L103.National_Accounts_mil90usd_R_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L203.Ag_LaborCapital_R %>%
          select(region, year, AgVA_Labor_Mil1990USD, AgVA_Capital_Mil1990USD, AgVA_Land_Mil1990USD,
                 AgCap_Mil1990USD, AgEmp_Mppl),
        by = c("region", "year")) %>%
      mutate(wages = wages - AgVA_Labor_Mil1990USD,
             capital.value = capital.value - AgVA_Capital_Mil1990USD - AgVA_Land_Mil1990USD,
             capital = capital - AgCap_Mil1990USD,
             AgVA_Labor = AgVA_Labor_Mil1990USD,
             AgVA_Capital = AgVA_Capital_Mil1990USD,
             AgVA_Land = AgVA_Land_Mil1990USD,
             AgCap = AgCap_Mil1990USD
             ) %>%
      select(-AgVA_Capital_Mil1990USD, -AgCap_Mil1990USD, -AgVA_Land_Mil1990USD) ->
      L203.NationalAccounts_Yh_1

    assert_that(
      all(L203.NationalAccounts_Yh_1[ , setdiff(names(L203.NationalAccounts_Yh_1),
                                           c("capital.net.export", "region", "savings.rate"))] >= 0),
      msg = "Unexpected negative values in L203.NationalAccounts_Yh_1"
    )


    ## (1.2) Partition energy capital and capital value from economy-wide variables in National Account ----

    ## Derive shares for partitioning the total capital stock and investment by (energy) sector

    # We want to partition the total capital stock, capital compensation ( & potentially investment)
    # to split out energy capital usage
    # The Penn World table does not include this level of detail so we will need to utilize GTAP
    # capital data to do this.
    L100.GTAP_capital_stock %>%
      # calculate regional total
      group_by(region, year) %>%
      # remove AgLU (which has been partitioned above)
      filter(GCAM_sector != "AgLU") %>%
      mutate(stock_total = sum(VKE),
             CapitalCost_total = sum(CapitalCost)) %>% ungroup() %>%
      # calculate sector shares by region & year
      transmute(year, region, GCAM_sector,
                en_stock_share.gtap = VKE / stock_total,
                en_capital_compensation_share.gtap = CapitalCost / CapitalCost_total) %>%
      ungroup() %>%
      # filter Energy sector and complete HISTORICAL_YEARS
      filter(GCAM_sector == "Energy") %>%
      select(-GCAM_sector) %>%
      complete(region, year = HISTORICAL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(-region) %>%
      group_by(GCAM_region_ID) %>%
      mutate(en_stock_share.gtap = approx_fun(year, en_stock_share.gtap, rule = 2),
             en_capital_compensation_share.gtap = approx_fun(year, en_capital_compensation_share.gtap, rule = 2))  %>%
      ungroup() %>%
      gather(var, value, -GCAM_region_ID, -year) %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L203.National_Accounts_En_capital_inv_share_R_Yh

    # Adding quick assertions to flag potential data issues for future updates
    assertthat::assert_that(L203.National_Accounts_En_capital_inv_share_R_Yh$value %>% min > 0.04, msg = "check min value in data")
    assertthat::assert_that(L203.National_Accounts_En_capital_inv_share_R_Yh$value %>% max < 0.5, msg = "check max value in data")
    assertthat::assert_that(L203.National_Accounts_En_capital_inv_share_R_Yh %>%
                              anti_join(GCAM_region_names, ., by = "GCAM_region_ID") %>% nrow == 0,
                            msg = "not all GCAM region available in data")


    ### Apply energy service capital share in rest of economy (post AgLU partition) ----
    # a fixed share (40%) of relevant capital is allocated to energy service activities across regions
    # this value can be viewed as a parameter governing the connection between energy service and the rest of
    # the economy. High share leads to higher implied capital compensation in the GCAM energy & service sectors
    # and thus high room for energy service value entering materials production function.
    # Once we have improved the energy price data in GCAM, we can further validate this value.


    socioeconomics.EN_SERVICE_CAPITAL_SHARE <- .4

    L203.National_Accounts_En_capital_inv_share_R_Yh %>%
      mutate(value = value + (1 - value) * socioeconomics.EN_SERVICE_CAPITAL_SHARE) ->
      L203.National_Accounts_En_capital_inv_share_R_Yh


    ## Partition sectoral (En, Materials, etc) capital & inv shares using GTAP shares

    L203.NationalAccounts_Yh_1 %>%
      left_join_error_no_match(
        L203.National_Accounts_En_capital_inv_share_R_Yh %>%
          spread(var, value),
        by = c("GCAM_region_ID", "year")) %>%
      mutate(GCAM_region_ID, year,
             capital = capital * (1.0 - en_stock_share.gtap),
             capital.value = capital.value * (1.0 - en_capital_compensation_share.gtap)) %>%
      select(-en_stock_share.gtap, -en_capital_compensation_share.gtap) ->
      L203.NationalAccounts_Yh_2

    assert_that(
      all(L203.NationalAccounts_Yh_2[ , setdiff(names(L203.NationalAccounts_Yh_2),
                                                c("capital.net.export", "region", "savings.rate"))] >= 0),
      msg = "Unexpected negative values in L203.NationalAccounts_Yh_2"
    )


    ## (1.3) add future periods in L203.NationalAccounts ----

    # extend hist to future model years
    L203.NationalAccounts_Yh_2 %>%
      select(-AgVA_Labor, -AgVA_Capital, -AgVA_Land, -AgCap) %>%
      complete(nesting(GCAM_region_ID, region),
               year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      ## just use zeros
      mutate(across(c(wages, capital, capital.value, savings.rate),
                    ~replace_na(., 0))) %>%
      group_by(GCAM_region_ID, region) %>%
      ## Phase out trade imbalance by assumed year ----
      mutate(capital.net.export = if_else(year >= socioeconomics.TRADE_BALANCE_YEAR, 0, capital.net.export),
             # rule = 2 to allow "constant for all years"
             capital.net.export = approx_fun(year, capital.net.export, rule = 2)) %>%
      # future dep will be fixed
      fill(depreciation.rate, .direction = "down") %>%
      ungroup ->
      L203.NationalAccounts


    L203.NationalAccounts %>%
      add_title("National Accounts data") %>%
      add_units("mil90usd and rates") %>%
      add_comments("National accounts data: capital (Materials), depreciation (Materials),
      wages (Materials), savings rate, capital.net.export (phased out be a year);
      Two labor related columns are passed through: AgVA_Labor_Mil1990USD and AgEmp_Mppl") %>%
      add_legacy_name("L203.NationalAccounts") %>%
      add_precursors("L103.National_Accounts_mil90usd_R_Yh",
                     "common/GCAM_region_names",
                     "L100.GTAP_capital_stock",
                     "L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded") ->
      L203.NationalAccounts


    # (2) Savings rate regression parameters ----
    # In GCAM v8.2, we estimated a pooled regression model (panel data: GCAM regions × PWT data in 5-year steps),
    # and fitted these parameters based on that model. Details were provided in the v8.2 release.
    # Here, we use those parameters directly for simplicity and consistency.
    # However, we note that these parameters carry uncertainty: they may be sensitive to the chosen regression model
    # (e.g., fixed effects) and to data coverage (regions and years). Future work should revisit and refine them,
    # potentially with region-specific estimates.

    savRate.b0 = 0.01111129
    savRate.b1 = 0.1443347
    savRate.b2 = 0.8922693
    GCAM_region_names %>%
      mutate(saving.rate.param.base = savRate.b0,
             saving.rate.param.GR.coef = savRate.b1,
             saving.rate.param.SR.coef = savRate.b2) ->
      L203.SavingsRateParams

    L203.SavingsRateParams %>%
      add_title("Savings rate regression parameters") %>%
      add_units("unitless") %>%
      add_comments("Regresion parameters used to forcast a savings rate to use in") %>%
      add_comments("future model periods. Given GDP is one of the regression dimensions") %>%
      add_comments("we will need to use these directly in GCAM to ensure consistency") %>%
      add_precursors("common/GCAM_region_names") ->
      L203.SavingsRateParams



    # (3) macro materials production function parameters ----

    gcam_macro_nested_CES_function %>%
      filter(node.name == "intermediate") %>%
      rename(rho = rho_Intermediate) %>%
      select(LEVEL2_DATA_NAMES[["NestedCESMacro"]]) ->
      L203.Materials_nestedCES_prod_function_intermediate

    gcam_macro_nested_CES_function %>%
      filter(node.name == "capital-labor") %>%
      rename(rho = rho_Intermediate, gamma = gamma_ValueAdded) %>%
      select(LEVEL2_DATA_NAMES[["NestedCESMacroVA"]]) ->
      L203.Materials_nestedCES_prod_function_valueadded

    L203.Materials_nestedCES_prod_function_intermediate %>%
      add_title("Nested CES Function, the intermediate nest") %>%
      add_units("Unitless") %>%
      add_comments("Inputs and parameters for nested CES function, the intermediate nest") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/gcam_macro_nested_CES_function") ->
      L203.Materials_nestedCES_prod_function_intermediate

    L203.Materials_nestedCES_prod_function_valueadded %>%
      add_title("Nested CES Function, the value added nest") %>%
      add_units("Unitless") %>%
      add_comments("Inputs and parameters for nested CES function, the value added nest") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/gcam_macro_nested_CES_function") ->
      L203.Materials_nestedCES_prod_function_valueadded


    # (4) factor productivity (not used by default) ----

    A81.factor_productivity %>%
      gather_years(value_col = "productivity") %>%
      complete(nesting(fn.name, node.name, leaf.name), year = MODEL_FUTURE_YEARS) %>%
      group_by(fn.name, node.name, leaf.name) %>%
      mutate(productivity = approx_fun(year, productivity)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(productivity = (1+productivity)^(year - MODEL_FINAL_BASE_YEAR)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[['FactorProductivityVA']], GCAM_region_names) ->
      L203.FactorProductivity

    L203.FactorProductivity %>%
      add_title("Per factor productivity") %>%
      add_units("Scaler") %>%
      add_comments("Cumulative per factor productivity to augment factor inputs IO coefficient") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A81.factor_productivity") ->
      L203.FactorProductivity


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}


