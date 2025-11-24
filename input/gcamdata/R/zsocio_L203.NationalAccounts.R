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
#' \code{L203.GDP_macro_function}, \code{L203.FactorProductivity}
#' @details National accounts data and GDP macro function parameters for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK 2020 PP 2023 XZ 2025
#'
module_socio_L203.NationalAccounts <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/A81.factor_productivity",
      "L103.National_Accounts_mil90usd_R_Yh")

  MODULE_OUTPUTS <-
    c("L203.NationalAccounts",
      "L203.SavingsRateParams",
      "L203.GDP_macro_function",
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

    # extend hist to future model years
    L103.National_Accounts_mil90usd_R_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      complete(nesting(GCAM_region_ID, region),
               year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      ## just use zeros
      mutate(across(c(wages, capital, capital.value, energy.investment, savings.rate),
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
      add_comments("National accounts data: capital, depreciation, savings rate,
                   energy investment, labor wages, capital.net.export (phased out be a year)") %>%
      add_legacy_name("L203.NationalAccounts") %>%
      add_precursors("L103.National_Accounts_mil90usd_R_Yh",
                     "common/GCAM_region_names") ->
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
    GCAM_region_names %>%
      mutate(fn.name = "nested-CES",
             rho = socioeconomics.CES_RHO,
             gamma = 0,
             is.primary.factor = 0,
             is.energy = 0,
             is.labor = 0,
             is.capital = 0,
             factor.type = "",
             node.name = "",
             leaf.name = "") ->
      gdp_macro_function_base
    #labor input for nested-CES function
    gdp_macro_function_base %>%
      mutate(gamma = socioeconomics.CES_GAMMA,
             is.primary.factor = 1,
             is.labor = 1,
             node.name = "capital-labor",
             leaf.name = "labor") ->
      gdp_macro_function
    #capital input for nested-CES function
    gdp_macro_function_base %>%
      mutate(gamma = socioeconomics.CES_GAMMA,
             is.primary.factor = 1,
             is.capital = 1,
             node.name = "capital-labor",
             leaf.name = "capital") %>%
      bind_rows(gdp_macro_function) ->
      gdp_macro_function
    #energy input for nested-CES function
    gdp_macro_function_base %>%
      mutate(is.energy = 1,
             node.name = "energy",
             leaf.name = "energy") %>%
      bind_rows(gdp_macro_function) ->
      gdp_macro_function

    gdp_macro_function %>%
      select(region, fn.name, rho, node.name, gamma, leaf.name,
             is.primary.factor, is.capital, is.labor, is.energy) ->
      L203.GDP_macro_function

    L203.GDP_macro_function %>%
      add_title("Nested CES Function") %>%
      add_units("Unitless") %>%
      add_comments("Inputs and parameters for nested CES function") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names") ->
      L203.GDP_macro_function


    # (4) factor productivity ----

    A81.factor_productivity %>%
      gather_years(value_col = "productivity") %>%
      complete(nesting(fn.name, node.name, leaf.name), year = MODEL_FUTURE_YEARS) %>%
      group_by(fn.name, node.name, leaf.name) %>%
      mutate(productivity = approx_fun(year, productivity)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(productivity = (1+productivity)^(year - MODEL_FUTURE_YEARS[1])) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[['FactorProductivity']], GCAM_region_names) ->
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


