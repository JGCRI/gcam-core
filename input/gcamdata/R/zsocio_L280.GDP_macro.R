# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L280.GDP_macro
#'
#' National accounts information for GDP macro.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L280.nationalAccounts}, \code{L280.SavingsRateParams}, \code{L280.GDP_macro_function}, \code{L280.FactorProductivity}.
#' There is no corresponding file in the original data system.
#' @details National accounts data and GDP macro function parameters for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020
#'
module_socio_L280.GDP_macro <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/A81.factor_productivity",
             "L101.Pop_thous_R_Yh",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L180.nationalAccounts",
             "L180.laborForceSSP"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L280.nationalAccounts",
             "L280.SavingsRateParams",
             "L280.GDP_macro_function",
             "L280.FactorProductivity"))
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    # --------------------------------------------------	---------------------------
    # 1. Read data

    all_data <- list(...)[[1]]

    # note this data is based on market exchange rate (mer)
    # macroeconomic date from Penn World Tables
    L180.nationalAccounts <- get_data(all_data, "L180.nationalAccounts", strip_attributes = TRUE)
    A81.factor_productivity <- get_data(all_data, "socioeconomics/A81.factor_productivity", strip_attributes = TRUE)
    # future labor force share from SSP dataset
    L180.laborForceSSP <- get_data(all_data, "L180.laborForceSSP", strip_attributes = TRUE)
    gdp.calibrated.data <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y", strip_attributes = TRUE)
    pop.hist.data <- get_data(all_data, "L101.Pop_thous_R_Yh", strip_attributes = TRUE)
    gcam.region.id <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    # -----------------------------------------------------------------------------

    #Calculate fractional wage rates to determine employment weighted
    #wage rate by GCAM region.
    L180.nationalAccounts %>% select(GCAM_region_ID, year, labor.force) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() %>%
      rename(labor.force.tot = labor.force) -> labor.force.gcam.reg.tbl
    L180.nationalAccounts %>% left_join_error_no_match(labor.force.gcam.reg.tbl,
                                                       by=c("GCAM_region_ID", "year")) %>%
      mutate(wage.rate.fraction = labor.force/labor.force.tot * wage.rate) %>%
      select(-labor.force.tot) -> L180.nationalAccounts

    #Aggregate national accounts from country to GCAM region
    #Remove all shares and rates by country, sum by GCAM region and recalculate
    L180.nationalAccounts %>% select(-iso, -country_name, -labor.force.share, -wage.rate,
                                     -hrs.worked.annual, -depreciation.rate, -savings.rate) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() %>%
      left_join_error_no_match(gcam.region.id, by="GCAM_region_ID") %>%
      arrange( GCAM_region_ID, year ) -> national.accounts.hist

    #Recalculate labor force share, depreciation rate, savings rate, capital share of GDP,
    #and labor (wage) share of GDP.
    #Rates could be weighted average by GCAM region as an alternative, like the wage rate calculation.
    #Savings rate is not personal savings rate, but Investment/GDP.
    national.accounts.hist %>% mutate(
      labor.force.share = labor.force/pop.pwt,
      depreciation.rate = depreciation/capital.stock,
      savings.rate = savings/gdp.pwt,
      capital.share.gdp = capital.stock/gdp.pwt,
      labor.share.gdp = wages/gdp.pwt) %>%
      rename(wage.rate = wage.rate.fraction) -> national.accounts.hist
    # change any inf generated above to NA to ensure they get picked up by fill etc
    national.accounts.hist[sapply(national.accounts.hist, is.nan)] <- NA_real_

    #Save PWT historical GDP, pop and savings rate for determining savings rate model below.
    national.accounts.hist %>% select(GCAM_region_ID, region, year, gdp.pwt, pop.pwt, savings.rate) %>%
      mutate(gdp.cap.pwt = gdp.pwt/pop.pwt) -> savings.rate.hist

    #Calculate annual labor productivity growth rate
    #NAs introduced by lag() for years prior to first available.
    national.accounts.hist %>% group_by(GCAM_region_ID) %>%
      mutate( wage.rate.lag = lag(wage.rate),
              labor.prod.rate = (wage.rate/wage.rate.lag)-1) %>%
      ungroup() %>%
      select(-wage.rate.lag) -> national.accounts.hist

    #filter for gcam historical years and add gcam region name
    national.accounts.hist %>% filter( year %in% HISTORICAL_YEARS ) -> national.accounts.hist

    #historical population, convert from thousands to millions
    pop.hist.data %>% filter( year %in% HISTORICAL_YEARS ) %>%
      rename(pop = value) %>%
      mutate(pop = pop / 1000) %>%
      arrange(GCAM_region_ID, year) -> pop.hist

    #calibrated gdp from original socioeconomic data processing
    #Note: gSSP2 is the "core" GCAM socioeconomic scenario
    gdp.calibrated.data %>% filter(scenario == 'gSSP2') %>%
      rename(gdp = value) %>%
      arrange(GCAM_region_ID, year) -> gdp.ssp2
    #ssp2 history from 1971 - 2015
    gdp.ssp2 %>% filter(year %in% HISTORICAL_YEARS) -> gdp.ssp2.hist
    #use ssp2 for future year gdp calibration
    gdp.ssp2 %>%
      filter( year %in% MODEL_FUTURE_YEARS ) ->
      national.accounts.FutureYrs

    #merge in GCAM's historical gdp from original socioeconomic data
    #left join on gdp.ssp2 to retain all gcam periods for all regions
    #some regions may have missing years
    gdp.ssp2.hist %>% left_join(national.accounts.hist,
      by=c("GCAM_region_ID", "year")) -> national.accounts.hist

    # scale all PWT data to adjust for differences between PWT estimates of historical
    # GDP and GCAM's
    national.accounts.hist %>%
      mutate(gdp.scaler = replace_na(gdp / gdp.pwt, 1),
             gdp.pwt = gdp.pwt * gdp.scaler,
             consumption = consumption * gdp.scaler,
             cons.plus.invest = consumption * gdp.scaler,
             capital.stock = capital.stock * gdp.scaler,
             depreciation = depreciation * gdp.scaler,
             savings = savings * gdp.scaler,
             wages = wages * gdp.scaler,
             energy.investment = replace_na(energy.investment * gdp.scaler, 0),
             capital.net.export = replace_na(capital.net.export * gdp.scaler, 0)) ->
      national.accounts.hist

    # Check validity of national account data by GCAM region.
    # Data missing for earlier years for Central Asia, Europe_Eastern, and Russia.
    # Fill from the last available year ("up") missing productivity and other rates.
    national.accounts.hist %>% group_by( region ) %>%
      fill( labor.force.share, depreciation.rate, savings.rate,
            labor.share.gdp, capital.share.gdp, labor.prod.rate, .direction = "up" ) %>%
      ungroup() %>%
      arrange( GCAM_region_ID, year ) -> national.accounts.hist

    #Data missing for Pakistan.
    #Replace Pakistan labor share and productivity rate with values from India
    national.accounts.hist %>% filter( region=="Pakistan" ) %>%
      mutate(labor.prod.rate = 0) -> national.accounts.hist.pakistan
    national.accounts.hist %>% filter( region=="India" ) %>%
      select( year, labor.share.gdp, labor.prod.rate ) -> national.accounts.hist.india
    national.accounts.hist.pakistan %>% left_join_error_no_match( national.accounts.hist.india,
                                        by = c("year")) %>%
      rename( labor.share.gdp = labor.share.gdp.y,
              labor.prod.rate = labor.prod.rate.y ) %>%
      mutate( wages = gdp.pwt * labor.share.gdp ) %>%
      select( -labor.share.gdp.x,-labor.prod.rate.x ) -> gcam.macro.hist.pakistan
    #remove original Pakistan and add new Pakistan data
    national.accounts.hist %>%  filter( region != "Pakistan" ) %>%
      bind_rows( gcam.macro.hist.pakistan ) -> national.accounts.hist

    #Filter for GCAM model base years
    national.accounts.hist %>% filter(year %in% MODEL_BASE_YEARS) -> national.accounts.BaseYrs

    # Normalize capital and wages to be consistent with GCAM GDP and not Penn World Table GDP.
    # Apply capital and wage share of PWT GDP to GCAM GDP
    national.accounts.BaseYrs %>% mutate(wages = gdp * labor.share.gdp,
      capital.stock = gdp * capital.share.gdp) -> national.accounts.BaseYrs

    national.accounts.FutureYrs %>%
      left_join_error_no_match(gcam.region.id, by=c("GCAM_region_ID")) ->
      national.accounts.FutureYrs


    # get final historical year (base year) capital stock depreciation rate for future rate
    national.accounts.hist %>% filter( year == MODEL_FINAL_BASE_YEAR ) %>%
      select( GCAM_region_ID, depreciation.rate ) -> depreciation.rate.base
    # apply base year capital stock depreciation rate for all future years by region
    national.accounts.FutureYrs %>% left_join_error_no_match( depreciation.rate.base,
                                    by=c("GCAM_region_ID") ) -> national.accounts.FutureYrs

    # Calculate labor force share (share population 15-64 not in school of total pop)
    # Use SSP2
    L180.laborForceSSP %>% filter(scenario %in% "ssp2") %>%
      select(-scenario) -> laborForceSSP2
    laborForceSSP2 %>% spread(var,value) %>%
      mutate(labor.force = as.numeric(labor.force), pop = as.numeric(pop)) %>%
      select(-iso, -unit) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(labor.force.share = labor.force/pop) %>%
      select(-labor.force) -> laborForceShareSSP2

    #Future labor force share for Taiwan (30) missing.
    #Use final historical year labor force share and pop for all future periods.
    #Todo: final alternative wage pop data.
    taiwan.region.name = gcam.region.id %>% filter(GCAM_region_ID == socioeconomics.TAIWAN_REGION_ID) %>% pull(region)
    national.accounts.BaseYrs %>% filter( region == taiwan.region.name & year %in% MODEL_BASE_YEARS ) %>%
      select( GCAM_region_ID, year, labor.force.share, pop=pop.pwt ) -> labor.force.share.Taiwan.BYS
    national.accounts.BaseYrs %>% filter( region == taiwan.region.name & year == MODEL_FINAL_BASE_YEAR ) %>%
      select( GCAM_region_ID, labor.force.share, pop=pop.pwt ) -> labor.force.share.Taiwan.FBY
    tibble(year = as.integer(MODEL_FUTURE_YEARS), GCAM_region_ID = socioeconomics.TAIWAN_REGION_ID) %>%
      left_join_error_no_match(labor.force.share.Taiwan.FBY, by=c("GCAM_region_ID")) -> laborForceShare.Taiwan.future

    laborForceShareSSP2 %>% bind_rows(labor.force.share.Taiwan.BYS) %>%
      bind_rows(laborForceShare.Taiwan.future) %>%
      arrange( GCAM_region_ID, year ) -> laborForceShareSSP2

    #Calculate employment rate from historical employed share and working age pop from SSP data.
    #Apply employment rate from final base year to to future working age pop share.
    national.accounts.BaseYrs %>% filter( year == MODEL_FINAL_BASE_YEAR ) %>%
      select( GCAM_region_ID, employed.share = labor.force.share ) -> employed.share.FBY
    laborForceShareSSP2 %>% filter(year == MODEL_FINAL_BASE_YEAR ) -> laborForceShareSSP2.FBY
    laborForceShareSSP2.FBY %>% left_join_error_no_match(employed.share.FBY, by=c("GCAM_region_ID") ) %>%
      mutate(employment.rate = employed.share/labor.force.share) %>%
      select(-year, -pop, -employed.share, -labor.force.share) -> employment.rate.SSP2.FBY
    laborForceShareSSP2 %>% filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(employment.rate.SSP2.FBY, by=c("GCAM_region_ID")) %>%
      mutate(labor.force.share = labor.force.share * employment.rate) -> laborForceShareSSP2.MFY
    #Add future labor force shares to national accounts table
    national.accounts.FutureYrs %>% left_join_error_no_match(laborForceShareSSP2.MFY,
                                    by=c("GCAM_region_ID","year") ) -> national.accounts.FutureYrs

    ######### Start Future Savings Rate Estimation ########
    #Make sure gdp, pop, gdp/cap, and savings rate are from consistent data sources.
    #Create average gdp/cap and savings rate by 5 year period.
    #This averages 2 years before and after 5 year intervals.
    savings.rate.hist %>% select(GCAM_region_ID, region, year, gdp.cap.pwt, savings.rate) %>%
        mutate(period = if_else( (year%%5) > 2, (year%/%5 + 1)*5, (year%/%5)*5 )) ->
      savings.rate.hist
    #calculate average annual gdp/cap growthrate and savings rate by region
    savings.rate.hist %>% select(-year) %>%
      group_by(GCAM_region_ID, region, period) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup() -> savings.rate.hist

    #calculate gdp per capita growthrate and lagged savings rate
    savings.rate.hist %>% arrange(GCAM_region_ID, period) %>%
      group_by(GCAM_region_ID) %>%
      mutate(gdp.cap.gr = gdp.cap.pwt / lag(gdp.cap.pwt) - 1,
             savings.rate.lag = lag(savings.rate)) %>%
      ungroup() -> savings.rate.hist

    # LM Regressions using 5-year average periods
    lm(savings.rate ~ gdp.cap.gr + savings.rate.lag, data = savings.rate.hist) -> savings.rate.model
    # savings rate prediction using actual savings rate
    predict.lm(savings.rate.model, savings.rate.hist) -> savings.rate.hist$savings.rate.fit
    # Get coefficients to predict future savings rate
    coef.est <- summary(savings.rate.model)$coefficients
    savRate.b0 <- coef.est["(Intercept)", "Estimate"]
    savRate.b1 <- coef.est["gdp.cap.gr", "Estimate"]
    savRate.b2 <- coef.est["savings.rate.lag", "Estimate"]

    national.accounts.FutureYrs %>%
      mutate(gdp.cap = gdp/pop,
             gdp.cap.gr=0, savings.rate = 0, savings.rate.lag = 0, savings.rate.fit = 0 ) %>%
      select(GCAM_region_ID, region, period=year, gdp.cap, savings.rate, gdp.cap.gr,
             savings.rate.lag, savings.rate.fit) -> savings.rate.future

    #revise gdp per cap to use consistent historical and SSP2 gdp and pop
    #(i.e. do not use PWT data)
    gdp.ssp2.hist %>% left_join_error_no_match(pop.hist, by=c("GCAM_region_ID", "year")) %>%
      mutate(gdp.cap = gdp/pop) %>%
      select(GCAM_region_ID, period=year, gdp.cap) -> gdp.cap.hist
    # Note: using left_join as savings.rate.hist is going to have some NAs in there already
    # (see comment in below in savings.rate.predict)
    savings.rate.hist %>% left_join(gdp.cap.hist, by=c("GCAM_region_ID", "period")) %>%
      select(-gdp.cap.pwt) -> savings.rate.hist
    #combine historical and future years savings tables
    bind_rows(savings.rate.hist, savings.rate.future) %>%
      filter(period >= min(MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, period) -> savings.rate.all

    savings.rate.all %>% arrange(GCAM_region_ID, period) %>%
      group_by(GCAM_region_ID) %>%
      mutate(gdp.cap.gr2 = gdp.cap/lag(gdp.cap) - 1,
             savings.rate.fit2 = savings.rate) %>%
      ungroup() -> savings.rate.all

    #Function for predicting savings rate.
    #Skips first row since lagged savings is not available for first year/period.
    savings.rate.predict <- function(sr,gr){
      for (i in 2:length(sr)){
        if(!is.nan(sr[i-1]) & !is.nan(gr[i])){
          sr[i] = savRate.b0 + savRate.b1 * gr[i] + savRate.b2 * sr[i-1]
        }
        # cover the case where this is the first year and no lagged savings
        # rate is available or we just have missing data (i.e. FSU related)
        # in which case just return a reasonable value
        # there are just a few such data points so it is unlikey to affect results
        else{ sr[i] = 0.15 }
      }
      return(sr)
    }

    #Predict savings rate for all years. Fit2 uses lagged predicted savings rate
    #and not actual savings rate for history.
    savings.rate.all %>% group_by(GCAM_region_ID) %>%
      mutate(savings.rate.fit2 = savings.rate.predict(savings.rate.fit2, gdp.cap.gr2)) %>%
      ungroup() -> savings.rate.all

    savings.rate.all %>% filter( period %in% MODEL_FUTURE_YEARS) %>%
      select(GCAM_region_ID, year = period, savings.rate = savings.rate.fit2) -> savings.rate.only

    #Add savings rate to future national accounts
    national.accounts.FutureYrs %>% left_join_error_no_match(savings.rate.only,
      by=c("GCAM_region_ID","year")) -> national.accounts.FutureYrs

    ######### End Savings Rate ########

    national.accounts.BaseYrs %>% rename(capital = capital.stock) %>%
      select(GCAM_region_ID, region, year, capital, depreciation.rate,
      savings.rate, energy.investment, wages, labor.force.share, capital.net.export) -> national.accounts.BaseYrs

    national.accounts.FutureYrs %>% mutate(capital = 0, wages = 0, energy.investment = 0, capital.net.export = NA_real_) %>%
      select(GCAM_region_ID, region, year, capital, depreciation.rate,
      savings.rate, energy.investment, wages, labor.force.share, capital.net.export) -> national.accounts.FutureYrs

    #combine historical and future national accounts
    national.accounts.BaseYrs %>% bind_rows(national.accounts.FutureYrs)  %>%
      arrange( GCAM_region_ID, year) -> L280.nationalAccounts

    # Phase out trade imbalance by assumed year, note a value greater than the final model year will
    # end up just holding the imbalance constant for all years
    L280.nationalAccounts %>%
      group_by(region) %>%
      mutate(capital.net.export = if_else(year >= socioeconomics.TRADE_BALANCE_YEAR, 0, capital.net.export),
             # rule = 2 to allow "constant for all years"
             capital.net.export = approx_fun(year, capital.net.export, rule = 2)) %>%
      ungroup() ->
      L280.nationalAccounts

    # Use fitted savings to ensure smooth transition from final historical year to first modeling period.
    savings.rate.all %>% filter(period == MODEL_FINAL_BASE_YEAR) %>%
      select(GCAM_region_ID, year = period, savings.rate = savings.rate.fit2) -> savings.rate.FBY

    # using regular left_join as we are explicitly looking to replace the NAs in L280.nationalAccounts with
    # values from savings.rate.FBY
    L280.nationalAccounts %>% left_join(savings.rate.FBY, by=c("GCAM_region_ID", "year")) %>%
      mutate( savings.rate = if_else(is.na(savings.rate.y), savings.rate.x, savings.rate.y) ) %>%
      select( -savings.rate.x, -savings.rate.y ) -> L280.nationalAccounts

    ## Read in savings rate regression parameters ##
    # Use these directly in the model so that the future savings rates can be consistent
    # with a changing GDP and so we do not have to redo this calculation for SSPs
    gcam.region.id %>%
      mutate(saving.rate.param.base = savRate.b0,
             saving.rate.param.GR.coef = savRate.b1,
             saving.rate.param.SR.coef = savRate.b2) ->
      L280.SavingsRateParams


    ####### GDP macro function parameters ########
    gcam.region.id %>% mutate( fn.name = "nested-CES",
                               rho = socioeconomics.CES_RHO,
                               gamma = 0,
                               is.primary.factor = 0,
                               is.energy = 0,
                               is.labor = 0,
                               is.capital = 0,
                               factor.type = "",
                               node.name = "",
                               leaf.name = "") -> gdp_macro_function_base
    #labor input for nested-CES function
    gdp_macro_function_base %>% mutate( gamma = socioeconomics.CES_GAMMA,
                                        is.primary.factor = 1,
                                        is.labor = 1,
                                        node.name = "capital-labor",
                                        leaf.name = "labor") -> gdp_macro_function
    #capital input for nested-CES function
    gdp_macro_function_base %>% mutate( gamma = socioeconomics.CES_GAMMA,
                                        is.primary.factor = 1,
                                        is.capital = 1,
                                        node.name = "capital-labor",
                                        leaf.name = "capital") %>%
                                bind_rows(gdp_macro_function) -> gdp_macro_function
    #energy input for nested-CES function
    gdp_macro_function_base %>% mutate( is.energy = 1,
                                        node.name = "energy",
                                        leaf.name = "energy" ) %>%
      bind_rows(gdp_macro_function) -> gdp_macro_function

    A81.factor_productivity %>%
      gather_years(value_col = "productivity") %>%
      complete(nesting(fn.name, node.name, leaf.name), year = MODEL_FUTURE_YEARS) %>%
      group_by(fn.name, node.name, leaf.name) %>%
      mutate(productivity = approx_fun(year, productivity)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(productivity = (1+productivity)^(year - MODEL_FUTURE_YEARS[1])) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[['FactorProductivity']], gcam.region.id) ->
      L280.FactorProductivity

    L280.nationalAccounts %>% select(region, year, capital, depreciation.rate,
                                savings.rate, energy.investment, wages, labor.force.share,
                                capital.net.export) -> L280.nationalAccounts
    # use 3 decimal places
    L280.nationalAccounts %>% dplyr::mutate_at(vars(capital, wages, energy.investment), list(~ round(.,3))) %>%
      dplyr::mutate_at(vars(depreciation.rate, savings.rate, labor.force.share), list(~ round(.,6))) ->
      L280.nationalAccounts


    gdp_macro_function %>% select(region, fn.name, rho, node.name, gamma, leaf.name,
                                  is.primary.factor, is.capital, is.labor, is.energy) -> L280.gdp_macro_function
    # ===================================================

    # Produce outputs

    L280.nationalAccounts %>%
      add_title("National Accounts data") %>%
      add_units("Unitless") %>%
      add_comments("National accounts data: GDP, GDP_calibrated, capital, depreciation, savings rate,
               energy investment, labor wages, labor force, labor force share") %>%
      add_legacy_name("% per year") %>%
      add_precursors("L101.Pop_thous_R_Yh",
                     "L102.gdp_mil90usd_Scen_R_Y",
                     "L180.nationalAccounts",
                     "L180.laborForceSSP",
                     "common/GCAM_region_names") ->
      L280.nationalAccounts

    L280.SavingsRateParams %>%
      add_title("Savings rate regression parameters") %>%
      add_units("unitless") %>%
      add_comments("Regresion parameters used to forcast a savings rate to use in") %>%
      add_comments("future model periods. Given GDP is one of the regression dimensions") %>%
      add_comments("we will need to use these directly in GCAM to ensure consistency") %>%
      add_precursors("L101.Pop_thous_R_Yh",
                     "L102.gdp_mil90usd_Scen_R_Y",
                     "L180.nationalAccounts",
                     "common/GCAM_region_names") ->
      L280.SavingsRateParams

    L280.gdp_macro_function %>%
      add_title("Nested CES Function") %>%
      add_units("Unitless") %>%
      add_comments("Inputs and parameters for nested CES function") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names",
                     "L102.gdp_mil90usd_Scen_R_Y") ->
      L280.GDP_macro_function

    L280.FactorProductivity %>%
      add_title("Per factor productivity") %>%
      add_units("Scaler") %>%
      add_comments("Cumulative per factor productivity to augment factor inputs IO coefficient") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A81.factor_productivity") ->
      L280.FactorProductivity

    return_data(L280.nationalAccounts,
                L280.SavingsRateParams,
                L280.GDP_macro_function,
                L280.FactorProductivity)

  } else {
    stop("Unknown command")
  }
}
