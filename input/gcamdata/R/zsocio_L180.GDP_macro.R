# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L180.GDP_macro
#'
#' National accounts information for GDP macro.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L180.nationalAccounts}, \code{L180.laborForceSSP}.
#' There is no corresponding file in the original data system.
#' @details Select national accounts data from Penn World Table for all countries.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020
#'
module_socio_L180.GDP_macro <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/pwt91",
             FILE = "socioeconomics/pwt91_na",
             "L100.GTAP_capital_stock"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L180.nationalAccounts",
             "L180.laborForceSSP"))
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    # --------------------------------------------------	---------------------------
    # 1. Read data

    all_data <- list(...)[[1]]

    # note this data is based on market exchange rate (mer)
    # macroeconomic date from Penn World Tables
    PWT91.raw <- get_data(all_data, "socioeconomics/pwt91")
    PWT91.supplemental <- get_data(all_data, "socioeconomics/pwt91_na")
    # population by cohort for determining labor force
    pop.cohort.ssp.data <- get_data(all_data, "socioeconomics/SSP_database_v9")
    gcam.reg.iso <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)

    L100.GTAP_capital_stock <- get_data(all_data, "L100.GTAP_capital_stock", strip_attributes = TRUE)


    # -----------------------------------------------------------------------------
    # Data ID Info for Penn World Table used here (for complete list, see PWT91 in raw data)
    # Variable name       Variable definition
    # countrycode	        3-letter ISO country code
    # country	            Country name
    # currency_unit	      Currency unit
    # year	              Year
    # rgdpo               Output-side real GDP at chained PPPs (in mil. 2011US$)
    # pop	                Population (in millions)
    # emp	                Number of persons engaged (in millions)
    # avh	                Average annual hours worked by persons engaged
    # hc	                Index of human capital per person, based on years of schooling (see PWT9)
    # rgdpna	            RealGDP at constant 2011 national prices (in mil. 2011US$)
    # rconna	            Real consumption at constant 2011 national prices (in mil. 2011US$)
    # rdana	              Real domestic absorption at constant 2011 national prices (in mil. 2011US$)
    ## Real domestic absorption = consumption (HH+G) + investment
    # rnna	              Capital stock at constant 2011 national prices (in mil. 2011US$)
    # rtfpna	            TFP at constant national prices (2011=1)
    # labsh	              Share of labour compensation in GDP at current national prices
    # delta           	  Average depreciation rate of the capital stock
    #

    PWT91.raw %>% select(countrycode, country, year, pop, emp, avh, rgdpna, rconna, rdana,
                         rnna, labsh, delta) %>%
      rename(iso = countrycode,
             labor.force = emp,
             hrs.worked.annual = avh,
             pop.pwt = pop,
             gdp.pwt = rgdpna,
             consumption = rconna,
             cons.plus.invest = rdana,
             capital.stock = rnna,
             labor.share.gdp = labsh,
             depreciation.rate = delta) %>%
      mutate(iso = tolower(iso)) %>%
      gather(var, value, -iso, -country, -year) %>%
      filter(!is.na(value)) -> pwt

    # process supplemental data to be consistent with main pwt data, namely:
    # 1. convert from constant local currency to constant USD using the provided
    #    exchange rate in the base dollar year
    # 2. ensure export and imports balance globally (they are off by ~ 1-2%)
    PWT91.supplemental %>%
      rename(iso = countrycode) %>%
      mutate(iso = tolower(iso)) %>%
      filter(iso %in% unique(pwt$iso)) ->
      PWT91.supplemental

    PWT91.supplemental %>%
      # get the exchange rate of the base dollar year
      filter(year == socioeconomics.PWT_CONSTANT_CURRENCY_YEAR) %>%
      select(iso, xr) %>%
      # join that exchange rate back onto the base q_x, q_m (exports and imports)
      # which are already in the base currency year so that we can jump from
      # local currency to USD
      left_join_error_no_match(PWT91.supplemental %>% select(iso, year, q_x, q_m), ., by=c("iso")) %>%
      mutate(exports = q_x / xr,
             imports = q_m / xr) %>%
      select(iso, year, exports, imports) %>%
      # scale imports so that they exactly match exports globally
      group_by(year) %>%
      mutate(imports = imports * sum(exports, na.rm=T) / sum(imports, na.rm = T)) %>%
      ungroup() %>%
      gather(var, value, -iso, -year) %>%
      filter(!is.na(value)) -> pwt_supp

    pwt %>%
      select(-country) %>%
      bind_rows(pwt_supp) ->
      pwt

    # replace iso:sxm with iso:nld for Dutch part of Saint Maarten
    pwt %>% mutate(iso = gsub("sxm", "nld", iso)) %>%
      group_by(iso, var, year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() -> pwt

    ## Process and aggregate data to GCAM inputs
    ## Check for iso errors, do not include.
    pwt %>% filter(!(iso %in% gcam.reg.iso$iso)) -> pwt.no.iso
    assertthat::assert_that(nrow(pwt.no.iso) == 0)

    pwt %>%
      left_join_error_no_match(gcam.reg.iso, by = "iso") %>%
      spread(var, value) %>%
      mutate(hrs.worked.annual = if_else(is.na(hrs.worked.annual), socioeconomics.DEFAULT_MEDIAN_HOURS_WORKED, hrs.worked.annual), # Replace missing with median average hours
             wages = gdp.pwt * labor.share.gdp, #million 2011US$
             wage.rate = wages / labor.force / hrs.worked.annual, #wages/worker-hr
             labor.force.share = labor.force / pop.pwt,
             depreciation = capital.stock * depreciation.rate,
             investment = cons.plus.invest - consumption,
             net.export = exports - imports,
             capital.net.export = -net.export,
             savings = investment - capital.net.export,
             savings.rate = savings / gdp.pwt) -> nationalAccounts.2011dollar

    # Convert all US dollar year from 2011 to 1990 for consistency with GCAM input
    nationalAccounts.2011dollar %>% mutate( gdp.pwt = gdp.pwt * gdp_deflator(1990, 2011),
                                            consumption = consumption * gdp_deflator(1990, 2011),
                                            cons.plus.invest = cons.plus.invest * gdp_deflator(1990, 2011),
                                            wages = wages * gdp_deflator(1990, 2011),
                                            wage.rate = wage.rate * gdp_deflator(1990, 2011),
                                            capital.stock = capital.stock * gdp_deflator(1990, 2011),
                                            depreciation = depreciation * gdp_deflator(1990, 2011),
                                            investment = investment * gdp_deflator(1990, 2011),
                                            savings = savings * gdp_deflator(1990, 2011),
                                            capital.net.export = capital.net.export * gdp_deflator(1990, 2011)) -> L180.nationalAccounts

    # We want to partition the total capital stock and investment to split out energy capital usage
    # The Penn World table does not include this level of detail so we will need to utilize GTAP
    # capital data to do this.  The two datasets will not agree on total capital stock values so
    # instead we will compute shares from the GTAP data then apply that to the Penn World table values

    # Note: we will have fewer countries in the GTAP databases.  We will give the missing countries
    # the GCAM regional shares so first compute that.
    L100.GTAP_capital_stock %>%
      group_by(region_GCAM, year) %>%
      mutate(invest_total = sum(CapitalCost), stock_total = sum(VKE)) %>%
      group_by(region_GCAM, year, GCAM_sector) %>%
      summarize(gtap_ene_inv_share = sum(CapitalCost) / mean(invest_total),
                gtap_ene_stock_share = sum(VKE) / mean(stock_total)) %>%
      ungroup() ->
      GTAP_inv_share.regional
    # Compute shares for the countries we have
    L100.GTAP_capital_stock %>%
      group_by(region_GTAP, year) %>%
      mutate(invest_total = sum(CapitalCost), stock_total = sum(VKE)) %>%
      group_by(region_GTAP, year, GCAM_sector) %>%
      summarize(gtap_ene_inv_share = sum(CapitalCost) / mean(invest_total),
                gtap_ene_stock_share = sum(VKE) / mean(stock_total)) %>%
      ungroup() ->
      GTAP_inv_share.ctry
    # Set the shares for the countries not included in GTAP
    L180.nationalAccounts %>%
      select(iso) %>%
      distinct() %>%
      left_join_error_no_match(gcam.reg.iso %>% select(iso, GCAM_region_ID), by=c("iso")) %>%
      filter(!iso %in% unique(L100.GTAP_capital_stock$region_GTAP)) %>%
      left_join_error_no_match(GCAM_region_names, by=c("GCAM_region_ID")) %>%
      select(region_GTAP = iso, region_GCAM = region) %>%
      # note: we are using left_join as we are using it to filter the ISOs which
      # are not available in GTAP (which of course will the get assigned regional
      # average shares)
      left_join(GTAP_inv_share.regional, by=c("region_GCAM")) %>%
      select(-region_GCAM) %>%
      # combine with the GTAP ctry shares to now have coverage across all ISOs
      bind_rows(GTAP_inv_share.ctry) %>%
      rename(iso = region_GTAP) ->
      GTAP_inv_share

    # filter for just Energy as that is what we are interested at the moment, then fillout for
    # all historical years holding values constant beyond the endpoints of the GTAP data
    GTAP_inv_share %>%
      filter(GCAM_sector == "Energy") %>%
      select(-GCAM_sector) %>%
      full_join(tibble(year = unique(L180.nationalAccounts$year)), by=c("year")) %>%
      complete(year, nesting(iso)) %>%
      group_by(iso) %>%
      mutate(gtap_ene_inv_share = approx_fun(year, gtap_ene_inv_share, rule = 2),
             gtap_ene_stock_share = approx_fun(year, gtap_ene_stock_share, rule = 2)) %>%
      ungroup() ->
      GTAP_inv_share_complete

    # Finally, adjust the Penn World table values with the GTAP capital shares
    L180.nationalAccounts %>%
      left_join_error_no_match(GTAP_inv_share_complete, by=c("iso", "year")) %>%
      mutate(capital.stock = capital.stock * (1.0 - gtap_ene_stock_share),
             depreciation = depreciation * (1.0 - gtap_ene_stock_share),
             energy.investment = investment * gtap_ene_inv_share) %>%
      select(-gtap_ene_inv_share, -gtap_ene_stock_share) ->
      L180.nationalAccounts




    #Future labor force share of population from SSP population by cohort
    #Clean up dataset for processing.
    pop.cohort.ssp.data %>%
      rename(model = MODEL, scenario = SCENARIO, iso = REGION, var = VARIABLE, unit = UNIT) %>%
      filter(model == "IIASA-WiC POP") %>%
      mutate(var = gsub("\\|", "_", var),
             scenario = tolower(substr(scenario, 1, 4)),
             iso = tolower(iso),
             var = gsub("Population", "pop", var),
             var = gsub("\\-", "_", var),
             var = gsub("Female", "n", var),
             var = gsub("Male", "n", var),
             var = gsub("Aged", "", var)) -> pop.cohort.ssp #gender neutral for total
    #Filter working age population, excluding some cohorts
    #Working age population = ages 15-64. Don't include ages 15-19 in HS and 20-24 in college.
    pop.cohort.ssp %>% filter(var %in% c("pop", "pop_n_15_19_No Education", "pop_n_15_19_Primary Education",
                      "pop_n_20_24_No Education", "pop_n_20_24_Primary Education",
                      "pop_n_20_24_Secondary Education",
                      "pop_n_25_29", "pop_n_30_34", "pop_n_35_39" , "pop_n_40_44",
                      "pop_n_45_49", "pop_n_50_54", "pop_n_55_59" , "pop_n_60_64")) %>%
      select(-model) -> pop.labor.force.ssp
    pop.labor.force.ssp %>% filter(var %in% "pop") %>%
      gather(year, value, -scenario, -var, -iso, -unit) %>%
      mutate(year = as.integer(year),
             value = as.numeric(value)) -> pop.ssp
    pop.labor.force.ssp %>% filter(!(var %in% "pop")) %>%
      gather(year, value, -scenario, -var, -iso, -unit) %>%
      mutate(year = as.integer(year),
             value = as.numeric(value)) -> labor.force.cohort.ssp
    labor.force.cohort.ssp %>% select(-var) %>%
      group_by(scenario, iso, unit, year) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(var ="labor.force") -> labor.force.ssp

    #include total SSP population (pop) in table
    labor.force.ssp %>%
      bind_rows(pop.ssp) %>%
      left_join_error_no_match(gcam.reg.iso, by = "iso") %>%
      select(scenario, iso, GCAM_region_ID, var, year, value, unit) %>%
      arrange(scenario, iso, var, year) ->
      L180.laborForceSSP

    # WARNING!!!
    # Not all data is available for every country and year.
    # Check for missing national accounts data by country before using for GCAM region.
    # All rates and shares should be recalculated based on total values for GCAM aggregte regions.
    L180.nationalAccounts %>% select(iso, country_name, GCAM_region_ID, year,
                                     pop.pwt, labor.force, gdp.pwt, consumption,
                                     cons.plus.invest, capital.stock, depreciation,
                                     savings, wages, hrs.worked.annual, wage.rate,
                                     labor.force.share, depreciation.rate, savings.rate,
                                     energy.investment, capital.net.export ) -> L180.nationalAccounts

    # ===================================================

    # Produce outputs

    L180.laborForceSSP %>%
      add_title("Labor Force and Pop by SSP Scenarios") %>%
      add_units("millions") %>%
      add_comments("Total pop and working age population less ages 15-19 in HS and 20-24 in college") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/SSP_database_v9") ->
      L180.laborForceSSP

    L180.nationalAccounts %>%
      add_title("Processed National Accounts Data from Penn World Table") %>%
      add_units("million 1990US$") %>%
      add_comments("National accounts data: GDP, capital, depreciation, savings rate,
               labor wages, labor productivity, labor force, and labor force share, energy investment") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names",
                     "socioeconomics/pwt91", "socioeconomics/pwt91_na",
                     "L100.GTAP_capital_stock") ->
      L180.nationalAccounts

    return_data(L180.nationalAccounts, L180.laborForceSSP)

  } else {
    stop("Unknown command")
  }
}
