#' module_gcamusa_L273.nonghg_fossil_res_USA
#'
#' Non-GHG input emissions parameters for fossil resource production in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.nonghg_state_fossil_res_EF_USA}, \code{L273.ResReadInControl_nonghg_res_USA}.
#' @details This chunk calculates Non-GHG emissions coefficients for fossil resource production (oil, gas, and coal) in the USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW/ TRW September 2023

module_gcamusa_L273.nonghg_fossil_res_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/emissions/BC_OC_assumptions",
             FILE = "gcam-usa/emissions/BCOC_PM25_ratios",
             "L270.nonghg_tg_state_fossil_res_F_Yb",
             "L222.StubTech_en_USA",
             "L211.RsrcCalProd_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.nonghg_state_fossil_res_EF_USA",
             "L273.ResReadInControl_nonghg_res_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- region <- supplysector <- subsector <- calibrated.value <- Non.CO2 <- value <- input.emissions <-
      fuel_input <- emiss.coef <- fuel <- sector <- emiss.coeff <- stub.technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions", strip_attributes = TRUE)
    BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios", strip_attributes = TRUE)
    L270.nonghg_tg_state_fossil_res_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_fossil_res_F_Yb", strip_attributes = TRUE)
    L222.StubTech_en_USA <- get_data(all_data, "L222.StubTech_en_USA", strip_attributes = TRUE)
    L211.RsrcCalProd_USA <- get_data(all_data, "L211.RsrcCalProd_USA", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # Start by preparing the table needed to split BC/OC emissions from PM2.5

    # Use fractions of PM2.5 to calculate BC/OC emissions.
    # We need to modify the BC_OC_assumptions table, as the BCOC_PM25_ratios table has updated values that are time dependent
    # If there are sector/subsector/tech combos that are in BCOC_PM25_ratios, we want to replace those entries in
    # the BC_OC_assumptions table. We also need to extend the data.
    # Extrapolate the data to future model years, and format the table
    BCOC_PM25_ratios_ext <- BCOC_PM25_ratios %>%
      gather_years() %>%
      complete(nesting(Parameter,sector,subsector,technology),
               year = MODEL_YEARS) %>%
      # extrapolate missing years
      group_by(Parameter,sector,subsector,technology) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      spread( Parameter, value ) %>%
      rename( "BC_fraction" = `BC Fraction`,
              "OC_fraction" = `OC Fraction`)

    BC_OC_assumptions_ext <- BC_OC_assumptions %>%
      mutate( year = min( MODEL_BASE_YEARS ) ) %>%
      complete(nesting(sector,subsector,technology, BC_fraction, OC_fraction),
               year = MODEL_YEARS)

    # Join the tables, keeping values from BC_OC_assumptions_ext that do not appear in BCOC_PM25_ratios
    BC_OC_assumptions_years <- BC_OC_assumptions_ext %>%
      anti_join( BCOC_PM25_ratios_ext,
                 by = c("sector", "subsector", "technology", "year") ) %>%
      # bind to BCOC_PM25_assumptions
      bind_rows( BCOC_PM25_ratios_ext ) %>%
      # since CEDS has years between model base years, we want to fill this table in
      complete(nesting(sector,subsector,technology), year = HISTORICAL_YEARS) %>%
      # extrapolate missing years
      group_by(sector,subsector,technology) %>%
      mutate(BC_fraction = approx_fun(year, BC_fraction, rule = 2),
             OC_fraction = approx_fun(year, OC_fraction, rule = 2)) %>%
      ungroup()

    # ===================================================
    # Consistent with GCAM-32 method - NEI “petroleum production” and “natural gas production and distribution” are combined
    # into total oil and gas emissions, then split by energy shares to derive emissions factors.
    # except for outlier replacement, which is done state-by-state rather than comparing the total US to other GCAM regions

    NEI_tg_oilgas_state_Yb_usa.noBCOC <- L270.nonghg_tg_state_fossil_res_F_Yb %>%
      filter(sector %in% c("NG_production_distribution", "petroleum_production")) %>%
      group_by(state, Non.CO2, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      # add columns that specify what sector / tech these emissions are (necessary for BC OC function)
      # since we are aggregated natural gas and oil production, and they have the same BC OC fractions,
      # we just need to assign either natural gas or oil as the sector
      mutate(supplysector = "petroleum_production",
             subsector = as.character(NA), stub.technology = as.character(NA)) %>%
      rename(region = state)


    # Compute BC and OC EFs based off of PM2.5
    NEI_tg_oilgas_USA_Yb_usa <-
      compute_BC_OC(NEI_tg_oilgas_state_Yb_usa.noBCOC %>%
                      # renaming value to emiss.coef for use in compute_BC_OC(),
                      # even though they are emissions not emissions coefficients
                      rename(emiss.coef = value),
                    BC_OC_assumptions_years) %>%
      # change emiss.coef back to emissions
      rename(emissions = emiss.coef) %>%
      # select columns we want to keep
      select(region, Non.CO2, year, emissions)

    # match aggregate oil and gas emissions with energy
    # to calculate emissions coefficients

    # get energy consumption
    oil_gas_prod_energy <- L211.RsrcCalProd_USA %>%
      filter( resource %in% c("natural gas", "crude oil"),
              year %in% MODEL_YEARS,
              region %in% gcamusa.STATES ) %>%
      # combine oil and gas production
      group_by(region, year) %>%
      summarize(cal.production = sum(cal.production))

    # join with emissions
    oil_gas_EFs_withNAs <- NEI_tg_oilgas_USA_Yb_usa %>%
      filter(year %in% MODEL_YEARS) %>%
      # can't use LJENM b/c there are some year/region/gas combinations with
      # emissions data but no production data; these will be replaced with the
      # national median emissions factor later
      left_join(oil_gas_prod_energy,
                by = c("region", "year")) %>%
      # calculate emission factor
      mutate(emiss.coef = emissions / cal.production) %>%
      select(-c(emissions, cal.production)) %>%
      # extrapolate missing years (NEI data start in 1990 so 1975 is missing)
      complete(nesting(region, Non.CO2), year = MODEL_BASE_YEARS) %>%
      group_by(region, Non.CO2) %>%
      mutate(emiss.coef = approx_fun(year, emiss.coef, rule = 2)) %>%
      ungroup()

    # calculate national median emissions factors to replace missing ones
    oil_gas_median_emfacts <- oil_gas_EFs_withNAs %>%
      # remove Inf emfacts so as not to skew the median (these will be replaced)
      # but keep NA emfacts so that year/Non.CO2 combinations with
      # no EFs for any regions can be assigned 0
      filter(emiss.coef != Inf | is.na(emiss.coef)) %>%
      group_by(Non.CO2, year) %>%
      summarise(median_emfact = median(emiss.coef, na.rm = T)) %>%
      replace_na(list(emiss.coef = 0))


    # create a structure with all year-state-nonCO2-resource-subresource combinations
    oil_gas_prod_structure <- L211.RsrcCalProd_USA %>%
      select(resource, reserve.subresource) %>%
      filter(resource %in% c("natural gas", "crude oil")) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(NEI_tg_oilgas_USA_Yb_usa$Non.CO2)))

    # assign the aggregate oil_gas emissions factor to each resource/subresource
    # and replace missing emissions factors with the national median
    # also replace emissions factors above a threshold (20 times the national median)
    L273.nonghg_state_fossil_res_EF_USA <- oil_gas_prod_structure %>%
      # can't use LJENM because there are missing EFs (will be replaced)
      left_join(oil_gas_EFs_withNAs,
                by = c("region", "Non.CO2", "year")) %>%
      left_join_error_no_match(oil_gas_median_emfacts,
                               by = c("Non.CO2", "year")) %>%
      mutate(threshold = median_emfact*20,
             emiss.coef = case_when(is.na(emiss.coef) | emiss.coef > threshold ~ median_emfact,
                                    T ~ emiss.coef),
             resource.reserve.technology = reserve.subresource) %>%
      select(region, resource, subresource = reserve.subresource,
             technology = resource.reserve.technology, year, Non.CO2, emiss.coef) %>%
      mutate(emiss.units = "Tg")

    # comment below copied from zemissions_L201.en_nonco2.R.....................

    # Resources have vintaging going on in the historical years.
    # The above emissions coefficients are the coefficients across vintages
    # in the given model year.  The best way to ensure the correct "total"
    # emissions factor across vintages is realized is to use the ReadInControl
    # to change the coefficients by vintage for all vintages.  Note given the way
    # the C++ operates we need to read in the "base" EmissCoef table and then the
    # same values in the ReadInControl table all read into the first model period
    # vintage.  We will also need to "fillout" the value in the final calibration
    # to the future model periods otherwise the vintage would revert back to the
    # value in the EmissCoef table.  Finally to turn "off" any adjustments made to
    # new vintages in future model periods we must have ReadInControl with values of
    # zero starting in the first future model period.


    # L273.ResReadInControl_nonghg_res_USA:
    # Vintage adjustments for Pollutant emissions for energy resources in all states
    L273.nonghg_state_fossil_res_EF_USA %>%
      # copy the final historical year value to the future model periods
      bind_rows(L273.nonghg_state_fossil_res_EF_USA %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))) %>%
      rename(future.emiss.coeff.year = year) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             future.emiss.coeff.name = "vintage_adjust") %>%
      select(LEVEL2_DATA_NAMES[["ResReadInControl"]])->
      L273.ResReadInControl_nonghg_res_USA
    # turn "off" vintage adjustments for future year vintages
    L273.ResReadInControl_nonghg_res_USA %>%
      mutate(year = MODEL_FUTURE_YEARS[1],
             emiss.coef = 0.0) %>%
      bind_rows(L273.ResReadInControl_nonghg_res_USA) ->
      L273.ResReadInControl_nonghg_res_USA

    # ===================================================

    # Produce outputs

    L273.nonghg_state_fossil_res_EF_USA %>%
      add_title("Non-GHG emission factor parameters for refining technologies in the USA") %>%
      add_units("") %>%
      add_comments("Emission factors at the USA level for natural gas and petroleum distribution") %>%
      add_precursors(FILE="gcam-usa/states_subregions",
                     FILE = "gcam-usa/emissions/BC_OC_assumptions",
                     FILE = "gcam-usa/emissions/BCOC_PM25_ratios",
                     "L270.nonghg_tg_state_fossil_res_F_Yb",
                     "L222.StubTech_en_USA",
                     "L211.RsrcCalProd_USA") ->
      L273.nonghg_state_fossil_res_EF_USA

    L273.ResReadInControl_nonghg_res_USA %>%
      add_title("Vintaging adjustments for nonGHG emission factors from resource production") %>%
      add_units("Tg/EJ") %>%
      add_comments("Used to make per vintage adjustments to ensure overall emissions") %>%
      add_comments("factors match the ones in L273.nonghg_state_fossil_res_EF_USA in historical years") %>%
      add_precursors(FILE="gcam-usa/states_subregions",
                         FILE = "gcam-usa/emissions/BC_OC_assumptions",
                         FILE = "gcam-usa/emissions/BCOC_PM25_ratios",
                         "L270.nonghg_tg_state_fossil_res_F_Yb",
                         "L222.StubTech_en_USA",
                         "L211.RsrcCalProd_USA") ->
      L273.ResReadInControl_nonghg_res_USA

    return_data(L273.nonghg_state_fossil_res_EF_USA, L273.ResReadInControl_nonghg_res_USA)

  } else {
    stop("Unknown command")
  }
}
