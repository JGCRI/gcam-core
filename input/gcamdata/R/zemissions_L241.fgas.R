# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L241.fgas
#'
#' Format fgases emission inputs for GCAM and estimates future emission factors for f gases for the SSP scenarios.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.hfc_all}, \code{L241.pfc_all}, \code{L241.hfc_future}, \code{L241.fgas_all_units}. The corresponding file in the
#' original data system was \code{L241.fgas.R} (emissions level2).
#' @details Formats hfc and pfc gas emissions for input. Calculates future emission factors for hfc gases based on 2010 region emissions and USA emission factors and emission factors from Guus Velders (http://www.sciencedirect.com/science/article/pii/S135223101530488X) for the  SSP scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr gather spread
#' @author KD July 2017
module_emissions_L241.fgas <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/FUT_EMISS_GV",
             "L141.hfc_R_S_T_Yh",
             "L141.hfc_ef_R_cooling_Yh",
             "L142.pfc_R_S_T_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions         <- get_data(all_data, "emissions/A_regions")
    FUT_EMISS_GV      <- get_data(all_data, "emissions/FUT_EMISS_GV")
    L142.pfc_R_S_T_Yh <- get_data(all_data, "L142.pfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_R_S_T_Yh <- get_data(all_data, "L141.hfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_ef_R_cooling_Yh <- get_data(all_data, "L141.hfc_ef_R_cooling_Yh", strip_attributes = T)

    ## silence package check.
    . <- `2010` <- `2020` <- `2030` <- EF <- Emissions <- GCAM_region_ID <- GDP <-
      Non.CO2 <- Ratio_2020 <- Ratio_2030 <- Scenario <- Species <- USA_factor <-
      Year <- curr_table <- emiss.coeff <- input.emissions <- region <-
      stub.technology <- subsector <- supplysector <- value <- year <-
      year_min <- NULL

    # ===================================================
    # Format and round emission values for HFC gas emissions for technologies in all regions.
    L141.hfc_R_S_T_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(value, emissions.DIGITS_EMISSIONS)) %>%
      select(-GCAM_region_ID, -value) ->
      L241.hfc_all

    # L241.pfc: F-gas emissions for technologies in all regions.
    #
    # Remove anything that's zero in all base years for any technology, because no future
    # coefs are read in for any techs.
    #
    # Then round future gas emissions and format the data frame.
    L142.pfc_R_S_T_Yh %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2) %>%
      filter(sum(value) != 0, year %in% MODEL_BASE_YEARS) %>%
      mutate(input.emissions = round(value, emissions.DIGITS_EMISSIONS), year = as.numeric(year)) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID, -value) ->
      L241.pfc_all


    # F-gas emissions factors for future years
    #
    # First, create a subset of the cooling emission factors from  the max year, currently 2010.
    # (Update 11 Aug 2017: subset the last HFC_MODEL_BASE_YEARS present in data, letting us pass timeshift test.)
    # Eventually these values will be used to estimate future emission factors by scaling with
    # USA emission factors.
    MAX_DATA_YEAR <- max(intersect(L141.hfc_ef_R_cooling_Yh$year, emissions.HFC_MODEL_BASE_YEARS))

    L141.hfc_ef_R_cooling_Yh %>%
      filter(year == MAX_DATA_YEAR) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L141.hfc_ef_cooling_maxhistyr


    # From the max historical year (2010) hfc cooling emission factors select USA emission factors, in
    # subsequent steps the USA emission factors will be used to estimate future
    # emission factors.
    # But first correct the USA factor emissions for HFC134a by dividing by three
    # since it is less commonly used now in USA.
    L141.hfc_ef_cooling_maxhistyr %>%
      filter(region == gcam.USA_REGION) %>%
      mutate(value = if_else(Non.CO2 == "HFC134a", value / 3, value)) %>%
      select(value, -region, Non.CO2, supplysector) ->
      L141.hfc_ef_cooling_maxhistyr_USA


    # Match USA cooling hfc emissions factors from by sector and gas with max hist year (2010)
    # emission factors for other regions. Eventually the USA factor emissions will
    # be used to interpolate future emission factors for the other regions.
    #
    L141.hfc_ef_cooling_maxhistyr %>%
      select(-year, -value) %>%
      left_join_error_no_match(L141.hfc_ef_cooling_maxhistyr_USA, by = c("supplysector", "Non.CO2")) %>%
      mutate(year = emissions.HFC_FUT_YEAR) ->
      L241.hfc_cool_ef_futyr_USfactor

    # Format the data frame of max hist year (2010) regional emission factors and max hist year (2010)
    # USA emission factors for the next step where future emission factors are calculated.
    #
    # Future emission factors are will not be calculated for regions with max hist year (2010) emission factors
    # greater than the max hist year (2010) USA emission factor because of the way that the calculated as a
    # fraction of the change between the region and USA max hist year (2010) emission factors, negative emission
    # factors would be estimated.
    L141.hfc_ef_cooling_maxhistyr %>%
      bind_rows(L241.hfc_cool_ef_futyr_USfactor) %>%
      group_by(GCAM_region_ID , supplysector, subsector, stub.technology, Non.CO2, region) %>%
      filter(value[year == emissions.HFC_FUT_YEAR] > value[year == MAX_DATA_YEAR]) %>%
      ungroup() ->
      L241.hfc_cool_ef_update

    # Linearlly interpolate future regional emission factors from max yr (2010) emission factor and
    # the max year (2010) USA emission factor for all model years between
    # MAX_DATA_YEAR (2010) and emissions.HFC_FUT_YEAR.
    years_to_complete <- MODEL_YEARS[MODEL_YEARS < emissions.HFC_FUT_YEAR & MODEL_YEARS > MAX_DATA_YEAR]
    L241.hfc_cool_ef_update %>%
      complete(year = years_to_complete, nesting(GCAM_region_ID, Non.CO2, region,
                                                 stub.technology, subsector, supplysector)) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2) %>%
      mutate(value = approx_fun(as.numeric(year), value))  %>%
      ungroup() ->
      L241.hfc_cool_ef_update_all

    # Subset the future emission factors for the hfc model base years.
    #
    # These emission factors will be used in a ratio to compare
    # future emission factors.
    L241.hfc_cool_ef_update_all %>%
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_cool_ef_update_filtered


    # Estimate future emission for non-cooling emissions.
    #
    # First, subset the hfc emissions for non-cooling emissions.
    L141.hfc_R_S_T_Yh %>%
      filter(!supplysector %in% c("resid cooling", "comm cooling")) %>%
      # EF is 1000 x emissions for non-cooling sectors
      mutate(value = value * 1000) %>%
      filter(year == MAX_DATA_YEAR) %>%
      filter(value > 0) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L241.hfc_ef_maxhistyr

    # Use data from Guus Velders (a f-gas expert) of near future f gas
    # emissions to calculate the future to max hist yr (2010) emission factor ratios.
    # These emission factor ratios will be used to update the non-cooling
    # emission factors.
    #
    # Format the FUT_EMISS_GV species by removing the "-" so that the species
    # can be used to join the FUT_EMISS_GV emission factors with L241.hfc_ef_2010
    # in the next step.
    #
    # In case max_data_year is not in FUT_EMISS_GV (ie timeshift), use minimum year instead
    if(MAX_DATA_YEAR %in% FUT_EMISS_GV$Year){
      ratio_years <- c(MAX_DATA_YEAR, emissions.GV_YEARS)
    } else {
      ratio_years <-  c(min(FUT_EMISS_GV$Year), emissions.GV_YEARS)}

    FUT_EMISS_GV %>%
      select(-Emissions, -GDP, -Scenario) %>%
      rename(year = Year) %>%
      filter(year %in% ratio_years) %>%
      group_by(Species) %>%
      mutate(ratio = EF / EF[year == min(ratio_years)]) %>%
      ungroup %>%
      mutate(Species = gsub("-", "", Species)) %>%
      filter(year %in% emissions.GV_YEARS) ->
      L241.FUT_EF_Ratio

    # Use the future emission factor ratios to update/scale the non-cooling
    # emission factors.
    L241.hfc_ef_maxhistyr %>%
      select(-year) %>%
      # Since Guus Velders data set contains information on extra gases we can use left_join here because we expect there to be NAs that will latter be removed.
      left_join(L241.FUT_EF_Ratio, by = c("Non.CO2" = "Species")) %>%
      mutate(value = value * ratio) %>%
      select(-ratio, -EF) %>%
      na.omit() %>%
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_ef_update_all

    # Combine the updated cooling and non-cooling hfc gas emission
    # factor data frames together.
    L241.hfc_ef_update_all %>%
      bind_rows(L241.hfc_cool_ef_update_filtered) %>%
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS),
             year = as.numeric(year)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L241.hfc_future

    # Now subset only the relevant technologies and gases (i.e., drop ones whose values are zero in all years).
    L241.hfc_all %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      filter(sum(input.emissions) != 0, year %in% MODEL_BASE_YEARS) %>%
      mutate(year = as.numeric(year)) %>%
      ungroup ->
      L241.hfc_all

    # Set the units string for the hfc and pfc gases.
    L241.pfc_all %>%
      bind_rows(L241.hfc_all) %>%
      bind_rows(L241.hfc_future) %>%
      # we need to just add the unit tag for all gasses and model years
      # however we need to be careful because some gasses do not start
      # in the same year
      select(region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      summarize(year_min = min(year)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year >= year_min) %>%
      select(-year_min) %>%
      mutate(emissions.unit = emissions.F_GAS_UNITS) ->
      L241.fgas_all_units

    # ===================================================

    L241.hfc_all %>%
      add_title("HFC gas emission input table") %>%
      add_units("Gg") %>%
      add_comments("Emission values from L1 rounded to the appropriate digits.") %>%
      add_legacy_name("L241.hfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_all

    L241.pfc_all %>%
      add_title("PFC gas emission input table") %>%
      add_units("Gg") %>%
      add_comments("Emission values from L1 are rounded to the appropriate digits.") %>%
      add_legacy_name("L241.pfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.pfc_all

    L241.hfc_future %>%
      add_title("Future HFC emission factors") %>%
      add_units("Gg") %>%
      add_comments("Cooling future emission factors are calculated from 2010 USA emission factors.") %>%
      add_comments("Non-cooling future emission factors are calculated from Guus Velders emission factors.") %>%
      add_legacy_name("L241.hfc_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_future

    L241.fgas_all_units %>%
      add_title("Units for f gases.") %>%
      add_units("Gg") %>%
      add_comments("NA") %>%
      add_legacy_name("L241.fgas_all_units") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.fgas_all_units

    return_data(L241.hfc_all, L241.pfc_all, L241.hfc_future, L241.fgas_all_units)

  } else {
    stop("Unknown command")
  }
}
