#' module_emissions_L241.fgas
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.hfc_all}, \code{L241.pfc_all}, \code{L241.hfc_future}, \code{L241.fgas_all_units}. The corresponding file in the
#' original data system was \code{L241.fgas.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
module_emissions_L241.fgas <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/FUT_EMISS_GV",
             FILE = "temp-data-inject/L141.hfc_R_S_T_Yh",
             FILE = "temp-data-inject/L141.hfc_ef_R_cooling_Yh",
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
    L142.pfc_R_S_T_Yh <- get_data(all_data, "L142.pfc_R_S_T_Yh")

    get_data(all_data, "temp-data-inject/L141.hfc_R_S_T_Yh")  %>%
      # The following 2 lines of code will be removed latter when we are using the 'real' data
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(year = as.numeric(substr(year, 2, 5))) -> L141.hfc_R_S_T_Yh

    get_data(all_data, "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # The following 2 lines of code will be removed latter when we are using the 'real' data
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(year = as.numeric(substr(year, 2, 5))) -> L141.hfc_ef_R_cooling_Yh


    # ===================================================
    # HFC emissions
    # L241.hfc: F-gas emissions for technologies in all regions
    # #Interpolate and add region name
    L141.hfc_R_S_T_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(value, emissions.DIGITS_EMISSIONS)) %>%
      select(-GCAM_region_ID, -value) -> #do i need spaces between the - and the word??/
      L241.hfc_all


    # L241.pfc: F-gas emissions for technologies in all regions
    #
    # Because no future coefs are read in for any techs, anything that's zero in all base years can be
    # removed from the data frame.
    L142.pfc_R_S_T_Yh %>%
      filter(year %in% emissions.HISTORICAL) %>%
      spread(year, value) %>%
      mutate(yrsum = rowSums(.[grep(YEAR_PATTERN, names(.))])) %>%
      filter(yrsum != 0) %>%
      gather(year, value, grep(YEAR_PATTERN, names(.))) %>%
      filter(year %in% BASE_YEARS) ->
      L142.pfc_R_S_T_Yh_future

    # Format the future gas emissions for technologies data frame for the csv file. Add GRAM region name
    # and drop/rename variables as needed. Round input emissions to the appropriate digits.
    L142.pfc_R_S_T_Yh_future %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions = value) %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS), year = as.numeric(year))->
      L241.pfc_all

    # L241.hfc_future: F-gas emissions factors for future years
    #
    # First, prepare 2010 ef for cooling
    L141.hfc_ef_R_cooling_Yh %>%
      filter(year == max(emissions.HFC_MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L141.hfc_ef_cooling_2010

    # Determine which regions need updated emissions factors
    #
    # First, subset the 2010 hcf ef 2010 data frame for USA, assume that GCAM Region ID 1 equals USA.
    # The USA hfc ef cooling emissions will be scaled and used as a factor in the next step.
    L141.hfc_ef_cooling_2010 %>%
      filter(GCAM_region_ID == 1) %>%
      select(USA_factor = `value`, -region, year, Non.CO2, supplysector) ->
      L141.hfc_ef_cooling_2010_USA

    # Match USA ef cooling emissions for 2010 with other supplysectors by year, scale the the USA factor emissions for HFC134a by
    # dividing by three since it is less commonly used now in USA.
    L141.hfc_ef_cooling_2010 %>%
      left_join_error_no_match(L141.hfc_ef_cooling_2010_USA, by = c("supplysector", "Non.CO2", "year")) %>%
      mutate(USA_factor = if_else(Non.CO2 == "HFC134a", USA_factor/3, USA_factor)) -> # replace was not working
      L241.hfc_cool_ef_2010_USfactor

    L241.hfc_cool_ef_2010_USfactor %>%
      filter(USA_factor > value) %>%
      rename("2010" = `value`, "2030" = `USA_factor`) %>%
      select(-year) ->
      L241.hfc_cool_ef_update

    L241.hfc_cool_ef_update %>%
      mutate(`2015` = `2010`+(5 / 20)*(`2030` - `2010`)) %>%
      mutate(`2020` = `2010`+(10 / 20)*(`2030` - `2010`)) %>%
      mutate(`2025` = `2010`+(15 / 20)*(`2030` - `2010`)) ->
      L241.hfc_cool_ef_update_all # check about the spaces between math operators

    L241.hfc_cool_ef_update_all %>%
      gather(year, value, grep(YEAR_PATTERN, names(.))) %>%
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_cool_ef_update_filtered


    #Then, prepare 2010 ef for non-cooling
    L141.hfc_R_S_T_Yh  %>%
      filter(!supplysector %in% c("resid cooling", "comm cooling")) %>%
      mutate(value = value * 1000) %>%  # EF is 1000 x emissions for non-cooling sectors
      filter(year == max(emissions.HFC_MODEL_BASE_YEARS)) %>%
      filter(value > 0 ) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L241.hfc_ef_2010

    #Use Data from Guus Velders to Update EF in the near term for non-cooling
    FUT_EMISS_GV %>%
      select(-Emissions, -GDP) %>%
      spread(Year, EF) %>%
      select(`Species`, `Scenario`, `2010`, `2020`, `2030`) %>%
      mutate(Species = gsub("-", "", Species )) ->
      L241.FUT_EF

    L241.FUT_EF %>%
      mutate(Ratio_2020 = `2020` / `2010`) %>%
      mutate(Ratio_2030 =  `2030` / `2010`) ->
      L241.FUT_EF_Ratio

    # Because there are different number of fgas gases in the two data sets we can used left join here because
    # we expect that there will be NA values, ultimately the NA values will be removed.
    L241.hfc_ef_2010 %>%
      left_join(L241.FUT_EF_Ratio, by = c("Non.CO2" = "Species")) %>%
      mutate(`2020` = value * Ratio_2020) %>%
      mutate(`2030` = value * Ratio_2030) %>%
      select(-Ratio_2020, -Ratio_2030, -Scenario) %>%
      na.omit() ->
      L241.hfc_ef_2010_update

    L241.hfc_ef_2010_update %>%
      select(-year, -value, -`2010`) %>%
      gather(year, value, grep(YEAR_PATTERN, names(.))) %>%
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_ef_2010_update_all

    # Combine the updated data frames of updated data together. Then format the dataset for the csv file.
    L241.hfc_ef_2010_update_all %>%
      bind_rows(L241.hfc_cool_ef_update_filtered) %>%
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS), year = as.numeric(year)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L241.hfc_future


      if(OLD_DATA_SYSTEM_BEHAVIOR){
    # Now subset only the relevant technologies and gases (i.e., drop ones whose values are zero in all years). The old data system
    # fails to drop technologies and gases that have zero emissions in all years. I talked to Kate and she said that this
    # this has no implications for model performance. The if(OLD_DATA_SYSTEM_BEHVAIOR) is technically unnecessary because the 0s
    # can be left in.
    L241.hfc_all %>%
      mutate(year = as.numeric(year)) ->
      L241.hfc_all

    }else{

      # Subset so that only the relevant technologies and gases (i.e., those whose emission values are zero for all years are dropped).
      L241.hfc_all %>%
        spread(year, input.emissions) %>%
        mutate(yrsum = rowSums(.[grep(YEAR_PATTERN, names(.))])) %>%
        filter(yrsum != 0) %>%
        gather(year, input.emissions, grep(YEAR_PATTERN, names(.))) %>%
        mutate(year = as.numeric(year)) ->
        L241.hfc_all
    } # end of if old data system

    # Set the units string for all fgases
    L241.pfc_all %>%
      bind_rows(L241.hfc_all) %>%
      bind_rows(L241.hfc_future) %>%
      select(-input.emissions, -emiss.coeff) %>%
      unique %>%
      mutate(emissions.unit = emissions.F_GAS_UNITS) ->
      L241.fgas_all_units

    if(OLD_DATA_SYSTEM_BEHAVIOR){

    } else {
    # Formating the data frames to write as csv. Add value and year to columns. Now the columns names of the
    # new.csv files do not match
    L241.hfc_all %>%
      rename(value = `input.emissions`) ->
      L241.hfc_all

    L241.pfc_all %>%
      rename(value = `input.emissions`) ->
      L241.pfc_all

    L241.hfc_future %>%
      rename(value = `emiss.coeff`) ->
      L241.hfc_future

    L241.fgas_all_units %>%
      rename(value = `emissions.unit`) ->
      L241.fgas_all_units
    }


    # ===================================================

    # Produce outputsdriver()
    L241.hfc_all %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.hfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_all

    L241.pfc_all %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.pfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") ->
      L241.pfc_all

    L241.hfc_future %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.hfc_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_future

    L241.fgas_all_units %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.fgas_all_units") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") ->
      L241.fgas_all_units

    return_data(L241.hfc_all, L241.pfc_all, L241.hfc_future, L241.fgas_all_units)

  } else {
    stop("Unknown command")
  }
}
