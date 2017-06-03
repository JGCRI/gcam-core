#' module_aglu_LA105.an_FAO_R_C_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.an_Food_Mt_R_C_Y}, \code{L105.an_Food_Pcal_R_C_Y}, \code{L105.an_kcalg_R_C_Y}, \code{L105.an_Prod_Mt_R_C_Y}, \code{L105.an_Prod_Mt_ctry_C_Y}, \code{L105.an_StockShares_R_BufGoat_2005}. The corresponding file in the
#' original data system was \code{LA105.an_FAO_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA105.an_FAO_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "aglu/FAO_an_items_cal_SUA",
             "L100.FAO_an_Food_t",
             "L100.FAO_an_Prod_t",
             FILE = "temp-data-inject/L100.FAO_an_Stocks"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.an_Food_Mt_R_C_Y",
             "L105.an_Food_Pcal_R_C_Y",
             "L105.an_kcalg_R_C_Y",
             "L105.an_Prod_Mt_R_C_Y",
             "L105.an_Prod_Mt_ctry_C_Y",
             "L105.an_StockShares_R_BufGoat_2005"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO_an_items_cal_SUA")
    L100.FAO_an_Food_t <- get_data(all_data, "L100.FAO_an_Food_t")
    L100.FAO_an_Prod_t <- get_data(all_data, "L100.FAO_an_Prod_t")
    L100.FAO_an_Stocks <- get_data(all_data, "temp-data-inject/L100.FAO_an_Stocks") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -countries, -country.codes, -item, -item.codes, -element, -element.codes, -iso) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    # Process FAO animal products food consumption data: map in GCAM region and commodities, convert units, aggregate to region and commodity
    L100.FAO_an_Food_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item")  %>%                                       # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(Mt = value * CONV_TON_MEGATON, Pcal = value * Mcal_t * CONV_MCAL_PCAL)  %>%      # Convert from tons to Mt and Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(Mt = sum(Mt), Pcal = sum(Pcal)) %>%                                           # Aggregate food consumption in Mt and Pcal
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(Mt = 0, Pcal = 0)) %>%
      # Calculate average caloric content of consumed commodities (kcal/g) for each region and animal type, set NA values to 1
      mutate(value = if_else(Mt == 0, 1, Pcal / Mt))  ->
      L105.an_Food_R_C_Y

    # Build table of food consumption in Mt
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, Mt) %>%
      rename(value = Mt) ->
      L105.an_Food_Mt_R_C_Y
    # Build table of food consumption in Pcal
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, Pcal) %>%
      rename(value = Pcal) ->
      L105.an_Food_Pcal_R_C_Y
    # Build table of caloric content in kcal/g
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) ->
      L105.an_kcalg_R_C_Y

    # Process FAO animal products production data: map in GCAM region and commodities, convert units
    L100.FAO_an_Prod_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item")  %>%                                       # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON )  ->                                           # Convert from tons to Mt
      L105.FAO_an_Prod_Mt
    # Build country table, aggregate to iso and GCAM commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(iso, GCAM_commodity, year) %>%                                                 # Group by iso, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(iso = unique(iso_GCAM_regID$iso),                                              # Fill in missing country/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_ctry_C_Y
    # Build region table, aggregate to GCAM region and commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_R_C_Y

    # For water downscaling to specific animal types, calculate the shares of selected animal types by GCAM region
    L100.FAO_an_Stocks %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                               # Map in GCAM regions
      mutate(animal_type = "Bovine",
             animal_type = replace(animal_type, item %in% c("Sheep", "Goats"), "SheepGoat")) %>%
      # We only care about 2005 right now
      filter(year == 2005) ->
      L105.FAO_an_Stocks_2005

    # Aggregate by GCAM region and animal type
    L105.FAO_an_Stocks_2005 %>%
      group_by(GCAM_region_ID, animal_type) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L105.an_Stocks_R_type_2005

    # Aggregate by GCAM region and FAO item, and calculate the shares of buffaloes and goats of animal type in each region
    L105.FAO_an_Stocks_2005 %>%
      filter(item %in% c("Buffaloes", "Goats")) %>%
      group_by(GCAM_region_ID, item, animal_type) %>%
      summarise(value_item = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L105.an_Stocks_R_type_2005, by = c("GCAM_region_ID", "animal_type")) %>%
      # Calculate shares of buffaloes and goats of animal type
      mutate(value = value_item / value) %>%
      select(-value_item, -animal_type) %>%
      full_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      # Fill in missing regions with 0
      complete(region, item, fill = list(value = 0)) %>%
      spread(item, value) %>%
      rename(bfracFAO2005 = Buffaloes, gfracFAO2005 = Goats) ->
      L105.an_StockShares_R_BufGoat_2005

    # Produce outputs
    L105.an_Food_Mt_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_Food_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Food_t") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_Food_Mt_R_C_Y

    L105.an_Food_Pcal_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_Food_Pcal_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Food_t") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_Food_Pcal_R_C_Y

    L105.an_kcalg_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_kcalg_R_C_Y") %>%
      add_precursors() %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_kcalg_R_C_Y

    L105.an_Prod_Mt_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_Prod_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Prod_t") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_Prod_Mt_R_C_Y

    L105.an_Prod_Mt_ctry_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_Prod_Mt_ctry_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Prod_t") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_Prod_Mt_ctry_C_Y

    L105.an_StockShares_R_BufGoat_2005 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L105.an_StockShares_R_BufGoat_2005") %>%
      add_precursors("common/GCAM_region_names",
                     "temp-data-inject/L100.FAO_an_Stocks") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L105.an_StockShares_R_BufGoat_2005

    return_data(L105.an_Food_Mt_R_C_Y, L105.an_Food_Pcal_R_C_Y, L105.an_kcalg_R_C_Y, L105.an_Prod_Mt_R_C_Y, L105.an_Prod_Mt_ctry_C_Y, L105.an_StockShares_R_BufGoat_2005)
  } else {
    stop("Unknown command")
  }
}
