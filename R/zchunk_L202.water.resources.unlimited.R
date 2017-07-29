#' module_water_L202.water.resources.unlimited
#'
#' Create unlimited resource markets for water types, and read in fixed prices for water types.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.UnlimitRsrc}, \code{L202.UnlimitRsrcPrice}. The corresponding file in the
#' original data system was \code{L202.water.resources.unlimited.R} (water level2).
#' @details Create unlimited resource markets (i.e., 32 GCAM regions) for water types (i.e., water consumption, withdrawals, biophysical water consumption and seawater),
#' and read in fixed prices for water types.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YL July 2017
module_water_L202.water.resources.unlimited <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L102.unlimited_water_price_R_W_Y_75USDm3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.UnlimitRsrc",
             "L202.UnlimitRsrcPrice"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- GCAM_region_ID <- water_type <- region <- unlimited.resource <- output.unit <- price.unit <-
      market <- capacity.factor <- value <- price <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L102.unlimited_water_price_R_W_Y_75USDm3 <- get_data(all_data, "L102.unlimited_water_price_R_W_Y_75USDm3")

    # Create unlimited resource markets for water types
    L102.unlimited_water_price_R_W_Y_75USDm3 %>%
      # the file is in long year format (58 years), and the intent is to select one year
      # (the year order does not matter) since all other years are duplicate and not needed.
      filter(year == first(year)) %>%
      select(GCAM_region_ID, water_type) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(market = region) %>%
      rename(unlimited.resource = water_type) %>%
      # Capacity factor is not used for water resources
      mutate(output.unit = WATER_UNITS_QUANTITY, price.unit = WATER_UNITS_PRICE, capacity.factor = 1) %>%
      select(-GCAM_region_ID) %>%
      # Remove water goods that are only used by ag technologies, in regions with no aglu module
      filter(!region %in% aglu.NO_AGLU_REGIONS | !unlimited.resource %in% AG_ONLY_WATER_TYPES) %>%
      arrange(region, unlimited.resource, output.unit, price.unit, market, capacity.factor) ->
      UnlimitRsrc

    # Read in fixed prices for water types
    L102.unlimited_water_price_R_W_Y_75USDm3 %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_keep_first_only(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(unlimited.resource = water_type, price = value) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS | !unlimited.resource %in% AG_ONLY_WATER_TYPES) %>%
      select(-GCAM_region_ID) %>%
      arrange(region, unlimited.resource, year, price) ->
      UnlimitRsrcPrice

    #========================================================================
    # Produce outputs
    UnlimitRsrc %>%
      add_title("unlimited resources of water (i.e., water consumption, withdrawals, biophysical water consumption and seawater)") %>%
      add_units("NA") %>%
      add_comments("1) select the 1st year (1971) data records from the long format of L102.unlimited_water_price_R_W_Y_75USDm3
                      because only  records for 1 year are needed.
                   2) left_join with GCAM_region_names by GCAM_region_ID;
                   3) assign WATER_UNITS_QUANTITY, WATER_UNITS_PRICE and capacity.factor;
                   4) remove water goods that are only used by ag technologies, in regions with no aglu module.") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrc") %>%
      add_precursors("common/GCAM_region_names", "L102.unlimited_water_price_R_W_Y_75USDm3") ->
      L202.UnlimitRsrc

    UnlimitRsrcPrice %>%
      add_title("price for unlimited resources of water (i.e., water consumption, withdrawals, biophysical water consumption and seawater)") %>%
      add_units("1975$/m^3") %>%
      add_comments("1) select the records that cover the MODEL_YEARS period;
                   2) left_join with GCAM_region_names by GCAM_region_ID;
                   3) rename field names;
                   4) remove water goods that are only used by ag technologies, in regions with no aglu module.") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrcPrice") %>%
      add_precursors("common/GCAM_region_names", "L102.unlimited_water_price_R_W_Y_75USDm3") ->
      L202.UnlimitRsrcPrice

    return_data(L202.UnlimitRsrc, L202.UnlimitRsrcPrice)
  } else {
    stop("Unknown command")
  }
}
