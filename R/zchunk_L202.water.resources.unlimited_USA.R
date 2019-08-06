#' module_gcamusa_L202.water.resources.unlimited
#'
#' Create unlimited resource markets for water types, and read in fixed prices for water types.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.UnlimitRsrc_USA}, \code{L202.UnlimitRsrcPrice_USA}. The corresponding file in the
#' original data system was \code{L202.water.resources.unlimited.R} (water level2).
#' @details Create unlimited resource markets (i.e., 50 GCAM-USA states) for water types (i.e., water consumption, withdrawals, biophysical water consumption and seawater),
#' and read in fixed prices for water types.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG August 2019
module_gcamusa_L202.water.resources.unlimited <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L102.unlimited_water_price_state_R_W_Y_75USDm3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.UnlimitRsrc_USA",
             "L202.UnlimitRsrcPrice_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- water_type <- region <- unlimited.resource <- output.unit <- price.unit <-
      market <- capacity.factor <- value <- price <- NULL  # silence package check notes

    # Load required inputs
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    L102.unlimited_water_price_state_R_W_Y_75USDm3 <- get_data(all_data, "L102.unlimited_water_price_state_R_W_Y_75USDm3")

    # Create unlimited resource markets for water types
    L102.unlimited_water_price_state_R_W_Y_75USDm3 %>%
      # the file is in long year format (58 years), and the intent is to select one year
      # (the year order does not matter) since all other years are duplicate and not needed.
      filter(year == first(year)) %>%
      select(region, water_type) %>%
      #left_join_error_no_match(GCAM_state_names, by = "state") %>%
      mutate(market = region) %>%
      rename(unlimited.resource = water_type) %>%
      # Capacity factor is not used for water resources
      mutate(output.unit = water.WATER_UNITS_QUANTITY, price.unit = water.WATER_UNITS_PRICE, capacity.factor = 1) %>%
      #select(-GCAM_region_ID) %>%
      # Remove water goods that are only used by ag technologies, in regions with no aglu module
      #filter(!region %in% aglu.NO_AGLU_REGIONS | !unlimited.resource %in% water.AG_ONLY_WATER_TYPES) %>%
      select(region, unlimited.resource,	output.unit,	price.unit,	market) %>%
      arrange(region, unlimited.resource, output.unit, price.unit, market) ->
      UnlimitRsrc_USA

    # Read in fixed prices for water types
    L102.unlimited_water_price_state_R_W_Y_75USDm3 %>%
      filter(year %in% MODEL_YEARS) %>%
      #left_join_keep_first_only(GCAM_state_names, by = "state") %>%
      rename(unlimited.resource = water_type, price = value) %>%
      ##The following line is currently in to alter the DF in order for add_title to
      ##work appropriately. FIX!
      mutate(hello = "1") %>%
      select(-hello)%>%
      #filter(!region %in% aglu.NO_AGLU_REGIONS | !unlimited.resource %in% water.AG_ONLY_WATER_TYPES) %>%
      #select(-GCAM_region_ID) %>%
      arrange(region, unlimited.resource, year, price) ->
      UnlimitRsrcPrice_USA

    #========================================================================
    # Produce outputs
    UnlimitRsrc_USA %>%
      add_title("unlimited resources of water (i.e., water consumption, withdrawals, biophysical water consumption and seawater)") %>%
      add_units("NA") %>%
      add_comments("1) select the 1st year (1971) data records from the long format of L102.unlimited_water_price_R_W_Y_75USDm3
                      because only  records for 1 year are needed.
                   2) left_join with GCAM_region_names by GCAM_region_ID;
                   3) assign WATER_UNITS_QUANTITY, WATER_UNITS_PRICE and capacity.factor;
                   4) remove water goods that are only used by ag technologies, in regions with no aglu module.") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrc") %>%
      add_precursors("gcam-usa/states_subregions", "L102.unlimited_water_price_state_R_W_Y_75USDm3") ->
      L202.UnlimitRsrc_USA

    UnlimitRsrcPrice_USA %>%
      add_title("price for unlimited resources of water (i.e., water consumption, withdrawals, biophysical water consumption and seawater)") %>%
      add_units("1975$/m^3") %>%
      add_comments("1) select the records that cover the MODEL_YEARS period;
                   2) left_join with GCAM_region_names by GCAM_region_ID;
                   3) rename field names;
                   4) remove water goods that are only used by ag technologies, in regions with no aglu module.") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrcPrice") %>%
      add_precursors("gcam-usa/states_subregions", "L102.unlimited_water_price_state_R_W_Y_75USDm3") ->
      L202.UnlimitRsrcPrice_USA

    return_data(L202.UnlimitRsrc_USA, L202.UnlimitRsrcPrice_USA)
  } else {
    stop("Unknown command")
  }
}
