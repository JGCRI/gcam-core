#' module_aglu_LB133.ag_Costs_USA_C_2005
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L133.ag_Cost_75USDkg_C}. The corresponding file in the
#' original data system was \code{LB133.ag_Costs_USA_C_2005.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB133.ag_Costs_USA_C_2005 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/USDA_crops",
             FILE = "aglu/USDA_item_cost",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "aglu/USDA_cost_data",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L133.ag_Cost_75USDkg_C"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    USDA_crops <- get_data(all_data, "aglu/USDA_crops")
    USDA_item_cost <- get_data(all_data, "aglu/USDA_item_cost")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    USDA_cost_data <- get_data(all_data, "aglu/USDA_cost_data")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

    # convert USDA_cost_data to long form:
    USDA_cost_data %>%
      gather(year, value, -Crop, -Region, -Item, -Unit) %>%
      # force year to integer
      mutate(year = as.integer(year)) ->
      USDA_cost_data

    # 2. Perform computations

    # Lines 35-43 in original file
    # old comment: Add vectors for GCAM commodity and cost component
    # Take USDA item cost data from input table USDA_cost_data, and add GCAM commodity and GTAP crop mapping info
    # from the USDA crop mapping input table, USDA_crops.
    # Then add cost type (variable or na) from the USDA_item_cost input table.
    # Finally, select only vairable price data, only in MODEL_COST_YEARS = 2001:2005 by default
    # Take USDA item cost data:
    USDA_cost_data %>%
      # join in the GCAM and GTAP mapping information:
      left_join_error_no_match(USDA_crops, by = c("Crop" = "USDA_crop")) %>%
      # then join cost_type information:
      left_join_error_no_match(USDA_item_cost, by = c("Item")) %>%
      # select only variable cost data and only MODEL_COST_YEARS
      filter(cost_type == "variable", year %in% MODEL_COST_YEARS)  %>%
      # select just identifying information of interest:
      select(GCAM_commodity, GTAP_crop, Item, year, value) ->
      # in a table of costs ub US Dollars/acre by commodity and year:
      L133.ag_Cost_USDacr_C_Y




    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L133.ag_Cost_75USDkg_C") %>%
      add_precursors("aglu/USDA_crops",
                     "aglu/USDA_item_cost",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "aglu/USDA_cost_data",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L132.ag_an_For_Prices") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L133.ag_Cost_75USDkg_C

    return_data(L133.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
