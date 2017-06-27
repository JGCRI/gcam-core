#' module_aglu_LB164.ag_Costs_USA_C_2005_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L164.ag_Cost_75USDkg_C}. The corresponding file in the
#' original data system was \code{LB164.ag_Costs_USA_C_2005_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB164.ag_Costs_USA_C_2005_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/USDA_crops",
             FILE = "aglu/USDA_item_cost",
             FILE = "aglu/USDA_cost_data",
             "L100.LDS_ag_HA_ha",
             "L133.ag_Cost_75USDkg_C",
             FILE = "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L164.ag_Cost_75USDkg_C"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    USDA_crops <- get_data(all_data, "aglu/USDA_crops")
    USDA_item_cost <- get_data(all_data, "aglu/USDA_item_cost")
    USDA_cost_data <- get_data(all_data, "aglu/USDA_cost_data")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L133.ag_Cost_75USDkg_C <- get_data(all_data, "L133.ag_Cost_75USDkg_C")
    L161.ag_irrHA_frac_R_C_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU")


    # Perform computations
    # old comment: The method here is to start from the cost file that has already been processed, and
    #              to deduct costs of purchased irrigation water from each cost estimate. This is less
    #              repetitive than following all of the same steps in that prior file, but with irrigation
    #              water mapped elsewhere.

    # Step 1: Compute the share of total variable costs from purchased irrigation water.
    # CostFrac = IrrCost / TotCost
    USDA_cost_data %>%
      gather(year, cost, -Crop, -Item, -Unit) %>%
      mutate(year = as.integer(year)) %>%
      filter(Item == "Purchased irrigation water" | Item == "Total operating costs",
             year %in% MODEL_COST_YEARS) %>%
      spread(Item, cost) %>%
      rename(IrrCost = `Purchased irrigation water`,
             TotCost = `Total operating costs`) %>%
      mutate(CostFrac = IrrCost / TotCost) %>%
      select(-Unit, -IrrCost, -TotCost) %>%
      group_by(Crop) %>%
      mutate(waterCostFrac = mean(CostFrac, na.rm = T)) %>%
      ungroup %>%
      left_join_error_no_match(USDA_crops, by = c("Crop" = "USDA_crop")) ->
      L164.waterCostFrac_Cusda

    # Step 2: Use L100 LDS harvested area data to weight the waterCostFrac for each USDA
    # crop when aggregating to USDA commodities.
    L100.LDS_ag_HA_ha %>%
      filter(GTAP_crop %in% USDA_crops$GTAP_crop,
             iso == "usa") %>%
      group_by(iso, GTAP_crop) %>%
      summarise(weight = sum(value)) %>%
      ungroup %>%
      select(-iso) %>%
      left_join_error_no_match(L164.waterCostFrac_Cusda, ., by = "GTAP_crop") -> A





    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L164.ag_Cost_75USDkg_C") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/USDA_crops",
                     "aglu/USDA_item_cost",
                     "aglu/USDA_cost_data",
                     "L100.LDS_ag_HA_ha",
                     "L133.ag_Cost_75USDkg_C",
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L164.ag_Cost_75USDkg_C

    return_data(L164.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
