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
      # filter out any crops with IrrCost in all years
      group_by(Crop) %>%
      filter(!all(is.na(IrrCost))) %>%
      select(-Unit, -IrrCost, -TotCost) %>%
      # calculate average Cost fraction for each crop, over MODEL_COST_YEARS
      mutate(waterCostFrac = mean(CostFrac, na.rm = T)) %>%
      ungroup %>%
      left_join_error_no_match(USDA_crops, by = c("Crop" = "USDA_crop")) ->
      L164.waterCostFrac_Cusda

    # Step 2: Use L100 LDS harvested area data to weight the waterCostFrac for each USDA
    # crop when aggregating to GCAM commodities that are represented in USDA cost spreadsheets.
    # Use the aggregated weighted waterCostFrac and weights to compute weighted average water
    # cost fraction by GCAM commodities that are represented in USDA cost spreadsheets.
    L100.LDS_ag_HA_ha %>%
      filter(GTAP_crop %in% USDA_crops$GTAP_crop &
             iso == "usa") %>%
      group_by(iso, GTAP_crop) %>%
      summarise(weight = sum(value)) %>%
      ungroup %>%
      select(-iso) %>%
      left_join_error_no_match(L164.waterCostFrac_Cusda, ., by = "GTAP_crop") %>%
      mutate(waterCostFrac_wt = waterCostFrac * weight) %>%
      group_by(GCAM_commodity) %>%
      summarise(waterCostFrac_wt = sum(waterCostFrac_wt),
                weight = sum(weight)) %>%
      ungroup %>%
      # compute weighted average water cost fraction by GCAM commodities that are represented in USDA cost spreadsheets.
      mutate(waterCostFrac = waterCostFrac_wt / weight) ->
      L164.waterCostFrac_C


    # Step 3: Compute water cost fractions for crops not in the USDA cost spreadsheets.
    # For the crops with missing values, make a simple linear regression to predict water cost fraction as a function of
    # irrigation fraction
    L161.ag_irrHA_frac_R_C_GLU %>%
      filter(GCAM_region_ID == iso_GCAM_regID[["GCAM_region_ID"]][iso_GCAM_regID$iso == "usa"]) %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(irrHA = sum(irrHA),
                rfdHA = sum(rfdHA)) %>%
      ungroup %>%
      mutate(irrHA_frac = irrHA / (irrHA + rfdHA)) %>%
      # join in water costs for the commodities that ARE covered by USDA spreadsheets, L164.waterCostFrac_C
      # preserve NA's to process
      left_join(select(L164.waterCostFrac_C, GCAM_commodity, waterCostFrac),
                               by = "GCAM_commodity") ->
      L164.ag_irrHA_frac_USA_C

    # Use L164.ag_irrHA_frac_USA_C to form the linear model waterCostFrac = f(irrHA_frac)
    # and predict the missing water cost fractions in L164.ag_irrHA_frac_USA_C.
    L164.ag_irrHA_frac_USA_C %>%
      glm(waterCostFrac ~ irrHA_frac, data =.) %>%
      predict(type = "response",
              newdata = select(filter(L164.ag_irrHA_frac_USA_C,is.na(waterCostFrac)),
                               irrHA_frac, waterCostFrac)) %>%
      tibble::tibble(missingWaterCost = .) %>%
      bind_cols(filter(L164.ag_irrHA_frac_USA_C,is.na(waterCostFrac))) %>%
      mutate(waterCostFrac = missingWaterCost) %>%
      select(-missingWaterCost) ->
      L164.ag_irrHA_frac_USA_missingC

    # Finally, bind the table of missing cost commodities, L164.ag_irrHA_frac_USA_missingC,
    # to the table of commodities covered by USDA spreadsheets
    L164.ag_irrHA_frac_USA_C %>%
      filter(!is.na(waterCostFrac)) %>%
      bind_rows(L164.ag_irrHA_frac_USA_missingC) ->
      L164.ag_irrHA_frac_USA_C


    # Step 4: Calculate the revised variable cost as the prior total minus the water cost fraction
    # For any crops not grown in the US, assume no water cost deduction (not used w present dataset
    # and mappings)
    L133.ag_Cost_75USDkg_C %>%
      left_join_error_no_match(select(L164.ag_irrHA_frac_USA_C, GCAM_commodity, waterCostFrac),
                               by = "GCAM_commodity") %>%
      mutate(Cost_75USDkg_new = Cost_75USDkg * (1 - waterCostFrac),
             Cost_75USDkg_new = replace(Cost_75USDkg_new,
                                        is.na(Cost_75USDkg_new),
                                        Cost_75USDkg)) %>%
      select(-Cost_75USDkg, -waterCostFrac) %>%
      rename(Cost_75USDkg = Cost_75USDkg_new) ->
      L164.ag_Cost_75USDkg_C


    # Produce outputs
    L164.ag_Cost_75USDkg_C %>%
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
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") ->
      L164.ag_Cost_75USDkg_C

    return_data(L164.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
