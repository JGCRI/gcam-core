#' module_aglu_LB164.ag_Costs_USA_C_2005_irr
#'
#' This module calculates production costs of GCAM commodities not including purchased irrigation water.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L164.ag_Cost_75USDkg_C}. The corresponding file in the
#' original data system was \code{LB164.ag_Costs_USA_C_2005_irr.R} (aglu level1).
#' @details USDA cost data is used to calculate total production costs - purchased irrigation water costs for
#' GCAM commodities covered in USDA spreadsheets. This produces a commodity level water cost fraction for some
#' but not all GCAM commodities. L161.ag_irr_HA_frac_R_C_GLU irrigated vs rainfed harvested area data for the
#' covered GCAM commodities is used to form a linear model describing water cost fraction as a function of the
#' irrigated fraction of harvested area. This linear model then predicts the water cost fraction based on
#' irrigated fraction of harvested area for the missing commodities. Finally, cost data from LB133.ag_Cost_75USDkg_C
#' is adjusted as LB133_cost * (1 - water cost fraction) = production cost - purchased irrigation water for each
#' commodity.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom stats glm predict
#' @author ACS June 2017
module_aglu_LB164.ag_Costs_USA_C_2005_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/USDA_crops",
             FILE = "aglu/USDA_item_cost",
             FILE = "aglu/USDA_cost_data",
             "L100.LDS_ag_HA_ha",
             "L133.ag_Cost_75USDkg_C",
             "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L164.ag_Cost_75USDkg_C"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    CostFrac <- . <- Cost_75USDkg <- Cost_75USDkg_new <- Crop <- GCAM_commodity <-
      GCAM_region_ID <- GTAP_crop <- IrrCost <- Item <- LEVEL2_DATA_NAMES <-
      Purchased <- irrigation <- water <- TotCost <- Total <- operating <- costs <-
      Unit <- cost <- curr_table <- glm <- irrHA <- irrHA_frac <- iso <- missingWaterCost <-
      predict <- rfdHA <- value <- waterCostFrac <- weight <- year <-
      `Purchased irrigation water` <- `Total operating costs` <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    USDA_crops <- get_data(all_data, "aglu/USDA_crops")
    USDA_item_cost <- get_data(all_data, "aglu/USDA_item_cost")
    USDA_cost_data <- get_data(all_data, "aglu/USDA_cost_data")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L133.ag_Cost_75USDkg_C <- get_data(all_data, "L133.ag_Cost_75USDkg_C")
    L161.ag_irrHA_frac_R_C_GLU <- get_data(all_data, "L161.ag_irrHA_frac_R_C_GLU")


    # Perform computations
    # The method here is to start from the cost file that has already been processed in LB133, and
    # to deduct costs of purchased irrigation water from each cost estimate.

    # Step 1: Compute the share of total variable costs from purchased irrigation water.
    # CostFrac = IrrCost / TotCost
    USDA_cost_data %>%
      gather_years(value_col = "cost") %>%
      filter(Item == "Purchased irrigation water" | Item == "Total operating costs",
             year %in% aglu.MODEL_COST_YEARS ) %>%
      spread(Item, cost) %>%
      rename(IrrCost = `Purchased irrigation water`,
             TotCost = `Total operating costs`) %>%
      mutate(CostFrac = IrrCost / TotCost) %>%
      # keep only crops with IrrCost for at least one MODEL_COST_YEAR. Trivially, these are the only
      # crops whose production costs will be impacted by purchased irrigation water.
      group_by(Crop) %>%
      filter(!all(is.na(IrrCost))) %>%
      select(-Unit, -IrrCost, -TotCost) %>%
      # calculate average Cost fraction for each crop, over aglu.MODEL_COST_YEARS
      mutate(waterCostFrac = mean(CostFrac, na.rm = TRUE)) %>%
      ungroup %>%
      left_join_error_no_match(USDA_crops, by = c("Crop" = "USDA_crop")) ->
      L164.waterCostFrac_Cusda

    # Step 2: Use L100 LDS harvested area data to weight the waterCostFrac for each USDA
    # crop when aggregating to GCAM commodities that are represented in USDA cost spreadsheets.
    L100.LDS_ag_HA_ha %>%
      # get the weights for a weighted average
      filter(GTAP_crop %in% USDA_crops$GTAP_crop &
               iso == "usa") %>%
      group_by(iso, GTAP_crop) %>%
      summarise(weight = sum(value)) %>%
      ungroup %>%
      select(-iso) %>%
      # join the quantities to be weighted and averaged
      left_join_error_no_match(L164.waterCostFrac_Cusda, ., by = "GTAP_crop") %>%
      # compute the weighted average
      group_by(GCAM_commodity) %>%
      summarise(waterCostFrac = weighted.mean(waterCostFrac, weight)) %>%
      ungroup ->
      L164.waterCostFrac_C


    # Step 3: Compute water cost fractions for crops not in the USDA cost spreadsheets.
    # For the crops with missing values, make a simple linear regression to predict water cost fraction as a function of
    # irrigation fraction
    L161.ag_irrHA_frac_R_C_GLU %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
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
      glm(waterCostFrac ~ irrHA_frac, data = .) %>%
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
      add_title("Production costs of GCAM commodities not including purchased irrigation water") %>%
      add_units("1975USD/kg") %>%
      add_comments("USDA cost data is used to calculate total production costs - purchased irrigation water costs for") %>%
      add_comments("GCAM commodities covered in USDA spreadsheets. This produces a commodity level water cost fraction for some") %>%
      add_comments("but not all GCAM commodities. L161.ag_irr_HA_frac_R_C_GLU irrigated vs rainfed harvested area data for the") %>%
      add_comments("covered GCAM commodities is used to form a linear model describing water cost fraction as a function of the") %>%
      add_comments("irrigated fraction of harvested area. This linear model then predicts the water cost fraction based on") %>%
      add_comments("irrigated fraction of harvested area for the missing commodities. Finally, cost data from LB133.ag_Cost_75USDkg_C") %>%
      add_comments("is adjusted as LB133_cost * (1 - water cost fraction) = production cost - purchased irrigation water for each commodity.") %>%
      add_legacy_name("L164.ag_Cost_75USDkg_C") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/USDA_crops",
                     "aglu/USDA_item_cost",
                     "aglu/USDA_cost_data",
                     "L100.LDS_ag_HA_ha",
                     "L133.ag_Cost_75USDkg_C",
                     "L161.ag_irrHA_frac_R_C_GLU") ->
      L164.ag_Cost_75USDkg_C

    return_data(L164.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
