# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_land_input_1_xml
#'
#' Construct XML data structure for \code{land_input_1.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_1.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_1.xml.R} (aglu XML).
module_aglu_land_input_1_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L221.LN0_Logit",
      "L221.LN0_Land",
      "L221.LN0_SoilTimeScale",
      "L221.LN1_ValueLogit",
      "L221.LN1_HistUnmgdAllocation",
      "L221.LN1_UnmgdAllocation",
      "L221.LN1_UnmgdCarbon",
      # Inputs for calculating cropland rental profits
      "L2252.LN5_MgdAllocation_crop",
      "L2012.AgProduction_ag_irr_mgmt",
      "L2012.AgSupplySector",
      "L2052.AgCost_ag_irr_mgmt")

  MODULE_OUTPUTS <-
    c(XML = "land_input_1.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================


    # Update shadow price of cropland for unmanaged land rental price ----
    UsingGCAMCroplandRentalProfit = TRUE

    if (UsingGCAMCroplandRentalProfit == TRUE) {

    # Formula:
    # Rental profit (1975$ per thousand km2)
    # Rental profit = (P - NLC) * yield
    # (1975$/kg - 1975$/kg) * kg/thousand km2
    # (1975$/kg - 1975$/kg) * Mt * 10^9/bm2


    # Calculate P - NLC
    # Note that water cost is not included in NLC; it is ignored here
    L2052.AgCost_ag_irr_mgmt %>%
      left_join_error_no_match(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                               by = c("region", "AgSupplySector")) %>%
      mutate(Profit = calPrice - nonLandVariableCost) ->
      L2052.UnAdjProfits

    # Calculate yield can rental profit
    L2252.LN5_MgdAllocation_crop %>%
      rename(bm2 = allocation) %>%
      left_join(
        L2012.AgProduction_ag_irr_mgmt %>%
          select(region, year, LandLeaf = AgProductionTechnology, Mt = calOutputValue),
        by = c("region", "LandLeaf", "year")
      ) %>%
      left_join(
        L2052.UnAdjProfits %>%
          select(region, year, LandLeaf = AgProductionTechnology, USDPerKg = Profit),
        by = c("region", "LandLeaf", "year")) %>%
      mutate(RentalProfit = USDPerKg * Mt * 10^9 / bm2) ->
      Cropland_RentalProfit_GLU_C_Y_IRR_MGMT

    Cropland_RentalProfit_GLU_C_Y_IRR_MGMT %>%
      group_by(region, year) %>%
      summarize(unManagedLandValue = weighted.mean(RentalProfit, w = bm2)) %>%
      ungroup() %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year) -> Cropland_RentalProfit_R

    Cropland_RentalProfit_GLU_C_Y_IRR_MGMT %>%
      group_by(region, LandAllocatorRoot, LandNode1, year) %>%
      summarize(unManagedLandValue = weighted.mean(RentalProfit, w = bm2)) %>%
      ungroup() %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year) -> Cropland_RentalProfit_GLU


    # Update the value ----
    L221.LN1_ValueLogit %>%
      rename(unManagedLandValue_GTAP = unManagedLandValue) %>%
      left_join(Cropland_RentalProfit_GLU, by = c("region", "LandAllocatorRoot", "LandNode1")) %>%
      left_join_error_no_match(Cropland_RentalProfit_R %>% rename(unManagedLandValue_R = unManagedLandValue),
                               by = c("region")) %>%
      mutate(unManagedLandValue = if_else(is.na(unManagedLandValue), unManagedLandValue_R, unManagedLandValue)) %>%
      # The difference can be analysed here
      # In short, much smaller variation with the new data
      select(names(L221.LN1_ValueLogit)) ->
      L221.LN1_ValueLogit
    }

    # Produce outputs ----
    create_xml("land_input_1.xml") %>%
      add_logit_tables_xml(L221.LN0_Logit, "LN0_Logit") %>%
      add_xml_data(L221.LN0_Land, "LN0_Land") %>%
      add_xml_data(L221.LN0_SoilTimeScale, "LN0_SoilTimeScale") %>%
      add_logit_tables_xml(L221.LN1_ValueLogit, "LN1_ValueLogit", "LN1_Logit") %>%
      add_xml_data(L221.LN1_HistUnmgdAllocation, "LN1_HistUnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdAllocation, "LN1_UnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdCarbon, "LN1_UnmgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L221.LN0_Logit", "L221.LN0_Land", "L221.LN0_SoilTimeScale", "L221.LN1_ValueLogit", "L221.LN1_HistUnmgdAllocation", "L221.LN1_UnmgdAllocation", "L221.LN1_UnmgdCarbon") ->
      land_input_1.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
