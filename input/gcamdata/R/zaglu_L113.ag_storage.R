# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L113_ag_storage
#'
#' Preparing and processing agricultural storage data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.ag_Storage_Mt_R_C_Y_adj}.
#' @details This chunk calculates ag storage values, losses, and costs by GCAM region and commodity.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select transmute
#' @importFrom tidyr gather spread replace_na
#' @author XZ 2023
module_aglu_L113_ag_storage <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_agStorageSector",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      "L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg")

  MODULE_OUTPUTS <-
    c("L113.StorageTechAndPassThrough")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # [ToDo: adding forest (L110) to here later when seeing fit]
    # so regional forest and total forest would be separated


    # 1. Prepare storage cost shares ----

    # Storage cost shares summary ----
    # There is no available data of storage cost worldwide
    # Based on the data points shown above (grains), the cost shares (marginal; second-half of the year)
    # are usually in 2 - 7%
    # However, the difference between the cost for interannual storage vs average storage is not clear.
    # Storage cost vs. market price of commercial storage service could be another source of uncertainty.
    # Here we will assume 3% of the price is the storage cost everywhere in all sectors.


    ## Storage cost share in producer prices ----
    InterAnnualStorageCostShare <- 0.03

    L1321.ag_prP_R_C_75USDkg %>%
      bind_rows(L1321.an_prP_R_C_75USDkg) %>%
      filter(GCAM_commodity %in% (A_agStorageSector %>%
                                    filter(storage_model == T) %>% pull(GCAM_commodity))) %>%
      mutate(value = value * InterAnnualStorageCostShare) ->
      L113.ClosingStockCost_R_C

    # 2. Get storage data from the adjusted SUA balances ----
    # And get parameters ready
    L109.ag_ALL_Mt_R_C_Y %>%
      gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y %>%
          gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID)
      ) %>%
      # Keep relevant elements, storage comm., and base years only
      filter(element %in% c("Opening stocks", "Closing stocks", "InterAnnualStockLoss"),
             year %in% MODEL_BASE_YEARS) %>%
      # Keep sectors in A_agStorageSector which are the sectors with regional markets
      # And join sector mappings and parameters
      # all storage commodities are in L109 SUA data; this was asserted in the earlier stage
      inner_join(A_agStorageSector, by = "GCAM_commodity") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) ->
      L113.ag_Storage_Mt_R_C_Y_adj1


    # 3. Compute loss-coefficients and arrange data to get table needed ----
    L113.ag_Storage_Mt_R_C_Y_adj1 %>%
      spread(element, value) %>%
      mutate(LossCoef = 1 - InterAnnualStockLoss / `Closing stocks`) %>%
      replace_na(list(LossCoef = 0)) %>%
      group_by(GCAM_commodity, region) %>%
      # compute Carry-forward here
      mutate(Carryforward = lead(`Opening stocks`),
             Carryforward = if_else(is.na(Carryforward),
                                    `Closing stocks` * LossCoef, Carryforward)) %>% ungroup %>%
      select(supplysector, region, year, LossCoef, `Closing stocks`, `Opening stocks`,
             Carryforward, logit.exponent, technology, minicam_energy_input, GCAM_commodity, storage_model) ->
      L113.ag_Storage_Mt_R_C_Y_adj2


    ## Join storage cost ----
    L113.ag_Storage_Mt_R_C_Y_adj2 %>%
      # NA is possible so replace_na later
      left_join(L113.ClosingStockCost_R_C %>%
                  select(region, GCAM_commodity, ClosingStockCost = value),
                by = c("region", "GCAM_commodity")) %>%
      replace_na(list(ClosingStockCost = 0))->
      L113.ag_Storage_Mt_R_C_Y_adj



    # 3. Prepare a storage tech data table for generating XMLs. ----

    # Calculate lifetime in the carry-over structure to be exactly equal
    # to two model periods
    L113.StorageLifeTime <-
      tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(timestep = replace_na(year - lag(year), modeltime.PERIOD0_TIMESTEP),
             # the lifetime needs to last the current timestep plus the next
             lifetime = timestep + lead(timestep)) %>%
      tidyr::fill(lifetime, .direction="down")

    # Construct the key table for storage tech.
    # ToDo: storage.cost
    L113.StorageTechAndPassThrough <-
      L113.ag_Storage_Mt_R_C_Y_adj %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology = technology,
                       year,
                       share.weight = 1,
                       logit.exponent,
                       logit.type = NA, # Note that this is not used but added to avoid warnings
                       storage.cost = ClosingStockCost,
                       closing.stock = `Closing stocks`,
                       loss.coefficient = LossCoef,
                       opening.stock = Carryforward,
                       minicam.energy.input = minicam_energy_input,
                       GCAM_commodity, storage_model) %>%
      left_join_error_no_match(L113.StorageLifeTime, by = "year")

    # add a future year here for storage commodities (storage_model == TRUE)
    L113.StorageTechAndPassThrough <-
      L113.StorageTechAndPassThrough %>%
      bind_rows(
        L113.StorageTechAndPassThrough %>% filter(storage_model == TRUE) %>%
          filter(year == max(MODEL_BASE_YEARS)) %>%
          mutate(year = min(MODEL_FUTURE_YEARS),
                 # setting carried.forward and closing stock to zero
                 # The two are not important because the values are not used
                 opening.stock = closing.stock * loss.coefficient,
                 closing.stock = opening.stock,
                 lifetime = sort(MODEL_FUTURE_YEARS)[2] - max(MODEL_BASE_YEARS))
      )

    # Produce outputs ----
    L113.StorageTechAndPassThrough %>%
      add_title("Ag storage data and parameter assumptions") %>%
      add_units("various") %>%
      add_comments("Table inlcudes the data and parameter of the storage tech or the pass-through sector of generating, e.g., regional corn from total corn") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agStorageSector",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L1321.ag_prP_R_C_75USDkg",
                     "L1321.an_prP_R_C_75USDkg") ->
      L113.StorageTechAndPassThrough


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
