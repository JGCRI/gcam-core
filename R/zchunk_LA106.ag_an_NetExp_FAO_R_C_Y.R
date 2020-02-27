# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA106.ag_an_NetExp_FAO_R_C_Y
#'
#' Calculate the net exports of primary agricultural goods and animal products by GCAM region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L106.ag_NetExp_Mt_R_C_Y}, \code{L106.an_NetExp_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LA106.ag_an_NetExp_FAO_R_C_Y.R} (aglu level1).
#' @details This chunk calculate the net exports of primary agricultural goods and animal products by GCAM region / commodity / year.
#' Regional gross exports are adjusted so that global net exports are zero of each commodity / year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete spread
#' @author RC June 2017
module_aglu_LA106.ag_an_NetExp_FAO_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_cal_SUA",
             FILE = "aglu/FAO/FAO_an_items_cal_SUA",
             "L100.FAO_ag_Exp_t",
             "L100.FAO_ag_Imp_t",
             "L100.FAO_an_Exp_t",
             "L100.FAO_an_Imp_t"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L106.ag_NetExp_Mt_R_C_Y",
             "L106.an_NetExp_Mt_R_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    Exp_t <- GCAM_commodity <- GCAM_region_ID <- Imp_t <- curr_table <- element <- item <-
      netExp <- scaler <- value <- year <- NULL   # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_items_cal_SUA")
    FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_an_items_cal_SUA")
    L100.FAO_ag_Exp_t <- get_data(all_data, "L100.FAO_ag_Exp_t")
    L100.FAO_ag_Imp_t <- get_data(all_data, "L100.FAO_ag_Imp_t")
    L100.FAO_an_Exp_t <- get_data(all_data, "L100.FAO_an_Exp_t")
    L100.FAO_an_Imp_t <- get_data(all_data, "L100.FAO_an_Imp_t")

    # Combine FAO-GCAM mapping files of primary agriculture goods and animal products
    FAO_ag_items_cal_SUA %>%
      select(item, GCAM_commodity) %>%
      bind_rows(select(FAO_an_items_cal_SUA, item, GCAM_commodity)) ->
      FAO_items_map

    # Calculate FAO primary agricultural goods and animal products net exports by GCAM region and commodity as exports minus imports.
    L100.FAO_ag_Exp_t %>%
      # Combine all FAO primary agricultural goods and animal products exports and imports data
      bind_rows(L100.FAO_ag_Imp_t, L100.FAO_an_Exp_t, L100.FAO_an_Imp_t) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                     # Map in GCAM regions
      left_join(FAO_items_map, by = "item") %>%                                    # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                           # Remove commodities not included in GCAM
      group_by(GCAM_region_ID, GCAM_commodity, element, year) %>%                  # Group by region, commodity, year
      summarise(value = sum(value)) %>%                                            # Aggregate exports and imports
      ungroup() %>%
      mutate(element = sub("ag_", "", element),                                    # Change the element (export and import) name for wide format
             element = sub("an_", "", element)) %>%
      spread(element, value) %>%                                                   # Wide format for net export calculation
      na.omit() %>%                                                                # Drop observations with missing export or import, fill in later
      mutate(netExp = Exp_t - Imp_t) ->                                            # Calculate net exports as exports minus imports
      L106.NetExp_t_R_C_Y

    # Net exports must add to zero globally, so adjust gross exports in all regions so that global net exports add to zero.
    # NOTE: give precedence to imports (rather than exports) of each commodity. This is arbitrary but of little consequence, and generally reduces amount of trade.
    L106.NetExp_t_R_C_Y %>%
      group_by(GCAM_commodity, year) %>%                                           # Group by commodity and year
      summarise(netExp = sum(netExp), Exp_t = sum(Exp_t)) %>%                      # Sum global total net exports and gross exports
      ungroup() %>%
      # Calculate the export scaler for each commodity/year - the ratio of adjusted gross exports relative to original gross exports
      mutate(scaler = (Exp_t - netExp) / Exp_t) %>%
      select(-Exp_t, -netExp) ->
      L106.ExpScaler_C_Y

    # Adjust gross exports and recompile net exports table, convert unit, and fill in missing values
    L106.NetExp_t_R_C_Y %>%
      left_join_error_no_match(L106.ExpScaler_C_Y, by = c("GCAM_commodity", "year")) %>%     # Match in export scaler
      mutate(value = Exp_t * scaler - Imp_t,                                                 # Calulate regional net exports as adjusted regional gross exports minus imports
             value = value * CONV_T_MT) %>%                                                  # Convert unit from ton to megaton
      select(GCAM_region_ID, GCAM_commodity, year, value) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                       # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L106.NetExp_Mt_R_C_Y

    # Produce outputs
    L106.NetExp_Mt_R_C_Y %>%
      semi_join(select(FAO_ag_items_cal_SUA, GCAM_commodity), by = "GCAM_commodity") %>%
      add_title("Net exports of primary agricultural goods by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO primary agricultural goods gross exports and imports and calculate net exports by GCAM region, commodity and year") %>%
      add_comments("Gross exports are adjusted so that global net exports add to zero") %>%
      add_comments("Re-calculate regional net exports using adjusted gross exports minus gross imports") %>%
      add_legacy_name("L106.ag_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_cal_SUA",
                     "L100.FAO_ag_Exp_t",
                     "L100.FAO_ag_Imp_t") ->
      L106.ag_NetExp_Mt_R_C_Y

    L106.NetExp_Mt_R_C_Y %>%
      semi_join(select(FAO_an_items_cal_SUA, GCAM_commodity), by = "GCAM_commodity") %>%
      add_title("Net exports of animal products by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO animal products gross exports and imports and calculate net exports by GCAM region, commodity and year") %>%
      add_comments("Gross exports are adjusted so that global net exports add to zero") %>%
      add_comments("Re-calculate regional net exports using adjusted gross exports minus gross imports") %>%
      add_legacy_name("L106.an_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Exp_t",
                     "L100.FAO_an_Imp_t") ->
      L106.an_NetExp_Mt_R_C_Y

    return_data(L106.ag_NetExp_Mt_R_C_Y, L106.an_NetExp_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
