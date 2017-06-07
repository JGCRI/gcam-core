#' module_aglu_LA106.ag_an_NetExp_FAO_R_C_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L106.ag_NetExp_Mt_R_C_Y}, \code{L106.an_NetExp_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LA106.ag_an_NetExp_FAO_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
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

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_items_cal_SUA")
    FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_an_items_cal_SUA")
    L100.FAO_ag_Exp_t <- get_data(all_data, "L100.FAO_ag_Exp_t")
    L100.FAO_ag_Imp_t <- get_data(all_data, "L100.FAO_ag_Imp_t")
    L100.FAO_an_Exp_t <- get_data(all_data, "L100.FAO_an_Exp_t")
    L100.FAO_an_Imp_t <- get_data(all_data, "L100.FAO_an_Imp_t")

    # Combine FAO-GCAM mapping files
    FAO_ag_items_cal_SUA %>%
      select(item, GCAM_commodity) %>%
      bind_rows(select(FAO_an_items_cal_SUA, item, GCAM_commodity)) ->
      FAO_items

    # Process FAO animal commodity export and import data: map in GCAM region and commodities, convert units, aggregate to region and commodity
    L100.FAO_ag_Exp_t %>%
      bind_rows(L100.FAO_ag_Imp_t, L100.FAO_an_Exp_t, L100.FAO_an_Imp_t) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%           # Map in GCAM regions
      left_join(FAO_items, by = "item")  %>%                   # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                  # Remove commodities not included in GCAM
      group_by(GCAM_region_ID, GCAM_commodity, element, year) %>%         # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                   # Aggregate export
      ungroup() %>%                                                       # Ungroup
      mutate(element = sub("ag_", "", element),
             element = sub("an_", "", element)) %>%
      spread(element, value) %>%
      na.omit() %>%
      mutate(netExp = Exp_t - Imp_t) ->                             # Calculate net exports as exports minus imports
      L106.NetExp_t_R_C_Y

    # Net exports must add to zero globally. Adjust exports in all regions so that global net exports add to 0
    # NOTE: giving precedence to imports ( rather than exports ) of each crop. This is arbitrary but of little consequence, and generally reduces amount of trade.
    L106.NetExp_t_R_C_Y %>%
      group_by(GCAM_commodity, year) %>%                                   # Sum by year and crop type
      summarise(netExp = sum(netExp), Exp_t = sum(Exp_t)) %>%
      ungroup() %>%
      mutate(scaler = (Exp_t - netExp) / Exp_t) %>%               # calculate global scalers for exports of each crop
      select(-Exp_t, -netExp) ->
      L106.ExpScaler_C_Y

    # Adjust exports and recompile table, removing gross exports and imports
    L106.NetExp_t_R_C_Y %>%
      left_join_error_no_match(L106.ExpScaler_C_Y, by = c("GCAM_commodity", "year")) %>%
      mutate(value = Exp_t * scaler - Imp_t,
             value = value * CONV_T_MT) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L106.NetExp_Mt_R_C_Y

    # Produce outputs
    L106.NetExp_Mt_R_C_Y %>%
      semi_join(select(FAO_ag_items_cal_SUA, GCAM_commodity)) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L106.ag_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_cal_SUA",
                     "L100.FAO_ag_Exp_t",
                     "L100.FAO_ag_Imp_t") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L106.ag_NetExp_Mt_R_C_Y

    L106.NetExp_Mt_R_C_Y %>%
      semi_join(select(FAO_an_items_cal_SUA, GCAM_commodity)) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L106.an_NetExp_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Exp_t",
                     "L100.FAO_an_Imp_t") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L106.an_NetExp_Mt_R_C_Y

    return_data(L106.ag_NetExp_Mt_R_C_Y, L106.an_NetExp_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
