#' module_aglu_LA108.ag_Feed_R_C_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L108.ag_Feed_Mt_R_C_Y}, \code{L108.ag_NetExp_Mt_R_FodderHerb_Y}. The corresponding file in the
#' original data system was \code{LA108.ag_Feed_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA108.ag_Feed_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_cal_SUA",
             "L100.FAO_ag_Feed_t",
             "L103.ag_Prod_Mt_R_C_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L108.ag_Feed_Mt_R_C_Y",
             "L108.ag_NetExp_Mt_R_FodderHerb_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO_ag_items_cal_SUA")
    L100.FAO_ag_Feed_t <- get_data(all_data, "L100.FAO_ag_Feed_t")
    L103.ag_Prod_Mt_R_C_Y <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y")
    L107.an_Feed_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Feed_Mt_R_C_Sys_Fd_Y")

    # Part 1: FEEDCROPS
    # Compute regional feedcrop demands by GCAM region, commodity, and year in Mt/yr.
    # Use crop-specific information from FAO, in combination with feed totals from IMAGE,
    # to calculate region/crop specific information. This ensures totals match IMAGE.
    # First, calculate FAO crop-region totals
    L100.FAO_ag_Feed_t %>%
      select(iso, item, year, value) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                     # Map in GCAM region ID
      left_join(select(FAO_ag_items_cal_SUA, item, GCAM_commodity), by = "item") %>%               # Map in GCAM commodity
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarize(value = sum(value)) %>%                                                            # Aggregate by crop, region, year
      mutate(value = value * CONV_TON_MEGATON) %>%                                                 # Convert from tons to Mt
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                    # Fill in missing region/commodity combinations with 0
      ag_Feed_Mt_R_Cnf_Y

    # Then, calculate feedcrop shares by crop within each region
    ag_Feed_Mt_R_Cnf_Y %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(total = sum(value)) %>%                                                            # Aggregate to compute regional totals
      right_join(ag_Feed_Mt_R_Cnf_Y, by = c("GCAM_region_ID", "year")) %>%                         # Map back in the crop/region specific FAO data
      mutate(Feedfrac = value / total) %>%                                                         # Calculate each crop's share of total feed in a region
      mutate(Feedfrac = if_else(is.na(Feedfrac), 0, Feedfrac)) ->                                  # Replace missing data with 0 (assumes no share for those crops)
      ag_Feedfrac_R_Cnf_Y

    # Now, compute feedcrop demand by region, crop, and year using IMAGE totals and feed fractions computed above
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      group_by(GCAM_region_ID, feed, year) %>%
      summarize(value = sum(value)) %>%                                                            # Compute total feed by IMAGE feed system, region, year
      filter(feed == "FeedCrops") %>%                                                              # Filter to only include "FeedCrops"
      right_join(select(ag_Feedfrac_R_Cnf_Y, GCAM_region_ID, GCAM_commodity, year, Feedfrac),
                 by = c("GCAM_region_ID", "year" ) ) %>%                                           # Map in feed fractions computed from FAO data
      mutate(value = value * Feedfrac) ->                                                          # Compute FAO-IMAGE adjusted feed crop demand
      ag_Feed_Mt_R_Cnf_Y_adj


    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L108.ag_Feed_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO_ag_items_cal_SUA", "L100.FAO_ag_Feed_t",
                     "L103.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_Feed_Mt_R_C_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L108.ag_NetExp_Mt_R_FodderHerb_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO_ag_items_cal_SUA", "L100.FAO_ag_Feed_t",
                     "L103.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_NetExp_Mt_R_FodderHerb_Y

    return_data(L108.ag_Feed_Mt_R_C_Y, L108.ag_NetExp_Mt_R_FodderHerb_Y)
  } else {
    stop("Unknown command")
  }
}
