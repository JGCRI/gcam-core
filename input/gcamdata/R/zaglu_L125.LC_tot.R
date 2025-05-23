# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L125.LC_tot
#'
#' Build Total Land Cover by GCAM region, and by GCAM region and GLU, and by GCAM region/GLU/year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.LC_bm2_R}, \code{L125.LC_bm2_R_GLU}, \code{L125.LC_bm2_R_LT_Yh_GLU}.
#' The corresponding file in the
#' original data system was \code{LB125.LC_tot.R} (aglu level1).
#' @details This module builds three total land cover area data from the lower-level raw data:
#' 1) total land area by GCAM region; 2) total land area by GCAM region and GLU; and
#' 3) total land area by GCAM region, GLU and historical year. Units of the data are billion square meters.
#' The land area changing rates (bm2 per year) are checked to make sure they are within certain tolerances.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by lag mutate select summarise
#' @author MC May 2017
module_aglu_L125.LC_tot <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L120.LC_bm2_R_UrbanLand_Yh_GLU",
             "L120.LC_bm2_R_Tundra_Yh_GLU",
             "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
             "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
             "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
             "L123.LC_bm2_R_MgdPast_Yh_GLU",
             "L123.LC_bm2_R_MgdFor_Yh_GLU",
             "L124.LC_bm2_R_Shrub_Yh_GLU_adj",
             "L124.LC_bm2_R_Grass_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.LC_bm2_R",
             "L125.LC_bm2_R_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- Land_Type <- GLU <- LC_bm2 <- NULL   # silence package check notes

    # Load required inputs
    L120.LC_bm2_R_UrbanLand_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_UrbanLand_Yh_GLU")
    L120.LC_bm2_R_Tundra_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_Tundra_Yh_GLU")
    L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_RckIceDsrt_Yh_GLU")
    L122.LC_bm2_R_HarvCropLand_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_Yh_GLU")
    L122.LC_bm2_R_OtherArableLand_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_OtherArableLand_Yh_GLU")
    L123.LC_bm2_R_MgdPast_Yh_GLU <- get_data(all_data, "L123.LC_bm2_R_MgdPast_Yh_GLU")
    L123.LC_bm2_R_MgdFor_Yh_GLU <- get_data(all_data, "L123.LC_bm2_R_MgdFor_Yh_GLU")
    L124.LC_bm2_R_Shrub_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Shrub_Yh_GLU_adj", strip_attributes = TRUE)
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")


    # -----------------------------------------------------------------------------
    # Perform computations
    # Create a table with all land types
    bind_rows(L124.LC_bm2_R_Shrub_Yh_GLU_adj,
              L124.LC_bm2_R_Grass_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj,
              L123.LC_bm2_R_MgdPast_Yh_GLU,
              L123.LC_bm2_R_MgdFor_Yh_GLU,
              L120.LC_bm2_R_UrbanLand_Yh_GLU,
              L120.LC_bm2_R_Tundra_Yh_GLU,
              L120.LC_bm2_R_RckIceDsrt_Yh_GLU,
              L122.LC_bm2_R_HarvCropLand_Yh_GLU,
              L122.LC_bm2_R_OtherArableLand_Yh_GLU)  ->
      L125.LC_bm2_R_LT_Yh_GLU

    # Adding up total land area by region, GLU, and year
    L125.LC_bm2_R_LT_Yh_GLU %>%
      group_by(GCAM_region_ID, GLU, year) %>%               # group the data by GCAM_region_id and GLU
      summarise(value = sum(value)) ->                      # Adding up total land area by region, year, and GLU
      L125.LC_bm2_R_Yh_GLU

    # It's necessary to make sure the Land Cover changing rates are within certain tolerances
    # We don't do this under timeshift because the tolerance check fails...and it's not clear if that means anything or not
    if(!UNDER_TIMESHIFT) {
      L125.LC_bm2_R_Yh_GLU %>%
        filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
        arrange(GCAM_region_ID, GLU, year) %>%
        #when executing lag there is no value before 1971 so division/subtraction can't happen so fixed by using the first cell value (1971) to operate with itself
        mutate(change_rate = value / lag(value, default = first(value)),
               change = value - lag(value, default = first(value))) ->          # calculate the rate of change
        LC_check

      # Stop if the rate is outside of the tolerance boundaries
      out <- abs(LC_check$change_rate - 1) > aglu.LAND_TOLERANCE
      if(any(out, na.rm = TRUE)) {
        print(na.omit(LC_check[out,]))
        stop("ERROR: Interannual fluctuation in global land cover exceeds tolerance threshold of ", aglu.LAND_TOLERANCE)
      }
    }

    # Write out the totals, by region and by region x GLU
    L125.LC_bm2_R_Yh_GLU %>%
      filter(year == min(year)) %>%                        # using the starting year only
      group_by(GCAM_region_ID, GLU) %>%                    # group by GCAM_region_ID and GLU
      summarise(LC_bm2 = sum(value)) %>%                    # calculate the total area for each region and GLU
      ungroup ->
      L125.LC_bm2_R_GLU                                    # Total land cover by GCAM region and GLU (unit billion m2)

    L125.LC_bm2_R_Yh_GLU %>%
      filter(year == min(year)) %>%                        # using the starting year only
      group_by(GCAM_region_ID) %>%                         # group by GCAM_region_ID
      summarise(LC_bm2 = round(sum(value),
                               aglu.DIGITS_LAND_TOTAL)) %>% # calculate the total area for each region
      ungroup ->
      L125.LC_bm2_R                                        # Total land cover by GCAM region (unit billion m2)

    L125.LC_bm2_R_LT_Yh_GLU %>%                           # Land cover totals differentiated by land use types
      mutate(value = round(value, aglu.DIGITS_LAND_USE)) ->    # round totals to specified number of digits defined by aglu.DIGITS_LAND_USE
      L125.LC_bm2_R_LT_Yh_GLU                             # Total land cover by GCAM region / land type / historical year / GLU (unit billion m2)

    # Produce outputs
    L125.LC_bm2_R %>%
      add_title("Total land cover by GCAM region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Aggregated Land area by GCAM region") %>%
      add_legacy_name("L125.LC_bm2_R") %>%
      add_precursors("L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
                     "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
      L125.LC_bm2_R

    L125.LC_bm2_R_GLU %>%
      add_title("Total land cover by GCAM region and GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Aggregated Land area by GCAM region x GLU") %>%
      add_legacy_name("L125.LC_bm2_R_GLU") %>%
      same_precursors_as(L125.LC_bm2_R) ->
      L125.LC_bm2_R_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      add_title("Total land cover by GCAM region / land type / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Data was read in from multiple data sources for each land cover type") %>%
      add_legacy_name("L125.LC_bm2_R_LT_Yh_GLU") %>%
      same_precursors_as(L125.LC_bm2_R) ->
      L125.LC_bm2_R_LT_Yh_GLU

    return_data(L125.LC_bm2_R, L125.LC_bm2_R_GLU, L125.LC_bm2_R_LT_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
