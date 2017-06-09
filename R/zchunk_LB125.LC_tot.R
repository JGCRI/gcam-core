#' module_aglu_LB125.LC_tot
#'
#' Build Total Land Cover by GCAM region, or by GCAM region and GLU, or by GCAM region/GLU/year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.LC_bm2_R}, \code{L125.LC_bm2_R_GLU}, \code{L125.LC_bm2_R_LT_Yh_GLU}.
#' The corresponding file in the
#' original data system was \code{LB125.LC_tot.R} (aglu level1).
#' @details This module builds three total land cover area data from the lower-level raw data.
#' The three types of data include: 1) total land area by GCAM region; 2) total land area by GCAM region and GLU;
#' 3) total land area by GCAM region, GLU and historical year. Unit of the data is billion square meters.
#' The land area changing rates (bm2 per year) were checked to make sure they are in certain tolerances.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MC May 2017
module_aglu_LB125.LC_tot <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L120.LC_bm2_R_UrbanLand_Yh_GLU",
              "L120.LC_bm2_R_Tundra_Yh_GLU",
              "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
              "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
              "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
              FILE = "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
              FILE = "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
              FILE = "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.LC_bm2_R",
             "L125.LC_bm2_R_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L120.LC_bm2_R_UrbanLand_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_UrbanLand_Yh_GLU")
    L120.LC_bm2_R_Tundra_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_Tundra_Yh_GLU")
    L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_RckIceDsrt_Yh_GLU")
    L122.LC_bm2_R_HarvCropLand_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_Yh_GLU")
    L122.LC_bm2_R_OtherArableLand_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_OtherArableLand_Yh_GLU")
    L123.LC_bm2_R_MgdPast_Yh_GLU <- get_data(all_data, "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU")
    L123.LC_bm2_R_MgdFor_Yh_GLU <- get_data(all_data, "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU")
    L124.LC_bm2_R_Shrub_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")


    # -----------------------------------------------------------------------------
    # Perform computations
    # Create a table with all land types
    bind_rows(L123.LC_bm2_R_MgdPast_Yh_GLU,
              L123.LC_bm2_R_MgdFor_Yh_GLU,
              L124.LC_bm2_R_Shrub_Yh_GLU_adj,
              L124.LC_bm2_R_Grass_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj) %>%
      # The next two lines are TEMPORARY - because currently we're using temp-data-inject data
      # When using final data, don't need to reshape them
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      bind_rows(L120.LC_bm2_R_UrbanLand_Yh_GLU,
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

    # It's necessary to make sure the Land Cover changing rates are in certain tolerances
    L125.LC_bm2_R_Yh_GLU %>%
      arrange(GCAM_region_ID, GLU, year) %>%
      mutate(changing_rate = value / lag(value)) %>%          # calculate the changing rate
      replace_na(list(changing_rate = 1)) %>%                 # assign na to 1 (no change)
      ungroup() %>%
      select(changing_rate) ->
      LC_check

    # Stop if the changing rate out of the tolerance boundaries
    if (max(abs(LC_check - 1)) > LAND_TOLERANCE)
    {
      print(max(abs(LC_check - 1)))
      stop("ERROR: Interannual fluctuation in global land cover exceeds tolerance threshold")
      }

    #Write out the totals, by region and by region x GLU
    L125.LC_bm2_R_Yh_GLU %>%
      filter(year == min(year)) %>%                        # using the starting year only
      group_by(GCAM_region_ID, GLU) %>%                    # group by GCAM_region_ID and GLU
      summarise(value = sum(value)) %>%                    # summarize area by region and GLU
      rename(LC_bm2 = value) ->                            # rename the column
      L125.LC_bm2_R_GLU

    L125.LC_bm2_R_Yh_GLU %>%
      filter(year == min(year)) %>%                        # using the starting year only
      group_by(GCAM_region_ID) %>%                         # group by GCAM_region_ID
      summarise(value = sum(value)) %>%                    # summarize area by region
      rename(LC_bm2 = value) %>%                           # rename the column
      mutate(LC_bm2 = round(LC_bm2, DIGITS_LAND_TOTAL)) -> # keep the data with digit precision defined by DIGITS_LAND_TOTAL
      L125.LC_bm2_R

    L125.LC_bm2_R_LT_Yh_GLU %>%                           # Land cover totals differentiated by land use types
      mutate(value = round(value, DIGITS_LAND_USE)) ->    # round totals to specified number of digits defined by DIGITS_LAND_USE
      L125.LC_bm2_R_LT_Yh_GLU

    # Produce outputs
    L125.LC_bm2_R %>%
      add_title("Total land cover by GCAM region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Aggregated Land area by GCAM region") %>%
      add_comments("Rounded to 2 digit") %>%
      add_legacy_name("L125.LC_bm2_R") %>%
      add_precursors("L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
                     "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
      L125.LC_bm2_R
    L125.LC_bm2_R_GLU %>%
      add_title("Total land cover by GCAM region and GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Aggregated Land area by GCAM region x GLU") %>%
      add_legacy_name("L125.LC_bm2_R_GLU") %>%
      add_precursors("L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
                     "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
      L125.LC_bm2_R_GLU
    L125.LC_bm2_R_LT_Yh_GLU %>%
      add_title("Total land cover by GCAM region / land type / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Data was read in from multiple data sources for each land cover type") %>%
      add_comments("Rounded to 7 digit") %>%
      add_legacy_name("L125.LC_bm2_R_LT_Yh_GLU") %>%
      add_precursors("L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
                     "L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.LC_bm2_R_LT_Yh_GLU

    return_data(L125.LC_bm2_R, L125.LC_bm2_R_GLU, L125.LC_bm2_R_LT_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
