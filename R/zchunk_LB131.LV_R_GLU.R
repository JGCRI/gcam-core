#' module_aglu_LB131.LV_R_GLU
#'
#' Compute average value of land in $/m2
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.LV_USD75_m2_R_GLU}. The corresponding file in the
#' original data system was \code{LB131.LV_R_GLU.R} (aglu level1).
#' @details Computes average value of land in $/m2, using total land value
#' and harvested cropland area.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KVC April 2017
#' @export
module_aglu_LB131.LV_R_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "temp-data-inject/L100.GTAP_LV_milUSD",
             FILE = "temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.LV_USD75_m2_R_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.GTAP_LV_milUSD <- get_data(all_data, "temp-data-inject/L100.GTAP_LV_milUSD")
    L122.LC_bm2_R_HarvCropLand_Yh_GLU <- get_data(all_data, "temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU")
  
    # Calculate the total value of each geographic land unit (GLU)
    L100.GTAP_LV_milUSD %>% 
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                                         # Map in ISO codes
      mutate(value = value * CONV_2001_1975_USD) %>%                                                    # Convert to 1975$
      group_by(GCAM_region_ID, GLU) %>%                                                                 # Group by GCAM_region_ID and GLU
      summarize(value = sum(value)) %>%                                                                 # Aggregate value to GCAM region and GLU
      rename(LV_milUSD75 = value) ->                                                                    # Rename column to what is used in old data system
      LV_R_GLU
    
    # Prepare land area for use in compuating land value
    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
      gather(year, value, -GCAM_region_ID, -GLU, -Land_Type) %>%                                      # Convert to long format
      filter(year == X_GTAP_HISTORICAL_YEAR) %>%                                                      # Only use cropland area from the GTAP historical year
      rename(HarvCropLand_bm2 = value) ->                                                             # Rename column to what is used in old data system
      LC_R_GLU
    
    # Compute value in $/m2
    LV_R_GLU %>% 
      left_join(LC_R_GLU, by = c( "GCAM_region_ID", "GLU"))  %>%                                      # Map in GTAP harvested cropland area
      mutate(LV_USD75_m2 = LV_milUSD75 / CONV_BIL_MIL / HarvCropLand_bm2) %>%                         # Calculate land value ($/m2) using value and cropland area
      select(-Land_Type, -year ) ->                                                                   # Remove extra columns
      L131.LV_USD75_m2_R_GLU

    # Produce outputs
    L131.LV_USD75_m2_R_GLU %>%
      add_title("Land value by GCAM region and GLU") %>%
      add_units("1975$ per m2") %>%
      add_comments("Compute average value of land using total value from L100 and harvested area from L122") %>%
      add_legacy_name("L131.LV_USD75_m2_R_GLU") %>%
      add_precursors("temp-data-inject/L100.GTAP_LV_milUSD", "temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU", "common/iso_GCAM_regID") ->
      L131.LV_USD75_m2_R_GLU

    return_data(L131.LV_USD75_m2_R_GLU)
  } else {
    stop("Unknown command")
  }
}



