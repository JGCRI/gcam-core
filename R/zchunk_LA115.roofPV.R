#' module_energy_LA115.roofPV
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L115.RsrcCurves_EJ_R_roofPV}. The corresponding file in the
#' original data system was \code{LA115.roofPV.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA115.roofPV <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A15.roofPV_curves",
             FILE = "energy/A15.roofPV_TechChange",
             FILE = "socioeconomics/L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.RsrcCurves_EJ_R_roofPV"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A15.roofPV_curves <- get_data(all_data, "energy/A15.roofPV_curves")
    A15.roofPV_TechChange <- get_data(all_data, "energy/A15.roofPV_TechChange") #not used
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "socioeconomics/L100.Pop_thous_ctry_Yh")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...

    #Categorizing countries and regions to GCAM Region ID; selecting population in 2010
    L100.Pop_thous_ctry_Yh %>%
      select(iso, X2010) %>%
      left_join(iso_GCAM_regID %>%
                  select(-country_name)) %>%
      select(-iso) ->
      x

    #Sum of population by regions
    x %>%
      group_by(region_GCAM3) %>%
      summarise(popSum = sum(X2010)) ->
      pop_RG3


    #Building resource curves by GCAM Region ID
    x %>%
      left_join(pop_RG3, by = "region_GCAM3") %>%
      left_join(A15.roofPV_curves, by = "region_GCAM3") %>%
      mutate(maxSubResource = maxSubResource*X2010/popSum) %>%
      rename(`curve.exponent` = `curve-exponent`, `mid.price` = `mid-price`) %>%
      group_by(GCAM_region_ID, resource, subresource, curve.exponent, gdpSupplyElast, subResourceCapacityFactor) %>%
      summarise(maxSubResource = sum(maxSubResource),
                mid.price = median(mid.price)) %>%
      select(GCAM_region_ID, resource, subresource, maxSubResource, mid.price, curve.exponent, gdpSupplyElast, subResourceCapacityFactor) ->
      L115.RsrcCurves_EJ_R_roofPV

    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L115.RsrcCurves_EJ_R_roofPV %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L115.RsrcCurves_EJ_R_roofPV") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A15.roofPV_curves", "energy/A15.roofPV_TechChange", "socioeconomics/L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L115.RsrcCurves_EJ_R_roofPV

    return_data(L115.RsrcCurves_EJ_R_roofPV)
  } else {
    stop("Unknown command")
  }
}
