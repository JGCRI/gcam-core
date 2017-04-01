#' module_energy_LA100.CDIAC_downscale_ctry
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.CDIAC_CO2_ctry_hist}. The corresponding file in the
#' original data system was \code{LA100.CDIAC_downscale_ctry.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_energy_LA100.CDIAC_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/CDIAC_CO2_by_nation",
             FILE = "energy/CDIAC_Cseq_by_nation",
             FILE = "energy/CDIAC_nation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.CDIAC_CO2_ctry_hist"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    CDIAC_CO2_by_nation <- get_data(all_data, "energy/CDIAC_CO2_by_nation")
    CDIAC_Cseq_by_nation <- get_data(all_data, "energy/CDIAC_Cseq_by_nation")
    CDIAC_nation <- get_data(all_data, "energy/CDIAC_nation")

    # Merge the sequestration and emissions datasets
    CDIAC_nation %>%
      select(nation, UN_code) %>%
      distinct(UN_code, .keep_all = TRUE) %>%
      left_join_error_no_match(CDIAC_Cseq_by_nation, ., by = "UN_code") %>%

      # Join with CDIAC_CO2_by_nation
      select(nation, year, liquids.sequestration) %>%
      right_join(CDIAC_CO2_by_nation, by = c("nation", "year")) %>%

      # Zero out NAs and subset to years being processed
      mutate(liquids.sequestration = if_else(is.na(liquids.sequestration), 0, liquids.sequestration)) %>%
      filter(year %in% energy.CDIAC_CO2_HISTORICAL_YEARS ) ->
      L100.CDIAC_CO2_ctry_hist

    # Generate time series of Former Soviet Union by country, using national shares in the first available year
    L100.CDIAC_CO2_ctry_hist %>%
      filter(nation != "USSR") ->
      L100.CO2_ctry_noUSSR_hist

    L100.CDIAC_CO2_ctry_hist %>%
      filter(nation == "USSR") ->
      L100.CO2_USSR_hist
    USSR_years <- unique(L100.CO2_USSR_hist$year)

    L100.CO2_USSR_hist %>%
      repeat_add_columns(tibble(iso = CDIAC_nation$iso[CDIAC_nation$nation == "USSR"])) ->
      L100.CO2_USSR_hist_repCtry

    CDIAC_nation %>%
      select(nation, iso) %>%
      distinct(iso, .keep_all = TRUE) %>%
      right_join(L100.CO2_ctry_noUSSR_hist, by = "nation") %>%

      # TODO: at this point there are way more NAs in this d.f. than in the old script!

      na.omit ->
      L100.CO2_ctry_noUSSR_hist

    L100.CO2_ctry_noUSSR_hist <- na.omit( L100.CO2_ctry_noUSSR_hist )
    L100.CO2_postUSSR_hist <- subset( L100.CO2_ctry_noUSSR_hist, iso %in% L100.CO2_USSR_hist_repCtry$iso & year == max( USSR_years ) + 1 )
    datacols <- names( L100.CDIAC_CO2_ctry_hist )[ names( L100.CDIAC_CO2_ctry_hist ) %!in% c( "nation", "year", "iso" ) ]
    L100.CO2_postUSSR_hist_shares <- L100.CO2_postUSSR_hist
    L100.CO2_postUSSR_hist_shares[ datacols ] <- sweep( L100.CO2_postUSSR_hist[ datacols ],
                                                        2, colSums( L100.CO2_postUSSR_hist[ datacols ] ), "/" )



    # Produce output
    L100.CDIAC_CO2_ctry_hist %>%
      add_title("CO2 emissions by country / fuel type / historical year") %>%
      add_units("kt C") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.CDIAC_CO2_ctry_hist") %>%
      add_precursors("energy/CDIAC_CO2_by_nation",
                     "energy/CDIAC_Cseq_by_nation",
                     "energy/CDIAC_nation") ->
      L100.CDIAC_CO2_ctry_hist

    return_data(L100.CDIAC_CO2_ctry_hist)
  } else {
    stop("Unknown command")
  }
}



