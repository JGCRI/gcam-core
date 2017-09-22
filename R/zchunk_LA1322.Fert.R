#' module_gcam.usa_LA1322.Fert
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.out_Mt_state_Fert_Yh}, \code{L1322.IO_GJkg_state_Fert_F_Yh}, \code{L1322.in_EJ_state_Fert_Yh}. The corresponding file in the
#' original data system was \code{LA1322.Fert.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA1322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/Census_ind_VoS_state",
             "L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.out_Mt_state_Fert_Yh",
             "L1322.IO_GJkg_state_Fert_F_Yh",
             "L1322.in_EJ_state_Fert_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    Census_ind_VoS_state <- get_data(all_data, "gcam-usa/Census_ind_VoS_state")
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh")

    # ===================================================

    #RIght now weverything is from the old data system exact ransaltion and comments
    # 2. Perform computations
    # "Assigning national fertilizer production to states on the basis of value of shipments of NAICS 3273 by state" )
    # Note: The NAICS code 3253 includes fertilizers (nitrogenous, phosphatic, and mixed), pesticides, and other agricultural chemicals

    Census_ind_VoS_state %>%
      filter(NAICS_code == 3253) ->
      L1322.VoS_share_state_Fert

    L1322.VoS_share_state_Fert %>%
      group_by(year) %>%
      mutate(value = VoS_thousUSD/sum(VoS_thousUSD)) %>%
      ungroup %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) ->
      L1322.VoS_share_state_Fert

    L1322.VoS_share_state_Fert %>%
      mutate(sector = aglu.FERT_NAME) ->
      L1322.VoS_share_state_Fert

    #Note that the only relevant fuel in the USA is gas
    L1322.VoS_share_state_Fert %>%
      mutate(fuel = "gas") ->
      L1322.VoS_share_state_Fert


    L1322.Fert_Prod_MtN_R_F_Y %>%
      filter(GCAM_region_ID == 1) %>%
      select(year, value, sector, fuel)->
      L1322.Fert_Prod_MtN_R_F_Y_USA


    L1322.VoS_share_state_Fert %>%
      left_join(L1322.Fert_Prod_MtN_R_F_Y_USA, by = c("year", "sector", "fuel")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, value, year)->
      L1322.out_Mt_state_Fert_Yh

  #  printlog( "Assuming all states have the same IO coefficients" )

    L1322.out_Mt_state_Fert_Yh %>%
      select(state) %>%
      distinct ->
      state_list

    L1322.IO_R_Fert_F_Yh %>%
      filter(GCAM_region_ID == gcam.USA_CODE, fuel == "gas") %>%
      repeat_add_columns(state_list) %>%
      select(state, sector, fuel, year, value) ->
      L1322.IO_GJkg_state_Fert_F_Yh

    L1322.IO_GJkg_state_Fert_F_Yh %>%
      left_join(L1322.out_Mt_state_Fert_Yh, by = c("state", "sector", "fuel", "year")) %>%
      na.omit %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value) ->
      L1322.in_EJ_state_Fert_Yh



    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L1322.out_Mt_state_Fert_Yh %>%
      add_title("add") %>%
      add_units("add") %>%
      add_comments("added") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.out_Mt_state_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-usa/Census_ind_VoS_state", "L1322.IO_R_Fert_F_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.out_Mt_state_Fert_Yh

    L1322.IO_GJkg_state_Fert_F_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.IO_GJkg_state_Fert_F_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-usa/Census_ind_VoS_state", "L1322.IO_R_Fert_F_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.IO_GJkg_state_Fert_F_Yh

    L1322.in_EJ_state_Fert_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.in_EJ_state_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-usa/Census_ind_VoS_state", "L1322.IO_R_Fert_F_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.in_EJ_state_Fert_Yh

    return_data(L1322.out_Mt_state_Fert_Yh, L1322.IO_GJkg_state_Fert_F_Yh, L1322.in_EJ_state_Fert_Yh)
  } else {
    stop("Unknown command")
  }
}
