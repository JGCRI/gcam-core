#' module_emissions_L1221.ghg_agr_R_S_T_Y_IRR
#'
#' Downscales emissions to irrigated/rainfed technologies on the basis of production share
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1221.ghg_tg_R_agr_C_Y_GLU_IRR}. The corresponding file in the
#' original data system was \code{L1221.ghg_agr_R_S_T_Y_IRR.R} (emissions level1).
#' @details Downscales emissions to irrigated/rainfed technologies. These are already calculated
#' in L1211.ag_irrShare_R_C_Y_GLU_irr
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RMH AUG 2017

module_emissions_L1221.ghg_agr_R_S_T_Y_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1211.ag_irrShare_R_C_Y_GLU_irr",
             "L122.ghg_tg_R_agr_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1221.ghg_tg_R_agr_C_Y_GLU_IRR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L1211.ag_irrShare_R_C_Y_GLU_irr <- get_data(all_data, "L1211.ag_irrShare_R_C_Y_GLU_irr")
    L122.ghg_tg_R_agr_C_Y_GLU <- get_data(all_data, "L122.ghg_tg_R_agr_C_Y_GLU")

    # Perform computations

    # Production shares were computed in a prior file and written out, so just rearrange in one step
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- L122.ghg_tg_R_agr_C_Y_GLU %>%
      select(-value) %>%
      repeat_add_columns( tibble(Irr_Rfd = c( "IRR", "RFD" ) ) ) %>%
      left_join_error_no_match(L1211.ag_irrShare_R_C_Y_GLU_irr, by = c("GCAM_region_ID","GCAM_commodity","GLU","Irr_Rfd","year"))

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- L1221.ghg_tg_R_agr_C_Y_GLU_IRR %>%
      add_title("Agriculture emissions by GCAM region / commodity / GLU / irrigation level / historical year") %>%
      add_units("Tg") %>%
      add_comments("outputs calculated in L1211.ag_irrShare_R_C_Y_GLU_irr, just add column IRR_Rfd and rearrange") %>%
      add_legacy_name("L1221.ghg_tg_R_agr_C_Y_GLU_IRR") %>%
      add_precursors("L1211.ag_irrShare_R_C_Y_GLU_irr",
                     "L122.ghg_tg_R_agr_C_Y_GLU") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR)

    return_data(L1221.ghg_tg_R_agr_C_Y_GLU_IRR)
  } else {
    stop("Unknown command")
  }
}
