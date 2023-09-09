# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L1221.ghg_agr_R_S_T_Y_IRR
#'
#' Downscale emissions to irrigated/rainfed technologies on the basis of production share.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1221.ghg_tg_R_agr_C_Y_GLU_IRR}. The corresponding file in the
#' original data system was \code{L1221.ghg_agr_R_S_T_Y_IRR.R} (emissions level1).
#' @details Downscales emissions to irrigated/rainfed technologies by production share.
#' Multiply total ag emissions (\code{module_emissions_L112.ceds_ghg_en_R_S_T_Y}) by production share by irr/rfd (\code{L1211.ag_irrShare_R_C_Y_GLU_irr})
#' which are both calculated in \code{\link{module_emissions_L122.ghg_agr_R_S_T_Y}} and
#' \code{\link{module_emissions_L1211.nonco2_awb_R_S_T_Y_IRR}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join mutate select
#' @author RMH AUG 2017
module_emissions_L1221.ghg_agr_R_S_T_Y_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1211.ag_irrShare_R_C_Y_GLU_irr",
             "L122.ghg_tg_R_agr_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1221.ghg_tg_R_agr_C_Y_GLU_IRR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    value.emissions <- value.share <- NULL  # silence package check notes

    # Load required inputs
    L1211.ag_irrShare_R_C_Y_GLU_irr <- get_data(all_data, "L1211.ag_irrShare_R_C_Y_GLU_irr", strip_attributes = TRUE)
    L122.ghg_tg_R_agr_C_Y_GLU <- get_data(all_data, "L122.ghg_tg_R_agr_C_Y_GLU", strip_attributes = TRUE)

    # Perform computations

    # Production shares were computed in a prior file and written out.
    # Repeat ag emission (L122.ghg_tg_R_agr_C_Y_GLU) for each irrigation (IRR) and rainfed (RFD)
    # Join in Production share by IRR/RFD then multiply emissions by production share
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- L122.ghg_tg_R_agr_C_Y_GLU %>%
      repeat_add_columns( tibble(Irr_Rfd = c("IRR", "RFD"))) %>%
      left_join(L1211.ag_irrShare_R_C_Y_GLU_irr,
                by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "Irr_Rfd", "year"),
                suffix = c(".emissions", ".share")) %>% # under timeshift conditions, L122.ghg_tg_R_agr_C_Y_GLU has NaN values (N2O_AGR), which throws error in left_join_no_error as NA values, even though they are matched correctly.
      mutate(value = value.emissions * value.share) %>%
      select(-value.emissions, -value.share)

    # Produce outputs
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- L1221.ghg_tg_R_agr_C_Y_GLU_IRR %>%
      add_title("Agriculture emissions by GCAM region / commodity / GLU / irrigation level / historical year") %>%
      add_units("Tg") %>%
      add_comments("outputs calculated in L1211.ag_irrShare_R_C_Y_GLU_irr") %>%
      add_legacy_name("L1221.ghg_tg_R_agr_C_Y_GLU_IRR") %>%
      add_precursors("L1211.ag_irrShare_R_C_Y_GLU_irr",
                     "L122.ghg_tg_R_agr_C_Y_GLU")

    return_data(L1221.ghg_tg_R_agr_C_Y_GLU_IRR)
  } else {
    stop("Unknown command")
  }
}
