# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L151.ctrl_R_en_S_T
#'
#' Compute maximum reduction by region / sector / gas.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L151.nonghg_ctrl_R_en_S_T}. The corresponding file in the
#' original data system was \code{L151.ctrl_R_en_S_T.R} (emissions level1).
#' @details Compute maximum reduction by region and sector for SO2, CO, NOx, NMVOC (all 2005 reference),
#' BC, and OC (2005). Reductions are limited to [0, 100]. EDGAR emissions inventory only goes up
#' to 2008, so for calculating emissions coefficients, 2005 is the final base year. These steps
#' calculate the "reduction" required to get from the 2005 observed emissions coefficients (for each
#' region / technology / emissions species) to the exogenous minimum coefficient for each
#' technology / emission species.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr gather
#' @author BBL April 2017
module_emissions_L151.ctrl_R_en_S_T <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A51.min_coeff",
             "L111.nonghg_tgej_R_en_S_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L151.nonghg_ctrl_R_en_S_T"))
  } else if(command == driver.MAKE) {

    year <- value <- Non.CO2 <- supplysector <- subsector <- stub.technology <-
      GCAM_region_ID <- curr_coeff <- min_coeff <- max_reduction <- . <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    A51.min_coeff <- get_data(all_data, "emissions/A51.min_coeff")
    L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh", strip_attributes = TRUE)

    # First, set up min coeff data frame
    A51.min_coeff %>%
      gather(Non.CO2, value, -supplysector, -subsector, -stub.technology) %>%
      na.omit ->
      L151.min_coeff

    # The following code is common to all gases
    # It joins w/ the min coefficient d.f. and then computes max reduction
    map_and_compute_max_reduction <- function(obj, L151.min_coeff) {
      obj %>%
        # Map in relevant information
        rename(curr_coeff = value) %>%
        left_join(L151.min_coeff, by = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
        rename(min_coeff = value) %>%
        na.omit %>%
        # Compute maximum reduction as percentage
        mutate(max_reduction = 100 * (curr_coeff - min_coeff) / curr_coeff) %>%
        na.omit %>%
        mutate(max_reduction = if_else(max_reduction > 100, 100, max_reduction),
               max_reduction = if_else(max_reduction < 0, 0, max_reduction))
    } # map_and_compute_max_reduction


    # Compute max emissions reduction for non_CO2s
    L111.nonghg_tgej_R_en_S_F_Yh %>%
      filter(year == 2005) %>%
      map_and_compute_max_reduction(L151.min_coeff) ->
      L151.nonghg_ctrl_R_en_S_T


    L151.nonghg_ctrl_R_en_S_T %>%
      add_title("Maximum reduction by region / sector / gas") %>%
      add_units("%") %>%
      add_comments("Compute maximum reduction by region and sector for SO2, CO, NOx, NMVOC (all 2005 reference), BC, and OC (2005)") %>%
      add_legacy_name("L151.nonghg_ctrl_R_en_S_T") %>%
      add_precursors("emissions/A51.min_coeff",
                     "L111.nonghg_tgej_R_en_S_F_Yh") ->
      L151.nonghg_ctrl_R_en_S_T

    return_data(L151.nonghg_ctrl_R_en_S_T)
  } else {
    stop("Unknown command")
  }
}
