# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1232.desal_from_elec
#'
#' Output ratio of desalinated seawater produced as secondary output from electric power plants.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1232.desalsecout_R_elec_F_tech}.
#' @details This chunk computes the output-ratio of desalinated water from electric power plants, indicated in m3 of
#'   desalinated water per GJ of electricity produced. It is only computed in region(s) with combined electric and desal
#'   plants.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate rename select ungroup
#' @author GPK January 2019
module_energy_LA1232.desal_from_elec <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1231.out_EJ_R_elec_F_tech_Yh",
             "L171.out_km3_R_desalfromelec_Yh",
             "L171.out_km3_R_desal_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1232.desalsecout_R_elec_F_tech"))
  } else if(command == driver.MAKE) {

    # silence package check
    year <- technology <- GCAM_region_ID <- value <- desal_fromelec <- elec_out <-
      desal_km3 <- secout_coef <- sector <- fuel <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh", strip_attributes = TRUE)
    L171.out_km3_R_desalfromelec_Yh <- get_data(all_data, "L171.out_km3_R_desalfromelec_Yh", strip_attributes = TRUE)
    L171.out_km3_R_desal_F_tech_Yh <- get_data(all_data, "L171.out_km3_R_desal_F_tech_Yh", strip_attributes = TRUE)

    # ===================================================

    # Secondary output coefficients on desalinated water produced by combined elec + desal plants

    # This method adds up the output of all candidate electric power technologies that are assumed to be capable of
    # co-producing desalinated water; this is the denominator of the secondary output ratio. The numerator is the volume
    # of desalinated water produced. (At this point we don't have fuel- or technology-level data on desalinated water
    # co-production)
    L1232.out_EJ_R_elec_Yh <- filter(L1231.out_EJ_R_elec_F_tech_Yh, technology %in% efw.ELEC_DESAL_TECHS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(elec_out = sum(value)) %>%
      ungroup()

    L1232.desalsecout_R_elec <- rename(L171.out_km3_R_desalfromelec_Yh, desal_fromelec = value) %>%
      left_join_error_no_match(L1232.out_EJ_R_elec_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(secout_coef = if_else(is.na(desal_fromelec / elec_out), 0, desal_fromelec / elec_out))

    # An additional step ensures that the desalination market exists in the given region; if the entire desal
    # supply came from secondary output of electricity + desal plants, then the model wouldn't be able to compute a price
    # for the commodity and the model would crash. This step just checks to make sure this isn't the case.
    L1232.out_km3_R_desal_Yh <- group_by(L171.out_km3_R_desal_F_tech_Yh, GCAM_region_ID, year) %>%
      summarise(desal_primary = sum(desal_km3)) %>%
      ungroup()

    L1232.desalsecout_R_elec <- left_join(L1232.desalsecout_R_elec,
                                          L1232.out_km3_R_desal_Yh,
                                          by = c("GCAM_region_ID", "year"))
    if(any(L1232.desalsecout_R_elec$desal_fromelec > 0 & L1232.desalsecout_R_elec$desal_primary == 0)){
      stop(paste0("Secondary output of desalinated water but no market for desalinated water in region: ",
                  unique(L1232.desalsecout_R_elec$GCAM_region_ID[
                    L1232.desalsecout_R_elec$desal_fromelec > 0 & L1232.desalsecout_R_elec$desal_primary == 0])))
    }

    L1232.desalsecout_R_elec_F_tech <- filter(L1231.out_EJ_R_elec_F_tech_Yh,
                                              technology %in% efw.ELEC_DESAL_TECHS &
                                                GCAM_region_ID %in% L1232.desalsecout_R_elec$GCAM_region_ID) %>%
      left_join_error_no_match(select(L1232.desalsecout_R_elec, GCAM_region_ID, year, secout_coef),
                               by = c("GCAM_region_ID", "year")) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, secout_coef)

    # ===================================================

    # Produce outputs

    L1232.desalsecout_R_elec_F_tech %>%
      add_title("Secondary output ratio of water from electricity sector by GCAM region / fuel / technology / historical year", overwrite = TRUE) %>%
      add_units("m^3/GJ") %>%
      add_comments("Desalinated water secondary output per unit electricity produced") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh",
                     "L171.out_km3_R_desalfromelec_Yh",
                     "L171.out_km3_R_desal_F_tech_Yh") ->
      L1232.desalsecout_R_elec_F_tech

    return_data(L1232.desalsecout_R_elec_F_tech)
  } else {
    stop("Unknown command")
  }
}
