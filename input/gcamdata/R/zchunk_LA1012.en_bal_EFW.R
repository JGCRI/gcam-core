#' module_energy_LA1012.en_bal_EFW
#'
#' Adjustments to the IEA energy balances to account for energy-for-water processes explicitly modeled.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1012.en_bal_EJ_R_Si_Fi_Yh}.
#' @details This chunk deducts bottom-up energy-for-water (EFW) estimates from the IEA Energy Balances estimates of
#'   energy consumption in the commercial and industrial sectors, applying a set of rules that ensure that resulting
#'   energy demand levels are reasonable. The result is the full energy balance table with an additional EFW
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author GPK January 2019
module_energy_LA1012.en_bal_EFW <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "water/EFW_mapping",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L171.in_EJ_R_desal_F_Yh",
             "L172.in_EJ_R_irr_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.MAKE) {

    # silence package check
    technology <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")
    L171.in_EJ_R_desal_F_Yh <- get_data(all_data, "L171.in_EJ_R_desal_F_Yh")
    L172.in_EJ_R_irr_F_Yh <- get_data(all_data, "L172.in_EJ_R_irr_F_Yh")

    # ===================================================

    # modifications go here
    L1012.en_bal_EJ_R_Si_Fi_Yh <- mutate(L1011.en_bal_EJ_R_Si_Fi_Yh, asdf = "blah") %>%
      select(-asdf)

    # ===================================================

    # Produce outputs

    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      add_title("Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year", overwrite = TRUE) %>%
      add_units("EJ") %>%
      add_comments("EFW processes disaggregated, commercial/industrial energy consumption quantities modified") %>%
      add_precursors("common/iso_GCAM_regID", "energy/calibrated_techs", "water/EFW_mapping", "L1011.en_bal_EJ_R_Si_Fi_Yh") ->
      L1012.en_bal_EJ_R_Si_Fi_Yh

    return_data(L1012.en_bal_EJ_R_Si_Fi_Yh)
  } else {
    stop("Unknown command")
  }
}
