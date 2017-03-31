#' module_energy_LA117.tradbio
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L117.RsrcCurves_EJ_R_tradbio}. The corresponding file in the
#' original data system was \code{LA117.tradbio.R} (energy level1).
#' @details Creates regional traditional biomass supply curves by multiplying the max historical amount
#' used in each region by the supply curve amount in the global assumption supply curve
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH March 2017
#'
module_energy_LA117.tradbio <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "common/iso_GCAM_regID",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "energy/A17.tradbio_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L117.RsrcCurves_EJ_R_tradbio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh")
    A17.tradbio_curves <- get_data(all_data, "energy/A17.tradbio_curves")

    # ===================================================

    # Calculating the max resource of tradbio as the maximum during the historical years
    L117.maxSubResource_tradbio <- L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(fuel == "biomass_tradbio", sector == "TPES") %>%
      gather(Xyear, value, X1971:X2010) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(value = max(value))

    # Writing the supply curves to all regions, multiplying maxSubResouce by quantity available at each grade
    repeat_add_columns(A17.tradbio_curves,L117.maxSubResource_tradbio) %>%
      arrange(GCAM_region_ID) %>%
      mutate(available = available*value) %>%
    # Removing resource curves in regions where this fuel does not apply
      filter(value != 0) %>%
      select(GCAM_region_ID, resource, subresource, grade, available, extractioncost) %>%

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all

      add_title("Traditional biomass resources by GCAM region ") %>%
      add_units("EJ") %>%
      add_comments("Multiply the max historical amount used in each region by the supply curve amount") %>%
      add_legacy_name("L117.RsrcCurves_EJ_R_tradbio") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A17.tradbio_curves") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_XYEAR) ->
      L117.RsrcCurves_EJ_R_tradbio

    return_data(L117.RsrcCurves_EJ_R_tradbio)
  } else {
    stop("Unknown command")
  }
}
