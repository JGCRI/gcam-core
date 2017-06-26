#' module_aglu_LB163.bio_Yield_R_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L163.ag_irrBioYield_GJm2_R_GLU}, \code{L163.ag_rfdBioYield_GJm2_R_GLU}. The corresponding file in the
#' original data system was \code{LB163.bio_Yield_R_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB163.bio_Yield_R_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L163.ag_irrBioYield_GJm2_R_GLU",
             "L163.ag_rfdBioYield_GJm2_R_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")


    # Perform computations
    # old comment: This method follows the same method as LB113, with the exception that
    #              the yield indices are computed separately for rainfed/irrigated, but
    #              again against the global average for each crop, across both irrigated
    #              and rainfed. This method should roughly preserve the global average
    #              bioenergy yields; what we want to avoid here is increasing the global
    #              average yields just by separating irr/rfd.
    #
    # Step 1: Aggregate FAO harvested area and production for each GTAP_crop to get global
    # yields in a base year.


    # Step 2: Calculate yield for each region-GLU-crop-irrigation and compare to global
    # average yield from Step 1, according to the formula



    # Produce outputs
      tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L163.ag_irrBioYield_GJm2_R_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L163.ag_irrBioYield_GJm2_R_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L163.ag_rfdBioYield_GJm2_R_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L163.ag_rfdBioYield_GJm2_R_GLU

    return_data(L163.ag_irrBioYield_GJm2_R_GLU, L163.ag_rfdBioYield_GJm2_R_GLU)
  } else {
    stop("Unknown command")
  }
}
