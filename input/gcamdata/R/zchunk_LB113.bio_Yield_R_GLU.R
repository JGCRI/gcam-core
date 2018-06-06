#' module_aglu_LB113.bio_Yield_R_GLU
#'
#' Calculate base year bioenergy yields by GCAM region and GLU
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.ag_bioYield_GJm2_R_GLU}. The corresponding file in the
#' original data system was \code{LB113.bio_Yield_R_GLU.R} (aglu level1).
#' @details Calculate global average yields for each FAO crop in the base year;
#' calculate each region / zone / crop's comparative yield; compute bioenergy yields as
#' this region/zone-specific index multiplied by a base yield.
#' @references Wullschleger, S.D., E.B. Davis, M.E. Borsuk, C.A. Gunderson, and L.R. Lynd. 2010.
#' Biomass production in switchgrass across the United States: database description and determinants
#' of yield. Agronomy Journal 102: 1158-1168. doi:10.2134/agronj2010.0087.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL June 2017
module_aglu_LB113.bio_Yield_R_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.ag_bioYield_GJm2_R_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    value <- HA_ha <- HA <- Yield_avg <- Yield <- Ratio <- iso <- GCAM_region_ID <- GCAM_REGION_ID
    GLU <- Ratio_weight <- . <- YieldIndex <- GTAP_crop <- NULL # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")

    # Calculate global average yields for each FAO crop in the base year (31-39 old file)
    L100.LDS_ag_HA_ha %>%
      group_by(GTAP_crop) %>%
      summarise(HA_ha = sum(value)) ->
      L113.ag_HA_ha_glbl_crop

    L100.LDS_ag_prod_t %>%
      group_by(GTAP_crop) %>%
      summarise(value = sum(value)) %>%
      left_join_error_no_match(L113.ag_HA_ha_glbl_crop, by = "GTAP_crop") %>%
      mutate(Yield_avg = value / HA_ha) ->
      L113.ag_prod_t_glbl_crop

    # Calculate each region / zone / crop's comparative yield (41-50)
    L100.LDS_ag_HA_ha %>%
      rename(HA = value) %>%
      left_join_error_no_match(L100.LDS_ag_prod_t, by = c("iso", "GLU", "GTAP_crop")) %>%
      mutate(Yield = value / HA) %>%
      # Drop the missing values, where the harvested area was above the min threshold but production was not
      na.omit ->
      LDS_ag_Yield_tha

    # Match in the global avg yield for each crop, sum up both area and the yield-to-avg-yield ratio
    # by region and GLU, and then compute the area-weighted yield index (52-61)
    LDS_ag_Yield_tha %>%
      left_join_error_no_match(select(L113.ag_prod_t_glbl_crop, GTAP_crop, Yield_avg), by = "GTAP_crop") %>%
      mutate(Ratio = Yield / Yield_avg,
             Ratio_weight = Ratio * HA) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, GLU) %>%
      summarise(HA = sum(HA), Ratio_weight = sum(Ratio_weight)) %>%
      ungroup %>%
      mutate(YieldIndex = Ratio_weight / HA) ->
      L113.YieldIndex_R_GLU

    # Bioenergy yields are equal to this region/zone-specific index multiplied by a base yield
    # The base yield is taken to be the maximum of the yields in the USA region, or the region
    # containing the USA, because the Wullschleger paper (10.2134/agronj2010.0087)
    # from which the yield estimate was derived was for the USA.
    iso_GCAM_regID %>%
      filter(iso == "usa") %>%
      pull(GCAM_region_ID) ->
      USAreg
    L113.base_bio_yield_tha <- aglu.MAX_BIO_YIELD_THA / max(L113.YieldIndex_R_GLU$YieldIndex[L113.YieldIndex_R_GLU$GCAM_region_ID == USAreg])
    L113.base_bio_yield_GJm2 <- L113.base_bio_yield_tha * aglu.BIO_ENERGY_CONTENT_GJT / CONV_HA_M2
    L113.YieldIndex_R_GLU %>%
      select(GCAM_region_ID, GLU, YieldIndex) %>%
      mutate(Yield_GJm2 = YieldIndex * L113.base_bio_yield_GJm2) %>%
      select(-YieldIndex) %>%

    # Produce outputs
      add_title("Base year bioenergy yields by GCAM region and GLU") %>%
      add_units(" GJ/m2") %>%
      add_comments("Calculate global average yields for each FAO crop in the base year;") %>%
      add_comments("calculate each region / zone / crop's comparative yield; compute bioenergy yields") %>%
      add_comments("as this region/zone-specific index multiplied by a base yield") %>%
      add_legacy_name("L113.ag_bioYield_GJm2_R_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t") ->
      L113.ag_bioYield_GJm2_R_GLU

    return_data(L113.ag_bioYield_GJm2_R_GLU)
  } else {
    stop("Unknown command")
  }
}
