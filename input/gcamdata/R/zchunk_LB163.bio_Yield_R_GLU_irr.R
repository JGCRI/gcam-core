#' module_aglu_LB163.bio_Yield_R_GLU_irr
#'
#' Compute base year rainfed and irrigated bioenergy crop yields for each GCAM region and GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L163.ag_irrBioYield_GJm2_R_GLU}, \code{L163.ag_rfdBioYield_GJm2_R_GLU}. The corresponding file in the
#' original data system was \code{LB163.bio_Yield_R_GLU_irr.R} (aglu level1).
#' @details A global average yield is calculated for each GTAP crop. This is then used to calculate a yield Ratio for each
#' iso-GLU-irrigation for each GTAP crop. This ratio and harvested area are then summed across all GTAP crops to the GCAM
#' region-GLU-irrigation level and are used to calculate a YieldIndex for each region-GLU-irrigation. This YieldIndex is
#' then multiplied by a base yield (calculated from USA yields) to get bioenergy yields for each region-GLU-irrigation.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ACS June 2017
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

    GTAP_crop <- value <- Prod <- HA <- irrHA <- irrProd <- rfdHA <- rfdProd <- Yield_avg <-
      Yield <- Ratio <- iso <- GCAM_region_ID <- GLU <- Irr_Rfd <- Ratio_weight <- . <-
      YieldIndex <- NULL  # silence package check notes

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
    #              and rainfed.
    #
    # Step 1: Aggregate FAO harvested area and production for each GTAP_crop to get global
    # yields in a base year.
    # Harvested area:
    L100.LDS_ag_HA_ha %>%
      group_by(GTAP_crop) %>%
      summarise(HA = sum(value)) %>%
      ungroup() ->
      L163.ag_HA_ha_glbl_crop

    # Aggregate Production and join aggregated HA to calculate global average yield for each GTAP crop:
    L100.LDS_ag_prod_t %>%
      group_by(GTAP_crop) %>%
      summarise(Prod = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L163.ag_HA_ha_glbl_crop, by = "GTAP_crop") %>%
      mutate(Yield_avg = Prod / HA) ->
      L163.ag_prod_t_glbl_crop


    # Step 2: Calculate yield for each region-GLU-GTAPcrop-irrigation and compare to global
    # average yield from Step 1.
    #
    # Process irrigated HA and production by iso-GLU-GTAPcrop for joining and calculating yield:
    L151.ag_irrHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(HA = irrHA) ->
      L151.ag_irrHA_ha_ctry_crop

    L151.ag_irrProd_t_ctry_crop %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(Prod = irrProd) ->
      L151.ag_irrProd_t_ctry_crop

    # Process rainfed HA and production by iso-GLU-GTAPcrop for joining and calculating yield:
    L151.ag_rfdHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(HA = rfdHA) ->
      L151.ag_rfdHA_ha_ctry_crop

    L151.ag_rfdProd_t_ctry_crop %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(Prod = rfdProd) ->
      L151.ag_rfdProd_t_ctry_crop

    # Join all four processed L151 data frames and use to calculate yield
    # by iso-GLU-GTAPcrop-irrigation.
    # Then, join in global average yield from step 1 for each GTAPcrop,
    # use it to compute a Ratio = Yield / Yield_avg and
    # a Ratio_weight = Ratio * HA.
    # HA and Ratio_weight can then be aggregated from iso to GCAM region
    # and used to calculate YieldIndex = Ratio_weight/HA:
    L151.ag_irrHA_ha_ctry_crop %>%
      bind_rows(L151.ag_rfdHA_ha_ctry_crop) %>%
      left_join_error_no_match(bind_rows(L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop),
                               by = c("iso", "GLU", "GTAP_crop", "Irr_Rfd")) %>%
      mutate(Yield = Prod / HA) %>%
      # drop NA's - values where HA = 0
      na.omit() %>%
      # join global average yield for each GTAP crop, and calculate Ratio and Ratio_weight
      # for aggregation from iso to GCAM region
      left_join_error_no_match(select(L163.ag_prod_t_glbl_crop, GTAP_crop, Yield_avg),
                               by = "GTAP_crop") %>%
      mutate(Ratio = Yield / Yield_avg,
             Ratio_weight = Ratio * HA) %>%
      # add GCAM region info and aggregate HA and Ratio_weight
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, GLU, Irr_Rfd) %>%
      summarise(HA = sum(HA),
                Ratio_weight = sum(Ratio_weight)) %>%
      ungroup() %>%
      mutate(YieldIndex = Ratio_weight / HA) ->
      L163.YieldIndex_R_GLU_irr


    # Step 3: Bioenergy yields are equal to the region-glu-irrigation index,
    # calculated in Step 2 - L163.YieldIndex_R_GLU_irr, multiplied by a base yield.
    # The base yield is taken to be the maximum of the yields in the USA
    # region, or the region containing the USA because the Wullschleger paper
    # from which the yield estimate was derived was for the USA.

    # USA region ID:
    iso_GCAM_regID %>%
      filter(iso == "usa") %>%
      pull(GCAM_region_ID) ->
      USAreg

    # Calculate the base yield, a scaler value:
     L163.base_bio_yield_tha <- aglu.MAX_BIO_YIELD_THA / max(L163.YieldIndex_R_GLU_irr$YieldIndex[L163.YieldIndex_R_GLU_irr$GCAM_region_ID == USAreg])
     L163.base_bio_yield_GJm2 <- L163.base_bio_yield_tha * aglu.BIO_ENERGY_CONTENT_GJT / CONV_HA_M2

    # Finally, calculate bioenergy yields in each region-glu-irrigation combo:
    L163.YieldIndex_R_GLU_irr %>%
      mutate(Yield_GJm2 = YieldIndex * L163.base_bio_yield_GJm2) %>%
      select(-HA, -Ratio_weight, -YieldIndex) ->
      L163.ag_bioYield_GJm2_R_GLU_irr



    # Step 4: Split rainfed and irrigated into separate tables for the write-out
    # (to be consistent with other files)
    L163.ag_bioYield_GJm2_R_GLU_irr %>%
      filter(Irr_Rfd == "IRR") %>%
      select(-Irr_Rfd) ->
      L163.ag_irrBioYield_GJm2_R_GLU

    L163.ag_bioYield_GJm2_R_GLU_irr %>%
      filter(Irr_Rfd == "RFD") %>%
      select(-Irr_Rfd) ->
      L163.ag_rfdBioYield_GJm2_R_GLU


    # Produce outputs
    L163.ag_irrBioYield_GJm2_R_GLU %>%
      add_title("Reference base year bioenergy yields for irrigated crops by GCAM region / GLU") %>%
      add_units("Gigajoule per square meter (GJ/m2)") %>%
      add_comments("A global average yield is calculated for each GTAP_crop and is used to calculate") %>%
      add_comments("aggregate irrigated harvested areas to the GCAM region level (summing over all GTAPcrops), ") %>%
      add_comments("and then to calculate a Yield Index for each irrigated region-GLU. The region-GLU ") %>%
      add_comments("specific index is then multiplied by a base yield to give irrigated bioenergy yields.") %>%
      add_legacy_name("L163.ag_irrBioYield_GJm2_R_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop")  ->
      L163.ag_irrBioYield_GJm2_R_GLU
    L163.ag_rfdBioYield_GJm2_R_GLU %>%
      add_title("Reference base year bioenergy yields for rainfed crops by GCAM region / GLU") %>%
      add_units("Gigajoule per square meter (GJ/m2)") %>%
      add_comments("A global average yield is calculated for each GTAP_crop and is used to calculate") %>%
      add_comments("aggregate rainfed harvested areas to the GCAM region level (summing over all GTAPcrops), ") %>%
      add_comments("and then to calculate a Yield Index for each rainfed region-GLU. The region-GLU ") %>%
      add_comments("specific index is then multiplied by a base yield to give rainfed bioenergy yields.") %>%
      add_legacy_name("L163.ag_rfdBioYield_GJm2_R_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop") ->
      L163.ag_rfdBioYield_GJm2_R_GLU

    return_data(L163.ag_irrBioYield_GJm2_R_GLU, L163.ag_rfdBioYield_GJm2_R_GLU)
  } else {
    stop("Unknown command")
  }
}
