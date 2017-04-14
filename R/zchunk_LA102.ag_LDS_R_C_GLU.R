#' module_aglu_LA102.ag_LDS_R_C_GLU
#'
#' Initializes harvested area and production values using LDS data for each GCAM Region-Commodity-GLU combination.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.ag_HA_bm2_R_C_GLU}, \code{L102.ag_Prod_Mt_R_C_GLU}. The corresponding file in the
#' original data system was \code{LA102.ag_LDS_R_C_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA102.ag_LDS_R_C_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.ag_HA_bm2_R_C_GLU",
             "L102.ag_Prod_Mt_R_C_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")


    # Perform Computations:

    # Lines 33-50 in original file
    # Add GCAM_region_ID and GCAM_commodity identifiers, aggregate to the GCAM region, GLU, and commodity level,
    # and convert units for each:
    # harvested area table, L100.LDS_ag_HA_ha
    # agricultural production table, L100.LDS_ag_prod_t
    #
    # printlog( "Adding GCAM region and commodity info to LDS tables" )
    # printlog( "Collapsing production and harvested area data to GCAM regions and commodities")
    # printlog( "Converting GTAP mass to Mt and area to thousand km2 (billion m2, or bm2)")

    # Take the harvested area table, L100.LDS_ag_HA_ha
    L100.LDS_ag_HA_ha %>%
      # append GCAM region information from the iso table:
      left_join_error_no_match(iso_GCAM_regID, by=c("iso")) %>%
      # remove the country_name and GCAM3 columns added from the iso table:
      select(-country_name, -region_GCAM3) %>%
      # append GCAM_commodity information from the FAO table using a left_join to preserve NA values as in old system:
      left_join(., FAO_ag_items_PRODSTAT, by = c("GTAP_crop")) %>%
      # select only the relevant columns; iso and GTAP_crop are dropped because they are ommitted from the aggregation:
      ungroup () %>% select(GCAM_region_ID, GCAM_commodity,GLU, value ) %>%
      # group by GCAM region, GCAM commodity, and GLU for the aggregation:
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%
      # aggregate to the GCAM region, commodity, GLU level:
      summarise_if(is.numeric, sum) %>%
      # convert units from hectares (ha) to thou km^2 (=billion m^2, bm2):
      mutate(value=value*CONV_HA_BM2)  %>%
      # omit na values, since they do not appear in the original table:
      na.omit()->
      # store in the final, labeled table for harvested area (HA) in units bm2 at the region-commodity-glu level:
      L102.ag_HA_bm2_R_C_GLU

    # Take the production table, L100.LDS_ag_HA_ha
    L100.LDS_ag_prod_t %>%
      # append GCAM region information from the iso table:
      left_join_error_no_match(iso_GCAM_regID, by=c("iso")) %>%
      # remove the country_name and GCAM3 columns added from the iso table:
      select(-country_name, -region_GCAM3) %>%
      # append GCAM_commodity information from the FAO table using a left_join to preserve NA values as in old system:
      left_join(., FAO_ag_items_PRODSTAT, by = c("GTAP_crop")) %>%
      # select only the relevant columns; iso and GTAP_crop are dropped because they are ommitted from the aggregation:
      ungroup () %>% select(GCAM_region_ID, GCAM_commodity,GLU, value ) %>%
      # group by GCAM region, GCAM commodity, and GLU for the aggregation:
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%
      # aggregate to the GCAM region, commodity, GLU level:
      summarise_if(is.numeric, sum) %>%
      # convert units from ton (t) to Megatons (Mt):
      mutate(value=value*CONV_TON_MEGATON) %>%
      # omit na values, since they do not appear in the original table:
      na.omit() ->
      # store in the final, labeled table for production in units Mt at the region-commodity-glu level:
      L102.ag_Prod_Mt_R_C_GLU


    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L102.ag_HA_bm2_R_C_GLU %>%
      add_title("Harvested area by GCAM region / commodity / GLU") %>%
      add_units("Thousand km^2 = billion m^2 (bm2") %>%
      add_comments("LDS harvested area data, sourced from SAGE, Hyde, and others,") %>%
      add_comments("is read in at the iso-GLU-GTAP_crop level, and converted to GCAM") %>%
      add_comments("regions, commodities, and units.") %>%
      add_legacy_name("L102.ag_HA_bm2_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t") ->
      L102.ag_HA_bm2_R_C_GLU
    L102.ag_Prod_Mt_R_C_GLU %>%
      add_title("Crop production by GCAM region / commodity / GLU") %>%
      add_units("Megatons (Mt)") %>%
      add_comments("LDS production data, sourced from SAGE, Hyde, and others,") %>%
      add_comments("is read in at the iso-GLU-GTAP_crop level, and converted to GCAM") %>%
      add_comments("regions, commodities, and units.") %>%
      add_legacy_name("L102.ag_Prod_Mt_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t") ->
      L102.ag_Prod_Mt_R_C_GLU

    return_data(L102.ag_HA_bm2_R_C_GLU, L102.ag_Prod_Mt_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
