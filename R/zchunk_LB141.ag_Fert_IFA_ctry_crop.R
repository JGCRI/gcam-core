#' module_aglu_LB141.ag_Fert_IFA_ctry_crop
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.ag_Fert_Cons_MtN_ctry_crop}. The corresponding file in the
#' original data system was \code{LB141.ag_Fert_IFA_ctry_crop.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB141.ag_Fert_IFA_ctry_crop <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             "L100.LDS_ag_HA_ha",
             "L101.ag_HA_bm2_R_C_Y",
             "L102.ag_HA_bm2_R_C_GLU",
             FILE = "aglu/IFA2002_Fert_ktN",
             FILE = "aglu/IFA_Fert_ktN"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L101.ag_HA_bm2_R_C_Y <- get_data(all_data, "L101.ag_HA_bm2_R_C_Y")
    L102.ag_HA_bm2_R_C_GLU <- get_data(all_data, "L102.ag_HA_bm2_R_C_GLU")
    IFA2002_Fert_ktN <- get_data(all_data, "aglu/IFA2002_Fert_ktN")
    IFA_Fert_ktN <- get_data(all_data, "aglu/IFA_Fert_ktN")


    # Perform Calculations

    # Lines 37-60 in original file
    # Bring together disparate LDS and FAO harvested area data in order to calculate an FAO_LDS scaler for
    # each region-commodity combination.
    # This region-commodity level scaler is used to scale the LDS harvested area data given at the
    # isoCountry-GLU-GTAPcrop level. This scaled harvested area is then multiplied by Fert application rates
    # and summed to calculate fertilizer demands at the iso-GTAPcrop level.
    #
    # old comment: printlog( "Briefly reconciling Monfreda/LDS and FAO datasets for the 1998-2002 time period" )
    #              This part of the methods is sensitive to any discrepancies between the two data sources, as the Monfreda/LDS
    #              based data are used to estimate fertilizer by crop and country, and then the actual fertilizer IO coefs are derived
    #              using FAO Prodstat data. For most crops this is OK, but some differ by 10-50 and can cause fertilizer costs well
    #              in excess of crop production revenues.
    #              printlog( "Using harvested area totals by GCAM region and commodity as the basis for scaling back LDS harvested area by country, GLU, and GTAP crop" )
    #
    #
    # First, calculate the FAO_LDS scaler for each region-commodity combination
    # Take the FAO Harvested area data in table L101.ag_HA_bm2_R_C_Y and get the average value for each
    # region-commodity combination over the LDS years = 1998-2002 default.
    # Then Take the LDS harvested area data, L102.ag_HA_bm2_R_C_GLU, sum to the region-commodity level,
    # join it to the averaged FAO data and compute an FAO_LDS scaler as FAO/LDS:
    #
    # Take FAO HA data and calulate the average value over FAO_LDS_years for each region-commodity combo:
    L101.ag_HA_bm2_R_C_Y %>%
      filter(year %in% FAO_LDS_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(FAO = mean(value)) %>% ungroup() ->
      L141.FAO
    # Take LDS HA data and aggregate to the GCAM region-commodity level:
    L102.ag_HA_bm2_R_C_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(LDS = sum(value)) %>% ungroup() %>%
      # Join in the FAO data from the previous pipeline
      left_join_error_no_match(L141.FAO, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      # Calculate the FAO_LDS scaler value = FAO/LDS
      mutate(scaler = FAO/LDS) ->
      # store in an FAO_LDS table:
      L141.FAO_LDS

    # Second, use the region-commodity scalers to sclae LDS harvested area data at the iso-GLU-GTAPcrop level.
    # old comment: Match the GCAM region and commodity class into the LDS dataset, and multiply by the scaler above
    #
    # Take LDS harvested area data at the iso-GLU-GTAPcrop level, L100.LDS_ag_HA_ha, join in the GCAM_region_ID info for
    # each iso, join in the GCAM_commodity info for each GTAPcrop, join the FAO_LDS scaler calculated in the previous
    # pipeline, and use the FAO_LDS scaler to scale the iso-GLU-GTAPcrop harvested area data
    L100.LDS_ag_HA_ha %>%
      # join GCAM region ID information
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = c("iso")) %>%
      # join GCAM commodity information and preserve NAs to be omitted, as in original code
      left_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = c("GTAP_crop")) %>%
      na.omit() %>%
      # join FAO_LDS scaler for each region-commodity
      left_join_error_no_match(select(L141.FAO_LDS, GCAM_region_ID, GCAM_commodity, scaler),
                               by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      # use the region-commodity scaler to scale down each iso-GLU-GTAP harvested area
      mutate(value = value * scaler) %>%
      # drop unwanted columns
      select(-GCAM_region_ID, -GCAM_commodity, -scaler) ->
      # store in an LDS harvested area table
      L141.LDS_ag_HA_ha


    #





    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L141.ag_Fert_Cons_MtN_ctry_crop") %>%
      add_precursors("aglu/FAO_ag_items_PRODSTAT",
                     "common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "L100.LDS_ag_HA_ha",
                     "L101.ag_HA_bm2_R_C_Y",
                     "L102.ag_HA_bm2_R_C_GLU",
                     "aglu/IFA2002_Fert_ktN",
                     "aglu/IFA_Fert_ktN") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L141.ag_Fert_Cons_MtN_ctry_crop

    return_data(L141.ag_Fert_Cons_MtN_ctry_crop)
  } else {
    stop("Unknown command")
  }
}
