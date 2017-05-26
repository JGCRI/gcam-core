#' module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L162.ag_YieldRatio_R_C_Ysy_GLU_irr}, \code{L162.ag_YieldRate_R_C_Y_GLU_irr}, \code{L162.bio_YieldRate_R_Y_GLU_irr}. The corresponding file in the
#' original data system was \code{LB162.ag_prodchange_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/A_defaultYieldRate",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO_ag_CROSIT",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L162.ag_YieldRatio_R_C_Ysy_GLU_irr",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_defaultYieldRate <- get_data(all_data, "aglu/A_defaultYieldRate")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO_ag_CROSIT")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU")


    # Perform calculations

    # Prepare CROSIT database by replacing country and crop IDs with names from
    # AGLU_ctry and FAO_ag_items_PRODSTAT respectively. Because these are larger tables
    # with data for multiple uses, a call to dplyr::distinct is used to reduce to only
    # CROSIT-relevant countries and crops. This allows for left_join_error_no_match to
    # be used.
    # Some regions have 0 production but positive harvested area and a non zero yield -
    # use the yields to recalculate production = yield * harvested area when yields are
    # available; when yields are not available, reset harvested area to 0.
    FAO_ag_CROSIT %>%
      left_join_error_no_match(dplyr::distinct(select(AGLU_ctry, CROSIT_country_ID, CROSIT_ctry)),
                               by = c("country_ID" = "CROSIT_country_ID")) %>%
      left_join_error_no_match(dplyr::distinct(select(FAO_ag_items_PRODSTAT, CROSIT_crop, CROSIT_cropID)),
                               by = c("crop_ID" = "CROSIT_cropID")) %>%
      # Process 0 production / nonzero HA and yield cases
      group_by(country_ID, crop_ID, year) %>%
      mutate(Prod_kt_irrigated = replace(Prod_kt_irrigated,
                                         Prod_kt_irrigated == 0 & HA_kha_irrigated != 0,
                                         Yield_kgHa_irrigated * HA_kha_irrigated * CONV_KG_T)) %>%
      mutate(Prod_kt_rainfed = replace(Prod_kt_rainfed,
                                         Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                         Yield_kgHa_rainfed * HA_kha_rainfed * CONV_KG_T)) %>%
      mutate(HA_kha_irrigated = replace(HA_kha_irrigated,
                                        Prod_kt_irrigated == 0 & HA_kha_irrigated != 0,
                                        0)) %>%
      mutate(HA_kha_rainfed = replace(HA_kha_rainfed,
                                      Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                        0)) %>%
      ungroup() ->
      FAO_ag_CROSIT


    # Use the CROSIT database to prepare a table of yields by country, crop, irrigation, and year,
    # and interpolate to fill in all of the specified agricultural production years, SPEC_AG_PROD_YEARS.
    # Finally, Calculate yield multipliers from the base year <=> first year of SPEC_AG_PROD_YEARS.
    # For each country, crop, irrigation, the multiplier for each year is
    # yield in that year / yield in base year.

    # pull off irrigated table of yield, country, crop; append irrigation information
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_irrigated) %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(yield_kgHa = Yield_kgHa_irrigated) ->
      L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y

    # Do same for rainfed and bind the irrigated table.
    # Then complete missing agricultural years from SPEC_AG_PROD_YEARS and interpolate
    # to fill in yields. Keep only the years in SPEC_AG_PROD_YEARS.
    # Finally, Calculate yield multipliers.
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_rainfed) %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(yield_kgHa = Yield_kgHa_rainfed) %>%
      bind_rows(L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y ) %>%
      # add the missing SPEC_AG_PROD_YEARS and interpolate the yields
      tidyr::complete(year = c(year, SPEC_AG_PROD_YEARS) ,
                      CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      select(CROSIT_ctry, CROSIT_crop, Irr_Rfd, year, yield_kgHa) %>%
      arrange(year) %>%
      group_by(CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      mutate(yield_kgHa = approx_fun(year, yield_kgHa)) %>%
      ungroup() %>% filter(year %in% SPEC_AG_PROD_YEARS) %>%
      # calculate yield multipliers
      group_by(CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      mutate(Mult = yield_kgHa / first(yield_kgHa)) %>%
      ungroup() %>%
      select(-yield_kgHa) %>%
      # Drop the NaN's = crops with zero base year production / harvested area
      na.omit() ->
      L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr


    # We apply the above yield multipliers to crop-specific changes in harvested area.
    # This removes bias from changes in composition of GCAM commodities in the FAO projections.
    # These yield multipliers are now ready to be matched into the GTAP/LDS-based table of
    # country x crop x zone harvested area in the base year, L151.ag_irrHA/rfdHA...
    #
    # First, merge the separate rainfed and irrigated harvested area datasets to simplify processing.
    # Then join CROSIT country and crop identifiers and aggregate the LDS area to CROSIT country
    # and crop levels.
    # CROSIT_ctr identifiers come from the AGLU_ctry table, which needs preprocessing to avoid unwanted
    # extra rows.

    # deal with unfilled in Yugoslavia entries in AGLU_ctry without impacting other code chunks.
    AGLU_ctry %>%
      select(iso, CROSIT_ctry) %>%
      filter(!is.na(CROSIT_ctry)) %>%
      dplyr::distinct() ->
      AGLU_ctry_iso_CROSIT

    L151.ag_irrHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(HA = irrHA) ->
      L151.ag_irrHA_ha_ctry_crop
    L151.ag_rfdHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(HA = rfdHA) %>%
      bind_rows(L151.ag_irrHA_ha_ctry_crop) %>%
      # Join CROSIT country and crop information and aggregate
      # keeping NA's for later processing
      left_join(dplyr::distinct(select(AGLU_ctry, iso, CROSIT_ctry)), by = "iso") %>%
      left_join(dplyr::distinct(select(FAO_ag_items_PRODSTAT, CROSIT_crop, GTAP_crop)),
                               by = "GTAP_crop") %>%
      group_by(CROSIT_ctry, CROSIT_crop, GLU, Irr_Rfd) %>%
      summarise(HA = sum(HA)) %>%
      na.omit() %>%
      ungroup() ->
      L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr








    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.ag_YieldRatio_R_C_Ysy_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.ag_YieldRatio_R_C_Ysy_GLU_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.ag_YieldRate_R_C_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.ag_YieldRate_R_C_Y_GLU_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.bio_YieldRate_R_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.bio_YieldRate_R_Y_GLU_irr

    return_data(L162.ag_YieldRatio_R_C_Ysy_GLU_irr, L162.ag_YieldRate_R_C_Y_GLU_irr, L162.bio_YieldRate_R_Y_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
