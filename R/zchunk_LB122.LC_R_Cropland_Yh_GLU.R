#' module_aglu_LB122.LC_R_Cropland_Yh_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.ag_HA_to_CropLand_R_Y_GLU}, \code{L122.ag_EcYield_kgm2_R_C_Y_GLU}, \code{L122.LC_bm2_R_OtherArableLand_Yh_GLU}, \code{L122.LC_bm2_R_ExtraCropLand_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_C_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_Yh_GLU}. The corresponding file in the
#' original data system was \code{LB122.LC_R_Cropland_Yh_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB122.LC_R_Cropland_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L100.FAO_fallowland_kha",
             "L100.FAO_CL_kha",
             "L100.FAO_harv_CL_kha",
             "L103.ag_HA_bm2_R_C_Y_GLU",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L120.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.ag_HA_to_CropLand_R_Y_GLU",
             "L122.ag_EcYield_kgm2_R_C_Y_GLU",
             "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
             "L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             "L122.LC_bm2_R_HarvCropLand_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.FAO_fallowland_kha <- get_data(all_data, "L100.FAO_fallowland_kha")
    L100.FAO_CL_kha <- get_data(all_data, "L100.FAO_CL_kha")
    L100.FAO_harv_CL_kha <- get_data(all_data, "L100.FAO_harv_CL_kha")
    L103.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L103.ag_HA_bm2_R_C_Y_GLU")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")

    # convert all years in above tables to integers, Per Robert's notes on DSR slack:
    L100.FAO_fallowland_kha %>%
      mutate(year = as.integer(year)) ->
      L100.FAO_fallowland_kha

    L100.FAO_CL_kha %>%
      mutate(year = as.integer(year)) ->
      L100.FAO_CL_kha

    L100.FAO_harv_CL_kha %>%
      mutate(year = as.integer(year)) ->
      L100.FAO_harv_CL_kha

    L103.ag_HA_bm2_R_C_Y_GLU %>%
      mutate(year = as.integer(year)) ->
      L103.ag_HA_bm2_R_C_Y_GLU

    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      mutate(year = as.integer(year)) ->
      L103.ag_Prod_Mt_R_C_Y_GLU

    L120.LC_bm2_R_LT_Yh_GLU %>%
      mutate(year = as.integer(year)) ->
      L120.LC_bm2_R_LT_Yh_GLU

    # 2. Perform computations
    # Line 36 in original file
    # take a subset of the land cover table L120.LC_bm2_R_LT_YH_GLU: cropland, and only in aglu historical years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      # filter the Cropland land type only:
      filter(Land_Type == "Cropland") %>%
      # filter for AGLU historical years 1971-2010:
      filter(year %in% AGLU_HISTORICAL_YEARS) ->
      # store in table Land Cover in bm2 by Region, Year and GLU for Cropland.
      L122.LC_bm2_R_CropLand_Y_GLU

    # Lines 40-45 in original file
    # The harvested area/production tables L103.ag.X (from Monfreda) may have R_GLUs not in the cropland table L122.LC_bm2_R_CropLand_Y_GLU (from Hyde),
    # and vice versa.
    # Fill out the cropland table to include all R_GLUs in Monfreda:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # find the region-GLU combos in the Monfreda harvested area table L103.ag_HA_bm2_R_C_Y_GLU NOT contained
      # in the L122.LC_bm2_R_CropLand_Y_GLU land cover table:
      anti_join(L103.ag_HA_bm2_R_C_Y_GLU[,c("GCAM_region_ID", "GLU", "year")], .,
                by = c("GCAM_region_ID", "GLU", "year")) %>%
      # save only the unique combinations:
      unique() %>%
      # arrange so rows occur in more sensible order order:
      arrange(GCAM_region_ID, GLU, year) %>%
      # add values of 0 everywhere for now:
      mutate(value = 0) %>%
      # add a Land_Type identifier:
      mutate(Land_Type = "Cropland") %>%
      # add these rows to the original Land Cover table:
      bind_rows(L122.LC_bm2_R_CropLand_Y_GLU) ->
      # save as the Land C over table:
      L122.LC_bm2_R_CropLand_Y_GLU

    # Line 48 in original file
    # old comment: compile harvested area across all crops
    # Take the input historical harvested area table, L103.ag_HA_bm2_R_C_Y_GLU, and sum over GCAM_commodity, so that each
    # region-GLU-year combo has a single value.
    L103.ag_HA_bm2_R_C_Y_GLU %>%
      #ungroup and remove GCAM_commodity:
      ungroup() %>% select(-GCAM_commodity) %>%
      #group by region-GLU-year
      group_by(GCAM_region_ID, GLU, year) %>%
      # and sum:
      summarise(value = sum(value)) ->
      L122.ag_HA_bm2_R_Y_GLU


    # Lines 51-64 in original file
    # old comment: Calculate the average cropland, fallow land, and land in temporary crops from FAO RESOURCESTAT
    # old comment: The time series is unreliable, so only using the last available year (and applying to all historical years)
    # based on above old comment, get the last available year and set as fallowland_year:
    fallowland_year <- AGLU_HISTORICAL_YEARS[length(AGLU_HISTORICAL_YEARS)]
    # And continue with calculating average crop land, fallow land, land in temporary crops:
    # old comment: compile the ratio of "temporary crops" to total arable land in each region
    # old comment: make table with fallow land compared to total arable land
    # printlog ( "Calculating the average percent of cropland that is fallow in each region" )
    # FAO fallow land data, L100.FAO_fallowland_kha, is appended to the FAO cropland data table. Quantities are aggregated
    # from iso to GCAM region and the fraction of fallowland is calculated.
    #
    # The old data sytem uses match to append FAO fallow land data to FAO cropland data by iso. This was absolutely correct when the
    # code was originally written (2010). However, since then, Ethiopia and Sudan and Belgium-Luxembourg have each split into two
    # distinct countries and FAO produces data for each.
    # Specifically, there are two rows with iso = "eth" and two rows with iso = "sdn" and two rows with iso = "bel" in some FAO data.
    # Because match only pulls the first value when there are multiple potential matches, both "eth" cropland rows get the first
    # "eth" fallowland row, rather than their correct value. The same occurs for the two "sdn" and "bel" rows. The solution is simple:
    # joining by iso and countries (or country.codes) instead of simply iso. Alternatively, aggregating to the iso level in each
    # FAO table (L100.FAO_X) BEFORE doing further joins and calculations should also lead to the correct answer.

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # Take the FAO cropland table, L100.FAO_CL_kha:
      L100.FAO_CL_kha %>%
        # pull off only the fallowland_year data:
        filter(year == fallowland_year) %>%
        # keep only the iso and the value for each; keep year information for easier joining:
        select(iso, countries, value, year) %>%
        # rename value to cropland:
        rename(cropland = value) %>%
        # append in fallow land data in fallowland_year from FAO, L100.FAO_fallowland_kha, keeping NA values as old DS does:
        left_join(L100.FAO_fallowland_kha, by = c("iso","countries", "year")) %>%
        # rename value to fallow:
        rename(fallow = value) %>%
        # MAKING MATCH OLD SYSTEM:
        # Update the Ethiopia, Sudan and Belgium rows so that calculations match the old, incorrect DS:
        mutate(fallow = if_else(iso == "eth", 762.5, if_else(iso == "sdn", 178, if_else(iso == "bel", 10, fallow)))) %>%
        # select only the columns of interest:
        select(iso, cropland, fallow) %>%
        # remove NA values:
        na.omit() %>%
        # add GCAM region information from iso_GCAM_regID
        mutate(GCAM_region_ID = left_join(.,iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
        # remove all columns that are not GCAM_region_ID, cropland, and fallow values:
        select(GCAM_region_ID, cropland, fallow) %>%
        # aggregate cropland and fallow values to the GCAM region level:
        group_by(GCAM_region_ID) %>%
        summarise_all(sum) %>%
        # calculate the fraction of total land in each GCAM region that is fallow:
        mutate(fallow_frac = fallow/cropland) ->
        # store in a table of cropland, fallow information by region:
        L122.cropland_fallow_R
    } else {
      # Take the FAO cropland table, L100.FAO_CL_kha:
      L100.FAO_CL_kha %>%
        # pull off only the fallowland_year data:
        filter(year == fallowland_year) %>%
        # keep only the iso country and the value for each:
        select(iso, countries, value, year) %>%
        # rename value to cropland:
        rename(cropland = value) %>%
        # append in fallow land data in fallowland_year from FAO, L100.FAO_fallowland_kha, keeping NA values:
        left_join(L100.FAO_fallowland_kha, by = c("iso", "countries", "year")) %>%
        # rename value to fallow:
        rename(fallow = value) %>%
        # remove NA values:
        na.omit() %>%
        # add GCAM region information from iso_GCAM_regID
        mutate(GCAM_region_ID = left_join(.,iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
        # remove all columns that are not GCAM_region_ID, cropland, and fallow values:
        select(GCAM_region_ID, cropland, fallow) %>%
        # ungroup:
        ungroup() %>%
        # aggregate cropland and fallow values to the GCAM region level:
        group_by(GCAM_region_ID) %>%
        summarise_all(sum) %>%
        # calculate the fraction of total land in each GCAM region that is fallow:
        mutate(fallow_frac = fallow/cropland) ->
        # store in a table of cropland, fallow information by region:
        L122.cropland_fallow_R
    }

    # Lines 55-74 in original file
    # old comment: make table with cropped land compared to total arable land
    # printlog ( "Calculating the average percent of cropland that is not in active crop rotations in each region" )
    # FAO harvested area data, L100.FAO_harv_CL_kha, is appended to the FAO cropland data table. Quantities are aggregated
    # from iso to GCAM region and the fraction of cropped land is calculated.
    #
    # These lines of code also pull FAO data that is susceptible to the double labeling in Ethiopia, Sudan, and Belgium.
    # The same match error occurs as above, and we fix in the same way:
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # Take the FAO cropland table, L100.FAO_CL_kha:
      L100.FAO_CL_kha %>%
        # pull off only the fallowland_year data:
        filter(year == fallowland_year) %>%
        # keep only the iso country and the value for each:
        select(iso, countries, value, year) %>%
        # rename value to cropland:
        rename(cropland = value) %>%
        # append in cropped land data in fallowland_year from FAO, L100.FAO_harv_CL_kha, keeping NA values:
        left_join(L100.FAO_harv_CL_kha, by = c("iso", "countries", "year")) %>%
        # rename value to cropped:
        rename(cropped = value) %>%
        # MAKING MATCH OLD SYSTEM:
        # Update the Ethiopia, Sudan and Belgium rows so that calculations match the old, incorrect DS:
        mutate(cropped = if_else(iso == "eth", 11772, if_else(iso == "sdn", 4086, if_else(iso == "bel", 747, cropped)))) %>%
        # select only the columns of interest:
        select(iso, cropland, cropped) %>%
        # remove NA values:
        na.omit() %>%
        # add GCAM region information from iso_GCAM_regID
        mutate(GCAM_region_ID = left_join(.,iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
        # remove all columns that are not GCAM_region_ID, cropland, and cropped values:
        select(GCAM_region_ID, cropland, cropped) %>%
        # ungroup:
        ungroup() %>%
        # aggregate cropland and cropped values to the GCAM region level:
        group_by(GCAM_region_ID) %>%
        summarise_all(sum) %>%
        # calculate the fraction of total land in each GCAM region that is cropped:
        mutate(cropped_frac = cropped/cropland) ->
        # store in a table of cropland, fallow information by region:
        L122.cropland_cropped_R
    } else {
      # Take the FAO cropland table, L100.FAO_CL_kha:
      L100.FAO_CL_kha %>%
        # pull off only the fallowland_year data:
        filter(year == fallowland_year) %>%
        # keep only the iso country and the value for each:
        select(iso, countries, value, year) %>%
        # rename value to cropland:
        rename(cropland = value) %>%
        # append in cropped land data in fallowland_year from FAO, L100.FAO_harv_CL_kha, keeping NA values:
        left_join(L100.FAO_harv_CL_kha, by = c("iso", "countries", "year")) %>%
        # rename value to cropped:
        rename(cropped = value) %>%
        # remove NA values:
        na.omit() %>%
        # add GCAM region information from iso_GCAM_regID
        mutate(GCAM_region_ID = left_join(.,iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
        # remove all columns that are not GCAM_region_ID, cropland, and cropped values:
        select(GCAM_region_ID, cropland, cropped) %>%
        # ungroup:
        ungroup() %>%
        # aggregate cropland and cropped values to the GCAM region level:
        group_by(GCAM_region_ID) %>%
        summarise_all(sum) %>%
        # calculate the fraction of total land in each GCAM region that is cropped:
        mutate(cropped_frac = cropped/cropland) ->
        # store in a table of cropland, fallow information by region:
        L122.cropland_cropped_R
    }

    # Lines 76-89 in original file
    # old comment: calculate the average amount of cropland that is not in production as the fallow land fraction, where available, or else the non-cropped cropland
    # printlog ( "NOTE: based on availability, using (1) fallow land fraction, (2) land not in crop rotations, or (3) 0" )





    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.ag_HA_to_CropLand_R_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ag_HA_to_CropLand_R_Y_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.ag_EcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ag_EcYield_kgm2_R_C_Y_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.LC_bm2_R_OtherArableLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.LC_bm2_R_OtherArableLand_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.LC_bm2_R_ExtraCropLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L103.ag_HA_bm2_R_C_Y_GLU",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    return_data(L122.ag_HA_to_CropLand_R_Y_GLU, L122.ag_EcYield_kgm2_R_C_Y_GLU, L122.LC_bm2_R_OtherArableLand_Yh_GLU, L122.LC_bm2_R_ExtraCropLand_Yh_GLU, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, L122.LC_bm2_R_HarvCropLand_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
