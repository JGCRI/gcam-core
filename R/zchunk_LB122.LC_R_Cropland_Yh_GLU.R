#' module_aglu_LB122.LC_R_Cropland_Yh_GLU
#'
#' This chunk integrates disparate data sources in order to calculate OtherArableLand
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
      mutate(GCAM_region_ID = left_join(., iso_GCAM_regID, by = "iso")$GCAM_region_ID) %>%
      # remove all columns that are not GCAM_region_ID, cropland, and fallow values:
      select(GCAM_region_ID, cropland, fallow) %>%
      # ungroup:
      ungroup() %>%
      # aggregate cropland and fallow values to the GCAM region level:
      group_by(GCAM_region_ID) %>%
      summarise_all(sum) %>%
      # calculate the fraction of total land in each GCAM region that is fallow:
      mutate(fallow_frac = fallow / cropland) ->
      # store in a table of cropland, fallow information by region:
      L122.cropland_fallow_R

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L122.cropland_fallow_R %>%
        # Update GCAM_region 2 (Ethiopia, Sudan) and 12 (Belgium) rows so that calculations match the old, incorrect DS:
        mutate(fallow = if_else(GCAM_region_ID == 2, 1881.7000, if_else(GCAM_region_ID == 13, 5060.2225, fallow))) %>%
        mutate(fallow_frac = if_else(GCAM_region_ID == 2, 0.057756913, if_else(GCAM_region_ID == 13, 0.072119754, fallow_frac)))->
        L122.cropland_fallow_R
    } else {
      # do nothing, the above code is correct
    }


    # Lines 55-74 in original file
    # old comment: make table with cropped land compared to total arable land
    # printlog ( "Calculating the average percent of cropland that is not in active crop rotations in each region" )
    # FAO harvested area data, L100.FAO_harv_CL_kha, is appended to the FAO cropland data table. Quantities are aggregated
    # from iso to GCAM region and the fraction of cropped land is calculated.
    #
    # These lines of code also pull FAO data that is susceptible to the double labeling in Ethiopia, Sudan, and Belgium.
    # The same match error occurs as above, and we fix in the same way:

    # Take the FAO cropland table, L100.FAO_CL_kha:
    L100.FAO_CL_kha %>%
      # pull off only the fallowland_year data:
      filter(year == fallowland_year) %>%
      # keep only the iso country and the value for each:
      select(iso, countries, value, year) %>%
      # rename value to cropland:
      rename(cropland = value) %>%
      # append in cropped land data in fallowland_year from FAO, L100.FAO_harv_CL_kha, keeping NA values:
      left_join(L100.FAO_harv_CL_kha[L100.FAO_harv_CL_kha$year == fallowland_year, ],
                by = c("iso", "countries", "year")) %>%
      # rename value to cropped:
      rename(cropped = value) %>%
      # remove NA values:
      na.omit() %>%
      # add GCAM region information from iso_GCAM_regID
      mutate(GCAM_region_ID = left_join(., iso_GCAM_regID, by = "iso")$GCAM_region_ID) %>%
      # remove all columns that are not GCAM_region_ID, cropland, and cropped values:
      select(GCAM_region_ID, cropland, cropped) %>%
      # ungroup:
      ungroup() %>%
      # aggregate cropland and cropped values to the GCAM region level:
      group_by(GCAM_region_ID) %>%
      summarise_all(sum) %>%
      # calculate the fraction of total land in each GCAM region that is cropped:
      mutate(cropped_frac = cropped/cropland) ->
      # store in a table of cropland, cropped information by region:
      L122.cropland_cropped_R

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L122.cropland_cropped_R %>%
        # Update GCAM_region 2 (Ethiopia, Sudan) and 12 (Belgium) rows so that calculations match the old, incorrect DS:
        mutate(cropped = if_else(GCAM_region_ID == 2, 31746.925, if_else(GCAM_region_ID == 13, 42484.390, cropped))) %>%
        mutate(cropped_frac = if_else(GCAM_region_ID == 2, 0.8347940, if_else(GCAM_region_ID == 13, 0.6195761, cropped_frac))) ->
        L122.cropland_cropped_R
    } else {
      # do nothing, the above code is correct
    }


    # Lines 76-89 in original file
    # old comment: calculate the average amount of cropland that is not in production as the fallow land fraction, where available, or else the non-cropped cropland
    # printlog ( "NOTE: based on availability, using (1) fallow land fraction, (2) land not in crop rotations, or (3) 0" )
    # Calculating cropland not in production is done via a series of nested if statements.

    # take the unique GCAM regions
    tibble::tibble(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID)) %>%
      # sort the ids:
      arrange(GCAM_region_ID) %>%
      # add a land type identifier for each region:
      mutate(Land_Type = "Cropland") %>%
      # join in fallow land table, L122.cropland_fallow_R, to get the fallow_frac for each region,
      # maintaining NA's for use in determining data availability to calculate nonharvested_frac:
      mutate(fallow_frac = left_join(., L122.cropland_fallow_R, by = "GCAM_region_ID")$fallow_frac) %>%
      # join in cropped land table, L122.cropland_cropped_R, to get the cropped_frac for each region, and
      # use to calculate the uncropped fraction, maintaining NA's to set to 0 later:
      mutate(uncropped_frac = 1 - left_join(., L122.cropland_cropped_R, by = "GCAM_region_ID")$cropped_frac) %>%
      # calculate the nonharvested fraction of land via nested if statements
      #   based on availability, using (1) fallow land fraction, (2) land not in crop rotations, or (3) 0
      #   This step determines (1) versus (2):
      #                                  If there is no available fallow land data
      mutate(nonharvested_frac = if_else(is.na(fallow_frac),
                                         #                                  use the uncropped fraction
                                         uncropped_frac,
                                         #                                  otherwise, use the available fallow land data:
                                         fallow_frac)) %>%
      #   This step determines (3) when (1) and (2) are not available:
      mutate(nonharvested_frac = if_else(is.na(nonharvested_frac), 0, nonharvested_frac)) ->
      # store in a table of nonharvested cropland fractions by region:
      L122.nonharvested_cropland_R


    # Lines 91-99 in original file
    # old comment: make a table with fallow land, and the remaining cropland that is "available" for harvest, by region, year, and GLU
    # printlog( "NOTE: applying regional average fallow fractions to all GLUs within each region" )
    # Actually making two tables: the fallow land table and then, using the fallow land table, the available cropland table

    # take cropland table with region, landtype and glu information for each year:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # join in the non harvested fraction for each region:
      #   Note that there is no information for GCAM region 30 in L122.LC_bm2_R_CropLand_Y_GLU, even though
      #   a nonharvested fraction (of 0) is calculated in L122.nonharvested_cropland_R.
      left_join(L122.nonharvested_cropland_R, by = c("GCAM_region_ID", "Land_Type")) %>%
      # drop unnecessary columns that come from the join:
      select(-fallow_frac, -uncropped_frac) %>%
      # use the nonharvested_frac and the cropland area information (value) for each Region-GLU-Year to calculate
      # the amouunt of fallowland in that Region-GLU-Year = value * nonharvested_frac
      mutate(value = value * nonharvested_frac) %>%
      # drop nonharvested_frac since no longer necessary:
      select(-nonharvested_frac) %>%
      # update the Land_Type identifier to reflect that this is now FallowLand, not Cropland
      mutate(Land_Type = "FallowLand") ->
      # store in a table of fallowland land cover in bm2 for each region-glu-year
      L122.LC_bm2_R_FallowLand_Y_GLU

    # Use the just made fallow land table to calculate a table of Available CropLand in each region-glu-year
    # Available cropland = totalcropland from L122.LC_bm2_R_CropLand_Y_GLU - fallowland from L122.LC_bm2_R_FallowLand_Y_GLU
    # Take the table of total cropland in each region-GLU-year:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # join in the land that is fallow in each region-glu-year
      # absolutely should match up and it's a problem at this point if they don't, so left_join_error_no_match:
      left_join_error_no_match(L122.LC_bm2_R_FallowLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = value from Cropland table, the total amount of cropland in the region-glu-year
      # value.y = value from FallowLand table, the amount of fallowland in the region-glu-year
      # therefore, available cropland value = value.x-value.y:
      mutate(value = value.x-value.y) %>%
      # remove value.x, value.y and Land_Type.y as now unnecessary:
      select(-value.x, -value.y, -Land_Type.y) %>%
      # rename Land_Type.x to Land_Type:
      rename(Land_Type = Land_Type.x) ->
      # store in a table of available cropland:
      L122.LC_bm2_R_AvailableCropLand_Y_GLU


    # Lines 101-114 in original file
    # old comment: Calculate the harvested to cropped land ratio for all crops, by region, year, and GLU
    # printlog( "NOTE: applying minimum and maximum harvested:cropped ratios" )
    # old comment: Maximum harvested:cropped ratio may cause cropland to expand
    # old comment: This additional cropland will need to be balanced by a deduction from other land types later on, so it is tracked below
    # take harvested area by region-glu-year:
    L122.ag_HA_bm2_R_Y_GLU %>%
      # join the available cropland by region-glu-year
      left_join_error_no_match(L122.LC_bm2_R_AvailableCropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = value from L122.ag_HA_bm2_R_Y_GLU, the harvested area
      # value.y = value from L122.LC_bm2_R_AvailableCropLand_Y_GLU, the available cropland
      # harvested area to available cropland ratio is value.x/value.y
      mutate(value = value.x / value.y) %>%
      # remove unneeded columns:
      select(-value.x, -value.y, -Land_Type) %>%
      # if the harvested to cropland ratio, value, is less than the min acceptable harvested area to cropland,
      # MIN_HA_TO_CROPLAND, replace with MIN_HA_TO_CROPLAND
      mutate(value = if_else(value < MIN_HA_TO_CROPLAND, MIN_HA_TO_CROPLAND, value)) %>%
      # if the harvested to cropland ratio is greater than the max acceptable harvested area to cropland,
      # MAX_HA_TO_CROPLAND, replace with MAX_HA_TO_CROPLAND
      mutate(value = if_else(value > MAX_HA_TO_CROPLAND, MAX_HA_TO_CROPLAND, value)) ->
      # store in a table of HA to cropland ratios by region-glu-year
      L122.ag_HA_to_CropLand_R_Y_GLU

    ### double check wording of old comments in above section with Page; in general, make sure understanding what all
    ### tables actually mean so comments accurate
    ### also, find out about printlogs for sure

    # Lines 116-126 in original file
    # old comment: The ag_HA_to_CropLand ratio is assumed to be a property of the region and GLU ( not individual crops ), as individual sites are planted with different crops
    # old comment: Calculate cropland requirements of each crop as harvested area divided by regional ag_HA_to_CropLand ratio
    # printlog( "Calculating cropland requirements of each crop as harvested area divided by HA:CL" )
    # old comment: Calculate land cover as harvested area divided by HA:CL
    # Take input harvested area by region-commodity-glu-year:
    L103.ag_HA_bm2_R_C_Y_GLU %>%
      # join the harvested area to cropland ratios, L122.ag_HA_to_CropLand_R_Y_GLU:
      left_join_error_no_match(L122.ag_HA_to_CropLand_R_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = original harvested area info for each GCAM_commodity from L103.ag_HA_bm2_R_C_GLU
      # value.y = HA:CL ratio from L122.ag_HA_to_CropLand_R_Y_GLU just joined
      # calcualte the harvested cropland for each commodity as value = value.x/value.y:
      mutate(value = value.x/value.y) %>%
      # removed unnecessary columns:
      select(-value.x, -value.y) %>%
      # add a Land_Type identifier:
      mutate(Land_Type = "HarvCropLand") ->
      # store in a table of Harvested Cropland by region-commodity-glu-year:
      L122.LC_bm2_R_HarvCropLand_C_Y_GLU

    # Lines 125:128 in original file
    # old comment: aggregate across crops to get harvested cropland area, by region/GLU/year
    # The harvested cropland area by region-commodity-glu-year table, L122.Lc_bm2_R_HarvCropLand_C_Y_GLU,
    # is used for other calculations below. That is why this is not part of the previous pipeline
    # Take harvested cropland area by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      # remove commodity info since that is what aggregate over:
      select(-GCAM_commodity) %>%
      # group by region-glu-landtype-year:
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      # aggregate:
      summarise(value = sum(value)) ->
      # store in a table of Harvested cropland by region-glu-year:
      L122.LC_bm2_R_HarvCropLand_Y_GLU



    # Lines 130-136 in original file
    # old comment: Calculate economic yield by each crop as production divided by cropland. Write out this preliminary table.
    # Production by region-glu-commodity-year comes from input data L103.ag_Prod_Mt_R_C_Y_GLU.
    # Cropland area by region-glu-commodity-year was calculated as table L122.LC_bm2_R_HarvCropLand_C_Y_GLU
    # Take cropland by region-glu-commodity-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      # join the production by region-glu-commodity-year information:
      left_join_error_no_match(L103.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      # value.x = the cropland area by region-commodity-glu-year from L122.LC_bm2_R_HarvCropLand_C_Y_GLU
      # value.y = production by region-commodity-glu-year from L103.ag_Prod_Mt_R_C_Y_GLU
      # Calculate yield by region-commodity-glu-year as value = value.y/value.x:
      mutate(value = value.y / value.x) %>%
      # remove unnecessary columns:
      select(-value.x, -value.y) %>%
      # set any NA values to 0 yield:
      mutate(value = if_else(is.na(value), 0, value)) %>%
      # remove now-unneeded land type data:
      select(-Land_Type) ->
      # store in a table of Economic Yield by region-commodity-glu-year:
      L122.ag_EcYield_kgm2_R_C_Y_GLU


    # Calculating OtherArableLand:

    # Lines 138:148 in original file
    # old comment: The minimum threshold on HA:CL means that some cropland in Hyde will not be assigned to a crop. This is mapped to "other arable land"
    # old comment: The maximum threshold on HA:CL means that cropland in Hyde in some ag regions may be less than cropland in GCAM
    # old comment: In the latter case, land needs to be mapped to cropland from other uses.
    # The first step in executing the three above lines of comment is:
    # old comment: Calculate the residual land cover that is cropland (may be positive or negative)
    # Take harvested cropland by region-glu-year:
    L122.LC_bm2_R_HarvCropLand_Y_GLU %>%
      # update the landtype to be residual cropland
      ungroup() %>% mutate(Land_Type = "ResidualCropLand") %>%
      # Join in the available cropland by region-glu-year
      left_join_error_no_match(L122.LC_bm2_R_AvailableCropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = region-glu-year harvested cropland from L122.LC_bm2_R_HarvCropLand_Y_GLU
      # value.y = region-glu-year available cropland from L122.LC_bm2_R_AvailableCropLand_Y_GLU
      # Calculate residual cropland as harvested-available, value = value.x-value.y:
      mutate(value = value.x - value.y) %>%
      # remove unnecessary columns:
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) ->
      # store in a table of Residual Cropland by region-glu-year
      L122.LC_bm2_R_ResidualCropLand_Y_GLU

    # second step to execute max/min comments above is to process the residuals
    # Lines 150-161 in original file
    # Where residuals are negative, this is "unused" cropland that will be mapped to other arable land
    # printlog( "Calculating unused cropland; this is added with fallow land to calculate other arable land" )
    # Take the residual cropland by region-glu-year:
    L122.LC_bm2_R_ResidualCropLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>% mutate(Land_Type = "UnusedCropLand") %>%
      # positive residuals cannot be remapped to OtherArableLand. Therefore, they do not count as UnusedCropLand
      # and region-glu-years with positive residual have 0 UnusedCropLand:
      mutate(value = if_else(value > 0, 0, value)) %>%
      # negative residuals ARE UnusedCropLand. -1 * the negative residual is the amount of UnusuedCropLand that
      # can be mapped to OtherArable Land. Calculate UnusedCropLand:
      mutate(value = -1 * value) ->
      # store in a table of UnusedCropLand by region-glu-year
      L122.LC_bm2_R_UnusedCropLand_Y_GLU

    # Where residuals are positive, this is "extra" land that will later be deducted from other categories.
    # printlog( "Calculating extra cropland; this will be balanced by a deduction from unmanaged lands" )
    # Take the residual cropland by region-glu-year:
    L122.LC_bm2_R_ResidualCropLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>% mutate(Land_Type = "ExtraCropLand") %>%
      # negative residuals do not count as ExtraCropLand. Therefore, any negative values in value are
      # remapped to 0 to represent 0 ExtraCropLand:
      mutate(value = if_else(value < 0, 0, value)) ->
      # store in a table of ExtraCropLand by region-glu-year:
      L122.LC_bm2_R_ExtraCropLand_Y_GLU

    # Finally, calculate OtherArableLand
    # Lines 163-178 in original file
    # printlog( "Calculating other arable land: known fallow plus discrepancy between sum of harvested area and cropland" )
    # printlog( "Assigning cropland to other arable land wherever harvested area is zero" )
    # If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
    # values in the above table, and all of this cropland should be assigned to other arable land.
    #
    # Take FallowLand by region-GLU-year:
    L122.LC_bm2_R_FallowLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>% mutate(Land_Type = "OtherArableLand") %>%
      # join the table of unused cropland by region-glu-year, L122.LC_bm2_UnusedCropLand_Y_GLU
      # NAs retained as in old DS:
      left_join(L122.LC_bm2_R_UnusedCropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = fallow land in region-glu-year from L122.LC_bm2_R_FallowLand_Y_GLU
      # value.y = unused land in region-glu-year from L122.LC_bm2_R_FallowLand_Y_GLU
      # OtherArableLand = fallow + unused <=> value = value.x + value.y:
      mutate(value = value.x + value.y) %>%
      # remove unnecessary columns:
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) %>%
      # join the cropland info so that can handle NA's
      left_join(L122.LC_bm2_R_CropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = OtherArableLand value
      # value.y = Cropland value
      # printlog( "Assigning cropland to other arable land wherever harvested area is zero" )
      # old comment: If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
      # old comment: values in the above table, and all of this cropland should be assigned to other arable land.
      # for NA values in OtherArableLand (value.x), replace with the cropland value (value.y):
      mutate(value = if_else(is.na(value.x), value.y, value.x)) %>%
      # remove unnecessary columns
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) ->
      # store in a table of OtherArableLand by region-glu-year:
      L122.LC_bm2_R_OtherArableLand_Y_GLU


    # Land use history
    # Lines 181 - 200 in original file
    # printlog( "Cropland quantities prior to the first AGLU historical year, for spinup of simple carbon cycle model" )
    # printlog( "NOTE: Simply assigning this to other arable land" )
    # old comment: This method differs from what we had done in the past, where 1971 land cover quantities were rolled back on the basis of the cropland ratios
    # old comment: between each land history year and 1971. The problem with this method is that zero values in 1971 can not match non-zero values in prior years.
    # old comment: The current method instead will have apparent land use change in going from the land history years to the model base years, but due to equal
    # old comment: soil carbon contents, and similar/small vegetative carbon contents, the net emissions signal should be negligible.
    # old comment: First, make a table with cropland in the pre-aglu years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      # only save the years in pre-AGLU years:
      filter(year %in% PREAGLU_YEARS) %>%
      # insure that there is cropland for each GCAM region-glu that appear L122.LC_bm2_R_CropLand_Y_GLU:
      tidyr::complete(Land_Type = unique(L122.LC_bm2_R_CropLand_Y_GLU$Land_Type),
                      tidyr::nesting(GCAM_region_ID, GLU, year), fill = list(value = NA)) %>%
      unique() %>%
      # join this historical cropland information to the region-glu-landtypes of L122.LC_bm2_R_CropLand_Y_GLU, preserving
      # NAs as in old DS:
      left_join(unique(L122.LC_bm2_R_CropLand_Y_GLU[, colnames(L122.LC_bm2_R_CropLand_Y_GLU)
                                                    %in% c("GCAM_region_ID", "GLU", "Land_Type")]), .,
                by = c("GCAM_region_ID", "GLU", "Land_Type") ) %>%
      # missing values are overwritten to 0:
      mutate(value = if_else(is.na(value), 0, value)) %>%
      # bind to the table of OtherArableLand information by region-glu-year:
      bind_rows(L122.LC_bm2_R_OtherArableLand_Y_GLU, .) %>%
      # update the landtype identifier
      ungroup() %>% mutate(Land_Type = "OtherArableLand") ->
      # store in a table of OtherArableLand by region-glu-year, including historical years:
      L122.LC_bm2_R_OtherArableLand_Yh_GLU


    # Lines 202-205 in original file
    # printlog( "All other cropland uses are zero in the pre-aglu years" )
    # Specifically, ExtraCropLand is expanded to include historical years in each region-glu, with a value of 0.
    # Take ExtraCropLand by region-glu-year:
    L122.LC_bm2_R_ExtraCropLand_Y_GLU %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(PREAGLU_YEARS, AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of ExtraCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU


    # Lines 207-210 in original file
    # old comment: Harvested cropland history
    # HarvCropLand is expanded to include historical years for each region-glu-commodity, with a vlaue of 0.
    # Take HarvCropLand by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(PREAGLU_YEARS, AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GCAM_commodity, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of HarvCropland by region-commodity-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    # Lines 212-217 in original file
    # old comment: Combine crop types to get land use history by all harvested cropland.
    # Use the table of HarvCropland by region-commodity-glu-year, including historical years, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU,
    # and aggregate across commodity to arrive at a table of All Harvested Cropland by region-glu-year, including historical years.
    # Take HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      # remove commodity because that is what aggregate over:
      select(-GCAM_commodity) %>%
      # group by region-glu-year to aggregate:
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      # aggregate:
      summarise(value = sum(value)) ->
      # store in a table of all HarvCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # remove the now-unneeded Land_Type identifier from HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      select(-Land_Type) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU


    # Force-Convert years and GCAM_region_IDs in some problematic outputs to integers:
    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU  %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # Produce outputs
    L122.ag_HA_to_CropLand_R_Y_GLU  %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ag_HA_to_CropLand_R_Y_GLU

    L122.ag_EcYield_kgm2_R_C_Y_GLU %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ag_EcYield_kgm2_R_C_Y_GLU

    L122.LC_bm2_R_OtherArableLand_Yh_GLU %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L122.LC_bm2_R_OtherArableLand_Yh_GLU

    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
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
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    return_data(L122.ag_HA_to_CropLand_R_Y_GLU, L122.ag_EcYield_kgm2_R_C_Y_GLU, L122.LC_bm2_R_OtherArableLand_Yh_GLU, L122.LC_bm2_R_ExtraCropLand_Yh_GLU, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, L122.LC_bm2_R_HarvCropLand_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
