# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L201.water_resources_constrained
#'
#' Constrained surface and groudwater.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.RenewRsrcCurves_calib},
#' \code{201.GrdRenewRsrcMax_runoff}, \code{L201.DepRsrcCurves_ground}. The corresponding file in the
#' original data system was \code{L102.water_supply_unlimited.R} (water level1).
#' @details  Genereates water resource input files for region + basin which includes runoff and groundwater.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join case_when distinct filter if_else inner_join mutate pull right_join select
#' @importFrom tidyr complete nesting
#' @importFrom stats spline
#' @author ST Oct 2018
module_water_L201.water_resources_constrained <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "water/basin_to_country_mapping",
             FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "water/basin_water_demand_1990_2010",
             "L100.runoff_accessible",
             "L100.runoff_max_bm3",
             "L101.groundwater_depletion_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.groundwater_grades_uniform_bm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_uncalibrated",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground_uniform",
             "L201.RenewRsrcCurves_calib",
             "L201.DepRsrcCurves_ground",
             "L201.RenewRsrcTechShrwt",
             "L201.RsrcTechShrwt"))
  } else if(command == driver.MAKE) {

    region <- ISO <- iso <- GCAM_basin_ID <- Basin_name <- GCAM_region_ID <-
      basin_id <- GLU <- water_type <- basin_name <- resource <- runoff_max <-
      renewresource <- year <- access_fraction <- sub.renewable.resource <-
      grade <- available <- extractioncost <- price <- avail <- basin.id <-
      demand <- depletion <- runoff <- accessible <- x <- . <- accessible_runoff <-
      deficit <- years <- deficit_total <- subresource <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    basin_ids <- get_data(all_data, "water/basin_ID")
    water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L100.runoff_max_bm3 <- get_data(all_data, "L100.runoff_max_bm3")
    L100.runoff_accessible <- get_data(all_data, "L100.runoff_accessible")
    L101.groundwater_depletion_bm3 <- get_data(all_data, "L101.groundwater_depletion_bm3")
    L101.DepRsrcCurves_ground_uniform_bm3 <- get_data(all_data, "L101.groundwater_grades_uniform_bm3")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")
    basin_water_demand_1990_2010 <- get_data(all_data, "water/basin_water_demand_1990_2010")
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")

    # Basin_to_country_mapping table include only one set of distinct basins
    # that are mapped to a single country with largest basin share.
    # Assign GCAM region name to each basin.
    # Basin with overlapping GCAM regions assign to region with largest basin area.
    basin_to_country_mapping %>%
      rename(iso = ISO) %>%
      mutate(iso = tolower(iso)) %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      # ^^ non-restrictive join required (NA values generated for unmapped iso)
      # basins without gcam region mapping excluded (right join)
      # Antarctica not assigned
      right_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(basin_id = GCAM_basin_ID,
             basin_name = Basin_name) %>%
      select(GCAM_region_ID, region, basin_id) %>%
      arrange(region) ->
      RegionBasinHome

    # identify basins without gcam region mapping (anti_join)
    basin_to_country_mapping %>%
      rename(iso = ISO) %>%
      mutate(iso = tolower(iso)) %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      #not all iso included in basin mapping
      # ^^ non-restrictive join required (NA values generated for unmapped iso)
      anti_join(GCAM_region_names, by = "GCAM_region_ID") ->
      BasinNoRegion

    # create full set of region/basin combinations
    # some basins overlap multiple regions
    # Use left join to ensure only those basins in use by GCAM regions are included
    bind_rows(water_mapping_R_GLU_B_W_Ws_share %>%
                rename(basin_id = GLU),
              water_mapping_R_B_W_Ws_share) %>%
      select(GCAM_region_ID, basin_id, water_type) %>%
      filter(water_type == "water withdrawals") %>%
      distinct() %>%
      left_join(basin_ids, by = "basin_id") %>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(water_type = "water withdrawals",
             resource = paste(basin_name, water_type, sep="_")) %>%
      arrange(region, basin_name) ->
      L201.region_basin

    # create unique set of region/basin combination with
    # basin contained by home region (region with largest basin area)
    L201.region_basin %>%
      inner_join(RegionBasinHome, by = c("basin_id","GCAM_region_ID","region")) %>%
      arrange(region, basin_name) ->
      L201.region_basin_home

    # create the delete for the unlimited resource markets for withdrawals
    L201.region_basin %>%
      arrange(region, basin_name) %>%
      rename(unlimited.resource = resource) %>%
      select(LEVEL2_DATA_NAMES[["DeleteUnlimitRsrc"]]) ->
      L201.DeleteUnlimitRsrc

    # create resource markets for water withdrawals with
    # unique or shared region/basin market
    L201.region_basin %>%
      arrange(region) %>%
      mutate(output.unit = water.WATER_UNITS_QUANTITY,
             price.unit = water.WATER_UNITS_PRICE,
             market = basin_name) %>%
      arrange(region, resource) %>%
      select(LEVEL2_DATA_NAMES[["Rsrc"]]) ->
      L201.Rsrc

    # read in base year price
    L201.Rsrc %>%
      mutate(year = MODEL_YEARS[1],
             price = water.DEFAULT_BASEYEAR_WATER_PRICE
             ) %>%
      arrange(region, resource) %>%
      select(LEVEL2_DATA_NAMES[["RsrcPrice"]]) ->
      L201.RsrcPrice

    # Read in annual water runoff supply for each basin
    # Use L201.region_basin_home to assign actual resource to
    # home region.
    # Runoff table includes basins not in use by GCAM regions.
    L201.region_basin_home %>%
      left_join(L100.runoff_max_bm3, by = "basin_id") %>% #only basins with withdrawals are used
      # ^^ non-restrictive join required (NA values generated for unused basins)
      mutate(sub.renewable.resource = "runoff") %>%
      rename(renewresource = resource,
             maxSubResource = runoff_max) %>%
      arrange(region, renewresource, year) %>%
      select(LEVEL2_DATA_NAMES[["GrdRenewRsrcMaxNoFillOut"]]) ->
      L201.GrdRenewRsrcMax_runoff

    # ==========================================================#
    # CREATE INPUTS FOR THE UNCALIBRATED WATER SUPPLY XML

    # basin accessible fraction of total runoff
    L201.region_basin_home %>%
      #only basins with withdrawals are used
      left_join(L100.runoff_accessible, by = "basin_id") ->
      # ^^ non-restrictive join required (NA values generated for unused basins)
      access_fraction_uncalibrated

    access_fraction_uncalibrated %>%
      mutate(grade = "grade2",
             sub.renewable.resource = "runoff") %>%
      rename(available = access_fraction) %>%
      complete(grade = c("grade1", "grade2", "grade3"),
               nesting(region, resource, sub.renewable.resource)) %>%
      mutate(available = case_when( #accessible fraction
        grade == "grade1" ~ 0, #none available
        grade == "grade2" ~ available,
        grade == "grade3" ~ 1 #100% available
        ) ) %>%
      mutate(extractioncost = case_when(
        grade == "grade1" ~ water.RENEW.COST.GRADE1,
        grade == "grade2" ~ water.RENEW.COST.GRADE2,
        grade == "grade3" ~ water.RENEW.COST.GRADE3
        ) ) %>%
      select(region, resource, sub.renewable.resource, grade, available, extractioncost) %>%
      arrange(region, resource, grade) ->
      L201.RenewRsrcCurves_uncalibrated

    # depleteable ground water supply curve for uniform resources
    L201.region_basin_home %>%
      # not all basin groundwater curves are in used
      left_join(L101.DepRsrcCurves_ground_uniform_bm3, by = c("basin_id" = "basin.id")) %>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      mutate(subresource = "groundwater") %>%
      arrange(region, resource, price) %>%
      rename(extractioncost = price,
             available = avail) %>%
      select(LEVEL2_DATA_NAMES[["RsrcCurves"]]) ->
      L201.DepRsrcCurves_ground_uniform

    # ==========================================================#
    # CREATE INPUTS FOR THE CALIBRATED WATER SUPPLY XML

    # Calibration procedure (this will be deprecated when the water supply is switched to logits)
    # Step 1: For basins with groundwater depletion... get historical (2000 - 2010) runoff, demand, and groundwater depletion
    # Step 2: Assume no unconventional water withdrawals; back-calculate withdrawn runoff fraction using demand and groundwater depletion
    # Step 3: Combine with uncalibrated accessible water (used for basins where there is no groundwater depletion historically)
    # Step 4: Expand out for smooth resource curve (helps with GCAM solve)
    # Step 5: Determine historical grade groundwater based to be allowed and combine with depletion curves

    # Step 1
    basin_water_demand_1990_2010 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      arrange(basin.id, year) %>%
      group_by(basin.id) %>% summarise(demand = mean(demand)) %>%
      ungroup() ->
      basin_water_demand_2000_2010

    L100.runoff_max_bm3 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      group_by(basin_id) %>% summarise(runoff = mean(runoff_max)) %>%
      ungroup() ->
      basin_max_runoff_2000_2010

    # not all basin runoff water are in used
    # ^^ non-restrictive join required (NA values generated for unused basins)
    left_join(basin_water_demand_2000_2010,
              basin_max_runoff_2000_2010,
              by = c("basin.id" = "basin_id")) ->
      demand_runoff_cal

    # Step 2
    L101.groundwater_depletion_bm3 %>%
      right_join(demand_runoff_cal, by = "basin.id") %>%
      mutate(accessible = (demand - depletion) / runoff,
            accessible = if_else(accessible < 0, NA_real_, accessible)) %>%
      select(basin_id = basin.id, accessible) ->
      accessible_water

    # Step 3
    L201.region_basin_home %>%
      # not all basin runoff water are in used
      left_join(accessible_water, by= "basin_id") %>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      select(resource, accessible) %>%
      right_join(L201.RenewRsrcCurves_uncalibrated, by = "resource") %>%
      mutate(available = case_when(
        grade == "grade2" & is.na(accessible) == TRUE ~ available,
        grade == "grade2" & is.na(accessible) == FALSE ~ accessible,
        grade == "grade1" | grade == "grade3" ~ available
      )) %>% select(-accessible) %>%
      group_by(resource) %>%
      mutate(x = cumsum(available),
             available = if_else(x >= 2, x, available)) %>%
      select(-x) %>%
      ungroup() ->
      accessible_water_unsmoothed

    # Step 4
    # make function to expand out the 3-point resource curve to an interpolated 20-point curve
    get_smooth_renewresource <- function(resource, data){
      x <- data[data$resource == resource, ]
      av <- x$available
      ex <- x$extractioncost
      x_region <- x$region[1]
      #starting with only 3 points
      rnw_spline <- spline(av, ex, method = "hyman",
                           xout = c(
                             seq(av[1], av[2], length.out = 10),
                             seq(av[2], av[3], length.out = 11))[-10])

      tibble(region = x_region,
             resource = resource,
             sub.renewable.resource = "runoff",
             grade = paste0("grade", 1:20),
             available = rnw_spline$x,
             extractioncost = rnw_spline$y)
    }

    # apply smoothing across all basins
    # filter out Arctic Ocean basin since no water demand
    accessible_water_unsmoothed %>%
      filter(!grepl("Arctic Ocean", resource)) %>%
      lapply(unique(pull(., resource)), get_smooth_renewresource, .) %>%
      bind_rows(filter(accessible_water_unsmoothed,
                       grepl("Arctic Ocean", resource))) ->
      L201.RenewRsrcCurves_calib

    # Step 5
    L100.runoff_max_bm3 %>%
      filter(year %in% water.RUNOFF_HISTORICAL) %>%
      group_by(basin_id) %>% summarise(runoff = mean(runoff_max)) %>%
      ungroup() %>%
      #keep only basins in use
      filter(basin_id %in% L201.region_basin_home$basin_id) ->
      runoff_mean_hist

    access_fraction_uncalibrated %>%
        select(basin_id, access_fraction) %>%
        left_join(accessible_water, by = "basin_id") %>%
        # ^^ non-restrictive join required (NA values generated for unused basins)
        left_join_error_no_match(runoff_mean_hist, by = "basin_id") %>%
        mutate(accessible = if_else(is.na(accessible),
                                    access_fraction,
                                    accessible),
               accessible_runoff = runoff * accessible) %>%
        # ^^ get runoff volumes available
        select(basin.id = basin_id, accessible_runoff) %>%
        right_join(basin_water_demand_1990_2010, by = "basin.id") %>%
        # ^^ join the historical demand
        mutate(deficit = demand - accessible_runoff,
               deficit = if_else(deficit <=0, 0, deficit)) %>%
        # ^^ determine how much water needs to be met by groundwater depletion
        left_join_error_no_match(tibble(year = MODEL_BASE_YEARS[MODEL_BASE_YEARS >= water.GW_DEPLETION_BASE_YEAR],
                         years = diff(MODEL_BASE_YEARS)), by = "year") %>%
        mutate(deficit_total = deficit * years) %>%
        group_by(basin.id) %>% summarise(available = sum(deficit_total)) %>%
        ungroup() %>%
        filter(available > 0) %>%
        mutate(grade = "grade hist", price = water.DEFAULT_BASEYEAR_WATER_PRICE) ->
      groundwater_hist

      bind_rows(
        L201.region_basin_home %>%
          left_join(L101.groundwater_grades_constrained_bm3,
                    by = c("basin_id" = "basin.id")),
        # ^^ non-restrictive join required (NA values generated for unused basins)
        L201.region_basin_home %>%
          left_join(groundwater_hist,
                    by = c("basin_id" = "basin.id")) %>%
          # ^^ non-restrictive join required (NA values generated for unused basins)
          filter(!is.na(grade))) %>%
        rename(extractioncost = price) %>%
        mutate(subresource = "groundwater",
               available = round(available, water.DIGITS_GROUND_WATER_RSC),
               extractioncost = round(extractioncost, water.DIGITS_GROUND_WATER_RSC)) %>%
        select(LEVEL2_DATA_NAMES[["RsrcCurves"]]) %>%
        arrange(region, resource, extractioncost) ->
        L201.DepRsrcCurves_ground

      # problem with original groundwater constrained input file
      # contains extra 0 available grade and thus creastes discontinuous supply curve
      L201.DepRsrcCurves_ground %>%
        filter(grade == "grade24" & available == 0) ->
        L201.DepRsrcCurves_ground_last

      bind_rows(
        L201.DepRsrcCurves_ground %>%
          filter(available > 0),
        L201.DepRsrcCurves_ground_last ) %>%
        arrange(region, resource, extractioncost) ->
        L201.DepRsrcCurves_ground

      # Create an empty technology for all water resources and subresources.
      # Include a share weight of 1 to facilatate creating a technology.
      # Create technology for renewable freshwater and depletable groundwater subresource
      L201.RenewRsrcCurves_calib %>%
        distinct(region, resource, sub.renewable.resource) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(technology = sub.renewable.resource,
               share.weight = 1.0) %>%
        rename(subresource = sub.renewable.resource) %>%
        select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
        L201.RenewRsrcTechShrwt

      L201.DepRsrcCurves_ground %>%
        distinct(region, resource, subresource) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(technology = subresource,
               share.weight = 1.0) %>%
        select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
        L201.RsrcTechShrwt

    # ===================================================

    # Produce outputs

      L201.DeleteUnlimitRsrc %>%
        add_title("Delete Unlimited Resources") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_legacy_name("L201.DeleteUnlimitRsrc") %>%
        add_precursors("water/basin_ID",
                       "common/GCAM_region_names",
                       "L103.water_mapping_R_GLU_B_W_Ws_share",
                       "L103.water_mapping_R_B_W_Ws_share") ->
        L201.DeleteUnlimitRsrc

      L201.Rsrc %>%
        add_title("Resource markets for water withdrawals") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_legacy_name("L201.Rsrc") %>%
        add_precursors("water/basin_ID",
                       "water/basin_to_country_mapping",
                       "common/GCAM_region_names",
                       "common/iso_GCAM_regID",
                       "L103.water_mapping_R_GLU_B_W_Ws_share",
                       "L103.water_mapping_R_B_W_Ws_share") ->
        L201.Rsrc

      L201.RsrcPrice %>%
        add_title("Base year price") %>%
        add_units("1975$") %>%
        add_comments("") %>%
        add_legacy_name("L201.RsrcPrice") %>%
        add_precursors("water/basin_ID",
                       "water/basin_to_country_mapping",
                       "common/GCAM_region_names",
                       "common/iso_GCAM_regID",
                       "L103.water_mapping_R_GLU_B_W_Ws_share",
                       "L103.water_mapping_R_B_W_Ws_share") ->
        L201.RsrcPrice

      L201.RenewRsrcCurves_uncalibrated %>%
        add_title("Uncalibrated renewable resource curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("") %>%
        add_legacy_name("L201.RenewRsrcCurves_uncalibrated") %>%
        add_precursors("L100.runoff_accessible") ->
        L201.RenewRsrcCurves_uncalibrated

      L201.GrdRenewRsrcMax_runoff %>%
        add_title("Maximum runoff") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("Upper limit of water supply; RenewRsrc is applied to this to get renewable water") %>%
        add_legacy_name("L201.GrdRenewRsrcMax_runoff") %>%
        add_precursors("L100.runoff_max_bm3") ->
        L201.GrdRenewRsrcMax_runoff

      L201.DepRsrcCurves_ground_uniform %>%
        add_title("Uniform depletable groundwater curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("") %>%
        add_legacy_name("L201.DepRsrcCurves_ground_uniform") %>%
        add_precursors("L101.groundwater_grades_uniform_bm3") ->
        L201.DepRsrcCurves_ground_uniform

      L201.RenewRsrcCurves_calib %>%
        add_title("Calibrated renewable water curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("Calibrated to ensure observed groundwater is taken in calibration years") %>%
        add_legacy_name("L201.RenewRsrcCurves_calib") %>%
        add_precursors("water/basin_water_demand_1990_2010",
                       "L101.groundwater_depletion_bm3",
                       "L100.runoff_accessible",
                       "L100.runoff_max_bm3") ->
        L201.RenewRsrcCurves_calib

      L201.DepRsrcCurves_ground %>%
        add_title("Depletable groundwater curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("Includes historical grades") %>%
        add_legacy_name("L201.DepRsrcCurves_ground") %>%
        add_precursors("water/basin_water_demand_1990_2010",
                       "L101.groundwater_grades_constrained_bm3") ->
        L201.DepRsrcCurves_ground

      L201.RenewRsrcTechShrwt %>%
        add_title("Water renewable resource technologies") %>%
        add_units("NA") %>%
        add_comments("share weight is 1") %>%
        add_precursors("L201.RenewRsrcCurves_calib") ->
        L201.RenewRsrcTechShrwt

      L201.RsrcTechShrwt %>%
        add_title("Water depletable resource technologies") %>%
        add_units("NA") %>%
        add_comments("share weight is 1") %>%
        add_precursors("L201.DepRsrcCurves_ground") ->
        L201.RsrcTechShrwt

      return_data(L201.DeleteUnlimitRsrc,
                  L201.Rsrc,
                  L201.RsrcPrice,
                  L201.RenewRsrcCurves_uncalibrated,
                  L201.GrdRenewRsrcMax_runoff,
                  L201.DepRsrcCurves_ground_uniform,
                  L201.RenewRsrcCurves_calib,
                  L201.DepRsrcCurves_ground,
                  L201.RenewRsrcTechShrwt,
                  L201.RsrcTechShrwt)

  } else {
    stop("Unknown command")
  }
}
