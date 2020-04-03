# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L201.water_resources_constrained
#'
#' Constrained surface and groundwater.
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
#' @importFrom dplyr anti_join case_when distinct filter if_else inner_join lead mutate pull right_join select
#' @importFrom tidyr complete nesting
#' @importFrom stats spline
#' @author ST Oct 2018
module_water_L201.water_resources_constrained <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             "L100.runoff_accessible",
             "L100.runoff_max_bm3",
             "L101.groundwater_depletion_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.DepRsrcCurves_ground_uniform_bm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share",
             "L125.LC_bm2_R_GLU",
             "L103.water_mapping_R_B_W_Ws_share",
             "L110.in_km3_water_primary_basin",
             "L1233.wdraw_km3_R_B_elec",
             "L133.water_demand_livestock_R_B_W_km3",
             "L165.ag_IrrEff_R",
             "L165.IrrWithd_km3_R_B_Y",
             "L203.Production_watertd"))
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
      GLU <- water_type <- basin_name <- resource <- runoff_max <-
      renewresource <- year <- access_fraction <- sub.renewable.resource <-
      grade <- available <- extractioncost <- price <- avail <-
      demand <- depletion <- runoff <- accessible <- x <- . <- accessible_runoff <-
      deficit <- years <- deficit_total <- subresource <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L100.runoff_max_bm3 <- get_data(all_data, "L100.runoff_max_bm3")
    L100.runoff_accessible <- get_data(all_data, "L100.runoff_accessible")
    L101.groundwater_depletion_bm3 <- get_data(all_data, "L101.groundwater_depletion_bm3")
    L101.DepRsrcCurves_ground_uniform_bm3 <- get_data(all_data, "L101.DepRsrcCurves_ground_uniform_bm3")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L110.in_km3_water_primary_basin <- get_data(all_data, "L110.in_km3_water_primary_basin")
    L1233.wdraw_km3_R_B_elec <- get_data(all_data, "L1233.wdraw_km3_R_B_elec")
    L133.water_demand_livestock_R_B_W_km3 <- get_data(all_data, "L133.water_demand_livestock_R_B_W_km3")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L165.IrrWithd_km3_R_B_Y <- get_data(all_data, "L165.IrrWithd_km3_R_B_Y")
    L203.Production_watertd <- get_data(all_data, "L203.Production_watertd")

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
      select(GCAM_region_ID, region, GCAM_basin_ID) %>%
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
    bind_rows(L103.water_mapping_R_GLU_B_W_Ws_share %>%
                rename(GCAM_basin_ID = GLU),
              L103.water_mapping_R_B_W_Ws_share) %>%
      select(GCAM_region_ID, GCAM_basin_ID, water_type) %>%
      filter(water_type == "water withdrawals") %>%
      distinct() %>%
      left_join(select(basin_to_country_mapping, GCAM_basin_ID, basin_name = GLU_name), by = "GCAM_basin_ID") %>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(water_type = "water withdrawals",
             resource = paste(basin_name, water_type, sep="_")) %>%
      arrange(region, basin_name) ->
      L201.region_basin

    # create unique set of region/basin combination with
    # basin contained by home region (region with largest basin area)
    L201.region_basin %>%
      inner_join(RegionBasinHome, by = c("GCAM_basin_ID","GCAM_region_ID","region")) %>%
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
      left_join(L100.runoff_max_bm3, by = "GCAM_basin_ID") %>% #only basins with withdrawals are used
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
      left_join(L100.runoff_accessible, by = "GCAM_basin_ID") ->
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

    # depletable ground water supply curve for uniform resources
    L201.region_basin_home %>%
      # not all basin groundwater curves are in used
      left_join(L101.DepRsrcCurves_ground_uniform_bm3, by = "GCAM_basin_ID") %>%
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
    # Historical water demand by basin
    # This part is somewhat confusing for industrial and munipal uses. Where most of the water demands are equal to the
    # "water withdrawals" by region multiplied by basin-level shares, calculated in L1 chunks, industrial and municipal
    # demands can't be assigned to basin at that stage, as they need to have their desalination-related water use
    # deducted. That is, the withdrawals by basin are equal to total regional withdrawals minus desal, then multiplied
    # by basin-wise shares. For this reason, the industrial and municipal quantities are pulled from the calibration
    # quantities read to the model.

    # Pre-process the basin-level data on water demands for binding together
    L110.in_km3_water_primary_basin <- filter(L110.in_km3_water_primary_basin, water_type == "water withdrawals")
    L133.water_demand_livestock_R_B_W_km3 <- filter(L133.water_demand_livestock_R_B_W_km3, water_type == "water withdrawals")
    L165.IrrWithd_km3_R_B_Y <- L165.IrrWithd_km3_R_B_Y %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GCAM_basin_ID),
                               by = c(GLU = "GLU_code")) %>%
      left_join_error_no_match(select(L165.ag_IrrEff_R, GCAM_region_ID, conveyance.eff),
                               by = "GCAM_region_ID") %>%
      mutate(value = IrrWithd_km3 / conveyance.eff)
    L203.Production_watertd <- filter(L203.Production_watertd, technology != water.DESAL) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_name, GCAM_basin_ID),
                               by = c(technology = "GLU_name")) %>%
      select(GCAM_region_ID, GCAM_basin_ID, year, value = calOutputValue)

    basin_water_demand_1990_2010 <- bind_rows(L1233.wdraw_km3_R_B_elec,
                                              L133.water_demand_livestock_R_B_W_km3,
                                              L110.in_km3_water_primary_basin,
                                              L165.IrrWithd_km3_R_B_Y,
                                              L203.Production_watertd) %>%
      filter(year %in% MODEL_BASE_YEARS,
             year >= water.GW_DEPLETION_BASE_YEAR) %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_name),
                               by = "GCAM_basin_ID") %>%
      group_by(GCAM_basin_ID, GLU_name, year) %>%
      summarise(demand = sum(value)) %>%
      ungroup()

    basin_water_demand_2000_2010 <- basin_water_demand_1990_2010 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      group_by(GCAM_basin_ID, year) %>%
      summarise(demand = sum(demand)) %>%
      ungroup() %>%
      group_by(GCAM_basin_ID) %>%
      summarise(demand = mean(demand)) %>%
      ungroup()

    L100.runoff_max_bm3 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      group_by(GCAM_basin_ID) %>% summarise(runoff = mean(runoff_max)) %>%
      ungroup() ->
      basin_max_runoff_2000_2010

    # not all basin runoff water are in used
    # ^^ non-restrictive join required (NA values generated for unused basins)
    left_join(basin_water_demand_2000_2010,
              basin_max_runoff_2000_2010,
              by = "GCAM_basin_ID") ->
      demand_runoff_cal

    # Step 2
    L101.groundwater_depletion_bm3 %>%
      right_join(demand_runoff_cal, by = "GCAM_basin_ID") %>%
      mutate(accessible = (demand - depletion) / runoff,
            accessible = if_else(accessible < 0, NA_real_, accessible)) %>%
      select(GCAM_basin_ID, accessible) ->
      accessible_water

    # Step 3
    L201.region_basin_home %>%
      # not all basin runoff water are in used
      left_join(accessible_water, by= "GCAM_basin_ID") %>%
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

    # replace 3-point curve with 20-point curve in all basins, except those whose demand is lower than a threshold fraction
    # of total max runoff. Such basins can cause solution difficulties in the model due to the flatness of the supply curves
    # at the relevant point.
    three_point_supply_curve_resources <- demand_runoff_cal %>%
      mutate(demand_frac = demand / runoff) %>%
      filter(demand_frac < water.DEMAND_FRAC_THRESHOLD) %>%
      inner_join(select(L201.region_basin_home, GCAM_basin_ID, resource),
                               by = "GCAM_basin_ID") %>%
      pull(resource)

    accessible_water_unsmoothed %>%
      filter(!resource %in% three_point_supply_curve_resources) %>%
      lapply(unique(pull(., resource)), get_smooth_renewresource, .) %>%
      bind_rows(filter(accessible_water_unsmoothed,
                       resource %in% three_point_supply_curve_resources)) ->
      L201.RenewRsrcCurves_calib

    # Step 5
    L100.runoff_max_bm3 %>%
      filter(year %in% water.RUNOFF_HISTORICAL) %>%
      group_by(GCAM_basin_ID) %>% summarise(runoff = mean(runoff_max)) %>%
      ungroup() %>%
      #keep only basins in use
      filter(GCAM_basin_ID %in% L201.region_basin_home$GCAM_basin_ID) ->
      runoff_mean_hist

    access_fraction_uncalibrated %>%
        select(GCAM_basin_ID, access_fraction) %>%
        left_join(accessible_water, by = "GCAM_basin_ID") %>%
        # ^^ non-restrictive join required (NA values generated for unused basins)
        left_join_error_no_match(runoff_mean_hist, by = "GCAM_basin_ID") %>%
        mutate(accessible = if_else(is.na(accessible),
                                    access_fraction,
                                    accessible),
               accessible_runoff = runoff * accessible) %>%
        # ^^ get runoff volumes available
        select(GCAM_basin_ID, accessible_runoff) %>%
        right_join(basin_water_demand_1990_2010, by = "GCAM_basin_ID") %>%
        # ^^ join the historical demand
        mutate(deficit = demand - accessible_runoff,
               deficit = if_else(deficit <=0, 0, deficit)) %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        # ^^ determine how much water needs to be met by groundwater depletion
        left_join_error_no_match(tibble(year = MODEL_BASE_YEARS[MODEL_BASE_YEARS >= water.GW_DEPLETION_BASE_YEAR],
                         years = diff(MODEL_BASE_YEARS)), by = "year") %>%
        mutate(deficit_total = deficit * years) %>%
        group_by(GCAM_basin_ID) %>%
        summarise(available = sum(deficit_total) * water.GW_HIST_MULTIPLIER) %>%
        ungroup() %>%
        filter(available > 0) %>%
        mutate(grade = "grade hist", price = water.DEFAULT_BASEYEAR_WATER_PRICE) ->
      groundwater_hist

    # Parse out groundwater supply curves by basin to basin and region on the basis of land area shares
    region_within_basin_shares <- L125.LC_bm2_R_GLU %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GCAM_basin_ID),
                               by = c(GLU = "GLU_code")) %>%
      group_by(GCAM_basin_ID) %>%
      mutate(share = LC_bm2 / sum(LC_bm2)) %>%
      ungroup() %>%
      select(GCAM_region_ID, GCAM_basin_ID, share)

      bind_rows(
        L201.region_basin %>%
          left_join(region_within_basin_shares, by = c("GCAM_region_ID", "GCAM_basin_ID")) %>%
          # Assign zero groundwater share where our estimated region/basin land area is 0
          replace_na(list(share = 0)) %>%
          left_join(L101.groundwater_grades_constrained_bm3,
                    by = "GCAM_basin_ID") %>%
          mutate(available = available * share) %>%
          select(-share),
        # ^^ non-restrictive join required (NA values generated for unused basins)
        L201.region_basin_home %>%
          left_join(groundwater_hist,
                    by = "GCAM_basin_ID") %>%
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

      L201.DepRsrcCurves_ground %>%
        mutate(subresource = paste(subresource, grade)) %>%
        group_by(region, resource) %>%
        mutate(grade = dplyr::lead(grade),
               available = 0,
               extractioncost = dplyr::lead(extractioncost)) %>%
        filter(!is.na(grade)) %>%
        ungroup() %>%
        bind_rows(L201.DepRsrcCurves_ground %>% mutate(subresource = paste(subresource, grade)), .) %>%
        filter(subresource != "grade24") ->
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
        add_precursors("water/basin_to_country_mapping",
                       "common/GCAM_region_names",
                       "L103.water_mapping_R_GLU_B_W_Ws_share",
                       "L103.water_mapping_R_B_W_Ws_share") ->
        L201.DeleteUnlimitRsrc

      L201.Rsrc %>%
        add_title("Resource markets for water withdrawals") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_legacy_name("L201.Rsrc") %>%
        add_precursors("water/basin_to_country_mapping",
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
        add_precursors("water/basin_to_country_mapping",
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
        add_precursors("L101.DepRsrcCurves_ground_uniform_bm3") ->
        L201.DepRsrcCurves_ground_uniform

      L201.RenewRsrcCurves_calib %>%
        add_title("Calibrated renewable water curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("Calibrated to ensure observed groundwater is taken in calibration years") %>%
        add_legacy_name("L201.RenewRsrcCurves_calib") %>%
        add_precursors("L103.water_mapping_R_B_W_Ws_share",
                       "L110.in_km3_water_primary_basin",
                       "L1233.wdraw_km3_R_B_elec",
                       "L133.water_demand_livestock_R_B_W_km3",
                       "L165.ag_IrrEff_R",
                       "L165.IrrWithd_km3_R_B_Y",
                       "L203.Production_watertd",
                       "L101.groundwater_depletion_bm3",
                       "L100.runoff_accessible",
                       "L100.runoff_max_bm3") ->
        L201.RenewRsrcCurves_calib

      L201.DepRsrcCurves_ground %>%
        add_title("Depletable groundwater curves") %>%
        add_units("bm^3, 1975$") %>%
        add_comments("Includes historical grades") %>%
        add_legacy_name("L201.DepRsrcCurves_ground") %>%
        add_precursors("L103.water_mapping_R_B_W_Ws_share",
                       "L110.in_km3_water_primary_basin",
                       "L1233.wdraw_km3_R_B_elec",
                       "L133.water_demand_livestock_R_B_W_km3",
                       "L165.ag_IrrEff_R",
                       "L165.IrrWithd_km3_R_B_Y",
                       "L203.Production_watertd",
                       "L101.groundwater_grades_constrained_bm3",
                       "L125.LC_bm2_R_GLU") ->
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
