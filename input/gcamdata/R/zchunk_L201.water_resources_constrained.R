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
#' @details  Generates water resource input files for region + basin which includes runoff and groundwater.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join case_when distinct ends_with filter if_else inner_join lead mutate pull right_join select
#' @importFrom tidyr complete nesting
#' @importFrom stats spline
#' @author ST Oct 2018
module_water_L201.water_resources_constrained <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "water/A72.globaltech_coef",
             FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             "L100.runoff_accessible",
             "L100.runoff_max_bm3",
             "L101.groundwater_depletion_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.DepRsrcCurves_ground_uniform_bm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share",
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
             "L201.RsrcTechShrwt",
             "L201.RsrcTechCoef"))
  } else if(command == driver.MAKE) {

    region <- ISO <- iso <- GCAM_basin_ID <- Basin_name <- GCAM_region_ID <-
      GLU <- water_type <- basin_name <- resource <- runoff_max <-
      renewresource <- year <- access_fraction <- sub.renewable.resource <-
      grade <- available <- extractioncost <- price <- avail <-
      demand <- depletion <- runoff <- accessible <- x <- . <- accessible_runoff <-
      deficit <- years <- deficit_total <- subresource <- GLU_name <- GLU_code <-
      conveyance.eff <- IrrWithd_km3 <- technology <- calOutputValue <- value <-
      demand_frac <- lower_cost <- upper_cost <- elec_coef <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping", strip_attributes = TRUE)
    A72.globaltech_coef <- get_data(all_data, "water/A72.globaltech_coef", strip_attributes = TRUE)
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share", strip_attributes = TRUE)
    L100.runoff_max_bm3 <- get_data(all_data, "L100.runoff_max_bm3")
    L100.runoff_accessible <- get_data(all_data, "L100.runoff_accessible")
    L101.groundwater_depletion_bm3 <- get_data(all_data, "L101.groundwater_depletion_bm3")
    L101.DepRsrcCurves_ground_uniform_bm3 <- get_data(all_data, "L101.DepRsrcCurves_ground_uniform_bm3")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3", strip_attributes = TRUE )
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R", strip_attributes = TRUE)
    L165.IrrWithd_km3_R_B_Y <- get_data(all_data, "L165.IrrWithd_km3_R_B_Y", strip_attributes = TRUE)
    L203.Production_watertd <- get_data(all_data, "L203.Production_watertd", strip_attributes = TRUE)

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
      mutate(sub.renewable.resource = "runoff",
             renewresource = resource,
             maxSubResource = round(runoff_max, water.DIGITS_GROUND_WATER_RSC)) %>%
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
        grade == "grade2" ~ round(available, water.DIGITS_RENEW_WATER),
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
      mutate(subresource = "groundwater",
             extractioncost = round(price, water.DIGITS_GROUND_WATER_RSC),
             available = round(avail, water.DIGITS_GROUND_WATER_RSC)) %>%
      arrange(region, resource, price) %>%
      select(LEVEL2_DATA_NAMES[["RsrcCurves"]]) ->
      L201.DepRsrcCurves_ground_uniform

    # ==========================================================#
    # CREATE INPUTS FOR THE CALIBRATED WATER SUPPLY XML

    # Calibration procedure
    # Step 1: For basins with groundwater depletion... get historical (2000 - 2015) runoff, demand, and groundwater depletion
    ## We use the means across years for runoff; groundwater depletion is an annual value assumed invariant over time;
    ## We use the max of the estimated demands over all model base years (assumes that all of these historical withdrawals were "conventional")
    # Step 2: Assume no unconventional water withdrawals; back-calculate withdrawn runoff fraction using demand and groundwater depletion
    # Step 3: Combine with uncalibrated accessible water (used for basins where there is no groundwater depletion historically)
    # Step 4: Expand out for smooth resource curve (helps with GCAM solve)
    # Step 5: Determine historical grade groundwater based to be allowed and combine with depletion curves

    # Step 1
    # Historical water demand by basin
    # Where most of the water demands are available in L1 tables of
    # "water withdrawals" by region multiplied by basin-level shares, calculated in L1 chunks, industrial and municipal
    # demands can't be assigned to basin from that stage, as they need to have their desalination-related water use
    # deducted. For this reason, we use the calibration data generated in L203. This doesn't have ag.

    # Pre-process the basin-level data on water demands for binding together
    L201.IrrWithd_km3_R_B_Y <- L165.IrrWithd_km3_R_B_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GCAM_basin_ID),
                               by = c(GLU = "GLU_code")) %>%
      left_join_error_no_match(select(L165.ag_IrrEff_R, GCAM_region_ID, conveyance.eff),
                               by = "GCAM_region_ID") %>%
      mutate(value = IrrWithd_km3 / conveyance.eff)
    L201.NonIrrWithd_km3_R_B_Y <- L203.Production_watertd %>%
      filter(technology != water.DESAL,
             technology != "water consumption") %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_name, GCAM_basin_ID),
                               by = c(subsector = "GLU_name")) %>%
      select(GCAM_region_ID, GCAM_basin_ID, year, value = calOutputValue)

    basin_water_demand_1990_2015 <- bind_rows(L201.IrrWithd_km3_R_B_Y,
                                              L201.NonIrrWithd_km3_R_B_Y) %>%
      filter(year %in% MODEL_BASE_YEARS,
             year >= water.GW_DEPLETION_BASE_YEAR) %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_name),
                               by = "GCAM_basin_ID") %>%
      group_by(GCAM_basin_ID, GLU_name, year) %>%
      summarise(demand = sum(value)) %>%
      ungroup()

    basin_water_demand_2000_2015 <- basin_water_demand_1990_2015 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      group_by(GCAM_basin_ID, year) %>%
      summarise(demand = sum(demand)) %>%
      ungroup() %>%
      group_by(GCAM_basin_ID) %>%
      summarise(demand = max(demand)) %>%
      ungroup()

    L100.runoff_max_bm3 %>%
      filter(year %in% water.GW_DEPLETION_HISTORICAL) %>%
      group_by(GCAM_basin_ID) %>% summarise(runoff = mean(runoff_max)) %>%
      ungroup() ->
      basin_max_runoff_2000_2015

    # not all basin runoff water are in used
    # ^^ non-restrictive join required (NA values generated for unused basins)
    left_join(basin_water_demand_2000_2015,
              basin_max_runoff_2000_2015,
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
             available = round(rnw_spline$x, water.DIGITS_RENEW_WATER_RSC),
             extractioncost = round(rnw_spline$y, water.DIGITS_RENEW_WATER_RSC))
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
        right_join(basin_water_demand_1990_2015, by = "GCAM_basin_ID") %>%
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
        mutate(subresource = "groundwater grade hist",
               lower_cost = water.DEFAULT_BASEYEAR_WATER_PRICE,
               upper_cost = water.GRADE_HIST_UPPER_BOUND) ->
      groundwater_hist

# Construction of the curves: grade hist first
    L201.DepRsrcCurves_ground_hist <- L201.region_basin_home %>%
      left_join(groundwater_hist,
                by = "GCAM_basin_ID") %>%
      # ^^ non-restrictive join required (NA values generated for unused basins)
      filter(!is.na(subresource)) %>%
      mutate(available = round(available * water.GW_HIST_MULTIPLIER, water.DIGITS_GROUND_WATER_RSC),
             # no additional electric energy is assigned to the historical grades of groundwater
             elec_coef = 0)

    L201.DepRsrcCurves_ground_fut <- inner_join(L101.groundwater_grades_constrained_bm3, L201.region_basin,
                                                by = c("GCAM_region_ID", "GCAM_basin_ID")) %>%
      mutate(subresource = paste("groundwater", grade)) %>%
      select(-grade)

    # Construct the supply curves. Within each subresource, the lower_cost is assigned to grade1, the upper cost is
    # assigned to grade2, and the available is assigned 100% to grade1. Grade2 has 0 available. The elec_coef has to be the same
    # Filtering out grade21 due to lack of meaningful upper bound cost (often equal to lower bound cost, sometimes very low)
    L201.DepRsrcCurves_ground <- bind_rows(L201.DepRsrcCurves_ground_hist, L201.DepRsrcCurves_ground_fut) %>%
      mutate(lower_cost = round(lower_cost, water.DIGITS_GROUND_WATER_RSC),
             upper_cost = round(upper_cost, water.DIGITS_GROUND_WATER_RSC)) %>%
      select(region, resource, subresource, available, lower_cost, upper_cost, elec_coef) %>%
      gather(key = "grade", value = "extractioncost", ends_with("_cost")) %>%
      mutate(available = if_else(grade == "lower_cost",
                                 round(available, water.DIGITS_GROUND_WATER_RSC),
                                 0),
             grade = if_else(grade == "lower_cost", "grade1", "grade2")) %>%
      filter(grade != "grade21") %>%
      select(c(LEVEL2_DATA_NAMES[["RsrcCurves"]], "elec_coef")) ->
      L201.DepRsrcCurves_ground

      # Create a technology for all water resources and subresources. This is where the groundwater electricity coefficient will be assigned.
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

      # Energy inputs
      elec_input_name <- unique(A72.globaltech_coef$minicam.energy.input)
      L201.DepRsrcCurves_ground %>%
        select(-grade,-available, -extractioncost) %>%
        distinct() %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(technology = subresource,
               minicam.energy.input = elec_input_name,
               coefficient = round(elec_coef, energy.DIGITS_COEFFICIENT)) %>%
        select(LEVEL2_DATA_NAMES[["ResTechCoef"]]) ->
        L201.RsrcTechCoef

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
                       "L165.ag_IrrEff_R",
                       "L165.IrrWithd_km3_R_B_Y",
                       "L203.Production_watertd",
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
        same_precursors_as(L201.DepRsrcCurves_ground) ->
        L201.RsrcTechShrwt

      L201.RsrcTechCoef %>%
        add_title("Water depletable resource technologies input-output coefficients") %>%
        add_units("GJ per m3") %>%
        add_comments("calculated from Superwell data") %>%
        same_precursors_as(L201.DepRsrcCurves_ground) %>%
        add_precursors("water/A72.globaltech_coef") ->
        L201.RsrcTechCoef


      return_data(L201.DeleteUnlimitRsrc,
                  L201.Rsrc,
                  L201.RsrcPrice,
                  L201.RenewRsrcCurves_uncalibrated,
                  L201.GrdRenewRsrcMax_runoff,
                  L201.DepRsrcCurves_ground_uniform,
                  L201.RenewRsrcCurves_calib,
                  L201.DepRsrcCurves_ground,
                  L201.RenewRsrcTechShrwt,
                  L201.RsrcTechShrwt,
                  L201.RsrcTechCoef)

  } else {
    stop("Unknown command")
  }
}
