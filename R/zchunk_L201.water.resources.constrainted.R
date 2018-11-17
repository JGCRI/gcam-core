#' module_water_L201.water.resources.constrained
#'
#' Constrained surface and groudwater.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.RenewRsrcCurves_calib},
#' \code{201.GrdRenewRsrcMax_runoff}, \code{L201.DepRsrcCurves_ground}. The corresponding file in the
#' original data system was \code{L102.water.supply.unlimited.R} (water level1).
#' @details  Genereates water withdrawal resource input files for region + basin which includes runoff and groundwater.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom purrr map map_dfr
#' @author ST Oct 2018
module_water_L201.water.resources.constrained <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "common/GCAM_region_names",
             FILE = "water/basin_water_demand_1990_2010",
             "L100.runoff_accessible",
             "L100.runoff_max_bm3",
             "L101.groundwater_depletion_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.groundwater_grades_uniform_bm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.NodeEquiv",
             "L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_uncalibrated",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground_uniform",
             "L201.RenewRsrcCurves_calib",
             "L201.DepRsrcCurves_ground"))
  } else if(command == driver.MAKE) {

    region <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    gcam_regions <- get_data(all_data, "common/GCAM_region_names")
    basin_ids <- get_data(all_data, "water/basin_ID")
    water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L100.runoff_max_bm3 <- get_data(all_data, "L100.runoff_max_bm3")
    L100.runoff_accessible <- get_data(all_data, "L100.runoff_accessible")
    L101.groundwater_depletion_bm3 <- get_data(all_data, "L101.groundwater_depletion_bm3")
    L201.DepRsrcCurves_ground_uniform_bm3 <- get_data(all_data, "L101.groundwater_grades_uniform_bm3")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")
    basin_water_demand_1990_2010 <- get_data(all_data, "water/basin_water_demand_1990_2010")
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L101.groundwater_grades_constrained_bm3 <- get_data(all_data, "L101.groundwater_grades_constrained_bm3")


    # build tables with all possible technologies

    # create node equivalence lists to allow use of same subresource headers
    # ... regardless of what type the actual resources is

    tibble(group.name = "Resources", tag1 = "resource",
           tag2 = "depresource", tag3 = "renewresource",
           tag4 = "unlimited.resource") ->
      L201.NodeEquiv

    # create full set of region/basin combinations
    bind_rows(water_mapping_R_GLU_B_W_Ws_share %>%
                rename(basin_id = GLU),
              water_mapping_R_B_W_Ws_share) %>%
      select(-water_sector, -share) %>% unique() %>%
      filter(water_type == "water withdrawals") %>%
      left_join(basin_ids, by = "basin_id") %>%
      left_join(gcam_regions, by = "GCAM_region_ID") ->
      L201.region_basin

    # create the delete for the unlimited resource markets for withdrawals
    L201.region_basin %>%
      arrange(region) %>%
      mutate(unlimited.resource = paste0("-", water_type)) %>%
      select(region, unlimited.resource) ->
      L201.DeleteUnlimitRsrc

    # create resource markets for water withdrawals
    L201.region_basin %>%
      arrange(region) %>%
      mutate(resource = paste0("-", water_type),
             output.unit = water.WATER_UNITS_QUANTITY,
             price.unit = water.WATER_UNITS_PRICE) %>%
      rename(market = basin_id) %>%
      select(region, resource, output.unit, price.unit, market) ->
      L201.Rsrc

    # get first basins only for each region
    L201.region_basin %>%
      group_by(basin_id) %>%
      summarise(GCAM_region_ID = min(GCAM_region_ID)) %>%
      left_join(L201.region_basin, by = c("basin_id", "GCAM_region_ID")) ->
      L201.region_basin.first_only

    # read in base year price
    L201.region_basin.first_only %>%
      mutate(renewresource = paste(basin_name, water_type, sep = "-"),
             year = MODEL_YEARS[1],
             price = water.DEFAULT_BASEYEAR_WATER_PRICE
             ) %>%
      select(region, renewresource, year, price) %>%
      arrange(region, renewresource) ->
      L201.RsrcPrice

    # Read in annual water runoff supply
    L201.region_basin.first_only %>% as_tibble() %>%
      left_join(L100.runoff_max_bm3, by = "basin_id") %>%
      mutate(renewresource = paste0(basin_name, "-", water_type),
             sub.renewable.resource = "runoff") %>%
      rename(year.fillout = year,
             maxSubResource = runoff_max) %>%
      select(region, renewresource, sub.renewable.resource, year.fillout, maxSubResource) %>%
      arrange(region, renewresource, year.fillout) ->
      L201.GrdRenewRsrcMax_runoff

    # ==========================================================#
    # CREATE INPUTS FOR THE UNCALIBRATED WATER SUPPLY XML

    L201.region_basin.first_only %>% as_tibble() %>%
      left_join(L100.runoff_accessible, by = "basin_id") ->
      access_fraction_uncalibrated

    access_fraction_uncalibrated %>%
      mutate(grade = "grade2",
             renewresource = paste0(basin_name, "-", water_type),
             sub.renewable.resource = "runoff") %>%
      rename(available = access_fraction) %>%
      complete(grade = c("grade1", "grade2", "grade3"),
               nesting(region, renewresource, sub.renewable.resource)) %>%
      mutate(available = case_when(
        grade == "grade1" ~ 0,
        grade == "grade2" ~ available,
        grade == "grade3" ~ 1
      )) %>% arrange(renewresource) %>%
      select(region, renewresource, sub.renewable.resource, grade, available) %>%
      mutate(extractioncost = case_when(
        grade == "grade1" ~ 0.0000100,
        grade == "grade2" ~ 0.00100,
        grade == "grade3" ~ 10.0
      )) ->
      L201.RenewRsrcCurves_uncalibrated

    # depleteable ground water supply curve for uniform resources
    L201.region_basin.first_only %>%
      left_join(L201.DepRsrcCurves_ground_uniform_bm3, by = c("basin_id" = "basin.id")) %>%
      mutate(depresource = paste(basin_name, water_type, sep = "-"),
             subresource = "groundwater") %>%
      arrange(region, depresource, price) %>%
      rename(extractioncost = price,
             available = avail) %>%
      select(one_of(LEVEL2_DATA_NAMES$DepRsrcCurves)) ->
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
      filter(year %in% c(2005, 2010)) %>%
      arrange(basin.id, year) %>%
      group_by(basin.id) %>% summarise(demand = mean(demand)) ->
      basin_water_demand_2000_2010

    L100.runoff_max_bm3 %>%
      filter(year %in% c(2005, 2010)) %>%
      group_by(basin_id) %>% summarise(runoff = mean(runoff_max)) ->
      basin_max_runoff_2000_2010

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
      aw

    # Step 3
    L201.region_basin.first_only %>%
      left_join(aw, by= "basin_id") %>%
      mutate(renewresource = paste0(basin_name, "-", water_type)) %>%
      select(renewresource, accessible) %>%
      right_join(L201.RenewRsrcCurves_uncalibrated, by = "renewresource") %>%
      mutate(available = case_when(
        grade == "grade2" & is.na(accessible) == TRUE ~ available,
        grade == "grade2" & is.na(accessible) == FALSE ~ accessible,
        grade == "grade1" | grade == "grade3" ~ available
      )) %>% select(-accessible) %>%
      group_by(renewresource) %>% mutate(x = cumsum(available)) %>%
      mutate(available = if_else(x >= 2, x, available)) %>%
      select(-x) %>% ungroup() ->
      accessible_water_unsmoothed

    # Step 4
    # make function to expand out the 3-point resource curve to an interpolated 20-point curve
    get_smooth_renewresource <- function(x){
      av <- x$available
      ex <- x$extractioncost
      x_region <- x$region[1]

      rnw_spline <- spline(av, ex, method = "hyman",
                           xout = c(
                             seq(av[1], av[2], length.out = 10),
                             seq(av[2], av[3], length.out = 11))[-10])

      tibble(available = rnw_spline$x,
             extractioncost = rnw_spline$y,
             sub.renewable.resource = "runoff",
             grade = paste0("grade", 1:20),
             region = x_region)
    }

    # apply smooth across all basins using purrr::map on split table
    accessible_water_unsmoothed %>%
      split(.$renewresource) %>%
      map(get_smooth_renewresource) %>%
      map_dfr(magrittr::extract, .id = "renewresource") %>%
      select(one_of(LEVEL2_DATA_NAMES$RenewRsrcCurves)) %>%
      filter(!(grepl("Arctic Ocean", renewresource))) %>%
      # ^^ remove convex case (Artic Ocean)
      bind_rows(filter(accessible_water_unsmoothed,
                       grepl("Arctic Ocean", renewresource))) ->
      L201.RenewRsrcCurves_calib

    # Step 5

    L100.runoff_max_bm3 %>%
      filter(year %in% c(1990, 2005, 2010)) %>%
      group_by(basin_id) %>% summarise(runoff = mean(runoff_max)) ->
      runoff_mean_hist


    access_fraction_uncalibrated %>%
        select(basin_id, access_fraction) %>%
        left_join(aw, by = "basin_id") %>%
        left_join(runoff_mean_hist, by = "basin_id") %>%
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
        left_join(tibble(year = MODEL_BASE_YEARS[MODEL_BASE_YEARS >= 1990],
                         years = diff(MODEL_BASE_YEARS)), by = "year") %>%
        mutate(deficit_total = deficit * years) %>%
        group_by(basin.id) %>% summarise(available = sum(deficit_total)) %>%
        filter(available > 0) %>%
        mutate(grade = "grade hist", price = 0.001) ->
      groundwater_hist

      bind_rows(
        L201.region_basin.first_only %>%
          left_join(L101.groundwater_grades_constrained_bm3,
                    by = c("basin_id" = "basin.id")),
        L201.region_basin.first_only %>%
          left_join(groundwater_hist,
                    by = c("basin_id" = "basin.id")) %>%
          filter(is.na(grade) == F)
      ) %>%
        rename(extractioncost = price) %>%
        mutate(depresource = paste0(basin_name, "-", water_type),
               subresource = "groundwater",
               available = round(available, 5),
               extractioncost = round(extractioncost, 5)) %>%
        select(one_of(LEVEL2_DATA_NAMES$DepRsrcCurves)) %>%
        arrange(region, depresource, extractioncost) ->
        L201.DepRsrcCurves_ground

    # ===================================================

    # Produce outputs


      L201.NodeEquiv %>%
        add_title("Node Equiv") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_legacy_name("L201.NodeEquiv") %>%
        add_precursors("water/basin_ID") ->
        L201.NodeEquiv

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
                       "common/GCAM_region_names",
                       "L103.water_mapping_R_GLU_B_W_Ws_share",
                       "L103.water_mapping_R_B_W_Ws_share") ->
        L201.Rsrc

      L201.RsrcPrice %>%
        add_title("Base year price") %>%
        add_units("1975$") %>%
        add_comments("") %>%
        add_legacy_name("L201.RsrcPrice") %>%
        add_precursors("water/basin_ID",
                       "common/GCAM_region_names",
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

      return_data(L201.NodeEquiv,
                  L201.DeleteUnlimitRsrc,
                  L201.Rsrc,
                  L201.RsrcPrice,
                  L201.RenewRsrcCurves_uncalibrated,
                  L201.GrdRenewRsrcMax_runoff,
                  L201.DepRsrcCurves_ground_uniform,
                  L201.RenewRsrcCurves_calib,
                  L201.DepRsrcCurves_ground)


  } else {
    stop("Unknown command")
  }
}
