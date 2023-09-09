# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L101.water_supply_groundwater
#'
#' Prepare GCAM basin groundwater supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.DepRsrcCurves_ground_uniform_bm3},
#' \code{L101.groundwater_grades_constrained_bm3}, \code{L101.groundwater_depletion_bm3}. The corresponding file in the
#' original data system was \code{L100.water_supply_runoff.R} (Water level1).
#' @details Prepares groundwater resource curves and sets up groundwater calibration data.
#' @importFrom tibble tibble
#' @importFrom dplyr arrange bind_rows filter group_by mutate rename row_number select ungroup
#' @importFrom tidyr spread
#' @author ST September 2018
module_water_L101.water_supply_groundwater <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/aquastat_ctry",
             FILE = "water/groundwater_uniform",
             FILE = "water/groundwater_trend_gleeson",
             FILE = "water/groundwater_trend_watergap",
             FILE = "water/superwell_groundwater_cost_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.DepRsrcCurves_ground_uniform_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.groundwater_depletion_bm3"))
  } else if(command == driver.MAKE) {

    . <- base.rsc <- price <- base.prc <-
      alpha <- base.cum <- GCAM_basin_ID <-
      avail <- hist.use <- hist.price <-
      grade <- scenario <- trend_km3PerYr <-
      hi <- nhi <- human_only <- depletion <-
      netDepletion <- Country <- iso <- cost_bin <-
      lower_cost <- upper_cost <-
      grade <- elec_EJ <- elec_coef <- Superwell_country <-
      GCAM_region_ID <- minNEcost <- available <-
      minNEcost_bilUSD <- maxNEcost_bilUSD <- maxNEcost <-
      lower <- upper <- range_mult <- NULL

    all_data <- list(...)[[1]]

    # Step 1. Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    aquastat_ctry <- get_data(all_data, "water/aquastat_ctry", strip_attributes = TRUE)
    gw_uniform <- get_data(all_data, "water/groundwater_uniform", strip_attributes = TRUE)
    superwell_groundwater_cost_elec <- get_data(all_data, "water/superwell_groundwater_cost_elec", strip_attributes = TRUE)

    # throw error if water.GROUNDWATER_CALIBRATION is incorrectly referenced in constants.R
    if(!(water.GROUNDWATER_CALIBRATION %in% c("watergap", "gleeson"))){
      stop("groundwater_calibration (see constants.R) must be watergap or gleeson")
    }

    if(water.GROUNDWATER_CALIBRATION == "watergap"){
      gw_dep <- get_data(all_data, "water/groundwater_trend_watergap")
    }
    if(water.GROUNDWATER_CALIBRATION == "gleeson"){
      gw_dep <- get_data(all_data, "water/groundwater_trend_gleeson")
    }

    # Step 2: Compute uniform groundwater grades (see Kim et al., 2016)

    gw_uniform %>%
      # add price points and expand out all basins to required number of grades
      # prices are from base.prc, base.prc * water.GROUNDWATER_MAX_PRICE_INC on a log interval
      repeat_add_columns(tibble(price=seq(log(1),
                                          log(water.GROUNDWATER_MAX_PRICE_INC),
                                          length.out = water.GROUNDWATER_UNIFORM_GRADES))) %>%
      mutate(price = exp(price + log(base.prc))) ->
      gw_uniform_grade_expand

    gw_uniform_grade_expand %>%
      mutate(avail = base.rsc * (price / base.prc) ^
               (alpha * water.GROUNDWATER_BETA) - base.cum) %>%
      arrange(GCAM_basin_ID, price) %>%
      select(GCAM_basin_ID, price, avail) %>%
      group_by(GCAM_basin_ID) %>%
      mutate(avail = dplyr::lead(avail, default = 0.0),
             grade = paste0("grade", row_number())) %>%
      ungroup() ->
      gw_uniform_unadjusted

    # corrections to cover historical use
    gw_uniform %>% filter(hist.use > 0) %>%
      rename(price = hist.price,
             avail = hist.use) %>%
      mutate(grade = "grade hist") %>%
      select(GCAM_basin_ID, price, avail, grade) %>%
      bind_rows(gw_uniform_unadjusted) %>%
      arrange(GCAM_basin_ID, price) ->
      L101.DepRsrcCurves_ground_uniform_bm3


    # Step 3: Prepare constrained groundwater
    # prepare the country mapping list
    superwell_ctry <- select(aquastat_ctry, Country = Superwell_country, iso) %>%
      filter(!is.na(Country)) %>%
      distinct()

    # Re-map the country for basin #103 from China to Taiwan
    L101.Superwell_costcurves_ctry <- superwell_groundwater_cost_elec %>%
      filter(!is.na(Country)) %>%
      mutate(Country = if_else(GCAM_basin_ID == 103, "Taiwan", Country)) %>%
      left_join_error_no_match(superwell_ctry, by = "Country") %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      mutate(cost_bin = if_else(cost_bin == "> 5", "(5, 5]", cost_bin))

    # Extract the grade numbers from the available levels
    grades <- tibble(cost_bin = sort(unique(L101.Superwell_costcurves_ctry$cost_bin)))
    grades$grade <-paste0("grade", 1:nrow(grades))
    grades$grade <- factor(grades$grade, levels = grades$grade)

    L101.groundwater_grades_constrained_bm3 <- L101.Superwell_costcurves_ctry %>%
      mutate(minNEcost_bilUSD = minNEcost * available,
             maxNEcost_bilUSD = maxNEcost * available,
             elec_EJ = elec_coef * CONV_KWH_GJ * available) %>%
      left_join_error_no_match(grades, by = "cost_bin") %>%
      group_by(GCAM_region_ID, GCAM_basin_ID, grade) %>%
      summarise(available = sum(available),
                minNEcost_bilUSD = sum(minNEcost_bilUSD),
                maxNEcost_bilUSD = sum(maxNEcost_bilUSD),
                elec_EJ = sum(elec_EJ)) %>%
      ungroup() %>%
      mutate(lower_cost = minNEcost_bilUSD / available,
             upper_cost = maxNEcost_bilUSD / available,
             elec_coef = elec_EJ / available) %>%
      # We need to guard against lower and upper costs that are "the same" as that
      # causes a discontinuity in GCAM.  This can happen because the bins are based
      # on total however lower/upper costs are based on just the non-energy costs
      # and in GCAM we can only use the average electricity coefficient.  As a work
      # around in these cases we arbitrarily increase the upper cost to have the same
      # difference as the original total cost.
      left_join_error_no_match(grades %>%
                                 separate(cost_bin, c("lower", "upper"), ',') %>%
                                 mutate(lower = as.numeric(gsub('^.', '', lower)),
                                        upper = as.numeric(gsub('.$', '', upper)),
                                        range_mult=(upper/lower)) %>%
                                 select(grade, range_mult), by="grade") %>%
      mutate(upper_cost = if_else(round(lower_cost, water.DIGITS_GROUND_WATER_RSC) == round(upper_cost, water.DIGITS_GROUND_WATER_RSC),
                                  lower_cost * range_mult,
                                  upper_cost)) %>%
      select(GCAM_region_ID, GCAM_basin_ID, grade, lower_cost, upper_cost, available, elec_coef)


    # step 4: Prepare groundwater depletion calibration data

    # prepare watergap calibration data
    if(water.GROUNDWATER_CALIBRATION == "watergap"){
      gw_dep %>%
        spread(scenario, trend_km3PerYr) %>%
        mutate(human_only = hi - nhi) %>%
        filter(human_only < 0, hi < 0) %>%
        rename(depletion = human_only) %>%
        mutate(depletion = -1 * depletion) %>%
        select(GCAM_basin_ID, depletion) ->
        L101.groundwater_depletion_bm3
    }

    if(water.GROUNDWATER_CALIBRATION == "gleeson"){
      gw_dep %>%
        rename(depletion = netDepletion) %>%
        filter(depletion > 0) %>%
        arrange(GCAM_basin_ID) %>%
        select(GCAM_basin_ID, depletion) ->
        L101.groundwater_depletion_bm3
    }

    # Prepare outputs

    L101.DepRsrcCurves_ground_uniform_bm3 %>%
      add_title("Uniform groundwater non-renewable resource curves") %>%
      add_units("km^3/yr") %>%
      add_comments("These curves are not based on estimates of actual groundwater volumes") %>%
      add_legacy_name("L101.DepRsrcCurves_ground_uniform_bm3") %>%
      add_precursors("water/groundwater_uniform") ->
      L101.DepRsrcCurves_ground_uniform_bm3

    L101.groundwater_grades_constrained_bm3 %>%
      add_title("Realistic groundwater non-renewable resource curves") %>%
      add_units("km^3/yr") %>%
      add_comments("These curves are based on global estimates of groundwater volumes") %>%
      add_legacy_name("L101.groundwater_grades_constrained_bm3") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/aquastat_ctry",
                     "water/superwell_groundwater_cost_elec") ->
      L101.groundwater_grades_constrained_bm3

    L101.groundwater_depletion_bm3 %>%
      add_title("Groundwater depletion trends") %>%
      add_units("km^3/yr") %>%
      add_comments("Used for calibration of historical withdrawals") %>%
      add_legacy_name("L101.groundwater_depletion_bm3") %>%
      add_precursors("water/groundwater_trend_watergap",
                     "water/groundwater_trend_gleeson") ->
      L101.groundwater_depletion_bm3


    return_data(L101.DepRsrcCurves_ground_uniform_bm3,
                L101.groundwater_grades_constrained_bm3,
                L101.groundwater_depletion_bm3)

  } else {
    stop("Unknown command")
  }
}
