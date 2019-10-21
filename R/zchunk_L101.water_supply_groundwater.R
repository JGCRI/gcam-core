#' module_water_L101.water_supply_groundwater
#'
#' Prepare GCAM basin groundwater supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.groundwater_grades_uniform_bm3},
#' \code{L101.groundwater_grades_constrained_bm3}, \code{L101.groundwater_depletion_bm3}. The corresponding file in the
#' original data system was \code{L100.water_supply_runoff.R} (Water level1).
#' @details Prepares groundwater resource curves and sets up groundwater calibration data.
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author ST September 2018
module_water_L101.water_supply_groundwater <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "water/groundwater_uniform",
             FILE = "water/groundwater_constrained",
             FILE = "water/groundwater_trend_gleeson",
             FILE = "water/groundwater_trend_watergap"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.groundwater_grades_uniform_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.groundwater_depletion_bm3"))
  } else if(command == driver.MAKE) {

    . <- base.rsc <- price <- base.prc <-
      alpha <- base.cum <- basin.id <-
      avail <- hist.use <- hist.price <-
      grade <- scenario <- trend_km3PerYr <-
      hi <- nhi <- human_only <- depletion <-
      netDepletion <- NULL

    all_data <- list(...)[[1]]

    # Step 1. Load required inputs

    basin_ids <- get_data(all_data, "water/basin_ID")
    gw_uniform <- get_data(all_data, "water/groundwater_uniform")
    gw_constrained <- get_data(all_data, "water/groundwater_constrained")

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
      arrange(basin.id, price) %>%
      select(basin.id, price, avail) %>%
      group_by(basin.id) %>%
      mutate(avail = lead(avail, default = 0.0),
             grade = paste0("grade", row_number())) %>%
      ungroup() ->
      gw_uniform_unadjusted

    # corrections to cover historical use
    gw_uniform %>% filter(hist.use > 0) %>%
      rename(price = hist.price,
             avail = hist.use) %>%
      mutate(grade = "grade hist") %>%
      select(basin.id, price, avail, grade) %>%
      bind_rows(gw_uniform_unadjusted) %>%
      arrange(basin.id, price) ->
      L101.groundwater_grades_uniform_bm3


    # Step 3: Prepare constrained groundwater
    gw_constrained %>%
      mutate(scenario = scenario) %>%
      # ^^ removes attributes from input
      filter(scenario == water.GROUNDWATER_SCENARIO) %>%
      select(-scenario) ->
      L101.groundwater_grades_constrained_bm3

    # step 4: Prepare groundwater depletion calibration data

    # prepare watergap calibration data
    if(water.GROUNDWATER_CALIBRATION == "watergap"){
      gw_dep %>%
        spread(scenario, trend_km3PerYr) %>%
        mutate(human_only = hi - nhi) %>%
        filter(human_only < 0, hi < 0) %>%
        rename(depletion = human_only) %>%
        mutate(depletion = round(-depletion, water.DIGITS_GROUND_WATER)) %>%
        select(basin.id, depletion) ->
        L101.groundwater_depletion_bm3
    }

    if(water.GROUNDWATER_CALIBRATION == "Gleeson"){
      gw_dep %>%
        rename(depletion = netDepletion) %>%
        filter(depletion > 0) %>%
        mutate(depletion = round(depletion, water.DIGITS_GROUND_WATER)) %>%
        arrange(basin.id) %>% select(basin.id, depletion) ->
        L101.groundwater_depletion_bm3
    }

    # Prepare outputs

    L101.groundwater_grades_uniform_bm3 %>%
      add_title("Uniform groundwater non-renewable resource curves") %>%
      add_units("km^3/yr") %>%
      add_comments("These curves are not based on estimates of actual groundwater volumes") %>%
      add_legacy_name("L101.groundwater_grades_uniform_bm3") %>%
      add_precursors("water/basin_ID",
                     "water/groundwater_uniform") ->
      L101.groundwater_grades_uniform_bm3

    L101.groundwater_grades_constrained_bm3 %>%
      add_title("Realistic groundwater non-renewable resource curves") %>%
      add_units("km^3/yr") %>%
      add_comments("These curves are based on global estimates of groundwater volumes") %>%
      add_legacy_name("L101.groundwater_grades_constrained_bm3") %>%
      add_precursors("water/basin_ID",
                     "water/groundwater_constrained") ->
      L101.groundwater_grades_constrained_bm3

    L101.groundwater_depletion_bm3 %>%
      add_title("Groundwater depletion trends") %>%
      add_units("km^3/yr") %>%
      add_comments("Used for calibration of historical withdrawals") %>%
      add_legacy_name("L101.groundwater_depletion_bm3") %>%
      add_precursors("water/groundwater_trend_watergap",
                     "water/groundwater_trend_gleeson") ->
      L101.groundwater_depletion_bm3


    return_data(L101.groundwater_grades_uniform_bm3,
                L101.groundwater_grades_constrained_bm3,
                L101.groundwater_depletion_bm3)

  } else {
    stop("Unknown command")
  }
}
