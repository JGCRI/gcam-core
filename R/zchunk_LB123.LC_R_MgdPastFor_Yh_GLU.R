#' module_aglu_LB123.LC_R_MgdPastFor_Yh_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.ag_Prod_Mt_R_Past_Y_GLU}, \code{L123.ag_Yield_kgm2_R_Past_Y_GLU}, \code{L123.LC_bm2_R_MgdPast_Yh_GLU}, \code{L123.For_Prod_bm3_R_Y_GLU}, \code{L123.For_Yield_m3m2_R_GLU}, \code{L123.LC_bm2_R_MgdFor_Yh_GLU}. The corresponding file in the
#' original data system was \code{LB123.LC_R_MgdPastFor_Yh_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB123.LC_R_MgdPastFor_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L102.ag_Prod_Mt_R_C_GLU",
             "L108.ag_Feed_Mt_R_C_Y",
             "L110.For_ALL_bm3_R_Y",
             "L120.LC_bm2_R_LT_Yh_GLU",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L121.Yield_kgm2_R_Past_GLU",
             "L101.Pop_thous_R_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.ag_Prod_Mt_R_Past_Y_GLU",
             "L123.ag_Yield_kgm2_R_Past_Y_GLU",
             "L123.LC_bm2_R_MgdPast_Yh_GLU",
             "L123.For_Prod_bm3_R_Y_GLU",
             "L123.For_Yield_m3m2_R_GLU",
             "L123.LC_bm2_R_MgdFor_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L102.ag_Prod_Mt_R_C_GLU <- get_data(all_data, "L102.ag_Prod_Mt_R_C_GLU")
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y") %>% unique
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L121.Yield_kgm2_R_Past_GLU <- get_data(all_data, "L121.Yield_kgm2_R_Past_GLU")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")

    # Part 1: Managed pasture production and land cover
    # Calculate bottom-up estimate of pasture production by region and GLU ( yield times land area )
    # Calculating total pasture grass production by region and GLU
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Pasture", year %in% AGLU_HISTORICAL_YEARS) ->
      L123.LC_bm2_R_Past_Y_GLU

    L123.LC_bm2_R_Past_Y_GLU %>%
      left_join_error_no_match(L121.Yield_kgm2_R_Past_GLU, by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      # Calculate production as land area multiply yield
      mutate(value = value * pasture_yield) ->
      L123.ag_potentialProd_Mt_R_Past_Y_GLU

    # Use this "potential production" to disaggregate actual pastureland production to GLUs
    # NOTE: use pasture grass production by region and GLU to disaggregate regional pasture consumption to GLU
    L123.ag_potentialProd_Mt_R_Past_Y_GLU %>%
      group_by(GCAM_region_ID, Land_Type, year) %>%
      summarise(total = sum(value)) %>%
      ungroup %>%
      right_join(L123.ag_potentialProd_Mt_R_Past_Y_GLU, by = c("GCAM_region_ID", "Land_Type", "year")) %>%
      mutate(frac = value / total) %>%
      select(-total, -value, -pasture_yield) %>%
      # Drop NAs for regions/years with no managed pasture
      replace_na(list(frac = 0)) %>%
      rename(GCAM_commodity = Land_Type) %>%
      left_join_error_no_match(L108.ag_Feed_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(value = value * frac) %>%
      select(-frac) ->
      L123.ag_Prod_Mt_R_Past_Y_GLU

    # Calculate land requirements.
    # Calculate pasture land required to produce grass consumed in pastures. This is managed pasture.
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      rename(Land_Type = GCAM_commodity) %>%
      left_join_error_no_match(L121.Yield_kgm2_R_Past_GLU,
                               by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      mutate(MgdPast = value / pasture_yield) %>%
      select(-value, -pasture_yield) %>%
    # Where managed pasture is greater than assumed threshold percentage of total pasture, reduce the managed pasture land.
    # Output is unaffected so these regions have higher yields.
    # Applying maximum percentage of any region/GLUs pasture that is allowed to be in production (managed).
    # NOTE: In region/GLUs where applicable, this threshold will result in increased pasture yields.
      left_join_error_no_match(L123.LC_bm2_R_Past_Y_GLU,
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      mutate(frac = MgdPast / value) %>%
      replace_na(list(frac = 0)) %>%
      mutate(frac = replace(frac, frac > MAX_MGDPAST_FRAC, MAX_MGDPAST_FRAC),
             # Recalculate managed pasture land, adjusted by assumed maximum portion that can be managed
             MgdPast_adj = value * frac) %>%
      select(-MgdPast, -value) ->
      L123.LC_bm2_R_MgdPast_Y_GLU_adj

    # Calculate pasture yield (after adjustments)
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      left_join_error_no_match(L123.LC_bm2_R_MgdPast_Y_GLU_adj,
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU", "year")) %>%
      mutate(value = value / MgdPast_adj) %>%
      replace_na(list(value = 0)) %>%
      select(-MgdPast_adj, -frac) ->
      L123.ag_Yield_kgm2_R_Past_Y_GLU

    # Multiply "managed" shares in the earliest available year by prior pasture land cover pathway to get historical managed pasture
    # Build managed pasture land use history
    # NOTE: Assume same managed:unmanaged pasture ratio in early land cover years
    L123.LC_bm2_R_MgdPast_Y_GLU_adj %>%
      filter(year == min(AGLU_HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Pasture", year %in% PREAGLU_YEARS),
                 by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      mutate(value = value * frac) %>%
      select(-MgdPast_adj, -frac) ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    L123.LC_bm2_R_MgdPast_Y_GLU_adj %>%
      rename(value = MgdPast_adj) %>%
      select(-frac) %>%
      bind_rows(L123.LC_bm2_R_MgdPast_Yh_GLU) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    # FORESTRY
    # Carbon densities are divided by mature age to get net primary productivity,
    # and used to derive exogenous yields for separating managed/unmanaged forest
    # calculate veg mass of each GLU based on above-ground carbon content of each GLU.
    # Part 2: Managed forest production and land cover
    # NOTE: Use average vegetation carbon densities and mature ages to estimate annual forest biomass production
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      filter(Land_Type == "Forest") %>%
      mutate(VegVolume_m3m2 = veg_c / aglu.AVG_WOOD_DENSITY_KGCM3,
             Yield_m3m2 = VegVolume_m3m2 / `mature age`) ->
      L123.For_Yield_m3m2_R_GLU

    # Disaggregate logging to GLU on the basis of annual biomass production rates
    # NOTE: Use forest biomass production (annual yield) by GLU to disaggregate regional wood production to GLUs
    L120.LC_bm2_R_LT_Yh_GLU %>%
      rename(GCAM_commodity = Land_Type) %>%
      filter(GCAM_commodity == "Forest", year %in% AGLU_HISTORICAL_YEARS) %>%
      left_join_error_no_match(select(L123.For_Yield_m3m2_R_GLU, GCAM_region_ID, GLU, Yield_m3m2),
                               by = c("GCAM_region_ID", "GLU")) %>%
      mutate(value = value * Yield_m3m2) ->
      L123.For_potentialProd_bm3_R_Y_GLU

    # Forest output by GLU = Regional forest output * GLU-wise forest biomass production fraction
    L123.For_potentialProd_bm3_R_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(total = sum(value)) %>%
      ungroup %>%
      right_join(L123.For_potentialProd_bm3_R_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(frac = value / total) %>%
      left_join_error_no_match(L110.For_ALL_bm3_R_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      # Calculate logging production as the regional total times the GLU-wise production fractions
      mutate(value = Prod_bm3 * frac) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L123.For_Prod_bm3_R_Y_GLU

    # Calculate land cover of "managed" forest as the output divided by the yield (net primary production), in each GLU and region
    L123.For_Prod_bm3_R_Y_GLU %>%
      left_join_error_no_match(L123.For_Yield_m3m2_R_GLU,
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU")) %>%
      mutate(MgdFor = value / Yield_m3m2) %>%
      select(GCAM_region_ID, Land_Type = GCAM_commodity, GLU, year, MgdFor) ->
      L123.LC_bm2_R_MgdFor_Y_GLU

    # Use historical population ratios to estimate managed forest in the pre-AGLU years
    # Build managed forest land use history
    # NOTE: Scaling historical managed forest land area with historical population
    L101.Pop_thous_R_Yh %>%
      group_by(GCAM_region_ID) %>%
      mutate(PopRatio = value / value[year == min(AGLU_HISTORICAL_YEARS)]) %>%
      select(-value) %>%
      filter(year %in% PREAGLU_YEARS) ->
      L123.PopRatio_R_Yhh

    # Add the pre-aglu years into the tibble
    L123.For_Prod_bm3_R_Y_GLU %>%
      select(GCAM_region_ID, Land_Type = GCAM_commodity, GLU) %>%
      unique %>%
      repeat_add_columns(tibble(year = PREAGLU_YEARS)) %>%
      bind_rows(L123.LC_bm2_R_MgdFor_Y_GLU) %>%
      group_by(GCAM_region_ID, GLU) %>%
      mutate(MgdFor = replace(MgdFor, year %in% PREAGLU_YEARS, MgdFor[year == min(AGLU_HISTORICAL_YEARS)])) %>%
      ungroup %>%
      left_join(L123.PopRatio_R_Yhh, by = c("GCAM_region_ID", "year")) %>%
      replace_na(list(PopRatio = 1)) %>%
      mutate(MgdFor = MgdFor * PopRatio) %>%
      select(-PopRatio) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    # Where managed forest is greater than assumed maximum percentage of total forest, reduce the managed forest land.
    # Output is unaffected so these regions have higher yields.
    # Apply maximum percentage of any region/GLUs forest that is allowed to be in production (managed)
    # NOTE: In region/GLUs where applicable, this threshold will result in increased forest yields
    L123.LC_bm2_R_MgdFor_Yh_GLU %>%
      left_join_error_no_match(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Forest", year %in% aglu.LAND_COVER_YEARS),
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      mutate(frac = MgdFor / value) %>%
      select(-value, -MgdFor) %>%
      # Re-set missing values to zeroes
      replace_na(list(frac = 0)) %>%
      mutate(frac = replace(frac, frac > MAX_MGDFOR_FRAC, MAX_MGDFOR_FRAC)) %>%
      left_join_error_no_match(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Forest", year %in% aglu.LAND_COVER_YEARS),
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      # Recalculate managed forest land, adjusted by assumed maximum portion that can be managed
      mutate(value = value * frac) %>%
      select(-frac) ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    # Recalculate forestry yield
    L123.For_Prod_bm3_R_Y_GLU %>%
      left_join_error_no_match(rename(L123.LC_bm2_R_MgdFor_Yh_GLU, MgdFor = value),
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU", "year")) %>%
      mutate(value = value / MgdFor) %>%
      select(-MgdFor) ->
      L123.For_Yield_m3m2_R_GLU

    # Set missing values to an assumed minimum forestry yield, from the data where available
    L123.For_Yield_m3m2_R_GLU %>%
      filter(year == min(HISTORICAL_YEARS)) %>%
      select(value) %>%
      na.omit %>%
      min -> min_forest_yield

    L123.For_Yield_m3m2_R_GLU %>%
      replace_na(list(value = min(min_forest_yield))) ->
      L123.For_Yield_m3m2_R_GLU

    # Produce outputs
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y",
                     "L120.LC_bm2_R_LT_Yh_GLU",
                     "L121.Yield_kgm2_R_Past_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.ag_Prod_Mt_R_Past_Y_GLU

    L123.ag_Yield_kgm2_R_Past_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.ag_Yield_kgm2_R_Past_Y_GLU") %>%
      same_precursors_as("L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.ag_Yield_kgm2_R_Past_Y_GLU

    L123.LC_bm2_R_MgdPast_Yh_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.LC_bm2_R_MgdPast_Yh_GLU") %>%
      same_precursors_as("L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    L123.For_Prod_bm3_R_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.For_Prod_bm3_R_Y_GLU") %>%
      add_precursors("L110.For_ALL_bm3_R_Y",
                     "L120.LC_bm2_R_LT_Yh_GLU",
                     "L121.CarbonContent_kgm2_R_LT_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.For_Prod_bm3_R_Y_GLU

    L123.LC_bm2_R_MgdFor_Yh_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.LC_bm2_R_MgdFor_Yh_GLU") %>%
      same_precursors_as("L123.For_Prod_bm3_R_Y_GLU") %>%
      add_precursors("L101.Pop_thous_R_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT) ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    L123.For_Yield_m3m2_R_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data geonerated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.For_Yield_m3m2_R_GLU") %>%
      same_precursors_as("L123.LC_bm2_R_MgdFor_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.For_Yield_m3m2_R_GLU

    return_data(L123.ag_Prod_Mt_R_Past_Y_GLU, L123.ag_Yield_kgm2_R_Past_Y_GLU, L123.LC_bm2_R_MgdPast_Yh_GLU, L123.For_Prod_bm3_R_Y_GLU, L123.For_Yield_m3m2_R_GLU, L123.LC_bm2_R_MgdFor_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
