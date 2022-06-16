# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB123.LC_R_MgdPastFor_Yh_GLU
#'
#' Calculates pasture and forests production, yields and managed land cover by GCAM region / GLU / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.ag_Prod_Mt_R_Past_Y_GLU}, \code{L123.ag_Yield_kgm2_R_Past_Y_GLU}, \code{L123.LC_bm2_R_MgdPast_Yh_GLU}, \code{L123.For_Prod_bm3_R_Y_GLU}, \code{L123.For_Yield_m3m2_R_GLU}, \code{L123.LC_bm2_R_MgdFor_Yh_GLU}. The corresponding file in the
#' original data system was \code{LB123.LC_R_MgdPastFor_Yh_GLU.R} (aglu level1).
#' @details This chunk calculates pasture and forests production and yields by GCAM region / GLU / year,
#' and the managed pasture and forests land cover by GCAM region / GLU / historical year.
#' Managed land area for both are adjusted below the threshold percentage of total pasture and forest land,
#' and yields are increased so that production stays unchanged for those regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate right_join select summarise
#' @importFrom tidyr replace_na
#' @author RC August 2017
module_aglu_LB123.LC_R_MgdPastFor_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L108.ag_Feed_Mt_R_C_Y",
             "L110.For_ALL_bm3_R_Y",
             "L120.LC_bm2_R_LT_Yh_GLU",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L121.Yield_kgm2_R_Past_GLU",
             "L101.Pop_thous_R_Yh",
             "L120.LC_soil_veg_carbon_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.ag_Prod_Mt_R_Past_Y_GLU",
             "L123.ag_Yield_kgm2_R_Past_Y_GLU",
             "L123.LC_bm2_R_MgdPast_Yh_GLU",
             "L123.For_Prod_bm3_R_Y_GLU",
             "L123.For_Yield_m3m2_R_GLU",
             "L123.LC_bm2_R_MgdFor_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_commodity <- GCAM_region_ID <- value <- year <- Land_Type <- pasture_yield <-
      frac <- total <- MgdPast <- MgdPast_adj <- veg_c <- VegVolume_m3m2 <- PopRatio <-
      `mature age` <- GLU <- Yield_m3m2 <- Prod_bm3 <- MgdFor <- MgdFor_adj <- NULL   # silence package check notes

    # Load required inputs
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y")
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")

    if(aglu.CARBON_DATA_SOURCE=="moirai"){

      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L120.LC_soil_veg_carbon_GLU")
    }else{
      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")

    }
    L121.Yield_kgm2_R_Past_GLU <- get_data(all_data, "L121.Yield_kgm2_R_Past_GLU")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")

    # Part 1: Pasture production, yield and managed pasture land

    # Calculate bottom-up estimates of total pasture grass production by region and GLU.
    # Start with total pasture land data
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Pasture", year %in% aglu.AGLU_HISTORICAL_YEARS) ->
      L123.LC_bm2_R_Past_Y_GLU

    # Match total pasture land area and yield data to calculate potential production
    L123.LC_bm2_R_Past_Y_GLU %>%
      left_join_error_no_match(L121.Yield_kgm2_R_Past_GLU, by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      # Calculate potential production as total land times yield
      mutate(value = value * pasture_yield) ->
      L123.ag_potentialProd_Mt_R_Past_Y_GLU

    # Use this "potential production" by region and GLU to disaggregate actual pasture land production to GLUs.
    # Assume regional consumption equals managed pasture land production.
    L123.ag_potentialProd_Mt_R_Past_Y_GLU %>%
      # Calculate regional total potential production
      group_by(GCAM_region_ID, Land_Type, year) %>%
      summarise(total = sum(value)) %>%
      ungroup %>%
      # Match in potential production by region and GLU to calculate the fraction
      right_join(L123.ag_potentialProd_Mt_R_Past_Y_GLU, by = c("GCAM_region_ID", "Land_Type", "year")) %>%
      # Calculate the GLU to region fraction
      mutate(frac = value / total) %>%
      select(-total, -value, -pasture_yield) %>%
      # Drop NAs for GLUs/years with no managed pasture
      replace_na(list(frac = 0)) %>%
      rename(GCAM_commodity = Land_Type) %>%
      # Match in regional pasture consumption (managed pasture land production) for disaggregation
      # Note: left_join_error_no_match fails timeshift, due to NAs
      left_join(L108.ag_Feed_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      # Disaggregate production to GLUs
      mutate(value = value * frac) %>%
      select(-frac) ->
      L123.ag_Prod_Mt_R_Past_Y_GLU

    # Calculate managed pasture land required using actual production and yields,
    # and make adjustements: where managed pasture land is greater than
    # assumed threshold percentage of total pasture land, reduce managed pasture land area.
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      rename(Land_Type = GCAM_commodity) %>%
      # Match in pasture yields by GLU
      left_join_error_no_match(L121.Yield_kgm2_R_Past_GLU, by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      # Calculate managed pasture land required as actual production devided by yields
      mutate(MgdPast = value / pasture_yield) %>%
      select(-value, -pasture_yield) %>%
      # Match in total pasture land
      left_join_error_no_match(L123.LC_bm2_R_Past_Y_GLU,
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      # Calculate the percentage of managed to total pasture land
      mutate(frac = MgdPast / value) %>%
      replace_na(list(frac = 0)) %>%
      # Apply maximum percentage of any region/GLUs pasture that is allowed to be in production (managed)
      mutate(frac = replace(frac, frac > aglu.MAX_MGDPAST_FRAC, aglu.MAX_MGDPAST_FRAC),
             # Recalculate the adjusted managed pasture land
             MgdPast_adj = value * frac) %>%
      select(-MgdPast, -value) ->
      L123.LC_bm2_R_MgdPast_Y_GLU_adj

    # Calculate pasture yield (after adjustments for managed pasture land)
    # Note: for GLUs where threshold percentage of total pasture is applied,
    # they have increased pasture yields, because prodction is unaffected.
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      # Match in the adjusted managed pasture land
      left_join_error_no_match(L123.LC_bm2_R_MgdPast_Y_GLU_adj,
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU", "year")) %>%
      # Calculate adjusted yields as production divided by managed pasture land
      mutate(value = value / MgdPast_adj) %>%
      replace_na(list(value = 0)) %>%
      select(-MgdPast_adj, -frac) ->
      L123.ag_Yield_kgm2_R_Past_Y_GLU

    # Build managed pasture land use in pre-aglu historical years
    # Assume the same managed:unmanaged pasture ratio from the earliest available aglu year
    L123.LC_bm2_R_MgdPast_Y_GLU_adj %>%
      # Filter managed paster land in the earliest available aglu year
      filter(year == min(aglu.AGLU_HISTORICAL_YEARS)) %>%
      select(-year) %>%
      # Match in pre-aglu historical total pasture land data
      right_join(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Pasture", year %in% aglu.PREAGLU_YEARS),
                 by = c("GCAM_region_ID", "Land_Type", "GLU")) %>%
      # Multiply the same "managed" shares by prior total pasture land to get pre-aglu historical managed pasture
      mutate(value = value * frac) %>%
      select(-MgdPast_adj, -frac) ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    # Combine all historical managed pasture land cover
    L123.LC_bm2_R_MgdPast_Y_GLU_adj %>%
      rename(value = MgdPast_adj) %>%
      select(-frac) %>%
      bind_rows(L123.LC_bm2_R_MgdPast_Yh_GLU) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    # Part 2: Forestry production, yield and managed forest land cover

    # Use average vegetation carbon densities and mature ages to estimate annual forest biomass production,
    # and used to derive exogenous yields for separating managed/unmanaged forest.
     L121.CarbonContent_kgm2_R_LT_GLU %>%
      filter(Land_Type == "Forest") %>%
      # Calculate veg mass of each GLU based on above-ground carbon content of each GLU
      mutate(VegVolume_m3m2 = (veg_c*aglu.CVEG_MULT_UNMGDPAST_MGDPAST) / aglu.AVG_WOOD_DENSITY_KGCM3,
             # Carbon densities are divided by mature age to get net primary productivity
             Yield_m3m2 = VegVolume_m3m2 / `mature age`) ->
      L123.For_Yield_m3m2_R_GLU

    # Use total forest land and yields to calculate potential forest biomass production
    L120.LC_bm2_R_LT_Yh_GLU %>%
      rename(GCAM_commodity = Land_Type) %>%
      # Filter total forest land
      filter(GCAM_commodity == "Forest", year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # Match in forest primary yields
      left_join_error_no_match(select(L123.For_Yield_m3m2_R_GLU, GCAM_region_ID, GLU, Yield_m3m2),
                               by = c("GCAM_region_ID", "GLU")) %>%
      # Calculate potential forest biomass production as total forest land times yields
      mutate(value = value * Yield_m3m2) ->
      L123.For_potentialProd_bm3_R_Y_GLU

    # Use the GLU fraction of potential forest biomass production to disaggregate regional wood production to GLU
    # Forest output by GLU = Regional forest output * GLU-wise forest biomass production fraction
    L123.For_potentialProd_bm3_R_Y_GLU %>%
      # Calculate regional total forest biomass production
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(total = sum(value)) %>%
      ungroup %>%
      # Match in forest biomass production by GLU
      right_join(L123.For_potentialProd_bm3_R_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      # Calculate the GLU to regional fraction of forest biomass production
      mutate(frac = value / total) %>%
      # Match in regional wood production
      left_join_error_no_match(L110.For_ALL_bm3_R_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      # Calculate logging production as the regional total times the GLU-wise forest biomass production fractions
      mutate(value = Prod_bm3 * frac) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L123.For_Prod_bm3_R_Y_GLU

    # Calculate land cover of "managed" forest as the wood production divided by yield (net primary productivity)
    L123.For_Prod_bm3_R_Y_GLU %>%
      left_join_error_no_match(L123.For_Yield_m3m2_R_GLU,
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU")) %>%
      # Managed forest land cover as wood production divided by yields
      mutate(MgdFor = value / Yield_m3m2) %>%
      select(GCAM_region_ID, Land_Type = GCAM_commodity, GLU, year, MgdFor) ->
      L123.LC_bm2_R_MgdFor_Y_GLU

    # Build managed forest land use in pre-aglu historical years
    # Use historical population ratios for scaling to estimate managed forest in the pre-aglu years
    L101.Pop_thous_R_Yh %>%
      group_by(GCAM_region_ID) %>%
      # Calculate the population ratio of pre-aglu historical years to the earliest aglu year for each region
      mutate(PopRatio = value / value[year == min(aglu.AGLU_HISTORICAL_YEARS)]) %>%
      ungroup %>%
      select(-value) %>%
      filter(year %in% aglu.PREAGLU_YEARS) ->
      L123.PopRatio_R_Yhh

    # Scale managed forest land to pre-aglu historical years by population
    # Start with building a tibble with region x GLUs that have wood production
    L123.For_Prod_bm3_R_Y_GLU %>%
      select(GCAM_region_ID, Land_Type = GCAM_commodity, GLU) %>%
      unique %>%
      # Copy to pre-aglu years
      repeat_add_columns(tibble(year = as.integer(aglu.PREAGLU_YEARS))) %>%
      # Bind with managed forest land data in aglu years
      bind_rows(L123.LC_bm2_R_MgdFor_Y_GLU) %>%
      group_by(GCAM_region_ID, GLU) %>%
      # Copy the managed forest land in earliest aglu year to all pre-aglu years for each region x GLU
      mutate(MgdFor = replace(MgdFor, year %in% aglu.PREAGLU_YEARS, MgdFor[year == min(aglu.AGLU_HISTORICAL_YEARS)])) %>%
      ungroup %>%
      # Match in the population ratio by region
      left_join(L123.PopRatio_R_Yhh, by = c("GCAM_region_ID", "year")) %>%
      # For aglu years, no need to scale, so set the ratio to one
      replace_na(list(PopRatio = 1)) %>%
      # Scale managed forest land based on population in pre-aglu years
      mutate(MgdFor = MgdFor * PopRatio) %>%
      select(-PopRatio) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    # Adjust managed forest land: where managed forest is greater than
    # assumed maximum percentage of total forest, reduce the managed forest land.
    L123.LC_bm2_R_MgdFor_Yh_GLU %>%
      # Match in total forest land
      left_join_error_no_match(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Forest", year %in% aglu.LAND_COVER_YEARS),
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      # Calculate the fraction of managed to total forest land
      mutate(frac = MgdFor / value) %>%
      select(-value, -MgdFor) %>%
      # Re-set missing values to zeroes
      replace_na(list(frac = 0)) %>%
      # Apply maximum percentage of any region/GLUs forest that is allowed to be in production (managed)
      mutate(frac = replace(frac, frac > aglu.MAX_MGDFOR_FRAC, aglu.MAX_MGDFOR_FRAC)) %>%
      # Match in total forest land again to calculate the adjusted managed forest land
      left_join_error_no_match(filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Forest", year %in% aglu.LAND_COVER_YEARS),
                               by = c("GCAM_region_ID", "Land_Type", "GLU", "year")) %>%
      # Recalculate managed forest land, adjusted by assumed maximum portion that can be managed
      mutate(value = value * frac) %>%
      select(-frac) ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    # Recalculate forestry yield (after adjustments for managed forest land)
    # Note: for region/GLUs where threshold percentage of total forest is applied,
    # they have increased forest yields, because prodction is unaffected.
    L123.For_Prod_bm3_R_Y_GLU %>%
      left_join_error_no_match(rename(L123.LC_bm2_R_MgdFor_Yh_GLU, MgdFor_adj = value),
                               by = c("GCAM_region_ID", "GCAM_commodity" = "Land_Type", "GLU", "year")) %>%
      # Re-calculate yields with production and the adjusted managed forest land
      mutate(value = value / MgdFor_adj) %>%
      select(-MgdFor_adj) ->
      L123.For_Yield_m3m2_R_GLU

    # Set missing values to an assumed minimum forestry yield (from the data where available)
    L123.For_Yield_m3m2_R_GLU %>%
      # Use the yields in final historical year
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(value) %>%
      na.omit %>%
      # Get the minimum yield value
      min -> min_forest_yield
    # Replace missing value with the minimum yield value
    L123.For_Yield_m3m2_R_GLU %>%
      replace_na(list(value = min_forest_yield)) ->
      L123.For_Yield_m3m2_R_GLU

    # Produce outputs
    L123.ag_Prod_Mt_R_Past_Y_GLU %>%
      add_title("Pasture production by GCAM region / year / GLU") %>%
      add_units("Mt") %>%
      add_comments("Calculate potential production by region and GLU as total pasture land area times yield") %>%
      add_comments("Use the GLU share of potential production to disaggregate regional managed land production to GLUs") %>%
      add_comments("Use regional consumption for managed pasture land production") %>%
      add_legacy_name("L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y",
                     "L120.LC_bm2_R_LT_Yh_GLU",
                     "L121.Yield_kgm2_R_Past_GLU") ->
      L123.ag_Prod_Mt_R_Past_Y_GLU

    L123.LC_bm2_R_MgdPast_Yh_GLU %>%
      add_title("Managed pasture land cover by GCAM region / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Calculate managed pasture land as managed pasture land production divided by yields") %>%
      add_comments("Reduce managed pasture land no greater than the assumed threshold percentage of total pasture land") %>%
      add_comments("The same managed land share in the earliest aglu year is assumed to pre-aglu historical years") %>%
      add_legacy_name("L123.LC_bm2_R_MgdPast_Yh_GLU") %>%
      same_precursors_as("L123.ag_Prod_Mt_R_Past_Y_GLU") ->
      L123.LC_bm2_R_MgdPast_Yh_GLU

    L123.ag_Yield_kgm2_R_Past_Y_GLU %>%
      add_title("Pasture yield by GCAM region / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Calculate yields as managed pasture land production divided by adjusted managed pasture land") %>%
      add_comments("Increase yields where managed pasture land is lowered to the threshold percentage of total pasture land") %>%
      add_legacy_name("L123.ag_Yield_kgm2_R_Past_Y_GLU") %>%
      same_precursors_as("L123.ag_Prod_Mt_R_Past_Y_GLU") ->
      L123.ag_Yield_kgm2_R_Past_Y_GLU

    L123.For_Prod_bm3_R_Y_GLU %>%
      add_title("Forest production by GCAM region / year / GLU") %>%
      add_units("bm3") %>%
      add_comments("Calculate potential forest biomass production by region and GLU as total forest land area times yield") %>%
      add_comments("Use the GLU share of forest biomass production to disaggregate regional wood production to GLUs") %>%
      add_comments("Forest yields are calculated as carbon densities are divided by mature age") %>%
      add_legacy_name("L123.For_Prod_bm3_R_Y_GLU") %>%
      add_precursors("L110.For_ALL_bm3_R_Y",
                     "L120.LC_bm2_R_LT_Yh_GLU",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU") ->
      L123.For_Prod_bm3_R_Y_GLU

    L123.LC_bm2_R_MgdFor_Yh_GLU %>%
      add_title("Managed forest land cover by GCAM region / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Calculate managed forest land as wood production divided by yields") %>%
      add_comments("Managed forest land share in the pre-aglu historical years are scaled by population") %>%
      add_comments("Reduce managed forest land no greater than the assumed threshold percentage of total forest land") %>%
      add_legacy_name("L123.LC_bm2_R_MgdFor_Yh_GLU") %>%
      same_precursors_as("L123.For_Prod_bm3_R_Y_GLU") %>%
      add_precursors("L101.Pop_thous_R_Yh") ->
      L123.LC_bm2_R_MgdFor_Yh_GLU

    L123.For_Yield_m3m2_R_GLU %>%
      add_title("Forest yield by GCAM region / year / GLU") %>%
      add_units("m2/m3") %>%
      add_comments("Calculate yields as wood production divided by adjusted managed forest land") %>%
      add_comments("Increase yields where managed forest land is lowered to the threshold percentage of total forest land") %>%
      add_comments("Missing yields are replaced with the assumed minimun yield value") %>%
      add_legacy_name("L123.For_Yield_m3m2_R_GLU") %>%
      same_precursors_as("L123.LC_bm2_R_MgdFor_Yh_GLU") ->
      L123.For_Yield_m3m2_R_GLU

    return_data(L123.ag_Prod_Mt_R_Past_Y_GLU, L123.ag_Yield_kgm2_R_Past_Y_GLU, L123.LC_bm2_R_MgdPast_Yh_GLU, L123.For_Prod_bm3_R_Y_GLU, L123.For_Yield_m3m2_R_GLU, L123.LC_bm2_R_MgdFor_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
