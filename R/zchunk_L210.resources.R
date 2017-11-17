#' module_energy_L210.resources
#'
#' Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DepRsrc}, \code{L210.RenewRsrc}, \code{L210.UnlimitRsrc}, \code{L210.DepRsrcPrice}, \code{L210.RenewRsrcPrice},
#' \code{L210.UnlimitRsrcPrice}, \code{L210.DepRsrcTechChange}, \code{L210.SmthRenewRsrcTechChange}, \code{L210.DepRsrcCalProd}, \code{L210.DepRsrcCurves_fos},
#' \code{L210.DepRsrcCurves_U}, \code{L210.SmthRenewRsrcCurves_MSW}, \code{L210.SmthRenewRsrcCurves_wind}, \code{L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV},
#' \code{L210.GrdRenewRsrcCurves_geo}, \code{L210.GrdRenewRsrcMax_geo}, \code{L210.GrdRenewRsrcCurves_EGS}, \code{L210.GrdRenewRsrcMax_EGS},
#' \code{L210.GrdRenewRsrcCurves_tradbio}, \code{L210.GrdRenewRsrcMax_tradbio}, \code{L210.DepRsrcTechChange_SSP1}, \code{L210.DepRsrcEnvironCost_SSP1},
#' \code{L210.DepRsrcTechChange_SSP2}, \code{L210.DepRsrcEnvironCost_SSP2}, \code{L210.DepRsrcTechChange_SSP3}, \code{L210.DepRsrcEnvironCost_SSP3},
#' \code{L210.DepRsrcTechChange_SSP4}, \code{L210.DepRsrcEnvironCost_SSP4}, \code{L210.DepRsrcTechChange_SSP5}, \code{L210.DepRsrcEnvironCost_SSP5}.
#' The corresponding file in the original data system was \code{L210.resources.R} (energy level2).
#' @details Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH November 2017
module_energy_L210.resources <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "energy/A10.rsrc_info",
             FILE = "energy/A10.subrsrc_info",
             FILE = "energy/A10.TechChange",
             FILE = "energy/A10.TechChange_SSPs",
             FILE = "energy/A10.EnvironCost_SSPs",
             FILE = "energy/A15.roofPV_TechChange",
             "L111.RsrcCurves_EJ_R_Ffos",
             "L111.Prod_EJ_R_F_Yh",
             "L112.RsrcCurves_Mt_R_U",
             "L113.RsrcCurves_EJ_R_MSW",
             "L114.RsrcCurves_EJ_R_wind",
             "L115.RsrcCurves_EJ_R_roofPV",
             "L116.RsrcCurves_EJ_R_geo",
             "L116.RsrcCurves_EJ_R_EGS",
             "L117.RsrcCurves_EJ_R_tradbio",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.DepRsrc",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.DepRsrcPrice",
             "L210.RenewRsrcPrice",
             "L210.UnlimitRsrcPrice",
             "L210.DepRsrcTechChange",
             "L210.SmthRenewRsrcTechChange",
             "L210.DepRsrcCalProd",
             "L210.DepRsrcCurves_fos",
             "L210.DepRsrcCurves_U",
             "L210.SmthRenewRsrcCurves_MSW",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo",
             "L210.GrdRenewRsrcCurves_EGS",
             "L210.GrdRenewRsrcMax_EGS",
             "L210.GrdRenewRsrcCurves_tradbio",
             "L210.GrdRenewRsrcMax_tradbio",
             "L210.DepRsrcTechChange_SSP1",
             "L210.DepRsrcEnvironCost_SSP1", # Units
             "L210.DepRsrcTechChange_SSP2",
             "L210.DepRsrcEnvironCost_SSP2", # Units
             "L210.DepRsrcTechChange_SSP3",
             "L210.DepRsrcEnvironCost_SSP3", # Units
             "L210.DepRsrcTechChange_SSP4",
             "L210.DepRsrcEnvironCost_SSP4", # Units
             "L210.DepRsrcTechChange_SSP5",
             "L210.DepRsrcEnvironCost_SSP5")) # Units
  } else if(command == driver.MAKE) {

    # Silence package checks
    . <- SSP <- year.fillout <- L210.DepRsrcTechChange_SSP1 <- L210.DepRsrcEnvironCost_SSP1 <- year <-
      L210.DepRsrcTechChange_SSP2 <- L210.DepRsrcEnvironCost_SSP2 <- L210.DepRsrcTechChange_SSP3 <-
      L210.DepRsrcEnvironCost_SSP3 <- L210.DepRsrcTechChange_SSP4 <- L210.DepRsrcTechChange_SSP5 <-
      L210.DepRsrcEnvironCost_SSP5 <- available <- cal.production <- capacity.factor <- curve.exponent <-
      depresource <- environCost <- extractioncost <- fuel <- gdpSupplyElast <- grade <- market <- value <-
      maxSubResource <- mid.price <- object <- `output-unit` <- `price-unit` <- region <- resource <-
      resource_type <- scenario <-subResourceCapacityFactor <- subresource <- subresource_type <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info") %>%
      gather_years
    A10.subrsrc_info <- get_data(all_data, "energy/A10.subrsrc_info")
    A10.TechChange <- get_data(all_data, "energy/A10.TechChange") %>%
      gather_years
    A10.TechChange_SSPs <- get_data(all_data, "energy/A10.TechChange_SSPs") %>%
      gather_years
    A10.EnvironCost_SSPs <- get_data(all_data, "energy/A10.EnvironCost_SSPs") %>%
      gather_years
    A15.roofPV_TechChange <- get_data(all_data, "energy/A15.roofPV_TechChange") %>%
      gather_years
    L111.RsrcCurves_EJ_R_Ffos <- get_data(all_data, "L111.RsrcCurves_EJ_R_Ffos")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
    L112.RsrcCurves_Mt_R_U <- get_data(all_data, "L112.RsrcCurves_Mt_R_U")
    L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW")
    L114.RsrcCurves_EJ_R_wind <- get_data(all_data, "L114.RsrcCurves_EJ_R_wind")
    L115.RsrcCurves_EJ_R_roofPV <- get_data(all_data, "L115.RsrcCurves_EJ_R_roofPV")
    L116.RsrcCurves_EJ_R_geo <- get_data(all_data, "L116.RsrcCurves_EJ_R_geo")
    L116.RsrcCurves_EJ_R_EGS <- get_data(all_data, "L116.RsrcCurves_EJ_R_EGS")
    L117.RsrcCurves_EJ_R_tradbio <- get_data(all_data, "L117.RsrcCurves_EJ_R_tradbio")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # ===================================================
    # A. Output unit, price unit, market
    L210.rsrc_info <- A10.rsrc_info %>%
      # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Remove traditional biomass from regions where it is not currently used
      filter(!(region %in% A_regions$region[A_regions$tradbio_region == 0] & resource == "traditional biomass")) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L210.DepRsrc: output unit, price unit, and market for depletable resources
    L210.DepRsrc <- L210.rsrc_info %>%
      filter(resource_type == "depresource") %>%
      select(region, depresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RenewRsrc: output unit, price unit, and market for renewable resources
    L210.RenewRsrc <- L210.rsrc_info %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.UnlimitRsrc: output unit, price unit, and market for unlimited resources
    L210.UnlimitRsrc <- L210.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market, capacity.factor) %>%
      distinct()

    # L210.DepRsrcPrice: historical prices for depletable resources
    L210.DepRsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "depresource",
             year %in% BASE_YEARS) %>%
      select(region, depresource = resource, year, price = value)

    # L210.RenewRsrcPrice: historical prices for renewable resources
    L210.RenewRsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "renewresource",
             year %in% BASE_YEARS) %>%
      select(region, renewresource = resource, year, price = value)

    # L210.UnlimitRsrcPrice: prices for unlimited resources
    L210.UnlimitRsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "unlimited-resource",
             year %in% BASE_YEARS) %>%
      select(region, unlimited.resource = resource, year, price = value)

    # B. Tech change
    # Repeat and add region to assumed techchange tables
    L210.rsrc_TechChange <- A10.TechChange%>%
      repeat_add_columns(GCAM_region_names) %>%
      # Add subresource type
      left_join_error_no_match(A10.subrsrc_info, by = c("resource", "subresource"))

    # Rooftop PV: follow same steps
    L210.roofPV_TechChange <- A15.roofPV_TechChange %>%
      repeat_add_columns(GCAM_region_names) %>%
      # Add subresource type
      left_join_error_no_match(A10.subrsrc_info, by = c("resource", "subresource"))

    # Combine these two tables
    L210.dep_rsrc_TechChange <- bind_rows(L210.rsrc_TechChange, L210.roofPV_TechChange) %>%
      filter(subresource_type == "subresource")

    L210.renew_rsrc_TechChange <- bind_rows(L210.rsrc_TechChange, L210.roofPV_TechChange) %>%
      filter(subresource_type == "smooth-renewable-subresource")

    # L210.DepRsrcTechChange: technological change for depletable resources
    L210.DepRsrcTechChange <- L210.dep_rsrc_TechChange %>%
      select(region, depresource = resource, subresource, year.fillout = year, techChange = value)

    # L210.SmthRenewRsrcTechChange: technological change for smooth renewable subresources
    L210.SmthRenewRsrcTechChange <- L210.renew_rsrc_TechChange %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout = year, techChange = value)

    # Tech change in the SSPs
    # Repeat and add region vector to assumed techchange tables
    L210.rsrc_TechChange_SSPs <- A10.TechChange_SSPs %>%
      repeat_add_columns(GCAM_region_names) %>%
      # Add subresource type
      left_join_error_no_match(A10.subrsrc_info, by = c("resource", "subresource"))

    # L210.DepRsrcTechChange_SSPs: technological change for depletable resources in the SSPs
    L210.DepRsrcTechChange_SSPs <- L210.rsrc_TechChange_SSPs %>%
      select(SSP, region, depresource = resource, subresource, year.fillout = year, techChange = value) %>%
      # Split by SSP and assign attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("A10.TechChange_SSPs written to all regions") %>%
          add_precursors("energy/A10.TechChange_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info")
      })

    # Assign each tibble in list
    for(i in names(L210.DepRsrcTechChange_SSPs)) {
      assign(paste0("L210.DepRsrcTechChange_", i), L210.DepRsrcTechChange_SSPs[[i]] %>%
               add_title(paste0("Depletable Resource Tech Change: ", i)) %>%
               add_legacy_name(paste0("L210.DepRsrcTechChange_", i)))
    }

    # C. Calibrated production (depletable resources only)
    # L210.DepRsrcCalProd: calibrated production of depletable resources
    # NOTE: Assuming only one calibrated subresource per depletable resource
    # NOTE: Unconventional oil production is calibrated in the traded unconventional oil technology

    # This is complicated. If the unconventional oil production is calibrated in the resource, then there is
    # no way to interpolate the base-year price-adders (calibration parameters) in the future. Regions that do
    # not produce in the base years effectively come in with no price wedge in the first future time period,
    # and those with the price wedge have their base-year behavior essentially carried forward to all periods.
    # Calibrating this in the "traded unconventional oil" sectors allows for shareweight interpolation.
    L210.DepRsrcCalProd <- L111.Prod_EJ_R_F_Yh %>%
      filter(fuel != "unconventional oil",
             year %in% BASE_YEARS) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add subresource
      left_join_error_no_match(A10.subrsrc_info, by = c("fuel" = "resource")) %>%
      mutate(cal.production = round(value, energy.DIGITS_CALPRODUCTION)) %>%
      select(region, depresource = fuel, subresource, year, cal.production)

    # D. Resource supply curves
    # L210.DepRsrcCurves_fos: supply curves of fossil resources
    L210.DepRsrcCurves_fos <- L111.RsrcCurves_EJ_R_Ffos %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_DEPRESOURCE)) %>%
      select(region, depresource = resource, subresource, grade, available, extractioncost)

    # L210.DepRsrcCurves_U: supply curves of uranium resources
    L210.DepRsrcCurves_U <- L112.RsrcCurves_Mt_R_U %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_DEPRESOURCE)) %>%
      select(region, depresource = resource, subresource, grade, available, extractioncost)

    # L210.SmthRenewRsrcCurves_MSW: supply curves of waste biomass resources
    L210.SmthRenewRsrcCurves_MSW <- L113.RsrcCurves_EJ_R_MSW %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             gdpSupplyElast = round(gdpSupplyElast, energy.DIGITS_GDP_SUPPLY_ELAST),
             year.fillout = min(BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent, gdpSupplyElast)

    # L210.SmthRenewRsrcCurves_wind: supply curves of wind resources
    L210.SmthRenewRsrcCurves_wind <- L114.RsrcCurves_EJ_R_wind %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             year.fillout = min(BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent)

    # L210.SmthRenewRsrcCurves_roofPV: supply curves of rooftop PV resources
    L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV <- L115.RsrcCurves_EJ_R_roofPV %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             gdpSupplyElast = round(gdpSupplyElast, energy.DIGITS_GDP_SUPPLY_ELAST),
             subResourceCapacityFactor = round(subResourceCapacityFactor, energy.DIGITS_CAPACITY_FACTOR),
             year.fillout = min(BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent, gdpSupplyElast, subResourceCapacityFactor)

    # L210.GrdRenewRsrcCurves_geo: graded supply curves of geothermal (hydrothermal) resources
    L210.GrdRenewRsrcCurves_geo <- L116.RsrcCurves_EJ_R_geo %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_geo: default max sub resource of geothermal (hydrothermal) resources
    # The old code is "subset( L210.GrdRenewRsrcCurves_geo, grade = unique( grade )[1] )"
    # It appears they meant to filter to "grade 1" only, however there is only one equals sign, so no subsetting occurs
    if(OLD_DATA_SYSTEM_BEHAVIOR){
      L210.GrdRenewRsrcMax_geo <- L210.GrdRenewRsrcCurves_geo %>%
        mutate(year.fillout = min(BASE_YEARS),
               maxSubResource = 1) %>%
        select(LEVEL2_DATA_NAMES[["maxSubResource"]])
    } else {
      L210.GrdRenewRsrcMax_geo <- L210.GrdRenewRsrcCurves_geo %>%
        filter(grade == "grade 1") %>%
        mutate(year.fillout = min(BASE_YEARS),
               maxSubResource = 1) %>%
        select(LEVEL2_DATA_NAMES[["maxSubResource"]])
    }


    # L210.GrdRenewRsrcCurves_EGS: graded supply curves of geothermal (EGS) resources
    L210.GrdRenewRsrcCurves_EGS <- L116.RsrcCurves_EJ_R_EGS %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_EGS: default max sub resource of EGS resources
    # The old code is "subset( L210.GrdRenewRsrcCurves_EGS, grade = unique( grade )[1] )"
    # It appears they meant to filter to "grade 1" only, however there is only one equals sign, so no subsetting occurs
    if(OLD_DATA_SYSTEM_BEHAVIOR){
      L210.GrdRenewRsrcMax_EGS <- L210.GrdRenewRsrcCurves_EGS %>%
        mutate(year.fillout = min(BASE_YEARS),
               maxSubResource = 1) %>%
        select(LEVEL2_DATA_NAMES[["maxSubResource"]])
    } else {
      L210.GrdRenewRsrcMax_EGS <- L210.GrdRenewRsrcCurves_EGS %>%
        filter(grade == "grade 1") %>%
        mutate(year.fillout = min(BASE_YEARS),
               maxSubResource = 1) %>%
        select(LEVEL2_DATA_NAMES[["maxSubResource"]])
    }

    # L210.GrdRenewRsrcCurves_tradbio: graded supply curves of traditional biomass resources
    L210.GrdRenewRsrcCurves_tradbio <- L117.RsrcCurves_EJ_R_tradbio %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_tradbio: default max sub resource of tradbio resources
    L210.GrdRenewRsrcMax_tradbio <- L210.GrdRenewRsrcCurves_tradbio %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])

    # L210.DepRsrcEnvironCost_SSPs: environmental cost for depletable resources in SSPs
    # Repeat and add region to assumed techchange tables
    L210.DepRsrcEnvironCost_SSPs <- A10.EnvironCost_SSPs %>%
      repeat_add_columns(GCAM_region_names) %>%
      # Add subresource type
      left_join_error_no_match(A10.subrsrc_info, by = c("resource", "subresource")) %>%
      select(SSP, region, depresource = resource, subresource, year.fillout = year, environCost = value) %>%
      # Split by SSP and assign attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Look Up") %>%
          add_comments("A10.EnvironCost_SSPs written to all regions") %>%
          add_precursors("energy/A10.EnvironCost_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info")
      })
    # Assign each tibble in list
    for(i in names(L210.DepRsrcEnvironCost_SSPs)) {
      assign(paste0("L210.DepRsrcEnvironCost_", i), L210.DepRsrcEnvironCost_SSPs[[i]] %>%
               add_title(paste0("Environmental Costs for Depletable Resources: ", i)) %>%
               add_legacy_name(paste0("L210.DepRsrcEnvironCost_", i)))
    }

    # SSP4 is handled differently because of its region groupings - we will handle its precursors separately below
    L210.pcgdp_max_base_year <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "SSP4",
             year == max(BASE_YEARS)) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = value * gdp_deflator(2010, 1990))

    # Define high and low growth regions
    L210.high_reg <- L210.pcgdp_max_base_year$region[L210.pcgdp_max_base_year$value > aglu.HIGH_GROWTH_PCGDP]
    L210.low_reg <- L210.pcgdp_max_base_year$region[L210.pcgdp_max_base_year$value < aglu.LOW_GROWTH_PCGDP]

    L210.DepRsrcEnvironCost_SSP4 <- L210.DepRsrcEnvironCost_SSP4 %>%
      # Set environmental costs for coal to 0 for low growth regions,
      # 10 * environcost for high growth regions
      mutate(environCost = if_else(depresource == "coal" & region %in% L210.low_reg, 0, environCost),
             environCost = if_else(depresource == "coal" & region %in% L210.high_reg, 10 * environCost, environCost)) %>%
      add_title("Environmental Costs for Depletable Resources: SSP4") %>%
      add_units("Look Up") %>%
      add_comments("A10.EnvironCost_SSPs written to all regions") %>%
      add_comments("EnvironCost adjusted for high growth and low growth regions ") %>%
      add_legacy_name("L210.DepRsrcEnvironCost_SSP4") %>%
      add_precursors("energy/A10.EnvironCost_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L210.DepRsrcEnvironCost_SSP4

    # ===================================================

    # Produce outputs
    L210.DepRsrc %>%
      add_title("Market information for depletable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.DepRsrc") %>%
      add_precursors("energy/A_regions", "common/GCAM_region_names", "energy/A10.rsrc_info") ->
      L210.DepRsrc

    L210.RenewRsrc %>%
      add_title("Market information for renewable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.RenewRsrc") %>%
      add_precursors("energy/A_regions", "common/GCAM_region_names", "energy/A10.rsrc_info") ->
      L210.RenewRsrc

    L210.UnlimitRsrc %>%
      add_title("Market information for unlimited resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.UnlimitRsrc") %>%
      add_precursors("energy/A_regions", "common/GCAM_region_names", "energy/A10.rsrc_info") ->
      L210.UnlimitRsrc

    L210.DepRsrcPrice %>%
      add_title("Historical prices for depletable resources") %>%
      add_units("1975$/kg for uranium;  1975$/GJ for everything else") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.DepRsrcPrice") %>%
      same_precursors_as(L210.DepRsrc) ->
      L210.DepRsrcPrice

    L210.RenewRsrcPrice %>%
      add_title("Historical prices for renewable resources") %>%
      add_units("1975$/GJ") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.RenewRsrcPrice") %>%
      same_precursors_as(L210.RenewRsrc) ->
      L210.RenewRsrcPrice

    L210.UnlimitRsrcPrice %>%
      add_title("Historical prices for unlimited resources") %>%
      add_units("1975$/kg for limestone;  1975$/GJ for everything else") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.UnlimitRsrcPrice") %>%
      same_precursors_as(L210.UnlimitRsrc) ->
      L210.UnlimitRsrcPrice

    L210.DepRsrcTechChange %>%
      add_title("Technological change parameter for depletable resources") %>%
      add_units("Unitless") %>%
      add_comments("Data from A10.TechChange added to all regions") %>%
      add_legacy_name("L210.DepRsrcTechChange") %>%
      add_precursors("energy/A10.TechChange", "energy/A10.subrsrc_info", "common/GCAM_region_names",
                     "energy/A15.roofPV_TechChange") ->
      L210.DepRsrcTechChange

    L210.SmthRenewRsrcTechChange %>%
      add_title("Technological change parameter for smooth renewable subresources") %>%
      add_units("Unitless") %>%
      add_comments("Data from A10.TechChange added to all regions") %>%
      add_legacy_name("L210.SmthRenewRsrcTechChange") %>%
      same_precursors_as(L210.DepRsrcTechChange) ->
      L210.SmthRenewRsrcTechChange

    L210.DepRsrcCalProd %>%
      add_title("Calibrated production of depletable resources") %>%
      add_units("EJ/yr") %>%
      add_comments("Data from L111.Prod_EJ_R_F_Yh") %>%
      add_legacy_name("L210.DepRsrcCalProd") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh", "common/GCAM_region_names", "energy/A10.subrsrc_info") ->
      L210.DepRsrcCalProd

    L210.DepRsrcCurves_fos %>%
      add_title("Supply curves of fossil resources") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Data from L111.RsrcCurves_EJ_R_Ffos") %>%
      add_legacy_name("L210.DepRsrcCurves_fos") %>%
      add_precursors("L111.RsrcCurves_EJ_R_Ffos", "common/GCAM_region_names") ->
      L210.DepRsrcCurves_fos

    L210.DepRsrcCurves_U %>%
      add_title("Supply curves of uranium resources") %>%
      add_units("available: MtU; extractioncost: 1975$/kgU") %>%
      add_comments("Data from L112.RsrcCurves_Mt_R_U") %>%
      add_legacy_name("L210.DepRsrcCurves_U") %>%
      add_precursors("L112.RsrcCurves_Mt_R_U", "common/GCAM_region_names") ->
      L210.DepRsrcCurves_U

    L210.SmthRenewRsrcCurves_MSW %>%
      add_title("Supply curves of waste biomass resources") %>%
      add_units("maxSubResource: EJ; mid.price = $1975/GJ") %>%
      add_comments("Data from L113.RsrcCurves_EJ_R_MSW") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_MSW") %>%
      add_precursors("L113.RsrcCurves_EJ_R_MSW", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurves_MSW

    L210.SmthRenewRsrcCurves_wind %>%
      add_title("Supply curves of wind resources") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Data from L114.RsrcCurves_EJ_R_wind") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_wind") %>%
      add_precursors("L114.RsrcCurves_EJ_R_wind", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurves_wind

    L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV %>%
      add_title("Supply curves of rooftop PV resources") %>%
      add_units("maxSubResource: EJ; mid.price = $1975/GJ") %>%
      add_comments("Data from L115.RsrcCurves_EJ_R_roofPV") %>%
      add_legacy_name("L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV") %>%
      add_precursors("L115.RsrcCurves_EJ_R_roofPV", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV

    L210.GrdRenewRsrcCurves_geo %>%
      add_title("Graded supply curves of geothermal (hydrothermal) resources") %>%
      add_units("available: EJ; extractioncost: $1975/GJ") %>%
      add_comments("Data from L116.RsrcCurves_EJ_R_geo") %>%
      add_legacy_name("L210.GrdRenewRsrcCurves_geo") %>%
      add_precursors("L116.RsrcCurves_EJ_R_geo", "common/GCAM_region_names") ->
      L210.GrdRenewRsrcCurves_geo

    L210.GrdRenewRsrcMax_geo %>%
      add_title("Default max sub resource of geothermal (hydrothermal) resources") %>%
      add_units("Unitless") %>%
      add_comments("Value of 1 assigned for all regions") %>%
      add_legacy_name("L210.GrdRenewRsrcMax_geo") %>%
      same_precursors_as(L210.GrdRenewRsrcCurves_geo) ->
      L210.GrdRenewRsrcMax_geo

    L210.GrdRenewRsrcCurves_EGS %>%
      add_title("Graded supply curves of geothermal (EGS) resources") %>%
      add_units("available: EJ; extractioncost: $1975/GJ") %>%
      add_comments("Data from L116.RsrcCurves_EJ_R_EGS") %>%
      add_legacy_name("L210.GrdRenewRsrcCurves_EGS") %>%
      add_precursors("L116.RsrcCurves_EJ_R_EGS", "common/GCAM_region_names") ->
      L210.GrdRenewRsrcCurves_EGS

    L210.GrdRenewRsrcMax_EGS %>%
      add_title("Default max sub resource of EGS resources") %>%
      add_units("Unitless") %>%
      add_comments("maxSubResource assumed to be 1 for all regions") %>%
      add_legacy_name("L210.GrdRenewRsrcMax_EGS") %>%
      same_precursors_as(L210.GrdRenewRsrcCurves_EGS) ->
      L210.GrdRenewRsrcMax_EGS

    L210.GrdRenewRsrcCurves_tradbio %>%
      add_title("Graded supply curves of traditional biomass resources") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Data from L117.RsrcCurves_EJ_R_tradbio") %>%
      add_legacy_name("L210.GrdRenewRsrcCurves_tradbio") %>%
      add_precursors("L117.RsrcCurves_EJ_R_tradbio", "common/GCAM_region_names") ->
      L210.GrdRenewRsrcCurves_tradbio

    L210.GrdRenewRsrcMax_tradbio %>%
      add_title("Default max sub resource of traditional biomass resources") %>%
      add_units("Unitless") %>%
      add_comments("maxSubResource assumed to be 1 for all regions") %>%
      add_legacy_name("L210.GrdRenewRsrcMax_tradbio") %>%
      same_precursors_as(L210.GrdRenewRsrcCurves_tradbio) ->
      L210.GrdRenewRsrcMax_tradbio

    return_data(L210.DepRsrc, L210.RenewRsrc, L210.UnlimitRsrc, L210.DepRsrcPrice, L210.RenewRsrcPrice, L210.UnlimitRsrcPrice, L210.DepRsrcTechChange,
                L210.SmthRenewRsrcTechChange, L210.DepRsrcCalProd, L210.DepRsrcCurves_fos, L210.DepRsrcCurves_U, L210.SmthRenewRsrcCurves_MSW,
                L210.SmthRenewRsrcCurves_wind, L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV, L210.GrdRenewRsrcCurves_geo, L210.GrdRenewRsrcMax_geo,
                L210.GrdRenewRsrcCurves_EGS, L210.GrdRenewRsrcMax_EGS, L210.GrdRenewRsrcCurves_tradbio, L210.GrdRenewRsrcMax_tradbio, L210.DepRsrcTechChange_SSP1,
                L210.DepRsrcEnvironCost_SSP1, L210.DepRsrcTechChange_SSP2, L210.DepRsrcEnvironCost_SSP2, L210.DepRsrcTechChange_SSP3, L210.DepRsrcEnvironCost_SSP3,
                L210.DepRsrcTechChange_SSP4, L210.DepRsrcEnvironCost_SSP4, L210.DepRsrcTechChange_SSP5, L210.DepRsrcEnvironCost_SSP5)
  } else {
    stop("Unknown command")
  }
}
