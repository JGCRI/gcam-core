# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L210.resources
#'
#' Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.Rsrc}, \code{L210.RenewRsrc}, \code{L210.UnlimitRsrc}, \code{L210.RsrcPrice}, \code{L210.RenewRsrcPrice},
#' \code{L210.UnlimitRsrcPrice}, \code{L210.RsrcTechChange}, \code{L210.SmthRenewRsrcTechChange}, \code{L210.SmthRenewRsrcTechChange_offshore_wind}, \code{L210.RsrcCalProd}, \code{L210.ReserveCalReserve}, \code{L210.RsrcCurves_fos},
#' \code{L210.RsrcCurves_U}, \code{L210.SmthRenewRsrcCurves_MSW}, \code{L210.SmthRenewRsrcCurves_wind}, \code{L210.SmthRenewRsrcCurves_offshore_wind}, \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV},
#' \code{L210.GrdRenewRsrcCurves_geo}, \code{L210.GrdRenewRsrcMax_geo}, \code{L210.GrdRenewRsrcCurves_EGS}, \code{L210.GrdRenewRsrcMax_EGS},
#' \code{L210.GrdRenewRsrcCurves_tradbio}, \code{L210.GrdRenewRsrcMax_tradbio}, \code{L210.RsrcTechChange_SSP1}, \code{L210.RsrcEnvironCost_SSP1},
#' \code{L210.RsrcTechChange_SSP2}, \code{L210.RsrcEnvironCost_SSP2}, \code{L210.RsrcTechChange_SSP3}, \code{L210.RsrcEnvironCost_SSP3},
#' \code{L210.RsrcTechChange_SSP4}, \code{L210.RsrcEnvironCost_SSP4}, \code{L210.RsrcTechChange_SSP5}, \code{L210.RsrcEnvironCost_SSP5},
#' \code{L210.ResSubresourceProdLifetime}, \code{L210.ResReserveTechLifetime}, \code{L210.ResReserveTechDeclinePhase}, \code{L210.ResReserveTechProfitShutdown}, \code{L210.ResTechShrwt}, \code{L210.ResTechShrwt_EGS}.
#' The corresponding file in the original data system was \code{L210.resources.R} (energy level2).
#' @details Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else mutate select semi_join
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
             FILE = "energy/A10.ResSubresourceProdLifetime",
             FILE = "energy/A10.SubresourcePriceAdder",
             FILE = "energy/A10.ResReserveTechLifetime",
             FILE = "energy/A10.ResReserveTechDeclinePhase",
             FILE = "energy/A10.ResReserveTechProfitShutdown",
             FILE = "energy/A21.globalrsrctech_cost",
             FILE = "energy/A21.globalrsrctech_coef",
             "L111.RsrcCurves_EJ_R_Ffos",
             "L111.Prod_EJ_R_F_Yh",
             "L112.RsrcCurves_Mt_R_U",
             "L113.RsrcCurves_EJ_R_MSW",
             "L114.RsrcCurves_EJ_R_wind",
             "L115.RsrcCurves_EJ_R_roofPV",
             "L116.RsrcCurves_EJ_R_geo",
             "L116.RsrcCurves_EJ_R_EGS",
             "L117.RsrcCurves_EJ_R_tradbio",
             "L120.RsrcCurves_EJ_R_offshore_wind",
             "L120.TechChange_offshore_wind",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.Rsrc",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.RsrcPrice",
             "L210.RenewRsrcPrice",
             "L210.UnlimitRsrcPrice",
             "L210.RsrcTechChange",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcTechChange_offshore_wind",
             "L210.RsrcCalProd",
             "L210.ReserveCalReserve",
             "L210.RsrcCurves_fos",
             "L210.RsrcCurves_U",
             "L210.SmthRenewRsrcCurves_MSW",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurves_offshore_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo",
             "L210.GrdRenewRsrcCurves_EGS",
             "L210.GrdRenewRsrcMax_EGS",
             "L210.GrdRenewRsrcCurves_tradbio",
             "L210.GrdRenewRsrcMax_tradbio",
             "L210.RsrcTechChange_SSP1",
             "L210.RsrcEnvironCost_SSP1", # Units
             "L210.RsrcTechChange_SSP2",
             "L210.RsrcEnvironCost_SSP2", # Units
             "L210.RsrcTechChange_SSP3",
             "L210.RsrcEnvironCost_SSP3", # Units
             "L210.RsrcTechChange_SSP4",
             "L210.RsrcEnvironCost_SSP4", # Units
             "L210.RsrcTechChange_SSP5",
             "L210.RsrcEnvironCost_SSP5", # Units
             "L210.ResSubresourceProdLifetime",
             "L210.SubresourcePriceAdder",
             "L210.ResReserveTechLifetime",
             "L210.ResReserveTechDeclinePhase",
             "L210.ResReserveTechProfitShutdown",
             "L210.ResTechShrwt",
             "L210.ResTechShrwt_EGS",
             "L210.ResTechCoef",
             "L210.ResTechCost"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    . <- SSP <- year.fillout <- L210.RsrcTechChange_SSP1 <- L210.RsrcEnvironCost_SSP1 <- year <-
      L210.RsrcTechChange_SSP2 <- L210.RsrcEnvironCost_SSP2 <- L210.RsrcTechChange_SSP3 <-
      L210.RsrcEnvironCost_SSP3 <- L210.RsrcTechChange_SSP4 <- L210.RsrcTechChange_SSP5 <-
      L210.RsrcEnvironCost_SSP5 <- available <- cal.production <- capacity.factor <- curve.exponent <-
      resource <- environCost <- extractioncost <- fuel <- gdpSupplyElast <- grade <- market <- value <-
      maxSubResource <- mid.price <- object <- `output-unit` <- `price-unit` <- region <- resource <-
      resource_type <- scenario <-subResourceCapacityFactor <- subresource <- subresource_type <-
      minicam.non.energy.input <- input.cost <- cal.reserve <- renewresource <- sub.renewable.resource <-
      avg.prod.lifetime <- timestep <- lifetime <- year_operate <- final_year <- GCAM_region_ID <-
      sector <- smooth.renewable.subresource <- tech.change <- reserve.subresource <- technology <- prod_value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info", strip_attributes = TRUE) %>%
      gather_years
    A10.subrsrc_info <- get_data(all_data, "energy/A10.subrsrc_info", strip_attributes = TRUE)
    A10.TechChange <- get_data(all_data, "energy/A10.TechChange") %>%
      gather_years
    A10.TechChange_SSPs <- get_data(all_data, "energy/A10.TechChange_SSPs") %>%
      gather_years
    A10.EnvironCost_SSPs <- get_data(all_data, "energy/A10.EnvironCost_SSPs") %>%
      gather_years
    A15.roofPV_TechChange <- get_data(all_data, "energy/A15.roofPV_TechChange") %>%
      gather_years
    A10.ResSubresourceProdLifetime <- get_data(all_data, "energy/A10.ResSubresourceProdLifetime", strip_attributes = TRUE)
    A10.SubresourcePriceAdder <- get_data(all_data, "energy/A10.SubresourcePriceAdder") %>%
      gather_years
    A10.ResReserveTechLifetime <- get_data(all_data, "energy/A10.ResReserveTechLifetime", strip_attributes = TRUE)
    A10.ResReserveTechDeclinePhase <- get_data(all_data, "energy/A10.ResReserveTechDeclinePhase", strip_attributes = TRUE)
    A10.ResReserveTechProfitShutdown <- get_data(all_data, "energy/A10.ResReserveTechProfitShutdown", strip_attributes = TRUE)
    A21.globalrsrctech_cost <- get_data(all_data, "energy/A21.globalrsrctech_cost", strip_attributes = TRUE) %>%
      gather_years(value_col = "input.cost")
    A21.globalrsrctech_coef <- get_data(all_data, "energy/A21.globalrsrctech_coef", strip_attributes = TRUE) %>%
      gather_years(value_col = "coefficient")
    L111.RsrcCurves_EJ_R_Ffos <- get_data(all_data, "L111.RsrcCurves_EJ_R_Ffos", strip_attributes = TRUE)
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh", strip_attributes = TRUE)
    L112.RsrcCurves_Mt_R_U <- get_data(all_data, "L112.RsrcCurves_Mt_R_U", strip_attributes = TRUE)
    L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW", strip_attributes = TRUE)
    L114.RsrcCurves_EJ_R_wind <- get_data(all_data, "L114.RsrcCurves_EJ_R_wind", strip_attributes = TRUE)
    L115.RsrcCurves_EJ_R_roofPV <- get_data(all_data, "L115.RsrcCurves_EJ_R_roofPV", strip_attributes = TRUE)
    L116.RsrcCurves_EJ_R_geo <- get_data(all_data, "L116.RsrcCurves_EJ_R_geo", strip_attributes = TRUE)
    L116.RsrcCurves_EJ_R_EGS <- get_data(all_data, "L116.RsrcCurves_EJ_R_EGS", strip_attributes = TRUE)
    L117.RsrcCurves_EJ_R_tradbio <- get_data(all_data, "L117.RsrcCurves_EJ_R_tradbio", strip_attributes = TRUE)
    L120.RsrcCurves_EJ_R_offshore_wind <- get_data(all_data, "L120.RsrcCurves_EJ_R_offshore_wind", strip_attributes = TRUE)
    L120.TechChange_offshore_wind <- get_data(all_data, "L120.TechChange_offshore_wind", strip_attributes = TRUE )
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Check for calibrated resource prices for final historical model year.
    # Otherwise, price behavior is undefinded, and so stop process.
    # There should be calibrated prices for all historical model years for
    # full consistency, however.
    if(!(MODEL_FINAL_BASE_YEAR %in% c(unique(A10.rsrc_info$year)))){
      stop("No calibrated prices for resources in final historical year")
    }

    # ===================================================
    # ------- FOSSIL RESOURCE RESERVE ADDITIONS
    # Kind of a level 1.5 we are going to calculate / update historical energy
    # but the years we choose as the model base years matter

    GCAM_timesteps <- diff(MODEL_BASE_YEARS)
    start.year.timestep <- modeltime.PERIOD0_TIMESTEP
    model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

    # a pipelne helper function to help back calculate new additions to reserve
    # from historical production
    lag_prod_helper <- function(year, value, year_operate, final_year) {
      ret <- value
      for(i in seq_along(year)) {
        if(i == 1) {
          # first year assume all production in this vintage
          ret[i] <- value[i]
        } else if( year_operate[i] > final_year[i]) {
          if(year_operate[i -1] >= final_year[i]) {
            # retired
            ret[i] <- 0
          } else {
            # final timestep that is operating so we must adjust the production
            # by the number of years into the timestep it should have operated
            # incase lifetime and timesteps do not neatly overlap
            ret[i] <- ret[i - 1] * (year_operate[i] - final_year[i]) / (year_operate[i] - year_operate[i-1])
          }
        } else if(year_operate[i] > year[i]) {
          # assume a vintage that as already invested continues at full
          # capacity
          ret[i] <- ret[i -1]
        } else {
          # to determine new investment we take the difference between
          # what the total should be and subtract off production from
          # previous vintages that are still operating
          ret[i] <- 0
          ret[i] <- pmax(value[i] - sum(ret[year_operate == year[i]]), 0)
        }
      }
      ret
    }
    # Back calculate reserve additions to be exactly enough given our historical production
    # and assumed production lifetime.  Note production lifetimes may not cover the entire
    # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
    # to help project forward production by each historical vintage so we can take this into
    # account.
    L111.Prod_EJ_R_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime, reserve.subresource) %>% distinct(),
                               by=c("fuel" = "resource", "technology" = "reserve.subresource")) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      select(-lifetime, -timestep, -year_operate) ->
      L210.Reserve_EJ_R_F_Yh

    # Given the mismatch between data sets for historical production / regional supply curves / and
    # assumption for production lifetimes it may be the case that for some region + resource there
    # is not enough supply in the supply curve to cover historical reserves.  We will add in just
    # enough to be able to cover the historical period.  And of course this means that in the future
    # model periods those region + resource will not be able to produce more which seems like the
    # correct compromise.
    L210.Reserve_EJ_R_F_Yh %>%
      group_by(GCAM_region_ID, fuel,technology) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      ReserveTotal_EJ_R_F
    L111.RsrcCurves_EJ_R_Ffos %>%
      group_by(GCAM_region_ID, resource,subresource) %>%
      summarize(available = sum(available)) %>%
      ungroup() %>%
      left_join_error_no_match(ReserveTotal_EJ_R_F %>% rename(subresource = technology), ., by=c("GCAM_region_ID", "fuel" = "resource","subresource")) %>%
      filter(value > available) %>%
      mutate(available = value - available) %>%
      select(-value) %>%
      rename(resource = fuel) %>%
      left_join_error_no_match(L111.RsrcCurves_EJ_R_Ffos %>%
                                 group_by(GCAM_region_ID, resource, subresource) %>%
                                 filter(extractioncost == max(extractioncost)) %>%
                                 ungroup() %>%
                                 mutate(grade = "extended for reserve1",
                                        extractioncost =  extractioncost* 1.1) %>%
                                 select(-available),
                               by = c("GCAM_region_ID", "resource","subresource")) ->
      RsrcCurve_ReserveDeficit
    RsrcCurve_ReserveDeficit %>%
      bind_rows(RsrcCurve_ReserveDeficit %>%
                  mutate(grade = "extended for reserve2") %>%
                  # Note the factor here does not matter because this region + resource will completely
                  # deplete in the historical period and none will be available during model operation
                  # anyways.
                  mutate(extractioncost = extractioncost * 1.2,
                         available = 0)) %>%
      bind_rows(L111.RsrcCurves_EJ_R_Ffos, .) ->
      L111.RsrcCurves_EJ_R_Ffos

    # A. Output unit, price unit, market
    L210.rsrc_info <- A10.rsrc_info %>%
      # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Remove traditional biomass from regions where it is not currently used
      filter(!(region %in% A_regions$region[A_regions$tradbio_region == 0] & resource == "traditional biomass")) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L210.Rsrc: output unit, price unit, and market for depletable resources
    L210.Rsrc <- L210.rsrc_info %>%
      filter(resource_type == "resource") %>%
      select(region, resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RenewRsrc: output unit, price unit, and market for renewable resources
    L210.RenewRsrc <- L210.rsrc_info %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.UnlimitRsrc: output unit, price unit, and market for unlimited resources
    L210.UnlimitRsrc <- L210.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RsrcPrice: historical prices for depletable resources
    L210.RsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "resource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, resource = resource, year, price = value)

    # L210.RenewRsrcPrice: historical prices for renewable resources
    L210.RenewRsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "renewresource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, renewresource = resource, year, price = value)

    # L210.UnlimitRsrcPrice: prices for unlimited resources
    L210.UnlimitRsrcPrice <- L210.rsrc_info %>%
      filter(resource_type == "unlimited-resource",
             year %in% MODEL_BASE_YEARS) %>%
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

    # L210.RsrcTechChange: technological change for depletable resources
    L210.RsrcTechChange <- L210.dep_rsrc_TechChange %>%
      select(region, resource = resource, subresource, year.fillout = year, techChange = value)

    # L210.SmthRenewRsrcTechChange: technological change for smooth renewable subresources
    L210.SmthRenewRsrcTechChange <- L210.renew_rsrc_TechChange %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout = year, techChange = value)

    # L210.SmthRenewRsrcTechChange_offshore_wind: technological change for offshore wind
    L210.SmthRenewRsrcTechChange_offshore_wind <- write_to_all_regions(L120.TechChange_offshore_wind, c("region", "year","tech.change"), GCAM_region_names)
    L210.SmthRenewRsrcTechChange_offshore_wind %>%
      mutate(renewresource = "offshore wind resource", smooth.renewable.subresource = "offshore wind resource") %>%
      select(region,renewresource, smooth.renewable.subresource, year.fillout = year, techChange = tech.change) -> L210.SmthRenewRsrcTechChange_offshore_wind

    # Tech change in the SSPs
    # Repeat and add region vector to assumed techchange tables
    L210.rsrc_TechChange_SSPs <- A10.TechChange_SSPs %>%
      repeat_add_columns(GCAM_region_names) %>%
      # Add subresource type
      left_join_error_no_match(A10.subrsrc_info, by = c("resource", "subresource"))

    # L210.RsrcTechChange_SSPs: technological change for depletable resources in the SSPs
    L210.RsrcTechChange_SSPs <- L210.rsrc_TechChange_SSPs %>%
      select(SSP, region, resource = resource, subresource, year.fillout = year, techChange = value) %>%
      # Split by SSP and assign attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, -SSP) %>%
          add_units("Unitless") %>%
          add_comments("A10.TechChange_SSPs written to all regions") %>%
          add_precursors("energy/A10.TechChange_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info")
      })

    # Assign each tibble in list
    for(i in names(L210.RsrcTechChange_SSPs)) {
      assign(paste0("L210.RsrcTechChange_", i), L210.RsrcTechChange_SSPs[[i]] %>%
               add_title(paste0("Depletable Resource Tech Change: ", i)) %>%
               add_legacy_name(paste0("L210.RsrcTechChange_", i)))
    }

    # C. Calibrated production (depletable resources only)
    # L210.RsrcCalProd: calibrated production of depletable resources
    # NOTE: Assuming only one calibrated subresource per depletable resource
    # NOTE: Unconventional oil production is calibrated in the traded unconventional oil technology

    # This is complicated. If the unconventional oil production is calibrated in the resource, then there is
    # no way to interpolate the base-year price-adders (calibration parameters) in the future. Regions that do
    # not produce in the base years effectively come in with no price wedge in the first future time period,
    # and those with the price wedge have their base-year behavior essentially carried forward to all periods.
    # Calibrating this in the "traded unconventional oil" sectors allows for shareweight interpolation.
    L210.RsrcCalProd <- L111.Prod_EJ_R_F_Yh %>%
      filter(fuel != "unconventional oil",
             year %in% MODEL_BASE_YEARS) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add subresource
      left_join_error_no_match(A10.subrsrc_info, by = c("fuel" = "resource","technology"= "subresource")) %>%
      mutate(cal.production = round(value, energy.DIGITS_CALPRODUCTION)) %>%
      select(region, resource = fuel, subresource= technology, year, cal.production)

    L210.Reserve_EJ_R_F_Yh %>%
      rename(cal.reserve = value) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Add subresource
      left_join_error_no_match(A10.subrsrc_info, by = c("fuel" = "resource","technology"= "subresource")) %>%
      select(region, resource = fuel, reserve.subresource = technology, year, cal.reserve) %>%
      filter(resource != "unconventional oil") ->
      L210.ReserveCalReserve

    L210.ReserveCalReserve_unoil <- L210.ReserveCalReserve %>% filter(reserve.subresource=="unconventional oil")

    L210.ReserveCalReserve.uncon_other_reg <- L210.ReserveCalReserve %>%
                                              filter(resource =="coal") %>%
                                              filter(!region %in% c(unique(L210.ReserveCalReserve_unoil$region))) %>%
                                              mutate(resource =paste0("crude oil"),reserve.subresource =paste0("unconventional oil"),cal.reserve=0)
    L210.ReserveCalReserve <- bind_rows(L210.ReserveCalReserve,L210.ReserveCalReserve.uncon_other_reg)

    # D. Resource supply curves
    # L210.RsrcCurves_fos: supply curves of fossil resources
    L210.RsrcCurves_fos <- L111.RsrcCurves_EJ_R_Ffos %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_RESOURCE)) %>%
      select(region, resource = resource, subresource, grade, available, extractioncost)

    # L210.RsrcCurves_U: supply curves of uranium resources
    L210.RsrcCurves_U <- L112.RsrcCurves_Mt_R_U %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_RESOURCE)) %>%
      select(region, resource = resource, subresource, grade, available, extractioncost)

    # L210.SmthRenewRsrcCurves_MSW: supply curves of waste biomass resources
    L210.SmthRenewRsrcCurves_MSW <- L113.RsrcCurves_EJ_R_MSW %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             gdpSupplyElast = round(gdpSupplyElast, energy.DIGITS_GDP_SUPPLY_ELAST),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent, gdpSupplyElast)

    # L210.SmthRenewRsrcCurves_wind: supply curves of wind resources
    L210.SmthRenewRsrcCurves_wind <- L114.RsrcCurves_EJ_R_wind %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent)

    # L210.SmthRenewRsrcCurves_offshore_wind: supply curves of offshore wind resources
    L210.SmthRenewRsrcCurves_offshore_wind <- L120.RsrcCurves_EJ_R_offshore_wind %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent)

    # L210.SmthRenewRsrcCurves_roofPV: supply curves of rooftop PV resources
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- L115.RsrcCurves_EJ_R_roofPV %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(maxSubResource = round(maxSubResource, energy.DIGITS_MAX_SUB_RESOURCE),
             mid.price = round(mid.price, energy.DIGITS_MID_PRICE),
             curve.exponent = round(curve.exponent, energy.DIGITS_CURVE_EXPONENT),
             gdpSupplyElast = round(gdpSupplyElast, energy.DIGITS_GDP_SUPPLY_ELAST),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, renewresource = resource, smooth.renewable.subresource = subresource, year.fillout, maxSubResource, mid.price, curve.exponent, gdpSupplyElast)

    # L210.GrdRenewRsrcCurves_geo: graded supply curves of geothermal (hydrothermal) resources
    L210.GrdRenewRsrcCurves_geo <- L116.RsrcCurves_EJ_R_geo %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_geo: default max sub resource of geothermal (hydrothermal) resources
    L210.GrdRenewRsrcMax_geo <- L210.GrdRenewRsrcCurves_geo %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])


    # L210.GrdRenewRsrcCurves_EGS: graded supply curves of geothermal (EGS) resources
    L210.GrdRenewRsrcCurves_EGS <- L116.RsrcCurves_EJ_R_EGS %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_EGS: default max sub resource of EGS resources
    L210.GrdRenewRsrcMax_EGS <- L210.GrdRenewRsrcCurves_EGS %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])

    L210.ResTechShrwt_EGS <- L210.GrdRenewRsrcMax_EGS %>%
      rename(resource = renewresource, subresource = sub.renewable.resource) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1 ) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]])


    # L210.GrdRenewRsrcCurves_tradbio: graded supply curves of traditional biomass resources
    L210.GrdRenewRsrcCurves_tradbio <- L117.RsrcCurves_EJ_R_tradbio %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_tradbio: default max sub resource of tradbio resources
    L210.GrdRenewRsrcMax_tradbio <- L210.GrdRenewRsrcCurves_tradbio %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])

    # L210.RsrcEnvironCost_SSPs: environmental cost for depletable resources in SSPs
    # Repeat and add region to assumed techchange tables
    L210.RsrcEnvironCost_SSPs <- A10.EnvironCost_SSPs %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(minicam.non.energy.input = "environCost") %>%
      rename(input.cost = value) %>%
      # Split by SSP and assign attributes
      split(.$SSP) %>%
      lapply(function(df) {
        select(df, LEVEL2_DATA_NAMES[["ResReserveTechCost"]]) %>%
          add_units("$/GJ") %>%
          add_comments("A10.EnvironCost_SSPs written to all regions") %>%
          add_precursors("energy/A10.EnvironCost_SSPs", "common/GCAM_region_names")
      })
    # Assign each tibble in list
    for(i in names(L210.RsrcEnvironCost_SSPs)) {
      assign(paste0("L210.RsrcEnvironCost_", i), L210.RsrcEnvironCost_SSPs[[i]] %>%
               add_title(paste0("Environmental Costs for Depletable Resources: ", i)) %>%
               add_legacy_name(paste0("L210.RsrcEnvironCost_", i)))
    }

    # SSP4 is handled differently because of its region groupings - we will handle its precursors separately below
    # Define high and low growth regions
    L210.high_reg <- get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "high")
    L210.low_reg <- get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low")

    L210.RsrcEnvironCost_SSP4 <- L210.RsrcEnvironCost_SSP4 %>%
      # Set environmental costs for coal to 0 for low growth regions,
      # 10 * environcost for high growth regions
      mutate(environCost = if_else(resource == "coal" & region %in% L210.low_reg, 0, environCost),
             environCost = if_else(resource == "coal" & region %in% L210.high_reg, 10 * environCost, environCost)) %>%
      add_title("Environmental Costs for Depletable Resources: SSP4", overwrite = TRUE) %>%
      add_units("$/GJ") %>%
      add_comments("A10.EnvironCost_SSPs written to all regions") %>%
      add_comments("EnvironCost adjusted for high growth and low growth regions ") %>%
      add_legacy_name("L210.RsrcEnvironCost_SSP4", overwrite = TRUE) %>%
      add_precursors("energy/A10.EnvironCost_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L210.RsrcEnvironCost_SSP4

    # Resource-reserve assumptions which just need to get copied to all regions and years
    A10.ResSubresourceProdLifetime %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResSubresourceProdLifetime"]]) ->
      L210.ResSubresourceProdLifetime

    A10.SubresourcePriceAdder %>%
      repeat_add_columns(GCAM_region_names) %>%
      rename(price.adder = value) %>%
      select(LEVEL2_DATA_NAMES[["SubresourcePriceAdder"]]) ->
      L210.SubresourcePriceAdder

    A10.ResReserveTechLifetime %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechLifetime"]]) ->
      L210.ResReserveTechLifetime

    A10.ResReserveTechDeclinePhase %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechDeclinePhase"]]) ->
      L210.ResReserveTechDeclinePhase

    A10.ResReserveTechProfitShutdown %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechProfitShutdown"]]) ->
      L210.ResReserveTechProfitShutdown

    A21.globalrsrctech_cost %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechCost"]]) -> L210.ResTechCost

    A21.globalrsrctech_coef %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechCoef"]])-> L210.ResTechCoef


    # We need to make sure we have at least a shell technology for ALL resources
    # and so we will just use the share weight table to facilatate doing that.
    A10.subrsrc_info %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(L210.RsrcCalProd %>%
                  mutate(prod_value = as.double(cal.production)),
                by = c("region", "resource", "subresource", "year")) %>%
      mutate(prod_value = if_else(is.na(prod_value), 0, prod_value),
             technology = subresource,
             share.weight = if_else(year > MODEL_FINAL_BASE_YEAR | prod_value > 0, 1, 0)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L210.ResTechShrwt
    # We need to remove regions + Subresources which should not exist
    L210.ResTechShrwt %>%
      semi_join(L210.GrdRenewRsrcMax_tradbio,
                by = c("region", "resource" = "renewresource", "subresource" = "sub.renewable.resource")) %>%
      bind_rows(filter(L210.ResTechShrwt, resource != "traditional biomass"), .) ->
      L210.ResTechShrwt
    L210.ResTechShrwt %>%
      semi_join(L210.RsrcCurves_U,
                by = c("region", "resource" = "resource", "subresource")) %>%
      bind_rows(filter(L210.ResTechShrwt, resource != "uranium"), .) ->
      L210.ResTechShrwt

    # ===================================================

    # Produce outputs

    L210.ResTechCost %>%
      add_title("Cost of resource production") %>%
      add_units("$/GJ") %>%
      add_comments("A21.globalrsrctech_cost written to all regions") %>%
      add_legacy_name("L210.ResTechCost") %>%
      add_precursors("energy/A21.globalrsrctech_cost", "common/GCAM_region_names") ->
      L210.ResTechCost

    L210.ResTechCoef %>%
      add_title("Co-efficients of resource production inputs") %>%
      add_units("NA") %>%
      add_comments("A21.globalrsrctech_coef written to all regions") %>%
      add_legacy_name("L210.ResTechCoef") %>%
      add_precursors("energy/A21.globalrsrctech_coef", "common/GCAM_region_names") ->
      L210.ResTechCoef


    L210.Rsrc %>%
      add_title("Market information for depletable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.Rsrc") %>%
      add_precursors("energy/A_regions", "common/GCAM_region_names", "energy/A10.rsrc_info") ->
      L210.Rsrc

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

    L210.RsrcPrice %>%
      add_title("Historical prices for depletable resources") %>%
      add_units("1975$/kg for uranium;  1975$/GJ for everything else") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.RsrcPrice") %>%
      same_precursors_as(L210.Rsrc) ->
      L210.RsrcPrice

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

    L210.RsrcTechChange %>%
      add_title("Technological change parameter for depletable resources") %>%
      add_units("Unitless") %>%
      add_comments("Data from A10.TechChange added to all regions") %>%
      add_legacy_name("L210.RsrcTechChange") %>%
      add_precursors("energy/A10.TechChange", "energy/A10.subrsrc_info", "common/GCAM_region_names",
                     "energy/A15.roofPV_TechChange") ->
      L210.RsrcTechChange

    L210.SmthRenewRsrcTechChange %>%
      add_title("Technological change parameter for smooth renewable subresources") %>%
      add_units("Unitless") %>%
      add_comments("Data from A10.TechChange added to all regions") %>%
      add_legacy_name("L210.SmthRenewRsrcTechChange") %>%
      same_precursors_as(L210.RsrcTechChange) ->
      L210.SmthRenewRsrcTechChange

    L210.SmthRenewRsrcTechChange_offshore_wind %>%
      add_title("Technological change parameter for offshore wind resource") %>%
      add_units("Unitless") %>%
      add_comments("Data from L120.TechChange_offshore_wind added to all regions") %>%
      same_precursors_as(L210.RsrcTechChange) %>%
      add_precursors("L120.TechChange_offshore_wind") ->
      L210.SmthRenewRsrcTechChange_offshore_wind

    L210.SubresourcePriceAdder %>%
      add_title("Adjust calibration price adders in future model years") %>%
      add_units("1975$/GJ") %>%
      add_comments("A10.SubresourcePriceAdder written to all regions") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.SubresourcePriceAdder") ->
      L210.SubresourcePriceAdder

    L210.RsrcCalProd %>%
      add_title("Calibrated production of depletable resources") %>%
      add_units("EJ/yr") %>%
      add_comments("Data from L111.Prod_EJ_R_F_Yh") %>%
      add_legacy_name("L210.RsrcCalProd") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh", "common/GCAM_region_names", "energy/A10.subrsrc_info") ->
      L210.RsrcCalProd

    L210.ReserveCalReserve %>%
      add_title("Calibrated reserves of depletable resource") %>%
      add_units("EJ cumulative") %>%
      add_comments("Calibrated reserve additions in each model year from which") %>%
      add_comments("the vintage will produce from for the assumed lifetime") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh", "energy/A10.ResSubresourceProdLifetime", "common/GCAM_region_names", "energy/A10.subrsrc_info") ->
      L210.ReserveCalReserve

    L210.RsrcCurves_fos %>%
      add_title("Supply curves of fossil resources") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Data from L111.RsrcCurves_EJ_R_Ffos") %>%
      add_legacy_name("L210.RsrcCurves_fos") %>%
      add_precursors("L111.RsrcCurves_EJ_R_Ffos", "L111.Prod_EJ_R_F_Yh", "energy/A10.ResSubresourceProdLifetime", "common/GCAM_region_names") ->
      L210.RsrcCurves_fos

    L210.RsrcCurves_U %>%
      add_title("Supply curves of uranium resources") %>%
      add_units("available: MtU; extractioncost: 1975$/kgU") %>%
      add_comments("Data from L112.RsrcCurves_Mt_R_U") %>%
      add_legacy_name("L210.RsrcCurves_U") %>%
      add_precursors("L112.RsrcCurves_Mt_R_U", "common/GCAM_region_names") ->
      L210.RsrcCurves_U

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

    L210.SmthRenewRsrcCurves_offshore_wind %>%
      add_title("Supply curves of offshore wind resources") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Data from L120.RsrcCurves_EJ_R_offshore_wind") %>%
      add_precursors("L120.RsrcCurves_EJ_R_offshore_wind", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurves_offshore_wind

    L210.SmthRenewRsrcCurvesGdpElast_roofPV %>%
      add_title("Supply curves of rooftop PV resources") %>%
      add_units("maxSubResource: EJ; mid.price = $1975/GJ") %>%
      add_comments("Data from L115.RsrcCurves_EJ_R_roofPV") %>%
      add_legacy_name("L210.SmthRenewRsrcCurvesGdpElast_roofPV") %>%
      add_precursors("L115.RsrcCurves_EJ_R_roofPV", "common/GCAM_region_names") ->
      L210.SmthRenewRsrcCurvesGdpElast_roofPV

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

    L210.ResSubresourceProdLifetime %>%
      add_title("Average production lifetime for reserve subresource") %>%
      add_units("Years") %>%
      add_comments("Used to annualize production of the cumulative resource reserve") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.ResSubresourceProdLifetime") ->
      L210.ResSubresourceProdLifetime

    L210.ResReserveTechLifetime %>%
      add_title("Resource reserve technology lifetime") %>%
      add_units("Years") %>%
      add_comments("Resource well / mine lifetime over which the reserve will be produced / depleted") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.ResReserveTechLifetime") ->
      L210.ResReserveTechLifetime

    L210.ResReserveTechDeclinePhase %>%
      add_title("Resource reserve technology decline phase percent") %>%
      add_units("fraction") %>%
      add_comments("When the total reserve has been depleted to this percent the production") %>%
      add_comments("will move into a linear decline phase.") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.ResReserveTechDeclinePhase") ->
      L210.ResReserveTechDeclinePhase

    L210.ResReserveTechProfitShutdown %>%
      add_title("Resource reserve technology profit shutdown decider") %>%
      add_units("NA") %>%
      add_comments("Resource profit shutdown to characterize a well / mine's ability scale back") %>%
      add_comments("production under unprofitable conditions") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.ResReserveTechProfitShutdown") ->
      L210.ResReserveTechProfitShutdown

    L210.ResTechShrwt %>%
      add_title("Share weights for technologies in resources") %>%
      add_units("NA") %>%
      add_comments("Share weights won't matter for resource technologies as there") %>%
      add_comments("is no competetion between technologies.") %>%
      add_precursors("common/GCAM_region_names", "energy/A10.subrsrc_info") ->
      L210.ResTechShrwt

    L210.ResTechShrwt_EGS %>%
      add_title("Share weights for technologies in EGS resources") %>%
      add_units("NA") %>%
      add_comments("Share weights won't matter for resource technologies as there") %>%
      add_comments("is no competetion between technologies.") %>%
      add_comments("Note EGS is serperated since the resource is only included in adv scenarios.") %>%
      same_precursors_as(L210.GrdRenewRsrcMax_EGS) ->
      L210.ResTechShrwt_EGS

    return_data(L210.Rsrc, L210.RenewRsrc, L210.UnlimitRsrc, L210.RsrcPrice, L210.RenewRsrcPrice, L210.UnlimitRsrcPrice, L210.RsrcTechChange,
                L210.SmthRenewRsrcTechChange, L210.SmthRenewRsrcTechChange_offshore_wind, L210.RsrcCalProd, L210.ReserveCalReserve, L210.RsrcCurves_fos, L210.RsrcCurves_U, L210.SmthRenewRsrcCurves_MSW,
                L210.SmthRenewRsrcCurves_wind, L210.SmthRenewRsrcCurves_offshore_wind, L210.SmthRenewRsrcCurvesGdpElast_roofPV, L210.GrdRenewRsrcCurves_geo, L210.GrdRenewRsrcMax_geo,
                L210.GrdRenewRsrcCurves_EGS, L210.GrdRenewRsrcMax_EGS, L210.GrdRenewRsrcCurves_tradbio, L210.GrdRenewRsrcMax_tradbio, L210.RsrcTechChange_SSP1,
                L210.RsrcEnvironCost_SSP1, L210.RsrcTechChange_SSP2, L210.RsrcEnvironCost_SSP2, L210.RsrcTechChange_SSP3, L210.RsrcEnvironCost_SSP3,
                L210.RsrcTechChange_SSP4, L210.RsrcEnvironCost_SSP4, L210.RsrcTechChange_SSP5, L210.RsrcEnvironCost_SSP5,
                L210.ResSubresourceProdLifetime, L210.SubresourcePriceAdder, L210.ResReserveTechLifetime, L210.ResReserveTechDeclinePhase, L210.ResReserveTechProfitShutdown,
                L210.ResTechShrwt, L210.ResTechShrwt_EGS, L210.ResTechCoef, L210.ResTechCost)
  } else {
    stop("Unknown command")
  }
}
