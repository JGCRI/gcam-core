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
#' @importFrom dplyr bind_rows distinct filter if_else mutate mutate_if select semi_join
#' @author RLH November 2017
module_energy_L210.resources <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "energy/A10.rsrc_info_fossils",
             FILE = "energy/A10.rsrc_info_renewables_others",
             FILE = "energy/A10.rsrc_info_uranium",
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
             "L210.rsrc_info",
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
             "L210.ResReserveTechInvestmentInput",
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
      resource_type <- scenario <-subResourceCapacityFactor <- subresource <- subresource_type <- resource.reserve.technology <-
      minicam.non.energy.input <- input.cost <- cal.reserve <- renewresource <- sub.renewable.resource <-
      avg.prod.lifetime <- timestep <- lifetime <- year_operate <- final_year <- GCAM_region_ID <-
      sector <- smooth.renewable.subresource <- tech.change <- reserve.subresource <- technology <- prod_value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A10.rsrc_info_fossils <- get_data(all_data, "energy/A10.rsrc_info_fossils", strip_attributes = TRUE)
    A10.rsrc_info_renewables_others <- get_data(all_data, "energy/A10.rsrc_info_renewables_others", strip_attributes = TRUE) %>%
      gather_years
    A10.rsrc_info_uranium <- get_data(all_data, "energy/A10.rsrc_info_uranium", strip_attributes = TRUE) %>%
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


    # Process resources prices data
    # Source: bp-stats-review-2021-all-data.xlsx
    # The specific regions, or region averages, used for global marker price described in the input file

    # unit and currency conversion of resource prices into 1975$/GJ
    A10.rsrc_info_fossils %>%
      mutate(energy.conv = case_when(resource == "natural gas" ~ CONV_MMBTU_GJ,
                                     resource == "crude oil" ~ CONV_BBL_GJ,
                                     resource == "coal" ~ CONV_COALTONNE_GJ,
                                     TRUE ~ NA_real_),
             # convert each year as fossil prices are nominal USD
             currency.conv = gdp_deflator(1975, year),
             price = price * currency.conv / energy.conv,
             `price-unit` = "1975$/GJ") %>%
      # we are taking the mean price accross the "source" dimension in case we have
      # multiple marker price markets for a given resource
      group_by(resource, resource_type, market, `output-unit`, `price-unit`, year) %>%
      summarize(value = mean(price)) %>%
      # Note: taking advantage of the standard dplyr behavior to "pop" the last grouping: year
      # which is what we want so that we can calculate moving average prices accross those yaers
      mutate(moving_avg = Moving_average_lagged(value, periods = energy.FUEL_PRICES_MEAN_PERIOD)) %>%
      ungroup() %>%
      # filling earlier years with just the annual price
      mutate(value = if_else(is.na(moving_avg), value, moving_avg)) %>%
      select(-moving_avg) ->
      A10.rsrc_info_fossils_processed_avg

    # uranium unit conversion to 1975$
    A10.rsrc_info_uranium %>%
      mutate(# the regex parses the currency unit to allow automatic currency deflation to $1975
        value = value * gdp_deflator(1975, as.numeric(unique(unlist(regmatches(`price-unit`, gregexpr("[[:digit:]]+", `price-unit`)))))),
        `price-unit` = gsub(unique(unlist(regmatches(`price-unit`, gregexpr("[[:digit:]]+", `price-unit`)))), "1975", `price-unit`)) -> A10.rsrc_info_uranium_processed


    # merge individually prepared resource prices of fossils, renewables, and uranium into one A10.rsrc_info data object
    A10.rsrc_info_merged <- bind_rows(A10.rsrc_info_fossils_processed_avg,
                                      A10.rsrc_info_renewables_others,
                                      A10.rsrc_info_uranium_processed
                                      )

    # Interpolate and extrapolate missing historical years
    A10.rsrc_info <- A10.rsrc_info_merged %>%
      complete(nesting(resource, resource_type, market, `output-unit`, `price-unit`), year = c(HISTORICAL_YEARS)) %>%
      group_by(resource, resource_type, market, `output-unit`, `price-unit`) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    # Check for calibrated resource prices for final historical model year.
    # Otherwise, price behavior is undefined, and so stop process.
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

    # a pipeline helper function to help back calculate new additions to reserve
    # from historical production
    lag_prod_helper <- function(data) {
      data %>%
        arrange(year_operate, year) %>%
        mutate(max.annual.prod = 0,
               annual.prod = 0,
               reserve = 0,
               cumul.prod = 0) ->
        data_proc

      # operate each model base year one at a time
      for(year_i in MODEL_BASE_YEARS) {
        curr_slice <- data_proc[data_proc$year_operate == year_i, ]
        if(year_i == MODEL_BASE_YEARS[1]) {
          # first year assume all production in this vintage
          curr_slice %>%
            mutate(max.annual.prod = value,
                   annual.prod = value,
                   reserve = annual.prod * lifetime,
                   cumul.prod = annual.prod * timestep) ->
            curr_slice
        } else {
          # pull out the new vintage slice
          curr_slice %>%
            filter(year == year_operate) ->
            new_inv_slice
          # grab the annual production in this year and the timestep which will be needed
          # to calculate production from existing vintages as well
          curr_demand <- new_inv_slice %>% pull(value)
          curr_timestep = new_inv_slice %>% pull(timestep)
          # calculate production from existing vintages first
          prev_slice %>%
            mutate(remain = reserve - cumul.prod,
                   # save previous production so as to be able to linearly adjust depletion
                   prev.annual.prod = annual.prod,
                   # the max annual production may need to get scaled down if this reserve is about
                   # to run out in this timestep
                   max.annual.prod = pmin(max.annual.prod,
                                          pmax((remain - curr_timestep * prev.annual.prod) * 2.0 / curr_timestep + prev.annual.prod, 0.0)),
                   # calculate the production short fall which if positive will drive new investment
                   supply.shortfall = curr_demand - sum(max.annual.prod),
                   # if the short fall is negative we have over capacity so annual production will need
                   # to scale down from the max
                   annual.prod = if_else(supply.shortfall >= 0, max.annual.prod,
                                         max.annual.prod * (curr_demand / sum(max.annual.prod)))) ->
            prev_slice
          # use the shortfall to set the new vintage production and reserves
          new_prod <- pmax(unique(prev_slice$supply.shortfall), 0.0)
          new_inv_slice %>%
            mutate(max.annual.prod = new_prod,
                   annual.prod = new_prod,
                   reserve = annual.prod * lifetime,
                   cumul.prod = annual.prod * timestep) ->
            new_inv_slice
          # update previous vintage values in the current "slice"
          curr_slice %>%
            filter(year != year_operate) %>%
            mutate(annual.prod = prev_slice$annual.prod,
                   # update cumulative depletion assuming linear change from previous production to current production
                   cumul.prod = prev_slice$cumul.prod + prev_slice$prev.annual.prod * timestep + 0.5 * ( annual.prod - prev_slice$prev.annual.prod) * timestep,
                   # copy forward the rest
                   max.annual.prod = prev_slice$max.annual.prod,
                   reserve = prev_slice$reserve) %>%
            # add back new investment
            bind_rows(new_inv_slice) ->
            curr_slice
        }
        # set the current "slice" back into the original DF
        prev_slice = curr_slice
        data_proc[data_proc$year_operate == year_i, ] = curr_slice
      }
      # ultimately we just need the new vintage reserves
        data_proc %>%
          filter(year == year_operate) %>%
          select(year, value = reserve)
    }
    # Back calculate reserve additions to be exactly enough given our historical production
    # and assumed production lifetime.  Note production lifetimes may not cover the entire
    # historical period and production may dip below capacity making the calculation a bit more
    # tricky.  We use the lag_prod_helper to help project forward production by each historical
    # vintage so we can take this into account.
    # Note: because we are back calculating this our choice of MODEL_BASE_YEARS matters, which is
    # why this is Level2 processing.
    L111.Prod_EJ_R_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime, reserve.subresource) %>% distinct(),
                               by=c("fuel" = "resource", "technology" = "reserve.subresource")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year_operate" = "year")) %>%
      filter(year_operate >= year) %>%
      tidyr::nest(data = -c(GCAM_region_ID, sector, fuel, technology)) %>%
      mutate(data = lapply(data, lag_prod_helper)) %>%
      tidyr::unnest(cols = data) ->
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
    L210.rsrc_info <- A10.rsrc_info # just output the file without regions

    L210.RsrcInfo <- A10.rsrc_info %>%
      # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Remove traditional biomass from regions where it is not currently used
      filter(!(region %in% A_regions$region[A_regions$tradbio_region == 0] & resource == "traditional biomass")) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L210.Rsrc: output unit, price unit, and market for depletable resources
    L210.Rsrc <- L210.RsrcInfo %>%
      filter(resource_type == "resource") %>%
      select(region, resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RenewRsrc: output unit, price unit, and market for renewable resources
    L210.RenewRsrc <- L210.RsrcInfo %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.UnlimitRsrc: output unit, price unit, and market for unlimited resources
    L210.UnlimitRsrc <- L210.RsrcInfo %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RsrcPrice: historical prices for depletable resources
    L210.RsrcPrice <- L210.RsrcInfo %>%
      filter(resource_type == "resource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, resource = resource, year, price = value)

    # L210.RenewRsrcPrice: historical prices for renewable resources
    L210.RenewRsrcPrice <- L210.RsrcInfo %>%
      filter(resource_type == "renewresource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, renewresource = resource, year, price = value)

    # L210.UnlimitRsrcPrice: prices for unlimited resources
    L210.UnlimitRsrcPrice <- L210.RsrcInfo %>%
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
      # ensure we fill in tech change for any years not in the assumption file
      complete(nesting(resource, subresource, SSP), year = c(year, MODEL_FUTURE_YEARS)) %>%
      group_by(SSP, resource, subresource) %>%
      # NOTE: interpolating is not appropriate for tech change values given the way they are
      # applied to replicate behavior with addition timesteps we should fill the missing years
      # with the next available value (`.direction = "up"`)
      tidyr::fill(value, .direction = "up") %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
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
      # Note: we give many digits here in case some grades had to be added to be able
      # to cover calibrated historical production where such resolution will be needed
      mutate(available = round(available, energy.DIGITS_CALOUTPUT)) %>%
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
             share.weight = if_else(year %in% MODEL_BASE_YEARS, 0, 1) ) %>%
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

    # A10.EnvironCost_SSPs doesn't have costs for new base-years
    # Linearly extrapolate from zero to the value provided in the input file for 2100.
    A10.EnvironCost_SSPs  <- A10.EnvironCost_SSPs %>%
      complete(nesting(SSP, resource, reserve.subresource, resource.reserve.technology),
               year = c(MODEL_FINAL_BASE_YEAR, max(MODEL_YEARS))) %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      complete(nesting(SSP, resource, reserve.subresource, resource.reserve.technology), year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
      group_by(SSP, resource, reserve.subresource, resource.reserve.technology) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS)

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
    L210.pcgdp_max_base_year <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "SSP4",
             year == MODEL_FINAL_BASE_YEAR) %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = value * gdp_deflator(2010, 1990))

    # Define high and low growth regions
    L210.high_reg <- get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "high")
    L210.low_reg <- get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low")

    # Following code only adjusts SSP4 environmental costs to match the SSP4 storyline,
    # where environmental costs are more strongly differentiated between regions
    L210.RsrcEnvironCost_SSP4 <- L210.RsrcEnvironCost_SSP4 %>%
      # Set environmental costs for coal to 0 for low growth regions,
      # 10 * environcost for high growth regions
      mutate(input.cost = if_else(resource == "coal" & region %in% L210.low_reg, 0, input.cost),
             input.cost = if_else(resource == "coal" & region %in% L210.high_reg, 10 * input.cost, input.cost)) %>%
      add_title("Environmental Costs for Depletable Resources: SSP4", overwrite = TRUE) %>%
      add_units("$/GJ") %>%
      add_comments("A10.EnvironCost_SSPs written to all regions") %>%
      add_comments("EnvironCost adjusted for high growth and low growth regions ") %>%
      add_legacy_name("L210.RsrcEnvironCost_SSP4", overwrite = TRUE) %>%
      add_precursors("energy/A10.EnvironCost_SSPs", "common/GCAM_region_names", "energy/A10.subrsrc_info", "L102.pcgdp_thous90USD_Scen_R_Y")

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

    # interpolating tech costs to cover all model years
    A21.globalrsrctech_cost %>%
      complete(nesting(resource, reserve.subresource, resource.reserve.technology, minicam.non.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      group_by(resource, reserve.subresource, resource.reserve.technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) -> A21.globalrsrctech_cost

    L210.ResSubresourceProdLifetime %>%
      mutate(resource.reserve.technology = reserve.subresource,
             invest_lifetime = avg.prod.lifetime / 2,
             FCR = (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime) / ((1+socioeconomics.DEFAULT_INTEREST_RATE)^invest_lifetime -1),
             capital.coef = socioeconomics.RESOURCE_CAPITAL_RATIO / FCR,
             minicam.non.energy.input = "investment-cost",
             tracking.market = "capital") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechInvestmentInput"]]) ->
      L210.ResReserveTechInvestmentInput

    A21.globalrsrctech_cost %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechCost"]]) -> L210.ResTechCost

    # write tech coefficients for the resource tech energy inputs
    A21.globalrsrctech_coef %>%
      complete(nesting(resource, reserve.subresource, resource.reserve.technology, minicam.energy.input), year = MODEL_YEARS) %>%
      arrange(year) %>%
      group_by(resource, reserve.subresource, resource.reserve.technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) -> A21.globalrsrctech_coef

    A21.globalrsrctech_coef %>%
      repeat_add_columns(GCAM_region_names) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechCoef"]])-> L210.ResTechCoef


    # We need to make sure we have at least a shell technology for ALL resources
    # and so we will just use the share weight table to facilitate doing that.
    A10.subrsrc_info %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(L210.RsrcCalProd %>%
                  mutate(prod_value = as.double(cal.production)),
                by = c("region", "resource", "subresource", "year")) %>%
      mutate(prod_value = if_else(is.na(prod_value), 0, prod_value),
             technology = subresource,
             share.weight = 1) %>%
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
      add_precursors("energy/A_regions", "common/GCAM_region_names",
                     "energy/A10.rsrc_info_fossils",
                     "energy/A10.rsrc_info_renewables_others",
                     "energy/A10.rsrc_info_uranium") ->
      L210.Rsrc

    # Resource info for various resource markets including
    # resource_type	market	output-unit	price-unit	year	value
    L210.rsrc_info %>%
      add_title("Market information for resources, including historical prices") %>%
      add_units("1975$/unit") %>%
      add_comments("A10.rsrc_info") %>%
      add_legacy_name("A10.rsrc_info") %>%
      same_precursors_as(L210.Rsrc) ->
      L210.rsrc_info

    L210.RenewRsrc %>%
      add_title("Market information for renewable resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.RenewRsrc") %>%
      same_precursors_as(L210.Rsrc)  ->
      L210.RenewRsrc

    L210.UnlimitRsrc %>%
      add_title("Market information for unlimited resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_legacy_name("L210.UnlimitRsrc") %>%
      same_precursors_as(L210.Rsrc) ->
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

    L210.ResReserveTechInvestmentInput %>%
      add_title("Non-energy input to keep track of resource curve investment cost") %>%
      add_units("NA") %>%
      add_comments("A resource reserve tech needs an input to keep track of the resource") %>%
      add_comments("curves cost that was used when the tech was invested.  It will be") %>%
      add_comments("for calculating shutdown deciders but also, this input will track") %>%
      add_comments("capital demands so those parameters are also read in") %>%
      same_attributes_as(L210.ResSubresourceProdLifetime) ->
      L210.ResReserveTechInvestmentInput

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

    return_data(L210.Rsrc, L210.rsrc_info, L210.RenewRsrc, L210.UnlimitRsrc, L210.RsrcPrice, L210.RenewRsrcPrice, L210.UnlimitRsrcPrice, L210.RsrcTechChange,
                L210.SmthRenewRsrcTechChange, L210.SmthRenewRsrcTechChange_offshore_wind, L210.RsrcCalProd, L210.ReserveCalReserve, L210.RsrcCurves_fos, L210.RsrcCurves_U, L210.SmthRenewRsrcCurves_MSW,
                L210.SmthRenewRsrcCurves_wind, L210.SmthRenewRsrcCurves_offshore_wind, L210.SmthRenewRsrcCurvesGdpElast_roofPV, L210.GrdRenewRsrcCurves_geo, L210.GrdRenewRsrcMax_geo,
                L210.GrdRenewRsrcCurves_EGS, L210.GrdRenewRsrcMax_EGS, L210.GrdRenewRsrcCurves_tradbio, L210.GrdRenewRsrcMax_tradbio, L210.RsrcTechChange_SSP1,
                L210.RsrcEnvironCost_SSP1, L210.RsrcTechChange_SSP2, L210.RsrcEnvironCost_SSP2, L210.RsrcTechChange_SSP3, L210.RsrcEnvironCost_SSP3,
                L210.RsrcTechChange_SSP4, L210.RsrcEnvironCost_SSP4, L210.RsrcTechChange_SSP5, L210.RsrcEnvironCost_SSP5,
                L210.ResSubresourceProdLifetime, L210.SubresourcePriceAdder, L210.ResReserveTechLifetime, L210.ResReserveTechDeclinePhase, L210.ResReserveTechProfitShutdown, L210.ResReserveTechInvestmentInput,
                L210.ResTechShrwt, L210.ResTechShrwt_EGS, L210.ResTechCoef, L210.ResTechCost)
  } else {
    stop("Unknown command")
  }
}

