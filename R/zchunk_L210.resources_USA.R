# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.usa_L210.resources_USA
#'
#' GCAM-USA resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DeleteRenewRsrc_USArsrc}, \code{L210.DeleteUnlimitRsrc_USArsrc}, \code{L210.RenewRsrc_USA},
#' \code{L210.UnlimitRsrc_USA}, \code{L210.UnlimitRsrcPrice_USA}, \code{L210.SmthRenewRsrcTechChange_USA},
#' \code{L210.SmthRenewRsrcCurves_wind_USA}, \code{L210.GrdRenewRsrcCurves_geo_USA}, \code{L210.GrdRenewRsrcMax_geo_USA},
#' \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA}, \code{L210.DeleteUnlimitRsrc_USAlimestone},
#' \code{L210.UnlimitRsrc_limestone_USA}, \code{L210.UnlimitRsrcPrice_limestone_USA}, \code{L210.ResTechShrwt_USA}. The corresponding file in the
#' original data system was \code{L210.resources_USA.R} (gcam-usa level2).
#' @details GCAM-USA resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter if_else group_by lag mutate select summarise
#' @importFrom tidyr gather spread
#' @author RLH November 2017

module_gcamusa_L210.resources_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "gcam-usa/us_state_wind",
             "L115.rsrc_state_rooftopPV",
             "L1231.out_EJ_state_elec_F_tech",
             "L1321.out_Mt_state_cement_Yh",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.UnlimitRsrcPrice",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.DeleteRenewRsrc_USArsrc", #
             "L210.DeleteUnlimitRsrc_USArsrc", #
             "L210.RenewRsrc_USA", #
             "L210.UnlimitRsrc_USA", #
             "L210.UnlimitRsrcPrice_USA", #
             "L210.SmthRenewRsrcTechChange_USA", #
             "L210.SmthRenewRsrcCurves_wind_USA", #
             "L210.GrdRenewRsrcCurves_geo_USA", #
             "L210.GrdRenewRsrcMax_geo_USA", #
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA", #
             "L210.DeleteUnlimitRsrc_USAlimestone", #
             "L210.UnlimitRsrc_limestone_USA", #
             "L210.UnlimitRsrcPrice_limestone_USA",
             "L210.ResTechShrwt_USA")) #
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    Geothermal_Hydrothermal_GWh <- State <- available <- b_exp <- cost_modifier <- curve.exponent <- curve_exponent <-
      extractioncost <- generation <- geothermal <- grade <- grade_share <- maxResource <- maxSubResource <- mid.price <-
      mid_p <- mid_price <- object <- offtake <- offtake_share <- region <- renewresource <- smooth.renewable.subresource <-
      state <- unlimited.resource <- value <- year <- year.fillout <- . <-
      sub.renewable.resource <- subresource <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    us_state_wind <- get_data(all_data, "gcam-usa/us_state_wind")
    L115.rsrc_state_rooftopPV <- get_data(all_data, "L115.rsrc_state_rooftopPV")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")
    L1321.out_Mt_state_cement_Yh <- get_data(all_data, "L1321.out_Mt_state_cement_Yh")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV")
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo")
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo")
    # ===================================================
    # States that produce cement
    cement_states <- unique(L1321.out_Mt_state_cement_Yh$state)

    # NOTE: geothermal resource is not created in the states considered to have zero hydrothermal production available
    NREL_us_re_technical_potential <- NREL_us_re_technical_potential %>%
      # remove TOTAL row
      filter(State != "TOTAL") %>%
      # Add state abbreviation
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      # Convert geothermal to EJ
      mutate(geothermal = Geothermal_Hydrothermal_GWh * CONV_GWH_EJ / gcamusa.GEOTHERMAL_DEFAULT_EFFICIENCY,
             renewresource = "geothermal") %>%
      select(region = state, renewresource, geothermal)

    # States that do not use geothermal
    geo_states_noresource <- NREL_us_re_technical_potential %>%
      filter(geothermal == 0) %>%
      select(region, renewresource)

    # States that use geothermal
    geo_states <- filter(NREL_us_re_technical_potential, geothermal > 0)$region %>% unique()

    # NOTE: keeping limestone resources separate, written out to XML batch file for cement
    # L210.DeleteRenewRsrc_USArsrc: remove selected renewable resources from the USA region
    L210.DeleteRenewRsrc_USArsrc <- L210.RenewRsrc %>%
      filter(region == gcam.USA_REGION,
             renewresource %in% gcamusa.STATE_RENEWABLE_RESOURCES) %>%
      select(region, renewresource) %>%
    # filtering and selecting was not removing attributes from L210.RenewRsrc, so we add this meaningless mutate
    mutate(region = region)

    # L210.DeleteUnlimitRsrc_USArsrc: remove selected renewable resources from the USA region
    L210.DeleteUnlimitRsrc_USArsrc <- L210.UnlimitRsrc %>%
      filter(region == gcam.USA_REGION,
             unlimited.resource %in% gcamusa.STATE_UNLIMITED_RESOURCES) %>%
      select(region, unlimited.resource) %>%
      # filtering and selecting was not removing attributes from L210.UnlimitRsrc, so we add this meaningless mutate
      mutate(region = region)

    # Separate into limestone and other unlimited resource
    L210.DeleteUnlimitRsrc_USAlimestone <- L210.DeleteUnlimitRsrc_USArsrc %>%
      filter(unlimited.resource == "limestone")

    L210.DeleteUnlimitRsrc_USArsrc <- L210.DeleteUnlimitRsrc_USArsrc %>%
      filter(unlimited.resource != "limestone")

    # L210.RenewRsrc_USA: renewable resource info in the states
    L210.RenewRsrc_USA <- L210.RenewRsrc %>%
      filter(region == gcam.USA_REGION,
             renewresource %in% gcamusa.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["RenewRsrc"]]) %>%
      # Remove geothermal from states that don't have it
      anti_join(geo_states_noresource, by = c("region", "renewresource")) %>%
      mutate(market = region)

    # L210.UnlimitRsrc_USA: unlimited resource info in the states
    L210.UnlimitRsrc_USA <- L210.UnlimitRsrc %>%
      filter(region == gcam.USA_REGION,
             unlimited.resource %in% gcamusa.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["UnlimitRsrc"]])

    L210.UnlimitRsrc_limestone_USA <- L210.UnlimitRsrc_USA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_states)

    L210.UnlimitRsrc_USA <- L210.UnlimitRsrc_USA %>%
      filter(unlimited.resource != "limestone")

    # L210.UnlimitRsrcPrice_USA: unlimited resource prices in the states
    L210.UnlimitRsrcPrice_USA <- L210.UnlimitRsrcPrice %>%
      filter(region == gcam.USA_REGION,
             unlimited.resource %in% gcamusa.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]])

    L210.UnlimitRsrcPrice_limestone_USA <- L210.UnlimitRsrcPrice_USA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_states)

    L210.UnlimitRsrcPrice_USA <- L210.UnlimitRsrcPrice_USA %>%
      filter(unlimited.resource != "limestone")

    # L210.SmthRenewRsrcTechChange_USA: smooth renewable resource tech change
    L210.SmthRenewRsrcTechChange_USA <- L210.SmthRenewRsrcTechChange %>%
      filter(region == gcam.USA_REGION,
             renewresource %in% gcamusa.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SmthRenewRsrcTechChange"]]) %>%
      # If geothermal is included in this table, remove states that don't exist
      anti_join(geo_states_noresource, by = c("region", "renewresource"))

    # L210.SmthRenewRsrcCurves_wind_USA: wind resource curves in the states
    L210.SmthRenewRsrcCurves_wind_USA <- L210.SmthRenewRsrcCurves_wind %>%
      filter(region == gcam.USA_REGION) %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      # Add in new maxSubResource, mid.price, and curve.exponent from us_state_wind
      left_join_error_no_match(us_state_wind, by = c("state" = "region")) %>%
      # Convert us_state_wind units from 2007$/kWh to 1975$/GJ
      mutate(mid_price = mid_price * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
      select(region = state, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource = maxResource, mid.price = mid_price, curve.exponent = curve_exponent)

    # L210.GrdRenewRsrcCurves_geo_USA: geothermal resource curves in the states
    L210.GrdRenewRsrcCurves_geo_USA <- L210.GrdRenewRsrcCurves_geo %>%
      filter(region == gcam.USA_REGION) %>%
      mutate(change_cost = extractioncost - lag(extractioncost))

    # Calculate the cost increment between grades 1 and 2
    L210.GeoGrade2Increment <- L210.GrdRenewRsrcCurves_geo_USA %>%
      summarise(value = extractioncost[grade == "grade 2"] - extractioncost[grade == "grade 1"])

    # Calculate the share of the resource to allocate to each grade
    # NOTE: The method here gives precendence to the state-level NREL data in defining the quantities
    L210.GrdRenewRsrcCurves_geo_USA <- L210.GrdRenewRsrcCurves_geo_USA %>%
      mutate(grade_share = available / sum(available)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = geo_states)) %>%
      left_join_error_no_match(NREL_us_re_technical_potential, by = c("region", "renewresource")) %>%
      mutate(available = round(grade_share * geothermal, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(L1231.out_EJ_state_elec_F_tech %>%
                                 filter(year == max(HISTORICAL_YEARS)), by = c("region" = "state", "renewresource" = "fuel")) %>%
      # Each state is assigned the same cost points, even though costs are obviously different by state
      # We don't have any data indicating the cost of the next geothermal power station by state, so we'll use the historical generation
      # to modify the floor of the cost curve in each state. This is ad-hoc and can be improved at some point.
      mutate(offtake = value / gcamusa.GEOTHERMAL_DEFAULT_EFFICIENCY,
             offtake_share = offtake / available) %>%
      group_by(region) %>%
      mutate(offtake_share = if_else(available == 0, offtake_share[available != 0], offtake_share)) %>%
      ungroup() %>%
      # Index everything to the state with the largest share of its resource in use. That will get the floor of the cost curve
      mutate(cost_modifier = 1 - (offtake_share / max(offtake_share)),
             extractioncost = if_else(grade == "grade 1",
                                      round(extractioncost + L210.GeoGrade2Increment$value * 0.5 * cost_modifier, energy.DIGITS_COST),
                                      extractioncost)) %>%
      select(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]])

    # Maximum resources: currently assuming this is just set to 1, and the resource info is stored in the grades
    # L210.GrdRenewRsrcMax_geo_USA: max sub resource for geothermal (placeholder)
    L210.GrdRenewRsrcMax_geo_USA <- L210.GrdRenewRsrcMax_geo %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = geo_states))

    # L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA: rooftop PV resource curves in the states
    L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA <- L210.SmthRenewRsrcCurvesGdpElast_roofPV %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region, -maxSubResource, -mid.price, -curve.exponent) %>%
      write_to_all_states(names = c(names(.), "region")) %>%
      left_join_error_no_match(L115.rsrc_state_rooftopPV, by = c("region" = "state")) %>%
      rename(maxSubResource = generation, mid.price = mid_p, curve.exponent = b_exp)

    # L210.ResTechShrwt_USA: To provide a shell for the technology object in the resources
    L210.SmthRenewRsrcCurves_wind_USA %>%
      select(region, resource = renewresource, subresource = smooth.renewable.subresource) %>%
      bind_rows(select(L210.GrdRenewRsrcMax_geo_USA, region, resource = renewresource, subresource = sub.renewable.resource)) %>%
      bind_rows(select(L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA, region, resource = renewresource, subresource = smooth.renewable.subresource)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L210.ResTechShrwt_USA
    # ===================================================

    # Produce outputs
    L210.DeleteRenewRsrc_USArsrc %>%
      add_title("Remove selected renewable resources from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteRenewRsrc_USArsrc") %>%
      add_precursors("L210.RenewRsrc") ->
      L210.DeleteRenewRsrc_USArsrc

    L210.DeleteUnlimitRsrc_USArsrc %>%
      add_title("Remove selected unlimited resources from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteUnlimitRsrc_USArsrc") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.DeleteUnlimitRsrc_USArsrc

    L210.DeleteUnlimitRsrc_USAlimestone %>%
      add_title("Remove limestone from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteUnlimitRsrc_USAlimestone") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.DeleteUnlimitRsrc_USAlimestone

    L210.RenewRsrc_USA %>%
      add_title("Renewable resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered and written to all states") %>%
      add_legacy_name("L210.RenewRsrc_USA") %>%
      add_precursors("L210.RenewRsrc", "gcam-usa/NREL_us_re_technical_potential", "gcam-usa/states_subregions") ->
      L210.RenewRsrc_USA

    L210.UnlimitRsrc_USA %>%
      add_title("Unlimited resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrc_USA") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.UnlimitRsrc_USA

    L210.UnlimitRsrc_limestone_USA %>%
      add_title("Limestone info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrc_limestone_USA") %>%
      add_precursors("L210.UnlimitRsrc", "L1321.out_Mt_state_cement_Yh") ->
      L210.UnlimitRsrc_limestone_USA

    L210.UnlimitRsrcPrice_USA %>%
      add_title("Unlimited resource prices in the states") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_USA") %>%
      add_precursors("L210.UnlimitRsrcPrice") ->
      L210.UnlimitRsrcPrice_USA

    L210.UnlimitRsrcPrice_limestone_USA %>%
      add_title("Limestone prices in the states") %>%
      add_units("1975$/kg") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_limestone_USA") %>%
      add_precursors("L210.UnlimitRsrcPrice", "L1321.out_Mt_state_cement_Yh") ->
      L210.UnlimitRsrcPrice_limestone_USA

    L210.SmthRenewRsrcTechChange_USA %>%
      add_title("Smooth renewable resource tech change: USA") %>%
      add_units("Unitless") %>%
      add_comments("L210.SmthRenewRsrcTechChange filtered and written to all states") %>%
      add_legacy_name("L210.SmthRenewRsrcTechChange_USA") %>%
      add_precursors("L210.SmthRenewRsrcTechChange", "energy/calibrated_techs") ->
      L210.SmthRenewRsrcTechChange_USA

    L210.SmthRenewRsrcCurves_wind_USA %>%
      add_title("Wind resource curves in the states") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("L210.SmthRenewRsrcCurves_wind filtered and written to all states") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_wind_USA") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-usa/us_state_wind") ->
      L210.SmthRenewRsrcCurves_wind_USA

    L210.GrdRenewRsrcCurves_geo_USA %>%
      add_title("Geothermal resource curves in the states") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("USA data from L210.GrdRenewRsrcCurves_geo shared out with NREL_us_re_technical_potential") %>%
      add_legacy_name("L210.GrdRenewRsrcCurves_geo_USA") %>%
      add_precursors("L210.GrdRenewRsrcCurves_geo", "gcam-usa/NREL_us_re_technical_potential", "L1231.out_EJ_state_elec_F_tech") ->
      L210.GrdRenewRsrcCurves_geo_USA

    L210.GrdRenewRsrcMax_geo_USA %>%
      add_title("Max sub resource for geothermal (placeholder)") %>%
      add_units("Unitless") %>%
      add_comments("L210.GrdRenewRsrcMax_geo filtered and written to relevant states, constant value used") %>%
      add_legacy_name("L210.GrdRenewRsrcMax_geo_USA") %>%
      add_precursors("L210.GrdRenewRsrcMax_geo") ->
      L210.GrdRenewRsrcMax_geo_USA

    L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA %>%
      add_title("Rooftop PV resource curves by state") %>%
      add_units("maxSubResource: EJ; mid.price = 1975$/GJ") %>%
      add_comments("Values from L115.rsrc_state_rooftopPV added to categories from L210.SmthRenewRsrcCurvesGdpElast_roofPV") %>%
      add_legacy_name("L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA") %>%
      add_precursors("L210.SmthRenewRsrcCurvesGdpElast_roofPV", "L115.rsrc_state_rooftopPV") ->
      L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA

    L210.ResTechShrwt_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-usa/us_state_wind",
                     "L210.GrdRenewRsrcMax_geo", "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
                     "L115.rsrc_state_rooftopPV") ->
      L210.ResTechShrwt_USA

    return_data(L210.DeleteRenewRsrc_USArsrc, L210.DeleteUnlimitRsrc_USArsrc, L210.RenewRsrc_USA, L210.UnlimitRsrc_USA,
                L210.UnlimitRsrcPrice_USA, L210.SmthRenewRsrcTechChange_USA, L210.SmthRenewRsrcCurves_wind_USA,
                L210.GrdRenewRsrcCurves_geo_USA, L210.GrdRenewRsrcMax_geo_USA, L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA,
                L210.DeleteUnlimitRsrc_USAlimestone, L210.UnlimitRsrc_limestone_USA, L210.UnlimitRsrcPrice_limestone_USA,
                L210.ResTechShrwt_USA)
  } else {
    stop("Unknown command")
  }
}
