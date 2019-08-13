#' module_gcamindia_L210.resources
#'
#' GCAM-india resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DeleteRenewRsrc_indiarsrc}, \code{L210.DeleteUnlimitRsrc_indiarsrc}, \code{L210.india_state_RenewRsrc},
#' \code{L210.india_state_UnlimitRsrc}, \code{L210.india_state_UnlimitRsrcPrice}, \code{L210.india_state_SmthRenewRsrcTechChange},
#' \code{L210.india_state_SmthRenewRsrcCurves_wind}, \code{L210.india_state_GrdRenewRsrcCurves_geo}, \code{L210.india_state_GrdRenewRsrcMax_geo},
#' \code{L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV}.
#' The corresponding file in the
#' original data system was \code{L210.resources_india.R} (gcam-india level2).
#' @details GCAM-india resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH November 2017

module_gcamindia_L210.resources <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-india/A23.india_re_technical_potential",
             FILE = "gcam-india/A23.india_state_wind",
             "L115.india_state_rsrc_rooftopPV",
             "L1231.india_state_out_EJ_elec_F_tech",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.UnlimitRsrcPrice",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.DeleteRenewRsrc_indiarsrc", #
             "L210.DeleteUnlimitRsrc_indiarsrc", #
             "L210.india_state_RenewRsrc", #
             "L210.india_state_UnlimitRsrc", #
             "L210.india_state_UnlimitRsrcPrice", #
             "L210.india_state_SmthRenewRsrcTechChange", #
             "L210.india_state_SmthRenewRsrcCurves_wind", #
             "L210.india_state_GrdRenewRsrcCurves_geo", #
             "L210.india_state_GrdRenewRsrcMax_geo", #
             "L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV")) #
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    Geothermal_Hydrothermal_GWh <- State <- available <- b_exp <- cost_modifier <- curve.exponent <- curve_exponent <-
      extractioncost <- generation <- geothermal <- grade <- grade_share <- maxResource <- maxSubResource <- mid.price <-
      mid_p <- mid_price <- object <- offtake <- offtake_share <- region <- renewresource <- smooth.renewable.subresource <-
      state <- unlimited.resource <- value <- year <- year.fillout <- . <- NULL

    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.india_re_technical_potential <- get_data(all_data, "gcam-india/A23.india_re_technical_potential")
    A23.india_state_wind <- get_data(all_data, "gcam-india/A23.india_state_wind")
    L115.india_state_rsrc_rooftopPV <- get_data(all_data, "L115.india_state_rsrc_rooftopPV")
    L1231.india_state_out_EJ_elec_F_tech <- get_data(all_data, "L1231.india_state_out_EJ_elec_F_tech")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV")
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo")
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo")
    # ===================================================

    # NOTE: geothermal resource is not created in the states considered to have zero hydrothermal production available
    A23.india_re_technical_potential <- A23.india_re_technical_potential %>%
      # remove TOTAL row
      filter(State != "TOTAL") %>%
      # Add state abbreviation
      left_join_error_no_match(india_states_subregions, by = c("State" = "state_name")) %>%
      # Convert geothermal to EJ
      mutate(geothermal = Geothermal_Hydrothermal_GWh * CONV_GWH_EJ / gcamindia.GEOTHERMAL_DEFAULT_EFFICIENCY,
             renewresource = "geothermal") %>%
      select(region = state, renewresource, geothermal)

    # States that do not use geothermal
    geo_states_noresource <- A23.india_re_technical_potential %>%
      filter(geothermal == 0) %>%
      select(region, renewresource)

    # States that use geothermal
    geo_states <- filter(A23.india_re_technical_potential, geothermal > 0)$region %>% unique()

        # L210.DeleteRenewRsrc_indiarsrc: remove selected renewable resources from the india region
    L210.DeleteRenewRsrc_indiarsrc <- L210.RenewRsrc %>%
      filter(region == gcam.india_REGION,
             renewresource %in% gcamindia.STATE_RENEWABLE_RESOURCES) %>%
      select(region, renewresource) %>%
    # filtering and selecting was not removing attributes from L210.RenewRsrc, so we add this meaningless mutate
    mutate(region = region)

    # L210.DeleteUnlimitRsrc_indiarsrc: remove selected renewable resources from the india region
    L210.DeleteUnlimitRsrc_indiarsrc <- L210.UnlimitRsrc %>%
      filter(region == gcam.india_REGION,
             unlimited.resource %in% gcamindia.STATE_UNLIMITED_RESOURCES) %>%
      select(region, unlimited.resource) %>%
      # filtering and selecting was not removing attributes from L210.UnlimitRsrc, so we add this meaningless mutate
      mutate(region = region)


   # L210.india_state_RenewRsrc: renewable resource info in the states
    L210.india_state_RenewRsrc <- L210.RenewRsrc %>%
      filter(region == gcam.india_REGION,
             renewresource %in% gcamindia.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_india_states(LEVEL2_DATA_NAMES[["RenewRsrc"]]) %>%
      # Remove geothermal from states that don't have it
      anti_join(geo_states_noresource, by = c("region", "renewresource")) %>%
      mutate(market = region)

    # L210.india_state_UnlimitRsrc: unlimited resource info in the states
    L210.india_state_UnlimitRsrc <- L210.UnlimitRsrc %>%
      filter(region == gcam.india_REGION,
             unlimited.resource %in% gcamindia.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_india_states(LEVEL2_DATA_NAMES[["UnlimitRsrc"]])

    L210.india_state_UnlimitRsrc <- L210.india_state_UnlimitRsrc %>%
      filter(unlimited.resource != "limestone")

    # L210.india_state_UnlimitRsrcPrice: unlimited resource prices in the states
    L210.india_state_UnlimitRsrcPrice <- L210.UnlimitRsrcPrice %>%
      filter(region == gcam.india_REGION,
             unlimited.resource %in% gcamindia.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_india_states(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]])


    L210.india_state_UnlimitRsrcPrice <- L210.india_state_UnlimitRsrcPrice %>%
      filter(unlimited.resource != "limestone")

    # L210.india_state_SmthRenewRsrcTechChange: smooth renewable resource tech change
    L210.india_state_SmthRenewRsrcTechChange <- L210.SmthRenewRsrcTechChange %>%
      filter(region == gcam.india_REGION,
             renewresource %in% gcamindia.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_india_states(LEVEL2_DATA_NAMES[["SmthRenewRsrcTechChange"]]) %>%
      # If geothermal is included in this table, remove states that don't exist
      anti_join(geo_states_noresource, by = c("region", "renewresource"))

    # L210.india_state_SmthRenewRsrcCurves_wind: wind resource curves in the states
    L210.india_state_SmthRenewRsrcCurves_wind <- L210.SmthRenewRsrcCurves_wind %>%
      filter(region == gcam.india_REGION) %>%
      repeat_add_columns(tibble(state = gcamindia.STATES)) %>%
      # Add in new maxSubResource, mid.price, and curve.exponent from A23.india_state_wind
      left_join_error_no_match(A23.india_state_wind, by = c("state" = "region")) %>%
      # Convert A23.india_state_wind units from 2007$/kWh to 1975$/GJ
      mutate(mid_price = mid_price * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
      select(region = state, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource = maxResource, mid.price = mid_price, curve.exponent = curve_exponent)

    # L210.india_state_GrdRenewRsrcCurves_geo: geothermal resource curves in the states
    L210.india_state_GrdRenewRsrcCurves_geo <- L210.GrdRenewRsrcCurves_geo %>%
      filter(region == gcam.india_REGION) %>%
      mutate(change_cost = extractioncost - lag(extractioncost))

    # Calculate the cost increment between grades 1 and 2
    L210.GeoGrade2Increment <- L210.india_state_GrdRenewRsrcCurves_geo %>%
      summarise(value = extractioncost[grade == "grade 2"] - extractioncost[grade == "grade 1"])

    # Calculate the share of the resource to allocate to each grade
    # NOTE: The method here gives precendence to the state-level data in defining the quantities
    L210.india_state_GrdRenewRsrcCurves_geo <- L210.india_state_GrdRenewRsrcCurves_geo %>%
      mutate(grade_share = available / sum(available)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = geo_states)) %>%
      left_join_error_no_match(A23.india_re_technical_potential, by = c("region", "renewresource")) %>%
      mutate(available = round(grade_share * geothermal, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(L1231.india_state_out_EJ_elec_F_tech %>%
                                 filter(year == max(HISTORICAL_YEARS)), by = c("region" = "state", "renewresource" = "fuel")) %>%
      # Each state is assigned the same cost points, even though costs are obviously different by state
      # We don't have any data indicating the cost of the next geothermal power station by state, so we'll use the historical generation
      # to modify the floor of the cost curve in each state. This is ad-hoc and can be improved at some point.
      mutate(offtake = value / gcamindia.GEOTHERMAL_DEFAULT_EFFICIENCY,
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

    #In India, as geothermal generation data is not available, the grade 1 availability has been made eqial to 0,
    # and the cost has been made equal to 0.001 so that the NA and 0 values don't create problems.
    # This has to be done as the code above has been currently copied from the US which has got geothermal data. This
    # might not work for India, and the above code should be modified for India later.
    L210.india_state_GrdRenewRsrcCurves_geo$available[which(L210.india_state_GrdRenewRsrcCurves_geo$available == 0)] <- 0
    L210.india_state_GrdRenewRsrcCurves_geo[is.na(L210.india_state_GrdRenewRsrcCurves_geo)] <- 0.001


    # Maximum resources: currently assuming this is just set to 1, and the resource info is stored in the grades
    # L210.india_state_GrdRenewRsrcMax_geo: max sub resource for geothermal (placeholder)
    L210.india_state_GrdRenewRsrcMax_geo <- L210.GrdRenewRsrcMax_geo %>%
      filter(region == gcam.india_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = geo_states))

    # L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV: rooftop PV resource curves in the states
    L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV <- L210.SmthRenewRsrcCurvesGdpElast_roofPV %>%
      filter(region == gcam.india_REGION) %>%
      select(-region, -maxSubResource, -mid.price, -curve.exponent) %>%
      write_to_all_india_states(names = c(names(.), "region")) %>%
      left_join_error_no_match(L115.india_state_rsrc_rooftopPV, by = c("region" = "state")) %>%
      rename(maxSubResource = generation, mid.price = mid_p, curve.exponent = b_exp)
    # ===================================================

    # Produce outputs
    L210.DeleteRenewRsrc_indiarsrc %>%
      add_title("Remove selected renewable resources from the india region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteRenewRsrc_indiarsrc") %>%
      add_precursors("L210.RenewRsrc") ->
      L210.DeleteRenewRsrc_indiarsrc

    L210.DeleteUnlimitRsrc_indiarsrc %>%
      add_title("Remove selected unlimited resources from the india region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteUnlimitRsrc_indiarsrc") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.DeleteUnlimitRsrc_indiarsrc


    L210.india_state_RenewRsrc %>%
      add_title("Renewable resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered and written to all states") %>%
      add_legacy_name("L210.india_state_RenewRsrc") %>%
      add_precursors("L210.RenewRsrc", "gcam-india/A23.india_re_technical_potential", "gcam-india/india_states_subregions") ->
      L210.india_state_RenewRsrc

    L210.india_state_UnlimitRsrc %>%
      add_title("Unlimited resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all states") %>%
      add_legacy_name("L210.india_state_UnlimitRsrc") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.india_state_UnlimitRsrc


    L210.india_state_UnlimitRsrcPrice %>%
      add_title("Unlimited resource prices in the states") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all states") %>%
      add_legacy_name("L210.india_state_UnlimitRsrcPrice") %>%
      add_precursors("L210.UnlimitRsrcPrice") ->
      L210.india_state_UnlimitRsrcPrice


    L210.india_state_SmthRenewRsrcTechChange %>%
      add_title("Smooth renewable resource tech change: india") %>%
      add_units("Unitless") %>%
      add_comments("L210.SmthRenewRsrcTechChange filtered and written to all states") %>%
      add_legacy_name("L210.india_state_SmthRenewRsrcTechChange") %>%
      add_precursors("L210.SmthRenewRsrcTechChange", "energy/calibrated_techs") ->
      L210.india_state_SmthRenewRsrcTechChange

    L210.india_state_SmthRenewRsrcCurves_wind %>%
      add_title("Wind resource curves in the states") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("L210.SmthRenewRsrcCurves_wind filtered and written to all states") %>%
      add_legacy_name("L210.india_state_SmthRenewRsrcCurves_wind") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-india/A23.india_state_wind") ->
      L210.india_state_SmthRenewRsrcCurves_wind

    L210.india_state_GrdRenewRsrcCurves_geo %>%
      add_title("Geothermal resource curves in the states") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("india data from L210.GrdRenewRsrcCurves_geo shared out with A23.india_re_technical_potential") %>%
      add_legacy_name("L210.india_state_GrdRenewRsrcCurves_geo") %>%
      add_precursors("L210.GrdRenewRsrcCurves_geo", "gcam-india/A23.india_re_technical_potential", "L1231.india_state_out_EJ_elec_F_tech") ->
      L210.india_state_GrdRenewRsrcCurves_geo

    L210.india_state_GrdRenewRsrcMax_geo %>%
      add_title("Max sub resource for geothermal (placeholder)") %>%
      add_units("Unitless") %>%
      add_comments("L210.GrdRenewRsrcMax_geo filtered and written to relevant states, constant value used") %>%
      add_legacy_name("L210.india_state_GrdRenewRsrcMax_geo") %>%
      add_precursors("L210.GrdRenewRsrcMax_geo") ->
      L210.india_state_GrdRenewRsrcMax_geo

    L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV %>%
      add_title("Rooftop PV resource curves by state") %>%
      add_units("maxSubResource: EJ; mid.price = 1975$/GJ") %>%
      add_comments("Values from L115.india_state_rsrc_rooftopPV added to categories from L210.SmthRenewRsrcCurvesGdpElast_roofPV") %>%
      add_legacy_name("L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV") %>%
      add_precursors("L210.SmthRenewRsrcCurvesGdpElast_roofPV", "L115.india_state_rsrc_rooftopPV") ->
      L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV

    return_data(L210.DeleteRenewRsrc_indiarsrc, L210.DeleteUnlimitRsrc_indiarsrc, L210.india_state_RenewRsrc, L210.india_state_UnlimitRsrc,
                L210.india_state_UnlimitRsrcPrice, L210.india_state_SmthRenewRsrcTechChange, L210.india_state_SmthRenewRsrcCurves_wind,
                L210.india_state_GrdRenewRsrcCurves_geo, L210.india_state_GrdRenewRsrcMax_geo, L210.india_state_SmthRenewRsrcCurvesGdpElast_roofPV)
  } else {
    stop("Unknown command")
  }
}
