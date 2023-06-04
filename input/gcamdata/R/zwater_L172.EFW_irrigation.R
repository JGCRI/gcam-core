# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L172.EFW_irrigation
#'
#' Energy requirements for irrigation water abstraction, by GCAM region
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L172.Coef_GJm3_IrrEnergy_R},
#'   \code{L172.in_EJ_R_irr_F_Yh}.
#' @details Generate estimates of energy consumption for irrigation water abstraction (i.e., pumping and conveyance),
#'   including energy input-output coefficients, by GCAM region, fuel, and historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by left_join mutate select summarise ungroup
#' @author GPK January 2019
module_water_L172.EFW_irrigation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/EFW_mapping",
             FILE = "water/Liu_EFW_inventory",
             "L165.ag_IrrEff_R",
             "L165.IrrWithd_km3_R_Y",
             "L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L172.Coef_GJm3_IrrEnergy_R",
             "L172.in_EJ_R_irr_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- year <- GCAM_region_ID <- ag <- pow_irr_gw <- gw_sc_EI_coef <-
      pow_irr_sf <- ag_energy_gw <- ag_energy_sw <- ag_energy <-
      conveyance.eff <- IrrWithd_km3 <- coefficient <- water_km3 <-
      sector <- fuel <- value <- energy_EJ <- avail_energy_EJ <- scaler <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    Liu_EFW_inventory <- get_data(all_data, "water/Liu_EFW_inventory")

    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L165.IrrWithd_km3_R_Y <- get_data(all_data, "L165.IrrWithd_km3_R_Y", strip_attributes = TRUE)
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")

    # ===================================================

    # Part 1: Disaggregating region-level irrigation ("blue") water withdrawals to surface water and groundwater, to
    # compute average electricity-water input-output coefficients
    # The method here uses the country-level inventory data put together by Yaling Liu et al (2016). Rather than using
    # the energy quantities, which are built up from different estimates of water withdrawals than the bottom-up
    # estimates used in GCAM, and include an exogenous primary:electric conversion factor, we instead use the provided
    # energy intensities, and surface versus groundwater ratios

    # Replace non-standard iso code
    Liu_EFW_inventory$iso[ Liu_EFW_inventory$iso == "vtc" ] <- "vat"

    # Our India ag EFW values by the default method (0.2 EJ in 2015) are low compared with the literature, even Liu et
    # al which had 0.36 in 2010. For ~2015 the IEA estimates it at 0.6; Indian Council of Agricultural Research has 0.7
    # (cited in Sidhu et al. 2020, World Development), and a Brookings report (Ali 2018) has 0.6. From the latter, "Of
    # the irrigated area, more  than  two-thirds  is  via  groundwater  sources." The following steps increase the
    # groundwater share, and the groundwater pumping energy intensity, in order to return estimates more consistent with
    # the literature in this country
    L172.Liu_EFW_inventory <- Liu_EFW_inventory
    L172.Liu_EFW_inventory$pow_irr_gw[L172.Liu_EFW_inventory$iso == "ind"] <- 0.7
    L172.Liu_EFW_inventory$pow_irr_sf[L172.Liu_EFW_inventory$iso == "ind"] <- 0.3
    L172.Liu_EFW_inventory$gw_sc_EI_coef[L172.Liu_EFW_inventory$iso == "ind"] <- 1.7
    L172.Liu_EFW_inventory <- L172.Liu_EFW_inventory %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      mutate(ag_energy_gw = ag * pow_irr_gw * gw_sc_EI_coef * efw.GW_ABSTRACTION_EFW,
             ag_energy_sw = ag * pow_irr_sf * efw.SW_ABSTRACTION_EFW,
             ag_energy = ag_energy_gw + ag_energy_sw)

    # Calculate the weighted average irrigation-related EFW from country to region.
    # For any GCAM regions constructed entirely of countries not in the inventory, back-fill with the median value
    L172.Coef_GJm3_IrrEnergy_R <- group_by(L172.Liu_EFW_inventory, GCAM_region_ID) %>%
      summarise(ag = sum(ag),
                ag_energy = sum(ag_energy)) %>%
      mutate(coefficient = ag_energy / ag) %>%
      select(GCAM_region_ID, coefficient) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID)) %>%
      mutate(coefficient = if_else(is.na(coefficient), median(coefficient, na.rm = T), coefficient))

    # Part 2: Estimating the electricity consumption of irrigation water abstraction by region
    # This is somewhat complicated. The irrigation withdrawal volume computed in the aglu module is the irrigation water
    # provided to the agricultural sector, NOT including upstream conveyance losses. However the energy for water
    # abstraction is estimated from the total water withdrawn from a source. This correction is applied here to get the
    # correct volume of water from which to compute energy consumption.

    # TODO: modify this to allow multiple fuels to be used for irrigation pumping (e.g., Pakistan, India)

    L172.in_EJ_R_irr_F_Yh <- L165.IrrWithd_km3_R_Y %>%
      left_join_error_no_match(select(L165.ag_IrrEff_R, GCAM_region_ID, conveyance.eff),
                               by = "GCAM_region_ID") %>%
      mutate(water_km3 = IrrWithd_km3 / conveyance.eff,
             sector = unique(EFW_mapping$sector[grepl("irrig", EFW_mapping$sector)]),
             fuel = unique(EFW_mapping$fuel[grepl("irrig", EFW_mapping$sector)])) %>%
      left_join_error_no_match(select(L172.Coef_GJm3_IrrEnergy_R, GCAM_region_ID, coefficient),
                               by = "GCAM_region_ID") %>%
      mutate(energy_EJ = water_km3 * coefficient)

    # Perform top-down check and scale coefficients, as necessary
    agEFW_from_sectors <- unique(c(EFW_mapping$from.sector[grepl("irrig", EFW_mapping$sector)],
                                   EFW_mapping$from.sector.2[grepl("irrig", EFW_mapping$sector)]))
    agEFW_from_fuel <- unique(EFW_mapping$fuel[grepl("irrig", EFW_mapping$sector)])
    L172.in_EJavail_ag <- subset(L1011.en_bal_EJ_R_Si_Fi_Yh,
                                 sector %in% agEFW_from_sectors &
                                   fuel %in% agEFW_from_fuel) %>%
      # If there are multiple sectors from which energy is allowed to be pulled, aggregate them.
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(avail_energy_EJ = sum(value)) %>%
      ungroup()

    # Join this back into the dataset above and, as necessary, scale down the energy requirements so that they remain
    # below the exogenous threshold of available energy that is allowed to be re-assigned to irrigation pumping
    L172.in_EJ_R_irr_F_Yh <- left_join_error_no_match(L172.in_EJ_R_irr_F_Yh, L172.in_EJavail_ag,
                                                      by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(scaler = if_else(energy_EJ / avail_energy_EJ > efw.MAX_AGCOMM_ENERGY_IRR,
                              efw.MAX_AGCOMM_ENERGY_IRR / (energy_EJ / avail_energy_EJ), 1),
             coefficient = coefficient * scaler,
             energy_EJ = energy_EJ * scaler)

    # Re-write the coefficient table with scaled coefs, and prepare both coef and energy consumption for write-out
    L172.Coef_GJm3_IrrEnergy_R <- select(L172.in_EJ_R_irr_F_Yh, GCAM_region_ID, sector, fuel, year, coefficient)
    L172.in_EJ_R_irr_F_Yh <- select(L172.in_EJ_R_irr_F_Yh, GCAM_region_ID, sector, fuel, year, energy_EJ)

    # ===================================================

    # Produce outputs
    L172.Coef_GJm3_IrrEnergy_R %>%
      add_title("Average energy-water input-output coefficient for irrigation water abstraction by region and fuel") %>%
      add_units("GJ/m^3") %>%
      add_comments("Coefs reflect shares of surface and groundwater, and are adjusted further for energy balancing") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/EFW_mapping",
                     "water/Liu_EFW_inventory",
                     "L165.ag_IrrEff_R",
                     "L165.IrrWithd_km3_R_Y",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh") ->
      L172.Coef_GJm3_IrrEnergy_R

    L172.in_EJ_R_irr_F_Yh %>%
      add_title("Irrigation water abstraction energy consumption by region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Energy quantities calculated as water withdrawal volumes multiplied by input-output coefficients") %>%
      same_precursors_as(L172.Coef_GJm3_IrrEnergy_R) ->
      L172.in_EJ_R_irr_F_Yh

    return_data(L172.Coef_GJm3_IrrEnergy_R,
                L172.in_EJ_R_irr_F_Yh)
  } else {
    stop("Unknown command")
  }
}
