# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L174.EFW_municipal
#'
#' Energy requirements for municipal water use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L174.water_km3_R_muniEFW_Yh},
#'   \code{L174.in_EJ_ctry_muniEFW_F_Yh}, \code{L174.in_EJ_R_muniEFWtot_F_Yh}, \code{L174.IO_GJkm3_R_muniEFW_F_Yh},
#'   \code{L174.WWtrtfrac_R_muni_Yh}.
#' @details Generate estimates of energy consumption, energy input-output coefficients, and water flow volumes for
#'   municipal water abstraction, treatment, distribution, and wastewater treatment, by GCAM region, fuel, and
#'   historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join group_by mutate rename select summarise ungroup
#' @importFrom tidyr complete gather nesting replace_na
#' @author GPK January 2019
module_water_L174.EFW_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/EFW_mapping",
             FILE = "water/Liu_EFW_inventory",
             FILE = "water/A74.globaltech_coef",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L145.municipal_water_ctry_W_Yh_km3",
             "L145.municipal_water_eff_ctry_Yh",
             "L173.trtshr_ctry_Yh",
             "L173.in_desal_km3_ctry_muni_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L174.water_km3_R_muniEFW_Yh",
             "L174.in_EJ_ctry_muniEFW_F_Yh",
             "L174.in_EJ_R_muniEFWtot_F_Yh",
             "L174.IO_GJkm3_R_muniEFW_F_Yh",
             "L174.WWtrtfrac_R_muni_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- year <- water_type <- sector <- water_km3 <- withdrawals <-
      municipal <- efficiency <- value <- desal_km3 <- desal_muni_km3 <-
      trtshr <- pcGDP <- waterCons_km3 <- GCAM_region_ID <- muni_EFW_energy_EJ <-
      supplysector <- subsector <- technology <- minicam.energy.input <- coefficient <-
      fuel <- from.sector <- from.sector.2 <- sf_shr2 <- gw_shr2 <- gw_sc_EI_coef <-
      avail_energy_EJ <- energy_EJ <- ind_EFW_energy_EJ <- scaler <- default_coef <-
      WWtrt_km3 <- WWtrtfrac <- avail_energy_EJ <- energy_EJ <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    Liu_EFW_inventory <- get_data(all_data, "water/Liu_EFW_inventory")
    A74.globaltech_coef <- get_data(all_data, "water/A74.globaltech_coef")

    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full")
    L145.municipal_water_ctry_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_ctry_W_Yh_km3", strip_attributes = TRUE)
    L145.municipal_water_eff_ctry_Yh <- get_data(all_data, "L145.municipal_water_eff_ctry_Yh")
    L173.trtshr_ctry_Yh <- get_data(all_data, "L173.trtshr_ctry_Yh")
    L173.in_desal_km3_ctry_muni_Yh <- get_data(all_data, "L173.in_desal_km3_ctry_muni_Yh")

    # ===================================================

    # Part 1: Calculating water flow volumes for water abstraction (1a) and treatment (1b)
    # Use the EFW_mapping table to get the names of the "sector" names for these EFW processes
    muniAbs <- with(EFW_mapping, unique(sector[grepl("municipal", sector) &
                                                grepl("abstraction", sector)]))
    muniTrt <- with(EFW_mapping, unique(sector[grepl("municipal", sector) &
                                                grepl("treatment", sector) &
                                                !grepl("waste", sector)]))
    muniDist <- with(EFW_mapping, unique(sector[grepl("municipal", sector) &
                                                 grepl("distrib", sector)]))
    muniWWTrt <- with(EFW_mapping, unique(sector[grepl("municipal", sector) &
                                                  grepl("wastewater", sector)]))

    # Part 1.1: Municipal water abstraction flow volume is equal to water withdrawals
    L174.water_km3_ctry_muniAbs_Yh <- L145.municipal_water_ctry_W_Yh_km3 %>%
      mutate(sector = muniAbs) %>%
      rename(water_km3 = withdrawals)

    # Part 1.2: Water treatment flow volume is also equal to water withdrawals = abstraction
    L174.water_km3_ctry_muniTrt_Yh <- mutate(L174.water_km3_ctry_muniAbs_Yh, sector = muniTrt)

    # Part 1.3: Water distribution is also equal to water withdrawals = abstraction = treatment
    L174.water_km3_ctry_muniDist_Yh <- mutate(L174.water_km3_ctry_muniAbs_Yh, sector = muniDist)

    # Part 1.4: Treated wastewater is equal to (withdrawals - consumption) * trtshr
    # Start with the withdrawal volume, deduct consumptive uses (water evaporated, or used in products), and multiply by
    # the portion of water treated, calculated in the industrial EFW step.
    # NOTE: the accounting of desalinated water differs between the industrial manufacturing and municipal water sectors.
    # Here, as per the AQUASTAT definition of "municipal water withdrawal", the municipal water withdrawal volume includes
    # desalinated water used by municipal consumers. As such, we need to deduct this quantity of water from the municipal
    # abstraction and treatment volumes, but not adjustment is needed here to adjust the municipal wastewater flow.

    # Join data on consumption (from "efficiency") and wastewater treatment share ("trtshr") into
    # the municipal water abstraction, in order to compute the wastewater flow volume
    L174.water_km3_ctry_muniWWTrt_Yh <- L174.water_km3_ctry_muniAbs_Yh %>%
      mutate(sector = muniWWTrt) %>%
      left_join_error_no_match(L145.municipal_water_eff_ctry_Yh, by = c("iso", "year")) %>%
      left_join_error_no_match(L173.trtshr_ctry_Yh, by = c("iso", "year"),
                               ignore_columns = "trtshr") %>%
      # Not all countries are present in the trtshr data; just assume zero wastewater treatment share
      replace_na(list(trtshr = 0)) %>%
      mutate(water_km3 = water_km3 * (1 - efficiency) * trtshr,
             water_km3 = if_else(is.na(water_km3), 0, water_km3)) %>%
      select(iso, sector, year, water_km3)

    # Part 1.5: Adjust municipal water abstraction and treatment flow volumes for desalinated water
    # Abstraction and treatment-related energy for desalinated water is already accounted
    L174.water_km3_ctry_muniAbs_Yh <-
      left_join_error_no_match(L174.water_km3_ctry_muniAbs_Yh, L173.in_desal_km3_ctry_muni_Yh,
                               by = c("iso", "year"),
                               ignore_columns = "desal_muni_km3") %>%
      mutate(desal_muni_km3 = if_else(is.na(desal_muni_km3), 0, desal_muni_km3),
             water_km3 = water_km3 - desal_muni_km3) %>%
      select(-desal_muni_km3)

    L174.water_km3_ctry_muniTrt_Yh <- mutate(L174.water_km3_ctry_muniAbs_Yh, sector = muniTrt)

    # Merge the four country-level tables and aggregate by GCAM region (water flow volumes)
    # This table is written out
    L174.water_km3_R_muniEFW_Yh <- bind_rows(L174.water_km3_ctry_muniAbs_Yh,
                                             L174.water_km3_ctry_muniTrt_Yh,
                                             L174.water_km3_ctry_muniDist_Yh,
                                             L174.water_km3_ctry_muniWWTrt_Yh) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(water_km3 = sum(water_km3)) %>%
      ungroup()

    # Part 2: Calculating energy for water requirements associated with manufacturing freshwater withdrawal volumes
    # This step starts by joining water flow volumes and EFW coefficients, adjusting abstraction-related coefficients
    # to accommodate the average ground:surface water ratios in each country, and multiplying to calculate energy
    # quantities.

    # 2.1: Estimate the EFW coefficients in all historical years
    # 2.1.1: interpolate the exogenous coefficients to all historical years
    L174.globaltech_coef <- gather_years(A74.globaltech_coef, value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(HISTORICAL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient)) %>%
      ungroup() %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel, from.sector, from.sector.2),
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input"),
                               ignore_columns = "from.sector.2") %>%
      select(sector, fuel, from.sector, from.sector.2, year, coefficient)

    # 2.1.2: Adjust water abstraction EFW coefficients to reflect country-level estimates of ground:surface water ratio
    L174.in_EJ_ctry_muniAbs_Yh <- L174.water_km3_ctry_muniAbs_Yh %>%
      left_join_error_no_match(select(L174.globaltech_coef, sector, fuel, year, coefficient),
                               by = c("sector", "year")) %>%
      # Missing values are allowed here, for countries not included in the Liu inventory
      left_join(select(Liu_EFW_inventory, iso, sf_shr2, gw_shr2, gw_sc_EI_coef),
                by = "iso") %>%
      # for countries not included in the Liu et al inventory, use the surface water abstraction coefficient
      mutate(coefficient = (sf_shr2 * efw.SW_ABSTRACTION_EFW + gw_shr2 * gw_sc_EI_coef * efw.GW_ABSTRACTION_EFW),
             coefficient = if_else(is.na(coefficient), efw.SW_ABSTRACTION_EFW, coefficient))

    # 2.2: Multiply the water volumes by the EFW coefficients to compute energy demands
    L174.in_ALL_ctry_muniEFW_F_Yh <- bind_rows(L174.water_km3_ctry_muniTrt_Yh,
                                               L174.water_km3_ctry_muniDist_Yh,
                                               L174.water_km3_ctry_muniWWTrt_Yh) %>%
      left_join_error_no_match(L174.globaltech_coef, by = c("sector", "year"),
                               ignore_columns = "from.sector.2") %>%
      bind_rows(L174.in_EJ_ctry_muniAbs_Yh) %>%
      mutate(energy_EJ = water_km3 * coefficient)

    # Part 3: Aggregation of EFW energy demands, check against industrial electricity, and re-scaling EFW
    # In this part, the total national manufacturing-related EFW is checked against the nation's total industrial
    # electricity consumption. EFW for industrial water systems is not allowed to exceed an exogenous threshold, and the
    # energy demands are scaled down as necessary to meet this assumption. Thus, regions with low industrial electricity
    # demand may also be assigned low EFW coefficients on water abstraction, treatment, and wastewater treatment, but
    # the underlying water flow volumes (estimated above) are not adjusted.

    # 3.1: Prepare the top-down inventory data for joining
    # Figure out the industrial energy sector(s) from which industrial EFW is re-allocated, from EFW_mapping. If only
    # one sector is assigned, "from.sector.2" will be either null or NA, and both are OK
    muniEFW_from_sectors <- unique(c(L174.in_ALL_ctry_muniEFW_F_Yh$from.sector,
                                     L174.in_ALL_ctry_muniEFW_F_Yh$from.sector.2))
    L174.in_EJavail_muni <- subset(L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                                   sector %in% muniEFW_from_sectors &
                                     fuel %in% unique(L174.in_ALL_ctry_muniEFW_F_Yh$fuel)) %>%
      rename(avail_energy_EJ = value) %>%
      # If there are two sectors from which energy is re-allocated, aggregate them.
      group_by(iso, fuel, year) %>%
      summarise(avail_energy_EJ = sum(avail_energy_EJ)) %>%
      ungroup()

    # 3.2: Add up the bottom-up energy consumption estimates by country and year, join in the energy consumed by the
    # available sectors (by fuel and year), and calculate an EFW coefficient scaler, where the industrial water EFW
    # exceeds an exogenous fraction of total available energy use
    L174.in_EJ_ctry_muniScaler_Yh <- select(L174.in_ALL_ctry_muniEFW_F_Yh,
                                            iso, sector, fuel, year, water_km3, coefficient, energy_EJ) %>%
      group_by(iso, year, fuel) %>%
      summarise(muni_EFW_energy_EJ = sum(energy_EJ)) %>%
      ungroup() %>%
      left_join_error_no_match(L174.in_EJavail_muni,
                               by = c("iso", "year", "fuel"),
                               ignore_columns = "avail_energy_EJ") %>%
      replace_na(list(avail_energy_EJ = 0)) %>%
      # Countries with zero industrial electricity consumption (and therefore water demands) return NA. Reset scalers to 0.
      mutate(scaler = if_else(muni_EFW_energy_EJ / avail_energy_EJ > efw.MAX_COMMIND_ENERGY_MUNI_EFW,
                              avail_energy_EJ * efw.MAX_COMMIND_ENERGY_MUNI_EFW / muni_EFW_energy_EJ, 1),
             scaler = if_else(is.na(scaler), 0, scaler))

    # 3.3: Join the scalers to the country-level data of EFW coefficients and water flow volumes by country and
    # process, and re-compute scaled energy as (water * EFWcoefficient * scaler)
    # Note - while nation-level coefficients are not compiled for write-out, they are retained at this stage in order to
    # replace missing values in a subsequent step
    L174.in_ALL_ctry_muniEFW_F_Yh <- L174.in_ALL_ctry_muniEFW_F_Yh %>%
      left_join_error_no_match(select(L174.in_EJ_ctry_muniScaler_Yh, iso, year, fuel, scaler),
                               by = c("iso", "year", "fuel")) %>%
      mutate(energy_EJ = water_km3 * coefficient * scaler) %>%
      select(iso, sector, year, fuel, water_km3, energy_EJ, coefficient)

    # 3.4: Aggregate from countries to regions, to calculate municipal EFW and IO coefficients by process
    # This will be used to modify the region-level energy balances
    L174.in_EJ_R_muniEFWtot_F_Yh <- L174.in_ALL_ctry_muniEFW_F_Yh %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      mutate(sector = unique(EFW_mapping$agg_sector[EFW_mapping$sector %in% L174.in_ALL_ctry_muniEFW_F_Yh$sector])) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(energy_EJ = sum(energy_EJ)) %>%
      ungroup()

    # Country-level energy for water, by process (sector)
    L174.in_EJ_ctry_muniEFW_F_Yh <- select(L174.in_ALL_ctry_muniEFW_F_Yh, iso, sector, year, fuel, energy_EJ)

    # Region-level input-output coefficients, by process (sector)
    L174.IO_GJkm3_R_muniEFW_F_Yh <- L174.in_ALL_ctry_muniEFW_F_Yh %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(water_km3 = sum(water_km3),
                energy_EJ = sum(energy_EJ),
                default_coef = median(coefficient)) %>%
      ungroup() %>%
      mutate(coefficient = if_else(water_km3 == 0, default_coef, energy_EJ / water_km3)) %>%
      select(GCAM_region_ID, sector, fuel, year, coefficient)

    # Part 4: Calculating region-level wastewater treatment fractions.
    # The numerator is the total volume of wastewater treated, calculated above. The denominator is the total water
    # withdrawals of the municipal sector (this includes desalinated water used by the municipal sector)
    L174.WWtrtfrac_R_muni_Yh <- subset(L174.in_ALL_ctry_muniEFW_F_Yh, sector == muniWWTrt) %>%
      rename(WWtrt_km3 = water_km3) %>%
      left_join_error_no_match(L145.municipal_water_ctry_W_Yh_km3, by = c("iso", "year")) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(WWtrt_km3 = sum(WWtrt_km3),
                water_km3 = sum(withdrawals)) %>%
      ungroup() %>%
      mutate(WWtrtfrac = WWtrt_km3 / water_km3) %>%
      select(GCAM_region_ID, sector, year, WWtrtfrac)

    # ===================================================

    # Produce outputs
    L174.water_km3_R_muniEFW_Yh %>%
      add_title("Water flow volumes for municipal water processes by region / historical year") %>%
      add_units("km^3") %>%
      add_comments("Amounts of water abstracted, treated, and the wastewater treated in the manufacturing sector") %>%
      add_precursors("water/EFW_mapping",
                     "water/Liu_EFW_inventory",
                     "L145.municipal_water_ctry_W_Yh_km3",
                     "L145.municipal_water_eff_ctry_Yh",
                     "L173.trtshr_ctry_Yh",
                     "L173.in_desal_km3_ctry_muni_Yh") ->
      L174.water_km3_R_muniEFW_Yh

    L174.in_EJ_ctry_muniEFW_F_Yh %>%
      add_title("Electricity energy for municipal water processes by country / historical year") %>%
      add_units("EJ") %>%
      add_comments("Amounts of energy capped at exogenous threshold of country-level sectoral electricity demands") %>%
      same_precursors_as(L174.water_km3_R_muniEFW_Yh) %>%
      add_precursors("water/A74.globaltech_coef",
                     "L101.en_bal_EJ_ctry_Si_Fi_Yh_full") ->
      L174.in_EJ_ctry_muniEFW_F_Yh

    L174.in_EJ_R_muniEFWtot_F_Yh %>%
      add_title("Electricity energy for municipal water processes by region / historical year") %>%
      add_units("EJ") %>%
      add_comments("Amounts of energy capped at exogenous threshold of country-level sectoral electricity demands") %>%
      same_precursors_as(L174.water_km3_R_muniEFW_Yh) %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/A74.globaltech_coef",
                     "L101.en_bal_EJ_ctry_Si_Fi_Yh_full") ->
      L174.in_EJ_R_muniEFWtot_F_Yh

    L174.IO_GJkm3_R_muniEFW_F_Yh %>%
      add_title("Input-output coefficients of electricity for municipal EFW by GCAM region / EFW process / historical year") %>%
      add_units("GJ/m^3") %>%
      add_comments("Coefficients adjusted in some regions due to application of exogenous max EFW fraction of sectoral electricity consumption") %>%
      same_precursors_as(L174.in_EJ_R_muniEFWtot_F_Yh) ->
      L174.IO_GJkm3_R_muniEFW_F_Yh

    L174.WWtrtfrac_R_muni_Yh %>%
      add_title("Municipal wastewater treated divided by municipal water withdrawals by GCAM region / historical year") %>%
      add_units("Unitless share") %>%
      add_comments("This fraction relates wastewater treatment volumes to municipal water withdrawals") %>%
      same_precursors_as(L174.water_km3_R_muniEFW_Yh) %>%
      add_precursors("common/iso_GCAM_regID") ->
      L174.WWtrtfrac_R_muni_Yh

    return_data(L174.water_km3_R_muniEFW_Yh,
                L174.in_EJ_ctry_muniEFW_F_Yh,
                L174.in_EJ_R_muniEFWtot_F_Yh,
                L174.IO_GJkm3_R_muniEFW_F_Yh,
                L174.WWtrtfrac_R_muni_Yh)
  } else {
    stop("Unknown command")
  }
}
