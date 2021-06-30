# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L173.EFW_manufacturing
#'
#' Energy requirements for manufacturing water use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L173.water_km3_R_indEFW_Yh},
#'   \code{L173.trtshr_ctry_Yh}, \code{L173.in_desal_km3_ctry_ind_Yh}, \code{L173.in_desal_km3_ctry_muni_Yh},
#'   \code{L173.in_EJ_R_indEFWtot_F_Yh}, \code{L173.IO_GJkm3_R_indEFW_F_Yh},  \code{L173.WWtrtfrac_R_ind_Yh}.
#' @details Generate estimates of energy consumption, energy input-output coefficients, and water flow volumes for
#'   manufacturing water abstraction, treatment, and wastewater treatment, by GCAM region, fuel, and historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by inner_join left_join mutate rename select summarise ungroup
#' @importFrom tidyr complete gather nesting replace_na
#' @author GPK January 2019
module_water_L173.EFW_manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/EFW_mapping",
             FILE = "water/Liu_EFW_inventory",
             FILE = "water/A73.globaltech_coef",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
             "L132.water_km3_ctry_ind_Yh",
             "L145.municipal_water_ctry_W_Yh_km3",
             "L171.in_km3_ctry_desal_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L173.water_km3_R_indEFW_Yh",
             "L173.trtshr_ctry_Yh",
             "L173.in_desal_km3_ctry_ind_Yh",
             "L173.in_desal_km3_ctry_muni_Yh",
             "L173.in_EJ_R_indEFWtot_F_Yh",
             "L173.IO_GJkm3_R_indEFW_F_Yh",
             "L173.WWtrtfrac_R_ind_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- year <- water_type <- sector <- water_km3 <- industrial <- withdrawals <-
      municipal <- indShare <- value <- desal_km3 <- desal_ind_km3 <- desal_muni_km3 <-
      lm <- trtshr <- pred_trtshr <- delta <- pcGDP <- waterCons_km3 <- GCAM_region_ID <-
      supplysector <- subsector <- technology <- minicam.energy.input <- coefficient <-
      fuel <- from.sector <- from.sector.2 <- sf_shr2 <- gw_shr2 <- gw_sc_EI_coef <-
      avail_energy_EJ <- energy_EJ <- ind_EFW_energy_EJ <- scaler <- default_coef <-
      WWtrt_km3 <- WWtrtfrac <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    Liu_EFW_inventory <- get_data(all_data, "water/Liu_EFW_inventory")
    A73.globaltech_coef <- get_data(all_data, "water/A73.globaltech_coef")

    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")
    L132.water_km3_ctry_ind_Yh <- get_data(all_data, "L132.water_km3_ctry_ind_Yh", strip_attributes = TRUE)
    L145.municipal_water_ctry_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_ctry_W_Yh_km3", strip_attributes = TRUE)
    L171.in_km3_ctry_desal_Yh <- get_data(all_data, "L171.in_km3_ctry_desal_Yh", strip_attributes = TRUE)

    # ===================================================

    # Part 1: Calculating water flow volumes for water abstraction (1a) and treatment (1b)
    # Use the EFW_mapping table to get the names of the "sector" names for these EFW processes
    indAbs <- with(EFW_mapping, unique(sector[grepl("industr", sector) &
                                                grepl("abstraction", sector)]))
    indTrt <- with(EFW_mapping, unique(sector[grepl("industr", sector) &
                                                grepl("treatment", sector) &
                                                !grepl("waste", sector)]))
    indWWTrt <- with(EFW_mapping, unique(sector[grepl("industr", sector) &
                                                  grepl("wastewater", sector)]))

    # Part 1.1: Industrial water abstraction flow volume is equal to water withdrawals
    # Note that industrial water withdrawals are defined as self-supply only, which excludes water purchased from
    # municipal or other third party suppliers
    L173.water_km3_ctry_indAbs_Yh <- L132.water_km3_ctry_ind_Yh %>%
      filter(water_type == "water withdrawals") %>%
      mutate(sector = indAbs) %>%
      select(iso, sector, year, water_km3)

    # Part 1.2: Water treatment flow volume is also equal to water withdrawals (i.e., the same as abstraction)
    L173.water_km3_ctry_indTrt_Yh <- mutate(L173.water_km3_ctry_indAbs_Yh, sector = indTrt)

    # Part 1.3: Treated wastewater is equal to (withdrawals - consumption + purchased desalinated water) * trtshr
    # Start with the withdrawal volume, deduct consumptive uses (water evaporated, or used in products), and add in any
    # purchased desalinated water. This quantity is multiplied by the portion of water treated on-site; the portion
    # (from Liu et al) is assumed country-specific. trtshr as applied to (withdrawals - consumption) is assumed equal
    # between municipal and industrial water systems

    # First step (1.3.1): compute the water consumption by the industrial sector
    L173.waterCons_km3_ctry_ind_Yh <- L132.water_km3_ctry_ind_Yh %>%
      filter(water_type == "water consumption") %>%
      select(iso, year, water_km3) %>%
      rename(waterCons_km3 = water_km3)

    # Second (1.3.2), compute desalinated water used by the industrial sector
    # This is somewhat complicated. Desalinated water is not included in the "abstraction" or "treatment" volumes as it
    # is not self-supplied, but it is a flow that is tracked in GCAM, and as such any desalinated water consumption will
    # contribute to the wastewater volume (net of consumptive losses), some portion of which is treated as wastewater.

    # 1.3.2.1: apportion desalinated water to municipal and industrial sectors, according to water demand shares.
    # Use full_join to not drop any countries that report either industrial or municipal water, but not both
    L173.water_km3_ctry_W_ind_Yh <- filter(L132.water_km3_ctry_ind_Yh, water_type == "water withdrawals") %>%
      rename(industrial = water_km3) %>%
      select(iso, year, industrial)

    L173.indShare_ctry_desal_Yh <- rename(L145.municipal_water_ctry_W_Yh_km3, municipal = withdrawals) %>%
      full_join(L173.water_km3_ctry_W_ind_Yh, by = c("iso", "year")) %>%
      mutate(industrial = if_else(is.na(industrial), 0, industrial),
             municipal = if_else(is.na(municipal), 0, municipal),
             indShare = industrial / (industrial + municipal),
             indShare = if_else(is.na(indShare), 0.5, indShare))

    # 1.3.2.2: Multiply by these shares by desalinated water production volumes, to compute municipal and industrial
    # demands of desalinated water
    L173.in_desal_km3_ctry_S_Yh <- rename(L171.in_km3_ctry_desal_Yh, desal_km3 = value) %>%
      left_join_error_no_match(L173.indShare_ctry_desal_Yh, by = c("iso", "year")) %>%
      mutate(desal_ind_km3 = desal_km3 * indShare,
             desal_muni_km3 = desal_km3 - desal_ind_km3)

    # Split this into two tables for write-out
    L173.in_desal_km3_ctry_ind_Yh <- select(L173.in_desal_km3_ctry_S_Yh, iso, year, desal_ind_km3)
    L173.in_desal_km3_ctry_muni_Yh <- select(L173.in_desal_km3_ctry_S_Yh, iso, year, desal_muni_km3)

    # Third (1.3.3), compute the share of wastewater treated ('trtshr') in each country and year
    # This is done as a linear function of GDP, bounded by maximum and minimum values, from a 2010
    # starting point in most nations. The approach is similar to the non-CO2s (share of pollutants controlled increases
    # with per-capita GDP), but differs in that (a) the function is parameterized from the base-year
    # observations, and that (b) a linear function is used for simplicity and versatility

    # Prepare the per-capita GDP data for joining in to the water flow volumes
    L102.pcgdp_thous90USD_GCAM3_ctry_Y <- rename(L102.pcgdp_thous90USD_GCAM3_ctry_Y, pcGDP = value)

    # Using inner_join due to some minor countries in Liu inventory that aren't in the GDP data (e.g., South Sudan, Vatican City)
    L173.trtshr_2010 <- Liu_EFW_inventory[c("iso", "trtshr")] %>%
      mutate(year = 2010) %>%
      inner_join(L102.pcgdp_thous90USD_GCAM3_ctry_Y, by = c("iso", "year"))

    #linear model
    L173.trtshr_lm <- lm(trtshr ~ pcGDP, data = L173.trtshr_2010)

    # join in the fitted values and compute the delta between predicted and observed in 2010
    L173.trtshr_2010 <- L173.trtshr_2010 %>%
      mutate(pred_trtshr = L173.trtshr_lm[["fitted.values"]],
             delta = trtshr - pred_trtshr)

    # Fourth (1.3.4), compute the volumes of industrial wastewater treated from the variables computed above in this step
    L173.water_km3_ctry_indWWTrt_Yh <- L173.water_km3_ctry_indAbs_Yh %>%
      mutate(sector = indWWTrt) %>%
      left_join_error_no_match(L173.waterCons_km3_ctry_ind_Yh, by = c("iso", "year")) %>%
      left_join_error_no_match(L173.in_desal_km3_ctry_ind_Yh, by = c("iso", "year"),
                               ignore_columns = "desal_ind_km3") %>%
      left_join_error_no_match(L102.pcgdp_thous90USD_GCAM3_ctry_Y, by = c("iso", "year"),
                               ignore_columns = "pcGDP") %>%
      left_join_error_no_match(select(L173.trtshr_2010, iso, delta), by = "iso",
                               ignore_columns = "delta") %>%
      # For countries with reported water use that aren't in the GDP data, use 0 GDP (base-year trtshr will default to
      # the intercept of the linear model)
      # For countries not in the Liu inventory (i.e., no 2010 trtshr data), default to the values predicted by linear
      # model in all historical years
      replace_na(list(desal_ind_km3 = 0, pcGDP = 0, delta = 0)) %>%
      mutate(trtshr = L173.trtshr_lm[["coefficients"]]["(Intercept)"] +
               L173.trtshr_lm[["coefficients"]]["pcGDP"] * pcGDP +
               delta,
             trtshr = pmin(trtshr, max(Liu_EFW_inventory$trtshr)),
             trtshr = pmax(trtshr, min(Liu_EFW_inventory$trtshr)),
             water_km3 = (water_km3 - waterCons_km3 + desal_ind_km3) * trtshr)

    # Prepare the trtshr data for write-out (to be used by the municipal water processing code)
    L173.trtshr_ctry_Yh <- select(L173.water_km3_ctry_indWWTrt_Yh, iso, year, trtshr)

    # Select just the relevant variables from the wastewater flow volume table
    L173.water_km3_ctry_indWWTrt_Yh <- select(L173.water_km3_ctry_indWWTrt_Yh, iso, sector, year, water_km3)

    # 1.3.5: Final step: bind the water flow tables and aggregate from countries to GCAM regions
    L173.water_km3_R_indEFW_Yh <- bind_rows(L173.water_km3_ctry_indAbs_Yh,
                                            L173.water_km3_ctry_indTrt_Yh,
                                            L173.water_km3_ctry_indWWTrt_Yh) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(water_km3 = sum(water_km3)) %>%
      ungroup()

    # Part 2: Calculating energy for water requirements associated with manufacturing freshwater withdrawal volumes
    # This step starts by joining water flow volumes and EFW coefficients, adjusting abstraction-related coefficients
    # to accommodate the average ground:surface water ratios in each country, and multiplying to calculate energy
    # quantities.

    # 2.1: Estimate the EFW coefficients in all historical years
    # 2.1.1: interpolate the exogenous coefficients to all historical years
    L173.globaltech_coef <- gather_years(A73.globaltech_coef, value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(HISTORICAL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient)) %>%
      ungroup() %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel, from.sector, from.sector.2),
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input"),
                               ignore_columns = "from.sector.2") %>%
      select(sector, fuel, from.sector, from.sector.2, year, coefficient)

    # 2.1.2: Adjust water abstraction EFW coefficients to reflect country-level estimates of ground:surface water ratio
    # Note also that there is a country-level adjustment factor, gw_sc_EI_coef, applied to groundwater pumping energy intensity,
    # that reflects the average depth of groundwater pumping
    L173.in_EJ_ctry_indAbs_Yh <- L173.water_km3_ctry_indAbs_Yh %>%
      left_join_error_no_match(select(L173.globaltech_coef, sector, fuel, year, coefficient),
                               by = c("sector", "year")) %>%
      # Missing values are allowed here, for countries not included in the Liu inventory
      left_join(select(Liu_EFW_inventory, iso, sf_shr2, gw_shr2, gw_sc_EI_coef),
                by = "iso") %>%
      # for countries not included in the Liu et al inventory, use the surface water abstraction coefficient
      mutate(coefficient = (sf_shr2 * efw.SW_ABSTRACTION_EFW + gw_shr2 * gw_sc_EI_coef * efw.GW_ABSTRACTION_EFW),
             coefficient = if_else(is.na(coefficient), efw.SW_ABSTRACTION_EFW, coefficient))

    # 2.2: Multiply the water volumes by the EFW coefficients to compute energy demands
    L173.in_ALL_ctry_indEFW_F_Yh <- bind_rows(L173.water_km3_ctry_indTrt_Yh,
                                              L173.water_km3_ctry_indWWTrt_Yh) %>%
      left_join_error_no_match(L173.globaltech_coef, by = c("sector", "year"),
                               ignore_columns = "from.sector.2") %>%
      bind_rows(L173.in_EJ_ctry_indAbs_Yh) %>%
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
    indEFW_from_sectors <- unique(c(L173.in_ALL_ctry_indEFW_F_Yh$from.sector,
                                    L173.in_ALL_ctry_indEFW_F_Yh$from.sector.2))
    L173.in_EJavail_ind <- subset(L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                                  sector %in% indEFW_from_sectors &
                                    fuel %in% unique(L173.in_ALL_ctry_indEFW_F_Yh$fuel)) %>%
      rename(avail_energy_EJ = value) %>%
      # If there are two sectors from which energy is re-allocated, aggregate them.
      group_by(iso, fuel, year) %>%
      summarise(avail_energy_EJ = sum(avail_energy_EJ)) %>%
      ungroup()

    # 3.2: Add up the bottom-up energy consumption estimates by country and year, join in the energy consumed by the
    # available sectors (by fuel and year), and calculate an EFW coefficient scaler, where the industrial water EFW
    # exceeds an exogenous fraction of total available energy use
    L173.in_EJ_ctry_indScaler_Yh <- select(L173.in_ALL_ctry_indEFW_F_Yh,
                                           iso, sector, fuel, year, water_km3, coefficient, energy_EJ) %>%
      group_by(iso, year, fuel) %>%
      summarise(ind_EFW_energy_EJ = sum(energy_EJ)) %>%
      ungroup() %>%
      left_join_error_no_match(L173.in_EJavail_ind,
                               by = c("iso", "year", "fuel")) %>%
      # Countries with zero industrial electricity consumption (and therefore water demands) return NA. Reset scalers to 0.
      mutate(scaler = if_else(ind_EFW_energy_EJ / avail_energy_EJ > efw.MAX_IND_ENERGY_EFW,
                              avail_energy_EJ * efw.MAX_IND_ENERGY_EFW / ind_EFW_energy_EJ, 1),
             scaler = if_else(is.na(scaler), 0, scaler))

    # 3.3: Join the scalers to the country-level data of EFW coefficients and water flow volumes by country and
    # process, and re-compute scaled energy as (water * EFWcoefficient * scaler)
    # Note - while nation-level coefficients are not compiled for write-out, they are retained at this stage in order to
    # replace missing values in a subsequent step
    L173.in_ALL_ctry_indEFW_F_Yh <- L173.in_ALL_ctry_indEFW_F_Yh %>%
      left_join_error_no_match(select(L173.in_EJ_ctry_indScaler_Yh, iso, year, fuel, scaler),
                               by = c("iso", "year", "fuel")) %>%
      mutate(energy_EJ = water_km3 * coefficient * scaler) %>%
      select(iso, sector, year, fuel, water_km3, energy_EJ, coefficient)

    # 3.4: Aggregate from countries to regions, to calculate industrial EFW and IO coefficients by process
    # The aggregation is done with the "agg_sector" name in order to compute the entire sector's EFW quantity,
    # named such that the data can be inserted into regional energy balances.
    # This will be used to modify the region-level energy balances
    L173.in_EJ_R_indEFWtot_F_Yh <- L173.in_ALL_ctry_indEFW_F_Yh %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      mutate(sector = unique(EFW_mapping$agg_sector[EFW_mapping$sector %in% L173.in_ALL_ctry_indEFW_F_Yh$sector])) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(energy_EJ = sum(energy_EJ)) %>%
      ungroup()

    # Country-level energy for water, by process (sector)
    L173.in_EJ_ctry_indEFW_F_Yh <- select(L173.in_ALL_ctry_indEFW_F_Yh, iso, sector, year, fuel, energy_EJ)

    # Region-level input-output coefficients, by process (sector)
    L173.IO_GJkm3_R_indEFW_F_Yh <- L173.in_ALL_ctry_indEFW_F_Yh %>%
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
    # flow through the industrial sector, equal to the withdrawals (abstraction) plus desalinated water use
    L173.WWtrtfrac_R_ind_Yh <- subset(L173.in_ALL_ctry_indEFW_F_Yh, sector == indWWTrt) %>%
      rename(WWtrt_km3 = water_km3) %>%
      left_join_error_no_match(
        select(filter(L132.water_km3_ctry_ind_Yh, water_type == "water withdrawals"), iso, year, water_km3),
        by = c("iso", "year")) %>%
      left_join_error_no_match(L173.in_desal_km3_ctry_ind_Yh, by = c("iso", "year"),
                               ignore_columns = "desal_ind_km3") %>%
      replace_na(list(desal_ind_km3 = 0)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(WWtrt_km3 = sum(WWtrt_km3),
                desal_ind_km3 = sum(desal_ind_km3),
                water_km3 = sum(water_km3)) %>%
      ungroup() %>%
      mutate(WWtrtfrac = WWtrt_km3 / (desal_ind_km3 + water_km3)) %>%
      select(GCAM_region_ID, sector, year, WWtrtfrac)

    # ===================================================

    # Produce outputs
    L173.water_km3_R_indEFW_Yh %>%
      add_title("Water flow volumes for industrial water processes by region / historical year") %>%
      add_units("km^3") %>%
      add_comments("Amounts of water abstracted, treated, and the wastewater treated in the manufacturing sector") %>%
      add_precursors("water/EFW_mapping",
                     "water/Liu_EFW_inventory",
                     "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
                     "L132.water_km3_ctry_ind_Yh",
                     "L145.municipal_water_ctry_W_Yh_km3",
                     "L171.in_km3_ctry_desal_Yh") ->
      L173.water_km3_R_indEFW_Yh

    L173.trtshr_ctry_Yh %>%
      add_title("Share of wastewater treated by country / historical year") %>%
      add_units("Unitless share") %>%
      add_comments("Based on Liu et al. (2016) inventory") %>%
      same_precursors_as(L173.water_km3_R_indEFW_Yh) ->
      L173.trtshr_ctry_Yh

    L173.in_desal_km3_ctry_ind_Yh %>%
      add_title("Industrial sector consumption of desalinated water by country / historical year") %>%
      add_units("km^3") %>%
      add_comments("Desalinated water production shared to municipal and industrial sectors according to relative water demands") %>%
      add_precursors("L132.water_km3_ctry_ind_Yh",
                     "L145.municipal_water_ctry_W_Yh_km3",
                     "L171.in_km3_ctry_desal_Yh") ->
      L173.in_desal_km3_ctry_ind_Yh

    L173.in_desal_km3_ctry_muni_Yh %>%
      add_title("Municipal sector consumption of desalinated water by country / historical year") %>%
      add_units("km^3") %>%
      add_comments("Desalinated water production shared to municipal and industrial sectors according to relative water demands") %>%
      same_precursors_as(L173.in_desal_km3_ctry_ind_Yh) ->
      L173.in_desal_km3_ctry_muni_Yh

    L173.in_EJ_R_indEFWtot_F_Yh %>%
      add_title("Electricity energy for manufacturing-related water processes by region / historical year") %>%
      add_units("EJ") %>%
      add_comments("Amounts of energy capped at exogenous threshold of total industrial electricity demands") %>%
      same_precursors_as(L173.water_km3_R_indEFW_Yh) %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/A73.globaltech_coef",
                     "L101.en_bal_EJ_ctry_Si_Fi_Yh_full") ->
      L173.in_EJ_R_indEFWtot_F_Yh

    L173.IO_GJkm3_R_indEFW_F_Yh %>%
      add_title("Input-output coefficients of electricity for industrial EFW by GCAM region / EFW process / historical year") %>%
      add_units("GJ/m^3") %>%
      add_comments("Coefficients adjusted in some regions due to application of exogenous max EFW fraction of industrial electricity consumption") %>%
      same_precursors_as(L173.in_EJ_R_indEFWtot_F_Yh) ->
      L173.IO_GJkm3_R_indEFW_F_Yh

    L173.WWtrtfrac_R_ind_Yh %>%
      add_title("Industrial wastewater treated divided by self-supplied industrial water withdrawals by GCAM region / historical year") %>%
      add_units("Unitless share") %>%
      add_comments("This fraction relates wastewater treatment volumes to municipal water withdrawals") %>%
      same_precursors_as(L173.water_km3_R_indEFW_Yh) %>%
      add_precursors("common/iso_GCAM_regID") ->
      L173.WWtrtfrac_R_ind_Yh

    return_data(L173.water_km3_R_indEFW_Yh,
                L173.trtshr_ctry_Yh,
                L173.in_desal_km3_ctry_ind_Yh,
                L173.in_desal_km3_ctry_muni_Yh,
                L173.in_EJ_R_indEFWtot_F_Yh,
                L173.IO_GJkm3_R_indEFW_F_Yh,
                L173.WWtrtfrac_R_ind_Yh)
  } else {
    stop("Unknown command")
  }
}
