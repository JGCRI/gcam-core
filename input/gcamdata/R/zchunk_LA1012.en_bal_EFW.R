# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1012.en_bal_EFW
#'
#' Adjustments to the IEA energy balances to account for energy-for-water processes explicitly modeled.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1012.en_bal_EJ_R_Si_Fi_Yh}.
#' @details This chunk deducts bottom-up energy-for-water (EFW) estimates from the IEA Energy Balances estimates of
#'   energy consumption in the commercial and industrial sectors, applying a set of rules that ensure that resulting
#'   energy demand levels are reasonable. The result is the full energy balance table with an additional EFW
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter group_by left_join mutate rename right_join select ungroup
#' @importFrom tidyr replace_na
#' @author GPK January 2019
module_energy_LA1012.en_bal_EFW <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "water/EFW_mapping",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L171.in_EJ_R_desal_F_Yh",
             "L172.in_EJ_R_irr_F_Yh",
             "L173.in_EJ_R_indEFWtot_F_Yh",
             "L174.in_EJ_R_muniEFWtot_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.MAKE) {

    # silence package check
    year <- sector <- fuel <- GCAM_region_ID <- value <- share <- energy_EJ <-
      deduction_EJ <- initial_energy_EJ <- from.sector <- from.sector.2 <-
      value.1 <- value.2 <- energy_EJ.1rev <- energy_EJ.2rev <- energy_EJ_revised <-
      agg_sector <- energy_EJrev <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    EFW_mapping <- get_data(all_data, "water/EFW_mapping", strip_attributes = TRUE)
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    L171.in_EJ_R_desal_F_Yh <- get_data(all_data, "L171.in_EJ_R_desal_F_Yh", strip_attributes = TRUE)
    L172.in_EJ_R_irr_F_Yh <- get_data(all_data, "L172.in_EJ_R_irr_F_Yh", strip_attributes = TRUE)
    L173.in_EJ_R_indEFWtot_F_Yh <- get_data(all_data, "L173.in_EJ_R_indEFWtot_F_Yh", strip_attributes = TRUE)
    L174.in_EJ_R_muniEFWtot_F_Yh <- get_data(all_data, "L174.in_EJ_R_muniEFWtot_F_Yh", strip_attributes = TRUE)

    # ===================================================

    # This chunk adjusts each region's energy balances according to bottom-up estimates of energy-for-water
    # Energy consumption is deducted from existing sectors

    # Part 1: Desalination
    # Notes: The attribution of desal-related energy to sectors ("flows") in the IEA Energy Balances is not known. While
    # its ISIC code (494) is water supply, most of whose energy likely maps to IEA's "Commercial and public services"
    # (COMMPUB), energy used for desalination at manufacturing plants, power plants, or mines will be classified with
    # those. The important part for incorporation into GCAM's energy system is to minimize distortions to energy
    # balances.

    # 1.1 Compute the share of each desal-related (i.e., commercial and industrial) sector's energy by region, fuel, and year
    L1012.desal_shares <- filter(L1011.en_bal_EJ_R_Si_Fi_Yh, sector %in% efw.DESAL_ENERGY_SECTORS) %>%
      filter(fuel %in% L171.in_EJ_R_desal_F_Yh$fuel) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      mutate(share = value / sum(value)) %>%
      replace_na(list(share = 0)) %>%
      select(GCAM_region_ID, sector, fuel, year, share)

    # 1.2 Deduct desalination-related energy from assigned sectors and re-balance the energy flows
    # Repeat the table of energy inputs to desal by the sectors being assigned, and join in the shares from the table
    # above to compute the amount of energy to deduct from each country/sector/fuel/year.
    L1012.deduction_EJ_R_desal_F_Yh <- repeat_add_columns(select(L171.in_EJ_R_desal_F_Yh, -sector),
                                                          tibble(sector = efw.DESAL_ENERGY_SECTORS )) %>%
      left_join_error_no_match(L1012.desal_shares,
                               by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(deduction_EJ = energy_EJ * share) %>%
      select(GCAM_region_ID, sector, fuel, year, deduction_EJ)

    L1012.en_bal_EJ_R_Si_Fi_Yh_desal <- right_join(L1011.en_bal_EJ_R_Si_Fi_Yh,
                                                   L1012.deduction_EJ_R_desal_F_Yh,
                                                   by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      rename(initial_energy_EJ = value)

    # Stop if the desal-related deduction exceeds an exogenous fraction of available energy
    stopifnot(with(subset(L1012.en_bal_EJ_R_Si_Fi_Yh_desal, initial_energy_EJ > 0),
                   deduction_EJ / initial_energy_EJ) < efw.MAX_COMMIND_ENERGY_DESAL)

    # Replace the corresponding values in the energy balance tables
    # left_join returns missing values for all unaffected sectors
    L1012.en_bal_EJ_R_Si_Fi_Yh <- L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      left_join(select(L1012.en_bal_EJ_R_Si_Fi_Yh_desal, GCAM_region_ID, sector, fuel, year, deduction_EJ),
                by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      replace_na(list(deduction_EJ = 0)) %>%
      mutate(value = value - deduction_EJ) %>%
      select(-deduction_EJ)

    L1012.en_bal_EJ_R_Si_Fi_Yh <- bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh,
                                            select(rename(L171.in_EJ_R_desal_F_Yh, value = energy_EJ),
                                                   GCAM_region_ID, sector, fuel, year, value))

    # Part 2: Irrigation
    # Irrigation-related energy consumption also does not correspond to a single sector in the IEA Energy Balances. The
    # data include an "agriculture" line item, but most nations either have zero or very little electricity
    # estimated/reported. The ISIC code of irrigation (497) has no clear assignment. The method used here allows for two
    # sectors to be assigned, set in EFW_mapping. At this point the energy quantities have already been checked against
    # available energy and the EFW coefs have been modified as necessary. The method below first joins in the sectors
    # (and energy consumption levels) from which energy is deducted, and then performs the calculations, applying limits
    # on how much energy can be deducted from each sector.

    L1012.deduction_EJ_R_irr_F_Yh <- rename(L172.in_EJ_R_irr_F_Yh, deduction_EJ = energy_EJ) %>%
      left_join_error_no_match(select(EFW_mapping, sector, fuel, from.sector, from.sector.2),
                               by = c("sector", "fuel")) %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh,
                               by = c("GCAM_region_ID", from.sector = "sector", "fuel", "year")) %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh,
                               by = c("GCAM_region_ID", from.sector.2 = "sector", "fuel", "year"),
                               suffix = c(".1", ".2")) %>%
      mutate(energy_EJ.1rev = if_else(value.1 * efw.MAX_AG_ENERGY_IRR >= deduction_EJ,
                                      value.1 - deduction_EJ,
                                      value.1 * (1 - efw.MAX_AG_ENERGY_IRR))) %>%
      mutate(energy_EJ.2rev = if_else(value.1 * efw.MAX_AG_ENERGY_IRR >= deduction_EJ,
                                      value.2,
                                      value.2 + (value.1 * efw.MAX_AG_ENERGY_IRR) - deduction_EJ))

    # Re-build the energy balance tables, substituting revised data for reported data, and appending the irrigation EFW
    L1012.in_EJ_R_revirr1_F_Yh <- select(L1012.deduction_EJ_R_irr_F_Yh,
                                         GCAM_region_ID, fuel, year, from.sector, energy_EJ.1rev) %>%
      rename(sector = from.sector, energy_EJ_revised = energy_EJ.1rev)
    L1012.in_EJ_R_revirr_F_Yh <- select(L1012.deduction_EJ_R_irr_F_Yh,
                                        GCAM_region_ID, fuel, year, from.sector.2, energy_EJ.2rev) %>%
      rename(sector = from.sector.2, energy_EJ_revised = energy_EJ.2rev) %>%
      bind_rows(L1012.in_EJ_R_revirr1_F_Yh)

    # Append irrigation EFW, and join revisions to ag and commercial energy back into the energy balances table
    # left_join returns missing values for all unaffected sectors
    L1012.en_bal_EJ_R_Si_Fi_Yh <- bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh,
                                            rename(L172.in_EJ_R_irr_F_Yh, value = energy_EJ)) %>%
      left_join(L1012.in_EJ_R_revirr_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = if_else(is.na(energy_EJ_revised), value, energy_EJ_revised)) %>%
      select(-energy_EJ_revised)

    # Part 3: Industrial manufacturing energy-for-water
    # The method here is generally the same as irrigation, with the exception that (depending on the EFW_mapping) there
    # may only be one sector to re-assign energy from. The method below is designed to allow either one or two sectors
    # to be available for the EFW deduction.

    EFW_mapping_agg <- distinct(select(EFW_mapping, agg_sector, fuel, from.sector, from.sector.2))

    # 3.1. Deducting industrial water-related energy from assigned industrial sector(s)
    L1012.deduction_EJ_R_mfg_F_Yh <- rename(L173.in_EJ_R_indEFWtot_F_Yh, deduction_EJ = energy_EJ) %>%
      left_join_error_no_match(EFW_mapping_agg,
                               by = c(sector = "agg_sector", "fuel"),
                               ignore_columns = "from.sector.2") %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh,
                               by = c("GCAM_region_ID", from.sector = "sector", "fuel", "year")) %>%
      mutate(energy_EJrev = if_else(value * efw.MAX_IND_ENERGY_EFW >= deduction_EJ,
                                    value - deduction_EJ,
                                    value * (1 - efw.MAX_IND_ENERGY_EFW)))

    L1012.in_EJ_R_revmfg_F_Yh <- select(L1012.deduction_EJ_R_mfg_F_Yh, GCAM_region_ID, fuel, year, from.sector, energy_EJrev) %>%
      rename(sector = from.sector, energy_EJ_revised = energy_EJrev)

    # this next sequence is only performed as necessary: if there are multiple sectors to deduct from, and if there
    # are regions whose industrial EFW is greater than the allowable fraction of the first deduction sector's energy
    if(!is.na(unique(L1012.deduction_EJ_R_mfg_F_Yh$from.sector.2)) &
       min(L1012.deduction_EJ_R_mfg_F_Yh$energy_EJrev / L1012.deduction_EJ_R_mfg_F_Yh$value) <
       (1 - efw.MAX_IND_ENERGY_EFW)){
      # Compute the new energy consumption level of the second sector. Equal to the original value unless the
      # first sector's energy times the max-efw-frac is not enough to cover the bottom-up efw estimate. In that case, it's
      # equal to its original value minus the difference
      L1012.in_EJ_R_revmfg2_F_Yh <- left_join_error_no_match(L1012.deduction_EJ_R_mfg_F_Yh,
                                                             L1012.en_bal_EJ_R_Si_Fi_Yh,
                                                             by = c("GCAM_region_ID", from.sector.2 = "sector", "fuel", "year"),
                                                             suffix = c(".1", ".2")) %>%
        mutate(energy_EJ.2rev = if_else(value.1 * efw.MAX_IND_ENERGY_EFW > deduction_EJ,
                                        value.2,
                                        value.2 + (value.1 * efw.MAX_IND_ENERGY_EFW) - deduction_EJ)) %>%
        select(GCAM_region_ID, fuel, year, from.sector.2, energy_EJ.2rev) %>%
        rename(sector = from.sector.2, energy_EJ_revised = energy_EJ.2rev)

      if(any(L1012.in_EJ_R_revmfg2_F_Yh$energy_EJ_revised < 0)){
        stop(paste0("Negative industrial energy demand from EFW incorporation in region ",
                    unique(L1012.in_EJ_R_revmfg2_F_Yh$GCAM_region_ID[L1012.in_EJ_R_revmfg2_F_Yh$energy_EJ_revised < 0]),
                    ". Please adjust the constant efw.MAX_IND_ENERGY_EFW or the deduction sectors in EFW_mapping. "))
      }

      # Bind this to the revisions from the first sector
      L1012.in_EJ_R_revmfg_F_Yh <- bind_rows(L1012.in_EJ_R_revmfg_F_Yh, L1012.in_EJ_R_revmfg2_F_Yh)
    }

    # Append industrial EFW, and join revisions to industrial energy back into the energy balances table
    L1012.en_bal_EJ_R_Si_Fi_Yh <- bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh,
                                            rename(L173.in_EJ_R_indEFWtot_F_Yh, value = energy_EJ)) %>%
      left_join(L1012.in_EJ_R_revmfg_F_Yh,
                by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = if_else(is.na(energy_EJ_revised), value, energy_EJ_revised)) %>%
      select(-energy_EJ_revised)

    # Part 4: Municipal water systems energy-for-water
    # Municipal water is somewhat different from the prior EFW sectors in that there is a clearly defined corresponding
    # sector in the IEA Energy Balances: "Commercial and public services", or "COMMPUB". However, in practice the
    # electricity consumption by COMMPUB is often zero or too low to be the only place where such electricity demands
    # may be deducted. As such, the method prioritizes this sector, but also requires a second one to be provided.

    L1012.deduction_EJ_R_muni_F_Yh <- rename(L174.in_EJ_R_muniEFWtot_F_Yh, deduction_EJ = energy_EJ) %>%
      left_join_error_no_match(EFW_mapping_agg, by = c("sector" = "agg_sector", "fuel")) %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh, by = c("GCAM_region_ID", from.sector = "sector", "fuel", "year")) %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh, by = c("GCAM_region_ID", from.sector.2 = "sector", "fuel", "year"),
                suffix = c(".1", ".2")) %>%
      mutate(energy_EJ.1rev = if_else(value.1 * efw.MAX_COMM_ENERGY_MUNI_EFW >= deduction_EJ,
                                      value.1 - deduction_EJ,
                                      value.1 * (1 - efw.MAX_COMM_ENERGY_MUNI_EFW))) %>%
      mutate(energy_EJ.2rev = if_else(value.1 * efw.MAX_COMM_ENERGY_MUNI_EFW >= deduction_EJ,
                                      value.2,
                                      value.2 + (value.1 * efw.MAX_COMM_ENERGY_MUNI_EFW) - deduction_EJ))

    # Re-build the energy balance tables, substituting revised data for reported data, and appending the municipal EFW
    L1012.in_EJ_R_revmuni1_F_Yh <- select(L1012.deduction_EJ_R_muni_F_Yh, GCAM_region_ID, fuel, year, from.sector, energy_EJ.1rev) %>%
      rename(sector = from.sector, energy_EJ_revised = energy_EJ.1rev)
    L1012.in_EJ_R_revmuni_F_Yh <- select(L1012.deduction_EJ_R_muni_F_Yh, GCAM_region_ID, fuel, year, from.sector.2, energy_EJ.2rev) %>%
      rename(sector = from.sector.2, energy_EJ_revised = energy_EJ.2rev) %>%
      bind_rows(L1012.in_EJ_R_revmuni1_F_Yh)

    # Append municipal total EFW, and join revisions to assigned sectors back into the energy balances table
    # This block uses left_join() because not all sectors and fuels have their energy demands revised by EFW
    L1012.en_bal_EJ_R_Si_Fi_Yh <- bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh,
                                            rename(L174.in_EJ_R_muniEFWtot_F_Yh, value = energy_EJ)) %>%
      left_join(L1012.in_EJ_R_revmuni_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = if_else(is.na(energy_EJ_revised), value, energy_EJ_revised)) %>%
      select(-energy_EJ_revised)

    # ===================================================

    # Produce outputs

    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      add_title("Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year", overwrite = TRUE) %>%
      add_units("EJ") %>%
      add_comments("EFW processes disaggregated, other sectors (e.g. commercial, industrial) energy consumption quantities modified") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "water/EFW_mapping",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "L171.in_EJ_R_desal_F_Yh",
                     "L172.in_EJ_R_irr_F_Yh",
                     "L173.in_EJ_R_indEFWtot_F_Yh",
                     "L174.in_EJ_R_muniEFWtot_F_Yh") ->
      L1012.en_bal_EJ_R_Si_Fi_Yh

    return_data(L1012.en_bal_EJ_R_Si_Fi_Yh)
  } else {
    stop("Unknown command")
  }
}
