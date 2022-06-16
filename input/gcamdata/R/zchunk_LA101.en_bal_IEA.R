# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA101.en_bal_IEA
#'
#' Rename IEA products and flows to intermediate fuels and sectors used for constructing GCAM's fuel and sector calibration.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.en_bal_EJ_R_Si_Fi_Yh_full}, \code{L101.en_bal_EJ_ctry_Si_Fi_Yh_full}, \code{L101.in_EJ_ctry_trn_Fi_Yh}, \code{L101.in_EJ_ctry_bld_Fi_Yh}. The corresponding file in the
#' original data system was \code{LA101.en_bal_IEA.R} (energy level1).
#' @details Assign IEA product and flow data to nomenclature used in GCAM (fuel and sector, respectively), summarizing
#' by (generally) iso and/or region, sector, fuel, and year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join matches mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author FF and BBL July 2017
module_energy_LA101.en_bal_IEA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A_regions",
             FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "energy/mappings/IEA_sector_fuel_modifications",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L100.IEA_en_bal_ctry_hist"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.en_bal_EJ_R_Si_Fi_Yh_full",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L101.in_EJ_ctry_trn_Fi_Yh",
             "L101.in_EJ_ctry_bld_Fi_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- GCAM_region_ID <- flow_code <- sector <- fuel <- product <- conversion <- tradbio_region <- sector_IEA <-
      fuel_IEA <- sector_initial <- fuel_initial <- sector3 <- sector2 <- fuel3 <- fuel2 <- conversion_IEA <- FLOW <-
      PRODUCT <- . <- year <- value <- trn <- bld <- reset <- conversion_initial <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_regions <- get_data(all_data, "energy/A_regions")
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    IEA_sector_fuel_modifications <- get_data(all_data, "energy/mappings/IEA_sector_fuel_modifications")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist")

    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running
    # without the proprietary IEA data files). If this is the case, we substitute
    # pre-built output datasets and exit.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used saved outputs
      L101.en_bal_EJ_R_Si_Fi_Yh_full <- extract_prebuilt_data("L101.en_bal_EJ_R_Si_Fi_Yh_full")
      L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- extract_prebuilt_data("L101.en_bal_EJ_ctry_Si_Fi_Yh_full")
      L101.in_EJ_ctry_trn_Fi_Yh <- extract_prebuilt_data("L101.in_EJ_ctry_trn_Fi_Yh")
      L101.in_EJ_ctry_bld_Fi_Yh <- extract_prebuilt_data("L101.in_EJ_ctry_bld_Fi_Yh")
    } else {

      # Add IEA data to main tibble (lines 35-46 in original file)
      L100.IEA_en_bal_ctry_hist %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
        # It is OK having NA at this stage since not every record has a match. NAs will be removed in the next step
        left_join(select(IEA_flow_sector, FLOW = flow_code, sector), by = "FLOW") %>%
        left_join(select(IEA_product_fuel, fuel, PRODUCT = product), by = "PRODUCT") %>%
        left_join(select(IEA_flow_sector, FLOW = flow_code, conversion), by = "FLOW") %>%
        na.omit() ->
        L101.IEA_en_bal_ctry_hist

      # The IEA commodity "Primary solid biomass" (i.e., wood, dung, straw, etc) consumed by the
      # residential sector is assigned to the GCAM commodity "traditional biomass" in selected regions,
      # indicated in A_regions. (48-65)
      L101.IEA_en_bal_ctry_hist %>%
        mutate(fuel = if_else(fuel == "biomass_tradbio" & sector != "in_bld_resid", "biomass", fuel)) %>%
        left_join_error_no_match(select(A_regions, tradbio_region, GCAM_region_ID), by = "GCAM_region_ID") %>%
        # Rename biomass_tradbio to biomas fuel to tradbio_region 0 (USA)
        mutate(fuel = if_else(fuel == "biomass_tradbio" & tradbio_region == 0, "biomass", fuel)) %>%
        select(-tradbio_region) %>%
        # In some countries, "gas works gas" is produced from coal. This is calibrated, assigned to the coal gasification
        # technology of gas processing.
        # Where the sector is gas works and the fuel is coal, re-name the sector to gas processing
        mutate(sector = if_else(sector == "net_gas works" & fuel == "coal", "in_gas processing", sector)) %>%
        # Where the sector is gas works and the fuel is not coal, this is assigned to industry/energy transformation
        mutate(sector = if_else(sector == "net_gas works" & fuel != "coal", "net_industry_energy transformation", sector)) ->
        L101.IEA_en_bal_ctry_hist

      # Reset some sector-fuel combinations, as specified in IEA_sector_fuel_modifications
      # Create a 'reset' flag to make our life easier
      L101.IEA_en_bal_ctry_hist %>%
        mutate(reset = paste(sector, fuel) %in% paste(IEA_sector_fuel_modifications$sector_initial, IEA_sector_fuel_modifications$fuel_initial)) ->
        L101.IEA_en_bal_ctry_hist

      L101.IEA_en_bal_ctry_hist %>%
        filter(reset) %>%
        rename(fuel_initial = fuel, sector_initial = sector, conversion_initial = conversion) %>%
        left_join(IEA_sector_fuel_modifications, by = c("sector_initial", "fuel_initial")) %>%
        # drop the initial fuel/sector/conversion data, as now we're using values specified in IEA_sector_fuel_modifications
        select(-fuel_initial, -sector_initial, -conversion_initial) %>%
        # bind with the non-reset rows
        bind_rows(filter(L101.IEA_en_bal_ctry_hist, !reset)) %>%
        # drop our temporary flag
        select(-reset) ->
        L101.IEA_en_bal_ctry_hist

      # Drop some sector-fuel combinations that are not relevant
      # Electricity-generation-only fuels (e.g., wind, solar, hydro, geothermal) consumed by sectors other than electricity generation
      # Primary biomass and district heat consumed by the transportation sector
      L101.IEA_en_bal_ctry_hist %>%
        mutate(sector = if_else(grepl("elec_", fuel) & !grepl("electricity generation",sector), NA_character_, sector),
               sector = if_else(fuel == "biomass" & grepl("trn_", sector), NA_character_, sector),
               sector = if_else(fuel == "heat" & grepl("trn_", sector), NA_character_, sector)) %>%
        na.omit() ->
        L101.IEA_en_bal_ctry_hist_clean

      # Aggregate by relevant categories, multiplying through by conversion factors (to EJ) (82-85)
      L101.IEA_en_bal_ctry_hist_clean %>%
        # note it's critical that 'conversion' is _last_ in this select, because summarise_all below will operate on it too!
        select(GCAM_region_ID, sector, fuel, matches(YEAR_PATTERN), conversion) %>%
        group_by(GCAM_region_ID, sector, fuel) %>%
        summarise_all(list(~ sum(. * conversion))) %>%
        select(-conversion) %>%
        # at this point dataset is much smaller; go to long form
        gather_years ->
        L101.en_bal_EJ_R_Si_Fi_Yh

      # Setting to zero net fuel production from energy transformation sectors modeled under the industrial sector
      # These processes (e.g., coke ovens) are modeled in GCAM as final energy consumption, not energy transformation/production
      # Setting to zero net production of fuels classified as coal at gas works (gas coke)
      L101.en_bal_EJ_R_Si_Fi_Yh %>%
        mutate(value = if_else(value < 0 & grepl("industry", sector), 0, value),
               value = if_else(value < 0 & sector == "in_gas processing", 0, value)) ->
        L101.en_bal_EJ_R_Si_Fi_Yh

      # Create a template table with all applicable combinations of sector and fuel found in any region
      # First, define the available combinations of sector and fuel and then repeat for all regions
      L101.en_bal_EJ_R_Si_Fi_Yh %>%
        ungroup() %>%
        distinct(sector, fuel)%>%
        repeat_add_columns(select(A_regions, GCAM_region_ID)) %>%
        select(GCAM_region_ID, sector, fuel) %>%
        repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
        left_join(select(L101.en_bal_EJ_R_Si_Fi_Yh,GCAM_region_ID, sector, fuel, year, value),
                  by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
        replace_na(list(value = 0)) ->
        L101.en_bal_EJ_R_Si_Fi_Yh_full

      # Calculate the total primary energy supply (TPES) in each region and fuel as the sum of all flows that are inputs
      # This guarantees that our TPES will be consistent with the tracked forms of consumption
      # (i.e. no statistical differences, stock changes, transfers)
      L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
        filter(grepl("in_", sector) | grepl("net_", sector)) %>%
        mutate(sector = "TPES")%>%
        group_by(GCAM_region_ID, sector, fuel, year) %>%
        summarise(value = sum(value)) ->
        L101.in_EJ_R_TPES_Fi_Yh

      # Append TPES onto the end of the energy balances
      L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
        bind_rows(L101.in_EJ_R_TPES_Fi_Yh) %>%
        add_title("Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") ->
        L101.en_bal_EJ_R_Si_Fi_Yh_full

      # For downscaling of buildings and transportation energy, aggregate by fuel and country
      # a: transport
      L101.IEA_en_bal_ctry_hist_clean %>%
        filter(grepl("trn", sector)) %>%
        left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by = "fuel") %>%
        select(-fuel, fuel = trn) %>%
        # note it's critical that 'conversion' is _last_ in this select, because summarise_all below will operate on it too!
        select(iso, sector, fuel, matches(YEAR_PATTERN), conversion) %>%
        group_by(iso, sector, fuel) %>%
        summarise_all(list(~ sum(. * conversion))) %>%
        ungroup %>%
        select(-conversion) %>%
        # at this point dataset is much smaller; go to long form
        gather_years %>%
        add_title("Transportation sector energy consumption by country / IEA mode / fuel / historical year") ->
        L101.in_EJ_ctry_trn_Fi_Yh

      # b: buildings
      L101.IEA_en_bal_ctry_hist_clean %>%
        filter(grepl("bld", sector)) %>%
        left_join_error_no_match(select(enduse_fuel_aggregation, fuel, bld), by = "fuel") %>%
        select(-fuel, fuel = bld) ->
        L101.in_ktoe_ctry_bld_Fiea

      L101.in_ktoe_ctry_bld_Fiea %>%
        # note it's critical that 'conversion' is _last_ in this select, because summarise_all below will operate on it too!
        select(iso, sector, fuel, matches(YEAR_PATTERN), conversion) %>%
        group_by(iso, sector, fuel) %>%
        summarise_all(list(~ sum(. * conversion))) %>%
        ungroup %>%
        select(-conversion) %>%
        # at this point dataset is much smaller; go to long form
        gather_years %>%
        add_title("Building energy consumption by country / IEA sector / fuel / historical year") ->
        L101.in_EJ_ctry_bld_Fi_Yh

      # For country-level comparisons, keep the iso and aggregate all sectors and fuels
      # This is a very expensive (slow) step
      L101.IEA_en_bal_ctry_hist_clean %>%
        # note it's critical that 'conversion' is _last_ in this select, because summarise_all below will operate on it too!
        select(iso, GCAM_region_ID, sector, fuel, matches(YEAR_PATTERN), conversion) %>%
        group_by(iso, GCAM_region_ID, sector, fuel) %>%
        summarise_all(list(~ sum(. * conversion))) %>%
        ungroup %>%
        select(-conversion) %>%
        # at this point dataset is much smaller; go to long form
        gather_years ->
        L101.en_bal_EJ_ctry_Si_Fi_Yh

      L101.en_bal_EJ_ctry_Si_Fi_Yh %>%
        filter(grepl("in_", sector) | grepl("net_", sector)) %>%
        mutate(sector = "TPES")%>%
        group_by(iso, GCAM_region_ID, sector, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup ->
        L101.in_EJ_ctry_TPES_Fi_Yh

      # bind all final tibbles
      L101.en_bal_EJ_ctry_Si_Fi_Yh %>%
        bind_rows(L101.in_EJ_ctry_TPES_Fi_Yh) %>%
        add_title("Energy balances by country / GCAM region / intermediate sector / intermediate fuel / historical year") ->
        L101.en_bal_EJ_ctry_Si_Fi_Yh_full

    ###############################################################################################################
    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      add_units("EJ") %>%
      add_comments("L101.en_bal_EJ_R_Si_Fi_Yh_full includes energy balances and assumptions for total primary energy supply (TPES)") %>%
      add_legacy_name("L101.en_bal_EJ_R_Si_Fi_Yh_full") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A_regions", "energy/mappings/IEA_flow_sector", "energy/mappings/IEA_product_fuel",
                     "energy/mappings/IEA_sector_fuel_modifications", "energy/mappings/enduse_fuel_aggregation",
                     "L100.IEA_en_bal_ctry_hist") ->
      L101.en_bal_EJ_R_Si_Fi_Yh_full

    L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
      add_units("EJ") %>%
      add_comments("For country-level comparisons, keep the iso and aggregate all sectors and fuels. It also includes TPES by country") %>%
      add_legacy_name("L101.en_bal_EJ_ctry_Si_Fi_Yh_full") %>%
      same_precursors_as(L101.en_bal_EJ_R_Si_Fi_Yh_full) ->
      L101.en_bal_EJ_ctry_Si_Fi_Yh_full

    L101.in_EJ_ctry_trn_Fi_Yh %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the transport sector by fuel and historical year. Aggregated by fuel and country") %>%
      add_legacy_name("L101.in_EJ_ctry_trn_Fi_Yh") %>%
      same_precursors_as(L101.en_bal_EJ_R_Si_Fi_Yh_full) ->
      L101.in_EJ_ctry_trn_Fi_Yh

    L101.in_EJ_ctry_bld_Fi_Yh %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the building sector by fuel and historical year. Aggregated by fuel and country") %>%
      add_legacy_name("L101.in_EJ_ctry_bld_Fi_Yh") %>%
      same_precursors_as(L101.en_bal_EJ_R_Si_Fi_Yh_full) ->
      L101.in_EJ_ctry_bld_Fi_Yh

    # At this point outputs should be identical to the prebuilt versions
    verify_identical_prebuilt(L101.en_bal_EJ_R_Si_Fi_Yh_full,
                              L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                              L101.in_EJ_ctry_trn_Fi_Yh,
                              L101.in_EJ_ctry_bld_Fi_Yh)
    }

    return_data(L101.en_bal_EJ_R_Si_Fi_Yh_full, L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                L101.in_EJ_ctry_trn_Fi_Yh, L101.in_EJ_ctry_bld_Fi_Yh)
  } else {
    stop("Unknown command")
  }
}
