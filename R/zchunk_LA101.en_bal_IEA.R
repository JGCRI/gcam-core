#' module_energy_LA101.en_bal_IEA
#'
#' This chunk takes IEA data and renames sectors and fuel data to nomenclature used in GCAM. It also adjustes information uploaded from L100.IEA_en_bal_ctry_hist according to IEA_sector_fuel_modifications
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.en_bal_EJ_R_Si_Fi_Yh_full}, \code{L101.en_bal_EJ_ctry_Si_Fi_Yh_full}, \code{L101.in_EJ_ctry_trn_Fi_Yh}, \code{L101.in_EJ_ctry_bld_Fi_Yh}. The corresponding file in the
#' original data system was \code{LA101.en_bal_IEA.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author FF July 2017
#' @export
module_energy_LA101.en_bal_IEA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/A_regions",
             FILE = "energy/IEA_flow_sector",
             FILE = "energy/IEA_product_fuel",
             FILE = "energy/IEA_sector_fuel_modifications",
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "temp-data-inject/L100.IEA_en_bal_ctry_hist"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.en_bal_EJ_R_Si_Fi_Yh_full",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L101.in_EJ_ctry_trn_Fi_Yh",
             "L101.in_EJ_ctry_bld_Fi_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_regions <- get_data(all_data, "energy/A_regions")
    IEA_flow_sector <- get_data(all_data, "energy/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/IEA_product_fuel")
    IEA_sector_fuel_modifications <- get_data(all_data, "energy/IEA_sector_fuel_modifications")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "temp-data-inject/L100.IEA_en_bal_ctry_hist")
    ###############################################################################################################
    # Data in long format.
    L100.IEA_en_bal_ctry_hist %>%
      gather(year, value, -iso, -FLOW, -PRODUCT) %>%
      mutate(year=as.integer(substr(year,2,5))) -> L101.IEA_en_bal_ctry_hist

    # Add IEA data to main tibble
    L101.IEA_en_bal_ctry_hist %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # It is OK having NA at this stage since not every record has a match. NAs will be removed in the next step
      left_join(select(IEA_flow_sector, FLOW=flow_code, sector), by ="FLOW") %>%
      left_join(select(IEA_product_fuel, fuel, PRODUCT = product), by ="PRODUCT") %>%
      left_join(select(IEA_flow_sector, FLOW = flow_code, conversion), by ="FLOW") %>%
      na.omit() -> L101.IEA_en_bal_ctry_hist

    # Rename biofuel fuel name for appropiate sectors
    L101.IEA_en_bal_ctry_hist %>%
      mutate( fuel = if_else(fuel == "biomass_tradbio" & sector != "in_bld_resid", "biomass", fuel)) %>%
      left_join_error_no_match(select(A_regions, tradbio_region, GCAM_region_ID), by = "GCAM_region_ID") %>%
      mutate(fuel = if_else(fuel == "biomass_tradbio" & tradbio_region == 0, "biomass", fuel)) %>%
      select(- tradbio_region) -> L101.IEA_en_bal_ctry_hist

    # In some countries, "gas works gas" is produced from coal. This is calibrated (coal gasification), so re-name the relevant sectors
    # Rename the sector and the fuel: where the sector is gas works and the fuel is coal, this is the input to gas processing
    L101.IEA_en_bal_ctry_hist %>%
      mutate(sector= if_else(sector == "net_gas works" & fuel == "coal", "in_gas processing", sector)) %>%
      #Where the sector is gas works and the fuel is not coal, this is industry/energy transformation
      mutate(sector= if_else(sector == "net_gas works" & fuel != "coal", "net_industry_energy transformation", sector)) -> L101.IEA_en_bal_ctry_hist

    # Create aux tibble for IEA_sector_fuel_modifications with renamed values for Re-setting sector-fuel combinations as specified in IEA_sector_fuel_modifications
    IEA_sector_fuel_modifications %>%
      mutate(sector = if_else(is.na(sector), "none", sector)) %>%
      mutate(fuel = if_else(is.na(fuel), "none", fuel)) %>%
      rename(sector_IEA = sector, fuel_IEA = fuel) %>%
      select(sector_IEA, fuel_IEA, sector = sector_initial, fuel = fuel_initial, conversion_IEA = conversion) %>%
      mutate(sector2 = sector, fuel2 = fuel) -> sector_fuel_tochange

    # Adding EIA sector and fuel combinations defined in sector_fuel_tochange (IEA_sector_fuel_modifications)
    # NAs are replaced by "none" since they create errors when joining.Any other NAs that that is generated is removed
    # Sector/fuel2 and 3 are used as dummy variables that are later removed. These are used to replace original sector and fuels by IEA ones.
    L101.IEA_en_bal_ctry_hist %>%
      mutate(sector = if_else(is.na(sector), "none", sector)) %>%
      mutate(fuel = if_else(is.na(fuel), "none", fuel)) %>%
      mutate(sector3 = sector, fuel3 = fuel) %>%
      left_join(sector_fuel_tochange, by = c("sector" , "fuel")) %>%
      mutate(sector = if_else(sector3 == sector2 & fuel3 == fuel2&!is.na(sector2)&!is.na(fuel2) , sector_IEA, sector)) %>%
      mutate(fuel   = if_else(sector3 == sector2 & fuel3 == fuel2&!is.na(sector2)&!is.na(fuel2) , fuel_IEA, fuel)) %>%
      mutate(conversion = if_else(sector3 == sector2 & fuel3 == fuel2&!is.na(sector2)&!is.na(fuel2) , conversion_IEA, conversion)) %>%
      select(-sector_IEA, -fuel_IEA, -conversion_IEA, -sector2, -fuel2, -sector3, -fuel3) %>%
      select(iso, FLOW, PRODUCT, GCAM_region_ID, sector, fuel, conversion, year, value) %>%
      arrange(iso, FLOW, PRODUCT, GCAM_region_ID, sector, fuel, year) %>%
      filter(fuel != "none") %>% na.omit()-> L101.IEA_en_bal_ctry_hist

    # Drop some sector-fuel combinations that are not relevant
    # Electricity-only fuels in sectors other than electricity generation
    L101.IEA_en_bal_ctry_hist %>%
      mutate(sector=if_else(grepl( "elec_", fuel) & !grepl( "electricity generation",sector),NA_character_, sector)) %>%
      mutate(sector=if_else(fuel=="biomass" & grepl( "trn_", sector),NA_character_, sector)) %>%
      mutate(sector=if_else(fuel=="heat" & grepl( "trn_", sector),NA_character_, sector)) %>%
      na.omit() -> L101.IEA_en_bal_ctry_hist_clean


    #Aggregate by relevant categories, multiplying through by conversion factors (to EJ)
    L101.IEA_en_bal_ctry_hist_clean %>%
      mutate(value = value * conversion) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) -> L101.en_bal_EJ_R_Si_Fi_Yh

    # Setting to zero net fuel production from energy transformation sectors modeled under the industrial sector
    # Setting to zero net production of fuels classified as coal at gas works (gas coke)
    L101.en_bal_EJ_R_Si_Fi_Yh%>%
      mutate(value = if_else(value <0 & grepl( "industry", sector), 0, value)) %>%
      mutate(value = if_else(value <0 & sector == "in_gas processing", 0, value)) -> L101.en_bal_EJ_R_Si_Fi_Yh

    # Create a template table with all applicable combinations of sector and fuel found in any region
    # First, define the available combinations of sector and fuel and then repeat for all regions
    L101.en_bal_EJ_R_Si_Fi_Yh %>% ungroup() %>%
      select(sector, fuel) %>%
      distinct(sector, fuel)%>%
      repeat_add_columns(select(A_regions,GCAM_region_ID)) %>%
      select(GCAM_region_ID, sector, fuel) -> L101.en_bal_EJ_R_Si_Fi_Yh_full

    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      left_join(select(L101.en_bal_EJ_R_Si_Fi_Yh,GCAM_region_ID, sector, fuel, year, value),
                by = c("GCAM_region_ID","sector", "fuel","year")) %>%
      mutate(value = if_else(is.na(value), 0, value)) -> L101.en_bal_EJ_R_Si_Fi_Yh_full



    # Calculate the total primary energy supply (TPES) in each region as the sum of all flows that are inputs
    # This guarantees that our TPES will be consistent with the tracked forms of consumption (i.e. no statdiffs, stock changes, transfers)
    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      filter(grepl("in_", sector) | grepl("net_", sector)) %>%
      mutate(sector = "TPES")%>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) -> L101.in_EJ_R_TPES_Fi_Yh

    # Append (rbind) TPES onto the end of the energy balances
    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      bind_rows(L101.in_EJ_R_TPES_Fi_Yh) -> L101.en_bal_EJ_R_Si_Fi_Yh_full

    # For downscaling of buildings and transportation energy, aggregate by fuel and country
    # a: transport
    L101.IEA_en_bal_ctry_hist_clean %>%
      filter(grepl("trn", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by ="fuel") %>%
      select(-fuel, fuel=trn) -> L101.in_ktoe_ctry_trn_Fiea

    L101.in_ktoe_ctry_trn_Fiea %>%
      select(iso, sector, fuel, year, value, conversion) %>%
      mutate(trn = value * conversion) %>%
      group_by(iso, sector, fuel, year) %>%
      summarise(value = sum(trn)) -> L101.in_EJ_ctry_trn_Fi_Yh

    # b: buildings
    L101.IEA_en_bal_ctry_hist_clean %>%
      filter(grepl("bld", sector)) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, bld), by ="fuel") %>%
      select(-fuel, fuel=bld) -> L101.in_ktoe_ctry_bld_Fiea

    L101.in_ktoe_ctry_bld_Fiea %>%
      select(iso, sector, fuel, year, value, conversion) %>%
      mutate(bld = value * conversion) %>%
      group_by(iso, sector, fuel, year) %>%
      summarise(value = sum(bld)) -> L101.in_EJ_ctry_bld_Fi_Yh

    # For country-level comparisons, keep the iso and aggregate all sectors and fuels
    L101.IEA_en_bal_ctry_hist_clean %>%
      select(-FLOW, -PRODUCT) %>%
      group_by(iso, GCAM_region_ID, sector, fuel, year) %>%
      mutate(value_int = value * conversion ) %>%
      summarise(value = sum(value_int)) -> L101.en_bal_EJ_ctry_Si_Fi_Yh

    L101.en_bal_EJ_ctry_Si_Fi_Yh %>%
      filter(grepl("in_", substr( sector, 1, 3 )) | grepl( "net_", substr( sector, 1, 4 ))) %>%
      ungroup() %>%
      mutate(sector = "TPES")%>%
      group_by(iso, GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) -> L101.in_EJ_ctry_TPES_Fi_Yh

    # bind all final tibbles
    L101.en_bal_EJ_ctry_Si_Fi_Yh %>%
      bind_rows(L101.in_EJ_ctry_TPES_Fi_Yh) -> L101.en_bal_EJ_ctry_Si_Fi_Yh_full


    ###############################################################################################################
    L101.en_bal_EJ_R_Si_Fi_Yh_full%>%
      add_title("Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("L101.en_bal_EJ_R_Si_Fi_Yh_full includes energy balances and assumptions for total primary energy supply (TPES)") %>%
      add_legacy_name("L101.en_bal_EJ_R_Si_Fi_Yh_full") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A_regions", "energy/IEA_flow_sector", "energy/IEA_product_fuel", "energy/IEA_sector_fuel_modifications", "energy/enduse_fuel_aggregation", "temp-data-inject/L100.IEA_en_bal_ctry_hist") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.en_bal_EJ_R_Si_Fi_Yh_full
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
      add_title("Energy balances by country / GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("For country-level comparisons, keep the iso and aggregate all sectors and fuels. It also includes TPES by country") %>%
      add_legacy_name("L101.en_bal_EJ_ctry_Si_Fi_Yh_full") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A_regions", "energy/IEA_flow_sector", "energy/IEA_product_fuel", "energy/IEA_sector_fuel_modifications", "energy/enduse_fuel_aggregation", "temp-data-inject/L100.IEA_en_bal_ctry_hist") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR,FLAG_SUM_TEST) ->
      L101.en_bal_EJ_ctry_Si_Fi_Yh_full
    L101.in_EJ_ctry_trn_Fi_Yh %>%
      add_title("Transportation sector energy consumption by country / IEA mode / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the transport sector by fuel and historical year. Aggregates by fuel and country") %>%
      add_legacy_name("L101.in_EJ_ctry_trn_Fi_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A_regions", "energy/IEA_flow_sector", "energy/IEA_product_fuel", "energy/IEA_sector_fuel_modifications", "energy/enduse_fuel_aggregation", "temp-data-inject/L100.IEA_en_bal_ctry_hist") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.in_EJ_ctry_trn_Fi_Yh
    L101.in_EJ_ctry_bld_Fi_Yh %>%
      add_title("Building energy consumption by country / IEA sector / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Consumption of energy by the building sector by fuel and historical year. Aggregates by fuel and country") %>%
      add_legacy_name("L101.in_EJ_ctry_bld_Fi_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A_regions", "energy/IEA_flow_sector", "energy/IEA_product_fuel", "energy/IEA_sector_fuel_modifications", "energy/enduse_fuel_aggregation", "temp-data-inject/L100.IEA_en_bal_ctry_hist") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.in_EJ_ctry_bld_Fi_Yh

    return_data(L101.en_bal_EJ_R_Si_Fi_Yh_full, L101.en_bal_EJ_ctry_Si_Fi_Yh_full, L101.in_EJ_ctry_trn_Fi_Yh, L101.in_EJ_ctry_bld_Fi_Yh)
  } else {
    stop("Unknown command")
  }
}


