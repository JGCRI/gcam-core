#' module_energy_LA154.transportation_UCD
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_R_trn_m_sz_tech_F_Yh}, \code{L154.in_EJ_ctry_trn_m_sz_tech_F}, \code{L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y}, \code{L154.loadfactor_R_trn_m_sz_tech_F_Y}, \code{L154.cost_usdvkm_R_trn_m_sz_tech_F_Y}, \code{L154.speed_kmhr_R_trn_m_sz_tech_F_Y}, \code{L154.out_mpkm_R_trn_nonmotor_Yh}. The corresponding file in the
#' original data system was \code{LA154.transportation_UCD.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH May 2017
#' @export
module_energy_LA154.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/calibrated_techs_trn_agg", #needs source
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "energy/mappings/UCD_ctry", #needs source
             FILE = "energy/mappings/UCD_techs", #needs source
             # This file is currently using a constant to select the correct SSP database
             # All SSP databases will be included in the input files
             # needs source, better description
             FILE = paste0("energy/UCD_trn_data_",energy.TRN_SSP),
             FILE = "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh",
             FILE = "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
             FILE = "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh",
             FILE = "temp-data-inject/L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.in_EJ_ctry_trn_m_sz_tech_F",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
             "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs_trn_agg <- get_data(all_data, "energy/mappings/calibrated_techs_trn_agg")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    UCD_ctry <- get_data(all_data, "energy/mappings/UCD_ctry")
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")
    UCD_trn_data <- get_data(all_data,  paste0("energy/UCD_trn_data_",energy.TRN_SSP)) %>%
      # Might want to gather in a different way
      gather(year, value, `2005`:`2095`) %>%
      mutate(year = as.integer(year))
    L101.in_EJ_ctry_trn_Fi_Yh <- get_data(all_data, "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L1011.in_EJ_ctry_intlship_TOT_Yh <- get_data(all_data, "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "temp-data-inject/L100.Pop_thous_ctry_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    # ===================================================
    # Part 1: downscaling country-level transportation energy data to UCD transportation technologies
    # NOTE: We are currently aggregating IEA's data on rail and road due to inconsistencies (e.g. no rail in the Middle East)

    # First, replace the international shipping data (swapping in EIA for IEA)
    # Only perform this swap for international shipping / refined liquids, and in countries in the EIA database
    L154.in_EJ_ctry_trn_Fi_Yh <- L101.in_EJ_ctry_trn_Fi_Yh %>%
      # left_join used here because we only want to replace certain values
      left_join(L1011.in_EJ_ctry_intlship_TOT_Yh %>% rename(EIA_value = value), by = c("iso","year")) %>%
      mutate(value = if_else(sector == "in_trn_international ship" &
                               fuel == "refined liquids" &
                               !is.na(EIA_value), EIA_value, value),
             sector = sub("in_", "", sector)) %>%
      select(iso, sector, fuel, year, value)

    # Need to map sector to UCD_category, calibrated_techs_trn_agg data is too busy
    UCD_category_mapping <- calibrated_techs_trn_agg %>% select(sector, UCD_category) %>% distinct

    # Aggregate to UCD_category in each country/year instead of sector
    L154.in_EJ_ctry_trn_Fi_Yh <- L154.in_EJ_ctry_trn_Fi_Yh %>%
      left_join_error_no_match(UCD_category_mapping, by = "sector") %>%
      group_by(iso, UCD_category, fuel, year) %>%
      summarise(value = sum(value))

    # Aggregating UCD transportation database by the general categories used for the IEA transportation data
    # These will be used to compute shares for allocation of energy to mode/technology/fuel within category/fuel
    L154.in_PJ_Rucd_trn_m_sz_tech_F<- UCD_trn_data %>%
      filter(variable == "energy") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel"))

    L154.in_PJ_Rucd_trn_Cat_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      # Filtering only to base year for computing shares
      filter(year == energy.UCD_EN_YEAR) %>%
      group_by(UCD_region, UCD_category, fuel) %>%
      summarise(agg = sum(value))

    # Match these energy quantities back into the complete table for computation of shares
    L154.in_PJ_Rucd_trn_m_sz_tech_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      filter(year == energy.UCD_EN_YEAR) %>%
      left_join_error_no_match(L154.in_PJ_Rucd_trn_Cat_F, by = c("UCD_region", "UCD_category", "fuel")) %>%
      # If the aggregate is 0 or value is NA, set share to 0, rather than NA
      mutate(UCD_share = if_else(agg == 0 | is.na(value), 0, value/agg))

    # Writing out the UC Davis mode/technology/fuel shares within category/fuel at the country level
    # First, creating a table of desired countries with their UCD regions
    ctry_iso_region <- tibble::tibble(iso = unique(L101.in_EJ_ctry_trn_Fi_Yh$iso)) %>%
      left_join_error_no_match(UCD_ctry, by = "iso")

    L154.share_ctry_trn_m_sz_tech_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      repeat_add_columns(ctry_iso_region) %>%
      filter(UCD_region.x == UCD_region.y) %>%
      select(iso, UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel, UCD_share)

    # Multiplying historical energy by country/category/fuel by shares of country/mode/tech/fuel within country/category/fuel
    # Need a value for each iso, year, UCD category, and fuel combo, even if not currently in L154.in_EJ_ctry_trn_Fi_Yh
     UCD_cat_fuel <- L154.share_ctry_trn_m_sz_tech_F %>%
       select(UCD_category, fuel) %>%
       distinct
     iso_year <- L154.in_EJ_ctry_trn_Fi_Yh %>%
       ungroup %>%
       select(iso, year) %>%
       distinct

     L154.in_EJ_ctry_trn_m_sz_tech_F <- UCD_cat_fuel %>%
       repeat_add_columns(iso_year) %>%
       left_join(L154.share_ctry_trn_m_sz_tech_F, by = c("UCD_category", "fuel", "iso")) %>%
       left_join(L154.in_EJ_ctry_trn_Fi_Yh, by = c("UCD_category", "fuel", "iso", "year")) %>%
       left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
       # Multiply value by share. Set missing values to 0. These are combinations not available in the data from IEA.
       mutate(value = if_else(is.na(value), 0, value*UCD_share)) %>%
       select(iso, UCD_sector, mode, size.class, UCD_technology,
              UCD_fuel, UCD_category, fuel, GCAM_region_ID, year, value)

     # Aggregating by GCAM region
     L154.in_EJ_R_trn_m_sz_tech_F <- L154.in_EJ_ctry_trn_m_sz_tech_F %>%
       group_by(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year) %>%
       summarise(value = sum(value))

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_R_trn_m_sz_tech_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_R_trn_m_sz_tech_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_ctry_trn_m_sz_tech_F") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_ctry_trn_m_sz_tech_F
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.loadfactor_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.loadfactor_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.cost_usdvkm_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.speed_kmhr_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.speed_kmhr_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.out_mpkm_R_trn_nonmotor_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      #typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.out_mpkm_R_trn_nonmotor_Yh

    return_data(L154.in_EJ_R_trn_m_sz_tech_F_Yh, L154.in_EJ_ctry_trn_m_sz_tech_F, L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y, L154.loadfactor_R_trn_m_sz_tech_F_Y, L154.cost_usdvkm_R_trn_m_sz_tech_F_Y, L154.speed_kmhr_R_trn_m_sz_tech_F_Y, L154.out_mpkm_R_trn_nonmotor_Yh)
  } else {
    stop("Unknown command")
  }
}
