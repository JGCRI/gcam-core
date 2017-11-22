#' module_energy_LA119.solar
#'
#' Computes relative average irradiance and direct normal irradiance by GCAM region for solar sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L119.Irradiance_rel_R}. The corresponding file in the
#' original data system was \code{LA119.solar.R} (energy level1).
#' @details The cunk computes relative average irradiance and direct normal irradiance from Smith_irradiance_ctry_kwh.csv
#' by GCAM region for solar sector. The average irradiance and direct normal irradiance is relative to USA values.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author LF November 2017
module_energy_LA119.solar <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/Land_type_area_ha",
             FILE = "energy/Smith_irradiance_ctry_kwh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L119.Irradiance_rel_R"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Land_type_area_ha <- get_data(all_data, "aglu/LDS/Land_type_area_ha")
    Smith_irradiance_ctry_kwh <- get_data(all_data, "energy/Smith_irradiance_ctry_kwh")

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    Area.bm2.R <- Area.share <- GCAM_region_ID <- dni <- dni.R <- dni.area <- dni.area.R <-
      dni_avg <- dni_avg_rel <- irradiance <- irradiance.R <- irradiance.area <-
      irradiance.area.R <- irradiance_avg <- irradiance_avg_rel <- iso <-
      region_GCAM3 <- value <- year <- NULL

    # ===================================================
    # 2. Perform computations
    # The irradiance data contained other * region categories which we will have to downscale
    # Seperate the other * region from the ones with valid country codes that do not need additonal
    # downscaling
    Smith_irradiance_ctry_kwh %>%
      filter(iso %in% grep("OTHER", iso, value = T)) %>%
      # Rename columns to indicate these are the regional values which will be used later
      # to downscale to the country
      mutate(region_GCAM3 = sub("OTHER_", "", iso)) %>%
      select(-iso) %>%
      rename(irradiance.R = irradiance,
             irradiance.area.R = irradiance.area,
             dni.R = dni,
             dni.area.R = dni.area) %>%
      left_join(iso_GCAM_regID, "region_GCAM3") -> # change of number of rows in data expected
      L119.Irradiance_kwh_otherR # intermediate tibble

    Smith_irradiance_ctry_kwh %>%
      filter( iso %in% grep("OTHER", iso, value = T, invert = T)) %>%
      inner_join(iso_GCAM_regID, "iso")->
      L119.Irradiance_kwh_ctry # intermediate tibble

    # Create a list of coutries that are missing and are in the other * region
    iso_GCAM_regID %>%
      filter(!(iso %in% L119.Irradiance_kwh_ctry[["iso"]]),
             region_GCAM3 %in% L119.Irradiance_kwh_otherR[["region_GCAM3"]]) ->
      L119.Other_ctry # intermediate tibble

    # Building up land areas of all of other countries and calculate their share of land
    # in the other * region
    Land_type_area_ha %>%
      filter(year == max(year)) %>%
      mutate(value = value * CONV_HA_BM2) %>%
      group_by(iso) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      inner_join(L119.Other_ctry, "iso") ->
      L119.LC_bm2_other_ctry # intermediate tibble

    L119.LC_bm2_other_ctry %>%
      group_by(region_GCAM3) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      rename(Area.bm2.R = value) %>%
      right_join(L119.LC_bm2_other_ctry, "region_GCAM3") %>%
      mutate(Area.share = value / Area.bm2.R) %>%
      # Now include the irradiance data at the regional level and multiply the share to downscale to the
      # country
      inner_join(L119.Irradiance_kwh_otherR, by = c( "region_GCAM3", "iso", "country_name", "GCAM_region_ID")) %>%
      mutate( irradiance = irradiance.R * Area.share,
              irradiance.area = irradiance.area.R * Area.share,
              dni = dni.R * Area.share,
              dni.area = dni.area.R * Area.share) %>%
      # Add the downscaled coutries in with the rest of the country irradiance data
      select(one_of(names(L119.Irradiance_kwh_ctry))) %>%
      bind_rows(L119.Irradiance_kwh_ctry) ->
      L119.Irradiance_kwh_ctry # intermediate tibble

    # Aggregate irradiance data to the regional level
    L119.Irradiance_kwh_ctry %>%
      group_by(GCAM_region_ID) %>%
      summarise(irradiance = sum(irradiance),
                irradiance.area = sum(irradiance.area),
                dni = sum(dni),
                dni.area = sum(dni.area)) %>%
      ungroup %>%
      # Compute average irradiance
      mutate(irradiance_avg = irradiance / irradiance.area,
             dni_avg = dni / dni.area) ->
      L119.Irradiance_kwh_R # intermediate tibble

    # Calculate average relative to USA
    L119.Irradiance_kwh_ctry %>%
      filter(iso == "usa") %>%
      mutate(irradiance_avg = irradiance / irradiance.area,
             dni_avg = dni / dni.area) ->
      L119.Irradiance_kwh_usa # intermediate tibble

    L119.Irradiance_kwh_R %>%
      mutate(irradiance_avg_rel = irradiance_avg / L119.Irradiance_kwh_usa[["irradiance_avg"]],
             dni_avg_rel = dni_avg / L119.Irradiance_kwh_usa[["dni_avg"]]) %>%
      mutate( irradiance_avg_rel = if_else(is.na(irradiance_avg_rel), 0.001, irradiance_avg_rel),
              dni_avg_rel = if_else(is.na(dni_avg_rel), 0.001, dni_avg_rel)) %>%
      # The only thing we need for later processing is relative irradiance
      select(GCAM_region_ID, irradiance_avg_rel, dni_avg_rel) ->
      L119.Irradiance_rel_R

    # ===================================================
    # Produce outputs

    L119.Irradiance_rel_R %>%
      add_title("Relative solar irradiance by GCAM region") %>%
      add_units("Unitless") %>%
      add_comments("The irradiance and dni for aggregated 'OTHER_*' regions are first downscaled to country level then combined with irradiance and dni for rest countries to aggregate to GCAM regions. ") %>%
      add_comments("The irradiance and dni by GCAM region are then calculated as relative average irradiance and dni using USA values.") %>%
      add_legacy_name("L119.Irradiance_rel_R") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/Land_type_area_ha", "energy/Smith_irradiance_ctry_kwh") ->
      L119.Irradiance_rel_R

    return_data(L119.Irradiance_rel_R)
  } else {
    stop("Unknown command")
  }
}
