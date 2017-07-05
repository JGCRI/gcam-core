#' module_energy_LA121.oil
#'
#' This chunk processes historical oil data and separates into unconventional oil, crude oil, and energy inputs to oil
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L121.in_EJ_R_unoil_F_Yh}, \code{L121.in_EJ_R_TPES_crude_Yh}, \code{L121.in_EJ_R_TPES_unoil_Yh}. The corresponding file in the
#' original data system was \code{LA121.oil.R} (energy level1).
#' @details This chunk uses energy production data to determine the energy inputs to the oil sector. It uses
#' the IEA energy balance to separate out unconventional oil production and crude oil (total liquids - unconventional oil).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author JDH June 2017
module_energy_LA121.oil <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "energy/mappings/IEA_product_rsrc",
             FILE = "energy/A21.unoil_demandshares",
             FILE = "energy/A21.globaltech_coef",
                    "L100.IEA_en_bal_ctry_hist",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "temp-data-inject/L111.Prod_EJ_R_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L121.in_EJ_R_unoil_F_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             "L121.in_EJ_R_TPES_unoil_Yh"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    year <- value <- iso <- FLOW <- PRODUCT <- fuel <- sector <-
      share_ctry_RG3 <- value_unoil <- GCAM_region_ID <- calibration <-
      secondary.output <- supplysector <- region_GCAM3 <- value_RG3 <-
      share <- share_RG3_world <- subsector <- technology <- minicam.energy.input <-
      value_coef <- fuel.y <- value_coef_gas <- resource <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    IEA_product_rsrc <- get_data(all_data, "energy/mappings/IEA_product_rsrc")
    A21.unoil_demandshares <- get_data(all_data, "energy/A21.unoil_demandshares")
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")

    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist") %>%
      gather(year, value, -iso, -FLOW, -PRODUCT) %>%
      mutate(year = as.integer(year)) -> L100.IEA_en_bal_ctry_hist

    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh")%>%
      gather(year, value, -fuel, -sector, -GCAM_region_ID) %>%
      mutate(year = sub("X", "", year)) %>%
      mutate(year = as.integer(year)) -> L1011.en_bal_EJ_R_Si_Fi_Yh

    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "temp-data-inject/L111.Prod_EJ_R_F_Yh")%>%
      gather(year, value, -fuel, -sector, -GCAM_region_ID) %>%
      mutate(year = sub("X", "", year)) %>%
      mutate(year = as.integer(year)) -> L111.Prod_EJ_R_F_Yh

    # ===================================================

    # Calculating energy inputs (gas) to unconventional oil production in the historical years
    A21.globaltech_coef %>%
      left_join(calibrated_techs, by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      filter(!is.na(sector)) %>%
      select(-sector, -fuel, -calibration, -secondary.output) -> L121.globaltech_coef

    L121.globaltech_coef %>%
      gather(year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      mutate(year = as.integer(year))  %>%
    # Adding empty historical years to fill in with interpolation
        complete(year = unique(c(HISTORICAL_YEARS, year)),
                 nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
        arrange(year) %>%
        group_by(technology, subsector, supplysector, minicam.energy.input)  %>%
    # Interpolate to fill in missing globaltech_coef historical years
        mutate(value = approx_fun(year, value)) %>%
        left_join(distinct(calibrated_techs), by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
        select(supplysector, subsector, technology, minicam.energy.input, year, value, sector, fuel) -> L121.globaltech_coef_interp

    # Energy inputs = production times fuel IO coef
    L111.Prod_EJ_R_F_Yh %>%
      # Join on sector / fuel, keeping only the rows that are in both tables
      inner_join(L121.globaltech_coef_interp %>%
                  rename(value_coef = value), by = c("sector", "fuel", "year")) %>%
      select(GCAM_region_ID, sector, fuel, year, value, value_coef ) %>%
      left_join(L121.globaltech_coef_interp %>%
                  rename(value_coef_gas = value), by = c("sector", "year")) %>%
      rename(fuel = fuel.y) %>%
      mutate(value = value * value_coef_gas) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L121.in_EJ_R_unoil_F_Yh

    # Downscaling unconventional oil consumption shares by GCAM 3.0 region to countries
    product_filters <- IEA_product_rsrc %>% filter(resource == "crude oil")
    product_filters <- unique(product_filters$PRODUCT)

    L100.IEA_en_bal_ctry_hist %>%
      filter(FLOW == "TPES", PRODUCT %in% product_filters) %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(iso, value) %>%
      filter(!is.na(value)) %>%
      group_by(iso) %>%
      summarise(value = sum(value)) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                  select(region_GCAM3, iso), by = "iso")-> L121.TPES_ktoe_ctry_oil_Yf

    L121.TPES_ktoe_ctry_oil_Yf %>%
      group_by(region_GCAM3) %>%
      summarise(value = sum(value)) -> L121.TPES_ktoe_RG3_oil_Yf

    # Calculate the shares of country-within-GCAM3-region, match in shares of GCAM3-region-within-world, and multiply
    L121.TPES_ktoe_ctry_oil_Yf %>%
      left_join(L121.TPES_ktoe_RG3_oil_Yf %>%
                  rename(value_RG3 = value), by = "region_GCAM3") %>%
      mutate(share_ctry_RG3 = value / value_RG3) %>%
      left_join(A21.unoil_demandshares, by = "region_GCAM3") %>%
      mutate(share_RG3_world = share) %>%
      mutate(share = share_ctry_RG3 * share_RG3_world) %>%
      select(-value_RG3) %>%
      left_join(iso_GCAM_regID %>%
                  select(iso, GCAM_region_ID), by = "iso") ->  L121.TPES_ktoe_ctry_oil_Yf

    # Aggregating country shares of unconventional oil demand to GCAM regions
    L121.TPES_ktoe_ctry_oil_Yf  %>%
      group_by(GCAM_region_ID) %>%
      summarise(share = sum(share)) %>%
      select(GCAM_region_ID, share) -> L121.share_R_TPES_unoil_Yf

    # Calculating unconventional oil demand by region and historical year
    L111.Prod_EJ_R_F_Yh %>%
      filter(fuel == "unconventional oil") %>%
      group_by(fuel, year, sector) %>%
      summarise(value = sum(value)) %>%
      select(fuel, year, value) -> L121.Prod_EJ_unoil_Yh

    L121.share_R_TPES_unoil_Yf %>%
      repeat_add_columns(L121.Prod_EJ_unoil_Yh) %>%
      mutate(value = share * value) %>%
      select(GCAM_region_ID, fuel, year, value) %>%
      mutate(sector = "TPES") -> L121.in_EJ_R_TPES_unoil_Yh

    #Conventional (crude) oil: calculate as liquids TPES - unconventional oil
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES", fuel == "refined liquids") -> L121.in_EJ_R_TPES_liq_Yh

    L121.in_EJ_R_TPES_liq_Yh %>%
      select(GCAM_region_ID, sector, fuel, year, value) %>%
      mutate(fuel = "crude oil") %>%
      left_join(L121.in_EJ_R_TPES_unoil_Yh %>%
                  rename(value_unoil = value, unoil = fuel), by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = value - value_unoil) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L121.in_EJ_R_TPES_crude_Yh


    # ===================================================
    # Produce outputs
    L121.in_EJ_R_unoil_F_Yh %>%
      add_title("Energy inputs to unconventional oil production by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Inputs to unconventional oil production calculated by multiplying production data by IO coef") %>%
      add_legacy_name("L121.in_EJ_R_unoil_F_Yh") %>%
      add_precursors("temp-data-inject/L111.Prod_EJ_R_F_Yh", "energy/A21.globaltech_coef", "energy/calibrated_techs") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L121.in_EJ_R_unoil_F_Yh
    L121.in_EJ_R_TPES_crude_Yh %>%
      add_title("Crude oil total primary energy supply by GCAM region / historical year") %>%
      add_units("EJ") %>%
      add_comments("Unconventional oil subtracted from total primary energy supply of liquids") %>%
      add_comments("to determine crude oil supply") %>%
      add_legacy_name("L121.in_EJ_R_TPES_crude_Yh") %>%
      add_precursors("temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L121.in_EJ_R_TPES_crude_Yh
    L121.in_EJ_R_TPES_unoil_Yh %>%
      add_title("Unconventional oil total primary energy supply by GCAM region / historical year") %>%
      add_units("EJ") %>%
      add_comments("Unconventional oil production shared out to GCAM regions") %>%
      add_legacy_name("L121.in_EJ_R_TPES_unoil_Yh") %>%
      add_precursors("temp-data-inject/L111.Prod_EJ_R_F_Yh", "energy/A21.unoil_demandshares", "L100.IEA_en_bal_ctry_hist", "common/iso_GCAM_regID", "energy/mappings/IEA_product_rsrc") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L121.in_EJ_R_TPES_unoil_Yh

    return_data(L121.in_EJ_R_unoil_F_Yh, L121.in_EJ_R_TPES_crude_Yh, L121.in_EJ_R_TPES_unoil_Yh)
  } else {
    stop("Unknown command")
  }
}
