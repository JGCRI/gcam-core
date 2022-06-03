# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA121.liquids
#'
#' Process historical oil data and separate into unconventional oil, crude oil, and energy inputs to oil.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L121.in_EJ_R_unoil_F_Yh},
#'   \code{L121.in_EJ_R_TPES_crude_Yh}, \code{L121.in_EJ_R_TPES_unoil_Yh}, \code{L121.share_R_TPES_biofuel_tech}. The
#'   corresponding file in the original data system was \code{LA121.oil.R} (energy level1).
#' @details This chunk uses energy production data to determine the energy inputs to the oil sector. It uses the IEA
#'   energy balance to separate out unconventional oil production and crude oil (total liquids - unconventional oil).
#'   It also uses data from IIASA to downscale ethanol and biodiesel consumption to modeled technologies and feedstocks.
#' @note If the (proprietary) IEA data aren't available, pre-built summaries are used.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter group_by if_else inner_join left_join mutate select summarise
#' @importFrom tidyr complete nesting
#' @author JDH June 2017, ed GPK April 2019
module_energy_LA121.liquids <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/IIASA_biofuel_production",
             FILE = "aglu/IIASA_biofuel_tech_mapping",
             FILE = "aglu/IIASA_biofuel_region_mapping",
             FILE = "aglu/A_OilSeed_SecOut",
             FILE = "energy/calibrated_techs",
             FILE = "energy/mappings/IEA_product_rsrc",
             FILE = "energy/A21.unoil_demandshares",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A21.globalrsrctech_coef",
             "L100.IEA_en_bal_ctry_hist",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L111.Prod_EJ_R_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L121.in_EJ_R_unoil_F_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L121.share_R_TPES_biofuel_tech",
             "L121.BiomassOilRatios_kgGJ_R_C"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    year <- value <- iso <- FLOW <- PRODUCT <- fuel <- sector <-
      share_ctry_RG3 <- value_unoil <- GCAM_region_ID <- calibration <-
      secondary.output <- supplysector <- region_GCAM3 <- value_RG3 <-
      share <- share_RG3_world <- subsector <- technology <- minicam.energy.input <-
      value_coef <- fuel.y <- value_coef_gas <- resource <- Production_ML <-
      Biofuel <- GCAM_commodity <- SecOutRatio <- IOcoef <- Weighted_IOcoef <-
      Weighted_SecOutRatio <- Weight <- region <- gas_coef <- val_unoil <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    IIASA_biofuel_production <- get_data(all_data, "aglu/IIASA_biofuel_production", strip_attributes = TRUE)
    IIASA_biofuel_tech_mapping <- get_data(all_data, "aglu/IIASA_biofuel_tech_mapping", strip_attributes = TRUE)
    IIASA_biofuel_region_mapping <- get_data(all_data, "aglu/IIASA_biofuel_region_mapping", strip_attributes = TRUE)
    A_OilSeed_SecOut <- get_data(all_data, "aglu/A_OilSeed_SecOut", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    IEA_product_rsrc <- get_data(all_data, "energy/mappings/IEA_product_rsrc", strip_attributes = TRUE)
    A21.unoil_demandshares <- get_data(all_data, "energy/A21.unoil_demandshares", strip_attributes = TRUE)
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef", strip_attributes = TRUE)
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    A21.globalrsrctech_coef <- get_data(all_data, "energy/A21.globalrsrctech_coef", strip_attributes = TRUE) %>%
      filter(minicam.energy.input == "regional natural gas") %>%
      gather_years(value_col = "gas_coef") %>%
      repeat_add_columns(tibble(region = c(iso_GCAM_regID$GCAM_region_ID)))

    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running
    # without the proprietary IEA data files). If this is the case, we substitute
    # pre-built output datasets and exit.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used prebuilt outputs
      L121.in_EJ_R_unoil_F_Yh <- extract_prebuilt_data("L121.in_EJ_R_unoil_F_Yh")
      L121.in_EJ_R_TPES_crude_Yh <- extract_prebuilt_data("L121.in_EJ_R_TPES_crude_Yh")
      L121.in_EJ_R_TPES_unoil_Yh <- extract_prebuilt_data("L121.in_EJ_R_TPES_unoil_Yh")
      L121.share_R_TPES_biofuel_tech <- extract_prebuilt_data("L121.share_R_TPES_biofuel_tech")
      L121.BiomassOilRatios_kgGJ_R_C <- extract_prebuilt_data("L121.BiomassOilRatios_kgGJ_R_C")
    } else {

      L100.IEA_en_bal_ctry_hist %>%
        gather_years -> L100.IEA_en_bal_ctry_hist

      L111.Prod_EJ_R_F_Yh <- L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh", strip_attributes = TRUE)

      # ===================================================

      A21.globalrsrctech_coef %>%
        select(region, year, minicam.energy.input, gas_coef) %>%
        rename(GCAM_region_ID=region, fuel = minicam.energy.input) %>%
        mutate(fuel="gas") %>%
        distinct() -> gas_uncov_ratio


      # Calculating energy inputs (gas) to unconventional oil production in the historical years
      A21.globaltech_coef %>%
        left_join(calibrated_techs, by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
        filter(!is.na(sector)) %>%
        select(-sector, -fuel, -calibration, -secondary.output) ->
        L121.globaltech_coef

      L121.globaltech_coef %>%
        gather_years %>%
        # Adding empty historical years to fill in with interpolation
        complete(year = unique(c(HISTORICAL_YEARS, year)),
                 nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
        arrange(year) %>%
        group_by(technology, subsector, supplysector, minicam.energy.input) %>%
        # Interpolate to fill in missing globaltech_coef historical years
        mutate(value = approx_fun(year, value)) %>%
        left_join(distinct(calibrated_techs), by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
        select(supplysector, subsector, technology, minicam.energy.input, year, value, sector, fuel) ->
        L121.globaltech_coef_interp



      # Downscaling unconventional oil consumption shares by GCAM 3.0 region to countries
      product_filters <- filter(IEA_product_rsrc, resource == "crude oil")
      product_filters <- unique(product_filters$PRODUCT)

      L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW == "TPES", PRODUCT %in% product_filters,
               year == max(HISTORICAL_YEARS), !is.na(value)) %>%
        group_by(iso) %>%
        summarise(value = sum(value)) %>%
        left_join_error_no_match(select(iso_GCAM_regID, region_GCAM3, iso), by = "iso") ->
        L121.TPES_ktoe_ctry_oil_Yf

      L121.TPES_ktoe_ctry_oil_Yf %>%
        group_by(region_GCAM3) %>%
        summarise(value = sum(value)) ->
        L121.TPES_ktoe_RG3_oil_Yf

      # Calculate the shares of country-within-GCAM3-region, match in shares of GCAM3-region-within-world, and multiply
      L121.TPES_ktoe_ctry_oil_Yf %>%
        left_join(rename(L121.TPES_ktoe_RG3_oil_Yf, value_RG3 = value), by = "region_GCAM3") %>%
        mutate(share_ctry_RG3 = value / value_RG3) %>%
        left_join(A21.unoil_demandshares, by = "region_GCAM3") %>%
        mutate(share_RG3_world = share,
               share = share_ctry_RG3 * share_RG3_world) %>%
        select(-value_RG3) %>%
        left_join(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") ->
        L121.TPES_ktoe_ctry_oil_Yf

      # Aggregating country shares of unconventional oil demand to GCAM regions
      L121.TPES_ktoe_ctry_oil_Yf %>%
        group_by(GCAM_region_ID) %>%
        summarise(share = sum(share)) %>%
        select(GCAM_region_ID, share) -> L121.share_R_TPES_unoil_Yf

      # Calculating unconventional oil demand by region and historical year
      L111.Prod_EJ_R_F_Yh %>%
        filter(technology == "unconventional oil") %>%
        group_by(fuel, year, sector) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        select(fuel, year, value) %>%
        distinct() %>%
        mutate(fuel = paste0("unconventional oil"))-> L121.Prod_EJ_unoil_Yh

      L121.share_R_TPES_unoil_Yf %>%
        repeat_add_columns(L121.Prod_EJ_unoil_Yh) %>%
        mutate(value = share * value) %>%
        select(GCAM_region_ID, fuel, year, value) %>%
        mutate(sector = "TPES") -> L121.in_EJ_R_TPES_unoil_Yh

      L111.Prod_EJ_R_F_Yh %>%
        filter(technology=="unconventional oil") -> unoil_prod



      L121.in_EJ_R_TPES_unoil_Yh %>% filter(value > 0) -> L121.in_EJ_R_TPES_unoil_Yh_temp
      # Conventional (crude) oil: calculate as liquids TPES - unconventional oil
      L1012.en_bal_EJ_R_Si_Fi_Yh %>%
        filter(sector == "TPES", fuel == "refined liquids") -> L121.in_EJ_R_TPES_liq_Yh

      L121.in_EJ_R_TPES_liq_Yh %>%
        select(GCAM_region_ID, sector, fuel, year, value) %>%
        mutate(fuel = "crude oil") %>%
        left_join(rename(L121.in_EJ_R_TPES_unoil_Yh, value_unoil = value, unoil = fuel), by = c("GCAM_region_ID", "sector", "year")) %>%
        mutate(value_unoil = if_else(is.na(value_unoil), 0, value_unoil),
               value = value - value_unoil) %>%
        select(GCAM_region_ID, sector, fuel, year, value) -> L121.in_EJ_R_TPES_crude_Yh

      L111.Prod_EJ_R_F_Yh %>%
        filter(fuel=="natural gas") %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        mutate(fuel=paste0("gas")) %>%
        left_join(gas_uncov_ratio,by=c("GCAM_region_ID","year","fuel")) %>%
        mutate(value =if_else(is.na(gas_coef),0,value*gas_coef)) %>%
        inner_join(unoil_prod %>% select(GCAM_region_ID, year, val_unoil =value),by=c("GCAM_region_ID","year"))%>%
        mutate(value=val_unoil*gas_coef) %>%
        select(GCAM_region_ID, fuel, year, value)->  L121.in_EJ_R_unoil_F_Yh



      # 4/23/2019 addendum - GPK.
      # Downscale biofuel consumption to specific technologies, per data from IIASA
      L121.share_ctry_biofuel_tech <- left_join(IIASA_biofuel_production,
                                             IIASA_biofuel_tech_mapping,
                                             by = c("Biofuel", "Crop")) %>%
        left_join(IIASA_biofuel_region_mapping, by = "Region") %>%
        filter(!is.na(technology),
               Production_ML > 0) %>%
        group_by(iso, Biofuel, technology, GCAM_commodity) %>%
        summarise(Production_ML = sum(Production_ML)) %>%
        ungroup() %>%
        group_by(iso, Biofuel) %>%
        mutate(share = Production_ML / sum(Production_ML)) %>%
        ungroup() %>%
        select(iso, Biofuel, technology, GCAM_commodity, share)

      L121.share_R_TPES_biofuel_tech <- filter(L100.IEA_en_bal_ctry_hist,
                                               iso %in% L121.share_ctry_biofuel_tech$iso,
                                               FLOW == "TPES",
                                               PRODUCT %in% c("Biogasoline", "Biodiesels"),
                                               year == max(HISTORICAL_YEARS),
                                               value > 0) %>%
        mutate(Biofuel = if_else(PRODUCT == "Biogasoline", "ethanol", "biodiesel")) %>%
        inner_join(L121.share_ctry_biofuel_tech, by = c("iso", "Biofuel")) %>%
        mutate(value = value * share) %>%
        left_join_error_no_match(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") %>%
        group_by(GCAM_region_ID, Biofuel, technology, GCAM_commodity) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        group_by(GCAM_region_ID, Biofuel) %>%
        mutate(share = value / sum(value)) %>%
        ungroup() %>%
        select(GCAM_region_ID, Biofuel, technology, GCAM_commodity, share)

      # 02/2020 addition (gpk) - regional average oilcrop -> biomassOil -> secondary output of feed ratios
      # vary considerably according to the feedstock type. Using a soybean-based secondary output coefficient
      # will over-estimate this flow in countries using mostly rapeseed feedstocks, and vice versa. This is
      # addressed by computing region-specific weighted average secondary output coefficients.
      L121.BiomassOilRatios_kgGJ_R_C <- left_join(IIASA_biofuel_production,
                                                       IIASA_biofuel_tech_mapping,
                                                       by = c("Biofuel", "Crop")) %>%
        left_join(IIASA_biofuel_region_mapping, by = "Region") %>%
        left_join(A_OilSeed_SecOut, by = "Crop") %>%
        filter(!is.na(SecOutRatio),
               Production_ML > 0) %>%
        mutate(Weighted_SecOutRatio = Production_ML * SecOutRatio,
               Weighted_IOcoef = Production_ML * IOcoef) %>%
        left_join_error_no_match(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") %>%
        group_by(GCAM_region_ID, GCAM_commodity) %>%
        summarise(Weight = sum(Production_ML),
                  Weighted_IOcoef = sum(Weighted_IOcoef),
                  Weighted_SecOutRatio = sum(Weighted_SecOutRatio)) %>%
        ungroup() %>%
        mutate(IOcoef = Weighted_IOcoef / Weight,
               SecOutRatio = Weighted_SecOutRatio / Weight) %>%
        select(GCAM_region_ID, GCAM_commodity, IOcoef, SecOutRatio)

      # ===================================================
      # Produce outputs
      L121.in_EJ_R_unoil_F_Yh %>%
        add_title("Energy inputs to unconventional oil production by GCAM region / fuel / historical year", overwrite = TRUE) %>%
        add_units("EJ") %>%
        add_comments("Inputs to unconventional oil production calculated by multiplying production data by IO coef") %>%
        add_legacy_name("L121.in_EJ_R_unoil_F_Yh") %>%
        add_precursors("L111.Prod_EJ_R_F_Yh", "energy/A21.globaltech_coef","energy/A21.globalrsrctech_coef",
                       "energy/calibrated_techs") ->
        L121.in_EJ_R_unoil_F_Yh

      L121.in_EJ_R_TPES_crude_Yh %>%
        add_title("Crude oil total primary energy supply by GCAM region / historical year", overwrite = TRUE) %>%
        add_units("EJ") %>%
        add_comments("Unconventional oil subtracted from total primary energy supply of liquids") %>%
        add_comments("to determine crude oil supply") %>%
        add_legacy_name("L121.in_EJ_R_TPES_crude_Yh") %>%
        add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh") ->
        L121.in_EJ_R_TPES_crude_Yh

      L121.in_EJ_R_TPES_unoil_Yh %>%
        add_title("Unconventional oil total primary energy supply by GCAM region / historical year", overwrite = TRUE) %>%
        add_units("EJ") %>%
        add_comments("Unconventional oil production shared out to GCAM regions") %>%
        add_legacy_name("L121.in_EJ_R_TPES_unoil_Yh") %>%
        add_precursors("L111.Prod_EJ_R_F_Yh", "energy/A21.unoil_demandshares",
                       "L100.IEA_en_bal_ctry_hist", "common/iso_GCAM_regID",
                       "energy/mappings/IEA_product_rsrc") ->
        L121.in_EJ_R_TPES_unoil_Yh

      L121.share_R_TPES_biofuel_tech %>%
        add_title("Share of biofuel consumption by region / technology / feedstock", overwrite = TRUE) %>%
        add_units("unitless") %>%
        add_comments("Ethanol and biodiesel consumption assigned to feedstock shares") %>%
        add_precursors("aglu/IIASA_biofuel_production", "aglu/IIASA_biofuel_region_mapping",
                       "aglu/IIASA_biofuel_tech_mapping", "L100.IEA_en_bal_ctry_hist", "common/iso_GCAM_regID") ->
        L121.share_R_TPES_biofuel_tech

      L121.BiomassOilRatios_kgGJ_R_C %>%
        add_title("BiomassOil input-output coefficient (kg crop / GJ oil) and secondary output ratio (kg feedcake / GJ oil) by region / feedstock", overwrite = TRUE) %>%
        add_units("kg / GJ") %>%
        add_comments("Calculated from weighted average OilCrop oil contents and assumptions about losses") %>%
        add_precursors("aglu/IIASA_biofuel_production", "aglu/IIASA_biofuel_region_mapping",
                       "aglu/IIASA_biofuel_tech_mapping", "aglu/A_OilSeed_SecOut", "common/iso_GCAM_regID") ->
        L121.BiomassOilRatios_kgGJ_R_C

      # At this point the objects should be identical to the prebuilt objects
      verify_identical_prebuilt(L121.in_EJ_R_unoil_F_Yh,
                                L121.in_EJ_R_TPES_crude_Yh,
                                L121.in_EJ_R_TPES_unoil_Yh,
                                L121.share_R_TPES_biofuel_tech,
                                L121.BiomassOilRatios_kgGJ_R_C)
    }

    return_data(L121.in_EJ_R_unoil_F_Yh,
                L121.in_EJ_R_TPES_crude_Yh,
                L121.in_EJ_R_TPES_unoil_Yh,
                L121.share_R_TPES_biofuel_tech,
                L121.BiomassOilRatios_kgGJ_R_C)
  } else {
    stop("Unknown command")
  }
}
