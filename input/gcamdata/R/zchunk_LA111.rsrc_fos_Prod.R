# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA111.rsrc_fos_Prod
#'
#' Calculate historical fossil energy production and fossil resource supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.Prod_EJ_R_F_Yh}, \code{L111.RsrcCurves_EJ_R_Ffos}. The corresponding file in the
#' original data system was \code{LA111.rsrc_fos_Prod.R} (energy level1).
#' @details For historical fossil energy production, determine regional shares of production for each primary fuel,
#' interpolate unconventional oil production to all historical years, deduct unconventional oil from total oil,
#' and include it in calibrated production table.
#' For fossil resource supply curves, downscale GCAM3.0 supply curves to the country level (on the basis of
#' resource production) and aggregate by the new GCAM regions, using crude oil production shares as a proxy
#' for unconventional oil resources.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author BBL August 2017
module_energy_LA111.rsrc_fos_Prod <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_product_rsrc",
             FILE = "energy/rsrc_unconv_oil_prod_bbld",
             FILE = "energy/A11.fos_curves",
             "L100.IEA_en_bal_ctry_hist",
             "L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh",
             "L111.RsrcCurves_EJ_R_Ffos"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- year <- value <- share <- iso <- GCAM_region_ID <- unconventionals <- value.x <-
      value.y <- FLOW <- PRODUCT <- resource <- region_GCAM3 <- CumulSum <- subresource <- grade <-
      available <- available_region_GCAM3 <- extractioncost <- technology <-  . <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_product_rsrc <- get_data(all_data, "energy/mappings/IEA_product_rsrc")
    rsrc_unconv_oil_prod_bbld <- get_data(all_data, "energy/rsrc_unconv_oil_prod_bbld")
    A11.fos_curves <- get_data(all_data, "energy/A11.fos_curves")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)

    # ------- HISTORICAL FOSSIL ENERGY PRODUCTION

    # (lines 38-56 in original file)
    # NOTE: Regional production is derived for each fuel as global TPES times regional share of global production
    # Determine global total primary energy supply (TPES) for each fuel
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L111.TPES_EJ_F_Yh

    # Determine regional shares of production for each primary fuel
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) ->
      L111.Prod_EJ_R_F_Yh_IEA

    L111.Prod_EJ_R_F_Yh_IEA %>%
      group_by(sector, fuel, year) %>%
      mutate(share = value / sum(value)) %>%
      select(-value) ->
      L111.Prod_share_R_F_Yh

    # Multiply through to calculate production by fuel
    L111.Prod_EJ_R_F_Yh_IEA %>%
      select(-value) %>%
      left_join_error_no_match(select(L111.TPES_EJ_F_Yh, fuel, year, value), by = c("fuel", "year")) %>%
      left_join_error_no_match(L111.Prod_share_R_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value * share) %>%
      select(-share) ->
      L111.Prod_EJ_R_F_Yh_IEA_adj

    # Determine unconventional oil production (58-67)
    rsrc_unconv_oil_prod_bbld %>%
      gather_years %>%
      # interpolate production to all historical years
      complete(iso, year = HISTORICAL_YEARS) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      arrange(iso, year) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # summarise by GCAM region and convert to EJ/yr
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value) * CONV_MBLD_EJYR) %>%
      # make unconventional oil a technology within crude oil
      mutate(sector = "out_resources",
             fuel = "crude oil",
             technology = "unconventional oil") ->
      L111.Prod_EJ_ctry_unconvOil_Yh

    # Subtract the unconventional oil, append the unconventional oil to the table, and write it out
    # Deduct unconventional oil from total oil and include unconventional oil in calibrated production table
    # Start by making an 'unconventionals' flag to easily split data
    L111.Prod_EJ_R_F_Yh_IEA_adj %>%
      mutate(unconventionals = GCAM_region_ID %in% L111.Prod_EJ_ctry_unconvOil_Yh$GCAM_region_ID &
               fuel == "refined liquids") ->
      L111.Prod_EJ_R_F_Yh

    L111.Prod_EJ_R_F_Yh %>%
      filter(unconventionals) %>%
      left_join_error_no_match(select(L111.Prod_EJ_ctry_unconvOil_Yh, GCAM_region_ID, year, value),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x - value.y) %>%
      bind_rows(filter(L111.Prod_EJ_R_F_Yh, !unconventionals)) %>%
      select(-value.x, -value.y, -unconventionals) %>%
      # switch the names from final to primary
      mutate(fuel = if_else(fuel == "refined liquids", "crude oil", fuel),
             fuel = if_else(fuel == "gas", "natural gas", fuel),
             technology = fuel) %>%
      bind_rows(select(L111.Prod_EJ_ctry_unconvOil_Yh, GCAM_region_ID, sector, fuel, year, value, technology)) -> L111.Prod_EJ_R_F_Yh


    # Produce outputs
    L111.Prod_EJ_R_F_Yh %>%
      add_title("Historical fossil energy production") %>%
      add_units("EJ") %>%
      add_comments("Determine regional shares of production for each primary fuel, ") %>%
      add_comments("interpolate unconventional oil production to all historical years, ") %>%
      add_comments("deduct unconventional oil from total oil and include it in calibrated production table.") %>%
      add_legacy_name("L111.Prod_EJ_R_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/rsrc_unconv_oil_prod_bbld") ->
      L111.Prod_EJ_R_F_Yh

    # ------- RESOURCE PRICES

    # Fossil resource historical prices are currently assumed. No level1 processing is needed.


    # ------- FOSSIL RESOURCE SUPPLY CURVES

    # Using supply curves from GCAM 3.0 (same as MiniCAM) (83-93)
    # These need to be downscaled to the country level (on the basis of resource production) and then
    # aggregated by the new GCAM regions. This requires that all regions have the same price points

    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running
    # without the proprietary IEA data files). If this is the case, we substitute
    # pre-built output datasets and exit.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used saved outputs
      L111.RsrcCurves_EJ_R_Ffos <- prebuilt_data("L111.RsrcCurves_EJ_R_Ffos")
    } else {

      L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW == "INDPROD", PRODUCT %in% IEA_product_rsrc$PRODUCT) %>%
        gather_years %>%
        # bring in resource information and summarise
        left_join_error_no_match(IEA_product_rsrc, by = "PRODUCT") %>%
        group_by(iso, resource) %>%
        summarise(CumulSum = sum(value)) %>%
        # calculate production shares of country within GCAM 3.0 region (95-102)
        left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
        group_by(resource, region_GCAM3) %>%
        mutate(share = CumulSum / sum(CumulSum)) %>%   # share of iso within region
        ungroup %>%
        select(iso, resource, share) ->
        L111.Prod_share_ctry_F_Yh

      # Downscale available resources by GCAM 3.0 region to countries (104-116)
      tidyr::crossing(iso = L111.Prod_share_ctry_F_Yh$iso,
                      subresource = A11.fos_curves$subresource) %>%
        left_join_keep_first_only(select(A11.fos_curves, resource, subresource), by = "subresource") %>%
        repeat_add_columns(tibble(grade = unique(A11.fos_curves$grade))) %>%
        # remove non-existent grades
        filter(paste(subresource, grade) %in% paste(A11.fos_curves$subresource, A11.fos_curves$grade)) %>%
        # match in GCAM3 region, along with available resources
        left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
        left_join_error_no_match(select(A11.fos_curves, region_GCAM3, subresource, grade, available_region_GCAM3 = available),
                                 by = c("region_GCAM3", "subresource", "grade")) %>%
        # match in the share of resource available by each country within GCAM 3.0 region
        # there are NAs here, so use left_join
        left_join(L111.Prod_share_ctry_F_Yh, by = c("iso", "resource")) ->
        L111.RsrcCurves_EJ_ctry_Ffos

      # Use crude oil production shares as a proxy for unconventional oil resources (123-130)
      L111.RsrcCurves_EJ_ctry_Ffos %>%
        filter(subresource == "crude oil") %>%
        select(iso, share) ->
        crude

      L111.RsrcCurves_EJ_ctry_Ffos %>%
        filter(subresource == "unconventional oil") %>%
        select(-share) %>%
        left_join_keep_first_only(crude, by = "iso") %>%
        bind_rows(filter(L111.RsrcCurves_EJ_ctry_Ffos, subresource != "unconventional oil")) %>%
        # set all other missing values to 0 (these are small countries)
        replace_na(list(share = 0)) %>%
        mutate(available = available_region_GCAM3 * share) ->
        L111.RsrcCurves_EJ_ctry_Ffos

      # Aggregate by GCAM regions (132-141)
      L111.RsrcCurves_EJ_ctry_Ffos %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
        group_by(GCAM_region_ID, resource, subresource, grade) %>%
        summarise(available = sum(available)) %>%
        ungroup %>%
        left_join_keep_first_only(select(A11.fos_curves, resource, subresource, grade, extractioncost),
                                  by = c("resource", "subresource", "grade")) ->
        L111.RsrcCurves_EJ_R_Ffos

      L111.RsrcCurves_EJ_R_Ffos %>%
        add_title("Fossil resource supply curves", overwrite = TRUE) %>%
        add_units("available: EJ; extractioncost: 1975$/GJ") %>%
        add_comments("Downscale GCAM3.0 supply curves to the country level (on the basis of resource") %>%
        add_comments("production) and aggregate by the new GCAM regions.") %>%
        add_comments("Use crude oil production shares as a proxy for unconventional oil resources.") %>%
        add_legacy_name("L111.RsrcCurves_EJ_R_Ffos") %>%
        add_precursors("common/iso_GCAM_regID", "energy/A11.fos_curves",
                       "energy/mappings/IEA_product_rsrc", "L100.IEA_en_bal_ctry_hist",
                       "L1012.en_bal_EJ_R_Si_Fi_Yh") ->
        L111.RsrcCurves_EJ_R_Ffos
    }

    return_data(L111.Prod_EJ_R_F_Yh, L111.RsrcCurves_EJ_R_Ffos)
  } else {
    stop("Unknown command")
  }
}
