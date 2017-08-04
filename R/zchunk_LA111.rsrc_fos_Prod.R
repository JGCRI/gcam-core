#' module_energy_LA111.rsrc_fos_Prod
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.Prod_EJ_R_F_Yh}, \code{L111.RsrcCurves_EJ_R_Ffos}. The corresponding file in the
#' original data system was \code{LA111.rsrc_fos_Prod.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL August 2017
#' @export
module_energy_LA111.rsrc_fos_Prod <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/IEA_product_rsrc",
             FILE = "energy/rsrc_unconv_oil_prod_bbld",
             FILE = "energy/A11.fos_curves",
             "L100.IEA_en_bal_ctry_hist",
             "L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh",
             "L111.RsrcCurves_EJ_R_Ffos"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_product_rsrc <- get_data(all_data, "energy/IEA_product_rsrc")
    rsrc_unconv_oil_prod_bbld <- get_data(all_data, "energy/rsrc_unconv_oil_prod_bbld")
    A11.fos_curves <- get_data(all_data, "energy/A11.fos_curves")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")

    # Historical fossil energy production (lines 38-56 in original)
    # NOTE: Regional production is derived for each fuel as global TPES times regional share of global production
    # Determine global total primary energy supply (TPES) for each fuel
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L111.TPES_EJ_F_Yh

    # Determine regional shares of production for each primary fuel
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
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
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      # interpolate production to all historical years
      complete(iso, year = HISTORICAL_YEARS) %>%
      arrange(iso, year) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # summarise by GCAM region and convert to EJ/yr
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value) * CONV_BBLD_EJYR) %>%
      mutate(sector = "unconventional oil production",
             fuel = "unconventional oil") ->
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
             fuel = if_else(fuel == "gas", "natural gas", fuel)) %>%
      bind_rows(select(L111.Prod_EJ_ctry_unconvOil_Yh, GCAM_region_ID, sector, fuel, year, value)) ->
      L111.Prod_EJ_R_F_Yh


    # Produce outputs
    L111.Prod_EJ_R_F_Yh %>%
      add_title("Historical fossil energy production") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.Prod_EJ_R_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/rsrc_unconv_oil_prod_bbld") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.Prod_EJ_R_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.RsrcCurves_EJ_R_Ffos") %>%
      add_precursors("common/iso_GCAM_regID", "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/rsrc_unconv_oil_prod_bbld") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.RsrcCurves_EJ_R_Ffos

    return_data(L111.Prod_EJ_R_F_Yh, L111.RsrcCurves_EJ_R_Ffos)
  } else {
    stop("Unknown command")
  }
}
