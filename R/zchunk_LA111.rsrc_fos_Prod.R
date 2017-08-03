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
      summarise(value = sum(value)) ->
      L111.TPES_EJ_R_F_Yh

    # Determine regional shares of production for each primary fuel
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) ->
      L111.Prod_EJ_R_F_Yh_IEA

    L111.Prod_EJ_R_F_Yh_IEA %>%
      group_by(sector, fuel, year) %>%
      summarise(share = value / sum(value)) ->
      L111.Prod_share_R_F_Yh

    # Multiply through to calculate production by fuel

    left_join_error_no_match(L111.Prod_share_R_F_Yh, by = "fuel")

    L111.Prod_EJ_R_F_Yh_IEA %>%

      L111.Prod_EJ_R_F_Yh_IEA_adj <- L111.Prod_EJ_R_F_Yh_IEA
    L111.Prod_EJ_R_F_Yh_IEA_adj[ X_historical_years ] <- L111.Prod_share_R_F_Yh[ X_historical_years ] *
      L111.TPES_EJ_F_Yh[ match( L111.Prod_EJ_R_F_Yh_IEA_adj$fuel, L111.TPES_EJ_F_Yh$fuel ),
                         X_historical_years ]


    # Determine unconventional oil production (58-67)
    # Interpolate production to all historical years
    rsrc_unconv_oil_prod_bbld %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      complete(year = HISTORICAL_YEARS) %>%
      arrange_by(iso, year) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # summarise by GCAM region and convert to EJ/yr
      group_by(GCAM_region_ID) %>%
      summarise(value = sum(value) * CONV_BBLD_EJYR) %>%
      mutate(sector = "unconventional oil production",
             fuel = "unconventional oil") ->
      L111.Prod_EJ_ctry_unconvOil_Yh

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.Prod_EJ_R_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.Prod_EJ_R_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.RsrcCurves_EJ_R_Ffos") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.RsrcCurves_EJ_R_Ffos

    return_data(L111.Prod_EJ_R_F_Yh, L111.RsrcCurves_EJ_R_Ffos)
  } else {
    stop("Unknown command")
  }
}
