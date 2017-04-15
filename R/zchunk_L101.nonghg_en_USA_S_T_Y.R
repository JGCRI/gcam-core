#' module_emissions_L101.nonghg_en_USA_S_T_Y
#'
#' Compute historical emissions factors for energy by GCAM technology, from EPA emissions data and IEA energy balances.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.so2_tgej_USA_en_Sepa_F_Yh}, \code{L101.co_tgej_USA_en_Sepa_F_Yh}, \code{L101.nox_tgej_USA_en_Sepa_F_Yh}, \code{L101.voc_tgej_USA_en_Sepa_F_Yh}, \code{L101.nh3_tgej_USA_en_Sepa_F_Yh}, \code{L101.in_EJ_R_en_Si_F_Yh}. The corresponding file in the
#' original data system was \code{L101.nonghg_en_USA_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L101.nonghg_en_USA_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/EPA_tech",
             FILE = "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
             FILE = "temp-data-inject/L1322.in_EJ_R_indenergy_F_Yh",
             FILE = "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
             FILE = "temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             FILE = "temp-data-inject/L1322.Fert_Prod_MtN_R_F_Y",
             FILE = "temp-data-inject/L1321.in_EJ_R_cement_F_Y",
             FILE = "temp-data-inject/L124.in_EJ_R_heat_F_Yh",
             FILE = "temp-data-inject/L111.Prod_EJ_R_F_Yh",
             FILE = "emissions/EPA_SO2",
             FILE = "emissions/EPA_CO",
             FILE = "emissions/EPA_NOx",
             FILE = "emissions/EPA_VOC",
             FILE = "emissions/EPA_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.so2_tgej_USA_en_Sepa_F_Yh",
             "L101.co_tgej_USA_en_Sepa_F_Yh",
             "L101.nox_tgej_USA_en_Sepa_F_Yh",
             "L101.voc_tgej_USA_en_Sepa_F_Yh",
             "L101.nh3_tgej_USA_en_Sepa_F_Yh",
             "L101.in_EJ_R_en_Si_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    EPA_SO2 <- get_data(all_data, "emissions/EPA_SO2")
    EPA_CO <- get_data(all_data, "emissions/EPA_CO")
    EPA_NOx <- get_data(all_data, "emissions/EPA_NOx")
    EPA_VOC <- get_data(all_data, "emissions/EPA_VOC")
    EPA_NH3 <- get_data(all_data, "emissions/EPA_NH3")

    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "temp-data-inject/L1322.in_EJ_R_indenergy_F_Yh")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "temp-data-inject/L1322.Fert_Prod_MtN_R_F_Y")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "temp-data-inject/L1321.in_EJ_R_cement_F_Y")
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "temp-data-inject/L124.in_EJ_R_heat_F_Yh")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "temp-data-inject/L111.Prod_EJ_R_F_Yh")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.so2_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.so2_tgej_USA_en_Sepa_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.co_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.co_tgej_USA_en_Sepa_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.nox_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.nox_tgej_USA_en_Sepa_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.voc_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.voc_tgej_USA_en_Sepa_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.nh3_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.nh3_tgej_USA_en_Sepa_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.in_EJ_R_en_Si_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.in_EJ_R_en_Si_F_Yh

    return_data(L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh, L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh, L101.nh3_tgej_USA_en_Sepa_F_Yh, L101.in_EJ_R_en_Si_F_Yh)
  } else {
    stop("Unknown command")
  }
}
