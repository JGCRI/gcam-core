#' module_emissions_L102.ceds_nonco2_R_S_T_Y
#'
#' Calculates emissions and emissions factors using EPA emissions factors and scales to EDGAR emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.cedsghg_tg_R_en_S_F_Yh}, \code{L112.cedsghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L112.ghg_en_R_S_T_Y.R} (emissions level1).
#' @details Calculates emissions using EPA emissions factors and energy data. Then scales to EDGAR emissions and calculates emissions factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CWR Apr. 2019

#
#
# module_emissions_L102.ceds_nonco2_R_S_T_Y <- function(command, ...) {
#   if(driver.EMISSIONS_SOURCE == "EDGAR") {
#     if(command == driver.DECLARE_INPUTS) {
#       return(NULL)
#     } else if(command == driver.DECLARE_OUTPUTS) {
#       return(NULL)
#     } else if(command == driver.MAKE) {
#       return_data()
#     } else {
#       stop("Unknown command")
#     }}
#   else {
#     if(command == driver.DECLARE_INPUTS) {
#       return(c(FILE = "common/GCAM_region_names",
#                FILE = "common/iso_GCAM_regID",
#                FILE = "emissions/CEDS/CH4_total_CEDS_emissions",
#                FILE = "emissions/mappings/GCAM_sector_tech_CEDS",
#                FILE = "energy/mappings/UCD_techs",
#                FILE = "energy/calibrated_techs",
#                FILE = "energy/calibrated_techs_bld_det",
#                FILE = "emissions/mappings/Trn_subsector",
#                FILE = "emissions/CEDS/CEDS_sector_tech",
#                FILE = "emissions/EPA_FCCC_IndProc_2005",
#                FILE = "emissions/mappings/EPA_ghg_tech",
#                FILE = "emissions/CEDS/GFED-CMIP6_LUC_emissions.csv",
#                FILE = "emissions/CEDS/LULUC_to_sector_Mapping.csv",
#                "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
#                "L142.ag_Fert_IO_R_C_Y_GLU",
#                "L101.in_EJ_R_en_Si_F_Yh",
#                "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
#                "L101.ag_Prod_Mt_R_C_Y_GLU",
#                "L111.ag_resbio_R_C",
#                "L103.ghg_tgmt_USA_an_Sepa_F_2005",
#                # FILE = "emissions/CEDS/BC_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/OC_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/CO_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/NH3_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/NMVOC_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/NOx_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/SO2_total_CEDS_emissions",
#                # FILE = "emissions/CEDS/CEDS_BC_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_BC_NC_EF",
#                FILE = "emissions/CEDS/CEDS_CH4_comb_EF",
#                FILE = "emissions/CEDS/CEDS_CH4_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_CO_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_CO_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_NH3_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_NH3_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_NMVOC_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_NMVOC_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_NOx_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_NOx_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_OC_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_OC_NC_EF",
#                # FILE = "emissions/CEDS/CEDS_SO2_comb_EF",
#                # FILE = "emissions/CEDS/CEDS_SO2_NC_EF",
#                FILE = "emissions/CEDS/ceds_sector_map",
#                FILE = "emissions/CEDS/ceds_fuel_map"))
#     } else if(command == driver.DECLARE_OUTPUTS) {
#       return(c("L112.cedsghg_tg_R_en_S_F_Yh",
#                "L112.cedsghg_tgej_R_en_S_F_Yh"))
#     } else if(command == driver.MAKE) {
#
#       all_data <- list(...)[[1]]
#
#
#
#
#
#
#
#
#       # ===============
#       # Produce outputs
#       L112.ceds_ghg_tg_R_en_S_F_Yh %>%
#         add_title("GHG emissions by energy sector, gas, region, and historical year") %>%
#         add_units("Tg") %>%
#         add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
#         add_legacy_name("L112.ghg_tg_R_en_S_F_Yh") %>%
#         add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "emissions/mappings/EPA_ghg_tech",
#                        "emissions/mappings/GCAM_sector_tech_CEDS", "L101.in_EJ_R_en_Si_F_Yh", "L103.ag_Prod_Mt_R_C_Y_GLU",
#                        "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "emissions/CEDS/CEDS_sector_tech",
#                        "L102.ghg_tgej_USA_en_Sepa_F_2005", "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O") ->
#         L112.ceds_ghg_tg_R_en_S_F_Yh
#
#       L112.ceds_ghg_tgej_R_en_S_F_Yh %>%
#         add_title("GHG emissions factors by energy sector, gas, region, and historical year") %>%
#         add_units("Tg/EJ") %>%
#         add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
#         add_comments("Then, emissions factors computed by dividing calculated emissions by energy data") %>%
#         add_legacy_name("L112.ghg_tgej_R_en_S_F_Yh") %>%
#         add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "emissions/mappings/EPA_ghg_tech",
#                        "emissions/mappings/GCAM_sector_tech_CEDS", "L101.in_EJ_R_en_Si_F_Yh", "emissions/CEDS/CEDS_sector_tech",
#                        "L102.ghg_tgej_USA_en_Sepa_F_2005", "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O") ->
#         L112.ceds_ghg_tgej_R_en_S_F_Yh
#
#       L131.nonco2_tg_R_prc_S_S_Yh %>%
#         add_title("GHG emissions by GCAM region / sector / technology / historical year") %>%
#         add_units("Tg") %>%
#         add_comments("Calculate historical emissions from the processing sector by GCAM, ") %>%
#         add_comments("technology computed from EDGAR emissions data and EPA emissions factors.") %>%
#         add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
#         add_precursors("common/GCAM_region_names",
#                        "common/iso_GCAM_regID",
#                        "emissions/EDGAR/EDGAR_sector",
#                        "emissions/mappings/EPA_ghg_tech",
#                        "emissions/mappings/GCAM_sector_tech",
#                        "EDGAR_gases",
#                        "emissions/EPA_FCCC_IndProc_2005")
#       return_data(L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh)
#     } else {
#       stop("Unknown command")
#     }
#   }
# }
