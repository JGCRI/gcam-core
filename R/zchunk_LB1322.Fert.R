#' module_energy_LB1322.Fert
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.Fert_Prod_MtN_R_F_Y}, \code{L1322.IO_R_Fert_F_Yh}, \code{L1322.in_EJ_R_indenergy_F_Yh}, \code{L1322.in_EJ_R_indfeed_F_Yh}, \code{L1322.Fert_NEcost_75USDkgN_F}. The corresponding file in the
#' original data system was \code{LB1322.Fert.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS June 2017
#' @export
module_energy_LB1322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/IEA_ctry",
             FILE = "energy/IEA_Fert_fuel_data",
             FILE = "energy/H2A_Prod_Tech",
             FILE = "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y",
             FILE = "energy/A10.rsrc_info",
             FILE = "energy/A21.globaltech_cost",
             FILE = "energy/A22.globaltech_cost",
             FILE = "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh",
             FILE = "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh",
             "L1322.Fert_NEcost_75USDkgN_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_ctry <- get_data(all_data, "energy/IEA_ctry")
    IEA_Fert_fuel_data <- get_data(all_data, "energy/IEA_Fert_fuel_data")
    H2A_Prod_Tech <- get_data(all_data, "energy/H2A_Prod_Tech")

    get_data(all_data, "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y") %>%
      gather(year, value, -iso) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L142.ag_Fert_Prod_MtN_ctry_Y

    A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A22.globaltech_cost <- get_data(all_data, "energy/A22.globaltech_cost")

    get_data(all_data, "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L1321.in_EJ_R_indenergy_F_Yh

    get_data(all_data, "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L132.in_EJ_R_indfeed_F_Yh

    # ===================================================


    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>%
      filter(grepl("share", variable)) %>% # Filter for only shares
      mutate(fuel = sub("share_", "", variable)) %>%
      mutate(fuel = sub("oil", "refined liquids", fuel)) %>% # Reset oil to refined liquids
      #mutate(fuel_region = paste(fuel, IEA_Fert_reg)) ->
      rename(value_share = value) ->
      L1322.IEA_fert_fuel_shares

    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>%
      filter(grepl("GJtNH3", variable)) %>% # Filter for only GJtNH3
      mutate(fuel = sub("_GJtNH3", "", variable)) %>%
      mutate(fuel = sub("oil", "refined liquids", fuel)) %>% # Reset oil to refined liquids
      mutate(value_intensity_GJkgN = value / CONV_T_KG / 14/17) %>%  # CONV_NH3_N  14 / 17 mass of nitrogen in ammonia...need to switch
      select(-value, -variable) ->
      L1322.IEA_fert_fuel_intensities

    # Repeat fertilizer production table by number of fuels and match in the IEA region, fuel share, and intensity for each country
    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      repeat_add_columns(tibble::tibble(fuel = c("coal", "gas", "refined liquids"))) %>%
      left_join(IEA_ctry, by = "iso") %>% # IEA_ctry has 202 iso, while L142.ag_Fert has 108
      #mutate(fuel_region = paste(iso, IEA_Fert_reg)) %>%
      left_join(L1322.IEA_fert_fuel_shares, by = c("fuel", "IEA_Fert_reg")) -> # NA values in new data columns...probably bc of excess iso's
      L1322.Fert_Prod_MtN_ctry_F_Y.melt

    # Switch individual countries' fuel shares according to literature or to make energy balances work at the regional level.
    # North Korea: coal in all years (http://www9.ocn.ne.jp/~aslan/dprkeng0409.pdf)
    L1322.Fert_Prod_MtN_ctry_F_Y.melt %>%
      mutate(value_share = replace(value_share, (iso == "prk") & (fuel == "coal"), 1)) %>%
      mutate(value_share = replace(value_share, (iso == "prk") & (fuel != "coal"), 0)) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_2

    # Multiply total production by fuel-specific shares to calculate fertilizer production by fuel type
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_2 %>%
      mutate(value_Fert_Prod_MtN = value * value_share) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_3

    # printlog( "Calculating energy inputs to fertilizer production by country and fuel type" )
    # Match in the energy intensity by fuel type and IEA fertilizer region
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_3 %>%
      left_join_error_no_match(L1322.IEA_fert_fuel_intensities, by = c("IEA_Fert_reg", "fuel")) %>%
      mutate(value_in_Fert = value_Fert_Prod_MtN * value_intensity_GJkgN) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_4

    # printlog( "Checking total energy inputs to fertilizer against industrial energy use for each region")
    # First, aggregate by region
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_4 %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value_in_Fert = sum(value_in_Fert)) -> # This is 3720 rows, old data has 3840 rows
      L1322.Fert_ALL_MtN_R_F_Y

    # Expand this table to all regions x fuels x years in case there are regions that aren't countries in the aglu databases
    iso_GCAM_regID %>%
      select(GCAM_region_ID) %>%
      unique() %>%
      arrange(GCAM_region_ID) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      repeat_add_columns(tibble::tibble(fuel = unique(L1322.Fert_ALL_MtN_R_F_Y$fuel))) %>% # Will need to chane $
      left_join(L1322.Fert_ALL_MtN_R_F_Y, by = c("GCAM_region_ID", "year", "fuel")) %>%
      replace_na(list(value_in_Fert = 0)) ->
      L1322.Fert_ALL_MtN_R_F_Y_2

    # Melt other energy tables (ind energy, ind feedstocks) and match in values
    L1322.Fert_ALL_MtN_R_F_Y_2 %>%
      left_join_error_no_match(L1321.in_EJ_R_indenergy_F_Yh, by = c("GCAM_region_ID", "year", "fuel")) %>%
      left_join_error_no_match(L132.in_EJ_R_indfeed_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      select(GCAM_region_ID, fuel, year, value_in_Fert, value_indenergy = value.x, value_indfeed = value.y) ->
      L1322.Fert_ALL_MtN_R_F_Y_3

    # Check whether the remaining available industrial energy use is not negative (ind. energy + ind. feedstocks - fert)
    L1322.Fert_ALL_MtN_R_F_Y_3 %>%
      mutate(check = value_indenergy + value_indfeed - value_in_Fert) ->
      L1322.Fert_ALL_MtN_R_F_Y_4

    #printlog( "Modifying fertilizer production shares so that industrial energy/feedstock use does not go negative for any fuels")
    # Modify the fuel shares in the country-level fertilizer production tables, and re-compute fertilizer production quantities
    L1322.Fert_ALL_MtN_R_F_Y_4 %>%
      filter(check < 0) %>%
      mutate(mult = (1 - (-1 * check / value_in_Fert ))) ->
      L1322.Fert_modify

    # For matched region/fuel/year, multiply the original shares by the multipliers from above
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_4 %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      left_join(L1322.Fert_modify, by = c("GCAM_region_ID", "fuel", "year")) %>%
      replace_na(list(mult = 1)) %>%
      mutate(value_share_adj = value_share * mult) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_5


    #For regions whose fuel shares changed, re-assign the production and energy inputs to the oil-based technology
    # L1322.Fert_Prod_MtN_ctry_F_Y.melt_5 %>%
    #   filter(fuel == "refined liquids") %>%
    #   mutate(value_share_adj = 1-)

    #For regions whose fuel shares changed, re-assign the production and energy inputs to the oil-based technology
    L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "refined liquids" ] <-
      1 - L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "coal" ] -
      L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "gas" ]

    # Re-compute fertilizer production and energy requirements using these new shares
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_5 %>%
      mutate()



    #Re-compute fertilizer production and energy requirements using these new shares
    L1322.Fert_Prod_MtN_ctry_F_Y.melt$Fert_Prod_MtN_adj <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, value * share_adj )
    L1322.Fert_Prod_MtN_ctry_F_Y.melt$in_Fert_adj <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, Fert_Prod_MtN_adj * intensity_GJkgN )





    L1322.Fert_ALL_MtN_R_F_Y %>%
      full_join(tibble_GCAM_region_ID, by = "GCAM_region_ID") %>%
      replace_na(0)
    complete(GCAM_region_ID, nesting(fuel, year), fill = list(value_in_Fert = 0)) %>%
      complete(fuel, nesting(GCAM_region_ID, year), fill = list(value_in_Fert = 0))

    list_IEA <- unique(IEA_ctry$iso)
    list_Fert <- unique(L142.ag_Fert_Prod_MtN_ctry_Y$iso)

    # ===================================================

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.Fert_Prod_MtN_R_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/IEA_ctry", "energy/IEA_Fert_fuel_data", "energy/H2A_Prod_Tech",
                     "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y", "energy/A10.rsrc_info", "energy/A21.globaltech_cost", "energy/A22.globaltech_cost",
                     "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh", "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.Fert_Prod_MtN_R_F_Y

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.IO_R_Fert_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/IEA_ctry", "energy/IEA_Fert_fuel_data", "energy/H2A_Prod_Tech",
                     "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y", "energy/A10.rsrc_info", "energy/A21.globaltech_cost", "energy/A22.globaltech_cost",
                     "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh", "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.IO_R_Fert_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/IEA_ctry", "energy/IEA_Fert_fuel_data", "energy/H2A_Prod_Tech",
                     "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y", "energy/A10.rsrc_info", "energy/A21.globaltech_cost", "energy/A22.globaltech_cost",
                     "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh", "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.in_EJ_R_indenergy_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.in_EJ_R_indfeed_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/IEA_ctry", "energy/IEA_Fert_fuel_data", "energy/H2A_Prod_Tech",
                     "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y", "energy/A10.rsrc_info", "energy/A21.globaltech_cost", "energy/A22.globaltech_cost",
                     "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh", "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.in_EJ_R_indfeed_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.Fert_NEcost_75USDkgN_F") %>%
      add_precursors("common/iso_GCAM_regID", "energy/IEA_ctry", "energy/IEA_Fert_fuel_data", "energy/H2A_Prod_Tech",
                     "temp-data-inject/L142.ag_Fert_Prod_MtN_ctry_Y", "energy/A10.rsrc_info", "energy/A21.globaltech_cost", "energy/A22.globaltech_cost",
                     "temp-data-inject/L1321.in_EJ_R_indenergy_F_Yh", "temp-data-inject/L132.in_EJ_R_indfeed_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.Fert_NEcost_75USDkgN_F

    return_data(L1322.Fert_Prod_MtN_R_F_Y, L1322.IO_R_Fert_F_Yh, L1322.in_EJ_R_indenergy_F_Yh, L1322.in_EJ_R_indfeed_F_Yh, L1322.Fert_NEcost_75USDkgN_F)
  } else {
    stop("Unknown command")
  }
}
