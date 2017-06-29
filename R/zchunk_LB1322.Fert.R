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
#' @author AJS July 2017
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


    # Silence package notes



    # Compute fertilizer production and energy inputs by technology
    # Disaggregating fertilizer production by country / year to production technologies (gas, coal, oil)

    # First, isolate fuel share data
    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>% # Convert to long form
      filter(grepl("share", variable)) %>% # Filter for only the share data
      mutate(fuel = sub("share_", "", variable)) %>% # Create a dedicated fuel column
      mutate(fuel = sub("oil", "refined liquids", fuel)) %>% # Rename oil to refined liquids
      #mutate(fuel_region = paste(fuel, IEA_Fert_reg)) ->
      rename(value_share = value) ->
      L1322.IEA_fert_fuel_shares # looks to match


    # Isolate intensities (energy per unit mass)
    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>%
      filter(grepl("GJtNH3", variable)) %>% # Filter for only GJtNH3
      mutate(fuel = sub("_GJtNH3", "", variable)) %>%
      mutate(fuel = sub("oil", "refined liquids", fuel)) %>% # Reset oil to refined liquids
      mutate(intensity_GJkgN = value / CONV_T_KG * (17/14)) %>%  # CONV_NH3_N  14 / 17 mass of nitrogen in ammonia...order right, mass in denominator
      select(-value, -variable) ->
      L1322.IEA_fert_fuel_intensities # looks to match

    # Repeat fertilizer production table by number of fuels and match in the IEA region, fuel share, and intensity for each country
    # Note to self, don't see intensities rolled in yet
    # IEA_ctry occasionally has muliple country names for a single iso. Dropping IEA_ctry (country name) column.
    IEA_ctry %>%
      select(iso, IEA_Fert_reg) %>%
      unique() ->
      IEA_ctry_2

    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      repeat_add_columns(tibble::tibble(fuel = c("coal", "gas", "refined liquids"))) %>%
      inner_join(IEA_ctry_2, by = "iso") %>% # IEA_ctry has 202 iso, while L142.ag_Fert has 108
      #mutate(fuel_region = paste(iso, IEA_Fert_reg)) %>%
      left_join_error_no_match(L1322.IEA_fert_fuel_shares, by = c("fuel", "IEA_Fert_reg")) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt

    # Switch individual countries' fuel shares according to literature or to make energy balances work at the regional level.
    # North Korea: coal in all years (http://www9.ocn.ne.jp/~aslan/dprkeng0409.pdf)
    L1322.Fert_Prod_MtN_ctry_F_Y.melt %>%
      mutate(value_share = replace(value_share, (iso == "prk") & (fuel == "coal"), 1)) %>%
      mutate(value_share = replace(value_share, (iso == "prk") & (fuel != "coal"), 0)) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_2 # looks okay except number of rows (1290 vs 1260)

    # Multiply total production by fuel-specific shares to calculate fertilizer production by fuel type
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_2 %>%
      mutate(Fert_Prod_MtN = value * value_share) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_3 # looks okay except number of rows

    # Calculating energy inputs to fertilizer production by country and fuel type
    # Match in the energy intensity by fuel type and IEA fertilizer region
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_3 %>%
      left_join_error_no_match(L1322.IEA_fert_fuel_intensities, by = c("IEA_Fert_reg", "fuel")) %>%
      mutate(in_Fert = Fert_Prod_MtN * intensity_GJkgN) %>%
      arrange(year, iso) -> # DELETE AT END
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_4 # No Taiwan (30) in old table either

    # Checking total energy inputs to fertilizer against industrial energy use for each region
    # First, aggregate by region
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_4 %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(in_Fert = sum(in_Fert)) %>% # This is 3720 rows, old data has 3840 rows (think this is fine)
      ungroup () ->
      L1322.Fert_ALL_MtN_R_F_Y # Missing region 30 (Taiwan). 30*3*40=360...Taiwan is introduced next step

    # Expand this table to all regions x fuels x years in case there are regions that aren't countries in the aglu databases
    list_fuels <- unique(L1322.Fert_ALL_MtN_R_F_Y$fuel)
    list_GCAM_region_ID <- unique(iso_GCAM_regID$GCAM_region_ID)

    # Revisit about also using complete here
    L1322.Fert_ALL_MtN_R_F_Y %>%
      complete(GCAM_region_ID = list_GCAM_region_ID,
               fuel = list_fuels,
               year = HISTORICAL_YEARS,
               fill = list(in_Fert = 0)) ->
      L1322.Fert_ALL_MtN_R_F_Y_2 # Same number of rows, sum is same too (should be 143.6132)

    # Melt other energy tables (ind energy, ind feedstocks) and match in values
    L1322.Fert_ALL_MtN_R_F_Y_2 %>%
      left_join_error_no_match(L1321.in_EJ_R_indenergy_F_Yh, by = c("GCAM_region_ID", "year", "fuel")) %>%
      left_join_error_no_match(L132.in_EJ_R_indfeed_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      select(GCAM_region_ID, fuel, year, in_Fert, in_indenergy = value.x, in_indfeed = value.y) ->
      L1322.Fert_ALL_MtN_R_F_Y_3 # sums of inenergy and indfeed match!

    # Check whether the remaining available industrial energy use is not negative (ind. energy + ind. feedstocks - fert)
    L1322.Fert_ALL_MtN_R_F_Y_3 %>%
      mutate(check = in_indenergy + in_indfeed - in_Fert) ->
      L1322.Fert_ALL_MtN_R_F_Y_4 # off by about 20 (has to be because of fert)

    # Modifying fertilizer production shares so that industrial energy/feedstock use does not go negative for any fuels
    # Modify the fuel shares in the country-level fertilizer production tables, and re-compute fertilizer production quantities
    L1322.Fert_ALL_MtN_R_F_Y_4 %>%
      filter(check < 0) %>%
      mutate(mult = (1 - (-1 * check / in_Fert ))) ->
      L1322.Fert_modify # still off, because of fert. when fert matches, it matches

    L1322.Fert_modify %>%
      select(GCAM_region_ID, fuel, year, mult) ->
      L1322.Fert_modify

    # For matched region/fuel/year, multiply the original shares by the multipliers from above
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_4 %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # left_join_error_no_match cannot be used because there are some rows in the base table left unmatched. NAs will be introduced.
      left_join(L1322.Fert_modify, by = c("GCAM_region_ID", "fuel", "year")) %>%
      replace_na(list(mult = 1)) %>%
      mutate(value_share_adj = value_share * mult) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_5 # sum of mult matches, but not value_share_adj, but should be corrected in next step

    # For regions whose fuel shares changed, re-assign the production and energy inputs to the oil-based technology
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_5 %>%
      #select(-variable) %>%
      select(iso, year, fuel, value_share_adj) %>%
      spread(fuel, value_share_adj) %>%
      mutate(`refined liquids` = 1 - coal - gas) %>%
      gather(fuel, value_share_adj, -iso, -year) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_6 # value_share_adj now matches

    # Re-compute fertilizer production and energy requirements using these new shares
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_5 %>%
      select(-value_share_adj) %>%
      # had to drop some important columns when spreading. Need to rejoin them.
      left_join_error_no_match(L1322.Fert_Prod_MtN_ctry_F_Y.melt_6, by = c("iso", "year", "fuel")) %>%
      mutate(Fert_Prod_MtN_adj = value * value_share_adj) %>%
      mutate(in_Fert_adj = Fert_Prod_MtN_adj * intensity_GJkgN) ->
      L1322.Fert_Prod_MtN_ctry_F_Y.melt_7


    # Re-checking total energy inputs to fertilizer against industrial energy use for each region
    L1322.Fert_Prod_MtN_ctry_F_Y.melt_7 %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(Fert_Prod_MtN_adj = sum(Fert_Prod_MtN_adj),
                in_Fert_adj = sum(in_Fert_adj),
                #in_indenergy = sum(in_indenergy),
                intensity_GJkgN = sum(intensity_GJkgN)) %>%
      ungroup() ->
      #value_feedstock_GJkgN = sum(value_feedstock_GJkgN),
      #value_energy_GJkgN = sum(value_energy_GJkgN))
      L1322.Fert_ALL_MtN_R_F_Y_adj # dropped quite a bit of value data. Note sure if two columns need to be linked. Wrong number of rows.


    # Again need to write out to all region x fuel x year combinations
    L1322.Fert_ALL_MtN_R_F_Y_adj %>%
      complete(GCAM_region_ID = list_GCAM_region_ID,
               fuel = list_fuels,
               year = HISTORICAL_YEARS,
               fill = list(in_Fert_adj = 0)) %>%
      replace_na(list(Fert_Prod_MtN_adj = 0)) -> # can probably nestle this above
      L1322.Fert_ALL_MtN_R_F_Y_adj_2



    L1322.Fert_ALL_MtN_R_F_Y_adj_2 %>%
      left_join_error_no_match(L1322.Fert_ALL_MtN_R_F_Y, by = c("iso", "fuel", "year")) -> # only need in_indenergy and in_indfeed
      L1322.Fert_ALL_MtN_R_F_Y_adj_3

    L1322.Fert_ALL_MtN_R_F_Y_adj_3 %>%
      mutate(check = in_indenergy + in_indfeed - in_Fert_adj) %>%
      mutate(check = replace(check, abs(check) < 1e-6, 0)) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj_4


    # Disaggregating input energy to fertilizer production into energy use (combustion) and feedstocks

    # The "feedstock" requirement is equal to the energy content of the hydrogen in NH3
    # see http://www.iea.org/papers/2009/chemical_petrochemical_sector.pdf
    NH3_energy_GJtNH3 <- H_energy_GJtH2 * NH3_H_frac

    # Convert from GJ per t NH3 to GJ per kg N
    NH3_energy_GJkgN <- NH3_energy_GJtNH3  / conv_t_kg / conv_NH3_N

    L1322.Fert_ALL_MtN_R_F_Y_adj_4 %>%
      mutate(intensity_GJkgN = in_Fert_adj / Fert_Prod_MtN_adj) %>%
      replace_na(list(intensity_GJkgN = 0)) %>%
      left_join_error_no_match(NH3_energy_GJkgN) %>%
      mutate(energy_GJkgN = intensity_GJkgN - NH3_energy_GJkgN) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj_5

    # Calculate first-order estimate of energy and feedstock quantities in the industrial sector
    L1322.Fert_ALL_MtN_R_F_Y_adj_5 %>%
      mutate(indfeed_to_Fert = Fert_Prod_MtN_adj * feedstock_GJkgN,
             indenergy_to_Fert = Fert_Prod_MtN_adj * energy_GJkgN) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj_6

    # Calculate remaining industrial energy and feedstock consumption re-allocating to avoid negative values
    L1322.Fert_ALL_MtN_R_F_Y_adj_6 %>%
      mutate(in_indenergy_netFert = in_indenergy - indenergy_to_Fert,
             in_indfeed_netFert = in_indfeed - indfeed_to_Fert) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj_7

    # This is complicated. This last step pertains to the available energy and feedstock quantities for the industrial sector, in regions
    # where either went negative as a result of including the fertilizer industry (using the IEA's shares of production by fuel type,
    # modified so that no regions went negative, and following the IEA's conventions on disaggregation of feedstocks and energy-use.
    # We are assuming that for the purposes of re-allocation of energy from the (general) industrial sector to the fertilizer sector, all energy
    # is available, whether initially classified as feedstocks or energy-use. This may zero out both energy and feedstocks or both in the
    # general industrial sector, causing all industrial demands of natural gas to be for the ammonia industry.
    # Note that the quantities are added because one is negative.
    # Note sure I got this right
    L1322.Fert_ALL_MtN_R_F_Y_adj_7 %>%
      #filter(in_indenergy_netFert < 0) %>%
      mutate(in_indenergy_netFert = replace(in_indenergy_netFert, in_indfeed_netFert < 0 & in_indenergy_netFert < 0,
                                            in_indenergy_netFert + in_indfeed_netFert)) %>%
      mutate(in_indfeed_netFert = replace(in_indfeed_netFert, in_indfeed_netFert < 0, 0)) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj_8





    L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] +
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ]
    L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <- 0

    L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] +
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ]
    L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <- 0

    printlog( "Building tables of fertilizer production by technology, IO coefs, and energy/feedstock inputs to rest of industry")
    L1322.Fert_ALL_MtN_R_F_Y_adj$sector <- Fert_name
    L1322.Fert_Prod_MtN_R_F_Y <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value.var = "Fert_Prod_MtN_adj" )
    L1322.IO_R_Fert_F_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value.var = "intensity_GJkgN" )
    L1322.in_EJ_R_indenergy_Ffert_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID  + fuel ~ Xyear, value.var = "in_indenergy_netFert" )
    L1322.in_EJ_R_indfeed_Ffert_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + fuel ~ Xyear, value.var = "in_indfeed_netFert" )

    L1322.in_EJ_R_indenergy_F_Yh <- L1321.in_EJ_R_indenergy_F_Yh












    # ===================================================

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
