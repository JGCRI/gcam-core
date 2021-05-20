# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LB1322.Fert
#'
#' Compute fertilizer production and energy inputs by technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.Fert_Prod_MtN_R_F_Y}, \code{L1322.IO_R_Fert_F_Yh}, \code{L1322.in_EJ_R_indenergy_F_Yh}, \code{L1322.in_EJ_R_indfeed_F_Yh}, \code{L1322.Fert_NEcost_75USDkgN_F}. The corresponding file in the
#' original data system was \code{LB1322.Fert.R} (energy level1).
#' @details Fertilizer production and energy information was calculated using country-level fuel share and energy intensity data
#' to allocate generic fertilizer production data across technologies. Re-allocation of energy use between energy and feedstock
#' quantities for the industrial sector was done to avoid negative values at the country level, before aggregating
#' to the regional level. Also, a final cost table was calculated using USA market fertilizer price data and H2A characteristics
#' of hydrogen production technologies.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate pull select summarise
#' @importFrom tidyr complete fill gather replace_na spread
#' @author AJS July 2017
module_energy_LB1322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_ctry",
             FILE = "energy/IEA_Fert_fuel_data",
             FILE = "energy/H2A_Prod_Tech",
             "L142.ag_Fert_Prod_MtN_ctry_Y",
             FILE = "energy/A10.rsrc_info",
             FILE = "energy/A21.globaltech_cost",
             FILE = "energy/A22.globaltech_cost",
             "L1321.in_EJ_R_indenergy_F_Yh",
             "L132.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh",
             "L1322.Fert_NEcost_75USDkgN_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    year <- value <- iso <- GCAM_region_ID <- sector <- fuel <- variable <- IEA_Fert_reg <- intensity_GJkgN <-
      value_share <- Fert_Prod_MtN <- in_Fert <- value.x <- value.y <- in_indenergy <- in_indfeed <- check <-
      mult <- value_share_adj <- coal <- gas <- Fert_Prod_MtN <- in_Fert_adj <- feedstock_GJkgN <- energy_GJkgN <-
      indenergy_to_Fert <- indfeed_to_Fert <- in_indenergy_netFert <- value_indenergy <- resource <- . <- `2005` <-
      supplysector <- subsector <- technology <- minicam.non.energy.input <- improvement.max <- improvement.rate <-
      improvement.shadow.technology <- NEcost <- Technology <- NEcost_75USDkgN <- `Central Natural Gas Sequestration` <-
      `Central Natural Gas` <- `Central Coal` <- `Central Coal Sequestration` <- Fert_Prod_MtN_adj <-
      in_indfeed_netFert <- Central_Natural_Gas_Sequestration <- Central_Natural_Gas <- Central_Coal <- Central_Coal_Sequestration <- NULL

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_ctry <- get_data(all_data, "energy/mappings/IEA_ctry")
    IEA_Fert_fuel_data <- get_data(all_data, "energy/IEA_Fert_fuel_data")
    H2A_Prod_Tech <- get_data(all_data, "energy/H2A_Prod_Tech")
    L142.ag_Fert_Prod_MtN_ctry_Y <- get_data(all_data, "L142.ag_Fert_Prod_MtN_ctry_Y")
    A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A22.globaltech_cost <- get_data(all_data, "energy/A22.globaltech_cost")
    L1321.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1321.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L132.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L132.in_EJ_R_indfeed_F_Yh")


    # ===================================================

    # Compute fertilizer production and energy inputs by technology
    # Disaggregating fertilizer production by country / year to production technologies (gas, coal, oil)

    # First, extract fuel share data to create a dedicated table for fuel shares
    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>% # Convert to long form
      filter(grepl("share", variable)) %>% # Filter for only the share data
      mutate(fuel = sub("share_", "", variable), # Create a dedicated fuel column
             fuel = sub("oil", "refined liquids", fuel)) %>% # Rename oil to refined liquids
      select(IEA_Fert_reg, fuel, value_share = value) ->
      L1322.IEA_fert_fuel_shares

    # Now, extract intensities (energy per unit mass) to create a dedicated table for fuel intensities
    IEA_Fert_fuel_data %>%
      gather(variable, value, -IEA_Fert_reg) %>%
      filter(grepl("GJtNH3", variable)) %>% # Filter for only GJtNH3 (i.e., energy per unit mass)
      mutate(fuel = sub("_GJtNH3", "", variable),
             fuel = sub("oil", "refined liquids", fuel), # Rename oil to refined liquids
             intensity_GJkgN = value / CONV_T_KG / CONV_NH3_N) %>%  # Convert units to kg of nitrogen instead of ton of ammonia
      select(IEA_Fert_reg, fuel, intensity_GJkgN) ->
      L1322.IEA_fert_fuel_intensities

    # Repeat fertilizer production table by number of fuels and match in the IEA region, fuel share, and intensity for each country
    # Note that IEA_ctry occasionally has muliple country names for a single iso. Dropping IEA_ctry (country name) column to avoid confusion.
    IEA_iso_region <- unique(select(IEA_ctry, iso, IEA_Fert_reg))

    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      repeat_add_columns(tibble::tibble(fuel = c("coal", "gas", "refined liquids"))) %>% # Exanding table to include fuels for each iso
      left_join_error_no_match(IEA_iso_region, by = "iso") %>% # Assign data to fert region using iso
      # Now we can attach the share data, matching to fert region
      left_join_error_no_match(L1322.IEA_fert_fuel_shares, by = c("fuel", "IEA_Fert_reg")) %>%
      # Switch individual countries' fuel shares according to literature or to make energy balances work at the regional level.
      # North Korea: coal in all years (http://www9.ocn.ne.jp/~aslan/dprkeng0409.pdf)
      mutate(value_share = replace(value_share, (iso == "prk") & (fuel == "coal"), 1),
             value_share = replace(value_share, (iso == "prk") & (fuel != "coal"), 0),
             # Multiply total production by fuel-specific shares to calculate fertilizer production by fuel type
             Fert_Prod_MtN = value * value_share) %>%
      # Calculating energy inputs to fertilizer production by country and fuel type
      # Match in the energy intensity by fuel type and IEA fertilizer region
      left_join_error_no_match(L1322.IEA_fert_fuel_intensities, by = c("IEA_Fert_reg", "fuel")) %>%
      mutate(in_Fert = Fert_Prod_MtN * intensity_GJkgN) %>%
      select(iso, year, fuel, value, value_share, in_Fert, intensity_GJkgN) ->
      L1322.Fert_Prod_MtN_ctry_F_Y

    # Checking total energy inputs to fertilizer against industrial energy use for each region
    # First, create a couple lists to use to expand table in a later step
    list_GCAM_region_ID <- unique(iso_GCAM_regID$GCAM_region_ID)
    list_fuels <- unique(L1322.Fert_Prod_MtN_ctry_F_Y$fuel)

    # Aggregate by GCAM region
    L1322.Fert_Prod_MtN_ctry_F_Y %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>% # Assign data to GCAM region using iso
      group_by(GCAM_region_ID, fuel, year) %>% # Aggregate by GCAM region, fuel, and year
      summarise(in_Fert = sum(in_Fert)) %>%
      ungroup() %>%
      # Expand this table to all regions, fuels, and years in case there are regions that aren't countries in the aglu databases
      complete(GCAM_region_ID = list_GCAM_region_ID,
               fuel = list_fuels,
               year = HISTORICAL_YEARS,
               fill = list(in_Fert = 0)) %>%
      # Attach data from ind energy and ind feedstocks
      left_join_error_no_match(L1321.in_EJ_R_indenergy_F_Yh, by = c("GCAM_region_ID", "year", "fuel")) %>%
      left_join_error_no_match(L132.in_EJ_R_indfeed_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      select(GCAM_region_ID, fuel, year, in_Fert, in_indenergy = value.x, in_indfeed = value.y) %>%
      # Finally, check whether the remaining available industrial energy use is not negative
      # (i.e., ind. energy + ind. feedstocks - fert)
      mutate(check = in_indenergy + in_indfeed - in_Fert) ->
      L1322.Fert_ALL_MtN_R_F_Y

    # Modifying fertilizer production shares so that industrial energy/feedstock use does not go negative for any fuels
    # Modify the fuel shares in the country-level fertilizer production tables, and re-compute fertilizer production quantities
    L1322.Fert_ALL_MtN_R_F_Y %>%
      filter(check < 0) %>%
      mutate(mult = (1 - (-1 * check / in_Fert))) %>%
      select(GCAM_region_ID, fuel, year, mult) ->
      L1322.Fert_modify

    # For matched region/fuel/year, multiply the original shares by the multipliers from above
    L1322.Fert_Prod_MtN_ctry_F_Y %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # left_join_error_no_match cannot be used because there are some rows in the base table left unmatched. NAs will be introduced.
      left_join(L1322.Fert_modify, by = c("GCAM_region_ID", "fuel", "year")) %>%
      replace_na(list(mult = 1)) %>%
      mutate(value_share_adj = value_share * mult) %>%
      select(iso, GCAM_region_ID, year, fuel, value, value_share_adj, intensity_GJkgN) ->
      L1322.Fert_Prod_MtN_ctry_F_Y_share_adj

    # For regions whose fuel shares changed, re-assign the production and energy inputs to the oil-based technology
    # Note that the sum of coal and gas shares never exceed 1. So the minimum share for refined liquids would be 0.
    L1322.Fert_Prod_MtN_ctry_F_Y_share_adj %>%
      select(iso, year, fuel, value_share_adj) %>%
      spread(fuel, value_share_adj) %>% # This is to set up the calculation in the next step
      mutate(`refined liquids` = 1 - coal - gas) %>%
      gather(fuel, value_share_adj, -iso, -year) ->
      Ctry_fuel_share_adj

    # Disaggregating input energy to fertilizer production into energy use (combustion) and feedstocks
    # The "feedstock" requirement is equal to the energy content of the hydrogen in NH3
    # see https://www.iea.org/publications/freepublications/publication/chemical_petrochemical_sector.pdf
    H_energy_GJtH2 <- 120 # Energy content of hydrogen is 120 MJ/kg, which is 120 GJ/t
    # See https://hypertextbook.com/facts/2005/MichelleFung.shtml
    NH3_H_frac <- 3 / 17 # Mass ratio of hydrogen in ammonia is about 3/17
    NH3_energy_GJtNH3 <- H_energy_GJtH2 * NH3_H_frac # Calculating energy in ammonia
    NH3_energy_GJkgN <- NH3_energy_GJtNH3  / CONV_T_KG / CONV_NH3_N # Calcuating energy per kg of N (from ton of NH3)

    # Re-compute fertilizer production and energy requirements using these new shares
    L1322.Fert_Prod_MtN_ctry_F_Y_share_adj %>%
      select(-value_share_adj) %>% # Drop old share. Will be replaced with new values in next step.
      left_join_error_no_match(Ctry_fuel_share_adj, by = c("iso", "year", "fuel")) %>%
      mutate(Fert_Prod_MtN_adj = value * value_share_adj, # Adjust Fert_Prod with adjusted shares
             in_Fert_adj = Fert_Prod_MtN_adj * intensity_GJkgN) %>% # Adjust in_Fert with adjusted production
      # Recheck total energy inputs to fertilizer against industrial energy use for each region
      # Aggregate to GCAM region
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(Fert_Prod_MtN_adj = sum(Fert_Prod_MtN_adj),
                in_Fert_adj = sum(in_Fert_adj)) %>%
      ungroup() %>%
      # Expand table to write out to all region x fuel x year combinations
      complete(GCAM_region_ID = list_GCAM_region_ID,
               fuel = list_fuels,
               year = HISTORICAL_YEARS,
               fill = list(in_Fert_adj = 0, Fert_Prod_MtN_adj = 0)) %>%
      # Re-checking total energy inputs to fertilizer against industrial energy use for each region
      left_join_error_no_match(L1322.Fert_ALL_MtN_R_F_Y, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(check = in_indenergy + in_indfeed - in_Fert_adj, # Recheck, adjusting for in_Fert_adj
             check = replace(check, abs(check) < 1e-6, 0),
             # Recalculate intensity
             intensity_GJkgN = in_Fert_adj / Fert_Prod_MtN_adj) %>%
      replace_na(list(intensity_GJkgN = 0)) %>%
      # Adding column for feedstock energy density, which was calcuated earlier using hydrogen density
      mutate(feedstock_GJkgN = NH3_energy_GJkgN, # Adding single column with energy density of feedstock
             energy_GJkgN = intensity_GJkgN - feedstock_GJkgN, # Substract energy density of feedstock
             # Calculate first-order estimate of energy and feedstock quantities in the industrial sector
             indfeed_to_Fert = Fert_Prod_MtN_adj * feedstock_GJkgN,
             indenergy_to_Fert = Fert_Prod_MtN_adj * energy_GJkgN,
             # Calculate remaining industrial energy and feedstock consumption re-allocating to avoid negative values
             in_indenergy_netFert = in_indenergy - indenergy_to_Fert,
             in_indfeed_netFert = in_indfeed - indfeed_to_Fert,
             # This next step pertains to the available energy and feedstock quantities for the industrial sector,
             # in regions where either went negative as a result of including the fertilizer industry (using the
             # IEA's shares of production by fuel type, modified so that no regions went negative, and following
             # the IEA's conventions on disaggregation of feedstocks and energy-use). We are assuming that for the
             # purposes of re-allocation of energy from the (general) industrial sector to the fertilizer sector,
             # all energy is available, whether initially classified as feedstocks or energy-use. This may zero out
             # both energy and feedstocks or both in the general industrial sector, causing all industrial demands
             # of natural gas to be for the ammonia industry.
             # Note that the quantities are added because one is negative.
             in_indenergy_netFert = replace(in_indenergy_netFert, in_indfeed_netFert < 0,
                                            round((in_indenergy_netFert + in_indfeed_netFert)[in_indfeed_netFert < 0], 10)),
             in_indfeed_netFert = replace(in_indfeed_netFert, in_indfeed_netFert < 0, 0),
             in_indfeed_netFert = replace(in_indfeed_netFert, in_indenergy_netFert < 0,
                                          round((in_indfeed_netFert + in_indenergy_netFert)[in_indenergy_netFert < 0], 10)),
             in_indenergy_netFert = replace(in_indenergy_netFert, in_indenergy_netFert < 0, 0),
             # Add column for sector, which will be "N fertilizer"
             sector = "N fertilizer") %>%
      select(GCAM_region_ID, fuel, sector, year, Fert_Prod_MtN_adj, intensity_GJkgN,
             in_indenergy_netFert, in_indfeed_netFert) ->
      L1322.Fert_ALL_MtN_R_F_Y_adj

    # -----------------------------------------------------------------------------------------------------------------
    # Building tables of fertilizer production by technology, IO coefs, and energy/feedstock inputs to rest of industry
    # Four of the final five tables will be built here.

    # Creating final output table "Fertilizer production by GCAM region / fuel / year"
    L1322.Fert_ALL_MtN_R_F_Y_adj %>%
      select(GCAM_region_ID, sector, fuel, year, value = Fert_Prod_MtN_adj) ->
      L1322.Fert_Prod_MtN_R_F_Y # Note that this is a final output table

    # Creating final output table "Fertilizer input-output coefs by GCAM region / fuel (base year techs only) / year"
    L1322.Fert_ALL_MtN_R_F_Y_adj %>%
      select(GCAM_region_ID, sector, fuel, year, value = intensity_GJkgN) ->
      L1322.IO_R_Fert_F_Yh # Note that this is a final output table

    # Creating final output table "Adjusted industrial feedstock use by GCAM region / fuel / year"
    # We already calcuated the values. We only need to add a column for the sector and strip out unnecessary columns.
    L1322.Fert_ALL_MtN_R_F_Y_adj %>%
      mutate(sector = "industry_feedstocks") %>%
      select(GCAM_region_ID, sector, fuel, year, value = in_indfeed_netFert) ->
      L1322.in_EJ_R_indfeed_F_Yh # Note that this is a final output table

    # Extract the values we calculated for industrial energy use. These values will be used for its respective final table.
    L1322.Fert_ALL_MtN_R_F_Y_adj %>%
      select(GCAM_region_ID, fuel, year, value = in_indenergy_netFert) ->
      L1322.in_EJ_R_indenergy_Ffert_Yh

    # Creating final output table "Adjusted industrial energy use by GCAM region / fuel / year"
    # We are starting with the input industrial energy table and replacing a portion of the values we calcuated in this chunk.
    L1321.in_EJ_R_indenergy_F_Yh %>%
      rename(value_indenergy = value) %>%
      # left_join_error_no_match cannot be used because the joining tibble has less rows, so NAs will be introduced.
      # This will be helpful as a filter in the following step.
      left_join(L1322.in_EJ_R_indenergy_Ffert_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>% # Join values we calculated
      # Replace any NAs with original industrial energy value
      mutate(value = replace(value, is.na(value), value_indenergy[is.na(value)])) %>%
      select(GCAM_region_ID, sector, fuel, year, value) ->
      L1322.in_EJ_R_indenergy_F_Yh # Note that this is a final output table.

    # -----------------------------------------------------------------------------------------------------------------

    # Calculate fertilizer non-energy costs by technology
    # These technologies include gas, gas with CCS, coal, coal with CCS, and oil
    # First, calculate gas cost per kg N
    # Calculating non-energy costs for gas technology as USA market fertilizer price minus GCAM fuel costs
    # Calculate the gas price as the sum of resource costs plus intermediate sectoral mark-ups

    # The processing steps below ensure that the base year for fertilizer prices is included
    A10.rsrc_info %>%
      filter(resource == "natural gas") %>%
      gather_years() %>%
      select(resource, year, value) %>%
      complete(resource, year = sort(unique(c(year, aglu.FERT_PRICE_YEAR)))) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(year == aglu.FERT_PRICE_YEAR) %>%
      mutate(value = replace_na(value, 0)) %>% 
      pull(value) -> # Save cost as single number. Units are 1975 USD per GJ.
      A10.rsrc_cost_aglu.FERT_PRICE_YEAR


    # A21.globaltech_cost and A22.globaltech_cost report costs on primary energy handling (A21) and transformation technologies (A22)
    # Units for both are 1975$/GJ
    # As mentioned above, because 2010 is the year used as the fertilizer base price (from A10.rsrc_info), we will interpolate for
    # this year for both global tech cost tables so that we may add up all costs consistently.

    # Interpolate to get cost of primary energy handling for natural gas in aglu.FERT_PRICE_YEAR
    A21.globaltech_cost %>%
      filter(technology == "regional natural gas") %>%
      gather_years() %>%
      select(technology, year, value) %>%
      complete(technology, year = sort(unique(c(year, aglu.FERT_PRICE_YEAR)))) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(year == aglu.FERT_PRICE_YEAR) %>%
      mutate(value = if_else(is.na(value) ,0 , as.double(value))) %>%
      pull(value) -> # Save cost as single number. Units are 1975 USD per GJ.
      A21.globaltech_cost_aglu.FERT_PRICE_YEAR



    # Interpolate to get cost of primary energy transformation for natural gas in aglu.FERT_PRICE_YEAR
    A22.globaltech_cost %>%
      filter(technology == "natural gas") %>%
      gather_years() %>%
      select(technology, year, value) %>%
      complete(technology, year = sort(unique(c(year, aglu.FERT_PRICE_YEAR)))) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(year == aglu.FERT_PRICE_YEAR) %>%
      mutate(value = if_else(is.na(value),0,as.double(value))) %>%
      pull(value) -> # Save cost as single number. Units are 1975 USD per GJ.
      A22.globaltech_cost_aglu.FERT_PRICE_YEAR


    # Sum up costs. Units are 1975 USD per GJ.
    L1322.P_gas_75USDGJ <- A10.rsrc_cost_aglu.FERT_PRICE_YEAR + energy.GAS_PIPELINE_COST_ADDER_75USDGJ

    # Obtain fertilizer input-output cofficient for natural gas in aglu.FERT_PRICE_YEAR
    L1322.IO_R_Fert_F_Yh %>%
      filter(year == aglu.FERT_PRICE_YEAR,
             GCAM_region_ID == gcam.USA_CODE,
             fuel == "gas") %>%
      pull(value) -> # Save coefficient as single number
      L1322.IO_GJkgN_Fert_gas

    # Multiply cost by input-output cofficient. Units are 1975 USD per GJ.
    L1322.Fert_Fuelcost_75USDGJ_gas <- L1322.P_gas_75USDGJ * L1322.IO_GJkgN_Fert_gas

    # Convert total NH3 cost (2010$/tNH3) to N cost (1975$/kgN)
    Fert_Cost_75USDkgN <- aglu.FERT_PRICE * gdp_deflator(1975, aglu.FERT_PRICE_YEAR) * CONV_KG_T / CONV_NH3_N

    # Calculate non-fuel cost of natural gas steam reforming (includes delivery charges)
    L1322.Fert_NEcost_75USDkgN_gas <- as.double(Fert_Cost_75USDkgN - L1322.Fert_Fuelcost_75USDGJ_gas)


    # Use H2A technology characteristics to derive characteristics of other technologies

    # NOTE: Because our NGSR NEcosts were calculated as a residual from mkt prices, and include delivery costs,
    # not using a ratio of costs, but rather an arithmetic adder. H2A costs are in $/kgH; convert to N-equivalent

    # First, calculate costs in 1975 USD per kg N
    H2A_Prod_Tech %>%
      mutate(NEcost_75USDkgN = NEcost * gdp_deflator(1975, aglu.FERT_PRICE_YEAR) * NH3_H_frac / CONV_NH3_N) ->
      H2A_Prod_Tech_1975

    # Derive costs as the cost of NGSR plus the specified cost adder
    H2A_Prod_Tech_1975 %>%
      select(Technology, NEcost_75USDkgN) %>%
      mutate(Technology= if_else(Technology=="Central Natural Gas Sequestration","Central_Natural_Gas_Sequestration",Technology),
             Technology= if_else(Technology=="Central Natural Gas","Central_Natural_Gas",Technology),
             Technology= if_else(Technology=="Central Coal","Central_Coal",Technology),
             Technology= if_else(Technology=="Central Coal Sequestration","Central_Coal_Sequestration",Technology)) %>%
      spread(Technology, NEcost_75USDkgN) %>%
      mutate(gasCCS = as.double(Central_Natural_Gas_Sequestration - Central_Natural_Gas + L1322.Fert_NEcost_75USDkgN_gas),
             coal = as.double(Central_Coal - Central_Natural_Gas+ L1322.Fert_NEcost_75USDkgN_gas),
             coalCCS = as.double(Central_Coal_Sequestration - Central_Natural_Gas+ L1322.Fert_NEcost_75USDkgN_gas))-> H2A_Prod_Tech_1975

    H2A_Prod_Tech_1975 ->L1322.Fert_NEcost_75USDkgN_technologies


    # Here the individual costs will be saved. These values will be used to build the final table.
    L1322.Fert_NEcost_75USDkgN_gasCCS <- L1322.Fert_NEcost_75USDkgN_technologies[["gasCCS"]]

    L1322.Fert_NEcost_75USDkgN_coal <- L1322.Fert_NEcost_75USDkgN_technologies[["coal"]]

    L1322.Fert_NEcost_75USDkgN_coalCCS <- L1322.Fert_NEcost_75USDkgN_technologies[["coalCCS"]]

    # Oil
    # For oil, the lack of differentiation in oil-derived products means that the fuel costs are too high
    # Fertilizer is made from relatively low-cost by-products of oil refining
    # Also, the technology is being phased out where it is currently used (primarily India)
    # To minimize price distortions from this phase-out, and to ensure no negative profit rates in the ag sector,
    # set the NE cost to generally balance the total net costs with natural gas steam reforming
    # Costs for natural gas were calculated above to be 0.074. So set costs for oil to be -0.1.
    L1322.Fert_NEcost_75USDkgN_oil <- -0.1

    # Build final output table with NE costs by technology
    L1322.Fert_NEcost_75USDkgN_F <- tibble(fuel = c("gas", "gas CCS", "coal", "coal CCS", "refined liquids"),
                                           NEcost_75USDkgN = c(L1322.Fert_NEcost_75USDkgN_gas,
                                                               L1322.Fert_NEcost_75USDkgN_gasCCS,
                                                               L1322.Fert_NEcost_75USDkgN_coal,
                                                               L1322.Fert_NEcost_75USDkgN_coalCCS,
                                                               L1322.Fert_NEcost_75USDkgN_oil))

    # ===================================================

    L1322.Fert_Prod_MtN_R_F_Y %>%
      add_title("Fertilizer production by GCAM region / fuel / year") %>%
      add_units("MtN") %>%
      add_comments("Generic fertilizer production data was broken down using country-level fuel share and energy intensity data.") %>%
      add_comments("Shares of fertilizer production were modified so that industrial energy/feedstock for any fuels were not negative for any country, before aggregating to the regional level.") %>%
      add_legacy_name("L1322.Fert_Prod_MtN_R_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/IEA_ctry", "energy/IEA_Fert_fuel_data",
                     "L142.ag_Fert_Prod_MtN_ctry_Y") ->
      L1322.Fert_Prod_MtN_R_F_Y

    L1322.IO_R_Fert_F_Yh %>%
      add_title("Fertilizer input-output coefs by GCAM region / fuel (base year techs only) / year") %>%
      add_units("GJ per kg N") %>%
      add_comments("Generic fertilizer production data was broken down using country-level fuel share and energy intensity data.") %>%
      add_comments("Re-allocation of energy use between energy and feedstock quantities for the industrial sector was done to avoid negative values at the country level, before aggregating to the regional level.") %>%
      add_legacy_name("L1322.IO_R_Fert_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/IEA_ctry", "energy/IEA_Fert_fuel_data",
                     "L142.ag_Fert_Prod_MtN_ctry_Y", "L1321.in_EJ_R_indenergy_F_Yh",
                     "L132.in_EJ_R_indfeed_F_Yh") ->
      L1322.IO_R_Fert_F_Yh

    L1322.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted industrial energy use by GCAM region / fuel / year") %>%
      add_units("EJ") %>%
      add_comments("Industrial energy use was calculated using country-level fuel share and energy intensity data to break down generic fertilizer production data.") %>%
      add_comments("Re-allocation of energy use between energy and feedstock quantities for the industrial sector was done to avoid negative values at the country level, before aggregating to the regional level.") %>%
      add_legacy_name("L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/IEA_ctry", "energy/IEA_Fert_fuel_data",
                     "L142.ag_Fert_Prod_MtN_ctry_Y", "L1321.in_EJ_R_indenergy_F_Yh",
                     "L132.in_EJ_R_indfeed_F_Yh") ->
      L1322.in_EJ_R_indenergy_F_Yh

    L1322.in_EJ_R_indfeed_F_Yh %>%
      add_title("Adjusted industrial feedstock use by GCAM region / fuel / year") %>%
      add_units("EJ") %>%
      add_comments("Industrial feedstock use was calculated using country-level fuel share and energy intensity data to break down generic fertilizer production data.") %>%
      add_comments("Re-allocation of energy use between energy and feedstock quantities for the industrial sector was done to avoid negative values at the country level, before aggregating to the regional level.") %>%
      add_legacy_name("L1322.in_EJ_R_indfeed_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/IEA_ctry", "energy/IEA_Fert_fuel_data",
                     "L142.ag_Fert_Prod_MtN_ctry_Y", "L1321.in_EJ_R_indenergy_F_Yh",
                     "L132.in_EJ_R_indfeed_F_Yh") ->
      L1322.in_EJ_R_indfeed_F_Yh

    L1322.Fert_NEcost_75USDkgN_F %>%
      add_title("Fertilizer non-energy costs by technology") %>%
      add_units("1975USD/kgN") %>%
      add_comments("Gas was calculated using USA market fertilizer price minus GCAM fuel costs.") %>%
      add_comments("Gas with CCS, coal, and coal with CCS were calculated using H2A characteristics of hydrogen production technologies") %>%
      add_comments("Oil was set to generally balance the total net costs with natural gas steam reforming.") %>%
      add_legacy_name("L1322.Fert_NEcost_75USDkgN_F") %>%
      add_precursors("energy/H2A_Prod_Tech", "energy/A10.rsrc_info",
                     "energy/A21.globaltech_cost", "energy/A22.globaltech_cost") ->
      L1322.Fert_NEcost_75USDkgN_F

    return_data(L1322.Fert_Prod_MtN_R_F_Y, L1322.IO_R_Fert_F_Yh, L1322.in_EJ_R_indenergy_F_Yh, L1322.in_EJ_R_indfeed_F_Yh, L1322.Fert_NEcost_75USDkgN_F)
  } else {
    stop("Unknown command")
  }
}
