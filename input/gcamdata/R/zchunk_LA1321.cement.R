# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1321.cement
#'
#' Sets up input, output, and IO coefficients for cement and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_R_cement_Yh}, \code{L1321.IO_GJkg_R_cement_F_Yh}, \code{L1321.in_EJ_R_cement_F_Y}, \code{L1321.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' original data system was \code{LA1321.cement.R} (energy level1).
#' @details This chunk generates input, output, and IO coefficients for the cement sector. It begins by downscaling Worrell regional data from 1994
#' to set up process emissions factors that are multiplied by country emissions from CDIAC to determine production. Limestone consumption is calculated from the same downscaling.
#' IEA fuelshares and heat and electricity are used to determine energy use by fuel. Energy inputs are then subtracted from industrial energy use and any resulting negative values
#' are dealt with by moving their accounting to the cement sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr complete gather nesting
#' @author CWR Nov 2017
module_energy_LA1321.cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/mappings/cement_regions",
             FILE = "energy/Worrell_1994_cement",
             FILE = "energy/IEA_cement_elec_kwht",
             FILE = "energy/IEA_cement_TPE_GJt",
             FILE = "energy/IEA_cement_fuelshares",
             "L100.CDIAC_CO2_ctry_hist",
             "L102.CO2_Mt_R_F_Yh",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L132.in_EJ_R_indenergy_F_Yh",
             "L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y",
             "L1321.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    . <- Biomass <- Biomass_EJ <- Coal <- Coal_EJ <- Country <- GCAM_region_ID <- Gas <- Gas_EJ <-
    IEA_fuelshare_region <- IEA_intensity_region <- IOelec <- Oil <- Oil_EJ <- TPE_GJkg <-
    Worrell_region <- cement_prod_Mt <- country_name <- elec_EJ <- elec_GJkg <-
    emiss_ktC <- fuel <- heat_EJ <- heat_GJkg <- in.value <- ind.value <- iso <-
    old.year <- out.value <- process_emissions_MtC <- process_emissions_ktC <-
    prod_Mt <- prod_emiss_ratio <- reg_process_emissions <- region_GCAM3 <- sector <-
    share <- value <- cement <- year <- value.y <- value.x <- neg <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    cement_regions <- get_data(all_data, "energy/mappings/cement_regions")
    Worrell_1994_cement <- get_data(all_data, "energy/Worrell_1994_cement")
    IEA_cement_elec_kwht <- get_data(all_data, "energy/IEA_cement_elec_kwht")
    IEA_cement_TPE_GJt <- get_data(all_data, "energy/IEA_cement_TPE_GJt")
    IEA_cement_fuelshares <- get_data(all_data, "energy/IEA_cement_fuelshares")
    L100.CDIAC_CO2_ctry_hist <- get_data(all_data, "L100.CDIAC_CO2_ctry_hist")
    L102.CO2_Mt_R_F_Yh <- get_data(all_data, "L102.CO2_Mt_R_F_Yh")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L132.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L132.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")

    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------

    # Extract carbon coefficient for limestone from assumption file
    LIMESTONE_CCOEF <- A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "limestone"]
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)
    ADDITIONAL_YEARS <- HISTORICAL_YEARS[!HISTORICAL_YEARS %in% energy.CDIAC_CO2_HISTORICAL_YEARS]
    FINAL_CO2_YEAR <- dplyr::last(energy.CDIAC_CO2_HISTORICAL_YEARS)

    # =======================================================================================
    # Derivation of cement production and limestone consumption by region and historical year
    # =======================================================================================

    # Downscale Worrell's process CO2 emissions and cement production in 1994 to country level using CDIAC emissions inventory
    # ------------------------------------------------------------------------------------------------------------------------

    # Prepare CDIAC country-level emissions data to match to Worrell regions and year (Worrell data only available for 1994)
    cement_regions %>%
      left_join(L100.CDIAC_CO2_ctry_hist, by = "iso") %>%
      filter(year == 1994) %>%
      select(iso, Worrell_region, year, cement) %>%
      na.omit() %>%
      rename(process_emissions_ktC = cement) ->
      L1321.Cement_Worrell_ctry

    # Aggregate CDIAC country-level process emissions to regional level
    L1321.Cement_Worrell_ctry %>%
      group_by(Worrell_region) %>%
      summarise(reg_process_emissions = sum(process_emissions_ktC)) %>%
      ungroup() ->
      L1321.Cement_Worrell_reg

    # Read in country and regional emissions, select Worrell's cement production and process emissions to multiply by country level shares
    # Compute country-level shares of regional emissions and multiply the region-level process emissions
    # and cement production by these shares to get the country-level estimates
    # (1994 cement data source: Table 4 in Worrell et al. 2001 Ann Rev Energ Env 26: 303-329)
    L1321.Cement_Worrell_ctry %>%
      left_join_error_no_match(L1321.Cement_Worrell_reg, by = "Worrell_region") %>%
      mutate(share = process_emissions_ktC / reg_process_emissions) %>%
      left_join_error_no_match(select(Worrell_1994_cement, Country, cement_prod_Mt, process_emissions_MtC), by = c("Worrell_region" = "Country")) %>%
      mutate(cement_prod_Mt = cement_prod_Mt * share, process_emissions_MtC = process_emissions_MtC * share) %>%

      # Now match and aggregate Worrell's process CO2 emissions and cement production in 1994 by GCAM region and compute the emissions ratio
      # which is assumed to be constant for all other years
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(-country_name, -region_GCAM3) %>%
      group_by(GCAM_region_ID) %>%
      summarise(cement_prod_Mt = sum(cement_prod_Mt), process_emissions_MtC = sum(process_emissions_MtC)) %>%
      mutate(prod_emiss_ratio = cement_prod_Mt / process_emissions_MtC) %>%
      ungroup() ->
      L1321.Cement_Worrell_R

    # Calculate cement production over time using ratio of production to emissions for L1321.out_Mt_R_cement_Yh
    # assuming that this ratio is constant over time for each region
    # ---------------------------------------------------------------------------------------------------------

    # If the CO2 emissions inventories do not go to the latest historical time period, copy the last available year
    L102.CO2_Mt_R_F_Yh %>%
      filter(fuel == "limestone") ->
      L1321.CO2_Mt_R_F_Yh_base

    L1321.CO2_Mt_R_F_Yh_base %>%
      filter(year == FINAL_CO2_YEAR) %>%
      rename(old.year = year) %>%
      repeat_add_columns(tibble(year = ADDITIONAL_YEARS)) %>%
      select(-old.year) %>%
      bind_rows(L1321.CO2_Mt_R_F_Yh_base) ->
      L1321.CO2_Mt_R_F_Yh

    # Calculate cement production over time by multiplying production emissions ratio by emissions
    L1321.Cement_Worrell_R %>%
      mutate(sector = "cement") %>%
      left_join(L1321.CO2_Mt_R_F_Yh, by = "GCAM_region_ID") %>%
      mutate(value = prod_emiss_ratio * value) %>%
      select(GCAM_region_ID, sector, year, value) ->
      L1321.out_Mt_R_cement_Yh

    # Calculate limestone consumption by region and fuel
    # --------------------------------------------------

    # Use the assumed limestone fuel carbon content (same in all regions) to calculate the limestone consumption
    # and limestone to cement IO coefficients in each region
    L1321.CO2_Mt_R_F_Yh %>%
      mutate(sector = "cement", in.value = value / LIMESTONE_CCOEF) %>%
      select(-value) ->
      L1321.in_Cement_Mt_R_limestone_Yh

    # Calculate input-output coefficients
    L1321.in_Cement_Mt_R_limestone_Yh %>%
      left_join_error_no_match(L1321.out_Mt_R_cement_Yh, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = in.value / value) %>%
      select(-in.value) ->
      L1321.IO_Cement_R_limestone_Yh

    # Derive energy inputs to cement production by region and historical year -
    # Calculate average electric and TPE intensity for each GCAM region (use process emissions as a weighting factor)
    # ---------------------------------------------------------------------------------------------------------------

    # Interpolate available data on electricity intensity to all historical years
    IEA_cement_elec_kwht %>%
      gather_years() %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) * CONV_KWH_GJ / CONV_T_KG) %>%
      ungroup() ->
      L1321.IEA_cement_elec_intensity

    # Interpolate available data on total primary energy intensity to all historical years by region
    IEA_cement_TPE_GJt %>%
      gather_years() %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) / CONV_T_KG) %>%
      ungroup() ->
      L1321.IEA_cement_TPE_intensity

    # Calculate the average electricity generation efficiencies by region to be added to L1321.Cement_ALL_ctry_Yh
    # -----------------------------------------------------------------------------------------------------------

    # Calculate average regional input energy for electricity across all fuels
    L123.in_EJ_R_elec_F_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(in.value = sum(value)) %>%
      ungroup() ->
      L1321.in_EJ_R_elec_Yh

    # Calculate average regional output energy for electricity across all fuels matching input, join to input energy, and calculate the IO coefficient
    L123.out_EJ_R_elec_F_Yh %>%
      # Filter out electricity fuel outputs with no matching energy inputs - by default removes non-fossil, non-bio energy
      semi_join(L123.in_EJ_R_elec_F_Yh, by = "fuel")  %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(out.value = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L1321.in_EJ_R_elec_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = in.value / out.value) %>%
      # NOTE: below replicates the old data system by interpolating IO value to all historical years, but this step can be skipped with current inputs (no new values generated)
      complete(nesting(GCAM_region_ID), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(GCAM_region_ID, year, value) %>%
      ungroup() ->
      L1321.IO_R_elec_Yh

    # Set cap on IO coefficients for regions and years exceeding maximum value
    # Generation technologies with extremely low efficiency indicate that a significant portion of the power sector 
    # in the given region/year is combined heat and power systems.
    # Since we don't account for that here, we use this constant to set a reasonable limit on how much of the 
    # total primary energy in IEA's estimate is electricity-related. (see issue #833)
    L1321.IO_R_elec_Yh$value[L1321.IO_R_elec_Yh$value > energy.MAX_IOELEC] <- energy.MAX_IOELEC

    # Build data frame including all above calculated values for cement production - intensity, fuel shares, energy for heat and electricity
    # --------------------------------------------------------------------------------------------------------------------------------------

    # Start with CO2 emissions from cement
    L100.CDIAC_CO2_ctry_hist %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(iso, year, cement) %>%
      rename(emiss_ktC = cement) %>%
      # Match in region IDs by iso code
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # Replace process emissions with actual cement production
      left_join_error_no_match(L1321.Cement_Worrell_R, by = "GCAM_region_ID") %>%
      mutate(prod_Mt = emiss_ktC * prod_emiss_ratio * CONV_KT_MT) %>%
      # add region names for intensity and fuelshare
      left_join_error_no_match(cement_regions, by = "iso") %>%
      # add above calculated energy intensities
      left_join_error_no_match(L1321.IEA_cement_elec_intensity, by = c("IEA_intensity_region" = "Country", "year")) %>%
      left_join_error_no_match(L1321.IEA_cement_TPE_intensity, by = c("IEA_intensity_region" = "Country", "year")) %>%
      rename(elec_GJkg = value.x, TPE_GJkg = value.y) %>%
      # remove unneeded columns from various left_joins
      select(iso, year, emiss_ktC, GCAM_region_ID, prod_Mt, IEA_intensity_region, elec_GJkg, TPE_GJkg, IEA_fuelshare_region) %>%
      # Match in IO coefficients by region and year
      left_join_error_no_match(L1321.IO_R_elec_Yh, by = c("GCAM_region_ID", "year")) %>%
      rename(IOelec = value) %>%
      # Match in fuelshares of (by default) coal, gas, oil, and biomass
      left_join_error_no_match(IEA_cement_fuelshares, by = c("IEA_fuelshare_region" = "Country")) %>%
      # Calculate heat intensity of energy and electricity, as well as total heat for each
      mutate(heat_GJkg = TPE_GJkg - elec_GJkg * IOelec, heat_EJ = heat_GJkg * prod_Mt, elec_EJ = elec_GJkg * prod_Mt) %>%
      # Calculate total heat by fuel using fuelshares
      mutate(Coal_EJ = Coal * heat_EJ, Oil_EJ = Oil * heat_EJ, Gas_EJ = Gas * heat_EJ, Biomass_EJ = Biomass * heat_EJ) ->
      L1321.Cement_ALL_ctry_Yh

    # ===============================================================================================================================================
    # Now that country level data has been built and downscaled into L1321.Cement_ALL_ctry_Yh, calculate needed GCAM input energy and IO coefficients
    # ===============================================================================================================================================

    # Calculate aggregated regional data on IO coefficients for cement production by fuel for heat and electricity
    # ------------------------------------------------------------------------------------------------------------

    # Aggregate country data to the regional level
    L1321.Cement_ALL_ctry_Yh %>%
      select(GCAM_region_ID, year, prod_Mt, heat_EJ, elec_EJ, Coal_EJ, Oil_EJ, Gas_EJ, Biomass_EJ) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      mutate(heat_GJkg = heat_EJ / prod_Mt, elec_GJkg = elec_EJ / prod_Mt) ->
      L1321.Cement_ALL_R_Yh

    # Separate regional electricity and heat coefficients, first removing all unneeded columns
    L1321.Cement_ALL_R_Yh %>%
      select(GCAM_region_ID, year, elec_GJkg, heat_GJkg) ->
      L1321.Cement_ALL_R_Yh_base

    # Copy final year value to any historical period values not contained in the data set
    L1321.Cement_ALL_R_Yh_base %>%
      filter(year == FINAL_CO2_YEAR) %>%
      rename(old.year = year) %>%
      repeat_add_columns(tibble(year = ADDITIONAL_YEARS)) %>%
      select(-old.year) %>%
      bind_rows(L1321.Cement_ALL_R_Yh_base) ->
      L1321.IO_Cement_GJkg_R_ALL_Yh

    # Assign sector and fuel names for heat and electricity data
    L1321.IO_Cement_GJkg_R_ALL_Yh %>%
      select(GCAM_region_ID, year, elec_GJkg) %>%
      mutate(sector = "cement", fuel = "electricity") %>%
      rename(value = elec_GJkg) ->
      L1321.IO_Cement_GJkg_R_elec_Yh

    L1321.IO_Cement_GJkg_R_ALL_Yh %>%
      select(GCAM_region_ID, year, heat_GJkg) %>%
      mutate(sector = "cement", fuel = "heat") %>%
      rename(value = heat_GJkg) ->
      L1321.IO_Cement_GJkg_R_heat_Yh

    # Compile electricity, heat, and limestone IO coefficients in L1321.IO_GJkg_R_cement_F_Yh
    L1321.IO_Cement_GJkg_R_elec_Yh %>%
      bind_rows(L1321.IO_Cement_GJkg_R_heat_Yh) %>%
      bind_rows(L1321.IO_Cement_R_limestone_Yh) ->
      L1321.IO_GJkg_R_cement_F_Yh

    # Calculate input energy for cement production by region, fuel, and year for L1321.in_EJ_R_cement_F_Y
    # ---------------------------------------------------------------------------------------------------

    # Compile regional historical data on energy use for cement by fuel
    L1321.Cement_ALL_R_Yh %>%
      select(GCAM_region_ID, year, elec_EJ, Coal_EJ, Oil_EJ, Gas_EJ, Biomass_EJ) %>%
      gather(fuel, value, elec_EJ, Coal_EJ, Oil_EJ, Gas_EJ, Biomass_EJ) %>%
      # Match fuel names to GCAM, removing _EJ and replacing oil and elec
      mutate(sector = "cement", fuel = tolower(gsub("_EJ", "", fuel)), fuel = gsub("elec", "electricity", fuel),
             fuel = gsub("oil", "refined liquids", fuel)) ->
      L1321.in_EJ_R_cement_F_Y_base

    # Copy final year value to any historical period values not contained in the data set
    L1321.in_EJ_R_cement_F_Y_base %>%
      filter(year == FINAL_CO2_YEAR) %>%
      rename(old.year = year) %>%
      repeat_add_columns(tibble(year = ADDITIONAL_YEARS)) %>%
      select(-old.year) %>%
      bind_rows(L1321.in_EJ_R_cement_F_Y_base) ->
      L1321.in_EJ_R_cement_F_Y

    # ---------------------------------------------------------------------------------------------------------------------
    ## 7/30/21: Modification for detailed industry
    ## Manually adjust coal use in South Korea, so there is enough left for iron and steel sector
    ## Upper bound is IEA coal consumption in non-metallic minerals

    L1321.in_EJ_R_cement_F_Y %>%
      left_join(L1011.en_bal_EJ_R_Si_Fi_Yh %>% filter(sector == "cement") %>% select(-sector),
                by = c("GCAM_region_ID", "year", "fuel")) %>%
      mutate(value = if_else(fuel == "coal" & GCAM_region_ID == 28 & value.x > value.y, value.y, value.x),
             neg = if_else(fuel == "coal" & GCAM_region_ID == 28 & value.x > value.y, value.y - value.x, 0)) ->
      L1321.in_EJ_R_cement_F_Y_adj

    # extra coal use will be moved to biomass
    L1321.in_EJ_R_cement_F_Y_adj %>%
      filter(neg < 0) %>%
      select(GCAM_region_ID, sector, fuel, year, value=neg) ->
      korea_coal_neg_en

    # Replace cement energy data frame with adjusted version
    L1321.in_EJ_R_cement_F_Y_adj %>%
      select(-value.x, -value.y, -neg) ->
      L1321.in_EJ_R_cement_F_Y
    # ---------------------------------------------------------------------------------------------------------------------


    # Calculate remaining industrial energy use (input), subtracting cement production energy from energy balances
    # ------------------------------------------------------------------------------------------------------------

    # Subtract input energy to cement sector from industrial energy
    L132.in_EJ_R_indenergy_F_Yh %>%
      rename(ind.value = value) %>%
      left_join(select(L1321.in_EJ_R_cement_F_Y, -sector), by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = ind.value - value) %>%
      select(-ind.value) ->
      L1321.in_EJ_R_indenergy_F_Yh_NAs

    # Replace NA values in sectors with no match in cement with original values from L132.in_EJ_R_indenergy_F_Yh
    L1321.in_EJ_R_indenergy_F_Yh_NAs %>%
      filter(is.na(value)) %>%
      select(-value) %>%
      left_join(select(L132.in_EJ_R_indenergy_F_Yh, -sector), by = c("GCAM_region_ID", "fuel", "year")) %>%
      bind_rows(filter(L1321.in_EJ_R_indenergy_F_Yh_NAs, !is.na(value))) ->
      L1321.in_EJ_R_indenergy_F_Yh_negbio

    # This dataset may now have negative values. If it's biomass, these can be changed without breaking energy balances, so we set the rest-of-industry to an exogenous minimum value
    L1321.in_EJ_R_indenergy_F_Yh_negbio %>%
      filter(value < 0, fuel == "biomass") %>%
      mutate(value = energy.MIN_IN_EJ_IND) %>%
      bind_rows(filter(L1321.in_EJ_R_indenergy_F_Yh_negbio, value >= 0 | fuel != "biomass")) ->
      L1321.in_EJ_R_indenergy_F_Yh

    # Negative values in non-bio industrial energy use are problematic and have to be zeroed out
    # In the below method, these negative values are zeroed in industrial energy and subtracted from positive values in fossil fuel use for cement
    # and then offset with a positive adjustment to cement biomass fuel use. This preserves the total energy balances while removing negative values
    # ----------------------------------------------------------------------------------------------------------------------------------------------

    # Currently offending negative values are mostly developing regions with (probably incorrectly) low bio shares
    # Check if any of these values exist and then conditionally perform the adjustments
    # Subset regions and years where any fuels are negative
    # and aggregate those negative values (in case one region/year has multiple fuels) as a positive adjustment to biomass
    L1321.in_EJ_R_indenergy_F_Yh %>%
      filter(value < -0) %>%
      mutate(sector = "cement") ->
      L1321.cement_adj_neg

    L1321.cement_adj_neg %>%
      # STEEL DECARONBONIZATION MODIFICATION: add korea excess coal to biomass
      bind_rows(korea_coal_neg_en) %>%
      mutate(fuel = "biomass") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value) * -1) %>%
      ungroup() ->
      L1321.cement_adj_pos

    # Reset the negative values to 0 in the industrial energy table
    L1321.in_EJ_R_indenergy_F_Yh[, "value"][L1321.in_EJ_R_indenergy_F_Yh[, "value"] < 0] <- 0

    # Add in the adjustments to the table of cement energy consumption.
    L1321.in_EJ_R_cement_F_Y %>%
      bind_rows(L1321.cement_adj_neg) %>%
      bind_rows(L1321.cement_adj_pos) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L1321.in_EJ_R_cement_F_Y

    # ===================================================
    # Produce outputs

    L1321.out_Mt_R_cement_Yh %>%
      add_title("Historical cement outputs by region, fuel, and year") %>%
      add_units("Mt cement") %>%
      add_comments("Outputs are calculated by by downscaling Worrell regions using CDIAC country emissions and then aggregating to GCAM regions") %>%
      add_comments("Final outputs are a product of regional emissions times the production emissions ratio") %>%
      add_legacy_name("L1321.out_Mt_R_cement_Yh") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef", "energy/Worrell_1994_cement", "energy/mappings/cement_regions", "L100.CDIAC_CO2_ctry_hist", "L102.CO2_Mt_R_F_Yh") ->
      L1321.out_Mt_R_cement_Yh

    L1321.IO_GJkg_R_cement_F_Yh %>%
      add_title("Input-output coefficients for cement production") %>%
      add_units("GJ/kg cement") %>%
      add_comments("IO coefficients for heat energy, electricity, and limestone consumption are calculated from weighted IEA fuel shares, CDIAC emissions data, and Worrell cement production") %>%
      add_legacy_name("L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef", "energy/Worrell_1994_cement", "energy/mappings/cement_regions", "L100.CDIAC_CO2_ctry_hist", "L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "energy/IEA_cement_elec_kwht",
                     "energy/IEA_cement_TPE_GJt", "energy/IEA_cement_fuelshares", "common/iso_GCAM_regID") ->
      L1321.IO_GJkg_R_cement_F_Yh

    L1321.in_EJ_R_cement_F_Y %>%
      add_title("Historical input energy use for the cement sector") %>%
      add_units("Exajoules") %>%
      add_comments("Input energy by fuel calculated from weighted fuel shares using energy intensity values for heat and electricity") %>%
      add_comments("Multiplied by raw fuel shares, all from IEA") %>%
      add_legacy_name("L1321.in_EJ_R_cement_F_Y") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "energy/IEA_cement_elec_kwht",
                     "energy/IEA_cement_TPE_GJt", "energy/IEA_cement_fuelshares", "common/iso_GCAM_regID", "L1011.en_bal_EJ_R_Si_Fi_Yh") ->
      L1321.in_EJ_R_cement_F_Y

    L1321.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted cement energy use from industrial energy use values in L132.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1321.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L100.CDIAC_CO2_ctry_hist", "L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "energy/IEA_cement_elec_kwht",
                     "energy/IEA_cement_TPE_GJt", "energy/IEA_cement_fuelshares", "L132.in_EJ_R_indenergy_F_Yh", "common/iso_GCAM_regID") ->
      L1321.in_EJ_R_indenergy_F_Yh

    return_data(L1321.out_Mt_R_cement_Yh, L1321.IO_GJkg_R_cement_F_Yh, L1321.in_EJ_R_cement_F_Y, L1321.in_EJ_R_indenergy_F_Yh)
  } else {
    stop("Unknown command")
  }
}
