# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1321.cement
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
module_energy_L1321.cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/mappings/cement_regions",
             FILE = "energy/mappings/Andrew_iso_correction",
             FILE = "energy/Andrew_cement_production",
             FILE = "emissions/Andrew_cement_emissions",
             FILE = "energy/IEA_cement_elec_kwht",
             FILE = "energy/IEA_cement_elec_kwht_update",
             FILE = "energy/IEA_cement_TPE_GJt",
             FILE = "energy/IEA_cement_thermal_energy_GJt_clinker_global_trend",
             "L102.CO2_Mt_R_F_Yh",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L132.in_EJ_R_indenergy_F_Yh",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y",
             "L1321.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    . <- Biomass <- Biomass_EJ <- Coal <- Coal_EJ <- Country <- GCAM_region_ID <- Gas <- Gas_EJ  <-
      IEA_intensity_region <- IOelec <- Oil <- Oil_EJ <- TPE_GJkg <-
      cement_prod_Mt <- country_name <- elec_EJ <- elec_GJkg <-
      emiss_ktC <- fuel <- heat_EJ <- heat_GJkg <- in.value <- ind.value <- iso <-
      old.year <- out.value <- process_emissions_MtC <- process_emissions_ktC <-
      prod_Mt <- prod_emiss_ratio <- reg_process_emissions <- region_GCAM3 <- sector <-
      share <- value <- cement <- year <- value.y <- value.x <- neg <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef", strip_attributes = TRUE)
    cement_regions <- get_data(all_data, "energy/mappings/cement_regions", strip_attributes = TRUE)
    Andrew_iso_correction <- get_data(all_data, "energy/mappings/Andrew_iso_correction", strip_attributes = TRUE)
    IEA_cement_elec_kwht <- get_data(all_data, "energy/IEA_cement_elec_kwht", strip_attributes = TRUE)
    IEA_cement_elec_kwht_update <- get_data(all_data, "energy/IEA_cement_elec_kwht_update", strip_attributes = TRUE)
    IEA_cement_TPE_GJt <- get_data(all_data, "energy/IEA_cement_TPE_GJt", strip_attributes = TRUE)
    IEA_cement_thermal_energy_GJt_clinker_global_trend <- get_data(all_data, "energy/IEA_cement_thermal_energy_GJt_clinker_global_trend", strip_attributes = TRUE)
    L102.CO2_Mt_R_F_Yh <- get_data(all_data, "L102.CO2_Mt_R_F_Yh", strip_attributes = TRUE)
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh", strip_attributes = TRUE)
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh", strip_attributes = TRUE)
    L132.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L132.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    Andrew_cement_production <- get_data(all_data, "energy/Andrew_cement_production", strip_attributes = TRUE)
    Andrew_cement_emissions <- get_data(all_data, "emissions/Andrew_cement_emissions", strip_attributes = TRUE)
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full", strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------

    # Extract carbon coefficient for limestone from assumption file
    LIMESTONE_CCOEF <- A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "limestone"]
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)
    ADDITIONAL_YEARS <- HISTORICAL_YEARS[!HISTORICAL_YEARS %in% energy.ANDREW_CO2_HISTORICAL_YEARS]
    FINAL_CO2_YEAR <- min( max(HISTORICAL_YEARS), Andrew_cement_production$Year %>% max )

    # =======================================================================================
    # Cement and Clinker Production, Cement to Clinker ratio
    # =======================================================================================

    # Check last year in Andrew cement data, if last year of data is less than max(historical year) then warn and carry forward
    Andrew_last_year <- Andrew_cement_production$Year %>% max
    if(Andrew_last_year <  max(HISTORICAL_YEARS) ){

      warning('L1321.cement: Last year of Andrew cement data is less than the max historical year. Carrying forward data. This data is regularly updated - can likely be updated in gcamdata.')

      Andrew_cement_production_duplicate <- Andrew_cement_production %>%
        filter(Year == Andrew_last_year)
      Andrew_cement_production_add <- do.call(bind_rows, replicate(n = 3, Andrew_cement_production_duplicate, simplify = F) )
      Andrew_cement_production_add$Year <- ADDITIONAL_YEARS
      Andrew_cement_production <- Andrew_cement_production %>%
        bind_rows(Andrew_cement_production_add)

      Andrew_cement_emissions_duplicate <- Andrew_cement_emissions %>%
        filter(Year == Andrew_last_year)
      Andrew_cement_emissions_add <- do.call(bind_rows, replicate(n = 3, Andrew_cement_emissionsn_duplicate, simplify = F) )
      Andrew_cement_emissions_add$Year <- ADDITIONAL_YEARS
      Andrew_cement_emissions <- Andrew_cement_emissions %>%
        bind_rows(Andrew_cement_emissions_add)
    }

    # =======================================================================================
    # Cement Production by Country
    # =======================================================================================

    Andrew_cement_production %>%
      gather(iso, value, -Year) %>%
      mutate(iso = tolower(iso)) %>%
      left_join(Andrew_iso_correction, by = c('iso')) %>%  #many NA matching on purpose
      mutate(iso_correction = if_else(iso == "sdn (729)", 'sdn',iso_correction),  #manually fix for now - the mapping file isn't working
             iso = if_else(!is.na(iso_correction), iso_correction, iso)) %>% #correct iso with correction mapping
      select(-iso_correction) %>%
      mutate(value = as.numeric(value)) %>%
      dplyr::rename(year = Year) %>%
      replace_na(list(value = 0)) %>%
      group_by(iso, year) %>%
      summarize(value = sum(value)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(value = value*CONV_KT_MT, #convert from kt to Mt
             sector = 'cement') %>%
      left_join(iso_GCAM_regID, by = c('iso')) -> L1321.out_Mt_C_cement_Yh_region_check
    # Use left join because there are some isos (very small islands etc) not mapped to GCAM regions
    # all these regions have zero cement production so not dropping cement production to NAs

    # Check for unmatched/dropped cement production
    L1321.out_Mt_C_cement_Yh_region_check %>%
      filter(is.na(GCAM_region_ID)) %>%
      pull(value) %>% sum(na.rm = TRUE) -> cement_check

    if( cement_check > 0 ) warning(paste( "L1321.cement: dropped cement production, look at Andrew cement iso mapping corrections. Dropped cement production is ",cement_check," MT"))

    L1321.out_Mt_C_cement_Yh_region_check %>%
      filter(!is.na(GCAM_region_ID)) %>% #drop the isos that aren't mapped
      select(iso, year, sector, value) %>%
      ungroup() -> L1321.out_Mt_C_cement_Yh # fix this mapping issue later

    # =======================================================================================
    # Cement Emissions by Country
    # =======================================================================================

    # cement in kt CO2
    # need kt C for limestone conversion
    # by country
    Andrew_cement_emissions %>%
      mutate(iso = tolower(ISO),
             iso = if_else(iso == 'ksv', 'xkx',iso)) %>% #correct wrong iso code in Andrew data
      select(-`UN code`,-`Name`,-ISO) %>%
      gather(year, value, -iso) %>%
      mutate(year = as.numeric(year)) %>%
      replace_na(list(value = 0)) %>%
      group_by(iso, year) %>%
      summarize(value = sum(value)) %>%
      #convert ktCO2 to ktC,  ktC to MtC
      mutate(value = value / emissions.CONV_C_CO2 / 1000) %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L1321.CO2_Mt_C_F_Yh

    # =======================================================================================
    # Cement Production and Cement Emissions - align estimates and aggregate by region
    # =======================================================================================
    # Andrew's cement production and emissions by country and year have some inconsistencies which are resolved here.
    # A large number of countries/years, particularly in Western Africa, have production with zero or very low emissions.
    # This is because these countries import clinker and grind it with additives to "produce" cement locally.
    # Conversely, countries/years with clinker exports will report disproportionately high CO2 emissions per production

    # The method below assigns minimum (EFmin) and maximum (EFmax) thresholds of emissions factors, and applies as follows:
    # 1) For observations with EF < EFmin, production = CO2 / EFmin
    # 2) For observations with EF > EFmax, production = CO2 / EFmax

    # Initial estimate of emissions factors (EF) in tonnes carbon per tonne cement (tCtc)
    L1321.out_Mt_C_cement_Yh %>%
      left_join(L1321.CO2_Mt_C_F_Yh, by = c('iso', 'year'), suffix = c(".production", ".emissions")) %>%
      mutate(EF = value.emissions / value.production) ->
      L1321.EF_tCtc_C_Yh_init

    # Assign EFmin and EFmax as the 1st and 9th deciles, respectively
    EFmin <- quantile(L1321.EF_tCtc_C_Yh_init$EF, probs = 0.1, na.rm = T)
    EFmean <- quantile(L1321.EF_tCtc_C_Yh_init$EF, probs = 0.5, na.rm = T)
    EFmax <- quantile(L1321.EF_tCtc_C_Yh_init$EF, probs = 0.9, na.rm = T)

    # First, missing values of emissions are set to 0 (this needs to be done after computing deciles), and EFs recomputed
    # Second, EFs are re-set where production is zero
    # Third, production is re-set where EFs are out of bounds
    L1321.EF_tCtc_C_Yh_init %>%
      replace_na(list(value.emissions = 0)) %>%
      mutate(EF = value.emissions / value.production,
             EF = if_else(value.production == 0 & value.emissions != 0, EFmax, EF),
             EF = if_else(value.production == 0 & value.emissions == 0, EFmean, EF),
             value.production = if_else(EF < EFmin, value.emissions / EFmin, value.production),
             value.production = if_else(EF > EFmax, value.emissions / EFmax, value.production)) ->
      L1321.EF_tCtc_C_Yh_corrected

    # Generate the consistent (corrected) data tables of production and CO2 emissions
    L1321.EF_tCtc_C_Yh_corrected %>%
      select(iso, year, sector, value = value.emissions) -> L1321.CO2_Mt_C_F_Yh_corrected
    L1321.EF_tCtc_C_Yh_corrected %>%
      select(iso, year, sector, value = value.production) -> L1321.out_Mt_C_cement_Yh_corrected


    # Aggregate cement production and emissions
    # by GCAM Region
    L1321.CO2_Mt_C_F_Yh_corrected %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = c('iso')) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()  -> L1321.CO2_Mt_R_F_Yh

    # Cement production by gcam region
    L1321.out_Mt_C_cement_Yh_corrected %>%
      left_join_error_no_match(iso_GCAM_regID, by = c('iso'))  %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarize_if (is.numeric, sum,na.rm = T) %>%
      filter(!is.na(GCAM_region_ID)) %>%
      select(GCAM_region_ID, sector, year, value) %>%
      ungroup() -> L1321.out_Mt_R_cement_Yh

    # =======================================================================================
    # Limestone
    # =======================================================================================

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
      select(-in.value)  %>%
      mutate(fuel = 'limestone') ->
      L1321.IO_Cement_R_limestone_Yh

    # =======================================================================================
    # Energy IO for Cement
    # =======================================================================================
    # Derive energy inputs to cement production by region and historical year -
    # Calculate average electric and TPE intensity for each GCAM region (use process emissions as a weighting factor)
    # ---------------------------------------------------------------------------------------------------------------

    # Merge cement electricity data (new and old data have different countries - need to merge and extrapolate)

    # Calculate average increase from 1971 to 2018 weighted by cement production - to back fill countries with no data
    elec_change_average_1971_to_2018 <-  IEA_cement_elec_kwht %>%
      full_join(IEA_cement_elec_kwht_update, by = c('Country')) %>%
      unique %>% # join the new and old data
      #select just those with 2010 and 2018 values so can calculate average
      filter(!is.na(`1971`), !is.na(`2018`)) %>%
      #join in 2018 cement production to weight the average
      left_join_error_no_match( L1321.out_Mt_C_cement_Yh_corrected %>%
                                  filter(year == 2018) %>%
                                  left_join(cement_regions %>% unique, by = c('iso')) %>%
                                  mutate(Country = IEA_intensity_region) %>%
                                  select(Country, value) %>%
                                  filter(!is.na(value)), by = c('Country')) %>%
      mutate(increase = `1971`/`2018`) %>%
      filter(Country != 'China') %>% #China seems to be dragging the efficiency increase waaaaay up so remove
      filter(Country != 'India') %>%
      # calculate the weighted average change from 2010 to 2018
      mutate(total_value = sum(value),
             weighted_increase = increase*value/total_value) %>%
      pull(weighted_increase) %>% sum()

    # Merge all elec data and fill in extrapolation from above
    IEA_cement_elec_kwht_merged <-  IEA_cement_elec_kwht %>%
      full_join(IEA_cement_elec_kwht_update, by = c('Country')) %>%
      filter(! is.na(`2018`)) %>%
      unique %>%
      # extrapolate back to 1971
      mutate(`1971`= if_else(is.na(`1971`),`2018`*elec_change_average_1971_to_2018,`1971`),
             `2021`=`2018`)

    # Interpolate available data on electricity intensity to all historical years
    IEA_cement_elec_kwht_merged %>%
      gather_years() %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) * CONV_KWH_GJ / CONV_T_KG) %>%
      ungroup() %>%
      unique() -> L1321.IEA_cement_elec_intensity

    # Calculate average change in clinker thermal energy production
    cement_thermal_efficiency_improvement <- IEA_cement_thermal_energy_GJt_clinker_global_trend %>%
      pivot_longer(!c(iso, fuel),names_to = 'year') %>%
      filter(year %in% c(2010, 2021)) %>%
      group_by(iso, year) %>%
      summarize(value = sum(value)) %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(improvement = `2021`/`2010`) %>%
      ungroup() %>%
      pull(improvement)

    # Interpolate available data on total primary energy intensity to all historical years by region
    IEA_cement_TPE_GJt %>%
      mutate(`2021` = `2010`*cement_thermal_efficiency_improvement) %>%
      gather_years() %>%
      complete(nesting(Country), year = c(year, HISTORICAL_YEARS)) %>%
      arrange(Country, year) %>%
      group_by(Country) %>%
      mutate(value = approx_fun(year, value, rule = 1) / CONV_T_KG) %>%
      ungroup() -> L1321.IEA_cement_TPE_intensity

    # Derive fuel shares from IEA data
    # ---------------------------------------------------------------------------------------------------------------
    # Calculate Fuel Shares
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
      filter(sector == 'cement') %>%
      mutate(fuel = if_else(str_detect(fuel, 'gas'),'gas', fuel),
             fuel = if_else(str_detect(fuel, 'refined'),'oil', fuel),
             fuel = if_else(str_detect(fuel, 'bio'),'biomass', fuel)) %>%
      filter(fuel %in% c('gas','oil','biomass','coal')) %>%
      group_by(iso, GCAM_region_ID, sector, fuel, year) %>%
      summarize(value = sum(value, na.rm = TRUE)) %>%
      spread(fuel, value) %>%
      replace_na(list(gas = 0, biomass = 0, oil = 0, coal = 0)) %>%
      mutate(fuel_total = gas+oil+biomass+coal,
             biomass = biomass/fuel_total,
             gas = gas/fuel_total,
             oil = oil/fuel_total,
             coal = coal/fuel_total) %>%
      select(-fuel_total) %>%
      replace_na(list(gas = 0, biomass = 0, oil = 0, coal = 0)) %>%
      mutate(total = biomass+oil+gas+coal,  # if zero fuel then 100% to coal
             coal = if_else(total == 0, 1, coal)) %>%
      select(-total) -> L1321.IEA_cement_fuel_share #This is missing some countries from the other data - need to fill in later


    # Calculate the average electricity generation efficiencies by region to be added to L1321.Cement_ALL_ctry_Yh
    # -----------------------------------------------------------------------------------------------------------

    # Calculate average regional input energy for electricity across all fuels
    L123.in_EJ_R_elec_F_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(in.value = sum(value)) %>%
      ungroup() ->
      L1321.in_EJ_R_elec_Yh

    # Calculate average regional output energy for electricity across all fuels
    # matching input,
    # join to input energy, and
    # calculate the IO coefficient
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

    # Start with Cement production MT
    L1321.out_Mt_C_cement_Yh_corrected %>%
      select(iso, year, value) %>%
      rename(prod_Mt = value) %>%
      # Match in region IDs by iso code
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # add region names for elec intensity and TPE intensity
      left_join_error_no_match(cement_regions, by = "iso") %>%
      # add above calculated energy intensities
      left_join_error_no_match(L1321.IEA_cement_elec_intensity, by = c("IEA_intensity_region" = "Country", "year")) %>%
      left_join(L1321.IEA_cement_TPE_intensity, by = c("IEA_TPE_intensity_region" = "Country", "year")) %>%
      rename(elec_GJkg = value.x, TPE_GJkg = value.y) %>%
      # remove unneeded columns from various left_joins
      select(iso, year, GCAM_region_ID, prod_Mt, IEA_intensity_region, elec_GJkg, TPE_GJkg) %>%
      # Match in IO coefficients by region and year
      left_join_error_no_match(L1321.IO_R_elec_Yh, by = c("GCAM_region_ID", "year")) %>%
      rename(IOelec = value) %>%
      # Match in fuelshares of (by default) coal, gas, oil, and biomass
      left_join(L1321.IEA_cement_fuel_share , by = c('iso', 'year', 'GCAM_region_ID')) %>% #Fuel shares are incomplete - so there will be NAs - correct below
      replace_na(list(gas = 0, biomass = 0, oil = 0, coal = 0, prod_Mt = 0)) %>%
      mutate(coal = if_else(gas+oil+biomass+coal == 0, 1, coal), #if no fuel share - default coal to 100%
      # Calculate heat intensity of energy and electricity, as well as total heat for each
             heat_GJkg = TPE_GJkg - elec_GJkg * IOelec,
             heat_EJ = heat_GJkg * prod_Mt,
             elec_EJ = elec_GJkg * prod_Mt,
      # Calculate total heat by fuel using fuelshares
             Coal_EJ = coal * heat_EJ,
             Oil_EJ = oil * heat_EJ,
             Gas_EJ = gas * heat_EJ,
             Biomass_EJ = biomass * heat_EJ) ->
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
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(heat_GJkg = heat_EJ / prod_Mt, elec_GJkg = elec_EJ / prod_Mt) ->
      L1321.Cement_ALL_R_Yh

    # Separate regional electricity and heat coefficients, first removing all unneeded columns
    L1321.Cement_ALL_R_Yh %>%
      select(GCAM_region_ID, year, elec_GJkg, heat_GJkg) ->
      L1321.Cement_ALL_R_Yh_base

    # Copy final year value to any historical period values not contained in the data set

    if(! length(ADDITIONAL_YEARS) ){

      L1321.Cement_ALL_R_Yh_base  ->
        L1321.IO_Cement_GJkg_R_ALL_Yh

    }else{
      L1321.Cement_ALL_R_Yh_base %>%
        filter(year == FINAL_CO2_YEAR) %>%
        rename(old.year = year) %>%
        repeat_add_columns(tibble(year = ADDITIONAL_YEARS)) %>%
        select(-old.year) %>%
        bind_rows(L1321.Cement_ALL_R_Yh_base) ->
        L1321.IO_Cement_GJkg_R_ALL_Yh }

    # Assign sector and fuel names for heat and electricity data
    L1321.IO_Cement_GJkg_R_ALL_Yh %>%
      select(GCAM_region_ID, year, elec_GJkg) %>%
      mutate(sector = "cement", fuel = "electricity") %>%
      rename(value = elec_GJkg) %>%
      unique ->
      L1321.IO_Cement_GJkg_R_elec_Yh

    L1321.IO_Cement_GJkg_R_ALL_Yh %>%
      select(GCAM_region_ID, year, heat_GJkg) %>%
      mutate(sector = "cement", fuel = "heat") %>%
      rename(value = heat_GJkg) %>%
      unique ->
      L1321.IO_Cement_GJkg_R_heat_Yh

    # Compile electricity, heat, and limestone IO coefficients in L1321.IO_GJkg_R_cement_F_Yh
    L1321.IO_Cement_GJkg_R_elec_Yh %>%
      bind_rows(L1321.IO_Cement_GJkg_R_heat_Yh) %>%
      bind_rows(L1321.IO_Cement_R_limestone_Yh)  ->
      L1321.IO_GJkg_R_cement_F_Yh

    # Calculate input energy for cement production by region, fuel, and year for L1321.in_EJ_R_cement_F_Y
    # ---------------------------------------------------------------------------------------------------

    # Compile regional historical data on energy use for cement by fuel
    L1321.Cement_ALL_R_Yh %>%
      select(GCAM_region_ID, year, elec_EJ, Coal_EJ, Oil_EJ, Gas_EJ, Biomass_EJ) %>%
      tidyr::gather(fuel, value, elec_EJ, Coal_EJ, Oil_EJ, Gas_EJ, Biomass_EJ) %>%
      # Match fuel names to GCAM, removing _EJ and replacing oil and elec
      mutate(sector = "cement", fuel = tolower(gsub("_EJ", "", fuel)),
             fuel = gsub("elec", "electricity", fuel),
             fuel = gsub("oil", "refined liquids", fuel)) ->
      L1321.in_EJ_R_cement_F_Y_base

    # Copy final year value to any historical period values not contained in the data set
    if(! length(ADDITIONAL_YEARS) ){
      L1321.in_EJ_R_cement_F_Y_base ->
        L1321.in_EJ_R_cement_F_Y  } else{
          L1321.in_EJ_R_cement_F_Y_base %>%
            filter(year == FINAL_CO2_YEAR) %>%
            rename(old.year = year) %>%
            repeat_add_columns(tibble(year = ADDITIONAL_YEARS)) %>%
            select(-old.year) %>%
            bind_rows(L1321.in_EJ_R_cement_F_Y_base) %>%
            unique ->
            L1321.in_EJ_R_cement_F_Y }

    # ---------------------------------------------------------------------------------------------------------------------
    ## 7/30/21: Modification for detailed industry
    ## Manually adjust coal use in South Korea, so there is enough left for iron and steel sector (do not delete until detailed industry is restructured)
    ## Upper bound is IEA coal consumption in non-metallic minerals

    L1321.in_EJ_R_cement_F_Y %>%
      left_join(L1012.en_bal_EJ_R_Si_Fi_Yh %>% filter(sector == "cement") %>% select(-sector),
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
      select(-value.x, -value.y, -neg) %>%
      unique ->
      L1321.in_EJ_R_cement_F_Y

    # ---------------------------------------------------------------------------------------------------------------------
    # Check calculated cement energy and compare to IEA non metallic energy use. If IEA is larger, then warn and replace.
    L1321.in_EJ_R_cement_F_Y %>%
      left_join( L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
                   filter(sector == 'cement') %>%
                   group_by(GCAM_region_ID, sector, fuel, year) %>%
                   summarize(value = sum(value) ) ,
                 by = c("GCAM_region_ID", "year", "fuel", "sector"),
                 suffix = c(".new", ".iea") ) %>%
      mutate(check = if_else(value.new > value.iea, 1,0)) %>%
      filter(check == 1 ) %>%
      select( -value.new, -value.iea) %>%
      pivot_wider(names_from = year, values_from = check) %>%
      arrange(GCAM_region_ID) -> L1321.in_EJ_R_cement_adjustment_warning

    if( nrow(L1321.in_EJ_R_cement_adjustment_warning)){
      warning("module_energy_L1321.cement.R: Estimated cement energy is larger than IEA non metallic mineral energy in ", nrow(L1321.in_EJ_R_cement_adjustment_warning), " rows.
      Replaced with IEA non metallic energy estimate. Replaced rows in L1321.in_EJ_R_cement_adjustment_warning.")
    }

    L1321.in_EJ_R_cement_F_Y %>%
      left_join( L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
                   filter(sector == 'cement') %>%
                   group_by(GCAM_region_ID, sector, fuel, year) %>%
                   summarize(value = sum(value) ) ,
                 by = c("GCAM_region_ID", "year", "fuel", "sector"),
                 suffix = c(".new", ".iea") ) %>%
      mutate(value = if_else(value.new > value.iea, value.iea, value.new)) %>%
      select(-value.new, -value.iea) %>%
      unique  -> L1321.in_EJ_R_cement_F_Y_adj

    # rename adjustment
    L1321.in_EJ_R_cement_F_Y_adj <- L1321.in_EJ_R_cement_F_Y

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
      bind_rows(filter(L1321.in_EJ_R_indenergy_F_Yh_negbio, value >= 0 | fuel != "biomass")) %>%
      unique ->
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
      filter(value < 0) %>%
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
    L1321.in_EJ_R_indenergy_F_Yh <- L1321.in_EJ_R_indenergy_F_Yh %>% unique

    # Add in the adjustments to the table of cement energy consumption.
    L1321.in_EJ_R_cement_F_Y %>%
      bind_rows(L1321.cement_adj_neg) %>%
      bind_rows(L1321.cement_adj_pos) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L1321.in_EJ_R_cement_F_Y

    # ===================================================
    # For Now zero out small negative cement values - these come from industry. Jira issue 512
    # remove this when this issue gets fixed
    L1321.in_EJ_R_cement_F_Y <- L1321.in_EJ_R_cement_F_Y %>%
      mutate(value = if_else(value <0, 0, value))

    # ===================================================
    # Produce outputs

    L1321.out_Mt_R_cement_Yh %>%
      add_title("Historical cement outputs by region, fuel, and year") %>%
      add_units("Mt cement") %>%
      add_comments("Outputs are calculated by by downscaling Worrell regions using CDIAC country emissions and then aggregating to GCAM regions") %>%
      add_comments("Final outputs are a product of regional emissions times the production emissions ratio") %>%
      add_legacy_name("L1321.out_Mt_R_cement_Yh") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef", "energy/mappings/Andrew_iso_correction", "energy/Andrew_cement_production", "emissions/Andrew_cement_emissions", "L102.CO2_Mt_R_F_Yh") ->
      L1321.out_Mt_R_cement_Yh

    L1321.IO_GJkg_R_cement_F_Yh %>%
      add_title("Input-output coefficients for cement production") %>%
      add_units("GJ/kg cement") %>%
      add_comments("IO coefficients for heat energy, electricity, and limestone consumption are calculated from weighted IEA fuel shares, CDIAC emissions data, and Worrell cement production") %>%
      add_legacy_name("L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef", "L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh",
                     "energy/IEA_cement_elec_kwht","energy/IEA_cement_elec_kwht_update",
                     "energy/IEA_cement_thermal_energy_GJt_clinker_global_trend", "energy/IEA_cement_TPE_GJt",  "common/iso_GCAM_regID") ->
      L1321.IO_GJkg_R_cement_F_Yh

    L1321.in_EJ_R_cement_F_Y %>%
      add_title("Historical input energy use for the cement sector") %>%
      add_units("Exajoules") %>%
      add_comments("Input energy by fuel calculated from weighted fuel shares using energy intensity values for heat and electricity") %>%
      add_comments("Multiplied by raw fuel shares, all from IEA") %>%
      add_legacy_name("L1321.in_EJ_R_cement_F_Y") %>%
      add_precursors("L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "energy/IEA_cement_elec_kwht",
                     "energy/IEA_cement_thermal_energy_GJt_clinker_global_trend", "energy/mappings/cement_regions",
                     "energy/IEA_cement_TPE_GJt",  "common/iso_GCAM_regID", "L1012.en_bal_EJ_R_Si_Fi_Yh") ->
      L1321.in_EJ_R_cement_F_Y

    L1321.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted cement energy use from industrial energy use values in L132.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1321.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L101.en_bal_EJ_ctry_Si_Fi_Yh_full", "L102.CO2_Mt_R_F_Yh", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "energy/IEA_cement_elec_kwht",
                     "energy/IEA_cement_thermal_energy_GJt_clinker_global_trend", "energy/mappings/cement_regions",
                     "energy/IEA_cement_elec_kwht_update", "energy/IEA_cement_TPE_GJt",  "L132.in_EJ_R_indenergy_F_Yh", "common/iso_GCAM_regID") ->
      L1321.in_EJ_R_indenergy_F_Yh

    return_data(L1321.out_Mt_R_cement_Yh, L1321.IO_GJkg_R_cement_F_Yh, L1321.in_EJ_R_cement_F_Y, L1321.in_EJ_R_indenergy_F_Yh)
  } else {
    stop("Unknown command")
  }
}
