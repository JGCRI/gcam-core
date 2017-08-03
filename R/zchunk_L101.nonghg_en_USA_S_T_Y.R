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
#' @details Compute historical emissions factors for energy (electricity, independent energy, buildings,
#' transportation, fertilizer production, cement, heating, fossil production) by GCAM technology, from EPA
#' emissions data and IEA energy balances.
#' For NH3, 1990 data are used for the 1971-1989 period because these data don't go back
#' as far as 1971, so we extrapolate in order to retain the capacity to run the model from
#' any historical year (i.e., any year 1971-2010), representing subsequent historical years
#'as future model time periods in order to check model performance against the observed data.
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
module_emissions_L101.nonghg_en_USA_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/EPA_tech",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L1322.Fert_Prod_MtN_R_F_Y",
             FILE = "temp-data-inject/L1321.in_EJ_R_cement_F_Y",
             "L124.in_EJ_R_heat_F_Yh",
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

    year <- value <- GCAM_region_ID <- sector <- fuel <- service <-
        size.class <- UCD_sector <- UCD_technology <- UCD_fuel <- technology <-
        EPA_agg_sector <- EPA_agg_fuel <- NULL # silence package check
    Source_Category_Raw <- Source_Category <- emissions <- energy <-
        em_factor <- sector_emissions <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    EPA_SO2 <- get_data(all_data, "emissions/EPA_SO2")
    EPA_CO <- get_data(all_data, "emissions/EPA_CO")
    EPA_NOx <- get_data(all_data, "emissions/EPA_NOx")
    EPA_VOC <- get_data(all_data, "emissions/EPA_VOC")
    EPA_NH3 <- get_data(all_data, "emissions/EPA_NH3")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh") %>%
      spread(year, value)

    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "temp-data-inject/L1321.in_EJ_R_cement_F_Y") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "temp-data-inject/L111.Prod_EJ_R_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # Add technology columns and combine all energy driver data into a single dataframe
    bind_rows(spread(L1322.in_EJ_R_indenergy_F_Yh, year, value),
              spread(L1322.Fert_Prod_MtN_R_F_Y, year, value),
              spread(L1321.in_EJ_R_cement_F_Y, year, value),
              spread(L124.in_EJ_R_heat_F_Yh, year, value),
              spread(L111.Prod_EJ_R_F_Yh, year, value),
              spread(L144.in_EJ_R_bld_serv_F_Yh, year, value) %>% rename(sector = service)) %>%
      mutate(technology = fuel) ->
      temp

    L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      mutate(technology = fuel,
             fuel = paste(mode, size.class, sep = "_"),
             sector = UCD_sector) %>%
      select(-mode, -UCD_sector, -size.class, -UCD_technology, -UCD_fuel) %>%

      # Bind all together
      bind_rows(temp,
                spread(L1231.in_EJ_R_elec_F_tech_Yh, year, value)) ->
      # NOTE we need to pass the L101.in_EJ_R_en_Si_F_Yh dataset on in WIDE, not
      # long, format, because it doesn't reshape cleanly (there are multiple year/row combinations)
      L101.in_EJ_R_en_Si_F_Yh

    L101.in_EJ_R_en_Si_F_Yh %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel, -technology) %>%
      mutate(year = as.integer(year)) ->
      L101.in_EJ_USA_en_Sepa_F_Yh.mlt

    # Subset for USA only and aggregate to EPA categories
    GCAM_sector_tech %>%
      select(EPA_agg_sector, EPA_agg_fuel, sector, fuel) %>%
      distinct(sector, fuel, .keep_all = TRUE) ->
      temp   # dataset we're about to merge below, replicating `match` behavior

    L101.in_EJ_USA_en_Sepa_F_Yh.mlt %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      left_join(temp, by = c("sector", "fuel")) %>%
      group_by(EPA_agg_sector, EPA_agg_fuel, year) %>%
      summarise(energy = sum(value)) %>%
      filter(!is.na(EPA_agg_sector)) ->
      L101.in_EJ_USA_en_Sepa_F_Yh.mlt

    # Convert EPA SO2, CO, NOx, VOC, and NH3 emissions inventories to Tg and aggregate by sector and technology
    # We do this for each gas, so define a function with all the steps
    EPA_convert_and_aggregate <- function(x, EPA_tech) {
      x %>%
        gather(year, value, -Source_Category_Raw, -Source_Category) %>%
        mutate(year = as.integer(year)) %>%
        left_join(distinct(EPA_tech), by = c("Source_Category" = "EPA_Category")) %>%
        filter(year %in% emissions.EPA_HISTORICAL_YEARS, !is.na(fuel)) %>%
        group_by(sector, fuel, year) %>%
        # summarise and convert to Tg
        summarise(value = sum(value) * emissions.TST_TO_TG) %>%
        # set missing values to zero
        replace_na(list(value = 0))
    }

    L101.so2_tg_USA_en_Sepa_F_Yh <- EPA_convert_and_aggregate(EPA_SO2, EPA_tech)
    L101.co_tg_USA_en_Sepa_F_Yh <- EPA_convert_and_aggregate(EPA_CO, EPA_tech)
    L101.nox_tg_USA_en_Sepa_F_Yh <- EPA_convert_and_aggregate(EPA_NOx, EPA_tech)
    L101.voc_tg_USA_en_Sepa_F_Yh <- EPA_convert_and_aggregate(EPA_VOC, EPA_tech)
    L101.nh3_tg_USA_en_Sepa_F_Yh <- EPA_convert_and_aggregate(EPA_NH3, EPA_tech)

    # Compute SO2, CO, NOx, VOC, and NH3 emissions factors by dividing EPA inventory by IEA energy balances
    # We do this for each gas, so define a function with all the steps
    EPA_compute_emissions_factors <- function(x, L101.in_EJ_USA_en_Sepa_F_Yh.mlt) {
      L101.in_EJ_USA_en_Sepa_F_Yh.mlt %>%
        left_join(x, by = c("EPA_agg_sector" = "sector", "EPA_agg_fuel" = "fuel", "year" = "year")) %>%
        rename(emissions = value) %>%
        replace_na(list(emissions = 0)) %>%
        mutate(em_factor = emissions / energy,
               em_factor = if_else(is.nan(em_factor) | is.infinite(em_factor), 0, em_factor)) ->
        x_em_factor

      # Compute sector emissions.
      # If these are zero, then reset all fuel em_factors to 1. Why? From Kate: I don't totally
      # remember, but I think there was a specific case where this was required. Essentially,
      # we will later scale these emissions to match EDGAR totals.
      # If there is a zero here, but positive emissions in EDGAR, then the scaling won't work.

      x_em_factor %>%
        group_by(EPA_agg_sector, year) %>%
        summarise(sector_emissions = sum(emissions)) ->
        x_sector_emissions

      x_em_factor %>%
        left_join_error_no_match(x_sector_emissions, by = c("EPA_agg_sector", "year")) %>%
        mutate(em_factor = if_else(sector_emissions == 0, 1, em_factor)) %>%
        rename(sector = EPA_agg_sector, fuel = EPA_agg_fuel) %>%
        select(-energy, -emissions, -sector_emissions)
    }

    # Run code above for each gas
    L101.so2_tgej_USA_en_Sepa_F_Yh <- EPA_compute_emissions_factors(L101.so2_tg_USA_en_Sepa_F_Yh, L101.in_EJ_USA_en_Sepa_F_Yh.mlt)
    L101.co_tgej_USA_en_Sepa_F_Yh <- EPA_compute_emissions_factors(L101.co_tg_USA_en_Sepa_F_Yh, L101.in_EJ_USA_en_Sepa_F_Yh.mlt)
    L101.nox_tgej_USA_en_Sepa_F_Yh <- EPA_compute_emissions_factors(L101.nox_tg_USA_en_Sepa_F_Yh, L101.in_EJ_USA_en_Sepa_F_Yh.mlt)
    L101.voc_tgej_USA_en_Sepa_F_Yh <- EPA_compute_emissions_factors(L101.voc_tg_USA_en_Sepa_F_Yh, L101.in_EJ_USA_en_Sepa_F_Yh.mlt)
    L101.nh3_tgej_USA_en_Sepa_F_Yh <- EPA_compute_emissions_factors(L101.nh3_tg_USA_en_Sepa_F_Yh, L101.in_EJ_USA_en_Sepa_F_Yh.mlt)

    # Use 1990 NH3 data for 1971-1989 because this dataset doesn't go back that far
    L101.nh3_tgej_USA_en_Sepa_F_Yh %>%
      filter(year %in% emissions.NH3_EXTRA_YEARS) %>%
      select(-em_factor, year) %>%
      left_join_error_no_match(select(filter(L101.nh3_tgej_USA_en_Sepa_F_Yh, year == 1990), -year), by = c("sector", "fuel")) %>%
      bind_rows(filter(L101.nh3_tgej_USA_en_Sepa_F_Yh, !year %in% emissions.NH3_EXTRA_YEARS)) ->
      L101.nh3_tgej_USA_en_Sepa_F_Yh

    # Produce outputs
    L101.in_EJ_R_en_Si_F_Yh %>%
      add_title("Energy consumption by region / GCAM sector / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Energy consumption (electricity, independent energy, buildings, transportation, fertilizer production, cement, heating, fossil production)") %>%
      add_legacy_name("L101.in_EJ_R_en_Si_F_Yh") %>%
      add_precursors("energy/mappings/IEA_flow_sector",
                     "energy/mappings/IEA_product_fuel",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/EPA_tech",
                     "L1231.in_EJ_R_elec_F_tech_Yh",
                     "L1322.in_EJ_R_indenergy_F_Yh",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
                     "L1322.Fert_Prod_MtN_R_F_Y",
                     "temp-data-inject/L1321.in_EJ_R_cement_F_Y",
                     "L124.in_EJ_R_heat_F_Yh",
                     "temp-data-inject/L111.Prod_EJ_R_F_Yh") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L101.in_EJ_R_en_Si_F_Yh

    L101.so2_tgej_USA_en_Sepa_F_Yh %>%
      rename(value = em_factor) %>%   # for testing system
      add_title("SO2 emissions factors for the USA by EPA sector / fuel / historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Matched to EPA categories, summed, converted to Tg") %>%
      add_legacy_name("L101.so2_tgej_USA_en_Sepa_F_Yh") %>%
      add_precursors("emissions/EPA_SO2") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.so2_tgej_USA_en_Sepa_F_Yh

    L101.co_tgej_USA_en_Sepa_F_Yh %>%
      rename(value = em_factor) %>%   # for testing system
      add_title("CO emissions factors for the USA by EPA sector / fuel / historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Matched to EPA categories, summed, converted to Tg") %>%
      add_legacy_name("L101.co_tgej_USA_en_Sepa_F_Yh") %>%
      same_precursors_as(L101.in_EJ_R_en_Si_F_Yh) %>%
      add_precursors("emissions/EPA_CO") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.co_tgej_USA_en_Sepa_F_Yh

    L101.nox_tgej_USA_en_Sepa_F_Yh %>%
      rename(value = em_factor) %>%   # for testing system
      add_title("NOx emissions factors for the USA by EPA sector / fuel / historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Matched to EPA categories, summed, converted to Tg") %>%
      add_legacy_name("L101.nox_tgej_USA_en_Sepa_F_Yh") %>%
      same_precursors_as(L101.in_EJ_R_en_Si_F_Yh) %>%
      add_precursors("emissions/EPA_NOx") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.nox_tgej_USA_en_Sepa_F_Yh

    L101.voc_tgej_USA_en_Sepa_F_Yh %>%
      rename(value = em_factor) %>%   # for testing system
      add_title("VOC emissions factors for the USA by EPA sector / fuel / historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Matched to EPA categories, summed, converted to Tg") %>%
      add_legacy_name("L101.voc_tgej_USA_en_Sepa_F_Yh") %>%
      same_precursors_as(L101.in_EJ_R_en_Si_F_Yh) %>%
      add_precursors("emissions/EPA_VOC") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.voc_tgej_USA_en_Sepa_F_Yh

    L101.nh3_tgej_USA_en_Sepa_F_Yh %>%
      rename(value = em_factor) %>%   # for testing system
      add_title("NH3 emissions factors for the USA by EPA sector / fuel / historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Matched to EPA categories, summed, converted to Tg") %>%
      add_legacy_name("L101.nh3_tgej_USA_en_Sepa_F_Yh") %>%
      same_precursors_as(L101.in_EJ_R_en_Si_F_Yh) %>%
      add_precursors("emissions/EPA_NH3") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.nh3_tgej_USA_en_Sepa_F_Yh

    return_data(L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh,
                L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh,
                L101.nh3_tgej_USA_en_Sepa_F_Yh, L101.in_EJ_R_en_Si_F_Yh)
  } else {
    stop("Unknown command")
  }
}
