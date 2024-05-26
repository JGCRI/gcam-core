# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1327.paper
#'
#' Sets up input, output, and IO coefficients for paper and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1327.in_EJ_R_paper_Yh}, \code{L1327.in_EJ_R_paper_Yh_tech}, \code{L1327.IO_GJkg_R_paper_F_Yh},\code{L1327.out_Mt_R_paper_Yh}, \code{L1327.in_EJ_R_indenergy_F_Yh}, \code{L127.in_EJ_R_indchp_F_Yh}. The corresponding file in the
#' @details This chunk generates input, Energy inputs are then subtracted from industrial energy use, feedstock and any resulting negative values
#' are dealt with by moving their accounting to the paper  sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author MMC July 2022
module_energy_L1327.paper <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1326.in_EJ_R_indenergy_F_Yh",
             FILE = "aglu/FAO/FAO_Paper_Prod_t_FORESTAT",
             FILE = "aglu/AGLU_ctry",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/A327.globaltech_coef",
             FILE = "energy/paper_food_TFE",
             FILE = "common/GCAM_region_names",
             "L123.in_EJ_R_indchp_F_Yh",
             FILE = "energy/A327.china_biomass",
             "L110.For_ALL_bm3_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1327.in_EJ_R_paper_F_Yh",
             "L1327.out_Mt_R_paper_Yh",
             "L1327.IO_GJkg_R_paper_F_Yh",
             "L1327.in_EJ_R_indenergy_F_Yh",
             "L1327.elec_noheat_adj_shwt_R",
             "L127.in_EJ_R_indchp_F_Yh",
             "L1327.IO_woodpulp_energy"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    value <- iso <- sector <- subsector <- GCAM_region_ID <- year <- fuel <- unit <-
      coef <- TFE_food <- TFE_paper <- paper_share <- paper_value <- coefficient <-
      calOutputValue <- GCAM_commodity <- biomass_EJ <- woodpulp_tons <- prod_tons <-
      industry <- has_district_heat <- region_heat <- en <- prod <- raw <- value_heat <-
      value.x <- value.y <- input <- output <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions",strip_attributes = TRUE)
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation", strip_attributes = TRUE)
    L1326.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1326.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    FAO_Paper_Prod_t_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_Paper_Prod_t_FORESTAT", strip_attributes = TRUE)
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry", strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A327.globaltech_coef <- get_data(all_data, "energy/A327.globaltech_coef", strip_attributes = TRUE)
    L123.in_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh", strip_attributes = TRUE)
    paper_food_TFE <- get_data(all_data, "energy/paper_food_TFE", strip_attributes = TRUE)
    china_biomass <- get_data(all_data, "energy/A327.china_biomass", strip_attributes = TRUE)
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y", strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # Paper  production - map to iso and aggregate to GCAM regions
    FAO_Paper_Prod_t_FORESTAT %>%
      gather(-c(countries, country.codes, item, item.codes, element, element.codes, unit), key = "year", value = "value") %>%
      mutate(value = replace_na(value, 0),
             year = as.integer(year),
             value = value * CONV_TON_MEGATON,
             unit = "Mt",
             sector = "paper") %>%
      left_join(AGLU_ctry %>% select(countries = FAO_country, iso), by = "countries") %>%
      left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      na.omit() ->
      L1327.out_Mt_R_paper_Yh


    # Get pulp and paper energy use from IEA energy balances
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("paper", sector)) ->
      L1327.in_EJ_R_paper_F_Yh

    # Map fuel used in paper sector
    L1327.in_EJ_R_paper_F_Yh %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()->
      L1327.in_EJ_R_paper_F_Yh

    # Drop heat in regions where heat is not modeled as a final fuel
    A_regions %>%
      filter(has_district_heat == 0) %>%
      select(GCAM_region_ID) %>%
      unique %>%
      mutate(fuel = "heat") ->
      region_heat

    L1327.in_EJ_R_paper_F_Yh  %>%
      anti_join(region_heat, by = c("GCAM_region_ID", "fuel")) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L1327.in_EJ_R_paper_F_Yh_nochp
    # This does not include energy use in CHP yet


    # --------------------------------------------------------------------------

    # Biomass CHP is important in pulp and paper industry, so we reallocate a portion of this from other industry sector
    # Fraction assigned to paper industry is share of paper TFE in paper + food production TFE, from IEA energy balances

    # get share by GCAM region
    paper_food_TFE %>%
      left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = 'iso') %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(TFE_food = sum(TFE_food),
                TFE_paper = sum(TFE_paper)) %>%
      ungroup() %>%
      mutate(paper_share = TFE_paper / (TFE_food + TFE_paper)) %>%
      # take average across years, in case there are inconsistencies in reporting
      group_by(GCAM_region_ID) %>%
      summarize(paper_share = mean(paper_share, na.rm=T)) ->
      paper_tfe_share

    # fill in all regions
    GCAM_region_names %>%
      select(GCAM_region_ID) %>%
      left_join(paper_tfe_share, by = "GCAM_region_ID") %>%
      mutate(paper_share = replace_na(paper_share, 0)) ->
      bio_cogen_paper_share

    # re-assign that share of biomass CHP to paper industry
    L123.in_EJ_R_indchp_F_Yh %>%
      filter(fuel == "biomass") %>%
      left_join(bio_cogen_paper_share, by = "GCAM_region_ID") %>%
      mutate(paper_value = value * paper_share,
             sector = "paper") %>%
      select(GCAM_region_ID, year, sector, fuel, value=paper_value) ->
      L1327.in_EJ_R_paper_chp_F_Yh

    # Add biomass cogen to paper energy use (L1327.in_EJ_R_paper_F_Yh)
    L1327.in_EJ_R_paper_F_Yh_nochp %>%
      bind_rows(L1327.in_EJ_R_paper_chp_F_Yh %>%
                  mutate(fuel = paste(fuel, "cogen"))) ->
      L1327.in_EJ_R_paper_F_Yh

    # subtract paper biomass cogen from other industry chp_elec inputs
    L123.in_EJ_R_indchp_F_Yh  %>%
      left_join(L1327.in_EJ_R_paper_chp_F_Yh %>% select(-sector), by = c("GCAM_region_ID", "year", "fuel")) %>%
      mutate(value.y = replace_na(value.y, 0),
             value = value.x - value.y) %>%
      select(-value.x, -value.y) ->
      L127.in_EJ_R_indchp_F_Yh


    # --------------------------------------------------------------------------

    # Manual adjustment for China - add biomass consumption to paper sector energy
    # Zero biomass is reported in IEA. We instead use values from literature
    china_biomass %>%
      gather(-c(fuel, iso,sector), key = "year", value = "value") %>%
      mutate(year = as.integer(year)) %>%
      left_join(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, fuel, sector, year) %>%
      summarize(value = sum(value)) %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      ungroup() ->
      L1327.china_biomass

    L1327.in_EJ_R_paper_F_Yh %>%
      bind_rows(L1327.china_biomass) ->
      L1327.in_EJ_R_paper_F_Yh


    # --------------------------------------------------------------------------
    # ADDITION FOR LINKAGE TO WOODPULP SECTOR

    US_biomass_coef = 0.0152263386604417

    L1327.IO_woodpulp_energy <- L110.For_ALL_bm3_R_Y %>%
      filter(grepl("pulp", GCAM_commodity)) %>%
      select(GCAM_region_ID, year, prod_tons = Prod_bm3, netExp_tons = NetExp_bm3, woodpulp_tons = Cons_bm3) %>% #correct unit labeling for clarity
      # Calculate IO coefficients for woodpulp to pulp energy (using IEA biomass in paper industry)
      left_join(L1327.in_EJ_R_paper_F_Yh %>%
                  filter(grepl("biomass", fuel)) %>%
                  group_by(GCAM_region_ID, year) %>%
                  summarize(biomass_EJ = sum(value)) %>%
                  ungroup(), by = c("GCAM_region_ID", "year")) %>%

      left_join(L1327.out_Mt_R_paper_Yh %>% rename(paper_prod=value) %>% select(-sector), by = c("GCAM_region_ID","year")) %>%
      mutate(paper_prod=if_else(is.na(paper_prod),0,paper_prod),
      ## Manual adjustment for Africa_Northern - fix extremely high coefficient by setting biomass to zero (will be replaced with default value)
             biomass_EJ = if_else(GCAM_region_ID == 3, 0, biomass_EJ),

             coefficient = woodpulp_tons / biomass_EJ)

    # Where reported biomass is 0, replace Inf coefficient with global median,
    # scaled by share of woodpulp produced domestically (to estimate black liquor availability).
    # If production is also 0, replace with global max as default value
    # Then estimate biomass use from coefficient, since 0 consumption is probably not realistic.
    DEFAULT_PULP_ENERGY_IO <- L1327.IO_woodpulp_energy %>%
      filter(biomass_EJ > 0) %>% # Filter for numeric values only
      filter(coefficient<3) %>%
      summarize(median(coefficient)) %>%
      pull

    # Recalculate coefficients with NA replacements
    L1327.IO_woodpulp_energy %>%
      mutate(coefficient = if_else(biomass_EJ > 0, woodpulp_tons / biomass_EJ,
                                   if_else(paper_prod > 0, woodpulp_tons/(paper_prod*US_biomass_coef),
                                           0)),
             calOutputValue = if_else(coefficient > 0, woodpulp_tons / coefficient, 0), # recalculate biomass based on coefficient
             GCAM_commodity = "woodpulp_energy") ->
      L1327.IO_woodpulp_energy


    # filter for countries/years where biomass was estimated, and add this back into pulp and paper energy
    L1327.IO_woodpulp_energy %>%
      filter(biomass_EJ == 0) %>%
      select(GCAM_region_ID, year, value=calOutputValue)  %>%
      mutate(fuel = "biomass",
             sector = "paper") ->
      pulp_energy_added

    L1327.IO_woodpulp_energy %>%
      select(GCAM_region_ID, year, GCAM_commodity, coefficient, calOutputValue) ->
      L1327.IO_woodpulp_energy


    L1327.in_EJ_R_paper_F_Yh %>%
      bind_rows(pulp_energy_added) %>%
      group_by(GCAM_region_ID, year, fuel, sector) %>%
      summarize(value = sum(value)) %>%
      ungroup ->
      L1327.in_EJ_R_paper_F_Yh

    # --------------------------------------------------------------------------

    # Check for regions/years with energy use but no production
    L1327.in_EJ_R_paper_F_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(en = sum(value)) %>%
      filter(en > 0) %>%
      ungroup ->
      L1327.in_EJ_R_paper_F_Yh_total

    L1327.out_Mt_R_paper_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(prod = sum(value)) %>%
      filter(prod > 0) %>%
      ungroup ->
      L1327.out_Mt_R_paper_Yh_total

    L1327.in_EJ_R_paper_F_Yh_total %>%
      left_join(L1327.out_Mt_R_paper_Yh_total, by = c("GCAM_region_ID", "year")) %>%
      filter(is.na(prod)) %>%
      select(GCAM_region_ID, year) ->
      rm_en

    # Remove energy use from regions with no paper production
    L1327.in_EJ_R_paper_F_Yh %>%
      anti_join(rm_en, by = c("GCAM_region_ID", "year")) ->
      L1327.in_EJ_R_paper_F_Yh


    # Check for regions/years with production but no energy use
    L1327.out_Mt_R_paper_Yh_total %>%
      left_join(L1327.in_EJ_R_paper_F_Yh_total, by = c("GCAM_region_ID", "year")) %>%
      filter(is.na(en),
             year %in% unique(L1327.in_EJ_R_paper_F_Yh_total$year)) %>%
      select(GCAM_region_ID, year) ->
      rm_prod

    # Remove production from regions with no energy use reported
    L1327.out_Mt_R_paper_Yh %>%
      anti_join(rm_prod, by = c("GCAM_region_ID", "year")) ->
      L1327.out_Mt_R_paper_Yh


    # Check for regions with electricity and estimated biomass use but no other fuels
    L1327.in_EJ_R_paper_F_Yh %>%
      mutate(input = if_else(fuel == 'electricity', 'electricity',
                             if_else(fuel == "biomass", "biomass", 'heat'))) %>%
      select(-fuel) %>%
      group_by(GCAM_region_ID, sector, year, input) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(value > 0) %>%
      spread(input, value) %>%
      filter(is.na(heat)) %>%
      select(GCAM_region_ID, sector, year) ->
      L1327.elec_noheat_adj_shwt_R
    # This will be used to adjust subsector share weights
    # allows non-existent process heat techs to come in after base year to avoid solution errors

    #Calculate remaining industrial energy use
    L1326.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID, nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      left_join(L1327.in_EJ_R_paper_F_Yh %>% select(-sector), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1327.in_EJ_R_indenergy_F_Yh_tmp

    # Check for negative energy use in remaining industry
    L1327.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0,
             !fuel == "biomass") -> # Leave biomass alone, as this won't cause calibration issues
      indeergy_tmp


    # Adjust negative energy use in remaining industry by adjusting energy in paper sector downward
    L1327.in_EJ_R_paper_F_Yh %>%
      left_join(indeergy_tmp %>% select(-sector), by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(value.y = replace_na(value.y, 0), value = value.x + value.y) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1327.in_EJ_R_paper_F_Yh_recal


    # Recalculate remaining industrial energy
    L1326.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1327.in_EJ_R_paper_F_Yh_recal %>% select(-sector), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value,
             # replace negative biomass values with 0 in remaining industrial energy
             value = if_else(value < 0, 0, value),
             raw = NULL) ->
      L1327.in_EJ_R_indenergy_F_Yh

    # Replace energy use in paper industry with adjusted version
    L1327.in_EJ_R_paper_F_Yh_recal ->
      L1327.in_EJ_R_paper_F_Yh

    # --------------------------------------------------------------------------

    # Calculate IO coefficient for heat, based on coefficients by tech

    # interpolate global tech coefficients by tech to all years
    A327.globaltech_coef %>%
      select(technology) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, FUTURE_YEARS))) %>%
      left_join(A327.globaltech_coef %>%
                  gather(-c(supplysector, subsector, technology, minicam.energy.input, secondary.output) , key = 'year', value = 'coef') %>%
                  select(technology, year, coef) %>% mutate(year = as.numeric(year)),
                by = c("technology", "year")) %>%
      group_by(technology) %>%
      mutate(coef = approx_fun(year, coef, rule=2)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      distinct() %>%
      na.omit() %>%
      ungroup() ->
      L1327.globaltech_coef_process_heat

    process_heat_techs <- unique(A327.globaltech_coef %>% filter(grepl("process heat", supplysector)))$technology
    biomass_techs <- unique(A327.globaltech_coef %>% filter(grepl("biomass", supplysector)))$technology
    electric_techs <- unique(A327.globaltech_coef %>% filter(grepl("electric", technology)))$technology

    # Merge with energy inputs by fuel, and calculate process heat output using global tech coefficients
    L1327.in_EJ_R_paper_F_Yh %>%
      rename(technology = fuel) %>%
      filter(technology %in% process_heat_techs,
             !technology %in% electric_techs) %>% # assume no electricity in heat production in historical years
      left_join_error_no_match(L1327.globaltech_coef_process_heat, by = c("technology", "year")) %>%
      mutate(value_heat = value / coef) %>%
      group_by(GCAM_region_ID, year, sector) %>%
      summarise(value = sum(value_heat)) %>%
      ungroup() %>%
      mutate(fuel = "heat") -> #all non-electric and non-biomass fuels get assigned to process heat
      L1327.total_process_heat_calculated

    # Calculate total biomass energy
    L1327.in_EJ_R_paper_F_Yh %>%
      mutate(technology = fuel) %>%
      filter(technology %in% biomass_techs) %>% # want coefficients for biomass/woodpulp only
      left_join_error_no_match(L1327.globaltech_coef_process_heat, by = c("technology", "year")) %>%
      mutate(value_heat = value / coef) %>%
      group_by(GCAM_region_ID, year, sector) %>%
      summarise(value = sum(value_heat)) %>%
      ungroup() %>%
      mutate(fuel = "waste biomass for paper") ->
      L1327.total_woodpulp_energy_calculated


    L1327.in_EJ_R_paper_F_Yh %>%
      filter(fuel == "electricity") %>%
      bind_rows(L1327.total_process_heat_calculated,
                L1327.total_woodpulp_energy_calculated) %>%
      rename(input = value) %>%
      left_join(L1327.out_Mt_R_paper_Yh %>% rename(output = value), by = c("GCAM_region_ID", "year", "sector")) %>%
      mutate(output = replace_na(output, 0),
             value = if_else(output > 0, input / output, 0)) ->
      L1327.IO_GJkg_R_paper_F_Yh


    # =======================================================
    # Produce outputs
    L1327.in_EJ_R_paper_F_Yh %>%
      add_title("Historical input energy use for the paper sector") %>%
      add_units("EJ") %>%
      add_comments("PAPERPRO sector from IEA energy balances aggregated to GCAM regions") %>%
      add_legacy_name("L1327.in_EJ_R_paper_F_Yh") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A_regions",
                     "common/iso_GCAM_regID", "energy/mappings/enduse_fuel_aggregation",
                     "L123.in_EJ_R_indchp_F_Yh", "energy/paper_food_TFE", "energy/A327.china_biomass") ->
      L1327.in_EJ_R_paper_F_Yh

    L1327.out_Mt_R_paper_Yh %>%
      add_title("Historical paper production by region, fuel, and year") %>%
      add_units("Mt") %>%
      add_comments("Output by country from FAO forestry statistics and aggregated to GCAM regions") %>%
      add_legacy_name("L1327.out_Mt_R_paper_Yh") %>%
      add_precursors( "aglu/FAO/FAO_Paper_Prod_t_FORESTAT", "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L1327.out_Mt_R_paper_Yh

    L1327.IO_GJkg_R_paper_F_Yh %>%
      add_title("Input-output coefficients for paper production") %>%
      add_units("GJ/kg paper") %>%
      add_legacy_name("L1327.IO_GJkg_R_paper_F_Yh") %>%
      add_comments("IO coefficients for heat energy and electricity are calculated from IEA energy consumption and FAO paper production data") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh", "aglu/FAO/FAO_Paper_Prod_t_FORESTAT", "aglu/AGLU_ctry", "energy/A_regions",
                     "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation", "L123.in_EJ_R_indchp_F_Yh",
                     "energy/paper_food_TFE", "energy/A327.globaltech_coef", "energy/A327.china_biomass") ->
      L1327.IO_GJkg_R_paper_F_Yh

    L1327.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted pulp and paper energy use from industrial energy use values in L1326.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1327.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh", "L1326.in_EJ_R_indenergy_F_Yh", "aglu/FAO/FAO_Paper_Prod_t_FORESTAT",
                     "aglu/AGLU_ctry", "energy/A_regions", "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation") ->
      L1327.in_EJ_R_indenergy_F_Yh

    L1327.elec_noheat_adj_shwt_R %>%
      add_title("Regions with electricity use but no fuel use in paper production historical years") %>%
      add_units("None") %>%
      add_comments("Subsector share weights for process heat technologies will be reset from 0 to 1 for these regions") %>%
      add_legacy_name("L1327.elec_noheat_adj_shwt_R") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A_regions",
                     "common/iso_GCAM_regID","energy/mappings/enduse_fuel_aggregation") ->
      L1327.elec_noheat_adj_shwt_R

    L127.in_EJ_R_indchp_F_Yh %>%
      add_title("Other industry inputs to industrial CHP by GCAM region / fuel / historical year") %>%
      add_units("None") %>%
      add_comments("Assumed biomass cogen in paper industry subtracted from L123.in_EJ_R_indchp_F_Yh") %>%
      add_legacy_name("L127.in_EJ_R_indchp_F_Yh") %>%
      add_precursors("L123.in_EJ_R_indchp_F_Yh", "energy/paper_food_TFE",
                     "energy/A_regions", "common/iso_GCAM_regID") ->
      L127.in_EJ_R_indchp_F_Yh

    L1327.IO_woodpulp_energy %>%
      add_title("Woodpulp to woodpulp_energy IO coefficients by GCAM region / fuel / historical year") %>%
      add_units("GJ/kg") %>%
      add_comments("Woodpulp in Mt divided by paper sector biomass energy in EJ") %>%
      add_legacy_name("L1327.IO_woodpulp_energy") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/A_regions", "common/GCAM_region_names",
                     "common/iso_GCAM_regID", "energy/mappings/enduse_fuel_aggregation",
                     "L123.in_EJ_R_indchp_F_Yh", "energy/paper_food_TFE", "energy/A327.china_biomass",
                     "L110.For_ALL_bm3_R_Y") ->
      L1327.IO_woodpulp_energy

    return_data(L1327.in_EJ_R_paper_F_Yh, L1327.out_Mt_R_paper_Yh, L1327.IO_GJkg_R_paper_F_Yh, L1327.in_EJ_R_indenergy_F_Yh, L1327.elec_noheat_adj_shwt_R, L127.in_EJ_R_indchp_F_Yh, L1327.IO_woodpulp_energy)

  } else {
    stop("Unknown command")
  }
}

