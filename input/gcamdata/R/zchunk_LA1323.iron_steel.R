# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1323.iron_steel
#'
#' Sets up input, output, and IO coefficients for iron and steel and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1323.out_Mt_R_iron_steel_Yh}, \code{L1323.IO_GJkg_R_iron_steel_F_Yh}, \code{L1323.in_EJ_R_iron_steel_F_Y}, \code{L1323.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' original data system was \code{LA1323.iron_steel.R} (energy level1).
#' @details This chunk generates input, output, and IO coefficients for the iron and steel sector. It begins by downscaling Worrell regional data from 1994
#' to set up process emissions factors that are multiplied by country emissions from CDIAC to determine production. Limestone consumption is calculated from the same downscaling.
#' IEA fuelshares and heat and electricity are used to determine energy use by fuel. Energy inputs are then subtracted from industrial energy use and any resulting negative values
#' are dealt with by moving their accounting to the iron and steel sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019
module_energy_LA1323.iron_steel <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/steel_prod",
             FILE = "energy/steel_intensity",
             FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1323.out_Mt_R_iron_steel_Yh",
             "L1323.IO_GJkg_R_iron_steel_F_Yh",
             "L1323.in_EJ_R_iron_steel_F_Y",
             "L1323.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
      share <- value <- iron_steel <- year <- value.y <- value.x <- iso <- unit_prod <-
      GCAM_region_ID <- fuel <- industry <- output <- energy_use <- scalar <- coefficient <-
      Unit <- technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    All_steel <- get_data(all_data, "energy/steel_prod")
    steel_intensity <- get_data(all_data, "energy/steel_intensity", strip_attributes = TRUE)
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")


    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================
    # Change steel production to long format and aggregate to region level
	All_steel %>%
      gather(subsector, value, -iso, -unit_prod, -year) %>%
      #Unit: kt to Mt
      mutate(value = value * CONV_KT_MT) %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, year,subsector, value) ->
      L1323.out_Mt_R_iron_steel_Yh

    # Get steel energy use from IEA energy balances
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("steel", sector)) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "iron and steel") ->
      en_steel

    # Map fuel in iron and steel sector
    en_steel %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      en_steel

    # Calculate bottom-up energy consumption = production * intensity from literature
    L1323.out_Mt_R_iron_steel_Yh %>%
      rename(output = value) %>%
      left_join(steel_intensity %>% select(-subsector), by = c("subsector"="technology")) %>%
      mutate(value = value * CONV_GJ_EJ / CONV_T_MT,
                    energy_use = output * value,
                    unit = "EJ") ->
      Intensity_literature

    # Scaler: IEA's estimates of fuel consumption divided by bottom-up estimate of energy consumption
    Intensity_literature %>%
      group_by(GCAM_region_ID, year, fuel) %>%
      dplyr::summarise(energy_use = sum(energy_use)) %>%
      ungroup() %>%
      left_join(en_steel %>% select(GCAM_region_ID, year, fuel, value),by = c("GCAM_region_ID","fuel",  "year")) %>%
      mutate(value = replace_na(value,0),
             scalar = replace_na(value / energy_use, 1),
             scalar = if_else(energy_use == 0 & value > 0, 1, scalar)) ->
      Scaler

    # Intensity scaled = Intensity from the literature times scaler.
    Intensity_literature %>%
      left_join(Scaler %>% select(GCAM_region_ID, year, fuel, scalar),by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(coefficient = value * scalar) %>%
      select(GCAM_region_ID, year, subsector, fuel, coefficient, Unit) ->
      Intensity_scaled

    IO_iron_steel <- steel_intensity %>%
      select(subsector, technology, fuel) %>%
      distinct() %>%
      mutate(sector = "iron and steel") %>%
      repeat_add_columns(select(iso_GCAM_regID, GCAM_region_ID) %>% distinct(GCAM_region_ID)) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      left_join(Intensity_scaled, by = c("GCAM_region_ID", "year", "subsector", "fuel")) %>%
      na.omit


     # Use IO to calculate energy input
    L1323.out_Mt_R_iron_steel_Yh %>%
      mutate(technology = subsector) %>%
      left_join(IO_iron_steel, by = c("subsector","technology","year","GCAM_region_ID")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, supplysector = "sector", year, subsector, technology, fuel, "value") ->
      L1323.in_EJ_R_iron_steel_F_Y

	  IO_iron_steel %>%
      select(GCAM_region_ID, year, supplysector = "sector", subsector, technology, fuel, coefficient) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

	# Subtract iron and steel energy use from other industrial energy use
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value,0),
             value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      L1323.in_EJ_R_indenergy_F_Yh

    #Adjust negative energy use

    # Identify rows with negative energy use
    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      select(-sector) ->
      negative

    # revise IO coefficients to zero for rows with negative energy use
    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      left_join(negative,by = c("GCAM_region_ID", "year", "fuel")) %>%
      mutate(coefficient = if_else(replace_na(value, 0) < 0, 0, coefficient),value = NULL) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    #Recalculate

    # Recalculate the input steel energy with revised IO coefficients
    L1323.out_Mt_R_iron_steel_Yh %>%
      # 10/11/2019 gpk modification: in order to avoid assigning output (and energy consumption) to technologies that do
      # not exist in the base years, we specify a "technology" column which is equal to the subsector. Note that this
      # method assumes that the techs with market share in the base years have the same name as their parent subsectors
      mutate(technology = subsector) %>%
      left_join(L1323.IO_GJkg_R_iron_steel_F_Yh, by = c("subsector", "technology", "GCAM_region_ID", "year")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, year, subsector, technology, fuel, value) ->
      L1323.in_EJ_R_iron_steel_F_Y

    # Redo the iron and steel energy use and other industrial energy use subtraction
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      replace_na(list(value = 0)) %>%
      mutate(value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh

    # ===================================================
    # Produce outputs

    L1323.out_Mt_R_iron_steel_Yh %>%
      add_title("Historical steel outputs by region, fuel, and year") %>%
      add_units("Mt iron_steel") %>%
      add_comments("Outputs are collecting from World steel association and then aggregating to GCAM regions") %>%
      add_legacy_name("L1323.out_Mt_R_iron_steel_Yh") %>%
      add_precursors( "energy/steel_prod", "common/iso_GCAM_regID") ->
      L1323.out_Mt_R_iron_steel_Yh

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      add_title("Input-output coefficients for steel production") %>%
      add_units("GJ/kg steel") %>%
      add_comments("IO coefficients for steel") %>%
      add_legacy_name("L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_precursors( "energy/steel_prod", "energy/steel_intensity", "L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    L1323.in_EJ_R_iron_steel_F_Y %>%
      add_title("Historical input energy use for the iron and steel sector") %>%
      add_units("Exajoules") %>%
      add_comments("Calculated by steel production and IO coefficients") %>%
      add_legacy_name("L1323.in_EJ_R_iron_steel_F_Y") %>%
      add_precursors("energy/steel_prod","energy/steel_intensity", "L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1323.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted iron and steel energy use from industrial energy use values in L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1323.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1322.in_EJ_R_indenergy_F_Yh", "energy/steel_prod", "L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_indenergy_F_Yh

    return_data(L1323.out_Mt_R_iron_steel_Yh, L1323.IO_GJkg_R_iron_steel_F_Yh, L1323.in_EJ_R_iron_steel_F_Y, L1323.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}

