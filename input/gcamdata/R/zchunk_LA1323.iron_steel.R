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
             FILE = "energy/IO_iron_steel_scaled",
             "L1322.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1323.out_Mt_R_iron_steel_Yh",
             "L1323.IO_GJkg_R_iron_steel_F_Yh",
             "L1323.in_EJ_R_iron_steel_F_Y",
             "L1323.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.MAKE) {


    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
    share <- value <- iron_steel <- year <- value.y <- value.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    All_steel <- get_data(all_data, "energy/steel_prod")
    IO_iron_steel <- get_data(all_data, "energy/IO_iron_steel_scaled", strip_attributes = TRUE)
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================
    # Change steel production to long format
	All_steel %>%
      gather(subsector, value, -region, -GCAM_region_ID,-unit_prod, -year) %>%
      #Unit: kt to Mt
      mutate(value = value/1000) %>%
      select(region,GCAM_region_ID,year,value,subsector) ->
      L1323.out_Mt_R_iron_steel_Yh

     # Use IO to calculate energy input
    L1323.out_Mt_R_iron_steel_Yh %>%
      mutate(technology = subsector) %>%
      left_join(IO_iron_steel %>% rename(sector = sector.name,subsector = subsector.name), by = c("subsector","technology","year","region","GCAM_region_ID")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, supplysector = "sector", year = "year",subsector, "technology", "minicam.energy.input", "value") ->
      L1323.in_EJ_R_iron_steel_F_Y

	  IO_iron_steel %>%
      select(region, GCAM_region_ID, year, supplysector = "sector.name", subsector = "subsector.name", technology, minicam.energy.input, coefficient) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

	#Calculate the remaining industrial energy use
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered coal", "coal"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="elect_td_ind", "electricity"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="refined liquids industrial", "refined liquids"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="wholesale gas", "gas"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered biomass", "biomass")) %>%
                  group_by(GCAM_region_ID, year, minicam.energy.input) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel" = "minicam.energy.input")) %>%
      ungroup() %>%
      mutate(value = replace_na(value,0)) %>%
      mutate(value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      L1323.in_EJ_R_indenergy_F_Yh

    #Adjust negative energy use
    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      mutate(fuel = replace(fuel, fuel == "coal", "delivered coal"),
             fuel = replace(fuel, fuel == "electricity", "elect_td_ind"),
             fuel = replace(fuel, fuel == "refined liquids" ,"refined liquids industrial"),
             fuel = replace(fuel, fuel == "gas" , "wholesale gas"),
             fuel = replace(fuel, fuel == "biomass" , "delivered biomass")) %>%
      select(-sector) ->
      negative

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      left_join(negative,by = c("GCAM_region_ID", "year", "minicam.energy.input" = "fuel")) %>%
      mutate(coefficient = if_else(replace_na(value, 0) < 0, 0, coefficient),value = NULL) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    #Recalculate
    L1323.out_Mt_R_iron_steel_Yh %>%
      # 10/11/2019 gpk modification: in order to avoid assigning output (and energy consumption) to technologies that do
      # not exist in the base years, we specify a "technology" column which is equal to the subsector. Note that this
      # method assumes that the techs with market share in the base years have the same name as their parent subsectors
      mutate(technology = subsector) %>%
      left_join(L1323.IO_GJkg_R_iron_steel_F_Yh , by = c("subsector", "technology", "GCAM_region_ID", "region", "year")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, year, subsector, technology, minicam.energy.input, value) ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered coal", "coal"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="elect_td_ind", "electricity"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="refined liquids industrial", "refined liquids"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="wholesale gas", "gas"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered biomass", "biomass")) %>%
                  group_by(GCAM_region_ID, year, minicam.energy.input) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", fuel = 'minicam.energy.input')) %>%
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
      add_precursors( "energy/steel_prod") ->
      L1323.out_Mt_R_iron_steel_Yh

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      add_title("Input-output coefficients for steel production") %>%
      add_units("GJ/kg steel") %>%
      add_comments("IO coefficients for steel") %>%
      add_legacy_name("L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_precursors( "energy/steel_prod", "energy/IO_iron_steel_scaled") ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    L1323.in_EJ_R_iron_steel_F_Y %>%
      add_title("Historical input energy use for the iron and steel sector") %>%
      add_units("Exajoules") %>%
      add_comments("Calculated by steel production and IO coefficients") %>%
      add_legacy_name("L1323.in_EJ_R_iron_steel_F_Y") %>%
      add_precursors("energy/steel_prod","energy/IO_iron_steel_scaled") ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1323.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted iron and steel energy use from industrial energy use values in L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1323.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1322.in_EJ_R_indenergy_F_Yh", "energy/steel_prod") ->
      L1323.in_EJ_R_indenergy_F_Yh

    return_data(L1323.out_Mt_R_iron_steel_Yh, L1323.IO_GJkg_R_iron_steel_F_Yh, L1323.in_EJ_R_iron_steel_F_Y, L1323.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}

