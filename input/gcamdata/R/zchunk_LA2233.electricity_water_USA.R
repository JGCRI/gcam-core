#' module_gcamusa_LA2233.electricity_water_USA
#'
#' Weighted water coefficient for reference scenario and load segment classification
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.StubTech_WaterCoef_ref}
#' The corresponding file in the
#' original data system was \code{LA2233.electricity_water_USA} (gcam-usa level2)
#' @details Weighted water coefficient for reference scenario and load segment classification
#' @author Zarrar Khan September 2018
module_gcamusa_LA2233.electricity_water_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "water/A03.sector",
             FILE = "gcam-usa/A23.elecS_inttech_mapping",
             FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire",
             FILE = "gcam-usa/A23.elecS_tech_availability",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             "L223.StubTechMarket_elec_USA",
             "L1233.wdraw_coef_R_elec_F_tech_Yh_ref",
             "L1233.wcons_coef_R_elec_F_tech_Yh_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.StubTech_WaterCoef_ref"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region<- supplysector <- subsector <-  technology <- year <- minicam.energy.input <-
      coefficient<- market.name <- Electric.sector <- Electric.sector.intermittent.technology <-
      intermittent.technology <- subsector_1 <- sector <- fuel <- Electric.sector.technology <-
      water_sector <- water_type <- state <- state_name <- ':=' <- x <- plant_type <-
      State <- value <- NULL

    # ===================================================
    # 1. Read files

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A23.elecS_inttech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_inttech_mapping")
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping")
    A23.elec_tech_mapping_coal_retire <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_coal_retire")%>%
      filter(grepl("generation", Electric.sector))
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    L223.StubTechMarket_elec_USA <- get_data(all_data, "L223.StubTechMarket_elec_USA")
    L1233.wdraw_coef_R_elec_F_tech_Yh_ref <- get_data(all_data, "L1233.wdraw_coef_R_elec_F_tech_Yh_ref")
    L1233.wcons_coef_R_elec_F_tech_Yh_ref <- get_data(all_data, "L1233.wcons_coef_R_elec_F_tech_Yh_ref")


  # -----------------------------------------------------------------------------
    # 2. Perform computations
    # There are several segment / technology combinations that we think do not make sense.  These combinations are
    # outlined in A23.elecS_tech_availability.  To avoid creating these technologies and then deleting them (which
    # eats up memory and causes a lot of error messages), we remove them from the relevant association files here.

    L2233.load_segments <- unique(A23.elecS_inttech_mapping$Electric.sector)

    A23.elecS_tech_mapping %>%
      anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
      mutate(Electric.sector = as.character(factor(Electric.sector, levels = L2233.load_segments))) %>%
      arrange(subsector, Electric.sector) -> A23.elecS_tech_mapping_Edit

    A23.elecS_inttech_mapping %>%
      anti_join(A23.elecS_tech_availability, by = c("Electric.sector.intermittent.technology" = "stub.technology")) %>%
      mutate(Electric.sector = as.character(factor(Electric.sector, levels = L2233.load_segments))) %>%
      arrange(subsector, Electric.sector) %>%
      rename(Electric.sector.technology = Electric.sector.intermittent.technology,
             technology = intermittent.technology)%>%
      bind_rows(A23.elecS_tech_mapping_Edit)%>%
      rename(sector = supplysector,
             fuel = subsector_1)%>%
      dplyr::select(sector, fuel, technology, Electric.sector, subsector, Electric.sector.technology) ->
      elec_tech_water_map

    # Added elec water coefficient in here

    # Ref scenario

    L1233.wdraw_coef_R_elec_F_tech_Yh_ref %>%
      mutate(fuel = gsub("solar CSP", "solar", fuel),
             fuel = gsub("solar PV", "solar", fuel),
             water_sector = "Electricity",
             water_type = "water withdrawals",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector)) ->
      L2233.StubTech_WaterCoef_ref.wdraw

    L1233.wcons_coef_R_elec_F_tech_Yh_ref %>%
      mutate(fuel = gsub("solar CSP", "solar", fuel),
             fuel = gsub("solar PV", "solar", fuel),
             water_sector = "Electricity",
             water_type = "water consumption",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector)) ->
      L2233.StubTech_WaterCoef_ref.wcons

    # Bind wdraw and wcons and add missing years.
    bind_rows(L2233.StubTech_WaterCoef_ref.wdraw, L2233.StubTech_WaterCoef_ref.wcons) ->
      L2233.StubTech_WaterCoef_ref_temp


    # Indicate states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      filter (State != "TOTAL") %>%
      left_join_error_no_match(states_subregions %>%
                               dplyr::select(state, state_name) %>%
                               unique %>% rename(State = state_name), by = c("State")) ->
      NREL_us_re_technical_potential

    geo_states_noresource <- tibble(region = gcamusa.STATES[gcamusa.STATES %in% NREL_us_re_technical_potential$state[NREL_us_re_technical_potential$Geothermal_Hydrothermal_GWh==0]],
                                    subsector = "geothermal")


    years_to_create <- MODEL_YEARS[!MODEL_YEARS %in% names(L2233.StubTech_WaterCoef_ref_temp)]
    L2233.StubTech_WaterCoef_ref_temp[, years_to_create] <- NA_real_

    # Interpolate for missing years using approx_fun and map elec_tech_water_map technologies
    L2233.StubTech_WaterCoef_ref_temp %>%
      gather(key = year, value = value, -state, -fuel, -plant_type, -technology, -water_sector, -water_type, -minicam.energy.input) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(state, fuel, plant_type, technology, water_sector, water_type, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 2),
             market.name = gcamusa.DEFAULT_MARKET) %>%
      ungroup() %>%
      # Mapping water coefficients to electricity sector technologies
      inner_join(unique(elec_tech_water_map[c("sector", "fuel", "technology", "Electric.sector", "Electric.sector.technology")]),
           by = c("fuel", "technology")) %>%
      dplyr::select(region = state,
                    supplysector = Electric.sector,
                    subsector = fuel,
                    technology = Electric.sector.technology,
                    year,
                    minicam.energy.input,
                    coefficient,
                    market.name) %>%
      anti_join(geo_states_noresource, by = c("region", "subsector")) ->   # Removing states which do not have geothermal technologies
      L2233.StubTech_WaterCoef_ref

    L2233.StubTech_WaterCoef_ref %>%
      inner_join(A23.elec_tech_mapping_coal_retire, by = c("subsector", "technology")) %>%
      mutate(technology = Electric.sector.technology,
             Electric.sector = NULL,
             Electric.sector.technology = NULL) ->
    L2233.StubTech_WaterCoef_ref_coal_split

    # Remove duplicate entries and sort data
    L2233.StubTech_WaterCoef_ref %>%
      filter(!technology %in% c("coal_base_conv pul", "coal_int_conv pul",
                                "coal_peak_conv pul",
                                "coal_subpeak_conv pul")) %>%
      bind_rows(L2233.StubTech_WaterCoef_ref_coal_split) %>%
      dplyr::select(region, supplysector, subsector, technology, year, minicam.energy.input, coefficient, market.name) %>%
      arrange(region, year) %>%
      rename(stub.technology = technology) ->
      L2233.StubTech_WaterCoef_ref

    # ===================================================
    # Outputs

    L2233.StubTech_WaterCoef_ref %>%
      add_title("Weighted water coefficients for reference scenario and load segment classification") %>%
      add_units("none") %>%
      add_comments("Generated using zchunk_LA2233.electricity_water_USA") %>%
      add_legacy_name("L2233.StubTech_WaterCoef_ref") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "water/A03.sector",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L223.StubTechMarket_elec_USA",
                     "L1233.wdraw_coef_R_elec_F_tech_Yh_ref",
                     "L1233.wcons_coef_R_elec_F_tech_Yh_ref") ->
      L2233.StubTech_WaterCoef_ref

    return_data(L2233.StubTech_WaterCoef_ref)
  } else {
    stop("Unknown command")
  }
}
