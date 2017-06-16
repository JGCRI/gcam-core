#' module_energy_LA1231.elec_tech
#'
#' This chunk obtaines inputs, outputs and efficiencies in the electricity sector for all technologies by region, sector, and year.
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1231.in_EJ_R_elec_F_tech_Yh}, \code{L1231.out_EJ_R_elec_F_tech_Yh}, \code{L1231.eff_R_elec_F_tech_Yh}. The corresponding file in the
#' original data system was \code{LA1231.elec_tech.R} (energy level1).
#' @details This chunk obtaines inputs, outputs and efficiencies in the electricity sector for all technologies by region, sector, and year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author FF, May 2017

module_energy_LA1231.elec_tech<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A23.globaltech_eff",
             FILE = "energy/calibrated_techs",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L123.eff_R_elec_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L1231.eff_R_elec_F_tech_Yh"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    subsector <- supplysector <- technology <- minicam.energy.input <- fuel <-
        value <- year <- efficiency_tech <- improvement.max <- improvement.rate <- NULL
    improvement.shadow.technology <- efficiency_tech.x <- efficiency_tech.y <-
        efficiency <- efficiency_tech1 <- efficiency_tech2 <- GCAM_region_ID <- NULL
    sector <- share_tech1 <- in_EJ <- in_EJ_tech1 <- in_EJ_tech2 <- output <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L123.eff_R_elec_F_Yh <- get_data(all_data, "L123.eff_R_elec_F_Yh")

    # ===================================================
    # Obtaining supply sector, gas technologies, minicam.energy.input and creating template to be used to create L1231.eff_R_elec_gas_tech
    calibrated_techs %>%
      filter(subsector == "gas")%>%
      filter(supplysector == "electricity")%>%
      distinct(supplysector, technology, minicam.energy.input) -> energy.GAS_TECH

    # Obtain unique gas technologies to avoid hard coding technologies later in the code
    # NOTE: It is assumed that there are 2 gas technologies only. If new gas technologies are added in the future, then this chunk has to be updated.
    calibrated_techs %>%
      filter(subsector == "gas")%>%
      filter(supplysector == "electricity")%>%
      distinct(technology) -> energy.GAS_TECH_CONST

    # GAS_TECH1: gas steam/CT
    energy.GAS_TECH_CONST$technology[1] -> GAS_TECH1

    # GAS_TECH2: gas CC
    energy.GAS_TECH_CONST$technology[2] -> GAS_TECH2

    # Natural gas: Disaggregate to CC and CT/steam on the basis of assumed efficiencies
    # Subset the actual efficiencies of gas -> electricity
    L123.eff_R_elec_F_Yh %>%
      filter(fuel == "gas") %>%
      rename(efficiency = value) -> L1231.eff_R_elec_gas_Yh_gathered

    # Create auxiliar tibble to perform interpolation (aprox_fun) on efficiencies for natural gas technologies
    # Interpolation is needed since input data is only provided for some intermediate years in the range 1971-2010.
    # Therefore there is not efficiency values for every historical year. Efficiencies for all historical years are needed
    # To estimate outputs from fuel/technology by region in the electricity sector
    tibble(supplysector = "electricity", subsector="gas", year = HISTORICAL_YEARS) %>%
      mutate(year = as.numeric(year)) %>%
      full_join(energy.GAS_TECH, by = "supplysector") -> Aux_gas_tech_elec

    # Perform interpolation for gas technologies efficiencies , max, and rate
    # Interpolation is needed since efficiencies are given for 1971, 1990, 2005 and 2010 only. Not for every historical year.
    A23.globaltech_eff %>%
      filter(subsector == "gas") %>%
      semi_join(calibrated_techs, by = c("supplysector", "subsector", "technology")) %>%
      gather(year, efficiency_tech, -supplysector, -subsector, -technology, -minicam.energy.input, -improvement.max, -improvement.rate, -improvement.shadow.technology) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(efficiency_tech = as.numeric(efficiency_tech)) %>%
      semi_join(Aux_gas_tech_elec, by = "year") %>%
      full_join(Aux_gas_tech_elec, by = c("supplysector", "subsector", "technology","minicam.energy.input","year")) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(efficiency_tech = approx_fun(year, efficiency_tech)) %>%
      mutate(improvement.max = approx_fun(year, improvement.max)) %>%
      mutate(improvement.rate = approx_fun(year, improvement.rate)) -> L1231.eff_R_elec_gas_tech

    # Reset upper and lower bound efficiencies, as needed
    # Where avg efficiency is outside of the range of the two technologies, set the lower bound equal to the average, and the upper equal to
    # the average plus a small adjustment. This adjustment avoids assigning 100% of market share to combined-cycle.
    # NOTE: this is complicated. Heat output is not allowed for combined cycle power plants--it is assumed that CHP plants are all single-cycle.
    # However if a region has very high efficiencies of CC power plants that bring the weighted average higher than our assumed CC efficiency (e.g. Turkey),
    # this could set the market share of single-cycle power plants to 0 and therefore zero out any heat production from these technologies.
    # Adding this small adjustment factor to the efficiencies avoids this outcome:
    # Small adjustment factor for elec efficienies (explaining its need in the comment above)
    ELEC_ADJ <- 0.03

    L1231.eff_R_elec_gas_Yh_gathered %>%
      full_join(select(filter(L1231.eff_R_elec_gas_tech, technology == GAS_TECH1), year,efficiency_tech, supplysector, subsector, technology),by="year") %>%
      select(-supplysector, -subsector, -technology) %>%
      left_join(select(filter(L1231.eff_R_elec_gas_tech, technology == GAS_TECH2), year, efficiency_tech, supplysector, subsector, technology), by = "year") %>%
      select(-supplysector, -subsector, -technology) %>%
      rename(efficiency_tech1 = efficiency_tech.x, efficiency_tech2 = efficiency_tech.y) %>%
      # performing adjustments here
      mutate(efficiency_tech1 = if_else(efficiency < efficiency_tech1, efficiency, efficiency_tech1)) %>%
      mutate(efficiency_tech2 = if_else(efficiency > efficiency_tech2, efficiency + ELEC_ADJ, efficiency_tech2)) -> L1231.eff_R_elec_gas_Yh_gathered

    # Create tibble for gas (steam/CT) that is going to be binded with gas(CC) to create output for gas technologies
    L1231.eff_R_elec_gas_Yh_gathered %>%
      select(-efficiency_tech2, -efficiency) %>%
      mutate(technology = GAS_TECH1) %>%
      rename(efficiency = efficiency_tech1) -> L1231.eff_R_elec_gas_tech1_Yh_gathered

    # Combined both gas technologies (efficiencies) in long format
    L1231.eff_R_elec_gas_Yh_gathered %>%
      select(-efficiency_tech1, -efficiency) %>%
      mutate(technology = GAS_TECH2) %>%
      rename(efficiency = efficiency_tech2) %>%
      bind_rows(L1231.eff_R_elec_gas_tech1_Yh_gathered) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, efficiency) -> L1231.eff_R_elec_gas_tech_Yh

    # Solve for share of first technology to be used to estimate gas technology inputs
    L1231.eff_R_elec_gas_Yh_gathered %>%
      mutate(share_tech1 = (efficiency - efficiency_tech2) / ( efficiency_tech1 - efficiency_tech2 )) -> L1231.eff_R_elec_gas_Yh_gathered

    # Use share obtained above to get inputs of gas by technology
    L1231.eff_R_elec_gas_Yh_gathered %>%
      select(GCAM_region_ID, sector, fuel, year, share_tech1) %>%
      full_join(filter(L123.in_EJ_R_elec_F_Yh, fuel == "gas"),by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      rename(in_EJ = value) %>%
      mutate(in_EJ_tech1=in_EJ*share_tech1) %>%
      mutate(in_EJ_tech2=in_EJ - in_EJ_tech1) -> L1231.in_EJ_R_elec_gas_Yh_gathered

    # Combined gas technologies (inputs) in long format
    L1231.in_EJ_R_elec_gas_Yh_gathered %>%
      select(-in_EJ, -share_tech1, -in_EJ_tech2) %>%
      mutate(technology = GAS_TECH1) %>%
      rename(value = in_EJ_tech1) -> L1231.eff_R_elec_gas_tech1_Yh_gathered

    L1231.in_EJ_R_elec_gas_Yh_gathered %>%
      select(-in_EJ, -share_tech1, -in_EJ_tech1) %>%
      mutate(technology = GAS_TECH2) %>%
      rename(value = in_EJ_tech2) %>%
      bind_rows(L1231.eff_R_elec_gas_tech1_Yh_gathered) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, value) -> L1231.in_EJ_R_elec_gas_tech_Yh

    # Perform computations for technologies other than those for gas.
    # All other (non-gas) technologies are not disaggregated further (only one tech per fuel type)

    # Obtain efficiencies for technologies (other than gas technologies)
    L123.eff_R_elec_F_Yh %>%
      filter(fuel != "gas") %>%
      left_join(select(calibrated_techs, sector, fuel, technology), by = c("sector", "fuel")) %>%
      rename(efficiency = value) %>%
      bind_rows(L1231.eff_R_elec_gas_tech_Yh) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, efficiency) -> L1231.eff_R_elec_F_tech_Yh

    # Obtain inputs for technologies (other than gas technologies) and then create final input output by binding rows with the
    # tibble that considers gas technologies only.
    L123.in_EJ_R_elec_F_Yh %>%
      filter(fuel != "gas") %>%
      left_join(select(calibrated_techs, sector, fuel, technology), by = c("sector", "fuel")) %>%
      bind_rows(L1231.in_EJ_R_elec_gas_tech_Yh) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, value) -> L1231.in_EJ_R_elec_F_tech_Yh

    #Calculate output as input * efficiency
    #This tibble only includes technologies with modeled efficiencies
    L1231.eff_R_elec_F_tech_Yh %>%
      left_join(L1231.in_EJ_R_elec_F_tech_Yh, by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(output = efficiency*value) %>%
      select(-value, -efficiency) -> L1231.out_EJ_R_elec_Fin_tech_Yh

    # Rename efficiecy as value for final output in long format so that data will be reshaped correctly by testing code
    L1231.eff_R_elec_F_tech_Yh %>%
      rename(value = efficiency) -> L1231.eff_R_elec_F_tech_Yh

    # Combine outputs with technologies modeled by output only (e.g. nuclear, hydro, renewables)
    # and combined all outpues for all fuels and technologies to create final tibble in long format
    L123.out_EJ_R_elec_F_Yh %>%
    # Use anti_join to drop all fuels that are not modeled by output only and keep nuclear, hydro, and renewables
     anti_join(L1231.out_EJ_R_elec_Fin_tech_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      arrange(fuel, GCAM_region_ID,year) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, technology), by = c("sector", "fuel")) %>%
      rename(output = value) %>%
      bind_rows(L1231.out_EJ_R_elec_Fin_tech_Yh) %>%
      rename(value = output) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, value) %>%
      ungroup -> L1231.out_EJ_R_elec_F_tech_Yh

    # add final details to tibbles and save them
    L1231.in_EJ_R_elec_F_tech_Yh%>%
      add_title("Inputs to electricity by Region / fuel / technology.") %>%
      add_units("EJ") %>%
      add_comments("Written by LA1231.electech.R") %>%
      add_comments("Inputs are calculated based on L123.in_EJ_R_elec_F_Yh and calibrated_techs. For gas technologies, the share of each has was estimated") %>%
      add_legacy_name("L1231.in_EJ_R_elec_F_tech_Yh") %>%
      add_precursors("energy/A23.globaltech_eff", "energy/calibrated_techs", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.eff_R_elec_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1231.in_EJ_R_elec_F_tech_Yh
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      add_title("Outputs of electricity by by Region / fuel / technology.") %>%
      add_units("EJ") %>%
      add_comments("Written by LA1231.electech.R") %>%
      add_comments("Outputs by fuel and coresponding technologies are adjusted based on the efficiency and input estimates")%>%
      add_legacy_name("L1231.out_EJ_R_elec_F_tech_Yh") %>%
      add_precursors("energy/A23.globaltech_eff", "energy/calibrated_techs", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.eff_R_elec_F_Yh") %>%      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1231.out_EJ_R_elec_F_tech_Yh
    L1231.eff_R_elec_F_tech_Yh %>%
      add_title("Electricity efficiency by by Region / fuel / technology") %>%
      add_units("Unitless") %>%
      add_comments("Written by LA1231.electech.R") %>%
      add_comments("Efficiencies for technologies other than gas technologies are based on L123.eff_R_elec_F_Yh. For gas technologies, their efficiences were adjusted when the average efficiency was outside of the range of the two technologies") %>%
      add_legacy_name("L1231.eff_R_elec_F_tech_Yh") %>%
      add_precursors("energy/A23.globaltech_eff", "energy/calibrated_techs", "L123.in_EJ_R_elec_F_Yh", "L123.out_EJ_R_elec_F_Yh", "L123.eff_R_elec_F_Yh") %>%      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1231.eff_R_elec_F_tech_Yh

    return_data(L1231.in_EJ_R_elec_F_tech_Yh, L1231.out_EJ_R_elec_F_tech_Yh, L1231.eff_R_elec_F_tech_Yh)
  } else {
    stop("Unknown command")
  }
}
