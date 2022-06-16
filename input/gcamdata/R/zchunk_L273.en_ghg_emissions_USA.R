# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L273.en_ghg_emissions_USA
#'
#' Define non-CO2 GHG emissions for GCAM-USA states, including 1. CH4 and N2O in refinery,
#' buildings, N fertilizer, industrial energy use; 2. F-gases in cooling and power
#' transmission; 3. MACs for F-gases
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.en_ghg_tech_coeff_USA}, \code{L273.en_ghg_emissions_USA}, \code{L273.out_ghg_emissions_USA},
#' and \code{L273.MAC_higwp_USA}. The corresponding file in the
#' original data system was \code{L273.en_ghg_emissions_USA.R} (gcam-usa level2).
#' @details KALYN MTB YO
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KRD 2018; MTB 2018; YO 2021

module_gcamusa_L273.en_ghg_emissions_USA <- function(command, ...) {
  UCD_tech_map_name <- if_else(energy.TRAN_UCD_MODE == 'rev.mode',
                               "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_techs")
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L123.out_EJ_state_ownuse_elec",
             "L1322.in_EJ_state_Fert_Yh",
             "L201.en_ghg_emissions",
             "L241.nonco2_tech_coeff",
             "L241.hfc_all",
             "L241.pfc_all",
             "L252.MAC_higwp",
             "L252.MAC_higwp_tc_average",
             "L252.MAC_higwp_phaseInTime",
             "L222.StubTech_en_USA",
             "L232.StubTechCalInput_indenergy_USA",
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.GlobalTechEff_bld",
             # the following files to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/A22.globaltech_input_driver",
             FILE = "energy/A23.globaltech_input_driver",
             FILE = "energy/A25.globaltech_input_driver",
             # the following to be able to map in the input.name to
             # use for the input-driver for res + ind
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/calibrated_techs_bld_usa",
             FILE = UCD_tech_map_name))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.en_ghg_tech_coeff_USA",
             "L273.en_ghg_emissions_USA",
             "L273.out_ghg_emissions_USA",
             "L273.MAC_higwp_USA",
             "L273.MAC_higwp_TC_USA",
             "L273.MAC_higwp_phaseInTime_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    CH4 <- GCAM_region_ID <- N2O <- Non.CO2 <- calibrated.value <- calibrated.value.x <-
      calibrated.value.y <- depresource <- efficiency <- elec_technology <- emiss.coef <-
      emiss.coeff <- fuel <- fuel_input <- fuel_input_share <- grid_region <-
      input.emissions <- keep <- mac.control <- mac.reduction <- market.name <-
      output.emissions <- palette <- region <- sector <- service_output <- service_output2 <-
      share <- state <- state_technology <- stub.technology <- subsector <- supplysector <-
      tax <- technology <- value <- value2 <- year <- minicam.energy.input <- tranSubsector <-
      tranTechnology <- tech.change.year <- tech.change <- mac.phase.in.time <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec", strip_attributes = TRUE)
    L1322.in_EJ_state_Fert_Yh <- get_data(all_data, "L1322.in_EJ_state_Fert_Yh", strip_attributes = TRUE)
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes = TRUE)
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff", strip_attributes = TRUE)
    L241.hfc_all <- get_data(all_data, "L241.hfc_all", strip_attributes = TRUE)
    L241.pfc_all <- get_data(all_data, "L241.pfc_all", strip_attributes = TRUE)
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp", strip_attributes = TRUE)
    L252.MAC_higwp_tc_average <- get_data(all_data, "L252.MAC_higwp_tc_average", strip_attributes = TRUE)
    L252.MAC_higwp_phaseInTime <- get_data(all_data, "L252.MAC_higwp_phaseInTime", strip_attributes = TRUE)
    L222.StubTech_en_USA <- get_data(all_data, "L222.StubTech_en_USA", strip_attributes = TRUE)
    L232.StubTechCalInput_indenergy_USA <- get_data(all_data, "L232.StubTechCalInput_indenergy_USA", strip_attributes = TRUE)
    L244.StubTechCalInput_bld_gcamusa <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa", strip_attributes = TRUE)
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld", strip_attributes = TRUE)

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver
    bind_rows(
      get_data(all_data, "energy/A22.globaltech_input_driver"),
      get_data(all_data, "energy/A23.globaltech_input_driver"),
      get_data(all_data, "energy/A25.globaltech_input_driver")
    ) %>%
      rename(stub.technology = technology) ->
      EnTechInputMap

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver. Filter for industrial sector as all others are
    # accounted for in previous EnTechInputMap
    bind_rows(
      get_data(all_data, "energy/calibrated_techs") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, "gcam-usa/calibrated_techs_bld_usa") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, UCD_tech_map_name) %>% select(supplysector, subsector = tranSubsector, technology = tranTechnology, minicam.energy.input)
    ) %>%
      rename(stub.technology = technology,
             input.name = minicam.energy.input) %>%
      distinct() ->
      EnTechInputNameMap

    # ===================================================

    # 2. Build tables for CSVs
    # Input Emissions coefficients
    # L273.en_ghg_tech_coeff_USA: GHG emissions coefficients for energy technologies in U.S. states
    # Write the USA coefficients for every state.
    ## Remove H2 production emissions for now because energy is not available on state level
    # Refining first:
    L241.nonco2_tech_coeff %>%
      filter(region == gcam.USA_REGION & Non.CO2 %in% emissions.GHG_NAMES & supplysector == "refining") ->
      L241.ref_ghg_tech_coeff_USA

    # Match the refining emission factors to the corresponding technologies in the states. Matching on subsector
    # and stub technology because the names of the sectors are different
    L222.StubTech_en_USA %>%
      repeat_add_columns(tibble("year" = unique(L241.ref_ghg_tech_coeff_USA$year))) %>%
      repeat_add_columns(tibble("Non.CO2" = unique(L241.ref_ghg_tech_coeff_USA$Non.CO2))) %>%
      # using left_join because there will be missing tech/year combinations. OK to omit
      left_join(L241.ref_ghg_tech_coeff_USA %>%
                  select("subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff"),
                by = c("subsector", "stub.technology", "year", "Non.CO2")) %>%

      na.omit %>%
      select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff") ->
      L273.ref_ghg_tech_coeff_USA

    # clean up refining emission coefficients and organize
    L273.ref_ghg_tech_coeff_USA %>%
      mutate(emiss.coeff = round(emiss.coeff, emissions.DIGITS_EMISSIONS)) %>%
      arrange(region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      left_join_error_no_match(EnTechInputMap %>% select(-supplysector), by = c("subsector", "stub.technology"))->
      L273.en_ghg_tech_coeff_USA

    # 2c. Input Emissions
    # L273.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state
    # Filter the emissions data into USA, and transportation is calibrated elsewhere
    L201.en_ghg_emissions %>%
      filter(region == gcam.USA_REGION & !grepl("trn",supplysector)) %>%
      spread(Non.CO2, input.emissions) %>%
      ###NOTE: emissions from coal use in commercial buildings "other" category does not have an equivalent representation
      #in the fifty state data. For now move these emissions over to comm heating
      mutate(supplysector = if_else(grepl("comm",supplysector) & subsector == "coal","comm heating", supplysector)) ->
      en_ghg_emissions_USA

    # Organize the state fuel input data
    # Fertilizer
    L1322.in_EJ_state_Fert_Yh %>%
      mutate(technology = fuel) %>%
      filter(year %in% en_ghg_emissions_USA$year) ->
      fert_fuel_input_state

    # Industry
    L232.StubTechCalInput_indenergy_USA %>%
      filter(year %in% en_ghg_emissions_USA$year) %>%
      # We do not expect at 1:1 match here because we are using the left join to subset for supplysector / subsector /
      # stub.tehcnology combinations in the data frame. Use a left_join_keep_first_only to preserve the old datasystem
      # behavior.
      left_join_keep_first_only(en_ghg_emissions_USA %>%
                                  select("supplysector", "subsector", "stub.technology") %>%
                                  mutate(keep = TRUE),
                                by = c("supplysector","subsector","stub.technology")) %>%
      filter(keep) %>%
      select(region, supplysector, subsector, stub.technology, year, calibrated.value) %>%
      rename(fuel_input = calibrated.value, sector = supplysector, fuel = subsector,
             technology = stub.technology, state = region) ->
      ind_fuel_input_state

    # Bind the fuel input tables into one
    ind_fuel_input_state %>%
      rename(value = fuel_input) %>%
      bind_rows(fert_fuel_input_state) ->
      fuel_input_state

    # Create aggregate fuel input table
    fuel_input_state %>%
      group_by(sector, fuel, technology, year) %>%
      summarise(fuel_input = sum(value)) ->
      fuel_input_USA

    # Compute state shares for each category in the fuel input table
    fuel_input_state %>%
      left_join_error_no_match(fuel_input_USA, by = c("sector", "fuel", "technology", "year")) %>%
      mutate(fuel_input_share = value / fuel_input) %>%
      left_join(en_ghg_emissions_USA %>%
                  select("supplysector", "subsector", "stub.technology",
                         "year", "N2O", "CH4"),
                by = c("sector" = "supplysector",
                       "fuel" = "subsector",
                       "technology" = "stub.technology",
                       "year")) %>%
      mutate(CH4 = fuel_input_share * CH4,
             N2O = fuel_input_share * N2O) %>%
      rename(region = state, supplysector = sector, subsector = fuel, stub.technology = technology) %>%
      select(region, supplysector, subsector, stub.technology, year, CH4, N2O) ->
      en_ghg_emissions_state

    # Buildings: First subset the heating and cooling demands
    L244.StubTechCalInput_bld_gcamusa %>%
      filter(year %in% en_ghg_emissions_USA$year & subsector %in% en_ghg_emissions_USA$subsector) %>%
      # Add a sector column to match with the emissions data
      mutate(sector = if_else(supplysector %in% c("comm heating", "comm cooling", "resid heating", "resid cooling"),
                              supplysector,if_else(grepl("comm", supplysector),"comm others","resid others"))) %>%
      select(region, sector, supplysector, subsector, stub.technology, year, calibrated.value) ->
      bld_fuel_input_state

    # Create aggregate table for total nation fuel inputs by emissions category
    bld_fuel_input_state %>%
      group_by(sector, subsector, year) %>%
      summarise(calibrated.value = sum(calibrated.value)) ->
      bld_fuel_input_agg

    # Compute shares of national and sector total for each fuel input technology
    bld_fuel_input_state %>%
      left_join_error_no_match(bld_fuel_input_agg, by = c("sector", "subsector", "year")) %>%
      mutate(share = calibrated.value.x / calibrated.value.y) %>%
      # some zero divided zero case
      replace_na(list(share = 0)) %>%
      left_join_error_no_match(en_ghg_emissions_USA %>%
                                 select("supplysector", "subsector", "year", "CH4", "N2O") %>%
                                 group_by(supplysector, subsector, year) %>%
                                 summarise(CH4 = sum(CH4), N2O = sum(N2O)) %>%
                                 ungroup(),
                               by = c("sector" = "supplysector","subsector","year")) %>%
      mutate(CH4 = share * CH4,
             N2O = share * N2O) %>%
      select(region, supplysector, subsector, stub.technology, year, CH4, N2O) ->
      bld_ghg_emissions_state

    # Combine the buildings and other energy input emissions tables and convert to long format
    en_ghg_emissions_state %>%
      bind_rows(bld_ghg_emissions_state) %>%
      gather(Non.CO2, input.emissions, -region, -supplysector, -subsector, -stub.technology, -year, convert=TRUE) %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2, year) ->
      L273.en_ghg_emissions_USA

    # Format for csv file
    L273.en_ghg_emissions_USA %>%
      select(LEVEL2_DATA_NAMES$StubTechYr, "Non.CO2", "input.emissions") %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      ## The stub.techs and supplysectors here do not match. Therefore we join via the subsectors and we want to keep the first
      ## joined value to avoid duplicates
      left_join_keep_first_only(EnTechInputNameMap %>% select(-stub.technology), by = c("supplysector", "subsector")) %>%
      na.omit() ->
      L273.en_ghg_emissions_USA

    # 2d. Output emissions
    # L273.out_ghg_emissions_USA: Output emissions of GHGs in U.S. states
    ###NOTE: This table will contain PFC and HFC emissions for now, from building cooling and electricity own use.
    ###the energy data is available at the state level and will be used to share out the emissions
    L241.hfc_all %>%
      bind_rows(L241.pfc_all) %>%
      filter(region == gcam.USA_REGION) ->
      L241.hfc_pfc_USA

    # SF6 Emissions from electricity own use
    L241.hfc_pfc_elec_ownuse <- filter(L241.hfc_pfc_USA, supplysector == "electricity_net_ownuse")

    # HFC Emissions from building cooling
    L241.hfc_pfc_bld <- filter(L241.hfc_pfc_USA, supplysector %in% c("resid cooling","comm cooling"))

    # Electricity net own use output by state
    L123.out_EJ_state_ownuse_elec %>%
      #Subset relevant years
      filter(year %in% L241.hfc_pfc_USA$year) %>%
      mutate(supplysector = "electricity_net_ownuse",
             subsector = supplysector,
             stub.technology = supplysector) ->
      L123.out_EJ_state_ownuse_elec.long

    # Compute aggregate USA electricity own use output
    L123.out_EJ_state_ownuse_elec.long %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L123.out_EJ_ownuse_elec_agg

    # Match state shares onto elec own use table
    L123.out_EJ_state_ownuse_elec.long %>%
      left_join_error_no_match(L123.out_EJ_ownuse_elec_agg %>%
                                 select(value2 = value, year),
                               by = "year") %>%
      mutate(share = value / value2) %>%
      # Add column with pollutant identifier
      repeat_add_columns(tibble("Non.CO2" = unique(L241.hfc_pfc_elec_ownuse$Non.CO2))) %>%
      left_join(L241.hfc_pfc_elec_ownuse %>%
                  select(year, Non.CO2, input.emissions),
                by = c("year", "Non.CO2")) %>%
      mutate(output.emissions = share * input.emissions) %>%
      select(state, supplysector, subsector, stub.technology, year, Non.CO2, output.emissions) %>%
      # The electricity ownuse emissions must be written out at the grid region level
      left_join_error_no_match(states_subregions, by = "state") %>%
      group_by(grid_region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      summarise(output.emissions = sum(output.emissions)) %>%
      ungroup() %>%
      rename(region = grid_region) ->
      L273.out_ghg_emissions_elec_ownuse

    # To compute building service output, multiply the building energy use by efficiency
    L244.StubTechCalInput_bld_gcamusa %>%
      # use inner_join to keep the cooling sectors defined in L241.hfc_pfc_bld
      inner_join(L241.hfc_pfc_bld %>%
                   select("supplysector","subsector","year") %>%
                   distinct(),
                 by = c("supplysector","subsector","year")) %>%
      left_join_error_no_match(L244.GlobalTechEff_bld, by = c("supplysector" = "sector.name",
                                                              "subsector" = "subsector.name",
                                                              "stub.technology" = "technology",
                                                              "year")) %>%
      mutate(service_output = calibrated.value * efficiency) %>%
      select(region, supplysector, subsector, stub.technology, year, service_output) ->
      L244.output_bld_cool

    # Compute aggregate USA building cooling service output for each subsector
    L244.output_bld_cool %>%
      group_by(supplysector, subsector, year) %>%
      summarise(service_output = sum(service_output)) ->
      L244.output_bld_cool_agg

    # Match shares onto service output table
    L244.output_bld_cool %>%
      # We do not expect a 1:1 match here so use left_join.
      left_join(L244.output_bld_cool_agg %>%
                  select(supplysector, subsector, year, service_output2 = service_output),
                by = c("supplysector", "subsector", "year")) %>%
      mutate(share = service_output / service_output2) %>%
      # Add column identifying pollutant
      repeat_add_columns(tibble("Non.CO2" = unique(L241.hfc_pfc_bld$Non.CO2))) %>%
      # Match on output emissions, sharing out to states and technologies
      left_join(L241.hfc_pfc_bld %>%
                  select("supplysector", "subsector", "year", "Non.CO2", "input.emissions"),
                by = c("supplysector", "subsector", "year", "Non.CO2")) %>%
      mutate(output.emissions = share * input.emissions) %>%
      select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "output.emissions") ->
      L273.out_ghg_emissions_bld_cool

    #Combine output emissions into one table and organize
    bind_rows(L273.out_ghg_emissions_elec_ownuse,
              L273.out_ghg_emissions_bld_cool) %>%
      arrange(Non.CO2, supplysector) ->
      L273.out_ghg_emissions_USA

    # 2e. MAC curves
    # L273.MAC_higwp_USA: abatement from HFCs and PFCs in all U.S. states
    # The MAC curves will be identical to those for the USA.
    L252.MAC_higwp_USA <- filter(L252.MAC_higwp, region == gcam.USA_REGION)

    # For building cooling, have to add more specific technologies on the state level. This means that both
    # of the building cooling techs in GCAM-USA will have identical MAC curves
    L252.MAC_higwp_USA %>%
      filter(supplysector %in% L273.out_ghg_emissions_bld_cool$supplysector) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control,
             tax, mac.reduction, market.name) %>%
      repeat_add_columns(tibble("state_technology" = unique(L273.out_ghg_emissions_bld_cool$stub.technology))) %>%
      # Now replace the existing stub technology column with the one containing the state-level techs
      select(-stub.technology) %>%
      rename(stub.technology = state_technology) %>%
      select(-region) %>%
      repeat_add_columns(tibble("region" = states_subregions$state)) ->
      L273.MAC_higwp_bld_cool

    # Electricity own use will be written out at the grid region level
    L252.MAC_higwp_USA %>%
      filter(supplysector %in% L273.out_ghg_emissions_elec_ownuse$subsector) %>%
      select(-region) %>%
      repeat_add_columns(tibble("region" = states_subregions$grid_region)) ->
      L273.MAC_higwp_elec_ownuse

    # Bind the MAC curve tables and organize
    L273.MAC_higwp_bld_cool %>%
      bind_rows(L273.MAC_higwp_elec_ownuse) %>%
      select(names(L252.MAC_higwp)) ->
      L273.MAC_higwp_USA

    # MAC tech.changes
    L252.MAC_higwp_tc_average <- filter(L252.MAC_higwp_tc_average, region == gcam.USA_REGION) %>%
      select(mac.control, tech.change.year, tech.change) %>%
      distinct()

    L273.MAC_higwp_TC_USA <- L273.MAC_higwp_USA %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      # number of rows will change as we need to tag all future "tech.change.year" to the same starting year
      left_join(L252.MAC_higwp_tc_average, by = "mac.control")

    # MAC phase-in-time
    L252.MAC_higwp_phaseInTime <- filter(L252.MAC_higwp_phaseInTime, region == gcam.USA_REGION) %>%
      select(mac.control, mac.phase.in.time) %>%
      distinct()

    L273.MAC_higwp_phaseInTime_USA <- L273.MAC_higwp_USA %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      # number of rows will change as we need to tag all future "tech.change.year" to the same starting year
      left_join(L252.MAC_higwp_phaseInTime, by = "mac.control")

    # ===================================================

    # Produce outputs
    L273.en_ghg_tech_coeff_USA %>%
      add_title("GHG emissions coefficients for energy technologies in U.S. states") %>%
      add_units("NA") %>%
      add_comments("Write the USA coefficients for every state, remove H2 production emissions and match the refining emission factors to the corresponding technologies in the states.") %>%
      add_legacy_name("L273.en_ghg_tech_coeff_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_ownuse_elec",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "energy/A22.globaltech_input_driver",
                     "energy/A23.globaltech_input_driver",
                     "energy/A25.globaltech_input_driver",
                     "L222.StubTech_en_USA",
                     "L232.StubTechCalInput_indenergy_USA",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.GlobalTechEff_bld") ->
      L273.en_ghg_tech_coeff_USA

    L273.en_ghg_emissions_USA %>%
      add_title("Calibrated input emissions of N2O and CH4 by U.S. state") %>%
      add_units("Tg") %>%
      add_comments("Compute shares of national and sector total for each fuel input technology") %>%
      add_legacy_name("L273.en_ghg_emissions_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_ownuse_elec",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "energy/calibrated_techs",
                     "gcam-usa/calibrated_techs_bld_usa",
                     "energy/mappings/UCD_techs_revised",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_USA",
                     "L232.StubTechCalInput_indenergy_USA",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.GlobalTechEff_bld") ->
      L273.en_ghg_emissions_USA

    L273.out_ghg_emissions_USA %>%
      add_title("Output emissions of GHGs in U.S. states") %>%
      add_units("Gg") %>%
      add_comments("Use state energy data to determine each state's share of the national emissions.") %>%
      add_legacy_name("L273.out_ghg_emissions_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_ownuse_elec",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_USA",
                     "L232.StubTechCalInput_indenergy_USA",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.GlobalTechEff_bld") ->
      L273.out_ghg_emissions_USA

    L273.MAC_higwp_USA %>%
      add_title("Abatement curves for the HFCs and PFCs in all U.S. states") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("The MAC curves will be identical to those for the USA.") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_ownuse_elec",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_USA",
                     "L232.StubTechCalInput_indenergy_USA",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L244.GlobalTechEff_bld") ->
      L273.MAC_higwp_USA

    L273.MAC_higwp_TC_USA %>%
      add_title("Technological change for the MACs of HFCs and PFCs in all U.S. states") %>%
      add_units("% improvement of maximum reduction potential per year") %>%
      add_comments("This will be identical to those for the USA.") %>%
      add_legacy_name("L273.MAC_higwp_TC_USA") %>%
      same_precursors_as("L273.MAC_higwp_USA") %>%
      add_precursors("L252.MAC_higwp_tc_average") ->
      L273.MAC_higwp_TC_USA

    L273.MAC_higwp_phaseInTime_USA %>%
      add_title("phase in time for the MACs of HFCs and PFCs in all U.S. states") %>%
      add_units("years of maximum reduction potential to be fully phased in") %>%
      add_comments("This will be identical to those for the USA.") %>%
      add_legacy_name("L273.MAC_higwp_phaseInTime_USA") %>%
      same_precursors_as("L273.MAC_higwp_USA") %>%
      add_precursors("L252.MAC_higwp_phaseInTime") ->
      L273.MAC_higwp_phaseInTime_USA


    return_data(L273.en_ghg_tech_coeff_USA, L273.en_ghg_emissions_USA, L273.out_ghg_emissions_USA,
                L273.MAC_higwp_USA, L273.MAC_higwp_TC_USA, L273.MAC_higwp_phaseInTime_USA)

  } else {
    stop("Unknown command")
  }
}
