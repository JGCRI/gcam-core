#' module_gcam.usa_L273.en_ghg_emissions_USA.R
#'KALYN
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.en_ghg_tech_coeff_USA}, \code{L273.en_ghg_emissions_USA}, \code{L273.out_ghg_emissions_USA},
#' and \code{L273.MAC_higwp_USA}. The corresponding file in the
#' original data system was \code{L273.en_ghg_emissions_USA.R} (gcam-usa level2).
#' @details KALYN
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KRD September 2018

module_gcam.usa_L273.en_ghg_emissions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A27.tech_associations",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_state_ownuse_elec",
             "L1231.in_EJ_state_elec_F_tech",
             "L1322.in_EJ_state_Fert_Yh",
             "L201.en_ghg_emissions",
             "L201.ghg_res",
             "L241.nonco2_tech_coeff",
             "L241.hfc_all",
             "L241.pfc_all",
             "L252.ResMAC_fos",
             "L252.MAC_higwp",
             "L222.StubTech_en_USA",
            "L223.StubTech_elec_USA",
             "L232.StubTechCalInput_indenergy_USA",
             "L244.StubTechCalInput_bld",
             "L244.GlobalTechEff_bld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.en_ghg_tech_coeff_USA",
             "L273.en_ghg_emissions_USA",
             "L273.out_ghg_emissions_USA",
             "L273.MAC_higwp_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    CH4 <- GCAM_region_ID <- N2O <- Non.CO2 <- calibrated.value <- calibrated.value.x <-
      calibrated.value.y <- depresource <- efficiency <- elec_technology <- emiss.coef <-
      emiss.coeff <- fuel <- fuel_input <- fuel_input_share <- grid_region <-
      input.emissions <- keep <- mac.control <- mac.reduction <- market.name <-
      output.emissions <- palette <- region <- sector <- service_output <- service_output2 <-
      share <- state <- state_technology <- stub.technology <- subsector <- supplysector <-
      tax <- technology <- value <- value2 <- year <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A27.tech_associations <- get_data(all_data, "gcam-usa/A27.tech_associations")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec")
    L1231.in_EJ_state_elec_F_tech <- get_data(all_data, "L1231.in_EJ_state_elec_F_tech")
    L1322.in_EJ_state_Fert_Yh <- get_data(all_data, "L1322.in_EJ_state_Fert_Yh")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff")
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")
    L252.ResMAC_fos <- get_data(all_data, "L252.ResMAC_fos")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")
    L222.StubTech_en_USA <- get_data(all_data, "L222.StubTech_en_USA")
    L223.StubTech_elec_USA <- get_data(all_data, "L223.StubTech_elec_USA")
    L232.StubTechCalInput_indenergy_USA <- get_data(all_data, "L232.StubTechCalInput_indenergy_USA")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")

    # ===================================================

    # 2. Build tables for CSVs
    # 2a. Resource emission coefficients
    # L273.res_ghg_tech_coeff_USA: GHG emissions for energy resources in all U.S. states
    # For resources all that needs to be done is to write the USA coefficients for every state
    L201.ghg_res %>%
      filter(region == "USA") %>%
      select(-region) %>%
      repeat_add_columns(tibble("region" = states_subregions$state)) %>%
      select(region, depresource, Non.CO2, emiss.coef) ->
      L273.res_ghg_tech_coeff_USA

    # Input Emissions coefficients
    # L273.en_ghg_tech_coeff_USA: GHG emissions coefficients for energy technologies in U.S. states
    # Write the USA coefficients for every state.
    ## Remove H2 production emissions for now because energy is not available on state level
    # Refining first:
     L241.nonco2_tech_coeff %>%
       filter(region == "USA" & Non.CO2 %in% c("N2O","CH4") & supplysector == "refining") ->
       L241.ref_ghg_tech_coeff_USA

     # Match the refining emission factors to the corresponding technologies in the states. Matching on subsector
     # and stub technology because the names of the sectors are different
     L222.StubTech_en_USA %>%
       repeat_add_columns(tibble("year" = unique(L241.ref_ghg_tech_coeff_USA$year))) %>%
       repeat_add_columns(tibble("Non.CO2" = unique(L241.ref_ghg_tech_coeff_USA$Non.CO2))) %>%
       left_join(L241.ref_ghg_tech_coeff_USA %>%
                   select("subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff"),
                 by = c("subsector", "stub.technology", "year", "Non.CO2")) %>%
       #MISSING VALUES: various tech/year combinations. OK to omit
       na.omit %>%
       select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff") ->
       L273.ref_ghg_tech_coeff_USA

     # Write electricity emission coefficients to states for technologies shared by GCAMUSA and emissions data
     L241.nonco2_tech_coeff %>%
       filter(region == "USA" & Non.CO2 %in% c("N2O","CH4") & supplysector == "electricity") ->
       L241.elc_ghg_tech_coeff_USA

     L223.StubTech_elec_USA %>%
       # TODO, check to see if this join is correct. We have to use the left_join_keep_first_only in order to
       # reproduce the old data system behavior.
       left_join_keep_first_only(L241.elc_ghg_tech_coeff_USA %>%
                   select(subsector, stub.technology) %>%
                   mutate(keep = TRUE), by = c("subsector","stub.technology")) %>%
       filter(TRUE) %>%
       select(-keep) %>%
       repeat_add_columns(tibble("year" = unique(L241.elc_ghg_tech_coeff_USA$year))) %>%
       repeat_add_columns(tibble("Non.CO2" = unique(L241.elc_ghg_tech_coeff_USA$Non.CO2))) %>%
       # TODO, check to see if this join is correct. We have to use the left_join_keep_first_only in order to
       # reproduce the old data system behavior.
       left_join_keep_first_only(L241.elc_ghg_tech_coeff_USA %>%
                                   select("stub.technology", "year", "Non.CO2", "emiss.coeff"),
                                 by = c("stub.technology", "year", "Non.CO2")) %>%
       na.omit %>%
       select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff") ->
       L273.elc_ghg_tech_coeff_USA

     # Subset the technologies that compete in the coal-gas dispatcher, and assign their emissions
     L273.elc_ghg_tech_coeff_USA %>%
       filter(stub.technology %in% A27.tech_associations$elec_technology) %>%
       select(-supplysector, -subsector) %>%
       left_join_error_no_match(A27.tech_associations %>%
                                  select("elec_technology", "supplysector", "subsector", "technology"),
                                by = c("stub.technology" = "elec_technology")) %>%
       select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff") ->
       disp_ghg_tech_coeff_USA

     # Up to now, we've just written the tech coefficients that were already present in the emissions data for
     # the dispatcher. But since there is nothing written out for the final base year we will miss emissions in this
     # year.So will have to use input emissions and fuel input on the USA level to come up with a technology coefficient
     # for the dispatcher technologies, and write this out for the first year that the dispatcher becomes available.
     L201.en_ghg_emissions_base_years <- max(L201.en_ghg_emissions$year[L201.en_ghg_emissions$year %in% BASE_YEARS])

     L201.en_ghg_emissions %>%
       filter(stub.technology %in% A27.tech_associations$elec_technology &
                year == L201.en_ghg_emissions_base_years &
                region == "USA") ->
       L201.disp_ghg_emissions_USA

     # Grab the relevant fuel inputs to electricity from the energy data
     L123.in_EJ_R_elec_F_Yh %>%
       filter(GCAM_region_ID == 1 & year %in% L201.disp_ghg_emissions_USA$year &
                fuel %in% L201.disp_ghg_emissions_USA$subsector) ->
       L123.in_EJ_USA_elec_F_Ydisp

     # Compute technology coefficients, write them to states, use them in the year that the dispatcher is available
     L201.disp_ghg_emissions_USA %>%
       left_join_error_no_match(L123.in_EJ_USA_elec_F_Ydisp %>%
                                  select(fuel, value), by = c("subsector" = "fuel")) %>%
       mutate(emiss.coeff = input.emissions / value,
              year = max(BASE_YEARS)) %>%
       select(-supplysector, -subsector) %>%
       left_join_error_no_match(A27.tech_associations %>%
                                  select(supplysector, subsector, technology, elec_technology),
                                by = c("stub.technology" = "elec_technology")) %>%
       select(supplysector, subsector, stub.technology, year, Non.CO2, input.emissions, emiss.coeff) %>%
       repeat_add_columns(tibble(region = states_subregions$state)) ->
       add_disp_ghg_tech_coeff_USA

     # Select relevant columns
     add_disp_ghg_tech_coeff_USA %>%
       select(names(disp_ghg_tech_coeff_USA)) ->
       add_disp_ghg_tech_coeff_USA

     # Bind the dispatcher technology coefficients into one table
     L273.disp_ghg_tech_coeff_USA <- bind_rows(add_disp_ghg_tech_coeff_USA, disp_ghg_tech_coeff_USA)

     # Bind the dispatcher rows to the electricity data frame
     L273.elcdisp_ghg_tech_coeff_USA <- bind_rows(L273.elc_ghg_tech_coeff_USA,L273.disp_ghg_tech_coeff_USA)

     # Bind rows containing refining and electricity emission coefficients into one table, and organize
     L273.ref_ghg_tech_coeff_USA %>%
       bind_rows(L273.elcdisp_ghg_tech_coeff_USA) %>%
       mutate(emiss.coeff = round(emiss.coeff, emissions.DIGITS_EMISSIONS)) %>%
       arrange(region, supplysector, subsector, stub.technology, year, Non.CO2) ->
       L273.en_ghg_tech_coeff_USA

     # 2c. Input Emissions
     # L273.en_ghg_emissions_USA: Calibrated input emissions of N2O and CH4 by U.S. state
     # Filter the emissions data into USA, and transportation is calibrated elsewhere
     L201.en_ghg_emissions %>%
       filter(region == "USA" & !grepl("trn",supplysector)) %>%
       spread(Non.CO2, input.emissions) %>%
       ###NOTE: emissions from coal use in commercial buildings "other" category does not have an equivalent representation
       #in the fifty state data. For now move these emissions over to comm heating
       mutate(supplysector = if_else(grepl("comm",supplysector) & subsector == "coal","comm heating", supplysector)) ->
       en_ghg_emissions_USA

     # Organize the state fuel input data
     # Electricity
     L1231.in_EJ_state_elec_F_tech %>%
       mutate(sector = "electricity") %>%
       filter(year %in% en_ghg_emissions_USA$year & technology %in% en_ghg_emissions_USA$stub.technology) ->
       elec_fuel_input_state

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
       bind_rows(elec_fuel_input_state,
                 fert_fuel_input_state) ->
       fuel_input_state

     # Create aggregate fuel input table
     fuel_input_state %>%
       group_by(sector, fuel, technology, year) %>%
       summarise(fuel_input = sum(value)) ->
       fuel_input_USA

     # Compute state shares for each category in the fuel input table
     fuel_input_state %>%
       left_join(fuel_input_USA, by = c("sector", "fuel", "technology", "year")) %>%
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
     L244.StubTechCalInput_bld %>%
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
       # TODO use left_join_keep_first_only to preserve the match in the old data system
       left_join_keep_first_only(en_ghg_emissions_USA %>%
                                  select("supplysector", "subsector", "year", "CH4", "N2O"),
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
       mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) ->
       L273.en_ghg_emissions_USA

     # 2d. Output emissions
     # L273.out_ghg_emissions_USA: Output emissions of GHGs in U.S. states
     ###NOTE: This table will contain PFC and HFC emissions for now, from building cooling and electricity own use.
     ###the energy data is available at the state level and will be used to share out the emissions
     L241.hfc_all %>%
       bind_rows(L241.pfc_all) %>%
       filter(region == "USA") ->
       L241.hfc_pfc_USA

     # Emissions from electricity own use
     L241.hfc_pfc_elec_ownuse <- filter(L241.hfc_pfc_USA, supplysector == "electricity_net_ownuse")

     # Emissions from building cooling
     L241.hfc_pfc_bld <- filter(L241.hfc_pfc_USA, supplysector %in% c("resid cooling","comm cooling"))

     # Electricity net own use output by state
     L123.out_EJ_state_ownuse_elec %>%
       #Subset relevant years
       filter(year %in% L241.hfc_pfc_USA$year) %>%
       mutate(supplysector = "electricity domestic supply") %>%
       mutate(subsector = "electricity_net_ownuse") %>%
       mutate(stub.technology = subsector) ->
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
       rename(region = grid_region) ->
       L273.out_ghg_emissions_elec_ownuse

     # To compute building service output, multiply the building energy use by efficiency
     L244.StubTechCalInput_bld %>%
       # TODO We do not expect a 1:1 match so can use a different left join here, there might
       # an issue with this match, must use left_join_keep_first_only to preserve the match
       # in the old data system but it might need to be a left_join.
       left_join_keep_first_only(L241.hfc_pfc_bld %>%
                   select("supplysector","subsector","year") %>%
                   mutate(keep = TRUE),
                 by = c("supplysector","subsector","year")) %>%
       filter(keep) %>%
       left_join_keep_first_only(L244.GlobalTechEff_bld, by = c("supplysector" = "sector.name",
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
       arrange(Non.CO2,supplysector) ->
       L273.out_ghg_emissions_USA

     # TODO: double check this, it seems we are missing PFCs in 2010
     L273.out_ghg_emissions_USA %>%
       filter(!(is.na(output.emissions) & year == 2010 & Non.CO2 %in% unique(L241.pfc_all$Non.CO2))) %>%
       ungroup ->
       L273.out_ghg_emissions_USA

     # 2e. MAC curves
     # L273.ResMAC_fos_USA: fossil resource MAC curves for all U.S. states
     # The MAC curves will be identical to those for the USA
     L252.ResMAC_fos %>%
       filter(region == "USA") %>%
       select(depresource, Non.CO2, mac.control, tax, mac.reduction, market.name) %>%
       repeat_add_columns(tibble("region" = states_subregions$state)) %>%
       select(region, depresource, Non.CO2, mac.control, tax, mac.reduction) ->
       L273.ResMAC_fos_USA

     # L273.MAC_higwp_USA: abatement from HFCs and PFCs in all U.S. states
     # The MAC curves will be identical to those for the USA.
     L252.MAC_higwp_USA <- filter(L252.MAC_higwp, region == "USA")

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
       mutate(supplysector = "electricity domestic supply") %>%
       select(-region) %>%
       repeat_add_columns(tibble("region" = states_subregions$grid_region)) ->
       L273.MAC_higwp_elec_ownuse

     # Bind the MAC curve tables and organize
     L273.MAC_higwp_bld_cool %>%
       bind_rows(L273.MAC_higwp_elec_ownuse) %>%
       select(names(L252.MAC_higwp)) ->
       L273.MAC_higwp_USA

    # ===================================================

    # Produce outputs
     L273.en_ghg_tech_coeff_USA %>%
      add_title("GHG emissions coefficients for energy technologies in U.S. states") %>%
      add_units("NA") %>%
      add_comments("Write the USA coefficients for every state, remove H2 production emissions and match the refining emission factors to the corresponding technologies in the states.") %>%
      add_legacy_name("L273.en_ghg_tech_coeff_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A27.tech_associations",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L123.out_EJ_state_ownuse_elec",
                     "L1231.in_EJ_state_elec_F_tech",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L201.ghg_res",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.ResMAC_fos",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_USA",
                     "L223.StubTech_elec_USA",
                     "L232.StubTechCalInput_indenergy_USA",
                     "L244.StubTechCalInput_bld",
                     "L244.GlobalTechEff_bld") ->
       L273.en_ghg_tech_coeff_USA

     L273.en_ghg_emissions_USA %>%
       add_title("Calibrated input emissions of N2O and CH4 by U.S. state") %>%
       add_units("Tg") %>%
       add_comments("Compute shares of national and sector total for each fuel input technology") %>%
       add_legacy_name("L273.en_ghg_emissions_USA") %>%
       add_precursors("gcam-usa/states_subregions",
                      "gcam-usa/A27.tech_associations",
                      "L123.in_EJ_R_elec_F_Yh",
                      "L123.out_EJ_state_ownuse_elec",
                      "L1231.in_EJ_state_elec_F_tech",
                      "L1322.in_EJ_state_Fert_Yh",
                      "L201.en_ghg_emissions",
                      "L201.ghg_res",
                      "L241.nonco2_tech_coeff",
                      "L241.hfc_all",
                      "L241.pfc_all",
                      "L252.ResMAC_fos",
                      "L252.MAC_higwp",
                      "L222.StubTech_en_USA",
                      "L223.StubTech_elec_USA",
                      "L232.StubTechCalInput_indenergy_USA",
                      "L244.StubTechCalInput_bld",
                      "L244.GlobalTechEff_bld") ->
       L273.en_ghg_emissions_USA

     L273.out_ghg_emissions_USA %>%
       add_title("Output emissions of GHGs in U.S. states") %>%
       add_units("Gg") %>%
       add_comments("Use state energy data to determine each state's share of the national emissions.") %>%
       add_legacy_name("L273.out_ghg_emissions_USA") %>%
       add_precursors("gcam-usa/states_subregions",
                      "gcam-usa/A27.tech_associations",
                      "L123.in_EJ_R_elec_F_Yh",
                      "L123.out_EJ_state_ownuse_elec",
                      "L1231.in_EJ_state_elec_F_tech",
                      "L1322.in_EJ_state_Fert_Yh",
                      "L201.en_ghg_emissions",
                      "L201.ghg_res",
                      "L241.nonco2_tech_coeff",
                      "L241.hfc_all",
                      "L241.pfc_all",
                      "L252.ResMAC_fos",
                      "L252.MAC_higwp",
                      "L222.StubTech_en_USA",
                      "L223.StubTech_elec_USA",
                      "L232.StubTechCalInput_indenergy_USA",
                      "L244.StubTechCalInput_bld",
                      "L244.GlobalTechEff_bld") ->
       L273.out_ghg_emissions_USA

     L273.MAC_higwp_USA %>%
       add_title("Abatement curves for the HFCs and PFCs in all U.S. states") %>%
       add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
       add_comments("The MAC curves will be identical to those for the USA.") %>%
       add_legacy_name("L252.MAC_higwp") %>%
       add_precursors("gcam-usa/states_subregions",
                      "gcam-usa/A27.tech_associations",
                      "L123.in_EJ_R_elec_F_Yh",
                      "L123.out_EJ_state_ownuse_elec",
                      "L1231.in_EJ_state_elec_F_tech",
                      "L1322.in_EJ_state_Fert_Yh",
                      "L201.en_ghg_emissions",
                      "L201.ghg_res",
                      "L241.nonco2_tech_coeff",
                      "L241.hfc_all",
                      "L241.pfc_all",
                      "L252.ResMAC_fos",
                      "L252.MAC_higwp",
                      "L222.StubTech_en_USA",
                      "L223.StubTech_elec_USA",
                      "L232.StubTechCalInput_indenergy_USA",
                      "L244.StubTechCalInput_bld",
                      "L244.GlobalTechEff_bld") ->
       L273.MAC_higwp_USA


    return_data(L273.en_ghg_tech_coeff_USA, L273.en_ghg_emissions_USA, L273.out_ghg_emissions_USA, L273.MAC_higwp_USA)

  } else {
    stop("Unknown command")
  }
}
