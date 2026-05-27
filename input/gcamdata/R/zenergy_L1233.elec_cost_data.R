# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1233.elec_cost_data
#'
#' Generates power sector cost input files based on 2019 ATB data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.globaltech_capital_ATB}, \code{L1233.globaltech_capital_ATB_adv},
#' \code{L1233.globaltech_capital_ATB_low},
#' \code{L1233.globaltech_OMfixed_ATB}, \code{L1233.globaltech_OMvar_ATB}.
#' There was no corresponding file in the original data system.
#' @details Includes ATB capital cost data as starting point, improvement rate and improvement max data generated based on cost pathway.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select pull left_join anti_join bind_rows arrange rename
#' @importFrom purrr reduce
#' @author GPK September 2025
module_energy_L1233.elec_cost_data <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "energy/A23.globaltech_fcr",
      FILE = "energy/A23.globaltech_cost_tc",
      FILE = "energy/NREL_ATB_capital_2024",
      FILE = "energy/NREL_ATB_OMfixed_2024",
      FILE = "energy/NREL_ATB_OMvar_2024",
      FILE = "energy/ReEDS_power_plant_costs",
      FILE = "energy/mappings/atb_gcam_elec_tech_mapping")

  MODULE_OUTPUTS <-
    c("L1233.globaltech_capital_ATB",
      "L1233.globaltech_capital_ATB_adv",
      "L1233.globaltech_capital_ATB_low",
      "L1233.globaltech_OMfixed_ATB",
      "L1233.globaltech_OMfixed_ATB_adv",
      "L1233.globaltech_OMfixed_ATB_low",
      "L1233.globaltech_OMvar_ATB",
      "L1233.globaltech_OMvar_ATB_adv",
      "L1233.globaltech_OMvar_ATB_low",
      "L1233.globaltech_capital_ATB_battery")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    value <- year <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # function to extend 2050 cost data to 2100 with exogenous technological change indicated by technology and scenario
    apply_exogenous_techchange <- function(df, cost_vars, use_default_rate = FALSE, default_rate = 0.001,
                                           tc_table = A23.globaltech_cost_tc,
                                           from.year = 2050, to.year = 2100){

      # if no techchange data table (tc_table) is provided, apply the default_rate to all technologies and scenarios
      if(use_default_rate){
        cost_multipliers <- tibble(year = MODEL_YEARS[MODEL_YEARS >= from.year & MODEL_YEARS <= to.year]) %>%
          mutate(cost_multiplier = (1 - default_rate)^(year - from.year))
        df_new <- left_join(df, cost_multipliers, by = "year") %>%
          replace_na(list(cost_multiplier = 1))
      } else {
        # if a techchange data table is provided, apply the rates on a technology and scenario-specific basis
        tc_by_tech_scen <- gather(tc_table, key = "case", value = "rate", -supplysector, -subsector, -technology) %>%
          repeat_add_columns(tibble(year = MODEL_YEARS[MODEL_YEARS >= from.year & MODEL_YEARS <= to.year])) %>%
          mutate(cost_multiplier = (1 - rate)^(year - from.year))
        df_new <- left_join(df, tc_by_tech_scen, by = c("case", "supplysector", "subsector", "technology", "year")) %>%
          replace_na(list(cost_multiplier = 1))
      }
        df_new[cost_vars] <- df_new[cost_vars] * df_new$cost_multiplier
        df_new <- select(df_new, names(df))
        return(df_new)
    }

    # ReEDS pre-processing
    L1233.reeds_costs <- ReEDS_power_plant_costs %>%
      separate(filename, into = c("tech_category", "source", "pubyear", "case"), sep = "_") %>%
      mutate(case = if_else(case %in% c("advanced", "low"), "Advanced",
                            if_else(case %in% c("conservative", "high"), "Conservative",
                                    "Moderate")),
             capcost = capcost * gdp_deflator(2022, dollaryear),
             fom = fom * gdp_deflator(2022, dollaryear),
             vom = vom * gdp_deflator(2022, dollaryear)) %>%
      select(case, reeds_tech = i, year = t, capcost, fom, vom)

    # ATB pre-processing
    L1233.atb_costs <- gather_years(NREL_ATB_capital_2024) %>%
      bind_rows(gather_years(NREL_ATB_OMfixed_2024),
                gather_years(NREL_ATB_OMvar_2024)) %>%
      spread(key = input, value = value)

    # Join the ATB and ReEDS pre-processed datasets in order to generate a continuous time series of all available
    # technologies in either. The ATB values will be used where available (variable names: capital, `OM-fixed`, `OM-var`),
    # but ReEDS data will be used to fill in technologies missing from the ATB, and the 2010-2022 years.

    # The first left_join is an expanding join, as multiple gcam techs may use the same reeds tech. E.g., reeds/battery_li
    # maps to gcam/battery, gcam/wind_storage,  and gcam/PV_storage. reeds/Gas- techs are mapped to gcam/refined liquids
    # and gcam/gas techs.
    # The second left_join returns missing values in all years prior to 2022, and for all techs in ReEDS but not the ATB
    L1233.reeds_atb_costs <- L1233.reeds_costs %>%
      filter(reeds_tech %in% atb_gcam_elec_tech_mapping$reeds_tech) %>%
      left_join(atb_gcam_elec_tech_mapping, by = "reeds_tech") %>%
      left_join(L1233.atb_costs, by = c("case", "tech_type", "tech_detail", "year")) %>%
    # Where a future technology (2022-2050) is in ReEDS but not ATB, use the ReEDS value. This is the case for biomass (conv CCS)
      mutate(capital = if_else(is.na(capital) & year >= 2022, capcost * cost_multiplier, capital * cost_multiplier),
             `OM-fixed` = if_else(is.na(`OM-fixed`) & year >= 2022, fom * cost_multiplier, `OM-fixed` * cost_multiplier),
             `OM-var` = if_else(is.na(`OM-var`) & year >= 2022, vom * cost_multiplier, `OM-var` * cost_multiplier),
             # Back-fill the 2010-2021 values directly from ReEDS
             capital = if_else(year < 2022, capcost * cost_multiplier, capital),
             `OM-fixed` = if_else(year < 2022, fom * cost_multiplier, `OM-fixed`),
             `OM-var` = if_else(year < 2022, vom * cost_multiplier, `OM-var`))

    # Generally ReEDS data for each technology can be copied with no modifications (exactly equal to ATB in 2022)
    # However, battery costs in ReEDS are $/kwh, whereas in GCAM, the capital costs (read from the ATB) are $/kW
    # Assume the same $/kW to $/kWh ratio as in 2022 (this ratio is the same for capcost and fom in the data)

    # Error out if "battery" is not in the technology set
    stopifnot("battery" %in% L1233.reeds_atb_costs$technology)

    L1233.battery_cost_ratio <- L1233.reeds_atb_costs %>%
      filter(year == 2022, case == "Moderate", reeds_tech == "battery_li" & technology == "battery") %>%
      mutate(cost_ratio = capital / capcost) %>%
      distinct(cost_ratio) %>%
      pull(cost_ratio)

    # This should only be one numerical value; if e.g. different battery capacities are in the L1233.reeds_atb_costs table,
    # a revision to this method will be needed
    stopifnot(length(L1233.battery_cost_ratio) == 1)

    # revise the battery costs to be $/kW instead of $/kwh as they are in ReEDS
    L1233.reeds_atb_costs <- L1233.reeds_atb_costs %>%
      mutate(capital = if_else(reeds_tech == "battery_li" & year < 2022, capcost * L1233.battery_cost_ratio * cost_multiplier, capital),
             `OM-fixed` = if_else(reeds_tech == "battery_li" & year < 2022, fom * L1233.battery_cost_ratio * cost_multiplier, `OM-fixed`),
             `OM-var` = if_else(reeds_tech == "battery_li" & year < 2022, vom * L1233.battery_cost_ratio * cost_multiplier, `OM-var`))

    # The battery table can be finalized for output here; it is not in the main output tables.
    # Needs to be converted to 1975$, filtered and extrapolated to all model years, applying post-2050 tech change
    L1233.globaltech_capital_ATB_battery <- filter(L1233.reeds_atb_costs, technology == "battery") %>%
      filter(year %in% MODEL_YEARS) %>%
      select(case, technology, year, capital, `OM-fixed`, `OM-var`) %>%
      complete(nesting(case, technology), year = MODEL_YEARS) %>%
      group_by(case, technology) %>%
      mutate(capital.cost = approx_fun(year, capital * gdp_deflator(1975, 2022), rule = 2),
             fixed.om = approx_fun(year, `OM-fixed` * gdp_deflator(1975, 2022), rule = 2),
             variable.om = approx_fun(year, `OM-var` * gdp_deflator(1975, 2022), rule = 2)) %>%
      ungroup() %>%
      apply_exogenous_techchange(cost_vars = c("capital.cost", "fixed.om", "variable.om"),
                                 use_default_rate = TRUE) %>%
      select(case, technology, year, capital.cost, fixed.om, variable.om)

    # The L1233.reeds_atb_costs data table has 2010-2050 for all technologies in ReEDS, using data from ATB where available
    # It is missing some technologies for GCAM that are filled out in subsequent steps.
    # The step below drops unnecessary columns, and adds the costs for technologies with multiple line items
    # (wind_storage and PV_storage have separate line items for the battery and the generation equipment)
    L1233.gcam_costs_from_reeds <- group_by(L1233.reeds_atb_costs, case, technology, year) %>%
      summarise(capital = sum(capital),
                `OM-fixed` = sum(`OM-fixed`),
                `OM-var` = sum(`OM-var`)) %>%
      ungroup()

    # L1233.gcam_costs_from_ATB: coal (IGCC CCS), rooftop_pv, and SMR technologies
    # Commercial and residential PV are averaged (unweighted) to get the rooftop_pv costs
    L1233.gcam_costs_from_ATB <- L1233.atb_costs %>%
      inner_join(select(filter(atb_gcam_elec_tech_mapping, is.na(reeds_tech)), -reeds_tech),
                 by = c("tech_type", "tech_detail")) %>%
      group_by(case, technology, year) %>%
      summarise(capital = mean(capital),
                `OM-fixed` = mean(`OM-fixed`),
                `OM-var` = mean(`OM-var`)) %>%
      ungroup() %>%
      complete(nesting(case, technology), year = 2010:2050) %>%
      group_by(case, technology) %>%
      mutate(capital = approx_fun(year, capital, rule = 2),
             `OM-fixed` = approx_fun(year, `OM-fixed`, rule = 2),
             `OM-var` = approx_fun(year, `OM-var`, rule = 2)) %>%
      ungroup()

    # Biomass IGCC and IGCC_CCS costs are inferred from corresponding biomass conv, coal conv, and coal IGCC technologies
    # biomass (IGCC) = biomass (conv) + coal (IGCC) - coal (conv pul)
    # biomass (IGCC CCS) = biomass (conv CCS) + coal (IGCC CCS) - coal (conv pul CCS)
    L1233.gcam_bio_igcc <- filter(L1233.gcam_costs_from_reeds, technology %in%
                                    c("biomass (conv CCS)", "biomass (conv)", "coal (conv pul CCS)", "coal (conv pul)", "coal (IGCC)"),
                                  case == "Moderate") %>%
      bind_rows(filter(L1233.gcam_costs_from_ATB, technology == "coal (IGCC CCS)", case == "Moderate")) %>%
      gather(key = "input", value = "value", -case, -technology, -year) %>%
      spread(key = technology, value = value) %>%
      mutate(`biomass (IGCC)` = `biomass (conv)` + `coal (IGCC)` - `coal (conv pul)`,
             `biomass (IGCC CCS)` = `biomass (conv CCS)` + `coal (IGCC CCS)` - `coal (conv pul CCS)`) %>%
      gather(key = "technology", value = "value", -case, -year, -input) %>%
      filter(technology %in% c("biomass (IGCC)", "biomass (IGCC CCS)")) %>%
      spread(key = input, value = value)

    # Compile the data into a single table, join with A23 to get fully qualified technology names, expand to all model years,
    # apply post-2050 tech change, multiply by dollar conversion
    L1233.gcam_costs_alltechs <- bind_rows(L1233.gcam_costs_from_reeds, L1233.gcam_costs_from_ATB, L1233.gcam_bio_igcc) %>%
      gather(key = input, value = "value", -case, -technology, -year) %>%
      complete(nesting(case, technology, input), year = MODEL_YEARS) %>%
      group_by(case, technology, input) %>%
      mutate(value = approx_fun(year, value * gdp_deflator(1975, 2022), rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      inner_join(select(A23.globaltech_fcr, supplysector, subsector, technology, interest.rate, payback.years),
                 by = "technology") %>%
      apply_exogenous_techchange(cost_vars = "value") %>%
      select(case, supplysector, subsector, technology, year, input, interest.rate, payback.years, value)

    # Split out into data tables for output
    L1233.globaltech_capital_ATB <- filter(L1233.gcam_costs_alltechs, case == "Moderate", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_capital_ATB_adv <- filter(L1233.gcam_costs_alltechs, case == "Advanced", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_capital_ATB_low <- filter(L1233.gcam_costs_alltechs, case == "Conservative", input == "capital") %>%
      rename(input.capital = input, capital.overnight = value) %>%
      select(-case)
    L1233.globaltech_OMfixed_ATB <- filter(L1233.gcam_costs_alltechs, case == "Moderate", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -interest.rate, -payback.years)
    L1233.globaltech_OMfixed_ATB_adv <- filter(L1233.gcam_costs_alltechs, case == "Advanced", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -interest.rate, -payback.years)
    L1233.globaltech_OMfixed_ATB_low <- filter(L1233.gcam_costs_alltechs, case == "Conservative", input == "OM-fixed") %>%
      rename(input.OM.fixed = input, OM.fixed = value) %>%
      select(-case, -interest.rate, -payback.years)
    L1233.globaltech_OMvar_ATB <- filter(L1233.gcam_costs_alltechs, case == "Moderate", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -interest.rate, -payback.years)
    L1233.globaltech_OMvar_ATB_adv <- filter(L1233.gcam_costs_alltechs, case == "Advanced", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -interest.rate, -payback.years)
    L1233.globaltech_OMvar_ATB_low <- filter(L1233.gcam_costs_alltechs, case == "Conservative", input == "OM-var") %>%
      rename(input.OM.var = input, OM.var = value) %>%
      select(-case, -interest.rate, -payback.years)

    # ===================================================
    # Produce outputs

    L1233.globaltech_capital_ATB %>%
      add_title("2024 ATB-based capital costs in the Moderate case)") %>%
      add_units("1975$/kW") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      add_precursors("energy/A23.globaltech_fcr",
                     "energy/A23.globaltech_cost_tc",
                     "energy/NREL_ATB_capital_2024",
                     "energy/ReEDS_power_plant_costs",
                     "energy/mappings/atb_gcam_elec_tech_mapping") ->
      L1233.globaltech_capital_ATB

    L1233.globaltech_capital_ATB_adv %>%
      add_title("2024 ATB-based capital costs in the Advanced case") %>%
      add_units("1975$/kW") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_capital_ATB) ->
      L1233.globaltech_capital_ATB_adv

    L1233.globaltech_capital_ATB_low %>%
      add_title("2024 ATB-based capital costs in the Conservative case") %>%
      add_units("1975$/kW") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_capital_ATB) ->
      L1233.globaltech_capital_ATB_low

    L1233.globaltech_OMfixed_ATB %>%
      add_title("2024 ATB-based fixed O&M costs in the Moderate case") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      add_precursors("energy/NREL_ATB_OMfixed_2024",
                     "energy/ReEDS_power_plant_costs",
                     "energy/mappings/atb_gcam_elec_tech_mapping") ->
      L1233.globaltech_OMfixed_ATB

    L1233.globaltech_OMfixed_ATB_adv %>%
      add_title("2024 ATB-based fixed O&M costs in the Advanced case") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_OMfixed_ATB) ->
      L1233.globaltech_OMfixed_ATB_adv

    L1233.globaltech_OMfixed_ATB_low %>%
      add_title("2024 ATB-based fixed O&M costs in the Conservative case") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_OMfixed_ATB) ->
      L1233.globaltech_OMfixed_ATB_low

    L1233.globaltech_OMvar_ATB %>%
      add_title("2024 ATB-based variable O&M costs in the Moderate case") %>%
      add_units("1975$/MWh") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      add_precursors("energy/NREL_ATB_OMvar_2024",
                     "energy/ReEDS_power_plant_costs",
                     "energy/mappings/atb_gcam_elec_tech_mapping") ->
      L1233.globaltech_OMvar_ATB

    L1233.globaltech_OMvar_ATB_adv %>%
      add_title("2024 ATB-based variable O&M costs in the Advanced case") %>%
      add_units("1975$/MWh") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_OMvar_ATB) ->
      L1233.globaltech_OMvar_ATB_adv

    L1233.globaltech_OMvar_ATB_low %>%
      add_title("2024 ATB-based variable O&M costs in the Conservative case") %>%
      add_units("1975$/MWh") %>%
      add_comments("Some technologies filled in from ReEDS model inputs; other imputed") %>%
      same_precursors_as(L1233.globaltech_OMvar_ATB) ->
      L1233.globaltech_OMvar_ATB_low

    L1233.globaltech_capital_ATB_battery %>%
      add_title("2024 ATB-based utility-scale battery costs") %>%
      add_units("1975$/kW; 1975$/kW/yr; 1975$/MWh") %>%
      add_comments("All costs indicated in a single table") %>%
      same_precursors_as(L1233.globaltech_capital_ATB) ->
      L1233.globaltech_capital_ATB_battery

    return_data(L1233.globaltech_capital_ATB,
                L1233.globaltech_capital_ATB_adv,
                L1233.globaltech_capital_ATB_low,
                L1233.globaltech_OMfixed_ATB,
                L1233.globaltech_OMfixed_ATB_adv,
                L1233.globaltech_OMfixed_ATB_low,
                L1233.globaltech_OMvar_ATB,
                L1233.globaltech_OMvar_ATB_adv,
                L1233.globaltech_OMvar_ATB_low,
                L1233.globaltech_capital_ATB_battery)
  } else {
    stop("Unknown command")
  }
}
