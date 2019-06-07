#' module_gcamusa_L2247.elecS_tech_costs_USA
#'
#' Generates add-ons to harmonize reference case generation technology costs for multiple load segments with AEO-2016.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2247.GlobalTechCapitalOnly_elecS_USA}, \code{L2247.GlobalIntTechCapitalOnly_elecS_USA},
#' \code{L2247.GlobalTechCapitalOnly_elecS_adv_USA}, \code{L2247.GlobalIntTechCapitalOnly_elecS_adv_USA},
#' \code{L2247.GlobalTechOMfixedOnly_elecS_adv_USA}, \code{L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA},
#' \code{L2247.GlobalTechOMvar_elecS_adv_USA}, \code{L2247.GlobalIntTechOMvar_elecS_adv_USA}, \code{L2247.GlobalTechFCROnly_elecS_itc_USA},
#' \code{L2247.GlobalIntTechFCROnly_elecS_itc_USA}, \code{L2247.GlobalTechCost_ptc_USA}, \code{L2247.GlobalIntTechCost_ptc_USA}.
#' The corresponding file in the original data system was \code{L2247.elecS_tech_costs.R} (gcam-usa level2).
#' @details This chunk creates add-ons to harmonize reference case generation technology costs for multiple load segments with AEO-2016.
#' These assumptions are also consistent with the Base case scenario. For technologies in GCAM that are not listed in the AEO,
#' we simply use the cost assumptions from the global version of GCAM. This chunk also creates add-ons for Advanced Tech scenario
#' for the multiple load segments consistent with the Advanced technology scenario.
#' This chunk also creates add-ons for ITC and PTC policies for multiple load segments technologies.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Sep 2018
module_gcamusa_L2247.elecS_tech_costs_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.itc_USA",
             FILE = "gcam-usa/A23.ptc_USA",
             FILE = "gcam-usa/A23.ptc_inttech_USA",
             FILE = "gcam-usa/A23.elec_overnight_costs_USA",
             FILE = "gcam-usa/A23.elec_overnight_costs_adv_USA",
             FILE = "gcam-usa/A23.elec_OM_adv_USA",
             FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elecS_inttech_mapping",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalIntTechCapital_elecS_USA",
             "L2234.GlobalTechOMfixed_elecS_USA",
             "L2234.GlobalIntTechOMfixed_elecS_USA",
             "L2234.GlobalTechOMvar_elecS_USA",
             "L2234.GlobalIntTechOMvar_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2247.GlobalTechCapitalOnly_elecS_USA",
             "L2247.GlobalIntTechCapitalOnly_elecS_USA",
             "L2247.GlobalTechCapitalOnly_elecS_adv_USA",
             "L2247.GlobalIntTechCapitalOnly_elecS_adv_USA",
             "L2247.GlobalTechOMfixedOnly_elecS_adv_USA",
             "L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA",
             "L2247.GlobalTechOMvar_elecS_adv_USA",
             "L2247.GlobalIntTechOMvar_elecS_adv_USA",
             "L2247.GlobalTechFCROnly_elecS_itc_USA",
             "L2247.GlobalIntTechFCROnly_elecS_itc_USA",
             "L2247.GlobalTechCost_ptc_USA",
             "L2247.GlobalIntTechCost_ptc_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- supplysector <- Electric.sector <- sector <- subsector <- subsector_1 <- ratio <-
      sector.name <- subsector.name <- technology <- intermittent.technology <- Electric.sector.technology <-
      Electric.sector.intermittent.technology <- capital.overnight <- overnight.cost<- fixed.charge.rate <-
      GCAM.technology <- itc <- ptc <- OM.fixed <- OM.var <- OM.fixed.x <- OM.var.x <- OM.fixed.y <- OM.var.y <-
      input.cost <- minicam.non.energy.input <- adv.ratio <- OM <- input.OM.fixed <- input.OM.var <- NULL

    # Load required inputs
    A23.itc_USA <- get_data(all_data, "gcam-usa/A23.itc_USA")
    A23.ptc_USA <- get_data(all_data, "gcam-usa/A23.ptc_USA")
    A23.ptc_inttech_USA <- get_data(all_data, "gcam-usa/A23.ptc_inttech_USA")
    A23.elec_overnight_costs_USA <- get_data(all_data, "gcam-usa/A23.elec_overnight_costs_USA")
    A23.elec_overnight_costs_adv_USA <- get_data(all_data, "gcam-usa/A23.elec_overnight_costs_adv_USA")
    A23.elec_OM_adv_USA <- get_data(all_data, "gcam-usa/A23.elec_OM_adv_USA")
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping")
    A23.elecS_inttech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_inttech_mapping")
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_USA")
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechCapital_elecS_USA")
    L2234.GlobalTechOMfixed_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMfixed_elecS_USA")
    L2234.GlobalIntTechOMfixed_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechOMfixed_elecS_USA")
    L2234.GlobalTechOMvar_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMvar_elecS_USA")
    L2234.GlobalIntTechOMvar_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechOMvar_elecS_USA")

    # -----------------------------------------------------------------------------
    # 2. Build tables for CSVs
    # 2a. Build GCAM capital cost table for all model years from AEO/DOE data

    # Define the minimum and maximum year of the input AEO data
    max_year_AEO <- max(A23.elec_overnight_costs_USA$year)
    min_year_AEO <- min(A23.elec_overnight_costs_USA$year)

    # First, copy the min_year_AEO (2015) data to all GCAM base years and convert to right units
    A23.elec_overnight_costs_USA %>%
      complete(nesting(GCAM.technology), year = c(MODEL_BASE_YEARS, unique(A23.elec_overnight_costs_USA$year))) %>%
      group_by(GCAM.technology) %>%
      # Use the 2015 value for all base years
      mutate(overnight.cost = replace(overnight.cost, year %in% MODEL_BASE_YEARS, overnight.cost[year == min_year_AEO]),
             # Convert to 1975USD
             overnight.cost = overnight.cost * gdp_deflator(1975, base_year = 2013)) %>%
      filter(!is.na(overnight.cost)) ->
      L2247.elec_overnight_costs_USA

    # Next, copy the AEO costs over to the global technology database. For years beyond max_year_AEO (typically 2040), we assume that the
    # rate of technological change is the same as what is assumed in the core.

    # Technology capital costs
    L2234.GlobalTechCapital_elecS_USA %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(ratio = capital.overnight / capital.overnight[year == max_year_AEO]) %>%
      # Not all GCAM global technologies have AEO mappings, NA generated, use left_join instead of left_join_error_no_match
      left_join(A23.elecS_tech_mapping %>%
                  select(Electric.sector, subsector, Electric.sector.technology, GCAM.technology = technology),
                by = c("supplysector" = "Electric.sector", "subsector", "technology" = "Electric.sector.technology")) %>%
      # Not all GCAM global technologies have AEO estimates, NA generated, use left_join instead of left_join_error_no_match
      left_join(L2247.elec_overnight_costs_USA, by = c("year", "GCAM.technology")) %>%
      mutate(capital.overnight = replace(capital.overnight, !is.na(overnight.cost), overnight.cost[!is.na(overnight.cost)]),
             capital.overnight = replace(capital.overnight, year > max_year_AEO,
                                         capital.overnight[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
      ungroup %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]], GCAM.technology) ->
      L2247.GlobalTechCapital_elecS_USA

    # Intermittent-Technology capital costs
    L2234.GlobalIntTechCapital_elecS_USA %>%
      group_by(supplysector, subsector, intermittent.technology) %>%
      mutate(ratio = capital.overnight / capital.overnight[year == max_year_AEO]) %>%
      left_join_error_no_match(A23.elecS_inttech_mapping %>%
                  select(Electric.sector, subsector, Electric.sector.intermittent.technology, GCAM.technology = intermittent.technology),
                by = c("supplysector" = "Electric.sector", "subsector", "intermittent.technology" = "Electric.sector.intermittent.technology")) %>%
      left_join(L2247.elec_overnight_costs_USA, by = c("year", "GCAM.technology")) %>%
      mutate(capital.overnight = replace(capital.overnight, !is.na(overnight.cost), overnight.cost[!is.na(overnight.cost)]),
             capital.overnight = replace(capital.overnight, year > max_year_AEO,
                                         capital.overnight[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
      ungroup %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapital"]], GCAM.technology) ->
      L2247.GlobalIntTechCapital_elecS_USA

    # 2b. Investment Tax Credits and Production Tax Credits

    # Adjust FCRs by 1-ITC for technologies that have ITC
    L2247.GlobalTechCapital_elecS_USA %>%
      left_join(A23.itc_USA, by = c("year", "GCAM.technology")) %>%
      filter(!is.na(itc)) %>%
      mutate(fixed.charge.rate = fixed.charge.rate * (1 - itc)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechFCROnly"]]) ->
      L2247.GlobalTechFCROnly_elecS_itc_USA

    L2247.GlobalIntTechCapital_elecS_USA %>%
      left_join(A23.itc_USA, by = c("year", "GCAM.technology")) %>%
      filter(!is.na(itc)) %>%
      mutate(fixed.charge.rate = fixed.charge.rate * (1 - itc)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechFCROnly"]]) ->
      L2247.GlobalIntTechFCROnly_elecS_itc_USA

    # Clean up tables to be printed.
    # We don't need FCR and capacity factor assumptions in the tables to be printed since we did not update those assumptions
    L2247.GlobalTechCapital_elecS_USA %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapitalOnly"]]) ->
      L2247.GlobalTechCapitalOnly_elecS_USA

    L2247.GlobalIntTechCapital_elecS_USA %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapitalOnly"]]) ->
      L2247.GlobalIntTechCapitalOnly_elecS_USA

    # Build table to read in PTC as cost adder (subtracter)
    A23.ptc_USA %>%
      left_join(A23.elecS_tech_mapping, by = c("supplysector", "subsector" = "subsector_1", "technology")) %>%
      select(sector.name = Electric.sector, subsector.name = subsector, technology = Electric.sector.technology,
             year, minicam.non.energy.input, input.cost) ->
      L2247.GlobalTechCost_ptc_USA

    A23.ptc_inttech_USA %>%
      left_join(A23.elecS_inttech_mapping, by = c("supplysector", "subsector" = "subsector_1", "intermittent.technology")) %>%
      select(sector.name = Electric.sector, subsector.name = subsector, intermittent.technology = Electric.sector.intermittent.technology,
             year, minicam.non.energy.input, input.cost) ->
      L2247.GlobalIntTechCost_ptc_USA

    # 2c. Generate csvs for advanced costs. These tables contain advanced tech assumptions for technologies that are provided by DOE.For technologies
    # without advanced assumptions, we assume reference assumptions

    # Convert 2013USD to 1975USD
    A23.elec_overnight_costs_adv_USA %>%
      mutate(overnight.cost = overnight.cost * gdp_deflator(1975, base_year = 2013)) ->
      L2247.elec_overnight_costs_adv_USA

    # Get a list of advance technologies
    adv_tech_usa <- unique(A23.elec_overnight_costs_adv_USA$GCAM.technology)

    # Technology capital costs
    L2247.GlobalTechCapital_elecS_USA %>%
      group_by(sector.name, subsector.name, technology) %>%
      mutate(ratio = capital.overnight / capital.overnight[year == max_year_AEO]) %>%
      left_join(A23.elecS_tech_mapping %>%
                  select(Electric.sector, subsector, Electric.sector.technology, GCAM.technology = technology),
                by = c("sector.name" = "Electric.sector", "subsector.name" = "subsector",
                       "technology" = "Electric.sector.technology", "GCAM.technology")) %>%
      left_join(L2247.elec_overnight_costs_adv_USA, by = c("year", "GCAM.technology")) %>%
      mutate(capital.overnight = replace(capital.overnight, !is.na(overnight.cost), overnight.cost[!is.na(overnight.cost)]),
             capital.overnight = replace(capital.overnight, year > max_year_AEO,
                                         capital.overnight[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
      ungroup %>%
      filter(GCAM.technology %in% adv_tech_usa) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]], GCAM.technology) ->
      L2247.GlobalTechCapital_elecS_adv_USA

    # Then, create separate advanced tech cost trajectories for PV_storage, CSP_storage and wind_storage.
    # We assume that the the costs of these technologies reduce at the same rate as the corresponding intermittent variant until the
    # maximum year of aeo data and then at the same rate as reference asumptions

    L2247.elec_overnight_costs_adv_USA %>%
      filter(GCAM.technology %in% gcamusa.INT_TECH_LIST) %>%
      group_by(GCAM.technology) %>%
      # Calculate the cost reduction rate from the first AEO year
      mutate(adv.ratio = overnight.cost / overnight.cost[year == min_year_AEO]) %>%
      ungroup %>%
      mutate(GCAM.technology = paste0(GCAM.technology, "_storage")) ->
      L2247.elec_overnight_costs_adv_storage_USA

    L2247.GlobalTechCapital_elecS_USA %>%
      group_by(sector.name, subsector.name, technology) %>%
      mutate(ratio = capital.overnight / capital.overnight[year == max_year_AEO]) %>%
      left_join(A23.elecS_tech_mapping %>%
                  select(Electric.sector, subsector, Electric.sector.technology, GCAM.technology = technology),
                by = c("sector.name" = "Electric.sector", "subsector.name" = "subsector",
                       "technology" = "Electric.sector.technology", "GCAM.technology")) %>%
      filter(GCAM.technology %in% gcamusa.STORAGE_TECH_LIST) %>%
      left_join(L2247.elec_overnight_costs_adv_storage_USA, by = c("year", "GCAM.technology")) %>%
      mutate(capital.overnight = replace(capital.overnight, year > min_year_AEO & year <= max_year_AEO,
                                         capital.overnight[year == min_year_AEO] * adv.ratio[year > min_year_AEO & year <= max_year_AEO]),
             capital.overnight = replace(capital.overnight, year > max_year_AEO,
                                         capital.overnight[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]], GCAM.technology) %>%
      # Now, update the advanced tech cost table with above assumptions
      bind_rows(L2247.GlobalTechCapital_elecS_adv_USA %>% filter(!grepl("storage", GCAM.technology))) %>%
      arrange(technology, year) ->
      L2247.GlobalTechCapital_elecS_adv_USA

    # Clean up the advanced tech capital cost tables to get ready for printing.
    # We don't need FCR and capacity factor assumptions in the tables to be printed since we did not update those assumptions
    L2247.GlobalTechCapital_elecS_adv_USA %>%
      # Filtering out years through 2015 to make sure that the model outputs are not different in 2015 between a reference and advanced case.
      filter(year > min(MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapitalOnly"]]) ->
      L2247.GlobalTechCapitalOnly_elecS_adv_USA


    # Intermittent-Technology capital costs
    L2247.GlobalIntTechCapital_elecS_USA %>%
      group_by(sector.name, subsector.name, intermittent.technology) %>%
      mutate(ratio = capital.overnight / capital.overnight[year == max_year_AEO]) %>%
      left_join_error_no_match(A23.elecS_inttech_mapping %>%
                                 select(Electric.sector, subsector, Electric.sector.intermittent.technology,
                                        GCAM.technology = intermittent.technology),
                               by = c("sector.name" = "Electric.sector", "subsector.name" = "subsector",
                                      "intermittent.technology" = "Electric.sector.intermittent.technology", "GCAM.technology")) %>%
      left_join(L2247.elec_overnight_costs_adv_USA, by = c("year", "GCAM.technology")) %>%
      mutate(capital.overnight = replace(capital.overnight, !is.na(overnight.cost), overnight.cost[!is.na(overnight.cost)]),
             capital.overnight = replace(capital.overnight, year > max_year_AEO,
                                         capital.overnight[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
      ungroup %>%
      # Filtering out years through 2015 to make sure that the model outputs are not different in 2015 between a reference and advanced case.
      filter(year > min(MODEL_FUTURE_YEARS), GCAM.technology %in% adv_tech_usa) %>%
      # We don't need FCR and capacity factor assumptions in the tables to be printed since we did not update those assumptions
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapitalOnly"]]) ->
      L2247.GlobalIntTechCapitalOnly_elecS_adv_USA

    # The next section process advanced OM cost assumptions with four steps.

    # First, convert 2014USD to 1975USD
    A23.elec_OM_adv_USA %>%
      mutate(OM.fixed = OM.fixed * gdp_deflator(1975, base_year = 2014),
             OM.var = OM.var * gdp_deflator(1975, base_year = 2014)) ->
      L2247.elec_OM_adv_USA

    # Second, define a function to process and replace OM costs with AEO data
    replace_AEO_data <- function(data, tech_map) {

      assert_that(is_tibble(data))
      assert_that(is_tibble(tech_map))

      data %>%
        group_by(supplysector, subsector, technology) %>%
        mutate(ratio = OM / OM[year == max_year_AEO]) %>%
        left_join(tech_map, by = c("supplysector", "subsector", "technology")) %>%
        left_join(L2247.elec_OM_adv_USA, by = c("year", "GCAM.technology")) %>%
        mutate(OM = replace(OM, !is.na(OM.fixed), OM.fixed[!is.na(OM.fixed)]),
               OM = replace(OM, year > max_year_AEO, OM[year == max_year_AEO] * ratio[year > max_year_AEO])) %>%
        ungroup %>%
        filter(year > min(MODEL_FUTURE_YEARS), GCAM.technology %in% adv_tech_usa) %>%
        replace_na(list(OM = 0))
    }

    # Third, create technology mapping tibbles
    A23.elecS_tech_mapping %>%
      select(supplysector = Electric.sector, subsector, GCAM.technology = technology,
             technology = Electric.sector.technology) ->
      elecS_tech_map

    A23.elecS_inttech_mapping %>%
      select(supplysector = Electric.sector, subsector, GCAM.technology = intermittent.technology,
             technology = Electric.sector.intermittent.technology) ->
      elecS_inttech_map

    # Forth, apply the function to replace OM costs with AEO values
    # Advanced Technology fixed OM-costs
    L2234.GlobalTechOMfixed_elecS_USA %>%
      rename(OM = OM.fixed) %>%
      replace_AEO_data(tech_map = elecS_tech_map) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, input.OM.fixed, OM.fixed = OM) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2247.GlobalTechOMfixedOnly_elecS_adv_USA

    # Advanced Intermittent Technology fixed OM-costs
    L2234.GlobalIntTechOMfixed_elecS_USA %>%
      rename(OM = OM.fixed, technology = intermittent.technology) %>%
      replace_AEO_data(tech_map = elecS_inttech_map) %>%
      select(sector.name = supplysector, subsector.name = subsector, intermittent.technology = technology, year, input.OM.fixed, OM.fixed = OM) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechOMfixed"]]) ->
      L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA

    # Advanced Technology Variable OM
    L2234.GlobalTechOMvar_elecS_USA %>%
      rename(OM = OM.var) %>%
      replace_AEO_data(tech_map = elecS_tech_map) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, input.OM.var, OM.var = OM) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2247.GlobalTechOMvar_elecS_adv_USA

    # Advanced Intermittent Technology Variable OM
    L2234.GlobalIntTechOMvar_elecS_USA %>%
      rename(OM = OM.var, technology = intermittent.technology) %>%
      replace_AEO_data(tech_map = elecS_inttech_map) %>%
      select(sector.name = supplysector, subsector.name = subsector, intermittent.technology = technology, year, input.OM.var, OM.var = OM) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechOMvar"]]) ->
      L2247.GlobalIntTechOMvar_elecS_adv_USA

    # -----------------------------------------------------------------------------
    # Produce outputs
    L2247.GlobalTechCapitalOnly_elecS_USA %>%
      add_title("Overnight capital costs for non-intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Base Case scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalTechCapitalOnly_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elec_overnight_costs_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "L2234.GlobalTechCapital_elecS_USA") ->
      L2247.GlobalTechCapitalOnly_elecS_USA

    L2247.GlobalIntTechCapitalOnly_elecS_USA %>%
      add_title("Overnight capital costs for intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Base Case scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalIntTechCapitalOnly_elecS_USA") %>%
      add_precursors("gcam-usa/A23.elec_overnight_costs_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "L2234.GlobalIntTechCapital_elecS_USA") ->
      L2247.GlobalIntTechCapitalOnly_elecS_USA

    L2247.GlobalTechCapitalOnly_elecS_adv_USA %>%
      add_title("Overnight capital costs for non-intermittent electricity sector advanced technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalTechCapitalOnly_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_overnight_costs_adv_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "L2234.GlobalTechCapital_elecS_USA") ->
      L2247.GlobalTechCapitalOnly_elecS_adv_USA

    L2247.GlobalIntTechCapitalOnly_elecS_adv_USA %>%
      add_title("Overnight capital costs for intermittent electricity sector advanced technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalIntTechCapitalOnly_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_overnight_costs_adv_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "L2234.GlobalIntTechCapital_elecS_USA") ->
      L2247.GlobalIntTechCapitalOnly_elecS_adv_USA

    L2247.GlobalTechOMfixedOnly_elecS_adv_USA %>%
      add_title("Fixed OM costs for non-intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalTechOMfixedOnly_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_OM_adv_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "L2234.GlobalTechOMfixed_elecS_USA") ->
      L2247.GlobalTechOMfixedOnly_elecS_adv_USA

    L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA %>%
      add_title("Fixed OM capital costs for intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_OM_adv_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "L2234.GlobalIntTechOMfixed_elecS_USA") ->
      L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA

    L2247.GlobalTechOMvar_elecS_adv_USA %>%
      add_title("Variable OM costs for non-intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalTechOMvar_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_OM_adv_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "L2234.GlobalTechOMvar_elecS_USA") ->
      L2247.GlobalTechOMvar_elecS_adv_USA

    L2247.GlobalIntTechOMvar_elecS_adv_USA %>%
      add_title("Variable OM capital costs for intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/kw") %>%
      add_comments("Use the AEO Advance Tech scenario costs in the global technology database.") %>%
      add_comments("For years beyond AEO max year (2040), rate of technological change is assumed the same as in the core.") %>%
      add_legacy_name("L2247.GlobalIntTechOMvar_elecS_adv_USA") %>%
      add_precursors("gcam-usa/A23.elec_OM_adv_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "L2234.GlobalIntTechOMvar_elecS_USA") ->
      L2247.GlobalIntTechOMvar_elecS_adv_USA

    L2247.GlobalTechFCROnly_elecS_itc_USA %>%
      add_title("Fixed charge rates for non-intermittent electricity sector technologies in USA") %>%
      add_units("Unitless") %>%
      add_comments("Adjust FCRs by 1 minus Investment Tax Credits for technologies that have ITC") %>%
      add_legacy_name("L2247.GlobalTechFCROnly_elecS_itc_USA") %>%
      add_precursors("gcam-usa/A23.itc_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "L2234.GlobalTechCapital_elecS_USA") ->
      L2247.GlobalTechFCROnly_elecS_itc_USA

    L2247.GlobalIntTechFCROnly_elecS_itc_USA %>%
      add_title("Fixed charge rates for intermittent electricity sector technologies in USA") %>%
      add_units("Unitless") %>%
      add_comments("Adjust FCRs by 1 minus Investment Tax Credits for technologies that have ITC") %>%
      add_legacy_name("L2247.GlobalIntTechFCROnly_elecS_itc_USA") %>%
      add_precursors("gcam-usa/A23.itc_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "L2234.GlobalIntTechCapital_elecS_USA") ->
      L2247.GlobalIntTechFCROnly_elecS_itc_USA

    L2247.GlobalTechCost_ptc_USA %>%
      add_title("Production cost adjustments for non-intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/GJ") %>%
      add_comments("Read in Poduction Tax Credits as cost adder (subtracter)") %>%
      add_legacy_name("L2247.GlobalTechCost_ptc_USA") %>%
      add_precursors("gcam-usa/A23.ptc_USA",
                     "gcam-usa/A23.elecS_tech_mapping") ->
      L2247.GlobalTechCost_ptc_USA

    L2247.GlobalIntTechCost_ptc_USA %>%
      add_title("Production cost adjustments for intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/GJ") %>%
      add_comments("Read in Poduction Tax Credits as cost adder (subtracter)") %>%
      add_legacy_name("L2247.GlobalIntTechCost_ptc_USA") %>%
      add_precursors("gcam-usa/A23.ptc_inttech_USA",
                     "gcam-usa/A23.elecS_inttech_mapping") ->
      L2247.GlobalIntTechCost_ptc_USA


    return_data(L2247.GlobalTechCapitalOnly_elecS_USA,
                L2247.GlobalIntTechCapitalOnly_elecS_USA,
                L2247.GlobalTechCapitalOnly_elecS_adv_USA,
                L2247.GlobalIntTechCapitalOnly_elecS_adv_USA,
                L2247.GlobalTechOMfixedOnly_elecS_adv_USA,
                L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA,
                L2247.GlobalTechOMvar_elecS_adv_USA,
                L2247.GlobalIntTechOMvar_elecS_adv_USA,
                L2247.GlobalTechFCROnly_elecS_itc_USA,
                L2247.GlobalIntTechFCROnly_elecS_itc_USA,
                L2247.GlobalTechCost_ptc_USA,
                L2247.GlobalIntTechCost_ptc_USA)
  } else {
    stop("Unknown command")
  }
}
