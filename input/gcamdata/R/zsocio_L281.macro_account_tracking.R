# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L281.macro_account_tracking
#'
#' Generate tibbles to add "hooks" into GCAM to be able to track certain accounts such as
#' final energy service and net energy trade.  In addition generate the trial value market/resources
#' for those accounts as well as the capital investment tracking markets.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L281.BasePriceSectorMapping}, \code{L281.GlobalTechAccountOutputUseBasePrice_fd}, \code{L281.TrialValueResource}
#' \code{L281.TechAccountOutput_entrade}, \code{L281.TechAccountInput_entrade}, \code{L281.TechAccountInput_NG_entrade}, \code{L281.GlobalTechAccountInput_entrade}.
#' \code{L280.factor_input_productivity}.
#' There is no corresponding file in the original data system.
#' @details National accounts data and GDP macro function parameters for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020
#'
module_socio_L281.macro_account_tracking <- function(command, ...) {

  all_fd_en_globaltech_names <-
     c("L232.GlobalTechEff_ind",
       "L2321.GlobalTechCoef_cement",
       "L2322.GlobalTechCoef_Fert",
       "L2323.GlobalTechCoef_iron_steel",
       "L2324.GlobalTechEff_Off_road",
       "L2325.GlobalTechEff_chemical",
       "L2326.GlobalTechCoef_aluminum",
       "L2328.GlobalTechCoef_food",
       "L2327.GlobalTechCoef_paper",
       "L244.StubTechEff_bld",
       "L254.StubTranTechCoef")

  all_fd_ag_globaltech_names <-
    c("L203.StubTech_demand_food",
      "L203.StubTech_demand_nonfood")

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A54.sector",
      # "Final demand" energy tracking
      all_fd_en_globaltech_names,
      # "Final demand" ag tracking
      all_fd_ag_globaltech_names,
      # Net energy trade tracking
      "L239.TechCoef_tra",
      "L243.TechCoef_TradedBio",
      "L239.TechCoef_reg",
      "L243.GlobalTechCoef_TotBio",
      "L2392.TechCoef_tra_NG",
      "L2392.TechCoef_reg_NG",
      # Ag trade tracking
      "L240.TechCoef_reg",
      "L240.TechCoef_tra",
      FILE = "emissions/mappings/USAbld_emission_mapping")

  MODULE_OUTPUTS <-
    c("L281.BasePriceSectorMapping",
      "L281.GlobalTechAccountOutputUseBasePrice_fd",
      "L281.CapResource",
      "L281.CapitalLink",
      "L281.TrialValueResource",
      "L281.TechAccountOutput_entrade",
      "L281.TechAccountInput_entrade",
      "L281.TechAccountInput_NG_entrade",
      "L281.GlobalTechAccountInput_entrade",
      "L281.TechAccountInput_agtrade",
      "L281.TechAccountOutput_agtrade")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- GCAM_region_ID <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # (1) L281.BasePriceSectorMapping ----
    # create a mapping of pass through sectors which we need to
    # ensure we calculate one consistent base price across all
    # of them

    L203.StubTech_demand_food %>%
      filter(stub.technology != "OtherMeat_Fish") -> L203.StubTech_demand_food

    L203.StubTech_demand_nonfood %>%
      filter(stub.technology != "OtherMeat_Fish") %>%
      filter(supplysector != "NonFoodDemand_woodpulp") -> L203.StubTech_demand_nonfood

    Ag.demand.sector <-
      c(unique(L203.StubTech_demand_food$supplysector),
        unique(L203.StubTech_demand_nonfood$supplysector)) %>%
      as_tibble() %>%
      select(from.sector = value, to.sector = value)

    A54.sector %>%
      select(from.sector = supplysector, to.sector = energy.final.demand) %>%
      distinct() %>%
      bind_rows(Ag.demand.sector) %>%
      add_title("Set up base price sector mapping") %>%
      add_units("NA") %>%
      add_comments("Sets up a sector mapping to make sure we calculate the base price") %>%
      add_comments("of the correct sector when pass-through sectors are involved") %>%
      add_precursors("energy/A54.sector", "L203.StubTech_demand_food",
                     "L203.StubTech_demand_nonfood",
                     all_fd_ag_globaltech_names) ->
      L281.BasePriceSectorMapping


    # (2) L281.GlobalTechAccountOutputUseBasePrice_fd ----
    ## energy service sector and price aggregation ----

    ## final energy calculations ----
    all_fd_en_globaltech_names %>%
      lapply(function(name) {
        d <- get(name)
        if("supplysector" %in% names(d)) {
          d <- rename(d, "sector.name" = "supplysector")
        }
        if("subsector" %in% names(d)) {
          d <- rename(d, "subsector.name" = "subsector")
        }
        if("tranSubsector" %in% names(d)) {
          d <- rename(d, "subsector.name" = "tranSubsector")
        }
        if("stub.technology" %in% names(d)) {
          d <- rename(d, "technology" = "stub.technology")
        }

        d %>%
          select("sector.name", "subsector.name", "technology", "year", "input.accounting" = "minicam.energy.input") %>%
          distinct()
      }) %>%
      bind_rows() %>%
      filter(sector.name %in% socioeconomics.FINAL_DEMAND_SECTORS) ->
      fd_en_global_tech_input_all


    # there are a few places that need to be updated if a new sector is broken out:
    # 1. socioeconomics.FINAL_DEMAND_SECTORS needs to be updated
    # 2. An appropriate input for the new sector needs to be included
    # 3. all_fd_en_globaltech_names also needs to be updated
    # given all of this it is a good idea to assert all the names match, granted this does
    # not protect against the case that a user didn't update any of these
    assertthat::assert_that(all.equal(sort(unique(fd_en_global_tech_input_all$sector.name)), sort(socioeconomics.FINAL_DEMAND_SECTORS)))

    # adjust for USA specific buildings structure
    USAbld_emission_mapping %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(fd_en_global_tech_input_all,
                               by=c("from.supplysector" = "sector.name",
                                    "from.subsector" = "subsector.name",
                                    "from.stub.technology" = "technology",
                                    "year")) %>%
      select(sector.name = to.supplysector,
             subsector.name = to.subsector,
             technology = to.stub.technology,
             year,
             input.accounting) %>%
      bind_rows(fd_en_global_tech_input_all, .) ->
      fd_en_global_tech_input_all

    ## all_ag_globaltech_names ----

    all_fd_ag_globaltech_names %>%
      lapply(function(name) {
        d <- get(name)
        if("supplysector" %in% names(d)) {
          d <- rename(d, "sector.name" = "supplysector")
        }
        if("subsector" %in% names(d)) {
          d <- rename(d, "subsector.name" = "subsector")
        }
        if("tranSubsector" %in% names(d)) {
          d <- rename(d, "subsector.name" = "tranSubsector")
        }
        if("stub.technology" %in% names(d)) {
          d <- rename(d, "technology" = "stub.technology")
        }

        d %>%
          select("sector.name", "subsector.name", "technology") %>%
          distinct()
      }) %>%
      bind_rows() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      # mutate(subsector.name = technology) -> # subsector has "Plant" and "Animal", should be the GCAM ag sector name, so assign technology to subsector
      #TODO: consider adding ag final demand sectors
      # filter(sector.name %in% socioeconomics.FINAL_DEMAND_SECTORS) ->
      fd_ag_global_tech_input_all

    # bind en and ag and create output.accounting
    fd_en_global_tech_input_all %>%
      select(-input.accounting) %>%
      distinct() %>%
      mutate(output.accounting = socioeconomics.EN_SERVICE_NAME,
             output.ratio = 1.0,
             use.base.price = 1) %>%
      bind_rows(fd_ag_global_tech_input_all %>%
                  filter(grepl("^NonFoodDemand", sector.name)) %>%
                  mutate(output.accounting = socioeconomics.AG_SERVICE_NAME,
                         output.ratio = 1.0,
                         use.base.price = 1)) %>%
      bind_rows(fd_ag_global_tech_input_all %>%
                  filter(grepl("^FoodDemand", sector.name)) %>%
                  mutate(output.accounting = socioeconomics.FOOD_SERVICE_NAME,
                         output.ratio = 1.0,
                         use.base.price = 0))->
      fd_accounting

    fd_accounting %>%
      add_title("Track final energy & ag service in currency value") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting secondary outputs to track final energy & ag service") %>%
      add_precursors(c(all_fd_en_globaltech_names,
                       all_fd_ag_globaltech_names,
                       "emissions/mappings/USAbld_emission_mapping")) ->
      L281.GlobalTechAccountOutputUseBasePrice_fd


    # (3) L281.TrialValueResource ----

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.EN_SERVICE_NAME,
             output.unit = "billion 1975$",
             price.unit = "billion 1975$",
             market = region,
             min.price = 0.0,
             max.price = 1.0e6) ->
      fd_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.AG_SERVICE_NAME,
             output.unit = "billion 1975$",
             price.unit = "billion 1975$",
             market = region,
             min.price = 0.0,
             max.price = 1.0e6) %>%
      bind_rows(fd_resource)->
      fd_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.AG_SERVICE_NAME,
             output.unit = "billion 1975$",
             price.unit = "billion 1975$",
             market = region,
             min.price = 0.0,
             max.price = 1.0e6) %>%
      bind_rows(
        GCAM_region_names %>%
          select(region) %>%
          mutate(trial.value.resource = socioeconomics.FOOD_SERVICE_NAME,
                 output.unit = "billion 1975$",
                 price.unit = "billion 1975$",
                 market = region,
                 min.price = 0.0,
                 max.price = 1.0e6)) %>%
      bind_rows(fd_resource)->
      fd_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.EN_TRADE_NAME,
             output.unit = "billion 1975$",
             price.unit = "billion 1975$",
             market = region,
             # We need to expand the range of min/max price
             # beyond the defaults as the values for this market
             # can actually exceed them
             min.price = -1.0e15,
             max.price = 1.0e15) ->
      entrade_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(resource = socioeconomics.CAPITAL_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "rate",
             market = region) ->
      cap_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(price.adjust = 1,
             demand.adjust = 1,
             market = region,
             linked.policy = socioeconomics.CAPITAL_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "rate") %>%
      repeat_add_columns(tibble(linked.ghg.policy = c(
        socioeconomics.EN_CAPITAL_MARKET_NAME,
        socioeconomics.AG_CAPITAL_MARKET_NAME,
        socioeconomics.EN_DURABLE_MARKET_NAME)))->
      cap_link_ghg

    cap_resource %>%
      add_title("Create a resource for capital investment") %>%
      add_units("NA") %>%
      add_comments("We just use the resource to create markets for capital") %>%
      add_comments("supply behavior is provided elsewhere in macro calculations") %>%
      add_precursors("common/GCAM_region_names") ->
      L281.CapResource

    cap_link_ghg %>%
      add_title("Create linked resource for various sources of demand for capital investment") %>%
      add_units("NA") %>%
      add_comments("We would like to easily track investments from various high level categories") %>%
      add_comments("such as Energy, Ag, or Consumer Durable.  However they will all share the same") %>%
      add_comments("price and aggregate demands into a single market.  Which we can accomplish using") %>%
      add_comments("linked markets") %>%
      add_precursors("common/GCAM_region_names") ->
      L281.CapitalLink

    bind_rows(fd_resource, entrade_resource) %>%
      add_title("Creates trial value markets for use with the various tracking accounts we need to use.") %>%
      add_units("Unitless") %>%
      add_comments("Trial value resources for final energy related tracking") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names") ->
      L281.TrialValueResource


    # (4) trade tracking ----

    # Note: this table will produce gas trade both the "old" way and the LNG/pipeline way
    # the batch chunks will need to filer accordingly and in this way we can still support
    # running with either gas trade approach
    L239.TechCoef_tra %>%
      bind_rows(L243.TechCoef_TradedBio,
                # TODO: statistical differences?
                L2392.TechCoef_tra_NG %>% filter(grepl('traded', supplysector))
      ) ->
      en_export

    ## Net fossil energy trade calculations ----
    en_export %>%
      rename(output.accounting = minicam.energy.input,
             output.ratio = coefficient) %>%
      mutate(output.accounting = socioeconomics.EN_TRADE_NAME,
             output.ratio = -1.0) ->
      L281.TechAccountOutput_entrade

    L239.TechCoef_reg %>%
      filter(grepl('import', subsector)) %>%
      select("region", "supplysector", "subsector", "technology", "year", "input.accounting" = "minicam.energy.input") %>%
      # Note: the AccountingInput can be used track the physical output as well as the currency
      # for energy trade we are just interested in the currency tracking, thus tracking.market is empty
      mutate(tracking.market = "",
             currency.tracking.market = socioeconomics.EN_TRADE_NAME) ->
      L281.TechAccountInput_entrade

    L2392.TechCoef_reg_NG %>%
      filter(grepl('import', subsector)) %>%
      select("region", "supplysector", "subsector", "subsector0", "technology", "year", "input.accounting" = "minicam.energy.input") %>%
      # Note: the AccountingInput can be used track the physical output as well as the currency
      # for energy trade we are just interested in the currency tracking, thus tracking.market is empty
      mutate(tracking.market = "",
             currency.tracking.market = socioeconomics.EN_TRADE_NAME) ->
      L281.TechAccountInput_NG_entrade

    L243.GlobalTechCoef_TotBio %>%
      filter(grepl('import', subsector.name)) %>%
      select("sector.name", "subsector.name", "technology", "year", "input.accounting" = "minicam.energy.input") %>%
      # Note: the AccountingInput can be used track the physical output as well as the currency
      # for energy trade we are just interested in the currency tracking, thus tracking.market is empty
      mutate(tracking.market = "",
             currency.tracking.market = socioeconomics.EN_TRADE_NAME) ->
      L281.GlobalTechAccountInput_entrade

    ## Ag trade tracking ----
    ### ag import tracking ----
    L240.TechCoef_reg %>%
      filter(grepl('import', subsector)) %>%
      select("region", "supplysector", "subsector", "technology", "year", "input.accounting" = "minicam.energy.input") %>%
      # Note: the AccountingInput can be used track the physical output as well as the value
      # for energy trade we are just interested in the value tracking, thus tracking.market is empty
      mutate(tracking.market = "",
             currency.tracking.market = socioeconomics.EN_TRADE_NAME) ->
      L281.TechAccountInput_agtrade

    ### ag export tracking ----
    L240.TechCoef_tra %>%
      rename(output.accounting = minicam.energy.input,
             output.ratio = coefficient) %>%
      mutate(output.accounting = socioeconomics.EN_TRADE_NAME,
             output.ratio = -1.0) ->
      L281.TechAccountOutput_agtrade


    # Produce outputs ----
    L281.TechAccountOutput_entrade %>%
      add_title("Net energy trade / exports accounting in supplysectors") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting secondary outputs to track regional exports of primary energy") %>%
      add_precursors("L239.TechCoef_tra", "L243.TechCoef_TradedBio", "L2392.TechCoef_tra_NG") ->
      L281.TechAccountOutput_entrade

    L281.TechAccountInput_entrade %>%
      add_title("Net energy trade / imports accounting in supplysectors") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting inputs to track regional imports of primary energy") %>%
      add_precursors("L239.TechCoef_reg") ->
      L281.TechAccountInput_entrade

    L281.TechAccountInput_NG_entrade %>%
      add_title("Net energy trade / imports accounting for NG trade") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting inputs to track regional imports of natural gas") %>%
      add_comments("This table has to be treated seperately since it has additional subsector nesting levels.") %>%
      add_precursors("L2392.TechCoef_reg_NG") ->
      L281.TechAccountInput_NG_entrade

    L281.GlobalTechAccountInput_entrade %>%
      add_title("Net energy trade / imports accounting in global techs") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting inputs to track regional imports of primary energy") %>%
      add_precursors("L243.GlobalTechCoef_TotBio") ->
      L281.GlobalTechAccountInput_entrade

    L281.TechAccountInput_agtrade %>%
      add_title("Net ag trade / imports accounting in global techs") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting inputs to track regional imports of primary ag") %>%
      add_precursors("L240.TechCoef_reg") ->
      L281.TechAccountInput_agtrade

    L281.TechAccountOutput_agtrade %>%
      add_title("Net ag trade / exports accounting in supplysectors") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting outputs to track regional exports of primary ag products") %>%
      add_precursors("L240.TechCoef_tra") ->
      L281.TechAccountOutput_agtrade

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}


