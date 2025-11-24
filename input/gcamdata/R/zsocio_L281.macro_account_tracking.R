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

  all_fd_globaltech_names <-
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

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A54.sector",
      # Final energy tracking
      all_fd_globaltech_names,
      # Net energy trade tracking
      "L239.TechCoef_tra",
      "L243.TechCoef_TradedBio",
      "L239.TechCoef_reg",
      "L243.GlobalTechCoef_TotBio",
      "L2392.TechCoef_tra_NG",
      "L2392.TechCoef_reg_NG")

  MODULE_OUTPUTS <-
    c("L281.BasePriceSectorMapping",
      "L281.GlobalTechAccountOutputUseBasePrice_fd",
      "L281.TrialValueResource",
      "L281.TechAccountOutput_entrade",
      "L281.TechAccountInput_entrade",
      "L281.TechAccountInput_NG_entrade",
      "L281.GlobalTechAccountInput_entrade")


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    # 1. Read data

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # (1) L281.BasePriceSectorMapping ----
    # create a mapping of pass through sectors which we need to
    # ensure we calculate one consistent base price across all
    # of them
    A54.sector %>%
      select(from.sector = supplysector, to.sector = energy.final.demand) %>%
      distinct() %>%
      add_title("Set up base price sector mapping") %>%
      add_units("NA") %>%
      add_comments("Sets up a sector mapping to make sure we calculate the base price") %>%
      add_comments("of the correct sector when pass-through sectors are involved") %>%
      add_precursors("energy/A54.sector") ->
      L281.BasePriceSectorMapping

    # (2) L281.GlobalTechAccountOutputUseBasePrice_fd ----
    ## energy service sector and price aggregation ----

    ## final energy calculations ----
    all_fd_globaltech_names %>%
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
      fd_global_tech_input_all

    # there are a few places that need to be updated if a new sector is broken out:
    # 1. socioeconomics.FINAL_DEMAND_SECTORS needs to be updated
    # 2. An appropriate input for the new sector needs to be included
    # 3. all_fd_globaltech_names also needs to be updated
    # given all of this it is a good idea to assert all the names match, granted this does
    # not protect against the case that a user didn't update any of these
    assertthat::assert_that(all.equal(sort(unique(fd_global_tech_input_all$sector.name)), sort(socioeconomics.FINAL_DEMAND_SECTORS)))

    fd_global_tech_input_all %>%
      select(-input.accounting) %>%
      distinct() %>%
      mutate(output.accounting = socioeconomics.EN_SERVICE_NAME,
             output.ratio = 1.0,
             use.base.price = 1) ->
      fd_accounting

    fd_accounting %>%
      add_title("Track final energy service in currency value") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting secondary outputs to track final energy service") %>%
      add_precursors("L232.GlobalTechEff_ind",
                     "L2321.GlobalTechCoef_cement",
                     "L2322.GlobalTechCoef_Fert",
                     "L2323.GlobalTechCoef_iron_steel",
                     "L2324.GlobalTechEff_Off_road",
                     "L2325.GlobalTechEff_chemical",
                     "L2326.GlobalTechCoef_aluminum",
                     "L2328.GlobalTechCoef_food",
                     "L2327.GlobalTechCoef_paper",
                     "L244.StubTechEff_bld",
                     "L254.StubTranTechCoef") ->
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
      mutate(trial.value.resource = socioeconomics.EN_CAPITAL_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "billion 1975$ per timestep",
             market = region,
             # We need to expand the range of min/max price
             # beyond the defaults as the values for this market
             # can actually exceed them
             min.price = 0.0,
             max.price = 1.0e15) ->
      en_cap_resource

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.EN_DURABLE_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "billion 1975$ per timestep",
             market = region,
             # We need to expand the range of min/max price
             # beyond the defaults as the values for this market
             # can actually exceed them
             min.price = 0.0,
             max.price = 1.0e15) ->
      en_CD_resource

    bind_rows(fd_resource, entrade_resource, en_cap_resource, en_CD_resource)  %>%
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

    # Net fossil energy trade calculations
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

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}


