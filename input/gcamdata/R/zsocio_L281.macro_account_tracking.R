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
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A54.sector",
             # Final energy tracking
             "L232.GlobalTechEff_ind",
             "L2321.GlobalTechCoef_cement",
             "L2322.GlobalTechCoef_Fert",
             "L2323.GlobalTechCoef_iron_steel",
             "L2324.GlobalTechEff_Off_road",
             "L2325.GlobalTechEff_chemical",
             "L2326.GlobalTechCoef_aluminum",
             "L244.StubTechEff_bld",
             "L254.StubTranTechCoef",
             # Net energy trade tracking
             "L239.TechCoef_tra",
             "L243.TechCoef_TradedBio",
             "L239.TechCoef_reg",
             "L243.GlobalTechCoef_TotBio",
             "L2392.TechCoef_tra_NG",
             "L2392.TechCoef_reg_NG"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L281.BasePriceSectorMapping",
             "L281.GlobalTechAccountOutputUseBasePrice_fd",
             "L281.TrialValueResource",
             "L281.TechAccountOutput_entrade",
             "L281.TechAccountInput_entrade",
             "L281.TechAccountInput_NG_entrade",
             "L281.GlobalTechAccountInput_entrade"))
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    # --------------------------------------------------	---------------------------
    # 1. Read data

    all_data <- list(...)[[1]]

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A54.sector <- get_data(all_data, "energy/A54.sector", strip_attributes = TRUE)

    all_fd_globaltech_names <- c("L232.GlobalTechEff_ind",
                                 "L2321.GlobalTechCoef_cement",
                                 "L2322.GlobalTechCoef_Fert",
                                 "L2323.GlobalTechCoef_iron_steel",
                                 "L2324.GlobalTechEff_Off_road",
                                 "L2325.GlobalTechEff_chemical",
                                 "L2326.GlobalTechCoef_aluminum",
                                 "L244.StubTechEff_bld",
                                 "L254.StubTranTechCoef")

    # Note: this table will produce gas trade both the "old" way and the LNG/pipeline way
    # the batch chunks will need to filer accordingly and in this way we can still support
    # running with either gas trade approach
    get_data(all_data, "L239.TechCoef_tra", strip_attributes = TRUE) %>%
      bind_rows(get_data(all_data, "L243.TechCoef_TradedBio", strip_attributes = TRUE),
                # TODO: statistical differences?
                get_data(all_data, "L2392.TechCoef_tra_NG", strip_attributes = TRUE) %>% filter(grepl('traded', supplysector))
      ) ->
      en_export

    L239.TechCoef_reg <- get_data(all_data, "L239.TechCoef_reg", strip_attributes = TRUE)
    L2392.TechCoef_reg_NG <- get_data(all_data, "L2392.TechCoef_reg_NG", strip_attributes = TRUE)
    L243.GlobalTechCoef_TotBio <- get_data(all_data, "L243.GlobalTechCoef_TotBio", strip_attributes = TRUE)



    # create a mapping of pass through sectors which we need to
    # ensure we calculate one consistent base price across all
    # of them
    A54.sector %>%
      select(from.sector = supplysector, to.sector = energy.final.demand) %>%
      distinct() ->
      L281.BasePriceSectorMapping


    # final energy calculations

    all_fd_globaltech_names %>%
      lapply(function(name) {
        d <- get_data(all_data, name, strip_attributes = TRUE)
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

    fd_global_tech_input_all %>%
      select(-input.accounting) %>%
      distinct() %>%
      mutate(output.accounting = socioeconomics.EN_SERVICE_NAME,
             output.ratio = 1.0,
             use.base.price = 1) ->
      fd_accounting

    GCAM_region_names %>%
      select(region) %>%
      mutate(trial.value.resource = socioeconomics.EN_SERVICE_NAME,
             output.unit = "billion 1975$",
             price.unit = "billion 1975$",
             market = region,
             min.price = 0.0,
             max.price = 1.0e6) ->
      fd_resource

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

    # ===================================================

    # Produce outputs

    L281.BasePriceSectorMapping %>%
      add_title("Set up base price sector mapping") %>%
      add_units("NA") %>%
      add_comments("Sets up a sector mapping to make sure we calculate the base price") %>%
      add_comments("of the correct sector when pass-through sectors are involved") %>%
      add_precursors("energy/A54.sector") ->
      L281.BasePriceSectorMapping

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
                     "L244.StubTechEff_bld",
                     "L254.StubTranTechCoef") ->
      L281.GlobalTechAccountOutputUseBasePrice_fd

    bind_rows(fd_resource, entrade_resource, en_cap_resource, en_CD_resource) %>%
      add_title("Creates trial value markets for use with the various tracking accounts we need to use.") %>%
      add_units("Unitless") %>%
      add_comments("Trial value resources for final energy related tracking") %>%
      add_legacy_name("NA") %>%
      add_precursors("common/GCAM_region_names") ->
      L281.TrialValueResource

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

    return_data(L281.BasePriceSectorMapping,
                L281.GlobalTechAccountOutputUseBasePrice_fd,
                L281.TrialValueResource,
                L281.TechAccountOutput_entrade,
                L281.TechAccountInput_entrade,
                L281.TechAccountInput_NG_entrade,
                L281.GlobalTechAccountInput_entrade)

  } else {
    stop("Unknown command")
  }
}
