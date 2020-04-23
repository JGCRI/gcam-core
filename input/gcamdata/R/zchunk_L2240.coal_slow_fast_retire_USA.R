# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2240.coal_slow_fast_retire_USA
#'
#' Generates GCAM-USA model input for removing coal capacity retired between 2010 and 2015.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2240.coal_conv_pul_delete_USA}, \code{L2240.StubTechProd_elecS_coal_USA},
#' \code{L2240.StubTechProd_elec_coalret_USA}, \code{L2240.StubTechEff_elec_coalret_USA}, \code{L2240.StubTechSCurve_elec_coalret_USA},
#' \code{L2240.StubTechMarket_elec_coalret_USA}, \code{L2240.GlobalTechShrwt_elec_coalret_USA}, \code{L2240.GlobalTechCapFac_elec_coalret_USA},
#' \code{L2240.GlobalTechCapital_elec_coalret_USA}, \code{L2240.GlobalTechOMfixed_elec_coalret_USA}, \code{L2240.GlobalTechOMvar_elec_coalret_USA},
#' \code{L2240.GlobalTechEff_elec_coalret_USA}, \code{L2240.GlobalTechProfitShutdown_elec_coalret_USA}.
#' The corresponding file in the original data system was \code{L2240.coal_slow_fast_retire_USA.R} (gcam-usa level2).
#' @details This chunk creates add-on files to take the fraction of reduction in coal electricity generation between 2010 and 2015 for each state and
#' forces that generation to retire in 2015. It also tempers retirement assumptions for the remaining coal fleet to allow
#' most 2015 generation to continue through mid-century.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author RC Aug 2018
module_gcamusa_L2240.coal_slow_fast_retire_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.coal_conv_pul_delete",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire",
             FILE = "gcam-usa/A23.elec_tech_coal_retire_SCurve",
             FILE = "gcam-usa/fraction_fast_retire_generation",
             "L2234.StubTechProd_elecS_USA",
             "L2234.StubTechEff_elecS_USA",
             "L2234.StubTechMarket_elecS_USA",
             "L2234.GlobalTechShrwt_elecS_USA",
             "L2234.GlobalTechCapFac_elecS_USA",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalTechOMfixed_elecS_USA",
             "L2234.GlobalTechOMvar_elecS_USA",
             "L2234.GlobalTechEff_elecS_USA",
             "L2234.GlobalTechProfitShutdown_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2240.coal_conv_pul_delete_USA",
             "L2240.StubTechProd_elecS_coal_USA",
             "L2240.StubTechProd_elec_coalret_USA",
             "L2240.StubTechEff_elec_coalret_USA",
             "L2240.StubTechSCurve_elec_coalret_USA",
             "L2240.StubTechMarket_elec_coalret_USA",
             "L2240.GlobalTechShrwt_elec_coalret_USA",
             "L2240.GlobalTechCapFac_elec_coalret_USA",
             "L2240.GlobalTechCapital_elec_coalret_USA",
             "L2240.GlobalTechOMfixed_elec_coalret_USA",
             "L2240.GlobalTechOMvar_elec_coalret_USA",
             "L2240.GlobalTechEff_elec_coalret_USA",
             "L2240.GlobalTechProfitShutdown_elec_coalret_USA"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- supplysector <- subsector <- stub.technology <- technology <- tech <- fast.retire <-
      Electric.sector <- Electric.sector.technology <- share.weight.year <- subs.share.weight <- share.weight <-
      calOutputValue <- capacity.factor <- input.capital <- capital.overnight <- fixed.charge.rate <-
      input.OM.fixed <- OM.fixed <- input.OM.var <- OM.var <- minicam.energy.input <- efficiency <-
      Non.CO2 <- input.emissions <- median.shutdown.point <- profit.shutdown.steepness <- NULL

    # Load required inputs
    A23.coal_conv_pul_delete <- get_data(all_data, "gcam-usa/A23.coal_conv_pul_delete")
    A23.elec_tech_mapping_coal_retire <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_coal_retire")
    A23.elec_tech_coal_retire_SCurve <- get_data(all_data, "gcam-usa/A23.elec_tech_coal_retire_SCurve") %>%
      select(-Electric.sector)
    fraction_fast_retire_generation <- get_data(all_data, "gcam-usa/fraction_fast_retire_generation")
    L2234.StubTechProd_elecS_USA <- get_data(all_data, "L2234.StubTechProd_elecS_USA")
    L2234.StubTechEff_elecS_USA <- get_data(all_data, "L2234.StubTechEff_elecS_USA")
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, "L2234.StubTechMarket_elecS_USA")
    L2234.GlobalTechShrwt_elecS_USA <- get_data(all_data, "L2234.GlobalTechShrwt_elecS_USA")
    L2234.GlobalTechCapFac_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapFac_elecS_USA")
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_USA")
    L2234.GlobalTechOMfixed_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMfixed_elecS_USA")
    L2234.GlobalTechOMvar_elecS_USA <- get_data(all_data, "L2234.GlobalTechOMvar_elecS_USA")
    L2234.GlobalTechEff_elecS_USA <- get_data(all_data, "L2234.GlobalTechEff_elecS_USA")
    L2234.GlobalTechProfitShutdown_elecS_USA <- get_data(all_data, "L2234.GlobalTechProfitShutdown_elecS_USA")

    # ===================================================
    # Perform computations

    # Delete old coal_subpeak_conv pul & coal_peak_conv pul stub-technologies in the states
    A23.coal_conv_pul_delete %>%
      write_to_all_states(c("region", "supplysector", "subsector", "stub.technology")) ->
      L2240.coal_conv_pul_delete_USA

    # Zero out calibrated production of coal_base_conv pul and coal_int_conv pul technologies
    L2234.StubTechProd_elecS_USA %>%
      rename(tech.share.weight = share.weight) %>%
      semi_join(A23.elec_tech_mapping_coal_retire %>%
                  select(supplysector = Electric.sector, subsector, stub.technology = technology) %>%
                  distinct(),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      anti_join(A23.coal_conv_pul_delete, by = c("supplysector", "subsector", "stub.technology")) %>%
      mutate(calOutputValue = 0, tech.share.weight = 0) ->
      L2240.StubTechProd_elecS_coal_USA

    # Prepare a table for stub technologies with all stats and base years
    A23.elec_tech_mapping_coal_retire %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) ->
      L2240.elec_USA_coalret_base

    # Create two technologies: coal_base_conv pul_slow_retire and coal_base_conv pul_fast_retire
    # L2240.StubTechProd_elec_USA_coalret:  Calibration outputs for conventional coal electricity plants by U.S. state
    L2240.elec_USA_coalret_base %>%
      left_join_error_no_match(L2234.StubTechProd_elecS_USA,
                               by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      left_join_error_no_match(fraction_fast_retire_generation, by = "region") %>%
      mutate(calOutputValue = replace(calOutputValue, grepl("fast_retire", stub.technology),
                                      calOutputValue[grepl("fast_retire", stub.technology)] * fast.retire[grepl("fast_retire", stub.technology)]),
             calOutputValue = replace(calOutputValue, grepl("slow_retire", stub.technology),
                                      calOutputValue[grepl("slow_retire", stub.technology)] * (1 - fast.retire[grepl("slow_retire", stub.technology)]))) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue, share.weight.year, subs.share.weight, tech.share.weight = share.weight) ->
      L2240.StubTechProd_elec_coalret_USA

    # Create a table to read in efficiencies for the new technologies in calibration years
    # L2240.StubTechEff_elec_USA_coalret: Efficiencies of U.S. conventional coal electricity plants in calibration years
    L2240.elec_USA_coalret_base %>%
      left_join_error_no_match(L2234.StubTechEff_elecS_USA,
                               by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      filter(efficiency != 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2240.StubTechEff_elec_coalret_USA

    # Create a table to read in s-curve retirement parameters for the new technologies
    # L2240.StubTechSCurve_elec_coalret:  S-curve shutdown decider for historic U.S. conventional coal electricity plants
    L2240.StubTechProd_elec_coalret_USA %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
      left_join_error_no_match(A23.elec_tech_coal_retire_SCurve,
                               by = c("stub.technology" = "Electric.sector.technology")) %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      L2240.StubTechSCurve_elec_coalret_USA


    # Create energy and non-energy inputs for the new technologies

    # Energy Inputs
    # L2240.StubTechMarket_elec_coalret:  Energy inputs
    L2240.elec_USA_coalret_base %>%
      complete(nesting(supplysector, subsector, stub.technology, technology, region), year = MODEL_YEARS) %>%
      left_join_error_no_match(L2234.StubTechMarket_elecS_USA,
                               by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L2240.StubTechMarket_elec_coalret_USA

    # Prepare a table for global technologies with all model years
    A23.elec_tech_mapping_coal_retire %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(supplysector = Electric.sector, tech = technology, technology = Electric.sector.technology) ->
      L2240.elec_USA_coalret

    # Share-weights
    # L2240.GlobalTechShrwt_elec_coalret: Shareweights for historic U.S. conventional coal electricity plants by load segment
    # Global techology shareweights set to zero for all model periods
    # Shareweights for stub.technologies which produce in base years are reset to 1 by L2240.StubTechProd_elec_coalret_USA
    # Shareweights for all future years are set to zero, as new deployment of the "retire" technologies will not occur
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechShrwt_elecS_USA, by = c("tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) %>%
      mutate(share.weight = 0) ->
      L2240.GlobalTechShrwt_elec_coalret_USA

    # Capacity factor
    # L2240.GlobalTechCapFac_elec_coalret: Capacity factors for historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechCapFac_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, capacity.factor) ->
      L2240.GlobalTechCapFac_elec_coalret_USA

    # Capital costs
    # L2240.GlobalTechCapital_elec_coalret: Capital costs of historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechCapital_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, input.capital, capital.overnight, fixed.charge.rate) ->
      L2240.GlobalTechCapital_elec_coalret_USA

    # Fixed OM costs
    # L2240.GlobalTechOMfixed_elec_coalret: Fixed OM costs of historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechOMfixed_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, input.OM.fixed, OM.fixed) ->
      L2240.GlobalTechOMfixed_elec_coalret_USA

    # Variable OM costs
    # L2240.GlobalTechOMvar_elec_coalret: Variable OM costs of historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechOMvar_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, input.OM.var, OM.var) ->
      L2240.GlobalTechOMvar_elec_coalret_USA

    # Efficiencies for future years - read in in the global technology database
    # L2240.GlobalTechEff_elec_coalret: Efficiencies of historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      left_join_error_no_match(L2234.GlobalTechEff_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, minicam.energy.input, efficiency) ->
      L2240.GlobalTechEff_elec_coalret_USA

    # Profit Shutdown decider
    # L2240.GlobalTechProfitShutdown_elec_coalret: Profit shut-down decider for historic U.S. conventional coal electricity plants by load segment
    L2240.elec_USA_coalret %>%
      filter(year >= max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L2234.GlobalTechProfitShutdown_elecS_USA,
                               by = c("supplysector", "subsector",  "tech" = "technology", "year")) %>%
      select(supplysector, subsector, technology, year, median.shutdown.point, profit.shutdown.steepness) ->
      L2240.GlobalTechProfitShutdown_elec_coalret_USA

    # ===================================================
    # Produce outputs

    L2240.coal_conv_pul_delete_USA %>%
      add_title("Stub-technology to be deleted in the states") %>%
      add_units("Unitless") %>%
      add_comments("Delete old coal_base_conv pul stub-technology in the states") %>%
      add_legacy_name("L2240.coal_conv_pul_delete_USA") %>%
      add_precursors("gcam-usa/A23.coal_conv_pul_delete") ->
      L2240.coal_conv_pul_delete_USA

    L2240.StubTechProd_elecS_coal_USA %>%
      add_title("Calibration outputs for conventional coal electricity technologies by load segment and U.S. state") %>%
      add_units("EJ") %>%
      add_comments("Zeroing out calibrated outpts for technologies that will be allocated to fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.coal_conv_pul_delete",
                     "gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.StubTechProd_elecS_USA") ->
    L2240.StubTechProd_elecS_coal_USA

    L2240.StubTechProd_elec_coalret_USA %>%
      add_title("Calibration outputs for conventional coal electricity plants by load segment and U.S. state") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity outpts are allocated to fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.StubTechProd_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "gcam-usa/fraction_fast_retire_generation",
                     "L2234.StubTechProd_elecS_USA") ->
      L2240.StubTechProd_elec_coalret_USA

    L2240.StubTechEff_elec_coalret_USA %>%
      add_title("Efficiencies of U.S. conventional coal electricity plants by load segment and state in calibration years") %>%
      add_units("Unitless") %>%
      add_comments("Set the same efficiency values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.StubTechEff_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.StubTechEff_elecS_USA") ->
      L2240.StubTechEff_elec_coalret_USA

    L2240.StubTechSCurve_elec_coalret_USA %>%
      add_title("S-curve shutdown decider for historic U.S. conventional coal electricity plants by load segment and state") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.StubTechSCurve_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "gcam-usa/A23.elec_tech_coal_retire_SCurve") ->
      L2240.StubTechSCurve_elec_coalret_USA

    L2240.StubTechMarket_elec_coalret_USA %>%
      add_title("Energy inputs of historic U.S. conventional coal electricity plants by load segment and state in all model years") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.StubTechMarket_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.StubTechMarket_elecS_USA") ->
      L2240.StubTechMarket_elec_coalret_USA

    L2240.GlobalTechShrwt_elec_coalret_USA %>%
      add_title("Shareweights for historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechShrwt_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechShrwt_elecS_USA") ->
      L2240.GlobalTechShrwt_elec_coalret_USA

    L2240.GlobalTechCapFac_elec_coalret_USA %>%
      add_title("Capacity factors for historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("Unitless") %>%
      add_comments("Set the same capacity factor values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechCapFac_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechCapFac_elecS_USA") ->
      L2240.GlobalTechCapFac_elec_coalret_USA

    L2240.GlobalTechCapital_elec_coalret_USA %>%
      add_title("Capital costs of historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("1975$ per kW; unitless (fixed.charge.rate)") %>%
      add_comments("Set the same capital cost values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechCapital_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechCapital_elecS_USA") ->
      L2240.GlobalTechCapital_elec_coalret_USA

    L2240.GlobalTechOMfixed_elec_coalret_USA %>%
      add_title("Fixed OM costs of historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Set the same fixed OM cost values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechOMfixed_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechOMfixed_elecS_USA") ->
      L2240.GlobalTechOMfixed_elec_coalret_USA

    L2240.GlobalTechOMvar_elec_coalret_USA %>%
      add_title("Variable OM costs of historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("1975$/MWh") %>%
      add_comments("Set the same variable OM cost values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechOMvar_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechOMvar_elecS_USA") ->
      L2240.GlobalTechOMvar_elec_coalret_USA

    L2240.GlobalTechEff_elec_coalret_USA %>%
      add_title("Efficiencies of historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("Unitless") %>%
      add_comments("Set the same efficiency values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechEff_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechEff_elecS_USA") ->
      L2240.GlobalTechEff_elec_coalret_USA

    L2240.GlobalTechProfitShutdown_elec_coalret_USA %>%
      add_title("Profit shut-down decider for historic U.S. conventional coal electricity plants by load segment and model year") %>%
      add_units("Unitless") %>%
      add_comments("Set the same values for fast retire and slow retire technologies") %>%
      add_legacy_name("L2240.GlobalTechProfitShutdown_elec_coalret_USA") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire",
                     "L2234.GlobalTechProfitShutdown_elecS_USA") ->
      L2240.GlobalTechProfitShutdown_elec_coalret_USA


    return_data(L2240.coal_conv_pul_delete_USA,
                L2240.StubTechProd_elecS_coal_USA,
                L2240.StubTechProd_elec_coalret_USA,
                L2240.StubTechEff_elec_coalret_USA,
                L2240.StubTechSCurve_elec_coalret_USA,
                L2240.StubTechMarket_elec_coalret_USA,
                L2240.GlobalTechShrwt_elec_coalret_USA,
                L2240.GlobalTechCapFac_elec_coalret_USA,
                L2240.GlobalTechCapital_elec_coalret_USA,
                L2240.GlobalTechOMfixed_elec_coalret_USA,
                L2240.GlobalTechOMvar_elec_coalret_USA,
                L2240.GlobalTechEff_elec_coalret_USA,
                L2240.GlobalTechProfitShutdown_elec_coalret_USA)

  } else {
    stop("Unknown command")
  }
}
