# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2245.geothermal_fixed_USA
#'
#' Provide the option for users to fix future geothermal generation capabilities to base year values.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2245.DeleteRenewRsrc_USAgeo}, \code{L2245.DeleteSupplysector_USAgeo},
#' \code{L2245.GlobalTechShrwt_USAgeo},\code{L2245.StubTechFixOut_geothermal_USA},
#' \code{L2245.SubsectorLogit_USAgeo},\code{L2245.SubsectorLogit_cool_USAgeo}.
#' @details Update state-level geothermal outputs to fixed while deleting renewresource
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author NTG February 2022
module_gcamusa_L2245.geothermal_fixed_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c('L210.RenewRsrc_USA',
             'L2233.SubsectorShrwt_elecS_cool_USA',
             'L2233.GlobalTechShrwt_elecS_cool_USA',
             'L2233.SubsectorLogit_elecS_USA',
             'L2233.SubsectorLogit_elecS_cool_USA',
             'L2233.StubTechProd_elecS_cool_USA'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2245.DeleteRenewRsrc_USAgeo",
             "L2245.DeleteNestingSubsector1_USAgeo",
             "L2245.GlobalTechShrwt_USAgeo",
             "L2245.DeleteGlobalTech_USAgeo",
             "L2245.SubsectorLogit_USAgeo",
             "L2245.SubsectorLogit_cool_USAgeo",
             "L2245.StubTechFixOut_geothermal_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.RenewRsrc_USA <- get_data(all_data, "L210.RenewRsrc_USA", strip_attributes = TRUE)
    L2233.SubsectorShrwt_elecS_cool_USA <- get_data(all_data, "L2233.SubsectorShrwt_elecS_cool_USA", strip_attributes = TRUE)
    L2233.GlobalTechShrwt_elecS_cool_USA <- get_data(all_data, 'L2233.GlobalTechShrwt_elecS_cool_USA', strip_attributes = TRUE)
    L2233.SubsectorLogit_elecS_USA <- get_data(all_data,'L2233.SubsectorLogit_elecS_USA', strip_attributes = TRUE)
    L2233.SubsectorLogit_elecS_cool_USA <- get_data(all_data,'L2233.SubsectorLogit_elecS_cool_USA', strip_attributes = TRUE)
    L2233.StubTechProd_elecS_cool_USA <- get_data(all_data, 'L2233.StubTechProd_elecS_cool_USA', strip_attributes = TRUE)

    # Silence package checks
    subsector <- year <- fixedOutput <- region <- supplysector <- stub.technology <-
      share.weight.year <- subs.share.weight <- tech.share.weight <- subsector.name <-
      technology <- subsector0 <- renewresource <- share.weight <- calOutputValue <- NULL

    # ===================================================
    # Data Processing

    # Isolate geothermal renewable resource in states to allow for them to be deleted.
    L210.RenewRsrc_USA %>%
      filter(renewresource == "geothermal") %>%
      select(region, renewresource)  -> L2245.DeleteRenewRsrc_USAgeo

    # Isolate the supplysector, nesting subsector, and technology that create
    # structure for geo_base in USA electricity segments
    L2233.SubsectorShrwt_elecS_cool_USA %>%
      filter(subsector0 == "geothermal") %>%
      select(region, supplysector, subsector0) %>%
      unique()-> L2245.DeleteNestingSubsector1_USAgeo

    # As we are deleting the entire nesting-subsector of geothermal in the original
    # input XML, we need to pull in the logit exponents at both the subsector and
    # nesting-subsector levels for geothermal in the new input file.
    L2233.SubsectorLogit_elecS_USA %>%
      filter(subsector0 == "geothermal") ->
      L2245.SubsectorLogit_USAgeo

    L2233.SubsectorLogit_elecS_cool_USA %>%
      filter(subsector0 == "geothermal") ->
      L2245.SubsectorLogit_cool_USAgeo

    # Pull pre-defined share-weights of 'geo_base' in the global technology database
    # and set to 0 as all demand is fixed so share-weights are unnecessary
    L2233.GlobalTechShrwt_elecS_cool_USA %>%
      filter(subsector.name0 =="geothermal") %>%
      mutate(share.weight = 0) -> L2245.GlobalTechShrwt_USAgeo

    L2233.GlobalTechShrwt_elecS_cool_USA %>%
      filter(subsector.name0 =="geothermal") %>%
      select(-year, -share.weight) ->
      L2245.DeleteGlobalTech_USAgeo

    # Take all previously calibrated output for geothermal technologies and create
    # a new tibble which fixes the output of all future years to that of the base year
    L2233.StubTechProd_elecS_cool_USA %>%
      select(-share.weight.year) %>%
      filter(subsector0 == "geothermal",
             year == max(MODEL_BASE_YEARS)) %>%
      complete(year = MODEL_FUTURE_YEARS, nesting(region, supplysector, subsector0, subsector, technology,
                                                  subs.share.weight, tech.share.weight, calOutputValue)) %>%
      bind_rows(
        L2233.StubTechProd_elecS_cool_USA %>%
          filter(subsector0 == "geothermal",
                 year %in% MODEL_BASE_YEARS)
      ) %>%
      rename(fixedOutput = calOutputValue) %>%
      mutate(share.weight.year = year,
             fixedOutput = round(fixedOutput, energy.DIGITS_CALOUTPUT),
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      arrange(region,year) -> L2245.StubTechFixOut_geothermal_USA


    # ===================================================
    # Produce outputs

    L2245.DeleteRenewRsrc_USAgeo %>%
      add_title("Deleting Geothermal Renewable Resource in States which have availability") %>%
      add_units("unitless") %>%
      add_comments("Renewresource is deleted to allow for fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.DeleteRenewRsrc_USAgeo") %>%
      add_precursors('L210.RenewRsrc_USA') ->
      L2245.DeleteRenewRsrc_USAgeo

    L2245.DeleteNestingSubsector1_USAgeo %>%
      add_title("Deleting existing supplysector structure in GCAM-USA electricity segments") %>%
      add_units("unitless") %>%
      add_comments("Structure is deleted to allow for fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.DeleteNestingSubsector1_USAgeo") %>%
      add_precursors('L2233.SubsectorShrwt_elecS_cool_USA') ->
      L2245.DeleteNestingSubsector1_USAgeo

    L2245.GlobalTechShrwt_USAgeo %>%
      add_title("Set global technology share weights to 0 for fixedOutput geothermal in GCAM-USA") %>%
      add_units("unitless") %>%
      add_comments("Share-weights are set to 0 to eliminate competition and complete fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.GlobalTechShrwt_USAgeo") %>%
      add_precursors('L2233.GlobalTechShrwt_elecS_cool_USA') ->
      L2245.GlobalTechShrwt_USAgeo

    L2245.DeleteGlobalTech_USAgeo %>%
      add_title("Delete global technology database for fixedOutput geothermal in GCAM-USA") %>%
      add_units("unitless") %>%
      add_comments("Need to remove all associated costs in global technology database for fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.DeleteGlobalTech_USAgeo") %>%
      add_precursors('L2233.GlobalTechShrwt_elecS_cool_USA') ->
      L2245.DeleteGlobalTech_USAgeo

    L2245.SubsectorLogit_USAgeo %>%
      add_title("Pull out nesting-subsector logits for fixedOutput geothermal in GCAM-USA") %>%
      add_units("unitless") %>%
      add_comments("Logits are recalled from the deleted nesting-subsector for fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.SubsectorLogit_USAgeo") %>%
      add_precursors('L2233.SubsectorLogit_elecS_USA') ->
      L2245.SubsectorLogit_USAgeo

    L2245.SubsectorLogit_cool_USAgeo %>%
      add_title("Pull out subsector logits for fixedOutput geothermal in GCAM-USA") %>%
      add_units("unitless") %>%
      add_comments("Logits are recalled from the deleted subsector for fixedOutput geothermal in GCAM-USA") %>%
      add_legacy_name("L2245.SubsectorLogit_cool_USAgeo") %>%
      add_precursors('L2233.SubsectorLogit_elecS_cool_USA') ->
      L2245.SubsectorLogit_cool_USAgeo

    L2245.StubTechFixOut_geothermal_USA %>%
      add_title("Fixed Output values for geothermal electricity generation in GCAM-USA") %>%
      add_units("EJ") %>%
      add_comments("Values from final base year are held constant in the future to eliminate") %>%
      add_comments("the potential for growth in certain futures") %>%
      add_legacy_name("L2245.StubTechFixOut_geothermal_USA") %>%
      add_precursors('L2233.StubTechProd_elecS_cool_USA') ->
      L2245.StubTechFixOut_geothermal_USA


    return_data(L2245.DeleteRenewRsrc_USAgeo,
                L2245.DeleteNestingSubsector1_USAgeo,
                L2245.GlobalTechShrwt_USAgeo,
                L2245.DeleteGlobalTech_USAgeo,
                L2245.SubsectorLogit_USAgeo,
                L2245.SubsectorLogit_cool_USAgeo,
                L2245.StubTechFixOut_geothermal_USA)

  } else {
    stop("Unknown command")
  }
}
