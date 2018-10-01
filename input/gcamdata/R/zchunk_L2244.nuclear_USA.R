#' module_gcam.usa_L2244.nuclear_USA
#'
#' Generates an add-on file to update nuclear assumptions in GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2244.StubTechSCurve_nuc_gen2_USA}, \code{L2244.StubTechShrwt_nuc_gen3_USA}.
#' The corresponding file in the original data system was \code{L2244.nuclear_USA.R} (gcam-usa level2).
#' @details This chunk creates an add-on file to update nuclear assumptions in GCAM-USA. Specifically, it reads in state-specific
#' s-curve retirement functions and lifetimes for existing nuclear vintage based on data put together by Dr. Son H. Kim.
#' The data reflect planned retirements by state and nuclear power plant. Further, the script updates share-weight assumptions for Gen 3 technology.
#' It assumes that no new nuclear is deployed in any state through 2035, except in GA, based on current understanding. It does not update
#' subsector share-weight assumptions in this add-on.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Sep 2018; edited MB Sep 2018
module_gcam.usa_L2244.nuclear_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/nuc_gen2",
             FILE = "gcam-usa/A23.elecS_tech_associations",
             FILE = "gcam-usa/A23.elecS_tech_availability"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2244.StubTechSCurve_nuc_gen2_USA",
             "L2244.StubTechShrwt_nuc_gen3_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- year <- supplysector <- Electric.sector <- sector <- subsector <- technology <- time <-
      Electric.sector.technology <- gen <- plant <- Units <- base_year_data <- evaluated_data <- data <-
      steepness <- half.life <- t <- isfirst_zero <- lifetime <- optim  <- share.weight <- NULL # silence package check notes

    # Load required inputs
    nuc_gen2 <- get_data(all_data, "gcam-usa/nuc_gen2")
    A23.elecS_tech_associations <- get_data(all_data, "gcam-usa/A23.elecS_tech_associations")
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability")

    # -----------------------------------------------------------------------------
    # Function to evaluate the output fraction for a smooth s-curve retirement function as coded in the model.
    evaluate_smooth_s_curve <- function(steepness, half.life, t) {
      s_curve_output_fraction <- 1 / (1 + exp(steepness * (t - half.life)))
      return(s_curve_output_fraction)
    }

    # The function, smooth_s_curve_approx_error, checks for the error between input nuc_gen2 trajectory data
    # and calculated generation using assumed retirement fuction.
    smooth_s_curve_approx_error <- function(nuc_gen2_data, par) {
      nuc_gen2_data %>%
        filter(time == 0) -> base_year_data

      nuc_gen2_data %>%
        mutate(base_year_data = base_year_data$data) %>%
        mutate(evaluated_data = if_else(time == 0, base_year_data,
                                        (base_year_data * evaluate_smooth_s_curve(par[1], par[2], nuc_gen2_data$time)))) %>%
        mutate(error = evaluated_data - data) -> nuc_gen2_data
      return(crossprod(nuc_gen2_data$error, nuc_gen2_data$error))
    }

    # Prepare a table by state, year, and desired nuc_gen2 generation in EJ.
    nuc_gen2 %>%
      gather(year, gen, -region, -plant, -Units) %>%
      mutate(year = as.integer(year))  %>%
      filter(year >= max(HISTORICAL_YEARS)) %>%
      mutate(time = year - max(HISTORICAL_YEARS)) %>%
      group_by(region, year, time) %>%
      summarise(gen = sum(gen)) %>%
      ungroup() %>%
      arrange(region, year) ->
      L2244.nuc_gen2_gen

    # Calculate lifetime for Gen II technology by state based on first year of zero generation in each state
    L2244.nuc_gen2_gen %>%
      group_by(region) %>%
      mutate(isfirst_zero = if_else(gen == 0 & !is.na(lag(gen)) & lag(gen) != 0, TRUE, FALSE)) %>%
      filter(isfirst_zero == TRUE) %>%
      select(region, lifetime = time) ->
      L2244.nuc_gen2_lifetime


    # Loop into states and find s-curve parameters such that the error between actual and calculated data is minimized
    L2244.nuc_gen2_s_curve_parameters <- tibble()

    for (L2244.state in unique(L2244.nuc_gen2_gen$region)) {

    L2244.nuc_gen2_gen %>%
        filter(region == L2244.state ) %>%
        rename(data = gen) ->
        L2244.nuc_gen2_gen_state

      parameters_optim <- optim(par = c(0.1, 30), f = smooth_s_curve_approx_error, nuc_gen2_data = L2244.nuc_gen2_gen_state)

      L2244.nuc_gen2_gen_state %>%
        mutate(steepness = parameters_optim$par[1], half.life = parameters_optim$par[2]) %>%
        select(region, steepness, half.life) %>%
        unique() ->
        L2244.nuc_gen2_s_curve_parameters_state


      L2244.nuc_gen2_s_curve_parameters %>%
        bind_rows(L2244.nuc_gen2_s_curve_parameters_state) ->
        L2244.nuc_gen2_s_curve_parameters

    }

    # Need to correct for negative coefficients.
    # This is rather arbitrary assumptions for now since VT is the only state with negative coefficinets and they retire capacity too soon.
    # The assumed parameters seemed to make the most sense.

    L2244.nuc_gen2_s_curve_parameters %>%
      mutate(half.life = replace(half.life, half.life <= 0, 0),
             steepness = replace(steepness, steepness <= 0, 0.6)) ->
      L2244.nuc_gen2_s_curve_parameters

    # Prepare table to read in s-curve parameters for base-year nuclear Gen II technology.
    # L2244.StubTechSCurve_elecS_nuc_gen2:  S-curve shutdown decider for historic U.S. nuclear plants

    A23.elecS_tech_associations %>%
      anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
      filter(subsector == "nuclear") %>%
      select(Electric.sector, subsector, Electric.sector.technology) ->
      L2244.nuc_stubtech

    L2244.nuc_stubtech %>%
      filter(!grepl("Gen III", Electric.sector.technology)) %>%
      repeat_add_columns(tibble(region = unique(L2244.nuc_gen2_gen$region))) %>%
      left_join_error_no_match(L2244.nuc_gen2_s_curve_parameters, by = "region") %>%
      left_join(L2244.nuc_gen2_lifetime, by ="region") %>%
      filter(!is.na(lifetime)) %>%
      mutate(year = max(BASE_YEARS)) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSCurve"]]) ->
      L2244.StubTechSCurve_nuc_gen2_USA


    # Prepare table to read in 0 shareweights for Gen III technologies in all states through 2030
    L2244.nuc_stubtech %>%
      filter(grepl("Gen III", Electric.sector.technology)) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      filter(year <= 2030) %>%
      mutate(share.weight = 0,
             # Adjust Gen III shareweight for GA which which has a plant under construction that's
             # expected to come online in the 2026-2030 time frame
             share.weight = replace(share.weight, region == "GA" & year == 2030, 1)) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]]) ->
      L2244.StubTechShrwt_nuc_gen3_USA

    # -----------------------------------------------------------------------------
    # Produce outputs
    L2244.StubTechSCurve_nuc_gen2_USA %>%
      add_title("S-curve shutdown decider for historic U.S. nuclear plants") %>%
      add_units("Unitless") %>%
      add_comments("Lifetime for Gen II technology by state is calculated based on first year of zero generation in each state.") %>%
      add_comments("S-curve parameters are based on minimized error between estimated and planned retirement data.") %>%
      add_legacy_name("L2244.StubTechSCurve_nuc_gen2_USA") %>%
      add_precursors("gcam-usa/nuc_gen2",
                     "gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_tech_associations") ->
      L2244.StubTechSCurve_nuc_gen2_USA

    L2244.StubTechShrwt_nuc_gen3_USA %>%
      add_title("Shareweights for Gen III nuclear technologies in all U.S. states through 2030") %>%
      add_units("Unitless") %>%
      add_comments("Zero shareweight for all states through 2030 except for GA in 2030 which has one plant under construction to be online") %>%
      add_legacy_name("L2244.StubTechShrwt_nuc_gen3_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_availability",
                     "gcam-usa/A23.elecS_tech_associations") ->
      L2244.StubTechShrwt_nuc_gen3_USA

    return_data(L2244.StubTechSCurve_nuc_gen2_USA, L2244.StubTechShrwt_nuc_gen3_USA)

  } else {
    stop("Unknown command")
  }
}
