# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2244.nuclear_USA
#'
#' Generates an add-on file to update nuclear assumptions in GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2244.StubTechSCurve_nuc_gen2_USA}.
#' The corresponding file in the original data system was \code{L2244.nuclear_USA.R} (gcam-usa level2).
#' @details This chunk creates an add-on file to update nuclear assumptions in GCAM-USA. Specifically, it reads in state-specific
#' s-curve retirement functions and lifetimes for existing nuclear vintage, based on planned retirements by state and nuclear power plant.
#' The current and future power generation data from existing nuclear plants in the USA is based on US Nuclear Regulatory Commission (NRC) of
#' nuclear reactor characteristic and operational history from January 2011, with additional updates to operating licenses and plant closures.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter lag mutate select
#' @importFrom tidyr gather
#' @author RC Sep 2018; edited MB Sep 2018
module_gcamusa_L2244.nuclear_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/nuc_gen2",
             FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/A23.elecS_tech_availability"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2244.StubTechSCurve_nuc_gen2_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- year <- supplysector <- Electric.sector <- sector <- subsector <- technology <- time <-
      Electric.sector.technology <- gen <- plant <- Units <- base_year_data <- evaluated_data <- data <-
      steepness <- half.life <- t <- isfirst_zero <- lifetime <- optim <- NULL # silence package check notes

    # Load required inputs
    nuc_gen2 <- get_data(all_data, "gcam-usa/nuc_gen2")
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping")
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability")

    # -----------------------------------------------------------------------------
    # Function to evaluate the output fraction for a smooth s-curve retirement function as coded in the model.
    evaluate_smooth_s_curve <- function(steepness, half.life, t) {
      1 / (1 + exp(steepness * (t - half.life)))
    }

    # The function, smooth_s_curve_approx_error, checks for the error between input nuc_gen2 trajectory data
    # and calculated generation using assumed retirement fuction.
    smooth_s_curve_approx_error <- function(nuc_gen2_data, par) {
      nuc_gen2_data %>%
        mutate(evaluated_data = if_else(time == 0, data,
                                        (data[time == 0] * evaluate_smooth_s_curve(par[1], par[2], nuc_gen2_data$time))),
               error = evaluated_data - data) ->
        nuc_gen2_data
      crossprod(nuc_gen2_data$error, nuc_gen2_data$error)
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
    L2244.nuc_gen2_s_curve_parameters <- list()

    for (L2244.state in unique(L2244.nuc_gen2_gen$region)) {

    L2244.nuc_gen2_gen %>%
        filter(region == L2244.state) %>%
        rename(data = gen) ->
        L2244.nuc_gen2_gen_state

      parameters_optim <- optim(par = c(0.1, 30), f = smooth_s_curve_approx_error, nuc_gen2_data = L2244.nuc_gen2_gen_state)

      L2244.nuc_gen2_gen_state %>%
        mutate(steepness = parameters_optim$par[1], half.life = parameters_optim$par[2]) %>%
        select(region, steepness, half.life) %>%
        unique() ->
        L2244.nuc_gen2_s_curve_parameters[[L2244.state]]

    }

    # Need to correct for negative coefficients.
    # This is rather arbitrary assumptions for now since VT is the only state with negative coefficinets and they retire capacity too soon.
    # The assumed parameters seemed to make the most sense.

    bind_rows(L2244.nuc_gen2_s_curve_parameters) %>%
      mutate(half.life = replace(half.life, half.life <= 0, 0),
             steepness = replace(steepness, steepness <= 0, 0.6)) ->
      L2244.nuc_gen2_s_curve_parameters

    # Prepare table to read in s-curve parameters for base-year nuclear Gen II technology.
    # L2244.StubTechSCurve_elecS_nuc_gen2: S-curve shutdown decider for historic U.S. nuclear plants

    A23.elecS_tech_mapping %>%
      anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
      filter(subsector == "nuclear") %>%
      select(Electric.sector, subsector, Electric.sector.technology) ->
      L2244.nuc_stubtech

    L2244.nuc_stubtech %>%
      filter(!grepl("Gen III", Electric.sector.technology)) %>%
      repeat_add_columns(tibble(region = unique(L2244.nuc_gen2_gen$region))) %>%
      left_join_error_no_match(L2244.nuc_gen2_s_curve_parameters, by = "region") %>%
      # Oregon (OR) has zero generation in the last historical period, thus no lifetime data
      # left_join_error_no_match throws error due to NA for Oregon, so left_join is used instead
      left_join(L2244.nuc_gen2_lifetime, by = "region") %>%
      filter(!is.na(lifetime)) %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      rename(supplysector = Electric.sector, stub.technology = Electric.sector.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSCurve"]]) ->
      L2244.StubTechSCurve_nuc_gen2_USA


    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs
    add_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology) -> data_new

      data_new %>% filter(grepl("seawater",technology)) %>% filter(!(region %in% gcamusa.NO_SEAWATER_STATES)) %>%
        bind_rows(data_new %>% filter(!grepl("seawater",technology))) %>%
        arrange(region,year) -> data_new
      return(data_new)
    }
      L2244.StubTechSCurve_nuc_gen2_USA <- add_cooling_techs(L2244.StubTechSCurve_nuc_gen2_USA)

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
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elecS_tech_mapping") ->
      L2244.StubTechSCurve_nuc_gen2_USA

    return_data(L2244.StubTechSCurve_nuc_gen2_USA)

  } else {
    stop("Unknown command")
  }
}
