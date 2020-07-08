# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L105.nh3_an_USA_S_T_Y
#'
#' Historical NH3 emissions factors for animals by GCAM technology, computed from EPA emissions data and FAO animal statistics
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.nh3_tgmt_USA_an_Yh}. The corresponding file in the
#' original data system was \code{L105.nh3_an_USA_S_T_Y.R} (emissions level1).
#' @details Historical NH3 emission factors for animal production are calculated from EPA NH3 estimates (1990 - 2002)
#' and FAO production statistics by GCAM sector and technology.
#' Emission factors for historical years (1971 - 1989) are estiamted as the emissions factor for 1990.
#' All historical NH3 Emission factors use US values.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter funs group_by left_join mutate select rename summarize summarize_at vars
#' @author RMH April 2017
module_emissions_L105.nh3_an_USA_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/mappings/EPA_tech",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             FILE = "emissions/EPA_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.nh3_tgmt_USA_an_Yh"))
  } else if(command == driver.MAKE) {

    sector <- fuel <- Source_Category_Raw <- Source_Category <- year <- value <-
      GCAM_region_ID <- emissions <- production <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    EPA_NH3 <- get_data(all_data, "emissions/EPA_NH3")

    # ===================================================

    # Process EPA NH3 emissions estimates:
    # Map to GCAM sector and Technology, convert to Tg and aggregate
    # Notes:
    # - when mapping from EPA sectors to GCAM sectors and technologies there should be NAs
    #   (don't map very aggregate or very detailed EPA sectors to avoid double counting)

    L105.nh3_tg_USA_an_Sepa_F_Yh <- EPA_NH3 %>%
      left_join(EPA_tech, by = c("Source_Category" = "EPA_Category")) %>% # map EPA NH3 emissions to GCAM sector/technology
      filter(sector == 'Animals',!is.na(fuel)) %>% # select only Animals to produce animal emission factors with FAO data
      select(-Source_Category_Raw, -Source_Category) %>%
      gather_years %>%
      filter(year %in% emissions.NH3_HISTORICAL_YEARS) %>%
      mutate(value = value * CONV_TST_TG) %>%  # convert from thousand short tons to Tg
      rename(emissions = value)

    # Process FAO production data:
    # Subset US data, aggregate to EPA sectors (all animal production)

    L105.out_Mt_USA_an_C_Sys_Fd_Yh <- L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      group_by(year) %>%
      summarize_at(vars(value), funs(sum)) %>%
      rename(production = value) %>%
      mutate(year = as.numeric(year))

    # Compute GHG emissions factors by dividing EPA inventory by FAO animal production
    L105.nh3_tgmt_USA_an_Yh <-  L105.nh3_tg_USA_an_Sepa_F_Yh %>%
      left_join_error_no_match(L105.out_Mt_USA_an_C_Sys_Fd_Yh, by = "year") %>%
      mutate(value = emissions/production) %>%
      select(-emissions, -production)

    # Add historical years (use 1990 estimate for other historical years (1971 - 1989) )
    extend_sector <- L105.nh3_tgmt_USA_an_Yh %>% filter(year == 1990) %>% select(sector) %>% as.character
    extend_fuel <- L105.nh3_tgmt_USA_an_Yh %>% filter(year == 1990) %>% select(fuel) %>% as.character
    extend_value <- L105.nh3_tgmt_USA_an_Yh %>% filter(year == 1990) %>% select(value)%>% as.numeric

    L105.nh3_tgmt_USA_an_Yh <- L105.nh3_tgmt_USA_an_Yh %>%
      bind_rows(tibble(sector = rep(extend_sector, times = length(emissions.NH3_EXTRA_YEARS)),
                       fuel = rep(extend_fuel, times = length(emissions.NH3_EXTRA_YEARS)),
                       year = emissions.NH3_EXTRA_YEARS,
                       value = rep(extend_value, times = length(emissions.NH3_EXTRA_YEARS)))) %>%
      arrange(year)

    # Produce outputs

    L105.nh3_tgmt_USA_an_Yh %>%
      add_title("NH3 emissions factors for animals by sector / technology / year estiamted from EPA NH3 emissions and FAO production statistics") %>%
      add_units("Tg/Mt") %>%
      add_comments("NH3 animal emission factors for all historical years are estimates from US values") %>%
      add_comments("Historical years 1971 - 1989 are estimated as 1990 values") %>%
      add_legacy_name("L105.nh3_tgmt_USA_an_Yh") %>%
      add_precursors("emissions/mappings/EPA_tech",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "emissions/EPA_NH3") ->
      L105.nh3_tgmt_USA_an_Yh

    return_data(L105.nh3_tgmt_USA_an_Yh)
  } else {
    stop("Unknown command")
  }
}
