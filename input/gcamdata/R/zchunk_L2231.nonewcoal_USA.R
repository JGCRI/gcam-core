# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2231.nonewcoal_USA
#'
#' Generates optional moratorium on new pulverized coal plants in USA states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.StubTechShrwt_nonewcoal_USA}, \code{L2231.StubTechShrwt_coal_delay_USA}.
#' The corresponding file in the
#' original data system was \code{L2231.nonewcoal_USA.R} (gcam-usa level2).
#' @details This chunk sets zero share-weights of pulverized coal technologies, which assumes
#' no new pulverized coal plants without CCS will be built in USA states.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter mutate select
#' @author RC Aug 2018
module_gcamusa_L2231.nonewcoal_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elecS_tech_availability",
             "L222.StubTechMarket_en_USA",
             "L222.StubTech_en",
             "L225.StubTech_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.StubTechShrwt_nonewcoal_USA",
             "L2231.StubTechShrwt_coal_delay_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- year <- supplysector <- subsector <- stub.technology <- share.weight <-
      Electric.sector.technology <- Electric.sector <- NULL  # silence package check notes

    # Load required inputs
    L222.StubTechMarket_en_USA <- get_data(all_data, "L222.StubTechMarket_en_USA")
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2")
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping")
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability")

    # ===================================================
    # Perform computations

    A23.elecS_tech_mapping %>%
      # Remove the load segments and technology combinations that are not created in electricity sector
      anti_join(A23.elecS_tech_availability, by = c("Electric.sector.technology" = "stub.technology")) %>%
      arrange(subsector, Electric.sector) %>%
      mutate(Electric.sector = as.character(Electric.sector)) %>%
      select(supplysector = Electric.sector, subsector, stub.technology = Electric.sector.technology) %>%
      # Get the conventional coal technology without CCS
      filter(subsector == "coal", !grepl("CCS", stub.technology)) %>%
      bind_rows(tibble(supplysector = "industrial energy use", subsector = "coal", stub.technology = "coal cogen")) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], share.weight) ->
      L2231.StubTechShrwt_elec_USA

    L222.StubTechMarket_en_USA %>%
      filter(year %in% MODEL_FUTURE_YEARS, subsector == "coal to liquids", !grepl("CCS",stub.technology)) %>%
      distinct(region, supplysector, subsector, stub.technology, year) %>%
      mutate(share.weight = 0) ->
      L2231.StubTechShrwt_refining_USA

    L222.StubTech_en %>%
      filter(region == gcam.USA_REGION, subsector == "coal gasification", !grepl("CCS",stub.technology)) %>%
      bind_rows(L225.StubTech_h2 %>%
                  filter(region == gcam.USA_REGION, subsector == "coal", !grepl("CCS",stub.technology))) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], share.weight) ->
      L2231.StubTechShrwt_en_USA

    bind_rows(L2231.StubTechShrwt_elec_USA,
              L2231.StubTechShrwt_refining_USA,
              L2231.StubTechShrwt_en_USA) ->
      L2231.StubTechShrwt_nonewcoal_USA

    L2231.StubTechShrwt_nonewcoal_USA %>%
      filter(year <= gcamusa.FIRST_NEW_COAL_YEAR) ->
      L2231.StubTechShrwt_coal_delay_USA

    # ===================================================
    # Produce outputs

    L2231.StubTechShrwt_nonewcoal_USA %>%
      add_title("Share-weights for pulverized coal stub technologies in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for all coal pulverized stub technologies in all USA states and future years") %>%
      add_legacy_name("L2231.StubTechShrwt_coal_USA") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L222.StubTechMarket_en_USA",
                     "L222.StubTech_en",
                     "L225.StubTech_h2") ->
      L2231.StubTechShrwt_nonewcoal_USA

    L2231.StubTechShrwt_coal_delay_USA %>%
      add_title("Share-weights for pulverized coal stub technologies in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for all coal pulverized stub technologies in all USA states for near-future") %>%
      add_comments("New coal power deployment can begin in gcamusa.FIRST_NEW_COAL_YEAR (see constants.R; default is 2035) ") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L222.StubTechMarket_en_USA",
                     "L222.StubTech_en",
                     "L225.StubTech_h2") ->
      L2231.StubTechShrwt_coal_delay_USA

    return_data(L2231.StubTechShrwt_nonewcoal_USA,
                L2231.StubTechShrwt_coal_delay_USA)
  } else {
    stop("Unknown command")
  }
}
