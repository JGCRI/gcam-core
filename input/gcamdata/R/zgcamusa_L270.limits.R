# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L270.limits
#'
#' Add the 50 states to the USA market in each of the L270 limits polices.  In
#' particular to limit the fraction of liquid feedstocks and inputs to electricity
#' generation which can come from sources other than crude oil.  Constrain the
#' total amount of subsidy as a fraction of GDP which an economy is will to give
#' to have net negative emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L270.CreditMkt_USA}, \code{L270.CreditInput_elecS_USA}, \code{L270.NegEmissBudgetMaxPrice_USA},
#' \code{paste0( "L270.NegEmissBudget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)) )}.
#' @details Add 50 states to USA market for GCAM policy constraints which enforce limits
#' to liquid feedstocks and the amount of subsidies given for net negative emissions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter
#' @author PLP June 2018
module_gcamusa_L270.limits <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/A23.elecS_tech_availability",
             "L270.CreditOutput",
             "L270.CreditMkt",
             "L270.CreditInput_elec",
             "L270.NegEmissBudget"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditMkt_USA",
             "L270.CreditOutput_USA",
             "L270.CreditInput_elecS_USA",
             "L270.NegEmissBudget_USA"))
  } else if(command == driver.MAKE) {

    value <- subsector <- supplysector <- year <- GCAM_region_ID <- sector.name <-
      region <- scenario <- constraint <- . <- subsector_1 <- Electric.sector <-
      subsector.name<- Electric.sector.technology <- minicam.energy.input <-
      coefficient <- to.technology <- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool", strip_attributes = TRUE)
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability", strip_attributes = TRUE)
    L270.CreditMkt <- get_data(all_data, "L270.CreditMkt", strip_attributes = TRUE)
    L270.CreditOutput <- get_data(all_data, "L270.CreditOutput", strip_attributes = TRUE)
    L270.CreditInput_elec <- get_data(all_data, "L270.CreditInput_elec", strip_attributes = TRUE)
    L270.NegEmissBudget <- get_data(all_data, "L270.NegEmissBudget")

    # ===================================================
    # Data Processing

    L270.CreditMkt %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(names(L270.CreditMkt)) ->
      L270.CreditMkt_USA

    L270.CreditOutput %>%
      mutate(sector.name = "oil refining",
             subsector.name = "oil refining") ->
      L270.CreditOutput_USA

    L270.CreditInput_elec %>%
      # join is intended to duplicate rows
      # left_join_error_no_match throws error, so left_join is used
      # Altered to include cooling technologies in the global tech database
      left_join(A23.elecS_tech_mapping_cool,
                by = c("technology", "sector.name" = "supplysector", "subsector.name" = "subsector")) %>%
      select(-subsector_1, -sector.name)%>%
      # There are several electricity load segment / technology combinations that we think
      # do not make sense. These combinations are outlined in A23.elecS_tech_availability,
      # and are removed here.
      anti_join(A23.elecS_tech_availability,
                by = c("Electric.sector" = "supplysector",
                        "subsector.name" = "subsector",
                        "technology" = "stub.technology")) %>%
      select(Electric.sector, subsector.name, Electric.sector.technology, to.technology,
             year, minicam.energy.input, coefficient) %>%
      rename(sector.name = Electric.sector, subsector.name0 = subsector.name,
             subsector.name = Electric.sector.technology, technology = to.technology) ->
      L270.CreditInput_elecS_USA

    L270.NegEmissBudget %>%
      filter(region == gcam.USA_REGION) %>%
      # market is unchanged so as to share the USA policy
      write_to_all_states(names(L270.NegEmissBudget)) ->
      L270.NegEmissBudget_USA

    # ===================================================
    # Produce outputs

    L270.CreditMkt_USA %>%
      add_title("Add 50 states to the oil-credits RES market") %>%
      add_units("NA") %>%
      add_comments("Boiler plate and units for creating the actual") %>%
      add_comments("market for balancing oil-credits") %>%
      add_precursors("gcam-usa/states_subregions", "L270.CreditMkt") ->
      L270.CreditMkt_USA

    L270.CreditOutput_USA %>%
      add_title("Secondary output to add oil refining output to oil-credits market in GCAM-USA") %>%
      add_units("NA") %>%
      add_comments("The secondary output from L270.CreditOutput does not suffice in ") %>%
      add_comments("GCAM-USA because we renamed the sector / subsector thus the") %>%
      add_comments("global tech does not match.") %>%
      add_precursors("L270.CreditOutput") ->
      L270.CreditOutput_USA

    L270.CreditInput_elecS_USA %>%
      add_title("Creates demand of oil credits in GCAM_USA electricity load segment sectors") %>%
      add_units("Elec coef * constraint") %>%
      add_comments("Consumes oil-credits limiting the blend of refined liquids that can be used generate electricity") %>%
      add_comments("Adding GCAM_USA electricity load segment refined liquids technologies as consumers of oil-credits") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L270.CreditInput_elec") ->
      L270.CreditInput_elecS_USA

    L270.NegEmissBudget_USA %>%
      # inherit most attributes
      add_precursors("gcam-usa/states_subregions", "L270.NegEmissBudget") ->
      L270.NegEmissBudget_USA

    return_data(L270.CreditMkt_USA,
                L270.CreditOutput_USA,
                L270.CreditInput_elecS_USA,
                L270.NegEmissBudget_USA)
  } else {
    stop("Unknown command")
  }
}
