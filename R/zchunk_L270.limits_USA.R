# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.usa_L270.limits_USA
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
module_gcamusa_L270.limits_USA <- function(command, ...) {
  negative_emiss_input_names <- paste0("L270.NegEmissBudget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)) )
  negative_emiss_output_names <- sub('NegEmissBudget', 'NegEmissBudget_USA', negative_emiss_input_names)
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/A23.elecS_tech_availability",
             "L270.CreditOutput",
             "L270.CreditMkt",
             "L270.CreditInput_elec",
             "L270.NegEmissBudgetMaxPrice",
             negative_emiss_input_names))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditMkt_USA",
             "L270.CreditOutput_USA",
             "L270.CreditInput_elecS_USA",
             "L270.NegEmissBudgetMaxPrice_USA",
             # TODO: might just be easier to keep the scenarios in a single
             # table here and split when making XMLs but to match the old
             # data system we will split here
             negative_emiss_output_names))
  } else if(command == driver.MAKE) {

    value <- subsector <- supplysector <- year <- GCAM_region_ID <- sector.name <-
      region <- scenario <- constraint <- . <- subsector_1 <- Electric.sector <-
      subsector.name<- Electric.sector.technology <- minicam.energy.input <-
      coefficient<- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    A23.elecS_tech_availability <- get_data(all_data, "gcam-usa/A23.elecS_tech_availability")
    L270.CreditMkt <- get_data(all_data, "L270.CreditMkt")
    L270.CreditOutput <- get_data(all_data, "L270.CreditOutput")
    L270.CreditInput_elec <- get_data(all_data, "L270.CreditInput_elec")
    L270.NegEmissBudgetMaxPrice <- get_data(all_data, "L270.NegEmissBudgetMaxPrice")

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
                by=c("technology",
                     "sector.name"="supplysector","subsector.name" ="subsector")) %>%
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
      rename(sector.name = Electric.sector,subsector.name0=subsector.name, subsector.name = Electric.sector.technology, technology = to.technology)-> L270.CreditInput_elecS_USA

    L270.NegEmissBudgetMaxPrice %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(names(L270.NegEmissBudgetMaxPrice)) ->
      L270.NegEmissBudgetMaxPrice_USA




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

    L270.NegEmissBudgetMaxPrice_USA %>%
      add_title("A hint for the solver for what the max price of this market is") %>%
      add_units("%") %>%
      add_comments("This value is just used to give the solver a hint of the") %>%
      add_comments("range of prices which are valid.  For the negative emissions") %>%
      add_comments("budget constraint the price is a fraction from 0 to 1") %>%
      add_precursors("L270.NegEmissBudgetMaxPrice") ->
      L270.NegEmissBudgetMaxPrice_USA

    ret_data <- c("L270.CreditMkt_USA",
                  "L270.CreditOutput_USA",
                  "L270.CreditInput_elecS_USA",
                  "L270.NegEmissBudgetMaxPrice_USA")

    # Create the negative emissions GDP budget constraint limits

    # We will generate a bunch of tibbles for the negative emissions budgets for each scenario
    # and use assign() to save them to variables with names as L270.NegEmissBudget_[SCENARIO]
    # Note that since the call to assign() is in the for loop we must explicitly set the
    # environment to just outside of the loop:
    curr_env <- environment()
    for(i in seq_along(negative_emiss_input_names)) {
      curr_data <- get_data(all_data, negative_emiss_input_names[i])
      curr_data %>%
        filter(region == gcam.USA_REGION) %>%
        write_to_all_states(names(curr_data)) %>%
        add_title(paste0("The negative emissions budget in scenario ", negative_emiss_input_names[i])) %>%
        add_units("mil 1990$") %>%
        add_comments("The budget a market is willing to subsidize negative emissions") %>%
        add_precursors(negative_emiss_input_names[i]) %>%
        assign(negative_emiss_output_names[i], ., envir = curr_env)

      ret_data <- c(ret_data, negative_emiss_output_names[i])
    }

    # Call return_data but we need to jump through some hoops since we generated some of the
    # tibbles from the scenarios so we will generate the call to return_data
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()
  } else {
    stop("Unknown command")
  }
}
