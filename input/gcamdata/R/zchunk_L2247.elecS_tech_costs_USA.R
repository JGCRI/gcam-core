# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2247.elecS_tech_costs_USA
#'
#' Generates add-ons to harmonize reference case generation technology costs for multiple load segments with AEO-2016.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2247.GlobalTechCapitalOnly_elecS_USA}, \code{L2247.GlobalIntTechCapitalOnly_elecS_USA},
#' \code{L2247.GlobalTechCapitalOnly_elecS_adv_USA}, \code{L2247.GlobalIntTechCapitalOnly_elecS_adv_USA},
#' \code{L2247.GlobalTechOMfixedOnly_elecS_adv_USA}, \code{L2247.GlobalIntTechOMfixedOnly_elecS_adv_USA},
#' \code{L2247.GlobalTechOMvar_elecS_adv_USA}, \code{L2247.GlobalIntTechOMvar_elecS_adv_USA}, \code{L2247.GlobalTechFCROnly_elecS_itc_USA},
#' \code{L2247.GlobalIntTechFCROnly_elecS_itc_USA}, \code{L2247.GlobalTechCost_ptc_USA}, \code{L2247.GlobalIntTechCost_ptc_USA}.
#' The corresponding file in the original data system was \code{L2247.elecS_tech_costs.R} (gcam-usa level2).
#' @details This chunk creates add-ons to harmonize reference case generation technology costs for multiple load segments with AEO-2016.
#' These assumptions are also consistent with the Base case scenario. For technologies in GCAM that are not listed in the AEO,
#' we simply use the cost assumptions from the global version of GCAM. This chunk also creates add-ons for Advanced Tech scenario
#' for the multiple load segments consistent with the Advanced technology scenario.
#' This chunk also creates add-ons for ITC and PTC policies for multiple load segments technologies.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr complete nesting replace_na
#' @author RC Sep 2018
module_gcamusa_L2247.elecS_tech_costs_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.itc_USA",
             FILE = "gcam-usa/A23.ptc_USA",
             FILE = "gcam-usa/A23.ptc_inttech_USA",
             FILE = "gcam-usa/A23.elecS_tech_mapping",
             FILE = "gcam-usa/A23.elecS_inttech_mapping",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             "L2234.GlobalTechCapital_elecS_USA",
             "L2234.GlobalIntTechCapital_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2247.GlobalTechFCROnly_elecS_itc_USA",
             "L2247.GlobalIntTechFCROnly_elecS_itc_USA",
             "L2247.GlobalTechCost_ptc_USA",
             "L2247.GlobalIntTechCost_ptc_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- supplysector <- Electric.sector <- sector <- subsector <- subsector_1 <- ratio <-
      sector.name <- subsector.name <- technology <- intermittent.technology <- Electric.sector.technology <-
      Electric.sector.intermittent.technology <- capital.overnight <- overnight.cost<- fixed.charge.rate <-
      GCAM.technology <- itc <- ptc <- OM.fixed <- OM.var <- OM.fixed.x <- OM.var.x <- OM.fixed.y <- OM.var.y <-
      input.cost <- minicam.non.energy.input <- adv.ratio <- OM <- input.OM.fixed <- input.OM.var <-
      technology.y <- to.technology <- subsector.name0 <- plant_type <- cooling_system <- water_type <-
      to.technology <- NULL

    # Load required inputs
    A23.itc_USA <- get_data(all_data, "gcam-usa/A23.itc_USA", strip_attributes = TRUE)
    A23.ptc_USA <- get_data(all_data, "gcam-usa/A23.ptc_USA", strip_attributes = TRUE)
    A23.ptc_inttech_USA <- get_data(all_data, "gcam-usa/A23.ptc_inttech_USA", strip_attributes = TRUE)
    A23.elecS_tech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping", strip_attributes = TRUE)
    A23.elecS_inttech_mapping <- get_data(all_data, "gcam-usa/A23.elecS_inttech_mapping", strip_attributes = TRUE)
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool", strip_attributes = TRUE)
    L2234.GlobalTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_USA", strip_attributes = TRUE)
    L2234.GlobalIntTechCapital_elecS_USA <- get_data(all_data, "L2234.GlobalIntTechCapital_elecS_USA", strip_attributes = TRUE)

    # -----------------------------------------------------------------------------
    # 2. Build tables for CSVs
    # Investment Tax Credits and Production Tax Credits

    # Adjust FCRs by 1-ITC for technologies that have ITC
    A23.elecS_tech_mapping %>%
      bind_rows(A23.elecS_inttech_mapping %>%
                  rename(Electric.sector.technology = Electric.sector.intermittent.technology,
                         technology = intermittent.technology)) %>%
      select(Electric.sector.technology, technology) %>%
      # filter for technologies which are included in the ITC policy
      semi_join(A23.itc_USA, by = c("technology" = "GCAM.technology")) %>%
      # join is intended to duplicate rows (from single elec. sector techs to elec segments techs)
      # LJENM throws error, so left_join is used
      left_join(A23.itc_USA, by = c("technology" = "GCAM.technology")) %>%
      select(year, Electric.sector.technology, itc) -> A23.itc_elecS_USA

    L2234.GlobalTechCapital_elecS_USA %>%
      # filter for technologies which are included in the ITC policy
      semi_join(A23.itc_elecS_USA, by = c("technology" = "Electric.sector.technology", "year")) %>%
      left_join_error_no_match(A23.itc_elecS_USA, by = c("year", "technology" = "Electric.sector.technology")) %>%
      mutate(fixed.charge.rate = fixed.charge.rate * (1 - itc)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechFCROnly"]]) ->
      L2247.GlobalTechFCROnly_elecS_itc_USA

    L2234.GlobalIntTechCapital_elecS_USA %>%
      # filter for technologies which are included in the ITC policy
      semi_join(A23.itc_elecS_USA,
                by = c("intermittent.technology" = "Electric.sector.technology", "year")) %>%
      left_join_error_no_match(A23.itc_elecS_USA,
                               by = c("year", "intermittent.technology" = "Electric.sector.technology")) %>%
      mutate(fixed.charge.rate = fixed.charge.rate * (1 - itc)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechFCROnly"]]) ->
      L2247.GlobalIntTechFCROnly_elecS_itc_USA

    # Build table to read in PTC as cost adder (subtracter)
    A23.ptc_USA %>%
      # join is intended to duplicate rows (from single elec. sector techs to elec segments techs)
      # LJENM throws error, so left_join is used
      left_join(A23.elecS_tech_mapping, by = c("supplysector", "subsector" = "subsector_1", "technology")) %>%
      select(sector.name = Electric.sector, subsector.name = subsector, technology = Electric.sector.technology,
             year, minicam.non.energy.input, input.cost) ->
      L2247.GlobalTechCost_ptc_USA

    A23.ptc_inttech_USA %>%
      # join is intended to duplicate rows (from single elec. sector techs to elec segments techs)
      # LJENM throws error, so left_join is used
      left_join(A23.elecS_inttech_mapping, by = c("supplysector", "subsector" = "subsector_1", "intermittent.technology")) %>%
      select(sector.name = Electric.sector, subsector.name = subsector, intermittent.technology = Electric.sector.intermittent.technology,
             year, minicam.non.energy.input, input.cost) ->
      L2247.GlobalIntTechCost_ptc_USA


    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs
    add_cooling_techs <- function(data){
      data_new <- data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("technology"="Electric.sector.technology",
                       "sector.name"="Electric.sector","subsector.name"="subsector")) %>%
        select(-subsector_1,-technology.y)%>%
        rename(subsector.name0=subsector.name,
               subsector.name=technology,
               technology = to.technology)%>%
        arrange(sector.name,year) %>%
        mutate(technology = if_else(subsector.name0=="wind"|subsector.name0=="solar",subsector.name,
                                    if_else(subsector.name0=="grid_storage",subsector.name0,technology)))
      return(data_new)
    }

    L2247.GlobalTechFCROnly_elecS_itc_USA <- add_cooling_techs(L2247.GlobalTechFCROnly_elecS_itc_USA)
    L2247.GlobalTechCost_ptc_USA <- add_cooling_techs(L2247.GlobalTechCost_ptc_USA)


    add_cooling_int_techs <- function(data){
      data_new <- data %>%
        filter(!grepl("CSP",intermittent.technology)) %>%
        rename(subsector.name0=subsector.name, subsector.name = intermittent.technology) %>%
        mutate(intermittent.technology=subsector.name) %>%
        bind_rows(
      data %>%
        filter(grepl("CSP",intermittent.technology)) %>%
          left_join(A23.elecS_tech_mapping_cool,
                  by=c("intermittent.technology"="Electric.sector.technology",
                       "sector.name"="Electric.sector", "subsector.name"="subsector")) %>%
        select(-technology,-subsector_1, -supplysector, -plant_type, -cooling_system, -water_type)%>%
        rename(subsector.name0=subsector.name,
               subsector.name = intermittent.technology,
               intermittent.technology = to.technology) ) %>%
        arrange(sector.name,year)
      return(data_new)
    }

    L2247.GlobalIntTechFCROnly_elecS_itc_USA <- add_cooling_int_techs(L2247.GlobalIntTechFCROnly_elecS_itc_USA)
    L2247.GlobalIntTechCost_ptc_USA <- add_cooling_int_techs(L2247.GlobalIntTechCost_ptc_USA)


    # -----------------------------------------------------------------------------
    # Produce outputs

    L2247.GlobalTechFCROnly_elecS_itc_USA %>%
      add_title("Fixed charge rates for non-intermittent electricity sector technologies in USA") %>%
      add_units("Unitless") %>%
      add_comments("Adjust FCRs by 1 minus Investment Tax Credits for technologies that have ITC") %>%
      add_legacy_name("L2247.GlobalTechFCROnly_elecS_itc_USA") %>%
      add_precursors("gcam-usa/A23.itc_USA",
                     "gcam-usa/A23.elecS_tech_mapping",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2234.GlobalTechCapital_elecS_USA") ->
      L2247.GlobalTechFCROnly_elecS_itc_USA

    L2247.GlobalIntTechFCROnly_elecS_itc_USA %>%
      add_title("Fixed charge rates for intermittent electricity sector technologies in USA") %>%
      add_units("Unitless") %>%
      add_comments("Adjust FCRs by 1 minus Investment Tax Credits for technologies that have ITC") %>%
      add_legacy_name("L2247.GlobalIntTechFCROnly_elecS_itc_USA") %>%
      add_precursors("gcam-usa/A23.itc_USA",
                     "gcam-usa/A23.elecS_inttech_mapping",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "L2234.GlobalIntTechCapital_elecS_USA") ->
      L2247.GlobalIntTechFCROnly_elecS_itc_USA

    L2247.GlobalTechCost_ptc_USA %>%
      add_title("Production cost adjustments for non-intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/GJ") %>%
      add_comments("Read in Poduction Tax Credits as cost adder (subtracter)") %>%
      add_legacy_name("L2247.GlobalTechCost_ptc_USA") %>%
      add_precursors("gcam-usa/A23.ptc_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elecS_tech_mapping") ->
      L2247.GlobalTechCost_ptc_USA

    L2247.GlobalIntTechCost_ptc_USA %>%
      add_title("Production cost adjustments for intermittent electricity sector technologies in USA") %>%
      add_units("1975$US/GJ") %>%
      add_comments("Read in Poduction Tax Credits as cost adder (subtracter)") %>%
      add_legacy_name("L2247.GlobalIntTechCost_ptc_USA") %>%
      add_precursors("gcam-usa/A23.ptc_inttech_USA",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elecS_inttech_mapping") ->
      L2247.GlobalIntTechCost_ptc_USA


    return_data(L2247.GlobalTechFCROnly_elecS_itc_USA,
                L2247.GlobalIntTechFCROnly_elecS_itc_USA,
                L2247.GlobalTechCost_ptc_USA,
                L2247.GlobalIntTechCost_ptc_USA)
  } else {
    stop("Unknown command")
  }
}
