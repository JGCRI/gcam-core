#' module_gcamindia_L242.building_agg_comm
#'
#' Calculate supply sector, subsector, and technology information for the comm sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.india_state_Supplysector_comm}, \code{L242.india_state_FinalEnergyKeyword_comm}, \code{L242.india_state_SubsectorLogit_comm},
#' \code{L242.india_state_SubsectorShrwtFllt_comm}, \code{L242.india_state_SubsectorInterp_comm}, \code{L242.india_state_StubTech_comm},
#' \code{L242.india_state_GlobalTechInterp_comm}, \code{L242.india_state_GlobalTechShrwt_comm}, \code{L242.india_state_GlobalTechEff_comm},
#' \code{L242.india_state_GlobalTechCost_comm}, \code{L242.india_state_StubTechCalInput_comm}, \code{L242.india_state_StubTechMarket_comm}, \code{L242.india_state_FuelPrefElast_comm},
#' \code{L242.india_state_PerCapitaBased_comm}, \code{L242.india_state_PriceElasticity_comm}, \code{L242.india_state_IncomeElasticity_comm}, \code{L242.india_state_BaseService_comm}. The corresponding file in the
#' original data system was \code{L242.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS August 2017
module_gcamindia_L242.building_agg_comm <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "gcam-india/A42.india_state_calibrated_tech_comm",
             FILE = "gcam-india/A42.india_state_sector_comm",
             FILE = "gcam-india/A42.india_state_subsector_interp_comm",
             FILE = "gcam-india/A42.india_state_subsector_logit_comm",
             FILE = "gcam-india/A42.india_state_subsector_shrwt_comm",
             FILE = "gcam-india/A42.india_state_globaltech_cost_comm",
             FILE = "gcam-india/A42.india_state_globaltech_eff_comm",
             FILE = "gcam-india/A42.india_state_globaltech_shrwt_comm",
             FILE = "gcam-india/A42.india_state_globaltech_interp_comm",
             FILE = "gcam-india/A42.india_state_fuelprefElasticity_comm",
             FILE = "gcam-india/A42.india_state_demand_comm",
             FILE = "gcam-india/A42.india_state_price_elasticity_comm",
             FILE = "gcam-india/A42.india_state_income_elasticity_comm",
             "L142.india_state_in_EJ_comm_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.india_state_Supplysector_comm",
             "L242.india_state_FinalEnergyKeyword_comm",
             "L242.india_state_SubsectorLogit_comm",
             "L242.india_state_SubsectorShrwtFllt_comm",
             "L242.india_state_SubsectorInterp_comm",
             "L242.india_state_StubTech_comm",
             "L242.india_state_GlobalTechInterp_comm",
             "L242.india_state_GlobalTechShrwt_comm",
             "L242.india_state_GlobalTechEff_comm",
             "L242.india_state_GlobalTechCost_comm",
             "L242.india_state_StubTechCalInput_comm",
             "L242.india_state_StubTechMarket_comm",
             "L242.india_state_FuelPrefElast_comm",
             "L242.india_state_PerCapitaBased_comm",
             "L242.india_state_PriceElasticity_comm",
             "L242.india_state_IncomeElasticity_comm",
             "L242.india_state_BaseService_comm"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    . <- GCAM_region_ID <- MgdFor_adj <- base.service <- technology <- to.value <-
      calibrated.value <- coefficient <- curr_table <- efficiency <- fuel <-
      has_district_heat <- input.cost <- logit.exponent <- minicam.energy.input <-
      minicam.non.energy.input <- output <- region <- region_subsector <- sector <-
      share.weight <- share.weight.year <- stub.technology <- subsector <- supplysector <-
      tradbio_region <- year <- year.fillout <- value <- apply.to <- from.year <- to.year <-
      interpolation.function <- sector.name <- subsector.name <- subs.share.weight <-
      tech.share.weight <- fuelprefElasticity <- perCapitaBased <- energy.final.demand <-
      price.elasticity <- logit.type <- output.unit <- input.unit <- price.unit <-
      logit.year.fillout <- final.energy <- NULL


    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    A42.india_state_calibrated_tech_comm <- get_data(all_data, "gcam-india/A42.india_state_calibrated_tech_comm")
    A42.india_state_sector_comm <- get_data(all_data, "gcam-india/A42.india_state_sector_comm")
    A42.india_state_subsector_interp_comm <- get_data(all_data, "gcam-india/A42.india_state_subsector_interp_comm")
    A42.india_state_subsector_logit_comm <- get_data(all_data, "gcam-india/A42.india_state_subsector_logit_comm")
    A42.india_state_subsector_shrwt_comm <- get_data(all_data, "gcam-india/A42.india_state_subsector_shrwt_comm")
    A42.india_state_globaltech_cost_comm <- get_data(all_data, "gcam-india/A42.india_state_globaltech_cost_comm")
    A42.india_state_globaltech_eff_comm <- get_data(all_data, "gcam-india/A42.india_state_globaltech_eff_comm")
    A42.india_state_globaltech_shrwt_comm <- get_data(all_data, "gcam-india/A42.india_state_globaltech_shrwt_comm")
    A42.india_state_globaltech_interp_comm <- get_data(all_data, "gcam-india/A42.india_state_globaltech_interp_comm")
    A42.india_state_fuelprefElasticity_comm <- get_data(all_data, "gcam-india/A42.india_state_fuelprefElasticity_comm")
    A42.india_state_demand_comm <- get_data(all_data, "gcam-india/A42.india_state_demand_comm")
    A42.india_state_price_elasticity_comm <- get_data(all_data, "gcam-india/A42.india_state_price_elasticity_comm")
    A42.india_state_income_elasticity_comm <- get_data(all_data, "gcam-india/A42.india_state_income_elasticity_comm")
        L142.india_state_in_EJ_comm_F <- get_data(all_data, "L142.india_state_in_EJ_comm_F")

    # ===================================================

    A42.india_state_calibrated_tech_comm <- A42.india_state_calibrated_tech_comm %>% mutate(region = "India")
    A42.india_state_sector_comm <- A42.india_state_sector_comm %>% mutate(region = "India")
    A42.india_state_subsector_interp_comm <- A42.india_state_subsector_interp_comm %>% mutate(region = "India")
    A42.india_state_subsector_logit_comm <- A42.india_state_subsector_logit_comm %>% mutate(region = "India")
    A42.india_state_subsector_shrwt_comm <- A42.india_state_subsector_shrwt_comm %>% mutate(region = "India")
    A42.india_state_globaltech_cost_comm <- A42.india_state_globaltech_cost_comm %>% mutate(region = "India")
    A42.india_state_globaltech_eff_comm <- A42.india_state_globaltech_eff_comm %>% mutate(region = "India")
    A42.india_state_globaltech_shrwt_comm <- A42.india_state_globaltech_shrwt_comm %>% mutate(region = "India")
    A42.india_state_globaltech_interp_comm <- A42.india_state_globaltech_interp_comm %>% mutate(region = "India")
    A42.india_state_fuelprefElasticity_comm <- A42.india_state_fuelprefElasticity_comm %>% mutate(region = "India")
    A42.india_state_demand_comm <- A42.india_state_demand_comm %>% mutate(region = "India")
    A42.india_state_price_elasticity_comm <- A42.india_state_price_elasticity_comm %>% mutate(region = "India")
    A42.india_state_income_elasticity_comm <- A42.india_state_income_elasticity_comm %>% mutate(region = "India")


    # convert to long form
    A42.india_state_globaltech_eff_comm <- A42.india_state_globaltech_eff_comm %>%
      gather_years

    A42.india_state_globaltech_cost_comm <- A42.india_state_globaltech_cost_comm %>%
      gather_years

    A42.india_state_globaltech_shrwt_comm <- A42.india_state_globaltech_shrwt_comm %>%
      gather_years

    A42.india_state_price_elasticity_comm <- A42.india_state_price_elasticity_comm %>%
      gather_years()

    A42.india_state_income_elasticity_comm <- A42.india_state_income_elasticity_comm %>%
      gather_years()



    # The comm_india_processing function is used in place of a for loop in the old data sytem.
    # This function checks to see if the input data needs to be expanded to all states or used as
    # is.

    # comm_india_processing: is a function that
    comm_india_processing <- function(data) {

      # Subset the input data frame for the india region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the india
      # is not found in the region column that regions have already been processed.

      check_india <- filter(data, region == gcam.india_REGION)

      if(nrow(check_india) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains india region information
        # then expand the input data to all states.

        data %>%
          filter(region == gcam.india_REGION) %>%
          write_to_all_india_states(names = names(data)) ->
          new_data

      }

      return(new_data)
    } # end of function

    A42.india_state_sector_comm <- comm_india_processing(A42.india_state_sector_comm)
    A42.india_state_subsector_interp_comm <- comm_india_processing(A42.india_state_subsector_interp_comm)
    A42.india_state_subsector_logit_comm <- comm_india_processing(A42.india_state_subsector_logit_comm)
    A42.india_state_subsector_shrwt_comm <- comm_india_processing(A42.india_state_subsector_shrwt_comm)
    A42.india_state_globaltech_eff_comm <- comm_india_processing(A42.india_state_globaltech_eff_comm)
    A42.india_state_globaltech_cost_comm <- comm_india_processing(A42.india_state_globaltech_cost_comm)
    A42.india_state_globaltech_shrwt_comm <- comm_india_processing(A42.india_state_globaltech_shrwt_comm)
    A42.india_state_globaltech_interp_comm <- comm_india_processing(A42.india_state_globaltech_interp_comm)
    A42.india_state_fuelprefElasticity_comm <- comm_india_processing(A42.india_state_fuelprefElasticity_comm)
    A42.india_state_demand_comm <- comm_india_processing(A42.india_state_demand_comm)
    A42.india_state_calibrated_tech_comm <- comm_india_processing(A42.india_state_calibrated_tech_comm)
    A42.india_state_price_elasticity_comm <- comm_india_processing(A42.india_state_price_elasticity_comm)
    A42.india_state_income_elasticity_comm <- comm_india_processing(A42.india_state_income_elasticity_comm)



    # Shareweights of comm sector technologies # OUTPUT
    L242.india_state_GlobalTechShrwt_comm <- A42.india_state_globaltech_shrwt_comm %>%
     # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology,region)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, region) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      select(region, sector.name = supplysector, subsector.name = subsector, technology, year, share.weight)


    # Technology shareweight interpolation of comm sector # OUTPUT
    L242.india_state_GlobalTechInterp_comm <- A42.india_state_globaltech_interp_comm %>%
      select(region, sector.name = supplysector, subsector.name = subsector, technology, apply.to, from.year, to.year, interpolation.function)

        # Energy inputs and coefficients of comm technologies # OUTPUT
    DIGITS_EFFICIENCY = 3

    L242.india_state_GlobalTechEff_comm <- A42.india_state_globaltech_eff_comm %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input, region)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.energy.input, region) %>%
      mutate(efficiency = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(efficiency = round(efficiency, digits = DIGITS_EFFICIENCY)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(region, sector.name = supplysector, subsector.name = subsector, technology, minicam.energy.input, year, efficiency)


    # Costs of comm technologies
    # Capital costs of comm technologies # OUTPUT
    DIGITS_COST <- 4

    #output
        L242.india_state_GlobalTechCost_comm <- A42.india_state_globaltech_cost_comm %>%
          # Expand table to include all model base and future years
          complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input, region)) %>%
          # Extrapolate to fill out values for all years
          # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
          mutate(input.cost = approx_fun(year, value, rule = 2),
                 input.cost = round(input.cost, digits = DIGITS_COST)) %>%
          ungroup() %>%
          filter(year %in% MODEL_YEARS) %>% # This will drop 1971
          # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
          select(region, sector.name = supplysector, subsector.name = subsector, technology, minicam.non.energy.input, year, input.cost)




    # OUTPUT
    L242.india_state_FuelPrefElast_comm <- A42.india_state_fuelprefElasticity_comm %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS)) %>%
      select(region, supplysector, subsector, year.fillout, fuelprefElasticity)


    L242.india_state_Supplysector_comm <- A42.india_state_sector_comm %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(region, supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type)


    L242.india_state_FinalEnergyKeyword_comm <- L242.india_state_Supplysector_comm %>%
      mutate (final.energy = supplysector) %>%
    select(region, supplysector, final.energy)


    L242.india_state_SubsectorLogit_comm <- A42.india_state_subsector_logit_comm %>%
      mutate(year = min(MODEL_BASE_YEARS)) %>%
      select(region, supplysector, subsector, logit.year.fillout = year, logit.exponent, logit.type)


    L242.india_state_SubsectorShrwtFllt_comm <- A42.india_state_subsector_shrwt_comm %>%
      select(region, supplysector, subsector, year.fillout, share.weight)


    L242.india_state_SubsectorInterp_comm <- A42.india_state_subsector_interp_comm %>%
      select(region, supplysector, subsector, apply.to, from.year, to.year, interpolation.function)


    L242.india_state_StubTech_comm <- A42.india_state_globaltech_shrwt_comm %>%
      select(region, supplysector, subsector, stub.technology = technology) %>%
      unique()




    # Calibration and region-specific data
    # Calibrated input of commtechnologies

    # get calibrated input of comm energy use technologies

    L242.in_EJ_R_bld_F_Yh <- L142.india_state_in_EJ_comm_F %>%
      rename(region = state) %>%
      # Subset table for model base years
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(A42.india_state_calibrated_tech_comm, by = c("region", "sector","fuel"))  %>%
    # Aggregate as indicated in the supplysector/subsector/technology mapping (dropping fuel)
      group_by(region, supplysector, subsector, stub.technology = technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()


    DIGITS_CALOUTPUT <- 7
    # OUTPUT
    L242.india_state_StubTechCalInput_comm <- L242.in_EJ_R_bld_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join_error_no_match(L242.india_state_GlobalTechEff_comm, by = c("region", "supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology","year")) %>%
      mutate(value = round(value, digits = DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, share.weight.year, year) %>%
      summarise(calibrated.value = sum(value)) %>%
      ungroup() %>%
      mutate(subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])


    # Base-year service output of comm final demand
    # Base service is equal to the output of the comm supplysector
    # OUTPUT
    L242.india_state_BaseService_comm <- L242.india_state_StubTechCalInput_comm %>%
      left_join_error_no_match(L242.india_state_GlobalTechEff_comm, by = c("region", "supplysector" = "sector.name", "subsector" = "subsector.name",
                                                              "stub.technology" = "technology", "minicam.energy.input",
                                                              "share.weight.year" = "year")) %>%
      mutate(output = calibrated.value * efficiency) %>%
      group_by(region, supplysector, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() %>%
      mutate(base.service = round(output, digits = DIGITS_CALOUTPUT)) %>%
      select(region, energy.final.demand = supplysector, year, base.service)


    # Expand price elasticity of comm final demand across all regions and future years.
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure.
    # OUTPUT

    L242.india_state_PerCapitaBased_comm <- A42.india_state_demand_comm %>%
      select (region, energy.final.demand, perCapitaBased)

    L242.india_state_PriceElasticity_comm <- A42.india_state_price_elasticity_comm %>%
      select (region, energy.final.demand, year, price.elasticity = value)

    L242.india_state_IncomeElasticity_comm <- A42.india_state_income_elasticity_comm %>%
      select (region, energy.final.demand, year, income.elasticity = value)



    # Get markets for fuels consumed by the state comm sectors
    L242.india_state_StubTechMarket_comm  <- L242.india_state_StubTech_comm %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_keep_first_only( L242.india_state_GlobalTechEff_comm %>%
                                   select(sector.name, subsector.name, technology, minicam.energy.input),
                                 by = c( "supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology")) %>%
      filter(is.na(minicam.energy.input) == FALSE) %>%
      # ^^ includes generic comm technology that is not required here...
      mutate(market.name = gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      left_join_error_no_match(india_states_subregions %>% select(state, grid_region), by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(-grid_region) %>%
      mutate(market.name = if_else(grepl("elect_td", minicam.energy.input), region, market.name))


    # ===================================================


    L242.india_state_Supplysector_comm %>%
      add_title("Supply sector information for building sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information for building sector was written for all regions") %>%
      add_legacy_name("L242.india_state_Supplysector_comm") %>%
      add_precursors("gcam-india/A42.india_state_sector_comm") ->
      L242.india_state_Supplysector_comm

    L242.india_state_FinalEnergyKeyword_comm %>%
      add_title("Supply sector keywords for building sector") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for building sector was written for all regions") %>%
      add_legacy_name("L242.india_state_FinalEnergyKeyword_comm") %>%
      add_precursors("gcam-india/A42.india_state_sector_comm") ->
      L242.india_state_FinalEnergyKeyword_comm

    L242.india_state_SubsectorLogit_comm %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector logit exponents of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.india_state_SubsectorLogit_comm") %>%
      add_precursors("gcam-india/A42.india_state_subsector_logit_comm") ->
      L242.india_state_SubsectorLogit_comm

    L242.india_state_SubsectorShrwtFllt_comm %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of building sector were written for all regions") %>%
        add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
        add_legacy_name("L242.india_state_SubsectorShrwtFllt_comm") %>%
        add_precursors("gcam-india/A42.india_state_subsector_shrwt_comm") ->
      L242.india_state_SubsectorShrwtFllt_comm


    L242.india_state_SubsectorInterp_comm %>%
        add_title("Subsector shareweight interpolation data of building sector") %>%
        add_units("NA") %>%
        add_comments("Subsector shareweight interpolation data of building sector were written for all regions") %>%
        add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
        add_legacy_name("L242.india_state_SubsectorInterp_comm") %>%
        add_precursors("gcam-india/A42.india_state_subsector_interp_comm") ->
      L242.india_state_SubsectorInterp_comm


    L242.india_state_StubTech_comm %>%
      add_title("Identification of stub technologies of building sector") %>%
      add_units("NA") %>%
      add_comments("Identification of stub technologies of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.india_state_StubTech_comm") %>%
      add_precursors("gcam-india/A42.india_state_globaltech_shrwt_comm") ->
      L242.india_state_StubTech_comm

    L242.india_state_GlobalTechInterp_comm %>%
      add_title("Global technology shareweight interpolation of building sector") %>%
      add_units("NA") %>%
      add_comments("From/to years set to min/max of model years") %>%
      add_legacy_name("L242.india_state_GlobalTechInterp_comm") %>%
      add_precursors("gcam-india/A42.india_state_globaltech_interp_comm") ->
      L242.india_state_GlobalTechInterp_comm

    L242.india_state_GlobalTechShrwt_comm %>%
      add_title("Shareweights of global building sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated shareweight data across model years") %>%
      add_legacy_name("L242.india_state_GlobalTechShrwt_comm") %>%
      add_precursors("gcam-india/A42.india_state_globaltech_shrwt_comm") ->
      L242.india_state_GlobalTechShrwt_comm

    L242.india_state_GlobalTechEff_comm %>%
      add_title("Energy inputs and coefficients of global building energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated data across model years") %>%
      add_legacy_name("L242.india_state_GlobalTechEff_comm") %>%
      add_precursors("gcam-india/A42.india_state_globaltech_eff_comm") ->
      L242.india_state_GlobalTechEff_comm

    L242.india_state_GlobalTechCost_comm %>%
      add_title("Capital costs of global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated data across model years") %>%
      add_legacy_name("L242.india_state_GlobalTechCost_comm") %>%
      add_precursors("gcam-india/A42.india_state_globaltech_cost_comm") ->
      L242.india_state_GlobalTechCost_comm

    L242.india_state_StubTechCalInput_comm %>%
      add_title("Calibrated input of building energy use technologies") %>%
      add_units("EJ") %>%
      add_comments("Data were aggregated (dropping fuel) and shareweights were determined from the calibrated value") %>%
      add_legacy_name("L242.india_state_StubTechCalInput_comm") %>%
      add_precursors("gcam-india/A42.india_state_calibrated_tech_comm",
                     "gcam-india/A42.india_state_globaltech_eff_comm", "L142.india_state_in_EJ_comm_F") ->
      L242.india_state_StubTechCalInput_comm


    L242.india_state_StubTechMarket_comm %>%
      add_title("Fuel preference elasticities of building energy use") %>%
      add_units("Unitless") %>%
      add_comments("Data were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.india_state_StubTechMarket_comm") %>%
      add_precursors("gcam-india/india_states_subregions", "gcam-india/A42.india_state_globaltech_shrwt_comm", "gcam-india/A42.india_state_globaltech_eff_comm") ->
      L242.india_state_StubTechMarket_comm


        L242.india_state_FuelPrefElast_comm %>%
      add_title("Fuel preference elasticities of building energy use") %>%
      add_units("Unitless") %>%
      add_comments("Data were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.india_state_FuelPrefElast_comm") %>%
      add_precursors("gcam-india/india_states_subregions", "gcam-india/A42.india_state_calibrated_tech_comm",
                     "gcam-india/A42.india_state_fuelprefElasticity_comm") ->
      L242.india_state_FuelPrefElast_comm

    L242.india_state_PerCapitaBased_comm %>%
      add_title("Per-capita based flag for building final demand") %>%
      add_units("Unitless") %>%
      add_comments("Data were written for all regions") %>%
      add_legacy_name("L242.india_state_PerCapitaBased_comm") %>%
      add_precursors("gcam-india/A42.india_state_demand_comm") ->
      L242.india_state_PerCapitaBased_comm

    L242.india_state_PriceElasticity_comm %>%
      add_title("Price elasticity of building final demand") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity data were written for all regions and only applied to future model years") %>%
      add_legacy_name("L242.india_state_PriceElasticity_comm") %>%
      add_precursors("gcam-india/A42.india_state_price_elasticity_comm") ->
      L242.india_state_PriceElasticity_comm

    L242.india_state_IncomeElasticity_comm %>%
      add_title("Income elasticity of building final demand") %>%
      add_units("Unitless") %>%
      add_comments("Income elasticity data were written for all regions and only applied to future model years") %>%
      add_legacy_name("L242.india_state_IncomeElasticity_comm") %>%
      add_precursors("gcam-india/A42.india_state_income_elasticity_comm") ->
      L242.india_state_IncomeElasticity_comm

    L242.india_state_BaseService_comm %>%
      add_title("Base-year service output of building final demand") %>%
      add_units("EJ") %>%
      add_comments("Base service is equal to the output of the building supplysector") %>%
      add_legacy_name("L242.india_state_BaseService_comm") %>%
      add_precursors("gcam-india/A42.india_state_calibrated_tech_comm",
                     "L142.india_state_in_EJ_comm_F", "gcam-india/A42.india_state_globaltech_eff_comm") ->
      L242.india_state_BaseService_comm

    return_data(L242.india_state_Supplysector_comm,
                L242.india_state_FinalEnergyKeyword_comm,
                L242.india_state_SubsectorLogit_comm,
                L242.india_state_SubsectorShrwtFllt_comm,
                L242.india_state_SubsectorInterp_comm,
                L242.india_state_StubTech_comm,
                L242.india_state_GlobalTechInterp_comm,
                L242.india_state_GlobalTechShrwt_comm,
                L242.india_state_GlobalTechEff_comm,
                L242.india_state_GlobalTechCost_comm,
                L242.india_state_StubTechCalInput_comm,
                L242.india_state_StubTechMarket_comm,
                L242.india_state_FuelPrefElast_comm,
                L242.india_state_PerCapitaBased_comm,
                L242.india_state_PriceElasticity_comm,
                L242.india_state_IncomeElasticity_comm,
                L242.india_state_BaseService_comm)
  } else {
    stop("Unknown command")
  }
}
