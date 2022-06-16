# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2321.cement_USA
#'
#' Make the logit and input tables for the cement sector in gcam-usa
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.DeleteSupplysector_USAcement}, \code{L2321.DeleteFinalDemand_USAcement}, \code{L2321.StubTechProd_cement_USA},
#' \code{L2321.StubTechCoef_cement_USA}, \code{L2321.StubTechCalInput_cement_heat_USA}, \code{L2321.StubTechMarket_cement_USA}, \code{L2321.BaseService_cement_USA},
#' \code{L2321.Supplysector_cement_USA}, \code{L2321.FinalEnergyKeyword_cement_USA}, \code{L2321.SubsectorLogit_cement_USA},
#' \code{L2321.SubsectorShrwtFllt_cement_USA}, \code{L2321.StubTech_cement_USA}, \code{L2321.PerCapitaBased_cement_USA},
#'  \code{L2321.PriceElasticity_cement_USA}, \code{L2321.IncomeElasticity_cement_gcam3_USA} .
#'  The corresponding file in the original data system was \code{L2321.cement_USA.R} (gcam-usa level2).
#' @details Make the logit and input tables for the cement sector in gcam-usa
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting spread
#' @author KD  November 2017
module_gcamusa_L2321.cement_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A321.demand",
             FILE = "energy/A321.globaltech_coef",
             "L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             "L2321.StubTech_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.PriceElasticity_cement",
             "L2321.IncomeElasticity_cement_gcam3",
             "L1321.in_EJ_state_cement_F_Y",
             "L1321.IO_GJkg_state_cement_F_Yh",
             "L1321.out_Mt_state_cement_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2321.DeleteSupplysector_USAcement",
             "L2321.DeleteFinalDemand_USAcement",
             "L2321.StubTechProd_cement_USA",
             "L2321.StubTechCoef_cement_USA",
             "L2321.StubTechCalInput_cement_heat_USA",
             "L2321.StubTechMarket_cement_USA",
             "L2321.BaseService_cement_USA",
             "L2321.Supplysector_cement_USA",
             "L2321.FinalEnergyKeyword_cement_USA",
             "L2321.SubsectorLogit_cement_USA",
             "L2321.SubsectorShrwtFllt_cement_USA",
             "L2321.SubsectorInterp_cement_USA",
             "L2321.StubTech_cement_USA",
             "L2321.PerCapitaBased_cement_USA",
             "L2321.PriceElasticity_cement_USA",
             "L2321.IncomeElasticity_cement_gcam3_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A321.demand <- get_data(all_data, "energy/A321.demand")
    A321.globaltech_coef <- get_data(all_data, "energy/A321.globaltech_coef")

    # The following lines will be un commented when #789 is merged.
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement", strip_attributes = TRUE)
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement", strip_attributes = TRUE)
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement", strip_attributes = TRUE)
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement", strip_attributes = TRUE)
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement", strip_attributes = TRUE)
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement", strip_attributes = TRUE)
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement", strip_attributes = TRUE)
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement", strip_attributes = TRUE)
    L2321.IncomeElasticity_cement_gcam3 <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3", strip_attributes = TRUE)
    L1321.in_EJ_state_cement_F_Y <- get_data(all_data, "L1321.in_EJ_state_cement_F_Y", strip_attributes = TRUE)
    L1321.IO_GJkg_state_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_state_cement_F_Yh", strip_attributes = TRUE)
    L1321.out_Mt_state_cement_Yh <- get_data(all_data, "L1321.out_Mt_state_cement_Yh", strip_attributes = TRUE)

    # Silence package checks
    state <- region <- supplysector <- energy.final.demand <- region <- year <-
      value <- calibration <- sector <- subsector <- technology <- calOutputValue <-
      subs.share.weight <- share.weight.year <- fuel <- minicam.energy.input <-
      coefficient <- market.name <- grid_region <- stub.technology <- calibrated.value <-
      tech.share.weight <- object <- NULL

    # ===================================================
    # Make the logit and input tables for with creating cement sectors in cement
    # producing states.

    # Not all states produce cement so save a vector of cement producing states bases on census data.
    # This vector will be used to create cement sectors in only the cement producing states.
    L1321.out_Mt_state_cement_Yh %>%
      select(state) %>%
      unique ->
      cement_states


    # In order to create state cement sectors on the state level for gcam-USA the
    # cement sector will need to be removed from the USA region in the supply sector
    # and enery-final-demands input tables
    #
    # Create a table that will be used remove the cement supply sector input table.
    L2321.Supplysector_cement %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.DeleteSupplysector_USAcement

    # Create the table that will be used to remove the cement sector information from the
    # energy.final.demand input table.
    L2321.PerCapitaBased_cement %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, energy.final.demand) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.DeleteFinalDemand_USAcement


    # The cement_USA_processing function replaces the "identical processing for loop"
    # in the old data system. Function inputs include an input data frame to be
    # checked and processed if deemed necessary and a list of the cement producing
    # states.

    cement_USA_processing <- function(data, cement_states) {

      # Subset the input data frame for the USA region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the USA
      # is not found in the region column that regions have already been processed.

      check_df <- filter(data, region == gcam.USA_REGION)

      if(nrow(check_df) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains USA region information
        # then expand the input data to all cement producing states.

        data %>%
          filter(region == gcam.USA_REGION) %>%
          write_to_all_states(names = names(data)) %>%
          filter(region %in% cement_states[["state"]]) ->
          new_data

      }

      return(new_data)
    } # end of function



    # Use the cement_USA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all cement producing states.
    L2321.Supplysector_cement_USA <- cement_USA_processing(L2321.Supplysector_cement, cement_states)
    L2321.FinalEnergyKeyword_cement_USA <- cement_USA_processing(L2321.FinalEnergyKeyword_cement, cement_states)
    L2321.SubsectorLogit_cement_USA <- cement_USA_processing(L2321.SubsectorLogit_cement, cement_states)
    L2321.SubsectorShrwtFllt_cement_USA <- cement_USA_processing(L2321.SubsectorShrwtFllt_cement, cement_states)
    L2321.SubsectorInterp_cement_USA <- cement_USA_processing(L2321.SubsectorInterp_cement, cement_states)
    L2321.StubTech_cement_USA <- cement_USA_processing(L2321.StubTech_cement, cement_states)
    L2321.PerCapitaBased_cement_USA <- cement_USA_processing(L2321.PerCapitaBased_cement, cement_states)
    L2321.PriceElasticity_cement_USA <- cement_USA_processing(L2321.PriceElasticity_cement, cement_states)
    L2321.IncomeElasticity_cement_gcam3_USA <- cement_USA_processing(L2321.IncomeElasticity_cement_gcam3, cement_states)


    # Create stub-technology calibrated cement production input table.
    #
    # Start by sub setting the cement production by state / historical year
    # for model base years and rounding to the appropriate number of digits.
    L1321.out_Mt_state_cement_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, energy.DIGITS_CALOUTPUT),
             region = state ) ->
      L2321.StubTechProd_cement_USA

    # Subset the calibrated intermediate sectors and fuels to supplysector / subsector / technology
    # mapping file for unique sector / calibration / supplysector/ subsector / technology combinations.
    # This tibble will be used to add cement sector information add to the state cement production
    # input table.
    calibrated_techs %>%
      # We are only interested in the technology IDs where calibration = output.
      filter(calibration == "output") %>%
      select(sector, calibration, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_cement_sector_info

    # Combine the cement sector information found above and the stub-technology calibrated
    # cement production into a single data frame.
    L2321.StubTechProd_cement_USA %>%
      left_join_error_no_match(calibrated_techs_cement_sector_info, by = "sector") %>%
      select(state, sector, calOutputValue, year, region, supplysector, subsector, technology) ->
      L2321.StubTechProd_cement_USA

    # Add share weight information to the state cement production data frame and format.
    L2321.StubTechProd_cement_USA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, tech.share.weight) ->
      L2321.StubTechProd_cement_USA


    # Create the coefficients of cement production technologies input table.
    #
    # Start by creating a data frame of unique sector, fuel, supplysector, subsector,
    # technology, and minicam.energy.input combinations. This data frame will be used to
    # add sector information to input-ouput coefficients for state cement production.
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      cement_production_technologies

    # Add cement sector information to the data frame of input-output coefficients of
    # cement production by state.
    L1321.IO_GJkg_state_cement_F_Yh %>%
      left_join_error_no_match(cement_production_technologies, by = c("sector", "fuel")) %>%
      select(state,fuel, sector, year, value, supplysector, subsector, technology, minicam.energy.input) ->
      L2321.IO_GJkg_state_cement_F_Yh

    # Interpolate cement production default coefficients to future years.
    #
    # First change format of the the production default coefficients data frame from wide to long.
    A321.globaltech_coef_long <- gather_years(A321.globaltech_coef)

    # Then linearly interpolate the default coefficients for future years. In the next step these
    # values will be added to the state input-output coefficients data frame.
    A321.globaltech_coef_long %>%
      complete(nesting(supplysector, subsector, minicam.energy.input, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, minicam.energy.input, technology, year) %>%
      group_by(supplysector, subsector, minicam.energy.input, technology) %>%
      mutate(value = approx_fun(year, value), value = signif(value, energy.DIGITS_COEFFICIENT)) %>%
      ungroup ->
      L2321.globaltech_coef

    # Subset the global technology coefficients and format the data frame to join with the
    # state input-output coefficients.
    L2321.globaltech_coef %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      spread(year, value) ->
      L2321.globaltech_coef_yfut

    # Combine the future global technology coefficients with the state energy input-output
    # coefficients by supplysector / subsector / technology / minicam.energy.input combinations.
    L2321.IO_GJkg_state_cement_F_Yh %>%
      spread(year, value) %>%
      left_join_error_no_match(L2321.globaltech_coef_yfut,
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input")) ->
      IO_and_globaltech

    # Format the the data frame and round the number of digits.
    IO_and_globaltech %>%
      gather_years %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(coefficient = signif(value, energy.DIGITS_COEFFICIENT)) %>%
      select(-value) ->
      L2321.IO_GJkg_state_cement_F_Yh_complete

    # Save lists of unique the minicam.energy.input from the complete
    # state input-output coefficients data frame to add to the state
    # stub technology data frame.
    L2321.IO_GJkg_state_cement_F_Yh_complete %>%
      select(minicam.energy.input) %>%
      unique ->
      minicam_to_add

    # Subset the cement stub-technology coefficient data frame for supply sectors in the
    # input-output data frame, add model years and minicam information in preparation
    # for the left join in the next step.
    L2321.StubTech_cement_USA  %>%
      filter(supplysector %in% L2321.IO_GJkg_state_cement_F_Yh_complete$supplysector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(minicam_to_add) ->
      L2321.StubTechCoef_cement_USA

    # Join the input-output coefficients data frame with the  Cement stub technologies data frame by
    # region / supplysector/ minicam.energy.input/ year.
    L2321.StubTechCoef_cement_USA %>%
      left_join(L2321.IO_GJkg_state_cement_F_Yh_complete %>%
                  select(coefficient, region = state, supplysector, minicam.energy.input, year),
                by =c("region", "supplysector", "minicam.energy.input", "year")) ->
      L2321.StubTechCoef_cement_USA

    # Add market information: default is USA. replace for fuels with grid region level markets and state markets.
    # Process heat cement and limestone are also represented at the state level (names referenced from existing objects).
    L2321.StubTechCoef_cement_USA %>%
      left_join(states_subregions %>%
                  select(region = state, grid_region),
                by = "region") %>%
      mutate(market.name = gcam.USA_REGION,
             market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% c(gcamusa.STATE_UNLIMITED_RESOURCES, L2321.Supplysector_cement$supplysector),
                                   region, market.name)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2321.StubTechCoef_cement_USA

    # Create the calibrated input of fuel consumption for producing heat input table.
    #
    # Start by subsetting the energy inputs to cement production by state for model base years,
    # rounding to the correct xml digit and adding a region column.
    L1321.in_EJ_state_cement_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = signif(value, gcamusa.DIGITS_CALOUTPUT), region = state) %>%
      select(-value)  ->
      L2321.StubTechCalInput_cement_heat_USA

    # Add supplysector / subsector / technology / minicam.energy.input information
    # from the calibrated technology mapping file to the energy inputs to cement production
    # data frame.
    L2321.StubTechCalInput_cement_heat_USA %>%
      left_join_error_no_match(calibrated_techs %>%
                                 select(sector, fuel, supplysector, subsector, technology, minicam.energy.input),
                               by = c("sector", "fuel")) ->
      L2321.StubTechCalInput_cement_heat_USA

    # Since this table should only contain the technologies for producing heat, remove
    # electricity inputs to the cement production technology from the stub technology coefficients by
    # filtering for supply sectors NOT included in the L2321.StubTechCoef_cement_USA data frame which
    # contains electricity inputs.
    L2321.StubTechCalInput_cement_heat_USA %>%
      filter(!supplysector %in% L2321.StubTechCoef_cement_USA$supplysector) ->
      L2321.StubTechCalInput_cement_heat_USA_NOelectricity

    # Add region and share weight information, format.
    L2321.StubTechCalInput_cement_heat_USA_NOelectricity %>%
      mutate(region = state,
             stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input,
             calibrated.value, share.weight.year, subs.share.weight, tech.share.weight) ->
      L2321.StubTechCalInput_cement_heat_USA


    # Create the input table with the market names  for the fuels consumed for heat by the state cement sectors
    #
    # Add model years to the stub technology coefficients for the cement sector. Then remove the
    # cement supplysector leaving only the process heat supplysector.
    L2321.StubTech_cement_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(!supplysector %in% L2321.StubTechCoef_cement_USA$supplysector) ->
      L2321.StubTechMarket_cement_USA

    # Use the calibrated technology mapping data frame to add minicam.energy.input information to
    # the process heat supplysector data frame.
    L2321.StubTechMarket_cement_USA %>%
      # Use left join here to pass time shift test.
      left_join(A321.globaltech_coef %>%
                  select(supplysector, subsector, stub.technology = technology,
                         minicam.energy.input),
                by = c("supplysector", "subsector", "stub.technology")) ->
      L2321.StubTechMarket_cement_USA

    # Fuels are from the USA markets, except for regional fuel markets
    L2321.StubTechMarket_cement_USA %>%
      mutate(market.name = gcam.USA_REGION) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, market.name) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market, and with the state name if it's a state market
      left_join_error_no_match(states_subregions %>%
                                 select(region = state, grid_region),
                               by = "region") %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name)) %>%
      select(-grid_region) ->
      L2321.StubTechMarket_cement_USA


    # Create the base-year service output for cement final demand input table.
    #
    # Since base service is equal to the output of the cement supplysector use
    # the coefficients from the stub technology production data frame and add
    # energy.final.demand form cement final demand perCapitaBased and price elasticity
    # assumption file.
    L2321.StubTechProd_cement_USA %>%
      mutate(energy.final.demand = A321.demand$energy.final.demand) %>%
      select(region, energy.final.demand, year, base.service = calOutputValue) ->
      L2321.BaseService_cement_USA

    # ===================================================
    # Produce outputs
    L2321.DeleteSupplysector_USAcement %>%
      add_title("Cement sector information to remove from supplysectors input table") %>%
      add_units("NA") %>%
      add_comments("Cement supply sector information for region USA") %>%
      add_legacy_name("L2321.DeleteSupplysector_USAcement") %>%
      add_precursors("L2321.Supplysector_cement") ->
      L2321.DeleteSupplysector_USAcement

    L2321.DeleteFinalDemand_USAcement %>%
      add_title("Cement sector information to remove from energy.final.demand input table") %>%
      add_units("NA") %>%
      add_comments("Cement sector information from the energy.final.demand for region USA ") %>%
      add_legacy_name("L2321.DeleteFinalDemand_USAcement") %>%
      add_precursors("L2321.PerCapitaBased_cement") ->
      L2321.DeleteFinalDemand_USAcement

    L2321.StubTechProd_cement_USA %>%
      add_title("Calibrated cement stub technologies for cement producing states") %>%
      add_units("NA") %>%
      add_comments("Added cement calibrated technology coefficients to cement production by state") %>%
      add_legacy_name("L2321.StubTechProd_cement_USA") %>%
      add_precursors("energy/calibrated_techs", "L1321.out_Mt_state_cement_Yh") ->
      L2321.StubTechProd_cement_USA

    L2321.StubTechCoef_cement_USA %>%
      add_title("Cement production technologies coefficients by state") %>%
      add_units("coefficient = GJ/kg (gigajoules per kilogram of cement)") %>%
      add_comments("Linearly interpolated input-output coefficients for future years") %>%
      add_comments("Matched input-output coefficients with stub technologies of cement by region / supplysector / minicam.energy.input") %>%
      add_comments("Rename markets with regional gird name if using regional regional fuel markets") %>%
      add_legacy_name("L2321.StubTechCoef_cement_USA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_state_cement_F_Yh",
                     "gcam-usa/states_subregions", "energy/A321.globaltech_coef") ->
      L2321.StubTechCoef_cement_USA

    L2321.StubTechCalInput_cement_heat_USA %>%
      add_title("Calibrated input of fuel consumption for producing heat in cement production") %>%
      add_units("calibrated.value = exajoules") %>%
      add_comments("Subset energy inputs to cement production to include heat producing technologies") %>%
      add_comments("Added sector information from calibrated technology mapping file") %>%
      add_comments("Added share weight information") %>%
      add_legacy_name("L2321.StubTechCalInput_cement_heat_USA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_state_cement_F_Yh",
                     "gcam-usa/states_subregions", "L1321.in_EJ_state_cement_F_Y", "energy/calibrated_techs") ->
      L2321.StubTechCalInput_cement_heat_USA

    L2321.StubTechMarket_cement_USA %>%
      add_title("Market names for the consumed for heat by the state cement sectors") %>%
      add_units("NA") %>%
      add_comments("Select heat producing technologies used in state cement production") %>%
      add_comments("Add sector information from calibrated technology mapping file") %>%
      add_comments("Add market name based on region or regional grid if using regional regional fuel markets") %>%
      add_legacy_name("L2321.StubTechMarket_cement_USA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_state_cement_F_Yh",
                     "gcam-usa/states_subregions", "energy/calibrated_techs", "L2321.StubTech_cement_USA") ->
      L2321.StubTechMarket_cement_USA

    L2321.BaseService_cement_USA %>%
      add_title("Base-year service output of cement final demand") %>%
      add_units("NA") %>%
      add_comments("Base service is equal to the output of the cement supplysector added final demand to calibrated cement stub technologies for cement producing states input table") %>%
      add_legacy_name("L2321.BaseService_cement_USA") %>%
      add_precursors("energy/calibrated_techs", "L1321.out_Mt_state_cement_Yh", "energy/A321.demand") ->
      L2321.BaseService_cement_USA

    L2321.Supplysector_cement_USA %>%
      add_title("Supply sector information for cement sector in cement producing states") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information to cement producing states in region USA") %>%
      add_legacy_name("L2321.Supplysector_cement_USA") %>%
      add_precursors("L2321.Supplysector_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.Supplysector_cement_USA

    L2321.FinalEnergyKeyword_cement_USA %>%
      add_title("Subsector logit exponents of cement sector in cement producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded supply sector keywords information cement producing states in region USA") %>%
      add_legacy_name("L2321.FinalEnergyKeyword_cement_USA") %>%
      add_precursors("L2321.FinalEnergyKeyword_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.FinalEnergyKeyword_cement_USA

    L2321.SubsectorLogit_cement_USA %>%
      add_title("Supply sector keywords for cement sector in cement producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector logit exponents to to cement producing states in region USA") %>%
      add_legacy_name("L2321.SubsectorLogit_cement_USA") %>%
      add_precursors("L2321.SubsectorLogit_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.SubsectorLogit_cement_USA

    L2321.SubsectorShrwtFllt_cement_USA %>%
      add_title("Subsector shareweights of cement sector in cement producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector shareweights to to cement producing states in region USA") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement_USA") %>%
      add_precursors("L2321.SubsectorShrwtFllt_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.SubsectorShrwtFllt_cement_USA

    L2321.SubsectorInterp_cement_USA %>%
      add_title("Subsector shareweight interpolation of cement sector in cement producing states") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to cement producing states in region USA") %>%
      add_legacy_name("L2321.SubsectorInterp_cement_USA") %>%
      add_precursors("L2321.SubsectorInterp_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.SubsectorInterp_cement_USA

    L2321.StubTech_cement_USA %>%
      add_title("Identification of stub technologies of cement in cement producing states") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of cement to cement producing states in region USA") %>%
      add_legacy_name("L2321.StubTech_cement_USA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.StubTech_cement_USA

    L2321.PerCapitaBased_cement_USA %>%
      add_title("Per-capita based flag for cement exports final demand in cement producing states") %>%
      add_units("NA") %>%
      add_comments("Expanded Per-capita based flag for cement exports final demand to cement producing states in region USA") %>%
      add_legacy_name("L2321.PerCapitaBased_cement_USA") %>%
      add_precursors("L2321.PerCapitaBased_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.PerCapitaBased_cement_USA

    L2321.PriceElasticity_cement_USA %>%
      add_title("Price elasticity for cement in cement producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for cement to cement producing states in region USA") %>%
      add_legacy_name("L2321.PriceElasticity_cement_USA") %>%
      add_precursors("L2321.PriceElasticity_cement", "L1321.out_Mt_state_cement_Yh") ->
      L2321.PriceElasticity_cement_USA

    L2321.IncomeElasticity_cement_gcam3_USA %>%
      add_title("Price elasticity for cement in cement producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for cement to cement producing states in region USA") %>%
      add_legacy_name("L2321.IncomeElasticity_cement_gcam3_USA") %>%
      add_precursors("L2321.IncomeElasticity_cement_gcam3", "L1321.out_Mt_state_cement_Yh") ->
      L2321.IncomeElasticity_cement_gcam3_USA

    return_data(L2321.DeleteSupplysector_USAcement, L2321.DeleteFinalDemand_USAcement,
                L2321.StubTechProd_cement_USA, L2321.StubTechCoef_cement_USA,
                L2321.StubTechCalInput_cement_heat_USA, L2321.StubTechMarket_cement_USA,
                L2321.BaseService_cement_USA, L2321.Supplysector_cement_USA,
                L2321.FinalEnergyKeyword_cement_USA, L2321.SubsectorLogit_cement_USA,
                L2321.SubsectorShrwtFllt_cement_USA, L2321.SubsectorInterp_cement_USA,
                L2321.StubTech_cement_USA, L2321.PerCapitaBased_cement_USA,
                L2321.PriceElasticity_cement_USA, L2321.IncomeElasticity_cement_gcam3_USA)
  } else {
    stop("Unknown command")
  }
}
