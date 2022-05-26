# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamdata_L262.dac_USA
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for dac-related GCAM-USA inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L262.SectorLogitTables[[ curr_table ]]$data}, \code{L262.Supplysector_dac}, \code{L262.FinalEnergyKeyword_dac},
#' \code{L262.SubsectorLogitTables[[ curr_table ]]$data}, \code{L262.SubsectorLogit_dac}, \code{L262.SubsectorShrwtFllt_dac},
#' \code{L262.SubsectorInterp_dac}, \code{L262.StubTech_dac}, \code{L262.GlobalTechShrwt_dac}, \code{L262.GlobalTechCoef_dac},
#' \code{L262.GlobalTechCost_dac}, \code{L262.GlobalTechCapture_dac}, \code{L262.StubTechProd_dac}, \code{L262.StubTechCalInput_dac_heat},
#' \code{L262.StubTechCoef_dac}, \code{L262.PerCapitaBased_dac}, \code{L262.BaseService_dac}, \code{L262.PriceElasticity_dac}, \code{object}.
#' The corresponding file in the original data system was \code{L262.dac.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for dac sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author JF March 2021 / YO March 2022
module_gcamdata_L262.dac_USA <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Dooley_CCS_USA",
             FILE = "energy/calibrated_techs_cdr",
             FILE = "energy/A62.demand",
             "L262.CarbonCoef_dac",
             "L262.GlobalTechCoef_dac",
             "L262.Supplysector_dac",
             "L262.FinalEnergyKeyword_dac",
             "L262.SubsectorLogit_dac",
             "L262.SubsectorShrwtFllt_dac",
             "L262.SubsectorInterp_dac",
             "L262.StubTech_dac",
             "L262.PerCapitaBased_dac",
             "L262.PriceElasticity_dac",
             "L262.StubTechProd_dac"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L262.DeleteSupplysector_USAdac",
             "L262.DeleteFinalDemand_USAdac",
             "L262.StubTechCoef_dac_USA_ssp1",
             "L262.StubTechCoef_dac_USA_ssp2",
             "L262.StubTechCoef_dac_USA_ssp3",
             "L262.StubTechCoef_dac_USA_ssp4",
             "L262.StubTechCoef_dac_USA_ssp5",
             "L262.SubsectorLogit_dac_USA",
             "L262.SubsectorShrwtFllt_dac_USA",
             "L262.SubsectorInterp_dac_USA",
             "L262.StubTech_dac_USA",
             "L262.PerCapitaBased_dac_USA",
             "L262.PriceElasticity_dac_USA",
             "L262.FinalEnergyKeyword_dac_USA",
             "L262.BaseService_dac_USA",
             "L262.Supplysector_dac_USA",
             "L262.StubTechProd_dac_USA",
             "L262.CarbonCoef_dac_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs_cdr")
    Dooley_CCS_USA <- get_data(all_data, "gcam-usa/Dooley_CCS_USA")
    A62.demand <- get_data(all_data, "energy/A62.demand")

    L262.GlobalTechCoef_dac <- get_data(all_data, "L262.GlobalTechCoef_dac", strip_attributes = TRUE)
    L262.Supplysector_dac <- get_data(all_data, "L262.Supplysector_dac", strip_attributes = TRUE)
    L262.FinalEnergyKeyword_dac <- get_data(all_data, "L262.FinalEnergyKeyword_dac", strip_attributes = TRUE)
    L262.SubsectorLogit_dac <- get_data(all_data, "L262.SubsectorLogit_dac", strip_attributes = TRUE)
    L262.SubsectorShrwtFllt_dac <- get_data(all_data, "L262.SubsectorShrwtFllt_dac", strip_attributes = TRUE)
    L262.SubsectorInterp_dac <- get_data(all_data, "L262.SubsectorInterp_dac", strip_attributes = TRUE)
    L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac", strip_attributes = TRUE)
    L262.PerCapitaBased_dac <- get_data(all_data, "L262.PerCapitaBased_dac", strip_attributes = TRUE)
    L262.PriceElasticity_dac <- get_data(all_data, "L262.PriceElasticity_dac", strip_attributes = TRUE)
    L262.StubTechProd_dac <- get_data(all_data, "L262.StubTechProd_dac", strip_attributes = TRUE)
    L262.CarbonCoef_dac <- get_data(all_data, "L262.CarbonCoef_dac", strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    state <- region <- supplysector <- energy.final.demand <- region <- year <-
      value <- calibration <- sector <- subsector <- technology <- calOutputValue <-
      subs.share.weight <- share.weight.year <- fuel <- minicam.energy.input <-
      coefficient <- market.name <- grid_region <- stub.technology <- calibrated.value <-
      tech.share.weight <- object <- NULL

    # ===================================================
    # 1. Perform computations

    # 1a. Supplysector information
    # L262.Supplysector_dac: Supply sector information for CO2 removal sector containing dac

    L262.Supplysector_dac %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) ->
      L262.DeleteSupplysector_USAdac


    L262.PerCapitaBased_dac %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, energy.final.demand) ->
      L262.DeleteFinalDemand_USAdac


    dac_USA_processing <- function(data, gcamusa.STATES) {

      # Subset the input data frame for the USA region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the USA
      # is not found in the region column that regions have already been processed.

      check_df <- filter(data, region == gcam.USA_REGION)

      if(nrow(check_df) == 0) {

        new_data <- data

      } else {

        # If the input data frame contains USA region information
        # then expand the input data to all states.

        data %>%
          filter(region == gcam.USA_REGION) %>%
          write_to_all_states(names = names(data)) %>%
          filter(region %in% gcamusa.STATES) ->
          new_data

      }

      return(new_data)
    } # end of function

    # Use the dac_USA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all states.
    L262.Supplysector_dac_USA <- dac_USA_processing(L262.Supplysector_dac, gcamusa.STATES)
    L262.FinalEnergyKeyword_dac_USA <- dac_USA_processing(L262.FinalEnergyKeyword_dac, gcamusa.STATES)
    L262.SubsectorLogit_dac_USA <- dac_USA_processing(L262.SubsectorLogit_dac, gcamusa.STATES)
    L262.SubsectorShrwtFllt_dac_USA <- dac_USA_processing(L262.SubsectorShrwtFllt_dac, gcamusa.STATES)
    L262.SubsectorInterp_dac_USA <- dac_USA_processing(L262.SubsectorInterp_dac, gcamusa.STATES)
    L262.StubTech_dac_USA <- dac_USA_processing(L262.StubTech_dac, gcamusa.STATES)
    L262.PerCapitaBased_dac_USA <- dac_USA_processing(L262.PerCapitaBased_dac, gcamusa.STATES)
    L262.PriceElasticity_dac_USA <- dac_USA_processing(L262.PriceElasticity_dac, gcamusa.STATES)
    L262.CarbonCoef_dac_USA <- dac_USA_processing(L262.CarbonCoef_dac, gcamusa.STATES)

    L262.GlobalTechCoef_dac_USA <- dac_USA_processing(L262.GlobalTechCoef_dac %>%
                                                        mutate(region = gcam.USA_REGION), gcamusa.STATES)

    # calibrated DAC availability based on cumulative carbon storage
    cumStorage <- Dooley_CCS_USA %>%
      group_by(state) %>%
      summarise(cumStorage = sum(CO2_Mt)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION) %>%
      group_by(region) %>%
      mutate(cumStorage.share = cumStorage/sum(cumStorage)) %>%
      ungroup() %>%
      select(region, state, cumStorage.share)

    # downscale USA total DAC calibration to state by cumulative C storage share
    L262.StubTechProd_dac %>%
      select(region, sector = supplysector, year, calOutputValue) %>%
      filter(region == gcam.USA_REGION) %>%
      # map to all states, no. of rows will change
      left_join(cumStorage, by = "region") %>%
      mutate(value = cumStorage.share * calOutputValue) %>%
      select(state, sector, year, value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, energy.DIGITS_CALOUTPUT),
             region = state) ->
      L262.StubTechProd_dac_USA

    # Subset the calibrated intermediate sectors and fuels to supplysector / subsector / technology
    # mapping file for unique sector / calibration / supplysector/ subsector / technology combinations.
    # This tibble will be used to add dac sector information add to the state dac
    # input table.
    calibrated_techs %>%
      # We are only interested in the technology IDs where calibration = output.
      filter(calibration == "output") %>%
      select(sector, calibration, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_dac_sector_info

    # Combine the dac sector information found above and the stub-technology calibrated
    # dac production into a single data frame.
    L262.StubTechProd_dac_USA %>%
      left_join_error_no_match(calibrated_techs_dac_sector_info, by = "sector") %>%
      select(state, sector, calOutputValue, year, region, supplysector, subsector, technology) ->
      L262.StubTechProd_dac_USA

    # Add share weight information to the state dac production data frame and format.
    L262.StubTechProd_dac_USA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, tech.share.weight) ->
      L262.StubTechProd_dac_USA


    # Create the coefficients of dac production technologies input table.
    #
    # Start by creating a data frame of unique sector, fuel, supplysector, subsector,
    # technology, and minicam.energy.input combinations. This data frame will be used to
    # add sector information to input-ouput coefficients for state dac production.
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      dac_production_technologies


    L262.GlobalTechCoef_dac_USA %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             value = coefficient) %>%
      # Add market information. process heat are state level markets where as electricity
      # comes from the grid level.
      mutate(market.name = region,
             market.name = if_else(grepl("elec", minicam.energy.input), gcam.USA_REGION, market.name)) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(states_subregions %>%
                                 select(region = state, grid_region), by = "region") %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(-grid_region) %>%
      # Change market name to reflect the fact that electricity is consumed from state markets.
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.ELECT_TD_SECTORS,
                                   region, market.name)) %>%
      rename(stub.technology = technology,
             coefficient = value) ->
      L262.StubTechCoef_dac_USA

    # Create the base-year service output for dac final demand input table.
    #
    # Since base service is equal to the output of the dac supplysector use
    # the coefficients from the stub technology production data frame and add
    # energy.final.demand form dac final demand perCapitaBased and price elasticity
    # assumption file.

    L262.StubTechProd_dac_USA %>%
      mutate(energy.final.demand = A62.demand$energy.final.demand) %>%
      select(region, energy.final.demand, year, base.service = calOutputValue) ->
      L262.BaseService_dac_USA



    # ===================================================
    # Produce outputs

    L262.DeleteSupplysector_USAdac %>%
      add_title("Supply sector information for CO2 removal sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A62.sector is expended into all GCAM regions") %>%
      add_legacy_name("L262.Supplysector_dac") %>%
      add_precursors("L262.Supplysector_dac") ->
      L262.DeleteSupplysector_USAdac

    L262.DeleteFinalDemand_USAdac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A62.sector are expended into all GCAM regions") %>%
      add_legacy_name("L262.FinalEnergyKeyword_dac") %>%
      add_precursors("L262.FinalEnergyKeyword_dac") ->
      L262.DeleteFinalDemand_USAdac

    L262.SubsectorLogit_dac_USA %>%
      add_title("Supply sector keywords for dac sector in all states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector logit exponents to to prospective dac states in region USA") %>%
      add_legacy_name("L262.SubsectorLogit_dac_USA") %>%
      add_precursors("L262.SubsectorLogit_dac") ->
      L262.SubsectorLogit_dac_USA

    L262.SubsectorShrwtFllt_dac_USA %>%
      add_title("Subsector shareweights of dac sector in dac producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector shareweights to to all states in region USA") %>%
      add_legacy_name("L262.SubsectorShrwtFllt_dac_USA") %>%
      add_precursors("L262.SubsectorShrwtFllt_dac") ->
      L262.SubsectorShrwtFllt_dac_USA

    L262.SubsectorInterp_dac_USA %>%
      add_title("Subsector shareweight interpolation of dac sector in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to all states in region USA") %>%
      add_legacy_name("L262.SubsectorInterp_dac_USA") %>%
      add_precursors("L262.SubsectorInterp_dac") ->
      L262.SubsectorInterp_dac_USA

    L262.StubTech_dac_USA %>%
      add_title("Identification of stub technologies of dac in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of dac to all states in region USA") %>%
      add_legacy_name("L262.StubTech_dac_USA") %>%
      add_precursors("L262.StubTech_dac") ->
      L262.StubTech_dac_USA

    L262.PerCapitaBased_dac_USA %>%
      add_title("Per-capita based flag for dac exports final demand in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded Per-capita based flag for dac exports final demand to all states in region USA") %>%
      add_legacy_name("L262.PerCapitaBased_dac_USA") %>%
      add_precursors("L262.PerCapitaBased_dac") ->
      L262.PerCapitaBased_dac_USA

    L262.PriceElasticity_dac_USA %>%
      add_title("Price elasticity for dac in all states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for dac to all states in region USA") %>%
      add_legacy_name("L262.PriceElasticity_dac_USA") %>%
      add_precursors("L262.PriceElasticity_dac") ->
      L262.PriceElasticity_dac_USA

    L262.FinalEnergyKeyword_dac_USA %>%
      add_title("Subsector logit exponents of dac sector in dac producing states") %>%
      add_units("Unitless") %>%
      add_comments("Expanded supply sector keywords information dac producing states in region USA") %>%
      add_legacy_name("L262.FinalEnergyKeyword_dac_USA") %>%
      add_precursors("L262.FinalEnergyKeyword_dac") ->
      L262.FinalEnergyKeyword_dac_USA

    # create L262.StubTechCoef_dac_USA for five ssps
    TECH_PARAMETRIZATION_OUTPUTS <- paste0("ssp", 1:5)

    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L262.StubTechCoef_dac_USA %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title(paste("dac technologies by state -", sce)) %>%
        add_units("coefficient = GJ/kg (gigajoules per kilogram of dac)") %>%
        add_comments("Rename markets with regional gird name if using regional regional fuel markets") %>%
        add_legacy_name("L262.StubTechCoef_dac_USA") %>%
        add_precursors("L262.GlobalTechCoef_dac","gcam-usa/states_subregions") ->
        x
      assign(paste0("L262.StubTechCoef_dac_USA_", tolower(sce)), x)
    }

    L262.Supplysector_dac_USA %>%
      add_title("Supply sector information for dac sector in all states") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information to dac producing states in region USA") %>%
      add_legacy_name("L262.Supplysector_dac_USA") %>%
      add_precursors("L262.Supplysector_dac") ->
      L262.Supplysector_dac_USA

    L262.StubTechProd_dac_USA %>%
      add_title("Dac calibration coefficients to all states") %>%
      add_units("NA") %>%
      add_comments("Added dac calibration coefficients to all states") %>%
      add_legacy_name("L262.StubTechProd_dac_USA") %>%
      add_precursors("gcam-usa/Dooley_CCS_USA","energy/A62.demand",
                     "energy/calibrated_techs_cdr", "L262.StubTechProd_dac") ->
      L262.StubTechProd_dac_USA

    L262.BaseService_dac_USA %>%
      add_title("Base-year service output for dac final demand input table") %>%
      add_units("NA") %>%
      add_comments("base service is equal to the output of the dac supplysector use") %>%
      add_legacy_name("L262.BaseService_dac_USA") %>%
      same_precursors_as("L262.StubTechProd_dac_USA") ->
      L262.BaseService_dac_USA

    L262.CarbonCoef_dac_USA %>%
      add_title("Carbon coefficent for airCO2 defined at state level") %>%
      add_units("NA") %>%
      add_comments("copy the same value 1 to all states") %>%
      add_legacy_name("L262.CarbonCoef_dac_USA") %>%
      add_precursors("L262.CarbonCoef_dac") ->
      L262.CarbonCoef_dac_USA


    return_data(L262.DeleteSupplysector_USAdac,
                L262.DeleteFinalDemand_USAdac,
                L262.SubsectorLogit_dac_USA,
                L262.SubsectorShrwtFllt_dac_USA,
                L262.SubsectorInterp_dac_USA,
                L262.StubTech_dac_USA,
                L262.PerCapitaBased_dac_USA,
                L262.PriceElasticity_dac_USA,
                L262.FinalEnergyKeyword_dac_USA,
                L262.BaseService_dac_USA,
                L262.StubTechCoef_dac_USA_ssp1,
                L262.StubTechCoef_dac_USA_ssp2,
                L262.StubTechCoef_dac_USA_ssp3,
                L262.StubTechCoef_dac_USA_ssp4,
                L262.StubTechCoef_dac_USA_ssp5,
                L262.Supplysector_dac_USA,
                L262.StubTechProd_dac_USA,
                L262.CarbonCoef_dac_USA)
  } else {
    stop("Unknown command")
  }
}
