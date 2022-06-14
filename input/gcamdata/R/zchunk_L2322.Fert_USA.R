# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2322.Fert_USA
#'
#' Produce tables to create the N fertilizer sector in GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.DeleteSubsector_USAFert}, \code{L2322.FinalEnergyKeyword_USAFert}, \code{L2322.FinalEnergyKeyword_Fert_USA}, \code{L2322.StubTech_Fert_USA}, \code{L2322.SubsectorLogit_USAFert}, \code{L2322.SubsectorShrwtFllt_USAFert}, \code{L2322.TechShrwt_USAFert}, \code{L2322.Production_USAFert}, \code{L2322.TechCoef_USAFer}, \code{L2322.StubTechProd_Fert_USA}, \code{L2322.StubTechCoef_Fert_USA}, \code{L2322.StubTechMarket_Fert_USA}, \code{L2322.SubsectorLogit_Fert_USA}, \code{L2322.Supplysector_Fert_USA}, \code{L2322.SubsectorShrwtFllt_Fert_USA}, \code{L2322.SubsectorInterp_Fert_USA},\code{L2322.SubsectorInterp_USAFert}
#'  The corresponding file in the original data system was \code{L2322.Fert_USA.R} (gcam-usa level2).
#' @details This chunk produces tables to create the N fertilizer sector in GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter if_else left_join mutate select
#' @importFrom tidyr unite
#' @author KD October 2017
module_gcamusa_L2322.Fert_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.globaltech_coef",
             "L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L1322.IO_GJkg_state_Fert_F_Yh",
             "L1322.out_Mt_state_Fert_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.DeleteSubsector_USAFert",
             "L2322.FinalEnergyKeyword_USAFert",
             "L2322.FinalEnergyKeyword_Fert_USA",
             "L2322.StubTech_Fert_USA",
             "L2322.SubsectorLogit_USAFert",
             "L2322.SubsectorShrwtFllt_USAFert",
             "L2322.TechShrwt_USAFert",
             "L2322.Production_USAFert",
             "L2322.TechCoef_USAFert",
             "L2322.StubTechProd_Fert_USA",
             "L2322.StubTechCoef_Fert_USA",
             "L2322.StubTechMarket_Fert_USA",
             "L2322.SubsectorLogit_Fert_USA",
             "L2322.Supplysector_Fert_USA",
             "L2322.SubsectorShrwtFllt_Fert_USA",
             "L2322.SubsectorInterp_Fert_USA",
             "L2322.SubsectorInterp_USAFert"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef", strip_attributes = TRUE)
    L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert", strip_attributes = TRUE)
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert", strip_attributes = TRUE)
    L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert", strip_attributes = TRUE)
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert", strip_attributes = TRUE)
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert", strip_attributes = TRUE)
    L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert", strip_attributes = TRUE)
    L1322.IO_GJkg_state_Fert_F_Yh <- get_data(all_data, "L1322.IO_GJkg_state_Fert_F_Yh", strip_attributes = TRUE)
    L1322.out_Mt_state_Fert_Yh <- get_data(all_data, "L1322.out_Mt_state_Fert_Yh", strip_attributes = TRUE)

    # Silence package checks
    regions <- supplysector <- subsector <- state <- value <- year <- subs.share.weights <-
      technology <- share.weight.year <- minicam.energy.input <- coefficient <- market.name <-
      sector <- fuel <- stub.technology <- grid_region <- region <- calOutputValue <-
      subs.share.weight <- tech.share.weight <- logit.year.fillout <- logit.exponent <- logit.type <- NULL


    # ===================================================
    # In the old data system this chunk processed the user defined outputs,
    # L2322.SubsectorInterpTo_Fert and L2322.SubsectorShrwt_Fert, that were produced by
    # the upstream chunk, L2322.Fert. However as per discission with P. Kyle
    # and K. Calvin these outputs were removed from the upstream chunk and therefore are no
    # longer processed by this chunk.

    # In the GCAM region USA N fertilizer is retained as a sector, as is the Imports subsector
    # but the the fuel subsectors will be deleted and replaced with state subsectors. Subset the
    # subsector logit exponents of fertilizer sector for the fuel subsectors to be removed in
    # GCAM-USA.
    L2322.SubsectorLogit_Fert %>%
      filter(region == gcam.USA_REGION, supplysector == gcamusa.FERT_NAME, subsector != "Imports") %>%
      mutate(region = region) %>%
      select(region, supplysector, subsector) ->
      L2322.DeleteSubsector_USAFert

    # Subset the supply sector keywords for fertilizer sector in the USA region.
    L2322.FinalEnergyKeyword_Fert %>%
      filter(region == gcam.USA_REGION) %>%
      mutate(final.energy = "none") ->
      L2322.FinalEnergyKeyword_USAFert


    # Since N fertilizer sectors are only created in states where the NAICS shipping information
    # indicates fertilizer production, create a tibble of the fertilizer producing states. This
    # tibble will be used to create the N fertilizer tables for GCAM-USA.
    L1322.out_Mt_state_Fert_Yh %>%
      select(state) %>%
      distinct ->
      Fert_states

    # Select the supply sector information for fertilizer sector within the US and expand to all of the
    # sates that are fertilizer producers, then create subsector from state and fertilizer name.
    L2322.Supplysector_Fert %>%
      filter(region == gcam.USA_REGION, supplysector == gcamusa.FERT_NAME) %>%
      select(region, supplysector) %>%
      repeat_add_columns(Fert_states) %>%
      mutate(subsector = paste(state, gcamusa.FERT_NAME)) ->
      L2322.Supplysector_Fert_states

    # Now add the logit table information to the state fertilizer supply sector data frame.
    L2322.Supplysector_Fert_states %>%
      mutate(logit.year.fillout = min(MODEL_YEARS),
             logit.exponent = gcamusa.FERT_LOGIT_EXP,
             logit.type = NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L2322.SubsectorLogit_USAFert

    # Create the subsector default share-weights for the using the min base years
    # for the fill out year.
    L2322.SubsectorLogit_USAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) ->
      L2322.SubsectorShrwtFllt_USAFert

    # Use the subsector logit exponents of fertilizer sector to create
    # a table of the subsector default technology share-weights for the US.
    # that will be interpolated.
    L2322.SubsectorLogit_USAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2322.SubsectorInterp_USAFert


    # Expand the supply sector and subsector share weights to all model years.
    L2322.SubsectorLogit_USAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2322.TechShrwt_USAFert


    # Subset the state fertilizer production data for model base years,
    # format digits, and add region and supplysector information to prepare
    # the data frame to add logit table information.
    L1322.out_Mt_state_Fert_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, aglu.DIGITS_LAND_USE)) %>%
      select(-value) %>%
      mutate(region = gcam.USA_REGION, supplysector = gcamusa.FERT_NAME) %>%
      unite(subsector, state, supplysector, sep = " ", remove = FALSE) ->
      L2322.Production_USAFert

    # Add technology and subsector share weights and other logit table
    # information to the calibrated output production for fertilizer in the
    # USA region fertilizer sector.
    L2322.Production_USAFert %>%
      mutate(technology = subsector,
             input = gcamusa.FERT_NAME,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue == 0, 0, 1),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, technology, year, calOutputValue,
             share.weight.year, subs.share.weight,
             tech.share.weight) ->
      L2322.Production_USAFert


    # Add minicam energy input information and coefficient to the
    # to the technology share weight data frame.
    L2322.TechShrwt_USAFert %>%
      mutate(minicam.energy.input = gcamusa.FERT_NAME,
             coefficient = 1) %>%
      # Parse out state market name from the fertilizer subsector.
      mutate(market.name = substr(start = 1, stop = 2, subsector)) %>%
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name) ->
      L2322.TechCoef_USAFert


    # The Fert_USA_processing function replaces the "identical processing for loop"
    # in the old data system. The function inputs include an input data frame to be
    # checked and processed if deemed necessary and a list of the fertilizer producing
    # states.

    Fert_USA_processing <- function(data, Fert_states) {

      # Subset the input data frame for the USA region and N fertilizer supply sector.
      # If the subsetted data frame does not contain any fertilizer supplysector information
      # for the USA region then it is assumed that the data frame has already been
      # processed, and the input data frame is returned as is.
      check_df <- dplyr::filter(data, region == gcam.USA_REGION & supplysector == gcamusa.FERT_NAME)

      if(nrow(check_df) == 0) {
        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)
      } else {

        # If the data frame contains USA region information for the N fertilizer
        # supply sector then expand the input data to all states then subset for
        # the fertilizer producing states only.

        # Save the column names for the input data frame.
        df_names <- names(data)

        # Subset for observations in the USA region and expand all of the
        # input data frame columns to all USA states and then subset by the
        # fertilizer producing states.
        data %>%
          filter(region == gcam.USA_REGION, supplysector == gcamusa.FERT_NAME) %>%
          write_to_all_states(names = df_names) %>%
          filter(region %in% Fert_states[["state"]]) ->
          new_df

        # If the input data frame includes subsector information subset the
        # data frame for gas since state-level N fertilizer should not include
        # the Imports subsector and there is no need for the alternative fuels either.
        check_subsector <- c("subsector" %in% names(new_df))
        if(check_subsector) {
          new_df %>%
            filter(grepl("gas", subsector)) ->
            new_df
        }
      }
      return(new_df)
    } # end of function


    # Use the Fert_USA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all fertilizer producing states without
    # the Imports subsector if applicable.
    L2322.FinalEnergyKeyword_Fert_USA <- Fert_USA_processing(L2322.FinalEnergyKeyword_Fert, Fert_states)
    L2322.Supplysector_Fert_USA <- Fert_USA_processing(L2322.Supplysector_Fert, Fert_states)
    L2322.SubsectorLogit_Fert_USA <- Fert_USA_processing(L2322.SubsectorLogit_Fert, Fert_states)
    L2322.StubTech_Fert_USA <- Fert_USA_processing(L2322.StubTech_Fert, Fert_states)
    L2322.SubsectorShrwtFllt_Fert_USA <- Fert_USA_processing(L2322.SubsectorShrwtFllt_Fert, Fert_states)
    L2322.SubsectorInterp_Fert_USA <- Fert_USA_processing(L2322.SubsectorInterp_Fert, Fert_states)


    # Create the logit table for the calibrated state fertilizer production.
    #
    # Start by formating the state fertilizer production by subsetting for model base years,
    # rounding to the appropriate digits, and adding region information.
    L1322.out_Mt_state_Fert_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, digits = gcamusa.DIGITS_CALOUTPUT),
             region = state) ->
      L2322.StubTechProd_Fert_USA

    # Next combine the formated state fertilizer production data frame with the mapping form
    # calibrated intermediate sectors and fuels to supplysector / subsector / technology / input in GCAM
    # data frame.
    L2322.StubTechProd_Fert_USA %>%
      left_join_error_no_match(calibrated_techs %>% select(sector, fuel, supplysector, technology, subsector),
                               by = c("fuel", "sector")) ->
      L2322.StubTechProd_Fert_USA

    # Lastly, add the logit table information.
    L2322.StubTechProd_Fert_USA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year,
             subs.share.weight, tech.share.weight) ->
      L2322.StubTechProd_Fert_USA


    # Create the logit table for the coefficients of fertilizer production technologies
    #
    # Start by subsetting the state fertilizer input-output coefficient data frame for model base years
    # and rounding the input-output coefficient value to the appropriate digits.
    L1322.IO_GJkg_state_Fert_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(coefficient = signif(value, aglu.DIGITS_LAND_USE)) %>%
      select(-value) %>%
      mutate(region = state) ->
      L2322.StubTechCoef_Fert_USA

    # Next combine the formated input-output data frame with the mapping form calibrated
    # intermediate sectors and fuels to supplysector / subsector / technology / input in GCAM
    # data frame.
    L2322.StubTechCoef_Fert_USA %>%
      left_join(calibrated_techs %>% select(supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                by = c("fuel", "sector")) ->
      L2322.StubTechCoef_Fert_USA

    # Next add stub.technology and market.name columns and select the columns to include in the final
    # output.
    L2322.StubTechCoef_Fert_USA %>%
      mutate(stub.technology = technology, market.name = gcam.USA_REGION) %>%
      select(region, supplysector, subsector, stub.technology,
             year, minicam.energy.input, coefficient, market.name) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
             grid_region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                   region, market.name)) %>%
      select(-grid_region) ->
	  L2322.StubTechCoef_Fert_USA


    # Create a table of the market for the fuel inputs into the state fertilizer sectors
    #
    # Start by expanding the region / supplysector / stub.technology for the fertilizer producing states
    # to all model years.
    L2322.StubTech_Fert_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L2322.StubTechMarket_Fert_USA

    # Then add minicam.energy.input coefficients from the fertilizer production default coefficients data frame
    # by supplysector, subsector, and technology, then add USA as the default market name.
    L2322.StubTechMarket_Fert_USA %>%
      left_join_error_no_match(A322.globaltech_coef %>%
                                 select(supplysector, subsector, technology, minicam.energy.input),
                               by = c("supplysector", "subsector", c("stub.technology" = "technology"))) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(-grid_region) ->
	  L2322.StubTechMarket_Fert_USA


    # ===================================================

    # Produce outputs
    L2322.DeleteSubsector_USAFert %>%
      add_title("Subsector logit exponents of fertilizer sector to remove from GCAM-USA") %>%
      add_units("NA") %>%
      add_comments("Subset L2322.SubsectorLogit_Fert for all observation other than subsector Imports and supplysector N fertilizer in the US") %>%
      add_legacy_name("L2322.DeleteSubsector_USAFert") %>%
      add_precursors("L2322.SubsectorLogit_Fert") ->
      L2322.DeleteSubsector_USAFert

    L2322.FinalEnergyKeyword_USAFert %>%
      add_title("Supply sector keywords for fertilizer sector for GCAM USA") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for fertilizer are subset for the USA region.") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_USAFert") %>%
      add_precursors("L2322.FinalEnergyKeyword_Fert") ->
      L2322.FinalEnergyKeyword_USAFert

    L2322.FinalEnergyKeyword_Fert_USA %>%
      add_title("Supply sector keywords for fertilizer sector for fertilizer producing states in region USA") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for fertilizer are subset for the USA region then expanded for all fertilizer producing states") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert_USA") %>%
      add_precursors("L2322.FinalEnergyKeyword_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.FinalEnergyKeyword_Fert_USA

    L2322.StubTech_Fert_USA %>%
      add_title("Stub-technology (gas and gas CCS) for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Stub-technology (coal, coal CCS, and etc.) for fertilizer sector in region USA expanded to all fertilizer producing states.") %>%
      add_comments("Delete the fuel subsector and replace with state  fuel subsectors will be deleted and replaced with the relevant state subsectors (gas).") %>%
      add_legacy_name("L2322.StubTech_Fert_USA") %>%
      add_precursors("L2322.StubTech_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.StubTech_Fert_USA

    L2322.SubsectorLogit_USAFert %>%
      add_title("Subsector logit exponents of fertilizer sector for GCAM USA") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector in region USA, the subsector logit exponents are expanded for US states with fertilizer census data.") %>%
      add_legacy_name("L2322.SubsectorLogit_USAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorLogit_USAFert

    L2322.SubsectorShrwtFllt_USAFert %>%
      add_title("Subsector share-weight fill out table for fertilizer sector in region USA") %>%
      add_units("NA") %>%
      add_comments("Added share-weight fillout and fill out year to fertilizer supplysector / subsector for region USA") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_USAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorShrwtFllt_USAFert

    L2322.SubsectorInterp_USAFert %>%
      add_title("Subsector interpolation table for fertilizer subsector in region USA") %>%
      add_units("NA") %>%
      add_comments("Expanded USA region subsector to all fertilizer producing states in region USA") %>%
      add_comments("Added the apply.to, from.year, and interpolation.function columns") %>%
      add_legacy_name("L2322.SubsectorInterp_USAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorInterp_USAFert

    L2322.TechShrwt_USAFert %>%
      add_title("Technology share-weight for N fertilizer in region USA") %>%
      add_units("NA") %>%
      add_comments("Added fertilizer producing states to subsector and supply sector information in region USA") %>%
      add_comments("Expanded for all model years") %>%
      add_legacy_name("L2322.TechShrwt_USAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.TechShrwt_USAFert

    L2322.Production_USAFert %>%
      add_title("Calibrated fertilizer production in region USA by state fertilizer supplysector") %>%
      add_units("calOutputValue = Mt (megatonnes = teragrams)") %>%
      add_comments("Added share-wight information to state fertilizer production") %>%
      add_legacy_name("L2322.Production_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.Production_USAFert

    L2322.TechCoef_USAFert %>%
      add_title("Technology coefficients of USA region fertilizer") %>%
      add_units("NA") %>%
      add_comments("Added fertilizer producing states to fertilizer subsector & technology in region USA") %>%
      add_comments("Added minicam.energy.input, coefficient, and market.name") %>%
      add_legacy_name("L2322.TechCoef_USAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.TechCoef_USAFert

    L2322.StubTechProd_Fert_USA %>%
      add_title("Calibrated fertilizer production by state") %>%
      add_units("value = Mt (megatonnes = teragrams)") %>%
      add_comments("Added supplysector / technology / subsector information from calibrated_techs mapping file") %>%
      add_comments("Added share weight information to state fertilizer production from L1322.out_Mt_state_Fert_Yh") %>%
      add_legacy_name("L2322.StubTechProd_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh", "energy/calibrated_techs") ->
      L2322.StubTechProd_Fert_USA

    L2322.StubTechCoef_Fert_USA %>%
      add_title("Stub-technology input output energy coefficient for fertilizer production in region USA") %>%
      add_units("coefficient = GJkg (gigajoules used/kg fertilizer produced)") %>%
      add_comments("Added supplysector / subsector / technology / minicam.energy.input information from calibrated_tech mapping file") %>%
      add_comments("Add market.name, default is USA but depending on user defined input, market.name cane be replace with region grid from states_subregions ") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh", "L1322.IO_GJkg_state_Fert_F_Yh", "gcam-usa/states_subregions", "energy/calibrated_techs") ->
      L2322.StubTechCoef_Fert_USA

    L2322.StubTechMarket_Fert_USA %>%
      add_title("Market for the fuel inputs to the state fertilizer sectors by fertilizer producing states") %>%
      add_units("NA") %>%
      add_comments("Added minicam.energy.input to subsector/ stub.technology for supplysector expanded to all N fertilizer producing states") %>%
      add_comments("Add market.name, default is USA but depending on user defined input, market.name cane be replace with region grid from states_subregions ") %>%
      add_legacy_name("L2322.StubTechMarket_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh", "L2322.StubTech_Fert", "gcam-usa/states_subregions", "energy/A322.globaltech_coef") ->
      L2322.StubTechMarket_Fert_USA

    L2322.SubsectorLogit_Fert_USA %>%
      add_title("Subsector logit exponents of fertilizer sector for fertilizer producing states in region GCAM USA") %>%
      add_units("NA") %>%
      add_comments("Replace region from L2322.SubsectorLogit_USAFert with the fertilizer producing states within region USA") %>%
      add_comments("Replace state subsector with gas") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert_USA") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorLogit_Fert_USA

    L2322.Supplysector_Fert_USA %>%
      add_title("Supply sector information for fertilizer sector for fertilizer producing states in region USA") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information for fertilizer sector for region USA to all fertilizer producing states") %>%
      add_legacy_name("L2322.Supplysector_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh", "L2322.Supplysector_Fert") ->
      L2322.Supplysector_Fert_USA

    L2322.SubsectorShrwtFllt_Fert_USA %>%
      add_title("Subsector share-weight fill out table for fertilizer sector in fertilizer producing states") %>%
      add_units("NA") %>%
      add_comments("Added share-weight fillout and fill out year to fertilizer supplysector / subsector for region USA") %>%
      add_comments("Expanded to all fertilizer producing states in region USA and subsetted for the relevant state subsector (gas)") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert_USA") %>%
      add_precursors("L2322.SubsectorShrwtFllt_Fert", "L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorShrwtFllt_Fert_USA

    L2322.SubsectorInterp_Fert_USA %>%
      add_title("Subsector interpolation table for fertilizer producing states in region USA") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector to fertilizer producing states in region USA") %>%
      add_comments("Added the apply.to, from.year, and interpolation.function columns") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert_USA") %>%
      add_precursors("L2322.SubsectorInterp_Fert", "L2322.Supplysector_Fert", "L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorInterp_Fert_USA

    return_data(L2322.DeleteSubsector_USAFert, L2322.FinalEnergyKeyword_USAFert, L2322.FinalEnergyKeyword_Fert_USA,
                L2322.StubTech_Fert_USA, L2322.SubsectorLogit_USAFert, L2322.SubsectorShrwtFllt_USAFert, L2322.TechShrwt_USAFert,
                L2322.Production_USAFert, L2322.TechCoef_USAFert, L2322.StubTechProd_Fert_USA,
                L2322.StubTechCoef_Fert_USA, L2322.StubTechMarket_Fert_USA, L2322.SubsectorLogit_Fert_USA,
                L2322.Supplysector_Fert_USA, L2322.SubsectorShrwtFllt_Fert_USA, L2322.SubsectorInterp_Fert_USA,
                L2322.SubsectorInterp_USAFert)
    } else {
      stop("Unknown command")
    }
  }
