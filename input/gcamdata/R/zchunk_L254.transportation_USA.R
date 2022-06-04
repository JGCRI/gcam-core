# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L254.transportation_USA
#'
#' Generates GCAM-USA model inputs for transportation sector by states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.DeleteSupplysector_USAtrn}, \code{L254.DeleteFinalDemand_USAtrn},
#' \code{L254.Supplysector_trn_USA}, \code{L254.FinalEnergyKeyword_trn_USA}, \code{L254.tranSubsectorLogit_USA},
#' \code{L254.tranSubsectorShrwtFllt_USA}, \code{L254.tranSubsectorInterp_USA}, \code{L254.tranSubsectorSpeed_USA},
#' \code{L254.tranSubsectorSpeed_passthru_USA}, \code{L254.tranSubsectorSpeed_noVOTT_USA},
#' \code{L254.tranSubsectorSpeed_nonmotor_USA}, \code{L254.tranSubsectorVOTT_USA}, \code{L254.tranSubsectorFuelPref_USA},
#' \code{L254.StubTranTech_USA}, \code{L254.StubTranTech_passthru_USA}, \code{L254.StubTranTech_nonmotor_USA},
#' \code{L254.StubTranTechLoadFactor_USA}, \code{L254.StubTranTechCost_USA}, \code{L254.StubTranTechCoef_USA},
#' \code{L254.PerCapitaBased_trn_USA}, \code{L254.PriceElasticity_trn_USA}, \code{L254.IncomeElasticity_trn_USA},
#' \code{L254.StubTranTechCalInput_USA}, \code{L254.StubTranTechProd_nonmotor_USA}, \code{L254.StubTranTechCalInput_passthru_USA},
#' \code{L254.BaseService_trn_USA}.
#' The corresponding file in the original data system was \code{L254.transportation_USA.R} (gcam-usa level2).
#' @details This chunk generates input files for transportation sector with generic information for supplysector,
#' subsector and technologies, as well as calibrated inputs and outputs by the US states.
#' @note The transportation structure is heavily nested. The GCAM structure of sector/subsector/technology only
#' allows two levels of nesting within any sector, but a technology of one sector (e.g., trn_pass) can consume the
#' output of another "sector" (e.g., trn_pass_road) that is really just used to represent lower nesting levels of
#' that first, or parent, sector. In the transportation sector, each lower-level nesting "sector" is named by
#' appending a string to the parent sector. So, \code{trn_pass} contains \code{trn_pass_road} which has
#' \code{trn_pass_road_LDV} which has \code{trn_pass_road_LDV_4W}. Each of the links between any two of those sectors
#' is done with a pass-through technology within the parent sector that consumes the output of the child sector.
#' The technology is called a "pass-through" because it (generally) only consumes the output of the child "sector"
#' without making any changes to it. There's an additional complication in the transportation sector, that the
#' pass-through technologies are normal, standard GCAM technologies, not "tranTechnologies" which have different
#' parameters read in, and perform a bunch of hard-wired unit conversions between inputs and outputs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr replace_na
#' @author RC Oct 2017
module_gcamusa_L254.transportation_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/UCD_techs",
             FILE = "energy/A54.globaltech_nonmotor",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.sector",
             FILE=  "energy/mappings/UCD_size_class_revisions",
             FILE=  "energy/mappings/UCD_techs_revised",
             FILE = "gcam-usa/states_subregions",
             "L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L154.in_EJ_state_trn_m_sz_tech_F",
             "L154.out_mpkm_state_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.DeleteSupplysector_USAtrn",
             "L254.DeleteFinalDemand_USAtrn",
             "L254.Supplysector_trn_USA",
             "L254.FinalEnergyKeyword_trn_USA",
             "L254.tranSubsectorLogit_USA",
             "L254.tranSubsectorShrwtFllt_USA",
             "L254.tranSubsectorInterp_USA",
             "L254.tranSubsectorSpeed_USA",
             "L254.tranSubsectorSpeed_passthru_USA",
             "L254.tranSubsectorSpeed_noVOTT_USA",
             "L254.tranSubsectorSpeed_nonmotor_USA",
             "L254.tranSubsectorVOTT_USA",
             "L254.tranSubsectorFuelPref_USA",
             "L254.StubTranTech_USA",
             "L254.StubTranTech_passthru_USA",
             "L254.StubTranTech_nonmotor_USA",
             "L254.StubTranTechLoadFactor_USA",
             "L254.StubTranTechCost_USA",
             "L254.StubTranTechCoef_USA",
             "L254.PerCapitaBased_trn_USA",
             "L254.PriceElasticity_trn_USA",
             "L254.IncomeElasticity_trn_USA",
             "L254.StubTranTechCalInput_USA",
             "L254.StubTranTechProd_nonmotor_USA",
             "L254.StubTranTechCalInput_passthru_USA",
             "L254.BaseService_trn_USA",
             "L254.StubTranTechOutput_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    region <- supplysector <- technology <- minicam.energy.input <- year <- value <- coefficient <-
      UCD_fuel <- UCD_sector <- UCD_technology <- calibrated.value <- coefficient <- loadFactor <-
      size.class <- state <- tranSubsector <- tranTechnology <- calibrated.value <- stub.technology <-
      output <- output_agg <- output_cum <- share.weight <- subs.share.weight <- share.weight.year <-
      tech.share.weight <- calOutputValue <- energy.final.demand <- base.service <- year.fillout <-
      fuelprefElasticity <- income.elasticity <- UCD_region <- sce <- . <- NULL

    # Load required inputs
    #kbn 2019-10-14 Making same changes here for UCD techs that we made in L254:
    Size_class_New<- get_data(all_data, "energy/mappings/UCD_size_class_revisions",strip_attributes = TRUE) %>%
      select(-UCD_region) %>%
      distinct()

    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs",strip_attributes = TRUE) # Mapping file of transportation technology from the UC Davis report (Mishra et al. 2013)

    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){

      UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs_revised")

      UCD_techs<-UCD_techs %>%
        inner_join(Size_class_New, by=c("mode","size.class"))%>%
        select(-mode,-size.class)%>%
        distinct()

      colnames(UCD_techs)[colnames(UCD_techs)=='rev_size.class']<-'size.class'
      colnames(UCD_techs)[colnames(UCD_techs)=='rev.mode']<-'mode'
    }

    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor",strip_attributes = TRUE)
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru",strip_attributes = TRUE)
    A54.sector <- get_data(all_data, "energy/A54.sector",strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions",strip_attributes = TRUE)
    #kbn 2020-02-27 Making changes to select the CORE scenario for transportation in GCAM USA
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn",strip_attributes = TRUE) %>% filter(sce %in% c("CORE"))
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn",strip_attributes = TRUE)%>% filter(sce %in% c("CORE"))
    L154.in_EJ_state_trn_m_sz_tech_F <- get_data(all_data, "L154.in_EJ_state_trn_m_sz_tech_F",strip_attributes = TRUE)
    L154.out_mpkm_state_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_state_trn_nonmotor_Yh",strip_attributes = TRUE)

    # Need to delete the transportation sector in the USA region (energy-final-demands and supplysectors)
    # L254.DeleteSupplysector_USAtrn: Delete transportation supplysectors of the USA region
    L254.Supplysector_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) ->
      L254.DeleteSupplysector_USAtrn

    # L254.DeleteFinalDemand_USAtrn: Delete energy final demand sectors of the USA region
    L254.PerCapitaBased_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["EnergyFinalDemand"]],sce) ->
      L254.DeleteFinalDemand_USAtrn

    # Process tables at the USA region level to the states level.
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_USA_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.USA_REGION) %>%
        write_to_all_states(names = c(names(data), "region"))

      # Re-set markets from USA to grid region, if the minicam.energy.input is considered a regional fuel market
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      # For fuels consumed from state markets, the market.name is the region
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS,
                                       region[minicam.energy.input %in% gcamusa.STATE_FUEL_MARKETS]))
      }

      data_new
    }

    process_USA_to_states(L254.Supplysector_trn) -> L254.Supplysector_trn_USA
    process_USA_to_states(L254.FinalEnergyKeyword_trn) -> L254.FinalEnergyKeyword_trn_USA
    process_USA_to_states(L254.tranSubsectorLogit) -> L254.tranSubsectorLogit_USA
    process_USA_to_states(L254.tranSubsectorShrwtFllt) -> L254.tranSubsectorShrwtFllt_USA
    process_USA_to_states(L254.tranSubsectorInterp) -> L254.tranSubsectorInterp_USA

    process_USA_to_states(L254.tranSubsectorSpeed) %>%
      # parent file (L254.tranSubsectorSpeed) contains duplicate values... remove these
      distinct()-> L254.tranSubsectorSpeed_USA

    process_USA_to_states(L254.tranSubsectorSpeed_passthru) -> L254.tranSubsectorSpeed_passthru_USA
    process_USA_to_states(L254.tranSubsectorSpeed_noVOTT) -> L254.tranSubsectorSpeed_noVOTT_USA
    process_USA_to_states(L254.tranSubsectorSpeed_nonmotor) -> L254.tranSubsectorSpeed_nonmotor_USA
    process_USA_to_states(L254.tranSubsectorVOTT) -> L254.tranSubsectorVOTT_USA
    process_USA_to_states(L254.tranSubsectorFuelPref) -> L254.tranSubsectorFuelPref_USA
    process_USA_to_states(L254.StubTranTech) -> L254.StubTranTech_USA
    process_USA_to_states(L254.StubTech_passthru) -> L254.StubTranTech_passthru_USA
    process_USA_to_states(L254.StubTech_nonmotor) -> L254.StubTranTech_nonmotor_USA
    process_USA_to_states(L254.StubTranTechLoadFactor) -> L254.StubTranTechLoadFactor_USA
    process_USA_to_states(L254.StubTranTechCost) -> L254.StubTranTechCost_USA

    L254.StubTranTechCoef %>%
      mutate(coefficient = round(coefficient, digits = gcamusa.DIGITS_TRNUSA_DEFAULT)) %>%
      process_USA_to_states ->
      L254.StubTranTechCoef_USA

    process_USA_to_states(L254.PerCapitaBased_trn) -> L254.PerCapitaBased_trn_USA
    process_USA_to_states(L254.PriceElasticity_trn) -> L254.PriceElasticity_trn_USA
    process_USA_to_states(L254.IncomeElasticity_trn) -> L254.IncomeElasticity_trn_USA

    # Calibration
    # L254.StubTranTechCalInput_USA: calibrated energy consumption by all technologies
    #kbn 2019-10-14 Switching to left_join_keep_first
    L154.in_EJ_state_trn_m_sz_tech_F %>%
      mutate(sce=paste0("CORE")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_keep_first_only(select(UCD_techs, UCD_sector, mode, size.class, UCD_technology, UCD_fuel,
                                      supplysector, tranSubsector, stub.technology = tranTechnology, minicam.energy.input),
                               by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, minicam.energy.input, calibrated.value,sce) ->
      L254.StubTranTechCalInput_USA

    # NOTE: NEED TO WRITE THIS OUT FOR ALL TECHNOLOGIES, NOT JUST THOSE THAT EXIST IN SOME BASE YEARS.
    # Model may make up calibration values otherwise.
    L254.StubTranTechCoef_USA %>%
      filter(sce=="CORE") %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(c(names(.)[names(.) %in% LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]],sce)) %>%
      left_join(L254.StubTranTechCalInput_USA,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input","sce")) %>%
      # Set calibration values as zero for technolgies that do not exist in some base years
      replace_na(list(calibrated.value = 0)) %>%
      mutate(share.weight.year = year,
             # Create the needed variables to use the function set_subsector_shrwt
             subsector = tranSubsector, calOutputValue = calibrated.value) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]],sce) ->
      L254.StubTranTechCalInput_USA

    # Non-motorized technologies
    # L254.StubTranTechProd_nonmotor_USA: service output of non-motorized transportation technologies
    L154.out_mpkm_state_trn_nonmotor_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_MPKM),
             region = state, tranSubsector = mode) %>%
      left_join_error_no_match(A54.globaltech_nonmotor, by = "tranSubsector") %>%
      mutate(stub.technology = technology) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, calOutputValue) ->
      L254.StubTranTechProd_nonmotor_USA

    # L254.StubTranTechCalInput_passthru_USA: calibrated input of passthrough technologies
    # trn_pass, trn_pass_road, trn_pass_road_LDV, trn_freight

    # The transportation structure is heavily nested.
    # The GCAM structure of sector/subsector/technology only allows two levels of nesting within any sector,
    # but a technology of one sector (e.g., trn_pass) can consume the output of another "sector" (e.g., trn_pass_road)
    # that is really just used to represent lower nesting levels of that first, or parent, sector. In the
    # transportation sector, each lower-level nesting "sector" is named by appending a string to the parent sector.
    # So, trn_pass contains trn_pass_road which has trn_pass_road_LDV which has trn_pass_road_LDV_4W. Each of the links
    # between any two of those sectors is done with a pass-through technology within the parent sector that consumes
    # the output of the child sector. The technology is called a "pass-through" because it (generally) only consumes
    # the output of the child "sector" without making any changes to it. There's an additional complication in the
    # transportation sector: the pass-through technologies are normal, standard GCAM technologies, not "tranTechnologies"
    # which have different parameters read in, and perform a bunch of hard-wired unit conversions between inputs and outputs.

    # First, need to calculate the service output for all tranTechnologies
    # calInput * loadFactor * unit_conversion / (coef * unit conversion)
    L254.StubTranTechCalInput_USA %>%
      left_join_error_no_match(L254.StubTranTechLoadFactor_USA %>% filter(sce=="CORE"),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year","sce")) %>%
      left_join_error_no_match(L254.StubTranTechCoef_USA%>% filter(sce=="CORE"),
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input","sce")) %>%
      mutate(output = round(calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
                            digits = gcamusa.DIGITS_TRNUSA_DEFAULT))  ->
      L254.StubTranTechOutput_USA

    # The next step is to calculate the aggregated outputs by supplysector
    # Outputs of certain supplysectors are inputs for the passthrough technologies
    L254.StubTranTechOutput_USA %>%
      filter(sce=="CORE") %>%
      group_by(region, year, supplysector) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTranTechOutput_USA_agg

    # Write all possible pass-through technologies to all regions
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_states(names = c(names(.), "region")) %>%
      select(region, supplysector, tranSubsector, stub.technology = technology, year, minicam.energy.input) %>%
      # Subset only the passthrough technologies that are applicable in each region
      semi_join(L254.StubTranTech_passthru_USA,
                by = c("region", "supplysector", "tranSubsector", "stub.technology")) %>%
      # Match in outputs of supplysectors that are inputs for the passthrough technologies
      left_join(L254.StubTranTechOutput_USA_agg,
                by = c("region", "year", "minicam.energy.input" = "supplysector")) %>%
      # Some of the technologies are sub-totals, assign zero value now, will be calculated below
      replace_na(list(output_agg = 0)) %>%
      # Arrange input sectors so that sub-total sector is behind the subsectors
      arrange(dplyr::desc(minicam.energy.input)) %>%
      group_by(region, year) %>%
      # Calculate the cumulative for sub-total sector
      mutate(output_cum = cumsum(output_agg)) %>%
      ungroup() ->
      L254.StubTranTechCalInput_passthru_USA_cum

    # Prepare a list of the supplysector in the passthrough input table to filter the sub-total sectors
    LIST_supplysector <- unique(L254.StubTranTechCalInput_passthru_USA_cum$supplysector)

    L254.StubTranTechCalInput_passthru_USA_cum %>%
      # Use the cumulative value for sub-total sectors
      mutate(calibrated.value = if_else(minicam.energy.input %in% LIST_supplysector,
                                        output_cum, output_agg)) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]) ->
      L254.StubTranTechCalInput_passthru_USA

    # L254.BaseService_trn_USA: base-year service output of transportation final demand
    L254.StubTranTechOutput_USA %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, base.service = output) %>%
      bind_rows(L254.StubTranTechProd_nonmotor_USA %>%
                  select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, base.service = calOutputValue)) %>%
      left_join_error_no_match(select(A54.sector, supplysector, energy.final.demand), by = "supplysector") %>%
      group_by(region, energy.final.demand, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup ->
      L254.BaseService_trn_USA


    # Produce outputs
    L254.DeleteSupplysector_USAtrn %>%
      add_title("Delect transportation supply sectors of the full USA region") %>%
      add_units("NA") %>%
      add_comments("Delect transportation supply sectors of the full USA region") %>%
      add_legacy_name("L254.DeleteSupplysector_USAtrn") %>%
      add_precursors("L254.Supplysector_trn") ->
      L254.DeleteSupplysector_USAtrn

    L254.DeleteFinalDemand_USAtrn %>%
      add_title("Delete energy final demand sectors of the full USA region") %>%
      add_units("NA") %>%
      add_comments("Delete energy final demand sectors of the full USA region") %>%
      add_legacy_name("L254.DeleteFinalDemand_USAtrn") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      L254.DeleteFinalDemand_USAtrn

    L254.Supplysector_trn_USA %>%
      add_title("Supply sector information for transportation sector in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.Supplysector_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.Supplysector_trn") ->
      L254.Supplysector_trn_USA

    L254.FinalEnergyKeyword_trn_USA %>%
      add_title("Supply sector final energy keywords for transportation sector in the US states") %>%
      add_units("NA") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.FinalEnergyKeyword_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.FinalEnergyKeyword_trn") ->
      L254.FinalEnergyKeyword_trn_USA

    L254.tranSubsectorLogit_USA %>%
      add_title("Subsector logit exponents of transportation sector in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorLogit_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorLogit") ->
      L254.tranSubsectorLogit_USA

    L254.tranSubsectorShrwtFllt_USA %>%
      add_title("Subsector shareweights of transportation sector in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorShrwtFllt_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorShrwtFllt") ->
      L254.tranSubsectorShrwtFllt_USA

    L254.tranSubsectorInterp_USA %>%
      add_title("Temporal subsector shareweight interpolation of transportation sector in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorInterp_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorInterp") ->
      L254.tranSubsectorInterp_USA

    L254.tranSubsectorSpeed_USA %>%
      add_title("Speeds of transportation modes (not including pass-through sectors) in the US states") %>%
      add_units("km / hr") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorSpeed_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed") ->
      L254.tranSubsectorSpeed_USA

    L254.tranSubsectorSpeed_passthru_USA %>%
      add_title("Speeds of pass-through transportation subsectors in the US states") %>%
      add_units("km / hr") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorSpeed_passthru_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_passthru") ->
      L254.tranSubsectorSpeed_passthru_USA

    L254.tranSubsectorSpeed_noVOTT_USA %>%
      add_title("Speeds of transportation subsectors whose time value is not considered in the US states") %>%
      add_units("km / hr") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorSpeed_noVOTT_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_noVOTT") ->
      L254.tranSubsectorSpeed_noVOTT_USA

    L254.tranSubsectorSpeed_nonmotor_USA %>%
      add_title("Speeds of non-motorized transportation subsectors in the US states") %>%
      add_units("km / hr") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorSpeed_nonmotor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorSpeed_nonmotor") ->
      L254.tranSubsectorSpeed_nonmotor_USA

    L254.tranSubsectorVOTT_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorVOTT_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorVOTT") ->
      L254.tranSubsectorVOTT_USA

    L254.tranSubsectorFuelPref_USA %>%
      add_title("Subsector (fuel) preferences elasticity that are tied to GDP in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("Fuel preferences are unrelated to time value") %>%
      add_legacy_name("L254.tranSubsectorFuelPref_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.tranSubsectorFuelPref") ->
      L254.tranSubsectorFuelPref_USA

    L254.StubTranTech_USA %>%
      add_title("Transportation stub technologies in the US states") %>%
      add_units("NA") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.StubTranTech_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTech") ->
      L254.StubTranTech_USA

    L254.StubTranTech_passthru_USA %>%
      add_title("Transportation stub technologies for passthrough sectors in the US states") %>%
      add_units("NA") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name(" L254.StubTranTech_passthru_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTech_passthru") ->
      L254.StubTranTech_passthru_USA

    L254.StubTranTech_nonmotor_USA %>%
      add_title("Transportation stub technologies for non-motorized subsectors in the US states") %>%
      add_units("NA") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.StubTranTech_nonmotor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTech_nonmotor") ->
      L254.StubTranTech_nonmotor_USA

    L254.StubTranTechLoadFactor_USA %>%
      add_title("Load factors of transportation stub technologies in the US states") %>%
      add_units("person/vehicle and tonnes/vehicle") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.StubTranTechLoadFactor_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechLoadFactor") ->
      L254.StubTranTechLoadFactor_USA

    L254.StubTranTechCost_USA %>%
      add_title("Costs of transportation stub technologies in the US states") %>%
      add_units("$1990USD / vkm") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.StubTranTechCost_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechCost") ->
      L254.StubTranTechCost_USA

    L254.StubTranTechCoef_USA %>%
      add_title("Coefficients of transportation stub technologies in the US states") %>%
      add_units("BTU / vkm") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("Re-set electricity consumed at the state markets") %>%
      add_legacy_name("L254.StubTranTechCoef_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.StubTranTechCoef") ->
      L254.StubTranTechCoef_USA

    L254.PerCapitaBased_trn_USA %>%
      add_title("Per-capita based flag for transportation final demand in the US states") %>%
      add_units("NA") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.PerCapitaBased_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.PerCapitaBased_trn") ->
      L254.PerCapitaBased_trn_USA

    L254.PriceElasticity_trn_USA %>%
      add_title("Price elasticity of transportation final demand in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.PriceElasticity_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.PriceElasticity_trn") ->
      L254.PriceElasticity_trn_USA

    L254.IncomeElasticity_trn_USA %>%
      add_title("Income elasticity of transportation final demand in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L254.IncomeElasticity_trn_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L254.IncomeElasticity_trn") ->
      L254.IncomeElasticity_trn_USA

    L254.StubTranTechCalInput_USA %>%
      add_title("Calibrated energy consumption by all transportation stub technologies in the US states") %>%
      add_units("EJ") %>%
      add_comments("Set calibration values for those technologies that do not exist in some base years as zero") %>%
      add_legacy_name("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      add_precursors("L154.in_EJ_state_trn_m_sz_tech_F",
                     "energy/mappings/UCD_techs",
                     "energy/mappings/UCD_techs_revised") ->
      L254.StubTranTechCalInput_USA

    L254.StubTranTechProd_nonmotor_USA %>%
      add_title("Calibrated service output of non-motorized transportation technologies in the US states") %>%
      add_units("Million pass-km") %>%
      add_comments("Not match shareweights to the calOutputValue because no region should ever have a zero here") %>%
      add_legacy_name("L254.StubTranTechProd_nonmotor_USA") %>%
      add_precursors("L154.out_mpkm_state_trn_nonmotor_Yh",
                     "energy/A54.globaltech_nonmotor") ->
      L254.StubTranTechProd_nonmotor_USA

    L254.StubTranTechCalInput_passthru_USA %>%
      add_title("Calibrated energy consumption of transportation passthrough technologies in the US states") %>%
      add_units("EJ") %>%
      add_comments("Use outputs of the supplysectors that are inputs for passthrough technologies") %>%
      add_comments("Outputs of all motorized technologies are calculated as calInput * loadFactor / coefficient") %>%
      add_legacy_name("L254.StubTranTechCalInput_passthru_USA") %>%
      same_precursors_as("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      same_precursors_as("L254.StubTranTech_passthru_USA") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.StubTranTechCalInput_passthru_USA

    L254.BaseService_trn_USA %>%
      add_title("Base-year service output of transportation final demand") %>%
      add_units("Million pass-km and million ton-km") %>%
      add_comments("Service outputs of all motorized technologies are calculated as calInput * loadFactor / coefficient") %>%
      add_comments("Combine with service output of non-motorized transportation technologies") %>%
      add_legacy_name("L254.BaseService_trn_USA") %>%
      same_precursors_as("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") %>%
      same_precursors_as("L254.StubTranTechProd_nonmotor_USA") %>%
      add_precursors("energy/A54.sector",
                     "energy/mappings/UCD_size_class_revisions") ->
      L254.BaseService_trn_USA

    L254.StubTranTechOutput_USA %>%
      add_title("service output for all tranTechnologies") %>%
      add_units("Million pass-km and million ton-km") %>%
      add_comments("Service outputs of all motorized technologies are calculated as calInput * loadFactor / coefficient") %>%
      add_legacy_name("L254.StubTranTechOutput_USA") %>%
      same_precursors_as("L254.StubTranTechCalInput_USA") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_USA") %>%
      same_precursors_as("L254.StubTranTechCoef_USA") ->
      L254.StubTranTechOutput_USA

    return_data(L254.DeleteSupplysector_USAtrn, L254.DeleteFinalDemand_USAtrn,
                L254.Supplysector_trn_USA,
                L254.FinalEnergyKeyword_trn_USA,
                L254.tranSubsectorLogit_USA,
                L254.tranSubsectorShrwtFllt_USA,
                L254.tranSubsectorInterp_USA,
                L254.tranSubsectorSpeed_USA,
                L254.tranSubsectorSpeed_passthru_USA,
                L254.tranSubsectorSpeed_noVOTT_USA,
                L254.tranSubsectorSpeed_nonmotor_USA,
                L254.tranSubsectorVOTT_USA,
                L254.tranSubsectorFuelPref_USA,
                L254.StubTranTech_USA,
                L254.StubTranTech_passthru_USA,
                L254.StubTranTech_nonmotor_USA,
                L254.StubTranTechLoadFactor_USA,
                L254.StubTranTechCost_USA,
                L254.StubTranTechCoef_USA,
                L254.PerCapitaBased_trn_USA,
                L254.PriceElasticity_trn_USA,
                L254.IncomeElasticity_trn_USA,
                L254.StubTranTechCalInput_USA, L254.StubTranTechProd_nonmotor_USA,
                L254.StubTranTechCalInput_passthru_USA, L254.BaseService_trn_USA,
                L254.StubTranTechOutput_USA)
  } else {
    stop("Unknown command")
  }
}
