#' module_gcamindia_L232.industry
#'
#' Prepare level 2 industry sector files for india.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.india_state_DeleteSupplysector_ind}, \code{L2321.india_state_DeleteSupplysector_cement}, \code{L232.india_state_DeleteFinalDemand_ind},
#' \code{L2321.india_state_DeleteFinalDemand_cement}, \code{L232.india_state_StubTechCalInput_indenergy}, \code{L232.india_state_StubTechCalInput_indfeed}, \code{L232.india_state_StubTechProd_industry},
#' \code{L232.india_state_StubTechCoef_industry}, \code{L232.india_state_StubTechMarket_ind}, \code{L232.india_state_StubTechSecMarket_ind},
#' \code{L232.india_state_BaseService_ind}, \code{L232.india_state_Supplysector_ind}, \code{L232.india_state_FinalEnergyKeyword_ind},
#' \code{L232.india_state_SubsectorLogit_ind}, \code{L232.india_state_SubsectorShrwtFllt_ind}, \code{L232.india_state_SubsectorInterp_ind},
#' \code{L232.india_state_StubTech_ind}, \code{L232.india_state_StubTechInterp_ind}, \code{L232.india_state_PerCapitaBased_ind},
#' \code{L232.india_state_PriceElasticity_ind}, \code{L232.india_state_IncomeElasticity_ind_gcam3}.
#' The corresponding file in the original data system was \code{L232.industry_india.R} (gcam-india level2).
#' @details Prepare level 2 industry sector files for india.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST October 2017
module_gcamindia_L232.industry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "gcam-india/A32.india_state_demand",
             FILE = "gcam-india/A32.india_state_industry_tech_eff",
             FILE = "gcam-india/A32.india_state_calibrated_techs",
             "L232.Supplysector_ind",
             "L2321.Supplysector_cement",
             "L232.StubTech_ind",
             "L232.PerCapitaBased_ind",
             "L2321.PerCapitaBased_cement",
             "L132.india_state_in_EJ_indnochp_F",
             "L132.india_state_in_EJ_indfeed_F",
             "L132.india_state_in_EJ_indchp_F",
             "L232.FinalEnergyKeyword_ind",
             "L232.SubsectorLogit_ind",
             "L232.SubsectorShrwtFllt_ind",
             "L232.SubsectorInterp_ind",
             "L232.StubTech_ind",
             "L232.StubTechInterp_ind",
             "L232.PriceElasticity_ind",
             "L232.IncomeElasticity_ind_gcam3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.india_state_DeleteSupplysector_ind",
             "L232.india_state_DeleteFinalDemand_ind",
             "L2321.india_state_DeleteSupplysector_cement",
             "L2321.india_state_DeleteFinalDemand_cement",
             "L232.india_state_StubTechCalInput_indenergy",
             "L232.india_state_StubTechCalInput_indfeed",
             "L232.india_state_StubTechProd_industry",
             "L232.india_state_StubTechCoef_industry",
             "L232.india_state_StubTechMarket_ind",
             "L232.india_state_StubTechSecMarket_ind",
             "L232.india_state_BaseService_ind",
             "L232.india_state_Supplysector_ind",
             "L232.india_state_FinalEnergyKeyword_ind",
             "L232.india_state_SubsectorLogit_ind",
             "L232.india_state_SubsectorShrwtFllt_ind",
             "L232.india_state_SubsectorInterp_ind",
             "L232.india_state_StubTech_ind",
             "L232.india_state_StubTechInterp_ind",
             "L232.india_state_PerCapitaBased_ind",
             "L232.india_state_PriceElasticity_ind",
             "L232.india_state_IncomeElasticity_ind_gcam3"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- value <- output_tot <- grid_region <- market.name <-
      calOutputValue <- calibrated.value <- calibration <- technology <-
      efficiency <- fuel <- minicam.energy.input <- object <-
      output_tot <- region <- secondary.output <- sector <- state <-
      subs.share.weight <- subsector <- supplysector <- value <- x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    A32.india_state_demand <- get_data(all_data, "gcam-india/A32.india_state_demand")
    A32.india_state_industry_tech_eff <- get_data(all_data, "gcam-india/A32.india_state_industry_tech_eff")
    A32.india_state_calibrated_techs <- get_data(all_data, "gcam-india/A32.india_state_calibrated_techs")
    L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind")
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement")
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
    L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind")
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement")
    L132.india_state_in_EJ_indnochp_F <- get_data(all_data, "L132.india_state_in_EJ_indnochp_F")
    L132.india_state_in_EJ_indfeed_F <- get_data(all_data, "L132.india_state_in_EJ_indfeed_F")
    L132.india_state_in_EJ_indchp_F <- get_data(all_data, "L132.india_state_in_EJ_indchp_F")
    L232.FinalEnergyKeyword_ind <- get_data(all_data, "L232.FinalEnergyKeyword_ind")
    L232.SubsectorLogit_ind <- get_data(all_data, "L232.SubsectorLogit_ind")
    L232.SubsectorShrwtFllt_ind <- get_data(all_data, "L232.SubsectorShrwtFllt_ind")
    L232.SubsectorInterp_ind <- get_data(all_data, "L232.SubsectorInterp_ind")
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
    L232.StubTechInterp_ind <- get_data(all_data, "L232.StubTechInterp_ind")
    L232.PriceElasticity_ind <- get_data(all_data, "L232.PriceElasticity_ind")
    L232.IncomeElasticity_ind_gcam3 <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3")


    # ===================================================
    # Data Processing

    # convert to long form
    A32.india_state_industry_tech_eff <- A32.india_state_industry_tech_eff %>%
      gather_years

    # delete industry sectors in the india region (energy-final-demands and supplysectors)
    L232.Supplysector_ind %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]]) ->
      L232.india_state_DeleteSupplysector_ind  ## OUTPUT

    # deleting energy final industry demand sectors in the full india region
    L232.PerCapitaBased_ind %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteFinalDemand"]]) ->
      L232.india_state_DeleteFinalDemand_ind  ## OUTPUT


    # Create a table that will be used remove the cement supply sector input table for india.
    L2321.Supplysector_cement %>%
      filter(region == gcam.india_REGION) %>%
      select(region, supplysector) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.india_state_DeleteSupplysector_cement

    # Create the table that will be used to remove the cement sector information from the
    # energy.final.demand input table for india.
    L2321.PerCapitaBased_cement %>%
      filter(region == gcam.india_REGION) %>%
      select(region, energy.final.demand) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.india_state_DeleteFinalDemand_cement



    # The industry_india_processing function is used in place of a for loop in the old data sytem.
    # This function checks to see if the input data needs to be expanded to all states or used as
    # is.

    # industry_india_processing: is a function that
    industry_india_processing <- function(data) {

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

    L232.india_state_Supplysector_ind <- industry_india_processing(L232.Supplysector_ind)
    L232.india_state_FinalEnergyKeyword_ind <- industry_india_processing(L232.FinalEnergyKeyword_ind)
    L232.india_state_SubsectorLogit_ind <- industry_india_processing(L232.SubsectorLogit_ind)
    L232.india_state_SubsectorShrwtFllt_ind <- industry_india_processing(L232.SubsectorShrwtFllt_ind)
    L232.india_state_SubsectorInterp_ind <- industry_india_processing(L232.SubsectorInterp_ind)
    L232.india_state_StubTech_ind <- industry_india_processing(L232.StubTech_ind)
    L232.india_state_StubTechInterp_ind <- industry_india_processing(L232.StubTechInterp_ind)
    L232.india_state_PerCapitaBased_ind <- industry_india_processing(L232.PerCapitaBased_ind)
    L232.india_state_PriceElasticity_ind <- industry_india_processing(L232.PriceElasticity_ind)
    L232.india_state_IncomeElasticity_ind_gcam3 <- industry_india_processing(L232.IncomeElasticity_ind_gcam3)

    # get calibrated input of industrial energy use technologies, including cogen
    L232.in_EJ_state_indenergy_F_Yh <- L132.india_state_in_EJ_indnochp_F %>%
      bind_rows(L132.india_state_in_EJ_indchp_F) %>%
      complete(nesting(state, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(state, sector, fuel) %>% mutate(value = approx_fun(year, value)) %>%
      ungroup %>% filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = state) %>%
      left_join_keep_first_only(A32.india_state_calibrated_techs, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology)

    ## OUTPUT
    L232.india_state_StubTechCalInput_indenergy  <- L232.in_EJ_state_indenergy_F_Yh %>% select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join_keep_first_only(select(A32.india_state_industry_tech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(x = sum(calibrated.value),
             subs.share.weight = if_else(x > 0, 1, 0)) %>% ungroup %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])



    # get calibrated input of industrial feedstock technologies
    L232.in_EJ_state_indfeed_F_Yh <- L132.india_state_in_EJ_indfeed_F %>%
      complete(nesting(state, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(state, sector, fuel) %>% mutate(value = approx_fun(year, value)) %>%
      ungroup %>% filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = state) %>%
      left_join_keep_first_only(A32.india_state_calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) %>%
      rename(stub.technology = technology)


    L232.in_EJ_state_indfeed_F_Yh %>% select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join_keep_first_only(select(A32.india_state_industry_tech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>% mutate(x = sum(calibrated.value)) %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      mutate(subs.share.weight = if_else(x > 0, 1, 0)) %>% ungroup %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L232.india_state_StubTechCalInput_indfeed  ## OUTPUT

    # get industrial sector calibrated output
    A32.india_state_industry_tech_eff_interp <- A32.india_state_industry_tech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(value = approx_fun(year, value)) %>% ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(efficiency = value) %>%
      mutate(efficiency = round(efficiency, energy.DIGITS_EFFICIENCY))


    # ^^ service output, by technology, for energy-use and feedstocks
    L232.out_EJ_state_ind_serv_F_Yh <- L232.in_EJ_state_indenergy_F_Yh %>%
      bind_rows(L232.in_EJ_state_indfeed_F_Yh) %>%
      left_join_keep_first_only(select(A32.india_state_industry_tech_eff_interp, supplysector, subsector, technology, year, efficiency),
                                by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT))


    L232.india_state_StubTechProd_industry <- L232.out_EJ_state_ind_serv_F_Yh %>%
      group_by(region, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>% ungroup %>%
      # ^^ aggregate to get output of industrial sector in each region
      mutate(supplysector = "industry",
             subsector = "industry",
             stub.technology = "industry",
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])


    # get calibrated output of industrial sector
    L232.out_EJ_state_ind_serv_F_Yh %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>% ungroup %>%
      # ^^ aggregate service output by sector
      left_join_keep_first_only(L232.india_state_StubTechProd_industry %>%
                                  rename(output_tot = calOutputValue) %>% select(region, year, output_tot),
                                by = c("region", "year")) %>%
      mutate(coefficient = round(calOutputValue / output_tot, energy.DIGITS_COEFFICIENT)) %>%
      # ^^ get coefficient
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = "industry",
             subsector = "industry",
             stub.technology = "industry",
             market.name = region) ->
      L232.india_state_StubTechCoef_industry_base
    # ^^ covers only base years

    L232.india_state_StubTechCoef_industry_base %>%
      filter(year == max(MODEL_BASE_YEARS)) %>% select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L232.india_state_StubTechCoef_industry_fut
    # ^^ future years copied from final base year
    # note: this is not typical, but is suitable here as no energy:feedstock evolution in the industrial...
    # ... sector is expected for india

    bind_rows(L232.india_state_StubTechCoef_industry_base, L232.india_state_StubTechCoef_industry_fut) %>%
      select(LEVEL2_DATA_NAMES$StubTechCoef) ->
      L232.india_state_StubTechCoef_industry  ## OUTPUT

    # Get markets for fuels consumed by the state industrial sectors
    L232.india_state_StubTechMarket_ind  <- L232.StubTech_ind %>% filter(region == gcam.india_REGION) %>% select(-region) %>%
      write_to_all_india_states(names = c(names(L232.StubTech_ind), "region")) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_keep_first_only(A32.india_state_industry_tech_eff %>% select(supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      filter(is.na(minicam.energy.input) == FALSE) %>%
      # ^^ includes generic industrial technology that is not required here...
      mutate(market.name = gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      left_join_error_no_match(india_states_subregions %>% select(state, grid_region), by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS,
                                   grid_region, market.name)) %>%
      select(-grid_region) %>%
      mutate(market.name = if_else(grepl("elect_td", minicam.energy.input), region, market.name))


    # markets for the cogenerated electricity (secondary output)
    A32.india_state_industry_tech_eff %>%
      filter(is.na(secondary.output) == FALSE) %>%
      select(supplysector, subsector, technology) ->
      L232.chp_techs

    L232.india_state_StubTechMarket_ind %>%
      # ^^ electricity is consumed from state markets
      semi_join(L232.chp_techs, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # ^^ filters for rows contained in L232.chp_techs
      mutate(secondary.output = "electricity") %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "secondary.output", "market.name") %>%
      mutate(market.name = gcam.india_REGION) %>%
      # ^^ over-ride regional market names
      left_join_error_no_match(india_states_subregions %>%
                                 select(state, grid_region),
                               by = c("region" = "state")) %>%
      mutate(market.name = grid_region) %>%
      select(-grid_region) ->
      L232.india_state_StubTechSecMarket_ind  ## OUTPUT

    A32.india_state_industry_demand<-A32.india_state_demand %>%
      filter(energy.final.demand == "industry")

       # base-year service output of industry final demand, base service is equal to the output of the industry supplysector
    L232.india_state_BaseService_ind <- L232.india_state_StubTechProd_industry %>%
      select(region, year, calOutputValue) %>%
      rename(base.service = calOutputValue) %>%
      mutate(energy.final.demand = A32.india_state_industry_demand$energy.final.demand)



    # ===================================================
    # Produce outputs

    L232.india_state_DeleteSupplysector_ind %>%
      add_title("india industry supply sectors") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting industry sectors from input") %>%
      add_legacy_name("L232.india_state_DeleteSupplysector_ind") %>%
      add_precursors("L232.Supplysector_ind") ->
      L232.india_state_DeleteSupplysector_ind

    L232.india_state_DeleteFinalDemand_ind %>%
      add_title("india final energy demand table for industry") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting final demand sectors") %>%
      add_legacy_name("L232.india_state_DeleteFinalDemand_ind") %>%
      add_precursors("L232.PerCapitaBased_ind") ->
      L232.india_state_DeleteFinalDemand_ind

    L2321.india_state_DeleteSupplysector_cement %>%
      add_title("Cement sector information to remove from supplysectors input table") %>%
      add_units("NA") %>%
      add_comments("Cement supply sector information for region india") %>%
      add_legacy_name("L2321.india_state_DeleteSupplysector_cement") %>%
      add_precursors("L2321.Supplysector_cement") ->
      L2321.india_state_DeleteSupplysector_cement

    L2321.india_state_DeleteFinalDemand_cement %>%
      add_title("Cement sector information to remove from energy.final.demand input table") %>%
      add_units("NA") %>%
      add_comments("Cement sector information from the energy.final.demand for region india ") %>%
      add_legacy_name("L2321.india_state_DeleteFinalDemand_cement") %>%
      add_precursors("L2321.PerCapitaBased_cement") ->
      L2321.india_state_DeleteFinalDemand_cement

    L232.india_state_StubTechCalInput_indenergy %>%
      add_title("calibrated input of industrial energy use technologies (including cogen)") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.india_state_StubTechCalInput_indenergy") %>%
      add_precursors("L132.india_state_in_EJ_indnochp_F",
                     "L132.india_state_in_EJ_indchp_F",
                     "gcam-india/A32.india_state_calibrated_techs",
                     "gcam-india/A32.india_state_industry_tech_eff") ->
      L232.india_state_StubTechCalInput_indenergy

    L232.india_state_StubTechCalInput_indfeed %>%
      add_title("Calibrated input of industrial feedstock technologies") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.india_state_StubTechCalInput_indfeed") %>%
      add_precursors("L132.india_state_in_EJ_indfeed_F",
                     "gcam-india/A32.india_state_calibrated_techs",
                     "gcam-india/A32.india_state_industry_tech_eff") ->
      L232.india_state_StubTechCalInput_indfeed

    L232.india_state_StubTechProd_industry %>%
      add_title("industrial sector output") %>%
      add_units("Unitless") %>%
      add_comments("Service output aggregated to industrial sector for each region") %>%
      add_legacy_name("L232.india_state_StubTechProd_industry") %>%
      add_precursors("gcam-india/A32.india_state_industry_tech_eff",
                     "L132.india_state_in_EJ_indnochp_F",
                     "L132.india_state_in_EJ_indfeed_F") ->
      L232.india_state_StubTechProd_industry

    L232.india_state_StubTechCoef_industry %>%
      add_title("industrial sector calibrated output") %>%
      add_units("Unitless") %>%
      add_comments("Generated by bind base and future year coefficients") %>%
      add_legacy_name("L232.india_state_StubTechCoef_industry") %>%
      add_precursors("gcam-india/A32.india_state_industry_tech_eff",
                     "L132.india_state_in_EJ_indnochp_F",
                     "L132.india_state_in_EJ_indfeed_F") ->
      L232.india_state_StubTechCoef_industry

    L232.india_state_StubTechMarket_ind %>%
      add_title("Markets for the fuels consumed by the state industrial sectors") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L232.india_state_StubTechMarket_ind") %>%
      add_precursors("L232.StubTech_ind",
                     "gcam-india/A32.india_state_industry_tech_eff",
                     "gcam-india/india_states_subregions") ->
      L232.india_state_StubTechMarket_ind

    L232.india_state_StubTechSecMarket_ind %>%
      add_title("markets for the cogenerated electricity (secondary output)") %>%
      add_units("NA") %>%
      add_comments("derived from L232.india_state_StubTechMarket_ind") %>%
      add_legacy_name("L232.india_state_StubTechSecMarket_ind") %>%
      add_precursors("L232.StubTech_ind",
                     "gcam-india/A32.india_state_industry_tech_eff",
                     "gcam-india/india_states_subregions") ->
      L232.india_state_StubTechSecMarket_ind

    L232.india_state_BaseService_ind %>%
      add_title("base-year service output of industry final demand") %>%
      add_units("NA") %>%
      add_comments("base service is equal to the output of the industry supplysector") %>%
      add_legacy_name("L232.india_state_BaseService_ind") %>%
      add_precursors("gcam-india/A32.india_state_industry_tech_eff",
                     "L132.india_state_in_EJ_indnochp_F",
                     "L132.india_state_in_EJ_indfeed_F",
                     "gcam-india/A32.india_state_demand") ->
      L232.india_state_BaseService_ind

    L232.india_state_Supplysector_ind %>%
      add_title("Supply sector information for industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A32.sector is expanded for gcam-india") %>%
      add_legacy_name("L232.india_state_Supplysector_ind") %>%
      add_precursors("L232.Supplysector_ind") ->
      L232.india_state_Supplysector_ind

    L232.india_state_FinalEnergyKeyword_ind %>%
      add_title("Supply sector keywords for industry sector") %>%
      add_units("NA") %>%
      add_comments("Set supply sector keywords for industry sector for all gcam-india regions") %>%
      add_legacy_name("L232.india_state_FinalEnergyKeyword_ind") %>%
      add_precursors("L232.FinalEnergyKeyword_ind") ->
      L232.india_state_FinalEnergyKeyword_ind

    L232.india_state_SubsectorLogit_ind %>%
      add_title("Subsector logit exponents of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector logit exponents from A32.subsector_logit are expanded into all gcam-india regions with non-existent heat subsectors removed") %>%
      add_legacy_name("L232.india_state_SubsectorLogit_ind") %>%
      add_precursors("L232.SubsectorLogit_ind") ->
      L232.india_state_SubsectorLogit_ind

    L232.india_state_SubsectorShrwtFllt_ind %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt are expanded into all gcam-india regions with non-existent heat technologies") %>%
      add_legacy_name("L232.india_state_SubsectorShrwtFllt_ind") %>%
      add_precursors("L232.SubsectorShrwtFllt_ind") ->
      L232.india_state_SubsectorShrwtFllt_ind

    L232.india_state_SubsectorInterp_ind %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp is expanded into all gcam-india regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.india_state_SubsectorInterp_ind") %>%
      add_precursors("L232.SubsectorInterp_ind") ->
      L232.india_state_SubsectorInterp_ind

    L232.india_state_StubTech_ind %>%
      add_title("Identification of stub technologies of industrial sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the stub technologies from A32.globaltech_shrwt are expanded into all gcam-india regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.india_state_StubTech_ind") %>%
      add_precursors("L232.StubTech_ind") ->
      L232.india_state_StubTech_ind

    L232.india_state_StubTechInterp_ind %>%
      add_title("Shareweight interpolation of global industrial sector technologies") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the interpolation function from A32.globaltech_interp are expanded into all GCAM regions") %>%
      add_legacy_name("L232.india_state_StubTechInterp_ind") %>%
      add_precursors("L232.StubTechInterp_ind") ->
      L232.india_state_StubTechInterp_ind

    L232.india_state_PerCapitaBased_ind %>%
      add_title("Per-capita based flag for industry final demand") %>%
      add_units("NA") %>%
      add_comments("Extracted per-capita based flag for industry final demand from A32.india_state_demand") %>%
      add_legacy_name("L232.india_state_PerCapitaBased_ind") %>%
      add_precursors("L232.PerCapitaBased_ind") ->
      L232.india_state_PerCapitaBased_ind

    L232.india_state_PriceElasticity_ind %>%
      add_title("Price elasticity of industry final demand") %>%
      add_units("Unitless") %>%
      add_comments("Extracted price elasticity of industry final demand from A32.india_state_demand") %>%
      add_comments("Price elasticities are only applied to future periods. Application in base years will cause solution failure") %>%
      add_legacy_name("L232.india_state_PriceElasticity_ind") %>%
      add_precursors("L232.PriceElasticity_ind") ->
      L232.india_state_PriceElasticity_ind

    L232.india_state_IncomeElasticity_ind_gcam3 %>%
      add_title("Income elasticity of industry - GCAM3") %>%
      add_units("Unitless") %>%
      add_comments("First calculate industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity") %>%
      add_comments("Then back out the appropriate income elasticities from industrial output") %>%
      add_comments("Note lower income elasticities for SSP1 are hard-coded.") %>%
      add_legacy_name("L232.india_state_IncomeElasticity_ind_gcam3") %>%
      add_precursors("L232.IncomeElasticity_ind_gcam3") ->
      L232.india_state_IncomeElasticity_ind_gcam3


    return_data(L232.india_state_DeleteSupplysector_ind,
                L2321.india_state_DeleteSupplysector_cement,
                L232.india_state_DeleteFinalDemand_ind,
                L2321.india_state_DeleteFinalDemand_cement,
                L232.india_state_StubTechCalInput_indenergy,
                L232.india_state_StubTechCalInput_indfeed,
                L232.india_state_StubTechProd_industry,
                L232.india_state_StubTechCoef_industry,
                L232.india_state_StubTechMarket_ind,
                L232.india_state_StubTechSecMarket_ind,
                L232.india_state_BaseService_ind,
                L232.india_state_Supplysector_ind,
                L232.india_state_FinalEnergyKeyword_ind,
                L232.india_state_SubsectorLogit_ind,
                L232.india_state_SubsectorShrwtFllt_ind,
                L232.india_state_SubsectorInterp_ind,
                L232.india_state_StubTech_ind,
                L232.india_state_StubTechInterp_ind,
                L232.india_state_PerCapitaBased_ind,
                L232.india_state_PriceElasticity_ind,
                L232.india_state_IncomeElasticity_ind_gcam3)
  } else {
    stop("Unknown command")
  }
}
