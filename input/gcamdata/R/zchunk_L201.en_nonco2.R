# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L201.en_nonco2
#'
#' Generate non-CO2 emissions: pollutants, GHGs, non-CO2, BC/OCs, and reduction data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.en_pol_emissions}, \code{L201.en_ghg_emissions}, \code{L201.en_bcoc_emissions}, \code{L201.OutputEmissions_elec}, \code{L201.nonghg_max_reduction}, \code{L201.nonghg_steepness}, \code{L201.nonghg_max_reduction_res}, \code{L201.nonghg_steepness_res}, \code{L201.nonghg_res}, \code{L201.ghg_res}, \code{L201.ResReadInControl_nonghg_res}, \code{L201.ResReadInControl_ghg_res}. The corresponding file in the
#' original data system was \code{L201.en_nonco2.R} (emissions level2).
#' @details Set up all of the inputs needed for the energy system non-CO2 emissions in GCAM.
#' This includes historical emissions, drivers (input or output), and pollution controls.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join left_join mutate pull select
#' @importFrom tidyr gather
#' @author BBL July 2017
module_emissions_L201.en_nonco2 <- function(command, ...) {
  UCD_tech_map_name <- if_else(energy.TRAN_UCD_MODE == 'rev.mode', "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_techs")
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "energy/A_regions",
             "L111.nonghg_tg_R_en_S_F_Yh",
             "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L112.ghg_tg_R_en_S_F_Yh",
             "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L114.bcoc_tgej_R_en_S_F_2000",
             "L151.nonghg_ctrl_R_en_S_T",
             FILE = "emissions/A51.steepness",
             "L244.DeleteThermalService",
             # the following to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/calibrated_techs",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = UCD_tech_map_name))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.en_pol_emissions",
             "L201.en_ghg_emissions",
             "L201.en_bcoc_emissions",
             "L201.OutputEmissions_elec",
             "L201.nonghg_max_reduction",
             "L201.nonghg_steepness",
             "L201.nonghg_max_reduction_res",
             "L201.nonghg_steepness_res",
             "L201.nonghg_res",
             "L201.ghg_res",
             "L201.ResReadInControl_nonghg_res",
             "L201.ResReadInControl_ghg_res"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- supplysector <- region <- subsector <- stub.technology <- Non.CO2 <-
      input.emissions <- `2000` <- emiss.coef <- ctrl.name <- max_reduction <- variable <-
      steepness <- SO2 <- NOx <- CO <- BC <- OC <- NMVOC <- resource <- has_district_heat <-
      . <- region <- supplysector <- max.reduction <- technology <- minicam.energy.input <-
      tranSubsector <- tranTechnology <- input.name <- efficiency <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    A_regions.en <- get_data(all_data, "energy/A_regions")

    L111.nonghg_tg_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tg_R_en_S_F_Yh", strip_attributes = TRUE)
    L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP", strip_attributes = TRUE)
    L112.ghg_tg_R_en_S_F_Yh <- get_data(all_data, "L112.ghg_tg_R_en_S_F_Yh", strip_attributes = TRUE)
    L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP <- get_data(all_data, "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP", strip_attributes = TRUE)
    L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000", strip_attributes = TRUE)
    L151.nonghg_ctrl_R_en_S_T <- get_data(all_data, "L151.nonghg_ctrl_R_en_S_T", strip_attributes = TRUE)
    A51.steepness <- get_data(all_data, "emissions/A51.steepness", strip_attributes = TRUE)
    L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService", strip_attributes = TRUE)

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver
    bind_rows(
      get_data(all_data, "energy/calibrated_techs") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, "energy/calibrated_techs_bld_det") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, UCD_tech_map_name) %>% select(supplysector, subsector = tranSubsector, technology = tranTechnology, minicam.energy.input)
      ) %>%
      rename(stub.technology = technology,
             input.name = minicam.energy.input) %>%
      distinct() ->
      EnTechInputNameMap

    # L201.en_pol_emissions: Pollutant emissions for energy technologies in all regions
    L111.nonghg_tg_R_en_S_F_Yh %>%
      filter(supplysector != "out_resources",
             year %in% emissions.MODEL_BASE_YEARS) %>%
      # add region name and round output
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      select(region, supplysector, subsector, stub.technology, year, input.emissions = value, Non.CO2, input.name) %>%
      mutate(input.emissions = signif(input.emissions, emissions.DIGITS_EMISSIONS)) ->
      L201.en_pol_emissions

    # L201.en_ghg_emissions: GHG emissions for energy technologies in all regions
    L112.ghg_tg_R_en_S_F_Yh %>%
      filter(supplysector != "out_resources",
             year %in% emissions.MODEL_BASE_YEARS) %>%
      # add region name and round output
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      select(region, supplysector, subsector, stub.technology, year, input.emissions = value, Non.CO2, input.name) %>%
      mutate(input.emissions = signif(input.emissions, emissions.DIGITS_EMISSIONS)) ->
      L201.en_ghg_emissions

    # L201.en_bcoc_emissions: BC/OC emissions factors for energy technologies in all regions
    L114.bcoc_tgej_R_en_S_F_2000 %>%
      filter(supplysector != "out_resources") %>%
      # add region name, extend emissions factors across all base years, and round output
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      select(region, supplysector, subsector, stub.technology, year, emiss.coef = `2000`, Non.CO2, input.name) %>%
      mutate(emiss.coef = signif(emiss.coef, emissions.DIGITS_EMISSIONS)) ->
      L201.en_bcoc_emissions

    # Convert electricity to use output-driver instead.  We do this, despite the addional hoops, because it makes it
    # easier to swap out a different structure for electricity which requires pass-through technologies such as to
    # add cooling technologies

    # L201.OutputEmissions_elec: processing is straightforward here, just move electricity rows out of the InputEmissions
    # table into the OutputEmissions
    bind_rows(filter(L201.en_pol_emissions, supplysector == "electricity"),
              filter(L201.en_ghg_emissions, supplysector == "electricity")) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissions"]]) ->
      L201.OutputEmissions_elec
    L201.en_pol_emissions <- filter(L201.en_pol_emissions, supplysector != "electricity")
    L201.en_ghg_emissions <- filter(L201.en_ghg_emissions, supplysector != "electricity")
    L201.en_bcoc_emissions <- filter(L201.en_bcoc_emissions, supplysector != "electricity")


    # L201.nonghg_max_reduction: maximum reduction for energy technologies in all regions
    L151.nonghg_ctrl_R_en_S_T %>%
      filter(supplysector != "out_resources") %>%
      # add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-year)->L201.max_reduction


    L201.max_reduction %>%
      # select only certain columns in preparation for join below
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      mutate(year = emissions.CTRL_BASE_YEAR, ctrl.name = "GDP_control") %>%
      left_join_error_no_match(L201.max_reduction,
                               by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      na.omit %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, ctrl.name, max_reduction) ->
      L201.nonghg_max_reduction

    # L201.nonghg_steepness: steepness of reduction for energy technologies in all regions
    A51.steepness %>%
      gather(variable, steepness, SO2, NOx, CO, BC, OC, NMVOC) %>%
      filter(supplysector != "out_resources") %>%
      # extend steepness factors across all regions
      repeat_add_columns(tibble(region = GCAM_region_names$region)) ->
      L201.steepness

    L201.nonghg_max_reduction %>%
      mutate(year = emissions.CTRL_BASE_YEAR, ctrl.name = "GDP_control") %>%
      left_join(L201.steepness,
                by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2" = "variable")) %>%
      na.omit %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, ctrl.name, steepness) ->
      L201.nonghg_steepness

    # Remove rows where we only have a value for one of max.reduction or steepness
    # TODO: is this what we want or should we raise an error? [from original code]
    L201.nonghg_max_reduction %>%
      full_join(L201.nonghg_steepness,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name")) %>%
      na.omit %>%
      # No need to include a GDP control when the max.reduction is zero
      filter(max_reduction > 0) ->
      L201.nonghg_gdp_control

    L201.nonghg_max_reduction <- select(L201.nonghg_gdp_control, -steepness)
    L201.nonghg_steepness <- select(L201.nonghg_gdp_control, -max_reduction)

    # L201.nonghg_res: Pollutant emissions for energy resources in all regions
    L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      filter(supplysector == "out_resources",
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(resource = subsector) %>%
      rename(subresource = subsector, technology = stub.technology, emiss.coef = value) %>%
      select(LEVEL2_DATA_NAMES[["ResEmissCoef"]]) %>%
      mutate(emiss.coef = signif(emiss.coef, emissions.DIGITS_EMISSIONS)) ->
      L201.nonghg_res

    # L201.ghg_res: GHG emissions from resource production in all regions
    L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      filter(supplysector == "out_resources",
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(resource = subsector) %>%
      rename(subresource = subsector, technology = stub.technology, emiss.coef = value) %>%
      select(LEVEL2_DATA_NAMES[["ResEmissCoef"]]) %>%
      mutate(emiss.coef = signif(emiss.coef, emissions.DIGITS_EMISSIONS)) ->
      L201.ghg_res

    # Resources have vintaging going on in the historical years.
    # The above emissions coefficients are the coefficients across vintages
    # in the given model year.  The best way to ensure the correct "total"
    # emissions factor across vintages is realized is to use the ReadInControl
    # to change the coefficients by vintage for all vintages.  Note given the way
    # the C++ operates we need to read in the "base" EmissCoef table and then the
    # same values in the ReadInControl table all read into the first model period
    # vintage.  We will also need to "fillout" the value in the final calibration
    # to the future model periods otherwise the vintage would revert back to the
    # value in the EmissCoef table.  Finally to turn "off" any adjustments made to
    # new vintages in future model periods we must have ReadInControl with values of
    # zero starting in the first future model period.

    # L201.ResReadInControl_nonghg_res: Vintage adjustments for Pollutant emissions for energy resources in all regions
    L201.nonghg_res %>%
      # copy the final historical year value to the future model periods
      bind_rows(L201.nonghg_res %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))) %>%
      rename(future.emiss.coeff.year = year) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             future.emiss.coeff.name = "vintage_adjust") %>%
      select(LEVEL2_DATA_NAMES[["ResReadInControl"]]) ->
      L201.ResReadInControl_nonghg_res
    # turn "off" vintage adjustments for future year vintages
    L201.ResReadInControl_nonghg_res %>%
      mutate(year = MODEL_FUTURE_YEARS[1],
             emiss.coef = 0.0) %>%
      bind_rows(L201.ResReadInControl_nonghg_res) ->
      L201.ResReadInControl_nonghg_res

    # L201.ResReadInControl_ghg_res: Vintage adjustments for GHG emissions from resource production in all regions
    L201.ghg_res %>%
      # copy the final historical year value to the future model periods
      bind_rows(L201.ghg_res %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))) %>%
      rename(future.emiss.coeff.year = year) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             future.emiss.coeff.name = "vintage_adjust") %>%
      select(LEVEL2_DATA_NAMES[["ResReadInControl"]]) ->
      L201.ResReadInControl_ghg_res
    # turn "off" vintage adjustments for future year vintages
    L201.ResReadInControl_ghg_res %>%
      mutate(year = MODEL_FUTURE_YEARS[1],
             emiss.coef = 0.0) %>%
      bind_rows(L201.ResReadInControl_ghg_res) ->
      L201.ResReadInControl_ghg_res


    # L201.nonghg_max_reduction_res: maximum reduction for resources in all regions
    L151.nonghg_ctrl_R_en_S_T %>%
      filter(supplysector == "out_resources") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(resource = subsector,
             year = emissions.GHG_CONTROL_READIN_YEAR,
             ctrl.name = "GDP_control") %>%
      rename(subresource = subsector, technology = stub.technology, max.reduction = max_reduction) %>%
      select(LEVEL2_DATA_NAMES[["GDPCtrlMaxRes"]]) ->
      L201.nonghg_max_reduction_res

    # L201.nonghg_steepness_res: steepness of reduction for resources in all regions
    A51.steepness %>%
      gather(Non.CO2, value, SO2, NOx, CO, BC, OC, NMVOC) %>%
      filter(supplysector == "out_resources") %>%
      # extend steepness factors across all regions
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      mutate(resource = subsector,
             year = emissions.GHG_CONTROL_READIN_YEAR,
             ctrl.name = "GDP_control") %>%
      rename(subresource = subsector, technology = stub.technology, steepness = value) %>%
      select(LEVEL2_DATA_NAMES[["GDPCtrlSteepRes"]]) ->
      L201.nonghg_steepness_res

    # Remove rows where we only have a value for one of max.reduction or steepness
    # TODO: is this what we want or should we raise an error?
    L201.nonghg_max_reduction_res %>%
      full_join(L201.nonghg_steepness_res, by = c("region", "resource", "subresource", "technology", "year", "Non.CO2", "ctrl.name")) %>%
      na.omit %>%
      # No need to include a GDP control when the max.reduction is zero
      filter(max.reduction > 0) ->
      L201.nonghg_gdp_control_res

    L201.nonghg_max_reduction_res <- select(L201.nonghg_gdp_control_res, -steepness)
    L201.nonghg_steepness_res <- select(L201.nonghg_gdp_control_res, -max.reduction)

    # Rename to regional SO2
    L201.en_pol_emissions <- rename_SO2(L201.en_pol_emissions, A_regions, FALSE)
    L201.OutputEmissions_elec <- rename_SO2(L201.OutputEmissions_elec, A_regions, FALSE)
    L201.nonghg_max_reduction <- rename_SO2(L201.nonghg_max_reduction, A_regions, FALSE)
    L201.nonghg_steepness <- rename_SO2(L201.nonghg_steepness, A_regions, FALSE)
    L201.nonghg_res <- rename_SO2(L201.nonghg_res, A_regions, FALSE)
    L201.ResReadInControl_nonghg_res <- rename_SO2(L201.ResReadInControl_nonghg_res, A_regions, FALSE)
    L201.nonghg_steepness_res <- rename_SO2(L201.nonghg_steepness_res, A_regions, FALSE)
    L201.nonghg_max_reduction_res <- rename_SO2(L201.nonghg_max_reduction_res, A_regions, FALSE)

    # Remove district heat from regions that do have have it
    A_regions.en %>%
      filter(has_district_heat == 1) %>%
      pull(region) ->
      distheat.regions
    L201.en_pol_emissions <- filter(L201.en_pol_emissions, supplysector != "district heat" | region %in% distheat.regions)
    L201.en_ghg_emissions <- filter(L201.en_ghg_emissions, supplysector != "district heat" | region %in% distheat.regions)
    L201.en_bcoc_emissions <- filter(L201.en_bcoc_emissions, supplysector != "district heat" | region %in% distheat.regions)
    L201.nonghg_max_reduction <- filter(L201.nonghg_max_reduction, supplysector != "district heat" | region %in% distheat.regions)
    L201.nonghg_steepness <- filter(L201.nonghg_steepness, supplysector != "district heat" | region %in% distheat.regions)

    # It may be the case with certain regional aggregations that regions exist that have no
    # heating or cooling sectors. We should delete those here.
    # Delete sectors that do not exist due to zero heating/cooling degree days
    delete_nonexistent_sectors <- function(x, L201.delete.sectors) {
      filter(x, ! paste0(region, supplysector) %in% L201.delete.sectors)
    }
    L201.delete.sectors <- paste0(L244.DeleteThermalService$region, L244.DeleteThermalService$supplysector)
    L201.en_pol_emissions <- delete_nonexistent_sectors(L201.en_pol_emissions, L201.delete.sectors)
    L201.en_ghg_emissions <- delete_nonexistent_sectors(L201.en_ghg_emissions, L201.delete.sectors)
    L201.en_bcoc_emissions <- delete_nonexistent_sectors(L201.en_bcoc_emissions, L201.delete.sectors)
    L201.nonghg_max_reduction <- delete_nonexistent_sectors(L201.nonghg_max_reduction, L201.delete.sectors)
    L201.nonghg_steepness <- delete_nonexistent_sectors(L201.nonghg_steepness, L201.delete.sectors)


    # Produce outputs
    L201.en_pol_emissions %>%
      add_title("Pollutant emissions for energy technologies in all regions") %>%
      add_units("Tg") %>%
      add_comments("Take non-GHG emissions for the energy system, filter out resources") %>%
      add_comments("in model base years, rename to regional SO2, filter to district heat, ") %>%
      add_comments("and delete sectors with zero heating and zero cooling degree days.") %>%
      add_legacy_name("L201.en_pol_emissions") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions", "energy/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name,
                     "L111.nonghg_tg_R_en_S_F_Yh",
                     "L244.DeleteThermalService") ->
      L201.en_pol_emissions

    L201.en_ghg_emissions %>%
      add_title("GHG emissions for energy technologies in all regions") %>%
      add_units("Tg") %>%
      add_comments("Take GHG emissions for the energy system, filter out resources") %>%
      add_comments("in model base years, rename to regional SO2, filter to district heat, ") %>%
      add_comments("and delete sectors with zero heating and zero cooling degree days.") %>%
      add_legacy_name("L201.en_ghg_emissions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name,
                     "L112.ghg_tg_R_en_S_F_Yh",
                     "L244.DeleteThermalService") ->
      L201.en_ghg_emissions

    L201.en_bcoc_emissions %>%
      add_title("BC/OC emissions factors for energy technologies in all regions") %>%
      add_units("Tg/EJ") %>%
      add_comments("Take BC/OC emissions factors for energy technologies, filter out resources") %>%
      add_comments("in model base years, rename to regional SO2, filter to district heat, ") %>%
      add_comments("and delete sectors with zero heating and zero cooling degree days.") %>%
      add_legacy_name("L201.en_bcoc_emissions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name,
                     "L114.bcoc_tgej_R_en_S_F_2000",
                     "L244.DeleteThermalService") ->
      L201.en_bcoc_emissions

    L201.OutputEmissions_elec %>%
      add_title("GHG and pollutant emissions for the electricity sector") %>%
      add_units("Tg") %>%
      add_comments("We've seperated electricity out to be driven by output-driver so we") %>%
      add_comments("more easily re-configure the strucutre of the sector to swap in cooling") %>%
      add_comments("technology choice which is implemented with pass-through sector/tech") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions", "energy/A_regions",
                     "L111.nonghg_tg_R_en_S_F_Yh") ->
      L201.OutputEmissions_elec

    L201.nonghg_max_reduction %>%
      rename(max.reduction = max_reduction) %>% # no idea why old data system renamed this
      add_title("Maximum emissions reduction rates for energy technologies in all regions") %>%
      add_units("%") %>%
      add_comments("The maximum reduction is calculated in L151 to match the maximum emissions controls assumed in GCAM3.") %>%
      add_comments("Controls are removed when the maximum reduction is zero.") %>%
      add_legacy_name("L201.nonghg_max_reduction") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions", "energy/A_regions",
                     "L151.nonghg_ctrl_R_en_S_T",
                     "L244.DeleteThermalService") ->
      L201.nonghg_max_reduction

    L201.nonghg_steepness %>%
      add_title("Steepness of emissions reduction for energy technologies in all regions") %>%
      add_units("%") %>%
      add_comments("The steepness is from an assumptions file (A51_steepness). It was chosen to replicate the pollutant reduction rates for SO2 in GCAM3.") %>%
      add_comments("Steepness is removed for technologies with maximum reduction rates of 0.") %>%
      add_legacy_name("L201.nonghg_steepness") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions", "energy/A_regions",
                     "L151.nonghg_ctrl_R_en_S_T",
                     "emissions/A51.steepness",
                     "L244.DeleteThermalService") ->
      L201.nonghg_steepness

    L201.nonghg_max_reduction_res %>%
      add_title("Maximum reduction for resources in all regions") %>%
      add_units("%") %>%
      add_comments("The maximum reduction is calculated in L151 to match the maximum emissions controls assumed in GCAM3.") %>%
      add_comments("Controls are removed when the maximum reduction is zero.") %>%
      add_legacy_name("L201.nonghg_max_reduction_res") %>%
      same_precursors_as(L201.nonghg_max_reduction) ->
      L201.nonghg_max_reduction_res

    L201.nonghg_steepness_res %>%
      add_title("Steepness of reduction for resources in all regions") %>%
      add_units("%") %>%
      add_comments("The steepness is from an assumptions file (A51_steepness). It was chosen to replicate the pollutant reduction rates for SO2 in GCAM3.") %>%
      add_comments("Steepness is removed for technologies with maximum reduction rates of 0.") %>%
      add_legacy_name("L201.nonghg_steepness_res") %>%
      same_precursors_as(L201.nonghg_steepness) ->
      L201.nonghg_steepness_res

    L201.nonghg_res %>%
      add_title("Pollutant emissions for energy resources in all regions") %>%
      add_units("Tg/EJ") %>%
      add_comments("Take non-GHG emissions for the energy system, filter to include only resources") %>%
      add_comments("in model base years, and rename to regional SO2.") %>%
      add_legacy_name("L201.nonghg_res") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions",
                     "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP") ->
      L201.nonghg_res

    L201.ghg_res %>%
      add_title("GHG emission factors from resource production in all regions") %>%
      add_units("Tg/EJ") %>%
      add_comments("Take GHG emissions for resource production using EPA 2019 data") %>%
      add_legacy_name("L201.ghg_res") %>%
      add_precursors("common/GCAM_region_names",
                     "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP") ->
      L201.ghg_res

    L201.ResReadInControl_nonghg_res %>%
      add_title("Vintaging adjustments for pollutant emissions for energy resources") %>%
      add_units("Tg/EJ") %>%
      add_comments("Used to make per vintage adjustments to ensure overall emissions") %>%
      add_comments("factors match the ones in L201.nonghg_res in historical years") %>%
      same_precursors_as(L201.nonghg_res) ->
      L201.ResReadInControl_nonghg_res

    L201.ResReadInControl_ghg_res %>%
      add_title("Vintaging adjustments for GHG emission factors from resource production") %>%
      add_units("Tg/EJ") %>%
      add_comments("Used to make per vintage adjustments to ensure overall emissions") %>%
      add_comments("factors match the ones in L201.ghg_res in historical years") %>%
      same_precursors_as(L201.ghg_res) ->
      L201.ResReadInControl_ghg_res

    return_data(L201.en_pol_emissions, L201.en_ghg_emissions, L201.en_bcoc_emissions, L201.OutputEmissions_elec, L201.nonghg_max_reduction, L201.nonghg_steepness, L201.nonghg_max_reduction_res, L201.nonghg_steepness_res, L201.nonghg_res, L201.ghg_res, L201.ResReadInControl_nonghg_res, L201.ResReadInControl_ghg_res)
  } else {
    stop("Unknown command")
  }
}
