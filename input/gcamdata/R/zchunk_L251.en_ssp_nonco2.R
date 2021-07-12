# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L251.en_ssp_nonco2
#'
#' Produce regional non-CO2 emissions coefficient data for SSPs 1/5, 2, and 3/4 as well as a GDP control.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L251.ctrl.delete}, \code{L251.ssp15_ef}, \code{L251.ssp2_ef}, \code{L251.ssp34_ef}, \code{L251.ssp15_ef_vin}, \code{L251.ssp2_ef_vin}, \code{L251.ssp34_ef_vin}. The corresponding file in the
#' original data system was \code{L251.en_ssp_nonco2.R} (emissions level2).
#' @details This section takes in the non-CO2 emissions factors for SSP 1/5, 2, and 3/4 across sectors.
#' First, create data that spans the years 2010-2100 in five year increments by interpolation of input data.
#' Next, add emissions controls for future years of vintaged technologies for SSP emission factors.
#' Then, add columns that have regional SO2 emission species.
#' A GDP control of regional non-CO2 emissions in all regions is also created.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select semi_join
#' @author CDL May 2017
module_emissions_L251.en_ssp_nonco2 <- function(command, ...) {
  UCD_tech_map_name <- if_else(energy.TRAN_UCD_MODE == 'rev.mode', "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_techs")
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             # the following files to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/calibrated_techs",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = UCD_tech_map_name,
             "L161.SSP2_EF",
             "L161.SSP15_EF",
             "L161.SSP34_EF",
             "L201.nonghg_steepness",
             "L223.GlobalTechEff_elec"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L251.ctrl.delete",
             "L251.ssp15_ef",
             "L251.ssp2_ef",
             "L251.ssp34_ef",
             "L251.ssp15_ef_elec",
             "L251.ssp2_ef_elec",
             "L251.ssp34_ef_elec",
             "L251.ssp15_ef_vin",
             "L251.ssp2_ef_vin",
             "L251.ssp34_ef_vin"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- Non.CO2 <- supplysector <- subsector <-
      stub.technology <- agg_sector <- MAC_region <- bio_N2O_coef <- future.emiss.coeff.year <-
      SO2_name <- GAINS_region <- emiss.coeff <- technology <- minicam.energy.input <-
      tranSubsector <- tranTechnology <- input.name <- efficiency <-
      future.emiss.coeff.year <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data(all_data, "emissions/A_regions") ->
      A_regions
    get_data(all_data, "L161.SSP2_EF") ->
      L161.SSP2_EF
    get_data(all_data, "L161.SSP15_EF") ->
      L161.SSP15_EF
    get_data(all_data, "L161.SSP34_EF") ->
      L161.SSP34_EF
    get_data(all_data, "L201.nonghg_steepness") -> L201.nonghg_steepness
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")

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

    # ===================================================

    # For GCAM SSP scenarios air pollution emission factors are read in explicitly for every year.
    # This section takes in the non-CO2 emissions factors for SSP 1/5, 2, and 3/4 across sectors and
    # interpolates across years 2010-2100 in 5 year segments.
    # The air pollution controls in SSPs are specified in 3 groupings: by 1/5, 2, and 3/4 (Rao et al, 2017).

    GAINS_years <- unique(L161.SSP15_EF$year)

    L161.SSP15_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = unique(c(GAINS_years, emissions.SSP_FUTURE_YEARS)),
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year)) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID") %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS)) %>%
      ungroup() %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      mutate(emiss.coeff= if_else(is.na(emiss.coeff),0,emiss.coeff)) %>%
      # Discard columns that are not needed.  Resulting table retains 7 columns.
      # Columns include "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", and "emiss.coeff".
      # This applies to the next 2 tables as well.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp15_ef

    L161.SSP2_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = unique(c(GAINS_years, emissions.SSP_FUTURE_YEARS)),
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      #There are some sector country combinations especially for BCOC where we don't have data. Output these as 0 so as to avoid NA values in the xml.
      mutate(year = as.integer(year)) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID") %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS)) %>%
      ungroup() %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      mutate(emiss.coeff= if_else(is.na(emiss.coeff),0,emiss.coeff)) %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp2_ef

    L161.SSP34_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = unique(c(GAINS_years, emissions.SSP_FUTURE_YEARS)),
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year)) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID") %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS)) %>%
      ungroup() %>%
      left_join_error_no_match(EnTechInputNameMap, by = c("supplysector", "subsector", "stub.technology")) %>%
      mutate(emiss.coeff= if_else(is.na(emiss.coeff),0,emiss.coeff)) %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp34_ef

    # This section deletes the default GCAM default GDP control functions, so they can be replaced with
    # explicit emission factors by year.

    # Create a control table where year 2010 is the control period and renamed 1975, discarding all other years.
    L251.ssp2_ef %>%
      select(-emiss.coeff) %>%
      filter(year == min(year)) %>%
      mutate(year = emissions.GHG_CONTROL_READIN_YEAR,
             ctrl.name = "GDP_control") ->
      L251.ctrl.delete

    # Convert electricty to use output-driver instead.  We do this, despite the addional hoops, because it makes it
    # easier to swap out a different structure for electricity which requires pass-through technologies such as to
    # add cooling technologies
    # We need to be careful with the processing here as we need to adjust the input coef according to the fuel IO-ceofficient
    # which will change over time.  We can get that data from L223.GlobalTechEff_elec
    L251.ssp15_ef %>%
      filter(supplysector == "electricity") %>%
      left_join_error_no_match(L223.GlobalTechEff_elec, by = c("supplysector" = "sector.name",
                                                               "subsector" = "subsector.name",
                                                               "stub.technology" = "technology",
                                                               "year")) %>%
      mutate(emiss.coeff = round(emiss.coeff / efficiency, emissions.DIGITS_EMISS_COEF)) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]]) ->
      L251.ssp15_ef_elec
    L251.ssp15_ef <- filter(L251.ssp15_ef, supplysector != "electricity")
    L251.ssp2_ef %>%
      filter(supplysector == "electricity") %>%
      left_join_error_no_match(L223.GlobalTechEff_elec, by = c("supplysector" = "sector.name",
                                                               "subsector" = "subsector.name",
                                                               "stub.technology" = "technology",
                                                               "year")) %>%
      mutate(emiss.coeff = round(emiss.coeff / efficiency, emissions.DIGITS_EMISS_COEF)) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]]) ->
      L251.ssp2_ef_elec
    L251.ssp2_ef <- filter(L251.ssp2_ef, supplysector != "electricity")
    L251.ssp34_ef %>%
      filter(supplysector == "electricity") %>%
      left_join_error_no_match(L223.GlobalTechEff_elec, by = c("supplysector" = "sector.name",
                                                               "subsector" = "subsector.name",
                                                               "stub.technology" = "technology",
                                                               "year")) %>%
      mutate(emiss.coeff = round(emiss.coeff / efficiency, emissions.DIGITS_EMISS_COEF)) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]]) ->
      L251.ssp34_ef_elec
    L251.ssp34_ef <- filter(L251.ssp34_ef, supplysector != "electricity")

    # This section adds emissions controls for future years of vintaged electricity technologies for SSP emission factors.
    # They need to be read in seperatly from the *ef.csv files because they have a different csv to xml header.
    L251.ssp15_ef_elec %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = emissions.GHG_CONTROL_READIN_YEAR)->
      L251.ssp15_ef_vin

    L251.ssp2_ef_elec %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = emissions.GHG_CONTROL_READIN_YEAR)->
      L251.ssp2_ef_vin

    L251.ssp34_ef_elec %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = emissions.GHG_CONTROL_READIN_YEAR)->
      L251.ssp34_ef_vin

    # This section renames SO2 variables so that it has regional SO2 emission species.
    L251.ctrl.delete <- rename_SO2(L251.ctrl.delete, A_regions, FALSE)
    L251.ssp15_ef <- rename_SO2(L251.ssp15_ef, A_regions, FALSE)
    L251.ssp2_ef <- rename_SO2(L251.ssp2_ef, A_regions, FALSE)
    L251.ssp34_ef <- rename_SO2(L251.ssp34_ef, A_regions, FALSE)
    L251.ssp15_ef_elec <- rename_SO2(L251.ssp15_ef_elec, A_regions, FALSE)
    L251.ssp2_ef_elec <- rename_SO2(L251.ssp2_ef_elec, A_regions, FALSE)
    L251.ssp34_ef_elec <- rename_SO2(L251.ssp34_ef_elec, A_regions, FALSE)
    L251.ssp15_ef_vin <- rename_SO2(L251.ssp15_ef_vin, A_regions, FALSE)
    L251.ssp2_ef_vin <- rename_SO2(L251.ssp2_ef_vin, A_regions, FALSE)
    L251.ssp34_ef_vin <- rename_SO2(L251.ssp34_ef_vin, A_regions, FALSE)

    # This section removes historical years (e.g., 2010) to ensure consistency with the CORE.
    L251.ssp15_ef <- L251.ssp15_ef %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp2_ef <- L251.ssp2_ef %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp34_ef <- L251.ssp34_ef %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp15_ef_elec <- L251.ssp15_ef_elec %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp2_ef_elec <- L251.ssp2_ef_elec %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp34_ef_elec <- L251.ssp34_ef_elec %>% filter(year %in% MODEL_FUTURE_YEARS)
    L251.ssp15_ef_vin <- L251.ssp15_ef_vin %>% filter(future.emiss.coeff.year %in% MODEL_FUTURE_YEARS)
    L251.ssp2_ef_vin <- L251.ssp2_ef_vin %>% filter(future.emiss.coeff.year %in% MODEL_FUTURE_YEARS)
    L251.ssp34_ef_vin <- L251.ssp34_ef_vin %>% filter(future.emiss.coeff.year %in% MODEL_FUTURE_YEARS)

    # This section performs a filtering join that discards rows that do not have a SSP emission GDP control steepness value.
    L251.ctrl.delete %>%
      semi_join(L201.nonghg_steepness,
                by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) ->
      L251.ctrl.delete

    # ===================================================
    # Produce outputs
    # No flags are necessary because old data is in 'long' format.

    L251.ctrl.delete %>%
      add_title("Delete GDP control of regional non-CO2 emissions.") %>%
      add_units("unitless") %>%
      add_comments("First, year 2010 is used as the default year, all other years are deleted.") %>%
      add_comments("Then, created a new column for data that has regional non-CO2 emission species.") %>%
      add_comments("Finally, delete GDP control functions that exist.") %>%
      add_legacy_name("L251.ctrl.delete") %>%
      add_precursors("L161.SSP2_EF",
                     "L201.nonghg_steepness",
                     "emissions/A_regions") ->
      L251.ctrl.delete
    L251.ssp15_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP1 and SSP5.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 1/5 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp15_ef") %>%
      add_precursors("L161.SSP15_EF",
                     "emissions/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name) ->
      L251.ssp15_ef
    L251.ssp2_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP2.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 2 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp2_ef") %>%
      add_precursors("L161.SSP2_EF",
                     "emissions/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name) ->
      L251.ssp2_ef
    L251.ssp34_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP3 and SSP4.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 3/4 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp34_ef") %>%
      add_precursors("L161.SSP34_EF",
                     "emissions/A_regions",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     UCD_tech_map_name) ->
      L251.ssp34_ef
    L251.ssp15_ef_elec %>%
      add_title("Regional electricity sector non-CO2 emissions coefficient data for SSP1 and SSP5.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 1/5 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_comments("We've seperated electricity out to be driven by output-driver so we") %>%
      add_comments("more easily re-configure the strucutre of the sector to swap in cooling") %>%
      add_comments("technology choice which is implemented with pass-through sector/tech") %>%
      add_precursors("L161.SSP15_EF",
                     "emissions/A_regions",
                     "L223.GlobalTechEff_elec") ->
      L251.ssp15_ef_elec
    L251.ssp2_ef_elec %>%
      add_title("Regional electricity sector non-CO2 emissions coefficient data for SSP2.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 2 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_comments("We've seperated electricity out to be driven by output-driver so we") %>%
      add_comments("more easily re-configure the strucutre of the sector to swap in cooling") %>%
      add_comments("technology choice which is implemented with pass-through sector/tech") %>%
      add_precursors("L161.SSP2_EF",
                     "emissions/A_regions",
                     "L223.GlobalTechEff_elec") ->
      L251.ssp2_ef_elec
    L251.ssp34_ef_elec %>%
      add_title("Regional electricity sector non-CO2 emissions coefficient data for SSP3 and SSP4.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 3/4 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_comments("We've seperated electricity out to be driven by output-driver so we") %>%
      add_comments("more easily re-configure the strucutre of the sector to swap in cooling") %>%
      add_comments("technology choice which is implemented with pass-through sector/tech") %>%
      add_precursors("L161.SSP34_EF",
                     "emissions/A_regions",
                     "L223.GlobalTechEff_elec") ->
      L251.ssp34_ef_elec
    L251.ssp15_ef_vin %>%
      add_title("Regional SO2 emissions coefficient data of vintaged electric technologies for SSP1 and SSP5.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 1/5 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp15_ef_vin") %>%
      add_precursors("L161.SSP15_EF",
                     "emissions/A_regions") ->
      L251.ssp15_ef_vin
    L251.ssp2_ef_vin %>%
      add_title("Regional non-CO2 emissions coefficient data of vintaged electric technologies for SSP 2.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 2 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp2_ef_vin") %>%
      add_precursors("L161.SSP2_EF",
                     "emissions/A_regions") ->
      L251.ssp2_ef_vin
    L251.ssp34_ef_vin %>%
      add_title("Regional non-CO2 emissions coefficient data of vintaged electric technologies for SSP3 and SSP4.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 3/4 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp34_ef_vin") %>%
      add_precursors("L161.SSP34_EF",
                     "emissions/A_regions") ->
      L251.ssp34_ef_vin

    return_data(L251.ctrl.delete, L251.ssp15_ef, L251.ssp2_ef, L251.ssp34_ef, L251.ssp15_ef_elec, L251.ssp2_ef_elec, L251.ssp34_ef_elec, L251.ssp15_ef_vin, L251.ssp2_ef_vin, L251.ssp34_ef_vin)
  } else {
    stop("Unknown command")
  }
}
