#' module_emissions_L251.en_ssp_nonco2
#'
#' This chunk produces regional non-CO2 emissions coefficient data for SSPs 1/5, 2, and 3/4 as well as a GDP control.
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
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL May 2017
module_emissions_L251.en_ssp_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "temp-data-inject/L161.SSP2_EF",
             FILE = "temp-data-inject/L161.SSP15_EF",
             FILE = "temp-data-inject/L161.SSP34_EF",
             "L201.nonghg_steepness"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L251.ctrl.delete",
             "L251.ssp15_ef",
             "L251.ssp2_ef",
             "L251.ssp34_ef",
             "L251.ssp15_ef_vin",
             "L251.ssp2_ef_vin",
             "L251.ssp34_ef_vin"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- Non.CO2 <- supplysector <- subsector <-
      stub.technology <- agg_sector <- MAC_region <- bio_N2O_coef <-
      SO2_name <- GAINS_region <- emiss.coeff <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data(all_data, "emissions/A_regions") ->
      A_regions
    get_data(all_data, "temp-data-inject/L161.SSP2_EF") %>%
      gather(year, value, -GCAM_region_ID, -Non.CO2, -supplysector, -subsector, -stub.technology, -agg_sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.SSP2_EF
    get_data(all_data, "temp-data-inject/L161.SSP15_EF") %>%
      gather(year, value, -GCAM_region_ID, -Non.CO2, -supplysector, -subsector, -stub.technology, -agg_sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.SSP15_EF
    get_data(all_data, "temp-data-inject/L161.SSP34_EF")  %>%
      gather(year, value, -GCAM_region_ID, -Non.CO2, -supplysector, -subsector, -stub.technology, -agg_sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.SSP34_EF
    get_data(all_data, "L201.nonghg_steepness") -> L201.nonghg_steepness

    # ===================================================

    # For GCAM SSP scenarios air pollution emission factors are read in explicitly for every year.
    # This section takes in the non-CO2 emissions factors for SSP 1/5, 2, and 3/4 across sectors and
    # interpolates across years 2010-2100 in 5 year segments.
    # The air pollution controls in SSPs are specified in 3 groupings: by 1/5, 2, and 3/4 (Rao et al, 2017).

    L161.SSP15_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year)) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID") %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.  Resulting table retains 7 columns.
      # Columns include "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", and "emiss.coeff".
      # This applies to the next 2 tables as well.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp15_ef

    L161.SSP2_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year))  %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value))  %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID")  %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp2_ef

    L161.SSP34_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year))  %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value)) %>%
      # Now add regional information.
      left_join_error_no_match(A_regions, by = "GCAM_region_ID") %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp34_ef

    # This section deletes the default GCAM default GDP control functions, so they can be replaced with
    # explicit emission factors by year.

    # Create a control table where year 2010 is the control period and renamed 1975, discarding all other years.
    L251.ssp2_ef %>%
      select(-emiss.coeff) %>%
      filter(year == min(year)) %>%
      mutate(year = GHG_CONTROL_READIN_YEAR,
             ctrl.name ="GDP_control") ->
      L251.ctrl.delete

    # This section adds emissions controls for future years of vintaged electricity technologies for SSP emission factors.
    # They need to be read in seperatly from the *ef.csv files because they have a different csv to xml header.
    L251.ssp15_ef %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = GHG_CONTROL_READIN_YEAR)->
      L251.ssp15_ef_vin

    L251.ssp2_ef %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = GHG_CONTROL_READIN_YEAR)->
      L251.ssp2_ef_vin

    L251.ssp34_ef %>%
      # Isolate the "electricity" supply sector.
      filter(supplysector == "electricity") %>%
      # Create 2 new columns for future emission factors.
      # Future emission coefficient year is based on pre-existing "year" column.
      # Future emission coefficient name is a descriptor, and is constant.
      mutate(future.emiss.coeff.year = year,
             future.emiss.coeff.name = "SSP_GAINS",
             # Previous year column that only includes the electricity supply sector is now a constant.
             year = GHG_CONTROL_READIN_YEAR)->
      L251.ssp34_ef_vin

    # This section renames SO2 variables so that it has regional SO2 emission species.
    L251.ctrl.delete <- rename_SO2(L251.ctrl.delete, A_regions, FALSE)
    L251.ssp15_ef <- rename_SO2(L251.ssp15_ef, A_regions, FALSE)
    L251.ssp2_ef <- rename_SO2(L251.ssp2_ef, A_regions, FALSE)
    L251.ssp34_ef <- rename_SO2(L251.ssp34_ef, A_regions, FALSE)
    L251.ssp15_ef_vin <- rename_SO2(L251.ssp15_ef_vin, A_regions, FALSE)
    L251.ssp2_ef_vin <- rename_SO2(L251.ssp2_ef_vin, A_regions, FALSE)
    L251.ssp34_ef_vin <- rename_SO2(L251.ssp34_ef_vin, A_regions, FALSE)

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
      add_precursors("temp-data-inject/L161.SSP2_EF",
                     "L201.nonghg_steepness",
                     "emissions/A_regions") ->
      L251.ctrl.delete
    L251.ssp15_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP1 and SSP5.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 1/5 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp15_ef") %>%
      add_precursors("temp-data-inject/L161.SSP15_EF",
                     "emissions/A_regions") ->
      L251.ssp15_ef
    L251.ssp2_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP2.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 2 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp2_ef") %>%
      add_precursors("temp-data-inject/L161.SSP2_EF",
                     "emissions/A_regions") ->
      L251.ssp2_ef
    L251.ssp34_ef %>%
      add_title("Regional non-CO2 emissions coefficient data for SSP3 and SSP4.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 3/4 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp34_ef") %>%
      add_precursors("temp-data-inject/L161.SSP34_EF",
                     "emissions/A_regions") ->
      L251.ssp34_ef
    L251.ssp15_ef_vin %>%
      add_title("Regional SO2 emissions coefficient data of vintaged electric technologies for SSP1 and SSP5.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 1/5 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp15_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP15_EF",
                     "emissions/A_regions") ->
      L251.ssp15_ef_vin
    L251.ssp2_ef_vin %>%
      add_title("Regional non-CO2 emissions coefficient data of vintaged electric technologies for SSP 2.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 2 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp2_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP2_EF",
                     "emissions/A_regions") ->
      L251.ssp2_ef_vin
    L251.ssp34_ef_vin %>%
      add_title("Regional non-CO2 emissions coefficient data of vintaged electric technologies for SSP3 and SSP4.") %>%
      add_units("Tg / EJ") %>%
      add_comments("First, the non-CO2 emissions factors for SSP 3/4 are interpolated across years 2010-2100 in 5 year segments.") %>%
      add_comments("Then, emissions controls are added for future years of vintaged technologies for SSP emission factors.") %>%
      add_comments("Finally, regional non-CO2 emission species information is added.") %>%
      add_legacy_name("L251.ssp34_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP34_EF",
                     "emissions/A_regions") ->
      L251.ssp34_ef_vin

    return_data(L251.ctrl.delete, L251.ssp15_ef, L251.ssp2_ef, L251.ssp34_ef, L251.ssp15_ef_vin, L251.ssp2_ef_vin, L251.ssp34_ef_vin)
  } else {
    stop("Unknown command")
  }
}
