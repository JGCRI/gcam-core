#' module_emissions_L251.en_ssp_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L251.ctrl.delete}, \code{L251.ssp15_ef}, \code{L251.ssp2_ef}, \code{L251.ssp34_ef}, \code{L251.ssp15_ef_vin}, \code{L251.ssp2_ef_vin}, \code{L251.ssp34_ef_vin}. The corresponding file in the
#' original data system was \code{L251.en_ssp_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L251.en_ssp_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "common/GCAM_region_names",
             FILE = "temp-data-inject/L161.SSP2_EF",
             FILE = "temp-data-inject/L161.SSP15_EF",
             FILE = "temp-data-inject/L161.SSP34_EF",
             FILE = "temp-data-inject/L201.nonghg_steepness"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L251.ctrl.delete",
             "L251.ssp15_ef",
             "L251.ssp2_ef",
             "L251.ssp34_ef",
             "L251.ssp15_ef_vin",
             "L251.ssp2_ef_vin",
             "L251.ssp34_ef_vin"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data(all_data, "common/GCAM_region_names") ->
      GCAM_region_names
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
    get_data(all_data, "temp-data-inject/L201.nonghg_steepness") ->
      L201.nonghg_steepness

    # ===================================================

# This section takes in the Emissions factors for SSP 1.5, 2, and 3/4 across sectors.
# Output tables have the emission coefficients for non-CO2 emissions (*ef.csv)
# and non-CO2 emission coefficient for the historical control and future periods (*ef_vin.csv).

    L161.SSP15_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
      fill = list(value = NA)) %>%
      mutate(year = as.integer(year))  %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value))  %>%
      # Now add regional data
      left_join_error_no_match(A_regions, by = c("GCAM_region_ID")) %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
    L251.ssp15_ef

    # Then data is reordered to match old.
    L251.ssp15_ef <- L251.ssp15_ef[c(6,3:5,1:2,7)]

    L161.SSP2_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
                      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
                      fill = list(value = NA)) %>%
      mutate(year = as.integer(year))  %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value))  %>%
      # Now add regional data
      left_join_error_no_match(A_regions, by = c("GCAM_region_ID"))  %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp2_ef

    # Then data is reordered to match old.
    L251.ssp2_ef <- L251.ssp2_ef[c(6,3:5,1:2,7)]

    L161.SSP34_EF %>%
      # Input data only contains 3 future years.  Need to include years between 2010-2100 in 5 year segments,
      # and interpolate between each time segment.
      tidyr::complete(year = SSP_FUTURE_YEARS,
      tidyr::nesting(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector),
      fill = list(value = NA)) %>%
      mutate(year = as.integer(year))  %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, agg_sector) %>%
      mutate(value = approx_fun(year, value))  %>%
      # Now add regional data
      left_join_error_no_match(A_regions, by = c("GCAM_region_ID"))  %>%
      # Emission coefficients values are too long, so we round to the 10th decimal point.
      mutate(emiss.coeff = round(value, 10)) %>%
      ungroup() %>%
      # Discard columns that are not needed.
      select(-MAC_region, -bio_N2O_coef, -SO2_name, -GAINS_region, -GCAM_region_ID, -agg_sector, -value) ->
      L251.ssp34_ef

    # Then data is reordered to match old.
    L251.ssp34_ef <- L251.ssp34_ef[c(6,3:5,1:2,7)]

# +++++++++++++++++++++++++++++++
     L251.ssp2_ef %>%
      select(-emiss.coeff) %>%
      subset(year == min(year)) %>%
      mutate(year = 1975,
             ctrl.name ="GDP_control") ->
      L251.ctrl.delete

     L251.ssp15_ef %>%
       subset(supplysector == "electricity") %>%
       mutate(future.emiss.coeff.year = year,
              future.emiss.coeff.name = "SSP_GAINS",
              year = 1975)->
       L251.ssp15_ef_vin

     L251.ssp2_ef %>%
       subset(supplysector == "electricity") %>%
       mutate(future.emiss.coeff.year = year,
              future.emiss.coeff.name = "SSP_GAINS",
              year = 1975)->
       L251.ssp2_ef_vin

     L251.ssp34_ef %>%
       subset(supplysector == "electricity") %>%
       mutate(future.emiss.coeff.year = year,
              future.emiss.coeff.name = "SSP_GAINS",
              year = 1975)->
       L251.ssp34_ef_vin

# ++++++++++++++
# ++++++++++++++
     L251.ctrl.delete %>%
       semi_join(L201.nonghg_steepness,
                 by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) ->
       L251.ctrl.delete

# ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    #   L251.ctrl.delete %>%
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ctrl.delete") %>%
      add_precursors("temp-data-inject/L161.SSP2_EF","temp-data-inject/L201.nonghg_steepness","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ctrl.delete
    #   L251.ssp15_ef %>%
    tibble()%>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp15_ef") %>%
      add_precursors("temp-data-inject/L161.SSP15_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp15_ef
    #   L251.ssp2_ef %>%
    tibble()%>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp2_ef") %>%
      add_precursors("temp-data-inject/L161.SSP2_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp2_ef
    #    L251.ssp34_ef %>%
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp34_ef") %>%
      add_precursors("temp-data-inject/L161.SSP34_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp34_ef
    #    L251.ssp15_ef_vin %>%
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp15_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP15_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp15_ef_vin
    #   L251.ssp2_ef_vin %>%
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp2_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP2_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp2_ef_vin
    #   L251.ssp34_ef_vin %>%
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L251.ssp34_ef_vin") %>%
      add_precursors("temp-data-inject/L161.SSP34_EF","emissions/A_regions") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L251.ssp34_ef_vin

    return_data(L251.ctrl.delete, L251.ssp15_ef, L251.ssp2_ef, L251.ssp34_ef, L251.ssp15_ef_vin, L251.ssp2_ef_vin, L251.ssp34_ef_vin)
  } else {
    stop("Unknown command")
  }
}
