# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L241.en_newtech_nonco2
#'
#' Produce emission coefficient tables for model input tables related to new energy technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.nonco2_tech_coeff}, \code{L241.nonco2_max_reduction}, \code{L241.nonco2_steepness}. The corresponding file in the
#' original data system was \code{L241.en_newtech_nonco2.R} (emissions level2).
#' @details Generate input tables of non-CO2 emission coefficients.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter left_join mutate right_join select
#' @importFrom tidyr unite
#' @author KD August 2017
module_emissions_L241.en_newtech_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "energy/A_regions",
             FILE = "emissions/A41.tech_coeff",
             FILE = "emissions/A51.max_reduction",
             FILE = "emissions/A51.steepness",
             # the following files to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/A22.globaltech_input_driver",
             FILE = "energy/A23.globaltech_input_driver",
             FILE = "energy/A25.globaltech_input_driver",
             "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L223.GlobalTechEff_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.nonco2_tech_coeff",
             "L241.OutputEmissCoeff_elec",
             "L241.nonco2_max_reduction",
             "L241.nonco2_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    Emiss_A_regions <- get_data(all_data, "emissions/A_regions")
    En_A_regions <- get_data(all_data, "energy/A_regions")
    A41.tech_coeff <- get_data(all_data, "emissions/A41.tech_coeff")
    A51.max_reduction <- get_data(all_data, "emissions/A51.max_reduction")
    A51.steepness <- get_data(all_data, "emissions/A51.steepness")

    L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP")
    L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP <- get_data(all_data, "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP")
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")

    year <- value <- GCAM_region_ID <- supplysector <- subsector <- stub.technology <- Non.CO2 <-
      exception <- exception_tech <- may.be.historic <- region <- sector_tech_id <- region_eth <-
      ethanol <- region_bio <- biodiesel <- emiss.coeff <- technology <- efficiency <- NULL  # silence package check notes

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver
    bind_rows(
      get_data(all_data, "energy/A22.globaltech_input_driver"),
      get_data(all_data, "energy/A23.globaltech_input_driver"),
      get_data(all_data, "energy/A25.globaltech_input_driver")
    ) %>%
      rename(stub.technology = technology) ->
      EnTechInputMap

    # ===================================================
    # Assign new technology emission factors to all GCAM regions
    # for nonco2 emissions.
    A41.tech_coeff %>%
      gather(Non.CO2, emiss.coeff, -supplysector, -subsector, -stub.technology, -exception, -exception_tech, -may.be.historic) %>%
      repeat_add_columns(tibble(region = GCAM_region_names[["region"]])) ->
      L241.nonco2_tech_coeff

    # Select the most recent CO emission coefficients for the technologies with exceptions.
    L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year == max(year), Non.CO2 == "CO") ->
      L241.co_tgej_R_en_S_F_fy

    # Combine the new technology emission factors and the CO emission coefficients into a single data frame.
    L241.nonco2_tech_coeff %>%
      select(-emiss.coeff) %>%
      filter(exception == "CO", Non.CO2 == "CO") %>%
      # Use left_join here to pass the time shift test.
      left_join(select(L241.co_tgej_R_en_S_F_fy, -subsector),
                by = c("region", "Non.CO2", "supplysector", "exception_tech" = "stub.technology")) %>%
      select(-GCAM_region_ID, -year) %>%
      rename(emiss.coeff = value) ->
      L241.co_tech_coeff_except

    # Now select the CH4 emission coefficients for technologies with exceptions.
    L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year == max(year), Non.CO2 == "CH4") ->
      L241.ch4_tgej_R_en_S_F_fy

    # Combine the CH4 emission coefficients for technologies with exceptions into
    # a single data frame.
    L241.nonco2_tech_coeff %>%
      select(-emiss.coeff) %>%
      filter(exception == "CH4", Non.CO2 == "CH4") %>%
      # Use left_join here to pass the time shift test.
      left_join(select(L241.ch4_tgej_R_en_S_F_fy, value, region, Non.CO2, stub.technology),
                by = c("region", "Non.CO2", "exception_tech" = "stub.technology")) %>%
      rename(emiss.coeff = value) ->
      L241.ch4_tech_coeff_except

    # Now replace the missing with the CO and CH4 emission coefficients for
    # technologies with expectations and select for future years.
    L241.nonco2_tech_coeff %>%
      filter(!is.na(emiss.coeff)) %>%
      bind_rows(L241.co_tech_coeff_except, L241.ch4_tech_coeff_except) %>%
      mutate(year = min(MODEL_FUTURE_YEARS)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L241.nonco2_tech_coeff

    # Emission reduction from energy technologies.
    #
    # Assign the assumed emissions reduction from energy technologies to all regions.
    A51.max_reduction %>%
      gather(Non.CO2, value, -supplysector, -subsector, -stub.technology) %>%
      repeat_add_columns(tibble(region = GCAM_region_names[["region"]])) ->
      L241.max_reduction

    # Select the future years from the new technologies emission coefficients
    # data frame.
    L241.nonco2_tech_coeff %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      mutate(year = min(MODEL_FUTURE_YEARS),
             ctrl.name = "GDP_control") ->
      L241.nonco2_max_reduction

    # Combine the max emission reduction coefficients and future new technology
    # emission coefficients into one data frame.
    L241.nonco2_max_reduction %>%
      right_join(select(L241.max_reduction, value, region, supplysector, subsector, stub.technology, Non.CO2),
                 by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      na.omit ->
      L241.nonco2_max_reduction


    # Steepness of emission reduction
    #
    # Assign the steepness of emission reduction based on energy technologies to all regions.
    A51.steepness %>%
      gather(Non.CO2, value, -supplysector, -subsector, -stub.technology) %>%
      repeat_add_columns(tibble(region = GCAM_region_names[["region"]])) ->
      L241.steepness

    # Select closest future max reduction emission coefficients.
    L241.nonco2_max_reduction %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      mutate(ctrl.name = "GDP_control") ->
      L241.nonco2_steepness

    # Combine closet future max reduction emission coefficients with the assumed steepness
    # of emission reductions into one data frame.
    L241.steepness %>%
      select(steepness = value, region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # Use left_join here because we expect there to be NAs.
      left_join(L241.nonco2_steepness, by = c("region", "supplysector", "subsector",  "stub.technology", "Non.CO2")) %>%
      na.omit ->
      L241.nonco2_steepness

    # Rename SO2 to regional SO2.
    L241.nonco2_tech_coeff <- rename_SO2(L241.nonco2_tech_coeff, Emiss_A_regions, FALSE)
    L241.nonco2_max_reduction <- rename_SO2(L241.nonco2_max_reduction, Emiss_A_regions, FALSE)
    L241.nonco2_steepness <- rename_SO2(L241.nonco2_steepness, Emiss_A_regions, FALSE)


    # TODO: better way to handle this, probably these technologies should pull from historical data
    # Ensure only regions that have first gen biofuels get read in
    # See GitHub issue #650

    # Not all of the regions started using the new technologies, "newtech", at the same time,
    # some regions started using them in historical years whereas other regions started
    # using them in future years. For the regions that may have been using these
    # technologies in historical years replace the first model base year with the maybe
    # historical value to include the historical "newtech" nonco2 parameters in the historical data.
    #
    # First create a list of data frame of region, supply sector, technology and may.be.historic. This
    # data frame will be used to determine which "newtech" nonco2 parameters should start in historical
    # years.

    A41.tech_coeff %>%
      select(supplysector, subsector, stub.technology, may.be.historic) %>%
      na.omit %>%
      unite(sector_tech_id, supplysector, subsector, stub.technology, remove = FALSE) ->
      L241.maybe_historic

    # Start the new technology emission coefficients for the technologies & reigons that may be have
    # used in historical years in the first model base year.
    L241.nonco2_tech_coeff %>%
      unite(sector_tech_id, supplysector, subsector, stub.technology, remove = FALSE) %>%
      mutate(year = replace(year, sector_tech_id %in% L241.maybe_historic$sector_tech_id, min(MODEL_BASE_YEARS))) %>%
      select(-sector_tech_id) ->
      L241.nonco2_tech_coeff

    # Start the max emission reduction for the technologies & reigons that may be have
    # used in historical years in the first model base year.
    L241.nonco2_max_reduction %>%
      unite(sector_tech_id, supplysector, subsector, stub.technology, remove = FALSE) %>%
      mutate(year = replace(year, sector_tech_id %in% L241.maybe_historic$sector_tech_id, min(MODEL_BASE_YEARS))) %>%
      select(-sector_tech_id) ->
      L241.nonco2_max_reduction

    # Start the steepness of emission reduction coefficients for the technologies & reigons that may be have
    # used in historical years in the first model base year.
    L241.nonco2_steepness %>%
      unite(sector_tech_id, supplysector, subsector, stub.technology, remove = FALSE) %>%
      mutate(year = replace(year, sector_tech_id %in% L241.maybe_historic$sector_tech_id, min(MODEL_BASE_YEARS))) %>%
      select(-sector_tech_id) ->
      L241.nonco2_steepness

    # Create a vector of the first generation biofuel technologies this vector will be used to
    # remove non-applicable first-genbio technologies from the data frames.
    L241.firstgenbio_techs <- c("corn ethanol", "sugarbeet ethanol", "sugar cane ethanol", "biodiesel")

    # Create region ethanol type and region biodiesel type identifiers from the
    # region-specific assumptions in the energy system. These identifiers will be
    # used to remove non-applicable first-genbio technologies.
    En_A_regions %>%
      unite(col = region_eth, region, ethanol, sep = "~", remove = FALSE) %>%
      unite(col = region_bio, region, biodiesel, sep = "~") ->
      En_A_regions_biofuels

    # Create a list of the region biofuel identifiers from the pervious step.
    region_biofuels <- c(En_A_regions_biofuels$region_eth, En_A_regions_biofuels$region_bio)

    # Now remove the non-applicable first-genbio technologies from the data frames.
    L241.nonco2_tech_coeff %>%
      unite(region_bio, region, stub.technology, sep = "~", remove = FALSE) %>%
      filter(!stub.technology %in% L241.firstgenbio_techs | region_bio %in% region_biofuels) %>%
      select(-region_bio) %>%
      left_join_error_no_match(EnTechInputMap, by = c("supplysector", "subsector", "stub.technology")) ->
      L241.nonco2_tech_coeff

    # Convert electricity to use output-driver instead.  We do this, despite the addional hoops, because it makes it
    # easier to swap out a different structure for electricity which requires pass-through technologies such as to
    # add cooling technologies
    # L241.OutputEmissCoeff_elec: we need to be careful with the processing here as we need to adjust the input coef
    # according to the fuel IO-ceofficient which will change over time.  We can get that data from L223.GlobalTechEff_elec
    L241.nonco2_tech_coeff %>%
      filter(supplysector == "electricity") %>%
      left_join_error_no_match(L223.GlobalTechEff_elec,
                               by = c("supplysector" = "sector.name",
                                      "subsector" = "subsector.name",
                                      "stub.technology" = "technology",
                                      "year")) %>%
      mutate(emiss.coeff = round(emiss.coeff / efficiency, emissions.DIGITS_EMISS_COEF)) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeff"]]) ->
      L241.OutputEmissCoeff_elec
    L241.nonco2_tech_coeff <- filter(L241.nonco2_tech_coeff, supplysector != "electricity")

    L241.nonco2_max_reduction %>%
      unite(region_bio, region, stub.technology, sep = "~", remove = FALSE) %>%
      filter(!stub.technology %in% L241.firstgenbio_techs | region_bio %in% region_biofuels) %>%
      select(-region_bio) %>%
      rename(max.reduction = value) ->
      L241.nonco2_max_reduction

    L241.nonco2_steepness %>%
      unite(region_bio, region, stub.technology, sep = "~", remove = FALSE) %>%
      filter(!stub.technology %in% L241.firstgenbio_techs | region_bio %in% region_biofuels) %>%
      select(-region_bio) ->
      L241.nonco2_steepness

    # ===================================================

    # Produce outputs
    L241.nonco2_tech_coeff %>%
      add_title("Non-CO2 new technology emission coefficients by supply sector") %>%
      add_units("NA") %>%
      add_comments("Combine historical and expect emission coefficients for non-CO2 emissions for new energy technologies") %>%
      add_legacy_name("L241.nonco2_tech_coeff") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "energy/A_regions",
                     "emissions/A41.tech_coeff",
                     "energy/A22.globaltech_input_driver",
                     "energy/A23.globaltech_input_driver",
                     "energy/A25.globaltech_input_driver",
                     "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
                     "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP")  ->
      L241.nonco2_tech_coeff

    L241.OutputEmissCoeff_elec %>%
      add_title("Non-CO2 new technology emission coefficients for the electricity sector") %>%
      add_units("NA") %>%
      add_comments("Combine historical and expect emission coefficients for non-CO2 emissions for new energy technologies") %>%
      add_comments("We've seperated electricity out to be driven by output-driver so we") %>%
      add_comments("more easily re-configure the strucutre of the sector to swap in cooling") %>%
      add_comments("technology choice which is implemented with pass-through sector/tech") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "energy/A_regions",
                     "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
                     "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
                     "L223.GlobalTechEff_elec") ->
      L241.OutputEmissCoeff_elec

    L241.nonco2_max_reduction %>%
      add_title("Max reduction of non-CO2 emissions by supply sector") %>%
      add_units("NA") %>%
      add_comments("Pass through may be historic values into the non CO2 max reduction emission coefficients") %>%
      add_legacy_name("L241.nonghg_max_reduction") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "energy/A_regions",
                     "emissions/A41.tech_coeff",
                     "emissions/A51.max_reduction",
                     "emissions/A51.steepness",
                     "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
                     "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP")  ->
      L241.nonco2_max_reduction

    L241.nonco2_steepness %>%
      add_title("Steepness of emissions reduction by supply sector for non CO2 emissions") %>%
      add_units("percentage") %>%
      add_comments("Pass through may be historic values into the non CO2 steepness emission coefficients") %>%
      add_legacy_name("L241.nonco2_steepness") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "energy/A_regions",
                     "emissions/A41.tech_coeff",
                     "emissions/A51.max_reduction",
                     "emissions/A51.steepness",
                     "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
                     "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP")  ->
      L241.nonco2_steepness

    return_data(L241.nonco2_tech_coeff, L241.OutputEmissCoeff_elec, L241.nonco2_max_reduction, L241.nonco2_steepness)
  } else {
    stop("Unknown command")
  }
}
