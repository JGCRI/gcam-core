#' module_gcam.usa_L271.trn_nonco2_USA
#'
#' U.S. non CO2 emission coeff from the transportation sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.nonco2_trn_tech_coeff_USA} and \code{L271.nonco2_trn_emiss_control_USA}.
#' The corresponding file in the original data system was \code{L271.trn_nonco2_USA.R} (gcam-usa level2).
#' @details Estimate future emission coeff for the HDV and LDVs via linear interpolation.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD September 2018
module_gcam.usa_L271.trn_nonco2_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/MARKAL_UCD_class",
             FILE = "gcam-usa/MARKAL_UCD_LDV_fuel",
             FILE = "gcam-usa/MARKAL_UCD_HDV_fuel",
             FILE = "gcam-usa/state_census_region",
             "L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA",
             "L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA",
             "L254.GlobalTranTechSCurve",
             "L254.StubTranTech_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.nonco2_trn_tech_coeff_USA",
             "L271.nonco2_trn_emiss_control_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    MARKAL_UCD_class <- get_data(all_data, "gcam-usa/MARKAL_UCD_class")
    MARKAL_UCD_LDV_fuel <- get_data(all_data, "gcam-usa/MARKAL_UCD_LDV_fuel")
    MARKAL_UCD_HDV_fuel <- get_data(all_data, "gcam-usa/MARKAL_UCD_HDV_fuel")
    state_census_region <- get_data(all_data, "gcam-usa/state_census_region")
    L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA <- get_data(all_data, "L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA")
    L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA     <- get_data(all_data, "L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA")
    L254.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve")
    L254.StubTranTech_USA     <- get_data(all_data, "L254.StubTranTech_USA")


    # Silence pacakge checks
    `2010` <- MARKAL_HDV_fuel <- MARKAL_LDV_fuel <- MARKAL_class <- MARKAL_fuel <- Non.CO2 <-
      census_region_id <- emiss.coeff <- end.year <- final.emissions.coefficient <-
      fuel <- half.life <- lifetime <- linear.control <- lm <- palette <- pollutant <-
      predict.lm <- region <- replacement_value <- sector.name <- start.year <- steepness <-
      stub.technology <- subsector.name <- supplysector <- tranSubsector <-
      tranTechnology <- value <- vintage <- year <- NULL



    # ===================================================

    # L271.nonco2_trn_tech_coeff_USA: Pollutant emissions for transportation technologies in all U.S. states
    # LDV emission coefficients
    L254.StubTranTech_USA %>%
      filter(supplysector == "trn_pass_road_LDV_4W") %>%
      left_join_error_no_match(MARKAL_UCD_class, by = c("tranSubsector" = "UCD_class")) %>%
      left_join_error_no_match(MARKAL_UCD_LDV_fuel, by = c("stub.technology" = "UCD_LDV_fuel" )) %>%
      repeat_add_columns(tibble(year = c(2005, 2010,MODEL_FUTURE_YEARS))) ->
      L254.StubTranTech_USA_LDV


    # Subset relevant classes and fuels from emission factor table
    L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA %>%
      filter(class %in% L254.StubTranTech_USA_LDV$MARKAL_class &
               fuel %in% L254.StubTranTech_USA_LDV$MARKAL_LDV_fuel) ->
      L171.nonco2_tgej_state_LDV_S_F_Y

    # L171.nonco2_tgej_state_LDV_S_F_Y only contains data up till 2050,
    # add the rest of the future years here, assume constant at 2050.
    L171.nonco2_tgej_state_LDV_S_F_Y %>%
      filter(year == 2050) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS > 2050])) %>%
      bind_rows(L171.nonco2_tgej_state_LDV_S_F_Y) ->
      L171.nonco2_tgej_state_LDV_S_F_Y

    # Match the emission coefficients onto the stub technology table
    L254.StubTranTech_USA_LDV %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L171.nonco2_tgej_state_LDV_S_F_Y$pollutant))) %>%
      # TODO look at the join used in the old data system.
      left_join_keep_first_only(L171.nonco2_tgej_state_LDV_S_F_Y, by = c("region" = "state",
                                                                        "MARKAL_class" = "class",
                                                                        "MARKAL_LDV_fuel" = "fuel",
                                                                        "year" = "year",
                                                                        "Non.CO2" = "pollutant")) %>%
      # Remove the EV emission factors for pollutants and NH3 EFs for CNG vehicles where there is no data.
      na.omit %>%
      # TODO remove the hard coded digits and add a gcam usa constant?
      mutate(emiss.coeff = round(value, 5)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L271.nonco2_LDV_USA

    # HDV emission coefficients
    L254.StubTranTech_USA %>%
      filter(supplysector %in% c("trn_pass_road","trn_freight_road")) %>%
      left_join_error_no_match(MARKAL_UCD_class, by = c("tranSubsector" = "UCD_class")) %>%
      left_join_error_no_match(MARKAL_UCD_HDV_fuel, by = c("stub.technology" = "UCD_HDV_fuel")) %>%
      repeat_add_columns(tibble(year = c(2005, 2010,MODEL_FUTURE_YEARS))) ->
      L254.StubTranTech_USA_HDV

    # Subset relevant classes and fuels from emission factor table
    L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA %>%
      filter(class %in% L254.StubTranTech_USA_HDV$MARKAL_class &
               fuel %in% L254.StubTranTech_USA_HDV$MARKAL_HDV_fuel) ->
      L171.nonco2_tgej_state_HDV_S_F_Y

    # Only contains values up till 2050 add values for the remaning future years here,
    # assume the future years are constant after 2050.
    L171.nonco2_tgej_state_HDV_S_F_Y %>%
      filter(year == 2050) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS > 2050])) %>%
      bind_rows(L171.nonco2_tgej_state_HDV_S_F_Y) ->
      L171.nonco2_tgej_state_HDV_S_F_Y

    # Match the emission coefficients onto the stub technology table
    L254.StubTranTech_USA_HDV %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L171.nonco2_tgej_state_HDV_S_F_Y$pollutant))) %>%
      # TODO look at the join used in the old data system.
      left_join_keep_first_only(L171.nonco2_tgej_state_HDV_S_F_Y, by = c("region" = "state",
                                                                         "MARKAL_class" = "class",
                                                                         "MARKAL_HDV_fuel" = "fuel",
                                                                         "year" = "year",
                                                                         "Non.CO2" = "pollutant")) %>%
      # Remove the EV emission factors for pollutants and NH3 EFs for CNG vehicles where there is no data.
      na.omit %>%
      # TODO remove the hard coded digits and add a gcam usa constant?
      mutate(emiss.coeff = round(value, 5)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, Non.CO2, emiss.coeff) ->
      L271.nonco2_HDV_USA

    # Bind the LDV and HDV tables and clean up
    L271.nonco2_HDV_USA %>%
      bind_rows(L271.nonco2_LDV_USA) %>%
      # Re-name the pollutants to match names of existing gases
      mutate(Non.CO2 = gsub("VOC","NMVOC",Non.CO2)) %>%
      mutate(Non.CO2 = gsub("NOX","NOx",Non.CO2)) %>%
      arrange(region, supplysector, tranSubsector, stub.technology, year, Non.CO2) ->
      L271.nonco2_trn_tech_coeff_USA

    # L271.nonco2_trn_emiss_control_USA: Emissions coeff controls for transportation technologies in all U.S. states
    # Need vehicle lifetimes in order to calculate final-emissions-coeff parameter.
    # These are written out for future years so assume the base year lifetimes are equal to those in 2010
    # TODO No idea if this is correct (this was a comment in the old data system might want to be addressed before merging into master)
    L254.GlobalTranTechSCurve %>%
      spread(year, lifetime) %>%
      # Assume that 2005 is equal to 2010
      mutate(`2005` = `2010`) %>%
      gather(year, lifetime, -sector.name, -subsector.name, -tranTechnology, -steepness, -half.life) %>%
      filter(!is.na(lifetime)) %>%
      mutate(year = as.integer(year)) ->
      L254.GlobalTranTechSCurve.long

    # Create table for emissions control
    L254.StubTranTech_USA_LDV %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L171.nonco2_tgej_state_LDV_S_F_Y$pollutant))) %>%
      # TODO use left_join_keep_first_only to preserve the old data system match that only kept
      # the first match, need to look into to see if this was bad match.
      left_join_keep_first_only(L171.nonco2_tgej_state_LDV_S_F_Y, by = c("region" = "state",
                                                                        "MARKAL_class" = "class",
                                                                        "MARKAL_LDV_fuel" = "fuel",
                                                                        "Non.CO2" = "pollutant",
                                                                        "year" = "year")) %>%
      # Remove the EV emission factors for pollutants and NH3 emission factors
      # for CNG vehicles where there is no data.
      na.omit %>%
      rename(MARKAL_fuel = MARKAL_LDV_fuel) ->
      L254.StubTranTech_USA_LDV.coeff

    L254.StubTranTech_USA_HDV %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L171.nonco2_tgej_state_HDV_S_F_Y$pollutant))) %>%
      # We do not expect at 1:1 match so use left join here.
      left_join(L171.nonco2_tgej_state_HDV_S_F_Y, by = c("region" = "state",
                                                                        "MARKAL_class" = "class",
                                                                        "MARKAL_HDV_fuel" = "fuel",
                                                                        "Non.CO2" = "pollutant",
                                                                        "year" = "year")) %>%
      # Remove the CNG-fueled vehicle emission factors for technologies where there is no data.
      na.omit %>%
      rename(MARKAL_fuel = MARKAL_HDV_fuel) ->
      L254.StubTranTech_USA_HDV.coeff

    # Combine LDV and HDV tables
    L254.StubTranTech_USA_LDV.coeff %>%
      bind_rows(L254.StubTranTech_USA_HDV.coeff) %>%
      filter(year %in% L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA$vintage) %>%
      # Add on lifetime for each technology that can be used to calculate final emissions coefficient
      # We do not expect a 1:1 match so use left join here.
      left_join(L254.GlobalTranTechSCurve.long, by = c("tranSubsector" = "subsector.name",
                                                                      "stub.technology" = "tranTechnology",
                                                                      "year" = "year")) %>%
      select(region, supplysector, tranSubsector, stub.technology, MARKAL_class, MARKAL_fuel,
             year, Non.CO2, emiss.coeff = value, lifetime) ->
      L254.StubTranTech_USA_trn

    # It seems that "Bus" does not have a lifetime. Give it 25 years for now
    # TODO change this from default to a gcam usa constant?
    L254.StubTranTech_USA_trn %>%
      mutate(lifetime = if_else(is.na(lifetime), as.integer(25), lifetime)) ->
      L254.StubTranTech_USA_trn

    # Add on start year and end year, census region, and id column for applying linear fits to estimate
    # final emissions coefficient
    # TODO used left_join_keep_first_only to preserve old data system behavior.
    L254.StubTranTech_USA_trn %>%
      left_join_keep_first_only(state_census_region, by = c("region" = "state")) %>%
      mutate(start.year = year,
             end.year = year + lifetime) %>%
      tidyr::unite(id, MARKAL_class, MARKAL_fuel, start.year, Non.CO2, census_region_id, sep = " ", remove = FALSE) ->
      L254.StubTranTech_USA_trn

    # Prepare table containing emissions factor degradation data
    L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA %>%
      filter(class %in% L254.StubTranTech_USA_trn$MARKAL_class &
               fuel %in% L254.StubTranTech_USA_trn$MARKAL_fuel) %>%
      tidyr::unite(id, class, fuel, vintage, pollutant, region, sep = " ", remove = FALSE) ->
      degrates

    # Create a list that contains linear fits for each degradation rate id, with labels
    model_rslt <- lapply(X = split(degrates, degrates$id), FUN = function(X){ lm(value ~ year, data = X) })

    # Extract the needed information (unique end year and id) from the emission control table and
    # compute the linear prediction using the model corresponding to each id.
    L254.StubTranTech_USA_trn %>%
      select(end.year, id) %>%
      distinct %>%
      mutate(final.emissions.coefficient = 0) %>%
      rename(year = end.year) ->
      prediction_table


    invisible(lapply(X = as.list(prediction_table$id), FUN = function(X){

      prediction_table[prediction_table$id == X, "final.emissions.coefficient"]  <<- predict.lm(model_rslt[[X]], newdata = prediction_table[prediction_table$id == X, ])

      }))

    # Add the final emissions coefficients onto the emissions control table
    L254.StubTranTech_USA_trn %>%
      select("region", "supplysector", "tranSubsector", "stub.technology", "MARKAL_class",
             "MARKAL_fuel", "year", "Non.CO2", "emiss.coeff", "lifetime",
             "start.year", "end.year", "census_region", "id") %>%
      left_join_error_no_match(prediction_table,  by = c("id", "end.year" = "year")) ->
      L271.nonco2_trn_emiss_control_USA

    # Some final emissions coefficients are negative. In these cases, change the final value to the last known
    L271.nonco2_trn_emiss_control_USA %>%
      filter(final.emissions.coefficient < 0) %>%
      pull(id) ->
      negative_ids

    degrates %>%
      filter(id %in% negative_ids) %>%
      group_by(id) %>%
      filter(year == max(year)) %>%
      ungroup %>%
      rename(replacement_value = value) ->
      last_avail_coeff

    # If there are coefficents that need to be replaced with the last available coeff do so now.
    if(nrow(last_avail_coeff) > 0){

      # TODO use left_join_keep_fist_only to preserve old dataystem behavior.
      L271.nonco2_trn_emiss_control_USA %>%
        left_join_keep_first_only(last_avail_coeff %>%
                                    select(id, replacement_value), by = 'id') %>%
        mutate(final.emissions.coefficient = if_else(final.emissions.coefficient < 0, replacement_value, final.emissions.coefficient)) %>%
        select("region", "supplysector", "tranSubsector", "stub.technology", "MARKAL_class",
               "MARKAL_fuel", "year", "Non.CO2", "emiss.coeff", "lifetime", "start.year",
               "end.year", "census_region", "id", "final.emissions.coefficient") ->
        L271.nonco2_trn_emiss_control_USA

    }

    # Add linear control object and round to 5 decimal places
    L271.nonco2_trn_emiss_control_USA %>%
      mutate(linear.control = "Emissions Coefficient Degradation",
             final.emissions.coefficient = round(final.emissions.coefficient, gcamusa.DIGITS_EMISSIONS)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, Non.CO2, linear.control,
             start.year, end.year, final.emissions.coefficient) ->
      L271.nonco2_trn_emiss_control_USA

    # Add on the remaining future years, keeping the final emissions coefficient constant at last known data point
    L254.StubTranTech_USA_LDV.coeff %>%
      bind_rows(L254.StubTranTech_USA_HDV.coeff) %>%
      filter(year >= 2050) %>%
      # We do not expect a 1:1 match here sp use left_join to Add on lifetime for each technology that can be used
      # to calculate final emissions coefficient
      left_join(L254.GlobalTranTechSCurve.long %>%
                                 select(subsector.name, tranTechnology, year, lifetime),
                               by = c("tranSubsector" = "subsector.name",
                                      "stub.technology" = "tranTechnology",
                                      "year")) ->
      remaining_future_years

    # MISSING VALUES: It seems that "Bus" does not have a lifetime. Give it 25 years for now
    remaining_future_years %>%
      mutate(lifetime = if_else(is.na(lifetime), as.integer(25), lifetime)) ->
      remaining_future_years

    # Subset the table containing the 2045 final emissions coefficients to match onto the future years
    # Also add an id column for easier matching
    L271.nonco2_trn_emiss_control_USA %>%
      filter(year == 2045) %>%
      tidyr::unite(id, region, supplysector, tranSubsector, stub.technology, Non.CO2, sep = " ", remove = FALSE) ->
      emiss_control_2045

    # Add on start year, end year, linear control object, and id for easier matching
    remaining_future_years %>%
      mutate(start.year = year,
             end.year = year + lifetime,
             linear.control =  "Emissions Coefficient Degradation") %>%
      tidyr::unite(id, region, supplysector, tranSubsector, stub.technology, Non.CO2, sep = " ", remove = FALSE) %>%
      # Match on the 2045 final emissions coefficients
      left_join(emiss_control_2045 %>%
                  select(id, final.emissions.coefficient), by = "id") %>%
      select(region, supplysector, tranSubsector, stub.technology, year, Non.CO2, linear.control,
             start.year, end.year, final.emissions.coefficient) ->
      remaining_future_years

    # Bind the future years to the existing emissions control table
    L271.nonco2_trn_emiss_control_USA %>%
      bind_rows(remaining_future_years) %>%
      #Re-name pollutants to match names of GCAM gases
      mutate(Non.CO2 = gsub("VOC","NMVOC",Non.CO2)) %>%
      mutate(Non.CO2 = gsub("NOX","NOx",Non.CO2)) %>%
      arrange(region, supplysector, tranSubsector, stub.technology, year, Non.CO2) ->
      L271.nonco2_trn_emiss_control_USA

    stopifnot(!any(is.na(L271.nonco2_trn_tech_coeff_USA)))
    stopifnot(!any(is.na(L271.nonco2_trn_emiss_control_USA)))


    # ===================================================

    # Produce outputs
    L271.nonco2_trn_tech_coeff_USA %>%
      add_title("Pollutant emissions coefficents for transportation technologies in all U.S. states") %>%
      add_units("NA") %>%
      add_comments("U.S. emission ceoff based on LDV and HDV technology and population") %>%
      add_legacy_name("L271.nonco2_trn_tech_coeff_USA") %>%
      add_precursors("gcam-usa/MARKAL_UCD_class",
                     "gcam-usa/MARKAL_UCD_LDV_fuel",
                     "gcam-usa/MARKAL_UCD_HDV_fuel",
                     "gcam-usa/state_census_region",
                     "L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA",
                     "L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA",
                     "L254.GlobalTranTechSCurve",
                     "L254.StubTranTech_USA") ->
      L271.nonco2_trn_tech_coeff_USA

    L271.nonco2_trn_emiss_control_USA %>%
      add_title("Emissions coeff controls for transportation technologies in all U.S. states") %>%
      add_units("NA") %>%
      add_comments("Linerally interpolate future emission coeff") %>%
      add_legacy_name("L271.nonco2_trn_emiss_control_USA") %>%
      add_precursors("gcam-usa/MARKAL_UCD_class",
                     "gcam-usa/MARKAL_UCD_LDV_fuel",
                     "gcam-usa/MARKAL_UCD_HDV_fuel",
                     "gcam-usa/state_census_region",
                     "L171.nonco2_tgej_censusR_trn_SMarkal_F_V_Y_USA",
                     "L171.nonco2_tgej_state_trn_SMarkal_F_Y_USA",
                     "L254.GlobalTranTechSCurve",
                     "L254.StubTranTech_USA") ->
      L271.nonco2_trn_emiss_control_USA

    return_data(L271.nonco2_trn_tech_coeff_USA, L271.nonco2_trn_emiss_control_USA)
  } else {
    stop("Unknown command")
  }
}
