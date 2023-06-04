# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L107.an_IMAGE_R_C_Sys_Fd_Y
#'
#' Build IMAGE Animal Production, Feed Consumption, and Input-Output Coefficients for each GCAM region and GCAM commodity in every year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L107.an_Prod_Mt_R_C_Sys_Fd_Y}, \code{L107.an_Feed_Mt_R_C_Sys_Fd_Y}. The corresponding file in the
#' original data system was \code{LA107.an_IMAGE_R_C_Sys_Fd_Y.R} (aglu level1).
#' @details This module builds three separate outputs at the GCAM region - GCAM commodity - Management system - Feed type - Year level of resolution.
#' First, Animal Production is built at the country-level from IMAGE country-level total production, mixed system fraction, and feed type fraction and aggregated to region.
#' Second, Feed Consumption is built at the country-level from IMAGE country-level input-output coefficients and animal production information and aggregated to region.
#' Finally, Feed Input-Output coefficients are calculated from region-level Feed Consumption and Animal Production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise
#' @importFrom tidyr replace_na
#' @author ACS April 2017
module_aglu_L107.an_IMAGE_R_C_Sys_Fd_Y <- function(command, ...) {

  MODULE_INPUTS <-
    c("L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
      "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
      "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
      "L105.an_Prod_Mt_ctry_C_Y",
      "L100.IMAGE_an_watercontent_ctry_C")

  MODULE_OUTPUTS <-
    c("L107.an_Prod_Mt_R_C_Sys_Fd_Y",
      "L107.an_Feed_Mt_R_C_Sys_Fd_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- iso <- IMAGEv3p2_region <- commodity <- input <-
      GCAM_commodity <- value.x <- value.y <- . <- GCAM_region_ID <- feed <-
      totAnProd <- mixAnProd <- feedVal <- prodVal <- compVal <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 1. Calculate livestock production by system (in wet tons) ----

    # Take the total production table L105 and filter so that the GCAM_commodity in L105 match the commodity in the
    # fraction of mixed production table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y.
    # Fish_othermeat not included in L100.IMAGE_an_Prodmixfrac_ctry_C_Y
    # Note that GCAM_region_ID in L105.an_Prod_Mt_ctry_C_Y is passed through
    L105.an_Prod_Mt_ctry_C_Y %>%
      filter(GCAM_commodity %in% L100.IMAGE_an_Prodmixfrac_ctry_C_Y$commodity) ->
      # store in a new table of total production by country, commodity, and year:
      L107.an_Prod_Mt_ctry_C_Y


    # Use the mixed fraction table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y, and the total animal production table,
    # L107.an_Prod_Mt_ctry_C_Y, to calculate the amount of mixed animal production. Add an identifier, system.
    L107.an_Prod_Mt_ctry_C_Y %>%
      left_join_error_no_match(L100.IMAGE_an_Prodmixfrac_ctry_C_Y %>%
                                 rename(MixedFrac = value) %>%
                                 mutate(PastoralFrac = 1 - MixedFrac),
                               by = c("iso", "year", "GCAM_commodity" = "commodity")) %>%
      # mixed animal production = total animal production * fraction mixed for each country, commodity, year:
      mutate(Mixed = value * MixedFrac,
             Pastoral = value * PastoralFrac) %>%
      select(-MixedFrac, -PastoralFrac, -value, -IMAGEv3p2_region) %>%
      # gather systems
      gather(system, value, Mixed, Pastoral) ->
      # store in a table specifying mixed animal production by country, commodity, and year
      L107.an_Prod_Mt_ctry_C_Sys_Y


    # Check NA
    assertthat::assert_that(L107.an_Prod_Mt_ctry_C_Sys_Y %>% filter(is.na(value)) %>% nrow == 0)


    # 2. Further compute livestock production by tech per system ----

    # take the table that contains animal production for each unique country, commodity, system, year combo
    L107.an_Prod_Mt_ctry_C_Sys_Y %>%
      # using right_join to expand input and only keep system & input combinations in L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y
      right_join(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y %>% distinct(GCAM_commodity = commodity, system, input),
                 by = c("GCAM_commodity", "system")) %>%
      # bring in the feed fraction for each country, commodity, system, year as value.y:
      left_join_error_no_match(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y %>%
                                 rename(FeedFrac = value, GCAM_commodity = commodity),
                               by = c("iso", "GCAM_commodity", "year", "system", "input")) %>%
      # calculate feed type animal production = total animal production * fraction feed type for each country, commodity, system, year:
      mutate(value = value * FeedFrac) %>%
      rename(feed = input) %>%
      select(-FeedFrac, -IMAGEv3p2_region) ->
      # store in a table specifying animal production by country, commodity, system, feed, and year:
      L107.an_Prod_Mt_ctry_C_Sys_Fd_Y


    # 3. Calculate feed demand based on IO coef from IMAGE ----

    # Use the country, commodity, system, feed, year animal production table, L107.an_Prod_Mt_ctry_C_Sys_Fd_Y
    # and IMAGE input-output coefficients by country, commodity, system, and year, L100.IMAGE_an_FeedIO_ctry_C_Sys_Y
    # to calculate animal feed consumption by country, commodity, system, feed, and year.
    # Assume IO coef is the same with in a system
    # Take the animal production by country, commodity, system, feed type, and year table
    # Note that water content (output) is needed since production is in wet tons & IO was in dry tons
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      left_join_error_no_match(L100.IMAGE_an_watercontent_ctry_C %>%
                                 select(GCAM_commodity = commodity, iso, WaterContent = value),
                               by = c("iso", "GCAM_commodity")) %>%
      # add the country, commodity, system, year input output coefficient from IMAGE data:
      left_join_error_no_match(L100.IMAGE_an_FeedIO_ctry_C_Sys_Y %>% rename(IOCoef = value),
                               by = c("iso", "year",  "GCAM_commodity" = "commodity", "system")) %>%
      # Calculate the country, commodity, system, feed type, year
      # consumption = [country, commodity, system, feed type, year production] * water Adj * [country, commodity, system, year IO from IMAGE]:
      mutate(value = value * (1 - WaterContent) * IOCoef) %>%
      select(-IOCoef, -IMAGEv3p2_region, -WaterContent)->
      L107.an_Feed_Mt_ctry_C_Sys_Fd_Y


    # 4. Aggregate animal production, feed inputs, and IO coef into GCAM regions ----

    ## 4.1. livestock production in GCAM regions ----
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      select(-iso) %>%
      # sum by GCAM region, commodity, year, system, and feed
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y

    ## Assert production consistency
    assertthat::assert_that(
      L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
        group_by(GCAM_region_ID, GCAM_commodity,  year) %>%
        summarize(value = sum(value), .groups = "drop" ) %>%
        left_join_error_no_match(
          L105.an_Prod_Mt_ctry_C_Y %>%
            group_by(GCAM_region_ID, GCAM_commodity,  year) %>%
            summarize(value1 = sum(value), .groups = "drop"), by = c("GCAM_region_ID", "GCAM_commodity", "year")
        ) %>%
        filter(abs(value - value1) >0.0001) %>%
        nrow ==0,
      msg = "Livestock production at GCAM regional level changed (L105.an_Prod_Mt_ctry_C_Y used in LB109)"
    )


    ## 4.2. Feed demand in GCAM regions ----
    L107.an_Feed_Mt_ctry_C_Sys_Fd_Y %>%
      # add in the GCAM region id corresponding to the country:
      #left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      select(-iso) %>%
      # sum by GCAM region, commodity, year, system, and feed
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y

    # Note that IO coefficients are now computed in L202 so any adjustment in feed demand can be included.



    # Produce outputs
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      add_title("Animal production by GCAM region / commodity / system / feed type / year") %>%
      add_units("Megatons (Mt)") %>%
      add_comments("IMAGE country-level data regarding feed type fraction and mixed versus") %>%
      add_comments("pastoral system fraction are used to downscale IMAGE country-level total") %>%
      add_comments("animal production data.") %>%
      add_legacy_name("L107.an_Prod_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "L105.an_Prod_Mt_ctry_C_Y") ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y

    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      add_title("Animal feed consumption by GCAM region / commodity / system / feed type / year") %>%
      add_units("Megatons (Mt)") %>%
      add_comments("Country-level IMAGE feed type input-output coefficient data is used with") %>%
      add_comments("Country-level production data to calculate Feed Consumption.") %>%
      add_legacy_name("L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "L105.an_Prod_Mt_ctry_C_Y",
                     "L100.IMAGE_an_watercontent_ctry_C") ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
