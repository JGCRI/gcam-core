#' module_aglu_LA107.an_IMAGE_R_C_Sys_Fd_Y
#'
#' Build IMAGE Animal Production, Feed Consumption, and Input-Output Coefficients for each GCAM region and GCAM commodity in every year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L107.an_Prod_Mt_R_C_Sys_Fd_Y}, \code{L107.an_Feed_Mt_R_C_Sys_Fd_Y}, \code{L107.an_FeedIO_R_C_Sys_Fd_Y}. The corresponding file in the
#' original data system was \code{LA107.an_IMAGE_R_C_Sys_Fd_Y.R} (aglu level1).
#' @details This module builds three separate outputs at the GCAM region - GCAM commodity - Management system - Feed type - Year level of resolution.
#' First, Animal Production is built at the country-level from IMAGE country-level total production, mixed system fraction, and feed type fraction and aggregated to region.
#' Second, Feed Consumption is built at the country-level from IMAGE country-level input-output coefficients and animal production information and aggregated to region.
#' Finally, Feed Input-Output coefficients are calculated from region-level Feed Consumption and Animal Production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ACS April 2017
module_aglu_LA107.an_IMAGE_R_C_Sys_Fd_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
             "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
             "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
             FILE = "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
             "L107.an_FeedIO_R_C_Sys_Fd_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- iso <- IMAGE_region_ID <- commodity <- input <-
      GCAM_commodity <- value.x <- value.y <- . <- GCAM_region_ID <- feed <-
      NULL                        # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.IMAGE_an_Prodmixfrac_ctry_C_Y <- get_data(all_data, "L100.IMAGE_an_Prodmixfrac_ctry_C_Y")
    L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y <- get_data(all_data, "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y")
    L100.IMAGE_an_FeedIO_ctry_C_Sys_Y <- get_data(all_data, "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y")
    L105.an_Prod_Mt_ctry_C_Y <- get_data(all_data, "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y")

    # The temp-data-inject L105.an_Prod is in wide format. Convert to long:
    L105.an_Prod_Mt_ctry_C_Y %>%
      gather(year, value, -iso, -GCAM_commodity) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L105.an_Prod_Mt_ctry_C_Y

    # Perform computations:
    #
    # Lines 35-50 in original files
    # Take IMAGE total Animal Product data by country, commodity and year (L105.an_Prod_Mt_ctry_C_Y),
    # use IMAGE information about the fraction of animal production that is mixed (L100.IMAGE_an_Prodmixfrac_ctry_C_Y)
    # to calculate the system=Mixed animal production ( total * fraction mixed) and the system=Pastoral animal
    # production (total production - total*fraction mixed) by country, GCAM commodity and year.
    #
    # A difficulty is that the tibble L100.IMAGE_an_Prodmixfrac_ctry_C_Y omits regions that have 0 fraction mixed production and
    # L105.an_Prod_Mt_ctry_C_Y includes regions that have 0 total production. The match in the original code brings in NA's for
    # these values. The original code does not remove the NA values from this table, so they must be included here.
    # Therefore, a left_join rather than a left_join_error_no_match must be used.
    #
    # Old comment: calculate mixed production as total prod times mixed fraction. Use this to build
    # a table disaggregated by system.
    # printlog( "Calculating animal production by country, commodity, and system" )
    # Old comment: Multiply the total production by the fraction mixed, for the relevant commodities
    #
    #
    #
    # Take the total production table L105 and filter so that the GCAM_commodity in L105 match the commodity in the
    # fraction of mixed production table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y.
    L105.an_Prod_Mt_ctry_C_Y %>%
      filter(GCAM_commodity %in% L100.IMAGE_an_Prodmixfrac_ctry_C_Y$commodity) ->
      # store in a new table of total production by country, commodity, and year:
      L107.an_Prod_Mt_ctry_C_Y

    # Use the mixed fraction table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y, and the total animal production table,
    # L107.an_Prod_Mt_ctry_C_Y, to calculate the amount of mixed animal production. Add an identifier, system.
    L107.an_Prod_Mt_ctry_C_Y %>%
      # bring in the mixed fraction for each country, commodity, year as value.y. A left_join rather than
      # left_join_error_no_match is used because we preserve the NA's from mismatches:
      left_join(L100.IMAGE_an_Prodmixfrac_ctry_C_Y, by = c("iso", "year", "GCAM_commodity" = "commodity")) %>%
      # mixed animal production = total animal production * fraction mixed for each country, commodity, year:
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y, -IMAGE_region_ID) %>%
      # add the system identifier indicating this is mixed production:
      mutate(system = "Mixed") ->
      # store in a table specifying mixed animal production by country, commodity, and year
      L107.an_Prod_Mt_ctry_C_mix_Y

    # Use the total animal production table, L107.an_Prod_Mt_ctry_C_Y, and the mixed animal production table, L107.an_Prod_Mt_ctry_C_mix_Y
    # to calculate the amount of pastoral animal production. Add an identifier, system
    L107.an_Prod_Mt_ctry_C_Y %>%
      rename(totAnProd = value) %>%
      # pastoral animal production = total animal production - mixed animal production for each country, commodity, year:
      # ACS old code:
      # mutate(value = value - L107.an_Prod_Mt_ctry_C_mix_Y$value) %>%
      left_join(select(L107.an_Prod_Mt_ctry_C_mix_Y, iso, GCAM_commodity, year, value),
                by = c("iso","GCAM_commodity", "year")) %>%
      rename(mixAnProd = value) %>%
      mutate(value = totAnProd - mixAnProd) %>%
      select(-totAnProd, -mixAnProd) %>%
      # add the system identifier indicating this is pastoral production:
      mutate(system = "Pastoral") ->
      # store in a table specifying pastoral animal production by country, commodity, and year
      L107.an_Prod_Mt_ctry_C_past_Y

    # Combine the mixed production table, L107.an_Prod_Mt_ctry_C_mix_Y, and the pastoral production table, L107.an_Prod_Mt_ctry_C_past_Y,
    # to form a table organizing the amount of animal production by country, commodity, system, and year:
    L107.an_Prod_Mt_ctry_C_Sys_Y <- bind_rows(L107.an_Prod_Mt_ctry_C_mix_Y, L107.an_Prod_Mt_ctry_C_past_Y)

    # Lines 51-57 in original file
    # Use IMAGE feed fraction by country, commodity, system, feed type and year information, L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y,
    # to further disaggregate animal production, L107.an_Prod_Mt_ctry_C_Sys_Y, by feed type.
    # In the original code, this table omits NA values.
    #
    # printlog( "Calculating animal production by country, commodity, system, and feed type" )
    #
    # take the table that contains animal production for each unique country, commodity, system, year combo
    L107.an_Prod_Mt_ctry_C_Sys_Y %>%
      # add a column containing the feed types from the IMAGE feed fraction table, L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y and
      # repeat the entire animal production for each unique country, commodity, system, year combo dataframe for each feed type:
      repeat_add_columns(tibble::tibble(feed = unique(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y$input))) %>%
      # bring in the feed fraction for each country, commodity, system, year as value.y:
      left_join(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y, by = c("iso", "year", "GCAM_commodity" = "commodity", "system", "feed"="input")) %>%
      # calculate feed type animal production = total animal production * fraction feed type for each country, commodity, system, year:
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y, -IMAGE_region_ID) %>%
      na.omit ->
      # store in a table specifying animal production by country, commodity, system, feed, and year:
      L107.an_Prod_Mt_ctry_C_Sys_Fd_Y

    # Lines 59-64 in the original file
    # Use the country, commodity, system, feed, year animal production table, L107.an_Prod_Mt_ctry_C_Sys_Fd_Y
    # and IMAGE input-output coefficients by country, commodity, system, and year, L100.IMAGE_an_FeedIO_ctry_C_Sys_Y
    # to calcluate animal feed consumption by country, commodity, system, feed, and year.
    #
    # Take the animal production by country, commodity, system, feed type, and year table
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      # add the country, commodity, system, year input output coefficient from IMAGE data:
      left_join_error_no_match(L100.IMAGE_an_FeedIO_ctry_C_Sys_Y, by = c("iso", "year",  "GCAM_commodity" = "commodity", "system")) %>%
      # Calculate the country, commodity, system, feed type, year consumption = [country, commodity, system, feed type, year production] * [country, commodity, system, year IO from IMAGE]:
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y, -IMAGE_region_ID)->
      L107.an_Feed_Mt_ctry_C_Sys_Fd_Y

    # Lines 66-71 in original file
    # Aggregate animal production into GCAM regions:

    # take country level animal production data:
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      # add in the GCAM region id corresponding to the country:
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # ACS old code:
      #      mutate(GCAM_region_ID =  left_join_error_no_match(., iso_GCAM_regID, by = c("iso"))[['GCAM_region_ID']]) %>%
      select(-iso) %>%
      # sum by GCAM region, commodity, year, system, and feed
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value = sum(value)) ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y

    # take country level feed consumption data:
    L107.an_Feed_Mt_ctry_C_Sys_Fd_Y %>%
      # add in the GCAM region id corresponding to the country:
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # ACS old code:
      #      mutate(GCAM_region_ID = left_join_error_no_match(., iso_GCAM_regID, by = c("iso"))[['GCAM_region_ID']]) %>%
      select(-iso) %>%
      # sum by GCAM region, commodity, year, system, and feed
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value = sum(value)) ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y

    # Lines 73-80 in original file
    # Calculate the weighted average feed input-output coefficients by region, commodity, system, feed, and year

    # take the region, commodity, system, feed type, year feed consumption:
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      rename(feedVal = value) %>%
      # add in the corresponding animal production amount
      left_join_error_no_match(L107.an_Prod_arranged,
                               by = c("GCAM_region_ID", "GCAM_commodity", "year", "system", "feed")) %>%
      rename(prodVal = value) %>%
      # calculate the region, commodity, system, feed type, year IO coefficient as feed consumption/animal production
      mutate(value = feedVal / prodVal) %>%
      select(-feedVal, -prodVal) %>%
      # replace NA/NaN with the value 100
      replace_na(list(value = 100)) ->
      # store in a table specifying IO coefficients by region, commodity, system, feed type, and year:
      L107.an_FeedIO_R_C_Sys_Fd_Y


    # ACS old code:
    # L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
    #   # add in the corresponding animal production amount as value.y
    #   left_join_error_no_match(L107.an_Prod_Mt_R_C_Sys_Fd_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year", "system", "feed")) %>%
    #   # calculate the region, commodity, system, feed type, year IO coefficient as feed consumption/animal production
    #   mutate(value = value.x / value.y) %>%
    #   select(-value.x, -value.y) %>%
    #   # replace NA/NaN with the value 100
    #   replace_na(list(value = 100)) ->
    #   # store in a table specifying IO coefficients by region, commodity, system, feed type, and year:
    #   L107.an_FeedIO_R_C_Sys_Fd_Y

    # Produce outputs
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      add_title("Animal production by GCAM region / commodity / system / feed type / year") %>%
      add_units("Megatons (Mt)") %>%
      add_comments("IMAGE country-level data regarding feed type fraction and mixed versus") %>%
      add_comments("pastoral system fraction are used to downscale IMAGE country-level total") %>%
      add_comments("animal production data.") %>%
      add_legacy_name("L107.an_Prod_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y

    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      add_title("Animal feed consumption by GCAM region / commodity / system / feed type / year") %>%
      add_units("Megatons (Mt)") %>%
      add_comments("Country-level IMAGE feed type input-output coefficient data is used with") %>%
      add_comments("Country-level production data to calculate Feed Consumption.") %>%
      add_legacy_name("L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y

    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      add_title("Animal production input-output coefficients by GCAM region / commodity / system / feed type / year") %>%
      add_units("Unitless") %>%
      add_comments("GCAM-region-level feed consumption is divided by GCAM-region-level production to give") %>%
      add_comments("GCAM-region-level IO coefficients. NA values are rewritten to 100.") %>%
      add_legacy_name("L107.an_FeedIO_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_FeedIO_R_C_Sys_Fd_Y

    return_data(L107.an_Prod_Mt_R_C_Sys_Fd_Y, L107.an_Feed_Mt_R_C_Sys_Fd_Y, L107.an_FeedIO_R_C_Sys_Fd_Y)
  } else {
    stop("Unknown command")
  }
}
