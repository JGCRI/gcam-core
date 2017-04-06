#' module_aglu_LA107.an_IMAGE_R_C_Sys_Fd_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L107.an_Prod_Mt_R_C_Sys_Fd_Y}, \code{L107.an_Feed_Mt_R_C_Sys_Fd_Y}, \code{L107.an_FeedIO_R_C_Sys_Fd_Y}. The corresponding file in the
#' original data system was \code{LA107.an_IMAGE_R_C_Sys_Fd_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA107.an_IMAGE_R_C_Sys_Fd_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "temp-data-inject/L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
             FILE = "temp-data-inject/L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
             FILE = "temp-data-inject/L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
             FILE = "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
             "L107.an_FeedIO_R_C_Sys_Fd_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.IMAGE_an_Prodmixfrac_ctry_C_Y <- get_data(all_data, "temp-data-inject/L100.IMAGE_an_Prodmixfrac_ctry_C_Y")
    L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y <- get_data(all_data, "temp-data-inject/L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y")
    L100.IMAGE_an_FeedIO_ctry_C_Sys_Y <- get_data(all_data, "temp-data-inject/L100.IMAGE_an_FeedIO_ctry_C_Sys_Y")
    L105.an_Prod_Mt_ctry_C_Y <- get_data(all_data, "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y")


    # The inputs L100.IMAGE_X and L105.an_Prod  are in wide format. Convert to long:
      if (ncol(L100.IMAGE_an_Prodmixfrac_ctry_C_Y) > 5 ){
        L100.IMAGE_an_Prodmixfrac_ctry_C_Y %>%
          gather(year, value, -iso, -IMAGE_region_ID, -commodity) %>%
          mutate( year = substr(year, 2, 5)) ->
          L100.IMAGE_an_Prodmixfrac_ctry_C_Y
      }


      if (ncol(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y) > 7 ){
        L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y %>%
          gather(year, value, -iso, -IMAGE_region_ID, -commodity, -system, -input) %>%
          mutate( year = substr(year, 2, 5)) ->
          L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y
      }


      if (ncol(L100.IMAGE_an_FeedIO_ctry_C_Sys_Y) > 6 ){
        L100.IMAGE_an_FeedIO_ctry_C_Sys_Y %>%
          gather(year, value, -iso, -IMAGE_region_ID, -commodity, -system) %>%
          mutate( year = substr(year, 2, 5)) ->
          L100.IMAGE_an_FeedIO_ctry_C_Sys_Y
      }


      if (ncol(L105.an_Prod_Mt_ctry_C_Y) > 4 ){
        L105.an_Prod_Mt_ctry_C_Y %>%
          gather(year, value, -iso, -GCAM_commodity) %>%
          mutate( year = substr(year, 2, 5)) ->
          L105.an_Prod_Mt_ctry_C_Y
      }



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
    # these values. I tried a left_join to replicate that with no success, and so ultimately used a combination of semi_join and
    # tidyr::complete.
    # The original code does not remove the NA values from this table, so they must be included here.
    #
    # Old comment: calculate mixed production as total prod times mixed fraction. Use this to build
    # a table disaggregated by system.
    # printlog( "Calculating animal production by country, commodity, and system" )
    # Old comment: Multiply the total production by the fraction mixed, for the relevant commodities


    # Take the total production table L105 and filter so that the GCAM_commodity in L105 match the commodity in the
    # fraction of mixed production table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y.
    # This creates a new table of total production by country, commodity, and year: L107.an_Prod_Mt_ctry_C_Y
    L105.an_Prod_Mt_ctry_C_Y %>%
      filter(GCAM_commodity %in% L100.IMAGE_an_Prodmixfrac_ctry_C_Y$commodity) ->
      L107.an_Prod_Mt_ctry_C_Y

    # Form a complete table of L100.IMAGE_an_Prodmixfrac_ctry_C_Y whose iso and commodity match up with
    # L107.an_Prod_Mt_ctry_C_Y iso and GCAM_commodity. Use tidyr::complete to bring in the NA's the original
    # match function did for country-commodity combos that appear in total production (L107) but not the mixed
    # fraction table (L100). This then creates a complete table of fractions of mixed production by country,
    # commodity and year.
    # A left join cannot be used because we want to actually have values for every country-commodity combo, and we
    # must introduce NA's for those that are missing.
    L100.IMAGE_an_Prodmixfrac_ctry_C_Y %>%
      # select the country, year, commodity of the L100.IMAGE_an_Prodmixfrac_ctry_C_Y mixed fraction table that match the
      # total production table, L107.an_Prod_Mt_ctry_C_Y:
      semi_join(L107.an_Prod_Mt_ctry_C_Y, by = c("iso","year", "commodity" = "GCAM_commodity")) %>%
      # Fill in any country-commodity combinations in the L100.IMAGE_an_Prodmixfrac_ctry_C_Y mixed fraction table that appear
      # in the total production table, L107.an_Prod_Mt_ctry_C_Y:
      tidyr::complete( iso = unique( L107.an_Prod_Mt_ctry_C_Y$iso), commodity = unique( L107.an_Prod_Mt_ctry_C_Y$GCAM_commodity),
                       year, fill = list(value = NA) ) %>%
      # Arrange by year and commodity so that the fraction of mixed production table L100.IMAGE_an_Prodmixfrac_ctry_C_Y_dummy is
      # ordered in the same way as the total production table, L107.an_Prod_Mt_ctry_C_Y for the next pipeline:
      arrange(year, commodity) ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y_complete



    # Use the mixed fraction table, L100.IMAGE_an_Prodmixfrac_ctry_C_Y_dummy, and the total animal production table,
    # L107.an_Prod_Mt_ctry_C_Y, to calculate the amount of mixed animal production. Add an identifier, system.
    L107.an_Prod_Mt_ctry_C_Y %>%
      # mixed animal production = total animal production * fraction mixed for each country, commodity, year:
      mutate(value = value *  L100.IMAGE_an_Prodmixfrac_ctry_C_Y_complete$value) %>%
      # add the system identifier indicating this is mixed production:
      mutate(system="Mixed") ->
      # store in a table specifying mixed animal production by country, commodity, and year
      L107.an_Prod_Mt_ctry_C_mix_Y

    # Use the total animal production table, L107.an_Prod_Mt_ctry_C_Y, and the mixed animal production table, L107.an_Prod_Mt_ctry_C_mix_Y
    # to calculate the amount of pastoral animal production. Add an identifier, system
    L107.an_Prod_Mt_ctry_C_Y %>%
      # pastoral animal production = total animal production - mixed animal production for each country, commodity, year:
      mutate(value = value-L107.an_Prod_Mt_ctry_C_mix_Y$value) %>%
      # add the system identifier indicating this is pastoral production:
      mutate(system="Pastoral") ->
      # store in a table specifying pastoral animal production by country, commodity, and year
      L107.an_Prod_Mt_ctry_C_past_Y


    # Combine the mixed production table, L107.an_Prod_Mt_ctry_C_mix_Y, and the pastoral production table, L107.an_Prod_Mt_ctry_C_past_Y,
    # to form a table organizing the amount of animal production by country, commodity, system, and year:
    L107.an_Prod_Mt_ctry_C_Sys_Y <- bind_rows( L107.an_Prod_Mt_ctry_C_mix_Y, L107.an_Prod_Mt_ctry_C_past_Y )







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
      repeat_add_columns( tibble::tibble(unique(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y$input)))%>%
      # rename the added column to something more sensible:
      rename(feed = `unique(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y$input)`) %>%
      # use semi_join to restrict the total production table L107.an_Prod_Mt_ctry_C_Sys_Y to the country, commodity, system, input/feed,
      # year combinations that appear in the feed fraction table. This restriction is possible because this table omits NA's:
      semi_join(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y,by = c("iso","year",  "GCAM_commodity" = "commodity", "system", "feed"="input"))%>%
      #
      # calculate feed type animal production = total animal production * fraction feed type for each country, commodity, system, year:
      mutate(value = value *  L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y$value) %>%
      # omit NA's:
      na.omit->
      # store in a table specifying animal production by country, commodity, system, feed, and year:
      L107.an_Prod_Mt_ctry_C_Sys_Fd_Y




    # Lines 59-64 in the original file
    # Use the country, commodity, system, feed, year animal production table, L107.an_Prod_Mt_ctry_C_Sys_Fd_Y
    # and IMAGE input-output coefficients by country, commodity, system, and year, L100.IMAGE_an_FeedIO_ctry_C_Sys_Y
    # to calcluate inputs for animal production by country, commodity, system, feed, and year.
    #
    #
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      left_join_error_no_match(L100.IMAGE_an_FeedIO_ctry_C_Sys_Y,by = c("iso","year",  "GCAM_commodity" = "commodity", "system"))%>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y, -IMAGE_region_ID)->
      L107.an_Feed_Mt_ctry_C_Sys_Fd_Y





    # Lines 66-71 in original file
    # Aggregate animal production into GCAM regions:
    L107.an_Prod_Mt_ctry_C_Sys_Fd_Y %>%
      mutate(GCAM_region_ID =  left_join_error_no_match(., iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
      select(-iso) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value=sum(value)) ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y


    L107.an_Feed_Mt_ctry_C_Sys_Fd_Y %>%
      mutate(GCAM_region_ID =  left_join_error_no_match(., iso_GCAM_regID, by = c("iso"))$GCAM_region_ID) %>%
      select(-iso) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year, system, feed) %>%
      summarise(value=sum(value)) ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y


    # Lines 73-80 in original file
    # weighted average feed input-output coefficients by region, commodity, system, feed, and year

    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      left_join_error_no_match(L107.an_Prod_Mt_R_C_Sys_Fd_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year", "system", "feed")) %>%
      mutate(value = value.x/value.y) %>%
      select(-value.x, -value.y) %>%
      mutate(value=if_else(is.na(value), 100, value))->
      L107.an_FeedIO_R_C_Sys_Fd_Y


    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L107.an_Prod_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "temp-data-inject/L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "temp-data-inject/L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "temp-data-inject/L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_Prod_Mt_R_C_Sys_Fd_Y
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "temp-data-inject/L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "temp-data-inject/L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "temp-data-inject/L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y
    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L107.an_FeedIO_R_C_Sys_Fd_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "temp-data-inject/L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
                     "temp-data-inject/L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
                     "temp-data-inject/L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
                     "temp-data-inject/L105.an_Prod_Mt_ctry_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L107.an_FeedIO_R_C_Sys_Fd_Y

    return_data(L107.an_Prod_Mt_R_C_Sys_Fd_Y, L107.an_Feed_Mt_R_C_Sys_Fd_Y, L107.an_FeedIO_R_C_Sys_Fd_Y)
  } else {
    stop("Unknown command")
  }
}



