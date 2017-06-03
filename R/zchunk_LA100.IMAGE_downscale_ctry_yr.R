#' module_aglu_LA100.IMAGE_downscale_ctry_yr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y}, \code{L100.IMAGE_an_FeedIO_ctry_C_Sys_Y}, \code{L100.IMAGE_an_Prodmixfrac_ctry_C_Y}. The corresponding file in the
#' original data system was \code{LA100.IMAGE_downscale_ctry_yr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA100.IMAGE_downscale_ctry_yr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/AGLU_ctry",
             FILE = "aglu/IMAGE/IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y",
             FILE = "aglu/IMAGE/IMAGE_an_FeedIO_Rimg_C_Sys_Y",
             FILE = "aglu/IMAGE/IMAGE_an_Prodmixfrac_Rimg_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
             "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
             "L100.IMAGE_an_Prodmixfrac_ctry_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y <- get_data(all_data, "aglu/IMAGE/IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y")
    IMAGE_an_FeedIO_Rimg_C_Sys_Y <- get_data(all_data, "aglu/IMAGE/IMAGE_an_FeedIO_Rimg_C_Sys_Y")
    IMAGE_an_Prodmixfrac_Rimg_C_Y <- get_data(all_data, "aglu/IMAGE/IMAGE_an_Prodmixfrac_Rimg_C_Y")

    # Helper function: copy 1970 data to new year 1960
    extrapolate_1960 <- function(x) {
      x %>%
        filter(year == 1970) %>%
        mutate(year = 1960) %>%
        bind_rows(x)
    }

    # Extrapolate out each IMAGE table to all historical years
    IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y %>%
      extrapolate_1960 %>%
      gather(IMAGE_region_ID, value, -commodity, -system, -input, -year) %>%
      complete(year = union(AGLU_HISTORICAL_YEARS, year), nesting(commodity, system, input, IMAGE_region_ID)) %>%
      arrange(year) %>%
      group_by(commodity, system, input, IMAGE_region_ID) %>%
      mutate(value = approx_fun(year, value, rule = 2)) ->
      L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y

    IMAGE_an_FeedIO_Rimg_C_Sys_Y %>%
      extrapolate_1960 %>%
      gather(IMAGE_region_ID, value, -commodity, -system, -year) %>%
      complete(year = union(AGLU_HISTORICAL_YEARS, year), nesting(commodity, system, IMAGE_region_ID)) %>%
      arrange(year) %>%
      group_by(commodity, system, IMAGE_region_ID) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      # Re-set negative values in the feedfrac table to 0
      mutate(value = if_else(value < 0, 0, value)) ->
      L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y

    IMAGE_an_Prodmixfrac_Rimg_C_Y %>%
      extrapolate_1960 %>%
      gather(IMAGE_region_ID, value, -commodity, -year) %>%
      complete(year = union(AGLU_HISTORICAL_YEARS, year), nesting(commodity, IMAGE_region_ID)) %>%
      arrange(year) %>%
      group_by(commodity, IMAGE_region_ID) %>%
      mutate(value = approx_fun(year, value, rule = 2)) ->
      L100.IMAGE_an_Prodmixfrac_Rimg_C_Y


    # Downscale IMAGE region-level data to all countries

    # Change AGLU_ctry$IMAGE_region_ID to character for easy joining in helper function
    AGLU_ctry$IMAGE_region_ID <- as.character(AGLU_ctry$IMAGE_region_ID)

    # Helper function: filter, repeat and add columns, join with iso data
    downscale_IMAGE_regions <- function(x, AGLU_ctry, by) {
      hyd <- filter(x, year %in% AGLU_HISTORICAL_YEARS) %>% spread(year, value)

      x %>%
        ungroup %>%
        filter(IMAGE_region_ID == 1, ! year %in% AGLU_HISTORICAL_YEARS) %>%
        repeat_add_columns(tibble::tibble(iso = sort(unique(AGLU_ctry$iso)))) %>%
        select(-IMAGE_region_ID) %>%
        left_join_keep_first_only(select(AGLU_ctry, iso, IMAGE_region_ID), by = "iso") %>%
        spread(year, value) %>%
        left_join(hyd, by = by) %>%
        na.omit %>%
        gather(year, value, matches(YEAR_PATTERN)) %>%
        mutate(year = as.integer(year))
    }

    L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGE_region_ID", "commodity", "system", "input")) ->
      L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y

    L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGE_region_ID", "commodity", "system")) ->
      L100.IMAGE_an_FeedIO_ctry_C_Sys_Y

    L100.IMAGE_an_Prodmixfrac_Rimg_C_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGE_region_ID", "commodity")) ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y

    # Produce outputs
    L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y

    L100.IMAGE_an_FeedIO_ctry_C_Sys_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IMAGE_an_FeedIO_ctry_C_Sys_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_FeedIO_Rimg_C_Sys_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_FeedIO_ctry_C_Sys_Y

    L100.IMAGE_an_Prodmixfrac_ctry_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IMAGE_an_Prodmixfrac_ctry_C_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_Prodmixfrac_Rimg_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y

    return_data(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y, L100.IMAGE_an_FeedIO_ctry_C_Sys_Y, L100.IMAGE_an_Prodmixfrac_ctry_C_Y)
  } else {
    stop("Unknown command")
  }
}
