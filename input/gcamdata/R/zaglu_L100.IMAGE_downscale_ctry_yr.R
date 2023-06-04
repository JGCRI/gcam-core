# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.IMAGE_downscale_ctry_yr
#'
#' Extrapolate IMAGE data to all AGLU historical years and countries.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y}, \code{L100.IMAGE_an_FeedIO_ctry_C_Sys_Y}, \code{L100.IMAGE_an_Prodmixfrac_ctry_C_Y}. The corresponding file in the
#' original data system was \code{LA100.IMAGE_downscale_ctry_yr.R} (aglu level1).
#' @details Each IMAGE table is extrapolated to all AGLU historical years and then downscaled from IMAGE region to all AGLU countries.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select union
#' @importFrom tidyr complete gather nesting spread
#' @author BBL June 2017 XZ 2022
module_aglu_L100.IMAGE_downscale_ctry_yr <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "aglu/IMAGE/IMAGE_an_feed_bySystem",
      FILE = "aglu/IMAGE/IMAGE_an_meat",
      FILE = "aglu/IMAGE/IMAGE_an_head_bySystem",
      "L105.an_Prod_Mt_ctry_C_Y")

  MODULE_OUTPUTS <-
    c("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
      "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
      "L100.IMAGE_an_Prodmixfrac_ctry_C_Y",
      "L100.IMAGE_an_watercontent_ctry_C")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- IMAGEv3p2_region <- value <- commodity <-
      input <- iso <- year <- NULL # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Step 1 Interpolation across years  ----

    # Helper function: copy data to a new year
    create_new_yeardata <- function(x, source_year, new_year) {
      x %>%
        filter(year == source_year) %>%
        mutate(year = new_year) %>%
        bind_rows(x)
    }


    # Create IMAGE - GCAM mappings ----
    IMAGE_GCAM_COMM_Mapping <-
      data.frame(commodity_IMAGE = c("non-dairy cattle", "dairy cattle", "pigs", "poultry", "sheep & goats"),
                 commodity = c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat"))
    IMAGE_GCAM_FEED_Mapping <-
      data.frame(feed_IMAGE = c("food crops", "residues", "grass & fodder", "scavenging"),
                 input = c("FeedCrops", "FodderHerb_Residue", "Pasture_FodderGrass", "Scavenging_Other"))
    IMAGE_GCAM_MEAT_Mapping <-
      data.frame(commodity_IMAGE = c("beef", "milk", "pork", "poultry & eggs", "mutton & goat meat"),
                 commodity = c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat"))

    # Extrapolate each IMAGE table to all historical years
    # For each table, same basic steps: copy 1970 data to 1960; fill out all AGLU
    # historical years; interpolate.

    # Oct 2022: note that with the new IMAGE data, interpolation is no longer needed
    # but the processing was mostly retained

    # Warning: meat production and feed inputs in IMAGE data are in dry-matter units
    # That is, the IO coefficient or FCR (feed conversion ratio) is in DM ton per DM ton

    IMAGE_an_feed_bySystem %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      create_new_yeardata(1970, 1960) %>%
      gather(IMAGEv3p2_region, value, -system, -commodity, -feed, -year) %>%
      complete(year = union(aglu.AGLU_HISTORICAL_YEARS, year), nesting(commodity, system, feed, IMAGEv3p2_region)) %>%
      arrange(year) %>%
      spread(system, value) %>%
      rename(Mixed = `intensive grazing system`,
             Pastoral = `extensive grazing system`) %>%
      select(-total) %>%
      gather(system, value, Mixed, Pastoral) %>%
      # GCAM doesn't include animal feed for now
      filter(!feed %in% c("total", "animal products")) %>%
      rename(feed_IMAGE = feed) %>%
      # map feed input & calculate shares
      left_join_error_no_match(IMAGE_GCAM_FEED_Mapping, by = "feed_IMAGE") %>%
      select(-feed_IMAGE) %>%
      # map commodity
      filter(commodity != "total") %>%
      rename(commodity_IMAGE = commodity) %>%
      left_join_error_no_match(IMAGE_GCAM_COMM_Mapping, by = "commodity_IMAGE") %>%
      select(-commodity_IMAGE) %>%
      filter(!(commodity %in% c("Pork", "Poultry") & system == "Pastoral")) %>%
      # if values are negative (small discrepancy), use zero
      mutate(value = pmax(value, 0))->
      IMAGE_an_feed_bySystem1


    # Use animal head to calculate output share by system
    IMAGE_an_head_bySystem %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      create_new_yeardata(1970, 1960) %>%
      gather(IMAGEv3p2_region, value, -system, -commodity, -year) %>%
      complete(year = union(aglu.AGLU_HISTORICAL_YEARS, year), nesting(commodity, system, IMAGEv3p2_region)) %>%
      arrange(year) %>%
      group_by(commodity, IMAGEv3p2_region, system) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      spread(system, value) %>%
      transmute(year, commodity_IMAGE = commodity, IMAGEv3p2_region,
                value = `intensive grazing system`/ (`intensive grazing system` + `extensive grazing system`) ) %>%
      replace_na(list(value = 1)) %>%
      left_join_error_no_match(IMAGE_GCAM_COMM_Mapping, by = "commodity_IMAGE") %>%
      select(-commodity_IMAGE) ->
      L100.IMAGE_an_Prodmixfrac_Rimg_C_Y

    # Total meat output from IMAGE
    IMAGE_an_meat %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      create_new_yeardata(1970, 1960) %>%
      gather(IMAGEv3p2_region, value, -commodity, -year) %>%
      complete(year = union(aglu.AGLU_HISTORICAL_YEARS, year), nesting(commodity, IMAGEv3p2_region)) %>%
      arrange(year) %>%
      filter(commodity != "total") %>%
      rename(commodity_IMAGE = commodity) %>%
      left_join_error_no_match(IMAGE_GCAM_MEAT_Mapping, by = "commodity_IMAGE") %>%
      select(-commodity_IMAGE) ->
      L100.IMAGE_an_meat_KDMT

    # Calculate livestock product water content by comparing IMAGE meat vs. FAO meat
    # Using 5-year average and values are bounded by 10 - 80 percentile (regional) values
    # The bound is slighted skewed towards lower water
    # so that the total pasture_fodder matches IMAGE values in base year

    aglu.MODEL_MeatWaterContent_YEARS <- aglu.MODEL_PRICE_YEARS

    L105.an_Prod_Mt_ctry_C_Y %>%
      filter(year %in% aglu.MODEL_MeatWaterContent_YEARS) %>%
      left_join_error_no_match(AGLU_ctry %>% select(iso, IMAGEv3p2_region) %>% distinct, by = "iso") %>%
      group_by(commodity = GCAM_commodity, year, IMAGEv3p2_region) %>%
      summarize(Mt_Wet = sum(value)) %>%
      filter(commodity %in% unique(L100.IMAGE_an_meat_KDMT$commodity)) %>%
      left_join_error_no_match(L100.IMAGE_an_meat_KDMT %>%
                                 mutate(Mt_DM = value / 1000) %>% select(-value),
                               by = c("commodity", "year", "IMAGEv3p2_region")) %>%
      group_by(commodity, IMAGEv3p2_region) %>%
      summarize(Mt_DM = sum(Mt_DM), Mt_Wet = sum(Mt_Wet),
                WaterContent = 1- Mt_DM / Mt_Wet) %>%
      ungroup() %>%  group_by(commodity) %>%
      # Narrow the water content distribution across regions using 15 - 85 percentile values
      mutate(WC_QLow = quantile(WaterContent, 0.10),
             WC_QHigh = quantile(WaterContent, 0.80)) %>%
      ungroup() %>%
      mutate(WaterContent = pmax(WaterContent, WC_QLow),
             WaterContent = pmin(WaterContent, WC_QHigh)) %>%
      select(commodity, IMAGEv3p2_region, value = WaterContent)->
      L100.IMAGE_an_watercontent

    L100.IMAGE_an_meat_KDMT %>%
      # share meat output by system using animal system info
      left_join_error_no_match(L100.IMAGE_an_Prodmixfrac_Rimg_C_Y %>%
                                 rename(MixedShare = value),
                               by = c("year", "IMAGEv3p2_region", "commodity")) %>%
      mutate(Mixed = MixedShare * value,
             Pastoral = value - Mixed) %>%
      select(-MixedShare, -value) %>%
      gather(system, value, Mixed, Pastoral) %>%
      filter(!(commodity %in% c("Pork", "Poultry") & system == "Pastoral")) ->
      L100.IMAGE_an_meat_KDMT1

    L100.IMAGE_an_meat_KDMT1 %>%
      rename(output = value) %>%
      left_join_error_no_match(
        IMAGE_an_feed_bySystem1 %>%
          dplyr::group_by_at(vars(-input, -value)) %>%
          summarize(input = sum(value)) %>% ungroup(),
        by = c("year", "IMAGEv3p2_region", "commodity", "system")) %>%
      mutate(input = if_else(output == 0, 0, input),
             value = input / output) %>%
      replace_na(list(value = 0)) %>%
      select(-input, -output) ->
      L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y

    IMAGE_an_feed_bySystem1 %>%
      left_join_error_no_match(L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y %>%
                                 rename(IOCoef = value),
                               by = c("year", "IMAGEv3p2_region", "system", "commodity")) %>%
      # if IOCoef == 0, make feed = 0
      mutate(value = if_else(IOCoef == 0 & value != 0, 0, value)) %>%
      select(-IOCoef) %>%
    # Calculate feed shares
      dplyr::group_by_at(vars(-input, -value)) %>%
      mutate(value = value / sum(value), count = sum(dplyr::n()),
             value = if_else(is.na(value), 1/count, value)) %>%
      ungroup() %>%
      select(-count) ->
      L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y


    # Step 2 Mapping to all iso regions ----
    # Downscale IMAGE region-level data to all countries


    # Helper function: filter, repeat and add columns, join with iso data
    downscale_IMAGE_regions <- function(x, AGLU_ctry, by) {
      historical_data <- filter(x, year %in% aglu.AGLU_HISTORICAL_YEARS) %>% spread(year, value)

      x %>%
        ungroup %>% filter(IMAGEv3p2_region == first(x$IMAGEv3p2_region),
                           !year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
        # pick out a region but there's no significance to this choice; could be any
        repeat_add_columns(tibble::tibble(iso = sort(unique(AGLU_ctry$iso)))) %>%
        select(-IMAGEv3p2_region) %>%
        left_join_keep_first_only(select(AGLU_ctry, iso, IMAGEv3p2_region), by = "iso") %>%
        spread(year, value) %>%
        left_join(historical_data, by = by) %>%
        na.omit %>%
        gather_years %>%
        filter(year %in% aglu.AGLU_HISTORICAL_YEARS)
    }

    L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGEv3p2_region", "commodity", "system", "input")) ->
      L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y

    L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGEv3p2_region", "commodity", "system")) ->
      L100.IMAGE_an_FeedIO_ctry_C_Sys_Y

    L100.IMAGE_an_Prodmixfrac_Rimg_C_Y %>%
      downscale_IMAGE_regions(AGLU_ctry, by = c("IMAGEv3p2_region", "commodity")) ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y

    L100.IMAGE_an_watercontent %>%
      select(-value) %>%
      filter(IMAGEv3p2_region == first(L100.IMAGE_an_watercontent$IMAGEv3p2_region)) %>%
      repeat_add_columns(tibble::tibble(iso = sort(unique(AGLU_ctry$iso)))) %>%
      select(-IMAGEv3p2_region) %>%
      left_join_keep_first_only(select(AGLU_ctry, iso, IMAGEv3p2_region), by = "iso") %>%
      left_join(L100.IMAGE_an_watercontent, by = c("commodity", "IMAGEv3p2_region")) ->
      L100.IMAGE_an_watercontent_ctry_C


    # Produce outputs

    L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y %>%
      add_title("IMAGE feed fractions by country / commodity / system / feed type / year") %>%
      add_units("Unitless") %>%
      add_comments("Each IMAGE table extrapolated to all AGLU historical years and downscaled from IMAGE region to all AGLU countries") %>%
      add_legacy_name("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_feed_bySystem") ->
      L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y

    L100.IMAGE_an_FeedIO_ctry_C_Sys_Y %>%
      add_title("IMAGE input-output coefficients by country / commodity / system / year") %>%
      add_units("Unitless") %>%
      add_comments("Each IMAGE table extrapolated to all AGLU historical years and downscaled from IMAGE region to all AGLU countries") %>%
      add_legacy_name("L100.IMAGE_an_FeedIO_ctry_C_Sys_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_feed_bySystem",
                     "aglu/IMAGE/IMAGE_an_head_bySystem",
                     "aglu/IMAGE/IMAGE_an_meat") ->
      L100.IMAGE_an_FeedIO_ctry_C_Sys_Y

    L100.IMAGE_an_Prodmixfrac_ctry_C_Y %>%
      add_title("IMAGE mixed fractions by country / commodity / year") %>%
      add_units("Unitless") %>%
      add_comments("Each IMAGE table extrapolated to all AGLU historical years and downscaled from IMAGE region to all AGLU countries") %>%
      add_legacy_name("L100.IMAGE_an_Prodmixfrac_ctry_C_Y") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/IMAGE/IMAGE_an_head_bySystem",
                     "aglu/IMAGE/IMAGE_an_meat") ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y

    L100.IMAGE_an_watercontent_ctry_C %>%
      add_title("IMAGE and FAO implied water content in livestock products by country / commodity") %>%
      add_units("frac") %>%
      add_comments("IMAGE data is in dry tons while FAO is in wet tons. Implied water content is computed at IMAGE regions and mapped to all regions") %>%
      add_legacy_name("L100.IMAGE_an_watercontent_ctry_C") %>%
      add_precursors("aglu/AGLU_ctry",
                     "L105.an_Prod_Mt_ctry_C_Y",
                     "aglu/IMAGE/IMAGE_an_meat") ->
      L100.IMAGE_an_watercontent_ctry_C

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
