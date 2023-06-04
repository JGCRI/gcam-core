# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L133.water_demand_livestock
#'
#' Calculate livestock water coefficients by region ID / GCAM_commodity/ water type
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L133.water_demand_livestock_R_C_W_km3_Mt}. The corresponding file in the
#' original data system was \code{L133.water_demand_livestock.R} (water level1).
#' @details Water withdrawal and consumption coefficients by livestock using head count.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr replace_na
#' @author KRD November 2017
module_water_L133.water_demand_livestock <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "aglu/A_an_supplysector",
      "L105.an_Prod_Mt_R_C_Y",
      FILE = "water/LivestockWaterFootprint_MH2010",
      FILE = "water/FAO_an_items_Stocks",
      "L100.FAO_an_Stocks",
      "L100.FAO_an_Dairy_Stocks",
      "L103.water_mapping_R_B_W_Ws_share")

  MODULE_OUTPUTS <-
    c("L133.water_demand_livestock_R_C_W_km3_Mt",
      "L133.water_demand_livestock_R_B_W_km3")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    lapply(MODULE_INPUTS, function(d){
      # get name as the char after last /
      nm <- tail(strsplit(d, "/")[[1]], n = 1)
      # get data and assign
      assign(nm, get_data(all_data, d, strip_attributes = T),
             envir = parent.env(environment()))  })

    # Silence package checks
    year <- iso <- item <- value <- dairy.to.total <- dairy.adj <-
      coefficient <- GCAM_region_ID <- GCAM_commodity <- water.consumption <-
      water_type <- coefficient <- Coefficient <- water_sector <- share <-
      GCAM_basin_ID <- NULL

    # ===================================================
    # Calculate livestock water coefficients by region ID / GCAM_commodity/ water type.
    #
    # (XZ)Note that Milking cow was separated from cattle stock since they have
    # different water coefficients. However, layer chicker was not separated from
    # chicken stock since layer and boiler chicken had the same water coefficient.

    # Start by finding  the number of non-dairy producing livestock.

    # Create a tibble of dairy producing stocks by country and FAO animal
    # product name. Only use stock information from the year 2000 since that is the
    # year the water use coefficients are from. This tibble will be used in the next step to
    # remove dairy animals.
    L100.FAO_an_Dairy_Stocks %>%
      filter(year == 2000) %>%
      select(iso, item,item_code, value, year) %>%
      # item_code is used
      left_join_error_no_match(FAO_an_items_Stocks %>% select(item_code, dairy.to.total),
                               by = "item_code") %>%
      select(iso, item = dairy.to.total, dairy.adj = value, year) ->
      L133.dairy_an_adj

    # Adjust the total FAO animal stocks by removing the FAO dairy producing animals.
    # Assume the dairy stock has a value of zero if no data is available. The end
    # result is a count of non-dairy producing livestock.
    L100.FAO_an_Stocks %>%
      # Use left_join here because we do not expect a 1:1 match.
      left_join(L133.dairy_an_adj, by = c("item", "iso", "year")) %>%
      replace_na(list(dairy.adj = 0)) %>%
      mutate(value = value - dairy.adj) ->
      L133.FAO_an_heads

    # It seems the PDR stoped reporting data after 1994 for total livestock, this causes the
    # count of non-dairy producing livestock to be negative. For now set any negative
    # count of non-dairy producing livestock to zero.
    L133.FAO_an_heads <- mutate(L133.FAO_an_heads, value = if_else(value < 0, 0, value))

    # Now combine the nondairy producing livestock and dairy producing livestock information
    # into a single tibble. Subsest for the year 2000 since that is the year the water use
    # coefficients are from.
    L133.FAO_an_heads %>%
      select(iso, item, item_code, year, value) %>%
      bind_rows(L100.FAO_an_Dairy_Stocks %>%
                  select(iso, item, item_code, year, value)) %>%
      filter(year == 2000) ->
      L133.FAO_an_heads


    # Now calculate the water demand by FAO item.
    #
    # Add FAO stock information, livestock water use coefficient, and GCAM information to the dairy and non-dairy
    # livestock count form mapping files.
    L133.FAO_an_heads %>%
      # A 1:1 match is not expected and we do not want NAs introduced to the data frame so
      # use inner join here.
      inner_join(FAO_an_items_Stocks %>% select(item_code, Animal, GCAM_commodity), by = "item_code") %>%
      # A 1:1 match is not expected and we do not want NAs introduced to the data frame so
      # use inner join here.
      inner_join(LivestockWaterFootprint_MH2010, by = "Animal") %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") ->
      L133.FAO_an_heads

    # Multiply the livestock count by the livestock water use coefficient from Mekonnen and Hoekstra 2010.
    #
    # Since the Mekonnen and Hoekstra 2010 coefficient is in liters/head per day convert from L to m^3 per
    # 1000 heads by dividing by 1000 and then convert from daily consumption to per year.
    L133.FAO_an_heads %>%
      mutate(water.consumption = value * Coefficient / 1000 / CONV_DAYS_YEAR) ->
      L133.FAO_an_heads


    # Calculate water demand by GCAM_commodity
    #
    # Aggregate the livestock water consumption by GCAM region and commodity.
    L133.FAO_an_heads %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(water.consumption = sum(water.consumption)) %>%
      ungroup ->
      L133.water_demand_livestock_R_C_W_km3_Mt

    # Add FAO production information to the tibble of aggregated livestock water consumption.
    # Modification to set of commodities included (GPK) - the production data at this stage includes OtherMeat_Fish,
    # and the M+H data have an estimate for "Horses" which is being used as a proxy for this commodity class. Obviously
    # horses' water demands are a poor proxy for the remainder of the commodity class, which is mostly fish with some
    # waste products. This is explicitly excluded at this stage.
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      left_join_error_no_match(L105.an_Prod_Mt_R_C_Y %>%
                                 filter(year == 2000) %>%
                                 select(GCAM_region_ID, GCAM_commodity, year, value),
        by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      filter(GCAM_commodity %in% A_an_supplysector$supplysector) ->
      L133.water_demand_livestock_R_C_W_km3_Mt

    # Average the aggregated livestock water consumption by the total production. Since water
    # consumption is in m^3 and production is in Mt to km^3/Mt we must divide by 1e9.
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      mutate(coefficient = water.consumption / value / 1e9) ->
      L133.water_demand_livestock_R_C_W_km3_Mt


    # Water withdrawals are assumed to be the same as consumption.
    #
    # Add the water type information to the livestock water demand tibble. Add water type information
    # to the tibble, since the water withdrawals are the same as consumption for livestock use the same
    # coefficients for the water withdrawals and water consumption.
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) ->
      L133.water_demand_livestock_R_C_W_km3_Mt

    # Select the columns to output.
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      select(GCAM_region_ID, GCAM_commodity, water_type, coefficient) ->
      L133.water_demand_livestock_R_C_W_km3_Mt

    # Final step - write out the water demands by basin
    L133.water_demand_livestock_R_B_W_km3 <- L105.an_Prod_Mt_R_C_Y %>%
      inner_join(L133.water_demand_livestock_R_C_W_km3_Mt,
                by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      left_join(filter(L103.water_mapping_R_B_W_Ws_share, water_sector == "Livestock"),
                by = c("GCAM_region_ID", "water_type")) %>%
      mutate(value = value * coefficient * share) %>%
      group_by(GCAM_region_ID, GCAM_basin_ID, water_type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()



    # ===================================================

    # Produce outputs
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      add_title("Livestock water coefficients by region ID / GCAM_commodity/ water type") %>%
      add_units("coefficient = m^3 / Mt") %>%
      add_comments("Separate non-dairy and dairy producing livestock and multiply by the Mekonnen and Hoekstra 2010 livestock water use coefficient.") %>%
      add_comments("Aggregate the life stock water consumption by GCAM region livestock production.") %>%
      add_legacy_name("L133.water_demand_livestock_R_C_W_km3_Mt") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L105.an_Prod_Mt_R_C_Y",
                     "water/LivestockWaterFootprint_MH2010",
                     "water/FAO_an_items_Stocks",
                     "L100.FAO_an_Stocks",
                     "L100.FAO_an_Dairy_Stocks") ->
      L133.water_demand_livestock_R_C_W_km3_Mt

    L133.water_demand_livestock_R_B_W_km3 %>%
      add_title("Livestock water demands by region ID / GCAM_commodity/ water type / year") %>%
      add_units("km^3") %>%
      add_comments("Calculated by multiplying commodity production by water demand coefficients and basin-wise shares") %>%
      same_precursors_as(L133.water_demand_livestock_R_C_W_km3_Mt) %>%
      add_precursors("L103.water_mapping_R_B_W_Ws_share",
                     "aglu/A_an_supplysector") ->
      L133.water_demand_livestock_R_B_W_km3

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
