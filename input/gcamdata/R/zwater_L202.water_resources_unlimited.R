# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L202.water_resources_unlimited
#'
#' Create unlimited resource markets for water types, and read in fixed prices for water types.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.UnlimitRsrc_mapped}, \code{L202.UnlimitRsrc_nonmapped},
#' \code{L202.UnlimitRsrcPrice_mapped}, \code{L202.UnlimitRsrcPrice_nonmapped}. The corresponding file in the
#' original data system was \code{L202.water_resources_unlimited.R} (water level2).
#' @details Create unlimited resource markets (i.e., 32 GCAM regions) for water types (i.e., water consumption, withdrawals, biophysical water consumption and seawater),
#' and read in fixed prices for water types.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter mutate select
#' @author YL July 2017 / ST Oct 2018
module_water_L202.water_resources_unlimited <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "water/basin_to_country_mapping",
             "L102.unlimited_mapped_water_price_B_W_Y_75USDm3",
             "L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.UnlimitRsrc_mapped",
             "L202.Rsrc_mapped",
             "L202.UnlimitRsrc_nonmapped",
             "L202.UnlimitRsrcPrice_mapped",
             "L202.UnlimitRsrcPrice_nonmapped"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    year <- GCAM_region_ID <- water_type <- region <- unlimited.resource <- output.unit <- price.unit <-
      market <- capacity.factor <- value <- price <- GLU <- GCAM_basin_ID <- basin_name <- GLU_name <-
      is_unlimited <- resource <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping") %>%
      select(GCAM_basin_ID, basin_name = GLU_name)
    L102.unlimited_mapped_water_price_B_W_Y_75USDm3 <- get_data(all_data, "L102.unlimited_mapped_water_price_B_W_Y_75USDm3", strip_attributes = TRUE)
    L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 <- get_data(all_data, "L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3", strip_attributes = TRUE)
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share", strip_attributes = TRUE)

    # Create a list of region + basin that actually exist with names
    # Use left join to ensure only those basins contained in GCAM regions are included
    bind_rows(L103.water_mapping_R_GLU_B_W_Ws_share %>% rename(GCAM_basin_ID = GLU),
              L103.water_mapping_R_B_W_Ws_share) %>%
      select(GCAM_region_ID, GCAM_basin_ID, water_type) %>%
      distinct() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
      # determine which region will be the UnlimitedResource and the rest will only
      # have a Resource shell for the purposes of adding that Region to market
      # NOTE: it is important that the UnlimitedResource goes last to ensure the
      # market does not get solved
      arrange(GCAM_region_ID) %>%
      group_by(water_type, basin_name) %>%
      mutate(is_unlimited = region == dplyr::last(region)) %>%
      ungroup() ->
      L202.region_basin

    # Create unlimied research markets for mapped water types
    L202.region_basin %>%
      mutate(unlimited.resource = paste(basin_name, water_type, sep = "_"),
             output.unit = water.WATER_UNITS_QUANTITY,
             price.unit = water.WATER_UNITS_PRICE,
             capacity.factor = 1) %>%
      ## ^^ capacity factor is not used for water resources
      mutate(market = basin_name) ->
      L202.UnlimitRsrc_mapped

    # partition resources such only one region per basin and water type is an unlimited
    # resource and the rest only exist as regular resource to add the region to market
    L202.UnlimitRsrc_mapped %>%
      filter(!is_unlimited) %>%
      rename(resource = unlimited.resource) %>%
      select(LEVEL2_DATA_NAMES[["Rsrc"]]) ->
      L202.Rsrc_mapped
    L202.UnlimitRsrc_mapped %>%
      filter(is_unlimited) %>%
      select(LEVEL2_DATA_NAMES[["UnlimitRsrc"]]) ->
      L202.UnlimitRsrc_mapped


    # Read in fixed prices for mapped water types
    L202.region_basin %>%
      # filter to only the region + basin + water type that will actually be unlimited
      filter(is_unlimited) %>%
      # Left join ensures only those basins in use get prices
      left_join(L102.unlimited_mapped_water_price_B_W_Y_75USDm3,
                by = c("GCAM_basin_ID", "water_type")) %>%
      mutate(unlimited.resource = paste(basin_name, water_type, sep = "_")) %>%
      arrange(region, unlimited.resource) %>%
      select(one_of(LEVEL2_DATA_NAMES$UnlimitRsrcPrice)) ->
      L202.UnlimitRsrcPrice_mapped

    # Create unlimited resource markets for non-mapped water types
    L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 %>%
      select(GCAM_region_ID, water_type) %>%
      distinct() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(market = region) %>%
      rename(unlimited.resource = water_type) %>%
      mutate(output.unit = water.WATER_UNITS_QUANTITY,
             price.unit = water.WATER_UNITS_PRICE,
             capacity.factor = 1) %>%
      select(one_of(LEVEL2_DATA_NAMES[["UnlimitRsrc"]])) %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS) |
               !(unlimited.resource %in% water.AG_ONLY_WATER_TYPES)) ->
    # ^^ remove water goods that are used only by ag technologies, in regions with no aglu module
    L202.UnlimitRsrc_nonmapped

    # Read in fixed prices for non-mapped water types
    GCAM_region_names %>%
      # Left join ensures only those basins in use get prices
      left_join(L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3, by = "GCAM_region_ID") %>%
      rename(unlimited.resource = water_type) %>%
      select(one_of(LEVEL2_DATA_NAMES$UnlimitRsrcPrice)) %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS) |
               !(unlimited.resource %in% water.AG_ONLY_WATER_TYPES)) ->
    # ^^ remove water goods that are used only by ag technologies, in regions with no aglu module
      L202.UnlimitRsrcPrice_nonmapped

    #========================================================================


    # Produce outputs
    L202.UnlimitRsrc_mapped %>%
      add_title("Unlimited water resources (mapped)") %>%
      add_units("NA") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrc") %>%
      add_precursors("common/GCAM_region_names",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "water/basin_to_country_mapping") ->
      L202.UnlimitRsrc_mapped

    L202.Rsrc_mapped %>%
      add_title("water resources (mapped) used to add region to basin/market") %>%
      add_units("NA") %>%
      add_comments("This table is used to ensure a basin which are shared between regions") %>%
      add_comments("has only one unlimited resource object but the additional regions are") %>%
      add_comments("still included in that market") %>%
      same_precursors_as(L202.UnlimitRsrc_mapped) ->
      L202.Rsrc_mapped

    L202.UnlimitRsrcPrice_mapped %>%
      add_title("Price for unlimited water resources (mapped)") %>%
      add_units("1975$/m^3") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrcPrice") %>%
      add_precursors("common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "L102.unlimited_mapped_water_price_B_W_Y_75USDm3",
                     "water/basin_to_country_mapping") ->
      L202.UnlimitRsrcPrice_mapped

    L202.UnlimitRsrc_nonmapped %>%
      add_title("Unlimited water resources (non-mapped)") %>%
      add_units("NA") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrc") %>%
      add_precursors("common/GCAM_region_names",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "water/basin_to_country_mapping") ->
      L202.UnlimitRsrc_nonmapped

    L202.UnlimitRsrcPrice_nonmapped %>%
      add_title("Price for unlimited water resources (non-mapped)") %>%
      add_units("1975$/m^3") %>%
      add_comments("The removed record is biophysical water consumption for Taiwan") %>%
      add_legacy_name("L202.UnlimitRsrcPrice") %>%
      add_precursors("common/GCAM_region_names",
                     "L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3",
                     "water/basin_to_country_mapping") ->
      L202.UnlimitRsrcPrice_nonmapped

    return_data(L202.UnlimitRsrc_mapped, L202.Rsrc_mapped, L202.UnlimitRsrcPrice_mapped,
                L202.UnlimitRsrc_nonmapped, L202.UnlimitRsrcPrice_nonmapped)
  } else {
    stop("Unknown command")
  }
}
