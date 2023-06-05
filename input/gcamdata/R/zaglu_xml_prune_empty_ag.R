# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_prune_empty_ag_xml
#'
#' Construct XML data structure for \code{prune_empty_ag.xml}.  Through the
#' process of processing the myriad of AgLu data we end up with many combinations
#' of crop/tech and land node/leaf which have zero production / land allocation
#' in any historical year.  Given, with the exception of biomass, having no historical
#' values means zero future potential these combinations are just wasted space.  Ideally,
#' this would be filtered out in the data processing saving time and reducing XML size.
#' However, doing so would be fairly intrusive as it will touch hundreds of objects and
#' in some cases we will prune pretty far up the tree making it less than straightforward.
#' Thus this minimally intrusive approach: Instruct GCAM to delete=1 the "empty" crop/tech
#' land node/leaf in the last scenario component parsed (note: it is safe to parse this XML
#' multiple times, useful in large batch runs where custom inputs may need pruning too)
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{prune_empty_ag.xml}. (aglu XML).
module_aglu_prune_empty_ag_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2012.AgProduction_ag_irr_mgmt",
             # in case we prune so far that a region no longer has a crop to trade
             "L240.TechCoef_tra",
             "L240.TechCoef_reg",
             # in case we prune so far that we need to remove feed options (FodderGrass)
             "L202.StubTech_in",
             "L203.StubTech_demand_nonfood",
             "L2252.LN5_MgdAllocation_crop",
             "L2252.LN5_MgdCarbon_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "prune_empty_ag.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L240.TechCoef_tra <- get_data(all_data, "L240.TechCoef_tra")
    L240.TechCoef_reg <- get_data(all_data, "L240.TechCoef_reg")
    L202.StubTech_in <- get_data(all_data, "L202.StubTech_in")
    L203.StubTech_demand_nonfood <- get_data(all_data, "L203.StubTech_demand_nonfood")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2252.LN5_MgdCarbon_crop <- get_data(all_data, "L2252.LN5_MgdCarbon_crop")

    # ===================================================

    # start with Ag supply techs picking out instances of zeros across all historical years
    # then we will continually aggregate up to subsector then to sector doing the same checks
    L2012.AgProduction_ag_irr_mgmt %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
      summarize(calOutputValue = sum(calOutputValue)) ->
      # leave it grouped, this is a case where popping groups one at a time is exactly what we want
      prune_agsupply

    # save the empty techs
    prune_agsupply %>%
      filter(calOutputValue == 0) %>%
      select(-calOutputValue) ->
      empty_ag_tech

    # aggregate up
    prune_agsupply %>%
      summarize(calOutputValue = sum(calOutputValue)) ->
      prune_agsupply
    # save the empty subsectors
    prune_agsupply %>%
      filter(calOutputValue == 0) %>%
      select(-calOutputValue) ->
      empty_ag_subsec

    # aggregate up
    prune_agsupply %>%
      summarize(calOutputValue = sum(calOutputValue)) ->
      prune_agsupply
    # save the empty sectors
    prune_agsupply %>%
      filter(calOutputValue == 0, AgSupplySector != "FodderHerb") %>%
      select(-calOutputValue) ->
      empty_ag_sec

    # if we removed the sector we need to clean up a few more places such as trading
    # markets

    # do some renaming to simply the following processing
    empty_ag_sec %>%
      ungroup() %>%
      select(minicam.energy.input = AgSupplySector,
             market.name = region) ->
      prune_agsupply

    # do not attempt to export a region+crop that is empty
    L240.TechCoef_tra %>%
      inner_join(prune_agsupply, by=c("minicam.energy.input", "market.name")) %>%
      select(region, supplysector, subsector) %>%
      distinct() ->
      empty_ag_tra

    # do not attempt to get domestic consumption in a region+crop that is empty (imports
    # are left alone, although perhaps checking if those are empty too could lead to further
    # pruning but it is diminishing returns in terms of resources saved)
    L240.TechCoef_reg %>%
      inner_join(prune_agsupply, by=c("minicam.energy.input", "market.name")) %>%
      select(region, supplysector, subsector) %>%
      distinct() ->
      empty_ag_reg

    # Fodder grass is a special case where if a region didn't have it and we do
    # not clean it up from the feed sector it will result in errors thus we need to
    # do that now
    prune_agsupply %>%
      select(region = market.name,
             subsector = minicam.energy.input) %>%
      filter(subsector == "FodderGrass") ->
      prune_fodder_grass

    L202.StubTech_in %>%
      select(region, supplysector, subsector) %>%
      bind_rows(L203.StubTech_demand_nonfood %>% select(region, supplysector, subsector)) %>%
      inner_join(prune_fodder_grass, by=c("region", "subsector")) %>%
      distinct() ->
      empty_foddergrass

    ###
    # switching to the land side checking for zero land allocation across all historical years
    # then pruning up the nesting hierarchy as many levels as it goes that are empty

    LandNode_columns <- names(L2252.LN5_MgdAllocation_crop)[grepl('LandNode', names(L2252.LN5_MgdAllocation_crop))]
    LandNode_MaxDepth <- length(LandNode_columns)

    # start with some error checking that non-zero land matches non-zero supply
    L2252.LN5_MgdCarbon_crop %>%
      select(region, LandAllocatorRoot, matches('LandNode'), LandLeaf) %>%
      repeat_add_columns(tibble(year=MODEL_BASE_YEARS)) %>%
      # some empty land node/leaves appear because we have read in carbon information
      # for them but they do not appear in the historical land allocation table (L2252.LN5_MgdAllocation_crop)
      # so join them here and fill zeros so we can clean them up together with the
      # rest of the zero land node/leaves
      left_join(L2252.LN5_MgdAllocation_crop, by=c("region", "LandAllocatorRoot", LandNode_columns, "LandLeaf", "year")) %>%
      mutate(allocation = if_else(is.na(allocation), 0, allocation)) %>%
      left_join_error_no_match(L2012.AgProduction_ag_irr_mgmt %>%
                  select(region, AgProductionTechnology, year, calOutputValue),
                by=c("region", "LandLeaf" = "AgProductionTechnology", "year")) %>%
      filter((allocation <= 0 & calOutputValue > 0) | (calOutputValue <= 0 & allocation > 0)) ->
      mismath_double_check
    assertthat::assert_that(nrow(mismath_double_check) == 0)

    # prepare the historical land areas for our empty checks
    L2252.LN5_MgdCarbon_crop %>%
      select(region, LandAllocatorRoot, matches('LandNode'), LandLeaf) %>%
      repeat_add_columns(tibble(year=MODEL_BASE_YEARS)) %>%
      # some empty land node/leaves appear because we have read in carbon information
      # for them but they do not appear in the historical land allocation table (L2252.LN5_MgdAllocation_crop)
      # so join them here and fill zeros so we can clean them up together with the
      # rest of the zero land node/leaves
      left_join(L2252.LN5_MgdAllocation_crop, by=c("region", "LandAllocatorRoot", LandNode_columns, "LandLeaf", "year")) %>%
      mutate(allocation = if_else(is.na(allocation), 0, allocation)) %>%
      dplyr::group_by_at(vars(-year, -allocation)) %>%
      summarize(allocation = sum(allocation)) ->
      # leave it grouped, this is a case where popping groups one at a time is exactly what we want
      prune_data

    # because we are going to need to go up an unknown number of land node levels it is
    # easier to just process this recursively in a function
    recursive_add_landnode_delete <- function(prune_data, xml, level) {
      # aggregate up
      prune_data_new = prune_data %>% summarize(allocation = sum(allocation))
      # look for empty nests
      empty_nest = prune_data_new %>% filter(allocation == 0)
      if(nrow(empty_nest) > 0) {
        # just add the LNi_Delete to the XML directly, no need for extra book keeping
        header = paste0('LN', level, "_Delete")
        xml_back = add_xml_data(xml, empty_nest, header)
        # recursively process up the nesting tree
        recursive_add_landnode_delete(prune_data_new, xml_back, level -1) ->
          xml_back
      }
      else {
        # no longer have any empty nodes, so no need to check any further up
        xml_back = xml
      }
      return(xml_back)
    }

    # produce output
    create_xml("prune_empty_ag.xml") %>%
      # now call the function to recursively find the empty land node/leaf to prune
      recursive_add_landnode_delete(prune_data, ., LandNode_MaxDepth) %>%
      # add the LandNode rename table
      add_rename_landnode_xml() %>%
      # add on the tables from the Ag supply side
      add_xml_data(empty_ag_tech, "AgTechDelete", "AgTech") %>%
      add_xml_data(empty_ag_subsec %>% rename(supplysector = AgSupplySector, subsector = AgSupplySubsector), "DeleteSubsector") %>%
      add_xml_data(empty_ag_sec %>% rename(supplysector = AgSupplySector), "DeleteSupplysector") %>%
      add_xml_data(empty_ag_tra, "DeleteSubsector") %>%
      add_xml_data(empty_ag_reg, "DeleteSubsector") %>%
      add_xml_data(empty_foddergrass, "DeleteSubsector") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt",
                     "L240.TechCoef_tra",
                     "L240.TechCoef_reg",
                     "L202.StubTech_in",
                     "L203.StubTech_demand_nonfood",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2252.LN5_MgdCarbon_crop") ->
      prune_empty_ag.xml

    return_data(prune_empty_ag.xml)
  } else {
    stop("Unknown command")
  }
}
