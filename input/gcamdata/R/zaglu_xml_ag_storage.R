# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_storage_xml
#'
#' Construct XML data structure for \code{ag_storage.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_storage.xml}.
#' @author XZ 2023
module_aglu_batch_ag_storage_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L113.StorageTechAndPassThrough")

  MODULE_OUTPUTS <-
    c(XML = "ag_storage.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 0. data ready ----
    L113.StorageTechAndPassThrough %>%
      filter(storage_model == TRUE) ->
      L113.StorageTechTable

    L113.StorageTechAndPassThrough %>%
      filter(storage_model == FALSE) %>%
      rename(technology = food.storage.technology) %>%
      mutate(technology = "no-storage-pass-through",
             minicam.non.energy.input = "border-cost", input.cost = 0) ->
      L113.PassThroughTable

    # 0. Shared tables

    # Pull region and sector to prepare for tables for xml generating
    L113.StorageTechAndPassThrough %>%
      distinct(region, supplysector, food.storage.technology) ->
      L113.ag_Storage_region_supplysector

    # Tables for xmls
    supplysec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       output.unit = "Mt", input.unit = "Mt",
                       price.unit = "1975$/kg", logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = -3, logit.type = NA)

    subsec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector, logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       year.fillout = min(MODEL_BASE_YEARS), share.weight = 1)

    subsec_shwt <-
      L113.StorageTechAndPassThrough %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])


    # 1. tables ready for StorageTech commodities ----

    fst_interp <-
      L113.StorageTechTable %>%
      distinct(region, supplysector, food.storage.technology) %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology,
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])


    fst_extra <-
      L113.StorageTechTable %>%
      select(LEVEL2_DATA_NAMES[["FoodTech"]]) %>%
      bind_rows(
        L113.StorageTechTable %>%
          select(LEVEL2_DATA_NAMES[["FoodTech"]]) %>%
          # Add cost to future years as the last base year
          filter(year == min(MODEL_FUTURE_YEARS)) %>% select(-year) %>%
          # Note that the first future year was included above
          repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS[-1])) %>%
          mutate(opening.stock = 0, closing.stock = 0) %>%
          filter(!is.na(region))
      )


    fst_coef <-
      L113.StorageTechTable %>%
      mutate(coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    fst_coef <-
      fst_coef %>%
      bind_rows(
        fst_coef %>%
          # Add cost to future years as the last base year
          filter(year == min(MODEL_FUTURE_YEARS)) %>% select(-year) %>%
          # Note that the first future year was included above
          repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS[-1])) %>%
          filter(!is.na(region))
      )

    # The cost here is not really useful for storage. It is cost to main product
    # fst_cost <-
    #   L113.StorageTechTable %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
    #   select(LEVEL2_DATA_NAMES[["FoodTechCost"]])

    fst_RESSecOut <-
      L113.StorageTechTable %>%
      mutate(res.secondary.output = GCAM_commodity,
             output.ratio = 1, pMultiplier = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechRESSecOut"]])

    fst_RESSecOut <-
      fst_RESSecOut %>%
        bind_rows(
          fst_RESSecOut %>%
            # Add cost to future years as the last base year
            filter(year == min(MODEL_FUTURE_YEARS)) %>% select(-year) %>%
            # Note that the first future year was included above
            repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS[-1])) %>%
            filter(!is.na(region))
        )


    # 2. tables ready for PassThrough commodities ----


    passthrough_interp <-
      L113.PassThroughTable %>%
      distinct(region, supplysector, technology) %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       technology,
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["TechInterp"]])

    passthrough_techshrwt <-
      L113.PassThroughTable %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    passthrough_coef <-
      L113.PassThroughTable %>%
      mutate(coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    passthrough_cost <-
      L113.PassThroughTable %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])


    # Produce outputs ----
    ag_storage.xml <-
      create_xml("ag_storage.xml") %>%
      add_logit_tables_xml(supplysec, "Supplysector") %>%
      add_logit_tables_xml(subsec, "SubsectorLogit") %>%
      add_xml_data(subsec_sw, "SubsectorShrwtFllt") %>%
      add_xml_data(subsec_shwt, "SubsectorShrwt") %>%
      add_xml_data(fst_interp, "FoodTechInterp") %>%
      add_xml_data(fst_extra, "FoodTech") %>%
      #add_xml_data(fst_cost, "FoodTechCost") %>%
      add_xml_data(fst_coef, "FoodTechCoef") %>%
      add_xml_data(fst_RESSecOut, "FoodTechRESSecOut") %>%
      add_xml_data(passthrough_interp, "TechInterp") %>%
      add_xml_data(passthrough_techshrwt, "TechShrwt") %>%
      add_xml_data(passthrough_coef, "TechCoef") %>%
      add_xml_data(passthrough_cost, "TechCost") %>%
      add_precursors("L113.StorageTechAndPassThrough")


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
