# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.FAO_preprocessing_OtherData
#'
#' Get FAO data ready for forestry, fertilizer, animal stock, and land cover
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.FAO_CL_kha}, \code{L100.FAO_fallowland_kha}, \code{L100.FAO_harv_CL_kha},
#' \code{L100.FAO_Fert_Cons_tN}, \code{L100.FAO_Fert_Prod_tN}, \code{L100.FAO_For_Exp_m3}, \code{L100.FAO_For_Imp_m3}, \code{L100.FAO_For_Prod_m3}.
#' @details Get FAO data ready for forestry, fertilizer, animal stock, and land cover.
#' Calculate rolling five-year averages.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows distinct filter full_join left_join rename select add_row
#' @importFrom tidyr gather replace_na spread
#' @author BBL XZ 2022
module_aglu_L100.FAO_preprocessing_OtherData <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "common/iso_GCAM_regID",
      FILE = "aglu/FAO/GCAMFAOSTAT_ForProdTrade",
      FILE = "aglu/FAO/GCAMFAOSTAT_AnimalStock",
      FILE = "aglu/FAO/GCAMFAOSTAT_LandCover",
      FILE = "aglu/FAO/GCAMFAOSTAT_NFertilizer")

  MODULE_OUTPUTS <-
    c("L100.FAO_an_Stocks",
      "L100.FAO_an_Dairy_Stocks",
      "L100.FAO_CL_kha",
      "L100.FAO_fallowland_kha",
      "L100.FAO_harv_CL_kha",
      "L100.FAO_Fert_Cons_tN",
      "L100.FAO_Fert_Prod_tN",
      "L100.FAO_For_Exp_m3",
      "L100.FAO_For_Imp_m3",
      "L100.FAO_For_Prod_m3")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    iso <- FAO_country <- `country codes` <- `element codes` <- `item codes` <-
      year <- value <- countries <- country.codes <- item <- item.codes <-
      element <- element.codes <- `2011` <- NULL # silence package check.


    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)



    # A helper function to disaggregate dissolved region and join iso & GCAM region mappings ----
    # moving average over aglu.MODEL_MEAN_PERIOD_LENGTH
    # and keep years in aglu.AGLU_HISTORICAL_YEARS
    FAO_REG_YEAR_MAP <- function(.DF, MA_period = aglu.MODEL_MEAN_PERIOD_LENGTH){
      .DF %>%
        # disaggregate dissolved region
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL %>%
        # the iso mapping in AGLU_ctry works good now
        left_join_error_no_match(AGLU_ctry %>% distinct(area_code = FAO_country_code, iso), by = c("area_code")) %>%
        left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
        # Adding moving average
        dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
        mutate(value = if_else(is.na(Moving_average(value, periods = MA_period)),
                               value, Moving_average(value, periods = MA_period))) %>%
        ungroup() %>%
        filter(year %in% aglu.AGLU_HISTORICAL_YEARS)
    }

    #Currently using sum below since the forestry data is not processed via Xin's tool. Once that is done, revisit this.
    FAO_REG_YEAR_MAP_FOREST <- function(.DF, MA_period = aglu.MODEL_MEAN_PERIOD_LENGTH){
      .DF %>%
        # disaggregate dissolved region
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL %>%
        # the iso mapping in AGLU_ctry works good now
        left_join_error_no_match(AGLU_ctry %>% distinct(area_code = FAO_country_code, iso), by = c("area_code")) %>%
        left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
        filter(year %in% aglu.AGLU_HISTORICAL_YEARS)
    }


    # Section1. Animal stocks ----

    # assert that we have the right unit names
    # since FAO change "head" to "An"
    assertthat::assert_that(
      c("1000 An", "An") %in%
        (GCAMFAOSTAT_AnimalStock %>% distinct(unit) %>% pull) %>% all()
    )


    ##* L100.FAO_an_Stocks ----
    L100.FAO_an_Stocks <-
      GCAMFAOSTAT_AnimalStock %>%
      filter(element == "Stocks") %>%
      gather_years()%>%
      # filter out nonexist regions years due to gather e.g., USSR after 1991
      filter(!is.na(value)) %>%
      # change unit if 1000 head to head
      mutate(value = if_else(unit == "1000 An", value * 1000, value),
             unit = if_else(unit == "1000 An", "An", unit)) %>%
      FAO_REG_YEAR_MAP

    ##* L100.FAO_an_Dairy_Stocks ----
    L100.FAO_an_Dairy_Stocks <-
      GCAMFAOSTAT_AnimalStock %>%
      filter(element == "Milk Animals") %>%
      gather_years()%>%
      # filter out nonexist regions years due to gather e.g., USSR after 1991
      filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP

    ### Produce outputs ----
    L100.FAO_an_Stocks %>%
      add_title("FAO animal stocks country, item, year", overwrite = T) %>%
      add_units("Head/An") %>%
      add_comments("FAO animal stocks; unit of 1000 head were converted to head") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_AnimalStock",
                     "aglu/AGLU_ctry","common/iso_GCAM_regID") ->
      L100.FAO_an_Stocks

    L100.FAO_an_Dairy_Stocks %>%
      add_title("FAO dairy producing animal stocks country, item, year", overwrite = T) %>%
      add_units("Head/An") %>%
      add_comments("FAO dairy cow stocks") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_AnimalStock",
                     "aglu/AGLU_ctry","common/iso_GCAM_regID") ->
      L100.FAO_an_Dairy_Stocks


    # Section2. Forest supply and trade ----

    GCAMFAOSTAT_ForProdTrade %>%
      gather_years() %>%
      # NA area values that should not exist, e.g., USSR after 1991
      filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP ->
      L100.For_bal


    ##* L100.FAO_For_Prod_m3 ----
    L100.For_bal %>%
      filter(element == "Production")  %>%
      add_title("FAO forestry production by country, year") %>%
      add_comments("FAO primary roundwood production") %>%
      add_units("m3/t") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_ForProdTrade",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID") ->
      L100.FAO_For_Prod_m3


    ##* L100.FAO_For_Prod_m3 ----
    L100.For_bal %>%
      filter(element == "Export")  %>%
      add_title("FAO forestry export by country, year") %>%
      add_comments("FAO primary roundwood gross export") %>%
      add_units("m3/t") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_ForProdTrade",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID") ->
      L100.FAO_For_Exp_m3


    ##* L100.FAO_For_Imp_m3 ----
    L100.For_bal %>%
      filter(element == "Import")  %>%
      add_title("FAO forestry import by country, year") %>%
      add_comments("FAO primary roundwood gross import") %>%
      add_units("m3/t") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_ForProdTrade",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_For_Imp_m3

    rm(L100.For_bal,
       GCAMFAOSTAT_ForProdTrade)


    #Section3. Fertilizer and Land cover ----

    # assert that we have the right item/element names here
    assertthat::assert_that(
      c("Agricultural Use", "Production") %in%
        (GCAMFAOSTAT_NFertilizer %>% distinct(element) %>% pull) %>% all()
    )

    ##* L100.FAO_Fert_Cons_tN ----
    GCAMFAOSTAT_NFertilizer %>%
      filter(element == "Agricultural Use") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fertilizer consumption by country, year") %>%
      add_comments("FAO nitrogen N (total) consumption") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NFertilizer",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_Fert_Cons_tN


    ##* L100.FAO_Fert_Prod_tN ----
    GCAMFAOSTAT_NFertilizer %>%
      filter(element == "Production") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fertilizer production by country, year") %>%
      add_comments("FAO nitrogen N (total) production") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_NFertilizer",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_Fert_Prod_tN


    # assert that we have the right item names
    assertthat::assert_that(
      c("Arable land", "Temporary fallow", "Temporary crops") %in%
        (GCAMFAOSTAT_LandCover %>% distinct(item) %>% pull) %>% all()
    )

    ##* L100.FAO_CL_kha ----
    GCAMFAOSTAT_LandCover %>%
      filter(item == "Arable land") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO cropland area by country, year") %>%
      add_comments("FAO arable land") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_LandCover",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_CL_kha


    ##* L100.FAO_fallowland_kha ----
    GCAMFAOSTAT_LandCover %>%
      filter(item == "Temporary fallow") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fallow land area by country, year") %>%
      add_comments("FAO and with temporary fallow") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_LandCover",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID")->
      L100.FAO_fallowland_kha


    ##* L100.FAO_harv_CL_kha ----
    GCAMFAOSTAT_LandCover %>%
      filter(item == "Temporary crops") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO harvested cropland (temporary crops) area by country, year") %>%
      add_comments("FAO cropland cover") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMFAOSTAT_LandCover",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID")->
      L100.FAO_harv_CL_kha



    #*********************************************************

    # Return data ----
    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

# *******************************************************************************



