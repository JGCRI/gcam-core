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
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_AnimalStock_202Regs_22Items_1973to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_NFertilizerProdDemand_175Regs_1Item_1973to2020")

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
        left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
        left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
        # Adding moving average
        dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
        mutate(value = if_else(is.na(Moving_average(value, periods = MA_period)),
                               value, Moving_average(value, periods = MA_period))) %>%
        ungroup() %>%
        filter(year %in% aglu.AGLU_HISTORICAL_YEARS)
    }


    # Section1. Animal stocks ----
    # corrected the unit issue in the old data
    # unit should be head (except beehive, which is not used)

    ##* L100.FAO_an_Stocks ----
    L100.FAO_an_Stocks <-
      GCAMDATA_FAOSTAT_AnimalStock_202Regs_22Items_1973to2020 %>%
      filter(element == "Stocks") %>%
      gather_years()%>%
      # filter out nonexist regions years due to gather e.g., USSR after 1991
      filter(!is.na(value)) %>%
      # change unit if 1000 head to head
      mutate(value = if_else(unit == "1000 Head", value * 1000, value),
             unit = if_else(unit == "1000 Head", "Head", unit)) %>%
      FAO_REG_YEAR_MAP

    ##* L100.FAO_an_Dairy_Stocks ----
    L100.FAO_an_Dairy_Stocks <-
      GCAMDATA_FAOSTAT_AnimalStock_202Regs_22Items_1973to2020 %>%
      filter(element == "Milk Animals") %>%
      gather_years()%>%
      # filter out nonexist regions years due to gather e.g., USSR after 1991
      filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP

    ### Produce outputs ----
    L100.FAO_an_Stocks %>%
      add_title("FAO animal stocks country, item, year", overwrite = T) %>%
      add_units("number") %>%
      add_comments("FAO animal stocks; unit of 1000 head were converted to head") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_AnimalStock_202Regs_22Items_1973to2020",
                     "aglu/AGLU_ctry","common/iso_GCAM_regID") ->
      L100.FAO_an_Stocks

    L100.FAO_an_Dairy_Stocks %>%
      add_title("FAO dairy producing animal stocks country, item, year", overwrite = T) %>%
      add_units("Head") %>%
      add_comments("FAO dairy cow stocks") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_AnimalStock_202Regs_22Items_1973to2020",
                     "aglu/AGLU_ctry","common/iso_GCAM_regID") ->
      L100.FAO_an_Dairy_Stocks


    # Section2. Forest supply and trade ----

    GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020 %>%
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
      add_units("m3") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID") ->
      L100.FAO_For_Prod_m3


    ##* L100.FAO_For_Prod_m3 ----
    L100.For_bal %>%
      filter(element == "Export")  %>%
      add_title("FAO forestry export by country, year") %>%
      add_comments("FAO primary roundwood gross export") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
                     "aglu/AGLU_ctry",
                     "common/iso_GCAM_regID") ->
      L100.FAO_For_Exp_m3


    ##* L100.FAO_For_Imp_m3 ----
    L100.For_bal %>%
      filter(element == "Import")  %>%
      add_title("FAO forestry import by country, year") %>%
      add_comments("FAO primary roundwood gross import") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_For_Imp_m3

    rm(L100.For_bal,
       GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020)


    #Section3. Fertilizer and Land cover ----

    ##* L100.FAO_Fert_Cons_tN ----
    GCAMDATA_FAOSTAT_NFertilizerProdDemand_175Regs_1Item_1973to2020 %>%
      filter(element == "Agricultural Use") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fertilizer consumption by country, year") %>%
      add_comments("FAO nitrogen N (total) consumption") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_NFertilizerProdDemand_175Regs_1Item_1973to2020",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_Fert_Cons_tN


    ##* L100.FAO_Fert_Prod_tN ----
    GCAMDATA_FAOSTAT_NFertilizerProdDemand_175Regs_1Item_1973to2020 %>%
      filter(element == "Production") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fertilizer production by country, year") %>%
      add_comments("FAO nitrogen N (total) production") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_NFertilizerProdDemand_175Regs_1Item_1973to2020",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_Fert_Prod_tN


    ##* L100.FAO_CL_kha ----
    GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020 %>%
      filter(item == "Arable land") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO cropland area by country, year") %>%
      add_comments("FAO arable land") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID") ->
      L100.FAO_CL_kha


    ##* L100.FAO_fallowland_kha ----
    GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020 %>%
      filter(item == "Land with temporary fallow") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO fallow land area by country, year") %>%
      add_comments("FAO and with temporary fallow") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020",
                     "aglu/AGLU_ctry", "common/iso_GCAM_regID")->
      L100.FAO_fallowland_kha


    ##* L100.FAO_harv_CL_kha ----
    GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020 %>%
      filter(item == "Land under temporary crops") %>%
      gather_years() %>% filter(!is.na(value)) %>%
      FAO_REG_YEAR_MAP %>%
      add_title("FAO harvested cropland (temporary crops) area by country, year") %>%
      add_comments("FAO cropland cover") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/GCAMDATA_FAOSTAT_LandCover_229Regs_3Covers_1973to2020",
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



