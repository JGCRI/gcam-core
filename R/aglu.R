# aglu.R


#' module_aglu_LA100.FAO_downscale_ctry
#'
#' Construct the \code{gcam-usa} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
module_aglu_LA100.FAO_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.FAO_ag_HA_ha",
             "L100.FAO_ag_Prod_t",
             "L100.FAO_ag_Exp_t",
             "L100.FAO_ag_Feed_t",
             "L100.FAO_ag_Food_t",
             "L100.FAO_ag_Imp_t",
             "L100.FAO_an_Exp_t",
             "L100.FAO_an_Food_t",
             "L100.FAO_an_Imp_t",
             "L100.FAO_an_Prod_t",
             "L100.FAO_CL_kha",
             "L100.FAO_fallowland_kha",
             "L100.FAO_harv_CL_kha",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L100.FAO_For_Exp_m3",
             "L100.FAO_For_Imp_m3",
             "L100.FAO_For_Prod_m3"))
  } else if(command == driver.DECLARE_INPUTS) {
    return(c("aglu/AGLU_ctry",
             "aglu/FAO_ag_HA_ha_PRODSTAT",
             "aglu/FAO_ag_Prod_t_PRODSTAT",
             "aglu/FAO_ag_Exp_t_SUA",
             "aglu/FAO_ag_Feed_t_SUA",
             "aglu/FAO_ag_Food_t_SUA",
             "aglu/FAO_ag_Imp_t_SUA",
             "aglu/FAO_an_Exp_t_SUA",
             "aglu/FAO_an_Food_t_SUA",
             "aglu/FAO_an_Imp_t_SUA",
             "aglu/FAO_an_Prod_t_SUA",
             "aglu/FAO_CL_kha_RESOURCESTAT",
             "aglu/FAO_fallowland_kha_RESOURCESTAT",
             "aglu/FAO_harv_CL_kha_RESOURCESTAT",
             "aglu/FAO_Fert_Cons_tN_RESOURCESTAT_archv",
             "aglu/FAO_Fert_Cons_tN_RESOURCESTAT",
             "aglu/FAO_Fert_Prod_tN_RESOURCESTAT_archv",
             "aglu/FAO_Fert_Prod_tN_RESOURCESTAT",
             "aglu/FAO_For_Exp_m3_FORESTAT",
             "aglu/FAO_For_Imp_m3_FORESTAT",
             "aglu/FAO_For_Prod_m3_FORESTAT"))
  } else if(command == driver.MAKE) {
    aglu_LA100.FAO_downscale_ctry_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' aglu_LA100.FAO_downscale_ctry_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all aglu data.
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom tidyr gather spread
#' @export
aglu_LA100.FAO_downscale_ctry_makedata <- function(all_data) {
  # printlog("Historical agricultural data from the FAO downscaled to modern country")

  # sourcedata("COMMON_ASSUMPTIONS", "A_common_data")
  # sourcedata("AGLU_ASSUMPTIONS", "A_aglu_data")
  get_data(all_data, "aglu/AGLU_ctry") %>%
    select(iso, FAO_country) %>%
    distinct ->
    AGLU_ctry

  FAO_ag_HA_ha_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_HA_ha_PRODSTAT")
  FAO_ag_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_Prod_t_PRODSTAT")
  FAO_ag_Exp_t_SUA <- get_data(all_data, "aglu/FAO_ag_Exp_t_SUA")
  FAO_ag_Feed_t_SUA <- get_data(all_data, "aglu/FAO_ag_Feed_t_SUA")
  FAO_ag_Food_t_SUA <- get_data(all_data, "aglu/FAO_ag_Food_t_SUA")
  FAO_ag_Imp_t_SUA <- get_data(all_data, "aglu/FAO_ag_Imp_t_SUA")
  FAO_an_Exp_t_SUA <- get_data(all_data, "aglu/FAO_an_Exp_t_SUA")
  FAO_an_Food_t_SUA <- get_data(all_data, "aglu/FAO_an_Food_t_SUA")
  FAO_an_Imp_t_SUA <- get_data(all_data, "aglu/FAO_an_Imp_t_SUA")
  FAO_an_Prod_t_SUA <- get_data(all_data, "aglu/FAO_an_Prod_t_SUA")
  FAO_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO_CL_kha_RESOURCESTAT")
  FAO_fallowland_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO_fallowland_kha_RESOURCESTAT")
  FAO_harv_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO_harv_CL_kha_RESOURCESTAT")
  FAO_Fert_Cons_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO_Fert_Cons_tN_RESOURCESTAT_archv")
  FAO_Fert_Cons_tN_RESOURCESTAT <- get_data(all_data, "aglu/FAO_Fert_Cons_tN_RESOURCESTAT")
  FAO_Fert_Prod_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO_Fert_Prod_tN_RESOURCESTAT_archv")
  FAO_Fert_Prod_tN_RESOURCESTAT<- get_data(all_data, "aglu/FAO_Fert_Prod_tN_RESOURCESTAT")
  FAO_For_Exp_m3_FORESTAT <- get_data(all_data, "aglu/FAO_For_Exp_m3_FORESTAT")
  FAO_For_Imp_m3_FORESTAT <- get_data(all_data, "aglu/FAO_For_Imp_m3_FORESTAT")
  FAO_For_Prod_m3_FORESTAT <- get_data(all_data, "aglu/FAO_For_Prod_m3_FORESTAT")

  coitel_colnames <- c("countries", "country codes", "item", "item codes", "element", "element codes")
  coitel_columns <- 1:6
  itel_colnames <- c("item", "item codes", "element", "element codes")

  # Replace the item and element code names with what is used in the more recent datasets
  FAO_Fert_Cons_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Cons_tN_RESOURCESTAT[1, itel_colnames]
  FAO_Fert_Prod_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Prod_tN_RESOURCESTAT[1, itel_colnames]

  # Merge resourcestat fertilizer databases with 'archive' years (1961-2002) and more recent
  # years (2002-2010). FAOSTAT notes that the methods changed between the two datasets; we
  # ignore this discrepancy but use the 2002 data from the more recent dataset

  # Helper function - applied to both the "Cons" and "Prod" datasets
  f <- function(d, d_archv) {
    d_archv %>%
      select(-`2002`) %>%
      full_join(d, by = coitel_colnames) %>%
      # dplyr seems to have a weird bug - if we group by quoted column
      # names, they get included in the summarise_all call, causing an
      # error. Rename them temporarily to work around this.
      rename(element.codes = `element codes`,
             item.codes = `item codes`,
             country.codes = `country codes`) %>%
      group_by(countries, country.codes, item, item.codes, element, element.codes) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      rename(`element codes` = element.codes,
             `item codes` = item.codes,
             `country codes` = country.codes)
  }
  FAO_Fert_Cons_tN_RESOURCESTAT <- f(FAO_Fert_Cons_tN_RESOURCESTAT, FAO_Fert_Cons_tN_RESOURCESTAT_archv)
  FAO_Fert_Prod_tN_RESOURCESTAT <- f(FAO_Fert_Prod_tN_RESOURCESTAT, FAO_Fert_Prod_tN_RESOURCESTAT_archv)

  list("FAO_ag_Exp_t_SUA" = FAO_ag_Exp_t_SUA,
       "FAO_ag_Feed_t_SUA" = FAO_ag_Feed_t_SUA,
       "FAO_ag_Food_t_SUA" = FAO_ag_Food_t_SUA,
       "FAO_ag_Imp_t_SUA" = FAO_ag_Imp_t_SUA,
       "FAO_an_Exp_t_SUA" = FAO_an_Exp_t_SUA,
       "FAO_an_Food_t_SUA" = FAO_an_Food_t_SUA,
       "FAO_an_Imp_t_SUA" = FAO_an_Imp_t_SUA,
       "FAO_an_Prod_t_SUA" = FAO_an_Prod_t_SUA,
       "FAO_Fert_Cons_tN_RESOURCESTAT" = FAO_Fert_Cons_tN_RESOURCESTAT,
       "FAO_Fert_Prod_tN_RESOURCESTAT" = FAO_Fert_Prod_tN_RESOURCESTAT,
       "FAO_ag_HA_ha_PRODSTAT" = FAO_ag_HA_ha_PRODSTAT,
       "FAO_ag_Prod_t_PRODSTAT" = FAO_ag_Prod_t_PRODSTAT,
       "FAO_CL_kha_RESOURCESTAT" = FAO_CL_kha_RESOURCESTAT,
       "FAO_fallowland_kha_RESOURCESTAT" = FAO_fallowland_kha_RESOURCESTAT,
       "FAO_harv_CL_kha_RESOURCESTAT" = FAO_harv_CL_kha_RESOURCESTAT,
       "FAO_For_Exp_m3_FORESTAT" = FAO_For_Exp_m3_FORESTAT,
       "FAO_For_Imp_m3_FORESTAT" = FAO_For_Imp_m3_FORESTAT,
       "FAO_For_Prod_m3_FORESTAT" = FAO_For_Prod_m3_FORESTAT) %>%
    # apply the following function over all list elements
    lapply(FUN = function(df) {
      # Not all databases go to 2011. Extrapolate each dataset to 2011, repeating
      # the data for 2009/10. Where missing 1961, substitute 1962
      if(!"1961" %in% colnames(df)) df$`1961` <- df$`1962`
      if(!"2010" %in% colnames(df)) df$`2010` <- df$`2009`
      if(!"2011" %in% colnames(df)) df$`2011` <- df$`2009`
      df %>%
        ungroup %>%
        select(-element) %>%
        gather(year, value, -countries, -`country codes`, -item, -`item codes`, -`element codes`) %>%
        mutate(year = as.numeric(year),
               value = as.numeric(value))
    }) %>%
    # combine everything together
    bind_rows(.id = "element") ->
    FAO_data_ALL

  # Match the iso names
  # Note that one of dplyr's join operations will not work here!
  # `match` returns the first match, and that's the behavior we want to duplicate
  # Could also I guess join, then filter for row_number==1
  FAO_data_ALL$iso <- AGLU_ctry$iso[match(FAO_data_ALL$countries, AGLU_ctry$FAO_country)]

  # Replace all missing values with 0
  FAO_data_ALL$value[is.na(FAO_data_ALL$value)] <- 0

  # Check that data is the same!
  # new1 <- spread(FAO_data_ALL, year, value)
  # old1 <- readr::read_csv("~/Desktop/FAO_data_all_old1.csv")
  # new1 <- new1[c(2:5, 1, 6, 8:58, 7)]
  # new1$element <- gsub(pattern = "_[A-Z]*$", "", new1$element)
  # new1$element <- gsub(pattern = "^FAO_", "", new1$element)
  # new1 <- arrange(new1, countries, `country codes`, item,
  #                 `item codes`, element, `element codes`)
  # old1 <- arrange(old1, countries, country.codes, item,
  #                 item.codes, element, element.codes)
  # names(new1) <- names(old1)
  # readr::write_csv(new1,"~/Desktop/FAO_data_all_new1.csv")
  # print(all.equal(new1, old1))
  #browser()

  # Downscale countries individually NOTE: This is complicated. The FAO data need to be downscaled
  # to all FAO historical years (i.e. back to 1961 regardless of when we are starting our
  # historical time series). Otherwise the early historical years will get averaged with zeroes.
  # Czechoslovakia
  FAO_data_ALL %>%
    filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Czechoslovakia"]) %>%
    downscale_FAO_country("Czechoslovakia", 1993, years = FAO_HISTORICAL_YEARS) ->
    FAO_data_ALL_cze

  # USSR
  # FAO_data_ALL %>%
  #   filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "USSR"]) %>%
  #   downscale_FAO_country() ->
  #   FAO_data_ALL_ussr

  # # Yugoslavia
  # FAO_data_ALL %>%
  #   filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Yugoslav SFR"]) %>%
  #   downscale_FAO_country() %>%
  #   # combine these downscaled databases
    # bind_rows(FAO_data_ALL_cze, FAO_data_ALL_ussr) ->
    # FAO_data_ALL_downscaled

  # Drop these countries from the full database and combine
  # FAO_data_ALL %>%
  #   filter(!iso %in% unique(FAO_data_ALL_downscaled$iso)) %>%
  #   bind_rows(FAO_data_ALL_downscaled) ->
  #   FAO_data_ALL

  # # Drop rows where all years are 0
  # FAO_data_ALL <- FAO_data_ALL[rowSums(FAO_data_ALL[X_FAO_historical_years]) != 0, ]
  #
  # # ----------------------------------------------------------------------------- 3.
  # # Calculate rolling five-year averages from available data
  # FAO_data_ALL_5yr <- FAO_data_ALL
  #
  # # In the first and last two years, use the 3 and 4 available years
  # FAO_data_ALL_5yr[X_FAO_historical_years][1] <- rowMeans(FAO_data_ALL[X_FAO_historical_years][1:3])
  # FAO_data_ALL_5yr[X_FAO_historical_years][2] <- rowMeans(FAO_data_ALL[X_FAO_historical_years][1:4])
  #
  # # Precalculate a few things for loop speed
  # lastcol <- ncol(FAO_data_ALL_5yr[X_FAO_historical_years]) - 2
  # x <- FAO_data_ALL[X_FAO_historical_years]
  # lenXFAO <- length(X_FAO_historical_years)
  #
  # # Main calculation loop
  # for(i in 3:lastcol) {
  #   FAO_data_ALL_5yr[X_FAO_historical_years][, i] <- rowMeans(x[i + -2:2])
  # }
  # FAO_data_ALL_5yr[X_FAO_historical_years][lenXFAO - 1] <-
  #   rowMeans(FAO_data_ALL[X_FAO_historical_years][(lenXFAO - 3):lenXFAO])
  # FAO_data_ALL_5yr[X_FAO_historical_years][lenXFAO] <-
  #   rowMeans(FAO_data_ALL[X_FAO_historical_years][(lenXFAO - 2):lenXFAO])
  #
  # # From here on, only use the specified AGLU historical years
  # FAO_data_ALL_5yr <- FAO_data_ALL_5yr[c(coitel_colnames, "iso", X_AGLU_historical_years)]

  # FAO_data_ALL_5yr %>%
  #   filter(year %in% HISTORICAL_YEARS) %>%
  #   group_by(element)

  # Re-split into separate tables for each element
  # for(i in unique(FAO_data_ALL$element)) {
  #   assign(i, filter(FAO_data_ALL, element == i))
  # }

  # # ----------------------------------------------------------------------------- 4.
  # # Write tables as CSV files
  # writedata(FAO_ag_HA_ha, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_HA_ha",
  #           comments = c("FAO agricultural harvested area by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_ag_Prod_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_Prod_t",
  #           comments = c("FAO agricultural production by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_ag_Exp_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_Exp_t",
  #           comments = c("FAO agricultural exports by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_ag_Feed_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_Feed_t",
  #           comments = c("FAO agricultural feed by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_ag_Food_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_Food_t",
  #           comments = c("FAO agricultural food consumption by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_ag_Imp_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_ag_Imp_t",
  #           comments = c("FAO agricultural imports by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_an_Exp_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_an_Exp_t",
  #           comments = c("FAO animal exports by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_an_Food_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_an_Food_t",
  #           comments = c("FAO animal food consumption by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_an_Imp_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_an_Imp_t",
  #           comments = c("FAO animal imports by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_an_Prod_t, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_an_Prod_t",
  #           comments = c("FAO animal production by country / item / year", "Unit = t"),
  #           readr = TRUE)
  # writedata(FAO_CL_kha, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_CL_kha",
  #           comments = c("FAO cropland area by country / year", "Unit = kha"),
  #           readr = TRUE)
  # writedata(FAO_fallowland_kha, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_fallowland_kha",
  #           comments = c("FAO fallow land area by country / year", "Unit = kha"),
  #           readr = TRUE)
  # writedata(FAO_harv_CL_kha, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_harv_CL_kha",
  #           comments = c("FAO harvested cropland (temporary crops) by country / year", "Unit = kha"),
  #           readr = TRUE)
  # writedata(FAO_Fert_Cons_tN, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_Fert_Cons_tN",
  #           comments = c("FAO fertilizer consumption by country / year", "Unit = tonnes of N"),
  #           readr = TRUE)
  # writedata(FAO_Fert_Prod_tN, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_Fert_Prod_tN",
  #           comments = c("FAO fertilizer production by country / year", "Unit = tonnes of N"),
  #           readr = TRUE)
  # writedata(FAO_For_Exp_m3, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_For_Exp_m3",
  #           comments = c("FAO forestry exports by country / year", "Unit = m3"),
  #           readr = TRUE)
  # writedata(FAO_For_Imp_m3, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_For_Imp_m3",
  #           comments = c("FAO forestry imports by country / year", "Unit = m3"),
  #           readr = TRUE)
  # writedata(FAO_For_Prod_m3, domain = "AGLU_LEVEL1_DATA", fn = "L100.FAO_For_Prod_m3",
  #           comments = c("FAO forestry production by country / year", "Unit = m3"),
  #           readr = TRUE)
  FAO_For_Prod_m3 <- tibble(x=1)
  return_data(FAO_For_Prod_m3)
}
