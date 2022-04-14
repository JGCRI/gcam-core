# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA100.FAO_downscale_ctry
#'
#' Downscale FAO production and consumption agricultural data to AGLU countries.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.FAO_ag_HA_ha}, \code{L100.FAO_ag_Prod_t}, \code{L100.FAO_ag_Exp_t}, \code{L100.FAO_ag_Feed_t}, \code{L100.FAO_ag_Food_t}, \code{L100.FAO_ag_Imp_t}, \code{L100.FAO_an_Exp_t}, \code{L100.FAO_an_Food_t}, \code{L100.FAO_an_Imp_t}, \code{L100.FAO_an_Prod_t}, \code{L100.FAO_CL_kha}, \code{L100.FAO_fallowland_kha}, \code{L100.FAO_harv_CL_kha}, \code{L100.FAO_Fert_Cons_tN}, \code{L100.FAO_Fert_Prod_tN}, \code{L100.FAO_For_Exp_m3}, \code{L100.FAO_For_Imp_m3}, \code{L100.FAO_For_Prod_m3}. The corresponding file in the
#' original data system was \code{LA100.FAO_downscale_ctry.R} (aglu level1).
#' @details Extrapolate each FAO dataset to 2011; match with country names; extrapolate to countries that
#' split or combined at some point (e.g. Czechoslovakia needs to be split into Czech Republic and
#' Slovakia); and calculate rolling five-year averages.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom stats aggregate
#' @importFrom dplyr bind_rows distinct filter full_join left_join rename select add_row
#' @importFrom tidyr gather replace_na spread
#' @author BBL
module_aglu_LA100.FAO_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_HA_ha_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_Exp_t_SUA",
             FILE = "aglu/FAO/FAO_ag_Feed_t_SUA",
             FILE = "aglu/FAO/FAO_ag_Food_t_SUA",
             FILE = "aglu/FAO/FAO_ag_Imp_t_SUA",
             FILE = "aglu/FAO/FAO_an_Exp_t_SUA",
             FILE = "aglu/FAO/FAO_an_Food_t_SUA",
             FILE = "aglu/FAO/FAO_an_Imp_t_SUA",
             FILE = "aglu/FAO/FAO_an_Prod_t_SUA",
             FILE = "aglu/FAO/FAO_an_Stocks",
             FILE = "aglu/FAO/FAO_an_Dairy_Stocks",
             FILE = "aglu/FAO/FAO_CL_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_fallowland_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv",
             FILE = "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv",
             FILE = "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT",
             FILE = "aglu/FAO/FAO_For_Exp_m3_FORESTAT",
             FILE = "aglu/FAO/FAO_For_Imp_m3_FORESTAT",
             FILE = "aglu/FAO/FAO_For_Prod_m3_FORESTAT"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
             "L100.FAO_an_Stocks",
             "L100.FAO_an_Dairy_Stocks",
             "L100.FAO_CL_kha",
             "L100.FAO_fallowland_kha",
             "L100.FAO_harv_CL_kha",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L100.FAO_For_Exp_m3",
             "L100.FAO_For_Imp_m3",
             "L100.FAO_For_Prod_m3"))
  } else if(command == driver.MAKE) {

    iso <- FAO_country <- `country codes` <- `element codes` <- `item codes` <-
      year <- value <- countries <- country.codes <- item <- item.codes <-
      element <- element.codes <- `2011` <- NULL # silence package chck.


    all_data <- list(...)[[1]]

    # Load required inputs
    get_data(all_data, "aglu/AGLU_ctry") %>%
      select(iso, FAO_country) %>%
      distinct ->
      AGLU_ctry

    FAO_ag_HA_ha_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_HA_ha_PRODSTAT")
    FAO_ag_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_Prod_t_PRODSTAT")
    FAO_ag_Exp_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Exp_t_SUA")
    FAO_ag_Feed_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Feed_t_SUA")
    FAO_ag_Food_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Food_t_SUA")
    FAO_ag_Imp_t_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_Imp_t_SUA")
    FAO_an_Exp_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Exp_t_SUA")
    FAO_an_Food_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Food_t_SUA")
    FAO_an_Imp_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Imp_t_SUA")
    FAO_an_Prod_t_SUA <- get_data(all_data, "aglu/FAO/FAO_an_Prod_t_SUA")
    FAO_an_Stocks <- get_data(all_data, "aglu/FAO/FAO_an_Stocks")
    FAO_an_Dairy_Stocks <- get_data(all_data, "aglu/FAO/FAO_an_Dairy_Stocks")
    FAO_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_CL_kha_RESOURCESTAT")
    FAO_fallowland_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_fallowland_kha_RESOURCESTAT")
    FAO_harv_CL_kha_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT")
    FAO_Fert_Cons_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv")
    FAO_Fert_Cons_tN_RESOURCESTAT <- get_data(all_data, "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT")
    FAO_Fert_Prod_tN_RESOURCESTAT_archv <- get_data(all_data, "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv")
    FAO_Fert_Prod_tN_RESOURCESTAT<- get_data(all_data, "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT")
    FAO_For_Exp_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Exp_m3_FORESTAT")
    FAO_For_Imp_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Imp_m3_FORESTAT")
    FAO_For_Prod_m3_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Prod_m3_FORESTAT")

    itel_colnames <- c("item", "item codes", "element", "element codes")
    coitel_colnames <- c("countries", "country codes", itel_colnames)
    FAO_histyear_cols <- as.character(aglu.FAO_HISTORICAL_YEARS)


    #kbn 2019/11/04 Write function to fill holes for base year-
    #This function will fill holes from the specified cut-off year to the base year for any region, comoditty combination
    #that is missing data in any year. It will not fill in data if no data exists for a region commoditty combination before the cut-off year.
    #It will not add new commoditties or regions.If Printlog is set to TRUE, the function will print out,how many data points you are missing in the
    #base year, how many points could be filled in and how many points could not be filled in.Current cut-off year is set to 2007. 1961 is the first year of FAO
    #data.

    get_baseyear_or_mostrecent_FAO<-function(df,BaseYear=MODEL_FINAL_BASE_YEAR,StartYear=1961,CutOff=2007,Printlog=FALSE){

      #Step 1: Get values for start year, last year and dataset name
      StartYear<-toString(StartYear)
      LastYear<-colnames(df)[ncol(df)]
      name<-deparse(substitute(df))

      #Step 2: Convert from tibble to dataframe for processing.Will convert back to tibble in the last step.
     df<-as.data.frame(df)

     #Step 3: Create a list of required years
    Yrs<-seq(CutOff+1,BaseYear,1)

      #Step 4: If there is no column, create it.
      for (i in Yrs){
        if (!toString(i) %in% c(colnames(df))){
          df[,toString(i)]<-NA_integer_
        }
      }

      #Step 5: Print first warning message
      if (Printlog==TRUE){

        print(paste("No data found in dataset ",name," for ",(sum(is.na((df[,toString(BaseYear)]))))," values.",sep = ""))}


      #Step 6: Fill with neighbor

      for (i in Yrs){
        df[,toString(i)]<-if_else(is.na(df[,toString(i)]),df[,toString(i-1)],df[,toString(i)])
      }


      if (Printlog==TRUE){
        #Step7: Print second warning message
        print(paste("Could not fill in data for base year ",name," for ",sum(is.na(df[,toString(BaseYear)]))," values. There was no data available after ", CutOff,sep = ""))}

    #Step 8: Convert back to tibble

    df<-as_tibble(df)

      return(df)
    }


    #kbn 2019/12/16 Adding in hole-filling for all FAO data.
    FAO_ag_HA_ha_PRODSTAT <- get_baseyear_or_mostrecent_FAO(FAO_ag_HA_ha_PRODSTAT)
    FAO_ag_Prod_t_PRODSTAT <- get_baseyear_or_mostrecent_FAO(FAO_ag_Prod_t_PRODSTAT)
    FAO_ag_Exp_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Exp_t_SUA)
    FAO_ag_Feed_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Feed_t_SUA)
    FAO_ag_Food_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Food_t_SUA)
    FAO_ag_Imp_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_ag_Imp_t_SUA)
    FAO_an_Exp_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Exp_t_SUA)
    FAO_an_Food_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Food_t_SUA)
    FAO_an_Imp_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Imp_t_SUA)
    FAO_an_Prod_t_SUA <- get_baseyear_or_mostrecent_FAO(FAO_an_Prod_t_SUA)
    FAO_fallowland_kha_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_fallowland_kha_RESOURCESTAT)
    FAO_harv_CL_kha_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_harv_CL_kha_RESOURCESTAT)
    FAO_Fert_Cons_tN_RESOURCESTAT <- get_baseyear_or_mostrecent_FAO(FAO_Fert_Cons_tN_RESOURCESTAT)
    FAO_Fert_Prod_tN_RESOURCESTAT<- get_baseyear_or_mostrecent_FAO(FAO_Fert_Prod_tN_RESOURCESTAT)


    # Replace the item and element code names with what is used in the more recent datasets
    FAO_Fert_Cons_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Cons_tN_RESOURCESTAT[1, itel_colnames]
    FAO_Fert_Prod_tN_RESOURCESTAT_archv[itel_colnames] <- FAO_Fert_Prod_tN_RESOURCESTAT[1, itel_colnames]

    # Merge resourcestat fertilizer databases with 'archive' years (1961-2002) and more recent
    # years (2002-2010). FAOSTAT notes that the methods changed between the two datasets; we
    # ignore this discrepancy but use the 2002 data from the more recent dataset
    FAO_Fert_Cons_tN_RESOURCESTAT_archv$`2002` <- NULL
    FAO_Fert_Prod_tN_RESOURCESTAT_archv$`2002` <- NULL

    # Interesting: dplyr can't go as fast as the approach taken in the original data system
    # A number of dplyr operations are *considerably* slower with this big dataset, and take more lines
    # So most of this function, the slowest in the entire data system, retains the original
    # code (though cleaned up considerably) and logic
    cons <- full_join(FAO_Fert_Cons_tN_RESOURCESTAT_archv,
                      FAO_Fert_Cons_tN_RESOURCESTAT, by = c("countries", "country codes", "item", "item codes", "element", "element codes"))
    prod <- full_join(FAO_Fert_Prod_tN_RESOURCESTAT_archv,
                      FAO_Fert_Prod_tN_RESOURCESTAT, by = c("countries", "country codes", "item", "item codes", "element", "element codes"))

    # Aggregate to complete the merge of the two datasets
    FAO_Fert_Cons_tN_RESOURCESTAT <- aggregate(cons[names(cons) %in% FAO_histyear_cols],
                                               by = as.list(cons[coitel_colnames]),
                                               sum, na.rm = TRUE)
    FAO_Fert_Prod_tN_RESOURCESTAT <- aggregate(prod[names(prod) %in% FAO_histyear_cols],
                                               by = as.list(prod[coitel_colnames]),
                                               sum, na.rm = TRUE)

    # Some data in an_Stocks are in 1000s of heads instead of just heads; convert them
    # Also remove the units column to be consistent with the other FAO tables.
    fhyc <- names(FAO_an_Stocks) %in% aglu.FAO_HISTORICAL_YEARS
    thr <- FAO_an_Stocks$units == "1000 Head"
    FAO_an_Stocks[thr, fhyc] <- FAO_an_Stocks[thr, fhyc] * 1000
    FAO_an_Stocks$units <- FAO_an_Dairy_Stocks$units <- NULL
    # as necessary, expand the animal stocks data to 2012
    if(!"2012" %in% names(FAO_an_Stocks)) FAO_an_Stocks$`2012` <- FAO_an_Stocks$`2011`

    # Fodder data only goes through 2011, but won't be extrapolated to 2012 in
    # the function below because the column `2012` exists.
    # Fill in the Fodder 2012 data here.
    FAO_ag_HA_ha_PRODSTAT %>%
      filter(grepl("forage", item) | grepl("Fodder", item) | grepl("silage", item)) %>%
      mutate(`2012` = `2011`) ->
      FAO_fodder_HA_ha_PRODSTAT

    FAO_ag_HA_ha_PRODSTAT %>%
      filter(!grepl("forage", item) & !grepl("Fodder", item) & !grepl("silage", item)) %>%
      bind_rows(FAO_fodder_HA_ha_PRODSTAT) ->
      FAO_ag_HA_ha_PRODSTAT

    FAO_ag_Prod_t_PRODSTAT %>%
      filter(grepl("forage", item) | grepl("Fodder", item) | grepl("silage", item)) %>%
      mutate(`2012` = `2011`) ->
      FAO_fodder_Prod_t_PRODSTAT

    FAO_ag_Prod_t_PRODSTAT %>%
      filter(!grepl("forage", item) & !grepl("Fodder", item) & !grepl("silage", item)) %>%
      bind_rows(FAO_fodder_Prod_t_PRODSTAT) ->
      FAO_ag_Prod_t_PRODSTAT

    # China vegetable food consumption is estimated from production volumes in the SUA database that are far higher
    # than the quantities for the constituent crops reported in the PRODSTAT database, which we use for production.
    # This discrepancy is corrected (approximately) here.
    FAO_ag_Food_t_SUA[FAO_histyear_cols] <- lapply(FAO_ag_Food_t_SUA[FAO_histyear_cols], as.numeric)
    FAO_ag_Food_t_SUA[FAO_ag_Food_t_SUA$countries == "China" & FAO_ag_Food_t_SUA$item == "Vegetables, Other",
                      FAO_histyear_cols] <-
      FAO_ag_Food_t_SUA[FAO_ag_Food_t_SUA$countries == "China" & FAO_ag_Food_t_SUA$item == "Vegetables, Other",
                        FAO_histyear_cols] * aglu.CHN_VEG_FOOD_MULT

    # Not all databases go to 2012. Extrapolate each dataset to 2012, repeating
    # the data for 2009/10. Where missing 1961, substitute 1962
    list("FAO_ag_Exp_t_SUA" = FAO_ag_Exp_t_SUA,
         "FAO_ag_Feed_t_SUA" = FAO_ag_Feed_t_SUA,
         "FAO_ag_Food_t_SUA" = FAO_ag_Food_t_SUA,
         "FAO_ag_Imp_t_SUA" = FAO_ag_Imp_t_SUA,
         "FAO_an_Exp_t_SUA" = FAO_an_Exp_t_SUA,
         "FAO_an_Food_t_SUA" = FAO_an_Food_t_SUA,
         "FAO_an_Imp_t_SUA" = FAO_an_Imp_t_SUA,
         "FAO_an_Prod_t_SUA" = FAO_an_Prod_t_SUA,
         "FAO_an_Stocks" = FAO_an_Stocks,
         "FAO_an_Dairy_Stocks" = FAO_an_Dairy_Stocks,
         "FAO_Fert_Cons_tN_RESOURCESTAT" = FAO_Fert_Cons_tN_RESOURCESTAT,
         "FAO_Fert_Prod_tN_RESOURCESTAT" = FAO_Fert_Prod_tN_RESOURCESTAT,
         "FAO_ag_HA_ha_PRODSTAT" = FAO_ag_HA_ha_PRODSTAT,
         "FAO_ag_Prod_t_PRODSTAT" = FAO_ag_Prod_t_PRODSTAT,
         "FAO_For_Exp_m3_FORESTAT" = FAO_For_Exp_m3_FORESTAT,
         "FAO_For_Imp_m3_FORESTAT" = FAO_For_Imp_m3_FORESTAT,
         "FAO_For_Prod_m3_FORESTAT" = FAO_For_Prod_m3_FORESTAT) %>%
      # apply the following function over all list elements
      lapply(FUN = function(df) {
        if(!"1961" %in% colnames(df)) df$`1961` <- df$`1962`
        if(!"2013" %in% colnames(df)) df$`2013` <- df$`2012`
        if(!"2014" %in% colnames(df)) df$`2014` <- df$`2013`
        if(!"2015" %in% colnames(df)) df$`2015` <- df$`2014`
        df$element <- NULL
        df
      }) %>%
      # combine everything together
      bind_rows(.id = "element") ->
      FAO_data_ALL

    # Replace all missing numeric values with 0
    FAO_data_ALL <- dplyr::mutate_if(FAO_data_ALL, is.numeric, replace_na, replace = 0)

    # Match the iso names
    FAO_data_ALL %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) ->
      FAO_data_ALL

    # Downscale countries individually NOTE: This is complicated. The FAO data need to be downscaled
    # to all FAO historical years (i.e. back to 1961 regardless of when we are starting our
    # historical time series). Otherwise the early historical years will get averaged with zeroes.
    # Czechoslovakia
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Czechoslovakia"]) %>%
      downscale_FAO_country("Czechoslovakia", 1993L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_cze

    # USSR
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "USSR"]) %>%
      downscale_FAO_country("USSR", 1992L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_ussr

    # Yugoslavia
    FAO_data_ALL %>%
      filter(iso %in% AGLU_ctry$iso[AGLU_ctry$FAO_country == "Yugoslav SFR"]) %>%
      downscale_FAO_country("Yugoslav SFR", 1992L, years = aglu.FAO_HISTORICAL_YEARS) ->
      FAO_data_ALL_yug

    # Drop these countries from the full database and combine
    FAO_data_ALL %>%
      filter(!iso %in% unique(c(FAO_data_ALL_cze$iso, FAO_data_ALL_ussr$iso, FAO_data_ALL_yug$iso))) %>%
      # combine these downscaled databases
      bind_rows(FAO_data_ALL_cze, FAO_data_ALL_ussr, FAO_data_ALL_yug) ->
      FAO_data_ALL

    # Make sure histyear_cols uses only names in our data set
    FAO_histyear_cols <- intersect(FAO_histyear_cols, names(FAO_data_ALL))
    # Drop observations where all years are zero
    FAO_data_ALL <- FAO_data_ALL[rowSums(FAO_data_ALL[FAO_histyear_cols]) != 0, ]

    # Calculate rolling five-year averages from available data
    FAO_data_ALL_5yr <- FAO_data_ALL

    # In the first and last two years, use the 3 and 4 available years
    FAO_data_ALL_5yr[FAO_histyear_cols][1] <- rowMeans(FAO_data_ALL[FAO_histyear_cols][1:3])
    FAO_data_ALL_5yr[FAO_histyear_cols][2] <- rowMeans(FAO_data_ALL[FAO_histyear_cols][1:4])

    # Precalculate a few things for loop speed
    lastcol <- ncol(FAO_data_ALL_5yr[FAO_histyear_cols]) - 2
    x <- FAO_data_ALL[FAO_histyear_cols]
    lenXFAO <- length(FAO_histyear_cols)

    # Main calculation loop
    for(i in 3:lastcol) {
      FAO_data_ALL_5yr[FAO_histyear_cols][, i] <- rowMeans(x[i + -2:2])
    }
    FAO_data_ALL_5yr[FAO_histyear_cols][lenXFAO - 1] <-
      rowMeans(FAO_data_ALL[FAO_histyear_cols][(lenXFAO - 3):lenXFAO])
    FAO_data_ALL_5yr[FAO_histyear_cols][lenXFAO] <-
      rowMeans(FAO_data_ALL[FAO_histyear_cols][(lenXFAO - 2):lenXFAO])

    # From here on, only use the specified AGLU historical years
    FAO_data_ALL_5yr <- FAO_data_ALL_5yr[c(coitel_colnames, "iso", as.character(aglu.AGLU_HISTORICAL_YEARS))]

    # Rename columns to old names
    FAO_data_ALL_5yr %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) ->
      FAO_data_ALL_5yr

    # Change `element` columns to match old data and reshape
    #    FAO_data_ALL_5yr <- FAO_data_ALL_5yr[c(1:6,8:47,7)]
    FAO_data_ALL_5yr$element <- gsub(pattern = "_[A-Z]*$", "", FAO_data_ALL_5yr$element)
    FAO_data_ALL_5yr$element <- gsub(pattern = "^FAO_", "", FAO_data_ALL_5yr$element)
    FAO_data_ALL_5yr <- gather_years(FAO_data_ALL_5yr)

    # Re-split into separate tables for each element
    L100.FAOlist <- split(seq(1, nrow(FAO_data_ALL_5yr)), FAO_data_ALL_5yr$element)
    names(L100.FAOlist) <- lapply(names(L100.FAOlist), function(x) { paste0("L100.FAO_", x) })
    # change list names to match the legacy
    # names
    fixup <- function(irows, legacy.name) {
        FAO_data_ALL_5yr[irows,] %>%
          add_comments("Downscale countries; calculate 5-yr averages") %>%
          add_legacy_name(legacy.name)
    }
    L100.FAOlist <- Map(fixup, L100.FAOlist, names(L100.FAOlist))

    # Fallow land data sets are missing lots of years, so we don't include them in the 5yr average
    # They need to be reshaped and written out now for use downstream
    FAO_CL_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years() %>%
      add_legacy_name("L100.FAO_CL_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_CL_kha
    FAO_fallowland_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years()  %>%
      add_legacy_name("L100.FAO_fallowland_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_fallowland_kha
    FAO_harv_CL_kha_RESOURCESTAT %>%
      left_join(distinct(AGLU_ctry, FAO_country, .keep_all = TRUE), by = c("countries" = "FAO_country")) %>%
      rename(country.codes = `country codes`,
             element.codes = `element codes`,
             item.codes = `item codes`) %>%
      gather_years() %>%
      add_legacy_name("L100.FAO_harv_CL_kha") %>%
      na.omit() %>%
      mutate(value = as.numeric(value)) %>%
      add_comments("Downscale countries") ->
      L100.FAO_harv_CL_kha

    # Add description, units, process (done above), and precursor information
    L100.FAOlist[["L100.FAO_ag_HA_ha"]] %>%
      add_title("FAO agricultural harvested area by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_HA_ha_PRODSTAT", "aglu/AGLU_ctry") ->
      L100.FAO_ag_HA_ha
    L100.FAOlist[["L100.FAO_ag_Prod_t"]] %>%
      add_title("FAO agricultural production by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_Prod_t_PRODSTAT", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Prod_t
    L100.FAOlist[["L100.FAO_ag_Exp_t"]] %>%
      add_title("FAO agricultural exports by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_Exp_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Exp_t
    L100.FAOlist[["L100.FAO_ag_Feed_t"]] %>%
      add_title("FAO agricultural feed by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_Feed_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Feed_t
    L100.FAOlist[["L100.FAO_ag_Food_t"]] %>%
      add_title("FAO agricultural food consumption by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_Food_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Food_t
    L100.FAOlist[["L100.FAO_ag_Imp_t"]] %>%
      add_title("FAO agricultural imports by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_ag_Imp_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_ag_Imp_t
    L100.FAOlist[["L100.FAO_an_Exp_t"]] %>%
      add_title("FAO animal exports by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_an_Exp_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_an_Exp_t
    L100.FAOlist[["L100.FAO_an_Food_t"]] %>%
      add_title("FAO animal food consumption by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_an_Food_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_an_Food_t
    L100.FAOlist[["L100.FAO_an_Imp_t"]] %>%
      add_title("FAO animal imports by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_an_Imp_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_an_Imp_t
    L100.FAOlist[["L100.FAO_an_Prod_t"]] %>%
      add_title("FAO animal production by country, item, year") %>%
      add_units("t") %>%
      add_precursors("aglu/FAO/FAO_an_Prod_t_SUA", "aglu/AGLU_ctry") ->
      L100.FAO_an_Prod_t
    L100.FAOlist[["L100.FAO_an_Stocks"]] %>%
      add_title("FAO animal stocks country, item, year") %>%
      add_units("number") %>%
      add_precursors("aglu/FAO/FAO_an_Stocks", "aglu/AGLU_ctry") ->
      L100.FAO_an_Stocks
    L100.FAOlist[["L100.FAO_an_Dairy_Stocks"]] %>%
      add_title("FAO dairy producing animal stocks country, item, year") %>%
      add_units("number") %>%
      add_precursors("aglu/FAO/FAO_an_Dairy_Stocks", "aglu/AGLU_ctry") ->
      L100.FAO_an_Dairy_Stocks
    L100.FAO_CL_kha %>%
      add_title("FAO cropland area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_CL_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_CL_kha
    L100.FAO_fallowland_kha %>%
      add_title("FAO fallow land area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_fallowland_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_fallowland_kha
    L100.FAO_harv_CL_kha %>%
      add_title("FAO harvested cropland (temporary crops) area by country, year") %>%
      add_units("kha") %>%
      add_precursors("aglu/FAO/FAO_harv_CL_kha_RESOURCESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_harv_CL_kha
    L100.FAOlist[["L100.FAO_Fert_Cons_tN"]] %>%
      add_title("FAO fertilizer consumption by country, year") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT",
                     "aglu/FAO/FAO_Fert_Cons_tN_RESOURCESTAT_archv",
                     "aglu/AGLU_ctry") ->
      L100.FAO_Fert_Cons_tN
    L100.FAOlist[["L100.FAO_Fert_Prod_tN"]] %>%
      add_title("FAO fertilizer production by country, year") %>%
      add_units("tonnes N") %>%
      add_precursors("aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT",
                     "aglu/FAO/FAO_Fert_Prod_tN_RESOURCESTAT_archv",
                     "aglu/AGLU_ctry") ->
      L100.FAO_Fert_Prod_tN
    L100.FAOlist[["L100.FAO_For_Exp_m3"]] %>%
      add_title("FAO forestry exports by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Exp_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Exp_m3
    L100.FAOlist[["L100.FAO_For_Imp_m3"]] %>%
      add_title("FAO forestry imports by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Imp_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Imp_m3
    L100.FAOlist[["L100.FAO_For_Prod_m3"]] %>%
      add_title("FAO forestry production by country, year") %>%
      add_units("m3") %>%
      add_precursors("aglu/FAO/FAO_For_Prod_m3_FORESTAT", "aglu/AGLU_ctry") ->
      L100.FAO_For_Prod_m3

    return_data(L100.FAO_ag_HA_ha,
                L100.FAO_ag_Prod_t,
                L100.FAO_ag_Exp_t,
                L100.FAO_ag_Feed_t,
                L100.FAO_ag_Food_t,
                L100.FAO_ag_Imp_t,
                L100.FAO_an_Exp_t,
                L100.FAO_an_Food_t,
                L100.FAO_an_Imp_t,
                L100.FAO_an_Prod_t,
                L100.FAO_an_Stocks,
                L100.FAO_an_Dairy_Stocks,
                L100.FAO_CL_kha,
                L100.FAO_fallowland_kha,
                L100.FAO_harv_CL_kha,
                L100.FAO_Fert_Cons_tN,
                L100.FAO_Fert_Prod_tN,
                L100.FAO_For_Exp_m3,
                L100.FAO_For_Imp_m3,
                L100.FAO_For_Prod_m3)
  } else {
    stop("Unknown command")
  }
}
