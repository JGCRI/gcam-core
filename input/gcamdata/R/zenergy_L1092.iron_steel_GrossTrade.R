# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1092.iron_steel_GrossTrade
#'
#' Reads country-level steel production, consumption, imports and exports data from World Steel Association data set.
#' Adjusts regional steel exports and imports by a scaling factor to fit the trade balance equation (consumption = production - exports + imports).
#' Aggregates trade data by GCAM region, Calculates and removes intra-regional trade using bilateral trade data from resource trade database.
#' Scales the regional trade data to ensure global imports minus exports and global production minus consumption is equal to zero.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{LB1092.Tradebalance_iron_steel_Mt_R_Y}.
#' @importFrom dplyr filter if_else mutate select distinct
#' @importFrom tidyr gather spread
#' @author Siddarth Durga July 2022
module_energy_L1092.iron_steel_GrossTrade <- function(command, ...){
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/WSA_steel_prod_cons_1970_2018",
             FILE = "energy/WSA_steel_trade_1970_2018",
             FILE = "energy/mappings/WSA_gcam_mapping",
             FILE = "energy/mappings/comtrade_countrycode_ISO",
             FILE = "energy/mappings/comtrade_countrycode_ISO",
             FILE = "energy/Rt_iron_steel_bilateral_trade",
             FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("LB1092.Tradebalance_iron_steel_Mt_R_Y"))
  } else if(command == driver.MAKE) {

    Country <- GCAM_region <- Metric <- '1970' <-'1971' <- '1972' <- '1973' <- '1974' <- '1975' <-
      '1976' <- '1977' <- '1978' <- '1979' <- '1980' <-'1981' <- '1982' <- '1983' <- '1984' <- '1985' <-
      '1986' <- '1987' <- '1988' <- '1989' <- '1990' <-'1991' <- '1992' <- '1993' <- '1994' <- '1995' <-
      '1996' <- '1997' <- '1998' <- '1999' <- '2000' <-'2001' <- '2002' <- '2003' <- '2004' <- '2005' <-
      '2006' <- '2007' <- '2008' <- '2009' <- '2010' <-'2011' <- '2012' <- '2013' <- '2014' <- '2015' <-
      '2016' <- '2017' <- '2018' <- '2019' <- '2020' <-'2021' <-NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    WSA_steel_prod_cons_1970_2018 <- get_data(all_data, "energy/WSA_steel_prod_cons_1970_2018",strip_attributes = TRUE)
    WSA_steel_trade_1970_2018 <- get_data(all_data, "energy/WSA_steel_trade_1970_2018",strip_attributes = TRUE)
    WSA_gcam_mapping <- get_data(all_data, "energy/mappings/WSA_gcam_mapping",strip_attributes = TRUE)
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID",strip_attributes = TRUE)
    comtrade_countrycode_ISO <- get_data(all_data, "energy/mappings/comtrade_countrycode_ISO",strip_attributes = TRUE)
    Rt_iron_steel_bilateral_trade_data <- get_data(all_data, "energy/Rt_iron_steel_bilateral_trade",strip_attributes = TRUE) %>%
      filter(Year %in% c("2000",MODEL_BASE_YEARS))


    # Bind iron and steel production, consumption, and trade data
    # Convert data from wide to long format, and adjust units
    WSA_steel_all_1970_2018 <- rbind(WSA_steel_prod_cons_1970_2018,WSA_steel_trade_1970_2018) %>%
      gather(key="year",value="value",'1970':'2021') %>%
      mutate(year=as.integer(year),
             value=value/1000, #Unit: to Mt
             value=replace_na(value,0)) #replace missing data with zero

    #Function to prepare trade data for further processing
    WSA_trade_data <- function(WSA_metric,value_name) {
      WSA_steel_all_1970_2018 %>%
        filter(Metric == WSA_metric)-> WS
      names(WS)[4] <- value_name
      return(WS)
      }

    WSA_steel_prod <- WSA_trade_data("Crude Steel Production","production")
    WSA_steel_cons <- WSA_trade_data("Apparent Steel Consumption (Crude Eq.)","consumption")
    WSA_steel_imports <- WSA_trade_data("Imports","imports")
    WSA_steel_exports <- WSA_trade_data("Exports","exports")

    WSA_steel_prod %>%
      left_join(WSA_steel_cons,by=c("Country","year")) %>%
      left_join(WSA_steel_imports,by=c("Country","year")) %>%
      left_join(WSA_steel_exports,by=c("Country","year")) -> WSA_steel_all_1970_2018

    #assign imports equal to consumption in countries where the reported consumption is greater than zero, but the reported imports, exports, and production are zero
    WSA_steel_all_1970_2018 %>%
      mutate(imports=ifelse(consumption > 0 & ((imports == 0 & exports == 0) | (imports == 0 & exports == 0 & production == 0)),consumption,imports)) %>%
      select(Country,year,imports,exports,consumption,production)%>%
      gather(key="metric",value="value",'imports':'production') -> WSA_steel_all_1970_2018

    #aggregate country-level data to GCAM_region-level data
    WSA_steel_all_1970_2018 %>%
      left_join(WSA_gcam_mapping,by=c("Country"))%>%
      group_by(GCAM_region,year,metric)%>%
      mutate(value=sum(value))%>%
      distinct(GCAM_region,year,metric,value)-> LB1092.Tradebalance_iron_steel_Mt_R_Y

    #adjust exports and imports to match apparent_steel_consumption_actual and eval
    LB1092.Tradebalance_iron_steel_Mt_R_Y %>% filter(metric=="consumption")%>% rename(consumption=value) %>%
      left_join(LB1092.Tradebalance_iron_steel_Mt_R_Y %>% filter(metric == "production") %>% rename(production=value),by=c("GCAM_region","year")) %>%
      left_join(LB1092.Tradebalance_iron_steel_Mt_R_Y %>% filter(metric == "exports")%>% rename(exports=value),by=c("GCAM_region","year")) %>%
      left_join(LB1092.Tradebalance_iron_steel_Mt_R_Y %>% filter(metric == "imports")%>% rename(imports=value),by=c("GCAM_region","year")) -> LB1092.Tradebalance_iron_steel_Mt_R_Y

    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      #remove region "NA" from the iron and steel trade data
      filter(GCAM_region!="NA")%>%
      #if production is equal to zero in the model base-year add a minimum calibration value of 0.01 Mt
      #This minimum calibration value is 5 times less than the smallest producer of steel in 2015
      mutate(production=ifelse(production==0 & year == MODEL_FINAL_BASE_YEAR,0.01,production),
             #estimate the imports and exports scaling factor to match the reported WSA steel consumption data
             #calculate scaling factor (x) so that consumption = production - x*exports + x*imports
             scaling_factor=(consumption-production)/(imports-exports),
             #re-evaluate the exports and imports by multiplying with the scaling factor
             exports_reval=scaling_factor*exports,imports_reval=scaling_factor*imports,
             #for regions with negative scaling factors use the WSA imports and exports
             exports_reval=ifelse(exports_reval<0,exports,exports_reval),imports_reval=ifelse(imports_reval<0,imports,imports_reval),
             #estimate the % increase in imports and exports after scaling
             diff_exports=ifelse(exports==0,0,((exports_reval-exports)/exports)*100),
             diff_imports=ifelse(imports==0,0,((imports_reval-imports)/imports)*100),
             #for regions with % increase in imports and exports greater than 10% (tolerance level) use the reported WSA imports and exports
             exports_reval=ifelse(diff_exports>=10 | diff_exports <=-10,exports,exports_reval),imports_reval=ifelse(diff_imports >=10 |diff_imports <=-10,imports,imports_reval),
             #estimate the domestic supply of iron and steel (production minus exports)
             #if this value is negative assume exports and imports for this region are 0
             exports_reval=ifelse(production-exports_reval<0,0,exports_reval),imports_reval=ifelse(production-exports_reval<0,0,imports_reval),
             domestic_supply=production-exports_reval,
             #estimate steel consumption using the final scaled imports and exports
             consumption_reval=production-exports_reval+imports_reval) %>%
      select(GCAM_region,year,production,imports_reval,exports_reval,consumption_reval,domestic_supply) %>%
      mutate(production=replace_na(production,0),imports_reval=replace_na(imports_reval,0),exports_reval=replace_na(exports_reval,0),
             consumption_reval=replace_na(consumption_reval,0),domestic_supply=replace_na(domestic_supply,0)) -> LB1092.Tradebalance_iron_steel_Mt_R_Y


    #Scale the regional trade data to ensure global imports and exports are equal.
    #Estimate the global scaling factors by region and year
    Global_scaling_factors <- LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      gather(key="metric",value="value",'production':'domestic_supply') %>%
      group_by(year,metric)%>%
      mutate(value=sum(value))%>%
      distinct(year,metric,value) %>%
      spread(key="metric",value="value") %>%
      mutate(imports_export_scale = exports_reval/imports_reval,
             imports_export_scale=replace_na(imports_export_scale,1))%>%
      select(year,imports_export_scale)

    LB1092.Tradebalance_iron_steel_Mt_R_Y <- LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      left_join(Global_scaling_factors,by="year") %>%
      mutate(imports_reval=imports_reval*imports_export_scale,
             consumption_reval=production-exports_reval+imports_reval) %>%
      select(-imports_export_scale)%>%
      gather(key="metric",value="value",'production':'domestic_supply') %>%
      ungroup()

    #calculate intra-regional trade from resource trade database
    #filter finished and semi-finished iron and steel products from resources
    Rt_iron_steel_bilateral_trade_data %>%
      select('Exporter ISO3','Exporter','Importer','Importer ISO3','Resource','Year','Value','Weight') %>%
      filter(Resource %in% energy.IRON_STEEL.RESOURCES)-> Rt_iron_steel_bilateral_trade_data

    #remove rows with NA
    Rt_iron_steel_bilateral_trade_data <- Rt_iron_steel_bilateral_trade_data[-which(is.na(Rt_iron_steel_bilateral_trade_data$Weight) | is.na(Rt_iron_steel_bilateral_trade_data$Value)),]

    #map import and export regions to GCAM_regions
    Rt_iron_steel_bilateral_trade_data %>%
      rename(Country_Name = Exporter, iso="Exporter ISO3")%>%
      left_join(comtrade_countrycode_ISO,by=c("Country_Name"))%>%
      mutate(iso=tolower(iso)) %>%
      left_join(iso_GCAM_regID,by=c("iso")) %>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      rename(Exporter_Region = region, Exporter_Country = Country_Name,Country_Name=Importer)%>%
      select(Exporter_Country,Country_Name,`Importer ISO3`,Resource,Year,Value,Weight,Exporter_Region)%>%
      left_join(comtrade_countrycode_ISO,by=c("Country_Name"))%>%
      rename(iso="Importer ISO3")%>%
      mutate(iso=tolower(iso))%>%
      left_join(iso_GCAM_regID,by=c("iso")) %>%
      left_join(GCAM_region_names,by=c("GCAM_region_ID"))%>%
      rename(Importer_Region = region, Importer_Country = Country_Name)%>%
      select(Exporter_Country,Importer_Country,Resource,Year,Value,Weight,Exporter_Region,Importer_Region) -> Rt_iron_steel_bilateral_trade_data

    #aggregate bi-lateral trade data by GCAM_region
    Rt_iron_steel_bilateral_trade_data %>%
      group_by(Year,Exporter_Region,Importer_Region)%>%
      mutate(Weight=sum(Weight)) %>%
      distinct(Year,Exporter_Region,Importer_Region,Weight)-> intra_regional_trade

    #estimate intra regional trade (percentage) across GCAM_regions and years
    intra_regional_trade %>%
      group_by(Year,Exporter_Region)%>%
      mutate(percent_trade=((Weight/sum(Weight))*100))%>%
      filter(Exporter_Region == Importer_Region) %>%
      rename(year=Year,GCAM_region=Exporter_Region) %>%
      select(year,GCAM_region,percent_trade)%>%
      ungroup()-> intra_regional_trade_pct

    #estimate intra regional trade (Mt) across GCAM_regions and years
    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      filter(metric=="exports_reval") %>%
      #use the year 2000's intra regional trade data for historical years where this data is unavailable
      left_join(intra_regional_trade_pct %>%
                  filter(year %in% c("2000")) %>%
                  rename(temp_percent_trade=percent_trade)%>%
                  select(GCAM_region,temp_percent_trade),by=c("GCAM_region")) %>%
      #add intra regional trade for model years where data is available
      left_join(intra_regional_trade_pct,by=c("year","GCAM_region")) %>%
      mutate(percent_trade=ifelse(is.na(percent_trade),temp_percent_trade,percent_trade),
             intra_exports=value*percent_trade/100,
             intra_exports=ifelse(is.na(intra_exports),0,intra_exports))%>%
      select(GCAM_region,year,intra_exports)-> intra_regional_trade_Mt_R_Y

    #remove intra region trade from imports and exports and add this amount to domestic supply
    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      left_join(intra_regional_trade_Mt_R_Y,by=c("GCAM_region","year"))%>%
      mutate(value=ifelse(metric %in% c("exports_reval","imports_reval"),value-intra_exports,value),
             value=ifelse(metric %in% c("domestic_supply"),value+intra_exports,value))%>%
      select(-intra_exports)-> LB1092.Tradebalance_iron_steel_Mt_R_Y


    # Produce outputs
    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      add_title("Gross trade of semi-finished and finished steel, by region / year.") %>%
      add_units("Mt") %>%
      add_comments("Determined from WSA steel production, consumption, imports, and exports data; only includes trade between countries in different GCAM regions") %>%
      add_precursors("energy/WSA_steel_prod_cons_1970_2018",
                     "energy/WSA_steel_trade_1970_2018",
                     "energy/mappings/WSA_gcam_mapping",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/Rt_iron_steel_bilateral_trade",
                     "common/GCAM_region_names",
                     "common/iso_GCAM_regID") -> LB1092.Tradebalance_iron_steel_Mt_R_Y

    return_data(LB1092.Tradebalance_iron_steel_Mt_R_Y)

  } else {
    stop("Unknown command")
  }
}
