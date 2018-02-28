# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu data system. Please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LA100.FAO_downscale_ctry.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Historical agricultural data from the FAO downscaled to modern country" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_HA_ha_PRODSTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_HA_ha_PRODSTAT" )
FAO_ag_Prod_t_PRODSTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Prod_t_PRODSTAT" )
FAO_ag_Exp_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Exp_t_SUA" )
FAO_ag_Feed_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Feed_t_SUA" )
FAO_ag_Food_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Food_t_SUA" )
FAO_ag_Imp_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Imp_t_SUA" )
FAO_an_Exp_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Exp_t_SUA" )
FAO_an_Food_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Food_t_SUA" )
FAO_an_Imp_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Imp_t_SUA" )
FAO_an_Prod_t_SUA <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Prod_t_SUA" )
FAO_an_Stocks <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Stocks" )
FAO_an_Dairy_Stocks <- readdata( "AGLU_LEVEL0_DATA", "FAO_an_Dairy_Stocks" )
FAO_CL_kha_RESOURCESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_CL_kha_RESOURCESTAT" )
FAO_fallowland_kha_RESOURCESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_fallowland_kha_RESOURCESTAT" )
FAO_harv_CL_kha_RESOURCESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_harv_CL_kha_RESOURCESTAT" )
FAO_Fert_Cons_tN_RESOURCESTAT_archv <- readdata( "AGLU_LEVEL0_DATA", "FAO_Fert_Cons_tN_RESOURCESTAT_archv" )
FAO_Fert_Cons_tN_RESOURCESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_Fert_Cons_tN_RESOURCESTAT" )
FAO_Fert_Prod_tN_RESOURCESTAT_archv <- readdata( "AGLU_LEVEL0_DATA", "FAO_Fert_Prod_tN_RESOURCESTAT_archv" )
FAO_Fert_Prod_tN_RESOURCESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_Fert_Prod_tN_RESOURCESTAT" )
FAO_For_Exp_m3_FORESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_For_Exp_m3_FORESTAT" )
FAO_For_Imp_m3_FORESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_For_Imp_m3_FORESTAT" )
FAO_For_Prod_m3_FORESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_For_Prod_m3_FORESTAT" )

# -----------------------------------------------------------------------------
# 2. Perform computations
country_colnames <- c( "countries", "country.codes" )
item_colnames <- c( "item", "item.codes" )
element_colnames <- c( "element", "element.codes" )

#Merge resourcestat fertilizer databases with "archive" years (1961-2002) and more recent years (2002-2010)
#FAOSTAT notes that the methods changed between the two datasets; we ignore this discrepancy but use the 2002 data from the more recent dataset
FAO_Fert_Cons_tN_RESOURCESTAT_archv <- FAO_Fert_Cons_tN_RESOURCESTAT_archv[ names( FAO_Fert_Cons_tN_RESOURCESTAT_archv ) != "X2002" ]
FAO_Fert_Prod_tN_RESOURCESTAT_archv <- FAO_Fert_Prod_tN_RESOURCESTAT_archv[ names( FAO_Fert_Prod_tN_RESOURCESTAT_archv ) != "X2002" ]
#Replace the item and element code names with what is used in the more recent datasets
FAO_Fert_Cons_tN_RESOURCESTAT_archv[ c( item_colnames, element_colnames ) ] <-
      FAO_Fert_Cons_tN_RESOURCESTAT[ 1, c( item_colnames, element_colnames ) ][ rep( 1, times = nrow( FAO_Fert_Cons_tN_RESOURCESTAT_archv ) ), ]
FAO_Fert_Prod_tN_RESOURCESTAT_archv[ c( item_colnames, element_colnames ) ] <-
      FAO_Fert_Prod_tN_RESOURCESTAT[ 1, c( item_colnames, element_colnames ) ][ rep( 1, times = nrow( FAO_Fert_Prod_tN_RESOURCESTAT_archv ) ), ]

FAO_Fert_Cons_tN_RESOURCESTAT <- merge( FAO_Fert_Cons_tN_RESOURCESTAT_archv, FAO_Fert_Cons_tN_RESOURCESTAT, all.x = T, all.y = T )
FAO_Fert_Prod_tN_RESOURCESTAT <- merge( FAO_Fert_Prod_tN_RESOURCESTAT_archv, FAO_Fert_Prod_tN_RESOURCESTAT, all.x = T, all.y = T )

#Aggregate to complete the merge of the two datasets
FAO_Fert_Cons_tN_RESOURCESTAT <- aggregate( FAO_Fert_Cons_tN_RESOURCESTAT[ names( FAO_Fert_Cons_tN_RESOURCESTAT ) %in% X_FAO_historical_years ],
      by=as.list( FAO_Fert_Cons_tN_RESOURCESTAT[ c( country_colnames, item_colnames, element_colnames ) ] ), sum, na.rm = T )
FAO_Fert_Prod_tN_RESOURCESTAT <- aggregate( FAO_Fert_Prod_tN_RESOURCESTAT[ names( FAO_Fert_Prod_tN_RESOURCESTAT ) %in% X_FAO_historical_years ],
      by=as.list( FAO_Fert_Prod_tN_RESOURCESTAT[ c( country_colnames, item_colnames, element_colnames ) ] ), sum, na.rm = T )

#Some data in an_Stocks are in 1000s of heads instead of just heads so we will convert them
#to be in the same units.
#Also remove the units column to be consistent with the other FAO tables.
FAO_an_Stocks[ FAO_an_Stocks$units == "1000 Head", X_FAO_historical_years ] <- 
    FAO_an_Stocks[ FAO_an_Stocks$units == "1000 Head", X_FAO_historical_years ] * 1000
FAO_an_Stocks$units <- NULL
FAO_an_Dairy_Stocks$units <- NULL

#Assign an identifier to all databases, which will allow them to be re-split following the downscaling
FAO_ag_HA_ha_PRODSTAT$element <- "ag_HA_ha"
FAO_ag_Prod_t_PRODSTAT$element <- "ag_Prod_t"
FAO_ag_Exp_t_SUA$element <- "ag_Exp_t"
FAO_ag_Feed_t_SUA$element <- "ag_Feed_t"
FAO_ag_Food_t_SUA$element <- "ag_Food_t"
FAO_ag_Imp_t_SUA$element <- "ag_Imp_t"
FAO_an_Exp_t_SUA$element <- "an_Exp_t"
FAO_an_Food_t_SUA$element <- "an_Food_t"
FAO_an_Imp_t_SUA$element <- "an_Imp_t"
FAO_an_Prod_t_SUA$element <- "an_Prod_t"
FAO_an_Stocks$element <- "an_Stocks"
FAO_an_Dairy_Stocks$element <- "an_Dairy_Stocks"
FAO_CL_kha_RESOURCESTAT$element <- "CL_kha"
FAO_fallowland_kha_RESOURCESTAT$element <- "fallowland_kha"
FAO_harv_CL_kha_RESOURCESTAT$element <- "harv_CL_kha"
FAO_Fert_Cons_tN_RESOURCESTAT$element <- "Fert_Cons_tN"
FAO_Fert_Prod_tN_RESOURCESTAT$element <- "Fert_Prod_tN"
FAO_For_Exp_m3_FORESTAT$element <- "For_Exp_m3"
FAO_For_Imp_m3_FORESTAT$element <- "For_Imp_m3"
FAO_For_Prod_m3_FORESTAT$element <- "For_Prod_m3"

#Not all databases go to 2011. Extrapolate each dataset to 2011, just repeating the data for 2009/10. Where missing 1961, substitute 1962
FAO_data_ALL.list <- list( FAO_ag_Exp_t_SUA, FAO_ag_Feed_t_SUA, FAO_ag_Food_t_SUA, FAO_ag_Imp_t_SUA,
      FAO_an_Exp_t_SUA, FAO_an_Food_t_SUA, FAO_an_Imp_t_SUA, FAO_an_Prod_t_SUA, FAO_an_Stocks, FAO_an_Dairy_Stocks, FAO_Fert_Cons_tN_RESOURCESTAT,
      FAO_Fert_Prod_tN_RESOURCESTAT, FAO_ag_HA_ha_PRODSTAT, FAO_ag_Prod_t_PRODSTAT,
      FAO_CL_kha_RESOURCESTAT, FAO_fallowland_kha_RESOURCESTAT, FAO_harv_CL_kha_RESOURCESTAT,
      FAO_For_Exp_m3_FORESTAT, FAO_For_Imp_m3_FORESTAT, FAO_For_Prod_m3_FORESTAT )
for( i in 1:length( FAO_data_ALL.list ) ){
	if( is.null( FAO_data_ALL.list[[i]][["X1961"]] ) ) FAO_data_ALL.list[[i]][["X1961"]] <- FAO_data_ALL.list[[i]][["X1962"]]
	if( is.null( FAO_data_ALL.list[[i]][["X2010"]] ) ) FAO_data_ALL.list[[i]][["X2010"]] <- FAO_data_ALL.list[[i]][["X2009"]]
	if( is.null( FAO_data_ALL.list[[i]][["X2011"]] ) ) FAO_data_ALL.list[[i]][["X2011"]] <- FAO_data_ALL.list[[i]][["X2009"]]
}

FAO_data_ALL <- do.call( rbind, FAO_data_ALL.list )

#Replace all missing values with 0
FAO_data_ALL[ is.na( FAO_data_ALL ) ] <- 0

# Match the iso names in
FAO_data_ALL$iso <- AGLU_ctry$iso[ match( FAO_data_ALL$countries, AGLU_ctry$FAO_country ) ]

#Downscale countries individually
#NOTE: This is complicated. The FAO data needs to be downscaled to all FAO historical years (i.e. back
# to 1961 regardless of when we are starting our historical time series). Otherwise the early historical
# years will get averaged with zeroes.
#Czechoslovakia
L100.FAO_data_ALL_cze4downscale <- subset( FAO_data_ALL, iso %in% AGLU_ctry$iso[ AGLU_ctry$FAO_country == "Czechoslovakia" ] )
L100.FAO_data_ALL_cze <- downscale_FAO_country( L100.FAO_data_ALL_cze4downscale, "Czechoslovakia", 1993,
                                                years = FAO_historical_years )

#USSR
L100.FAO_data_ALL_ussr4downscale <- subset( FAO_data_ALL, iso %in% AGLU_ctry$iso[ AGLU_ctry$FAO_country == "USSR" ] )
L100.FAO_data_ALL_ussr <- downscale_FAO_country( L100.FAO_data_ALL_ussr4downscale, "USSR", 1992,
                                                 years = FAO_historical_years  )

#Yugoslavia
L100.FAO_data_ALL_yug4downscale <- subset( FAO_data_ALL, iso %in% AGLU_ctry$iso[ AGLU_ctry$FAO_country == "Yugoslav SFR" ] )
L100.FAO_data_ALL_yug <- downscale_FAO_country( L100.FAO_data_ALL_yug4downscale, "Yugoslav SFR", 1992,
                                                years = FAO_historical_years  )

#Combine these downscaled databases and drop these countries from the full databases
L100.FAO_data_ALL_downscaled <- rbind( L100.FAO_data_ALL_cze, L100.FAO_data_ALL_ussr, L100.FAO_data_ALL_yug )
L100.FAO_data_ALL <- subset( FAO_data_ALL, !iso %in% unique( L100.FAO_data_ALL_downscaled$iso ) )

#Combine (rbind) downscaled data back into the full databases
L100.FAO_data_ALL <- rbind( L100.FAO_data_ALL, L100.FAO_data_ALL_downscaled )

#Drop rows where all years are 0
L100.FAO_data_ALL <- L100.FAO_data_ALL[ rowSums( L100.FAO_data_ALL[ X_FAO_historical_years ] ) !=0, ]

#Calculate rolling five-year averages from available data
L100.FAO_data_ALL_5yr <- L100.FAO_data_ALL
#In the first and last two years, use the 3 and 4 available years
L100.FAO_data_ALL_5yr[X_FAO_historical_years][1] <- apply( L100.FAO_data_ALL[ X_FAO_historical_years ][1:3], 1, mean )
L100.FAO_data_ALL_5yr[X_FAO_historical_years][2] <- apply( L100.FAO_data_ALL[ X_FAO_historical_years ][1:4], 1, mean )
for( i in 3:(ncol( L100.FAO_data_ALL_5yr[X_FAO_historical_years] ) - 2 ) ) {
	L100.FAO_data_ALL_5yr[X_FAO_historical_years][,i] <- apply( L100.FAO_data_ALL[X_FAO_historical_years][ c( i-2, i-1, i, i+1, i+2 ) ], 1, mean )
}
L100.FAO_data_ALL_5yr[X_FAO_historical_years][length( X_FAO_historical_years ) - 1 ] <-
      apply( L100.FAO_data_ALL[ X_FAO_historical_years ][ ( length( X_FAO_historical_years ) - 3 ):length( X_FAO_historical_years ) ], 1, mean )
L100.FAO_data_ALL_5yr[X_FAO_historical_years][length( X_FAO_historical_years ) ] <-
      apply( L100.FAO_data_ALL[ X_FAO_historical_years ][ ( length( X_FAO_historical_years ) - 2 ):length( X_FAO_historical_years ) ], 1, mean )

#From here on, only use the specified AGLU historical years
L100.FAO_data_ALL_5yr <- L100.FAO_data_ALL_5yr[ c( country_colnames, item_colnames, element_colnames, "iso", X_AGLU_historical_years ) ]

#Re-split into separate tables for each element
L100.FAO_ag_HA_ha <- subset( L100.FAO_data_ALL_5yr, element == "ag_HA_ha" )
L100.FAO_ag_Prod_t <- subset( L100.FAO_data_ALL_5yr, element == "ag_Prod_t" )
L100.FAO_ag_Exp_t <- subset( L100.FAO_data_ALL_5yr, element == "ag_Exp_t" )
L100.FAO_ag_Feed_t <- subset( L100.FAO_data_ALL_5yr, element == "ag_Feed_t" )
L100.FAO_ag_Food_t <- subset( L100.FAO_data_ALL_5yr, element == "ag_Food_t" )
L100.FAO_ag_Imp_t <- subset( L100.FAO_data_ALL_5yr, element == "ag_Imp_t" )
L100.FAO_an_Exp_t <- subset( L100.FAO_data_ALL_5yr, element == "an_Exp_t" )
L100.FAO_an_Food_t <- subset( L100.FAO_data_ALL_5yr, element == "an_Food_t" )
L100.FAO_an_Imp_t <- subset( L100.FAO_data_ALL_5yr, element == "an_Imp_t" )
L100.FAO_an_Prod_t <- subset( L100.FAO_data_ALL_5yr, element == "an_Prod_t" )
L100.FAO_an_Stocks <- subset( L100.FAO_data_ALL_5yr, element == "an_Stocks" )
L100.FAO_an_Dairy_Stocks <- subset( L100.FAO_data_ALL_5yr, element == "an_Dairy_Stocks" )
L100.FAO_CL_kha <- subset( L100.FAO_data_ALL_5yr, element == "CL_kha" )
L100.FAO_fallowland_kha <- subset( L100.FAO_data_ALL_5yr, element == "fallowland_kha" )
L100.FAO_harv_CL_kha <- subset( L100.FAO_data_ALL_5yr, element == "harv_CL_kha" )
L100.FAO_Fert_Cons_tN <- subset( L100.FAO_data_ALL_5yr, element == "Fert_Cons_tN" )
L100.FAO_Fert_Prod_tN <- subset( L100.FAO_data_ALL_5yr, element == "Fert_Prod_tN" )
L100.FAO_For_Exp_m3 <- subset( L100.FAO_data_ALL_5yr, element == "For_Exp_m3" )
L100.FAO_For_Imp_m3 <- subset( L100.FAO_data_ALL_5yr, element == "For_Imp_m3" )
L100.FAO_For_Prod_m3 <- subset( L100.FAO_data_ALL_5yr, element == "For_Prod_m3" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.FAO_ag_HA_ha <- c( "FAO agricultural harvested area by country / item / year","Unit = t" )
comments.L100.FAO_ag_Prod_t <- c( "FAO agricultural production by country / item / year","Unit = t" )
comments.L100.FAO_ag_Exp_t <- c( "FAO agricultural exports by country / item / year","Unit = t" )
comments.L100.FAO_ag_Feed_t <- c( "FAO agricultural feed by country / item / year","Unit = t" )
comments.L100.FAO_ag_Food_t <- c( "FAO agricultural food consumption by country / item / year","Unit = t" )
comments.L100.FAO_ag_Imp_t <- c( "FAO agricultural imports by country / item / year","Unit = t" )
comments.L100.FAO_an_Exp_t <- c( "FAO animal exports by country / item / year","Unit = t" )
comments.L100.FAO_an_Food_t <- c( "FAO animal food consumption by country / item / year","Unit = t" )
comments.L100.FAO_an_Imp_t <- c( "FAO animal imports by country / item / year","Unit = t" )
comments.L100.FAO_an_Prod_t <- c( "FAO animal production by country / item / year","Unit = t" )
comments.L100.FAO_an_Stocks <- c( "FAO animal stocks country / item / year","Unit = number" )
comments.L100.FAO_an_Dairy_Stocks <- c( "FAO dairy producing animal stocks country / item / year","Unit = number" )
comments.L100.FAO_CL_kha <- c( "FAO cropland area by country / year","Unit = kha" )
comments.L100.FAO_fallowland_kha <- c( "FAO fallow land area by country / year","Unit = kha" )
comments.L100.FAO_harv_CL_kha <- c( "FAO harvested cropland (temporary crops) by country / year","Unit = kha" )
comments.L100.FAO_Fert_Cons_tN <- c( "FAO fertilizer consumption by country / year","Unit = tonnes of N" )
comments.L100.FAO_Fert_Prod_tN <- c( "FAO fertilizer production by country / year","Unit = tonnes of N" )
comments.L100.FAO_For_Exp_m3 <- c( "FAO forestry exports by country / year","Unit = m3" )
comments.L100.FAO_For_Imp_m3 <- c( "FAO forestry imports by country / year","Unit = m3" )
comments.L100.FAO_For_Prod_m3 <- c( "FAO forestry production by country / year","Unit = m3" )

#write tables as CSV files
writedata( L100.FAO_ag_HA_ha, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_HA_ha", comments=comments.L100.FAO_ag_HA_ha )
writedata( L100.FAO_ag_Prod_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_Prod_t", comments=comments.L100.FAO_ag_Prod_t )
writedata( L100.FAO_ag_Exp_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_Exp_t", comments=comments.L100.FAO_ag_Exp_t )
writedata( L100.FAO_ag_Feed_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_Feed_t", comments=comments.L100.FAO_ag_Feed_t )
writedata( L100.FAO_ag_Food_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_Food_t", comments=comments.L100.FAO_ag_Food_t )
writedata( L100.FAO_ag_Imp_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_ag_Imp_t", comments=comments.L100.FAO_ag_Imp_t )
writedata( L100.FAO_an_Exp_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Exp_t", comments=comments.L100.FAO_an_Exp_t )
writedata( L100.FAO_an_Food_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Food_t", comments=comments.L100.FAO_an_Food_t )
writedata( L100.FAO_an_Imp_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Imp_t", comments=comments.L100.FAO_an_Imp_t )
writedata( L100.FAO_an_Prod_t, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Prod_t", comments=comments.L100.FAO_an_Prod_t )
writedata( L100.FAO_an_Stocks, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Stocks", comments=comments.L100.FAO_an_Stocks )
writedata( L100.FAO_an_Dairy_Stocks, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_an_Dairy_Stocks", comments=comments.L100.FAO_an_Dairy_Stocks )
writedata( L100.FAO_CL_kha, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_CL_kha", comments=comments.L100.FAO_CL_kha )
writedata( L100.FAO_fallowland_kha, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_fallowland_kha", comments=comments.L100.FAO_fallowland_kha )
writedata( L100.FAO_harv_CL_kha, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_harv_CL_kha", comments=comments.L100.FAO_harv_CL_kha )
writedata( L100.FAO_Fert_Cons_tN, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_Fert_Cons_tN", comments=comments.L100.FAO_Fert_Cons_tN )
writedata( L100.FAO_Fert_Prod_tN, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_Fert_Prod_tN", comments=comments.L100.FAO_Fert_Prod_tN )
writedata( L100.FAO_For_Exp_m3, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_For_Exp_m3", comments=comments.L100.FAO_For_Exp_m3 )
writedata( L100.FAO_For_Imp_m3, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_For_Imp_m3", comments=comments.L100.FAO_For_Imp_m3 )
writedata( L100.FAO_For_Prod_m3, domain="AGLU_LEVEL1_DATA", fn="L100.FAO_For_Prod_m3", comments=comments.L100.FAO_For_Prod_m3 )

# Every script should finish with this line
logstop()
