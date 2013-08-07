# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L106.ag_an_NetExp_FAO_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Trade in primary agricultural goods and animal products from FAO, assigned to GCAM region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_cal_SUA <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_cal_SUA" )
FAO_an_items_cal_SUA <- readdata( "AGLU_MAPPINGS", "FAO_an_items_cal_SUA" )
L100.FAO_ag_Exp_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Exp_t" )
L100.FAO_ag_Imp_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Imp_t" )
L100.FAO_an_Exp_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Exp_t" )
L100.FAO_an_Imp_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Imp_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations

printlog( "Adding region and crop lookup vectors to FAO tables" )
#Add iso (standard country) name
L100.FAO_ag_Exp_t$iso <- AGLU_ctry$iso[ match( L100.FAO_ag_Exp_t$countries, AGLU_ctry$FAO_country ) ]
L100.FAO_ag_Imp_t$iso <- AGLU_ctry$iso[ match( L100.FAO_ag_Imp_t$countries, AGLU_ctry$FAO_country ) ]

L100.FAO_an_Exp_t$iso <- AGLU_ctry$iso[ match( L100.FAO_an_Exp_t$countries, AGLU_ctry$FAO_country ) ]
L100.FAO_an_Imp_t$iso <- AGLU_ctry$iso[ match( L100.FAO_an_Imp_t$countries, AGLU_ctry$FAO_country ) ]

#Match in the region from the iso
L100.FAO_ag_Exp_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_ag_Exp_t$iso, iso_GCAM_regID$iso ) ]
L100.FAO_ag_Imp_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_ag_Imp_t$iso, iso_GCAM_regID$iso ) ]

L100.FAO_an_Exp_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_an_Exp_t$iso, iso_GCAM_regID$iso ) ]
L100.FAO_an_Imp_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_an_Imp_t$iso, iso_GCAM_regID$iso ) ]

#add lookup vectors for GCAM commodities
L100.FAO_ag_Exp_t$GCAM_commodity <- FAO_ag_items_cal_SUA$GCAM_commodity[ match( L100.FAO_ag_Exp_t$item, FAO_ag_items_cal_SUA$item ) ]
L100.FAO_ag_Imp_t$GCAM_commodity <- FAO_ag_items_cal_SUA$GCAM_commodity[ match( L100.FAO_ag_Imp_t$item, FAO_ag_items_cal_SUA$item ) ]

L100.FAO_an_Exp_t$GCAM_commodity <- FAO_an_items_cal_SUA$GCAM_commodity[ match( L100.FAO_an_Exp_t$item, FAO_an_items_cal_SUA$item ) ]
L100.FAO_an_Imp_t$GCAM_commodity <- FAO_an_items_cal_SUA$GCAM_commodity[ match( L100.FAO_an_Imp_t$item, FAO_an_items_cal_SUA$item ) ]

#build tables collapsed by GCAM region and commodity
printlog("Collapsing FAO trade data into GCAM regions and commodities")
L106.ag_Exp_t_R_C_Y <- aggregate( L100.FAO_ag_Exp_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_ag_Exp_t[ R_C ] ), sum )
L106.ag_Imp_t_R_C_Y <- aggregate( L100.FAO_ag_Imp_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_ag_Imp_t[ R_C ] ), sum )

L106.an_Exp_t_R_C_Y <- aggregate( L100.FAO_an_Exp_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_an_Exp_t[ R_C ] ), sum )
L106.an_Imp_t_R_C_Y <- aggregate( L100.FAO_an_Imp_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_an_Imp_t[ R_C ] ), sum )

#Build tables with gross exports and imports, and calculate net exports as exports minus imports
L106.ag_NetExp_t_R_C_Y <- L106.ag_Exp_t_R_C_Y
L106.ag_NetExp_t_R_C_Y[ X_AGLU_historical_years ] <- L106.ag_Exp_t_R_C_Y[ X_AGLU_historical_years ] - L106.ag_Imp_t_R_C_Y[
      match( vecpaste( L106.ag_Exp_t_R_C_Y[ R_C ] ), vecpaste( L106.ag_Imp_t_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

L106.an_NetExp_t_R_C_Y <- L106.an_Exp_t_R_C_Y
L106.an_NetExp_t_R_C_Y[ X_AGLU_historical_years ] <- L106.an_Exp_t_R_C_Y[ X_AGLU_historical_years ] - L106.an_Imp_t_R_C_Y[
      match( vecpaste( L106.an_Exp_t_R_C_Y[ R_C ] ), vecpaste( L106.an_Imp_t_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

#Net exports must add to zero globally. Sum by year and crop type, and calculate global scalers for exports of each crop.
#NOTE: giving precedence to imports ( rather than exports ) of each crop. This is arbitrary but of little consequence, and generally reduces amount of trade.
printlog("NOTE: Adjusting exports in all regions so that global net exports add to 0")
L106.ag_Exp_t_C_Y <- aggregate( L106.ag_Exp_t_R_C_Y[ X_AGLU_historical_years ], by=as.list( L106.ag_Exp_t_R_C_Y[ C ] ), sum )
L106.ag_NetExp_t_C_Y <- aggregate( L106.ag_NetExp_t_R_C_Y[ X_AGLU_historical_years ], by=as.list( L106.ag_NetExp_t_R_C_Y[ C ] ), sum )
L106.ag_ExpScaler_C_Y <- L106.ag_Exp_t_C_Y
L106.ag_ExpScaler_C_Y[ X_AGLU_historical_years ] <- ( L106.ag_Exp_t_C_Y[ X_AGLU_historical_years ] - L106.ag_NetExp_t_C_Y[ X_AGLU_historical_years ] ) /
      L106.ag_Exp_t_C_Y[ X_AGLU_historical_years ]

L106.an_Exp_t_C_Y <- aggregate( L106.an_Exp_t_R_C_Y[ X_AGLU_historical_years ], by=as.list( L106.an_Exp_t_R_C_Y[ C ] ), sum )
L106.an_NetExp_t_C_Y <- aggregate( L106.an_NetExp_t_R_C_Y[ X_AGLU_historical_years ], by=as.list( L106.an_NetExp_t_R_C_Y[ C ] ), sum )
L106.an_ExpScaler_C_Y <- L106.an_Exp_t_C_Y
L106.an_ExpScaler_C_Y[ X_AGLU_historical_years ] <- ( L106.an_Exp_t_C_Y[ X_AGLU_historical_years ] - L106.an_NetExp_t_C_Y[ X_AGLU_historical_years ] ) /
      L106.an_Exp_t_C_Y[ X_AGLU_historical_years ]

#Adjust exports and recompile table, removing gross exports and imports
L106.ag_Exp_t_R_C_Y_adj <- L106.ag_Exp_t_R_C_Y
L106.ag_Exp_t_R_C_Y_adj[ X_AGLU_historical_years ] <- L106.ag_Exp_t_R_C_Y[ X_AGLU_historical_years ] * L106.ag_ExpScaler_C_Y[
      match( L106.ag_Exp_t_R_C_Y$GCAM_commodity, L106.ag_ExpScaler_C_Y$GCAM_commodity ),
      X_AGLU_historical_years ]
L106.ag_NetExp_t_R_C_Y_adj <- L106.ag_Exp_t_R_C_Y_adj
L106.ag_NetExp_t_R_C_Y_adj[ X_AGLU_historical_years ] <- L106.ag_Exp_t_R_C_Y_adj[ X_AGLU_historical_years ] - L106.ag_Imp_t_R_C_Y[
      match( vecpaste( L106.ag_Exp_t_R_C_Y_adj[ R_C ] ), vecpaste( L106.ag_Imp_t_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

L106.an_Exp_t_R_C_Y_adj <- L106.an_Exp_t_R_C_Y
L106.an_Exp_t_R_C_Y_adj[ X_AGLU_historical_years ] <- L106.an_Exp_t_R_C_Y[ X_AGLU_historical_years ] * L106.an_ExpScaler_C_Y[
      match( L106.an_Exp_t_R_C_Y$GCAM_commodity, L106.an_ExpScaler_C_Y$GCAM_commodity ),
      X_AGLU_historical_years ]
L106.an_NetExp_t_R_C_Y_adj <- L106.an_Exp_t_R_C_Y_adj
L106.an_NetExp_t_R_C_Y_adj[ X_AGLU_historical_years ] <- L106.an_Exp_t_R_C_Y_adj[ X_AGLU_historical_years ] - L106.an_Imp_t_R_C_Y[
      match( vecpaste( L106.an_Exp_t_R_C_Y_adj[ R_C ] ), vecpaste( L106.an_Imp_t_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

#Convert to desired units ( Mt )
printlog( "Converting to Mt" )
L106.ag_NetExp_Mt_R_C_Y <- L106.ag_NetExp_t_R_C_Y_adj
L106.ag_NetExp_Mt_R_C_Y[ X_AGLU_historical_years ] <- L106.ag_NetExp_t_R_C_Y_adj[ X_AGLU_historical_years ] * conv_t_Mt

L106.an_NetExp_Mt_R_C_Y <- L106.an_NetExp_t_R_C_Y_adj
L106.an_NetExp_Mt_R_C_Y[ X_AGLU_historical_years ] <- L106.an_NetExp_t_R_C_Y_adj[ X_AGLU_historical_years ] * conv_t_Mt

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L106.ag_NetExp_Mt_R_C_Y <- c( "Net exports of primary agricultural goods by GCAM region / commodity / year","Unit = Mt" )
comments.L106.an_NetExp_Mt_R_C_Y <- c( "Net exports of animal products by GCAM region / commodity / year","Unit = Mt" )

#write tables as CSV files
writedata( L106.ag_NetExp_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA",fn="L106.ag_NetExp_Mt_R_C_Y", comments=comments.L106.ag_NetExp_Mt_R_C_Y )
writedata( L106.an_NetExp_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA",fn="L106.an_NetExp_Mt_R_C_Y", comments=comments.L106.an_NetExp_Mt_R_C_Y )

# Every script should finish with this line
logstop()
