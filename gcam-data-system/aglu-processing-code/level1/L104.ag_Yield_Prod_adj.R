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
logstart( "L104.ag_Yield_Prod_adj.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Adjustment to yield and production by GCAM region / commodity / year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data from previous files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L101.ag_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Prod_Mt_R_C_Y" )
L101.ag_HA_bm2_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_HA_bm2_R_C_Y" )
L103.ag_Prod_frac_R_C_AEZ<-readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_frac_R_C_AEZ")
L103.ag_Prod_Mt_R_C_Y_AEZ<-readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_AEZ")
L103.ag_HA_bm2_R_C_Y_AEZ<-readdata( "AGLU_LEVEL1_DATA", "L103.ag_HA_bm2_R_C_Y_AEZ")
L103.ag_Yield_kgm2_R_C_Y_AEZ<-readdata( "AGLU_LEVEL1_DATA", "L103.ag_Yield_kgm2_R_C_Y_AEZ")

# -----------------------------------------------------------------------------
# 2. Perform computations
#Specify which of the historical years will be used for determining maximum yields
X_Yield_Year <- X_AGLU_historical_years[ length( X_AGLU_historical_years ) ] 
USA_regID <- iso_GCAM_regID$GCAM_region_ID[ iso_GCAM_regID$iso == "usa" ]

#Define maximum allowable yields for each region / crop / AEZ. Using USA or all regions for palmfruit, and in the most recent historical year
L104.ag_maxYield_kgm2_C <- L103.ag_Yield_kgm2_R_C_Y_AEZ[ L103.ag_Yield_kgm2_R_C_Y_AEZ$GCAM_region_ID == USA_regID |
      L103.ag_Yield_kgm2_R_C_Y_AEZ$GCAM_commodity %in% c( "PalmFruit", "SugarCrop" ),
      c( R_C_AEZ, X_AGLU_historical_years[ length( X_AGLU_historical_years ) ] ) ]
L104.ag_maxYield_kgm2_C <- aggregate( L104.ag_maxYield_kgm2_C[ X_Yield_Year ],
      by=as.list( L104.ag_maxYield_kgm2_C[ C ] ), max )

#Calculate the regional average yield for each region and crop, setting NaN to 0. Put this in a table with the same format as the yields
L104.ag_Yield_kgm2_R_C_Y <- L101.ag_Prod_Mt_R_C_Y
L104.ag_Yield_kgm2_R_C_Y[ X_AGLU_historical_years ] <- L101.ag_Prod_Mt_R_C_Y[ X_AGLU_historical_years ] / L101.ag_HA_bm2_R_C_Y[
      match( vecpaste( L101.ag_Prod_Mt_R_C_Y[ R_C ] ), vecpaste( L101.ag_HA_bm2_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]
L104.ag_Yield_kgm2_R_C_Y[ is.na( L104.ag_Yield_kgm2_R_C_Y ) ] <- 0
L104.ag_avgYield_kgm2_R_C_Y_AEZ <- L103.ag_Yield_kgm2_R_C_Y_AEZ
L104.ag_avgYield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ] <- L104.ag_Yield_kgm2_R_C_Y[
      match( vecpaste( L104.ag_avgYield_kgm2_R_C_Y_AEZ[ R_C ] ), vecpaste( L104.ag_Yield_kgm2_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

#Match in production fraction (by region/crop/aez) and maximum yield (by crop) into table of historical yields
L104.ag_Yield_kgm2_R_C_Y_AEZ <- L103.ag_Yield_kgm2_R_C_Y_AEZ
L104.ag_Prod_frac_R_C_AEZ.melt <- melt( L103.ag_Prod_frac_R_C_AEZ, id.vars = R_C, variable_name = AEZ )
L104.ag_Yield_kgm2_R_C_Y_AEZ$Prod_frac <- L104.ag_Prod_frac_R_C_AEZ.melt$value[
      match( vecpaste( L104.ag_Yield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ), vecpaste( L104.ag_Prod_frac_R_C_AEZ.melt[ R_C_AEZ] ) ) ]
L104.ag_Yield_kgm2_R_C_Y_AEZ$MaxYield <- L104.ag_maxYield_kgm2_C[[X_Yield_Year]][
      match( L104.ag_Yield_kgm2_R_C_Y_AEZ$GCAM_commodity, L104.ag_maxYield_kgm2_C$GCAM_commodity ) ]

##NOTE: This step isn't working right now, but it also doesn't seem necessary
#Set rule: if yield in AEZ is greater than the USA max yield for that crop, and that AEZ accounts for less than given threshold
#of total regional production of that crop, set the yield in that AEZ equal to the regional average for that crop.
#printlog( "NOTE: Decreasing production in region / crop / AEZs where yield > assumed yield threshold," )
#printlog( "NOTE: and AEZ accounts for < assumed threshold percentage of region's production" )

for( i in 1:ncol( L104.ag_Yield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ] ) ){
	ifelse( L104.ag_Yield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ][[i]] > L104.ag_Yield_kgm2_R_C_Y_AEZ$MaxYield &
	             L104.ag_Yield_kgm2_R_C_Y_AEZ$Prod_frac < small_AEZ_prodfrac,
	        L104.ag_avgYield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ][[i]],
	        L104.ag_Yield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ][[i]] )	
}
L104.ag_Yield_kgm2_R_C_Y_AEZ <- L104.ag_Yield_kgm2_R_C_Y_AEZ[ c( R_C_AEZ, X_AGLU_historical_years ) ]
L104.ag_Yield_kgm2_R_C_Y_AEZ[ L104.ag_Yield_kgm2_R_C_Y_AEZ == Inf ] <- 0

#calculate adjusted production as harvested area times adjusted yields
L104.ag_Prod_Mt_R_C_Y_AEZ <- L104.ag_Yield_kgm2_R_C_Y_AEZ
L104.ag_Prod_Mt_R_C_Y_AEZ[ X_AGLU_historical_years ] <- L104.ag_Yield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ] * L103.ag_HA_bm2_R_C_Y_AEZ[
      match( vecpaste( L104.ag_Prod_Mt_R_C_Y_AEZ[ R_C_AEZ ] ), vecpaste( L103.ag_HA_bm2_R_C_Y_AEZ[ R_C_AEZ ] ) ),
      X_AGLU_historical_years ]
L104.ag_Prod_Mt_R_C_Y <- aggregate( L104.ag_Prod_Mt_R_C_Y_AEZ[ X_AGLU_historical_years ], by=as.list( L104.ag_Prod_Mt_R_C_Y_AEZ[ R_C ] ), sum )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L104.ag_Prod_Mt_R_C_Y_AEZ <- c( "Adjusted crop production by GCAM region / commodity / year / AEZ","Unit = Mt" )
comments.L104.ag_Prod_Mt_R_C_Y <- c( "Adjusted crop production by GCAM region / commodity / year","Unit = Mt" )
comments.L104.ag_Yield_kgm2_R_C_Y_AEZ <- c( "Adjusted agronomic yield by GCAM region / commodity / year / AEZ","Unit = kg.m2" )

writedata( L104.ag_Prod_Mt_R_C_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L104.ag_Prod_Mt_R_C_Y_AEZ", comments=comments.L104.ag_Prod_Mt_R_C_Y_AEZ )
writedata( L104.ag_Prod_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L104.ag_Prod_Mt_R_C_Y", comments=comments.L104.ag_Prod_Mt_R_C_Y )
writedata( L104.ag_Yield_kgm2_R_C_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L104.ag_Yield_kgm2_R_C_Y_AEZ", comments=comments.L104.ag_Yield_kgm2_R_C_Y_AEZ )

# Every script should finish with this line
logstop()
