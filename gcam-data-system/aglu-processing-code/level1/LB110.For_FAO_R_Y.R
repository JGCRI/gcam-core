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
logstart( "LB110.For_FAO_R_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Forest product data from FAO, assigned to GCAM region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.FAO_For_Prod_m3 <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_For_Prod_m3" )
L100.FAO_For_Imp_m3 <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_For_Imp_m3" )
L100.FAO_For_Exp_m3 <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_For_Exp_m3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Indicate the flow on each table and combine (rbind). Multiply imports by -1 and call both imports and exports the same flow
L100.FAO_For_Prod_m3$flow <- "Prod_m3"
L100.FAO_For_Imp_m3$flow <- "NetExp_m3"
L100.FAO_For_Imp_m3[ X_AGLU_historical_years ] <- L100.FAO_For_Imp_m3[ X_AGLU_historical_years ] * -1
L100.FAO_For_Exp_m3$flow <- "NetExp_m3"
L110.FAO_For_ALL_m3 <- rbind( L100.FAO_For_Prod_m3, L100.FAO_For_Imp_m3, L100.FAO_For_Exp_m3 )

#Add regionID lookup vectors and aggregate
printlog( "Adding region lookup vectors and aggregating forest production and trade by GCAM region" )
L110.FAO_For_ALL_m3$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L110.FAO_For_ALL_m3$iso, iso_GCAM_regID$iso ) ]
L110.FAO_For_ALL_m3$GCAM_commodity <- "Forest"

#Convert units in the aggregation step
L110.FAO_For_ALL_bm3_prelim <- aggregate( L110.FAO_For_ALL_m3[ X_AGLU_historical_years ] * conv_m3_bm3,
      by=as.list( L110.FAO_For_ALL_m3[ c( R_C, "flow" ) ] ), sum )
L110.FAO_For_ALL_bm3_prelim$flow <- sub( "_m3", "_bm3", L110.FAO_For_ALL_bm3_prelim$flow )

#Build volumetric balance table
printlog( "Building mass balance" )
L110.For_ALL_bm3_R_Y <- recast( L110.FAO_For_ALL_bm3_prelim, GCAM_region_ID + GCAM_commodity +variable ~ flow, id.var = c( R_C, "flow" ) )
names( L110.For_ALL_bm3_R_Y )[ names( L110.For_ALL_bm3_R_Y ) == "variable" ] <- Y
L110.For_ALL_bm3_R_Y$year <- as.numeric( sub( "X", "", L110.For_ALL_bm3_R_Y$year ) )
L110.For_ALL_bm3_R_Y$Cons_bm3 <- L110.For_ALL_bm3_R_Y$Prod_bm3 - L110.For_ALL_bm3_R_Y$NetExp_bm3

#Re-order columns
For_Flow_cols <- c( "Prod_bm3", "NetExp_bm3", "Cons_bm3" )
L110.For_ALL_bm3_R_Y <- L110.For_ALL_bm3_R_Y[ c( R_C_Y, For_Flow_cols ) ]

printlog( "NOTE: Scaling consumption so that sum of global consumption equals production" )
L110.For_ALL_bm3_glbl_Y <- aggregate( L110.For_ALL_bm3_R_Y[ For_Flow_cols ], by=as.list( L110.For_ALL_bm3_R_Y[ C_Y ] ), sum )
L110.For_ALL_bm3_glbl_Y$Cons_scaler <- L110.For_ALL_bm3_glbl_Y$Prod_bm3 / L110.For_ALL_bm3_glbl_Y$Cons_bm3
L110.For_ALL_bm3_R_Y$Cons_scaler <- L110.For_ALL_bm3_glbl_Y$Cons_scaler[match( L110.For_ALL_bm3_R_Y$year, L110.For_ALL_bm3_glbl_Y$year ) ]

L110.For_ALL_bm3_R_Y$Cons_bm3 <- L110.For_ALL_bm3_R_Y$Cons_bm3 * L110.For_ALL_bm3_R_Y$Cons_scaler
L110.For_ALL_bm3_R_Y$NetExp_bm3 <- L110.For_ALL_bm3_R_Y$Prod_bm3 - L110.For_ALL_bm3_R_Y$Cons_bm3

printlog( "Building adjusted forest mass balance table" )
L110.For_ALL_bm3_R_Y <- L110.For_ALL_bm3_R_Y[ c( R_C_Y, For_Flow_cols ) ]

#Translate to full table for any regions with no forest data
L110.For_ALL_bm3_R_Y <- translate_to_full_table( L110.For_ALL_bm3_R_Y,
      R, unique( iso_GCAM_regID[[R]] ),
      C, unique( L110.For_ALL_bm3_R_Y[[C]] ),
      Y, unique( L110.For_ALL_bm3_R_Y[[Y]] ),
      datacols = For_Flow_cols )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L110.For_ALL_bm3_R_Y <- c( "Forest products mass balance by GCAM region / year","Unit = bm3" )

#write tables as CSV files
writedata( L110.For_ALL_bm3_R_Y, domain="AGLU_LEVEL1_DATA",fn="L110.For_ALL_bm3_R_Y", comments=comments.L110.For_ALL_bm3_R_Y )

# Every script should finish with this line
logstop()
