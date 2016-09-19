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
logstart( "LB142.ag_Fert_IO_R_C_Y_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Fertilizer input-output coefficients by region / crop / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )
L100.FAO_ag_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Prod_t" )
L100.FAO_Fert_Cons_tN <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_Fert_Cons_tN" )
L100.FAO_Fert_Prod_tN <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_Fert_Prod_tN" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata ( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L141.ag_Fert_Cons_MtN_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L141.ag_Fert_Cons_MtN_ctry_crop" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compiling fertilizer production and consumption by country and by GCAM region" )
printlog( "Adjusting production so that global production equals consumption" )
L142.ag_Fert_Cons_tN_ctry_Y <- L100.FAO_Fert_Cons_tN
L142.ag_Fert_Prod_tN_ctry_Y <- L100.FAO_Fert_Prod_tN
for( i in 1:ncol( L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ] ) ){
	L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ][i] <- L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ][i] *
	   colSums( L100.FAO_Fert_Cons_tN[ X_AGLU_historical_years ][i] ) /
	   colSums( L100.FAO_Fert_Prod_tN[ X_AGLU_historical_years ][i] )
}

L142.ag_Fert_Prod_MtN_ctry_Y <- data.frame( L142.ag_Fert_Prod_tN_ctry_Y["iso"], L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ] * conv_t_Mt )

L142.ag_Fert_Cons_tN_ctry_Y[[R]] <- iso_GCAM_regID[[R]][ match( L142.ag_Fert_Cons_tN_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L142.ag_Fert_Prod_tN_ctry_Y[[R]] <- iso_GCAM_regID[[R]][ match( L142.ag_Fert_Prod_tN_ctry_Y$iso, iso_GCAM_regID$iso ) ]

L142.ag_Fert_Cons_MtN_R_Y <- aggregate( L142.ag_Fert_Cons_tN_ctry_Y[ X_AGLU_historical_years ] * conv_t_Mt,
                                        by = L142.ag_Fert_Cons_tN_ctry_Y[ R ], sum )
L142.ag_Fert_Prod_MtN_R_Y <- aggregate( L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ] * conv_t_Mt,
                                        by = L142.ag_Fert_Prod_tN_ctry_Y[ R ], sum )

L142.ag_Fert_Cons_MtN_R_Y[[C]] <- Fert_name
L142.ag_Fert_Prod_MtN_R_Y[[C]] <- Fert_name

#Make sure that no regions are missing
L142.ag_Fert_Cons_MtN_R_Y <- translate_to_full_table( L142.ag_Fert_Cons_MtN_R_Y,
      var1 = R, var1_values = sort( unique( iso_GCAM_regID[[R]] ) ),
      var2 = C, var2_values = Fert_name )
L142.ag_Fert_Prod_MtN_R_Y <- translate_to_full_table( L142.ag_Fert_Prod_MtN_R_Y,
      var1 = R, var1_values = sort( unique( iso_GCAM_regID[[R]] ) ),
      var2 = C, var2_values = Fert_name )

printlog( "Calculating net exports of N fertilizer" )
L142.ag_Fert_NetExp_MtN_R_Y <- data.frame( L142.ag_Fert_Prod_MtN_R_Y[ R_C ],
      L142.ag_Fert_Prod_MtN_R_Y[ X_AGLU_historical_years ] - L142.ag_Fert_Cons_MtN_R_Y[ X_AGLU_historical_years ] )

printlog( "Downscaling fertilizer demands by country and crop to GLU" )
printlog( "NOTE: Allocating fertilizer consumption to GLUs on the basis of production, not harvested area")
L142.ag_Fert_Cons_MtN_ctry_crop_GLU <- L100.LDS_ag_prod_t
L142.ag_Prod_t_ctry_crop <- aggregate( L100.LDS_ag_prod_t[ "value" ],
                                       by = L100.LDS_ag_prod_t[ c( "iso", "GTAP_crop" ) ], sum )
L142.ag_Fert_Cons_MtN_ctry_crop_GLU$Prod_share <- L142.ag_Fert_Cons_MtN_ctry_crop_GLU$value /
  L142.ag_Prod_t_ctry_crop$value[
    match( vecpaste( L142.ag_Fert_Cons_MtN_ctry_crop_GLU[ c( "iso", "GTAP_crop" ) ] ),
           vecpaste( L142.ag_Prod_t_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]

L142.ag_Fert_Cons_MtN_ctry_crop_GLU[[ "Fert_Cons_MtN" ]] <- L142.ag_Fert_Cons_MtN_ctry_crop_GLU$Prod_share *
      L141.ag_Fert_Cons_MtN_ctry_crop$Fert_Cons_MtN[
         match( vecpaste( L142.ag_Fert_Cons_MtN_ctry_crop_GLU[ c( "iso", "GTAP_crop" ) ] ),
                vecpaste( L141.ag_Fert_Cons_MtN_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]
L142.ag_Fert_Cons_MtN_ctry_crop_GLU[ is.na( L142.ag_Fert_Cons_MtN_ctry_crop_GLU ) ] <- 0

printlog( "Aggregating fertilizer demands by GCAM region, commodity, and GLU" )
L142.ag_Fert_Cons_MtN_ctry_crop_GLU[[R]] <- iso_GCAM_regID[[R]][
      match( L142.ag_Fert_Cons_MtN_ctry_crop_GLU$iso, iso_GCAM_regID$iso ) ]
L142.ag_Fert_Cons_MtN_ctry_crop_GLU[[C]] <- FAO_ag_items_PRODSTAT[[C]][
      match( L142.ag_Fert_Cons_MtN_ctry_crop_GLU$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L142.ag_Fert_Cons_MtN_R_C_GLU <- aggregate( L142.ag_Fert_Cons_MtN_ctry_crop_GLU[ "Fert_Cons_MtN" ],
                                            by = L142.ag_Fert_Cons_MtN_ctry_crop_GLU[ R_C_GLU ], sum )

printlog( "Calculating unscaled coefficients as unscaled fertilizer demands divided by production")
L142.ag_Fert_Cons_MtN_R_C_GLU$Prod_Mt <- L103.ag_Prod_Mt_R_C_Y_GLU[[X_base_year_IFA]][
  match( vecpaste( L142.ag_Fert_Cons_MtN_R_C_GLU[ R_C_GLU ] ),
         vecpaste( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] ) ) ]
L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO <- with( L142.ag_Fert_Cons_MtN_R_C_GLU, Fert_Cons_MtN / Prod_Mt )
L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO[ L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO == Inf ] <- 0
L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO[ is.na( L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO ) ] <- 0

#Melt production table, in order to match these coefficients in and compute unscaled historical Nfert
L142.ag_Fert_Cons_MtN_R_C_Y_GLU <- melt( L103.ag_Prod_Mt_R_C_Y_GLU, id.vars = R_C_GLU, variable.name = "Xyear", value.name = "Prod_Mt" )
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$year <- as.numeric( substr( L142.ag_Fert_Cons_MtN_R_C_Y_GLU$Xyear, 2, 5 ) )
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$Fert_IO_unscaled <- L142.ag_Fert_Cons_MtN_R_C_GLU$Fert_IO[
      match( vecpaste( L142.ag_Fert_Cons_MtN_R_C_Y_GLU[ c( R_C_GLU ) ] ),
             vecpaste( L142.ag_Fert_Cons_MtN_R_C_GLU[ c( R_C_GLU ) ] ) ) ]
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$Fert_Cons_MtN_unscaled <- with( L142.ag_Fert_Cons_MtN_R_C_Y_GLU, Prod_Mt * Fert_IO_unscaled )

#Melt regional fertilizer consumption by region and year
L142.ag_Fert_Cons_MtN_R_Y.melt <- melt( L142.ag_Fert_Cons_MtN_R_Y, id.vars = R_C, variable.name = "Xyear", value.name = "Fert_Cons_MtN" )
L142.ag_Fert_Cons_MtN_R_Y.melt$year <- as.numeric( substr( L142.ag_Fert_Cons_MtN_R_Y.melt$Xyear, 2, 5 ) )

#Compute region/year scalers so that consumption balances
L142.ag_Fert_ConsScaler_MtN_R_Y <- aggregate( L142.ag_Fert_Cons_MtN_R_C_Y_GLU[ "Fert_Cons_MtN_unscaled" ],
                                              by = L142.ag_Fert_Cons_MtN_R_C_Y_GLU[ R_Y ], sum )
L142.ag_Fert_ConsScaler_MtN_R_Y$Fert_Cons_MtN <- L142.ag_Fert_Cons_MtN_R_Y.melt$Fert_Cons_MtN[
      match( vecpaste( L142.ag_Fert_ConsScaler_MtN_R_Y[ R_Y ] ),
             vecpaste( L142.ag_Fert_Cons_MtN_R_Y.melt[ R_Y ] ) ) ]
L142.ag_Fert_ConsScaler_MtN_R_Y$scaler <- with( L142.ag_Fert_ConsScaler_MtN_R_Y, Fert_Cons_MtN / Fert_Cons_MtN_unscaled )
L142.ag_Fert_ConsScaler_MtN_R_Y$scaler[ is.na( L142.ag_Fert_ConsScaler_MtN_R_Y$scaler ) ] <- 0

#Calculate scaled consumption
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$scaler <- L142.ag_Fert_ConsScaler_MtN_R_Y$scaler[
         match( vecpaste( L142.ag_Fert_Cons_MtN_R_C_Y_GLU[ R_Y ] ),
                vecpaste( L142.ag_Fert_ConsScaler_MtN_R_Y[ R_Y ] ) ) ]
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$Fert_Cons_MtN <- with( L142.ag_Fert_Cons_MtN_R_C_Y_GLU, Fert_Cons_MtN_unscaled * scaler )
L142.ag_Fert_Cons_MtN_R_C_Y_GLU$Fert_IO <- with( L142.ag_Fert_Cons_MtN_R_C_Y_GLU, Fert_IO_unscaled * scaler )

#Cast the IO coefs into a new data frame to be written out
L142.ag_Fert_IO_R_C_Y_GLU <- dcast( L142.ag_Fert_Cons_MtN_R_C_Y_GLU, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "Fert_IO" )

#Check to make sure that the fertilizer inputs do not blink in and out (if present in any year, need to be present in all years)
L142.Fert_IO_check <- L142.ag_Fert_IO_R_C_Y_GLU[ rowSums( L142.ag_Fert_IO_R_C_Y_GLU[ X_AGLU_historical_years ] ) != 0, ]
if( any( L142.Fert_IO_check[ X_AGLU_historical_years ] == 0 ) ){
	stop( "Fertilizer input-output coefficients need to be specified in all historical years")
}

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L142.ag_Fert_Prod_MtN_ctry_Y <- c( "Fertilizer production by country / year","Unit = MtN" )
comments.L142.ag_Fert_NetExp_MtN_R_Y <- c( "Fertilizer net exports by GCAM region / year","Unit = MtN" )
comments.L142.ag_Fert_IO_R_C_Y_GLU <- c( "Fertilizer input-output coefficients by GCAM region / crop / year / GLU","Unitless IO" )

#write tables as CSV files
writedata( L142.ag_Fert_Prod_MtN_ctry_Y, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_Prod_MtN_ctry_Y", comments=comments.L142.ag_Fert_Prod_MtN_ctry_Y )
writedata( L142.ag_Fert_NetExp_MtN_R_Y, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_NetExp_MtN_R_Y", comments=comments.L142.ag_Fert_NetExp_MtN_R_Y )
writedata( L142.ag_Fert_IO_R_C_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_IO_R_C_Y_GLU", comments=comments.L142.ag_Fert_IO_R_C_Y_GLU )

# Every script should finish with this line
logstop()
