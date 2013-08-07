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
logstart( "L142.ag_Fert_IO_R_C_Y_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Fertilizer input-output coefficients by region / crop / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
Nfert_By_AEZ <- readdata( "AGLU_GIS_DATA", "Nfert_By_AEZ" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )
L100.FAO_ag_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Prod_t" )
L100.FAO_Fert_Cons_tN <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_Fert_Cons_tN" )
L100.FAO_Fert_Prod_tN <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_Fert_Prod_tN" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata ( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
L141.ag_Fert_Cons_MtN_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L141.ag_Fert_Cons_MtN_ctry_crop" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Aggregating Ramankutty spatial data of Nfert use by GCAM region and AEZ" )
#Paste region and AEZ vectors into Ramankutty database, and drop inland water
Nfert_By_AEZ$iso <- GIS_ctry_AEZ$iso[ match( Nfert_By_AEZ$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
Nfert_By_AEZ$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( Nfert_By_AEZ$iso, iso_GCAM_regID$iso ) ]
Nfert_By_AEZ$AEZ <- GIS_ctry_AEZ$AEZ[ match( Nfert_By_AEZ$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
Nfert_By_AEZ <- na.omit( Nfert_By_AEZ )

#Aggregate Ramankutty database by GCAM region and AEZ
L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty.melt <- aggregate( Nfert_By_AEZ[ "Nfert.kg." ] * conv_kg_Mt,
      by=as.list( Nfert_By_AEZ[ c( R_AEZ ) ] ), sum )

#Cast so that AEZs are columns
L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty <- cast( L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty.melt, GCAM_region_ID ~ AEZ, value = "Nfert.kg." )
L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty[ is.na( L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty ) ] <- 0

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
      by=as.list( L142.ag_Fert_Cons_tN_ctry_Y[ R ] ), sum )
L142.ag_Fert_Prod_MtN_R_Y <- aggregate( L142.ag_Fert_Prod_tN_ctry_Y[ X_AGLU_historical_years ] * conv_t_Mt,
      by=as.list( L142.ag_Fert_Prod_tN_ctry_Y[ R ] ), sum )

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

printlog( "Downscaling fertilizer demands by country and crop to AEZ" )
printlog( "NOTE: Allocating fertilizer consumption to AEZs on the basis of production, not harvested area")
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ <- GTAP_ag_Prod_t
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ AEZs ] <- sweep( GTAP_ag_Prod_t[ AEZs ], 1, rowSums( GTAP_ag_Prod_t[ AEZs ] ), "/" )
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ AEZs ] <- L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ AEZs ] *
      L141.ag_Fert_Cons_MtN_ctry_crop$Fert_Cons_MtN[
         match( vecpaste( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ c( "ctry", "GTAP_crop" ) ] ),
                vecpaste( L141.ag_Fert_Cons_MtN_ctry_crop[ c( "ctry", "GTAP_crop" ) ] ) ) ]
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ is.na( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ ) ] <- 0

printlog( "Aggregating fertilizer demands by GCAM region, commodity, and AEZ" )
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[[R]] <- iso_GCAM_regID[[R]][ match( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ$ctry, iso_GCAM_regID$iso ) ]
L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[[C]] <- FAO_ag_items_PRODSTAT[[C]][
      match( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L142.ag_Fert_Cons_MtN_R_C_AEZ <- aggregate( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ AEZs ],
      by=as.list( L142.ag_Fert_Cons_MtN_ctry_crop_AEZ[ R_C ] ), sum )

printlog( "Calculating unscaled coefficients as unscaled fertilizer demands divided by production")
L142.ag_Prod_Mt_R_C_AEZ <- L104.ag_Prod_Mt_R_C_Y_AEZ[ c( R_C_AEZ, X_base_year_IFA ) ]
L142.ag_Fert_IO_R_C_AEZ.melt <- melt( L142.ag_Fert_Cons_MtN_R_C_AEZ, id.vars = R_C, variable_name = AEZ )
L142.ag_Fert_IO_R_C_AEZ.melt$value <- L142.ag_Fert_IO_R_C_AEZ.melt$value / L142.ag_Prod_Mt_R_C_AEZ[[ X_base_year_IFA ]]
L142.ag_Fert_IO_R_C_AEZ.melt$value[ L142.ag_Fert_IO_R_C_AEZ.melt$value == Inf ] <- 0
L142.ag_Fert_IO_R_C_AEZ.melt$value[ is.na( L142.ag_Fert_IO_R_C_AEZ.melt$value ) ] <- 0

#Melt to match into table of production
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ <- melt( L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars = R_C_AEZ, variable_name = "Xyear" )
names( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ )[ names( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ ) == "value" ] <- "Prod_Mt"
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$year <- substr( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Xyear, 2, 5 )
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_IO_unscaled <- L142.ag_Fert_IO_R_C_AEZ.melt$value[
      match( vecpaste( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ[ c( R_C_AEZ ) ] ),
             vecpaste( L142.ag_Fert_IO_R_C_AEZ.melt[ c( R_C_AEZ ) ] ) ) ]
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_Cons_MtN_unscaled <- L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Prod_Mt * L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_IO_unscaled

#Melt regional fertilizer consumption by region and year
L142.ag_Fert_Cons_MtN_R_Y.melt <- melt( L142.ag_Fert_Cons_MtN_R_Y, id.vars = R_C, variable_name = "Xyear" )
L142.ag_Fert_Cons_MtN_R_Y.melt$year <- substr( L142.ag_Fert_Cons_MtN_R_Y.melt$Xyear, 2, 5 )

#Compute region/year scalers so that consumption balances
L142.ag_Fert_ConsScaler_MtN_R_Y <- aggregate( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ[ "Fert_Cons_MtN_unscaled" ],
      by=as.list( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ[ R_Y ] ), sum )
L142.ag_Fert_ConsScaler_MtN_R_Y$Fert_Cons_MtN <- L142.ag_Fert_Cons_MtN_R_Y.melt$value[
      match( vecpaste( L142.ag_Fert_ConsScaler_MtN_R_Y[ R_Y ] ),
             vecpaste( L142.ag_Fert_Cons_MtN_R_Y.melt[ R_Y ] ) ) ]
L142.ag_Fert_ConsScaler_MtN_R_Y$scaler <- L142.ag_Fert_ConsScaler_MtN_R_Y$Fert_Cons_MtN / L142.ag_Fert_ConsScaler_MtN_R_Y$Fert_Cons_MtN_unscaled

#Calculate scaled consumption
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$scaler <- L142.ag_Fert_ConsScaler_MtN_R_Y$scaler[
         match( vecpaste( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ[ R_Y ] ),
                vecpaste( L142.ag_Fert_ConsScaler_MtN_R_Y[ R_Y ] ) ) ]
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_Cons_MtN <- L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_Cons_MtN_unscaled * L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$scaler
L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_IO <- L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$Fert_IO_unscaled * L142.ag_Fert_Cons_MtN_R_C_Y_AEZ$scaler

#Cast the IO coefs into a new data frame to be written out
L142.ag_Fert_IO_R_C_Y_AEZ <- cast( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ, GCAM_region_ID + GCAM_commodity + AEZ ~ Xyear, value = "Fert_IO" )

#Check to make sure that the fertilizer inputs do not blink in and out (if present in any year, need to be present in all years)
L142.Fert_IO_check <- L142.ag_Fert_IO_R_C_Y_AEZ[ rowSums( L142.ag_Fert_IO_R_C_Y_AEZ[ X_AGLU_historical_years ] ) != 0, ]
if( any( L142.Fert_IO_check[ X_AGLU_historical_years ] == 0 ) ){
	stop( "Fertilizer input-output coefficients need to be specified in all historical years")
}

#Calculate fertilizer consumption by GCAM region and AEZ, for comparison with Ramankutty
printlog( "Aggregating GCAM fertilizer consumption by region and AEZ, for comparison with Ramankutty" )
#The Ramankutty dataset uses fertilizer consumption data from 1994 to 2001. Using 1998 as the basis for the comparison
L142.ag_Fert_Cons_MtN_R_C_fby_AEZ <- subset( L142.ag_Fert_Cons_MtN_R_C_Y_AEZ, year == 1998 )
L142.ag_Fert_Cons_MtN_R_AEZ.melt <- aggregate( L142.ag_Fert_Cons_MtN_R_C_fby_AEZ[ "Fert_Cons_MtN" ],
      by=as.list( L142.ag_Fert_Cons_MtN_R_C_fby_AEZ[ R_AEZ ] ), sum )
L142.ag_Fert_Cons_MtN_R_AEZ <- cast( L142.ag_Fert_Cons_MtN_R_AEZ.melt, GCAM_region_ID ~ AEZ, value = "Fert_Cons_MtN" )
L142.ag_Fert_Cons_MtN_R_AEZ[ is.na( L142.ag_Fert_Cons_MtN_R_AEZ ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L142.ag_Fert_Prod_MtN_ctry_Y <- c( "Fertilizer production by country / year","Unit = MtN" )
comments.L142.ag_Fert_NetExp_MtN_R_Y <- c( "Fertilizer net exports by GCAM region / year","Unit = MtN" )
comments.L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty <- c( "Ramankutty N fertilizer consumption by GCAM region / AEZ (~1994-2001)","Unit = MtN" )
comments.L142.ag_Fert_Cons_MtN_R_AEZ <- c( "N fertilizer consumption by GCAM region / AEZ (1998)","Unit = MtN" )
comments.L142.ag_Fert_IO_R_C_Y_AEZ <- c( "Fertilizer input-output coefficients by GCAM region / crop / year / AEZ","Unitless IO" )

#write tables as CSV files
writedata( L142.ag_Fert_Prod_MtN_ctry_Y, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_Prod_MtN_ctry_Y", comments=comments.L142.ag_Fert_Prod_MtN_ctry_Y )
writedata( L142.ag_Fert_NetExp_MtN_R_Y, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_NetExp_MtN_R_Y", comments=comments.L142.ag_Fert_NetExp_MtN_R_Y )
writedata( L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty", comments=comments.L142.ag_Fert_Cons_MtN_R_AEZ_Rmkty )
writedata( L142.ag_Fert_Cons_MtN_R_AEZ, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_Cons_MtN_R_AEZ", comments=comments.L142.ag_Fert_Cons_MtN_R_AEZ )
writedata( L142.ag_Fert_IO_R_C_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L142.ag_Fert_IO_R_C_Y_AEZ", comments=comments.L142.ag_Fert_IO_R_C_Y_AEZ )

# Every script should finish with this line
logstop()
