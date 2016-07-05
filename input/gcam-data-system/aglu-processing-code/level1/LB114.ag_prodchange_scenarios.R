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
logstart( "LB114.ag_prodchange_scenarios.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural productivity change for GCAM scenarios, 2005-2095" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
A_defaultYieldRate <- readdata( "AGLU_ASSUMPTIONS", "A_defaultYieldRate" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L112.ag_YieldRatio_R_C_Ysy_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L112.ag_YieldRatio_R_C_Ysy_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Translate the default yield rates into ratios to the base year
all_future_years <- max( historical_years ):max( future_years )
X_all_future_years <- paste0( "X", all_future_years )
L114.A_defaultYieldRatio <- gcam_interp( A_defaultYieldRate, all_future_years )
L114.A_defaultYieldRatio[ X_all_future_years ] <- 1 + L114.A_defaultYieldRatio[ X_all_future_years ]
L114.A_defaultYieldRatio <- L114.A_defaultYieldRatio[ c( C, X_all_future_years ) ]

#Loop through to calculate productivity change ratio from the base year
for (i in 3:ncol( L114.A_defaultYieldRatio[ X_all_future_years ] ) ){
	L114.A_defaultYieldRatio[ X_all_future_years ][i] <- L114.A_defaultYieldRatio[ X_all_future_years ][i] *
	     L114.A_defaultYieldRatio[ X_all_future_years ][i-1]
}

#Yields during specified yield years for crops, but not included in projections: use default rate
printlog( "Adding in unspecified crops to FAO projected crop yield improvement rates, if any" )
L114.ag_YieldRatio_Cnsy_Y <- L114.A_defaultYieldRatio[ L114.A_defaultYieldRatio[[C]] %!in% L112.ag_YieldRatio_R_C_Ysy_AEZ[[C]], ]
if( nrow( L114.ag_YieldRatio_Cnsy_Y ) != 0 ) {
	L114.ag_YieldRatio_Cnsy_Ysy.melt <- interpolate_and_melt( L114.ag_YieldRatio_Cnsy_Y, spec_ag_prod_years, value.name = "value" )
	L114.ag_YieldRatio_R_Cnsy_Ysy.melt <- repeat_and_add_vector( L114.ag_YieldRatio_Cnsy_Ysy.melt, R, sort( unique( iso_GCAM_regID[[R]] ) ) )
	L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ.melt <- repeat_and_add_vector( L114.ag_YieldRatio_R_Cnsy_Ysy.melt, AEZ, AEZs )

	#Drop the base year and cast by AEZs
	L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ.melt <- subset( L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ.melt, year %!in% historical_years )
	L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ <- dcast( L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ.melt, GCAM_region_ID + GCAM_commodity + year ~ AEZ)

	#Rbind this with the previous table and sort
	L114.ag_YieldRatio_R_C_Ysy_AEZ <- rbind( L112.ag_YieldRatio_R_C_Ysy_AEZ, L114.ag_YieldRatio_R_Cnsy_Ysy_AEZ )
	L114.ag_YieldRatio_R_C_Ysy_AEZ <- L114.ag_YieldRatio_R_C_Ysy_AEZ[ order( as.character( L114.ag_YieldRatio_R_C_Ysy_AEZ[[C]] ) ) , ]
}

if( nrow( L114.ag_YieldRatio_Cnsy_Y ) == 0 ) {
	L114.ag_YieldRatio_R_C_Ysy_AEZ <- L112.ag_YieldRatio_R_C_Ysy_AEZ
}

#YIELDS BEYOND THE SPECIFIED PERIODS
nonspec_years <- future_years[ !future_years %in% spec_ag_prod_years ]
X_nonspec_years <- paste0( "X", nonspec_years )
L114.A_defaultYieldRatio_nsy <- L114.A_defaultYieldRatio[ c( C, X_nonspec_years ) ]
#Re-index the yield ratios to the final specified productivity year
L114.A_defaultYieldRatio_nsy[ X_nonspec_years ] <- L114.A_defaultYieldRatio_nsy[ X_nonspec_years ] /
      L114.A_defaultYieldRatio[[X_spec_ag_prod_years[ length( X_spec_ag_prod_years ) ] ]]
L114.A_defaultYieldRatio_nsy.melt <- interpolate_and_melt( L114.A_defaultYieldRatio_nsy, nonspec_years )
L114.ag_YieldRatio_R_C_Ynsy_AEZ <- subset( L114.ag_YieldRatio_R_C_Ysy_AEZ, year == max( spec_ag_prod_years ) )
L114.ag_YieldRatio_R_C_Ynsy_AEZ <- repeat_and_add_vector( L114.ag_YieldRatio_R_C_Ynsy_AEZ, "year", nonspec_years )
L114.ag_YieldRatio_R_C_Ynsy_AEZ[ AEZs ] <- L114.ag_YieldRatio_R_C_Ynsy_AEZ[ AEZs ] * L114.A_defaultYieldRatio_nsy.melt$value[
      match( vecpaste( L114.ag_YieldRatio_R_C_Ynsy_AEZ[ C_Y ] ), vecpaste( L114.A_defaultYieldRatio_nsy.melt[ C_Y ] ) ) ]

#Combine (rbind) this with the table for the specified ag productivity years
L114.ag_YieldRatio_R_C_Y_AEZ_ref <- rbind( L114.ag_YieldRatio_R_C_Ysy_AEZ, L114.ag_YieldRatio_R_C_Ynsy_AEZ )

#BIOENERGY: REFERENCE SCENARIO
printlog( "Reference bioenergy scenario: using median improvement rates from main agricultural crops" )
#Use median values across all crops from core ag scenario
L114.bio_YieldRatio_R_AEZ_Y_ref <- aggregate( L114.ag_YieldRatio_R_C_Y_AEZ_ref[ AEZs ],
      by=as.list( L114.ag_YieldRatio_R_C_Y_AEZ_ref[ R_Y ] ), median )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L114.ag_YieldRatio_R_C_Y_AEZ_ref <- c( "Reference yield improvement rates by region / commodity / year/ AEZ","Unit: decimal improvement rate" )
comments.L114.bio_YieldRatio_R_AEZ_Y_ref <- c( "Reference bioenergy yield improvement rates by region / year/ AEZ","Unit: decimal improvement rate" )

#write tables as CSV files
writedata( L114.ag_YieldRatio_R_C_Y_AEZ_ref,domain="AGLU_LEVEL1_DATA", fn="L114.ag_YieldRatio_R_C_Y_AEZ_ref", comments=comments.L114.ag_YieldRatio_R_C_Y_AEZ_ref )
writedata( L114.bio_YieldRatio_R_AEZ_Y_ref,domain="AGLU_LEVEL1_DATA", fn="L114.bio_YieldRatio_R_AEZ_Y_ref", comments=comments.L114.bio_YieldRatio_R_AEZ_Y_ref )

# Every script should finish with this line
logstop()
