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
logstart( "LB112.ag_prodchange_R_C_Y_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural productivity change from FAO CROSIT model, 2005-2050" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A_defaultYieldRate <- readdata( "AGLU_ASSUMPTIONS", "A_defaultYieldRate" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. Initial preparation of CROSIT database - replace country and crop IDs with names, and add X to the year (for casting)
FAO_ag_CROSIT$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( FAO_ag_CROSIT$country_ID, AGLU_ctry$CROSIT_country_ID ) ]
FAO_ag_CROSIT$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( FAO_ag_CROSIT$crop_ID, FAO_ag_items_PRODSTAT$CROSIT_cropID ) ]
FAO_ag_CROSIT$Xyear <- paste( "X", FAO_ag_CROSIT$year, sep = "" )

#Intial data clean-up: some regions have 0 production with positive harvested area and a reported yield. Re-calculate production where yields are available.
FAO_ag_CROSIT$Prod_kt[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] <-
      FAO_ag_CROSIT$HA_kha[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] * 0.001
#Where no yields are given, re-set harvested area to 0
FAO_ag_CROSIT$HA_kha[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] <- 0

#Eritrea has non-sensical data in the base year for wheat, and >60x yield growth to 2050. Change the yield and production in the base year
FAO_ag_CROSIT$Yield_kgHa[ FAO_ag_CROSIT$CROSIT_ctry == "ERIT" & FAO_ag_CROSIT$CROSIT_crop == "WHEA" & FAO_ag_CROSIT$year == 2005 ] <- 1400
FAO_ag_CROSIT$Prod_kt[ FAO_ag_CROSIT$CROSIT_ctry == "ERIT" & FAO_ag_CROSIT$CROSIT_crop == "WHEA" & FAO_ag_CROSIT$year == 2005 ] <- 16 * 1.4

#Cast by year
printlog( "NOTE: Only using total production and harvested area from CROSIT (not separating rainfed and irrigated)")
crs_ID <- c( "CROSIT_ctry", "CROSIT_crop" )
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT[ c( crs_ID, "Xyear", "Yield_kgHa") ], CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa" )

#Interpolate to specified agricultural productivity years, and drop any years not specified
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- gcam_interp( L112.ag_Yield_kgHa_Rcrs_Ccrs_Y, spec_ag_prod_years )

#Order the columns
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ c( crs_ID, X_spec_ag_prod_years ) ]

#Calculate yield multipliers from the base year
printlog( "Calculating CROSIT multipliers from the base year to all specified years, for yield")
L112.ag_Yieldmult_Rcrs_Ccrs_Y <- data.frame(
      L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ crs_ID ],
      L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ X_spec_ag_prod_years ] / L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[[ X_spec_ag_prod_years[ 1 ] ]] )

#Drop the NaN's (crops with zero base year production / harvested area)
L112.ag_Yieldmult_Rcrs_Ccrs_Y <- na.omit( L112.ag_Yieldmult_Rcrs_Ccrs_Y )

#Melt the multipliers and specify the year
L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt <- melt( L112.ag_Yieldmult_Rcrs_Ccrs_Y, id.vars = crs_ID )
L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$year <- as.numeric( substr( L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$variable, 2, 5 ) )

printlog( "NOTE: We apply the yield ratio to crop-specific changes in harvested area" )
printlog( "This removes bias from changes in composition of GCAM commodities in the FAO projections" )
#These yield multipliers are now ready to be matched into the GTAP/LDS-based table of country x crop x zone harvested area in the base year
#First, match in the CROSIT region and commodity names for aggregation
printlog( "Aggregating LDS harvested area by CROSIT region and crop" )
L112.LDS_ag_HA_ha <- L100.LDS_ag_HA_ha
L112.LDS_ag_HA_ha$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L112.LDS_ag_HA_ha$iso, AGLU_ctry$iso ) ]
L112.LDS_ag_HA_ha$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L112.LDS_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L112.LDS_ag_HA_ha <- na.omit( L112.LDS_ag_HA_ha )

#Aggregate by CROSIT region and commodity
L112.ag_HA_ha_Rcrs_Ccrs_GLU <- aggregate( L112.LDS_ag_HA_ha["value"], by = L112.LDS_ag_HA_ha[ c( crs_ID, GLU) ], sum )

#Drop all crop x commodity combinations not present in the CROSIT yield multiplier data
L112.ag_HA_ha_Rcrs_Ccrs_GLU <- L112.ag_HA_ha_Rcrs_Ccrs_GLU[
      vecpaste( L112.ag_HA_ha_Rcrs_Ccrs_GLU[ crs_ID ] ) %in%
      vecpaste( L112.ag_Yieldmult_Rcrs_Ccrs_Y[ crs_ID ] ), ]

#Repeat by the number of years in the specified agricultural productivity set, and add a year vector
printlog( "Repeating GTAP GLU production / harvested area data by number of model timesteps" )
L112.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU <- repeat_and_add_vector( L112.ag_HA_ha_Rcrs_Ccrs_GLU, Y, spec_ag_prod_years )

#Match in the productivity multipliers
printlog( "Matching CROSIT yield multipliers into GTAP/LDS harvested area database" )
L112.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU$Mult <- L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$value[
      match( vecpaste( L112.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU[ c( crs_ID, Y ) ] ),
             vecpaste( L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt[ c( crs_ID, Y ) ] ) ) ]

#Multiply through to get the projected production (assuming constant land area)
printlog( "Calculating the adjusted production in each time period, for each CROSIT country / crop / GLU" )
#NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
L112.ag_HA_ha_ctry_crop_Ysy <- repeat_and_add_vector( L112.LDS_ag_HA_ha, Y, spec_ag_prod_years )

#Match in multipliers (from CROSIT database), GCAM regions, and GCAM commodities
L112.ag_HA_ha_ctry_crop_Ysy$Mult <- L112.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU$Mult[
      match( vecpaste( L112.ag_HA_ha_ctry_crop_Ysy[ c( crs_ID, Y ) ] ),
             vecpaste( L112.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU[ c( crs_ID, Y ) ] ) ) ]
L112.ag_HA_ha_ctry_crop_Ysy <- na.omit( L112.ag_HA_ha_ctry_crop_Ysy )
L112.ag_HA_ha_ctry_crop_Ysy$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L112.ag_HA_ha_ctry_crop_Ysy$iso, iso_GCAM_regID$iso ) ]
L112.ag_HA_ha_ctry_crop_Ysy$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L112.ag_HA_ha_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#Multiply base-year harvested area by the future productivity multipliers
L112.ag_HA_ha_ctry_crop_Ysy[["Prod_mod"]] <- with( L112.ag_HA_ha_ctry_crop_Ysy, value * Mult )

#Aggregate by GCAM region and commodity
printlog( "Aggregating by GCAM region / zone / commodity / year to calculate change in productivity from the base year" )
L112.ag_Prod_t_R_C_Ysy_GLU <- aggregate( L112.ag_HA_ha_ctry_crop_Ysy[ c( "value", "Prod_mod" ) ],
                                       by = L112.ag_HA_ha_ctry_crop_Ysy[ R_C_Y_GLU ], sum )

#Calculate the yield ratio as future production divided by base-year production
L112.ag_Prod_t_R_C_Ysy_GLU$YieldRatio <- L112.ag_Prod_t_R_C_Ysy_GLU$Prod_mod / L112.ag_Prod_t_R_C_Ysy_GLU$value

L112.ag_YieldRatio_R_C_Ysy_GLU <- L112.ag_Prod_t_R_C_Ysy_GLU[ c( R_C_Y_GLU, "YieldRatio" ) ]

# Write out the median improvement rate across all crops, to use as the default rate for biomass
printlog( "Reference bioenergy scenario: using median improvement rates from main agricultural crops" )
L112.bio_YieldRatio_R_GLU_Ysy <- aggregate( L112.ag_YieldRatio_R_C_Ysy_GLU[ "YieldRatio" ],
                                            by = L112.ag_YieldRatio_R_C_Ysy_GLU[ R_Y_GLU ], median )
L112.bio_YieldRatio_R_GLU_Ysy[[C]] <- "biomass"

#Fill this out to all future years and all crops with base-year production, using the default ag prod change assumptions
printlog( "Translating from yield ratios to annual improvement rates" )
L112.agBio_YieldRatio_R_C_Ysy_GLU.melt <- rbind( L112.ag_YieldRatio_R_C_Ysy_GLU, L112.bio_YieldRatio_R_GLU_Ysy )
L112.agBio_YieldRatio_R_C_Ysy_GLU.melt$Xyear <- paste0( "X", L112.agBio_YieldRatio_R_C_Ysy_GLU.melt$year )
L112.agBio_YieldRatio_R_C_Ysy_GLU <- dcast( L112.agBio_YieldRatio_R_C_Ysy_GLU.melt, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "YieldRatio" )
L112.agBio_YieldRate_R_C_Ysy_GLU <- L112.agBio_YieldRatio_R_C_Ysy_GLU
for( i in 2:length( X_spec_ag_prod_years ) ){
  timestep <- spec_ag_prod_years[i] - spec_ag_prod_years[i-1]
  L112.agBio_YieldRate_R_C_Ysy_GLU[[ X_spec_ag_prod_years[i] ]] <- 
    ( L112.agBio_YieldRatio_R_C_Ysy_GLU[[ X_spec_ag_prod_years[i] ]] /
        L112.agBio_YieldRatio_R_C_Ysy_GLU[[ X_spec_ag_prod_years[i-1] ]] ) ^ ( 1 / timestep ) - 1
}

#Match these annual improvement rates into a table of existing crop yields
# First, make a table of default yield improvement rates
L112.defaultYieldRate <- gcam_interp( A_defaultYieldRate, c( final_historical_year, future_years ), rule = 2 )
L112.ag_YieldRate_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ]
L112.ag_YieldRate_R_C_Y_GLU[ X_spec_ag_prod_years ] <- L112.agBio_YieldRate_R_C_Ysy_GLU[
  match( vecpaste( L112.ag_YieldRate_R_C_Y_GLU[ R_C_GLU ] ),
         vecpaste( L112.agBio_YieldRate_R_C_Ysy_GLU[ R_C_GLU ] ) ),
  X_spec_ag_prod_years ]
L112.ag_YieldRate_R_C_Y_GLU[ !complete.cases(L112.ag_YieldRate_R_C_Y_GLU), X_spec_ag_prod_years ] <-
  L112.defaultYieldRate[
    match( L112.ag_YieldRate_R_C_Y_GLU[[C]][ !complete.cases(L112.ag_YieldRate_R_C_Y_GLU) ],
           L112.defaultYieldRate[[C]] ),
    X_spec_ag_prod_years ]

#Fill this out to all future years
X_non_spec_years <- X_future_years[ X_future_years %!in% X_spec_ag_prod_years ]
L112.ag_YieldRate_R_C_Y_GLU[ X_non_spec_years ] <- L112.defaultYieldRate[
  match( L112.ag_YieldRate_R_C_Y_GLU[[C]],
         L112.defaultYieldRate[[C]] ),
  X_non_spec_years ]

printlog( "Writing out biomass yields" )
L112.bio_YieldRate_R_Y_GLU <- unique( L103.ag_Prod_Mt_R_C_Y_GLU[ R_GLU ] )
L112.bio_YieldRate_R_Y_GLU[[C]] <- "biomass"
L112.bio_YieldRate_R_Y_GLU[ X_spec_ag_prod_years ] <- L112.agBio_YieldRate_R_C_Ysy_GLU[
  match( vecpaste( L112.bio_YieldRate_R_Y_GLU[ R_C_GLU ] ),
         vecpaste( L112.agBio_YieldRate_R_C_Ysy_GLU[ R_C_GLU ] ) ),
  X_spec_ag_prod_years ]
L112.bio_YieldRate_R_Y_GLU[ !complete.cases(L112.bio_YieldRate_R_Y_GLU), X_spec_ag_prod_years ] <-
  L112.defaultYieldRate[
    match( L112.bio_YieldRate_R_Y_GLU[[C]][ !complete.cases(L112.bio_YieldRate_R_Y_GLU) ],
           L112.defaultYieldRate[[C]] ),
    X_spec_ag_prod_years ]
L112.bio_YieldRate_R_Y_GLU[ X_non_spec_years ] <- L112.defaultYieldRate[
  match( L112.bio_YieldRate_R_Y_GLU[[C]],
         L112.defaultYieldRate[[C]] ),
  X_non_spec_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L112.ag_YieldRatio_R_C_Ysy_GLU <- c( "Yield change ratios from final historical year by GCAM region / commodity (specified) / future year (specified)","Unitless" )
comments.L112.ag_YieldRate_R_C_Y_GLU <- c( "Yield change rates by GCAM region / commodity / future year","Annual rate" )
comments.L112.bio_YieldRate_R_Y_GLU <- c( "Biomass yield change rates by GCAM region / future year","Annual rate" )

#write tables as CSV files
writedata( L112.ag_YieldRatio_R_C_Ysy_GLU, domain="AGLU_LEVEL1_DATA",fn="L112.ag_YieldRatio_R_C_Ysy_GLU", comments=comments.L112.ag_YieldRatio_R_C_Ysy_GLU )
writedata( L112.ag_YieldRate_R_C_Y_GLU, domain="AGLU_LEVEL1_DATA",fn="L112.ag_YieldRate_R_C_Y_GLU", comments=comments.L112.ag_YieldRate_R_C_Y_GLU )
writedata( L112.bio_YieldRate_R_Y_GLU, domain="AGLU_LEVEL1_DATA",fn="L112.bio_YieldRate_R_Y_GLU", comments=comments.L112.bio_YieldRate_R_Y_GLU )

# Every script should finish with this line
logstop()
