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
logstart( "LB162.ag_prodchange_R_C_Y_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural productivity change from FAO CROSIT model, 2005-2050" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A_defaultYieldRate <- readdata( "AGLU_ASSUMPTIONS", "A_defaultYieldRate" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.ag_irrHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrHA_ha_ctry_crop" )
L151.ag_rfdHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdHA_ha_ctry_crop" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU" )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. Initial preparation of CROSIT database - replace country and crop IDs with names, and add X to the year (for casting)
FAO_ag_CROSIT$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( FAO_ag_CROSIT$country_ID, AGLU_ctry$CROSIT_country_ID ) ]
FAO_ag_CROSIT$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( FAO_ag_CROSIT$crop_ID, FAO_ag_items_PRODSTAT$CROSIT_cropID ) ]
FAO_ag_CROSIT$Xyear <- paste( "X", FAO_ag_CROSIT$year, sep = "" )

#Intial data clean-up: some regions have 0 production with positive harvested area and a reported yield. Re-calculate production where yields are available.
FAO_ag_CROSIT$Prod_kt_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] <-
      FAO_ag_CROSIT$HA_kha_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] * 0.001

FAO_ag_CROSIT$Prod_kt_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] <-
      FAO_ag_CROSIT$HA_kha_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] * 0.001

#Where no yields are given, re-set harvested area to 0
FAO_ag_CROSIT$HA_kha_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] <- 0
FAO_ag_CROSIT$HA_kha_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] <- 0

#Cast by year
printlog( "NOTE: We are separating rainfed and irrigated production from the CROSIT data")
crs_ID <- c( "CROSIT_ctry", "CROSIT_crop" )
L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT, CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa_irrigated" )
L162.ag_rfdYield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT, CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa_rainfed" )

#Combine (rbind) irrigated and rainfed data in order to simplify the processing
L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y[[irr]] <- "IRR"
L162.ag_rfdYield_kgHa_Rcrs_Ccrs_Y[[irr]] <- "RFD"
L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr <- rbind( L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y, L162.ag_rfdYield_kgHa_Rcrs_Ccrs_Y )

#Interpolate to specified agricultural productivity years, and drop any years not specified
L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr <- gcam_interp( L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr, spec_ag_prod_years )

#Order the columns
L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr <- L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr[ c( crs_ID, irr, X_spec_ag_prod_years ) ]

#Calculate yield multipliers from the base year
printlog( "Calculating CROSIT multipliers from the base year to all specified years, for yield")
L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr <- data.frame(
  L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr[ c( crs_ID, irr ) ],
  L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr[ X_spec_ag_prod_years ] / L162.ag_Yield_kgHa_Rcrs_Ccrs_Y_irr[[ X_spec_ag_prod_years[ 1 ] ]] )

#Drop the NaN's (crops with zero base year production / harvested area)
L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr <- na.omit( L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr )

#Melt the multipliers and specify the year
L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.melt <- melt( L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr, id.vars = c( crs_ID, irr ), value.name = "Mult" )
L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.melt$year <- as.numeric( substr( L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.melt$variable, 2, 5 ) )

printlog( "NOTE: We apply the yield ratio to crop-specific changes in harvested area" )
printlog( "This removes bias from changes in composition of GCAM commodities in the FAO projections" )
#These yield multipliers are now ready to be matched into the GTAP/LDS-based table of country x crop x zone harvested area in the base year
# First, merge the separate rainfed and irrigated harvested area datasets to simplify processing
L151.ag_irrHA_ha_ctry_crop[[irr]] <- "IRR"
names( L151.ag_irrHA_ha_ctry_crop )[ names( L151.ag_irrHA_ha_ctry_crop ) == "irrHA" ] <- "HA"
L151.ag_rfdHA_ha_ctry_crop[[irr]] <- "RFD"
names( L151.ag_rfdHA_ha_ctry_crop )[ names( L151.ag_rfdHA_ha_ctry_crop ) == "rfdHA" ] <- "HA"
L162.ag_HA_ha_ctry_crop_irr <- rbind( L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop )

#Match in the CROSIT region and commodity names for aggregation
printlog( "Aggregating LDS harvested area by CROSIT region and crop" )
L162.ag_HA_ha_ctry_crop_irr$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L162.ag_HA_ha_ctry_crop_irr$iso, AGLU_ctry$iso ) ]
L162.ag_HA_ha_ctry_crop_irr$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L162.ag_HA_ha_ctry_crop_irr$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr <- aggregate( L162.ag_HA_ha_ctry_crop_irr[ "HA" ],
                                             by = L162.ag_HA_ha_ctry_crop_irr[ c( crs_ID, GLU, irr ) ], sum, na.rm = T )

#Drop all crop x commodity combinations not present in the CROSIT yield area multiplier data
L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr <- L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr[
      vecpaste( L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr[ c( crs_ID, irr ) ] ) %in%
      vecpaste( L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr[ c( crs_ID, irr ) ] ), ]

#Repeat by the number of years in the specified agricultural productivity set, and add a year vector
printlog( "Repeating GTAP GLU production / harvested area data by number of model timesteps" )
L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr <- repeat_and_add_vector( L162.ag_HA_ha_Rcrs_Ccrs_GLU_irr, Y, spec_ag_prod_years )

#Match in the production multipliers from the CROSIT yield data for the given scenario, CROSIT region, CROSIT commodity, and year
# In this method, compositional shifts of CROSIT commodities within GCAM commodities do not translate to modified yields
printlog( "Matching CROSIT yield multipliers into base-year production. Harvested area multipliers are 1 in all periods" )
L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr$Mult <- L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.melt$Mult[
      match( vecpaste( L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr[ c( crs_ID, Y, irr ) ] ),
             vecpaste( L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.melt[ c( crs_ID, Y, irr ) ] ) ) ]

#Multiply through to get the projected production (assuming constant land area)
printlog( "Calculating the adjusted production / harvested area in each time period, for each CROSIT country / crop / GLU" )
#NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
L162.ag_HA_ha_ctry_crop_Ysy_irr <- repeat_and_add_vector( na.omit( L162.ag_HA_ha_ctry_crop_irr ), Y, spec_ag_prod_years )

#Match in multipliers (from CROSIT database), GCAM regions, and GCAM commodities
L162.ag_HA_ha_ctry_crop_Ysy_irr$Mult <- L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr$Mult[
  match( vecpaste( L162.ag_HA_ha_ctry_crop_Ysy_irr[ c( crs_ID, Y ) ] ),
         vecpaste( L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr[ c( crs_ID, Y ) ] ) ) ]
L162.ag_HA_ha_ctry_crop_Ysy_irr <- na.omit( L162.ag_HA_ha_ctry_crop_Ysy_irr )
L162.ag_HA_ha_ctry_crop_Ysy_irr$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[
  match( L162.ag_HA_ha_ctry_crop_Ysy_irr$iso, iso_GCAM_regID$iso ) ]
L162.ag_HA_ha_ctry_crop_Ysy_irr$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[
  match( L162.ag_HA_ha_ctry_crop_Ysy_irr$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#Multiply base-year harvested area by the future productivity multipliers
L162.ag_HA_ha_ctry_crop_Ysy_irr[["Prod_mod"]] <- with( L162.ag_HA_ha_ctry_crop_Ysy_irr, HA * Mult )

#Aggregate by GCAM region and commodity
L162.ag_Prod_t_R_C_Ysy_GLU_irr <- aggregate( L162.ag_HA_ha_ctry_crop_Ysy_irr[ c( "HA", "Prod_mod" ) ],
                                             by = L162.ag_HA_ha_ctry_crop_Ysy_irr[ R_C_Y_GLU_irr ], sum )

#Calculate the yield ratio as future production divided by base-year production
L162.ag_Prod_t_R_C_Ysy_GLU_irr$YieldRatio <- with( L162.ag_Prod_t_R_C_Ysy_GLU_irr, Prod_mod / HA )
L162.ag_Prod_t_R_C_Ysy_GLU_irr <- na.omit( L162.ag_Prod_t_R_C_Ysy_GLU_irr )
L162.ag_YieldRatio_R_C_Ysy_GLU_irr <- L162.ag_Prod_t_R_C_Ysy_GLU_irr[ c( R_C_Y_GLU_irr, "YieldRatio" ) ]

# Write out the median improvement rate across all crops, to use as the default rate for biomass
printlog( "Reference bioenergy scenario: using median improvement rates from main agricultural crops" )
L162.bio_YieldRatio_R_GLU_Ysy_irr <- aggregate( L162.ag_YieldRatio_R_C_Ysy_GLU_irr[ "YieldRatio" ],
                                                by = L162.ag_YieldRatio_R_C_Ysy_GLU_irr[ c( R_Y_GLU, irr ) ], median )
L162.bio_YieldRatio_R_GLU_Ysy_irr[[C]] <- "biomass"

#Fill this out to all future years and all crops with base-year production, using the default ag prod change assumptions
printlog( "Translating from yield ratios to annual improvement rates" )
L162.agBio_YieldRatio_R_C_Ysy_GLU_irr.melt <- rbind( L162.ag_YieldRatio_R_C_Ysy_GLU_irr, L162.bio_YieldRatio_R_GLU_Ysy_irr )
L162.agBio_YieldRatio_R_C_Ysy_GLU_irr.melt$Xyear <- paste0( "X", L162.agBio_YieldRatio_R_C_Ysy_GLU_irr.melt$year )
L162.agBio_YieldRatio_R_C_Ysy_GLU_irr <- dcast( L162.agBio_YieldRatio_R_C_Ysy_GLU_irr.melt,
                                            GCAM_region_ID + GCAM_commodity + GLU + Irr_Rfd ~ Xyear, value.var = "YieldRatio" )
L162.agBio_YieldRate_R_C_Ysy_GLU_irr <- L162.agBio_YieldRatio_R_C_Ysy_GLU_irr
for( i in 2:length( X_spec_ag_prod_years ) ){
  timestep <- spec_ag_prod_years[i] - spec_ag_prod_years[i-1]
  L162.agBio_YieldRate_R_C_Ysy_GLU_irr[[ X_spec_ag_prod_years[i] ]] <- 
    ( L162.agBio_YieldRatio_R_C_Ysy_GLU_irr[[ X_spec_ag_prod_years[i] ]] /
        L162.agBio_YieldRatio_R_C_Ysy_GLU_irr[[ X_spec_ag_prod_years[i-1] ]] ) ^ ( 1 / timestep ) - 1
}

#Match these annual improvement rates into a table of existing crop yields
# First, make a table of default yield improvement rates
L162.defaultYieldRate <- gcam_interp( A_defaultYieldRate, c( final_historical_year, future_years ), rule = 2 )
L161.ag_irrProd_Mt_R_C_Y_GLU[[irr]] <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_GLU[[irr]] <- "RFD"
L162.ag_Prod_Mt_R_C_Y_GLU_irr <- rbind( L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU )
L162.ag_YieldRate_R_C_Y_GLU_irr <- L162.ag_Prod_Mt_R_C_Y_GLU_irr[ R_C_GLU_irr ]
L162.ag_YieldRate_R_C_Y_GLU_irr[ X_spec_ag_prod_years ] <- L162.agBio_YieldRate_R_C_Ysy_GLU_irr[
  match( vecpaste( L162.ag_YieldRate_R_C_Y_GLU_irr[ R_C_GLU_irr ] ),
         vecpaste( L162.agBio_YieldRate_R_C_Ysy_GLU_irr[ R_C_GLU_irr ] ) ),
  X_spec_ag_prod_years ]
L162.ag_YieldRate_R_C_Y_GLU_irr[ !complete.cases(L162.ag_YieldRate_R_C_Y_GLU_irr), X_spec_ag_prod_years ] <-
  L162.defaultYieldRate[
    match( L162.ag_YieldRate_R_C_Y_GLU_irr[[C]][ !complete.cases(L162.ag_YieldRate_R_C_Y_GLU_irr) ],
           L162.defaultYieldRate[[C]] ),
    X_spec_ag_prod_years ]

#Fill this out to all future years
X_non_spec_years <- X_future_years[ X_future_years %!in% X_spec_ag_prod_years ]
L162.ag_YieldRate_R_C_Y_GLU_irr[ X_non_spec_years ] <- L162.defaultYieldRate[
  match( L162.ag_YieldRate_R_C_Y_GLU_irr[[C]],
         L162.defaultYieldRate[[C]] ),
  X_non_spec_years ]

printlog( "Writing out biomass yields" )
L162.bio_YieldRate_R_Y_GLU_irr <- unique( L162.ag_Prod_Mt_R_C_Y_GLU_irr[ R_GLU_irr ] )
L162.bio_YieldRate_R_Y_GLU_irr[[C]] <- "biomass"
L162.bio_YieldRate_R_Y_GLU_irr[ X_spec_ag_prod_years ] <- L162.agBio_YieldRate_R_C_Ysy_GLU_irr[
  match( vecpaste( L162.bio_YieldRate_R_Y_GLU_irr[ R_C_GLU_irr ] ),
         vecpaste( L162.agBio_YieldRate_R_C_Ysy_GLU_irr[ R_C_GLU_irr ] ) ),
  X_spec_ag_prod_years ]
L162.bio_YieldRate_R_Y_GLU_irr[ !complete.cases(L162.bio_YieldRate_R_Y_GLU_irr), X_spec_ag_prod_years ] <-
  L162.defaultYieldRate[
    match( L162.bio_YieldRate_R_Y_GLU_irr[[C]][ !complete.cases(L162.bio_YieldRate_R_Y_GLU_irr) ],
           L162.defaultYieldRate[[C]] ),
    X_spec_ag_prod_years ]
L162.bio_YieldRate_R_Y_GLU_irr[ X_non_spec_years ] <- L162.defaultYieldRate[
  match( L162.bio_YieldRate_R_Y_GLU_irr[[C]],
         L162.defaultYieldRate[[C]] ),
  X_non_spec_years ]


# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L162.ag_YieldRatio_R_C_Ysy_GLU_irr <- c( "Yield change ratios from final historical year by GCAM region / commodity (specified) / future year (specified) / irrigation","Unitless" )
comments.L162.ag_YieldRate_R_C_Y_GLU_irr <- c( "Yield change rates by GCAM region / commodity / future year / irrigation","Annual rate" )
comments.L162.bio_YieldRate_R_Y_GLU_irr <- c( "Biomass yield change rates by GCAM region / future year / irrigation","Annual rate" )

#write tables as CSV files
writedata( L162.ag_YieldRatio_R_C_Ysy_GLU_irr, domain="AGLU_LEVEL1_DATA",fn="L162.ag_YieldRatio_R_C_Ysy_GLU_irr", comments=comments.L162.ag_YieldRatio_R_C_Ysy_GLU_irr )
writedata( L162.ag_YieldRate_R_C_Y_GLU_irr, domain="AGLU_LEVEL1_DATA",fn="L162.ag_YieldRate_R_C_Y_GLU_irr", comments=comments.L162.ag_YieldRate_R_C_Y_GLU_irr )
writedata( L162.bio_YieldRate_R_Y_GLU_irr, domain="AGLU_LEVEL1_DATA",fn="L162.bio_YieldRate_R_Y_GLU_irr", comments=comments.L162.bio_YieldRate_R_Y_GLU_irr )

# Every script should finish with this line
logstop()
