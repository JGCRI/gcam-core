
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LA144.building_det_flsp.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Detailed building sector floorspace" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_bld_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A44.flsp_bm2_state_res <- readdata( "ENERGY_ASSUMPTIONS", "A44.flsp_bm2_state_res" )
A44.comm_flsp <- readdata( "ENERGY_ASSUMPTIONS", "A44.comm_flsp" )
A44.HouseholdSize <- readdata( "ENERGY_ASSUMPTIONS", "A44.HouseholdSize" )
flsp_mapping <- readdata( "ENERGY_MAPPINGS", "flsp_mapping" )
CEDB_ResFloorspace_chn <- readdata( "ENERGY_LEVEL0_DATA", "CEDB_ResFloorspace_chn" )
IEA_PCResFloorspace <- readdata( "ENERGY_LEVEL0_DATA", "IEA_PCResFloorspace" )
Odyssee_ResFloorspacePerHouse <- readdata( "ENERGY_LEVEL0_DATA", "Odyssee_ResFloorspacePerHouse" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )
L102.gdp_mil90usd_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_R_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
## FLOORSPACE CALCULATION
#China - divide by population and extrapolate to all historical years
China_flsp_years <- names( CEDB_ResFloorspace_chn )[ names( CEDB_ResFloorspace_chn ) %in% X_historical_years ]
L144.pcflsp_m2_chn_Yh <- data.frame(
      iso = "chn",
      CEDB_ResFloorspace_chn[ China_flsp_years ] * conv_bil_thous / 
      L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "chn", China_flsp_years ] )

#First, fill out the household size to all relevant years
Odyssee_flsp_years <- 1980:2009
X_Odyssee_flsp_years <- paste0( "X", Odyssee_flsp_years )
L144.HouseholdSize <- gcam_interp( A44.HouseholdSize, Odyssee_flsp_years, rule = 2 )    #use rule = 2 to fill out 

#Drop the Odyssee countries that we have estimates for in the IEA dataset (IEA assumed to be better as it presents per-capita flsp)
L144.Odyssee_phflsp_Yh <- subset( Odyssee_ResFloorspacePerHouse, !iso %in% IEA_PCResFloorspace$iso )
L144.Odyssee_pcflsp_Yh <- L144.Odyssee_phflsp_Yh
L144.Odyssee_pcflsp_Yh[ X_Odyssee_flsp_years ] <- L144.Odyssee_phflsp_Yh[ X_Odyssee_flsp_years ] /
      L144.HouseholdSize[rep( 1, times = nrow( L144.Odyssee_phflsp_Yh ) ), X_Odyssee_flsp_years ]

IEA_flsp_years <- 1980:2004
X_IEA_flsp_years <- paste0( "X", IEA_flsp_years )
L144.OECD_pcflsp_Yh <- rbind( L144.Odyssee_pcflsp_Yh[ c( "iso", X_IEA_flsp_years ) ], IEA_PCResFloorspace[ c( "iso", X_IEA_flsp_years ) ] )
L144.OECD_pcflsp_Yh <- L144.OECD_pcflsp_Yh[ order( L144.OECD_pcflsp_Yh$iso ), ]
#Drop any countries with all missing values
L144.OECD_pcflsp_Yh <- subset( L144.OECD_pcflsp_Yh, rowSums( L144.OECD_pcflsp_Yh[ X_IEA_flsp_years ], na.rm = T ) != 0 )

#Fill out missing values in specified countries
L144.OECD_pcflsp_Yh$X1980[ L144.OECD_pcflsp_Yh$iso == "usa" ] <- 49.5       #Derived from RECS; see RGCAM data system for documentation
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1991[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1992[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1995[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1996[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]

#Calculate average 1980-1990 growth rates for countries with 1980 data. Apply this to the 1990 data to return estimated 1980 floorspace
growthrate_1980_1990 <- sum( L144.OECD_pcflsp_Yh$X1990[ !is.na( L144.OECD_pcflsp_Yh$X1980 ) ] ) / sum( L144.OECD_pcflsp_Yh$X1980[ !is.na( L144.OECD_pcflsp_Yh$X1980 ) ] )
L144.OECD_pcflsp_Yh$X1980[ is.na( L144.OECD_pcflsp_Yh$X1980 ) ] <- L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1980 ) ] / growthrate_1980_1990

#Fill out australia and belgium 2004 data
L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "aus" ] <- L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "usa" ] / 
      L144.OECD_pcflsp_Yh$X1998[ L144.OECD_pcflsp_Yh$iso == "usa" ] * L144.OECD_pcflsp_Yh$X1998[ L144.OECD_pcflsp_Yh$iso == "aus" ]
L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "bel" ] <- L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "fra" ] / 
      L144.OECD_pcflsp_Yh$X2001[ L144.OECD_pcflsp_Yh$iso == "fra" ] * L144.OECD_pcflsp_Yh$X2001[ L144.OECD_pcflsp_Yh$iso == "bel" ]

#Interpolate and extrapolate the time series to all historical years
interp_years_early <- 1981:1989
L144.OECD_pcflsp_Yh <- gcam_interp( L144.OECD_pcflsp_Yh, interp_years_early )
interp_years_later <- 1991:2003
L144.OECD_pcflsp_Yh <- gcam_interp( L144.OECD_pcflsp_Yh, interp_years_later )

#Time series doesn't span entire "historical" range; need to extrapolate
#Just for now, use constant floorspace outside of available time series
L144.pcflsp_m2_chn_Yh <- gcam_interp( L144.pcflsp_m2_chn_Yh, historical_years, rule = 2 )
L144.OECD_pcflsp_Yh <- gcam_interp( L144.OECD_pcflsp_Yh, historical_years, rule = 2 )
L144.OECD_pcflsp_Yh <- L144.OECD_pcflsp_Yh[ c( "iso", X_historical_years ) ]

#Combine China floorspace with the OECD countries' floorspace
L144.OECD_pcflsp_Yh <- rbind( L144.OECD_pcflsp_Yh, L144.pcflsp_m2_chn_Yh )

#Replace the USA data with 50-state-derived data
A44.flsp_bm2_state_res$iso <- "usa"
L144.flsp_bm2_USA_res <- aggregate( A44.flsp_bm2_state_res[ names( A44.flsp_bm2_state_res ) %in% X_historical_years ],
      by=as.list( A44.flsp_bm2_state_res[ "iso"] ), sum )
USA_flsp_years <- names( L144.flsp_bm2_USA_res )[ names( L144.flsp_bm2_USA_res ) %in% X_historical_years ]
L144.pcflsp_m2_USA_res <- L144.flsp_bm2_USA_res
L144.pcflsp_m2_USA_res[ USA_flsp_years ] <- L144.flsp_bm2_USA_res[ USA_flsp_years ] * conv_bil_thous / L100.Pop_thous_ctry_Yh[
      match( L144.flsp_bm2_USA_res$iso, L100.Pop_thous_ctry_Yh$iso ),
      USA_flsp_years ]

#Extrapolate the US data to all years, and match into the table of OECD countries' floorspace
L144.pcflsp_m2_USA_res <- gcam_interp( L144.pcflsp_m2_USA_res, historical_years, rule = 2 )
L144.OECD_pcflsp_Yh[ L144.OECD_pcflsp_Yh$iso == "usa", X_historical_years ] <- L144.pcflsp_m2_USA_res[ X_historical_years ]

#Apply these estimates of per-capita floorspace to remaining countries in the world, using mapping assumptions
#First, build a table with all countries written out
L144.pcflsp_m2_ctry_Yh <- flsp_mapping
L144.pcflsp_m2_ctry_Yh$flsp_ctry[ is.na( L144.pcflsp_m2_ctry_Yh$flsp_ctry ) ] <- L144.pcflsp_m2_ctry_Yh$iso[ is.na( L144.pcflsp_m2_ctry_Yh$flsp_ctry ) ]
L144.pcflsp_m2_ctry_Yh[ X_historical_years ] <- L144.OECD_pcflsp_Yh[
      match( L144.pcflsp_m2_ctry_Yh$flsp_ctry, L144.OECD_pcflsp_Yh$iso ),
      X_historical_years ]
L144.pcflsp_m2_ctry_Yh <- na.omit( L144.pcflsp_m2_ctry_Yh )

#Need to multiply by population, match in the region names, and aggregate by (new) GCAM region
L144.flsp_bm2_ctry_res_Yh <- L144.pcflsp_m2_ctry_Yh
L144.flsp_bm2_ctry_res_Yh[ X_historical_years ] <- L144.flsp_bm2_ctry_res_Yh[ X_historical_years ] * conv_thous_bil *
  L100.Pop_thous_ctry_Yh[ match( L144.flsp_bm2_ctry_res_Yh$iso, L100.Pop_thous_ctry_Yh$iso ),
                          X_historical_years ]
L144.flsp_bm2_ctry_res_Yh[[R]] <- iso_GCAM_regID[[R]][match( L144.flsp_bm2_ctry_res_Yh$iso, iso_GCAM_regID$iso ) ]
L144.flsp_bm2_R_res_Yh <- aggregate( L144.flsp_bm2_ctry_res_Yh[ X_historical_years ],
                                     by=as.list( L144.flsp_bm2_ctry_res_Yh[ R ] ), sum, na.rm = T )

#Commercial Floorspace calculations
L144.comm_flsp <- gcam_interp( A44.comm_flsp, historical_years, rule=2 )
L100.Pop_thous_ctry_Yh [ c( "region_GCAM3", "GCAM_region_ID" ) ] <- iso_GCAM_regID[ 
  match( L100.Pop_thous_ctry_Yh$iso, iso_GCAM_regID$iso), c( "region_GCAM3", "GCAM_region_ID" ) ]
L144.flsp_bm2_ctry_comm_Yh <- L100.Pop_thous_ctry_Yh
L144.flsp_bm2_ctry_comm_Yh [ X_historical_years ] <- L144.flsp_bm2_ctry_comm_Yh [ X_historical_years ] * conv_thous_bil *
  L144.comm_flsp[ match( L144.flsp_bm2_ctry_comm_Yh$region_GCAM3, L144.comm_flsp$region_GCAM3 ), X_historical_years ] 
L144.flsp_bm2_R_comm_Yh <- aggregate( L144.flsp_bm2_ctry_comm_Yh [ X_historical_years ], 
  by = as.list( L144.flsp_bm2_ctry_comm_Yh[ R ] ), sum ) 

#Calculation of floorspace prices - using a simple formulation right now
L144.flspPrice_90USDm2_R_bld_Yh <- L144.flsp_bm2_R_res_Yh
L144.flspPrice_90USDm2_R_bld_Yh[ X_historical_years ] <-
      L102.gdp_mil90usd_GCAM3_R_Y[ X_historical_years ] * conv_mil_bil * bld_frac_of_income / 
      L144.flsp_bm2_R_res_Yh[ X_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.flsp_bm2_R_res_Yh <- c( "Residential floorspace by GCAM region / historical year","Unit = bm2" )
comments.L144.flsp_bm2_R_comm_Yh <- c( "Commercial floorspace by GCAM region / historical year","Unit = bm2" )
comments.L144.flspPrice_90USDm2_R_bld_Yh <- c( "Building floorspace prices by GCAM region / historical year","Unit = 1990$ / m2" )

#write tables as CSV files
writedata( L144.flsp_bm2_R_res_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.flsp_bm2_R_res_Yh", comments=comments.L144.flsp_bm2_R_res_Yh )
writedata( L144.flsp_bm2_R_comm_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.flsp_bm2_R_comm_Yh", comments=comments.L144.flsp_bm2_R_comm_Yh )
writedata( L144.flspPrice_90USDm2_R_bld_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.flspPrice_90USDm2_R_bld_Yh", comments=comments.L144.flspPrice_90USDm2_R_bld_Yh )

# Every script should finish with this line
logstop()
