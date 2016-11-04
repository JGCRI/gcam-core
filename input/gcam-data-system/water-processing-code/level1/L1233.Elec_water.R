# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L1233.Elec_water.R" )
printlog( "Water consumption by GCAM region, fuel, technology, and cooling system type" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
calibated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- readdata( "ENERGY_LEVEL1_DATA", "L101.en_bal_EJ_ctry_Si_Fi_Yh_full" )
L1231.in_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.in_EJ_R_elec_F_tech_Yh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )
A23.CoolingSystemShares_RG3 <- readdata( "WATER_ASSUMPTIONS", "A23.CoolingSystemShares_RG3" )
elec_tech_water_map <- readdata( "WATER_MAPPINGS", "elec_tech_water_map" )
Macknick_elec_water_m3MWh <- readdata( "WATER_LEVEL0_DATA", "Macknick_elec_water_m3MWh" )

# -----------------------------------------------------------------------------

printlog( "Downscaling assumed water cooling system shares from 14 GCAM regions to 201 countries" )
printlog( "First, re-name intermediate fuels and re-aggregate" )
L1233.out_EJ_ctry_elec_Fi_Yh_full <- subset( L101.en_bal_EJ_ctry_Si_Fi_Yh_full, sector == "out_electricity generation" )
L1233.out_EJ_ctry_elec_Fi_Yh_full$fuel <- enduse_fuel_aggregation$electricity[ match( L1233.out_EJ_ctry_elec_Fi_Yh_full$fuel, enduse_fuel_aggregation$fuel ) ]
L1233.out_EJ_ctry_elec_F_Yh <- aggregate( L1233.out_EJ_ctry_elec_Fi_Yh_full[ X_historical_years ],
      by = L1233.out_EJ_ctry_elec_Fi_Yh_full[ c( "iso", S_F ) ], sum )
L1233.out_EJ_ctry_elec_F_Yh$sector <- sub( "out_", "", L1233.out_EJ_ctry_elec_F_Yh$sector )

printlog( "Next, write out the 14-region level information on cooling system shares to all countries" )
#Only the technologies that exist in the calibration dataset are necessary for this purpose. The full list of all calibrated technologies
# is available in the calibrated_techs mapping table
elec_tech_water_map_cal <- subset( elec_tech_water_map, paste( sector, fuel, technology ) %in% paste(
      calibated_techs$sector, calibated_techs$fuel, calibated_techs$technology ) )
L1233.weights_ctry_elec_F_Yh_cool <- repeat_and_add_vector(
      elec_tech_water_map_cal[ c( S_F_tech_cool, "plant_type" ) ],
      "iso", unique( L1233.out_EJ_ctry_elec_F_Yh$iso ) )

#Match the region_GCAM3 into this data frame, in order to be able to then match in the region-specific cooling system shares
L1233.weights_ctry_elec_F_Yh_cool$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L1233.weights_ctry_elec_F_Yh_cool$iso, iso_GCAM_regID$iso ) ]
A23.CoolingSystemShares_RG3 <- gcam_interp( A23.CoolingSystemShares_RG3, historical_years )
L1233.weights_ctry_elec_F_Yh_cool[ X_historical_years ] <- A23.CoolingSystemShares_RG3[ 
      match( vecpaste( L1233.weights_ctry_elec_F_Yh_cool[ c( "region_GCAM3", "plant_type", "cooling_system", "water_type" ) ] ),
             vecpaste( A23.CoolingSystemShares_RG3[ c( "region_GCAM3", "plant_type", "cooling_system", "water_type" ) ] ) ),
      X_historical_years ]

# Technologies with no cooling are not indicated in the cooling system shares assumptions table, so go ahead and re-set their cooling system shares to 1
L1233.weights_ctry_elec_F_Yh_cool[ L1233.weights_ctry_elec_F_Yh_cool$plant_type == "no cooling", X_historical_years ] <- 1

printlog( "Multiply shares by country-level generation to calculate electricity output by generation technology and cooling technology" )
L1233.weights_ctry_elec_F_Yh_cool[ X_historical_years ] <- L1233.weights_ctry_elec_F_Yh_cool[ X_historical_years ] *
      L1233.out_EJ_ctry_elec_F_Yh[
        match( vecpaste( L1233.weights_ctry_elec_F_Yh_cool[ c( "iso", S_F ) ] ),
               vecpaste( L1233.out_EJ_ctry_elec_F_Yh[ c( "iso", S_F ) ] ) ),
        X_historical_years ]
#The country-level electricity table did not have all combinations of country x fuel written out, so missing values are generated above
# These are replaced with zero
L1233.weights_ctry_elec_F_Yh_cool[ is.na( L1233.weights_ctry_elec_F_Yh_cool ) ] <- 0

printlog( "Aggregating country-level data to new GCAM regions in order to compute shares" )
# Note that we would not want to aggregate this to region and write it out as output, because we have not matched in technology-level output.
# Gas single-cycle and combined-cycle information is incorrect in this table, but it's OK because this table is only for computing
# weighted shares by GCAM region. To do that, we just need to aggregate by GCAM regions, both with and without the cooling technologies.
L1233.weights_ctry_elec_F_Yh_cool[[R]] <- iso_GCAM_regID[[R]][ match( L1233.weights_ctry_elec_F_Yh_cool$iso, iso_GCAM_regID$iso ) ]
L1233.weights_R_elec_F_Yh_cool <- aggregate( L1233.weights_ctry_elec_F_Yh_cool[ X_historical_years ],
      by= L1233.weights_ctry_elec_F_Yh_cool[ c( R, S_F_tech_cool ) ], sum )
L1233.weights_R_elec_F_Yh <- aggregate( L1233.weights_ctry_elec_F_Yh_cool[ X_historical_years ],
      by= L1233.weights_ctry_elec_F_Yh_cool[ c( R, S_F_tech ) ], sum )

printlog( "Matching region-level shares of cooling technology within generation technology" )
L1233.shares_R_elec_F_tech_Yh_cool <- repeat_and_add_vector(
      elec_tech_water_map_cal[ c( S_F_tech_cool, "plant_type" ) ],
      R, unique( iso_GCAM_regID[[R]] ) )
L1233.shares_R_elec_F_tech_Yh_cool <- L1233.shares_R_elec_F_tech_Yh_cool[ c( R, S_F_tech_cool, "plant_type" ) ]
L1233.shares_R_elec_F_tech_Yh_cool[ X_historical_years ] <-
      L1233.weights_R_elec_F_Yh_cool[
        match( vecpaste( L1233.shares_R_elec_F_tech_Yh_cool[ c( R, S_F_tech_cool ) ] ),
               vecpaste( L1233.weights_R_elec_F_Yh_cool[ c( R, S_F_tech_cool ) ] ) ),
        X_historical_years ] /
      L1233.weights_R_elec_F_Yh[
        match( vecpaste( L1233.shares_R_elec_F_tech_Yh_cool[ c( R, S_F_tech ) ] ),
               vecpaste( L1233.weights_R_elec_F_Yh[ c( R, S_F_tech ) ] ) ),
        X_historical_years ]

#At this point, NaN's appear wherever there was zero output for a region and power generation technology. These should be able to be re-set
# to any number, as the output should simply be zero. This isn't the case however, as a number of countries have fuel inputs but no electricity
# output in the IEA balances; rather than re-map this energy input elsewhere, we use a default efficiency to estimate the actual electricity output.
# Rather than replicate those steps here, we'll just pass shares backwards (from more recent years), as this problem is confined to the 1970's and 1980's.
for( i in rev( 1:( length( X_historical_years ) - 1 ) ) ){
	L1233.shares_R_elec_F_tech_Yh_cool[X_historical_years[i]] <- ifelse(
	   is.na( L1233.shares_R_elec_F_tech_Yh_cool[[X_historical_years[i]]] ),
	   L1233.shares_R_elec_F_tech_Yh_cool[[X_historical_years[i+1]]],
	   L1233.shares_R_elec_F_tech_Yh_cool[[X_historical_years[i]]] )
}

#For ones with NaN's in all periods, set to -1 so that we can check later to make sure that we didn't have a positive value where we expected zero
L1233.shares_R_elec_F_tech_Yh_cool[ is.na( L1233.shares_R_elec_F_tech_Yh_cool ) ] <- -1

printlog( "Multiplying shares by technology-level output to calculate the new calibration dataset" )
L1233.out_EJ_R_elec_F_tech_Yh_cool <- L1233.shares_R_elec_F_tech_Yh_cool
L1233.out_EJ_R_elec_F_tech_Yh_cool[ X_historical_years ] <- L1233.shares_R_elec_F_tech_Yh_cool[ X_historical_years ] *
      L1231.out_EJ_R_elec_F_tech_Yh[
        match( vecpaste( L1233.out_EJ_R_elec_F_tech_Yh_cool[ c( R_S_F_tech ) ] ),
               vecpaste( L1231.out_EJ_R_elec_F_tech_Yh[ c( R_S_F_tech ) ] ) ),
        X_historical_years ]
if( any( L1233.out_EJ_R_elec_F_tech_Yh_cool < 0 ) ){
	printlog( "Electricity generation by technology not assigned a cooling system share" )
	stop()
}

printlog( "Using cooling-within-generation technology shares to partition fuel inputs to electricity generation" )
printlog( "NOTE: this method assumes the same energy efficiencies for all cooling system types" )
L1233.in_EJ_R_elec_F_tech_Yh_cool <- subset( L1233.shares_R_elec_F_tech_Yh_cool,
      paste( fuel, technology ) %in%
      paste( L1231.in_EJ_R_elec_F_tech_Yh$fuel, L1231.in_EJ_R_elec_F_tech_Yh$technology ) )
L1233.in_EJ_R_elec_F_tech_Yh_cool[ X_historical_years ] <- L1233.in_EJ_R_elec_F_tech_Yh_cool[ X_historical_years ] *
      L1231.in_EJ_R_elec_F_tech_Yh[
        match( vecpaste( L1233.in_EJ_R_elec_F_tech_Yh_cool[ c( R_S_F_tech ) ] ),
               vecpaste( L1231.in_EJ_R_elec_F_tech_Yh[ c( R_S_F_tech ) ] ) ),
        X_historical_years ]

printlog( "Multiplying electricity generation by withdrawal and consumption coefficients, and aggregating by region")
#Withdrawals
L1233.wdraw_km3_R_elec_F_tech_Yh_cool <- L1233.out_EJ_R_elec_F_tech_Yh_cool
L1233.wdraw_km3_R_elec_F_tech_Yh_cool[ X_historical_years ] <- L1233.out_EJ_R_elec_F_tech_Yh_cool[ X_historical_years ] *
      Macknick_elec_water_m3MWh$water_withdrawals[
        match( vecpaste( L1233.wdraw_km3_R_elec_F_tech_Yh_cool[ S_F_tech_cool ] ),
               vecpaste( Macknick_elec_water_m3MWh[ S_F_tech_cool ] ) ) ] /
        conv_MWh_GJ
L1233.wdraw_km3_R_elec <- aggregate( L1233.wdraw_km3_R_elec_F_tech_Yh_cool[ X_historical_years ],
      by=L1233.wdraw_km3_R_elec_F_tech_Yh_cool[ c( R_S, "water_type" ) ], sum )
L1233.wdraw_km3_R_elec <- subset( L1233.wdraw_km3_R_elec, water_type != "none" )

#Consumption
L1233.wcons_km3_R_elec_F_tech_Yh_cool <- L1233.out_EJ_R_elec_F_tech_Yh_cool
L1233.wcons_km3_R_elec_F_tech_Yh_cool[ X_historical_years ] <- L1233.out_EJ_R_elec_F_tech_Yh_cool[ X_historical_years ] *
      Macknick_elec_water_m3MWh$water_consumption[
        match( vecpaste( L1233.wcons_km3_R_elec_F_tech_Yh_cool[ S_F_tech_cool ] ),
               vecpaste( Macknick_elec_water_m3MWh[ S_F_tech_cool ] ) ) ] /
        conv_MWh_GJ
L1233.wcons_km3_R_elec <- aggregate( L1233.wcons_km3_R_elec_F_tech_Yh_cool[ X_historical_years ],
      by=L1233.wcons_km3_R_elec_F_tech_Yh_cool[ c( R_S, "water_type" ) ], sum )
L1233.wcons_km3_R_elec <- subset( L1233.wcons_km3_R_elec, water_type != "none" )

printlog( "Calculating region-level future share-weight paths, based on GCAM 3.0 14-region assumptions" )
printlog( "Using representative countries rather than weighted averages; using the ctry with the most elec as the representative" )
L1233.out_EJ_ctry_elec_Yfby <- aggregate( L1233.out_EJ_ctry_elec_Fi_Yh_full[ X_final_historical_year ],
      by = L1233.out_EJ_ctry_elec_Fi_Yh_full[ c( "iso", R ) ], sum )
L1233.R_iso_RG3 <- aggregate( L1233.out_EJ_ctry_elec_Yfby[ X_final_historical_year ],
      by = L1233.out_EJ_ctry_elec_Yfby[ R ], max )
L1233.R_iso_RG3$iso <- L1233.out_EJ_ctry_elec_Yfby$iso[
      match( L1233.R_iso_RG3[[X_final_historical_year]], L1233.out_EJ_ctry_elec_Yfby[[X_final_historical_year ]] ) ]
L1233.R_iso_RG3$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L1233.R_iso_RG3$iso, iso_GCAM_regID$iso ) ]

printlog( "Filling out table of all techs to match in shares" )
L1233.shrwt_R_elec_cool_Yf <- repeat_and_add_vector( elec_tech_water_map[ c( S_F_tech_cool, "plant_type" ) ], R, sort( unique( iso_GCAM_regID[[R]] ) ) )
L1233.shrwt_R_elec_cool_Yf$region_GCAM3 <- L1233.R_iso_RG3$region_GCAM3[
      match( L1233.shrwt_R_elec_cool_Yf[[R]], L1233.R_iso_RG3[[R]] ) ]

#Interpolate the future years in the assumptions table, and match in these shares
A23.CoolingSystemShares_RG3 <- gcam_interp( A23.CoolingSystemShares_RG3, future_years )

#Note that the CCS technologies are not indicated explicitly in the cooling system shares table, so dropping these from the "plant_type" variable
# parentheses and spaces seem to be treated as wildcards...not sure why
L1233.shrwt_R_elec_cool_Yf$plant_type <- sub( "\\ \\(CCS\\)", "", L1233.shrwt_R_elec_cool_Yf$plant_type )

# note - in case the final model calibration year is in the historical years, keep all of the historical years in this table even
# though it's only used for model future years
L1233.shrwt_R_elec_cool_Yf[ c( X_historical_years, X_future_years ) ] <- A23.CoolingSystemShares_RG3[
      match( vecpaste( L1233.shrwt_R_elec_cool_Yf[ c( "region_GCAM3", "plant_type", "cooling_system", "water_type" ) ] ),
             vecpaste( A23.CoolingSystemShares_RG3[ c( "region_GCAM3", "plant_type", "cooling_system", "water_type" ) ] ) ),
      c( X_historical_years, X_future_years ) ]
L1233.shrwt_R_elec_cool_Yf <- na.omit( L1233.shrwt_R_elec_cool_Yf[ c( R, S_F_tech_cool, X_historical_years, X_future_years ) ] )

# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table
comments.L1233.out_EJ_R_elec_F_tech_Yh_cool <- c( "Electricity output by region / fuel / technology / cooling system / water type","Unit = EJ" )
comments.L1233.in_EJ_R_elec_F_tech_Yh_cool <- c( "Fuel inputs to electricity generation by region / fuel / technology / cooling system / water type","Unit = EJ" )
comments.L1233.wdraw_km3_R_elec <- c( "Water withdrawals for electricity generation by region / water type","Unit = km3 (bm3)" )
comments.L1233.wcons_km3_R_elec <- c( "Water consumption for electricity generation by region / water type","Unit = km3 (bm3)" )
comments.L1233.shrwt_R_elec_cool_Yf <- c( "Future cooling system shareweights by region / electric sector / technology","Unitless" )

#write tables as CSV files
writedata( L1233.out_EJ_R_elec_F_tech_Yh_cool, domain="WATER_LEVEL1_DATA", fn="L1233.out_EJ_R_elec_F_tech_Yh_cool", comments=comments.L1233.out_EJ_R_elec_F_tech_Yh_cool )
writedata( L1233.in_EJ_R_elec_F_tech_Yh_cool, domain="WATER_LEVEL1_DATA", fn="L1233.in_EJ_R_elec_F_tech_Yh_cool", comments=comments.L1233.in_EJ_R_elec_F_tech_Yh_cool )
writedata( L1233.wdraw_km3_R_elec, domain="WATER_LEVEL1_DATA", fn="L1233.wdraw_km3_R_elec", comments=comments.L1233.wdraw_km3_R_elec )
writedata( L1233.wcons_km3_R_elec, domain="WATER_LEVEL1_DATA", fn="L1233.wcons_km3_R_elec", comments=comments.L1233.wcons_km3_R_elec )
writedata( L1233.shrwt_R_elec_cool_Yf, domain="WATER_LEVEL1_DATA", fn="L1233.shrwt_R_elec_cool_Yf", comments=comments.L1233.shrwt_R_elec_cool_Yf )

# Every script should finish with this line
logstop()