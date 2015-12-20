# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "L202.Ccoef.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Carbon contents of fuels in the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_PrimaryFuelCCoef <- readdata( "EMISSIONS_ASSUMPTIONS", "A_PrimaryFuelCCoef")
fuel_to_Ccoef <- readdata( "EMISSIONS_MAPPINGS", "fuel_to_Ccoef")
L102.Ccoef_kgCGJ_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L102.Ccoef_kgCGJ_F_Yh" )
L102.Ccoef_kgCGJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L102.Ccoef_kgCGJ_R_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Carbon contents
printlog( "L202.C_Coefs: Carbon contents of fuels in all regions" )
L202.CarbonCoef <- write_to_all_regions( A_PrimaryFuelCCoef, names_PrimaryFuelCO2Coef, has.traded = T )
if( use_GCAM3_Ccoefs == 1 ) printlog( "Using exogenous C coefs from GCAM 3.0")
if( use_GCAM3_Ccoefs == 0 ) {
	L202.CarbonCoef$fuel <- fuel_to_Ccoef$fuel[ match( L202.CarbonCoef$PrimaryFuelCO2Coef.name, fuel_to_Ccoef$PrimaryFuelCO2Coef.name ) ]
	if( use_global_Ccoefs == 1 ){
		printlog( "Using global average emissions coefficients based on CDIAC inventory and IEA energy balances")
		L202.Ccoef_kgCGJ_F_Yh.melt <- interpolate_and_melt( L102.Ccoef_kgCGJ_F_Yh, CO2_historical_years )  
		L202.Ccoef_kgCGJ_F_fby.melt <- subset( L202.Ccoef_kgCGJ_F_Yh.melt, year == inventory_match_year )
		L202.CarbonCoef$PrimaryFuelCO2Coef[ !is.na( L202.CarbonCoef$fuel ) ] <- round(
		   L202.Ccoef_kgCGJ_F_fby.melt$value[
		      match( L202.CarbonCoef$fuel[ !is.na( L202.CarbonCoef$fuel ) ], L202.Ccoef_kgCGJ_F_fby.melt$fuel ) ],	
		digits_CO2coef)
		L202.CarbonCoef <- L202.CarbonCoef[ names_PrimaryFuelCO2Coef ]
	}
	if( use_global_Ccoefs == 0 ){
		printlog( "Using region-specific average emissions coefficients based on CDIAC inventory and IEA energy balances")
		L202.Ccoef_kgCGJ_R_F_Yh.melt <- interpolate_and_melt( L102.Ccoef_kgCGJ_R_F_Yh, CO2_historical_years )  
		L202.Ccoef_kgCGJ_R_F_fby.melt <- subset( L202.Ccoef_kgCGJ_R_F_Yh.melt, year == inventory_match_year )
		L202.Ccoef_kgCGJ_R_F_fby.melt <- add_region_name( L202.Ccoef_kgCGJ_R_F_fby.melt )
		L202.CarbonCoef <- add_region_name( L202.CarbonCoef )
		L202.CarbonCoef$PrimaryFuelCO2Coef[ !is.na( L202.CarbonCoef$fuel ) ] <- round(
		   L202.Ccoef_kgCGJ_R_F_fby.melt$value[
		      match( paste( L202.CarbonCoef$region[ !is.na( L202.CarbonCoef$fuel ) ], L202.CarbonCoef$fuel[ !is.na( L202.CarbonCoef$fuel ) ] ),
		             paste( L202.Ccoef_kgCGJ_R_F_fby.melt$region, L202.Ccoef_kgCGJ_R_F_fby.melt$fuel ) ) ],
		digits_CO2coef)
		L202.CarbonCoef <- L202.CarbonCoef[ names_PrimaryFuelCO2Coef ]
	}
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L202.CarbonCoef, "CarbonCoef", "ENERGY_LEVEL2_DATA", "L202.CarbonCoef", "ENERGY_XML_BATCH", "batch_Ccoef.xml" ) 

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_Ccoef.xml", "ENERGY_XML_FINAL", "Ccoef.xml", "", xml_tag="outFile" )

logstop()
