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
logstart( "LA143.HDDCDD.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Heating and cooling degree days by GCAM region / year / scenario" )

# -----------------------------------------------------------------------------
# 1. Read files
# 1a. reading in standard files whose names are known
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry <- readdata( "ENERGY_MAPPINGS", "GIS_ctry" )
L101.Pop_thous_GCAM3_ctry_Y <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_ctry_Y" )

# 1b. reading HDDCDD files in a loop as we do not know their names or how many there are
GISfilepath <- readdomainpathmap()["ENERGY_GIS_DATA"][[1]]
GISfiles <- list.files( GISfilepath )
GISfiles <- sub( ".csv", "", GISfiles )
GISfiles.list <- list()
for( i in GISfiles){
  index <- which( GISfiles == i )
  GISfiles.list[[index]] <- readdata( "ENERGY_GIS_DATA", i )
}
names(GISfiles.list) <- GISfiles
base_year_HDDCDD_name <- GISfiles[ grepl( "_2010", GISfiles )]
base_year_HDDCDD_data <- GISfiles.list[[ base_year_HDDCDD_name ]]
GISfiles.list <- GISfiles.list[ names( GISfiles.list ) != base_year_HDDCDD_name ]
HDDCDD_data <- do.call( rbind, GISfiles.list )

# -----------------------------------------------------------------------------
#Currently the HDDCDD data stops at 2099. If this is the case, add 2100
if( !"X2100" %in% names( HDDCDD_data ) ){
	HDDCDD_data$X2100 <- HDDCDD_data$X2099
}

#Now we can name the variable (HDD/CDD), climate model, and SRES scenario using substrings
# Assuming that the sting "DD_" in the file names stands for degree days only
HDDCDD_data$variable <- substr( row.names( HDDCDD_data ),
                                regexpr( "DD_", row.names( HDDCDD_data ), fixed = T ) - 1,
                                regexpr( "DD_", row.names( HDDCDD_data ), fixed = T ) + 1 )
#Assuming that the GCM is 6 characters (WON'T ALWAYS BE TRUE)
HDDCDD_data$GCM <- substr( row.names( HDDCDD_data ),
                           regexpr( "DD_", row.names( HDDCDD_data ), fixed = T ) + 3,
                           regexpr( "DD_", row.names( HDDCDD_data ), fixed = T ) + 8 )
#Assuming that there will be no "." in the file names
HDDCDD_data$SRES <- substr( row.names( HDDCDD_data ),
                                regexpr( "DD_", row.names( HDDCDD_data ), fixed = T ) + 10,
                                regexpr( ".", row.names( HDDCDD_data ), fixed = T ) - 1 )

row.names( HDDCDD_data ) <- 1:nrow( HDDCDD_data )
HDDCDD_data[ HDDCDD_data < 0 ] <- 0

HDDCDD_IDstrings <- c( "country", "SRES", "GCM", "variable" )
L143.HDDCDD_scen_ctry_Y <- HDDCDD_data[ c( HDDCDD_IDstrings, X_historical_years, X_future_years ) ]

#Replace the base year value (2010) with actual data from re-analysis dataset
base_year_HDDCDD_data.melt <- melt( base_year_HDDCDD_data, id.vars = c( "id", "country" ) )
L143.HDDCDD_scen_ctry_Y[[ "X2010" ]] <- base_year_HDDCDD_data.melt$value[
      match( vecpaste( L143.HDDCDD_scen_ctry_Y[ c( "country", "variable" ) ] ),
             vecpaste( base_year_HDDCDD_data.melt[ c( "country", "variable" ) ] ) ) ]

# 4. Multiply HDD/CDD by population in each year matching on the iso name
L143.HDDCDD_scen_ctry_Y$iso <- GIS_ctry$iso[ match( L143.HDDCDD_scen_ctry_Y$country, GIS_ctry$country ) ]

#Serbia and Montenegro are currently combined. Copy to separated countries, assigning the same HDD and CDD to each
if( "scg" %in% L143.HDDCDD_scen_ctry_Y$iso ){
	L143.HDDCDD_scen_scg_Y <- subset( L143.HDDCDD_scen_ctry_Y, iso == "scg" )
	L143.HDDCDD_scen_scg_Y <- repeat_and_add_vector( L143.HDDCDD_scen_scg_Y, "iso", c( "srb", "mne" ) )
	L143.HDDCDD_scen_ctry_Y <- rbind( subset( L143.HDDCDD_scen_ctry_Y, iso != "scg" ), L143.HDDCDD_scen_scg_Y )
}

L143.wtHDDCDD_scen_ctry_Y <- L143.HDDCDD_scen_ctry_Y
L143.wtHDDCDD_scen_ctry_Y [ c( X_historical_years, X_future_years ) ] <- L143.HDDCDD_scen_ctry_Y [ c( X_historical_years, X_future_years ) ] * 
      L101.Pop_thous_GCAM3_ctry_Y[ match( L143.wtHDDCDD_scen_ctry_Y$iso, L101.Pop_thous_GCAM3_ctry_Y$iso ),
      c( X_historical_years, X_future_years ) ]

# 5. aggregate HDD/CDD by GCAM region and region_GCAM3
L143.wtHDDCDD_scen_ctry_Y[[ R ]] <- iso_GCAM_regID[[ R ]][ match( L143.wtHDDCDD_scen_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L143.wtHDDCDD_scen_ctry_Y[[ "region_GCAM3" ]] <- iso_GCAM_regID[[ "region_GCAM3" ]][ match( L143.wtHDDCDD_scen_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L143.wtHDDCDD_scen_R_Y <- aggregate( L143.wtHDDCDD_scen_ctry_Y [ c( X_historical_years, X_future_years ) ],
      by=as.list( L143.wtHDDCDD_scen_ctry_Y [ c( R, "SRES", "GCM", "variable" ) ] ), FUN=sum )
L143.wtHDDCDD_scen_RG3_Y <- aggregate( L143.wtHDDCDD_scen_ctry_Y [ c( X_historical_years, X_future_years ) ],
      by=as.list( L143.wtHDDCDD_scen_ctry_Y [ c( "region_GCAM3", "SRES", "GCM", "variable" ) ] ), FUN=sum )

# 6. aggregate population by region
L101.Pop_thous_GCAM3_ctry_Y[[ R ]] <- iso_GCAM_regID[[ R ]][ match( L101.Pop_thous_GCAM3_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_GCAM3_ctry_Y[[ "region_GCAM3" ]] <- iso_GCAM_regID[[ "region_GCAM3" ]][ match( L101.Pop_thous_GCAM3_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_GCAM3_R_Y <- aggregate( L101.Pop_thous_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L101.Pop_thous_GCAM3_ctry_Y [ R ] ), FUN=sum )
L101.Pop_thous_GCAM3_RG3_Y <- aggregate( L101.Pop_thous_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L101.Pop_thous_GCAM3_ctry_Y [ "region_GCAM3" ] ), FUN=sum )

# 7. divide HDD/CDD by population
L143.HDDCDD_scen_R_Y <- L143.wtHDDCDD_scen_R_Y
L143.HDDCDD_scen_R_Y [ c( X_historical_years, X_future_years ) ] <- L143.wtHDDCDD_scen_R_Y [ c( X_historical_years, X_future_years ) ] / 
     L101.Pop_thous_GCAM3_R_Y[ match( L143.wtHDDCDD_scen_R_Y$GCAM_region_ID, L101.Pop_thous_GCAM3_R_Y$GCAM_region_ID  ),
     c( X_historical_years, X_future_years ) ]
L143.HDDCDD_scen_RG3_Y <- L143.wtHDDCDD_scen_RG3_Y
L143.HDDCDD_scen_RG3_Y[ c( X_historical_years, X_future_years ) ] <- L143.wtHDDCDD_scen_RG3_Y [ c( X_historical_years, X_future_years ) ] / 
     L101.Pop_thous_GCAM3_RG3_Y[ match( L143.wtHDDCDD_scen_RG3_Y$region_GCAM3, L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3  ),
     c( X_historical_years, X_future_years ) ]
# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L143.HDDCDD_scen_R_Y <- c( "Heating and cooling degree days by scenario (GCM and socioeconomic) / region / historical year","Unit = degree F days" )
comments.L143.HDDCDD_scen_RG3_Y <- c( "Heating and cooling degree days by scenario (GCM and socioeconomic) / GCAM3 region / historical year","Unit = degree F days" )
comments.L143.HDDCDD_scen_ctry_Y <- c( "Heating and cooling degree days by scenario (GCM and socioeconomic) / country / historical year","Unit = degree F days" )

#write tables as CSV files
writedata( L143.HDDCDD_scen_R_Y, domain="ENERGY_LEVEL1_DATA", fn="L143.HDDCDD_scen_R_Y", comments=comments.L143.HDDCDD_scen_R_Y )
writedata( L143.HDDCDD_scen_RG3_Y, domain="ENERGY_LEVEL1_DATA", fn="L143.HDDCDD_scen_RG3_Y", comments=comments.L143.HDDCDD_scen_RG3_Y )
writedata( L143.HDDCDD_scen_ctry_Y, domain="ENERGY_LEVEL1_DATA", fn="L143.HDDCDD_scen_ctry_Y", comments=comments.L143.HDDCDD_scen_ctry_Y )

# Every script should finish with this line
logstop()
