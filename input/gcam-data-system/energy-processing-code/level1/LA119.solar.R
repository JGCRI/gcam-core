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
logstart( "LA119.solar.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Solar relative capacity calculations" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
Sage_Hyde15_Area <- readdata( "AGLU_GIS_DATA", "Sage_Hyde15_Area" )

Smith_irradiance_ctry_kwh <- readdata( "ENERGY_LEVEL0_DATA", "Smith_irradiance_ctry_kwh" )

col_names_with_R <- function( d, exclude_cols ) {
    cols <- names( d )
    cols[ cols %!in% exclude_cols ] <- paste( cols[ cols %!in% exclude_cols ], ".R", sep="" )
    return( cols )
}

col_names_without_R <- function( d, exclude_cols ) {
    cols <- names( d )
    cols[ cols %!in% exclude_cols ] <- sub( '.R$', '', cols[ cols %!in% exclude_cols ] )
    return( cols )
}

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "The irradiance data contained other * region categories which we will have to downscale" )
# Seperate the other * region from the ones with valid country codes that do not need additonal
# downscaling
L119.Irradiance_kwh_otherR <- subset( Smith_irradiance_ctry_kwh, grepl( 'OTHER', iso ) )
L119.Irradiance_kwh_ctry <- subset( Smith_irradiance_ctry_kwh, !grepl( 'OTHER', iso ) )

# Rename columns to indicate these are the regional values which will be used later
# to downscale to the country
L119.Irradiance_kwh_otherR$region_GCAM3 <- sub( 'OTHER_', '', L119.Irradiance_kwh_otherR$iso )
L119.Irradiance_kwh_otherR$iso <- NULL
names( L119.Irradiance_kwh_otherR ) <- col_names_with_R( L119.Irradiance_kwh_otherR, c( "region_GCAM3" ) )

L119.Irradiance_kwh_ctry <- merge( L119.Irradiance_kwh_ctry, iso_GCAM_regID )

# Create a list of coutries that are missing and are in the other * region
L119.Other_ctry <- iso_GCAM_regID[ iso_GCAM_regID$iso %!in% L119.Irradiance_kwh_ctry$iso &
    iso_GCAM_regID$region_GCAM3 %in% L119.Irradiance_kwh_otherR$region_GCAM3, ]

# Building up land areas of all of other countries and calculate their share of land
# in the other * region
L119.LC_km2_ctry_LT_AEZ <- subset( Sage_Hyde15_Area, Year == max( Year ) )
L119.LC_km2_ctry_LT_AEZ$iso <- GIS_ctry_AEZ$iso[ match( L119.LC_km2_ctry_LT_AEZ$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
L119.LC_km2_ctry <- aggregate( L119.LC_km2_ctry_LT_AEZ[ "Area.km2." ],
      by=as.list( L119.LC_km2_ctry_LT_AEZ[ "iso" ] ), sum ) 
L119.LC_km2_other_ctry <- merge( L119.Other_ctry, L119.LC_km2_ctry )
L119.LC_km2_other_R <- aggregate( Area.km2. ~ region_GCAM3, L119.LC_km2_other_ctry, FUN=sum )
names( L119.LC_km2_other_R )[ names( L119.LC_km2_other_R ) == "Area.km2." ] <- "Area.km2.R"
L119.LC_km2_other_ctry <- merge( L119.LC_km2_other_ctry, L119.LC_km2_other_R )
L119.LC_km2_other_ctry$Area.share <- L119.LC_km2_other_ctry$Area.km2. / L119.LC_km2_other_ctry$Area.km2.R

# Now include the irradiance data at the regional level and multiply the share to downscale to the
# country
L119.Irradiance_kwh_other_ctry <- merge( L119.LC_km2_other_ctry, L119.Irradiance_kwh_otherR )
L119.Irradiance_kwh_other_ctry[, col_names_without_R( L119.Irradiance_kwh_otherR, c( "region_GCAM3" ) ) ] <-
    L119.Irradiance_kwh_other_ctry[, names( L119.Irradiance_kwh_otherR )[ names( L119.Irradiance_kwh_otherR ) != "region_GCAM3" ] ] *
    L119.Irradiance_kwh_other_ctry$Area.share

# Add the downscaled coutries in with the rest of the country irradiance data
L119.Irradiance_kwh_ctry <- rbind( L119.Irradiance_kwh_ctry,
    L119.Irradiance_kwh_other_ctry[, names( L119.Irradiance_kwh_ctry ) ] )

printlog( "Aggregate irradiance data to the regional level" )
L119.Irradiance_kwh_R <- aggregate( cbind( irradiance, irradiance.area, dni, dni.area ) ~ GCAM_region_ID,
    L119.Irradiance_kwh_ctry, FUN=sum )

printlog( "Compute average irradiance" )
L119.Irradiance_kwh_R$irradiance_avg <- L119.Irradiance_kwh_R$irradiance / L119.Irradiance_kwh_R$irradiance.area
L119.Irradiance_kwh_R$dni_avg <- L119.Irradiance_kwh_R$dni / L119.Irradiance_kwh_R$dni.area

printlog( "Calculate average relative to USA" )
L119.Irradiance_kwh_usa <- subset( L119.Irradiance_kwh_ctry, iso == "usa" )
L119.Irradiance_kwh_usa$irradiance_avg <- L119.Irradiance_kwh_usa$irradiance / L119.Irradiance_kwh_usa$irradiance.area
L119.Irradiance_kwh_usa$dni_avg <- L119.Irradiance_kwh_usa$dni / L119.Irradiance_kwh_usa$dni.area
L119.Irradiance_kwh_R$irradiance_avg_rel <- L119.Irradiance_kwh_R$irradiance_avg / L119.Irradiance_kwh_usa$irradiance_avg
L119.Irradiance_kwh_R$dni_avg_rel <- L119.Irradiance_kwh_R$dni_avg / L119.Irradiance_kwh_usa$dni_avg
L119.Irradiance_kwh_R[ is.na( L119.Irradiance_kwh_R$irradiance_avg_rel ), "irradiance_avg_rel" ] <- 0.001
L119.Irradiance_kwh_R[ is.na( L119.Irradiance_kwh_R$dni_avg_rel ), "dni_avg_rel" ] <- 0.001

# The only thing we need for later processing is relative irradiance
L119.Irradiance_rel_R <- L119.Irradiance_kwh_R[, c( R, "irradiance_avg_rel", "dni_avg_rel" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L119.Irradiance_rel_R <- c( "Relative solar irradiance by GCAM region","Unit = rel" )

#write tables as CSV files
writedata( L119.Irradiance_rel_R, domain="ENERGY_LEVEL1_DATA", fn="L119.Irradiance_rel_R", comments=comments.L119.Irradiance_rel_R )

# Every script should finish with this line
logstop()
