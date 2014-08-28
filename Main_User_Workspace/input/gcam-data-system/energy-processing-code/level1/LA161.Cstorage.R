# L161.geo.R

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
logstart( "LA161.Cstorage.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Carbon storage resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
Sage_Hyde15_Area <- readdata( "AGLU_GIS_DATA", "Sage_Hyde15_Area" )
A61.Cstorage_curves <- readdata( "ENERGY_ASSUMPTIONS", "A61.Cstorage_curves" )
Dooley_Cstorage_RG3_MtCO2 <- readdata( "ENERGY_LEVEL0_DATA", "Dooley_Cstorage_RG3_MtCO2" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Calculate the supply curves by GCAM 3.0 region based on the available data
printlog( "Processing data on carbon storage resources in order to generate available supplies by GCAM 3 region" )
L161.Cstorage_MtCO2_RG3_src <- melt( Dooley_Cstorage_RG3_MtCO2, id.vars = "region_GCAM3" )
printlog( "NOTE: Not including any offshore carbon resources in the onshore resource estimates. Offshore is considered an unlimited resource")
L161.Cstorage_MtCO2_RG3_src <- subset( L161.Cstorage_MtCO2_RG3_src, !grepl( "Offshore", variable ) )
L161.Cstorage_MtCO2_RG3 <- aggregate( L161.Cstorage_MtCO2_RG3_src[ "value" ], by=as.list( L161.Cstorage_MtCO2_RG3_src[ "region_GCAM3" ] ), sum )

#Repeat by number of grades, and multiply by the fraction of the total resource assigned to each grade
L161.Cstorage_MtCO2_RG3_grade <- repeat_and_add_vector( L161.Cstorage_MtCO2_RG3, "grade", unique( A61.Cstorage_curves$grade ) )
L161.Cstorage_MtCO2_RG3_grade$available <- L161.Cstorage_MtCO2_RG3_grade$value * A61.Cstorage_curves$fraction[
      match( L161.Cstorage_MtCO2_RG3_grade$grade, A61.Cstorage_curves$grade ) ]

printlog( "Downscaling GCAM 3.0 carbon storage supply curves to countries on the basis of land area" )
#Calculate land cover shares of GCAM regions within region_GCAM3
L161.LC_km2_ctry_LT_AEZ <- subset( Sage_Hyde15_Area, Year == max( Year ) )
L161.LC_km2_ctry_LT_AEZ$iso <- GIS_ctry_AEZ$iso[ match( L161.LC_km2_ctry_LT_AEZ$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
L161.LC_km2_ctry <- aggregate( L161.LC_km2_ctry_LT_AEZ[ "Area.km2." ],
      by=as.list( L161.LC_km2_ctry_LT_AEZ[ "iso" ] ), sum )

###ADD IN TAIWAN TO THE LAND AREA
L161.LC_km2_ctry <- rbind( L161.LC_km2_ctry, data.frame( iso = "twn", Area.km2. = 36000))

#Match in the GCAM 3.0 region and aggregate to compute shares of countries within GCAM 3.0 region
L161.LC_km2_ctry$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L161.LC_km2_ctry$iso, iso_GCAM_regID$iso ) ]
L161.LC_km2_RG3 <- aggregate( L161.LC_km2_ctry[ "Area.km2." ], by=as.list( L161.LC_km2_ctry[ "region_GCAM3" ] ), sum )
L161.LC_km2_ctry$area_RG3 <- L161.LC_km2_RG3$Area.km2.[ match( L161.LC_km2_ctry$region_GCAM3, L161.LC_km2_RG3$region_GCAM3 ) ]
L161.LC_km2_ctry$share <- L161.LC_km2_ctry$Area.km2. / L161.LC_km2_ctry$area_RG3

#Repeat by number of grades, and match in the available quantities
#NOTE: The carbon storage quantities from the literature are in CO2; for GCAM need to convert to C. This is done here
L161.Available_MtC_ctry_C <- repeat_and_add_vector( L161.LC_km2_ctry, "grade", unique( A61.Cstorage_curves$grade ) )
L161.Available_MtC_ctry_C$available_RG3 <- L161.Cstorage_MtCO2_RG3_grade$available[
      match( vecpaste( L161.Available_MtC_ctry_C[ c( "region_GCAM3", "grade" ) ] ), vecpaste( L161.Cstorage_MtCO2_RG3_grade[ c( "region_GCAM3", "grade" ) ] ) ) ] /
      conv_C_CO2
L161.Available_MtC_ctry_C$available <- L161.Available_MtC_ctry_C$share * L161.Available_MtC_ctry_C$available_RG3

printlog( "Aggregating country-level supplies by GCAM regions")
L161.Available_MtC_ctry_C[[R]]<- iso_GCAM_regID[[R]][ match( L161.Available_MtC_ctry_C$iso, iso_GCAM_regID$iso ) ]
L161.Available_MtC_R_C <- aggregate( L161.Available_MtC_ctry_C[ "available" ], by=as.list( L161.Available_MtC_ctry_C[ c( R, "grade" ) ] ), sum )

printlog( "Building carbon storage supply curves")
L161.RsrcCurves_MtC_R <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A61.Cstorage_curves$resource ),
      subresource = unique( A61.Cstorage_curves$subresource) )
L161.RsrcCurves_MtC_R <- repeat_and_add_vector( L161.RsrcCurves_MtC_R, "grade", unique(  A61.Cstorage_curves$grade ) )
L161.RsrcCurves_MtC_R$available <- L161.Available_MtC_R_C$available[
      match( vecpaste( L161.RsrcCurves_MtC_R[ c( R, "grade" ) ] ),
             vecpaste( L161.Available_MtC_R_C[ c( R, "grade" ) ] ) ) ]
#NOTE: currently assuming that all regions have the same price points
# Rounding the digits here, as the fossil supply curves are normally already rounded to one digit
L161.RsrcCurves_MtC_R$extractioncost <- round(
      A61.Cstorage_curves$cost_2005USDtCO2[ match( L161.RsrcCurves_MtC_R$grade, A61.Cstorage_curves$grade ) ] *
         conv_C_CO2 / conv_1990_2005_USD,
      digits_cost )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L161.RsrcCurves_MtC_R <- c( "Carbon storage resource supply curves by GCAM region","Unit = Mt C" )

#write tables as CSV files
writedata( L161.RsrcCurves_MtC_R, domain="ENERGY_LEVEL1_DATA", fn="L161.RsrcCurves_MtC_R", comments=comments.L161.RsrcCurves_MtC_R )

# Every script should finish with this line
logstop()
