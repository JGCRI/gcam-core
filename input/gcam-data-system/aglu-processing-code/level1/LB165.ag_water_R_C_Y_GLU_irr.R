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
logstart( "LB165.ag_water_R_C_Y_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Water use coefficients by GCAM region, commodity, GLU" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L100.Water_footprint_m3 <- readdata( "AGLU_LEVEL1_DATA", "L100.Water_footprint_m3" )
Mekonnen_Hoekstra_Rep47_A2 <- readdata( "AGLU_LEVEL0_DATA", "Mekonnen_Hoekstra_Rep47_A2" )
Rohwer_2007_IrrigationEff <- readdata( "AGLU_LEVEL0_DATA", "Rohwer_2007_IrrigationEff" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )
L151.ag_irrProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrProd_t_ctry_crop" )
L151.ag_rfdProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdProd_t_ctry_crop" )
L151.ag_irrHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrHA_ha_ctry_crop" )

# ----------------------------------------------------------------q-------------
# 2. Perform computations

# Method note: The method here is simple in principle (only)--we are taking inventory estimates of either water demand
# coefficients by country and crop, or aggregated gridded volumes of water use by country, GLU, and crop, and
# using these estimates to calculate average water consumption coefficients by GCAM region, crop, GLU, and irrigation level.
# All data represent a statistical year around 2000, and the GTAP/Monfreda data were used in constructing the gridded
# and national inventory data, so this is the appropriate production data to use.
# The major methodological decisions relate to how to estimate the coefficients by GLU for crops that are not available
# in the gridded inventory, how to extrapolate from the given crops to the fodder crops, and how to assign priority for
# the crops included in both the gridded and national inventories. These decisions are documented below.

# The inventory data disaggregate green, blue, and gray water. Gray water indicates downstream pollution; this is not considered
# in GCAM and has not been included in the gridded data processing.
# Blue water is assigned to only irrigated production, but green water applies to both irrigated and rainfed production,
# as the two management technologies are not disaggregated in the inventory data.
# For rainfed crops, total biophysical = green, and for irrigated crops, total biophysical = blue + green.

printlog( "Initial data cleaning" )
L165.Water_footprint_m3 <- L100.Water_footprint_m3
names( L165.Water_footprint_m3 )[ names( L165.Water_footprint_m3 ) == "GTAP_crop" ] <- "MH_crop"
L165.Water_footprint_m3$GTAP_crop <- FAO_ag_items_PRODSTAT$GTAP_crop[ match( L165.Water_footprint_m3$MH_crop, FAO_ag_items_PRODSTAT$MH_crop ) ]

printlog( "Re-casting nation-level data so that blue and green are represented as data columns" )
L165.Mekonnen_Hoekstra_Rep47_A2 <- melt( Mekonnen_Hoekstra_Rep47_A2, id.vars = c( "FAO_crop", "water_type" ), variable.name = "iso", value.name = "coef_m3t" )
L165.Mekonnen_Hoekstra_Rep47_A2$coef_m3kg <- L165.Mekonnen_Hoekstra_Rep47_A2$coef_m3t * conv_kg_t
L165.Mekonnen_Hoekstra_Rep47_A2 <- dcast( L165.Mekonnen_Hoekstra_Rep47_A2, iso + FAO_crop ~ water_type, value.var = "coef_m3kg" )
#Set all missing values to zero -- this may be re-visited at another point. we may want to substitute default values from the MH2011 table
L165.Mekonnen_Hoekstra_Rep47_A2[ is.na( L165.Mekonnen_Hoekstra_Rep47_A2 ) ] <- 0

printlog( "Building inventory of water use by country, crop, and GLU for the 18 gridded crops" )
# We give precedence to the aggregated gridded data for the 18 crops that are mapped, and fill in nation-level inventory data
# for the remainder.
L165.ag_Prod_t_ctry_crop_GLU <- L100.LDS_ag_prod_t
names( L165.ag_Prod_t_ctry_crop_GLU )[ names( L165.ag_Prod_t_ctry_crop_GLU ) == "value" ] <- "Prod_t"
L165.ag_Water_ctry_MHcrop_GLU <- subset( L165.Water_footprint_m3, water_type %in% c( "green", "blue" ) )
L165.ag_Water_ctry_MHcrop_GLU$Prod_t <- L165.ag_Prod_t_ctry_crop_GLU$Prod_t[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_GLU[ c( "iso", "GTAP_crop", GLU ) ] ),
             vecpaste( L165.ag_Prod_t_ctry_crop_GLU[ c( "iso", "GTAP_crop", GLU ) ] ) ) ]

#M+H have some country/crop/GLU combinations that Monfreda doesn't. Set missing values to 0
L165.ag_Water_ctry_MHcrop_GLU$Prod_t[ is.na( L165.ag_Water_ctry_MHcrop_GLU$Prod_t ) ] <- 0

printlog( "For the 18 M+H gridded crops, calculate the coefs by country, GLU, and crop" )
printlog( "NOTE: dropping all country / GLU / crops with zero production; these won't have any water anyway" )
L165.ag_Water_ctry_MHcrop_GLU <- subset( L165.ag_Water_ctry_MHcrop_GLU, Prod_t > 0)
L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg <- with( L165.ag_Water_ctry_MHcrop_GLU, value / ( Prod_t * conv_t_kg ) )

printlog( "Clipping extremely high values which are due to statistical discrepancies" )
printlog( "The assumed maximum values are crop specific and based on analysis of the 2011 inventory data" )
# In this step, we're adding 2 m3/kg to every maximum observed value across all national averages. We want to use something a bit
# higher than the maximum of the national averages, because the sub-national regions' values will have a greater range.
# Still, this sort of cap is necessary because many of the water quantities, particularly in small regions,
# are clearly inconsistent with the underlying production data and produce extremely high water demand coefficients.
# In the case of Barley in Iraq it is clearly an error in the M+H gridded inventory, but for others it could be a simple
# mis-match in estimates of crop production by region, and share of irrigated production by region.
ctry_GLU_max_adder <- 2
L165.MaxWaterCoefs_m3kg <- aggregate( L165.Mekonnen_Hoekstra_Rep47_A2[ c( "Blue", "Green" ) ] + ctry_GLU_max_adder,
      by = L165.Mekonnen_Hoekstra_Rep47_A2[ "FAO_crop" ], max )
L165.MaxWaterCoefs_m3kg$MH_crop <- FAO_ag_items_PRODSTAT$MH_crop[ match( L165.MaxWaterCoefs_m3kg$FAO_crop, FAO_ag_items_PRODSTAT$item ) ]

L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "blue" ] <- pmin(
      L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "blue" ],
      L165.MaxWaterCoefs_m3kg$Blue[
        match( L165.ag_Water_ctry_MHcrop_GLU$MH_crop[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "blue" ],
               L165.MaxWaterCoefs_m3kg$MH_crop ) ] )
L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "green" ] <- pmin(
  L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "green" ],
  L165.MaxWaterCoefs_m3kg$Green[
    match( L165.ag_Water_ctry_MHcrop_GLU$MH_crop[ L165.ag_Water_ctry_MHcrop_GLU$water_type == "green" ],
           L165.MaxWaterCoefs_m3kg$MH_crop ) ] )

printlog( "Working through calculation sequence to determine blue and green water coefficients for irrigated and rainfed production for gridded crops" )
# The sequence has 5 steps:
#	(1) calculate the total biophysical (green + blue) water quantities and coefficients for each country, GLU, and crop.
#	(2) Match in the irrigated production and calculate the blue water coef for irrigated as min( blue water quantity / irr prod, total biophysical coef )
#	(3) Calculate the green water coef for irrigated crops as the total biophysical coef minus the blue water coef
#	(4) Calculate the total volume of green water on rainfed crops, as the total green water minus green water used by irrigated crops
#	(5) Calculate the green water coef for rainfed crops as rainfed green water volume divided by rainfed production
L165.ag_Water_ctry_MHcrop_GLU <- dcast( L165.ag_Water_ctry_MHcrop_GLU, iso + GTAP_crop + GLU + Prod_t ~ water_type, value.var = "coef_m3kg" )
#Missing values from the cast where blue was zero and green was positive
L165.ag_Water_ctry_MHcrop_GLU[ is.na( L165.ag_Water_ctry_MHcrop_GLU ) ] <- 0
names( L165.ag_Water_ctry_MHcrop_GLU )[ names( L165.ag_Water_ctry_MHcrop_GLU ) %in% c( "blue", "green" ) ] <-
  paste0( names( L165.ag_Water_ctry_MHcrop_GLU )[ names( L165.ag_Water_ctry_MHcrop_GLU ) %in% c( "blue", "green" ) ], "_m3kg" )
L165.ag_Water_ctry_MHcrop_GLU$total_m3kg <- with( L165.ag_Water_ctry_MHcrop_GLU, blue_m3kg + green_m3kg )
L165.ag_Water_ctry_MHcrop_GLU$blue_thousm3 <- with( L165.ag_Water_ctry_MHcrop_GLU, Prod_t * blue_m3kg )
L165.ag_Water_ctry_MHcrop_GLU$green_thousm3 <- with( L165.ag_Water_ctry_MHcrop_GLU, Prod_t * green_m3kg )

printlog( "Matching in the irrigated production to calculate the blue water coef for irrigated crops" )
L165.ag_Water_ctry_MHcrop_GLU$irrProd_t <- L151.ag_irrProd_t_ctry_crop$irrProd[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_GLU[ c( "iso", "GTAP_crop", GLU ) ] ),
             vecpaste( L151.ag_irrProd_t_ctry_crop[ c( "iso", "GTAP_crop", GLU ) ] ) ) ]
L165.ag_Water_ctry_MHcrop_GLU$BlueIrr_m3kg <- L165.ag_Water_ctry_MHcrop_GLU$blue_thousm3 / L165.ag_Water_ctry_MHcrop_GLU$irrProd_t

#Set the coefs to zero wherever irrigated production is zero (these couldn't have any water consumption in GCAM anyway)
L165.ag_Water_ctry_MHcrop_GLU$BlueIrr_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$irrProd_t == 0 ] <- 0
printlog( "Capping blue water coefficients at the total biophysical quantity" )
L165.ag_Water_ctry_MHcrop_GLU$BlueIrr_m3kg <- pmin( L165.ag_Water_ctry_MHcrop_GLU$BlueIrr_m3kg, L165.ag_Water_ctry_MHcrop_GLU$total_m3kg )

printlog( "Calculating the green water coefs of irrigated crops as total biophysical coef minus calculated blue water coef" )
L165.ag_Water_ctry_MHcrop_GLU$GreenIrr_m3kg <- with( L165.ag_Water_ctry_MHcrop_GLU, total_m3kg - BlueIrr_m3kg )

printlog( "Calculating the green water quantities for rainfed crops as total green water minus green water on irrigated" )
L165.ag_Water_ctry_MHcrop_GLU$GreenIrr_thousm3 <- with( L165.ag_Water_ctry_MHcrop_GLU, GreenIrr_m3kg * irrProd_t )
L165.ag_Water_ctry_MHcrop_GLU$GreenRfd_thousm3 <- with( L165.ag_Water_ctry_MHcrop_GLU, green_thousm3 - GreenIrr_thousm3 )
L165.ag_Water_ctry_MHcrop_GLU$rfdProd_t <- L151.ag_rfdProd_t_ctry_crop$rfdProd[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_GLU[ c( "iso", "GTAP_crop", GLU ) ] ),
             vecpaste( L151.ag_rfdProd_t_ctry_crop[ c( "iso", "GTAP_crop", GLU ) ] ) ) ]

printlog( "Calculating the green water coef on rainfed as green water on rainfed divided by rainfed production" )
L165.ag_Water_ctry_MHcrop_GLU$GreenRfd_m3kg <- with( L165.ag_Water_ctry_MHcrop_GLU, GreenRfd_thousm3 / rfdProd_t )
L165.ag_Water_ctry_MHcrop_GLU$GreenRfd_m3kg[ L165.ag_Water_ctry_MHcrop_GLU$rfdProd_t == 0 ] <- 0

printlog( "This table needs to be expanded to the more complete list of crops in the MH2011 inventory" )
printlog( "The expanded set only includes crops not already accounted in the gridded inventory" )
printlog( "Increasing the data set in countries and crops in the country-level inventory, and where production is non-zero" )
GTAP_addl_crops <- sort( unique( FAO_ag_items_PRODSTAT$GTAP_crop[ is.na( FAO_ag_items_PRODSTAT$MH_crop ) ] ) )
L165.ag_Water_ctry_MHcropX_GLU <- subset( L165.ag_Prod_t_ctry_crop_GLU,
      iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso &
      GTAP_crop %in% GTAP_addl_crops )

# We don't want to multiply each GLU's total production by the nation-level blue and green coefs, as this will do a poor job of
# allocating blue water to the GLUs with the most irrigated production. The method followed here is similar to the method followed above for
# gridded crops, but here it is performed without any GLU-level detail.
printlog( "Dropping the GLU from the first stage of calculations for the additional country-level crops" )
L165.ag_Water_ctry_MHcropX <- aggregate( L165.ag_Water_ctry_MHcropX_GLU[ "Prod_t" ],
      by = L165.ag_Water_ctry_MHcropX_GLU[ c( "iso", "GTAP_crop" ) ], sum )
L165.ag_Water_ctry_MHcropX$FAO_crop <- FAO_ag_items_PRODSTAT$item[
      match( L165.ag_Water_ctry_MHcropX$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

# A number of crops that are not in the MH country-level inventory--mostly the fodder crops--will instead inherit their water consumption
# coefficients from a proxy crop, indicated in the FAO_ag_items_PRODSTAT mapping file.
crops_to_substitute <- FAO_ag_items_PRODSTAT$item[ !is.na( FAO_ag_items_PRODSTAT$MH2014_proxy ) ]
proxy_crops <- FAO_ag_items_PRODSTAT$MH2014_proxy[ match( crops_to_substitute, FAO_ag_items_PRODSTAT$item ) ]
L165.ag_Water_ctry_MHcropX$FAO_crop[ L165.ag_Water_ctry_MHcropX$FAO_crop %in% crops_to_substitute ] <-
      proxy_crops[ match( L165.ag_Water_ctry_MHcropX$FAO_crop[ L165.ag_Water_ctry_MHcropX$FAO_crop %in% crops_to_substitute ],
                          crops_to_substitute ) ]

#Match in the water contents, and drop the crops that aren't assigned to anything, which will return missing values here
L165.ag_Water_ctry_MHcropX[ c( "blue_m3kg", "green_m3kg" ) ] <- L165.Mekonnen_Hoekstra_Rep47_A2[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "FAO_crop" ) ] ),
             vecpaste( L165.Mekonnen_Hoekstra_Rep47_A2[ c( "iso", "FAO_crop" ) ] ) ),
      c( "Blue", "Green" ) ]
L165.ag_Water_ctry_MHcropX <- na.omit( L165.ag_Water_ctry_MHcropX )

# See below for note on fodder crops - this method over-states the water coefs of fodder crops, and it is adjusted later

printlog( "The next steps follow the 5-step method described above for assigning blue and green water to irrigated and rainfed production" )
L165.ag_Water_ctry_MHcropX$total_m3kg <- with( L165.ag_Water_ctry_MHcropX, blue_m3kg + green_m3kg )
L165.ag_Water_ctry_MHcropX$blue_thousm3 <- with( L165.ag_Water_ctry_MHcropX, Prod_t * blue_m3kg )
L165.ag_Water_ctry_MHcropX$green_thousm3 <- with( L165.ag_Water_ctry_MHcropX, Prod_t * green_m3kg )

printlog( "Matching in the irrigated production to calculate the blue water coef for irrigated crops" )
L165.irrProd_t_ctry_crop <- aggregate( L151.ag_irrProd_t_ctry_crop[ "irrProd"],
                                       by = L151.ag_irrProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ], sum )
L165.ag_Water_ctry_MHcropX$irrProd_t <- L165.irrProd_t_ctry_crop$irrProd[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ),
             vecpaste( L165.irrProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg <- with( L165.ag_Water_ctry_MHcropX, blue_thousm3 / irrProd_t )
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg[ L165.ag_Water_ctry_MHcropX$irrProd_t == 0 ] <- 0
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg <- pmin( L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg, L165.ag_Water_ctry_MHcropX$total_m3kg )

printlog( "Calculating the green water coefs of irrigated crops as total biophysical coef minus calculated blue water coef" )
L165.ag_Water_ctry_MHcropX$GreenIrr_m3kg <- with( L165.ag_Water_ctry_MHcropX, total_m3kg - BlueIrr_m3kg )

printlog( "Calculating the green water coefs of rainfed crops as total green water minus green water on irrigated" )
L165.ag_Water_ctry_MHcropX$GreenIrr_thousm3 <- with( L165.ag_Water_ctry_MHcropX, GreenIrr_m3kg * irrProd_t )
L165.ag_Water_ctry_MHcropX$GreenRfd_thousm3 <- with( L165.ag_Water_ctry_MHcropX, green_thousm3 - GreenIrr_thousm3 )
L165.rfdProd_t_ctry_crop <- aggregate( L151.ag_rfdProd_t_ctry_crop[ "rfdProd"],
                                       by = L151.ag_rfdProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ], sum )
L165.ag_Water_ctry_MHcropX$rfdProd_t <- L165.rfdProd_t_ctry_crop$rfdProd[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ),
             vecpaste( L165.rfdProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]
L165.ag_Water_ctry_MHcropX$GreenRfd_m3kg <- with( L165.ag_Water_ctry_MHcropX, GreenRfd_thousm3 / rfdProd_t )
L165.ag_Water_ctry_MHcropX$GreenRfd_m3kg[ L165.ag_Water_ctry_MHcropX$rfdProd_t == 0 ] <- 0

printlog( "The grid-based and nation-based tables of green and blue water coefs and volumes for irrigated and rainfed crops..." )
printlog( "...can now be combined to provide estimates of green and blue water coefs for all available countries, crops, and GLUs" )
# First we need to write out the MH2014 coefficients to all applicable GLUs
L165.ag_Water_ctry_MHcropX_GLU <- subset( L165.ag_Prod_t_ctry_crop_GLU,
      iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso &
      GTAP_crop %in% L165.ag_Water_ctry_MHcropX$GTAP_crop )[ c( "iso", "GTAP_crop", GLU ) ]

L165.ag_Water_ctry_MHcropX_GLU$irrProd_t <-L151.ag_irrProd_t_ctry_crop$irrProd[
      match( vecpaste( L165.ag_Water_ctry_MHcropX_GLU[ c( "iso", "GTAP_crop", GLU ) ] ),
             vecpaste( L151.ag_irrProd_t_ctry_crop[ c( "iso", "GTAP_crop", GLU ) ] ) ) ]
L165.ag_Water_ctry_MHcropX_GLU$rfdProd_t <-L151.ag_rfdProd_t_ctry_crop$rfdProd[
      match( vecpaste( L165.ag_Water_ctry_MHcropX_GLU[ c( "iso", "GTAP_crop", GLU ) ] ),
             vecpaste( L151.ag_rfdProd_t_ctry_crop[ c( "iso", "GTAP_crop", GLU ) ] ) ) ]
L165.ag_Water_ctry_MHcropX_GLU[ c( "BlueIrr_m3kg", "GreenIrr_m3kg", "GreenRfd_m3kg" ) ] <-
      L165.ag_Water_ctry_MHcropX[
        match( vecpaste( L165.ag_Water_ctry_MHcropX_GLU[ c( "iso", "GTAP_crop" ) ] ),
               vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ) ),
        c( "BlueIrr_m3kg", "GreenIrr_m3kg", "GreenRfd_m3kg" ) ]

# Then combine with the grid-based estimates
L165.ag_Water_ctry_crop_GLU <- rbind( L165.ag_Water_ctry_MHcrop_GLU[ names( L165.ag_Water_ctry_MHcropX_GLU ) ], L165.ag_Water_ctry_MHcropX_GLU )

printlog( "Calculating green and blue water quantities by all available countries, crops, GLUs, and irrigated/rainfed" )
L165.ag_Water_ctry_crop_GLU$BlueIrr_thousm3 <- with( L165.ag_Water_ctry_crop_GLU, BlueIrr_m3kg * irrProd_t )
L165.ag_Water_ctry_crop_GLU$GreenIrr_thousm3 <- with( L165.ag_Water_ctry_crop_GLU, GreenIrr_m3kg * irrProd_t )
L165.ag_Water_ctry_crop_GLU$GreenRfd_thousm3 <- with( L165.ag_Water_ctry_crop_GLU, GreenRfd_m3kg * rfdProd_t )

printlog( "Matching in GCAM regions and commodities for aggregation" )
L165.ag_Water_ctry_crop_GLU[[R]] <- iso_GCAM_regID[[R]][ match( L165.ag_Water_ctry_crop_GLU$iso, iso_GCAM_regID$iso ) ]
L165.ag_Water_ctry_crop_GLU[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L165.ag_Water_ctry_crop_GLU$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

printlog( "NOTE: Fodder crops are not in the M+H inventories. They instead have been assigned a corresponding crop that is in the M+H inventories..." )
printlog( "... and at this point are multiplied by an exogenous fodder:crop adjustment factor" )
# This fodder:crop adjustment factor is designed to reflect that fodder crop masses include the whole plant, often harvested green (~70% water).
# As such, their water coefficients will tend to be a good deal lower than the corresponding crop to which they are assigned.
# The specific number is based on analysis of a virtual water content dataset (Chapagain 2004) that included fodder crops
# The "corresponding" crops are assigned in the mapping file (FAO_ag_items_PRODSTAT), based on species (e.g., maize) or family (e.g., alfalfa)
conv_crop_fodder <- 0.2
L165.ag_Water_ctry_crop_GLU[
      L165.ag_Water_ctry_crop_GLU[[C]] %in% c( "FodderGrass", "FodderHerb" ), c( "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ] <-
   L165.ag_Water_ctry_crop_GLU[
      L165.ag_Water_ctry_crop_GLU[[C]] %in% c( "FodderGrass", "FodderHerb" ), c( "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ] *
      conv_crop_fodder

printlog( "At this point, the crops are ready for aggregation by GCAM region and commodity" )
L165.ag_Water_R_C_GLU <- aggregate( L165.ag_Water_ctry_crop_GLU[ c( "irrProd_t", "rfdProd_t", "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ],
      by = L165.ag_Water_ctry_crop_GLU[ R_C_GLU ], sum )
L165.ag_Water_R_C_GLU$BlueIrr_m3kg <- with( L165.ag_Water_R_C_GLU, BlueIrr_thousm3 / irrProd_t )
L165.ag_Water_R_C_GLU$GreenIrr_m3kg <- with( L165.ag_Water_R_C_GLU, GreenIrr_thousm3 / irrProd_t )
L165.ag_Water_R_C_GLU$GreenRfd_m3kg <- with( L165.ag_Water_R_C_GLU, GreenRfd_thousm3 / rfdProd_t )
L165.ag_Water_R_C_GLU$TotIrr_m3kg <- with( L165.ag_Water_R_C_GLU, BlueIrr_m3kg + GreenIrr_m3kg )

#Re-set missing values (NaN) to zero
L165.ag_Water_R_C_GLU[ is.na( L165.ag_Water_R_C_GLU ) ] <- 0

printlog( "Create the final tables to be written out" )
L165.BlueIrr_m3kg_R_C_GLU <- L165.ag_Water_R_C_GLU[ c( R_C_GLU, "BlueIrr_m3kg" ) ]
L165.TotIrr_m3kg_R_C_GLU <- L165.ag_Water_R_C_GLU[ c( R_C_GLU, "TotIrr_m3kg" ) ]
L165.GreenRfd_m3kg_R_C_GLU <- L165.ag_Water_R_C_GLU[ c( R_C_GLU,"GreenRfd_m3kg" ) ]

printlog( "Irrigation Efficiency: Compile country-level data to GCAM regions, weighted by total irrigated harvested area" )
printlog( "NOTE: Using application and management efficiencies to define whole-system efficiencies (not conveyance)")
Rohwer_2007_IrrigationEff$field.eff <- with( Rohwer_2007_IrrigationEff, application.eff * management.eff )
L165.ag_irrHA_ha_ctry <- aggregate( L151.ag_irrHA_ha_ctry_crop[ "irrHA" ],
                                    by = L151.ag_irrHA_ha_ctry_crop[ "iso" ], sum )
L165.ag_irrHA_ha_ctry[ c( "field.eff", "conveyance.eff" ) ] <- Rohwer_2007_IrrigationEff[
      match( L165.ag_irrHA_ha_ctry$iso, Rohwer_2007_IrrigationEff$iso ),
      c( "field.eff", "conveyance.eff" ) ]
L165.ag_irrHA_ha_ctry <- na.omit( subset( L165.ag_irrHA_ha_ctry, irrHA > 0 ) )
L165.ag_irrHA_ha_ctry[ c( "field.eff.wtd", "conveyance.eff.wtd" ) ] <-
      L165.ag_irrHA_ha_ctry[ c( "field.eff", "conveyance.eff" ) ] * 
      L165.ag_irrHA_ha_ctry$irrHA
L165.ag_irrHA_ha_ctry[[R]] <- iso_GCAM_regID[[R]][ match( L165.ag_irrHA_ha_ctry$iso, iso_GCAM_regID$iso ) ]
L165.ag_IrrEff_R <- aggregate( L165.ag_irrHA_ha_ctry[ c( "field.eff.wtd", "conveyance.eff.wtd", "irrHA" ) ],
      by = L165.ag_irrHA_ha_ctry[ R ], sum )
L165.ag_IrrEff_R[ c( "field.eff", "conveyance.eff" ) ] <-
      L165.ag_IrrEff_R[ c( "field.eff.wtd", "conveyance.eff.wtd" ) ] /
      L165.ag_IrrEff_R$irrHA
L165.ag_IrrEff_R <- L165.ag_IrrEff_R[ c( R, "field.eff", "conveyance.eff" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L165.BlueIrr_m3kg_R_C_GLU <- c( "Blue water consumption coefficients for irrigated crops by GCAM region / commodity / GLU","Unit = m3 / kg" )
comments.L165.TotIrr_m3kg_R_C_GLU <- c( "Total biophysical water consumption coefficients for irrigated crops by GCAM region / commodity / GLU","Unit = m3 / kg" )
comments.L165.GreenRfd_m3kg_R_C_GLU <- c( "Green water consumption coefficients for rainfed crops by GCAM region / commodity / GLU","Unit = m3 / kg" )
comments.L165.ag_IrrEff_R <- c( "Irrigation efficiency by GCAM region","Unitless efficiency" )

#export final tables as CSV files
writedata( L165.BlueIrr_m3kg_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L165.BlueIrr_m3kg_R_C_GLU", comments=comments.L165.BlueIrr_m3kg_R_C_GLU )
writedata( L165.TotIrr_m3kg_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L165.TotIrr_m3kg_R_C_GLU", comments=comments.L165.TotIrr_m3kg_R_C_GLU )
writedata( L165.GreenRfd_m3kg_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L165.GreenRfd_m3kg_R_C_GLU", comments=comments.L165.GreenRfd_m3kg_R_C_GLU )
writedata( L165.ag_IrrEff_R, domain="AGLU_LEVEL1_DATA",fn="L165.ag_IrrEff_R", comments=comments.L165.ag_IrrEff_R )

# Every script should finish with this line
logstop()
            
