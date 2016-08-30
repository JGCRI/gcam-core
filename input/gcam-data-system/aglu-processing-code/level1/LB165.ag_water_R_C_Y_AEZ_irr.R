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
logstart( "LB165.ag_water_R_C_Y_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Water use coefficients by GCAM region, commodity, AEZ" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
WaterFootprint_ctry_aez_crop <- readdata( "AGLU_GIS_DATA", "WaterFootprint_ctry_aez_crop" )
Mekonnen_Hoekstra_Rep47_A2 <- readdata( "AGLU_LEVEL0_DATA", "Mekonnen_Hoekstra_Rep47_A2" )
Rohwer_2007_IrrigationEff <- readdata( "AGLU_LEVEL0_DATA", "Rohwer_2007_IrrigationEff" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )
L151.GTAP_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrProd_t" )
L151.GTAP_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdProd_t" )
L151.GTAP_ag_irrHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrHA_ha" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )

# ----------------------------------------------------------------q-------------
# 2. Perform computations
# Method note: The method here is simple in principle (only)--we are taking inventory estimates of either water demand
# coefficients by country and crop, or aggregated gridded volumes of water use by country, AEZ, and crop, and
# using these estimates to calculate average water consumption coefficients by GCAM region, crop, AEZ, and irrigation.
# All data represent a statistical year around 2000, and the GTAP/Monfreda data were used in constructing the gridded
# and national inventory data, so this is the appropriate production data to use.
# The major methodological decisions relate to how to estimate the coefficients by AEZ for crops that are not available
# in the gridded inventory, how to extrapolate from the given crops to the fodder crops, and how to assign priority for
# the crops included in both the gridded and national inventories. These decisions are documented below.

# The inventory data disaggregate green, blue, and gray water. Gray water indicates downstream pollution; this is not considered
# in GCAM and has not been included in the gridded data processing.
# Blue water is assigned to only irrigated production, but green water applies to both irrigated and rainfed production,
# as the two management technologies are not disaggregated in the inventory data.
# For rainfed crops, total biophysical = green, and for irrigated crops, total biophysical = blue + green.

printlog( "Initial data cleaning" )
L165.WaterFootprint_ctry_aez_crop <- WaterFootprint_ctry_aez_crop
L165.WaterFootprint_ctry_aez_crop$iso <- tolower( L165.WaterFootprint_ctry_aez_crop$Country )

# Romania was "rom" in 2000-era datasets, and Serbia and Montenegro were "scg"
L165.WaterFootprint_ctry_aez_crop$iso[ L165.WaterFootprint_ctry_aez_crop$iso == "rou" ] <- "rom"
L165.WaterFootprint_ctry_aez_crop$iso[ L165.WaterFootprint_ctry_aez_crop$iso %in% c( "srb", "mne" ) ] <- "scg"

#Dropping Western Sahara as it's omitted in the GTAP data (and it's all zeroes here)
L165.WaterFootprint_ctry_aez_crop <- subset( L165.WaterFootprint_ctry_aez_crop, iso != "esh" )

#Name the AEZs according to our conventions elsewhere, and match in the GTAP crop
L165.WaterFootprint_ctry_aez_crop$AEZ <- paste0( "AEZ", sprintf( "%02d", L165.WaterFootprint_ctry_aez_crop$AEZ ) )
L165.WaterFootprint_ctry_aez_crop$GTAP_crop <- FAO_ag_items_PRODSTAT$GTAP_crop[
      match( L165.WaterFootprint_ctry_aez_crop$crop, FAO_ag_items_PRODSTAT$MH_crop ) ]

printlog( "Re-casting nation-level data so that blue and green are represented as data columns" )
L165.Mekonnen_Hoekstra_Rep47_A2 <- melt( Mekonnen_Hoekstra_Rep47_A2, id.vars = c( "FAO_crop", "water_type" ), variable.name = "iso", value.name = "coef_m3t" )
L165.Mekonnen_Hoekstra_Rep47_A2$coef_m3kg <- L165.Mekonnen_Hoekstra_Rep47_A2$coef_m3t * conv_kg_t
L165.Mekonnen_Hoekstra_Rep47_A2 <- dcast( L165.Mekonnen_Hoekstra_Rep47_A2, iso + FAO_crop ~ water_type, value.var = "coef_m3kg" )
#Set all missing values to zero -- this may be re-visited at another point. we may want to substitute default values from the MH2011 table
L165.Mekonnen_Hoekstra_Rep47_A2[ is.na( L165.Mekonnen_Hoekstra_Rep47_A2 ) ] <- 0

printlog( "Building inventory of water use by country, crop, and AEZ for the 18 gridded crops" )
# We give precedence to the aggregated gridded data for the 18 crops that are mapped, and fill in nation-level inventory data
# for the remainder.
L165.ag_Prod_t_ctry_crop_AEZ <- melt( GTAP_ag_Prod_t, measure.vars = AEZs, variable.name = AEZ, value.name = "Prod_t" )
names( L165.ag_Prod_t_ctry_crop_AEZ )[ names( L165.ag_Prod_t_ctry_crop_AEZ ) == "ctry" ] <- "iso"
L165.ag_Water_ctry_MHcrop_AEZ <- L165.WaterFootprint_ctry_aez_crop
L165.ag_Water_ctry_MHcrop_AEZ$Prod_t <- L165.ag_Prod_t_ctry_crop_AEZ$Prod_t[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ),
             vecpaste( L165.ag_Prod_t_ctry_crop_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ) ) ]

printlog( "For the 18 M+H gridded crops, calculate the coefs by country, AEZ, and crop" )
printlog( "NOTE: dropping all country / AEZ / crops with zero production; these won't have any water anyway" )
L165.ag_Water_ctry_MHcrop_AEZ <- subset( L165.ag_Water_ctry_MHcrop_AEZ, Prod_t > 0)
L165.ag_Water_ctry_MHcrop_AEZ[ c( "Blue_m3kg", "Green_m3kg" ) ] <-
      L165.ag_Water_ctry_MHcrop_AEZ[ c( "bl..1000m3.", "gn..1000m3." ) ] /
      L165.ag_Water_ctry_MHcrop_AEZ$Prod_t

printlog( "Clipping extremely high values which are due to statistical discrepancies" )
printlog( "The assumed maximum values are crop specific and based on analysis of the 2011 inventory data" )
# In this step, we're adding 2 m3/kg to every maximum observed value across all national averages. We want to use something a bit
# higher than the maximum of the national averages, because the sub-national regions' values will have a greater range.
# Still, this sort of cap is necessary because many of the water quantities, particularly in small regions,
# are clearly inconsistent with the underlying production data and produce extremely high water demand coefficients.
# In the case of Barley in Iraq it is clearly an error in the M+H gridded inventory, but for others it could be a simple
# mis-match in estimates of crop production by region, and share of irrigated production by region.
ctry_aez_max_adder <- 2
L165.MaxWaterCoefs_m3kg <- aggregate( L165.Mekonnen_Hoekstra_Rep47_A2[ c( "Blue", "Green" ) ] + ctry_aez_max_adder,
      by = L165.Mekonnen_Hoekstra_Rep47_A2[ "FAO_crop" ], max )
L165.MaxWaterCoefs_m3kg$MH_crop <- FAO_ag_items_PRODSTAT$MH_crop[ match( L165.MaxWaterCoefs_m3kg$FAO_crop, FAO_ag_items_PRODSTAT$item ) ]
L165.ag_Water_ctry_MHcrop_AEZ$Blue_m3kg <- pmin(
      L165.ag_Water_ctry_MHcrop_AEZ$Blue_m3kg,
      L165.MaxWaterCoefs_m3kg$Blue[ match( L165.ag_Water_ctry_MHcrop_AEZ$crop, L165.MaxWaterCoefs_m3kg$MH_crop ) ] )
L165.ag_Water_ctry_MHcrop_AEZ$Green_m3kg <- pmin(
      L165.ag_Water_ctry_MHcrop_AEZ$Green_m3kg,
      L165.MaxWaterCoefs_m3kg$Blue[ match( L165.ag_Water_ctry_MHcrop_AEZ$crop, L165.MaxWaterCoefs_m3kg$MH_crop ) ] )

printlog( "Working through calculation sequence to determine blue and green water coefficients for irrigated and rainfed production for gridded crops" )
# The sequence has 5 steps:
#	(1) calculate the total biophysical (green + blue) water quantities and coefficients for each country, AEZ, and crop.
#	(2) Match in the irrigated production and calculate the blue water coef for irrigated as min( blue water quantity / irr prod, total biophysical coef )
#	(3) Calculate the green water coef for irrigated crops as the total biophysical coef minus the blue water coef
#	(4) Calculate the total volume of green water on rainfed crops, as the total green water minus green water used by irrigated crops
#	(5) Calculate the green water coef for rainfed crops as rainfed green water volume divided by rainfed production
L165.ag_Water_ctry_MHcrop_AEZ <- L165.ag_Water_ctry_MHcrop_AEZ[ c( "iso", "GTAP_crop", AEZ, "Prod_t", "Blue_m3kg", "Green_m3kg" ) ]
L165.ag_Water_ctry_MHcrop_AEZ$Total_m3kg <- L165.ag_Water_ctry_MHcrop_AEZ$Blue_m3kg + L165.ag_Water_ctry_MHcrop_AEZ$Green_m3kg
L165.ag_Water_ctry_MHcrop_AEZ$Blue_thousm3 <- L165.ag_Water_ctry_MHcrop_AEZ$Prod_t * L165.ag_Water_ctry_MHcrop_AEZ$Blue_m3kg
L165.ag_Water_ctry_MHcrop_AEZ$Green_thousm3 <- L165.ag_Water_ctry_MHcrop_AEZ$Prod_t * L165.ag_Water_ctry_MHcrop_AEZ$Green_m3kg

printlog( "Matching in the irrigated production to calculate the blue water coef for irrigated crops" )
L165.GTAP_ag_irrProd_t <- melt( L151.GTAP_ag_irrProd_t, measure.vars = AEZs, variable.name = AEZ, value.name = "irrProd_t" )
names( L165.GTAP_ag_irrProd_t )[ names( L165.GTAP_ag_irrProd_t ) == "ctry" ] <- "iso"
L165.ag_Water_ctry_MHcrop_AEZ$irrProd_t <- L165.GTAP_ag_irrProd_t$irrProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ),
             vecpaste( L165.GTAP_ag_irrProd_t[ c( "iso", "GTAP_crop", AEZ ) ] ) ) ]
L165.ag_Water_ctry_MHcrop_AEZ$BlueIrr_m3kg <- L165.ag_Water_ctry_MHcrop_AEZ$Blue_thousm3 / L165.ag_Water_ctry_MHcrop_AEZ$irrProd_t

#Set the coefs to zero wherever irrigated production is zero (these couldn't have any water consumption in GCAM anyway)
L165.ag_Water_ctry_MHcrop_AEZ$BlueIrr_m3kg[ L165.ag_Water_ctry_MHcrop_AEZ$irrProd_t == 0 ] <- 0
printlog( "Capping blue water coefficients at the total biophysical quantity" )
L165.ag_Water_ctry_MHcrop_AEZ$BlueIrr_m3kg <- pmin( L165.ag_Water_ctry_MHcrop_AEZ$BlueIrr_m3kg, L165.ag_Water_ctry_MHcrop_AEZ$Total_m3kg )

printlog( "Calculating the green water coefs of irrigated crops as total biophysical coef minus calculated blue water coef" )
L165.ag_Water_ctry_MHcrop_AEZ$GreenIrr_m3kg <- L165.ag_Water_ctry_MHcrop_AEZ$Total_m3kg - L165.ag_Water_ctry_MHcrop_AEZ$BlueIrr_m3kg

printlog( "Calculating the green water quantities for rainfed crops as total green water minus green water on irrigated" )
L165.ag_Water_ctry_MHcrop_AEZ$GreenIrr_thousm3 <- L165.ag_Water_ctry_MHcrop_AEZ$GreenIrr_m3kg * L165.ag_Water_ctry_MHcrop_AEZ$irrProd_t
L165.ag_Water_ctry_MHcrop_AEZ$GreenRfd_thousm3 <- L165.ag_Water_ctry_MHcrop_AEZ$Green_thousm3 - L165.ag_Water_ctry_MHcrop_AEZ$GreenIrr_thousm3
L165.GTAP_ag_rfdProd_t <- melt( L151.GTAP_ag_rfdProd_t, measure.vars = AEZs, variable.name = AEZ, value.name = "rfdProd_t" )
names( L165.GTAP_ag_rfdProd_t )[ names( L165.GTAP_ag_rfdProd_t ) == "ctry" ] <- "iso"
L165.ag_Water_ctry_MHcrop_AEZ$rfdProd_t <- L165.GTAP_ag_rfdProd_t$rfdProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcrop_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ),
             vecpaste( L165.GTAP_ag_rfdProd_t[ c( "iso", "GTAP_crop", AEZ ) ] ) ) ]

printlog( "Calculating the green water coef on rainfed as green water on rainfed divided by rainfed production" )
L165.ag_Water_ctry_MHcrop_AEZ$GreenRfd_m3kg <- L165.ag_Water_ctry_MHcrop_AEZ$GreenRfd_thousm3 / L165.ag_Water_ctry_MHcrop_AEZ$rfdProd_t
L165.ag_Water_ctry_MHcrop_AEZ$GreenRfd_m3kg[ L165.ag_Water_ctry_MHcrop_AEZ$rfdProd_t == 0 ] <- 0

printlog( "This table needs to be expanded to the more complete list of crops in the MH2011 inventory" )
printlog( "The expanded set only includes crops not already accounted in the gridded inventory" )
printlog( "Increasing the data set in countries and crops in the country-level inventory, and where production is non-zero" )
GTAP_addl_crops <- sort( unique( FAO_ag_items_PRODSTAT$GTAP_crop[ is.na( FAO_ag_items_PRODSTAT$MH_crop ) ] ) )
L165.ag_Water_ctry_MHcropX_AEZ <- subset( L165.ag_Prod_t_ctry_crop_AEZ,
      iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso &
      GTAP_crop %in% GTAP_addl_crops &
      Prod_t > 0 )

# We don't want to multiply each AEZ's total production by the nation-level blue and green coefs, as this will do a poor job of
# allocating blue water to the AEZs with the most irrigated production. The method followed here is similar to the method followed above for
# gridded crops, but here it is performed without any AEZ-level detail.
printlog( "Dropping the AEZ from the first stage of calculations for the additional country-level crops" )
L165.ag_Water_ctry_MHcropX <- aggregate( L165.ag_Water_ctry_MHcropX_AEZ[ "Prod_t" ],
      by = L165.ag_Water_ctry_MHcropX_AEZ[ c( "iso", "GTAP_crop" ) ], sum )
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
L165.ag_Water_ctry_MHcropX[ c( "Blue_m3kg", "Green_m3kg" ) ] <- L165.Mekonnen_Hoekstra_Rep47_A2[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "FAO_crop" ) ] ),
             vecpaste( L165.Mekonnen_Hoekstra_Rep47_A2[ c( "iso", "FAO_crop" ) ] ) ),
      c( "Blue", "Green" ) ]
L165.ag_Water_ctry_MHcropX <- na.omit( L165.ag_Water_ctry_MHcropX )

printlog( "The next steps follow the 5-step method described above for assigning blue and green water to irrigated and rainfed production" )
L165.ag_Water_ctry_MHcropX$Total_m3kg <- L165.ag_Water_ctry_MHcropX$Blue_m3kg + L165.ag_Water_ctry_MHcropX$Green_m3kg
L165.ag_Water_ctry_MHcropX$Blue_thousm3 <- L165.ag_Water_ctry_MHcropX$Prod_t * L165.ag_Water_ctry_MHcropX$Blue_m3kg
L165.ag_Water_ctry_MHcropX$Green_thousm3 <- L165.ag_Water_ctry_MHcropX$Prod_t * L165.ag_Water_ctry_MHcropX$Green_m3kg

printlog( "Matching in the irrigated production to calculate the blue water coef for irrigated crops" )
L165.irrProd_t_ctry_crop <- aggregate( L165.GTAP_ag_irrProd_t[ "irrProd_t"], by = L165.GTAP_ag_irrProd_t[ c( "iso", "GTAP_crop" ) ], sum )
L165.ag_Water_ctry_MHcropX$irrProd_t <- L165.irrProd_t_ctry_crop$irrProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ),
             vecpaste( L165.irrProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg <- L165.ag_Water_ctry_MHcropX$Blue_thousm3 / L165.ag_Water_ctry_MHcropX$irrProd_t
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg[ L165.ag_Water_ctry_MHcropX$irrProd_t == 0 ] <- 0
L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg <- pmin( L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg, L165.ag_Water_ctry_MHcropX$Total_m3kg )

printlog( "Calculating the green water coefs of irrigated crops as total biophysical coef minus calculated blue water coef" )
L165.ag_Water_ctry_MHcropX$GreenIrr_m3kg <- L165.ag_Water_ctry_MHcropX$Total_m3kg - L165.ag_Water_ctry_MHcropX$BlueIrr_m3kg

printlog( "Calculating the green water coefs of rainfed crops as total green water minus green water on irrigated" )
L165.ag_Water_ctry_MHcropX$GreenIrr_thousm3 <- L165.ag_Water_ctry_MHcropX$GreenIrr_m3kg * L165.ag_Water_ctry_MHcropX$irrProd_t
L165.ag_Water_ctry_MHcropX$GreenRfd_thousm3 <- L165.ag_Water_ctry_MHcropX$Green_thousm3 - L165.ag_Water_ctry_MHcropX$GreenIrr_thousm3
L165.rfdProd_t_ctry_crop <- aggregate( L165.GTAP_ag_rfdProd_t[ "rfdProd_t"], by = L165.GTAP_ag_rfdProd_t[ c( "iso", "GTAP_crop" ) ], sum )
L165.ag_Water_ctry_MHcropX$rfdProd_t <- L165.rfdProd_t_ctry_crop$rfdProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ),
             vecpaste( L165.rfdProd_t_ctry_crop[ c( "iso", "GTAP_crop" ) ] ) ) ]
L165.ag_Water_ctry_MHcropX$GreenRfd_m3kg <- L165.ag_Water_ctry_MHcropX$GreenRfd_thousm3 / L165.ag_Water_ctry_MHcropX$rfdProd_t
L165.ag_Water_ctry_MHcropX$GreenRfd_m3kg[ L165.ag_Water_ctry_MHcropX$rfdProd_t == 0 ] <- 0

printlog( "The grid-based and nation-based tables of green and blue water coefs and volumes for irrigated and rainfed crops..." )
printlog( "...can now be combined to provide estimates of green and blue water coefs for all available countries, crops, and AEZs" )
# First we need to write out the MH2014 coefficients to all applicable AEZs
L165.ag_Water_ctry_MHcropX_AEZ <- subset( L165.ag_Prod_t_ctry_crop_AEZ,
      iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso &
      GTAP_crop %in% L165.ag_Water_ctry_MHcropX$GTAP_crop &
      Prod_t > 0 )[ c( "iso", "GTAP_crop", AEZ ) ]

L165.ag_Water_ctry_MHcropX_AEZ$irrProd_t <-L165.GTAP_ag_irrProd_t$irrProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcropX_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ),
             vecpaste( L165.GTAP_ag_irrProd_t[ c( "iso", "GTAP_crop", AEZ ) ] ) ) ]
L165.ag_Water_ctry_MHcropX_AEZ$rfdProd_t <-L165.GTAP_ag_rfdProd_t$rfdProd_t[
      match( vecpaste( L165.ag_Water_ctry_MHcropX_AEZ[ c( "iso", "GTAP_crop", AEZ ) ] ),
             vecpaste( L165.GTAP_ag_rfdProd_t[ c( "iso", "GTAP_crop", AEZ ) ] ) ) ]
L165.ag_Water_ctry_MHcropX_AEZ[ c( "BlueIrr_m3kg", "GreenIrr_m3kg", "GreenRfd_m3kg" ) ] <-
      L165.ag_Water_ctry_MHcropX[
        match( vecpaste( L165.ag_Water_ctry_MHcropX_AEZ[ c( "iso", "GTAP_crop" ) ] ),
               vecpaste( L165.ag_Water_ctry_MHcropX[ c( "iso", "GTAP_crop" ) ] ) ),
        c( "BlueIrr_m3kg", "GreenIrr_m3kg", "GreenRfd_m3kg" ) ]

# Then combine with the grid-based estimates
L165.ag_Water_ctry_crop_AEZ <- rbind( L165.ag_Water_ctry_MHcrop_AEZ[ names( L165.ag_Water_ctry_MHcropX_AEZ ) ], L165.ag_Water_ctry_MHcropX_AEZ )

printlog( "Calculating green and blue water quantities by all available countries, crops, AEZs, and irrigated/rainfed" )
L165.ag_Water_ctry_crop_AEZ$BlueIrr_thousm3 <- L165.ag_Water_ctry_crop_AEZ$BlueIrr_m3kg * L165.ag_Water_ctry_crop_AEZ$irrProd_t
L165.ag_Water_ctry_crop_AEZ$GreenIrr_thousm3 <- L165.ag_Water_ctry_crop_AEZ$GreenIrr_m3kg * L165.ag_Water_ctry_crop_AEZ$irrProd_t
L165.ag_Water_ctry_crop_AEZ$GreenRfd_thousm3 <- L165.ag_Water_ctry_crop_AEZ$GreenRfd_m3kg * L165.ag_Water_ctry_crop_AEZ$rfdProd_t

printlog( "Matching in GCAM regions and commodities for aggregation" )
L165.ag_Water_ctry_crop_AEZ[[R]] <- iso_GCAM_regID[[R]][ match( L165.ag_Water_ctry_crop_AEZ$iso, iso_GCAM_regID$iso ) ]
L165.ag_Water_ctry_crop_AEZ[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L165.ag_Water_ctry_crop_AEZ$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

printlog( "NOTE: Fodder crops are not in the M+H inventories. They instead have been assigned a corresponding crop that is in the M+H inventories..." )
printlog( "... and at this point are multiplied by an exogenous fodder:crop adjustment factor" )
# This fodder:crop adjustment factor is designed to reflect that fodder crop masses include the whole plant, often harvested green (~70% water).
# As such, their water coefficients will tend to be a good deal lower than the corresponding crop to which they are assigned.
# The specific number is based on analysis of a virtual water content dataset (Chapagain 2004) that included fodder crops
# The "corresponding" crops are assigned in the mapping file (FAO_ag_items_PRODSTAT), based on species (e.g., maize) or family (e.g., alfalfa)
conv_crop_fodder <- 0.2
L165.ag_Water_ctry_crop_AEZ[
      L165.ag_Water_ctry_crop_AEZ[[C]] %in% c( "FodderGrass", "FodderHerb" ), c( "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ] <-
   L165.ag_Water_ctry_crop_AEZ[
      L165.ag_Water_ctry_crop_AEZ[[C]] %in% c( "FodderGrass", "FodderHerb" ), c( "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ] *
      conv_crop_fodder

printlog( "At this point, the crops are ready for aggregation by GCAM region and commodity" )
L165.ag_Water_R_C_AEZ <- aggregate( L165.ag_Water_ctry_crop_AEZ[ c( "irrProd_t", "rfdProd_t", "BlueIrr_thousm3", "GreenIrr_thousm3", "GreenRfd_thousm3" ) ],
      by = L165.ag_Water_ctry_crop_AEZ[ R_C_AEZ ], sum )
L165.ag_Water_R_C_AEZ$BlueIrr_m3kg <- L165.ag_Water_R_C_AEZ$BlueIrr_thousm3 / L165.ag_Water_R_C_AEZ$irrProd_t
L165.ag_Water_R_C_AEZ$GreenIrr_m3kg <- L165.ag_Water_R_C_AEZ$GreenIrr_thousm3 / L165.ag_Water_R_C_AEZ$irrProd_t
L165.ag_Water_R_C_AEZ$GreenRfd_m3kg <- L165.ag_Water_R_C_AEZ$GreenRfd_thousm3 / L165.ag_Water_R_C_AEZ$rfdProd_t
L165.ag_Water_R_C_AEZ$TotIrr_m3kg <- L165.ag_Water_R_C_AEZ$BlueIrr_m3kg + L165.ag_Water_R_C_AEZ$GreenIrr_m3kg

printlog( "Casting by AEZ to create the final tables to be written out" )
L165.BlueIrr_m3kg_R_C_AEZ <- dcast( L165.ag_Water_R_C_AEZ, GCAM_region_ID + GCAM_commodity ~ AEZ, value.var = "BlueIrr_m3kg" )
L165.TotIrr_m3kg_R_C_AEZ <- dcast( L165.ag_Water_R_C_AEZ, GCAM_region_ID + GCAM_commodity ~ AEZ, value.var = "TotIrr_m3kg" )
L165.GreenRfd_m3kg_R_C_AEZ <- dcast( L165.ag_Water_R_C_AEZ, GCAM_region_ID + GCAM_commodity ~ AEZ, value.var = "GreenRfd_m3kg" )

L165.BlueIrr_m3kg_R_C_AEZ[ is.na( L165.BlueIrr_m3kg_R_C_AEZ ) ] <- 0
L165.TotIrr_m3kg_R_C_AEZ[ is.na( L165.TotIrr_m3kg_R_C_AEZ ) ] <- 0
L165.GreenRfd_m3kg_R_C_AEZ[ is.na( L165.GreenRfd_m3kg_R_C_AEZ ) ] <- 0

printlog( "Irrigation Efficiency: Compile country-level data to GCAM regions, weighted by total irrigated harvested area" )
printlog( "NOTE: Using application and management efficiencies to define whole-system efficiencies (not conveyance)")
Rohwer_2007_IrrigationEff$field.eff <- Rohwer_2007_IrrigationEff$application.eff * Rohwer_2007_IrrigationEff$management.eff
L165.ag_irrHA_ha_ctry <- data.frame(
      iso = L151.GTAP_ag_irrHA_ha$ctry,
      HA_ha = rowSums( L151.GTAP_ag_irrHA_ha[ AEZs ] ) )
L165.ag_irrHA_ha_ctry[ c( "field.eff", "conveyance.eff" ) ] <- Rohwer_2007_IrrigationEff[
      match( L165.ag_irrHA_ha_ctry$iso, Rohwer_2007_IrrigationEff$iso ),
      c( "field.eff", "conveyance.eff" ) ]
L165.ag_irrHA_ha_ctry <- na.omit( subset( L165.ag_irrHA_ha_ctry, HA_ha > 0 ) )
L165.ag_irrHA_ha_ctry[ c( "field.eff.wtd", "conveyance.eff.wtd" ) ] <-
      L165.ag_irrHA_ha_ctry[ c( "field.eff", "conveyance.eff" ) ] * 
      L165.ag_irrHA_ha_ctry$HA_ha
L165.ag_irrHA_ha_ctry[[R]] <- iso_GCAM_regID[[R]][ match( L165.ag_irrHA_ha_ctry$iso, iso_GCAM_regID$iso ) ]
L165.ag_IrrEff_R <- aggregate( L165.ag_irrHA_ha_ctry[ c( "field.eff.wtd", "conveyance.eff.wtd", "HA_ha" ) ],
      by = L165.ag_irrHA_ha_ctry[ R ], sum )
L165.ag_IrrEff_R[ c( "field.eff", "conveyance.eff" ) ] <-
      L165.ag_IrrEff_R[ c( "field.eff.wtd", "conveyance.eff.wtd" ) ] /
      L165.ag_IrrEff_R$HA_ha
L165.ag_IrrEff_R <- L165.ag_IrrEff_R[ c( R, "field.eff", "conveyance.eff" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L165.BlueIrr_m3kg_R_C_AEZ <- c( "Blue water consumption coefficients for irrigated crops by GCAM region / commodity / AEZ","Unit = m3 / kg" )
comments.L165.TotIrr_m3kg_R_C_AEZ <- c( "Total biophysical water consumption coefficients for irrigated crops by GCAM region / commodity / AEZ","Unit = m3 / kg" )
comments.L165.GreenRfd_m3kg_R_C_AEZ <- c( "Green water consumption coefficients for rainfed crops by GCAM region / commodity / AEZ","Unit = m3 / kg" )
comments.L165.ag_IrrEff_R <- c( "Irrigation efficiency by GCAM region","Unitless efficiency" )

#export final tables as CSV files
writedata( L165.BlueIrr_m3kg_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.BlueIrr_m3kg_R_C_AEZ", comments=comments.L165.BlueIrr_m3kg_R_C_AEZ )
writedata( L165.TotIrr_m3kg_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.TotIrr_m3kg_R_C_AEZ", comments=comments.L165.TotIrr_m3kg_R_C_AEZ )
writedata( L165.GreenRfd_m3kg_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.GreenRfd_m3kg_R_C_AEZ", comments=comments.L165.GreenRfd_m3kg_R_C_AEZ )
writedata( L165.ag_IrrEff_R, domain="AGLU_LEVEL1_DATA",fn="L165.ag_IrrEff_R", comments=comments.L165.ag_IrrEff_R )

# Every script should finish with this line
logstop()
            
