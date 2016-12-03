#This file builds all tables necessarily for generating the model inputs for water
# Agricultural component
#  Water coefficients (km3 / Mt crop) for all regions, crops, GLUs, and years

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

#This file uses functions in the water module
if( !exists( "WATERPROC_DIR" ) ){
  if( Sys.getenv( "WATERPROC" ) != "" ){
    WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
  } else {
    stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L2071.ag_water_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for water withdrawals and consumption" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU", replace_GLU = T )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU", replace_GLU = T )
L161.ag_irrYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrYield_kgm2_R_C_Y_GLU", replace_GLU = T )
L165.BlueIrr_m3kg_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L165.BlueIrr_m3kg_R_C_GLU", replace_GLU = T )
L165.TotIrr_m3kg_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L165.TotIrr_m3kg_R_C_GLU", replace_GLU = T )
L165.GreenRfd_m3kg_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L165.GreenRfd_m3kg_R_C_GLU", replace_GLU = T )
L165.ag_IrrEff_R <- readdata( "AGLU_LEVEL1_DATA", "L165.ag_IrrEff_R" )
L2051.AgCost_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_ag_irr", skip = 4 )
L2051.AgCost_bio_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_bio_irr", skip = 4 )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )

# -----------------------------------------------------------------------------
# 2. Build tables
# Agricultural component
printlog( "Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out" )
L165.BlueIrr_m3kg_R_C_GLU <- L165.BlueIrr_m3kg_R_C_GLU[
  vecpaste( L165.BlueIrr_m3kg_R_C_GLU[ R_C_GLU ] ) %in% vecpaste( L161.ag_irrProd_Mt_R_C_Y_GLU[ R_C_GLU ] ), ]
L165.TotIrr_m3kg_R_C_GLU <- L165.TotIrr_m3kg_R_C_GLU[
  vecpaste( L165.TotIrr_m3kg_R_C_GLU[ R_C_GLU ] ) %in% vecpaste( L161.ag_irrProd_Mt_R_C_Y_GLU[ R_C_GLU ] ), ]
L165.GreenRfd_m3kg_R_C_GLU <- L165.GreenRfd_m3kg_R_C_GLU[
  vecpaste( L165.GreenRfd_m3kg_R_C_GLU[ R_C_GLU ] ) %in% vecpaste( L161.ag_rfdProd_Mt_R_C_Y_GLU[ R_C_GLU ] ), ]

#  Water consumption IO coefficients (km3 / Mt crop) for all regions, crops, GLUs, and years
printlog( "Table L2071.AgCoef_IrrWaterCons_ag: Irrigation water consumption IO coefficients by region / crop / year / GLU" )
L2071.Blue_IRR_IO_R_C_GLU <- add_region_name( L165.BlueIrr_m3kg_R_C_GLU )
L2071.Blue_IRR_IO_R_C_GLU[[irr]] <- "IRR"
L2071.Blue_IRR_IO_R_C_GLU <- add_agtech_names( L2071.Blue_IRR_IO_R_C_GLU )
L2071.Blue_IRR_IO_R_C_GLU[[input]] <- Irr_Cons_name

L2071.AgCoef_IrrWaterCons_ag <- repeat_and_add_vector( L2071.Blue_IRR_IO_R_C_GLU, Y, model_years )
L2071.AgCoef_IrrWaterCons_ag$coefficient <- round( L2071.AgCoef_IrrWaterCons_ag$BlueIrr_m3kg, digits_calOutput )
L2071.AgCoef_IrrWaterCons_ag <- L2071.AgCoef_IrrWaterCons_ag[ names_AgCoef ]
L2071.AgCoef_IrrWaterCons_ag <- subset( L2071.AgCoef_IrrWaterCons_ag, coefficient > 0 )

printlog( "Table L2071.AgCoef_IrrBphysWater_ag: Biophysical water consumption IO coefficients by region / irrigated crop / year / GLU" )
L2071.Bio_IRR_IO_R_C_GLU <- add_region_name( L165.TotIrr_m3kg_R_C_GLU )
L2071.Bio_IRR_IO_R_C_GLU[[irr]] <- "IRR"
L2071.Bio_IRR_IO_R_C_GLU <- add_agtech_names( L2071.Bio_IRR_IO_R_C_GLU )
L2071.Bio_IRR_IO_R_C_GLU[[input]] <- Bio_Cons_name

L2071.AgCoef_IrrBphysWater_ag <- repeat_and_add_vector( L2071.Bio_IRR_IO_R_C_GLU, Y, model_years )
L2071.AgCoef_IrrBphysWater_ag$coefficient <- round( L2071.AgCoef_IrrBphysWater_ag$TotIrr_m3kg, digits_calOutput )
L2071.AgCoef_IrrBphysWater_ag <- L2071.AgCoef_IrrBphysWater_ag[ names_AgCoef ]
L2071.AgCoef_IrrBphysWater_ag <- subset( L2071.AgCoef_IrrBphysWater_ag, coefficient > 0 )

printlog( "Table L2071.AgCoef_RfdBphysWater_ag: Biophysical water consumption IO coefficients by region / rainfed crop / year / GLU" )
L2071.Bio_RFD_IO_R_C_GLU <- add_region_name( L165.GreenRfd_m3kg_R_C_GLU )
L2071.Bio_RFD_IO_R_C_GLU[[irr]] <- "RFD"
L2071.Bio_RFD_IO_R_C_GLU <- add_agtech_names( L2071.Bio_RFD_IO_R_C_GLU )
L2071.Bio_RFD_IO_R_C_GLU[[input]] <- Bio_Cons_name

L2071.AgCoef_RfdBphysWater_ag <- repeat_and_add_vector( L2071.Bio_RFD_IO_R_C_GLU, Y, model_years )
L2071.AgCoef_RfdBphysWater_ag$coefficient <- round( L2071.AgCoef_RfdBphysWater_ag$GreenRfd_m3kg, digits_calOutput )
L2071.AgCoef_RfdBphysWater_ag <- L2071.AgCoef_RfdBphysWater_ag[ names_AgCoef ]
L2071.AgCoef_RfdBphysWater_ag <- subset( L2071.AgCoef_RfdBphysWater_ag, coefficient > 0 )

# Create table for water withdrawals (consumption divided by irrigation efficiency)
L2071.IrrigationEfficiency_R <- add_region_name( L165.ag_IrrEff_R )

L2071.AgCoef_IrrWaterWdraw_ag <- L2071.AgCoef_IrrWaterCons_ag
L2071.AgCoef_IrrWaterWdraw_ag$coefficient <- round(
  L2071.AgCoef_IrrWaterCons_ag$coefficient /
      L2071.IrrigationEfficiency_R$field.eff[
         match( L2071.AgCoef_IrrWaterWdraw_ag[[reg]],
                L2071.IrrigationEfficiency_R[[reg]] ) ],
      digits_calOutput )
L2071.AgCoef_IrrWaterWdraw_ag$minicam.energy.input <- Irr_W_name
       
#  Water IO coefficients for dedicated bioenergy crops (km3 / EJ biomass)
printlog( "Table L2071.AgCoef_BphysWater_bio: Biophysical water input-output coefficients by region / dedicated bioenergy crop / year / GLU" )
L2071.AgCoef_BphysWater_bio <- L2051.AgCost_bio_irr[ names_AgTechYr ]
L2071.AgCoef_BphysWater_bio[[input]] <- Bio_Cons_name
L2071.AgCoef_BphysWater_bio$coefficient <- bio_grass_Water_IO_km3EJ
L2071.AgCoef_BphysWater_bio$coefficient[ grepl( bio_tree_name, L2071.AgCoef_BphysWater_bio[[agtech]] ) ] <- bio_tree_Water_IO_km3EJ

printlog( "Compute blue water for irrigated bioenergy" )
#Match in green and blue water for existing crops in 2005 -- note for this we are only using blue & green from irrigated crops
#Compute % of blue water for irrigated
L2071.BlueFract_R_C_GLU <- merge( L165.BlueIrr_m3kg_R_C_GLU, L165.TotIrr_m3kg_R_C_GLU )
L2071.BlueFract_R_C_GLU$Prod_Mt <- L161.ag_irrProd_Mt_R_C_Y_GLU[[X_final_historical_year]][
  match( vecpaste( L2071.BlueFract_R_C_GLU[ R_C_GLU ] ),
         vecpaste( L161.ag_irrProd_Mt_R_C_Y_GLU[ R_C_GLU ] ) ) ]
L2071.BlueFract_R_C_GLU[ c( "BlueIrr_km3", "TotIrr_km3" ) ] <- L2071.BlueFract_R_C_GLU[ c( "BlueIrr_m3kg", "TotIrr_m3kg" ) ] * L2071.BlueFract_R_C_GLU$Prod_Mt
L2071.BlueFract_R_GLU <- aggregate( L2071.BlueFract_R_C_GLU[ c( "Prod_Mt", "BlueIrr_km3", "TotIrr_km3" ) ],
                                    by = L2071.BlueFract_R_C_GLU[ c( R_GLU ) ], sum )
L2071.BlueFract_R_GLU[ c( "BlueIrr_m3kg", "TotIrr_m3kg" ) ] <- L2071.BlueFract_R_GLU[ c( "BlueIrr_km3", "TotIrr_km3" ) ] / L2071.BlueFract_R_GLU$Prod_Mt
L2071.BlueFract_R_GLU$blue_fract <- with( L2071.BlueFract_R_GLU, BlueIrr_m3kg / TotIrr_m3kg )
L2071.BlueFract_R_GLU$blue_fract[ is.na( L2071.BlueFract_R_GLU$blue_fract ) ] <- 0
L2071.BlueFract_R_GLU <- add_region_name( L2071.BlueFract_R_GLU )
L2071.BlueFract_R_GLU <- repeat_and_add_vector( L2071.BlueFract_R_GLU, "year", model_years )
 
#Create table for model input  
L2071.AgCoef_IrrWaterCons_bio <- L2071.AgCoef_BphysWater_bio
L2071.AgCoef_IrrWaterCons_bio[[input]] <- Irr_Cons_name
L2071.AgCoef_IrrWaterCons_bio <- substring_GLU( L2071.AgCoef_IrrWaterCons_bio, agsubs )
L2071.AgCoef_IrrWaterCons_bio$coefficient <- round(
  L2071.AgCoef_BphysWater_bio$coefficient * L2071.BlueFract_R_GLU$blue_fract[
      match( vecpaste(L2071.AgCoef_IrrWaterCons_bio[ c( reg, GLU ) ] ),
             vecpaste(L2071.BlueFract_R_GLU[ c( reg, GLU ) ] ) ) ],
  digits_calOutput )
L2071.AgCoef_IrrWaterCons_bio$coefficient[ is.na( L2071.AgCoef_IrrWaterCons_bio$coefficient ) ] <- 0

#Remove extra columns
L2071.AgCoef_IrrWaterCons_bio <- L2071.AgCoef_IrrWaterCons_bio[ names_AgCoef ] 

# Create table for water withdrawals
L2071.AgCoef_IrrWaterWdraw_bio <- L2071.AgCoef_IrrWaterCons_bio
L2071.AgCoef_IrrWaterWdraw_bio[[input]] <- Irr_W_name
L2071.AgCoef_IrrWaterWdraw_bio$coefficient <- L2071.AgCoef_IrrWaterCons_bio$coefficient / L2071.IrrigationEfficiency_R$field.eff[
      match( L2071.AgCoef_IrrWaterWdraw_bio[[reg]],
             L2071.IrrigationEfficiency_R[[reg]] ) ]

printlog( "Ad hoc adjustment to water coefficients so that none of the region/GLU/crops have negative profit" )
# In GCAM, the profit rate is calculated as price minus cost times yield, so if the cost exceeds the price, then the profit goes negative.
# By adding in the water cost without modifying the non-land variable costs, we risk having costs that exceed commodity prices, which will
# cause solution/calibration failure in base years, and zero share in future years. This calculation checks whether any of our costs exceed
# the prices, and reduces water withdrawal coefficients where necessary.
# Note that the default unlimited water price is divided by the conveyance efficiency in order to replicate the prices in the model
L2071.AgCoef_IrrWaterWdraw_ag$WaterPrice <- DEFAULT_UNLIMITED_IRR_WATER_PRICE / L2071.IrrigationEfficiency_R$conveyance.eff[
  match( L2071.AgCoef_IrrWaterWdraw_ag$region, L2071.IrrigationEfficiency_R$region ) ]
L2071.AgCoef_IrrWaterWdraw_ag$WaterCost <- with( L2071.AgCoef_IrrWaterWdraw_ag, coefficient * WaterPrice )
L2071.AgCoef_IrrWaterWdraw_ag$NonLandCost <- L2051.AgCost_ag_irr$nonLandVariableCost[
      match( vecpaste( L2071.AgCoef_IrrWaterWdraw_ag[ c( reg, agtech, Y ) ] ),
             vecpaste( L2051.AgCost_ag_irr[ c( reg, agtech, Y ) ] ) ) ]
L2071.AgCoef_IrrWaterWdraw_ag$calPrice <- L132.ag_an_For_Prices$calPrice[
      match( L2071.AgCoef_IrrWaterWdraw_ag[[ agsupp ]], L132.ag_an_For_Prices[[C]] ) ]
L2071.AgCoef_IrrWaterWdraw_ag$Profit <- with( L2071.AgCoef_IrrWaterWdraw_ag, calPrice - WaterCost - NonLandCost)

printlog( "Assuming an exogenous floor on profit rates to prevent negative, zero, and very low profit rates" )
# For the model, low profit rates are fine as long as it's not the dominant crop in a nest where bioenergy is allowed
minProfit <- min( with( L2071.AgCoef_IrrWaterWdraw_ag, calPrice - NonLandCost ) ) / 2
L2071.AgCoef_IrrWaterWdraw_ag$coefficient <- with( L2071.AgCoef_IrrWaterWdraw_ag,
      round( pmin( coefficient, ( calPrice - NonLandCost - minProfit ) / WaterPrice ), digits_calOutput ) )

#Go ahead and print out the number of values being changed in each year
tmp1 <- subset( L2071.AgCoef_IrrWaterWdraw_ag, year == 2010 )
tmp2 <- subset( tmp1, Profit < minProfit )
printlog( "Out of", nrow( tmp1 ), "observations,", nrow(tmp2 ), "had water coefficients reduced to keep positive profit rates")

#As a last step, remove the unnecessary columns
L2071.AgCoef_IrrWaterWdraw_ag <- L2071.AgCoef_IrrWaterWdraw_ag[ names_AgCoef ]

printlog( "Map the water inputs to the appropraite water mapping sector." )

# A helper wrapper to help get the data in/out in a way that is usable by the
# get_water_inputs_for_mapping utility method.
do_water_rename <- function(d) {
  d[[water_sector]] <- "Irrigation"
  d <- substring_GLU( d, from.var = agsubs )
  d$minicam.energy.input <- get_water_inputs_for_mapping( d, A03.sector, water_type.col="minicam.energy.input" )
  d[[GLU]] <- NULL
  d[[water_sector]] <- NULL
  return(d)
}

L2071.AgCoef_IrrBphysWater_ag <- do_water_rename( L2071.AgCoef_IrrBphysWater_ag )
L2071.AgCoef_IrrWaterWdraw_ag <- do_water_rename( L2071.AgCoef_IrrWaterWdraw_ag )
L2071.AgCoef_IrrWaterCons_ag <- do_water_rename( L2071.AgCoef_IrrWaterCons_ag )
L2071.AgCoef_RfdBphysWater_ag <- do_water_rename( L2071.AgCoef_RfdBphysWater_ag )
L2071.AgCoef_BphysWater_bio <- do_water_rename( L2071.AgCoef_BphysWater_bio )
L2071.AgCoef_IrrWaterWdraw_bio <- do_water_rename( L2071.AgCoef_IrrWaterWdraw_bio )
L2071.AgCoef_IrrWaterCons_bio <- do_water_rename( L2071.AgCoef_IrrWaterCons_bio )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L2071.AgCoef_IrrBphysWater_ag, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L2071.AgCoef_IrrBphysWater_ag",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_IrrWaterWdraw_ag, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_IrrWaterWdraw_ag", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_IrrWaterCons_ag, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_IrrWaterCons_ag", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_RfdBphysWater_ag, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_RfdBphysWater_ag", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_BphysWater_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_BphysWater_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_IrrWaterWdraw_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_IrrWaterWdraw_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L2071.AgCoef_IrrWaterCons_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L2071.AgCoef_IrrWaterCons_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml", "AGLU_XML_FINAL", "ag_water_input_IRR.xml", "", xml_tag="outFile" )

logstop()
