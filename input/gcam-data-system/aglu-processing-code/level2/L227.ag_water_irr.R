#This file builds all tables necessarily for generating the model inputs for water
# Agricultural component
#  Water coefficients (km3 / Mt crop) for all regions, crops, AEZs, and years

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
source(paste(AGLUPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L227.ag_water_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
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
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
L161.ag_irrYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrYield_kgm2_R_C_Y_AEZ" )
L165.BlueIrr_m3kg_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L165.BlueIrr_m3kg_R_C_AEZ" )
L165.TotIrr_m3kg_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L165.TotIrr_m3kg_R_C_AEZ" )
L165.GreenRfd_m3kg_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L165.GreenRfd_m3kg_R_C_AEZ" )
L165.ag_IrrEff_R <- readdata( "AGLU_LEVEL1_DATA", "L165.ag_IrrEff_R" )
L225.AgCost_ag <- readdata( "AGLU_LEVEL2_DATA", "L225.AgCost_ag", skip = 4 )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )

# -----------------------------------------------------------------------------
# 2. Build tables
#Build base tables for agricultural water tables
printlog( "Building template tables for reading in water inputs" )
L227.Region_Commodity <- data.frame(
      region = rep( GCAM_region_names$region, times = length( unique( L161.ag_irrProd_Mt_R_C_Y_AEZ$GCAM_commodity ) ) ),
      GCAM_commodity = sort( rep( unique( L161.ag_irrProd_Mt_R_C_Y_AEZ$GCAM_commodity ),
                       times = length( GCAM_region_names$region ) ) ) )
L227.Region_Commodity_AEZ <- repeat_and_add_vector( L227.Region_Commodity, "AEZ", AEZs )
#Region by commodity by AEZ by base years
L227.Region_Commodity_Yby_AEZ <- repeat_and_add_vector( L227.Region_Commodity_AEZ, "year", model_base_years )
#Region by commodity by AEZ by future years
L227.Region_Commodity_Yfy_AEZ <- repeat_and_add_vector( L227.Region_Commodity_AEZ, "year", model_future_years )
#Region by commodity by AEZ by all years
L227.Region_Commodity_Y_AEZ <- rbind( L227.Region_Commodity_Yby_AEZ, L227.Region_Commodity_Yfy_AEZ )

#Match in region names to L1 tables
#L165.irrCons_km3Mt_R_C_AEZ <- add_region_name( L165.irrCons_km3Mt_R_C_AEZ )
#L165.bioPhysCons_km3Mt_R_C_AEZ <- add_region_name(  L165.bioPhysCons_km3Mt_R_C_AEZ )

#Water IO by AEZ by all years
L227.Blue_IRR_IO_R_C_Y_AEZ <- repeat_and_add_vector( L165.BlueIrr_m3kg_R_C_AEZ, "year", model_years )
L227.Bio_IRR_IO_R_C_Y_AEZ <- repeat_and_add_vector( L165.TotIrr_m3kg_R_C_AEZ, "year", model_years )
L227.Bio_RFD_IO_R_C_Y_AEZ <- repeat_and_add_vector( L165.GreenRfd_m3kg_R_C_AEZ, "year", model_years )

# Agricultural component
#  Water IO coefficients (km3 / Mt crop) for all regions, crops, AEZs, and years
printlog( "Table L227.AgCoef_Water: Water input-output coefficients by region / crop / year / AEZ" )
#Melt water IO coef tables, and round to specified number of digits
L227.Blue_IRR_IO_R_C_Y_AEZ.melt <- melt( L227.Blue_IRR_IO_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = AEZ )
L227.Blue_IRR_IO_R_C_Y_AEZ.melt <- add_region_name( L227.Blue_IRR_IO_R_C_Y_AEZ.melt )

L227.Bio_IRR_IO_R_C_Y_AEZ.melt <- melt( L227.Bio_IRR_IO_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = AEZ )
L227.Bio_IRR_IO_R_C_Y_AEZ.melt <- add_region_name( L227.Bio_IRR_IO_R_C_Y_AEZ.melt )

L227.Bio_RFD_IO_R_C_Y_AEZ.melt <- melt( L227.Bio_RFD_IO_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = AEZ )
L227.Bio_RFD_IO_R_C_Y_AEZ.melt <- add_region_name( L227.Bio_RFD_IO_R_C_Y_AEZ.melt )

#Base years
#Table L227.AgCoef_IrrWater
L227.AgCoef_Irr_Water_by <- data.frame( region = L227.Region_Commodity_Yby_AEZ$region,
      AgSupplySector = L227.Region_Commodity_Yby_AEZ$GCAM_commodity,
      AgSupplySubsector = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, sep = AEZ_delimiter ),
      AgProductionTechnology = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, "IRR", sep = AEZ_delimiter ),
      year = L227.Region_Commodity_Yby_AEZ$year,
      minicam.energy.input = Irr_Cons_name,
      coefficient = round(
         L227.Blue_IRR_IO_R_C_Y_AEZ.melt$value[
            match( vecpaste( L227.Region_Commodity_Yby_AEZ[ c( reg, C, AEZ, Y ) ] ),
                vecpaste( L227.Blue_IRR_IO_R_C_Y_AEZ.melt[ c( reg, C, AEZ, Y ) ] ) ) ],
         digits_calOutput ),
      stringsAsFactors = F )
# Note that the R_C_AEZ tables don't have all non-applicable combinations written out, so missing values are generated
# These are removed later on
                    
#Table L227.AgCoef_BioWater for irrigated crops
L227.AgCoef_BioIrr_Water_by <- data.frame( region = L227.Region_Commodity_Yby_AEZ$region,
      AgSupplySector = L227.Region_Commodity_Yby_AEZ$GCAM_commodity,
      AgSupplySubsector = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, sep = AEZ_delimiter ),
      AgProductionTechnology = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, "IRR", sep = AEZ_delimiter ),
      year = L227.Region_Commodity_Yby_AEZ$year,
      minicam.energy.input = Bio_Cons_name,
      coefficient = round(
         L227.Bio_IRR_IO_R_C_Y_AEZ.melt$value[
            match( vecpaste( L227.Region_Commodity_Yby_AEZ[ c( reg, C, AEZ, Y ) ] ),
                   vecpaste( L227.Bio_IRR_IO_R_C_Y_AEZ.melt[ c( reg, C, AEZ, Y ) ] ) ) ],
         digits_calOutput ),
      stringsAsFactors = F )

#Table L227.AgCoef_BioWater for rainfed crops
L227.AgCoef_BioRfd_Water_by <- data.frame( region = L227.Region_Commodity_Yby_AEZ$region,
      AgSupplySector = L227.Region_Commodity_Yby_AEZ$GCAM_commodity,
      AgSupplySubsector = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, sep = AEZ_delimiter ),
      AgProductionTechnology = paste( L227.Region_Commodity_Yby_AEZ$GCAM_commodity, L227.Region_Commodity_Yby_AEZ$AEZ, "RFD", sep = AEZ_delimiter ),
      year = L227.Region_Commodity_Yby_AEZ$year,
      minicam.energy.input = Bio_Cons_name,
      coefficient = round(
         L227.Bio_RFD_IO_R_C_Y_AEZ.melt$value[
            match( vecpaste( L227.Region_Commodity_Yby_AEZ[ c( reg, C, AEZ, Y ) ] ),
                   vecpaste( L227.Bio_RFD_IO_R_C_Y_AEZ.melt[ c( reg, C, AEZ, Y ) ] ) ) ],
         digits_calOutput ),
      stringsAsFactors = F )

#Future periods
printlog( "Setting future water coefs (km3 / Mt crop ) equal to base year values" )
L227.AgCoef_Irr_Water_fy <- repeat_and_add_vector( subset( L227.AgCoef_Irr_Water_by, year == max( year ) ), "year", model_future_years )
L227.AgCoef_BioIrr_Water_fy <- repeat_and_add_vector( subset( L227.AgCoef_BioIrr_Water_by, year == max( year ) ), "year", model_future_years )
L227.AgCoef_BioRfd_Water_fy <- repeat_and_add_vector( subset( L227.AgCoef_BioRfd_Water_by, year == max( year ) ), "year", model_future_years )

#Rbind and drop all zero values (no need to create blank energy inputs)
L227.AgCoef_Irr_Water <- rbind( L227.AgCoef_Irr_Water_by, L227.AgCoef_Irr_Water_fy )
L227.AgCoef_BioIrr_Water <- rbind( L227.AgCoef_BioIrr_Water_by, L227.AgCoef_BioIrr_Water_fy )
L227.AgCoef_BioRfd_Water <- rbind( L227.AgCoef_BioRfd_Water_by, L227.AgCoef_BioRfd_Water_fy )

L227.AgCoef_Irr_Water$coefficient[ L227.AgCoef_Irr_Water$coefficient %in% c( Inf, NA ) ] <- 0 
L227.AgCoef_BioIrr_Water$coefficient[ L227.AgCoef_BioIrr_Water$coefficient %in% c( Inf, NA ) ] <- 0 
L227.AgCoef_BioRfd_Water$coefficient[ L227.AgCoef_BioRfd_Water$coefficient %in% c( Inf, NA ) ] <- 0 

L227.AgCoef_Irr_Water <- subset( L227.AgCoef_Irr_Water, coefficient > 0 )
L227.AgCoef_BioIrr_Water <- subset( L227.AgCoef_BioIrr_Water, coefficient > 0 )
L227.AgCoef_BioRfd_Water <- subset( L227.AgCoef_BioRfd_Water, coefficient > 0 )

# Create table for water withdrawals
L227.IrrigationEfficiency_R <- repeat_and_add_vector( L165.ag_IrrEff_R, Y, model_years )
L227.IrrigationEfficiency_R <- add_region_name( L227.IrrigationEfficiency_R )

L227.AgCoef_Irr_WaterWithdrawals <- L227.AgCoef_Irr_Water
L227.AgCoef_Irr_WaterWithdrawals$coefficient <- round(
      L227.AgCoef_Irr_WaterWithdrawals$coefficient /
      L227.IrrigationEfficiency_R$field.eff[
         match( vecpaste(L227.AgCoef_Irr_WaterWithdrawals[ c( reg, Y ) ] ),
                vecpaste(L227.IrrigationEfficiency_R[ c( reg, Y ) ] ) ) ],
      digits_calOutput )
L227.AgCoef_Irr_WaterWithdrawals$minicam.energy.input <- Irr_W_name
       
#  Water IO coefficients for dedicated bioenergy crops (km3 / EJ biomass)
printlog( "Table L227.AgCoef_Water_bio: Water input-output coefficients by region / dedicated bioenergy crop / year / AEZ" )
#Compute average water IO for grassy and woody bioenergy crops
#Build template table for bioenergy crops
L227.Region_AgTechnology_bio <- data.frame(
      region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      AEZ = sort( rep( AEZs, times = length( GCAM_region_names$region ) ) ) )

#Name the AgSupplySector and AgSupplySubsector (not yet specified to bio crop types)
L227.Region_AgTechnology_bio[[agsupp]] <- "biomass"
L227.Region_AgTechnology_bio[[agsubs]] <- paste( L227.Region_AgTechnology_bio[[agsupp]], L227.Region_AgTechnology_bio[[AEZ]], sep = AEZ_delimiter )
L227.Region_AgTechnology_bio <- repeat_and_add_vector( L227.Region_AgTechnology_bio, irr, c( "IRR", "RFD" ) )
L227.Region_AgTechnology_bio$AgProductionTechnology <- paste( L227.Region_AgTechnology_bio[[agsubs]], L227.Region_AgTechnology_bio[[irr]], sep = AEZ_delimiter )

printlog( "Renaming specified bioenergy crops in biomass table" )
L227.Region_AgTechnology_bio <- rename_biocrops( L227.Region_AgTechnology_bio, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

#Create table for model input
L227.AgCoef_Bio_Water_bio <- repeat_and_add_vector( L227.Region_AgTechnology_bio[ c( reg, agsupp, agsubs, agtech ) ], Y, model_years )
L227.AgCoef_Bio_Water_bio$minicam.energy.input <- Bio_Cons_name
L227.AgCoef_Bio_Water_bio$coefficient <- swgr_Bio_IO_km3EJ

#Replace non-switchgrass bioenergy crops with other water coefficients
L227.AgCoef_Bio_Water_bio$coefficient[ substr( L227.AgCoef_Bio_Water_bio$AgSupplySubsector,
      1, nchar( as.character( L227.AgCoef_Bio_Water_bio$AgSupplySubsector ) ) - 5 ) %in% woody_biocrops ] <- wdy_Bio_IO_km3EJ
      
L227.AgCoef_Bio_Water_bio$coefficient[ substr( L227.AgCoef_Bio_Water_bio$AgSupplySubsector,
      1, nchar( as.character( L227.AgCoef_Bio_Water_bio$AgSupplySubsector ) ) - 5 ) %in% c( "miscanthus" ) ] <- misc_Bio_IO_km3EJ
      
L227.AgCoef_Bio_Water_bio$coefficient[ substr( L227.AgCoef_Bio_Water_bio$AgSupplySubsector,
      1, nchar( as.character( L227.AgCoef_Bio_Water_bio$AgSupplySubsector ) ) - 5 ) %in% c( "Jatropha" ) ] <- Jat_Bio_IO_km3EJ

printlog( "Compute blue water for irrigated bioenergy" )
#Match in green and blue water for existing crops in 2005 -- note for this we are only using blue & green from irrigated crops
L227.irrWater_R_C_AEZ.melt <- melt( L165.BlueIrr_m3kg_R_C_AEZ, id.vars = R_C, variable.name = AEZ, value.name = "BlueCoeff_perkg" )
L227.bioWater_R_C_AEZ.melt <- melt( L165.TotIrr_m3kg_R_C_AEZ, id.vars = R_C, variable.name = AEZ )
L227.irrWater_R_C_AEZ.melt$GreenCoeff_perkg <- L227.bioWater_R_C_AEZ.melt$value[
      match( vecpaste( L227.irrWater_R_C_AEZ.melt[ R_C_AEZ ] ),
             vecpaste( L227.bioWater_R_C_AEZ.melt[ R_C_AEZ ] ) ) ] -
      L227.irrWater_R_C_AEZ.melt$BlueCoeff_perkg

#Compute % of blue water for irrigated
L227.irrWater_R_C_AEZ.melt$blue_fract <- L227.irrWater_R_C_AEZ.melt$BlueCoeff_perkg /
      ( L227.irrWater_R_C_AEZ.melt$GreenCoeff_perkg + L227.irrWater_R_C_AEZ.melt$BlueCoeff_perkg )

#Drop extra columns
L227.irrWater_R_C_AEZ.melt <- L227.irrWater_R_C_AEZ.melt[ names(L227.irrWater_R_C_AEZ.melt) %!in% c( "BlueCoeff_perkg", "GreenCoeff_perkg" ) ]

#Average the relevant commodities
L227.irrWater_R_C_AEZ.melt <- subset( L227.irrWater_R_C_AEZ.melt, L227.irrWater_R_C_AEZ.melt$GCAM_commodity %in% cellulosic_crops )
L227.irrWater_R_C_AEZ.melt$blue_fract[ L227.irrWater_R_C_AEZ.melt$blue_fract == "NaN" ] <- 0
L227.BlueWaterFract <- aggregate( blue_fract ~ GCAM_region_ID + AEZ, L227.irrWater_R_C_AEZ.melt, mean )
L227.BlueWaterFract <- add_region_name( L227.BlueWaterFract )
L227.BlueWaterFract <- repeat_and_add_vector( L227.BlueWaterFract, "year", model_years )
 
#Create table for model input  
L227.AgCoef_Irr_Water_bio <- L227.Region_AgTechnology_bio[ L227.Region_AgTechnology_bio$Irr_Rfd == "IRR", ]
L227.AgCoef_Irr_Water_bio <- repeat_and_add_vector( L227.AgCoef_Irr_Water_bio[ c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ) ],
      "year", model_years )
L227.AgCoef_Irr_Water_bio$minicam.energy.input <- Irr_Cons_name
L227.AgCoef_Irr_Water_bio$AEZ <- substr( L227.AgCoef_Irr_Water_bio$AgSupplySubsector,
      nchar( as.character( L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ) - 4,
      nchar( as.character( L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ) )
L227.AgCoef_Irr_Water_bio$coefficient <- swgr_Bio_IO_km3EJ * L227.BlueWaterFract$blue_fract[
      match( vecpaste(L227.AgCoef_Irr_Water_bio[ c( reg, AEZ, Y ) ] ),
             vecpaste(L227.BlueWaterFract[ c( reg, AEZ, Y ) ] ) ) ]
L227.AgCoef_Irr_Water_bio$coefficient[ is.na( L227.AgCoef_Irr_Water_bio$coefficient ) ] <- 0

#Replace other bioenergy crops with crop-specific water coefficients
L227.AgCoef_Irr_Water_bio$coefficient[
      substr( L227.AgCoef_Irr_Water_bio$AgSupplySubsector, 1, nchar( as.character( L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ) - 5 ) %in% woody_biocrops ] <-
      wdy_Bio_IO_km3EJ * L227.BlueWaterFract$blue_fract[
         match( vecpaste( L227.AgCoef_Irr_Water_bio[
                          substr( L227.AgCoef_Irr_Water_bio$AgSupplySubsector, 1, nchar( as.character( L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ) - 5 ) %in% woody_biocrops,
                          c( reg, AEZ, Y ) ] ),
                vecpaste(L227.BlueWaterFract[ c( reg, AEZ, Y ) ] ) ) ]
      
L227.AgCoef_Irr_Water_bio$coefficient[ grepl( "miscanthus", L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ] <-
      misc_Bio_IO_km3EJ * L227.BlueWaterFract$blue_fract[
         match( vecpaste( L227.AgCoef_Irr_Water_bio[ grepl( "miscanthus", L227.AgCoef_Irr_Water_bio$AgSupplySubsector ), c( reg, AEZ, Y ) ] ),
                vecpaste(L227.BlueWaterFract[ c( reg, AEZ, Y ) ] ) ) ]
      
L227.AgCoef_Irr_Water_bio$coefficient[ grepl( "Jatropha", L227.AgCoef_Irr_Water_bio$AgSupplySubsector ) ] <-
      Jat_Bio_IO_km3EJ * L227.BlueWaterFract$blue_fract[
         match( vecpaste( L227.AgCoef_Irr_Water_bio[ grepl( "Jatropha", L227.AgCoef_Irr_Water_bio$AgSupplySubsector ), c( reg, AEZ, Y ) ] ),
                vecpaste(L227.BlueWaterFract[ c( reg, AEZ, Y ) ]) )]
      
#Remove extra columns
L227.AgCoef_Bio_Water_bio <- L227.AgCoef_Bio_Water_bio[ names( L227.AgCoef_Bio_Water_bio ) != AEZ ] 
L227.AgCoef_Bio_Water_bio$coefficient[ L227.AgCoef_Bio_Water_bio$coefficient %in% c( Inf, NA ) ] <- 0  

# Create table for water withdrawals
L227.AgCoef_Irr_WaterWithdrawals_bio <- L227.AgCoef_Irr_Water_bio
L227.AgCoef_Irr_WaterWithdrawals_bio$coefficient <- L227.AgCoef_Irr_WaterWithdrawals_bio$coefficient / L227.IrrigationEfficiency_R$field.eff[
      match( vecpaste( L227.AgCoef_Irr_WaterWithdrawals_bio[ c( reg, Y ) ] ),
             vecpaste( L227.IrrigationEfficiency_R[ c( reg, Y ) ] ) ) ]
L227.AgCoef_Irr_WaterWithdrawals_bio$minicam.energy.input <- Irr_W_name
L227.AgCoef_Irr_WaterWithdrawals_bio$coefficient[ L227.AgCoef_Irr_WaterWithdrawals_bio$coefficient %in% c( Inf, NA ) ] <- 0 

printlog( "Removing non-existent region / AEZs" )
L227.AgCoef_BioIrr_Water <- remove_AEZ_nonexist( L227.AgCoef_BioIrr_Water )
L227.AgCoef_BioRfd_Water <- remove_AEZ_nonexist( L227.AgCoef_BioRfd_Water )
L227.AgCoef_Irr_Water <- remove_AEZ_nonexist( L227.AgCoef_Irr_Water )
L227.AgCoef_Irr_WaterWithdrawals <- remove_AEZ_nonexist( L227.AgCoef_Irr_WaterWithdrawals )
L227.AgCoef_Irr_WaterWithdrawals_bio <- remove_AEZ_nonexist( L227.AgCoef_Irr_WaterWithdrawals_bio )
L227.AgCoef_Bio_Water_bio <- remove_AEZ_nonexist( L227.AgCoef_Bio_Water_bio )
L227.AgCoef_Irr_Water_bio <- remove_AEZ_nonexist( L227.AgCoef_Irr_Water_bio )

printlog( "Ad hoc adjustment to water coefficients so that none of the region/AEZ/crops have negative profit" )
# In GCAM, the profit rate is calculated as price minus cost times yield, so if the cost exceeds the price, then the profit goes negative.
# By adding in the water cost without modifying the non-land variable costs, we risk having prices that exceed commodity prices, which will
# cause solution/calibration failure in base years, and zero share in future years. This calculation checks whether any of our costs exceed
# the prices.
L227.AgCoef_Irr_WaterWithdrawals$WaterPrice <- DEFAULT_UNLIMITED_IRR_WATER_PRICE
L227.AgCoef_Irr_WaterWithdrawals$WaterCost <- with( L227.AgCoef_Irr_WaterWithdrawals, coefficient * WaterPrice )
L227.AgCoef_Irr_WaterWithdrawals$NonLandCost <- L225.AgCost_ag$nonLandVariableCost[
      match( vecpaste( L227.AgCoef_Irr_WaterWithdrawals[ c( reg, agtech, Y ) ] ),
             vecpaste( L225.AgCost_ag[ c( reg, agtech, Y ) ] ) ) ]
L227.AgCoef_Irr_WaterWithdrawals$calPrice <- L132.ag_an_For_Prices$calPrice[
      match( L227.AgCoef_Irr_WaterWithdrawals[[ agsupp ]], L132.ag_an_For_Prices[[C]] ) ]
L227.AgCoef_Irr_WaterWithdrawals$Profit <- with( L227.AgCoef_Irr_WaterWithdrawals, calPrice - WaterCost - NonLandCost)

printlog( "Assuming an exogenous floor on profit rates to prevent negative, zero, and very low profit rates" )
# For the model, low profit rates are fine as long as it's not the dominant crop in a nest where bioenergy is allowed
minProfit <- min( with( L227.AgCoef_Irr_WaterWithdrawals, calPrice - NonLandCost ) ) / 2
L227.AgCoef_Irr_WaterWithdrawals$coefficient <- with( L227.AgCoef_Irr_WaterWithdrawals,
      round( pmin( coefficient, ( calPrice - NonLandCost - minProfit ) / WaterPrice ), digits_calOutput ) )

#Go ahead and print out the number of values being changed in each year
tmp1 <- subset( L227.AgCoef_Irr_WaterWithdrawals, year == 2010 )
tmp2 <- subset( tmp1, Profit < minProfit )
printlog( "Out of", nrow( tmp1 ), "observations,", nrow(tmp2 ), "had water coefficients reduced to keep positive profit rates")

#As a last step, remove the unnecessary columns
L227.AgCoef_Irr_WaterWithdrawals <- L227.AgCoef_Irr_WaterWithdrawals[ names_AgCoef ]

printlog( "Map the water inputs to the appropraite water mapping sector." )

# A helper wrapper to help get the data in/out in a way that is usable by the
# get_water_inputs_for_mapping utility method.
do_water_rename <- function(d) {
    d[[water_sector]] <- "Irrigation"
    d[[AEZ]] <- as.integer( sub( '.*AEZ', '', d[[agsubs]] ) )
    d$minicam.energy.input <- as.character( d$minicam.energy.input )
    d$minicam.energy.input <- get_water_inputs_for_mapping( d, A03.sector, water_type.col="minicam.energy.input" )
    d[[AEZ]] <- NULL
    d[[water_sector]] <- NULL
    return(d)
}

L227.AgCoef_BioIrr_Water <- do_water_rename( L227.AgCoef_BioIrr_Water )
L227.AgCoef_BioRfd_Water <- do_water_rename( L227.AgCoef_BioRfd_Water )
L227.AgCoef_Irr_Water <- do_water_rename( L227.AgCoef_Irr_Water )
L227.AgCoef_Irr_WaterWithdrawals <- do_water_rename( L227.AgCoef_Irr_WaterWithdrawals )
L227.AgCoef_Irr_WaterWithdrawals_bio <- do_water_rename( L227.AgCoef_Irr_WaterWithdrawals_bio )
L227.AgCoef_Bio_Water_bio <- do_water_rename( L227.AgCoef_Bio_Water_bio )
L227.AgCoef_Irr_Water_bio <- do_water_rename( L227.AgCoef_Irr_Water_bio )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L227.AgCoef_BioIrr_Water, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L227.AgCoef_BioIrr_Water",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_BioRfd_Water, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L227.AgCoef_BioRfd_Water",
               batch_XML_domain="AGLU_XML_BATCH",  batch_XML_file="batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_Irr_Water, "AgCoef", "AGLU_LEVEL2_DATA", "L227.AgCoef_Irr_Water", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_Irr_WaterWithdrawals, "AgCoef", "AGLU_LEVEL2_DATA", "L227.AgCoef_Irr_WaterWithdrawals", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_Irr_WaterWithdrawals_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L227.AgCoef_Irr_WaterWithdrawals_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_Bio_Water_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L227.AgCoef_Bio_Water_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 
write_mi_data( L227.AgCoef_Irr_Water_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L227.AgCoef_Irr_Water_bio", "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_water_input_IRR.xml", "AGLU_XML_FINAL", "ag_water_input_IRR.xml", "", xml_tag="outFile" )

logstop()
