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
logstart( "L226.ag_Fert_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for agricultural fertilizer consumption" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_bio_cost_yield <- readdata( "AGLU_ASSUMPTIONS", "A_bio_cost_yield" )
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L164.ag_Cost_75USDkg_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L164.ag_Cost_75USDkg_C_AEZ" )
L142.ag_Fert_IO_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Table L226.AgCoef_Fert_ag_irr: Fertilizer input-output coefficients (kgN per kg crop) by region / crop / year / AEZ" )
#Melt fertilizer IO coef table, and round to specified number of digits
L226.Ag_Fert_IO_R_C_Y_AEZ.melt <- melt( L142.ag_Fert_IO_R_C_Y_AEZ, id.vars = R_C_AEZ, variable.name = Y, value.name = "coefficient" )
L226.Ag_Fert_IO_R_C_Y_AEZ.melt[[Y]] <- sub( "X", "", L226.Ag_Fert_IO_R_C_Y_AEZ.melt[[Y]] )
L226.Ag_Fert_IO_R_C_Y_AEZ.melt$coefficient <- round( L226.Ag_Fert_IO_R_C_Y_AEZ.melt$coefficient, digits_Fert_IO )
L226.Ag_Fert_IO_R_C_Y_AEZ.melt <- add_region_name( L226.Ag_Fert_IO_R_C_Y_AEZ.melt )

#Separating irrigated and rainfed technologies
L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt <- repeat_and_add_vector( L226.Ag_Fert_IO_R_C_Y_AEZ.melt, irr, c( "IRR", "RFD" ) )
L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[[agtech]] <- paste( L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[[agtech]], L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[[irr]], sep = AEZ_delimiter )
L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt <- add_agtech_names( L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt )
L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[[input]] <- Fert_name

#This table has the historical years but not the future years. Need to copy forward the final historical year
Fert_future_years <- model_years[ model_years > max( L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt$year ) ]
L226.AgCoef_Fert_ag_irr_hist <- L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[ L226.Ag_Fert_IO_R_C_Y_AEZ_irr.melt[[Y]] %in% model_years, names_AgCoef ]
L226.AgCoef_Fert_ag_irr_fut <- repeat_and_add_vector( subset( L226.AgCoef_Fert_ag_irr_hist, year == max( year ) ), Y, Fert_future_years )
L226.AgCoef_Fert_ag_irr <- rbind( L226.AgCoef_Fert_ag_irr_hist, L226.AgCoef_Fert_ag_irr_fut )

#Drop all zero values so as to not create empty energy input objects
printlog( "Dropping fertilizer inputs for crops where value would be 0" )
#Note: the fertilizer IO coefficients can not be specified in selected model time periods; they need to be >0 in all periods.
# This has already been checked in the corresponding level1 code file.
L226.AgCoef_Fert_ag_irr <- L226.AgCoef_Fert_ag_irr[ L226.AgCoef_Fert_ag_irr$coefficient > 0, ]

#  Fertilizer IO coefficients for dedicated bioenergy crops (kg N / GJ biomass)
printlog( "Table L226.AgCoef_Fert_bio_irr: Fertilizer input-output coefficients by region / dedicated bioenergy crop / year / AEZ" )
#Compute average fertilizer IO for grassy and woody bioenergy crops, from Adler et al. 2007
swgr_Fert_IO_kgNGJ <- swgr_Fert_IO_gNm2 * conv_g_kg / swgr_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
      Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass" ] ) /    # convert from carbon to wet biomass
      ( bio_GJt * conv_kg_t ) #convert from biomass to energy
swgr_Fert_IO_kgNGJ <- round( swgr_Fert_IO_kgNGJ, digits_Fert_IO )
popl_Fert_IO_kgNGJ <- popl_Fert_IO_gNm2 * conv_g_kg / popl_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
      Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "willow" ] ) /    # convert from carbon to wet biomass
      ( bio_GJt * conv_kg_t ) #convert from biomass to energy
popl_Fert_IO_kgNGJ <- round( popl_Fert_IO_kgNGJ, digits_Fert_IO )

L226.AgCoef_Fert_bio <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "biomass",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ), stringsAsFactors = F )
L226.AgCoef_Fert_bio_irr <- repeat_and_add_vector( L226.AgCoef_Fert_bio, irr, c( "IRR", "RFD" ) )
L226.AgCoef_Fert_bio_irr <- add_agtech_names( L226.AgCoef_Fert_bio_irr )
L226.AgCoef_Fert_bio_irr <- repeat_and_add_vector( L226.AgCoef_Fert_bio_irr, Y, model_years )
L226.AgCoef_Fert_bio_irr[[input]] <- Fert_name
L226.AgCoef_Fert_bio_irr[["coefficient"]] <- swgr_Fert_IO_kgNGJ

printlog( "Renaming specified bioenergy crops in biomass tables" )
L226.AgCoef_Fert_bio_irr <- rename_biocrops( L226.AgCoef_Fert_bio_irr, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

#Replace woody bioenergy crops with poplar-derived fertilizer coefficient
for( i in 1:length( woody_biocrops ) ){
	L226.AgCoef_Fert_bio_irr$coefficient[ grepl( woody_biocrops[i], L226.AgCoef_Fert_bio_irr[[agtech]] ) ] <- popl_Fert_IO_kgNGJ
}
L226.AgCoef_Fert_bio_irr <- L226.AgCoef_Fert_bio_irr[ names_AgCoef ]

#Adjustments to NonLandVariableCosts (75USD / kg crop) for all regions, crops, AEZs, and years
printlog( "Table L226.AgCost_ag_adj_irr: Adjusted NonLandVariableCost by region / crop / year / AEZ" )
L226.ag_Cost_75USDkg_C_AEZ.melt <- melt( L164.ag_Cost_75USDkg_C_AEZ, id.vars = C, variable.name = AEZ )
L226.ag_Cost_75USDkg_C_AEZ_irr.melt <- repeat_and_add_vector( L226.ag_Cost_75USDkg_C_AEZ.melt, irr, c( "IRR", "RFD" ) )
L226.ag_Cost_75USDkg_C_AEZ_irr.melt <- add_agtech_names( L226.ag_Cost_75USDkg_C_AEZ_irr.melt )

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L226.AgCost_ag_adj_irr <- L226.AgCoef_Fert_ag_irr
L226.AgCost_ag_adj_irr$Cost_init <- L226.ag_Cost_75USDkg_C_AEZ_irr.melt$value[
      match( L226.AgCost_ag_adj_irr[[ agtech ]], L226.ag_Cost_75USDkg_C_AEZ_irr.melt[[ agtech ]] ) ]
L226.AgCost_ag_adj_irr$FertCost <- L226.AgCost_ag_adj_irr$coefficient * Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Subtract fertilizer cost from other nonlandvariablecost and round
L226.AgCost_ag_adj_irr$nonLandVariableCost <- round( L226.AgCost_ag_adj_irr$Cost_init - L226.AgCost_ag_adj_irr$FertCost, digits_calPrice )
L226.AgCost_ag_adj_irr <- L226.AgCost_ag_adj_irr[ names_AgCost ]

#  Adjustments to NonLandVariableCosts (75USD / kg crop) for regions, bioenergy crops, AEZs, and years
printlog( "Table L226.AgCost_bio_adj_irr: Adjusted NonLandVariableCost by region / bioenergy crop / year / AEZ" )
L226.AgCost_bio_adj_irr <- L226.AgCoef_Fert_bio_irr

#Specify AEZs and crop names
L226.AgCost_bio_adj_irr[[AEZ]] <- substr( L226.AgCost_bio_adj_irr[[agsubs]], nchar( L226.AgCost_bio_adj_irr[[agsubs]] ) - 4, nchar( L226.AgCost_bio_adj_irr[[agsubs]] ) )
L226.AgCost_bio_adj_irr[[C]] <- substr( L226.AgCost_bio_adj_irr[[agsubs]], 1, nchar( as.character( L226.AgCost_bio_adj_irr[[agsubs]] ) ) - 5 )
      
#Only adjust the costs of the bioenergy that is not in high-cost AEZs, and whose years are in the time frame considered
L226.AgCost_bio_adj_irr <- L226.AgCost_bio_adj_irr[ !L226.AgCost_bio_adj_irr$AEZ %in% AEZs_hi_bio_cost, ]
L226.AgCost_bio_adj_irr$Cost_init <- A_bio_cost_yield$cost_USDGJ[ match( L226.AgCost_bio_adj_irr[[C]], A_bio_cost_yield[[C]] ) ]

#Match in fertilizer cost, and calculate the adjusted non-land variable cost
L226.AgCost_bio_adj_irr$FertCost <- L226.AgCost_bio_adj_irr$coefficient * Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N
L226.AgCost_bio_adj_irr$nonLandVariableCost <- round( L226.AgCost_bio_adj_irr$Cost_init - L226.AgCost_bio_adj_irr$FertCost, digits_calPrice )
L226.AgCost_bio_adj_irr <- L226.AgCost_bio_adj_irr[ names_AgCost ]

printlog( "Removing non-existent region x AEZ combinations" )
L226.AgCoef_Fert_ag_irr <- remove_AEZ_nonexist( L226.AgCoef_Fert_ag_irr )
L226.AgCoef_Fert_bio_irr <- remove_AEZ_nonexist( L226.AgCoef_Fert_bio_irr )
L226.AgCost_ag_adj_irr <- remove_AEZ_nonexist( L226.AgCost_ag_adj_irr )
L226.AgCost_bio_adj_irr <- remove_AEZ_nonexist( L226.AgCost_bio_adj_irr )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L226.AgCoef_Fert_ag_irr, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L226.AgCoef_Fert_ag_irr",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_Fert_IRR.xml" ) 
write_mi_data( L226.AgCoef_Fert_bio_irr, "AgCoef", "AGLU_LEVEL2_DATA", "L226.AgCoef_Fert_bio_irr", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 
write_mi_data( L226.AgCost_ag_adj_irr, "AgCost", "AGLU_LEVEL2_DATA", "L226.AgCost_ag_adj_irr", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 
write_mi_data( L226.AgCost_bio_adj_irr, "AgCost", "AGLU_LEVEL2_DATA", "L226.AgCost_bio_adj_irr", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml", "AGLU_XML_FINAL", "ag_Fert_IRR.xml", "", xml_tag="outFile" )

logstop()
