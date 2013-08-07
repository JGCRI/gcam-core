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
logstart( "L206.ag_Fert.R" )
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
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L133.ag_Cost_75USDkg_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C_AEZ" )
L142.ag_Fert_IO_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Table L206.AgCoef_Fert_ag: Fertilizer input-output coefficients (kgN per kg crop) by region / crop / year / AEZ" )
#Melt fertilizer IO coef table, and round to specified number of digits
L206.Ag_Fert_IO_R_C_Y_AEZ.melt <- melt( L142.ag_Fert_IO_R_C_Y_AEZ, id.vars = R_C_AEZ, variable_name = Y )
L206.Ag_Fert_IO_R_C_Y_AEZ.melt[[Y]] <- sub( "X", "", L206.Ag_Fert_IO_R_C_Y_AEZ.melt[[Y]] )
L206.Ag_Fert_IO_R_C_Y_AEZ.melt$value <- round( L206.Ag_Fert_IO_R_C_Y_AEZ.melt$value, digits_Fert_IO )
L206.Ag_Fert_IO_R_C_Y_AEZ.melt <- add_region_name( L206.Ag_Fert_IO_R_C_Y_AEZ.melt )
L206.Ag_Fert_IO_R_C_Y_AEZ.melt <- add_agtech_names( L206.Ag_Fert_IO_R_C_Y_AEZ.melt )
L206.Ag_Fert_IO_R_C_Y_AEZ.melt[[input]] <- Fert_name
names( L206.Ag_Fert_IO_R_C_Y_AEZ.melt )[ names( L206.Ag_Fert_IO_R_C_Y_AEZ.melt ) == "value" ] <- "coefficient"

#This table has the historical years but not the future years. Need to copy forward the final historical year
Fert_future_years <- model_years[ model_years > max( L206.Ag_Fert_IO_R_C_Y_AEZ.melt$year ) ]
L206.AgCoef_Fert_ag_hist <- L206.Ag_Fert_IO_R_C_Y_AEZ.melt[ L206.Ag_Fert_IO_R_C_Y_AEZ.melt[[Y]] %in% model_years, names_AgCoef ]
L206.AgCoef_Fert_ag_fut <- repeat_and_add_vector( subset( L206.AgCoef_Fert_ag_hist, year == max( year ) ), Y, Fert_future_years )
L206.AgCoef_Fert_ag <- rbind( L206.AgCoef_Fert_ag_hist, L206.AgCoef_Fert_ag_fut )

#Drop all zero values so as to not create empty energy input objects
printlog( "Dropping fertilizer inputs for crops where value would be 0" )
#Note: the fertilizer IO coefficients can not be specified in selected model time periods; they need to be >0 in all periods.
# This has already been checked in the corresponding level1 code file.
L206.AgCoef_Fert_ag <- L206.AgCoef_Fert_ag[ L206.AgCoef_Fert_ag$coefficient > 0, ]

#  Fertilizer IO coefficients for dedicated bioenergy crops (kg N / GJ biomass)
printlog( "Table L206.AgCoef_Fert_bio: Fertilizer input-output coefficients by region / dedicated bioenergy crop / year / AEZ" )
#Compute average fertilizer IO for grassy and woody bioenergy crops, from Adler et al. 2007
swgr_Fert_IO_kgNGJ <- swgr_Fert_IO_gNm2 * conv_g_kg / swgr_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
      Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass" ] ) /    # convert from carbon to wet biomass
      ( bio_GJt * conv_kg_t ) #convert from biomass to energy
swgr_Fert_IO_kgNGJ <- round( swgr_Fert_IO_kgNGJ, digits_Fert_IO )
popl_Fert_IO_kgNGJ <- popl_Fert_IO_gNm2 * conv_g_kg / popl_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
      Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "willow" ] ) /    # convert from carbon to wet biomass
      ( bio_GJt * conv_kg_t ) #convert from biomass to energy
popl_Fert_IO_kgNGJ <- round( popl_Fert_IO_kgNGJ, digits_Fert_IO )

L206.AgCoef_Fert_bio <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "biomass", AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ), stringsAsFactors = F )
L206.AgCoef_Fert_bio <- add_agtech_names( L206.AgCoef_Fert_bio )
L206.AgCoef_Fert_bio <- repeat_and_add_vector( L206.AgCoef_Fert_bio, Y, model_years )
L206.AgCoef_Fert_bio[[input]] <- Fert_name
L206.AgCoef_Fert_bio[["coefficient"]] <- swgr_Fert_IO_kgNGJ

printlog( "Renaming specified bioenergy crops in biomass table" )
L206.AgCoef_Fert_bio <- rename_biocrops( L206.AgCoef_Fert_bio, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

#Replace woody bioenergy crops with poplar-derived fertilizer coefficient
for( i in 1:length( woody_biocrops ) ){
	L206.AgCoef_Fert_bio$coefficient[ grepl( woody_biocrops[i], L206.AgCoef_Fert_bio[[agtech]] ) ] <- popl_Fert_IO_kgNGJ
}
L206.AgCoef_Fert_bio <- L206.AgCoef_Fert_bio[ names_AgCoef ]

#Adjustments to NonLandVariableCosts (75USD / kg crop) for all regions, crops, AEZs, and years
printlog( "Table L206.AgCost_ag_adj: Adjusted NonLandVariableCost by region / crop / year / AEZ" )
L206.ag_Cost_75USDkg_C_AEZ.melt <- melt( L133.ag_Cost_75USDkg_C_AEZ, id.vars = C, variable_name = AEZ )
L206.ag_Cost_75USDkg_C_AEZ.melt <- add_agtech_names( L206.ag_Cost_75USDkg_C_AEZ.melt )

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L206.AgCost_ag_adj <- L206.AgCoef_Fert_ag
L206.AgCost_ag_adj$Cost_init <- L206.ag_Cost_75USDkg_C_AEZ.melt$value[
      match( L206.AgCost_ag_adj[[ agtech ]], L206.ag_Cost_75USDkg_C_AEZ.melt[[ agtech ]] ) ]
L206.AgCost_ag_adj$FertCost <- L206.AgCost_ag_adj$coefficient * Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Subtract fertilizer cost from other nonlandvariablecost and round
L206.AgCost_ag_adj$nonLandVariableCost <- round( L206.AgCost_ag_adj$Cost_init - L206.AgCost_ag_adj$FertCost, digits_calPrice )
L206.AgCost_ag_adj <- L206.AgCost_ag_adj[ names_AgCost ]

#  Adjustments to NonLandVariableCosts (75USD / kg crop) for regions, bioenergy crops, AEZs, and years
printlog( "Table L206.AgCost_bio_adj: Adjusted NonLandVariableCost by region / bioenergy crop / year / AEZ" )
L206.AgCost_bio_adj <- L206.AgCoef_Fert_bio

#Specify AEZs and crop names
L206.AgCost_bio_adj[[AEZ]] <- substr( L206.AgCost_bio_adj[[agsubs]], nchar( L206.AgCost_bio_adj[[agsubs]] ) - 4, nchar( L206.AgCost_bio_adj[[agsubs]] ) )
L206.AgCost_bio_adj[[C]] <- substr( L206.AgCost_bio_adj[[agsubs]], 1, nchar( as.character( L206.AgCost_bio_adj[[agsubs]] ) ) - 5 )
      
#Only adjust the costs of the bioenergy that is not in high-cost AEZs, and whose years are in the time frame considered
L206.AgCost_bio_adj <- L206.AgCost_bio_adj[ !L206.AgCost_bio_adj$AEZ %in% AEZs_hi_bio_cost, ]
L206.AgCost_bio_adj$Cost_init <- A_bio_cost_yield$cost_USDGJ[ match( L206.AgCost_bio_adj[[C]], A_bio_cost_yield[[C]] ) ]

#Match in fertilizer cost, and calculate the adjusted non-land variable cost
L206.AgCost_bio_adj$FertCost <- L206.AgCost_bio_adj$coefficient * Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N
L206.AgCost_bio_adj$nonLandVariableCost <- round( L206.AgCost_bio_adj$Cost_init - L206.AgCost_bio_adj$FertCost, digits_calPrice )
L206.AgCost_bio_adj <- L206.AgCost_bio_adj[ names_AgCost ]

printlog( "Removing non-existent region x AEZ combinations" )
L206.AgCoef_Fert_ag <- remove_AEZ_nonexist( L206.AgCoef_Fert_ag )
L206.AgCoef_Fert_bio <- remove_AEZ_nonexist( L206.AgCoef_Fert_bio )
L206.AgCost_ag_adj <- remove_AEZ_nonexist( L206.AgCost_ag_adj )
L206.AgCost_bio_adj <- remove_AEZ_nonexist( L206.AgCost_bio_adj )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L206.AgCoef_Fert_ag, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L206.AgCoef_Fert_ag",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_Fert_input.xml" ) 
write_mi_data( L206.AgCoef_Fert_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L206.AgCoef_Fert_bio", "AGLU_XML_BATCH", "batch_ag_Fert_input.xml" ) 
write_mi_data( L206.AgCost_ag_adj, "AgCost", "AGLU_LEVEL2_DATA", "L206.AgCost_ag_adj", "AGLU_XML_BATCH", "batch_ag_Fert_input.xml" ) 
write_mi_data( L206.AgCost_bio_adj, "AgCost", "AGLU_LEVEL2_DATA", "L206.AgCost_bio_adj", "AGLU_XML_BATCH", "batch_ag_Fert_input.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_Fert_input.xml", "AGLU_XML_FINAL", "ag_Fert_input.xml", "", xml_tag="outFile" )

logstop()
