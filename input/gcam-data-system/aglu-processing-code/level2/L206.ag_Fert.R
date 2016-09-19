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
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
L142.ag_Fert_IO_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_GLU" )
L205.AgCost_ag <- readdata( "AGLU_LEVEL2_DATA", "L205.AgCost_ag", skip = 4 )
L205.AgCost_bio <- readdata( "AGLU_LEVEL2_DATA", "L205.AgCost_bio", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Table L206.AgCoef_Fert_ag: Fertilizer input-output coefficients (kgN per kg crop) by region / crop / year / GLU" )
#Melt fertilizer IO coef table, and round to specified number of digits
L206.AgCoef_Fert_ag <- interpolate_and_melt( L142.ag_Fert_IO_R_C_Y_GLU,
                                             model_base_years, value.name = "coefficient", digits = digits_Fert_IO )
L206.AgCoef_Fert_ag <- add_region_name( L206.AgCoef_Fert_ag )
L206.AgCoef_Fert_ag <- add_agtech_names( L206.AgCoef_Fert_ag )
L206.AgCoef_Fert_ag[[input]] <- Fert_name
L206.AgCoef_Fert_ag <- L206.AgCoef_Fert_ag[ names_AgCoef ]

#Drop all zero values so as to not create empty energy input objects
printlog( "Dropping fertilizer inputs for crops where value would be 0" )
#Note: the fertilizer IO coefficients can not be specified in selected model time periods; they need to be >0 in all periods.
# This has already been checked in the corresponding level1 code file.
L206.AgCoef_Fert_ag <- L206.AgCoef_Fert_ag[ L206.AgCoef_Fert_ag$coefficient > 0, ]

#This table has the historical years but not the future years. Need to copy forward the final historical year
L206.AgCoef_Fert_ag <- rbind( L206.AgCoef_Fert_ag[ L206.AgCoef_Fert_ag[[Y]] %in% model_base_years, ],
                              repeat_and_add_vector( L206.AgCoef_Fert_ag[
                                L206.AgCoef_Fert_ag[[Y]] == final_model_base_year, ],
                                Y, model_future_years ) )

#  Fertilizer IO coefficients for dedicated bioenergy crops (kg N / GJ biomass)
printlog( "Table L206.AgCoef_Fert_bio: Fertilizer input-output coefficients by region / dedicated bioenergy crop / year / GLU" )
#Compute average fertilizer IO for grassy and woody bioenergy crops, from Adler et al. 2007
bio_grass_Fert_IO_kgNGJ <- round(
  bio_grass_Fert_IO_gNm2 * conv_g_kg / bio_grass_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
  Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass_grass" ] ) /    # convert from carbon to wet biomass
  ( bio_GJt * conv_kg_t ), #convert from biomass to energy
  digits_Fert_IO )
bio_tree_Fert_IO_kgNGJ <- round(
  bio_tree_Fert_IO_gNm2 * conv_g_kg / bio_tree_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
  Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass_tree" ] ) /    # convert from carbon to wet biomass
  ( bio_GJt * conv_kg_t ), #convert from biomass to energy
  digits_Fert_IO )

# using the table of costs as the base for reading in fertilizer coefs
L206.AgCoef_Fert_bio <- L205.AgCost_bio[ names_AgTechYr ]
L206.AgCoef_Fert_bio[[input]] <- Fert_name
L206.AgCoef_Fert_bio$coefficient[ grepl( "grass", L206.AgCoef_Fert_bio$AgSupplySubsector ) ] <- bio_grass_Fert_IO_kgNGJ
L206.AgCoef_Fert_bio$coefficient[ grepl( "tree", L206.AgCoef_Fert_bio$AgSupplySubsector ) ] <- bio_tree_Fert_IO_kgNGJ

#Adjustments to NonLandVariableCosts (75USD / kg crop) for all regions, crops, GLUs, and years
printlog( "Table L206.AgCost_ag_adj: Adjusted NonLandVariableCost by region / crop / year / GLU" )
L206.AgCost_ag_adj <- L205.AgCost_ag

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L206.AgCost_ag_adj$FertCost <- L206.AgCoef_Fert_ag$coefficient[
  match( vecpaste( L206.AgCost_ag_adj[ names_AgTechYr ] ),
         vecpaste( L206.AgCoef_Fert_ag[ names_AgTechYr ] ) ) ] *
  Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Where the fert cost was 0 and the N fertilizer object is not created, we get missing values. Re-set the fert cost to 0
L206.AgCost_ag_adj$FertCost[ is.na( L206.AgCost_ag_adj$FertCost ) ] <- 0

#Subtract fertilizer cost from other nonlandvariablecost and round
L206.AgCost_ag_adj$nonLandVariableCost <- round( L205.AgCost_ag$nonLandVariableCost - L206.AgCost_ag_adj$FertCost, digits_calPrice )
L206.AgCost_ag_adj <- L206.AgCost_ag_adj[ names_AgCost ]

#NOTE: these non-land variable costs may be negative, where the fertilizer downscaling algorithm assigned high fertilizer coefs
# to technologies with otherwise low production costs. Not worrying about this for now.

#Adjustments to NonLandVariableCosts (75USD / GJ biomass) for bioenergy crops in all regions, crops, GLUs, and years
printlog( "Table L206.AgCost_bio_adj: Adjusted NonLandVariableCost by region / crop / year / GLU" )
L206.AgCost_bio_adj <- L205.AgCost_bio

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L206.AgCost_bio_adj$FertCost <- L206.AgCoef_Fert_bio$coefficient * Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Subtract fertilizer cost from other nonlandvariablecost and round
L206.AgCost_bio_adj$nonLandVariableCost <- round( L205.AgCost_bio$nonLandVariableCost - L206.AgCost_bio_adj$FertCost, digits_calPrice )
L206.AgCost_bio_adj <- L206.AgCost_bio_adj[ names_AgCost ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L206.AgCoef_Fert_ag, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L206.AgCoef_Fert_ag",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_Fert.xml" ) 
write_mi_data( L206.AgCoef_Fert_bio, "AgCoef", "AGLU_LEVEL2_DATA", "L206.AgCoef_Fert_bio", "AGLU_XML_BATCH", "batch_ag_Fert.xml" ) 
write_mi_data( L206.AgCost_ag_adj, "AgCost", "AGLU_LEVEL2_DATA", "L206.AgCost_ag_adj", "AGLU_XML_BATCH", "batch_ag_Fert.xml" ) 
write_mi_data( L206.AgCost_bio_adj, "AgCost", "AGLU_LEVEL2_DATA", "L206.AgCost_bio_adj", "AGLU_XML_BATCH", "batch_ag_Fert.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_Fert.xml", "AGLU_XML_FINAL", "ag_Fert.xml", "", xml_tag="outFile" )

logstop()
