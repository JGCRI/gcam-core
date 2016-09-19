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
logstart( "L2061.ag_Fert_irr.R" )
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
L2051.AgCost_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_ag_irr", skip = 4 )
L2051.AgCost_bio_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_bio_irr", skip = 4 )
L206.AgCoef_Fert_ag <- readdata( "AGLU_LEVEL2_DATA", "L206.AgCoef_Fert_ag", skip = 4 )
L206.AgCoef_Fert_bio <- readdata( "AGLU_LEVEL2_DATA", "L206.AgCoef_Fert_bio", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Table L2061.AgCoef_Fert_ag_irr: Fertilizer IO coefs (copy from L206; no changes for irr/rfd)" )
# we don't currently have any data indicating that the fert IO coefs (kg N / kg crop) differ by irr/rfd
L2061.AgCoef_Fert_ag_irr <- repeat_and_add_vector( L206.AgCoef_Fert_ag, irr, c( "IRR", "RFD" ) )
L2061.AgCoef_Fert_ag_irr[[agtech]] <- paste( L2061.AgCoef_Fert_ag_irr[[agsubs]], L2061.AgCoef_Fert_ag_irr[[irr]], sep = irr_delimiter )
L2061.AgCoef_Fert_ag_irr[[irr]] <- NULL

printlog( "Table L2061.AgCoef_Fert_bio_irr: Fertilizer IO coefs (copy from L206; no changes for irr/rfd)" )
L2061.AgCoef_Fert_bio_irr <- repeat_and_add_vector( L206.AgCoef_Fert_bio, irr, c( "IRR", "RFD" ) )
L2061.AgCoef_Fert_bio_irr[[agtech]] <- paste( L2061.AgCoef_Fert_bio_irr[[agsubs]], L2061.AgCoef_Fert_bio_irr[[irr]], sep = irr_delimiter )
L2061.AgCoef_Fert_bio_irr[[irr]] <- NULL

#Adjustments to NonLandVariableCosts (75USD / kg crop) for all regions, crops, GLUs, and years
printlog( "Table L2061.AgCost_ag_irr_adj: Adjusted NonLandVariableCost by region / crop / year / GLU" )
L2061.AgCost_ag_irr_adj <- L2051.AgCost_ag_irr

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L2061.AgCost_ag_irr_adj$FertCost <- L2061.AgCoef_Fert_ag_irr$coefficient[
  match( vecpaste( L2061.AgCost_ag_irr_adj[ names_AgTechYr ] ),
         vecpaste( L2061.AgCoef_Fert_ag_irr[ names_AgTechYr ] ) ) ] *
  Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Where the fert cost was 0 and the N fertilizer object is not created, we get missing values. Re-set the fert cost to 0
L2061.AgCost_ag_irr_adj$FertCost[ is.na( L2061.AgCost_ag_irr_adj$FertCost ) ] <- 0

#Subtract fertilizer cost from other nonlandvariablecost and round
L2061.AgCost_ag_irr_adj$nonLandVariableCost <- round( L2051.AgCost_ag_irr$nonLandVariableCost - L2061.AgCost_ag_irr_adj$FertCost, digits_calPrice )
L2061.AgCost_ag_irr_adj <- L2061.AgCost_ag_irr_adj[ names_AgCost ]

#NOTE: these non-land variable costs may be negative, where the fertilizer downscaling algorithm assigned high fertilizer coefs
# to technologies with otherwise low production costs. Not worrying about this for now.
# 5/12/16: This only occurs in 8 out of 7192 cases (0.1%)

#Adjustments to NonLandVariableCosts (75USD / GJ biomass) for bioenergy crops in all regions, crops, GLUs, and years
printlog( "Table L2061.AgCost_bio_irr_adj: Adjusted NonLandVariableCost by region / crop / year / GLU" )
L2061.AgCost_bio_irr_adj <- L2051.AgCost_bio_irr

#Calculate the fertilizer-related deduction to costs: fertilizer price (converted to 75USD/kgN) times IO coef
L2061.AgCost_bio_irr_adj$FertCost <- L2061.AgCoef_Fert_bio_irr$coefficient[
  match( vecpaste( L2061.AgCost_bio_irr_adj[ names_AgTechYr ] ),
         vecpaste( L2061.AgCoef_Fert_bio_irr[ names_AgTechYr ] ) ) ] *
  Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Subtract fertilizer cost from other nonlandvariablecost and round
L2061.AgCost_bio_irr_adj$nonLandVariableCost <- round( L2051.AgCost_bio_irr$nonLandVariableCost - L2061.AgCost_bio_irr_adj$FertCost, digits_calPrice )
L2061.AgCost_bio_irr_adj <- L2061.AgCost_bio_irr_adj[ names_AgCost ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#writing out fert_bio table first, in order to preserve region order in final XML file
write_mi_data( L2061.AgCoef_Fert_ag_irr, IDstring="AgCoef", domain="AGLU_LEVEL2_DATA", fn="L2061.AgCoef_Fert_ag_irr",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_Fert_IRR.xml" ) 
write_mi_data( L2061.AgCoef_Fert_bio_irr, "AgCoef", "AGLU_LEVEL2_DATA", "L2061.AgCoef_Fert_bio_irr", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 
write_mi_data( L2061.AgCost_ag_irr_adj, "AgCost", "AGLU_LEVEL2_DATA", "L2061.AgCost_ag_irr_adj", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 
write_mi_data( L2061.AgCost_bio_irr_adj, "AgCost", "AGLU_LEVEL2_DATA", "L2061.AgCost_bio_irr_adj", "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_Fert_IRR.xml", "AGLU_XML_FINAL", "ag_Fert_IRR.xml", "", xml_tag="outFile" )

logstop()
