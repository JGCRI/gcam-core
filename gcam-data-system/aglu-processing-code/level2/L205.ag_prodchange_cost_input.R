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
logstart( "L205.ag_prodchange_cost_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for agricultural costs and productivity change" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_bio_cost_yield <- readdata( "AGLU_ASSUMPTIONS", "A_bio_cost_yield" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
L114.ag_YieldRatio_R_C_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L114.ag_YieldRatio_R_C_Y_AEZ_ref" )
L114.bio_YieldRatio_R_AEZ_Y_ref <- readdata( "AGLU_LEVEL1_DATA", "L114.bio_YieldRatio_R_AEZ_Y_ref" )
L122.ag_EcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L133.ag_Cost_75USDkg_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C_AEZ" )

# -----------------------------------------------------------------------------

printlog( "Calculating crop yields in all time periods for the reference scenario" )
L205.ag_EcYield_kgm2_R_C_Y_AEZ <- L122.ag_EcYield_kgm2_R_C_Y_AEZ[
      names( L122.ag_EcYield_kgm2_R_C_Y_AEZ ) %in% c( R_C_AEZ, X_model_base_years, X_model_future_years ) ]
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt <- melt( L114.ag_YieldRatio_R_C_Y_AEZ_ref, id.vars = R_C_Y, variable_name = AEZ )
#Only use the yield ratios in years beyond the historical time series, for which actual yield data is available
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt <- subset( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt, year > max( historical_years ) )
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$Xyear <- paste0( "X", L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$year )
L205.ag_YieldRatio_R_C_Y_AEZ_ref <- cast( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt, GCAM_region_ID + GCAM_commodity + AEZ ~ Xyear )

#Only multiply in the ratios for "model future years" that are greater than the last historical year
#Distinction applies for using earlier final calibration years
X_yieldratio_years <- sort( unique( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$Xyear ) )
L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_yieldratio_years ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[[ X_final_historical_year ]] *
      L205.ag_YieldRatio_R_C_Y_AEZ_ref[
           match( vecpaste( L205.ag_EcYield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ), vecpaste( L205.ag_YieldRatio_R_C_Y_AEZ_ref[ R_C_AEZ ] ) ),
      X_yieldratio_years ]

#Some small region x crop combinations aren't in the future yield improvement dataset. Copy yields forward from final historical period
L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[ !complete.cases( L205.ag_EcYield_kgm2_R_C_Y_AEZ ), ]
L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs[ X_yieldratio_years ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs[[ X_final_historical_year ]]
L205.ag_EcYield_kgm2_R_C_Y_AEZ[ !complete.cases( L205.ag_EcYield_kgm2_R_C_Y_AEZ ), ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs

printlog( "Translating yields to rates of agricultural productivity change" )
#Build a dataframe of the timestep lengths, of the same dimensions as the yield dataframe (except without the initial period)
timesteps <- model_years[ 2:length( model_years ) ] - model_years[ 1:( length( model_years ) -1 ) ]
names( timesteps ) <- X_model_years[ 2:length( X_model_years ) ]
df.timesteps <- t( as.data.frame( timesteps ) )
df.timesteps <- df.timesteps[ rep( 1, length.out = nrow( L205.ag_EcYield_kgm2_R_C_Y_AEZ ) ), ]

#Calculate the yield ratio from the prior period in the yield dataframe
L205.ag_YieldRatio_R_C_Y_AEZ_ref <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[ c( R_C_AEZ, X_model_years[ 2:length( X_model_years ) ] ) ]
L205.ag_YieldRatio_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] /
      L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L205.ag_YieldRatio_R_C_Y_AEZ_ref[ is.na( L205.ag_YieldRatio_R_C_Y_AEZ_ref ) ] <- 1

#Solve for the yield rate using the ratio and the timestep
L205.ag_YieldRate_R_C_Y_AEZ_ref <- L205.ag_YieldRatio_R_C_Y_AEZ_ref
L205.ag_YieldRate_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      ( L205.ag_YieldRatio_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] ) ^
      (1 / df.timesteps ) - 1

printlog( "Table L205.AgProdChange_ag_ref: Reference scenario ag prod change (not incl biomass)" )
L205.ag_YieldRate_R_C_Y_AEZ_ref.melt <- melt( L205.ag_YieldRate_R_C_Y_AEZ_ref, id.vars = R_C_AEZ, variable_name = Y )
L205.ag_YieldRate_R_C_Y_AEZ_ref.melt[[Y]] <- sub( "X", "", L205.ag_YieldRate_R_C_Y_AEZ_ref.melt[[Y]] )
L205.ag_YieldRate_R_C_Y_AEZ_ref.melt <- add_region_name( L205.ag_YieldRate_R_C_Y_AEZ_ref.melt )
L205.ag_YieldRate_R_C_Y_AEZ_ref.melt <- add_agtech_names( L205.ag_YieldRate_R_C_Y_AEZ_ref.melt )
names( L205.ag_YieldRate_R_C_Y_AEZ_ref.melt )[ names( L205.ag_YieldRate_R_C_Y_AEZ_ref.melt ) == "value" ] <- "AgProdChange"
L205.AgProdChange_ag_ref <- L205.ag_YieldRate_R_C_Y_AEZ_ref.melt[
      L205.ag_YieldRate_R_C_Y_AEZ_ref.melt$year %in% model_future_years, names_AgProdChange ]
#If the final calibration year is less than the final historical year, this method will return Inf for crops that are 0 in one year
# and non-zero in subsequent years. e.g. Korea and FSU FodderGrass. Setting the agprodchange to 0, and keeping these techs out.
L205.AgProdChange_ag_ref[ L205.AgProdChange_ag_ref == Inf ] <- 0

printlog( "Translating bioenergy crop yield ratios to annualized improvement rates in specified time periods" )
L205.bio_YieldRatio_R_Y_AEZ_ref.melt <- melt( L114.bio_YieldRatio_R_AEZ_Y_ref, id.vars = R_Y, variable_name = AEZ )
#Only use the yield ratios in years after the introduction of biomass
L205.bio_YieldRatio_R_Y_AEZ_ref.melt <- subset( L205.bio_YieldRatio_R_Y_AEZ_ref.melt, year > Bio_start_year & year %in% model_future_years )
L205.bio_YieldRatio_R_Y_AEZ_ref.melt$Xyear <- paste0( "X", L205.bio_YieldRatio_R_Y_AEZ_ref.melt$year )
L205.bio_YieldRatio_R_Y_AEZ_ref <- cast( L205.bio_YieldRatio_R_Y_AEZ_ref.melt, GCAM_region_ID + AEZ ~ Xyear )
df.timesteps_bio <- df.timesteps[ rep( 1, length.out = nrow( L205.bio_YieldRatio_R_Y_AEZ_ref ) ),
      colnames( df.timesteps ) %in% names( L205.bio_YieldRatio_R_Y_AEZ_ref ) ]

#Solve for the yield rate using the ratio and the timestep
Bio_years <- model_future_years[ model_future_years >= Bio_start_year ]
X_Bio_years <- paste0( "X", Bio_years )
L205.bio_YieldRate_R_Y_AEZ_ref <- L205.bio_YieldRatio_R_Y_AEZ_ref
L205.bio_YieldRate_R_Y_AEZ_ref[ X_Bio_years[ 2:length( X_Bio_years ) ] ] <-
      ( L205.bio_YieldRatio_R_Y_AEZ_ref[ X_Bio_years[ 2:length( X_Bio_years ) ] ] ) ^
      (1 / df.timesteps_bio ) - 1

printlog( "Table L205.AgProdChange_bio_ref: Reference scenario ag prod change (biomass only)" )
L205.bio_YieldRate_R_Y_AEZ_ref <- data.frame( L205.bio_YieldRate_R_Y_AEZ_ref ) #Need to remove the info that cast left it with
L205.bio_YieldRate_R_Y_AEZ_ref[[C]] <- "biomass"
L205.bio_YieldRate_R_Y_AEZ_ref.melt <- melt( L205.bio_YieldRate_R_Y_AEZ_ref, id.vars = R_C_AEZ, variable_name = Y )
L205.bio_YieldRate_R_Y_AEZ_ref.melt[[Y]] <- sub( "X", "", L205.bio_YieldRate_R_Y_AEZ_ref.melt[[Y]] )
L205.bio_YieldRate_R_Y_AEZ_ref.melt <- add_region_name( L205.bio_YieldRate_R_Y_AEZ_ref.melt )
L205.bio_YieldRate_R_Y_AEZ_ref.melt <- add_agtech_names( L205.bio_YieldRate_R_Y_AEZ_ref.melt )
names( L205.bio_YieldRate_R_Y_AEZ_ref.melt )[ names( L205.bio_YieldRate_R_Y_AEZ_ref.melt ) == "value" ] <- "AgProdChange"
L205.AgProdChange_bio_ref <- L205.bio_YieldRate_R_Y_AEZ_ref.melt[ names_AgProdChange ]

printlog( "Renaming specified bioenergy crops in the productivity change tables" )
L205.AgProdChange_bio_ref <- rename_biocrops( L205.AgProdChange_bio_ref, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

#COSTS
#Round costs to specified number of digits; this is ready for model input
L205.ag_Cost_75USDkg_C_AEZ <- L133.ag_Cost_75USDkg_C_AEZ
L205.ag_Cost_75USDkg_C_AEZ[ AEZs ] <- round( L205.ag_Cost_75USDkg_C_AEZ[ AEZs ], digits_calPrice )

printlog( "Table L205.AgCost_ag: costs of ag commodities (excl forest and biomass)" )
#Melt table of costs and write it out to all regions and time periods
L205.ag_Cost_75USDkg_C_AEZ.melt <- melt( L205.ag_Cost_75USDkg_C_AEZ, id.vars = C, variable_name = AEZ )
names( L205.ag_Cost_75USDkg_C_AEZ.melt )[ names( L205.ag_Cost_75USDkg_C_AEZ.melt ) == "value" ] <- "nonLandVariableCost"
L205.ag_Cost_75USDkg_C_AEZ.melt <- add_agtech_names( L205.ag_Cost_75USDkg_C_AEZ.melt )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR <- repeat_and_add_vector( L205.ag_Cost_75USDkg_C_AEZ.melt, R, GCAM_region_names[[R]] )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR <- add_region_name( L205.ag_Cost_75USDkg_C_AEZ.melt_repR )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR_Y <- repeat_and_add_vector( L205.ag_Cost_75USDkg_C_AEZ.melt_repR, Y, model_years )
L205.AgCost_ag <- L205.ag_Cost_75USDkg_C_AEZ.melt_repR_Y[ names_AgCost ]

printlog( "Table L205.AgCost_For: costs of forest production" )
L205.AgCost_For <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "Forest",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ) )
L205.AgCost_For <- add_agtech_names( L205.AgCost_For )
L205.AgCost_For <- repeat_and_add_vector( L205.AgCost_For, Y, model_years )
L205.AgCost_For$nonLandVariableCost <- cost_For_75USDm3
L205.AgCost_For <- L205.AgCost_For[ names_AgCost ]

printlog( "Table L205.AgCost_bio: costs of bioenergy production" )
L205.AgCost_bio <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "biomass",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ), stringsAsFactors = F )
L205.AgCost_bio <- add_agtech_names( L205.AgCost_bio )
L205.AgCost_bio <- repeat_and_add_vector( L205.AgCost_bio, Y, model_years )

#NOTE: costs differ for the different specified bioenergy crops, so replace the names prior to matching in costs
printlog( "Renaming specified bioenergy crops prior to matching in the costs" )
L205.AgCost_bio <- rename_biocrops( L205.AgCost_bio, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ) 

#Map in costs from assumptions table
L205.AgCost_bio$nonLandVariableCost <- A_bio_cost_yield$cost_USDGJ[
      match( L205.AgCost_bio$GCAM_commodity, A_bio_cost_yield$GCAM_commodity ) ]

#Reset costs in hi-cost AEZs
L205.AgCost_bio$nonLandVariableCost[ L205.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ] <- A_bio_cost_yield$cost_hi_USDGJ[
      match( L205.AgCost_bio$GCAM_commodity[ L205.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ],
             A_bio_cost_yield$GCAM_commodity ) ]

#Use lower costs in AEZ07 in USA and Canada
L205.AgCost_bio$nonLandVariableCost[ L205.AgCost_bio$AEZ == "AEZ07" &
      L205.AgCost_bio$region %in% c( "USA", "Canada" ) ] <- A_bio_cost_yield$cost_USDGJ[
           match( L205.AgCost_bio$GCAM_commodity[ L205.AgCost_bio$AEZ == "AEZ07" &
                  L205.AgCost_bio$region %in% c( "USA", "Canada" ) ], A_bio_cost_yield$GCAM_commodity ) ]
L205.AgCost_bio <- L205.AgCost_bio[ names_AgCost ]

printlog( "Removing non-existent AEZs from all tables")
L205.AgProdChange_ag_ref <- remove_AEZ_nonexist( L205.AgProdChange_ag_ref )
L205.AgProdChange_bio_ref <- remove_AEZ_nonexist( L205.AgProdChange_bio_ref )
L205.AgCost_ag <- remove_AEZ_nonexist( L205.AgCost_ag )
L205.AgCost_For <- remove_AEZ_nonexist( L205.AgCost_For )
L205.AgCost_bio <- remove_AEZ_nonexist( L205.AgCost_bio )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L205.AgProdChange_ag_ref, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ag_ref",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ref.xml" ) 
write_mi_data( L205.AgProdChange_bio_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L205.AgProdChange_bio_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref.xml" ) 
write_mi_data( L205.AgCost_ag, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_ag", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_bio, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_bio", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref.xml", "AGLU_XML_FINAL", "ag_prodchange_ref.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost.xml", "AGLU_XML_FINAL", "ag_cost.xml", "", xml_tag="outFile" )

logstop()
