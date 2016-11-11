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
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L112.ag_YieldRate_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L112.ag_YieldRate_R_C_Y_GLU" )
L112.bio_YieldRate_R_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L112.bio_YieldRate_R_Y_GLU" )
L113.ag_bioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_GLU" )
L122.ag_EcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_GLU" )
L123.For_Yield_m3m2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Yield_m3m2_R_GLU" )
L123.LC_bm2_R_MgdPast_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.LC_bm2_R_MgdPast_Yh_GLU" )
L133.ag_Cost_75USDkg_C <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C" )
L201.AgYield_bio_grass <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_grass", skip = 4 )
L201.AgYield_bio_tree <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_tree", skip = 4 )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )

# -----------------------------------------------------------------------------

#In case the model's final base year is less than the final historical year (for hindcasting runs), we need to
# compute the ag prod change levels that will return the known yields during the historical time period
if( max( model_base_years ) < final_historical_year ){
  X_yrs <- names( L122.ag_EcYield_kgm2_R_C_Y_GLU )[ names( L122.ag_EcYield_kgm2_R_C_Y_GLU ) %in% c( X_final_model_base_year, X_model_future_years ) ]
  yrs <- as.numeric(substr( X_yrs, 2, 5 ) )
  L205.ag_EcYieldRate_R_C_Y_GLU <- L122.ag_EcYield_kgm2_R_C_Y_GLU[ c( R_C_GLU, X_yrs ) ]
  for( i in 2:length( X_yrs ) ){
    L205.ag_EcYieldRate_R_C_Y_GLU[ X_yrs ][i] <- ( L122.ag_EcYield_kgm2_R_C_Y_GLU[ X_yrs ][i] / L122.ag_EcYield_kgm2_R_C_Y_GLU[ X_yrs ][i-1] ) ^
      ( 1 / ( yrs[i] - yrs[i-1] ) ) - 1
  }
  L205.ag_EcYieldRate_R_C_Y_GLU[ is.na( L205.ag_EcYieldRate_R_C_Y_GLU ) ] <- 0
  L205.ag_EcYieldRate_R_C_Y_GLU[ L205.ag_EcYieldRate_R_C_Y_GLU == Inf ] <- 0

  # merge this back into the L112 table of future yield improvement rates
  L112.ag_YieldRate_R_C_Y_GLU[ X_yrs ] <- L205.ag_EcYieldRate_R_C_Y_GLU[
    match( vecpaste( L112.ag_YieldRate_R_C_Y_GLU[ R_C_GLU ] ),
           vecpaste( L205.ag_EcYieldRate_R_C_Y_GLU[ R_C_GLU ] ) ),
    X_yrs ]
# need to have values for the (non-existent) bioenergy crops in the historical years run as future periods too
  L112.bio_YieldRate_R_Y_GLU[ X_yrs ] <- 0
}

printlog( "Table L205.AgProdChange_ag_ref: Reference scenario ag prod change (not incl biomass)" )
L205.AgProdChange_ag_ref <- interpolate_and_melt( L112.ag_YieldRate_R_C_Y_GLU,
                                               model_future_years, value.name = "AgProdChange", digits = digits_AgProdChange )
L205.AgProdChange_ag_ref <- add_region_name( L205.AgProdChange_ag_ref )
L205.AgProdChange_ag_ref <- add_agtech_names( L205.AgProdChange_ag_ref )[ names_AgProdChange ]

printlog( "L205.AgProdChange_bio_ref: Reference scenario ag prod change for biomass crops" )
L205.AgProdChange_bio_ref <- rbind(
  unique( L201.AgYield_bio_grass[ names_AgTech ] ),
  unique( L201.AgYield_bio_tree[ names_AgTech ] ) )
L205.AgProdChange_bio_ref <- repeat_and_add_vector( L205.AgProdChange_bio_ref, Y, model_future_years)

#Melt the table of yields so that these can be pasted in
L205.bio_YieldRate_R_Y_GLU.melt <- interpolate_and_melt( L112.bio_YieldRate_R_Y_GLU,
                                                         model_future_years, value.name = "AgProdChange", digits = digits_AgProdChange )
L205.bio_YieldRate_R_Y_GLU.melt <- add_region_name( L205.bio_YieldRate_R_Y_GLU.melt )

L205.AgProdChange_bio_ref <- substring_GLU( L205.AgProdChange_bio_ref, agtech )
L205.AgProdChange_bio_ref$AgProdChange <- L205.bio_YieldRate_R_Y_GLU.melt$AgProdChange[
  match( vecpaste( L205.AgProdChange_bio_ref[ c( reg, GLU ) ] ),
         vecpaste( L205.bio_YieldRate_R_Y_GLU.melt[ c( reg, GLU ) ] ) ) ]
L205.AgProdChange_bio_ref <- L205.AgProdChange_bio_ref[ names_AgProdChange ]

#Note: the ag prod change values for bioenergy crops are applied equally to grass and tree crops. Grass crops are available
# in any land use regions with crop production, and tree crops are available in any region with forests. Because the yield
# growth rates are based on crops, some places that have forests but no cropland will not have yield improvement rates.
# These regions are assumed minor agriculturally and as such not assigned yield improvement for tree-based bioenergy crops.
L205.AgProdChange_bio_ref[ is.na( L205.AgProdChange_bio_ref ) ] <- 0

#COSTS
# use the L103 production table to specify which region x glu x crop will need costs assigned
printlog( "Table L205.AgCost_ag: costs of crop production" )
L205.AgCost_ag <- unique( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] )
L205.AgCost_ag$nonLandVariableCost <- round( 
  L133.ag_Cost_75USDkg_C$Cost_75USDkg[
    match( L205.AgCost_ag[[C]], L133.ag_Cost_75USDkg_C[[C]] ) ],
  digits_calPrice )
L205.AgCost_ag <- add_region_name( L205.AgCost_ag )
L205.AgCost_ag <- add_agtech_names( L205.AgCost_ag )
L205.AgCost_ag <- repeat_and_add_vector( L205.AgCost_ag, Y, model_years )
L205.AgCost_ag <- L205.AgCost_ag[ names_AgCost ]

printlog( "Table L205.AgCost_bio: costs of bioenergy production" )
# Use the yield table from level1 to determine where bioenergy crops are being read in, and merge with the tech table to get both grass and tree crops
L205.AgCost_bio <- rbind(
  unique( L201.AgYield_bio_grass[ names_AgTech ] ),
  unique( L201.AgYield_bio_tree[ names_AgTech ] ) )
L205.AgCost_bio <- repeat_and_add_vector( L205.AgCost_bio, Y, model_years )

L205.AgCost_bio$nonLandVariableCost[ grepl( "grass", L205.AgCost_bio[[agtech]] ) ] <- bio_grass_Cost_75USD_GJ
L205.AgCost_bio$nonLandVariableCost[ grepl( "tree", L205.AgCost_bio[[agtech]] ) ] <- bio_tree_Cost_75USD_GJ

printlog( "Table L205.AgCost_Past: costs of Pasture production" )
# Note - this table is included so that a CO2 tag will be assigned to pasture technologies. Also in case we ever want to read in a cost.
L205.AgCost_Past <- repeat_and_add_vector( L123.LC_bm2_R_MgdPast_Yh_GLU[ R_LT_GLU ], Y, model_years )
L205.AgCost_Past <- add_region_name( L205.AgCost_Past )
L205.AgCost_Past[[C]] <- L205.AgCost_Past[[LT]]
L205.AgCost_Past <- add_agtech_names( L205.AgCost_Past )
L205.AgCost_Past$nonLandVariableCost <- cost_Past_75USDkg
L205.AgCost_Past <- L205.AgCost_Past[ names_AgCost ]

printlog( "Table L205.AgCost_For: costs of forest production" )
L205.AgCost_For <- repeat_and_add_vector( L123.For_Yield_m3m2_R_GLU[ R_C_GLU ], Y, model_years )
L205.AgCost_For <- add_region_name( L205.AgCost_For )
L205.AgCost_For <- add_agtech_names( L205.AgCost_For )
L205.AgCost_For$nonLandVariableCost <- cost_For_75USDm3
L205.AgCost_For <- L205.AgCost_For[ names_AgCost ]

printlog( "Table L205.AgProdChange_high: High ag prod change (not incl biomass)" )
L205.AgProdChange_high <- L205.AgProdChange_ag_ref
L205.AgProdChange_high$AgProdChange <- L205.AgProdChange_high$AgProdChange * hi_ag_prod_growth_mult

printlog( "Table L205.AgProdChange_low: Low ag prod change (not incl biomass)" )
L205.AgProdChange_low <- L205.AgProdChange_ag_ref
L205.AgProdChange_low$AgProdChange <- L205.AgProdChange_low$AgProdChange * low_ag_prod_growth_mult

printlog( "Table L205.AgProdChange_SSP4: SSP4 ag prod change (not incl biomass)" )
L205.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_Scen_R_Y, scenario == "SSP4" )
L205.pcgdp_2010 <- L205.pcgdp_2010[ names( L205.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L205.pcgdp_2010 <- add_region_name( L205.pcgdp_2010 )
L205.pcgdp_2010$X2010 <- L205.pcgdp_2010$X2010 * conv_1990_2010_USD
L205.high_reg <- L205.pcgdp_2010$region[ L205.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L205.low_reg <- L205.pcgdp_2010$region[ L205.pcgdp_2010$X2010 < lo_growth_pcgdp ]

L205.AgProdChange_ssp4_lo <- subset( L205.AgProdChange_low, L205.AgProdChange_low$region %in% L205.low_reg )
L205.AgProdChange_ssp4_hi <- subset( L205.AgProdChange_high, L205.AgProdChange_high$region %in% L205.high_reg )
L205.AgProdChange_ssp4_med <- subset( L205.AgProdChange_ag_ref, L205.AgProdChange_ag_ref$region %!in% c( L205.high_reg, L205.low_reg ) )
L205.AgProdChange_ssp4 <- rbind( L205.AgProdChange_ssp4_lo, L205.AgProdChange_ssp4_med, L205.AgProdChange_ssp4_hi )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L205.AgProdChange_ag_ref, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ag_ref",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ref.xml" ) 
write_mi_data( L205.AgProdChange_bio_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L205.AgProdChange_bio_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref.xml" ) 
write_mi_data( L205.AgCost_ag, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_ag", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_bio, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_bio", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_Past, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_Past", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref.xml", "AGLU_XML_FINAL", "ag_prodchange_ref.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost.xml", "AGLU_XML_FINAL", "ag_cost.xml", "", xml_tag="outFile" )

write_mi_data( L205.AgProdChange_high, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp1",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp1.xml" ) 
write_mi_data( L205.AgProdChange_ag_ref, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp2",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp2.xml" ) 
write_mi_data( L205.AgProdChange_low, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp3",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp3.xml" ) 
write_mi_data( L205.AgProdChange_ssp4, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp4",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp4.xml" ) 
write_mi_data( L205.AgProdChange_high, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp5",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp5.xml" ) 
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp1.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp2.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp4.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp5.xml", "", xml_tag="outFile" )


logstop()
