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
logstart( "L2051.ag_prodchange_cost_input_irr.R" )
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
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU", replace_GLU = T )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU", replace_GLU = T )
L162.ag_YieldRate_R_C_Y_GLU_irr <- readdata( "AGLU_LEVEL1_DATA", "L162.ag_YieldRate_R_C_Y_GLU_irr", replace_GLU = T )
L162.bio_YieldRate_R_Y_GLU_irr <- readdata( "AGLU_LEVEL1_DATA", "L162.bio_YieldRate_R_Y_GLU_irr", replace_GLU = T )
L163.ag_irrBioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_GLU", replace_GLU = T )
L163.ag_rfdBioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_GLU", replace_GLU = T )
L164.ag_Cost_75USDkg_C <- readdata( "AGLU_LEVEL1_DATA", "L164.ag_Cost_75USDkg_C" )
L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_irrEcYield_kgm2_R_C_Y_GLU", replace_GLU = T )
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU", replace_GLU = T )
L205.AgProdChange_bio_ref <- readdata( "AGLU_LEVEL2_DATA", "L205.AgProdChange_bio_ref", skip = 4 )
L205.AgCost_bio <- readdata( "AGLU_LEVEL2_DATA", "L205.AgCost_bio", skip = 4 )
L205.AgCost_For <- readdata( "AGLU_LEVEL2_DATA", "L205.AgCost_For", skip = 4 )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables
#In case the model's final base year is less than the final historical year (for hindcasting runs), we need to
# compute the ag prod change levels that will return the known yields during the historical time period
if( max( model_base_years ) < final_historical_year ){
  L171.ag_irrEcYield_kgm2_R_C_Y_GLU[[irr]] <- "IRR"
  L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[[irr]] <- "RFD"
  L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr <- rbind( L171.ag_irrEcYield_kgm2_R_C_Y_GLU, L171.ag_rfdEcYield_kgm2_R_C_Y_GLU )
  X_yrs <- names( L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr )[ names( L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr ) %in% c( X_final_model_base_year, X_model_future_years ) ]
  yrs <- as.numeric(substr( X_yrs, 2, 5 ) )
  L2051.ag_EcYieldRate_R_C_Y_GLU_irr <- L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr[ c( R_C_GLU_irr, X_yrs ) ]
  for( i in 2:length( X_yrs ) ){
    L2051.ag_EcYieldRate_R_C_Y_GLU_irr[ X_yrs ][i] <- ( L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr[ X_yrs ][i] / L2051.ag_EcYield_kgm2_R_C_Y_GLU_irr[ X_yrs ][i-1] ) ^
      ( 1 / ( yrs[i] - yrs[i-1] ) ) - 1
  }
  L2051.ag_EcYieldRate_R_C_Y_GLU_irr[ is.na( L2051.ag_EcYieldRate_R_C_Y_GLU_irr ) ] <- 0
  L2051.ag_EcYieldRate_R_C_Y_GLU_irr[ L2051.ag_EcYieldRate_R_C_Y_GLU_irr == Inf ] <- 0
  
  # merge this back into the L162 table of future yield improvement rates
  L162.ag_YieldRate_R_C_Y_GLU_irr[ X_yrs ] <- L2051.ag_EcYieldRate_R_C_Y_GLU_irr[
    match( vecpaste( L162.ag_YieldRate_R_C_Y_GLU_irr[ R_C_GLU_irr ] ),
           vecpaste( L2051.ag_EcYieldRate_R_C_Y_GLU_irr[ R_C_GLU_irr ] ) ),
    X_yrs ]
  # need to have values for the (non-existent) bioenergy crops in the historical years run as future periods too
  L162.bio_YieldRate_R_Y_GLU_irr[ X_yrs ] <- 0
}

printlog( "Table L2051.AgProdChange_ag_irr_ref: Reference scenario ag prod change (not incl biomass)" )
L2051.AgProdChange_ag_irr_ref <- interpolate_and_melt( L162.ag_YieldRate_R_C_Y_GLU_irr,
                                                model_future_years, value.name = "AgProdChange", digits = digits_AgProdChange )
L2051.AgProdChange_ag_irr_ref <- add_region_name( L2051.AgProdChange_ag_irr_ref )
L2051.AgProdChange_ag_irr_ref <- add_agtech_names( L2051.AgProdChange_ag_irr_ref )[ names_AgProdChange ]
#If the final calibration year is less than the final historical year, this method will return Inf for crops that are 0 in one year
# and non-zero in subsequent years. e.g. Korea and FSU FodderGrass. Setting the agprodchange to 0, and keeping these techs out.
L2051.AgProdChange_ag_irr_ref[ L2051.AgProdChange_ag_irr_ref == Inf ] <- 0

printlog( "Table L2051.AgProdChange_bio_irr_ref: Reference scenario ag prod change for biomass" )
# Copy the prior prodchange file by irr and rfd, and then match in the yields from the L162 yield rate data
L2051.AgProdChange_bio_irr_ref <- repeat_and_add_vector( L205.AgProdChange_bio_ref, irr, c( "IRR", "RFD" ) )
L2051.AgProdChange_bio_irr_ref[[agtech]] <- paste( L2051.AgProdChange_bio_irr_ref[[agsubs]], L2051.AgProdChange_bio_irr_ref[[irr]], sep = irr_delimiter )

L2051.bio_YieldRate_R_Y_GLU_irr.melt <- interpolate_and_melt( L162.bio_YieldRate_R_Y_GLU_irr,
                                                              model_future_years, "AgProdChange", digits_AgProdChange )
L2051.bio_YieldRate_R_Y_GLU_irr.melt <- add_region_name( L2051.bio_YieldRate_R_Y_GLU_irr.melt )

L2051.AgProdChange_bio_irr_ref <- substring_GLU( L2051.AgProdChange_bio_irr_ref, agsubs )
L2051.AgProdChange_bio_irr_ref$AgProdChange <- L2051.bio_YieldRate_R_Y_GLU_irr.melt$AgProdChange[
  match( vecpaste( L2051.AgProdChange_bio_irr_ref[ c( reg, GLU, irr, Y ) ] ),
         vecpaste( L2051.bio_YieldRate_R_Y_GLU_irr.melt[ c( reg, GLU, irr, Y ) ] ) ) ]
L2051.AgProdChange_bio_irr_ref <- L2051.AgProdChange_bio_irr_ref[ names_AgProdChange ]
L2051.AgProdChange_bio_irr_ref[ is.na( L2051.AgProdChange_bio_irr_ref ) ] <- 0

#COSTS
# use the L161 production tables to specify which region x glu x crop will need costs assigned
printlog( "Table L2051.AgCost_ag_irr: costs of crop production" )
L161.ag_irrProd_Mt_R_C_Y_GLU[[irr]] <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_GLU[[irr]] <- "RFD"
L2051.ag_irrProd_Mt_R_C_Y_GLU <- rbind( L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU )

L2051.AgCost_ag_irr <- unique( L2051.ag_irrProd_Mt_R_C_Y_GLU[ R_C_GLU_irr ] )
L2051.AgCost_ag_irr$nonLandVariableCost <- round( 
  L164.ag_Cost_75USDkg_C$Cost_75USDkg[
    match( L2051.AgCost_ag_irr[[C]], L164.ag_Cost_75USDkg_C[[C]] ) ],
  digits_calPrice )
L2051.AgCost_ag_irr <- add_region_name( L2051.AgCost_ag_irr )
L2051.AgCost_ag_irr <- add_agtech_names( L2051.AgCost_ag_irr )
L2051.AgCost_ag_irr <- repeat_and_add_vector( L2051.AgCost_ag_irr, Y, model_years )
L2051.AgCost_ag_irr <- L2051.AgCost_ag_irr[ names_AgCost ]

printlog( "L2051.AgCost_bio_irr: no changes from corresponding L205 table, but repeat by irr/rfd and re-set tech names" )
L2051.AgCost_bio_irr <- repeat_and_add_vector( L205.AgCost_bio, irr, c( "IRR", "RFD" ) )
L2051.AgCost_bio_irr[[agtech]] <- paste( L2051.AgCost_bio_irr[[agsubs]], L2051.AgCost_bio_irr[[irr]], sep = irr_delimiter )
L2051.AgCost_bio_irr[[irr]] <- NULL

printlog( "L2051.AgCost_For: no changes from corresponding L205 table" )
L2051.AgCost_For <- L205.AgCost_For

printlog( "Table L2051.AgProdChange_irr_high: High ag prod change (not incl biomass)" )
L2051.AgProdChange_irr_high <- L2051.AgProdChange_ag_irr_ref
L2051.AgProdChange_irr_high$AgProdChange <- L2051.AgProdChange_irr_high$AgProdChange * hi_ag_prod_growth_mult

printlog( "Table L2051.AgProdChange_irr_low: Low ag prod change (not incl biomass)" )
L2051.AgProdChange_irr_low <- L2051.AgProdChange_ag_irr_ref
L2051.AgProdChange_irr_low$AgProdChange <- L2051.AgProdChange_irr_low$AgProdChange * low_ag_prod_growth_mult

printlog( "Table L2051.AgProdChange_irr_ssp4: SSP4 ag prod change (not incl biomass)" )
L225.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_Scen_R_Y, scenario == "SSP4" )
L225.pcgdp_2010 <- L225.pcgdp_2010[ names( L225.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L225.pcgdp_2010 <- add_region_name( L225.pcgdp_2010 )
L225.pcgdp_2010$X2010 <- L225.pcgdp_2010$X2010 * conv_1990_2010_USD
L225.high_reg <- L225.pcgdp_2010$region[ L225.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L225.low_reg <- L225.pcgdp_2010$region[ L225.pcgdp_2010$X2010 < lo_growth_pcgdp ]

L2051.AgProdChange_irr_ssp4_lo <- subset( L2051.AgProdChange_irr_low, L2051.AgProdChange_irr_low$region %in% L225.low_reg )
L2051.AgProdChange_irr_ssp4_hi <- subset( L2051.AgProdChange_irr_high, L2051.AgProdChange_irr_high$region %in% L225.high_reg )
L2051.AgProdChange_irr_ssp4_med <- subset( L2051.AgProdChange_ag_irr_ref, L2051.AgProdChange_ag_irr_ref$region %!in% c( L225.high_reg, L225.low_reg ) )
L2051.AgProdChange_irr_ssp4 <- rbind( L2051.AgProdChange_irr_ssp4_lo, L2051.AgProdChange_irr_ssp4_med, L2051.AgProdChange_irr_ssp4_hi )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2051.AgProdChange_ag_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_ag_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR.xml" ) 
write_mi_data( L2051.AgProdChange_bio_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_bio_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR.xml" ) 
write_mi_data( L2051.AgCost_ag_irr, "AgCost", "AGLU_LEVEL2_DATA", "L2051.AgCost_ag_irr", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 
write_mi_data( L2051.AgCost_bio_irr, "AgCost", "AGLU_LEVEL2_DATA", "L2051.AgCost_bio_irr", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 
write_mi_data( L2051.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L2051.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 

write_mi_data( L2051.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR.xml" ) 
write_mi_data( L2051.AgProdChange_ag_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_ag_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR.xml" ) 
write_mi_data( L2051.AgProdChange_irr_low, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_irr_low", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR.xml" ) 
write_mi_data( L2051.AgProdChange_irr_ssp4, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_irr_ssp4", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR.xml" ) 
write_mi_data( L2051.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L2051.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ref_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml", "AGLU_XML_FINAL", "ag_cost_IRR.xml", "", xml_tag="outFile" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp1_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp2_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp3_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp4_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp5_IRR.xml", "", xml_tag="outFile" )

logstop()
