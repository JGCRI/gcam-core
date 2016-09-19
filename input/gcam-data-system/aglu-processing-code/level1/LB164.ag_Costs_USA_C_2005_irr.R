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
logstart( "LB164.ag_Costs_USA_C_2005_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Non-land variable costs by commodity and GLU" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
USDA_crops <- readdata( "AGLU_MAPPINGS", "USDA_crops" )
USDA_item_cost <- readdata( "AGLU_MAPPINGS", "USDA_item_cost" )
USDA_cost_data <- readdata( "AGLU_LEVEL0_DATA", "USDA_cost_data" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L133.ag_Cost_75USDkg_C <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C" )
L161.ag_irrHA_frac_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_frac_R_C_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# The method here is to start from the cost file that has already been processed, and to deduct costs of purchased irrigation water
# from each cost estimate. This is less repetitive than following all of the same steps in that prior file, but with irrigation
# water mapped elsewhere.
printlog( "Computing the share of total variable costs that are from purchased irrigation water" )
L164.waterCostFrac_Cusda <- USDA_cost_data[ USDA_cost_data$Item == "Purchased irrigation water", c( "Crop", X_model_cost_years ) ]
L164.waterCostFrac_Cusda[ X_model_price_years ] <- L164.waterCostFrac_Cusda[ X_model_cost_years ] /
  USDA_cost_data[ match( paste( L164.waterCostFrac_Cusda$Crop, "Total operating costs" ),
                         vecpaste(USDA_cost_data[ c( "Crop", "Item" ) ] ) ),
                  X_model_cost_years ]
L164.waterCostFrac_Cusda$waterCostFrac <- rowMeans( L164.waterCostFrac_Cusda[ X_model_cost_years ], na.rm = T )
L164.waterCostFrac_Cusda[ c( "GTAP_crop", C ) ] <- USDA_crops[
  match( L164.waterCostFrac_Cusda$Crop, USDA_crops$USDA_crop ),
  c( "GTAP_crop", C ) ]

printlog( "Weighting USDA crops by harvested area in order to aggregate to GCAM commodities" )
L164.ag_HA_ha_usa_Cusda_GLU <- subset( L100.LDS_ag_HA_ha, GTAP_crop %in% USDA_crops$GTAP_crop & iso == "usa" )
L164.ag_HA_ha_usa_Cusda <- aggregate( L164.ag_HA_ha_usa_Cusda_GLU[ "value" ],
                                      by = L164.ag_HA_ha_usa_Cusda_GLU[ c( "iso", "GTAP_crop" ) ], sum )
L164.waterCostFrac_Cusda$weight <- L164.ag_HA_ha_usa_Cusda$value[
  match( L164.waterCostFrac_Cusda$GTAP_crop, L164.ag_HA_ha_usa_Cusda$GTAP_crop ) ]
L164.waterCostFrac_Cusda$waterCostFrac_wt <- with( L164.waterCostFrac_Cusda, waterCostFrac * weight )
L164.waterCostFrac_C <- aggregate( L164.waterCostFrac_Cusda[ c( "waterCostFrac_wt", "weight" ) ],
                                   by = L164.waterCostFrac_Cusda[ C ], sum )

printlog( "Computing weighted average water cost fraction by GCAM commodities that are represented in USDA cost spreadsheets")
L164.waterCostFrac_C$waterCostFrac <- with( L164.waterCostFrac_C, waterCostFrac_wt / weight )

printlog( "Computing water cost fractions for crops not in the USDA cost spreadsheets" )
L164.ag_irrHA_frac_USA_C_GLU <- subset( L161.ag_irrHA_frac_R_C_GLU, GCAM_region_ID == iso_GCAM_regID[[R]][ iso_GCAM_regID$iso == "usa" ] )
L164.ag_irrHA_frac_USA_C <- aggregate( L164.ag_irrHA_frac_USA_C_GLU[ c( "irrHA", "rfdHA" ) ],
                                       by = L164.ag_irrHA_frac_USA_C_GLU[ R_C ], sum )
L164.ag_irrHA_frac_USA_C$irrHA_frac <- with( L164.ag_irrHA_frac_USA_C, irrHA / ( irrHA + rfdHA ) )
L164.ag_irrHA_frac_USA_C$waterCostFrac <- L164.waterCostFrac_C$waterCostFrac[
  match( L164.ag_irrHA_frac_USA_C[[C]], L164.waterCostFrac_C[[C]] ) ]

#For the crops without missing values, make a simple linear regression to predict water cost fraction as a function of irrigation fraction
printlog( "Using linear regression to predict water cost fraction as a function of irrigated area fraction")
L164.lm_waterCostFrac <- glm( waterCostFrac ~ irrHA_frac, data = L164.ag_irrHA_frac_USA_C )
L164.ag_irrHA_frac_USA_C$waterCostFrac[ is.na( L164.ag_irrHA_frac_USA_C$waterCostFrac ) ] <-
  predict( L164.lm_waterCostFrac, type = "response",
           newdata = L164.ag_irrHA_frac_USA_C[ is.na( L164.ag_irrHA_frac_USA_C$waterCostFrac ), c( "irrHA_frac", "waterCostFrac" ) ] )

printlog( "Calculating the revised variable cost as the prior total minus the water cost fraction" )
L164.ag_Cost_75USDkg_C <- L133.ag_Cost_75USDkg_C
L164.ag_Cost_75USDkg_C$Cost_75USDkg <- L164.ag_Cost_75USDkg_C$Cost_75USDkg *
  ( 1 - L164.ag_irrHA_frac_USA_C$waterCostFrac[ match( L164.ag_Cost_75USDkg_C[[C]], L164.ag_irrHA_frac_USA_C[[C]] ) ] )

#For any crops not grown in the US, just assume no water cost deduction (not an issue w present dataset and mappings)
L164.ag_Cost_75USDkg_C$Cost_75USDkg[ is.na( L164.ag_Cost_75USDkg_C$Cost_75USDkg ) ] <-
  L133.ag_Cost_75USDkg_C$Cost_75USDkg[ is.na( L164.ag_Cost_75USDkg_C$Cost_75USDkg ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L164.ag_Cost_75USDkg_C <- c( "Production costs of GCAM commodities not including purchased irrigation water","Units = 1975$/kg" )

writedata( L164.ag_Cost_75USDkg_C, domain="AGLU_LEVEL1_DATA", fn="L164.ag_Cost_75USDkg_C", comments=comments.L164.ag_Cost_75USDkg_C )

# Every script should finish with this line
logstop()
                                                                                                                              
