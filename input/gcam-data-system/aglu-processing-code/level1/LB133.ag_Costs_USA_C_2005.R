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
logstart( "LB133.ag_Costs_USA_C_2005.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Non-land variable costs by commodity and AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
USDA_crops <- readdata( "AGLU_MAPPINGS", "USDA_crops" )
USDA_item_cost <- readdata( "AGLU_MAPPINGS", "USDA_item_cost" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
USDA_cost_data <- readdata( "AGLU_LEVEL0_DATA", "USDA_cost_data" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Add vectors for GCAM commodity and cost component
USDA_cost_data[ c( C, "GTAP_crop" ) ] <- USDA_crops[
  match( USDA_cost_data$Crop, USDA_crops$USDA_crop ),
  c( C, "GTAP_crop" ) ]
USDA_cost_data$cost_type <- USDA_item_cost$cost_type[ match( USDA_cost_data$Item, USDA_item_cost$Item ) ]

#Subset only the variable cost, and only in the relevant years
L133.ag_Cost_USDacr_C_Y <- USDA_cost_data[ USDA_cost_data$cost_type == "variable",
      c( C, "GTAP_crop", "Item", X_model_cost_years) ]

#Multiply dollars by GDP deflator to get 75 USD
#First, build dataframe of same dimensions for multiplication
printlog( "Converting nominal dollars to 1975 dollars" )
L133.GDP_deflators <- data.frame( X1996 = conv_1996_1975_USD, X1997 = conv_1997_1975_USD, X1998 = conv_1998_1975_USD,
     X1999 = conv_1999_1975_USD, X2000 = conv_2000_1975_USD, X2001 = conv_2001_1975_USD, X2002 = conv_2002_1975_USD,
     X2003 = conv_2003_1975_USD, X2004 = conv_2004_1975_USD, X2005 = conv_2005_1975_USD, X2006 = conv_2006_1975_USD,
     X2007 = conv_2007_1975_USD, X2008 = conv_2008_1975_USD, X2009 = conv_2009_1975_USD )

#Repeat by number of rows in cost dataframe
L133.GDP_deflators_repcost <- L133.GDP_deflators[ rep( 1, times = nrow( L133.ag_Cost_USDacr_C_Y ) ) , ]

#Multiply deflator dataframe by cost dataframe to get converted costs
L133.ag_Cost_75USDm2_Cusda_Yusda <- L133.ag_Cost_USDacr_C_Y
L133.ag_Cost_75USDm2_Cusda_Yusda[ X_model_cost_years ] <-
      L133.ag_Cost_USDacr_C_Y[ X_model_cost_years ]*
      L133.GDP_deflators_repcost[ X_model_cost_years ]

#Compute average
printlog( "Calculating unweighted averages across specified years, by crop and subregion" )
L133.ag_Cost_75USDm2_Cusda_Yusda$Cost_75USDm2 <- rowMeans( L133.ag_Cost_75USDm2_Cusda_Yusda[ X_model_cost_years ], na.rm = TRUE ) * conv_m2_acr

#If all years are NA, set this to 0 (indicates a variable cost not disaggregated in the target years)
L133.ag_Cost_75USDm2_Cusda_Yusda$Cost_75USDm2[ is.na( L133.ag_Cost_75USDm2_Cusda_Yusda$Cost_75USDm2 ) ] <- 0

#Aggregate interannual averages by crop and subregion (add the different cost components)
printlog( "Aggregating variable cost components" )
L133.ag_Cost_75USDm2_Cusda <- aggregate( L133.ag_Cost_75USDm2_Cusda_Yusda[ "Cost_75USDm2" ],
      by = L133.ag_Cost_75USDm2_Cusda_Yusda[ c( C, "GTAP_crop" ) ], sum ) 

printlog( "Matching in base-year harvested area, for weighted average price calculation (where necessary)" )
L133.LDS_ag_HA_ha_USA <- subset( L100.LDS_ag_HA_ha, iso == "usa" & GTAP_crop %in% L133.ag_Cost_75USDm2_Cusda$GTAP_crop )
L133.LDS_ag_HA_ha_USA <- aggregate( L133.LDS_ag_HA_ha_USA[ "value" ],
                                    by = L133.LDS_ag_HA_ha_USA[ "GTAP_crop" ], sum )

L133.ag_Cost_75USDm2_Cusda$HA_bm2 <- L133.LDS_ag_HA_ha_USA$value[
  match( L133.ag_Cost_75USDm2_Cusda$GTAP_crop, L133.LDS_ag_HA_ha_USA$GTAP_crop ) ] * conv_Ha_bm2
L133.ag_Cost_75USDm2_Cusda$Expenditures_bil75USD <- with( L133.ag_Cost_75USDm2_Cusda, Cost_75USDm2 * HA_bm2 )
L133.ag_Cost_75USDm2_C <- aggregate( L133.ag_Cost_75USDm2_Cusda[ c( "HA_bm2", "Expenditures_bil75USD" ) ],
                                     by = L133.ag_Cost_75USDm2_Cusda[C], sum )
L133.ag_Cost_75USDm2_C$Cost_75USDm2 <- with( L133.ag_Cost_75USDm2_C, Expenditures_bil75USD / HA_bm2 )

printlog( "Computing national average yields to translate costs per m2 to costs per kg")
L133.LDS_ag_prod_t_USA <- subset( L100.LDS_ag_prod_t, iso == "usa" & GTAP_crop %in% L133.ag_Cost_75USDm2_Cusda$GTAP_crop )
L133.LDS_ag_prod_t_USA[[C]] <- USDA_crops[[C]][ match( L133.LDS_ag_prod_t_USA$GTAP_crop, USDA_crops$GTAP_crop ) ]
L133.ag_Prod_Mt_USA_C <- aggregate( L133.LDS_ag_prod_t_USA[ "value" ] * conv_t_Mt,
                                    by = L133.LDS_ag_prod_t_USA[ C ], sum )

L133.ag_Cost_75USDm2_C$Prod_Mt <- L133.ag_Prod_Mt_USA_C$value[
  match( L133.ag_Cost_75USDm2_C[[C]], L133.ag_Prod_Mt_USA_C[[C]] ) ]
L133.ag_Cost_75USDm2_C$Yield_kgm2 <- with( L133.ag_Cost_75USDm2_C, Prod_Mt / HA_bm2 )
L133.ag_Cost_75USDm2_C$Cost_75USDkg <- with( L133.ag_Cost_75USDm2_C, Cost_75USDm2 / Yield_kgm2 )

printlog( "Checking to make sure that none of these costs are below the commodity prices minus some min-profit level" )
L133.ag_Cost_75USDm2_C$calPrice <- L132.ag_an_For_Prices$calPrice[
  match( L133.ag_Cost_75USDm2_C[[C]], L132.ag_an_For_Prices[[C]] ) ]
L133.ag_Cost_75USDm2_C$Cost_75USDkg <- pmin(
  L133.ag_Cost_75USDm2_C$Cost_75USDkg,
  L133.ag_Cost_75USDm2_C$calPrice * ( 1 - min_profit_margin ) )

#For remaining crops, calculate the cost required to return the weighted average land profit rate
L133.ag_Cost_75USDm2_C$Revenue_bil75USD <- with( L133.ag_Cost_75USDm2_C, Prod_Mt * calPrice )
L133.AvgProfit_75USDm2 <- with( L133.ag_Cost_75USDm2_C, ( sum( Revenue_bil75USD ) - sum( Expenditures_bil75USD ) ) / sum( HA_bm2 ) )

# Cost = Price - (Profit / Yield). Need to get the yields of these commodities
names( L100.LDS_ag_HA_ha )[ names( L100.LDS_ag_HA_ha ) == "value" ] <- "HA_ha"
names( L100.LDS_ag_prod_t )[ names( L100.LDS_ag_prod_t ) == "value" ] <- "Prod_t"
L133.LDS_usa <- merge( subset( L100.LDS_ag_HA_ha, iso == "usa" | iso == "idn" & GTAP_crop == "OilPalmFruit" ),
                       subset( L100.LDS_ag_prod_t, iso == "usa" | iso == "idn" & GTAP_crop == "OilPalmFruit"  ) )
L133.LDS_usa[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L133.LDS_usa$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L133.LDS_usa_oth <- subset( L133.LDS_usa, GCAM_commodity %!in% L133.ag_Cost_75USDm2_C[[C]] )
L133.ag_Cost_75USDkg_Cothr <- aggregate( L133.LDS_usa_oth[ c( "HA_ha", "Prod_t" ) ],
                                         by = L133.LDS_usa_oth[ C ], sum )
L133.ag_Cost_75USDkg_Cothr$Yield_kgm2 <- with( L133.ag_Cost_75USDkg_Cothr, Prod_t / HA_ha * conv_tha_kgm2 )
L133.ag_Cost_75USDkg_Cothr$calPrice <- L132.ag_an_For_Prices$calPrice[
  match( L133.ag_Cost_75USDkg_Cothr[[C]], L132.ag_an_For_Prices[[C]] ) ]
L133.ag_Cost_75USDkg_Cothr$Cost_75USDkg <- with( L133.ag_Cost_75USDkg_Cothr,
                                                 calPrice - L133.AvgProfit_75USDm2 / Yield_kgm2 )
L133.ag_Cost_75USDkg_C <- rbind(
  L133.ag_Cost_75USDm2_C[ c( C, "Cost_75USDkg" ) ],
  L133.ag_Cost_75USDkg_Cothr[ c( C, "Cost_75USDkg" ) ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L133.ag_Cost_75USDkg_C <- c( "Costs of GCAM commodities","Units = 1975$/kg" )

writedata( L133.ag_Cost_75USDkg_C, domain="AGLU_LEVEL1_DATA", fn="L133.ag_Cost_75USDkg_C", comments=comments.L133.ag_Cost_75USDkg_C )

# Every script should finish with this line
logstop()
                                                                                                                              
