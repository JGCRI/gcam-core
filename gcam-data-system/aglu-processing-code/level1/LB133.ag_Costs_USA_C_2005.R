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
USDA_reg_AEZ <- readdata( "AGLU_MAPPINGS", "USDA_reg_AEZ" )
USDA_item_cost <- readdata( "AGLU_MAPPINGS", "USDA_item_cost" )
USDA_cost_data <- readdata( "AGLU_LEVEL0_DATA", "USDA_cost_data" )
L122.ag_EcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_AEZ" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Add vectors for GCAM commodity and cost component
USDA_cost_data[[C]] <- USDA_crops[[C]][ match( USDA_cost_data$Crop, USDA_crops$Crop ) ]
USDA_cost_data$cost_type <- USDA_item_cost$cost_type[ match( USDA_cost_data$Item, USDA_item_cost$Item ) ]

#Subset only the variable cost, and only in the relevant years
L133.ag_Cost_USDacr_C_Y_sR <- USDA_cost_data[ USDA_cost_data$cost_type == "variable",
      c( C, "Region", "Item", X_model_cost_years) ]

#Multiply dollars by GDP deflator to get 75 USD
#First, build dataframe of same dimensions for multiplication
printlog( "Converting nominal dollars to 1975 dollars" )
L133.GDP_deflators <- data.frame( X1996 = conv_1996_1975_USD, X1997 = conv_1997_1975_USD, X1998 = conv_1998_1975_USD,
     X1999 = conv_1999_1975_USD, X2000 = conv_2000_1975_USD, X2001 = conv_2001_1975_USD, X2002 = conv_2002_1975_USD,
     X2003 = conv_2003_1975_USD, X2004 = conv_2004_1975_USD, X2005 = conv_2005_1975_USD, X2006 = conv_2006_1975_USD,
     X2007 = conv_2007_1975_USD, X2008 = conv_2008_1975_USD, X2009 = conv_2009_1975_USD )

#Repeat by number of rows in cost dataframe
L133.GDP_deflators_repcost <- L133.GDP_deflators[ rep( 1, times = nrow( L133.ag_Cost_USDacr_C_Y_sR ) ) , ]

#Multiply deflator dataframe by cost dataframe to get converted costs
L133.ag_Cost_75USDacr_Cusda_Yusda_sR <- L133.ag_Cost_USDacr_C_Y_sR
L133.ag_Cost_75USDacr_Cusda_Yusda_sR[ X_model_cost_years ] <-
      L133.ag_Cost_USDacr_C_Y_sR[ X_model_cost_years ]*
      L133.GDP_deflators_repcost[ X_model_cost_years ]

#Compute average
printlog( "Calculating unweighted averages across specified years, by crop and subregion" )
L133.ag_Cost_75USDacr_Cusda_Yusda_sR$avg <- rowMeans( L133.ag_Cost_75USDacr_Cusda_Yusda_sR[ X_model_cost_years ], na.rm = TRUE )

#If all years are NA, set this to 0 (indicates a variable cost not disaggregated in the target years)
L133.ag_Cost_75USDacr_Cusda_Yusda_sR$avg[ is.na( L133.ag_Cost_75USDacr_Cusda_Yusda_sR$avg ) ] <- 0

#Aggregate interannual averages by crop and subregion (add the different cost components)
printlog( "Aggregating variable cost components" )
L133.ag_Cost_75USDacr_Cusda_sR <- aggregate( L133.ag_Cost_75USDacr_Cusda_Yusda_sR[ "avg" ],
      by=as.list( L133.ag_Cost_75USDacr_Cusda_Yusda_sR[ c( "Region", C ) ] ), sum ) 

#Map in AEZs and calculate average by AEZ
printlog( "Averaging subregions by AEZs" )
L133.ag_Cost_75USDacr_Cusda_sR[[AEZ]] <- USDA_reg_AEZ[[AEZ]][ match( L133.ag_Cost_75USDacr_Cusda_sR$Region, USDA_reg_AEZ$Region) ]

#Aggregate by AEZ, using "mean" function
L133.ag_Cost_75USDacr_Cusda_AEZ <- aggregate( L133.ag_Cost_75USDacr_Cusda_sR[ "avg" ],
      by=as.list( L133.ag_Cost_75USDacr_Cusda_sR[ c( "GCAM_commodity", "AEZ" ) ] ), mean )

#Convert to dollars per square meter and add ID vector
L133.ag_Cost_75USDm2_Cusda_AEZ <- L133.ag_Cost_75USDacr_Cusda_AEZ[ C_AEZ ]
L133.ag_Cost_75USDm2_Cusda_AEZ$Cost_75USDm2 <- L133.ag_Cost_75USDacr_Cusda_AEZ$avg * conv_m2_acr

#Map in yield in order to calculate cost per kg of crop produced
#First, melt the economic yield table and generate lookup vector
L133.ag_EcYield_kgm2_USA_C_fby_AEZ <- L122.ag_EcYield_kgm2_R_C_Y_AEZ[ L122.ag_EcYield_kgm2_R_C_Y_AEZ[[R]]==1, c( R_C_AEZ, X_final_cost_year ) ]

#Map in yields, and calculate cost per kg of crop produced
printlog( "Dividing by yields to calculate cost per unit of crop produced" )
L133.ag_Cost_75USDm2_Cusda_AEZ$Yield_kgm2 <- L133.ag_EcYield_kgm2_USA_C_fby_AEZ[[ X_final_cost_year ]][
      match( vecpaste( L133.ag_Cost_75USDm2_Cusda_AEZ[ C_AEZ ] ),
             vecpaste( L133.ag_EcYield_kgm2_USA_C_fby_AEZ[ C_AEZ ] ) ) ]
L133.ag_Cost_75USDkg_Cusda_AEZ.melt <- L133.ag_Cost_75USDm2_Cusda_AEZ[ C_AEZ ]
L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value <- L133.ag_Cost_75USDm2_Cusda_AEZ$Cost_75USDm2 / L133.ag_Cost_75USDm2_Cusda_AEZ$Yield_kgm2

#Several crops have very high costs in AEZ 12. Set these to the maximum in other AEZs.
printlog( "NOTE: Ad-hoc adjustment of costs of crop / AEZs with anomalously high costs" )
L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value[ L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]] == "FiberCrop" &
                                           L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[AEZ]] == "AEZ12" ] <-
      L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value[ L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]] == "FiberCrop" &
                                           L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[AEZ]] == "AEZ10" ]

L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value[ L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]] == "OtherGrain" &
                                           L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[AEZ]] == "AEZ12" ] <-
      L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value[ L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]] == "OtherGrain" &
                                           L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[AEZ]] == "AEZ07" ]

#WARNING: If any costs exceed the value of the product, GCAM will zero out the production in the given crop/AEZ
#Check this
if( any( L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value > L132.ag_an_For_Prices$calPrice[
         match( L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]], L132.ag_an_For_Prices[[C]] ) ] ) )
   {  printlog( "The following crop and AEZs have variable costs that exceed the value of the product" )
      print( L133.ag_Cost_75USDkg_Cusda_AEZ.melt[ L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value > L132.ag_an_For_Prices$calPrice[
         match( L133.ag_Cost_75USDkg_Cusda_AEZ.melt[[C]], L132.ag_an_For_Prices[[C]]) ], ] ) }

#Build table with all crops' costs, in all AEZs
all_commodities <- unique( L122.ag_EcYield_kgm2_R_C_Y_AEZ[[C]] )
L133.ag_Cost_75USDkg_C_AEZ.melt <- data.frame(
      GCAM_commodity = rep( all_commodities, times = length( AEZs ) ),
      AEZ = sort( rep( AEZs, times = length( all_commodities ) ) ) )
L133.ag_Cost_75USDkg_C_AEZ.melt$value <- L133.ag_Cost_75USDkg_Cusda_AEZ.melt$value[
      match( vecpaste( L133.ag_Cost_75USDkg_C_AEZ.melt[ C_AEZ ] ),
             vecpaste( L133.ag_Cost_75USDkg_Cusda_AEZ.melt[ C_AEZ ] ) ) ]
L133.ag_Cost_75USDkg_C_AEZ <- dcast( L133.ag_Cost_75USDkg_C_AEZ.melt, GCAM_commodity ~ AEZ )

#AD HOC FILLOUT OF CROP COST TABLE
#Start with temperate AEZs, filling out missing AEZs for crops with cost data
printlog( "Filling out costs for all crops and AEZs" )
printlog( "NOTE: This step requires a series of ad-hoc assumptions" )
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Corn" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Corn" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Corn" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] <-
      L133.ag_Cost_75USDkg_C_AEZ$AEZ07[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] +
      ( L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] -
        L133.ag_Cost_75USDkg_C_AEZ$AEZ07[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] ) * 1/3
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] <-
      L133.ag_Cost_75USDkg_C_AEZ$AEZ07[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] +
      ( L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] -
        L133.ag_Cost_75USDkg_C_AEZ$AEZ07[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FiberCrop" ] ) * 2/3
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OilCrop" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OilCrop" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OilCrop" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ11[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ12[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "OtherGrain" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ07[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] <- L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ]
L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] <- L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ]
L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] <- L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] +
      ( L133.ag_Cost_75USDkg_C_AEZ$AEZ12[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] -
        L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] ) * 1/3
L133.ag_Cost_75USDkg_C_AEZ$AEZ11[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] <- L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] +
      ( L133.ag_Cost_75USDkg_C_AEZ$AEZ12[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] -
        L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Rice" ] ) * 2/3
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ11[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ12[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "SugarCrop" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ09[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ08[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ] )
L133.ag_Cost_75USDkg_C_AEZ$AEZ11[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ] <-
      mean( L133.ag_Cost_75USDkg_C_AEZ$AEZ10[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ],
            L133.ag_Cost_75USDkg_C_AEZ$AEZ12[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Wheat" ] )

#For crops with no cost data, fill out using price ratios with the index crop
L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FodderGrass", AEZs_temp ] <-
      L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == Cost_index_crop, AEZs_temp ] *
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "FodderGrass" ] /
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == Cost_index_crop ]
L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "FodderHerb", AEZs_temp ] <-
      L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == Cost_index_crop, AEZs_temp ] *
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "FodderHerb" ] /
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == Cost_index_crop ]
L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "MiscCrop", AEZs_temp ] <-
      L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == Cost_index_crop, AEZs_temp ] *
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "MiscCrop" ] /
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == Cost_index_crop ]
L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "PalmFruit", AEZs_temp ] <-
      L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == Cost_index_crop, AEZs_temp ] *
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "PalmFruit" ] /
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == Cost_index_crop ]
L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == "Root_Tuber", AEZs_temp ] <-
      L133.ag_Cost_75USDkg_C_AEZ[ L133.ag_Cost_75USDkg_C_AEZ[[C]] == Cost_index_crop, AEZs_temp ] *
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "Root_Tuber" ] /
      L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == Cost_index_crop ]

#Apply costs in temperate AEZs to tropical and polar AEZs with no adjustment
L133.ag_Cost_75USDkg_C_AEZ[ AEZs_trop ] <- L133.ag_Cost_75USDkg_C_AEZ[ AEZs_temp ]
L133.ag_Cost_75USDkg_C_AEZ[ AEZs_pol ] <- L133.ag_Cost_75USDkg_C_AEZ[ AEZs_temp ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L133.ag_Cost_75USDkg_C_AEZ <- c( "Costs of GCAM commodities and AEZs","Units = 1975$/kg" )

writedata( L133.ag_Cost_75USDkg_C_AEZ, domain="AGLU_LEVEL1_DATA", fn="L133.ag_Cost_75USDkg_C_AEZ", comments=comments.L133.ag_Cost_75USDkg_C_AEZ )

# Every script should finish with this line
logstop()
                                                                                                                              