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
logstart( "LB132.ag_an_For_Prices_USA_C_2005.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Weighted average global prices of all GCAM commodities" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_an_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_an_items_PRODSTAT" )
FAO_USA_ag_an_P_USDt_PRICESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_ag_an_P_USDt_PRICESTAT" )
FAO_USA_For_Exp_t_USD_FORESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_For_Exp_t_USD_FORESTAT" )
USDA_Alfalfa_prices_USDt <- readdata( "AGLU_LEVEL0_DATA", "USDA_Alfalfa_prices_USDt" )
L100.FAO_ag_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Prod_t" )
FAO_USA_an_Prod_t_PRODSTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_an_Prod_t_PRODSTAT" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Converting cotton back to primary equivalent (seed cotton)" )
#Seed cotton has no price in PRICESTAT. Need to derive its price from cotton lint and cottonseed
FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Seed cotton", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X_AGLU_historical_years ] <-
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cotton lint", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X_AGLU_historical_years ] * conv_cotton_lint + 
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cottonseed", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X_AGLU_historical_years ] * (1 - conv_cotton_lint )

printlog( "Assigning a price for game meat so that OtherMeat is assigned a price" )
FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Game meat", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X_AGLU_historical_years ] <-
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cattle meat", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X_AGLU_historical_years ]


#Calculate unweighted averages for each FAO commodity over price years
#Subset only the relevant country/item combinations from the ag prodstat database
#NOTE: This mostly excludes fodder crops and "not elsewhere specified" crops. It also excludes about seven crops that are
#minor enough to not distort price estimates, but their omission may cause problems if a large number of commodities is used
L132.FAO_ag_Prod_t <- L100.FAO_ag_Prod_t[
      vecpaste( L100.FAO_ag_Prod_t[ c( "countries", "item" ) ] ) %in%
      vecpaste( FAO_USA_ag_an_P_USDt_PRICESTAT[ c( "countries", "item" ) ] ), ]

printlog( "Computing average prices and production quantities of all commodities" )
FAO_USA_ag_an_P_USDt_PRICESTAT$avg <- rowMeans( FAO_USA_ag_an_P_USDt_PRICESTAT[ X_model_price_years ], na.rm = TRUE )
L132.FAO_ag_Prod_t$avg <- rowMeans( L132.FAO_ag_Prod_t[ X_model_price_years ], na.rm = TRUE )
FAO_USA_an_Prod_t_PRODSTAT$avg <- rowMeans( FAO_USA_an_Prod_t_PRODSTAT[ X_model_price_years ], na.rm = TRUE )
FAO_USA_For_Exp_t_USD_FORESTAT$avg <- rowMeans( FAO_USA_For_Exp_t_USD_FORESTAT[ X_model_price_years ], na.rm = TRUE )

#Remove NA rows
FAO_USA_ag_an_P_USDt_PRICESTAT <- FAO_USA_ag_an_P_USDt_PRICESTAT[ !is.na( FAO_USA_ag_an_P_USDt_PRICESTAT$avg) , ] 
L132.FAO_ag_Prod_t <- L132.FAO_ag_Prod_t[ !is.na( L132.FAO_ag_Prod_t$avg) , ] 
FAO_USA_an_Prod_t_PRODSTAT <- FAO_USA_an_Prod_t_PRODSTAT[ !is.na( FAO_USA_an_Prod_t_PRODSTAT$avg) , ] 

#Build tables with production and price, and calculate revenue
printlog( "Part 1: Primary agricultural goods and animal products" )
printlog( "Calculating revenue by commodity as production times price" )
#Primary agricultural goods
L132.ag_V_USA_Cfao_fby <- data.frame( L132.FAO_ag_Prod_t[ c( "iso", "item" ) ],
      Prod_t = L132.FAO_ag_Prod_t$avg )
L132.ag_V_USA_Cfao_fby$Price_USDt <- FAO_USA_ag_an_P_USDt_PRICESTAT$avg[
      match( L132.ag_V_USA_Cfao_fby$item, FAO_USA_ag_an_P_USDt_PRICESTAT$item ) ]
L132.ag_V_USA_Cfao_fby <- L132.ag_V_USA_Cfao_fby[ !is.na( L132.ag_V_USA_Cfao_fby$Price_USDt) , ] 
L132.ag_V_USA_Cfao_fby$V_USD <- L132.ag_V_USA_Cfao_fby$Prod_t * L132.ag_V_USA_Cfao_fby$Price_USDt

#Animal products
L132.an_V_USA_Cfao_fby <- data.frame( FAO_USA_an_Prod_t_PRODSTAT[ c( "countries", "item" ) ],
      Prod_t = FAO_USA_an_Prod_t_PRODSTAT$avg )
L132.an_V_USA_Cfao_fby$Price_USDt <- FAO_USA_ag_an_P_USDt_PRICESTAT$avg[
      match( L132.an_V_USA_Cfao_fby$item, FAO_USA_ag_an_P_USDt_PRICESTAT$item ) ]
L132.an_V_USA_Cfao_fby <- L132.an_V_USA_Cfao_fby[ !is.na( L132.an_V_USA_Cfao_fby$Price_USDt) , ] 
L132.an_V_USA_Cfao_fby$V_USD <- L132.an_V_USA_Cfao_fby$Prod_t * L132.an_V_USA_Cfao_fby$Price_USDt

#Map in vectors of GCAM commodities and aggregate
L132.ag_V_USA_Cfao_fby[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L132.ag_V_USA_Cfao_fby$item, FAO_ag_items_PRODSTAT$item) ]
L132.an_V_USA_Cfao_fby[[C]] <- FAO_an_items_PRODSTAT[[C]][ match( L132.an_V_USA_Cfao_fby$item, FAO_an_items_PRODSTAT$item) ]

#Remove any fodder crops
L132.ag_V_USA_Cfao_fby <- L132.ag_V_USA_Cfao_fby[ !L132.ag_V_USA_Cfao_fby[[C]] %in% c( "FodderHerb", "FodderGrass" ), ]

# 9/21/2016 GPK - removing Sorghum because it is used as a fodder crop in the USA, and its prices are relatively low.
# The net effect of excluding it here is to raise the price of the OtherGrain commodity, and therefore the profit rate,
# which otherwise is the lowest of all crops. Any land use regions where this is dominant become bioenergy.
L132.ag_V_USA_Cfao_fby <- subset( L132.ag_V_USA_Cfao_fby, item != "Sorghum" )

#Aggregate by GCAM crop names and compute average prices
printlog( "Aggregating by GCAM commodity and computing average prices" )
#Primary agricultural goods
L132.ag_V_USA_Cnf_fby <- aggregate( L132.ag_V_USA_Cfao_fby[ c( "Prod_t", "V_USD" ) ], by=as.list( L132.ag_V_USA_Cfao_fby[ C ] ), sum )
L132.ag_V_USA_Cnf_fby$Price_USDt <- L132.ag_V_USA_Cnf_fby$V_USD / L132.ag_V_USA_Cnf_fby$Prod_t

#Animal products
L132.an_V_USA_C_fby <- aggregate( L132.an_V_USA_Cfao_fby[ c( "Prod_t", "V_USD" ) ], by=as.list( L132.an_V_USA_Cfao_fby[ C ] ), sum )
L132.an_V_USA_C_fby$Price_USDt <- L132.an_V_USA_C_fby$V_USD / L132.an_V_USA_C_fby$Prod_t

printlog( "Part 2: Fodder crops and pasture" )
#Calculate average FodderHerb prices from alfalfa prices
L132.ag_V_USA_FodderHerb_fby <- data.frame( GCAM_commodity = "FodderHerb", Prod_t = NA, V_USD = NA,
      Price_USDt = mean(USDA_Alfalfa_prices_USDt$avg[ USDA_Alfalfa_prices_USDt$year %in% model_price_years ] ) )
L132.ag_V_USA_FodderGrass_fby <- L132.ag_V_USA_FodderHerb_fby
L132.ag_V_USA_FodderGrass_fby$GCAM_commodity <- "FodderGrass"
L132.ag_V_USA_FodderGrass_fby$Price_USDt <- L132.ag_V_USA_FodderHerb_fby$Price_USDt * priceratio_grass_alfalfa
printlog( "NOTE: Setting Pasture price equal to FodderGrass price" )
L132.ag_V_USA_Past_fby <- L132.ag_V_USA_FodderGrass_fby
L132.ag_V_USA_Past_fby$GCAM_commodity <- "Pasture"

#Combine crop tables to get all primary agricultural commodities in a single table
L132.ag_V_USA_C_fby <- rbind( L132.ag_V_USA_Cnf_fby, L132.ag_V_USA_FodderHerb_fby, L132.ag_V_USA_FodderGrass_fby,
      L132.ag_V_USA_Past_fby)

printlog( "Part 3: Forest products" )
L132.For_V_USA_fby <- data.frame( GCAM_commodity = "Forest",
      Exp_m3 = FAO_USA_For_Exp_t_USD_FORESTAT$avg[ grepl( "Quantity", FAO_USA_For_Exp_t_USD_FORESTAT$element ) ],
      ExpV_USD = FAO_USA_For_Exp_t_USD_FORESTAT$avg[ grepl( "Value", FAO_USA_For_Exp_t_USD_FORESTAT$element ) ] * 1000 )
L132.For_V_USA_fby$Price_USDm3 <- L132.For_V_USA_fby$ExpV_USD / L132.For_V_USA_fby$Exp_m3

#Convert to model units, and combine into a single table
printlog( "Part 4: Converting to model units and merging into a single table" )
L132.ag_V_USA_C_fby$calPrice <- round( L132.ag_V_USA_C_fby$Price_USDt * conv_2004_1975_USD / conv_t_kg,
      digits = digits_calPrice )
L132.ag_V_USA_C_fby$unit <- "1975$/kg"
L132.an_V_USA_C_fby$calPrice <- round( L132.an_V_USA_C_fby$Price_USDt * conv_t_metric_short * conv_2004_1975_USD / conv_t_kg,
      digits = digits_calPrice) 
L132.an_V_USA_C_fby$unit <- "1975$/kg"
L132.For_V_USA_fby$calPrice <- round( L132.For_V_USA_fby$Price_USDm3 * conv_2004_1975_USD, digits = digits_calPrice ) 
L132.For_V_USA_fby$unit <- "1975$/m3"

pricecols <- c( C, "calPrice", "unit" )
L132.ag_an_For_Prices <- rbind( L132.ag_V_USA_C_fby[ pricecols ] ,
      L132.an_V_USA_C_fby[ pricecols ] ,
      L132.For_V_USA_fby[ pricecols ] )
      
printlog( "Checking that all commodities being modeled have prices assigned" )
commodities <- unique( c( FAO_ag_items_PRODSTAT[[C]], FAO_an_items_PRODSTAT[[C]] ) )      
commodities <- commodities[ !is.na( commodities ) ]      
if( any( commodities %!in% L132.ag_an_For_Prices[[C]] ) ){
	missing_commodities <- commodities[ commodities %!in% L132.ag_an_For_Prices[[C]] ]
	stop( "Missing commodity prices for ", missing_commodities )
}      

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L132.ag_an_For_Prices <- c( "Prices for all GCAM AGLU commodities","Units = 1975$/kg and 1975$/m3" )

writedata( L132.ag_an_For_Prices, domain="AGLU_LEVEL1_DATA", fn="L132.ag_an_For_Prices", comments=comments.L132.ag_an_For_Prices )

# Every script should finish with this line
logstop()
