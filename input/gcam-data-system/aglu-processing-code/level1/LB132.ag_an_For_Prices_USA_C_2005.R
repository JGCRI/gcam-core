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
# FAO US producer price data updated in Jan 2017
FAO_USA_ag_an_P_USDt_PRICESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_ag_an_P_USDt_PRICESTAT" )
FAO_USA_For_Exp_t_USD_FORESTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_For_Exp_t_USD_FORESTAT" )
USDA_Alfalfa_prices_USDt <- readdata( "AGLU_LEVEL0_DATA", "USDA_Alfalfa_prices_USDt" )
# Use level0 production data instead of level1 with the 5-yr rolling average
FAO_ag_Prod_t_PRODSTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_Prod_t_PRODSTAT" )
FAO_USA_an_Prod_t_PRODSTAT <- readdata( "AGLU_LEVEL0_DATA", "FAO_USA_an_Prod_t_PRODSTAT" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Converting cotton back to primary equivalent (seed cotton)" )
X2008_X2011 <- c( "X2008", "X2009", "X2010", "X2011")
#Seed cotton has no price in PRICESTAT. Need to derive its price from cotton lint and cottonseed
FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Seed cotton", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X2008_X2011 ] <-
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cotton lint", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X2008_X2011 ] * conv_cotton_lint + 
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cottonseed", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X2008_X2011 ] * (1 - conv_cotton_lint )

printlog( "Assigning a price for game meat so that OtherMeat is assigned a price" )
FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Game meat", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X2008_X2011 ] <-
   FAO_USA_ag_an_P_USDt_PRICESTAT[
      FAO_USA_ag_an_P_USDt_PRICESTAT$item == "Cattle meat", names( FAO_USA_ag_an_P_USDt_PRICESTAT ) %in% X2008_X2011 ]


#Calculate unweighted averages for each FAO commodity over price years
#Subset only the relevant country/item combinations from the ag prodstat database
#NOTE: This mostly excludes fodder crops and "not elsewhere specified" crops. It also excludes about seven crops that are
#minor enough to not distort price estimates, but their omission may cause problems if a large number of commodities is used
L132.FAO_ag_Prod_t <- FAO_ag_Prod_t_PRODSTAT[
      vecpaste( FAO_ag_Prod_t_PRODSTAT[ c( "countries", "item" ) ] ) %in%
      vecpaste( FAO_USA_ag_an_P_USDt_PRICESTAT[ c( "countries", "item" ) ] ), ]

printlog( "Using 2008-2011 prices and production quantities of all commodities" )
FAO_USA_ag_an_P_USDt_PRICESTAT.melt <- FAO_USA_ag_an_P_USDt_PRICESTAT[ c( "countries", "item", X2008_X2011 )]
FAO_USA_ag_an_P_USDt_PRICESTAT.melt <- melt( FAO_USA_ag_an_P_USDt_PRICESTAT.melt, id = 1:2, value.name = "Price_USDt", variable.name = "year" )
L132.FAO_ag_Prod_t.melt <- L132.FAO_ag_Prod_t[ c( "countries", "item", X2008_X2011 )]
L132.FAO_ag_Prod_t.melt <- melt( L132.FAO_ag_Prod_t.melt, id = 1:2, value.name = "Prod_t", variable.name = "year" )
FAO_USA_an_Prod_t_PRODSTAT.melt <- FAO_USA_an_Prod_t_PRODSTAT[ c( "countries", "item", X2008_X2011 )]
FAO_USA_an_Prod_t_PRODSTAT.melt <- melt( FAO_USA_an_Prod_t_PRODSTAT.melt, id = 1:2, value.name = "Prod_t", variable.name = "year" )
FAO_USA_For_Exp_t_USD_FORESTAT.melt <- FAO_USA_For_Exp_t_USD_FORESTAT[ c( "countries", "item", "element", X2008_X2011 )]
FAO_USA_For_Exp_t_USD_FORESTAT.melt <- melt( FAO_USA_For_Exp_t_USD_FORESTAT.melt, id = 1:3, variable.name = "year" )

#Build tables with production and price, and calculate revenue, matching each year of 2008-2011
printlog( "Part 1: Primary agricultural goods and animal products" )
printlog( "Calculating revenue by commodity as production times price" )
#Primary agricultural goods
L132.ag_V_USA_Cfao_fby <- L132.FAO_ag_Prod_t.melt 
L132.ag_V_USA_Cfao_fby$Price_USDt <- FAO_USA_ag_an_P_USDt_PRICESTAT.melt$Price_USDt[
  match( paste( L132.ag_V_USA_Cfao_fby$item, L132.ag_V_USA_Cfao_fby$year), 
         paste( FAO_USA_ag_an_P_USDt_PRICESTAT.melt$item, FAO_USA_ag_an_P_USDt_PRICESTAT.melt$year ) ) ]
L132.ag_V_USA_Cfao_fby$V_USD <- L132.ag_V_USA_Cfao_fby$Prod_t * L132.ag_V_USA_Cfao_fby$Price_USDt
L132.ag_V_USA_Cfao_fby <- L132.ag_V_USA_Cfao_fby[ !is.na( L132.ag_V_USA_Cfao_fby$V_USD) , ] 

#Animal products
L132.an_V_USA_Cfao_fby <- FAO_USA_an_Prod_t_PRODSTAT.melt
L132.an_V_USA_Cfao_fby$Price_USDt <- FAO_USA_ag_an_P_USDt_PRICESTAT.melt$Price_USDt[
      match( paste( L132.an_V_USA_Cfao_fby$item, L132.an_V_USA_Cfao_fby$year), 
             paste( FAO_USA_ag_an_P_USDt_PRICESTAT.melt$item, FAO_USA_ag_an_P_USDt_PRICESTAT.melt$year ) ) ]
L132.an_V_USA_Cfao_fby$V_USD <- L132.an_V_USA_Cfao_fby$Prod_t * L132.an_V_USA_Cfao_fby$Price_USDt
L132.an_V_USA_Cfao_fby <- L132.an_V_USA_Cfao_fby[ !is.na( L132.an_V_USA_Cfao_fby$V_USD) , ] 

#Map in vectors of GCAM commodities and aggregate
L132.ag_V_USA_Cfao_fby[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L132.ag_V_USA_Cfao_fby$item, FAO_ag_items_PRODSTAT$item) ]
L132.an_V_USA_Cfao_fby[[C]] <- FAO_an_items_PRODSTAT[[C]][ match( L132.an_V_USA_Cfao_fby$item, FAO_an_items_PRODSTAT$item) ]

#Remove any fodder crops
L132.ag_V_USA_Cfao_fby <- L132.ag_V_USA_Cfao_fby[ !L132.ag_V_USA_Cfao_fby[[C]] %in% c( "FodderHerb", "FodderGrass" ), ]

#Aggregate by GCAM crop names and compute average prices, convert to 1975USD
conv_2010_1975_USD <- round( conv_1990_1975_USD / conv_1990_2010_USD, digits = 4 )
conv_2011_1975_USD <- 0.3036 # from BEA (2015), value in other years are slightly higher than those in the data system
printlog( "Aggregating by GCAM commodity and computing average prices" )
#Primary agricultural goods
L132.ag_V_USA_Cnf_fby <- aggregate( L132.ag_V_USA_Cfao_fby[ c( "Prod_t", "V_USD" ) ], by=as.list( L132.ag_V_USA_Cfao_fby[ c( C, "year" ) ] ), sum )
L132.ag_V_USA_Cnf_fby$Price_USDt <- L132.ag_V_USA_Cnf_fby$V_USD / L132.ag_V_USA_Cnf_fby$Prod_t
L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2008"] <- L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2008"] * conv_2008_1975_USD
L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2009"] <- L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2009"] * conv_2009_1975_USD
L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2010"] <- L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2010"] * conv_2010_1975_USD
L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2011"] <- L132.ag_V_USA_Cnf_fby$Price_USDt[ L132.ag_V_USA_Cnf_fby$year == "X2011"] * conv_2011_1975_USD
L132.ag_V_USA_Cnf_fby <- aggregate( L132.ag_V_USA_Cnf_fby[ c( "Price_USDt" ) ], by=as.list( L132.ag_V_USA_Cnf_fby[ C ] ), mean )

#Animal products
L132.an_V_USA_C_fby <- aggregate( L132.an_V_USA_Cfao_fby[ c( "Prod_t", "V_USD" ) ], by=as.list( L132.an_V_USA_Cfao_fby[ c( C, "year" ) ] ), sum )
L132.an_V_USA_C_fby$Price_USDt <- L132.an_V_USA_C_fby$V_USD / L132.an_V_USA_C_fby$Prod_t
L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2008"] <- L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2008"] * conv_2008_1975_USD
L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2009"] <- L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2009"] * conv_2009_1975_USD
L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2010"] <- L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2010"] * conv_2010_1975_USD
L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2011"] <- L132.an_V_USA_C_fby$Price_USDt[ L132.an_V_USA_C_fby$year == "X2011"] * conv_2011_1975_USD
L132.an_V_USA_C_fby <- aggregate( L132.an_V_USA_C_fby[ c( "Price_USDt" ) ], by=as.list( L132.an_V_USA_C_fby[ C ] ), mean )

printlog( "Part 2: Fodder crops and pasture" )
#Calculate average FodderHerb prices from 2008-2009 alfalfa prices, since 2010/11 is not available in data system
L132.ag_V_USA_FodderHerb_fby <- USDA_Alfalfa_prices_USDt[, c( "year", "avg" )]
L132.ag_V_USA_FodderHerb_fby$GCAM_commodity <- "FodderHerb"
L132.ag_V_USA_FodderHerb_fby <- subset( L132.ag_V_USA_FodderHerb_fby, year == 2008 | year == 2009 )
L132.ag_V_USA_FodderHerb_fby$Price_USDt[ L132.ag_V_USA_FodderHerb_fby$year == 2008 ] <- L132.ag_V_USA_FodderHerb_fby$avg[ L132.ag_V_USA_FodderHerb_fby$year == 2008 ] * conv_2008_1975_USD
L132.ag_V_USA_FodderHerb_fby$Price_USDt[ L132.ag_V_USA_FodderHerb_fby$year == 2009 ] <- L132.ag_V_USA_FodderHerb_fby$avg[ L132.ag_V_USA_FodderHerb_fby$year == 2009 ] * conv_2009_1975_USD
L132.ag_V_USA_FodderHerb_fby <- aggregate( L132.ag_V_USA_FodderHerb_fby[ c( "Price_USDt" ) ], by=as.list( L132.ag_V_USA_FodderHerb_fby[ C ] ), mean )
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
L132.For_V_USA_fby <- FAO_USA_For_Exp_t_USD_FORESTAT.melt[, c( "year", "element", "value" )]
L132.For_V_USA_fby$GCAM_commodity <- "Forest"
L132.For_V_USA_fby$element[ L132.For_V_USA_fby$element == "Export Quantity (m3)" ] <- "Exp_m3"
L132.For_V_USA_fby$element[ L132.For_V_USA_fby$element == "Export Value (1000 US$)" ] <- "ExpV_USD"
L132.For_V_USA_fby <- dcast( L132.For_V_USA_fby, GCAM_commodity + year ~ element )
L132.For_V_USA_fby$Price_USDm3 <- L132.For_V_USA_fby$ExpV_USD * 1000 / L132.For_V_USA_fby$Exp_m3
L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2008"] <- L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2008"] * conv_2008_1975_USD
L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2009"] <- L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2009"] * conv_2009_1975_USD
L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2010"] <- L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2010"] * conv_2010_1975_USD
L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2011"] <- L132.For_V_USA_fby$Price_USDm3[ L132.For_V_USA_fby$year == "X2011"] * conv_2011_1975_USD
L132.For_V_USA_fby <- aggregate( L132.For_V_USA_fby[ c( "Price_USDm3" ) ], by=as.list( L132.For_V_USA_fby[ C ] ), mean )

#Convert to model units, and combine into a single table
printlog( "Part 4: Converting to model units and merging into a single table" )
L132.ag_V_USA_C_fby$calPrice <- round( L132.ag_V_USA_C_fby$Price_USDt / conv_t_kg,
      digits = digits_calPrice )
L132.ag_V_USA_C_fby$unit <- "1975$/kg"
L132.an_V_USA_C_fby$calPrice <- round( L132.an_V_USA_C_fby$Price_USDt / conv_t_kg,
      digits = digits_calPrice)
L132.an_V_USA_C_fby$unit <- "1975$/kg"
L132.For_V_USA_fby$calPrice <- round( L132.For_V_USA_fby$Price_USDm3, digits = digits_calPrice )
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
