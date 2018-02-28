# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L133.water.demand.livestock.R" )
printlog( "Calculate water demand coefficients for livestock by animal product" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_iso <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
L105.an_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Prod_Mt_R_C_Y" )
LivestockWaterFootprint_MH2010 <- readdata( "WATER_LEVEL0_DATA", "LivestockWaterFootprint_MH2010" )
FAO_an_items_Stocks <- readdata( "WATER_MAPPINGS", "FAO_an_items_Stocks" )
L100.FAO_an_Stocks <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Stocks" )
L100.FAO_an_Dairy_Stocks<- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Dairy_Stocks" )

# -----------------------------------------------------------------------------

# 2. Process data

printlog(" Adjust total animal stocks to reduce by dairy animals" )
# Note we are only using the year 2000 since that is the year for which the water
# coefficients are for.
L133.dairy_an_adj <- merge( L100.FAO_an_Dairy_Stocks[, c( "iso", "item", "X2000" ) ],
    FAO_an_items_Stocks[, c( "item", "dairy.to.total" ) ] )
L133.dairy_an_adj$dairy.adj <- L133.dairy_an_adj$X2000
L133.dairy_an_adj$item <- L133.dairy_an_adj$dairy.to.total
L133.dairy_an_adj <- L133.dairy_an_adj[, c( "iso", "item", "dairy.adj" ) ]
L133.FAO_an_heads <- merge( L100.FAO_an_Stocks, L133.dairy_an_adj, all.x=T )
L133.FAO_an_heads[ is.na( L133.FAO_an_heads$dairy.adj ), "dairy.adj" ] <- 0
L133.FAO_an_heads$X2000 <- L133.FAO_an_heads$X2000 - L133.FAO_an_heads$dairy.adj
# It seems the PDR stoppeed reporting data after 1994 for total livestock but not
# dairy which causes the adjustment to be negative.  Just zero it out for now
L133.FAO_an_heads[ L133.FAO_an_heads$X2000 < 0, "X2000"] <- 0

# Now that we have adjusted the total stock data we can combine it together
# with the dairy for the rest of the processing
L133.FAO_an_heads <- rbind( L133.FAO_an_heads[, c( "iso", "item", "X2000" ) ],
    L100.FAO_an_Dairy_Stocks[, c( "iso", "item", "X2000" ) ] )

printlog( "Calculate water demand by FAO item" )
L133.FAO_an_heads <- merge( L133.FAO_an_heads, FAO_an_items_Stocks )
L133.FAO_an_heads <- merge( L133.FAO_an_heads, LivestockWaterFootprint_MH2010 )
L133.FAO_an_heads <- merge( L133.FAO_an_heads, GCAM_region_iso )
# Coefficient is liters/head which is the same as m^3/1000 heads
# Coefficient was in terms of day so we convert water consumption / year
L133.FAO_an_heads$water.consumption <- L133.FAO_an_heads$X2000 * L133.FAO_an_heads$Coefficient / 1000 / conv_days_year

printlog( "Calculate water demand by GCAM_commodity" )
# Note aggregating water consumption by GCAM_commodity and averaging over total production means
# we are assuming zero water consumption for an FAO item that does not match and have a water coefficient
L133.water_demand_livestock_R_C_W_km3_Mt <- aggregate( water.consumption ~ GCAM_region_ID + GCAM_commodity, L133.FAO_an_heads, FUN=sum )
L133.water_demand_livestock_R_C_W_km3_Mt <- merge( L133.water_demand_livestock_R_C_W_km3_Mt, L105.an_Prod_Mt_R_C_Y[, c( R, "GCAM_commodity", "X2000" ) ] )
# Water consumption is in m^3 and production is in Mt to get to km^3/Mt we must divide by 1e9 
L133.water_demand_livestock_R_C_W_km3_Mt$coefficient <- L133.water_demand_livestock_R_C_W_km3_Mt$water.consumption / L133.water_demand_livestock_R_C_W_km3_Mt$X2000 / 1e9
L133.water_demand_livestock_R_C_W_km3_Mt[[water_type]] <- water_C
# Water withdrawls are the same as consumption for livestock so just copy the coefficients
L133.water_demand_livestock_R_C_W_km3_Mt.W <- L133.water_demand_livestock_R_C_W_km3_Mt
L133.water_demand_livestock_R_C_W_km3_Mt.W[[water_type]] <- water_W
L133.water_demand_livestock_R_C_W_km3_Mt <- rbind( L133.water_demand_livestock_R_C_W_km3_Mt, L133.water_demand_livestock_R_C_W_km3_Mt.W )

L133.water_demand_livestock_R_C_W_km3_Mt <- L133.water_demand_livestock_R_C_W_km3_Mt[, c( R, "GCAM_commodity", water_type, "coefficient" ) ]

# 3. Output

#Add comments for each table
comments.L133.water_demand_livestock_R_C_W_km3_Mt<- c( "Livestock water coefficients by region ID / GCAM_commodity/ water type","Unit = m^3 / Mt" )

#write tables as CSV files
writedata( L133.water_demand_livestock_R_C_W_km3_Mt, domain="WATER_LEVEL1_DATA", fn="L133.water_demand_livestock_R_C_W_km3_Mt", comments=comments.L133.water_demand_livestock_R_C_W_km3_Mt )

# Every script should finish with this line
logstop()
