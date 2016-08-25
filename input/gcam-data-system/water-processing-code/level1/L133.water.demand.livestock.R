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
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
L105.an_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Prod_Mt_R_C_Y" )
LivestockWaterFootprint_MH2010 <- readdata( "WATER_LEVEL0_DATA", "LivestockWaterFootprint_MH2010" )
# TODO: these should be in AGLU data?
FAO_an_items_Stocks <- readdata( "WATER_MAPPINGS", "FAO_an_items_Stocks" )
FAO.dairy <- readdata( "WATER_LEVEL0_DATA", "FAO.dairy" )
FAO.livestock <- readdata( "WATER_LEVEL0_DATA", "FAO.livestock" )

# -----------------------------------------------------------------------------

# 2. Process data
printlog( "Map FAO country to GCAM region ID" )
L133.FAO_an_heads <- rbind( FAO.dairy, FAO.livestock )
# TODO: why 2000?
L133.FAO_an_heads <- subset( L133.FAO_an_heads, Year == 2000 )
L133.FAO_ctry_GCAM_region_ID <- merge( GCAM_region_iso[, c("iso", R) ], AGLU_ctry[, c("iso", "FAO_country" ) ] )

printlog( "Calculate water demand by FAO item" )
L133.FAO_an_heads <- merge( L133.FAO_an_heads, L133.FAO_ctry_GCAM_region_ID, by.x="AreaName", by.y="FAO_country")
L133.FAO_an_heads <- merge( L133.FAO_an_heads, FAO_an_items_Stocks, by.x="ItemName", by.y="item" )
L133.FAO_an_heads <- merge( L133.FAO_an_heads, LivestockWaterFootprint_MH2010 )
# Note Value is in 1000 heads for Poultry and heads for everything else.
# Coefficient is liters/head which is the same as m^3/1000 heads
# So we will convert Value to 1000s of heads except for Poultry
L133.FAO_an_heads[ L133.FAO_an_heads$GCAM_commodity != "Poultry", "Value" ] <- 
    L133.FAO_an_heads[ L133.FAO_an_heads$GCAM_commodity != "Poultry", "Value" ] / 1000
L133.FAO_an_heads$water.consumption <- L133.FAO_an_heads$Value * L133.FAO_an_heads$Coefficient
# Coefficient was in terms of day so we convert water consumption / year
L133.FAO_an_heads$water.consumption <- L133.FAO_an_heads$water.consumption / conv_days_year

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
