
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L142.building.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical building sector energy consumption by region and fuel" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
enduse_sector_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_sector_aggregation" )
L124.in_EJ_R_heat_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.in_EJ_R_heat_F_Yh" )
L131.in_EJ_R_Senduse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.in_EJ_R_Senduse_F_Yh" )
L131.share_R_Senduse_heat_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.share_R_Senduse_heat_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculation of building sector energy consumption
L142.in_EJ_R_bld_F_Yh <- subset( L131.in_EJ_R_Senduse_F_Yh, grepl( "bld", sector ) )
L142.in_EJ_R_bld_F_Yh$sector <- enduse_sector_aggregation$sector_agg[ match( L142.in_EJ_R_bld_F_Yh$sector, enduse_sector_aggregation$sector ) ]
L142.in_EJ_R_bld_F_Yh$fuel <- enduse_fuel_aggregation$bld[ match( L142.in_EJ_R_bld_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L142.in_EJ_R_bld_F_Yh <- aggregate( L142.in_EJ_R_bld_F_Yh[ X_historical_years ], by=as.list( L142.in_EJ_R_bld_F_Yh[ R_S_F ] ), sum )
L142.in_EJ_R_bld_F_Yh$sector <- sub( "in_", "", L142.in_EJ_R_bld_F_Yh$sector )

##Heat: fuel inputs to heat need to be added to building energy use, in regions where heat is not modeled as a final fuel
L142.share_R_bld_heat_Yh <- subset( L131.share_R_Senduse_heat_Yh, grepl( "bld", sector ) )
L142.share_R_bld_heat_Yh$sector <- sub( "in_", "", L142.share_R_bld_heat_Yh$sector )

#Multiply these shares by the energy inputs to heat
L142.in_EJ_R_bldheat_F_Yh <- subset( L124.in_EJ_R_heat_F_Yh, GCAM_region_ID %in% A_regions[[R]][ A_regions$heat == 0 ] )
L142.in_EJ_R_bldheat_F_Yh <- repeat_and_add_vector( L142.in_EJ_R_bldheat_F_Yh, "sector", unique( L142.share_R_bld_heat_Yh$sector ) )
L142.in_EJ_R_bldheat_F_Yh[ X_historical_years ] <- L142.in_EJ_R_bldheat_F_Yh[ X_historical_years ] * L142.share_R_bld_heat_Yh[
      match( vecpaste( L142.in_EJ_R_bldheat_F_Yh[ R_S ] ), vecpaste( L142.share_R_bld_heat_Yh[ R_S ] ) ),
      X_historical_years ]

#Re-calculate building energy as original estimate plus fuel inputs to heat in regions where heat is not modeled as its own fuel
L142.in_EJ_R_Sbld_F_Yh <- rbind( L142.in_EJ_R_bld_F_Yh, L142.in_EJ_R_bldheat_F_Yh )

#Drop heat in regions where this fuel is backed out to its fuel inputs
L142.in_EJ_R_Sbld_F_Yh <- subset( L142.in_EJ_R_Sbld_F_Yh, paste( GCAM_region_ID, fuel ) %!in%
      paste( A_regions[[R]][ A_regions$heat == 0 ], "heat" ) )
#Drop traditional biomass rows in regions where this fuel is not considered (energy is zero)
L142.in_EJ_R_Sbld_F_Yh <- subset( L142.in_EJ_R_Sbld_F_Yh, paste( GCAM_region_ID, fuel ) %!in%
      paste( A_regions[[R]][ A_regions$tradbio_region == 0 ], "traditional biomass" ) )
L142.in_EJ_R_bld_F_Yh <- aggregate( L142.in_EJ_R_Sbld_F_Yh[ X_historical_years ], by=as.list( L142.in_EJ_R_Sbld_F_Yh[R_S_F ] ), sum )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L142.in_EJ_R_bld_F_Yh <- c( "Building energy consumption by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L142.in_EJ_R_bld_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L142.in_EJ_R_bld_F_Yh", comments=comments.L142.in_EJ_R_bld_F_Yh )

# Every script should finish with this line
logstop()
