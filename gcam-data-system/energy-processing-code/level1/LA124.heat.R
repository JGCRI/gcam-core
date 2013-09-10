
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
logstart( "L124.heat.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical energy consumption for district heat production" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A24.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A24.globaltech_coef" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
heat_regionIDs <- A_regions$GCAM_region_ID[ A_regions$heat == 1]

# 2a. Fuel inputs to district heat
printlog( "Fuel inputs to heat: aggregating intermediate fuels as specified in ", file_fqn( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" ) )
#Process fuel inputs in all regions; some will have the energy assigned to bld/ind, and others have a district heat sector
L124.in_EJ_R_heat_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_heat" )
L124.in_EJ_R_heat_F_Yh$sector <- sub( "in_", "", L124.in_EJ_R_heat_F_Yh$sector )
L124.in_EJ_R_heat_F_Yh$fuel <- enduse_fuel_aggregation$heat[ match( L124.in_EJ_R_heat_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L124.in_EJ_R_heat_F_Yh <- aggregate( L124.in_EJ_R_heat_F_Yh[ X_historical_years ], by=as.list( L124.in_EJ_R_heat_F_Yh[ R_S_F ] ), sum )

# 2b. Heat production from district heat sector
printlog( "Heat output: fuel inputs to heat divided by exogenous input-output coefficients" )
L124.globaltech_coef <- gcam_interp( A24.globaltech_coef, historical_years )
L124.globaltech_coef[ S_F ] <- calibrated_techs[ match( vecpaste( L124.globaltech_coef[ s_s_t ] ), vecpaste( calibrated_techs[ s_s_t ] ) ), S_F ]
L124.out_EJ_R_heat_F_Yh <- L124.in_EJ_R_heat_F_Yh
L124.out_EJ_R_heat_F_Yh[ X_historical_years ] <- L124.in_EJ_R_heat_F_Yh[ X_historical_years ] / L124.globaltech_coef[
      match( vecpaste( L124.in_EJ_R_heat_F_Yh[ S_F ] ), vecpaste( L124.globaltech_coef[ S_F ] ) ),
      X_historical_years ]

#Output of district heat only applies to regions where this is modeled as a separate fuel. Drop all others
L124.out_EJ_R_heat_F_Yh <- L124.out_EJ_R_heat_F_Yh[ L124.out_EJ_R_heat_F_Yh[[R]] %in% heat_regionIDs, ]

# 2c. Secondary output of heat from main activity CHP plants
printlog( "Heat output from main activity CHP plants" )
#Only do this for regions where district heat is being modeled
L124.out_EJ_R_heatfromelec_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "out_electricity_heat" & GCAM_region_ID %in% heat_regionIDs )
L124.out_EJ_R_heatfromelec_F_Yh$sector <- sub( "out_", "", L124.out_EJ_R_heatfromelec_F_Yh$sector )
L124.out_EJ_R_heatfromelec_F_Yh$fuel <- enduse_fuel_aggregation$heat[ match( L124.out_EJ_R_heatfromelec_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L124.out_EJ_R_heatfromelec_F_Yh <- aggregate( L124.out_EJ_R_heatfromelec_F_Yh[ X_historical_years ], by=as.list( L124.out_EJ_R_heatfromelec_F_Yh[ R_S_F ] ), sum )

printlog( "Secondary output coefficients on heat produced by main activity CHP plants" )
#Only do this for regions where district heat is being modeled, and for technologies where heat output is modeled
L124.heatoutratio_R_elec_F_tech_Yh <- subset( L1231.out_EJ_R_elec_F_tech_Yh, GCAM_region_ID %in% heat_regionIDs &
      technology %in% calibrated_techs$technology[ calibrated_techs$secondary.output == "heat" ] )
L124.heatoutratio_R_elec_F_tech_Yh[ X_historical_years ] <- L124.out_EJ_R_heatfromelec_F_Yh[
      match( vecpaste( L124.heatoutratio_R_elec_F_tech_Yh[ R_F ] ), vecpaste( L124.out_EJ_R_heatfromelec_F_Yh[ R_F ] ) ), X_historical_years ] /
      L124.heatoutratio_R_elec_F_tech_Yh[ X_historical_years ]

#Reset missing and infinite values (applicable for CC in the base years) to 0
L124.heatoutratio_R_elec_F_tech_Yh[ is.na( L124.heatoutratio_R_elec_F_tech_Yh ) ] <- 0
L124.heatoutratio_R_elec_F_tech_Yh[ L124.heatoutratio_R_elec_F_tech_Yh == Inf ] <- 0

#Drop all rows where all years are 0
L124.heatoutratio_R_elec_F_tech_Yh <- L124.heatoutratio_R_elec_F_tech_Yh[ rowSums(L124.heatoutratio_R_elec_F_tech_Yh[ X_historical_years ] ) > 0, ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L124.in_EJ_R_heat_F_Yh <- c( "Inputs to heat by GCAM region / fuel / historical year","Unit = EJ" )
comments.L124.out_EJ_R_heat_F_Yh <- c( "Output of district heat sector by GCAM region / fuel / historical year","Unit = EJ" )
comments.L124.out_EJ_R_heatfromelec_F_Yh <- c( "Heat output from electricity generation by GCAM region / fuel / historical year","Unit = EJ" )
comments.L124.heatoutratio_R_elec_F_tech_Yh <- c( "Heat output ratio from electricity generation by GCAM region / fuel / historical year","GJ heat / GJ elec" )

#write tables as CSV files
writedata( L124.in_EJ_R_heat_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L124.in_EJ_R_heat_F_Yh", comments=comments.L124.in_EJ_R_heat_F_Yh )
writedata( L124.out_EJ_R_heat_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L124.out_EJ_R_heat_F_Yh", comments=comments.L124.out_EJ_R_heat_F_Yh )
writedata( L124.out_EJ_R_heatfromelec_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L124.out_EJ_R_heatfromelec_F_Yh", comments=comments.L124.out_EJ_R_heatfromelec_F_Yh )
writedata( L124.heatoutratio_R_elec_F_tech_Yh, domain="ENERGY_LEVEL1_DATA", fn="L124.heatoutratio_R_elec_F_tech_Yh", comments=comments.L124.heatoutratio_R_elec_F_tech_Yh )

# Every script should finish with this line
logstop()
