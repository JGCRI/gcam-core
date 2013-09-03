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
logstart( "L144.global_building.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Global Building Data" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
bld_iso_region <- readdata( "ENERGY_MAPPINGS", "bld_iso_region" )
calibrated_techs_bld_det <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_bld_det" )
A44.cost_efficiency <- readdata( "ENERGY_ASSUMPTIONS", "A44.cost_efficiency" )
A44.demand_satiation_mult <- readdata( "ENERGY_ASSUMPTIONS", "A44.demand_satiation_mult" )
A44.share_serv_fuel <- readdata( "ENERGY_ASSUMPTIONS", "A44.share_serv_fuel" )
A44.shell_eff_mult_RG3 <- readdata( "ENERGY_ASSUMPTIONS", "A44.shell_eff_mult_RG3" )
A44.tech_eff_mult_RG3 <- readdata( "ENERGY_ASSUMPTIONS", "A44.tech_eff_mult_RG3" )
A44.USA_TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A44.USA_TechChange" )
L142.in_EJ_R_bld_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L142.in_EJ_R_bld_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Calculate technology efficiency by region, service, technology, and year
# 2b. Calculate shell efficiency by region, and year
# 2c. Calculate costs by service, technology, and year
# 2d. Calculate energy consumption by region, service, technology, and year
#Service share data is share of total TFE by sector, not share within each fuel
#So, re-normalize
L144.share_fuel <- aggregate( A44.share_serv_fuel[[ "share_TFEbysector" ]], by=as.list( A44.share_serv_fuel[ c( "region_GCAM3", S_F ) ] ), sum )
names( L144.share_fuel )[ names( L144.share_fuel ) == "x" ] <- "fuel_share_of_TFEbysector"
A44.share_serv_fuel$fuel_share_of_TFEbysector <- L144.share_fuel$fuel_share_of_TFEbysector[
      match( vecpaste( A44.share_serv_fuel[ c( "region_GCAM3", S_F ) ] ), vecpaste( L144.share_fuel[ c( "region_GCAM3", S_F ) ] ) ) ]
A44.share_serv_fuel$share_serv_fuel <- A44.share_serv_fuel$share_TFEbysector / A44.share_serv_fuel$fuel_share_of_TFEbysector

#Reset NaNs for regions that do not have any of a given fuel type
A44.share_serv_fuel[ is.na( A44.share_serv_fuel ) ] <- 0

#For making the energy consumption table, start with the tech list that will be in each region, and repeat by number of regions
L144.in_EJ_R_bld_serv_F_Yh <- repeat_and_add_vector( calibrated_techs_bld_det, R, GCAM_region_names[[R]] )

#Match in the name of the region_GCAM3
L144.in_EJ_R_bld_serv_F_Yh$region_GCAM3 <- GCAM_region_names$region_GCAM3[ match( L144.in_EJ_R_bld_serv_F_Yh[[R]], GCAM_region_names[[R]] ) ]

#Match in the service shares, and then the energy quantities to be multiplied by the service shares
L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel <- A44.share_serv_fuel$share_serv_fuel[
      match( vecpaste( ))]

# 2e. Calculate service output by region, service, and year (efficiency times energy consumption, aggregated by service)






#Fill out assumptions to all historical and future years
A44.shell_eff_mult_RG3 <- gcam_interp(A44.shell_eff_mult_RG3,c(historical_years,future_years), rule=2)
A44.tech_eff_rate_RG3 <- gcam_interp(A44.tech_eff_rate_RG3, c(historical_years,future_years), rule=2)
A44.USA_TechChange <- gcam_interp(A44.USA_TechChange, c(historical_years,future_years), rule=2)




# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.OECD_pcflsp_Yh <- c( "Building energy consumption by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L144.OECD_pcflsp_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.OECD_pcflsp_Yh", comments=comments.L144.OECD_pcflsp_Yh )

# Every script should finish with this line
logstop()
