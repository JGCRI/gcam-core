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
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
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
# 2a. Calculate shell efficiency by region, and year
#Write out the tech change table to all desired years, and convert to ratios from a base year
L144.USA_TechChange <- gcam_interp( A44.USA_TechChange, c( historical_years, future_years ) )
L144.USA_TechChange <- L144.USA_TechChange[ c( supp, tech, c( X_historical_years, X_future_years ) ) ]

#Now, make a data table with the timesteps that corresponds to the above data frame
timesteps <- c( historical_years, future_years )[ 2:length( c( historical_years, future_years ) ) ] -
  c( historical_years, future_years )[ 1:(length( c( historical_years, future_years ) ) - 1 ) ]

timesteps_rep_techs <- t( data.frame( timesteps ) )[ rep( 1, times = nrow( L144.USA_TechChange ) ), ]

#Use this table to convert the tech change table into ratios (multipliers) from a base year
L144.USA_TechMult <- L144.USA_TechChange

L144.USA_TechMult[ X_historical_years[1] ] <- 1

for( i in 2:length( c( X_historical_years, X_future_years ) ) ){
  L144.USA_TechMult[ c( X_historical_years, X_future_years ) ][i] <-
    L144.USA_TechMult[ c( X_historical_years, X_future_years ) ][i-1] *
    ( 1 + L144.USA_TechChange[ c( X_historical_years, X_future_years ) ][ i ] )^
    timesteps_rep_techs[i-1]
}

#These multipliers assume a base year of the first historical year. However most of the efficiencies are based on data from more recent
# years. This next part adjusts the scale so that the index year is not the first historical year
base_tech_eff_Index_year <- "X2000"
L144.USA_TechMult[ c( X_historical_years, X_future_years ) ] <- L144.USA_TechMult[ c( X_historical_years, X_future_years ) ] /
      L144.USA_TechMult[[ base_tech_eff_Index_year ]]

#This table can then be repeated by the number of regions, and multiplied by region-specific
# adjustment factors (interpolated)( need to match on the region)

#repeat table by number of regions 
region_GCAM3 <- unique(iso_GCAM_regID$region_GCAM3)

L144.TechMult_R <- repeat_and_add_vector( L144.USA_TechMult, "region_GCAM3", region_GCAM3 )
L144.TechMult_R <- L144.TechMult_R[c("region_GCAM3", supp, tech, X_historical_years, X_future_years)]

#Shell efficiency calc
#First, subset the technology/year multiplier table so that it includes only shells
L144.ShellTechMult_R <- subset( L144.TechMult_R, grepl( "shell", technology ) )

#interpolate region specific adjustement factors to historical and future years
A44.shell_eff_mult_RG3 <- gcam_interp( A44.shell_eff_mult_RG3, c( historical_years, future_years ) )
A44.shell_eff_mult_RG3 <- A44.shell_eff_mult_RG3[ c( "region_GCAM3", X_historical_years, X_future_years ) ]

L144.shell_eff_R_Y <- L144.ShellTechMult_R 
L144.shell_eff_R_Y[ c( X_historical_years, X_future_years ) ] <- L144.ShellTechMult_R[ c( X_historical_years,X_future_years ) ] * 
      A44.shell_eff_mult_RG3[ match( L144.ShellTechMult_R$region_GCAM3, A44.shell_eff_mult_RG3$region_GCAM3 ),
      c( X_historical_years, X_future_years ) ]


# 2b. Calculate technology efficiency by region, service, technology, and year

A44.tech_eff_mult_RG3 <- gcam_interp( A44.tech_eff_mult_RG3, c( historical_years, future_years ) )
A44.tech_eff_mult_RG3 <- A44.tech_eff_mult_RG3[ c( "region_GCAM3", X_historical_years, X_future_years ) ]

#First, subset the technology/year multiplier table so that it includes only energy-consuming techs (no shells)
L144.EnduseTechMult_R <- subset( L144.TechMult_R, !grepl( "shell", technology ) )
L144.end_use_eff_Index <- L144.EnduseTechMult_R
L144.end_use_eff_Index[ c( X_historical_years, X_future_years ) ] <- L144.EnduseTechMult_R[ c( X_historical_years,X_future_years ) ] * 
  A44.tech_eff_mult_RG3[ match( L144.EnduseTechMult_R$region_GCAM3, A44.tech_eff_mult_RG3$region_GCAM3 ),
  c( X_historical_years, X_future_years ) ]

#These values are indexed to the USA in the base year. Unlike shells, the end-use technology values read to the model
# are not just indices, so need to multiply through by assumed base efficiency levels for each technology
L144.end_use_eff_Index[ c( X_historical_years, X_future_years ) ] <- L144.end_use_eff_Index[ c( X_historical_years, X_future_years ) ] *
      A44.cost_efficiency$efficiency[
          match( vecpaste( L144.end_use_eff_Index[ c( supp, tech ) ] ), vecpaste( A44.cost_efficiency[ c( supp, tech ) ] ) ) ]

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
L144.in_EJ_R_bld_serv_F_Yh <- repeat_and_add_vector( calibrated_techs_bld_det[ c( S_F, "service" ) ], R, GCAM_region_names[[R]] )

#Match in the name of the region_GCAM3
#NOTE: This just uses an approximate match between the new regions and the GCAM 3.0 regions, based on the first country alphabetically that is
# matched between the new and old regions. For new composite regions that are quite different from before, this can cause inconsistent mappings
L144.in_EJ_R_bld_serv_F_Yh$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L144.in_EJ_R_bld_serv_F_Yh[[R]], iso_GCAM_regID[[R]] ) ]

#Match in the service shares, and then the energy quantities to be multiplied by the service shares
L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel <- A44.share_serv_fuel$share_serv_fuel[
      match( vecpaste( L144.in_EJ_R_bld_serv_F_Yh[ c( "region_GCAM3", S_F, "service" ) ] ),
             vecpaste( A44.share_serv_fuel[ c( "region_GCAM3", S_F, "service" ) ] ) ) ]

# 2e. Calculate service output by region, service, and year (efficiency times energy consumption, aggregated by service)
L144.in_EJ_R_bld_serv_F_Yh[ X_historical_years ] <- L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel * L142.in_EJ_R_bld_F_Yh[
      match( vecpaste( L144.in_EJ_R_bld_serv_F_Yh[ R_S_F ] ),
             vecpaste( L142.in_EJ_R_bld_F_Yh[ R_S_F ] ) ),
      X_historical_years ]
      
#This has a number of combinations that do not apply. Drop the known ones.
#This should take care of all missing values
#Heat in regions where this is not modeled as a separate fuel
noheat_regions_heat <- paste( A_regions[[R]][ A_regions$heat == 0 ], "heat" )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( GCAM_region_ID, fuel ) %!in% noheat_regions_heat )

#Traditional biomass in regions where this fuel is not modeled
notradbio_regions_tradbio <- paste( A_regions[[R]][ A_regions$tradbio_region == 0 ], "traditional biomass" )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( GCAM_region_ID, fuel ) %!in% notradbio_regions_tradbio )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( sector, fuel ) != "bld_comm traditional biomass" )

#Need to check to make sure all energy is accounted for
tmp <- aggregate(L144.in_EJ_R_bld_serv_F_Yh[X_historical_years],by=as.list(L144.in_EJ_R_bld_serv_F_Yh[R_S_F]),sum)
tmp_diff <- tmp
tmp_diff[X_historical_years] <- tmp[X_historical_years] - L142.in_EJ_R_bld_F_Yh[X_historical_years]

#This returns a data table with differences.

#Fill out assumptions to all historical and future years
#A44.shell_eff_mult_RG3 <- gcam_interp(A44.shell_eff_mult_RG3,c(historical_years,future_years), rule=2)
#A44.tech_eff_mult_RG3 <- gcam_interp(A44.tech_eff_mult_RG3, c(historical_years,future_years), rule=2)
#A44.USA_TechChange <- gcam_interp(A44.USA_TechChange, c(historical_years,future_years), rule=2)




# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
#comments.L144.OECD_pcflsp_Yh <- c( "Building energy consumption by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
#writedata( L144.OECD_pcflsp_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.OECD_pcflsp_Yh", comments=comments.L144.OECD_pcflsp_Yh )

# Every script should finish with this line
logstop()
