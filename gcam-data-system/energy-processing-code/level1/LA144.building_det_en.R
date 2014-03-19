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
logstart( "LA144.building_det_en.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Global detailed buildings energy data" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_bld_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
calibrated_techs_bld_det <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_bld_det" )
A44.cost_efficiency <- readdata( "ENERGY_ASSUMPTIONS", "A44.cost_efficiency" )
A44.internal_gains <- readdata( "ENERGY_ASSUMPTIONS", "A44.internal_gains" )
A44.share_serv_fuel <- readdata( "ENERGY_ASSUMPTIONS", "A44.share_serv_fuel" )
A44.shell_eff_mult_RG3 <- readdata( "ENERGY_ASSUMPTIONS", "A44.shell_eff_mult_RG3" )
A44.tech_eff_mult_RG3 <- readdata( "ENERGY_ASSUMPTIONS", "A44.tech_eff_mult_RG3" )
A44.USA_TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A44.USA_TechChange" )
L101.in_EJ_ctry_bld_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L101.in_EJ_ctry_bld_Fi_Yh" )
L142.in_EJ_R_bld_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L142.in_EJ_R_bld_F_Yh" )
L143.HDDCDD_scen_RG3_Y <- readdata( "ENERGY_LEVEL1_DATA", "L143.HDDCDD_scen_RG3_Y" )
L143.HDDCDD_scen_ctry_Y <- readdata( "ENERGY_LEVEL1_DATA", "L143.HDDCDD_scen_ctry_Y" )
GCAM3_pcGDP <- readdata( "SOCIO_LEVEL0_DATA", "GCAM3_pcGDP" )
L102.pcgdp_thous90USD_GCAM3_ctry_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_GCAM3_ctry_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Calculate shell efficiency by region, and year
#Write out the tech change table to all desired years, and convert to ratios from a base year
L144.USA_TechChange <- gcam_interp( A44.USA_TechChange, c( historical_years, future_years ) )
L144.USA_TechChange[[ subs ]] <- calibrated_techs_bld_det[[ subs ]][
      match( vecpaste( L144.USA_TechChange[ c( supp, tech ) ] ),
             vecpaste( calibrated_techs_bld_det[ c( supp, tech ) ] ) ) ]
L144.USA_TechChange <- L144.USA_TechChange[ c( s_s_t, c( X_historical_years, X_future_years ) ) ]

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
    timesteps_rep_techs[,i-1]
}

#These multipliers assume a base year of the first historical year. However most of the efficiencies are based on data from more recent
# years. This next part adjusts the scale so that the index year is not the first historical year
base_tech_eff_Index_year <- "X2000"
L144.USA_TechMult[ c( X_historical_years, X_future_years ) ] <- L144.USA_TechMult[ c( X_historical_years, X_future_years ) ] /
      L144.USA_TechMult[[ base_tech_eff_Index_year ]]

#This table can then be repeated by the number of regions, and multiplied by region-specific
# adjustment factors (interpolated)( need to match on the region)

#repeat table by number of regions and match in the associated GCAM 3.0 region
#NOTE: This just uses an approximate match between the new regions and the GCAM 3.0 regions, based on the first country alphabetically that is
# matched between the new and old regions. For new composite regions that are quite different from before, this can cause inconsistent mappings
L144.TechMult_R <- repeat_and_add_vector( L144.USA_TechMult, R, unique(GCAM_region_names[[R]]) )
L144.TechMult_R$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L144.TechMult_R[[R]], iso_GCAM_regID[[R]] ) ]
L144.TechMult_R <- L144.TechMult_R[c(R, "region_GCAM3", s_s_t, X_historical_years, X_future_years)]

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
L144.end_use_eff <- L144.end_use_eff_Index
L144.end_use_eff[ c( X_historical_years, X_future_years ) ] <- L144.end_use_eff_Index[ c( X_historical_years, X_future_years ) ] *
      A44.cost_efficiency$efficiency[
          match( vecpaste( L144.end_use_eff_Index[ s_s_t ] ), vecpaste( A44.cost_efficiency[ s_s_t ] ) ) ]

#Drop district heat and traditional biomass in regions where this is not modeled
noheat_regions_distheat <- paste( A_regions[[R]][ A_regions$heat == 0 ], "district heat" )
notradbio_regions_tradbio <- paste( A_regions[[R]][ A_regions$tradbio == 0 ], "traditional biomass" )
L144.end_use_eff <- subset( L144.end_use_eff,
      paste( GCAM_region_ID, subsector ) %!in% c( noheat_regions_distheat, notradbio_regions_tradbio ) )

# 2c. Calculate costs by service, technology, and year
A44.cost_efficiency$CRF <- ( discount_rate_bld * ( ( 1 + discount_rate_bld )^A44.cost_efficiency$lifetime ) ) / 
                            ( ( ( 1 + discount_rate_bld )^A44.cost_efficiency$lifetime ) - 1 )
A44.cost_efficiency$CapitalCost <- A44.cost_efficiency$installed.cost * A44.cost_efficiency$CRF
A44.cost_efficiency$NonEnergyCost <- A44.cost_efficiency$CapitalCost + A44.cost_efficiency$O.M.cost
A44.cost_efficiency$ServiceOutput <- A44.cost_efficiency$UEC * A44.cost_efficiency$efficiency
A44.cost_efficiency$NEcostPerService <- ( A44.cost_efficiency$NonEnergyCost / A44.cost_efficiency$ServiceOutput ) * conv_2005_1975_USD
L144.NEcost_75USDGJ <- A44.cost_efficiency[ c( s_s_t, "NEcostPerService" ) ]

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

#For making the energy consumption table, start with the tech list that will be in each region, and repeat by number of countries from IEA
L144.in_EJ_ctry_bld_serv_F <- repeat_and_add_vector( calibrated_techs_bld_det[ c( S_F, "service" ) ], "iso", sort( unique( L101.in_EJ_ctry_bld_Fi_Yh$iso ) ) )

#Match in the name of the region_GCAM3
L144.in_EJ_ctry_bld_serv_F$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L144.in_EJ_ctry_bld_serv_F$iso, iso_GCAM_regID$iso ) ]

#The next sequence of steps is intended to modify service shares for countries within region_GCAM3, to account for sub-regional differences in HDDCDD.
# First, need to associate HDD and CDD with the corresponding services (heating and cooling, respectively)
thermal_services <- unique( calibrated_techs_bld_det$service[ !calibrated_techs_bld_det$supplysector %in% A44.internal_gains$supplysector ] )
heating_services <- thermal_services[ grepl( "heat", thermal_services ) ]
cooling_services <- thermal_services[ grepl( "cool", thermal_services ) ]
hddcdd_mapping <- rbind( 
      data.frame( service = cooling_services, variable = "CDD"),
      data.frame( service = heating_services, variable = "HDD" ) )

#Subset the thermal services
L144.in_EJ_ctry_bld_thrm_F <- subset( L144.in_EJ_ctry_bld_serv_F, service %in% thermal_services )
L144.in_EJ_ctry_bld_thrm_F$variable <- hddcdd_mapping$variable[ match( L144.in_EJ_ctry_bld_thrm_F$service, hddcdd_mapping$service ) ]

#Then, calculate the "normals" from the HDD and CDD data, both at the country level and the GCAM 3.0 region level.
L143.HDDCDD_scen_ctry_Y$normal <- apply( L143.HDDCDD_scen_ctry_Y[ X_climate_normal_years ], 1, mean )

##NOTE: wherever the climate normal is less than 1, this will round to 0 when hddcdd are read in to the model. Go ahead and put these at 0,
# in order to remove any instances where HDDCDD are treated as being greater than 0 when the model will see a 0
L143.HDDCDD_scen_ctry_Y$normal[ L143.HDDCDD_scen_ctry_Y$normal < 1 ] <- 0

#Calculate the normals for each country over the historical time period
L143.HDDCDD_scen_RG3_Y$normal <- apply( L143.HDDCDD_scen_RG3_Y[ X_climate_normal_years ], 1, mean )
L144.in_EJ_ctry_bld_thrm_F$normal <- L143.HDDCDD_scen_ctry_Y$normal[
      match( vecpaste( L144.in_EJ_ctry_bld_thrm_F[ c( "iso", "variable" ) ] ),
             vecpaste( L143.HDDCDD_scen_ctry_Y[ c( "iso", "variable" ) ] ) ) ]

#Missing values are countries where no HDDCDD data are available. These will default to their region's average
L144.in_EJ_ctry_bld_thrm_F$normal_RG3 <- L143.HDDCDD_scen_RG3_Y$normal[
      match( vecpaste( L144.in_EJ_ctry_bld_thrm_F[ c( "region_GCAM3", "variable" ) ] ),
             vecpaste( L143.HDDCDD_scen_RG3_Y[ c( "region_GCAM3", "variable" ) ] ) ) ]
L144.in_EJ_ctry_bld_thrm_F$normal[ is.na( L144.in_EJ_ctry_bld_thrm_F$normal ) ] <-
      L144.in_EJ_ctry_bld_thrm_F$normal_RG3[ is.na( L144.in_EJ_ctry_bld_thrm_F$normal ) ]

#Shares of energy allocated to heating are assumed to be modified only by HDD in each country. However, cooling
# energy shares may be better if modified in some way by GDP.
## NOTE: This method is not implemented at present
#L144.in_EJ_ctry_bld_thrm_F$pcGDP <- (
#      L102.pcgdp_thous90USD_GCAM3_ctry_Y[[ X_serv_share_year ]][
#      match( L144.in_EJ_ctry_bld_thrm_F$iso, L102.pcgdp_thous90USD_GCAM3_ctry_Y$iso ) ] )
#L144.in_EJ_ctry_bld_thrm_F$pcGDP_RG3 <- ( 
#      GCAM3_pcGDP[[ X_serv_share_year ]][
#      match( L144.in_EJ_ctry_bld_thrm_F$region_GCAM3, GCAM3_pcGDP$region_GCAM3 ) ] )

#Where GDP values for specific nations are missing, default to the regional average
#L144.in_EJ_ctry_bld_thrm_F$pcGDP[ is.na( L144.in_EJ_ctry_bld_thrm_F$pcGDP ) ] <-
#      L144.in_EJ_ctry_bld_thrm_F$pcGDP_RG3[ is.na( L144.in_EJ_ctry_bld_thrm_F$pcGDP ) ]

#Calculate the adjustment to the energy consumed by heating and cooling.
# Heating: use only the degree day ratio
L144.in_EJ_ctry_bld_thrm_F$adjustment <-
      L144.in_EJ_ctry_bld_thrm_F$normal /
      L144.in_EJ_ctry_bld_thrm_F$normal_RG3
#L144.in_EJ_ctry_bld_thrm_F$adjustment[ L144.in_EJ_ctry_bld_thrm_F$service %in% cooling_services ] <-
#      L144.in_EJ_ctry_bld_thrm_F$adjustment[ L144.in_EJ_ctry_bld_thrm_F$service %in% cooling_services ] *
#      L144.in_EJ_ctry_bld_thrm_F$pcGDP[ L144.in_EJ_ctry_bld_thrm_F$service %in% cooling_services ] /
#      L144.in_EJ_ctry_bld_thrm_F$pcGDP_RG3[ L144.in_EJ_ctry_bld_thrm_F$service %in% cooling_services ]

#Match in the unadjusted shares, and compute the first-order estimate of energy consumption
L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel_RG3 <- A44.share_serv_fuel$share_serv_fuel[
      match( vecpaste( L144.in_EJ_ctry_bld_thrm_F[ c( "region_GCAM3", S_F, "service" ) ] ),
             vecpaste( A44.share_serv_fuel[ c( "region_GCAM3", S_F, "service" ) ] ) ) ]

#Drop the rows where no service shares are modeled
L144.in_EJ_ctry_bld_thrm_F <- L144.in_EJ_ctry_bld_thrm_F[ complete.cases( L144.in_EJ_ctry_bld_thrm_F ), ]

#Match in the energy consumption quantities in a base year, multiplying by the service shares
## Using rowMeans() to include all historical years in the calc. This guards against accidentally dropping fuels that may be zero in one year but non-zero in others.
## NOTE: these energy consumption quantities are from early in the processing and are not scaled to the final energy quantities. Don't match in all historical years here
L101.in_EJ_ctry_bld_Fi_Yh$sector <- sub( "in_", "", L101.in_EJ_ctry_bld_Fi_Yh$sector )
L101.in_EJ_ctry_bld_Fi_Yh$Energy_EJ <- rowMeans( L101.in_EJ_ctry_bld_Fi_Yh[ X_historical_years ] )

#Energy_tot = energy consumption by country, sector, and fuel (not disaggregated to service)
L144.in_EJ_ctry_bld_thrm_F$Energy_tot_EJ <- L101.in_EJ_ctry_bld_Fi_Yh$Energy_EJ[
      match( vecpaste( L144.in_EJ_ctry_bld_thrm_F[ c( "iso", S_F ) ] ),
             vecpaste( L101.in_EJ_ctry_bld_Fi_Yh[ c( "iso", S_F ) ] ) ) ]
L144.in_EJ_ctry_bld_thrm_F$Energy_tot_EJ[ is.na( L144.in_EJ_ctry_bld_thrm_F$Energy_tot_EJ ) ] <- 0

#For the first-order estimate of energy by service, multiply the total energy by the default (region_GCAM3) shares
L144.in_EJ_ctry_bld_thrm_F$Energy_unadj_EJ <-
      L144.in_EJ_ctry_bld_thrm_F$Energy_tot_EJ *
      L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel_RG3

#Calculate the adjusted energy consumption (unadjusted energy times the adjustment factor)
L144.in_EJ_ctry_bld_thrm_F$Energy_adj_EJ <-
      L144.in_EJ_ctry_bld_thrm_F$Energy_unadj_EJ *
      L144.in_EJ_ctry_bld_thrm_F$adjustment

#This adjusted energy is unscaled, in that when aggregated by GCAM3 region, the.service allocations will be different
# than the original assumed amounts.
# The next steps calculate energy scalers specific to each region_GCAM3, sector, and fuel
L144.scalers_RG3_bld_thrm_F <- aggregate( L144.in_EJ_ctry_bld_thrm_F[ c( "Energy_unadj_EJ", "Energy_adj_EJ" ) ],
      by=as.list( L144.in_EJ_ctry_bld_thrm_F[ c( "region_GCAM3", S_F, "service" ) ] ), sum )
L144.scalers_RG3_bld_thrm_F$scaler <-
      L144.scalers_RG3_bld_thrm_F$Energy_unadj_EJ /
      L144.scalers_RG3_bld_thrm_F$Energy_adj_EJ
L144.scalers_RG3_bld_thrm_F$scaler[ is.na( L144.scalers_RG3_bld_thrm_F$scaler ) ] <- 1

#Use the scalers to calculate adjusted and scaled energy consumption by country, sector, fuel, and service
# These will be used to calculate the final service portions for each country
L144.in_EJ_ctry_bld_thrm_F$Energy_final_EJ <- L144.in_EJ_ctry_bld_thrm_F$Energy_adj_EJ *
      L144.scalers_RG3_bld_thrm_F$scaler[
         match( vecpaste( L144.in_EJ_ctry_bld_thrm_F[ c( "region_GCAM3", S_F, "service" ) ] ),
                vecpaste( L144.scalers_RG3_bld_thrm_F[ c( "region_GCAM3", S_F, "service" ) ] ) ) ]

#Now we can compute the shares of energy allocated to heating and cooling. Other will be the residual.
L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel <-
      L144.in_EJ_ctry_bld_thrm_F$Energy_final_EJ /
      L144.in_EJ_ctry_bld_thrm_F$Energy_tot_EJ
L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel[ is.na( L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel ) ] <- 0

#Never allow these shares to exceed a maximum assumed threshold
L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel[
      L144.in_EJ_ctry_bld_thrm_F$service %in% heating_services &
      L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel > 1 ] <- max_heating_share
L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel[
      L144.in_EJ_ctry_bld_thrm_F$service %in% cooling_services &
      L144.in_EJ_ctry_bld_thrm_F$share_serv_fuel > 1 ] <- max_cooling_share

#Aggregate the services to calculate the residual to be allocated to non-thermal services
L144.share_ctry_bld_thrm_F <- aggregate( L144.in_EJ_ctry_bld_thrm_F[ "share_serv_fuel" ],
      by=as.list( L144.in_EJ_ctry_bld_thrm_F[ c( "iso", S_F ) ] ), sum )

L144.in_EJ_ctry_bld_oth_F <- subset( L144.in_EJ_ctry_bld_serv_F, service %!in% thermal_services )
L144.in_EJ_ctry_bld_oth_F$share_serv_fuel <- 1 - L144.share_ctry_bld_thrm_F$share_serv_fuel[
      match( vecpaste( L144.in_EJ_ctry_bld_oth_F[ c( "iso", S_F ) ] ),
             vecpaste( L144.share_ctry_bld_thrm_F[ c( "iso", S_F ) ] ) ) ]

#Re-build the table with all services
L144.in_EJ_ctry_bld_serv_F <- rbind(
      L144.in_EJ_ctry_bld_thrm_F[ c( "iso", S_F, "service", "share_serv_fuel" ) ],
      L144.in_EJ_ctry_bld_oth_F[ c( "iso", S_F, "service", "share_serv_fuel" ) ] )

#Multiply by the country/sector/fuel energy consumption to get the estimate of energy consumption
L144.in_EJ_ctry_bld_serv_F$Energy_EJ <- L144.in_EJ_ctry_bld_serv_F$share_serv_fuel *
      L101.in_EJ_ctry_bld_Fi_Yh$Energy_EJ[
         match( vecpaste( L144.in_EJ_ctry_bld_serv_F[ c( "iso", S_F ) ] ),
                vecpaste( L101.in_EJ_ctry_bld_Fi_Yh[ c( "iso", S_F ) ] ) ) ]
L144.in_EJ_ctry_bld_serv_F$Energy_EJ[ is.na( L144.in_EJ_ctry_bld_serv_F$Energy_EJ ) ] <- 0

#This is now ready for aggregation and computation of service shares by the "new" GCAM regions
L144.in_EJ_ctry_bld_serv_F[[ R ]] <- iso_GCAM_regID[[ R ]][ match( L144.in_EJ_ctry_bld_serv_F$iso, iso_GCAM_regID$iso ) ]
L144.in_EJ_R_bld_serv_F_Yh <- aggregate( L144.in_EJ_ctry_bld_serv_F[ "Energy_EJ" ],
      by=as.list( L144.in_EJ_ctry_bld_serv_F[ c( R_S_F, "service" ) ] ), sum )
L144.in_EJ_R_bld_F_Yh <- aggregate( L144.in_EJ_ctry_bld_serv_F[ "Energy_EJ" ],
      by=as.list( L144.in_EJ_ctry_bld_serv_F[ R_S_F ] ), sum )
L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel <- L144.in_EJ_R_bld_serv_F_Yh$Energy_EJ / L144.in_EJ_R_bld_F_Yh$Energy_EJ[
      match( vecpaste( L144.in_EJ_R_bld_serv_F_Yh[ c( R_S_F ) ] ),
             vecpaste( L144.in_EJ_R_bld_F_Yh[ c( R_S_F ) ] ) ) ]
L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel[ is.na( L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel ) ] <- 0

#Multiply these shares by the (final, adjusted) energy consumption by region / sector / fuel
L144.in_EJ_R_bld_serv_F_Yh[ X_historical_years ] <- L144.in_EJ_R_bld_serv_F_Yh$share_serv_fuel * L142.in_EJ_R_bld_F_Yh[
      match( vecpaste( L144.in_EJ_R_bld_serv_F_Yh[ R_S_F ] ),
             vecpaste( L142.in_EJ_R_bld_F_Yh[ R_S_F ] ) ),
      X_historical_years ]
L144.in_EJ_R_bld_serv_F_Yh <- L144.in_EJ_R_bld_serv_F_Yh[ c( R_S_F, "service", X_historical_years ) ]

#This has a number of combinations that do not apply. Drop the known ones.
#This should take care of all missing values
#Heat in regions where this is not modeled as a separate fuel
noheat_regions_heat <- paste( A_regions[[R]][ A_regions$heat == 0 ], "heat" )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( GCAM_region_ID, fuel ) %!in% noheat_regions_heat )

#Traditional biomass in regions where this fuel is not modeled
notradbio_regions_tradbio <- paste( A_regions[[R]][ A_regions$tradbio_region == 0 ], "traditional biomass" )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( GCAM_region_ID, fuel ) %!in% notradbio_regions_tradbio )
L144.in_EJ_R_bld_serv_F_Yh <- subset( L144.in_EJ_R_bld_serv_F_Yh, paste( sector, fuel ) != "bld_comm traditional biomass" )

# 2f. Base service = output by each service = energy consumption times efficiency aggregated by region, sector, service
# Multiply the energy consumption in historical years times the efficiency in historical years, matching on 
#  Match the sector, fuel, and service names into the efficiency table using the calibrated_techs_det_bld lookup
#  Multiply using the match( vecpaste() ) functions
# Aggregate using the sum function and by=as.list(x[c("sector","fuel","service")])
# Write that out!

# match in sector, fuel, service into efficiency table
L144.end_use_eff_2f <- L144.end_use_eff
L144.end_use_eff_2f[ c( S_F, "service" ) ] <- calibrated_techs_bld_det[ match( vecpaste( L144.end_use_eff_2f[ s_s_t ] ), 
                            vecpaste(calibrated_techs_bld_det[ c( s_s_t ) ] ) ), c( 1,2,3 ) ]
L144.end_use_eff_2f <- L144.end_use_eff_2f[ c( R_S_F, "service", X_historical_years ) ]

#calculate base service
L144.base_service_EJ_serv <- L144.in_EJ_R_bld_serv_F_Yh[ c( R_S_F, "service", X_historical_years ) ]
L144.base_service_EJ_serv[ X_historical_years ] <-  L144.in_EJ_R_bld_serv_F_Yh[ X_historical_years ] * L144.end_use_eff_2f[ 
      match( vecpaste( L144.in_EJ_R_bld_serv_F_Yh[ c( R_S_F, "service" ) ] ) , 
             vecpaste( L144.end_use_eff_2f[c(R_S_F, "service" ) ] ) ), 
      X_historical_years ]
#aggregate across fuel types (by region, sector, service)
L144.base_service_EJ_serv <- aggregate( L144.base_service_EJ_serv[ X_historical_years ],
                     by=as.list( L144.base_service_EJ_serv[ c( R_S,"service" ) ] ), sum )


# 2g Internal gains: internal gain energy released, divided by efficiency of each technology
# Start with table of efficiencies. Subset only the supplysector/subsector/technologies that
# are in the internal gains assumptions table. Then divide the intgains assumptions by the
# efficiency, matching on supplysector / subsector / technology

#not sure if this is right way to subset, grepl "others" ? 
L144.end_use_eff_for_intgains <- subset( L144.end_use_eff,
      vecpaste( L144.end_use_eff[ c( supp, tech ) ] ) %in%
      vecpaste( A44.internal_gains[ c( supp, tech ) ] ) )

#this is for both historical and future years
L144.internal_gains <- L144.end_use_eff_for_intgains[
  c( "GCAM_region_ID", "region_GCAM3", s_s_t, X_historical_years, X_future_years ) ]
L144.internal_gains[ c( X_historical_years, X_future_years ) ] <-  A44.internal_gains[ 
  match( vecpaste( L144.internal_gains[ s_s_t ] ) ,
         vecpaste( A44.internal_gains[ s_s_t ] ) ), "input.ratio" ] /
  L144.internal_gains[ c( X_historical_years, X_future_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.end_use_eff <- c( "Building end-use technology efficiency by GCAM region / service / technology / historical year","Unitless efficiency" )
comments.L144.shell_eff_R_Y <- c( "Building end-use shell efficiency by GCAM region / service / technology / historical year","Unitless efficiency" )
comments.L144.in_EJ_R_bld_serv_F_Yh <- c( "Building energy consumption by GCAM region / sector / service / fuel / historical year","Unit = EJ/yr" )
comments.L144.NEcost_75USDGJ <- c( "Building Non energy cost by supplysector / subsector / technology","Unit = 1975$/GJ-service" )
comments.L144.internal_gains <- c( "Building Internal Gains by supplysector / subsector / technology","Unitless output ratio" )
comments.L144.base_service_EJ_serv <- c( "Building energy output by each service by GCAM region / sector / service / fuel / historical year","Unit = EJ/yr" )

#write tables as CSV files
writedata( L144.end_use_eff, domain="ENERGY_LEVEL1_DATA", fn="L144.end_use_eff", comments=comments.L144.end_use_eff )
writedata( L144.shell_eff_R_Y, domain="ENERGY_LEVEL1_DATA", fn="L144.shell_eff_R_Y", comments=comments.L144.shell_eff_R_Y )
writedata( L144.in_EJ_R_bld_serv_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.in_EJ_R_bld_serv_F_Yh", comments=comments.L144.in_EJ_R_bld_serv_F_Yh )
writedata( L144.NEcost_75USDGJ, domain="ENERGY_LEVEL1_DATA", fn="L144.NEcost_75USDGJ", comments=comments.L144.NEcost_75USDGJ )
writedata( L144.internal_gains, domain="ENERGY_LEVEL1_DATA", fn="L144.internal_gains", comments=comments.L144.internal_gains )
writedata( L144.base_service_EJ_serv, domain="ENERGY_LEVEL1_DATA", fn="L144.base_service_EJ_serv", comments=comments.L144.base_service_EJ_serv )

# Every script should finish with this line
logstop()
