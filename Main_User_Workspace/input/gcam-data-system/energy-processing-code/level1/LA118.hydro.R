# L117.geo.R

# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "LA118.hydro.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Hydropower production by region and year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
Hydropower_potential <- readdata( "ENERGY_LEVEL0_DATA", "Hydropower_potential" )
L100.IEA_en_bal_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.IEA_en_bal_ctry_hist" )
A18.hydro_output <- readdata( "ENERGY_ASSUMPTIONS", "A18.hydro_output" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Calculation of economic hydropower potential by country, in EJ/yr
#Convert table to EJ/yr
# Calculate a capacity factor for translating MW to GWh, using weighted average capacity factor of all existing dams
Hydro_capfac <- sum( Hydropower_potential$Installed_GWh ) / sum( ( Hydropower_potential$Installed_MW * conv_year_hours * conv_mil_bil ) )

# Economic potential is what we are interested in from this database; however it is often not reported. Many countries without reported
# economic potential nevertheless have estimates of the technical potential.
# Calculate a translation from Technical potential to Economic potential, using weighted average among regions where both are reported
# First, for countries reporting in MW, convert to GWh (most countries have potentials as GWh but some are in MW)
Hydropower_potential$Technical_GWh[ !is.na( Hydropower_potential$Technical_MW ) ] <-
      Hydropower_potential$Technical_MW[ !is.na( Hydropower_potential$Technical_MW ) ] * conv_year_hours * conv_mil_bil * Hydro_capfac
Hydropower_potential$Economic_GWh[ !is.na( Hydropower_potential$Economic_MW ) ] <-
      Hydropower_potential$Economic_MW[ !is.na( Hydropower_potential$Economic_MW ) ] * conv_year_hours * conv_mil_bil * Hydro_capfac

# Among countries with both technical and economic potential reported, calculate an average translation from one to the other
Hydro_tech_econ <- sum( Hydropower_potential$Economic_GWh[ !is.na( Hydropower_potential$Technical_GWh ) & !is.na( Hydropower_potential$Economic_GWh ) ] ) /
      sum( Hydropower_potential$Technical_GWh[ !is.na( Hydropower_potential$Technical_GWh ) & !is.na( Hydropower_potential$Economic_GWh ) ] )

#For countries with technical potential reported but no economic potential, estimate the economic potential
Hydropower_potential$Economic_GWh[ is.na( Hydropower_potential$Economic_GWh ) ] <-
      Hydropower_potential$Technical_GWh[ is.na( Hydropower_potential$Economic_GWh ) ] * Hydro_tech_econ

#This still leaves a few countries for which no estimates of hydro potentials are provided.
# The largest among them is North Korea; most of the others are islands or deserts that probably don't have much beyond present-day production.
# Not worrying about these (future growth rates will be set to 0)

Hydropower_potential$Economic_EJ <- Hydropower_potential$Economic_GWh * conv_GWh_EJ

#Calculate the growth potential by country, as the economic potential minus the actual generation in the most recent historical year (from the IEA balances)
L118.out_EJ_ctry_elec_hydro_fby <- subset( L100.IEA_en_bal_ctry_hist, FLOW == "ELOUTPUT" & PRODUCT == "Hydro" )[
      c( "iso", "FLOW", "PRODUCT", X_final_historical_year ) ]
L118.out_EJ_ctry_elec_hydro_fby[ X_final_historical_year ] <- L118.out_EJ_ctry_elec_hydro_fby[ X_final_historical_year ] * conv_GWh_EJ
L118.out_EJ_ctry_elec_hydro_fby$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L118.out_EJ_ctry_elec_hydro_fby$iso, iso_GCAM_regID$iso ) ]
L118.out_EJ_RG3_elec_hydro_fby <- aggregate( L118.out_EJ_ctry_elec_hydro_fby[ X_final_historical_year ],
      by=as.list( L118.out_EJ_ctry_elec_hydro_fby[ "region_GCAM3" ] ), sum )

#Calculate the future growth potential in each country as the economic potential minus the present-day generation
Hydropower_potential$Growth_potential_EJ <- Hydropower_potential$Economic_EJ - L118.out_EJ_ctry_elec_hydro_fby[[ X_final_historical_year ]][
      match( Hydropower_potential$iso, L118.out_EJ_ctry_elec_hydro_fby$iso ) ]
Hydropower_potential$Growth_potential_EJ[ is.na( Hydropower_potential$Growth_potential_EJ ) | Hydropower_potential$Growth_potential_EJ < 0 ] <- 0

#Downscaling of GCAM 3.0 scenario to the country level: Assign future growth according to shares of growth potential by country
L118.out_EJ_RG3_elec_hydro_Y <- L118.out_EJ_RG3_elec_hydro_fby
GCAM3_future_years <- names( A18.hydro_output )[ names( A18.hydro_output ) %in% X_future_years ]
L118.out_EJ_RG3_elec_hydro_Y[ GCAM3_future_years ] <- A18.hydro_output[
      match( L118.out_EJ_RG3_elec_hydro_Y$region_GCAM3, A18.hydro_output$region_GCAM3 ), GCAM3_future_years ]

#Interpolate any between years, and use the final year as a proxy for years thereafter
#Drop 2020 because it is lower than 2010 in many regions
L118.out_EJ_RG3_elec_hydro_Y <- L118.out_EJ_RG3_elec_hydro_Y[ names( L118.out_EJ_RG3_elec_hydro_Y ) != "X2020" ]
L118.out_EJ_RG3_elec_hydro_Y <- gcam_interp( L118.out_EJ_RG3_elec_hydro_Y, future_years, rule = 2 )[
      c( "region_GCAM3", X_final_historical_year, X_future_years ) ]

#Calculate the growth from the base year in each region_GCAM3. This will be partitioned to countries according to potential
L118.growth_EJ_RG3_elec_hydro_Y <- data.frame(
      L118.out_EJ_RG3_elec_hydro_Y[ "region_GCAM3" ],
      L118.out_EJ_RG3_elec_hydro_Y[ X_future_years ] - L118.out_EJ_RG3_elec_hydro_Y[[ X_final_historical_year ]] )

#Calculate the share of the growth potential by each country within its GCAM 3.0 region
Hydropower_potential$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( Hydropower_potential$iso, iso_GCAM_regID$iso ) ]
Hydropower_potential_RG3 <- aggregate( Hydropower_potential[ "Growth_potential_EJ" ], by=as.list( Hydropower_potential[ "region_GCAM3" ] ), sum )
Hydropower_potential$Growth_potential_EJ_R <- Hydropower_potential_RG3$Growth_potential_EJ[
      match( Hydropower_potential$region_GCAM3, Hydropower_potential_RG3$region_GCAM3 ) ]
Hydropower_potential$share <- Hydropower_potential$Growth_potential_EJ / Hydropower_potential$Growth_potential_EJ_R

#Add future years to table of base-year hydropower generation as base-year output plus region_GCAM3 growth times country-wise share
L118.out_EJ_ctry_elec_hydro_Y <- L118.out_EJ_ctry_elec_hydro_fby[ c( "iso", "region_GCAM3", X_final_historical_year ) ]
L118.out_EJ_ctry_elec_hydro_Y[ X_future_years ] <- L118.out_EJ_ctry_elec_hydro_Y[[ X_final_historical_year ]] +
      Hydropower_potential$share[ match( L118.out_EJ_ctry_elec_hydro_Y$iso, Hydropower_potential$iso ) ] *
      L118.growth_EJ_RG3_elec_hydro_Y[ match( L118.out_EJ_ctry_elec_hydro_Y$region_GCAM3, L118.growth_EJ_RG3_elec_hydro_Y$region_GCAM3 ),
      X_future_years ]

#For countries not in the world dams database (all are very small), copy final historical year forward
L118.out_EJ_ctry_elec_hydro_Y[ !L118.out_EJ_ctry_elec_hydro_Y$iso %in% Hydropower_potential$iso, X_future_years ] <-
      L118.out_EJ_ctry_elec_hydro_Y[ !L118.out_EJ_ctry_elec_hydro_Y$iso %in% Hydropower_potential$iso, X_final_historical_year ]

#Aggregate by region
L118.out_EJ_ctry_elec_hydro_Y[[R]] <- iso_GCAM_regID[[R]][ match( L118.out_EJ_ctry_elec_hydro_Y$iso, iso_GCAM_regID$iso ) ]
L118.out_EJ_R_elec_hydro_Yfut <- aggregate( L118.out_EJ_ctry_elec_hydro_Y[ c( X_final_historical_year, X_future_years ) ],
      by=as.list( L118.out_EJ_ctry_elec_hydro_Y[ R ] ), sum )
L118.out_EJ_R_elec_hydro_Yfut$sector <- "electricity generation"
L118.out_EJ_R_elec_hydro_Yfut$fuel <- "hydro"
L118.out_EJ_R_elec_hydro_Yfut <- L118.out_EJ_R_elec_hydro_Yfut[ c( R_S_F, X_final_historical_year, X_future_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L118.out_EJ_R_elec_hydro_Yfut <- c( "Hydropower production by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L118.out_EJ_R_elec_hydro_Yfut, domain="ENERGY_LEVEL1_DATA", fn="L118.out_EJ_R_elec_hydro_Yfut", comments=comments.L118.out_EJ_R_elec_hydro_Yfut )

# Every script should finish with this line
logstop()
