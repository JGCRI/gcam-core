
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
logstart( "L1321.cement.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical inputs and outputs of cement manufacturing" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A_PrimaryFuelCCoef <- readdata( "EMISSIONS_ASSUMPTIONS", "A_PrimaryFuelCCoef" )
cement_regions <- readdata( "ENERGY_MAPPINGS", "cement_regions" )
Worrell_1994_cement <- readdata( "ENERGY_LEVEL0_DATA", "Worrell_1994_cement" )
IEA_cement_elec_kwht <- readdata( "ENERGY_LEVEL0_DATA", "IEA_cement_elec_kwht" )
IEA_cement_TPE_GJt <- readdata( "ENERGY_LEVEL0_DATA", "IEA_cement_TPE_GJt" )
IEA_cement_fuelshares <- readdata( "ENERGY_LEVEL0_DATA", "IEA_cement_fuelshares" )
L100.CDIAC_CO2_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.CDIAC_CO2_ctry_hist" )
L102.CO2_Mt_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L102.CO2_Mt_R_F_Yh" )
L123.in_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_elec_F_Yh" )
L123.out_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_elec_F_Yh" )
L132.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L132.in_EJ_R_indenergy_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations

limestone_Ccoef <- A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[ A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "limestone" ]

#If the CO2 emissions inventories do not go to the latest historical time period, copy the last available year
X_additional_years <- X_historical_years[ !X_historical_years %in% X_CO2_historical_years ]      
X_final_CO2_year <- X_CO2_historical_years[ length( X_CO2_historical_years ) ]
L1321.CO2_Mt_R_F_Yh <- L102.CO2_Mt_R_F_Yh
L1321.CO2_Mt_R_F_Yh[ X_additional_years ] <- L1321.CO2_Mt_R_F_Yh[ X_final_CO2_year ]

# Downscale Worrell dataset to country level using CDIAC emissions inventory
printlog( "Part 1: Derivation of cement production and limestone consumption by region and historical year" )
printlog( "Downscaling Worrell's process CO2 emissions and cement production in 1994 to all countries")
L1321.Cement_Worrell_ctry <- cement_regions[ c( "iso", "Worrell_region" ) ]
L1321.Cement_Worrell_ctry$process_emissions_ktC <- L100.CDIAC_CO2_ctry_hist$cement[
      match( paste( L1321.Cement_Worrell_ctry$iso, 1994 ),
             vecpaste( L100.CDIAC_CO2_ctry_hist[ c( "iso", "year" ) ] ) ) ]
L1321.Cement_Worrell_ctry <- na.omit( L1321.Cement_Worrell_ctry )
L1321.Cement_Worrell_reg <- aggregate( L1321.Cement_Worrell_ctry[ "process_emissions_ktC"],
      by=as.list( L1321.Cement_Worrell_ctry[ "Worrell_region" ] ), sum )
L1321.Cement_Worrell_ctry$share <- L1321.Cement_Worrell_ctry$process_emissions_ktC /
      L1321.Cement_Worrell_reg$process_emissions_ktC[
           match( L1321.Cement_Worrell_ctry$Worrell_region, L1321.Cement_Worrell_reg$Worrell_region ) ]

#Multiply the region-level process emissions and cement production by these shares to get the country-level estimates
L1321.Cement_Worrell_ctry[ c( "cement_prod_Mt", "process_emissions_MtC") ] <- Worrell_1994_cement[
      match( L1321.Cement_Worrell_ctry$Worrell_region, Worrell_1994_cement$Country ),
      c( "cement_prod_Mt", "process_emissions_MtC") ] *
      L1321.Cement_Worrell_ctry$share

#Aggregate by region to calculate regional ratios of process emissions to cement production
printlog( "Aggregating Worrell's process CO2 emissions and cement production in 1994 by GCAM region" )
L1321.Cement_Worrell_ctry$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L1321.Cement_Worrell_ctry$iso, iso_GCAM_regID$iso ) ]
L1321.Cement_Worrell_R <- aggregate( L1321.Cement_Worrell_ctry[ c( "cement_prod_Mt", "process_emissions_MtC" ) ],
      by=as.list( L1321.Cement_Worrell_ctry[ "GCAM_region_ID" ] ), sum )
L1321.Cement_Worrell_R$prod_emiss_ratio <- L1321.Cement_Worrell_R$cement_prod_Mt / L1321.Cement_Worrell_R$process_emissions_MtC

#Calculate cement production over time, assuming that this ratio is constant over time for each region
printlog( "Using ratio of cement production to process emissions to calculate historical cement production" )
L1321.out_Mt_R_cement_Yh <- data.frame( GCAM_region_ID = L1321.Cement_Worrell_R$GCAM_region_ID, sector = "cement" )
L1321.out_Mt_R_cement_Yh[ X_historical_years ] <- L1321.Cement_Worrell_R$prod_emiss_ratio * L1321.CO2_Mt_R_F_Yh[
      match( paste( L1321.out_Mt_R_cement_Yh$GCAM_region_ID, "limestone" ), vecpaste( L1321.CO2_Mt_R_F_Yh[ R_F ] ) ),
      X_historical_years ]
      
#Use the assumed limestone fuel carbon content (same in all regions) to calculate the limestone consumption and limestone IO coefficients in each region
printlog( "Using assumed limestone carbon content to calculate limestone consumption and IO coefficients" )
L1321.in_Cement_Mt_R_limestone_Yh <- data.frame( L1321.out_Mt_R_cement_Yh[ c( "GCAM_region_ID", "sector" ) ], fuel = "limestone" )
L1321.in_Cement_Mt_R_limestone_Yh[ X_historical_years ] <- L1321.CO2_Mt_R_F_Yh[
      match( vecpaste( L1321.in_Cement_Mt_R_limestone_Yh[ R_F ] ), vecpaste( L1321.CO2_Mt_R_F_Yh[ R_F ] ) ), X_historical_years ] /
      limestone_Ccoef

L1321.IO_Cement_R_limestone_Yh <- data.frame( L1321.in_Cement_Mt_R_limestone_Yh[ R_S_F ],
      L1321.in_Cement_Mt_R_limestone_Yh[ X_historical_years ] / L1321.out_Mt_R_cement_Yh[ X_historical_years ] )

printlog( "Part 2: Derivation of energy inputs to cement production by region and historical year" )
printlog( "Interpolating available data on historical energy intensities" )
L1321.IEA_cement_elec_intensity <- interpolate_and_melt( IEA_cement_elec_kwht, historical_years )
L1321.IEA_cement_elec_intensity$elec_GJkg <- L1321.IEA_cement_elec_intensity$value * conv_kwh_GJ / conv_t_kg

L1321.IEA_cement_TPE_intensity <- interpolate_and_melt( IEA_cement_TPE_GJt, historical_years )
L1321.IEA_cement_TPE_intensity$TPE_GJkg <- L1321.IEA_cement_TPE_intensity$value / conv_t_kg

#Calculate average electric and TPE intensity for each GCAM region (use process emissions as a weighting factor)
L1321.Cement_ALL_ctry_Yh <- subset( L100.CDIAC_CO2_ctry_hist, year %in% historical_years, select = c( "iso", "year" ) )
L1321.Cement_ALL_ctry_Yh$emiss_ktC <- L100.CDIAC_CO2_ctry_hist$cement
#Replace process emissions with actual cement production
L1321.Cement_ALL_ctry_Yh$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L1321.Cement_ALL_ctry_Yh$iso, iso_GCAM_regID$iso ) ]
L1321.Cement_ALL_ctry_Yh$prod_Mt <- L1321.Cement_ALL_ctry_Yh$emiss_ktC * L1321.Cement_Worrell_R$prod_emiss_ratio[
      match( L1321.Cement_ALL_ctry_Yh$GCAM_region_ID, L1321.Cement_Worrell_R$GCAM_region_ID ) ] *
      conv_kt_Mt
L1321.Cement_ALL_ctry_Yh$IEA_intensity_region <- cement_regions$IEA_intensity_region[ match( L1321.Cement_ALL_ctry_Yh$iso, cement_regions$iso ) ]
L1321.Cement_ALL_ctry_Yh$elec_GJkg <- L1321.IEA_cement_elec_intensity$elec_GJkg[
      match( vecpaste( L1321.Cement_ALL_ctry_Yh[ c( "IEA_intensity_region", "year" ) ] ),
             vecpaste( L1321.IEA_cement_elec_intensity[ c( "Country", "year" ) ] ) ) ]
L1321.Cement_ALL_ctry_Yh$TPE_GJkg <- L1321.IEA_cement_TPE_intensity$TPE_GJkg[
      match( vecpaste( L1321.Cement_ALL_ctry_Yh[ c( "IEA_intensity_region", "year" ) ] ),
             vecpaste( L1321.IEA_cement_TPE_intensity[ c( "Country", "year" ) ] ) ) ]

#Calculate the average electricity generation efficiencies by region
L1321.in_EJ_R_elec_Yh <- aggregate( L123.in_EJ_R_elec_F_Yh[ X_historical_years ],
      by=as.list( L123.in_EJ_R_elec_F_Yh[ "GCAM_region_ID" ] ), sum )
L1321.out_EJ_R_elec_F_Yh <- subset( L123.out_EJ_R_elec_F_Yh, fuel %in% L123.in_EJ_R_elec_F_Yh$fuel )
L1321.out_EJ_R_elec_Yh <- aggregate( L1321.out_EJ_R_elec_F_Yh[ X_historical_years ],
      by=as.list( L1321.out_EJ_R_elec_F_Yh[ "GCAM_region_ID" ] ), sum )
L1321.IO_R_elec_Yh <- data.frame( GCAM_region_ID = L1321.in_EJ_R_elec_Yh$GCAM_region_ID,
      L1321.in_EJ_R_elec_Yh[ X_historical_years ] / L1321.out_EJ_R_elec_Yh[ X_historical_years ] )
L1321.IO_R_elec_Yh.melt <- interpolate_and_melt( L1321.IO_R_elec_Yh, historical_years )

#Match the electric generation intensity by GCAM region
L1321.Cement_ALL_ctry_Yh$IOelec <- L1321.IO_R_elec_Yh.melt$value[
      match( vecpaste( L1321.Cement_ALL_ctry_Yh[ R_Y ] ), vecpaste( L1321.IO_R_elec_Yh.melt[ R_Y ] ) ) ]
L1321.Cement_ALL_ctry_Yh$heat_GJkg <- L1321.Cement_ALL_ctry_Yh$TPE_GJkg -
      L1321.Cement_ALL_ctry_Yh$elec_GJkg * L1321.Cement_ALL_ctry_Yh$IOelec

fuelnames <- names( IEA_cement_fuelshares )[ names( IEA_cement_fuelshares ) != "Country" ]
L1321.Cement_ALL_ctry_Yh$IEA_fuelshare_region <- cement_regions$IEA_fuelshare_region[
      match( L1321.Cement_ALL_ctry_Yh$iso, cement_regions$iso ) ]
L1321.Cement_ALL_ctry_Yh[ fuelnames] <- IEA_cement_fuelshares[
      match( L1321.Cement_ALL_ctry_Yh$IEA_fuelshare_region, IEA_cement_fuelshares$Country ),
      fuelnames ]

#Calculate weighted average electric and heat intensities by GCAM region and fuel
weighted_fuels <- paste( fuelnames, "_EJ", sep = "" )
L1321.Cement_ALL_ctry_Yh$heat_EJ <- L1321.Cement_ALL_ctry_Yh$heat_GJkg * L1321.Cement_ALL_ctry_Yh$prod_Mt
L1321.Cement_ALL_ctry_Yh$elec_EJ <- L1321.Cement_ALL_ctry_Yh$elec_GJkg * L1321.Cement_ALL_ctry_Yh$prod_Mt
L1321.Cement_ALL_ctry_Yh[ weighted_fuels ] <- L1321.Cement_ALL_ctry_Yh[ fuelnames ] * L1321.Cement_ALL_ctry_Yh$heat_EJ
L1321.Cement_ALL_R_Yh <- aggregate( L1321.Cement_ALL_ctry_Yh[ c( "prod_Mt", "heat_EJ", "elec_EJ", weighted_fuels ) ],
      by=as.list( L1321.Cement_ALL_ctry_Yh[ R_Y ] ), sum )
L1321.Cement_ALL_R_Yh$heat_GJkg <- L1321.Cement_ALL_R_Yh$heat_EJ / L1321.Cement_ALL_R_Yh$prod_Mt
L1321.Cement_ALL_R_Yh$elec_GJkg <- L1321.Cement_ALL_R_Yh$elec_EJ / L1321.Cement_ALL_R_Yh$prod_Mt
L1321.Cement_ALL_R_Yh$Xyear <- paste( "X", L1321.Cement_ALL_R_Yh$year, sep = "" )

#Compile the IO coefficients (electricity and heat) into a table and combine with limestone. Need to convert to GJ per kg (EJ per Mt)
L1321.IO_Cement_GJkg_R_elec_Yh <- cast( L1321.Cement_ALL_R_Yh, GCAM_region_ID ~ Xyear, value = "elec_GJkg" )
L1321.IO_Cement_GJkg_R_elec_Yh[ X_additional_years ] <- L1321.IO_Cement_GJkg_R_elec_Yh[ X_final_CO2_year ]
L1321.IO_Cement_GJkg_R_elec_Yh <- data.frame( L1321.IO_Cement_GJkg_R_elec_Yh[ "GCAM_region_ID" ],
      sector = "cement", fuel = "electricity", L1321.IO_Cement_GJkg_R_elec_Yh[ X_historical_years ] )
L1321.IO_Cement_GJkg_R_heat_Yh <- cast( L1321.Cement_ALL_R_Yh, GCAM_region_ID ~ Xyear, value = "heat_GJkg" )
L1321.IO_Cement_GJkg_R_heat_Yh[ X_additional_years ] <- L1321.IO_Cement_GJkg_R_heat_Yh[ X_final_CO2_year ]
L1321.IO_Cement_GJkg_R_heat_Yh <- data.frame( L1321.IO_Cement_GJkg_R_heat_Yh[ "GCAM_region_ID" ],
      sector = "cement", fuel = "heat", L1321.IO_Cement_GJkg_R_heat_Yh[ X_historical_years ] )
L1321.IO_GJkg_R_cement_F_Yh <- rbind( L1321.IO_Cement_GJkg_R_elec_Yh, L1321.IO_Cement_GJkg_R_heat_Yh, L1321.IO_Cement_R_limestone_Yh )

#Compile energy consumption
L1321.in_EJ_R_cement_F_Y.melt <- melt( L1321.Cement_ALL_R_Yh[ c( "GCAM_region_ID", "Xyear", "elec_EJ", weighted_fuels ) ], id.vars = c( "GCAM_region_ID", "Xyear" ) )
L1321.in_EJ_R_cement_F_Y.melt$sector <- "cement"
#NOTE: This step sets the fuel names being written out as the lower case of the fuel names in the fuelshares input table
L1321.in_EJ_R_cement_F_Y.melt$fuel <- tolower( substr( L1321.in_EJ_R_cement_F_Y.melt$variable, 1,
      regexpr( "_EJ", L1321.in_EJ_R_cement_F_Y.melt$variable, fixed = T ) - 1 ) )
L1321.in_EJ_R_cement_F_Y.melt$fuel[ grepl( "elec", L1321.in_EJ_R_cement_F_Y.melt$fuel ) ] <- "electricity"
#Reset oil to refined liquids
L1321.in_EJ_R_cement_F_Y.melt$fuel <- sub( "oil", "refined liquids", L1321.in_EJ_R_cement_F_Y.melt$fuel )
L1321.in_EJ_R_cement_F_Y <- cast( L1321.in_EJ_R_cement_F_Y.melt, GCAM_region_ID + sector + fuel ~ Xyear )
L1321.in_EJ_R_cement_F_Y[ X_additional_years ] <- L1321.in_EJ_R_cement_F_Y[ X_final_CO2_year ]

#Calculate the remaining industrial energy use
L1321.in_EJ_R_indenergy_F_Yh <- L132.in_EJ_R_indenergy_F_Yh
L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ] <- L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ] - L1321.in_EJ_R_cement_F_Y[
      match( vecpaste( L1321.in_EJ_R_indenergy_F_Yh[ R_F ] ), vecpaste( L1321.in_EJ_R_cement_F_Y[ R_F ] ) ),
      X_historical_years ]
#For any fuels not in the cement database, reset to original values
L1321.in_EJ_R_indenergy_F_Yh[ !complete.cases( L1321.in_EJ_R_indenergy_F_Yh ), ] <-
      L132.in_EJ_R_indenergy_F_Yh[ !complete.cases( L1321.in_EJ_R_indenergy_F_Yh ), ]

#This dataset may have negative values. If it's biomass, doesn't matter; just set the rest-of-industry to exogenous minimum value
for( i in 1:ncol( L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ] ) ){
	L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ][i][
	    L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ][i] < 0 & L1321.in_EJ_R_indenergy_F_Yh$fuel == "biomass"] <- min_in_EJ_ind
}
if( any( L1321.in_EJ_R_indenergy_F_Yh[ X_historical_years ] < 0 ) ){
	printlog( "ERROR: Net negative industrial energy use")
}

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1321.out_Mt_R_cement_Yh <- c( "Cement production by GCAM region / historical year","Unit = Mt" )
comments.L1321.IO_GJkg_R_cement_F_Yh <- c( "Input-output coefficients of cement production by GCAM region / input / historical year","Unit = GJ/kg and kg/kg" )
comments.L1321.in_EJ_R_cement_F_Y <- c( "Energy inputs to cement production by GCAM region / fuel / historical year","Unit = EJ/yr" )
comments.L1321.in_EJ_R_indenergy_F_Yh <- c( "Industrial energy consumption (minus cement) by GCAM region / fuel / historical year","Unit = EJ/yr" )

#write tables as CSV files
writedata( L1321.out_Mt_R_cement_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1321.out_Mt_R_cement_Yh", comments=comments.L1321.out_Mt_R_cement_Yh )
writedata( L1321.IO_GJkg_R_cement_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1321.IO_GJkg_R_cement_F_Yh", comments=comments.L1321.IO_GJkg_R_cement_F_Yh )
writedata( L1321.in_EJ_R_cement_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L1321.in_EJ_R_cement_F_Y", comments=comments.L1321.in_EJ_R_cement_F_Y )
writedata( L1321.in_EJ_R_indenergy_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1321.in_EJ_R_indenergy_F_Yh", comments=comments.L1321.in_EJ_R_indenergy_F_Yh )

# Every script should finish with this line
logstop()
