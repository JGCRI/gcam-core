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
logstart( "L102.en_emiss_CDIAC.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical CO2 emissions by GCAM region and carbon source" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
CDIAC_fuel <- readdata( "EMISSIONS_MAPPINGS", "CDIAC_fuel" )
A32.nonenergy_Cseq <- readdata( "ENERGY_ASSUMPTIONS", "A32.nonenergy_Cseq" )
L100.CDIAC_CO2_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.CDIAC_CO2_ctry_hist" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
L102.CDIAC_CO2_ctry_hist.melt <- melt( L100.CDIAC_CO2_ctry_hist, id.vars = c( "iso", "year" ) )
L102.CDIAC_CO2_ctry_hist.melt$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L102.CDIAC_CO2_ctry_hist.melt$iso, iso_GCAM_regID$iso ) ]
L102.CDIAC_CO2_ctry_hist.melt$fuel <- CDIAC_fuel$fuel[ match( L102.CDIAC_CO2_ctry_hist.melt$variable, CDIAC_fuel$CDIAC_fuel ) ]

#Aggregate CO2 emissions by GCAM region and fuel
L102.CO2_Mt_R_F_Yh.melt <- aggregate( L102.CDIAC_CO2_ctry_hist.melt[ "value" ] * conv_kt_Mt,
      by=as.list( L102.CDIAC_CO2_ctry_hist.melt[ R_F_Y ] ), sum )

#Cast so that columns are years (column names need "X" pasted)
L102.CO2_Mt_R_F_Yh.melt$Xyear <- paste( "X", L102.CO2_Mt_R_F_Yh.melt$year, sep = "" )
L102.CO2_Mt_R_F_Yh <- cast( L102.CO2_Mt_R_F_Yh.melt, GCAM_region_ID + fuel ~ Xyear )

#Calculate regional and global CO2 emissions coefficients by fuel
#Calculate the TPES by fuel, deducting non-energy use of fuels that does not result in CO2 emissions
L102.en_TPES_EJ_R_Fi_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel )
L102.en_feedstocks_EJ_R_Fi_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_industry_feedstocks" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel )
L102.en_sequestered_EJ_R_Fi_Yh <- data.frame(
      L102.en_feedstocks_EJ_R_Fi_Yh[ R_S_F ],
      L102.en_feedstocks_EJ_R_Fi_Yh[ X_CO2_historical_years ] *
      A32.nonenergy_Cseq$remove.fraction[ match( L102.en_feedstocks_EJ_R_Fi_Yh$fuel, A32.nonenergy_Cseq$subsector ) ] )
L102.en_emitted_EJ_R_Fi_Yh <- data.frame(
      L102.en_TPES_EJ_R_Fi_Yh[ R_S_F ],
      L102.en_TPES_EJ_R_Fi_Yh[ X_CO2_historical_years ] -
      L102.en_sequestered_EJ_R_Fi_Yh[
         match( vecpaste( L102.en_TPES_EJ_R_Fi_Yh[ R_F ] ), vecpaste( L102.en_sequestered_EJ_R_Fi_Yh[ R_F ] ) ),
      X_CO2_historical_years ] )

#Calculate the emissions coefficients by fuel, using only the energy whose carbon is assumed to be emitted
#regional
L102.Ccoef_kgCGJ_R_F_Yh <- data.frame(
      L102.en_emitted_EJ_R_Fi_Yh[ R_F ],
      L102.CO2_Mt_R_F_Yh[ match( vecpaste( L102.en_emitted_EJ_R_Fi_Yh[ R_F ] ), vecpaste( L102.CO2_Mt_R_F_Yh[ R_F ] ) ), X_CO2_historical_years ] /
      L102.en_emitted_EJ_R_Fi_Yh[ X_CO2_historical_years ] )

## reset to defaults wherever NAs result from 0 energy consumption
L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "gas", ][is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "gas", ] ) ] <- default_gas_Ccoef
L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "coal", ][is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "coal", ] ) ] <- default_coal_Ccoef
L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "refined liquids", ][
      is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "refined liquids", ] ) ] <- default_liquids_Ccoef

#global
L102.en_emitted_EJ_Fi_Yh <- aggregate( L102.en_emitted_EJ_R_Fi_Yh[ X_CO2_historical_years ],
      by=as.list( L102.en_emitted_EJ_R_Fi_Yh[ "fuel" ] ), sum )
L102.CO2_Mt_F_Yh <- aggregate( L102.CO2_Mt_R_F_Yh[ X_CO2_historical_years ],
      by=as.list( L102.CO2_Mt_R_F_Yh[ "fuel" ] ), sum )
L102.Ccoef_kgCGJ_F_Yh <- data.frame(
      L102.en_emitted_EJ_Fi_Yh[ "fuel" ],
      L102.CO2_Mt_F_Yh[ match( L102.en_emitted_EJ_Fi_Yh$fuel, L102.CO2_Mt_F_Yh$fuel ), X_CO2_historical_years ] /
      L102.en_emitted_EJ_Fi_Yh[ X_CO2_historical_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L102.CO2_Mt_R_F_Yh <- c( "CO2 emissions by GCAM region / fuel type / historical year","Unit = Mt C" )
comments.L102.Ccoef_kgCGJ_R_F_Yh <- c( "CO2 emissions coefficients by GCAM region / fuel type / historical year","Unit = kg C / GJ fuel" )
comments.L102.Ccoef_kgCGJ_F_Yh <- c( "CO2 emissions coefficients by fuel type / historical year","Unit = kg C / GJ fuel" )

#write tables as CSV files
writedata( L102.CO2_Mt_R_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L102.CO2_Mt_R_F_Yh", comments=comments.L102.CO2_Mt_R_F_Yh )
writedata( L102.Ccoef_kgCGJ_R_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L102.Ccoef_kgCGJ_R_F_Yh", comments=comments.L102.Ccoef_kgCGJ_R_F_Yh )
writedata( L102.Ccoef_kgCGJ_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L102.Ccoef_kgCGJ_F_Yh", comments=comments.L102.Ccoef_kgCGJ_R_F_Yh )

# Every script should finish with this line
logstop()
