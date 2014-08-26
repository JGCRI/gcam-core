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
logstart( "LA1011.en_bal_adj.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Adjustments to IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
EIA_RFO_intlship_kbbld <- readdata( "ENERGY_LEVEL0_DATA", "EIA_RFO_intlship_kbbld" )
EIA_TOT_intlship_kbbld <- readdata( "ENERGY_LEVEL0_DATA", "EIA_TOT_intlship_kbbld" )
EIA_ctry <- readdata( "ENERGY_MAPPINGS", "EIA_ctry" )
A22.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_coef" )
L101.en_bal_EJ_R_Si_Fi_Yh_full <- readdata( "ENERGY_LEVEL1_DATA", "L101.en_bal_EJ_R_Si_Fi_Yh_full" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# subset only the relevant years and combine OECD with non-OECD
###MODIFICATIONS TO IEA ENERGY BALANCES
printlog( "Replacing IEA estimates of international shipping fuel consumption with EIA estimates")
#First, convert available data to EJ per year of total refined liquid products
EIA_yearcols <- names( EIA_RFO_intlship_kbbld )[ names( EIA_RFO_intlship_kbbld ) %in% X_historical_years ]
L1011.in_EJ_ctry_intlship_RFO_Yh <- data.frame(
      EIA_RFO_intlship_kbbld[ "Country" ],
      EIA_RFO_intlship_kbbld[ EIA_yearcols ] *
         conv_kbbl_bbl * conv_bbl_tonne_RFO * conv_tonne_GJ_RFO * conv_GJ_EJ / conv_days_year )
EIA_distillate_intlship_kbbld <- data.frame(
      EIA_RFO_intlship_kbbld[ "Country" ],
      EIA_TOT_intlship_kbbld[ EIA_yearcols ] - EIA_RFO_intlship_kbbld[ EIA_yearcols ] )
L1011.in_EJ_ctry_intlship_distillate_Yh <- data.frame(
      EIA_distillate_intlship_kbbld[ "Country" ],
      EIA_distillate_intlship_kbbld[ EIA_yearcols ] *
         conv_kbbl_bbl * conv_bbl_tonne_distillate * conv_tonne_GJ_RFO * conv_GJ_EJ / conv_days_year )
L1011.in_EJ_ctry_intlship_TOT_Yh <- data.frame(
      L1011.in_EJ_ctry_intlship_distillate_Yh[ "Country" ],
      L1011.in_EJ_ctry_intlship_distillate_Yh[ EIA_yearcols ] + L1011.in_EJ_ctry_intlship_RFO_Yh[ EIA_yearcols ] )

#Several countries blink in and out of the time series. Only changing Russia because it's the only one that is a really large amount of fuel
L1011.in_EJ_ctry_intlship_TOT_Yh[ is.na( L1011.in_EJ_ctry_intlship_TOT_Yh ) ] <- 0
L1011.in_EJ_ctry_intlship_TOT_Yh[ L1011.in_EJ_ctry_intlship_TOT_Yh$Country=="Russia", paste( "X", 2006:2010, sep = "" ) ] <-
      L1011.in_EJ_ctry_intlship_TOT_Yh$X2005[ L1011.in_EJ_ctry_intlship_TOT_Yh$Country=="Russia" ]

#Match in the countries and aggregate by region
L1011.in_EJ_ctry_intlship_TOT_Yh$iso <- EIA_ctry$iso[ match( L1011.in_EJ_ctry_intlship_TOT_Yh$Country, EIA_ctry$EIA_ctry ) ]
L1011.in_EJ_ctry_intlship_TOT_Yh$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L1011.in_EJ_ctry_intlship_TOT_Yh$iso, iso_GCAM_regID$iso)]
L1011.in_EJ_ctry_intlship_TOT_Yh <- subset( L1011.in_EJ_ctry_intlship_TOT_Yh, complete.cases( L1011.in_EJ_ctry_intlship_TOT_Yh ) )
L1011.in_EJ_R_intlship_Yh <- aggregate( L1011.in_EJ_ctry_intlship_TOT_Yh[ EIA_yearcols ],
      by=as.list( L1011.in_EJ_ctry_intlship_TOT_Yh[ "GCAM_region_ID" ] ), sum )

#Replace the data in the IEA energy balances table
L1011.en_bal_EJ_R_Si_Fi_Yh <- L101.en_bal_EJ_R_Si_Fi_Yh_full
L1011.en_bal_EJ_R_Si_Fi_Yh[ L1011.en_bal_EJ_R_Si_Fi_Yh$sector == "in_trn_international ship" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel == "refined liquids", EIA_yearcols ] <-
      L1011.in_EJ_R_intlship_Yh[ EIA_yearcols ]

#Re-calculate TPES after all adjustments are made
L1011.in_EJ_R_Si_Fi_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, grepl( "in_", substr( sector, 1, 3 ) ) | grepl( "net_", substr( sector, 1, 4 ) ) )
L1011.in_EJ_R_Si_Fi_Yh$sector <- "TPES"
L1011.en_bal_EJ_R_Si_Fi_Yh[ L1011.en_bal_EJ_R_Si_Fi_Yh$sector == "TPES", ] <- aggregate( L1011.in_EJ_R_Si_Fi_Yh[ X_historical_years ],
      by=as.list( L1011.in_EJ_R_Si_Fi_Yh[ R_S_F ] ), sum )

printlog( "NOTE: TPES of natural gas includes gasified coal. This is subtracted here" )
#This is complicated. Because the output of "gas works gas" that is produced from coal is not distinguished from that produced by other fuels,
# the process is modeled based on the fuel inputs, and the output fuel is assigned the same name as natural gas. As a result the estimates of
# TPES at this point for natural gas include both natural gas and gasified coal. This subtraction generally follows the method used in code file L122.
L1011.gasproc_coef <- gcam_interp( subset( A22.globaltech_coef, supplysector == "gas processing" ), historical_years )
L1011.gasproc_coef[ S_F ] <- calibrated_techs[ match( vecpaste( L1011.gasproc_coef[ s_s_t ] ), vecpaste( calibrated_techs[ s_s_t ] ) ), S_F ]
L1011.in_EJ_R_gasproc_coal_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_gas processing" & fuel == "coal" )
L1011.out_EJ_R_gasproc_coal_Yh <- L1011.in_EJ_R_gasproc_coal_Yh
L1011.out_EJ_R_gasproc_coal_Yh$sector <- sub( "in_", "", L1011.out_EJ_R_gasproc_coal_Yh$sector )
L1011.out_EJ_R_gasproc_coal_Yh[ X_historical_years ] <- L1011.out_EJ_R_gasproc_coal_Yh[ X_historical_years ] / L1011.gasproc_coef[
      match( vecpaste( L1011.out_EJ_R_gasproc_coal_Yh[ S_F ] ), vecpaste( L1011.gasproc_coef[ S_F ] ) ),
      X_historical_years ]

#Subtract
L1011.en_bal_EJ_R_Si_Fi_Yh[ L1011.en_bal_EJ_R_Si_Fi_Yh$sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel == "gas", X_historical_years ] <-
      L1011.en_bal_EJ_R_Si_Fi_Yh[ L1011.en_bal_EJ_R_Si_Fi_Yh$sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel == "gas", X_historical_years ] -
      L1011.out_EJ_R_gasproc_coal_Yh[ X_historical_years ]

#This is also complicated. In regions with very low natural gas use and high coal-to-gas with very high input-output coefs on coal-to-gas production
# (South Africa), dividing the coal input by the IO coef may cause gas production in excess of the demands in the region.
# If this is the case, need to return to original energy balance data and reduce the coal input to gas works (can re-allocate to another sector if desired)
if( any( L1011.en_bal_EJ_R_Si_Fi_Yh[ L1011.en_bal_EJ_R_Si_Fi_Yh$sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel == "gas", X_historical_years ] < 0 ) ){
	stop( "Exogenous IO coef on coal input to gas works caused an increase in natural gas beyond the regional TPES of gas")
}

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1011.en_bal_EJ_R_Si_Fi_Yh <- c( "Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year","Unit = EJ" )
comments.L1011.in_EJ_ctry_intlship_TOT_Yh <- c( "Liquid fuel consumption by international shipping by country / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L1011.en_bal_EJ_R_Si_Fi_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1011.en_bal_EJ_R_Si_Fi_Yh", comments=comments.L1011.en_bal_EJ_R_Si_Fi_Yh )
writedata( L1011.in_EJ_ctry_intlship_TOT_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1011.in_EJ_ctry_intlship_TOT_Yh", comments=comments.L1011.in_EJ_ctry_intlship_TOT_Yh )

# Every script should finish with this line
logstop()
