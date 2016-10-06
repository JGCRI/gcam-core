# this script reads in USDA GDP historical data and converts to GCAM units 
# USDA Source: http://www.ers.usda.gov/datafiles/International_Macroeconomic_Data/Historical_Data_Files/HistoricalRealGDPValues.xls
# USDA dataset is sourced from World Bank and IMF dataset combined to minimize missing values and inconsistencies. 

if( !exists( "SOCIOPROC_DIR" ) ){
  if( Sys.getenv( "SOCIOPROC" ) != "" ){
    SOCIOPROC_DIR <- Sys.getenv( "SOCIOPROC" )
  } else {
    stop("Could not determine location of socioeconomics processing scripts, please set the R var SOCIOPROC_DIR to the appropriate location")
  }
}

# Universal header file - provides logging, file support, etc.
source(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
logstart( "L100.GDP_hist.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Historical GDP downscaled to modern country" )

# --------------------------------------------------  ---------------------------
# 1. Read data
# this is updated to 2010 base year data
# should be re-updated when 2020 base year data becomes availble

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
USDA_GDP_MER <- readdata( "SOCIO_LEVEL0_DATA", "USDA_GDP_MER" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# At present the GDP database used requires no downscaling and all major countries are included, so really no
# processing steps are needed. All that happens in this file right now is subsetting the years that will be
# required by later files, and converting the units from [billion 2010 USD] to GCAM's GDP unit (million 1990 USD) 
L100.gdp_mil90usd_ctry_Yh <- na.omit(
  data.frame(
    USDA_GDP_MER[ "iso" ],
    USDA_GDP_MER[ X_historical_years ] * conv_bil_mil / conv_1990_2010_USD ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L100.gdp_mil90usd_ctry_Yh <- c( "Historical GDP downscaled to country (iso)","Unit = million 1990 US dollars" )

writedata( L100.gdp_mil90usd_ctry_Yh, domain="SOCIO_LEVEL1_DATA", fn="L100.gdp_mil90usd_ctry_Yh", comments=comments.L100.gdp_mil90usd_ctry_Yh )

# Every script should finish with this line
logstop()
